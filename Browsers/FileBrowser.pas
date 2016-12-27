unit FileBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, StdCtrls, Menus, Vcl.ExtCtrls, DelayLoadTree,
  VirtualTrees, Generics.Collections, CommonMessages, CommonResources, AssemblyDb,
  AssemblyDb.Assemblies, AssemblyDb.Files, AssemblyDb.Environ;

type
  TNodeType = (ntFolder, ntFile);

 {
  Folders of the FS are anchored to nodes in the tree.
  Each node may have one anchor (then it's called a monofolder) or multiple,
  each anchor is implicit (automatically given when loading this folder as a child of another)
  or explicit (manually assigned when binding root folders).
 }

  //Depending on a view mode, a node may host content from several database folders.
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Anchors: TArray<TFolderId>;    // to show multiple DB folders merged
    v: TFileEntryData;
  end;
  PNodeData = ^TNodeData;

  TMatchEntry = record
    id: int64;
    type_: TNodeType;     //actually TEntryType
    top: PVirtualNode;    //most precise existing node
    remPath: TFolderIDPath; //remainder path related to the top node
  end;

  TFileBrowserForm = class(TDelayLoadTree)
    Panel: TPanel;
    cbViewMode: TComboBox;
    FilterTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure cbViewModeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FilterTimerTimer(Sender: TObject);
  protected
    ExplicitRoots: TArray<PVirtualNode>;  //Nodes which have explicit anchoring, as opposed to just being children of their parents
    RootNodeData: TNodeData;  //NodeData for root (Tree does not store it) for anchors etc
    modelEnv: TEnvironment;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function ExpandPath(const APath: string): string;
    procedure AnchorRootFolders(ANode: PVirtualNode);
    procedure LoadFolders(ANode: PVirtualNode; Anchors: TArray<TFolderId>);
    procedure LoadFiles(ANode: PVirtualNode; const Anchors: TArray<TFolderId>);
    function GetNodeDataEx(Node: PVirtualNode): PNodeData; inline;
    procedure ResetRootNodeData;

    function ForceFolderPath(AParent: PVirtualNode; AFolderName: string): PVirtualNode;
    procedure AddExplicitAnchor(AFolder: PVirtualNode; AFolderId: TFolderId); overload;
    function AddExplicitAnchor(AParent: PVirtualNode; AFolderName: string; AFolderId: TFolderId): PVirtualNode; overload;
    function AddFolderNode(AParent: PVirtualNode; const AFolderName: string; AFolderId: TFolderId): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;

  protected
    FQuickFilterText: string;
    FMatches: TList<TMatchEntry>;
    procedure ApplyFilter; override;
    procedure ReloadMatches;
    function TraceOneLevel(var Match: TMatchEntry): boolean;
    procedure UpdateNodeFilter(ANode: PVirtualNode);
    procedure WmSetQuickfilter(var msg: TWmSetQuickFilter); message WM_SET_QUICKFILTER;

  public
    procedure Clear; override;

  end;

var
  FileBrowserForm: TFileBrowserForm;

implementation
uses StrUtils, VirtualTreeviewUtils, ManifestEnum.FileActions;

{$R *.dfm}

{$DEFINE ROOTC}

procedure TFileBrowserForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMatches := TList<TMatchEntry>.Create;
  //Prepare environment model
  //At the moment, we're lumping together x64 and wow64 assemblies. It would be too costly to query
  //every assembly for processorArchitecture.
  modelEnv := GetModelEnvironmentBlock('x64');
end;

procedure TFileBrowserForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMatches);
  inherited;
end;

procedure TFileBrowserForm.Clear;
begin
  inherited;
  FMatches.Clear;
  ExplicitRoots := nil;
  ResetRootNodeData;
end;

procedure TFileBrowserForm.cbViewModeChange(Sender: TObject);
begin
  inherited;
  Reload;
end;

//Enhanced GetNodeData, also returns node data for root node (nil)
function TFileBrowserForm.GetNodeDataEx(Node: PVirtualNode): PNodeData;
begin
  if Node = nil then
    Result := @Self.RootNodeData
  else
    Result := Tree.GetNodeData(Node);
end;

//Populates root node data with default / empty values
procedure TFileBrowserForm.ResetRootNodeData;
begin
  RootNodeData.DelayLoad.Touched := true; //though whatever
  RootNodeData.NodeType := ntFolder;
  RootNodeData.Anchors := nil;
  RootNodeData.v.id := 0;
  //The rest is irrelevant
end;

procedure TFileBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
begin
  if (AData <> nil) and (AData.NodeType = ntFile) then exit;

 {
  We'll have to do something strange with root files.

  Files which are parented to folderId==0 are NOT root files. They are files that had no path in the
  manifest. These files should go to one of a few special folders:
    x64/x32  = System32
    msil     = somewhere in .net
    driverUpdates = see below

  But after we've parsed the root FOLDERS and resolved their model paths, we might have assigned
  the root node a number of anchors.

  Files from these anchors SHOULD be displayed, as these are files from say, $(SystemDrive).

  So we add an anchor for actual zero-parented files, and then ignore those and process anchors for root.
 }
  if ANode = nil then begin
    ResetRootNodeData; //If we're reloading root, reset root anchors
    ExplicitRoots := nil;
    if cbViewMode.ItemIndex = 1 then begin
      AnchorRootFolders(ANode);
      //Anchor all zero-parented files to System32
      AddExplicitAnchor(nil, ExpandPath('$(runtime.System32)'), 0);
    end else
     //Just anchor root
      AddExplicitAnchor(nil, 0);
  end;

  //Load files and folders in the normal way
  if AData = nil then
    AData := @RootNodeData;
  LoadFolders(ANode, AData.Anchors);
  LoadFiles(ANode, AData.Anchors);
  UpdateNodeFilter(ANode);
end;

//Expands the given path according to current view mode
function TFileBrowserForm.ExpandPath(const APath: string): string;
begin
  if cbViewMode.ItemIndex = 0 then
    Result := APath
  else begin
    Result := ExpandEnvironmentVariables(modelEnv, APath);
   {$IFDEF ROOTC}
    if Result.StartsWith('C:\') then
      delete(Result, 1, 3);
   {$ENDIF}
  end;
end;

procedure TFileBrowserForm.AnchorRootFolders(ANode: PVirtualNode);
var AFolders: TFolderList;
  AChildFolderId: TFolderId;
begin
  AFolders := TFolderList.Create;
  try
    //Root folder + model paths == expand root children folders
    FDb.Files.GetFolders(0, AFolders);
    for AChildFolderId in AFolders.Keys do
      AddExplicitAnchor(ANode, ExpandPath(AFolders[AChildFolderId].name), AChildFolderId);
  finally
    FreeAndNil(AFolders);
  end;
end;

//Loads all child folder nodes for a given parent node
procedure TFileBrowserForm.LoadFolders(ANode: PVirtualNode; Anchors: TArray<TFolderId>);
var AFolders: TFolderList;
  AChildFolderId: TFolderId;
  i: integer;
begin
  AFolders := TFolderList.Create;
  try
    for i := 0 to Length(Anchors)-1 do
      if (Anchors[i] <> 0) or (ANode = nil) then //when root is anchored anywhere else, we mean to anchor only files
        Db.Files.GetFolders(Anchors[i], AFolders);
      for AChildFolderId in AFolders.Keys do
        AddFolderNode(ANode, AFolders[AChildFolderId].name, AChildFolderId);
  finally
    FreeAndNil(AFolders);
  end;
end;

//Loads all child file nodes for a given parent node
procedure TFileBrowserForm.LoadFiles(ANode: PVirtualNode; const Anchors: TArray<TFolderId>);
var AFiles: TList<TFileEntryData>;
  i: integer;
begin
  AFiles := TList<TFileEntryData>.Create;
  try
    for i := 0 to Length(Anchors)-1 do
      FDb.Files.GetFiles(Anchors[i], AFiles);
      //Note: We can't produce duplicate files, as every file is parented to exactly one folder.
    for i := 0 to AFiles.Count-1 do
      AddFileNode(ANode, AFiles[i]);
  finally
    FreeAndNil(AFiles);
  end;
end;


//Parses a given path and locates or creates all intermediate folder nodes as required (non-anchored by default).
//Returns the leaf node.
function TFileBrowserForm.ForceFolderPath(AParent: PVirtualNode; AFolderName: string): PVirtualNode;
var idx: integer;
  part: string;
  Node: PVirtualNode;
  Data: PNodeData;
begin
  Result := nil;
  while AFolderName <> '' do begin
    idx := pos('\', AFolderName);
    if idx > 0 then begin
      part := copy(AFolderName, 1, idx-1);
      delete(AFolderName, 1, idx);
    end else begin
      part := AFolderName;
      AFolderName := '';
    end;

    Result := nil;
    for Node in Tree.ChildNodes(AParent) do begin
      Data := Tree.GetNodeData(Node);
      if SameText(Data.v.name, part) then begin
        Result := Node;
        break;
      end;
    end;

    if Result <> nil then
      AParent := Result
    else begin
      Result := inherited AddNode(AParent);
      Data := Tree.GetNodeData(Result);
      Data.NodeType := ntFolder;
      Data.Anchors := nil;
      Data.v.name := part;
      Data.v.folder := 0; //no monofolder assigned
      Data.v.assembly := 0;
      AParent := Result;
    end;
  end;
  //The above continued until we found a leaf folder.
end;

//Adds explicit anchor to the given folder node. Registers it as an explicit anchor.
//Explicit anchors are different from implicit ones which are assigned every auto-created folder
//when loading children
procedure TFileBrowserForm.AddExplicitAnchor(AFolder: PVirtualNode; AFolderId: TFolderId);
var Data: PNodeData;
begin
  Data := Self.GetNodeDataEx(AFolder); //we support adding anchors to root also
  SetLength(Data.Anchors, Length(Data.Anchors)+1);
  Data.Anchors[Length(Data.Anchors)-1] := AFolderId;

  //Store this explicit anchor
  SetLength(ExplicitRoots, Length(ExplicitRoots)+1);
  ExplicitRoots[Length(ExplicitRoots)-1] := AFolder;
end;

//A version of AddExplicitAnchor which parses and locates/creates the path. Returns the leaf node.
function TFileBrowserForm.AddExplicitAnchor(AParent: PVirtualNode; AFolderName: string; AFolderId: TFolderId): PVirtualNode;
begin
  Result := ForceFolderPath(AParent, AFolderName);
  AddExplicitAnchor(Result, AFolderId);
end;

//Adds an auto-created child folder node with a given name, assigns it a given implicit FolderId
function TFileBrowserForm.AddFolderNode(AParent: PVirtualNode; const AFolderName: string; AFolderId: TFolderId): PVirtualNode;
var Node: PVirtualNode;
  AData: PNodeData;
begin
 //Try to find an existing node with the same name.
 //We have to merge nodes which were created explicitly (e.g. by resolving $(AppData) to Users\Public\AppData)
 //and by dynamic loading (e.g. $(Public)\AppData\SomeData).
  Result := nil;
  AData := nil;
  for Node in Tree.ChildNodes(AParent) do begin
    AData := Tree.GetNodeData(Node);
    if (AData.NodeType = ntFolder)
    and SameText(AData.v.name, AFolderName) then begin
      Result := Node;
      break;
    end;
  end;

  if Result = nil then begin
    Result := inherited AddNode(AParent);
    AData := Tree.GetNodeData(Result);
    AData.NodeType := ntFolder;
    AData.v.assembly := 0;
    AData.v.name := AFolderName; //we could call Db.GetFolderName(AFolderId), but why waste cycles
    SetLength(AData.Anchors, 1);
    AData.v.folder := AFolderId; //since it's a single anchor folder, assign explicit id
  end else begin
    SetLength(AData.Anchors, Length(AData.Anchors)+1);
    AData.v.folder := 0;         //it is now a multi-anchor folder
  end;

  AData.Anchors[Length(AData.Anchors)-1] := AFolderId;
end;

function TFileBrowserForm.AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.v := AFileData;
end;

procedure TFileBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TFileBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TFileBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TFileBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  inherited;
  if TextType <> ttNormal then exit;

  AData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0:
      CellText := AData.v.name;
  end;
end;

procedure TFileBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var AData: PNodeData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      case AData.NodeType of
        ntFolder: ImageIndex := imgFolder;
        ntFile: ImageIndex := imgFile;
      end;
    end;
  end;
end;

procedure TFileBrowserForm.TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
var Data: PNodeData;
begin
  inherited;
  if Node = nil then begin
    FileActions.SetSelectedFolders(nil);
    FileActions.SetSelectedFiles(nil);
    PopupMenu := FileActions.FolderPopupMenu;
    exit;
  end;

  Data := Sender.GetNodeData(Node);
  if Data.NodeType = ntFolder then begin
    if Data.v.folder > 0 then
      FileActions.SetSelectedFolder(Data.v.folder)
    else
      FileActions.SetSelectedFolders(nil);
    PopupMenu := FileActions.FolderPopupMenu;
  end else begin
    FileActions.SetSelectedFile(@Data.v);
    PopupMenu := FileActions.FilePopupMenu;
  end;
end;

procedure TFileBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  inherited;
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  Result := integer(AData1.NodeType) - integer(AData2.NodeType);
  if Result = 0 then
    Result := CompareText(AData1.v.name, AData2.v.name);
end;

procedure TFileBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var AData: PNodeData;
  Form: TWinControl;
  AIds: TArray<TAssemblyID>;
  Flid: TFolderId;
  Refs: TFolderReferees;
begin
  inherited;
  Form := Self.ParentForm;
  if Form = nil then exit;

  if Node = nil then begin
    CommonMessages.SetAssemblySelection(Form.Handle, nil);
    exit;
  end;

  AData := Sender.GetNodeData(Node);
  if AData.v.assembly > 0 then begin
    SetLength(AIDs, 1);
    AIDs[0] := AData.v.assembly;
  end else begin
    Refs := TFolderReferees.Create;
    try
      for Flid in AData.Anchors do
        Db.Files.GetFolderReferees(Flid, Refs);
      AIDs := Refs.Keys.ToArray;
    finally
      FreeAndNil(Refs);
    end;
  end;

  CommonMessages.SetAssemblySelection(Form.Handle, AIds);
end;


{
Filtering is complicated since we use delay loading. Outline:
1. We query the database and build a list of all matches.
2. Each match is assigned its "best available node" (deepest loaded node on the path to it)
3. All nodes that are assigned matches are visible.
4. When the user expands the node and delay-loads its children, matches that were assigned to that
  node are reassigned deeper, and matched child nodes are made visible.
Filtering complexity: O(NodeCount+MatchCount) + SearchComplexity
}

procedure TFileBrowserForm.ApplyFilter;
var Node: PVirtualNode;
  ShowAll: boolean;
  i: integer;
begin
 //We ignore common filtes atm and only honor quickfilter

  ReloadMatches;
  ShowAll := FQuickFilterText = '';

  Tree.BeginUpdate;
  try
   //Hide all nodes (or show all if not filtered)
    for Node in Tree.Nodes do
      Tree.IsVisible[Node] := false or ShowAll;

   //Show every matched node and their parents
    for i := 0 to FMatches.Count-1 do begin
      if FMatches[i].top = nil then continue; //no point
      Tree.MakeNodePathVisible(FMatches[i].top);
    end;
  finally
    Tree.EndUpdate;
  end;
end;

//Called after node children are loaded, to upgrade match binding and apply children visibility
procedure TFileBrowserForm.UpdateNodeFilter(ANode: PVirtualNode);
var ShowAll: boolean;
  Node: PVirtualNode;
  i: integer;
begin
  ShowAll := FQuickFilterText = '';

 //Hide all children nodes (or show all if not filtered)
  for Node in Tree.ChildNodes(ANode) do
    Tree.IsVisible[Node] := false or ShowAll;

  for i := 0 to FMatches.Count-1 do
    if FMatches[i].top = ANode then begin
      while TraceOneLevel(FMatches.List[i]) do begin end;
      //Wherever it did end up, make it visible
      Tree.MakeNodePathVisible(FMatches[i].top);
    end;
end;

//Reconstructs the set of all matching files and folders for the current quickfilter text
procedure TFileBrowserForm.ReloadMatches;
var FileList: TFileList;
  FolderList: TFolderList;
  FileData: TFileEntryData;
  FolderData: TFolderData;
  Match: TMatchEntry;
  i: integer;
begin
  FMatches.Clear;
  if FQuickFilterText = '' then exit;

  FileList := TFileList.Create;
  try
    Db.Files.FindFiles(FQuickFilterText, FileList);
    for FileData in FileList do begin
      Match.id := FileData.id;
      Match.type_ := ntFile;
      Match.top := nil;
      Match.remPath := Db.Files.GetFolderPathAsIDs(FileData.folder);
      FMatches.Add(Match);
    end;
  finally
    FreeAndNil(FileList);
  end;

  FolderList := TFolderList.Create;
  try
    Db.Files.FindFolders(FQuickFilterText, FolderList);
    for FolderData in FolderList.Values do begin
      Match.id := FolderData.id;
      Match.type_ := ntFolder;
      Match.top := nil;
      Match.remPath := Db.Files.GetFolderPathAsIDs(FolderData.id);
      FMatches.Add(Match);
    end;
  finally
    FreeAndNil(FolderList);
  end;

  //Trace paths
  for i := 0 to FMatches.Count-1 do
    while TraceOneLevel(FMatches.List[i]) do begin end;
end;

//Tries to resolve a match one level deeper into the tree
function TFileBrowserForm.TraceOneLevel(var Match: TMatchEntry): boolean;
var Node: PVirtualNode;
  Data: PNodeData;
  i: integer;
begin
  if Length(Match.remPath) <= 0 then begin
    Result := false;

    //As a last step, if we are still bound to folder node, we may try to re-bind to file node
    Data := Self.GetNodeDataEx(Match.top);
    if (Match.type_ = ntFile) and (Data.NodeType = ntFolder) then
      for Node in Tree.ChildNodes(Match.top) do begin
        Data := Tree.GetNodeData(Node);
        if (Data.NodeType = ntFile) and (Data.v.id = Match.id) then begin
          Match.top := Node;
          Result := true;
          break;
        end;
      end;

    exit;
  end;

  Result := false;

  //For matches that are bound to nil, also check all explicit roots wherever they are
  if Match.top = nil then
    for Node in ExplicitRoots do begin
      Data := Self.GetNodeDataEx(Node); //support root here
      for i := 0 to Length(Data.Anchors)-1 do
        if Data.Anchors[i] = Match.remPath[0] then begin
          Match.top := Node;
          Result := true;
          break;
        end;
    end;

  //Check all children anyway, because sometimes nil-bound matches are actually bound to root
  //via its anchors, and not just "not bound anywhere yet".
  //It doesn't hurt: false-match is impossible
  if not Result then
    for Node in Tree.ChildNodes(Match.top) do begin
      Data := Tree.GetNodeData(Node);
      for i := 0 to Length(Data.Anchors)-1 do
        if Data.Anchors[i] = Match.remPath[0] then begin
          Match.top := node;
          Result := true;
          break;
        end;
    end;

  if Result then
    Match.remPath.PopRoot;
end;


//Filtering is slow and we don't want to be stuck at each letter as we type,
//so we apply filtering with delay

procedure TFileBrowserForm.WmSetQuickfilter(var msg: TWmSetQuickFilter);
begin
  if FQuickFilterText <> msg.FilterText^ then begin
    FQuickFilterText := msg.FilterText^;
    FilterTimer.Enabled := true;
  end;
end;

procedure TFileBrowserForm.FilterTimerTimer(Sender: TObject);
begin
  ApplyFilter;
  FilterTimer.Enabled := false;
end;

end.
