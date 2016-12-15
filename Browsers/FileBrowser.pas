unit FileBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, StdCtrls, Menus, Vcl.ExtCtrls, DelayLoadTree,
  VirtualTrees, AssemblyDb, CommonResources, AssemblyDb.Assemblies, AssemblyDb.Files, AssemblyDb.Environ;

type
  TNodeType = (ntFolder, ntFile);

  //Depending on a view mode, a node may host content from several database folders.
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Anchors: TArray<TFolderId>;    // to show multiple DB folders merged
    v: TFileEntryData;
  end;
  PNodeData = ^TNodeData;

  TFileBrowserForm = class(TDelayLoadTree)
    lblWhoAdded: TLabel;
    Panel: TPanel;
    cbViewMode: TComboBox;
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
    procedure FormCreate(Sender: TObject);
  protected
    RootAnchors: TArray<TFolderId>;
    modelEnv: TEnvironment;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function ExpandPath(const APath: string): string;
    procedure AnchorRootFolders(ANode: PVirtualNode);
    procedure LoadFolders(ANode: PVirtualNode; Anchors: TArray<TFolderId>);
    procedure LoadFiles(ANode: PVirtualNode; const Anchors: TArray<TFolderId>);

    function AddFolderNode(AParent: PVirtualNode; const AFolderName: string; AFolderId: TFolderId): PVirtualNode;
    function AddFolderAnchor(AParent: PVirtualNode; AFolderName: string; AFolderId: TFolderId): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
  end;

var
  FileBrowserForm: TFileBrowserForm;

implementation
uses Generics.Collections, ManifestEnum.FileActions;

{$R *.dfm}

{$DEFINE ROOTC}

procedure TFileBrowserForm.FormCreate(Sender: TObject);
begin
  inherited;
  //Prepare environment model
  //At the moment, we're lumping together x64 and wow64 assemblies. It would be too costly to query
  //every assembly for processorArchitecture.
  modelEnv := GetModelEnvironmentBlock('x64');
end;

procedure TFileBrowserForm.cbViewModeChange(Sender: TObject);
begin
  inherited;
  Reload;
end;

procedure TFileBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  Anchors: TArray<TFolderId>;
begin
  if (AData <> nil) and (AData.NodeType = ntFile) then exit;

 {
  We'll have to do something strange with root files.

  Files with are parented to folderId==0 are NOT root files. They are files that had no path in the
  manifest. These files should go to one of a few special folders:
    x64/x32  = System32
    msil     = somewhere in .net
    driverUpdates = see below

  But after we've parsed the root FOLDERS and resolved their model paths, we might have assigned
  the root node a number of anchors.

  Files from these anchors SHOULD be displayed, as these are files from say, $(SystemDrive).

  So we add an anchor for actual zero-parented files, and then ignore those and process anchors for root.
 }
  if AData = nil then begin
    AddFolderAnchor(nil, ExpandPath('$(runtime.System32)'), 0);
    RootAnchors := nil; //If we're reloading root, reset root anchors
    if cbViewMode.ItemIndex = 1 then
      AnchorRootFolders(ANode)
    else begin
     //Just anchor root
      SetLength(RootAnchors, 1);
      RootAnchors[0] := 0;
    end;
  end;

  //Load files and folders in the normal way
  if AData = nil then
    Anchors := RootAnchors
  else
    Anchors := AData.Anchors;
  LoadFolders(ANode, Anchors);
  LoadFiles(ANode, Anchors);
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
      AddFolderAnchor(ANode, ExpandPath(AFolders[AChildFolderId]), AChildFolderId);
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
        AddFolderNode(ANode, AFolders[AChildFolderId], AChildFolderId);
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


//Adds a child node with a given name, assigns it a given FolderId
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

//Parses AFolderPath. Finds or adds the folder node (adding parent nodes as required) and assigns it
//additional anchor to a given folderId
function TFileBrowserForm.AddFolderAnchor(AParent: PVirtualNode; AFolderName: string; AFolderId: TFolderId): PVirtualNode;
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
      Data.v.folder := 0; //no explicit folder assigned
      Data.v.assembly := 0;
      AParent := Result;
    end;
  end;

  //The above continued until we found a leaf folder
  //Add an anchor to it
  if Result <> nil then begin
    Data := Tree.GetNodeData(Result);
    SetLength(Data.Anchors, Length(Data.Anchors)+1);
    Data.Anchors[Length(Data.Anchors)-1] := AFolderId;
  end else begin
    SetLength(RootAnchors, Length(RootAnchors)+1);
    RootAnchors[Length(RootAnchors)-1] := AFolderId;
  end;

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
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  if AData.v.assembly > 0 then
    lblWhoAdded.Caption := 'Component: '+FDb.Assemblies.GetAssembly(AData.v.assembly).identity.ToString
  else
    lblWhoAdded.Caption := '';
end;

end.
