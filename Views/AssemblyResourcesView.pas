unit AssemblyResourcesView;

//Shows various resources associated with a single assembly as a list.
//Currently supports:
// - Directories
// - Files
// - Registry keys
// - Tasks
// - Services
//Planned:
// - Event queues
// - Event types

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.Menus, DelayLoadTree, VirtualTrees, AssemblyDb,
  CommonResources, Generics.Collections, AssemblyDb.Assemblies, AssemblyDb.Files, AssemblyDb.Registry,
  AssemblyDb.Services;

type
  TNodeType = (
    ntAssembly,
    ntFolder,
    ntFile,
    ntRegistryValue,
    ntTask,
    ntService
  );
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    AssemblyId: TAssemblyId;
    ResourceId: int64;
  end;
  PNodeData = ^TNodeData;

  TAssemblyResourcesForm = class(TDelayLoadTree)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  protected
    FAssemblies: TList<TAssemblyId>;
    FShowDependencies: boolean;
    FFlatTree: boolean;
    procedure SetShowDependencies(const AValue: boolean);
    procedure SetFlatTree(const AValue: boolean);
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddFolderNode(AParent: PVirtualNode; AFolder: TFolderId; const AFolderData: TFolderReferenceData): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
    function AddRegistryValueNode(AParent: PVirtualNode; const ARegistryValueData: TRegistryValueData): PVirtualNode;
    function AddTaskNode(AParent: PVirtualNode; const ATaskData: TTaskEntryData): PVirtualNode;
    function AddServiceNode(AParent: PVirtualNode; const AServiceData: TServiceEntryData): PVirtualNode;
    function AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
  public
    property Assemblies: TList<TAssemblyId> read FAssemblies;
    property ShowDependencies: boolean read FShowDependencies write SetShowDependencies;
    property FlatTree: boolean read FFlatTree write SetFlatTree;
  end;

var
  AssemblyResourcesForm: TAssemblyResourcesForm;

implementation
uses ManifestEnum.RegistryActions, ManifestEnum.FileActions;

{$R *.dfm}

procedure TAssemblyResourcesForm.FormCreate(Sender: TObject);
begin
  inherited;
  FAssemblies := TList<TAssemblyId>.Create();
end;

procedure TAssemblyResourcesForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAssemblies);
  inherited;
end;

procedure TAssemblyResourcesForm.SetShowDependencies(const AValue: boolean);
begin
  if FShowDependencies <> AValue then begin
    FShowDependencies := AValue;
    Reload;
  end;
end;

procedure TAssemblyResourcesForm.SetFlatTree(const AValue: boolean);
begin
  if FFlatTree <> AValue then begin
    FFlatTree := AValue;
    Reload;
  end;
end;

procedure TAssemblyResourcesForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TAssemblyResourcesForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TAssemblyResourcesForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TAssemblyResourcesForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  inherited;
  if TextType <> ttNormal then exit;
  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0:
      CellText := AData.Name;
    1:
      case AData.NodeType of
        ntAssembly: CellText := 'Assembly';
        ntFolder: CellText := 'Dir';
        ntFile: CellText := 'File';
        ntRegistryValue: CellText := 'Key';
        ntTask: CellText := 'Task';
        ntService: CellText := 'Service';
      else CellText := '';
      end;
  end;
end;

procedure TAssemblyResourcesForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var AData: PNodeData;
begin
  inherited;
  if not (Kind in [ikNormal, ikSelected]) then exit;

  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      case AData.NodeType of
        ntAssembly: ImageIndex := imgAssembly;
        ntFolder: ImageIndex := imgFolder;
        ntFile: ImageIndex := imgFile;
        ntRegistryValue: ImageIndex := imgRegistryValue;
        ntTask: ImageIndex := imgTask;
        ntService: ImageIndex := imgService;
      end;
    end;
  end;
end;

procedure TAssemblyResourcesForm.TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
var AData: PNodeData;
begin
  inherited;
  if Node = nil then exit;

  AData := Sender.GetNodeData(Node);
  case AData.NodeType of
    ntFolder: begin
      FileActions.SetSelectedFiles(nil);
      FileActions.SetSelectedFolder(TFolderId(AData.ResourceId));
      PopupMenu := FileActions.FolderPopupMenu;
    end;
    ntFile: begin
      FileActions.SetSelectedFolders(nil);
      FileActions.SetSelectedFile(TFileEntryId(AData.ResourceId));
      PopupMenu := FileActions.FilePopupMenu;
    end;
    ntRegistryValue: begin
      RegistryActions.SetSelectedValue(TRegistryValueId(AData.ResourceId));
      PopupMenu := RegistryActions.ValuePopupMenu;
    end;
  end;
end;

procedure TAssemblyResourcesForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
const
  NodeTypeWeights: array[TNodeType] of integer = (
    10, //assembly
    1,  //dir
    1,  //file
    2,  //regkey
    3,  //task
    4   //service
  );
var AData1, AData2: PNodeData;
begin
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  Result := NodeTypeWeights[AData1.NodeType] - NodeTypeWeights[AData2.NodeType];
  if Result <> 0 then exit;

  Result := CompareText(AData1.Name, AData2.Name);
end;

procedure TAssemblyResourcesForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AAssemblyId: TAssemblyId;
  AFolders: TFolderReferences;
  AFiles: TList<TFileEntryData>;
  ARegistryValues: TList<TRegistryValueData>;
  ATasks: TList<TTaskEntryData>;
  AServices: TServiceList;
  AServiceId: TServiceId;
  AAssemblies: TAssemblyList;
  AAssemblyData: TAssemblyData;
  AFolder: TFolderId;
  i: integer;
begin
  if ANode = nil then begin //Root node
    if FAssemblies.Count <= 0 then exit; //nothing

    //Multiple root assemblies => add them
    if FAssemblies.Count > 1 then begin
      for i := 0 to FAssemblies.Count-1 do begin
        AAssemblyData := FDb.Assemblies.GetAssembly(FAssemblies[i]);
        Self.Touch(Self.AddAssemblyNode(nil, AAssemblyData));
      end;
      exit;
    end;

    //Single assembly => assume it's root
    AAssemblyId := FAssemblies[0];
  end else
   //Not a root node
    if AData.NodeType = ntAssembly then
      AAssemblyId := AData.AssemblyId
    else
      AAssemblyId := 0;
  if AAssemblyId = 0 then exit; //no assembly set or non-assembly node

  if FFlatTree then
    ANode := nil; //create all children under root node

  AFolders := TFolderReferences.Create;
  try
    FDb.Files.GetAssemblyFolders(AAssemblyId, AFolders);
    for AFolder in AFolders.Keys do
      AddFolderNode(ANode, AFolder, AFolders[AFolder]);
  finally
    FreeAndNil(AFolders);
  end;

  AFiles := TList<TFileEntryData>.Create;
  try
    FDb.Files.GetAssemblyFiles(AAssemblyId, AFiles);
    for i := 0 to AFiles.Count-1 do
      AddFileNode(ANode, AFiles[i]);
  finally
    FreeAndNil(AFiles);
  end;

  ARegistryValues := TList<TRegistryValueData>.Create;
  try
    FDb.Registry.GetAssemblyValues(AAssemblyId, ARegistryValues);
    for i := 0 to ARegistryValues.Count-1 do
      AddRegistryValueNode(ANode, ARegistryValues[i]);
  finally
    FreeAndNil(ARegistryValues);
  end;

  ATasks := TList<TTaskEntryData>.Create;
  try
    FDb.GetAssemblyTasks(AAssemblyId, ATasks);
    for i := 0 to ATasks.Count-1 do
      AddTaskNode(ANode, ATasks[i]);
  finally
    FreeAndNil(ATasks);
  end;

  AServices := TServiceList.Create;
  try
    FDb.Services.GetAssemblyServices(AAssemblyId, AServices);
    for AServiceId in AServices.Keys do
      AddServiceNode(ANode, AServices[AServiceId]);
  finally
    FreeAndNil(ATasks);
  end;

  if FShowDependencies then begin
    AAssemblies := TAssemblyList.Create;
    try
      FDb.GetDependencies(AAssemblyId, AAssemblies);
      for AAssemblyData in AAssemblies.Values do
        AddAssemblyNode(ANode, AAssemblyData);
    finally
      FreeAndNil(AAssemblies);
    end;
  end;
end;

function TAssemblyResourcesForm.AddFolderNode(AParent: PVirtualNode; AFolder: TFolderId; const AFolderData: TFolderReferenceData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFolder;
  AData.Name := FDb.Files.GetFolderPath(AFolder);
  AData.ResourceId := AFolder;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.Name := FDb.Files.GetFileFullDestinationName(AFileData);
  AData.ResourceId := AFileData.id;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddRegistryValueNode(AParent: PVirtualNode; const ARegistryValueData: TRegistryValueData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntRegistryValue;
  AData.Name := FDb.Registry.GetKeyPath(ARegistryValueData.key) + '\' + ARegistryValueData.name;
  AData.ResourceId := ARegistryValueData.id;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddTaskNode(AParent: PVirtualNode; const ATaskData: TTaskEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntTask;
  AData.Name := FDb.GetTaskFolderPath(ATaskData.folderId) + '\' + ATaskData.name;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddServiceNode(AParent: PVirtualNode; const AServiceData: TServiceEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntService;
  AData.Name := AServiceData.name;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntAssembly;
  AData.Name := AAssemblyData.identity.ToString;
  AData.AssemblyId := AAssemblyData.id;
  if Self.FlatTree then
    Touch(Result);
end;

end.
