unit AssemblyResourcesView;

//Shows various resources associated with a single assembly as a list.
//Currently supports:
// - Directories
// - Files
// - Registry keys
// - Tasks
//Planned:
// - Services
// - Event queues
// - Event types

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, DelayLoadTree, VirtualTrees, AssemblyDb;

type
  TNodeType = (
    ntAssembly,
    ntDirectory,
    ntFile,
    ntRegistryValue,
    ntTask
  );
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    AssemblyId: TAssemblyId;
  end;
  PNodeData = ^TNodeData;

  TAssemblyResourcesForm = class(TDelayLoadTree)
    NodeImages: TImageList;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  protected
    FAssembly: TAssemblyId;
    FShowDependencies: boolean;
    procedure SetAssembly(const AValue: TAssemblyId);
    procedure SetShowDependencies(const AValue: boolean);
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddDirectoryNode(AParent: PVirtualNode; const ADirectoryData: TDirectoryEntryData): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
    function AddRegistryValueNode(AParent: PVirtualNode; const ARegistryValueData: TRegistryValueData): PVirtualNode;
    function AddTaskNode(AParent: PVirtualNode; const ATaskData: TTaskEntryData): PVirtualNode;
    function AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
  public
    property Assembly: TAssemblyId read FAssembly write SetAssembly;
    property ShowDependencies: boolean read FShowDependencies write SetShowDependencies;
  end;

var
  AssemblyResourcesForm: TAssemblyResourcesForm;

implementation
uses Generics.Collections;

{$R *.dfm}

procedure TAssemblyResourcesForm.SetAssembly(const AValue: TAssemblyId);
begin
  if FAssembly <> AValue then begin
    FAssembly := AValue;
    Reload;
  end;
end;

procedure TAssemblyResourcesForm.SetShowDependencies(const AValue: boolean);
begin
  if FShowDependencies <> AValue then begin
    FShowDependencies := AValue;
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
        ntDirectory: CellText := 'Dir';
        ntFile: CellText := 'File';
        ntRegistryValue: CellText := 'Key';
        ntTask: CellText := 'Task';
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
      ImageList := Self.NodeImages;
      case Adata.NodeType of
        ntAssembly: ImageIndex := 4;
        ntDirectory: ImageIndex := 0;
        ntFile: ImageIndex := 1;
        ntRegistryValue: ImageIndex := 2;
        ntTask: ImageIndex := 3;
      end;
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
    3   //task
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
  ADirectories: TList<TDirectoryEntryData>;
  AFiles: TList<TFileEntryData>;
  ARegistryValues: TList<TRegistryValueData>;
  ATasks: TList<TTaskEntryData>;
  AAssemblies: TAssemblyList;
  AAssemblyData: TAssemblyData;
  i: integer;
begin
  if (ANode <> nil) and (AData.NodeType = ntAssembly) then
    AAssemblyId := AData.AssemblyId
  else
    AAssemblyId := FAssembly;
  if AAssemblyId = 0 then exit; //no assembly set or non-assembly node

  ADirectories := TList<TDirectoryEntryData>.Create;
  try
    FDb.GetAssemblyDirectories(AAssemblyId, ADirectories);
    for i := 0 to ADirectories.Count-1 do
      AddDirectoryNode(ANode, ADirectories[i]);
  finally
    FreeAndNil(ADirectories);
  end;

  AFiles := TList<TFileEntryData>.Create;
  try
    FDb.GetAssemblyFiles(AAssemblyId, AFiles);
    for i := 0 to AFiles.Count-1 do
      AddFileNode(ANode, AFiles[i]);
  finally
    FreeAndNil(AFiles);
  end;

  ARegistryValues := TList<TRegistryValueData>.Create;
  try
    FDb.GetAssemblyKeys(AAssemblyId, ARegistryValues);
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

function TAssemblyResourcesForm.AddDirectoryNode(AParent: PVirtualNode; const ADirectoryData: TDirectoryEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntDirectory;
  AData.Name := ADirectoryData.destinationPath;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.Name := AFileData.fullDestinationName;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyResourcesForm.AddRegistryValueNode(AParent: PVirtualNode; const ARegistryValueData: TRegistryValueData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntRegistryValue;
  AData.Name := FDb.GetRegistryKeyPath(ARegistryValueData.key) + '\' + ARegistryValueData.name;
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

function TAssemblyResourcesForm.AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntAssembly;
  AData.Name := AAssemblyData.identity.ToString;
  AData.AssemblyId := AAssemblyData.id;
end;

end.
