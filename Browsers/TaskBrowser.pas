unit TaskBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, DelayLoadTree, VirtualTrees, AssemblyDb,
  CommonResources, Vcl.StdCtrls;

type
  TNodeType = (ntFolder, ntTask);
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    AssemblyId: TAssemblyId;
    FolderId: TTaskFolderId;
  end;
  PNodeData = ^TNodeData;

  TTaskBrowserForm = class(TDelayLoadTree)
    lblWhoAdded: TLabel;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure FormShow(Sender: TObject);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  protected
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddFolderNode(AParent: PVirtualNode; ATaskFolderId: TTaskFolderId): PVirtualNode;
    function AddTaskNode(AParent: PVirtualNode; ATaskData: TTaskEntryData): PVirtualNode;
  end;

var
  TaskBrowserForm: TTaskBrowserForm;

implementation
uses Generics.Collections;

{$R *.dfm}

procedure TTaskBrowserForm.FormShow(Sender: TObject);
begin
  inherited;
  if FDb <> nil then
    Reload;
end;

procedure TTaskBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AFolderId: TTaskFolderId;
  AFolders: TList<TTaskFolderId>;
  ATasks: TList<TTaskEntryData>;
  i: integer;
begin
  if (AData <> nil) and (AData.NodeType = ntTask) then exit;

  if AData <> nil then
    AFolderId := AData.FolderId
  else
    AFolderId := 0;

  AFolders := TList<TTaskFolderId>.Create;
  try
    FDb.GetTaskFolders(AFolderId, AFolders);
    for i := 0 to AFolders.Count-1 do
      AddFolderNode(ANode, AFolders[i]);
  finally
    FreeAndNil(AFolders);
  end;

  ATasks := TList<TTaskEntryData>.Create;
  try
    FDb.GetTasks(AFolderId, ATasks);
    for i := 0 to ATasks.Count-1 do
      AddTaskNode(ANode, ATasks[i]);
  finally
    FreeAndNil(ATasks);
  end;
end;

function TTaskBrowserForm.AddFolderNode(AParent: PVirtualNode; ATaskFolderId: TTaskFolderId): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFolder;
  AData.FolderId := ATaskFolderId;
  AData.Name := FDb.GetTaskFolderName(ATaskFolderId);
  AData.AssemblyId := 0;
end;

function TTaskBrowserForm.AddTaskNode(AParent: PVirtualNode; ATaskData: TTaskEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntTask;
  AData.Name := ATaskData.name;
  AData.FolderId := ATaskData.folderId;
  AData.AssemblyId := ATaskData.assemblyId;
end;

procedure TTaskBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TTaskBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TTaskBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TTaskBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  inherited;
  if TextType <> ttNormal then exit;

  AData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0:
      CellText := AData.Name;
  end;
end;

procedure TTaskBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
        ntTask: ImageIndex := imgTask;
      end;
    end;
  end;
end;

procedure TTaskBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  inherited;
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  Result := integer(AData1.NodeType) - integer(AData2.NodeType);
  if Result = 0 then
    Result := CompareText(AData1.Name, AData2.Name);
end;

procedure TTaskBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  if AData.AssemblyId > 0 then
    lblWhoAdded.Caption := 'Component: '+FDb.GetAssembly(AData.AssemblyId).identity.ToString
  else
    lblWhoAdded.Caption := '';
end;


end.
