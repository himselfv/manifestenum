unit FileBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, DelayLoadTree, VirtualTrees, AssemblyDb,
  CommonResources, Vcl.StdCtrls, AssemblyDb.Assemblies;

type
  TNodeType = (ntFolder, ntFile);
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    AssemblyId: TAssemblyId;
    FolderId: TFolderId;
  end;
  PNodeData = ^TNodeData;

  TFileBrowserForm = class(TDelayLoadTree)
    lblWhoAdded: TLabel;
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
  protected
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddFolderNode(AParent: PVirtualNode; AFolderId: TFolderId; AFolderName: string): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
  end;

var
  FileBrowserForm: TFileBrowserForm;

implementation
uses Generics.Collections;

{$R *.dfm}

procedure TFileBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AFolderId: TFolderId;
  AFolders: TFolderList;
  AChildFolderId: TFolderId;
  AFiles: TList<TFileEntryData>;
  i: integer;
begin
  if (AData <> nil) and (AData.NodeType = ntFile) then exit;

  if AData <> nil then
    AFolderId := AData.FolderId
  else
    AFolderId := 0;

  AFolders := TFolderList.Create;
  try
    FDb.GetFolders(AFolderId, AFolders);
    for AChildFolderId in AFolders.Keys do
      AddFolderNode(ANode, AChildFolderId, AFolders[AChildFolderId]);
  finally
    FreeAndNil(AFolders);
  end;

  AFiles := TList<TFileEntryData>.Create;
  try
    FDb.GetFiles(AFolderId, AFiles);
    for i := 0 to AFiles.Count-1 do
      AddFileNode(ANode, AFiles[i]);
  finally
    FreeAndNil(AFiles);
  end;
end;

function TFileBrowserForm.AddFolderNode(AParent: PVirtualNode; AFolderId: TFolderId; AFolderName: string): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFolder;
  AData.FolderId := AFolderId;
  AData.Name := AFolderName; //we could call Db.GetFolderName(AFolderId), but why waste cycles
  AData.AssemblyId := 0;
end;

function TFileBrowserForm.AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.Name := AFileData.name;
  AData.FolderId := AFileData.folder;
  AData.AssemblyId := AFileData.assembly;
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
      CellText := AData.Name;
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

procedure TFileBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
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

procedure TFileBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  if AData.AssemblyId > 0 then
    lblWhoAdded.Caption := 'Component: '+FDb.Assemblies.GetAssembly(AData.AssemblyId).identity.ToString
  else
    lblWhoAdded.Caption := '';
end;

end.
