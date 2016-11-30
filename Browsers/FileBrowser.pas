unit FileBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, StdCtrls, Menus, DelayLoadTree, VirtualTrees,
  AssemblyDb, CommonResources, AssemblyDb.Assemblies;

type
  TNodeType = (ntFolder, ntFile);
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    v: TFileEntryData;
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
    procedure TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
  protected
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddFolderNode(AParent: PVirtualNode; AFolderId: TFolderId; AFolderName: string): PVirtualNode;
    function AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
  end;

var
  FileBrowserForm: TFileBrowserForm;

implementation
uses Generics.Collections, ManifestEnum.FileActions;

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
    AFolderId := AData.v.folder
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
  AData.v.folder := AFolderId;
  AData.v.name := AFolderName; //we could call Db.GetFolderName(AFolderId), but why waste cycles
  AData.v.assembly := 0;
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
