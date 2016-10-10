unit RegistryBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, AssemblyDb;

type
  TRegistryKeyNodeData = record
    keyId: TRegistryKeyId;
    keyName: string;
    childrenQueried: boolean;
  end;
  PRegistryKeyNodeData = ^TRegistryKeyNodeData;

  TRegistryBrowserForm = class(TForm)
    Tree: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  protected
    FDb: TAssemblyDb;
    procedure SetDb(ADb: TAssemblyDb);
    procedure CreateSubnodes(AParent: PVirtualNode; AKeyId: TRegistryKeyId);
    procedure TouchSubnodes(AParent: PVirtualNode);
  public
    procedure Clear;
    procedure Reload;
    property Db: TAssemblyDb read FDb write SetDb;
  end;

var
  RegistryBrowserForm: TRegistryBrowserForm;

implementation

{$R *.dfm}

procedure TRegistryBrowserForm.FormShow(Sender: TObject);
begin
  if FDb <> nil then
    Reload;
end;

procedure TRegistryBrowserForm.SetDb(ADb: TAssemblyDb);
begin
  FDb := ADb;
  if Self.Visible and not Self.Showing then
    if ADb <> nil then
      Reload
    else
      Clear;
end;

procedure TRegistryBrowserForm.Clear;
begin
  Tree.Clear;
end;

procedure TRegistryBrowserForm.Reload;
begin
  Clear;
  if FDb = nil then exit;
  CreateSubnodes(nil, 0);
  TouchSubnodes(nil);
end;

procedure TRegistryBrowserForm.CreateSubnodes(AParent: PVirtualNode; AKeyId: TRegistryKeyId);
var AList: TRegistryKeyList;
  ANode: PVirtualNode;
  AData: PRegistryKeyNodeData;
  AKey: TRegistryKeyId;
begin
  AList := TRegistryKeyList.Create;
  try
    FDb.GetRegistryKeys(AKeyId, AList);
    for AKey in AList.Keys do begin
      ANode := Tree.AddChild(AParent);
      Tree.ReinitNode(ANode, false);
      AData := Tree.GetNodeData(ANode);

      AData.keyId := AKey;
      AData.keyName := AList[AKey];
      AData.childrenQueried := false;
    end;
  finally
    FreeAndNil(AList);
  end;

  if AParent <> nil then begin
    AData := Tree.GetNodeData(AParent);
    AData.childrenQueried := true;
  end;
end;

procedure TRegistryBrowserForm.TouchSubnodes(AParent: PVirtualNode);
var ANode: PVirtualNode;
   AData: PRegistryKeyNodeData;
begin
  for ANode in Tree.ChildNodes(AParent) do begin
    AData := Tree.GetNodeData(ANode);
    if not AData.childrenQueried then
      CreateSubnodes(ANode, AData.keyId);
  end;
end;

procedure TRegistryBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TRegistryKeyNodeData);
end;

procedure TRegistryBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
 //
end;

procedure TRegistryBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
 //
end;

procedure TRegistryBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PRegistryKeyNodeData;
begin
  AData := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;

  case Column of
    NoColumn, 0:
      CellText := AData.keyName;
  end;
end;

procedure TRegistryBrowserForm.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
begin
  TouchSubnodes(Node);
end;


end.
