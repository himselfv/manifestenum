unit DelayLoadTree;

//Delay-load tree base implementation

//Nodes have two stages of initialization:
//  Add: when they are initially added to store their ID and count towards their parent's ChildCount
//  Touch: when they are fully initialized (including querying/adding their own children)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections, VirtualTrees, AssemblyDb;

type
 //Nodes of inherited forms must start with this
  TDelayLoadHeader = record
    Touched: boolean;
  end;
  PDelayLoadHeader = ^TDelayLoadHeader;

  TDelayLoadTree = class(TForm)
    Tree: TVirtualStringTree;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  protected
    FDb: TAssemblyDb;
    procedure Touch(ANode: PVirtualNode);
    procedure TouchChildren(ANode: PVirtualNode);
    function AddNode(AParent: PVirtualNode): PVirtualNode;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); virtual;
  public
    procedure Clear;
    procedure Reload;
    property Db: TAssemblyDb read FDb write FDb;
  end;

implementation

{$R *.dfm}

procedure TDelayLoadTree.Clear;
begin
  Tree.Clear;
end;

procedure TDelayLoadTree.Reload;
begin
  Clear;
  if FDb = nil then
    exit;

  Touch(nil);
  TouchChildren(nil);
end;

procedure TDelayLoadTree.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDelayLoadHeader);
end;

procedure TDelayLoadTree.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
begin
  TouchChildren(Node);
end;

procedure TDelayLoadTree.Touch(ANode: PVirtualNode);
var AData: PDelayLoadHeader;
begin
  if ANode <> nil then
    AData := Tree.GetNodeData(ANode)
  else
    AData := nil; //Root is a special case

  if (AData <> nil) and AData.Touched then exit;

  DelayLoad(ANode, AData);

  if AData <> nil then
    AData.Touched := true;
end;

procedure TDelayLoadTree.TouchChildren(ANode: PVirtualNode);
var AChild: PVirtualNode;
begin
  for AChild in Tree.ChildNodes(ANode) do
    Touch(AChild);
end;

function TDelayLoadTree.AddNode(AParent: PVirtualNode): PVirtualNode;
var AData: PDelayLoadHeader;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  AData := Tree.GetNodeData(Result);
  AData.Touched := false;
end;

procedure TDelayLoadTree.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
begin
//Override to do delayed loading
end;

end.
