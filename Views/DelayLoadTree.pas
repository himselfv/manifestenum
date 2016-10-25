unit DelayLoadTree;
{
Delay-load tree base implementation

Nodes have two stages of initialization:
  Add: when they are initially added to store their ID and count towards their parent's ChildCount
  Touch: when they are fully initialized (including querying/adding their own children)

Usage: inherit, implement DelayLoad() to Add() children of the given node.
}

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
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure FormShow(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    procedure SetDb(ADb: TAssemblyDb); virtual;
    procedure Touch(ANode: PVirtualNode);
    procedure TouchChildren(ANode: PVirtualNode);
    function AddNode(AParent: PVirtualNode): PVirtualNode;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); virtual;
  public
    procedure Clear;
    procedure Reload; virtual;
    property Db: TAssemblyDb read FDb write SetDb;
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

  Tree.BeginUpdate;
  try
    Touch(nil);
    TouchChildren(nil);
    if Tree.Header.SortColumn <> NoColumn then
      Tree.SortTree(Tree.Header.SortColumn, Tree.Header.SortDirection);
  finally
    Tree.EndUpdate;
  end;
end;

procedure TDelayLoadTree.FormShow(Sender: TObject);
begin
  if FDb <> nil then
    Reload;
end;

procedure TDelayLoadTree.SetDb(ADb: TAssemblyDb);
begin
  FDb := ADb;
  //The form automatically loads data at first reasonable time (visible + have db)
  if Self.Visible and not Self.Showing then
    if ADb <> nil then
      Reload
    else
      Clear;
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
  Sender.Sort(Node, TVirtualStringTree(Sender).Header.SortColumn, TVirtualStringTree(Sender).Header.SortDirection);
end;

procedure TDelayLoadTree.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end else
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;

procedure TDelayLoadTree.Touch(ANode: PVirtualNode);
var AData: PDelayLoadHeader;
begin
  if ANode <> nil then
    AData := Tree.GetNodeData(ANode)
  else
    AData := nil; //Root is a special case

  if (AData <> nil) and AData.Touched then exit;

  Tree.BeginUpdate;
  try
    DelayLoad(ANode, AData);
    if AData <> nil then
      AData.Touched := true;
  finally
    Tree.EndUpdate;
  end;
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
