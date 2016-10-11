unit AssemblyTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DelayLoadTree, VirtualTrees, AssemblyDb;

type
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    Name: string;
    Assembly: TAssemblyId;
  end;
  PNodeData = ^TNodeData;

  TAssemblyTreeForm = class(TDelayLoadTree)
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  protected
    FRootAssembly: TAssemblyId;
    procedure SetRootAssembly(const AValue: TAssemblyId);
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function AddNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode; reintroduce;
  public
    property RootAssembly: TAssemblyId read FRootAssembly write FRootAssembly;
  end;

var
  AssemblyTreeForm: TAssemblyTreeForm;

implementation

{$R *.dfm}

procedure TAssemblyTreeForm.SetRootAssembly(const AValue: TAssemblyId);
begin
  if FRootAssembly <> AValue then begin
    FRootAssembly := AValue;
    Reload;
  end;
end;

procedure TAssemblyTreeForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TAssemblyTreeForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TAssemblyTreeForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TAssemblyTreeForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  inherited;
  if TextType <> ttNormal then exit;
  AData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0: CellText := AData.Name;
  end;
end;

procedure TAssemblyTreeForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  inherited;
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  case Column of
    NoColumn, 0:
      Result := CompareText(AData1.Name, AData2.Name);
  end;
end;

procedure TAssemblyTreeForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AAssembly: TAssemblyId;
  ADependencies: TAssemblyList;
  ADependencyData: TAssemblyData;
begin
  if AData <> nil then
    AAssembly := AData.Assembly
  else
    AAssembly := Self.RootAssembly;

  //Add dependencies
  ADependencies := TAssemblyList.Create;
  try
    if AAssembly > 0 then
      FDb.GetDependencies(AAssembly, ADependencies)
    else
      FDb.GetAllAssemblies(ADependencies);
    for ADependencyData in ADependencies.Values do
      AddNode(ANode, ADependencyData);
  finally
    FreeAndNil(ADependencies);
  end;
end;

function TAssemblyTreeForm.AddNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.Name := AAssemblyData.identity.name;
  AData.Assembly := AAssemblyData.id;
end;



end.
