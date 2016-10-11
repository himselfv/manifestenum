unit AssemblyFilesView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections, VirtualTrees, AssemblyDb;

type
  TNodeType = (ntAssembly, ntFile);
  TNodeData = record
    NodeType: TNodeType;
    Name: string;
    Touched: boolean;
    Assembly: TAssemblyId;
  end;
  PNodeData = ^TNodeData;

  TAssemblyFilesForm = class(TForm)
    Tree: TVirtualStringTree;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  protected
    FDb: TAssemblyDb;
    FAssembly: TAssemblyId;
    FFollowDependencies: boolean;
    procedure SetAssembly(const Value: TAssemblyId);
    procedure SetFollowDependencies(const Value: boolean);
    procedure Touch(ANode: PVirtualNode);
    procedure TouchChildren(ANode: PVirtualNode);
    function AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
    function AddAssemblyNode(AParent: PVirtualNode; AAssemblyData: TAssemblyData): PVirtualNode;
  public
    procedure Clear;
    procedure Reload;
    property Db: TAssemblyDb read FDb write FDb;
    property Assembly: TAssemblyId read FAssembly write SetAssembly;
    property FollowDependencies: boolean read FFollowDependencies write SetFollowDependencies;
  end;

var
  AssemblyFilesForm: TAssemblyFilesForm;

implementation

{$R *.dfm}

procedure TAssemblyFilesForm.SetAssembly(const Value: TAssemblyId);
begin
  if FAssembly <> Value then begin
    FAssembly := Value;
    Reload;
  end;
end;

procedure TAssemblyFilesForm.SetFollowDependencies(const Value: boolean);
begin
  if FFollowDependencies <> Value then begin
    FFollowDependencies := Value;
    Reload;
  end;
end;

procedure TAssemblyFilesForm.Clear;
begin
  Tree.Clear;
end;

procedure TAssemblyFilesForm.Reload;
begin
  Clear;
  if (FDb = nil) or (FAssembly <= 0) then
    exit;

  Touch(nil);
  TouchChildren(nil);
end;

procedure TAssemblyFilesForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TAssemblyFilesForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TAssemblyFilesForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TAssemblyFilesForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  if TextType <> ttNormal then exit;
  AData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0: CellText := AData.Name;
  end;
end;

procedure TAssemblyFilesForm.TreeExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var Allowed: Boolean);
begin
  TouchChildren(Node);
end;

//Nodes have two stages of initialization:
//  Add: when they are initially added to store their ID and count towards their parent's ChildCount
//  Touch: when they are fully initialized (including querying/adding their own children)

procedure TAssemblyFilesForm.Touch(ANode: PVirtualNode);
var AData: PNodeData;
  AAssembly: TAssemblyId;
  AFiles: TList<TFileEntryData>;
  ADependencies: TAssemblyList;
  ADependencyData: TAssemblyData;
  i: integer;
begin
  if ANode <> nil then
    AData := Tree.GetNodeData(ANode)
  else
    AData := nil; //Root is a special case

  if AData <> nil then begin
    if AData.Touched then exit;
    if AData.NodeType <> ntAssembly then
      exit; //Files don't have children
    AAssembly := AData.Assembly;
  end else
    AAssembly := Self.Assembly; //Root assembly

  //Add files
  AFiles := TList<TFileEntryData>.Create;
  try
    FDb.GetAssemblyFiles(AAssembly, AFiles);
    for i := 0 to AFiles.Count-1 do
      AddFileNode(ANode, AFiles[i]);
  finally
    FreeAndNil(AFiles);
  end;

  //Add dependencies
  if Self.FollowDependencies then begin
    ADependencies := TAssemblyList.Create;
    try
      FDb.GetDependencies(AAssembly, ADependencies);
      for ADependencyData in ADependencies.Values do
        AddAssemblyNode(ANode, ADependencyData);
    finally
      FreeAndNil(ADependencies);
    end;
  end;

  if AData <> nil then
    AData.Touched := true;
end;

procedure TAssemblyFilesForm.TouchChildren(ANode: PVirtualNode);
var AChild: PVirtualNode;
begin
  for AChild in Tree.ChildNodes(ANode) do
    Touch(AChild);
end;

function TAssemblyFilesForm.AddFileNode(AParent: PVirtualNode; AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.Name := AFileData.fullDestinationName;
  AData.Touched := true;
  AData.Assembly := 0;
end;

function TAssemblyFilesForm.AddAssemblyNode(AParent: PVirtualNode; AAssemblyData: TAssemblyData): PVirtualNode;
var AData: PNodeData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntAssembly;
  AData.Name := AAssemblyData.identity.name;
  AData.Touched := false;
  AData.Assembly := AAssemblyData.id;
end;

end.
