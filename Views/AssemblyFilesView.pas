unit AssemblyFilesView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Generics.Collections, VirtualTrees, AssemblyDb,
  DelayLoadTree;

type
  TNodeType = (ntAssembly, ntFile);
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    Assembly: TAssemblyId;
  end;
  PNodeData = ^TNodeData;

  TAssemblyFilesForm = class(TDelayLoadTree)
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
  protected
    FAssembly: TAssemblyId;
    FFollowDependencies: boolean;
    procedure SetAssembly(const Value: TAssemblyId);
    procedure SetFollowDependencies(const Value: boolean);
    function AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
    function AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
  public
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

procedure TAssemblyFilesForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  inherited;
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  case Column of
    NoColumn, 0:
      if (AData1.NodeType = ntAssembly) and (AData2.NodeType = ntFile) then
        Result := -1
      else
      if (AData1.NodeType = ntFile) and (AData2.NodeType = ntAssembly) then
        Result := +1
      else
        Result := CompareText(AData1.Name, AData2.Name);
  end;
end;

function TAssemblyFilesForm.AddFileNode(AParent: PVirtualNode; const AFileData: TFileEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntFile;
  AData.Name := AFileData.fullDestinationName;
  AData.Assembly := 0;
  AData.DelayLoad.Touched := true;
end;

function TAssemblyFilesForm.AddAssemblyNode(AParent: PVirtualNode; const AAssemblyData: TAssemblyData): PVirtualNode;
var AData: PNodeData;
begin
  Result := AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntAssembly;
  AData.Name := AAssemblyData.identity.ToString;
  AData.Assembly := AAssemblyData.id;
end;

procedure TAssemblyFilesForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AAssembly: TAssemblyId;
  AFiles: TList<TFileEntryData>;
  ADependencies: TAssemblyList;
  ADependencyData: TAssemblyData;
  i: integer;
begin
  if AData <> nil then begin
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
end;

end.
