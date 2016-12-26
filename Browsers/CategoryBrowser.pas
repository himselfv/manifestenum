unit CategoryBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, VirtualTrees, CommonResources, AssemblyDb,
  AssemblyDb.Assemblies;

type
  TNodeType = (ntCategory, ntAssembly);
  TNodeData = record
    NodeType: TNodeType;
    Name: string;
    TypeName: string;
    Assembly: TAssemblyId;
  end;
  PNodeData = ^TNodeData;

  TCategoryBrowserForm = class(TForm)
    Tree: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
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
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  protected
    FDb: TAssemblyDb;
    procedure SetDb(AValue: TAssemblyDb);
    function AddCategoryNode(AParent: PVirtualNode; const AName: string): PVirtualNode;
    function AddAssemblyNode(AParent: PVirtualNode; const AAsmData: TAssemblyData; const ATypeName: string): PVirtualNode;
    function NeedCategoryNode(const AName: string): PVirtualNode;
    procedure FindCategoryNodeCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      var Abort: Boolean);
  public
    procedure Reload;
    property Db: TAssemblyDb read FDb write SetDb;
  end;

var
  CategoryBrowserForm: TCategoryBrowserForm;

implementation
uses CommonMessages;

{$R *.dfm}

procedure TCategoryBrowserForm.FormShow(Sender: TObject);
begin
  if FDb <> nil then
    Reload;
end;

procedure TCategoryBrowserForm.SetDb(AValue: TAssemblyDb);
begin
  if AValue <> FDb then begin
    FDb := AValue;
    if Self.Visible and not Self.Showing then
      Reload;
  end;
end;

procedure TCategoryBrowserForm.Reload;
var AList: TCategoryMemberships;
  AId: TAssemblyId;
  AData: TCategoryMembershipData;
  ACatNode: PVirtualNode;
begin
  Tree.Clear;
  if FDb = nil then exit;

  AList := nil;
  Tree.BeginUpdate;
  try
    AList := TCategoryMemberships.Create;
    FDb.GetCategoryMemberships(AList);

    //Reloading is slow, GetAssembly for every item is the slowest part
    for AId in AList.Keys do begin
      AData := AList[AId];
      ACatNode := NeedCategoryNode(AData.name);
      AddAssemblyNode(ACatNode, FDb.Assemblies.GetAssembly(AId), AData.typeName)
    end;
  finally
    FreeAndNil(AList);
    Tree.EndUpdate;
  end;

  Tree.SortTree(Tree.Header.SortColumn, Tree.Header.SortDirection);
end;

function TCategoryBrowserForm.AddCategoryNode(AParent: PVirtualNode; const AName: string): PVirtualNode;
var AData: PNodeData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntCategory;
  AData.Name := AName;
  AData.Assembly := 0;
end;

function TCategoryBrowserForm.AddAssemblyNode(AParent: PVirtualNode; const AAsmData: TAssemblyData;
  const ATypeName: string): PVirtualNode;
var AData: PNodeData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntAssembly;
  AData.Name := AAsmData.identity.ToString;
  AData.TypeName := ATypeName;
  AData.Assembly := AAsmData.id;
end;

function TCategoryBrowserForm.NeedCategoryNode(const AName: string): PVirtualNode;
begin
  Result := Tree.IterateSubtree(nil, FindCategoryNodeCallback, @AName);
  if Result = nil then
    Result := AddCategoryNode(nil, AName);
end;

procedure TCategoryBrowserForm.FindCategoryNodeCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var ANodeData: PNodeData;
begin
  ANodeData := Tree.GetNodeData(Node);
  if (ANodeData.NodeType = ntCategory) and SameText(ANodeData.Name, PString(Data)^) then
    Abort := true;
end;

procedure TCategoryBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := sizeof(TNodeData);
end;

procedure TCategoryBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  AData := Sender.GetNodeData(Node);
  Initialize(AData^)
end;

procedure TCategoryBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TCategoryBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  AData := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;

  case Column of
    NoColumn, 0:
      CellText := AData.Name;
    1:
      CellText := AData.TypeName;
  end;
end;

procedure TCategoryBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var AData: PNodeData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      if AData.NodeType = ntCategory then
        ImageIndex := imgCategory
      else
        ImageIndex := imgAssembly;
    end;
  end;
end;

procedure TCategoryBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  if AData1.NodeType <> AData2.NodeType then begin
    if AData1.NodeType = ntCategory then
      Result := +1
    else
      Result := -1;
    exit;
  end;

  case Column of
    NoColumn, 0:
      Result := CompareText(AData1.Name, AData2.Name);
    1:
      Result := CompareText(AData1.TypeName, AData2.TypeName);
  end;
end;

procedure TCategoryBrowserForm.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
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

procedure TCategoryBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Form: TWinControl;
  Data: PNodeData;
begin
  Form := Self.Parent;
  while (Form <> nil) and not (Form is TForm) do
    Form := Form.Parent;
  if Form = nil then exit;

  if Node = nil then begin
    SetAssemblySelection(Form.Handle, nil);
    exit;
  end;

  Data := Tree.GetNodeData(Node);
  if Data.Assembly = 0 then
    SetAssemblySelection(Form.Handle, nil)
  else
    SetAssemblySelection(Form.Handle, Data.Assembly);
end;


end.
