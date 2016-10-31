unit AssemblyBrowser;
// Displays the full list of all assemblies, allows to filter it by various keywords.
// Each assembly can be expanded to show its dependencies (and grand-dependencies and so on).

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Generics.Collections, DelayLoadTree, VirtualTrees, AssemblyDb.Core, AssemblyDb.Assemblies,
  CommonResources;

type
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    Name: string;
    Assembly: TAssemblyId;
    IsDeployment: boolean;
  end;
  PNodeData = ^TNodeData;

  TAssemblyEvent = procedure(Sender: TObject; AAssembly: TAssemblyId) of object;

  TAssemblyBrowserForm = class(TDelayLoadTree)
    pnlFilter: TPanel;
    sbFilterSettings: TSpeedButton;
    edtQuickFilter: TEdit;
    pnlFilterSettings: TPanel;
    cbFilterByName: TCheckBox;
    cbFilterByFiles: TCheckBox;
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
    procedure cbFilterByNameClick(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure sbFilterSettingsClick(Sender: TObject);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  protected
    FOnAssemblySelected: TAssemblyEvent;
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    procedure LoadRootAssemblies(AParent: PVirtualNode);
    function AddAssemblyNode(AParent: PVirtualNode; const AEntry: TAssemblyData): PVirtualNode;
    procedure ApplyFilter;
    function GetSelectedAssembly: TAssemblyId;
  public
    procedure Reload; override;
    property SelectedAssembly: TAssemblyId read GetSelectedAssembly;
    property OnAssemblySelected: TAssemblyEvent read FOnAssemblySelected write FOnAssemblySelected;
  end;

var
  AssemblyBrowserForm: TAssemblyBrowserForm;

implementation

{$R *.dfm}

procedure TAssemblyBrowserForm.Reload;
begin
  inherited;
  ApplyFilter;
end;

procedure TAssemblyBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
begin
  if ANode = nil then begin
    LoadRootAssemblies(nil);
    exit;
  end;
  //TODO: Load dependencies
end;

procedure TAssemblyBrowserForm.LoadRootAssemblies(AParent: PVirtualNode);
var list: TAssemblyList;
  entry: TAssemblyData;
begin
 //DelayLoad will be called for each of the root assemblies immediately, which is slow.
 //So we'll try to create root nodes already delay-initialized.

  list := TAssemblyList.Create;
  try
    FDb.Assemblies.GetAllAssemblies(list);
    for entry in list.Values do
      AddAssemblyNode(AParent, entry);
    //TODO: Delay-init added nodes
  finally
    FreeAndNil(list);
  end;
end;

function TAssemblyBrowserForm.AddAssemblyNode(AParent: PVirtualNode; const AEntry: TAssemblyData): PVirtualNode;
var Data: PNodeData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  Data := Tree.GetNodeData(Result);
  Data.Name := AEntry.identity.ToString;
  Data.Assembly := AEntry.id;
  Data.IsDeployment := AEntry.isDeployment;
  Data.DelayLoad.Touched := false;
end;

function TAssemblyBrowserForm.GetSelectedAssembly: TAssemblyId;
var Data: PNodeData;
begin
  if Tree.FocusedNode = nil then
    Result := 0
  else begin
    Data := Tree.GetNodeData(Tree.FocusedNode);
    Result := Data.Assembly;
  end;
end;

procedure TAssemblyBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TAssemblyBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNodeData;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TAssemblyBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PNodeData;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TAssemblyBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PNodeData;
begin
  if TextType <> ttNormal then exit;
  Data := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0: CellText := Data.Name;
  end;
end;

procedure TAssemblyBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var Data: PNodeData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  Data := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      ImageIndex := imgAssembly;
    end;
  end;
end;

procedure TAssemblyBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PNodeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    NoColumn, 0: Result := CompareText(Data1.Name, Data2.Name);
  end;
end;

procedure TAssemblyBrowserForm.TreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var Data: PNodeData;
begin
  inherited;
  Data := Tree.GetNodeData(Node);
  if Data.IsDeployment then
    TargetCanvas.Font.Color := clBlue
  else
    TargetCanvas.Font.Color := clBlack;
end;

procedure TAssemblyBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: PNodeData;
begin
  inherited;
  Data := Tree.GetNodeData(Node);
  if Assigned(Self.FOnAssemblySelected) then
    if Data <> nil then
      Self.FOnAssemblySelected(Self, Data.Assembly)
    else
      Self.FOnAssemblySelected(Self, 0);
end;

// Applies visibility to root nodes according to the current quick-filter
procedure TAssemblyBrowserForm.ApplyFilter;
var list: TAssemblyList;
  filter: string;
  Node: PVirtualNode;
  Data: PNodeData;
  ShowAll: boolean;
begin
  filter := edtQuickFilter.Text;

  list := TAssemblyList.Create;
  Tree.BeginUpdate;
  try
    ShowAll := (not cbFilterByName.Checked) and (not cbFilterByFiles.Checked);
    if cbFilterByName.Checked then
      FDb.FilterAssemblyByName(filter, list);
    if cbFilterByFiles.Checked then
      FDb.FilterAssemblyByFile(filter, list);
    for Node in Tree.ChildNodes(nil) do begin
      Data := Tree.GetNodeData(Node);
      if ShowAll or list.ContainsKey(Data.Assembly) then
        Tree.IsVisible[Node] := true
      else
        Tree.IsVisible[Node] := false;
    end;
  finally
    Tree.EndUpdate;
    FreeAndNil(list);
  end;
end;

procedure TAssemblyBrowserForm.edtQuickFilterChange(Sender: TObject);
begin
  inherited;
  ApplyFilter;
end;

procedure TAssemblyBrowserForm.cbFilterByNameClick(Sender: TObject);
begin
  inherited;
  ApplyFilter;
end;

procedure TAssemblyBrowserForm.sbFilterSettingsClick(Sender: TObject);
begin
  inherited;
  pnlFilterSettings.Visible := sbFilterSettings.Down;
end;


end.
