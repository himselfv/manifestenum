unit RegistryBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Menus, VirtualTrees,
  Generics.Collections, AssemblyDb, AssemblyDb.Registry, DelayLoadTree, CommonResources,
  ManifestEnum.RegistryActions;

type
  TRootKey = record
    Key: TRegistryKeyId;
    Caption: string;     // empty means use registry key name
  end;
  TRootKeyList = class(TList<TRootKey>)
  public
    procedure Add(AKey: TRegistryKeyId; const ACaption: string = '');
  end;

  TRegistryNodeType = (
    ntKey,
    ntValue
  );

  TRegistryNodeData = record
    DelayLoad: TDelayLoadHeader;
    type_: TRegistryNodeType;
    r: TRegistryValueData;
  end;
  PRegistryNodeData = ^TRegistryNodeData;

  TRegistryBrowserMode = (
    rmRegistry, //show all registry
    rmKeys      //show specific keys
  );

 {
  RegistryBrowser can show values both inline (in the main table) or in the separate panel
  to the right of it
 }
  TRegistryValuePresentation = (
    vpNone,     //show only keys
    vpInline,   //show values in the same tree
    vpPanel     //show on a separate panel
  );

  TRegistryValueNodeData = TRegistryValueData;
  PRegistryValueNodeData = ^TRegistryValueNodeData;

  TRegistryBrowserForm = class(TDelayLoadTree)
    splValues: TSplitter;
    vtValues: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure vtValuesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtValuesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vtValuesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtValuesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtValuesGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure vtValuesGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure vtValuesCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure vtValuesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  protected
    FMode: TRegistryBrowserMode;
    FRootKeys: TRootKeyList; //root keys to display in rmKeys mode
    FValuePresentation: TRegistryValuePresentation;
    procedure SetMode(const AValue: TRegistryBrowserMode);
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    procedure AddChildren(AParent: PVirtualNode; AKeyId: TRegistryKeyId);
    function AddRootKey(AParent: PVirtualNode; const AKey: TRootKey): PVirtualNode;
    function GetFocusedKey: TRegistryKeyId;
    procedure SetValuePresentation(AValue: TRegistryValuePresentation);
    procedure ApplyValuePresentation;
  public
    property Mode: TRegistryBrowserMode read FMode write SetMode;
    property RootKeys: TRootKeyList read FRootKeys;
    property ValuePresentation: TRegistryValuePresentation read FValuePresentation write SetValuePresentation;

  protected
    function AddValueNode(AParent: PVirtualNode; AValue: TRegistryValueData): PVirtualNode;
    procedure ReloadValueNodes;
    function GetSelectedValueNodes: TArray<PVirtualNode>;
    function GetSelectedValueNodesData: TArray<PRegistryValueData>;
    procedure vtValues_AddNodeToArray(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      var Abort: Boolean);

  end;

var
  RegistryBrowserForm: TRegistryBrowserForm;

implementation
uses AssemblyDb.Assemblies, CommonMessages;

{$R *.dfm}

procedure TRootKeyList.Add(AKey: TRegistryKeyId; const ACaption: string);
var AValue: TRootKey;
begin
  AValue.Key := AKey;
  AValue.Caption := ACaption;
  inherited Add(AValue);
end;

procedure TRegistryBrowserForm.FormCreate(Sender: TObject);
begin
  inherited;
  FRootKeys := TRootKeyList.Create;
  FMode := rmRegistry;
  FValuePresentation := vpPanel;
end;

procedure TRegistryBrowserForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRootKeys);
  inherited;
end;

procedure TRegistryBrowserForm.FormShow(Sender: TObject);
begin
  ApplyValuePresentation;
  inherited;
end;

procedure TRegistryBrowserForm.SetMode(const AValue: TRegistryBrowserMode);
begin
  if AValue <> Self.FMode then begin
    Self.FMode := AValue;
    Reload;
  end;
end;

//You will have to manually reload after changing the presentation. This is not designed to be
//changed mid-browsing anyway.
procedure TRegistryBrowserForm.SetValuePresentation(AValue: TRegistryValuePresentation);
begin
  if Self.FValuePresentation = AValue then exit;
  FValuePresentation := AValue;
  ApplyValuePresentation;
end;

procedure TRegistryBrowserForm.ApplyValuePresentation;
begin
  case FValuePresentation of
    vpNone, vpInline: begin
      vtValues.Visible := false;
      splValues.Visible := false;
      Tree.Align := alClient;
    end;
    vpPanel: begin
      Tree.Align := alLeft;
      Tree.Width := Self.Width div 3;
      splValues.Visible := true;
      splValues.Left := Tree.Left + Tree.Width + 2;
      vtValues.Visible := true;
      vtValues.Left := splValues.Left + splValues.Width + 2;
      vtValues.Align := alClient;
    end;
  end;

  if FValuePresentation = vpInline then begin
    Tree.Header.Options := Tree.Header.Options + [hoVisible];
    Tree.Header.Columns[1].Options := Tree.Header.Columns[1].Options + [coVisible];
    Tree.Header.Columns[2].Options := Tree.Header.Columns[2].Options + [coVisible];
    Tree.Header.AutoSizeIndex := 2;
    Tree.Header.Columns[0].Width := Tree.Width div 3;
  end else begin
    Tree.Header.Options := Tree.Header.Options - [hoVisible];
    Tree.Header.Columns[1].Options := Tree.Header.Columns[1].Options - [coVisible];
    Tree.Header.Columns[2].Options := Tree.Header.Columns[2].Options - [coVisible];
    Tree.Header.AutoSizeIndex := 0;
  end;
end;

procedure TRegistryBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PRegistryNodeData absolute ANodeData;
  i: integer;
begin
  if ANode = nil then begin //root node
    if FMode = rmRegistry then
      AddChildren(ANode, 0)
    else begin
      for i := 0 to FRootKeys.Count-1 do
        AddRootKey(nil, FRootKeys[i]);
    end;
  end else
    AddChildren(ANode, AData.r.key);
end;

//Adds all registry key children as key nodes. Do not call twice for the same node (you won't
//if you only call this from DelayLoad)
procedure TRegistryBrowserForm.AddChildren(AParent: PVirtualNode; AKeyId: TRegistryKeyId);
var AList: TRegistryKeyList;
  ANode: PVirtualNode;
  AData: PRegistryNodeData;
  AKey: TRegistryKeyId;
  AValues: TRegistryValueList;
  i: integer;
begin
  AList := TRegistryKeyList.Create;
  try
    FDb.Registry.GetKeys(AKeyId, AList);
    for AKey in AList.Keys do begin
      ANode := Self.AddNode(AParent);
      AData := Tree.GetNodeData(ANode);
      AData.type_ := ntKey;
      AData.r.key := AKey;
      AData.r.name := AList[AKey];
    end;
  finally
    FreeAndNil(AList);
  end;

  if FValuePresentation = vpInline then begin
    AValues := TRegistryValueList.Create;
    try
      FDb.Registry.GetKeyValues(AKeyId, AValues);
      for i := 0 to AValues.Count-1 do begin
        ANode := Self.AddNode(AParent);
        AData := Tree.GetNodeData(ANode);
        AData.type_ := ntValue;
        AData.r := AValues[i];
        AData.DelayLoad.Touched := true;
      end;
    finally
      FreeAndNil(AValues);
    end;
  end;
end;

//Adds a node anywhere from a root key structure
function TRegistryBrowserForm.AddRootKey(AParent: PVirtualNode; const AKey: TRootKey): PVirtualNode;
var AData: PRegistryNodeData;
begin
  Result := Self.AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.type_ := ntKey;
  AData.r.key := AKey.Key;
  AData.r.name := AKey.Caption;
  if AData.r.name = '' then
    AData.r.name := Db.Registry.GetKeyName(AKey.Key);
end;

procedure TRegistryBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TRegistryNodeData);
end;

procedure TRegistryBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var AData: PRegistryNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^)
end;

procedure TRegistryBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PRegistryNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TRegistryBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PRegistryNodeData;
begin
  AData := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;

  case Column of
    NoColumn, 0:
      CellText := AData.r.name;
    1:
      if AData.type_ = ntValue then
        CellText := GetRegistryValueTypeName(AData.r.valueType)
      else CellText := '';
    2: CellText := AData.r.value;
  end;
end;

procedure TRegistryBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var AData: PRegistryNodeData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      if AData.type_ = ntKey then
        ImageIndex := imgFolder
      else
        ImageIndex := imgRegistryValue;
    end;
  end;
end;

procedure TRegistryBrowserForm.TreeGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
var Data: PRegistryNodeData;
begin
  inherited;
  if Node = nil then begin
    RegistryActions.SetSelectedKeys(nil);
    RegistryActions.SetSelectedValues(nil);
    PopupMenu := RegistryActions.KeyPopupMenu;
    exit;
  end;

  Data := Sender.GetNodeData(Node);
  if Data.type_ = ntKey then begin
    if Data.r.key > 0 then
      RegistryActions.SetSelectedKey(Data.r.key)
    else
      RegistryActions.SetSelectedKeys(nil);
    PopupMenu := RegistryActions.KeyPopupMenu;
  end else begin
    RegistryActions.SetSelectedValue(@Data.r);
    PopupMenu := RegistryActions.ValuePopupMenu;
  end;
end;

procedure TRegistryBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PRegistryNodeData;
begin
  Data1 := Tree.GetNodeData(Node1);
  Data2 := Tree.GetNodeData(Node2);
  case Column of
    NoColumn, 0: begin
      Result := integer(Data1.type_) - integer(Data2.type_); //values go later
      if Result = 0 then
        Result := CompareText(Data1.r.name, Data2.r.name);
    end
  else
    inherited;
  end;
end;

procedure TRegistryBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var AData: PRegistryNodeData;
  Form: TWinControl;
  AList: TRegistryKeyReferees;
begin
  inherited;
  if vtValues.Visible then
    ReloadValueNodes;

  //Pass assembly selection
  Form := Self.ParentForm;
  if Form <> nil then
    if Node = nil then
      CommonMessages.SetAssemblySelection(Form.Handle, nil)
    else begin
      AData := Tree.GetNodeData(Node);
      AList := TRegistryKeyReferees.Create;
      try
        FDb.Registry.GetKeyReferees(AData.r.key, AList);
        CommonMessages.SetAssemblySelection(Form.Handle, AList.Keys.ToArray);
      finally
        FreeAndNil(AList);
      end;
    end;
end;


function TRegistryBrowserForm.GetFocusedKey: TRegistryKeyId;
var AData: PRegistryNodeData;
begin
  if Tree.FocusedNode = nil then begin
    Result := 0;
    exit;
  end;

  AData := Tree.GetNodeData(Tree.FocusedNode);
  Result := AData.r.key;
end;



// Values

procedure TRegistryBrowserForm.ReloadValueNodes;
var AKey: TRegistryKeyId;
  AList: TRegistryValueList;
  i: integer;
begin
  vtValues.Clear;

  AKey := Self.GetFocusedKey();
  if AKey <= 0 then exit;

  AList := TRegistryValueList.Create;
  vtValues.BeginUpdate;
  try
    Db.Registry.GetKeyValues(AKey, AList);
    for i := 0 to AList.Count-1 do
      AddValueNode(nil, AList[i]);
    vtValues.Sort(nil, vtValues.Header.SortColumn, vtValues.Header.SortDirection);
  finally
    vtValues.EndUpdate;
    FreeAndNil(AList);
  end;
end;

function TRegistryBrowserForm.AddValueNode(AParent: PVirtualNode; AValue: TRegistryValueData): PVirtualNode;
var AData: PRegistryValueNodeData;
begin
  Result := vtValues.AddChild(AParent);
  vtValues.ReinitNode(Result, false);
  AData := vtValues.GetNodeData(Result);
  AData^ := AValue;
end;

function TRegistryBrowserForm.GetSelectedValueNodes: TArray<PVirtualNode>;
begin
  vtValues.IterateSubtree(nil, vtValues_AddNodeToArray, @Result, [vsSelected]);
end;

procedure TRegistryBrowserForm.vtValues_AddNodeToArray(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var AArray: ^TArray<PVirtualNode> absolute Data;
begin
  SetLength(AArray^, Length(AArray^)+1);
  AArray^[Length(AArray^)-1] := Node;
end;

function TRegistryBrowserForm.GetSelectedValueNodesData: TArray<PRegistryValueData>;
var SelectedNodes: TArray<PVirtualNode>;
  i: integer;
begin
  SelectedNodes := GetSelectedValueNodes;
  SetLength(Result, Length(SelectedNodes));
  for i := 0 to Length(SelectedNodes)-1 do
    Result[i] := PRegistryValueData(vtValues.GetNodeData(SelectedNodes[i]));
end;

procedure TRegistryBrowserForm.vtValuesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TRegistryValueNodeData);
end;

procedure TRegistryBrowserForm.vtValuesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var AData: PRegistryValueNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TRegistryBrowserForm.vtValuesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PRegistryValueNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TRegistryBrowserForm.vtValuesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PRegistryValueNodeData;
begin
  AData := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;

  case Column of
    NoColumn, 0: begin
      CellText := AData.name;
      if CellText = '' then
        CellText := '(Default)';
    end;
    1: CellText := GetRegistryValueTypeName(AData.valueType);
    2: CellText := AData.value;
  end;
end;

procedure TRegistryBrowserForm.vtValuesGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var AData: PRegistryValueNodeData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  AData := Sender.GetNodeData(Node);

  case Column of
    NoColumn, 0: begin
      ImageList := ResourceModule.SmallImages;
      ImageIndex := imgRegistryValue;
    end;
  end;
end;

procedure TRegistryBrowserForm.vtValuesGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
begin
  inherited;
  RegistryActions.SetSelectedValues(GetSelectedValueNodesData());
  PopupMenu := RegistryActions.ValuePopupMenu;
end;

procedure TRegistryBrowserForm.vtValuesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PRegistryValueNodeData;
begin
  Data1 := Tree.GetNodeData(Node1);
  Data2 := Tree.GetNodeData(Node2);
  case Column of
    NoColumn, 0: Result := CompareText(Data1.name, Data2.name);
    1: Result := Data1.valueType - Data2.valueType;
    2: Result := CompareText(Data1.value, Data2.value);
  else
    inherited;
  end;
end;

procedure TRegistryBrowserForm.vtValuesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: PRegistryValueNodeData;
  Form: TWinControl;
begin
  inherited;
  Form := Self.ParentForm;
  if Form = nil then exit;

  if Node = nil then
    CommonMessages.SetAssemblySelection(Form.Handle, nil)
  else begin
    Data := vtValues.GetNodeData(Node);
    CommonMessages.SetAssemblySelection(Form.Handle, Data.assembly);
  end;
end;

end.
