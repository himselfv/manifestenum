unit ServiceBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Dialogs, Vcl.ImgList, DelayLoadTree, VirtualTrees,
  CommonMessages, CommonResources, AssemblyDb, AssemblyDb.Assemblies, AssemblyDb.Services;

type
  TNodeType = (ntFolder, ntService, ntServiceVersion);
  TNodeData = record
    DelayLoad: TDelayLoadHeader;
    NodeType: TNodeType;
    Name: string;
    ServiceId: TServiceId;
    Entry: TServiceEntryData;
  end;
  PNodeData = ^TNodeData;

  TServiceBrowserForm = class(TDelayLoadTree)
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
  protected
    procedure DelayLoad(ANode: PVirtualNode; ANodeData: pointer); override;
    function FindServiceNode(AParent: PVirtualNode; const AServiceName: string): PVirtualNode;
    procedure FindServiceNode_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      var Abort: Boolean);
    function GetServiceNode(AParent: PVirtualNode; const AServiceName: string): PVirtualNode;
    function AddServiceVersionNode(AParent: PVirtualNode; AId: TServiceId; const AServiceData: TServiceEntryData): PVirtualNode;
  protected
    FQuickFilterText: string;
    procedure ApplyFilter; override;
    procedure WmSetQuickfilter(var msg: TWmSetQuickFilter); message WM_SET_QUICKFILTER;
  end;

var
  ServiceBrowserForm: TServiceBrowserForm;

implementation
uses StrUtils, Generics.Collections, VirtualTreeviewUtils;

{$R *.dfm}

procedure TServiceBrowserForm.DelayLoad(ANode: PVirtualNode; ANodeData: pointer);
var AData: PNodeData absolute ANodeData;
  AList: TServiceList;
  AKey: TServiceId;
  group: PVirtualNode;
begin
  if (AData <> nil) and (AData.NodeType in [ntService, ntServiceVersion]) then exit;

  AList := TServiceList.Create;
  try
    FDb.Services.GetAllServices(AList);
    for AKey in AList.Keys do begin
      group := GetServiceNode(ANode, AList.Items[AKey].name);
      AddServiceVersionNode(group, AKey, AList.Items[AKey]);
    end;
  finally
    FreeAndNil(AList);
  end;
end;

function TServiceBrowserForm.FindServiceNode(AParent: PVirtualNode; const AServiceName: string): PVirtualNode;
begin
  Result := Tree.IterateSubtree(AParent, FindServiceNode_Callback, pointer(AServiceName));
end;

procedure TServiceBrowserForm.FindServiceNode_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
var NodeData: PNodeData;
begin
  NodeData := PNodeData(Sender.GetNodeData(Node));
  Abort := (NodeData.NodeType = ntService) and SameText(NodeData.Name, string(Data));
end;

function TServiceBrowserForm.GetServiceNode(AParent: PVirtualNode; const AServiceName: string): PVirtualNode;
var AData: PNodeData;
begin
  Result := FindServiceNode(AParent, AServiceName);
  if Result = nil then begin
    Result := inherited AddNode(AParent);
    AData := Tree.GetNodeData(Result);
    AData.NodeType := ntService;
    AData.Name := AServiceName;
    AData.ServiceId := 0;
  end;
end;

function TServiceBrowserForm.AddServiceVersionNode(AParent: PVirtualNode; AId: TServiceId; const AServiceData: TServiceEntryData): PVirtualNode;
var AData: PNodeData;
begin
  Result := inherited AddNode(AParent);
  AData := Tree.GetNodeData(Result);
  AData.NodeType := ntServiceVersion;
  AData.Name := AServiceData.name;
  AData.ServiceId := AId;
  AData.Entry := AServiceData;
end;

procedure TServiceBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TServiceBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Initialize(AData^);
end;

procedure TServiceBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var AData: PNodeData;
begin
  inherited;
  AData := Sender.GetNodeData(Node);
  Finalize(AData^);
end;

procedure TServiceBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData: PNodeData;
begin
  inherited;
  if TextType <> ttNormal then exit;

  AData := Sender.GetNodeData(Node);
  case Column of
    NoColumn, 0:
      CellText := AData.Name;
  end;
end;

procedure TServiceBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
        ntService,
        ntServiceVersion: ImageIndex := imgService;
      end;
    end;
  end;
end;

procedure TServiceBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var AData1, AData2: PNodeData;
begin
  inherited;
  AData1 := Sender.GetNodeData(Node1);
  AData2 := Sender.GetNodeData(Node2);

  Result := integer(AData1.NodeType) - integer(AData2.NodeType);
  if Result = 0 then
    Result := CompareText(AData1.Name, AData2.Name);
end;

procedure TServiceBrowserForm.TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var AData: PNodeData;
  Form: TWinControl;
begin
  inherited;
  Form := Self.ParentForm;
  if Form = nil then exit;

  if Node = nil then begin
    CommonMessages.SetAssemblySelection(Form.Handle, nil);
    exit;
  end;

  AData := Sender.GetNodeData(Node);
  if AData.Entry.assemblyId > 0 then
    CommonMessages.SetAssemblySelection(Form.Handle, AData.Entry.assemblyId)
  else
    CommonMessages.SetAssemblySelection(Form.Handle, nil);
end;


// Filters


procedure TServiceBrowserForm.ApplyFilter;
var Node: PVirtualNode;
  Data: PNodeData;
begin
 //We ignore common filtes atm and only honor quickfilter

  Tree.BeginUpdate;
  try
    //This tree is too simple to bother with complicated strategies for now.
    //Just show every matching item fully and with parents
    for Node in Tree.Nodes do begin
      Data := Tree.GetNodeData(Node);
      if (Self.FQuickFilterText = '') or AnsiContainsText(Data.Name, Self.FQuickFilterText) then
        Tree.MakeNodeContentVisible(Node)
      else
        Tree.IsVisible[Node] := false;
    end;

  finally
    Tree.EndUpdate;
  end;
end;

procedure TServiceBrowserForm.WmSetQuickfilter(var msg: TWmSetQuickFilter);
begin
  if FQuickFilterText <> msg.FilterText^ then begin
    FQuickFilterText := msg.FilterText^;
    ApplyFilter;
  end;
end;


end.
