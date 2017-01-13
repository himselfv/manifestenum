unit AssemblyDetails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Generics.Collections,
  AssemblyDb, AssemblyFilesView, AssemblyResourcesView, AssemblyDb.Assemblies;

type
  TAssemblyDetailsForm = class(TForm)
    pcDetails: TPageControl;
    tsGeneral: TTabSheet;
    tsDependencies: TTabSheet;
    lbDependencies: TListBox;
    tsDependents: TTabSheet;
    tsCategories: TTabSheet;
    lbDependents: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pcDetailsChange(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblyId: TAssemblyId;
    FData: TAssemblyData;
    procedure SetDb(ADb: TAssemblyDb);
    procedure SetAssemblyId(const AValue: TAssemblyId);
    procedure LoadAssemblyData;
    procedure LoadDependencies;
    procedure LoadDependents;
  protected //Additional tabs
    ResourcesForm: TAssemblyResourcesForm;
    ResourcesTab: TTabSheet;
    ResourcesTabFirstReloadDone: boolean;
    function AddTab(AForm: TForm): TTabSheet;
  public
    procedure Clear;
    procedure Reload;
    property Db: TAssemblyDb read FDb write SetDb;
    property AssemblyId: TAssemblyId read FAssemblyId write SetAssemblyId;
  end;

var
  AssemblyDetailsForm: TAssemblyDetailsForm;

implementation
uses SxsUtils, SxsExpand, ComObj, MSXML;

{$R *.dfm}

procedure TAssemblyDetailsForm.FormCreate(Sender: TObject);
begin
  ResourcesForm := TAssemblyResourcesForm.Create(Self);
  ResourcesForm.ShowDependencies := true;
  ResourcesTab := AddTab(ResourcesForm);
end;

procedure TAssemblyDetailsForm.FormShow(Sender: TObject);
begin
  Self.pcDetails.ActivePageIndex := 0;
end;

function TAssemblyDetailsForm.AddTab(AForm: TForm): TTabSheet;
begin
  Result := TTabSheet.Create(pcDetails);
  Result.PageControl := pcDetails;
  Result.Caption := AForm.Caption;
  AForm.ManualDock(Result, Result, alClient);
  AForm.Align := alClient;
  AForm.Visible := true;
end;

procedure TAssemblyDetailsForm.SetDb(ADb: TAssemblyDb);
begin
  FDb := ADb;
  ResourcesForm.Db := ADb;
end;

procedure TAssemblyDetailsForm.SetAssemblyId(const AValue: TAssemblyId);
begin
  if AValue <> FAssemblyId then begin
    FAssemblyId := AValue;
    ResourcesForm.Assemblies.Clear;
    ResourcesForm.Assemblies.Add(AValue);
    Reload;
  end;
end;

procedure TAssemblyDetailsForm.Clear;
begin
  lbDependencies.Clear;
  lbDependents.Clear;
  ResourcesForm.Clear;
end;

procedure TAssemblyDetailsForm.Reload;
begin
  Clear;
  if FAssemblyId <= 0 then exit;

  LoadAssemblyData;
  LoadDependencies;
  LoadDependents;
  if ResourcesTab.Visible then
    ResourcesForm.Reload;
end;

procedure TAssemblyDetailsForm.pcDetailsChange(Sender: TObject);
begin
 //It won't reload by itself the second+ time (when already "Visible" by its internal bookkeeping).
  if ResourcesTab.Visible then
    if ResourcesTabFirstReloadDone then
      ResourcesForm.Reload
    else
      ResourcesTabFirstReloadDone := true;
end;

//Returns node text if the node is not null
function nodeText(const ANode: IXmlDomNode): string;
begin
  if ANode <> nil then
    Result := ANode.text
  else
    Result := '';
end;

function attrText(const ANode: IXmlDomNode; const AAttrName: string): string;
begin
  Result := nodeText(ANode.attributes.getNamedItem(AAttrName));
end;

function newLabel(AParent: TWinControl; ACaption: string): TLabel;
begin
  Result := TLabel.Create(AParent);
  Result.Caption := ACaption;
  Result.Align := alTop;
  Result.WordWrap := true;
  AParent.InsertControl(Result);
  Result.Top := AParent.Height;
end;

procedure TAssemblyDetailsForm.LoadAssemblyData;
var lbl: TLabel;
  xmlStr: string;
  AXml: IXMLDOMDocument;
  node: IXmlDomNode;
  sXmlDisplayName, sXmlDescription, sXmlCopyright: string;
  sLocDisplayName, sLocDescription: string;
  textId: string;
  i: integer;
begin
  tsGeneral.DestroyComponents;
  if Self.FAssemblyId = 0 then exit;

  FData := Db.Assemblies.GetAssembly(Self.FAssemblyId);

  lbl := newLabel(tsGeneral, FData.identity.ToString);
  lbl.Font.Style := lbl.Font.Style + [fsBold];

  try
    xmlStr := LoadManifestFile(SxSManifestDir()+'\'+FData.manifestName+'.manifest');
  except
    on E: EFOpenError do
      exit;
  end;

  AXml := CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument;
  AXml.loadXML(xmlStr);

  node := AXml.selectSingleNode('/assembly');
  if node = nil then exit; //nothing more to add

  sXmlDisplayName := attrText(node, 'displayName');
  sXmlDescription := attrText(node, 'description');
  sXmlCopyright := attrText(node, 'copyright');

  sLocDisplayName := '';
  sLocDescription := '';
  node := AXml.selectSingleNode('/assembly/localization/resources/stringTable');
  if node <> nil then
    for i := 0 to node.childNodes.length-1 do begin
      textId := attrText(node.childNodes[i], 'id');
      if ((textId = 'displayName') or (textId = 'displayName0') or (textId = 'displayName1')) and (sLocDisplayName = '') then
        sLocDisplayName := attrText(node.childNodes[i], 'value');
      if ((textId = 'description') or (textId = 'description0') or (textId = 'description1')) and (sLocDescription = '') then
        sLocDescription := attrText(node.childNodes[i], 'value');
    end;

  if sXmlDisplayName <> '' then newLabel(tsGeneral, sXmlDisplayName);
  if sLocDisplayName <> '' then newLabel(tsGeneral, sLocDisplayName);

  if sXmlDescription <> '' then newLabel(tsGeneral, sXmlDescription);
  if sLocDescription <> '' then newLabel(tsGeneral, sLocDescription);

  if sXmlCopyright <> '' then newLabel(tsGeneral, sXmlCopyright);
end;

procedure TAssemblyDetailsForm.LoadDependencies;
var list: TAssemblyList;
  key: TAssemblyId;
begin
  list := TAssemblyList.Create();
  try
    FDb.GetDependencies(Self.FAssemblyId, list);
    for key in list.Keys do
      lbDependencies.Items.Add(list[key].identity.ToString);
  finally
    FreeAndNil(list);
  end;
end;

procedure TAssemblyDetailsForm.LoadDependents;
var list: TAssemblyList;
  key: TAssemblyId;
begin
  list := TAssemblyList.Create();
  try
    FDb.GetDependents(Self.FAssemblyId, list);
    for key in list.Keys do
      lbDependents.Items.Add(list[key].identity.ToString);
  finally
    FreeAndNil(list);
  end;
end;

end.
