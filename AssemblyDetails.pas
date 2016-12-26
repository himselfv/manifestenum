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
    lblName: TLabel;
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

procedure TAssemblyDetailsForm.LoadAssemblyData;
begin
  if Self.FAssemblyId <> 0 then begin
    FData := Db.Assemblies.GetAssembly(Self.FAssemblyId);
    lblName.Caption := FData.identity.ToString;
  end else begin
    lblName.Caption := '';
  end;
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
