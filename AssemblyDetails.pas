unit AssemblyDetails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Generics.Collections,
  AssemblyDb, AssemblyFilesView, AssemblyResourcesView;

type
  TAssemblyDetailsForm = class(TForm)
    pcDetails: TPageControl;
    tsGeneral: TTabSheet;
    tsDependencies: TTabSheet;
    lbDependencies: TListBox;
    tsDependents: TTabSheet;
    tsCategories: TTabSheet;
    tsAdditionalGear: TTabSheet;
    lbDependents: TListBox;
    procedure FormCreate(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblyId: TAssemblyId;
    procedure SetDb(ADb: TAssemblyDb);
    procedure SetAssemblyId(const AValue: TAssemblyId);
    procedure LoadAssemblyData;
    procedure LoadDependencies;
    procedure LoadDependents;
  protected //Additional tabs
    FilesTab: TAssemblyFilesForm;
    ResourcesTab: TAssemblyResourcesForm;
    procedure AddTab(AForm: TForm);
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
  FilesTab := TAssemblyFilesForm.Create(Self);
  FilesTab.FollowDependencies := true;
  AddTab(FilesTab);

  ResourcesTab := TAssemblyResourcesForm.Create(Self);
  ResourcesTab.ShowDependencies := true;
  AddTab(ResourcesTab);
end;

procedure TAssemblyDetailsForm.AddTab(AForm: TForm);
var ATab: TTabSheet;
begin
  ATab := TTabSheet.Create(pcDetails);
  ATab.PageControl := pcDetails;
  ATab.Caption := AForm.Caption;
  AForm.ManualDock(ATab, ATab, alClient);
  AForm.Align := alClient;
  AForm.Visible := true;
end;

procedure TAssemblyDetailsForm.SetDb(ADb: TAssemblyDb);
begin
  FDb := ADb;
  FilesTab.Db := ADb;
  ResourcesTab.Db := ADb;
end;

procedure TAssemblyDetailsForm.SetAssemblyId(const AValue: TAssemblyId);
begin
  if AValue <> FAssemblyId then begin
    FAssemblyId := AValue;
    Reload;
  end;
  FilesTab.Assembly := AValue;
  ResourcesTab.Assembly := AValue;
end;

procedure TAssemblyDetailsForm.Clear;
begin
  lbDependencies.Clear;
  lbDependents.Clear;
  FilesTab.Clear;
  ResourcesTab.Clear;
end;

procedure TAssemblyDetailsForm.Reload;
begin
  Clear;
  if FAssemblyId <= 0 then exit;

  LoadAssemblyData;
  LoadDependencies;
  LoadDependents;
  FilesTab.Reload;
  ResourcesTab.Reload;
end;

procedure TAssemblyDetailsForm.LoadAssemblyData;
begin

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
