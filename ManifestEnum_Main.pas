unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Generics.Collections, Vcl.Menus, AssemblyDb, Vcl.ExtCtrls, Vcl.Buttons,
  AssemblyDetails, RegistryBrowser, TaskBrowser;

type
  TMainForm = class(TForm)
    lbComponents: TListBox;
    MainMenu: TMainMenu;
    F1: TMenuItem;
    Exit1: TMenuItem;
    pmRebuildAssemblyDatabase: TMenuItem;
    N1: TMenuItem;
    Reload1: TMenuItem;
    pnlFilterSettings: TPanel;
    pnlFilter: TPanel;
    edtQuickFilter: TEdit;
    sbFilterSettings: TSpeedButton;
    cbFilterByName: TCheckBox;
    cbFilterByFiles: TCheckBox;
    Debug1: TMenuItem;
    Loadmanifestfile1: TMenuItem;
    OpenManifestDialog: TOpenDialog;
    pcMain: TPageControl;
    tsAssemblies: TTabSheet;
    PopupMenu: TPopupMenu;
    Savemanifest1: TMenuItem;
    SaveManifestDialog: TSaveDialog;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure lbComponentsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure sbFilterSettingsClick(Sender: TObject);
    procedure cbFilterByNameClick(Sender: TObject);
    procedure Loadmanifestfile1Click(Sender: TObject);
    procedure Savemanifest1Click(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblyDetails: TAssemblyDetailsForm;
    FRegistryBrowser: TRegistryBrowserForm;
    FTaskBrowser: TTaskBrowserForm;
    procedure AddPage(const AForm: TForm);
  public
    procedure UpdateAssemblyList;
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, AssemblyDbBuilder, SxSExpand;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  InitAssemblyDb(FDb, AppFolder+'\assembly.db', true);
  UpdateAssemblyList;

  FRegistryBrowser := TRegistryBrowserForm.Create(Application);
  FRegistryBrowser.Db := FDb;
  AddPage(FRegistryBrowser);

  FTaskBrowser := TTaskBrowserForm.Create(Application);
  FTaskBrowser.Db := FDb;
  AddPage(FTaskBrowser);

  FAssemblyDetails := TAssemblyDetailsForm.Create(Application);
  FAssemblyDetails.Db := FDb;
  FAssemblyDetails.ManualDock(Self.tsAssemblies, nil, alBottom);
  FAssemblyDetails.Align := alBottom;
  FAssemblyDetails.Visible := true;
  Splitter1.Top := FAssemblyDetails.Top - 10;
//  FAssemblyDetails.Top := Splitter1.Top + 50;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDb);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AddPage(const AForm: TForm);
var ts: TTabSheet;
begin
  ts := TTabSheet.Create(pcMain);
  ts.Caption := AForm.Caption;
  ts.PageControl := pcMain;

  AForm.Parent := ts;
  AForm.BorderStyle := bsNone;
  AForm.WindowState := wsMaximized;
  AForm.Align := alClient;
  AForm.Show;
end;

procedure TMainForm.pmRebuildAssemblyDatabaseClick(Sender: TObject);
begin
  RebuildAssemblyDatabase(FDb, AppFolder+'\assembly.db');
  UpdateAssemblyList;
end;

//Показывает новый отфильтрованный список пакетов
procedure TMainForm.UpdateAssemblyList;
var
  filter: string;
  list: TAssemblyList;
  entry: TAssemblyData;
begin
  list := TAssemblyList.Create;
  lbComponents.Items.BeginUpdate;
  try
    lbComponents.Items.Clear;

    filter := edtQuickFilter.Text;
    filter := filter.ToLower();

    if filter = '' then
      FDb.GetAllAssemblies(list)
    else begin
      if cbFilterByName.Checked then
        FDb.FilterAssemblyByName(filter, list);
      if cbFilterByFiles.Checked then
        FDb.FilterAssemblyByFile(filter, list);
    end;

    for entry in list.Values do
      lbComponents.AddItem(entry.identity.ToString, TObject(entry.id));
  finally
    lbComponents.Items.EndUpdate;
    FreeAndNil(list);
  end;
end;

procedure TMainForm.cbFilterByNameClick(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.edtQuickFilterChange(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.Reload1Click(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.lbComponentsClick(Sender: TObject);
begin
  if lbComponents.ItemIndex >= 0 then
    FAssemblyDetails.AssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex])
  else
    FAssemblyDetails.AssemblyId := 0;
end;

procedure TMainForm.sbFilterSettingsClick(Sender: TObject);
begin
//  sbFilterSettings.Down := not sbFilterSettings.Down;
  pnlFilterSettings.Visible := sbFilterSettings.Down;
end;

procedure TMainForm.Loadmanifestfile1Click(Sender: TObject);
begin
  with OpenManifestDialog do
    if Execute then
      FDb.ImportManifest(Filename);
end;

procedure TMainForm.Savemanifest1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  AManifestPath: string;
  ATargetFile: TStringList;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.GetAssembly(AAssemblyId);

  if AAssemblyData.manifestName = '' then begin
    MessageBox(Self.Handle, PChar('This assembly has no associated manifest'), PChar('Cannot save manifest'), MB_OK + MB_ICONEXCLAMATION);
    exit;
  end;

  SaveManifestDialog.Filename := AAssemblyData.manifestName+'.manifest';
  if not SaveManifestDialog.Execute then
    exit;

  AManifestPath := GetWindowsDir()+'\WinSxS\Manifests\'+AAssemblyData.manifestName+'.manifest';

  ATargetFile := TStringList.Create();
  try
    ATargetFile.Text := LoadManifestFile(AManifestPath);
    ATargetFile.SaveToFile(SaveManifestDialog.Filename);
  finally
    FreeAndNil(ATargetFile);
  end;
end;


end.
