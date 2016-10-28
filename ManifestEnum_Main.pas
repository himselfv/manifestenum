unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Generics.Collections, Vcl.Menus, AssemblyDb, Vcl.ExtCtrls, Vcl.Buttons,
  AssemblyDetails, FileBrowser, RegistryBrowser, TaskBrowser, CategoryBrowser;

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
    Uninstallassembly1: TMenuItem;
    Getassemblysize1: TMenuItem;
    Copy1: TMenuItem;
    Assemblyname1: TMenuItem;
    Assemblystrongname1: TMenuItem;
    Assemblydisplayname1: TMenuItem;
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
    procedure Uninstallassembly1Click(Sender: TObject);
    procedure Getassemblysize1Click(Sender: TObject);
    procedure Assemblyname1Click(Sender: TObject);
    procedure Assemblydisplayname1Click(Sender: TObject);
    procedure Assemblystrongname1Click(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblyDetails: TAssemblyDetailsForm;
    FCategoryBrowser: TCategoryBrowserForm;
    FFileBrowser: TFileBrowserForm;
    FRegistryBrowser: TRegistryBrowserForm;
    FTaskBrowser: TTaskBrowserForm;
    procedure AddPage(const AForm: TForm);
  public
    procedure UpdateAssemblyList;
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, AssemblyDbBuilder, ManifestParser, SxSExpand, AssemblyDb.Assemblies,
  DelayLoadTree, AutorunsBrowser, ShellExtBrowser, winsxs, ComObj, Clipbrd;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  InitAssemblyDb(FDb, AppFolder+'\assembly.db', true);
  UpdateAssemblyList;

  FCategoryBrowser := TCategoryBrowserForm.Create(Application);
  AddPage(FCategoryBrowser);

  FFileBrowser := TFileBrowserForm.Create(Application);
  AddPage(FFileBrowser);

  FRegistryBrowser := TRegistryBrowserForm.Create(Application);
  AddPage(FRegistryBrowser);

  FTaskBrowser := TTaskBrowserForm.Create(Application);
  AddPage(FTaskBrowser);

  AddPage(TAutorunsBrowserForm.Create(Application));
  AddPage(TShellExtensionBrowserForm.Create(Application));

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

  if AForm is TDelayLoadTree then
    TDelayLoadTree(AForm).Db := FDb;

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
      FDb.Assemblies.GetAllAssemblies(list)
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
var parser: TManifestParser;
begin
  with OpenManifestDialog do
    if Execute then begin
      parser := TManifestParser.Create(FDb);
      try
        parser.ImportManifest(Filename);
      finally
        FreeAndNil(parser);
      end;
    end;
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
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

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


procedure TMainForm.Assemblyname1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.name;
end;

procedure TMainForm.Assemblydisplayname1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToString;
end;


procedure TMainForm.Assemblystrongname1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToStrongName;
end;

procedure TMainForm.Getassemblysize1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  ACache: IAssemblyCache;
  AInfo: ASSEMBLY_INFO;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  OleCheck(CreateAssemblyCache(ACache, 0));
  FillChar(AInfo, SizeOf(AInfo), 0);
  AInfo.cbAssemblyInfo := SizeOf(AInfo);
  AInfo.dwAssemblyFlags := QUERYASMINFO_FLAG_GETSIZE;
  OleCheck(ACache.QueryAssemblyInfo(0, PChar(AAssemblyData.identity.ToStrongName), @AInfo));

  MessageBox(Self.Handle, PChar('Assembly size: '+IntToStr(AInfo.uliAssemblySizeInKB.QuadPart)+' Kb'),
    PChar('Assembly info'), MB_OK);
end;

procedure TMainForm.Uninstallassembly1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  ACache: IAssemblyCache;
  uresult: ULong;
begin
  if lbComponents.ItemIndex < 0 then
    exit;
  AAssemblyId := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  if MessageBox(Self.Handle, PChar('You are going to uninstall '+AAssemblyData.identity.name+'.'#13
      +'Do you really want to continue?'),
    PChar('Confirm uninstall'), MB_ICONQUESTION + MB_YESNO) <> ID_YES then
    exit;

  OleCheck(CreateAssemblyCache(ACache, 0));
  OleCheck(ACache.UninstallAssembly(0, PChar(AAssemblyData.identity.ToStrongName), nil, @uresult));
  MessageBox(Self.Handle, PChar('Uninstall result: '+IntToStr(uresult)), PChar('Done'), MB_OK);
end;

end.
