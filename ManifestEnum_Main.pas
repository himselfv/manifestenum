unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Menus, ExtCtrls,  Generics.Collections, VirtualTrees, CommonMessages, AssemblyDb,
  AssemblyDetails, FileBrowser, RegistryBrowser, TaskBrowser, CategoryBrowser, AssemblyBrowser,
  AssemblyDb.Assemblies;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    F1: TMenuItem;
    Exit1: TMenuItem;
    pmRebuildAssemblyDatabase: TMenuItem;
    N1: TMenuItem;
    Debug1: TMenuItem;
    Loadmanifestfile1: TMenuItem;
    OpenManifestDialog: TOpenDialog;
    pcMain: TPageControl;
    Splitter1: TSplitter;
    Installassembly1: TMenuItem;
    Expandfile1: TMenuItem;
    OpenAnyFileDialog: TOpenDialog;
    SaveAnyFileDialog: TSaveDialog;
    Options1: TMenuItem;
    miForceUninstall: TMenuItem;
    N2: TMenuItem;
    miUninstallByList: TMenuItem;
    OpenListDialog: TOpenDialog;
    miAssemblyDatabase: TMenuItem;
    miRefreshAssemblyDatabase: TMenuItem;
    miVerifyHashes: TMenuItem;
    N3: TMenuItem;
    miShowLog: TMenuItem;
    miShowInstalledOnly: TMenuItem;
    N4: TMenuItem;
    miShowDeploymentsOnly: TMenuItem;
    miService: TMenuItem;
    miDISMImageCleanup: TMenuItem;
    miOpenComponentsKey: TMenuItem;
    miOpenDeploymentsKey: TMenuItem;
    miOpenSxSFolder: TMenuItem;
    Queryassemblyscavener1: TMenuItem;
    miFilters: TMenuItem;
    edtQuickFilter: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Loadmanifestfile1Click(Sender: TObject);
    procedure Installassembly1Click(Sender: TObject);
    procedure Expandfile1Click(Sender: TObject);
    procedure miUninstallByListClick(Sender: TObject);
    procedure miRefreshAssemblyDatabaseClick(Sender: TObject);
    procedure miVerifyHashesClick(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure miShowInstalledOnlyClick(Sender: TObject);
    procedure miShowDeploymentsOnlyClick(Sender: TObject);
    procedure miDISMImageCleanupClick(Sender: TObject);
    procedure miOpenComponentsKeyClick(Sender: TObject);
    procedure miOpenDeploymentsKeyClick(Sender: TObject);
    procedure miOpenSxSFolderClick(Sender: TObject);
    procedure Queryassemblyscavener1Click(Sender: TObject);
    procedure miForceUninstallClick(Sender: TObject);
    procedure miFiltersClick(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure edtQuickFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  protected
    FDb: TAssemblyDb;
    FAssemblyBrowser: TAssemblyBrowserForm;
    FAssemblyDetails: TAssemblyDetailsForm;
    FCategoryBrowser: TCategoryBrowserForm;
    FFileBrowser: TFileBrowserForm;
    FRegistryBrowser: TRegistryBrowserForm;
    FTaskBrowser: TTaskBrowserForm;
    procedure AddPage(const AForm: TForm);
    procedure AssemblyBrowserSelectionChanged(Sender: TObject);
    procedure AssemblyBrowserGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
    procedure WmSetAssemblySelection(var msg: TWmSetAssemblySelection); message WM_SET_ASSEMBLY_SELECTION;

  protected
    //Settings
    procedure LoadSettings;
    procedure SaveSettings;

  end;

var
  MainForm: TMainForm;

implementation
uses UITypes, Registry, FilenameUtils, OsUtils, SxSExpand, AssemblyDbBuilder, SxsUtils, ComObj, WinSxS,
  DelayLoadTree, AutorunsBrowser, ShellExtBrowser, ServiceBrowser, CommonFilters,
  ManifestEnum.Log, ManifestEnum.AssemblyActions, ManifestEnum.RegistryActions, ManifestEnum.FileActions;

{$R *.dfm}
{$WARN SYMBOL_PLATFORM OFF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  InitAssemblyDb(FDb, AppFolder+'\assembly.db', true);

  FAssemblyBrowser := TAssemblyBrowserForm.Create(Application);
  FAssemblyBrowser.OnSelectionChanged := Self.AssemblyBrowserSelectionChanged;
  FAssemblyBrowser.Tree.OnGetPopupMenu := Self.AssemblyBrowserGetPopupMenu;
  FAssemblyBrowser.GroupingType := gtBundles;
  AddPage(FAssemblyBrowser);

  FFileBrowser := TFileBrowserForm.Create(Application);
  AddPage(FFileBrowser);

  FRegistryBrowser := TRegistryBrowserForm.Create(Application);
  AddPage(FRegistryBrowser);

  AddPage(TServiceBrowserForm.Create(Application));

  FTaskBrowser := TTaskBrowserForm.Create(Application);
  AddPage(FTaskBrowser);

  AddPage(TAutorunsBrowserForm.Create(Application));
  AddPage(TShellExtensionBrowserForm.Create(Application));

  FCategoryBrowser := TCategoryBrowserForm.Create(Application);
  FCategoryBrowser.Db := FDb;
  AddPage(FCategoryBrowser);

  FAssemblyDetails := TAssemblyDetailsForm.Create(Application);
  FAssemblyDetails.Db := FDb;
  FAssemblyDetails.ManualDock(Self, nil, alBottom);
  FAssemblyDetails.Align := alBottom;
  FAssemblyDetails.Visible := true;
  Splitter1.Top := FAssemblyDetails.Top - 10;
//  FAssemblyDetails.Top := Splitter1.Top + 50;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDb);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  AssemblyActions.Db := FDb;
  RegistryActions.Db := FDb;
  FileActions.Db := FDb;

  LoadSettings;
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

function SplitMultiVal(const str: string): TArray<string>;
begin
  if str = '' then
    Result := nil
  else
    Result := str.Split([',']);
end;

function JoinMultiVal(const val: TArray<string>): string;
begin
  Result := Result.Join(',', val);
end;

procedure TMainForm.LoadSettings;
var ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create('ManifestEnum');
  try
    AssemblyActions.ForceUninstall := ini.ReadBool('', 'ForceUninstall', false);
    Filters.ShowInstalledOnly := ini.ReadBool('Filter', 'ShowInstalledOnly', true);
    Filters.ShowDeploymentsOnly := ini.ReadBool('Filter', 'ShowDeploymentsOnly', false);
    Filters.Versions := SplitMultiVal(ini.ReadString('Filter', 'Versions', ''));
    Filters.ProcessorArchitectures := SplitMultiVal(ini.ReadString('Filter', 'ProcessorArchitectures', ''));
    Filters.PublicKeyTokens := SplitMultiVal(ini.ReadString('Filter', 'PublicKeyTokens', ''));
    Filters.Languages := SplitMultiVal(ini.ReadString('Filter', 'Languages', ''));
    miForceUninstall.Checked := AssemblyActions.ForceUninstall;
    miShowInstalledOnly.Checked := Filters.ShowInstalledOnly;
    miShowDeploymentsOnly.Checked := Filters.ShowDeploymentsOnly;
  finally
    FreeAndNil(ini);
  end;
  FilterChanged(nil);
end;

procedure TMainForm.SaveSettings;
var ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create('ManifestEnum');
  try
    ini.WriteBool('', 'ForceUninstall', AssemblyActions.ForceUninstall);
    ini.WriteBool('Filter', 'ShowInstalledOnly', Filters.ShowInstalledOnly);
    ini.WriteBool('Filter', 'ShowDeploymentsOnly', Filters.ShowDeploymentsOnly);
    ini.WriteString('Filter', 'Versions', JoinMultiVal(Filters.Versions));
    ini.WriteString('Filter', 'ProcessorArchitectures', JoinMultiVal(Filters.ProcessorArchitectures));
    ini.WriteString('Filter', 'PublicKeyTokens', JoinMultiVal(Filters.PublicKeyTokens));
    ini.WriteString('Filter', 'Languages', JoinMultiVal(Filters.Languages));
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainForm.SettingsChanged(Sender: TObject);
begin
  SaveSettings;
end;

procedure TMainForm.miForceUninstallClick(Sender: TObject);
begin
  AssemblyActions.ForceUninstall := miForceUninstall.Checked;
  SaveSettings;
end;

procedure TMainForm.miFiltersClick(Sender: TObject);
begin
  FiltersForm.Db := Self.FDb;
  if IsPositiveResult(FiltersForm.ShowModal) then
    SaveSettings;
  FilterChanged(nil);
end;

procedure TMainForm.miShowInstalledOnlyClick(Sender: TObject);
begin
  Filters.ShowInstalledOnly := miShowInstalledOnly.Checked;
  SaveSettings;
  FilterChanged(nil);
end;

procedure TMainForm.miShowDeploymentsOnlyClick(Sender: TObject);
begin
  Filters.ShowDeploymentsOnly := miShowDeploymentsOnly.Checked;
  SaveSettings;
  FilterChanged(nil);
end;

//Common event handler for many settings controls
procedure TMainForm.pmRebuildAssemblyDatabaseClick(Sender: TObject);
begin
  RebuildAssemblyDatabase(FDb, AppFolder+'\assembly.db');
  FAssemblyBrowser.Reload;
end;

procedure TMainForm.miRefreshAssemblyDatabaseClick(Sender: TObject);
begin
  RefreshAssemblyDatabase(FDb);
  FAssemblyBrowser.Reload;
end;

procedure TMainForm.miShowLogClick(Sender: TObject);
begin
  LogForm.Show;
end;

procedure TMainForm.edtQuickFilterChange(Sender: TObject);
var page: TTabSheet;
  i: integer;
begin
  page := pcMain.ActivePage;
  if page = nil then exit;
  for i := 0 to page.ControlCount-1 do
    if page.Controls[i] is TCustomForm then
      SetQuickFilter(TForm(page.Controls[i]).Handle, edtQuickFilter.Text);
end;

procedure TMainForm.edtQuickFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    edtQuickFilter.Text := '';
end;

procedure TMainForm.pcMainChange(Sender: TObject);
begin
  //Apply quickfilter to new page
  edtQuickFilterChange(edtQuickFilter);
end;

procedure TMainForm.AssemblyBrowserSelectionChanged(Sender: TObject);
var AIds: TArray<TAssemblyId>;
begin
  AIds := FAssemblyBrowser.SelectedAssemblies;
  if Length(AIds) = 1 then
    FAssemblyDetails.AssemblyId := AIds[0]
  else
    FAssemblyDetails.AssemblyId := 0;
end;

procedure TMainForm.AssemblyBrowserGetPopupMenu(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const P: TPoint; var AskParent: Boolean; var PopupMenu: TPopupMenu);
begin
  AssemblyActions.SetSelectedAssemblies(FAssemblyBrowser.SelectedAssemblies);
  PopupMenu := AssemblyActions.PopupMenu;
end;

procedure TMainForm.WmSetAssemblySelection(var msg: TWmSetAssemblySelection);
begin
  if Length(msg.Assemblies) >= 1 then
    FAssemblyDetails.AssemblyId := msg.Assemblies[0]  //TODO: The rest of the selection
  else
    FAssemblyDetails.AssemblyId := 0;
end;

procedure TMainForm.miDISMImageCleanupClick(Sender: TObject);
begin
  StartProcess(GetSystemDir()+'\dism.exe',
    PChar('dism.exe /Online /Cleanup-Image /StartComponentCleanup'));
end;

procedure TMainForm.miOpenComponentsKeyClick(Sender: TObject);
begin
  if not IsComponentsHiveLoaded then
    LoadComponentsHive();
  RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsComponentsKey);
end;

procedure TMainForm.miOpenDeploymentsKeyClick(Sender: TObject);
begin
  if not IsComponentsHiveLoaded then
    LoadComponentsHive();
  RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsDeploymentsKey);
end;

procedure TMainForm.miOpenSxSFolderClick(Sender: TObject);
begin
  ShellOpen(SxSDir());
end;

procedure SaveStreamToFile(AStream: TStream; const AFilename: string);
var fp: TFileStream;
begin
  fp := TFileStream.Create(AFilename, fmCreate);
  try
    fp.CopyFrom(AStream, AStream.Size);
  finally
    FreeAndNil(fp);
  end;
end;

procedure TMainForm.Expandfile1Click(Sender: TObject);
var data: TStream;
begin
  if not OpenAnyFileDialog.Execute() then
    exit;
  data := OpenSxSFile(OpenAnyFileDialog.FileName);
  try
    SaveAnyFileDialog.Filename := OpenAnyFileDialog.FileName;
    if SaveAnyFileDialog.Execute then
      SaveStreamToFile(data, SaveAnyFileDialog.FileName)
  finally
    FreeAndNil(data);
  end;
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

procedure TMainForm.Installassembly1Click(Sender: TObject);
var ACache: IAssemblyCache;
  ref: FUSION_INSTALL_REFERENCE;
begin
  if not OpenManifestDialog.Execute then exit;

  FillChar(ref, sizeof(ref), 0);
  ref.cbSize := sizeof(ref);
  ref.guidScheme := FUSION_REFCOUNT_OPAQUE_STRING_GUID;
  ref.szIdentifier := PChar('manifestenum');

  OleCheck(CreateAssemblyCache(ACache, 0));
  OleCheck(ACache.InstallAssembly(0, PChar(OpenManifestDialog.FileName), @ref));
end;

function NameVerStr(const Assembly: TAssemblyData): string;
begin
  Result := Assembly.identity.name+'_'+Assembly.identity.version;
end;

procedure TMainForm.miUninstallByListClick(Sender: TObject);
var Lines: TStringList;
  line: string;
  List: TAssemblyList;
  i, j: integer;
  ACache: IAssemblyCache;
  Assembly: TAssemblyData;
  uresult: ULong;
  IsDeployment: boolean;
  hr: HRESULT;
begin
  if not OpenListDialog.Execute then
    exit;

  LogForm.Show;
  LogForm.Clear;
  List := TAssemblyList.Create;
  try

    LogForm.Log('Building assembly list...');
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(OpenListDialog.FileName);
      for i := 0 to Lines.Count-1 do begin
        line := Lines[i];
        j := pos('#', line);  //remove comments
        if j > 0 then
          line := Copy(line, 1, j-1);
        line := Trim(line);
        if line = '' then continue;
        FDb.Assemblies.GetNameLike(line.Replace('*','%'), List);
      end;
    finally
      FreeAndNil(Lines);
    end;

    LogForm.Log(IntToStr(List.Count)+' assemblies found.');
    for Assembly in List.Values do
      LogForm.Log(Assembly.identity.ToString);

    if List.Count <= 0 then begin
      MessageBox(Self.Handle, PChar('Nothing to remove'), PChar('Batch uninstall'), MB_OK);
      exit;
    end;

    isDeployment := true;
    for Assembly in List.Values do
      if not Assembly.isDeployment then
        isDeployment := false;
    if (not isDeployment) and (MessageBox(Self.Handle,
      PChar(sConfirmUninstallNonDeployments),
      PChar('Batch uninstall'), MB_YESNO or MB_ICONQUESTION) <> ID_YES) then
      exit;

    if MessageBox(Self.Handle, PChar(IntToStr(List.Count)+' assemblies will be removed. Continue?'),
      PChar('Batch uninstall'), MB_ICONQUESTION+MB_YESNO) <> ID_YES then
      exit;

    OleCheck(CreateAssemblyCache(ACache, 0));

    if not IsComponentsHiveLoaded then
      LoadComponentsHive();

    for Assembly in List.Values do begin
      LogForm.Log('Uninstalling '+NameVerStr(Assembly)+'...');
      IsDeployment := SxsIsDeployment(Assembly.identity, Assembly.manifestName);
      if (not IsDeployment) and miForceUninstall.Checked then begin
        SxsConvertIntoDeployment(Assembly.identity, Assembly.manifestName);
        IsDeployment := true;
      end;
      if IsDeployment then
        SxsDeploymentAddUninstallSource(Assembly.identity, Assembly.manifestName);
      hr := ACache.UninstallAssembly(0, PChar(Assembly.identity.ToStrongName), nil, @uresult);
      LogForm.Log(NameVerStr(Assembly)+': 0x'+IntToHex(hr, 8)+', '+IntToStr(uresult));
    end;

  finally
    FreeAndNil(List);
  end;
end;

procedure TMainForm.miVerifyHashesClick(Sender: TObject);
var List: TAssemblyList;
  Assembly: TAssemblyData;
  h1, h2: string;
begin
  List := TAssemblyList.Create;
  try
    FDb.Assemblies.GetAllAssemblies(List);
    for Assembly in List.Values do begin
      if Assembly.manifestName = '' then continue;
      h1 := IntToHex(SxsHashIdentity(Assembly.identity), 16).ToLower;
      h2 := SxsExtractHash(Assembly.manifestName);
      if not SameStr(h1, h2) then
        LogForm.Log(Assembly.identity.ToString+': hashes differ');
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TMainForm.Queryassemblyscavener1Click(Sender: TObject);
var ACache: IAssemblyCache;
  AScavenger: IInterface;
begin
   OleCheck(CreateAssemblyCache(ACache, 0));
   OleCheck(ACache.Reserved(AScavenger)); //it's E_NOTIMPL at the time of writing
end;

end.
