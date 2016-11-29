unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Generics.Collections, Vcl.Menus, AssemblyDb, Vcl.ExtCtrls, Vcl.Buttons,
  AssemblyDetails, FileBrowser, RegistryBrowser, TaskBrowser, CategoryBrowser, AssemblyBrowser,
  AssemblyDb.Assemblies;

type
  PAssemblyEnum = ^TAssemblyEnum;
  TAssemblyEnumerator = record
    FEnum: PAssemblyEnum;
    FIndex: integer;
    function GetCurrent: TAssemblyData;
    function MoveNext: boolean;
    property Current: TAssemblyData read GetCurrent;
  end;
  TAssemblyEnum = record
    FIds: TArray<TAssemblyId>;
    FDb: TAssemblyDb;
    function GetEnumerator: TAssemblyEnumerator;
  end;

  TAssemblyProc = reference to procedure(const Assembly: TAssemblyData);
  TAssemblyStrFunc = reference to function(const Assembly: TAssemblyData): string;

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
    PopupMenu: TPopupMenu;
    miExportManifest: TMenuItem;
    SaveManifestDialog: TSaveDialog;
    miUninstallAssembly: TMenuItem;
    miGetAssemblySize: TMenuItem;
    Copy1: TMenuItem;
    miCopyAssemblyName: TMenuItem;
    miCopyAssemblyStrongName: TMenuItem;
    miCopyAssemblyDisplayName: TMenuItem;
    Splitter1: TSplitter;
    Installassembly1: TMenuItem;
    Expandfile1: TMenuItem;
    OpenAnyFileDialog: TOpenDialog;
    SaveAnyFileDialog: TSaveDialog;
    Export1: TMenuItem;
    miExportPackageData: TMenuItem;
    miCopyAssemblyManifestName: TMenuItem;
    Open1: TMenuItem;
    miJumpToComponentKey: TMenuItem;
    miJumpToDeploymentKey: TMenuItem;
    miConvertIntoDeployment: TMenuItem;
    miCopyComponentKeyform: TMenuItem;
    miCopyDeploymentKeyform: TMenuItem;
    Options1: TMenuItem;
    miForceUninstall: TMenuItem;
    N2: TMenuItem;
    miUninstallByList: TMenuItem;
    OpenListDialog: TOpenDialog;
    miAssemblyDatabase: TMenuItem;
    miRefreshAssemblyDatabase: TMenuItem;
    miCopyHash: TMenuItem;
    miCopyVersionlessHash: TMenuItem;
    miVerifyHashes: TMenuItem;
    N3: TMenuItem;
    miShowLog: TMenuItem;
    miShowInstalledOnly: TMenuItem;
    N4: TMenuItem;
    miShowDeploymentsOnly: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Loadmanifestfile1Click(Sender: TObject);
    procedure miExportManifestClick(Sender: TObject);
    procedure miUninstallAssemblyClick(Sender: TObject);
    procedure miGetAssemblySizeClick(Sender: TObject);
    procedure miCopyAssemblyNameClick(Sender: TObject);
    procedure miCopyAssemblyDisplayNameClick(Sender: TObject);
    procedure miCopyAssemblyStrongNameClick(Sender: TObject);
    procedure miCopyAssemblyManifestNameClick(Sender: TObject);
    procedure miCopyComponentKeyformClick(Sender: TObject);
    procedure miCopyDeploymentKeyformClick(Sender: TObject);
    procedure Installassembly1Click(Sender: TObject);
    procedure Expandfile1Click(Sender: TObject);
    procedure miExportPackageDataClick(Sender: TObject);
    procedure miJumpToComponentKeyClick(Sender: TObject);
    procedure miJumpToDeploymentKeyClick(Sender: TObject);
    procedure miConvertIntoDeploymentClick(Sender: TObject);
    procedure miUninstallByListClick(Sender: TObject);
    procedure miRefreshAssemblyDatabaseClick(Sender: TObject);
    procedure miCopyHashClick(Sender: TObject);
    procedure miCopyVersionlessHashClick(Sender: TObject);
    procedure miVerifyHashesClick(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure miShowInstalledOnlyClick(Sender: TObject);
    procedure miShowDeploymentsOnlyClick(Sender: TObject);

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
    procedure SaveManifest(const AManifestName: string; const ATargetName: string);

  protected
    //Settings
    procedure LoadSettings;
    procedure SaveSettings;

  protected
    //Assembly selection
    function SelectedAssemblyIDs: TArray<TAssemblyId>;
    function SelectedAssemblies: TAssemblyEnum;
    function SelectedSingleAssemblyID: TAssemblyId;
    function GetSelectedSingleAssembly(out AAssembly: TAssemblyData): boolean;
    procedure ForEachSelected(AFunc: TAssemblyProc);
    function ForEachSelectedJoin(AFunc: TAssemblyStrFunc; ASep: string = #13): string;

  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, OsUtils, SxsUtils, AclHelpers, AssemblyDbBuilder, ManifestParser, SxSExpand,
  DelayLoadTree, AutorunsBrowser, ShellExtBrowser, winsxs, ComObj, Clipbrd,
  IOUtils, Types, ServiceBrowser, Registry, ManifestEnum.Log, CommonFilters;

{$R *.dfm}
{$WARN SYMBOL_PLATFORM OFF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  InitAssemblyDb(FDb, AppFolder+'\assembly.db', true);

  FAssemblyBrowser := TAssemblyBrowserForm.Create(Application);
  FAssemblyBrowser.OnSelectionChanged := Self.AssemblyBrowserSelectionChanged;
  FAssemblyBrowser.Tree.PopupMenu := Self.PopupMenu;
  AddPage(FAssemblyBrowser);

  FCategoryBrowser := TCategoryBrowserForm.Create(Application);
  FCategoryBrowser.Db := FDb;
  AddPage(FCategoryBrowser);

  FFileBrowser := TFileBrowserForm.Create(Application);
  AddPage(FFileBrowser);

  FRegistryBrowser := TRegistryBrowserForm.Create(Application);
  AddPage(FRegistryBrowser);

  AddPage(TServiceBrowserForm.Create(Application));

  FTaskBrowser := TTaskBrowserForm.Create(Application);
  AddPage(FTaskBrowser);

  AddPage(TAutorunsBrowserForm.Create(Application));
  AddPage(TShellExtensionBrowserForm.Create(Application));

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

procedure TMainForm.LoadSettings;
var ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create('ManifestEnum');
  try
    miForceUninstall.Checked := ini.ReadBool('', 'ForceUninstall', false);
    CommonFilters.ShowInstalledOnly := ini.ReadBool('Filter', 'ShowInstalledOnly', true);
    CommonFilters.ShowDeploymentsOnly := ini.ReadBool('Filter', 'ShowDeploymentsOnly', false);
    miShowInstalledOnly.Checked := CommonFilters.ShowInstalledOnly;
    miShowDeploymentsOnly.Checked := CommonFilters.ShowDeploymentsOnly;
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
    ini.WriteBool('', 'ForceUninstall', miForceUninstall.Checked);
    ini.WriteBool('Filter', 'ShowInstalledOnly', CommonFilters.ShowInstalledOnly);
    ini.WriteBool('Filter', 'ShowDeploymentsOnly', CommonFilters.ShowDeploymentsOnly);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainForm.SettingsChanged(Sender: TObject);
begin
  SaveSettings;
end;

procedure TMainForm.miShowInstalledOnlyClick(Sender: TObject);
begin
  CommonFilters.ShowInstalledOnly := miShowInstalledOnly.Checked;
  SaveSettings;
  FilterChanged(nil);
end;

procedure TMainForm.miShowDeploymentsOnlyClick(Sender: TObject);
begin
  CommonFilters.ShowDeploymentsOnly := miShowDeploymentsOnly.Checked;
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

procedure TMainForm.AssemblyBrowserSelectionChanged(Sender: TObject);
var AIds: TArray<TAssemblyId>;
begin
  AIds := FAssemblyBrowser.SelectedAssemblies;
  if Length(AIds) = 1 then
    FAssemblyDetails.AssemblyId := AIds[0]
  else
    FAssemblyDetails.AssemblyId := 0;
end;

//Returns a list of all selected assembly IDs
function TMainForm.SelectedAssemblyIDs: TArray<TAssemblyId>;
begin
  Result := FAssemblyBrowser.SelectedAssemblies;
end;

//Returns a enumerator of all selected assemblies
function TMainForm.SelectedAssemblies: TAssemblyEnum;
begin
  Result.FIds := FAssemblyBrowser.SelectedAssemblies;
  Result.FDb := Self.FDb;
end;

//Returns ID of the only selected assembly, or 0 if none or multiple are selected
function TMainForm.SelectedSingleAssemblyID: TAssemblyId;
var AIds: TArray<TAssemblyId>;
begin
  AIDs := FAssemblyBrowser.SelectedAssemblies;
  if Length(AIDs) = 1 then
    Result := AIDs[0]
  else
    Result := 0;
end;

function TMainForm.GetSelectedSingleAssembly(out AAssembly: TAssemblyData): boolean;
var AId: TAssemblyId;
begin
  AId := SelectedSingleAssemblyId;
  Result := AId > 0;
  if Result then
    AAssembly := FDb.Assemblies.GetAssembly(AId);
end;

function TAssemblyEnum.GetEnumerator: TAssemblyEnumerator;
begin
  Result.FEnum := @Self;
  Result.FIndex := -1;
end;

function TAssemblyEnumerator.GetCurrent: TAssemblyData;
begin
  Result := FEnum.FDb.Assemblies.GetAssembly(FEnum.FIds[FIndex]);
end;

function TAssemblyEnumerator.MoveNext: boolean;
begin
  Result := FIndex < Length(FEnum.FIds)-1;
  if Result then
    Inc(FIndex);
end;

//Calls AFunc for each selected Assembly
procedure TMainForm.ForEachSelected(AFunc: TAssemblyProc);
var AIds: TArray<TAssemblyId>;
  i: integer;
begin
  AIDs := FAssemblyBrowser.SelectedAssemblies;
  for i := 0 to Length(AIds)-1 do
    AFunc(FDb.Assemblies.GetAssembly(AIds[i]));
end;

//Calls AFunc for each selected assembly and joins the results with ASep
function TMainForm.ForEachSelectedJoin(AFunc: TAssemblyStrFunc; ASep: string = #13): string;
var AIds: TArray<TAssemblyId>;
  i: integer;
begin
  Result := '';
  AIDs := FAssemblyBrowser.SelectedAssemblies;
  for i := 0 to Length(AIds)-1 do
    Result := Result + AFunc(FDb.Assemblies.GetAssembly(AIds[i])) + ASep;
  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result)-Length(ASep));
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

procedure TMainForm.miExportManifestClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  for Assembly in SelectedAssemblies do begin
    if Assembly.manifestName = '' then begin
      MessageBox(Self.Handle, PChar(Assembly.identity.name+': this assembly has no associated manifest'),
        PChar('Cannot save manifest'), MB_OK + MB_ICONEXCLAMATION);
      exit;
    end;

    SaveManifestDialog.Filename := Assembly.manifestName+'.manifest';
    if not SaveManifestDialog.Execute then
      exit;

    SaveManifest(Assembly.manifestName, SaveManifestDialog.Filename);
  end;
end;

procedure TMainForm.SaveManifest(const AManifestName: string; const ATargetName: string);
var AManifestPath: string;
  AData: UTF8String;
  fp: TFileStream;
begin
  AManifestPath := GetWindowsDir()+'\WinSxS\Manifests\'+AManifestName+'.manifest';

  AData := UTF8String(LoadManifestFile(AManifestPath));

  fp := TFileStream.Create(ATargetName, fmCreate);
  try
    fp.Write(AData[1], Length(AData));
  finally
    FreeAndNil(fp);
  end;
end;

procedure TMainForm.miExportPackageDataClick(Sender: TObject);
var Assembly: TAssemblyData;
  ATargetFolder: string;
  AFileDir: string;
  AFiles: TStringDynArray;
  AInFile, AOutFile: string;
  AStream: TStream;
begin
  for Assembly in SelectedAssemblies do begin
    if Assembly.manifestName = '' then begin
      MessageBox(Self.Handle, PChar(Assembly.identity.name+': this assembly has no associated manifest'),
        PChar('Cannot save manifest'), MB_OK + MB_ICONEXCLAMATION);
      exit;
    end;

    with TFileOpenDialog.Create(nil) do
    try
      Options := [fdoPickFolders];
      if not Execute then exit;
      ATargetFolder := Filename;
    finally
      Free;
    end;

    SaveManifest(Assembly.manifestName, ATargetFolder+'\'+Assembly.manifestName+'.manifest');

    AFileDir := GetWindowsDir()+'\WinSxS\'+Assembly.manifestName;
    if not DirectoryExists(AFileDir) then
      exit;

    AFiles := TDirectory.GetFiles(AFileDir, '*.*', TSearchOption.soAllDirectories);
    for AInFile in AFiles do begin
      AStream := OpenSxSFile(AInFile);
      try
        AOutFile := TPath.Combine(ATargetFolder, TPath.GetFileName(AInFile));
        SaveStreamToFile(AStream, AOutFile);
      finally
        FreeAndNil(AStream);
      end;
    end;

  end;
end;


procedure TMainForm.miCopyAssemblyNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.name;
  end);
end;

procedure TMainForm.miCopyAssemblyDisplayNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.ToString;
  end);
end;

procedure TMainForm.miCopyAssemblyStrongNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.ToStrongName;
  end);
end;

procedure TMainForm.miCopyAssemblyManifestNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.manifestName;
  end);
end;

procedure TMainForm.miCopyComponentKeyformClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := SxsComponentKeyform(Assembly.identity);
  end);
end;

procedure TMainForm.miCopyDeploymentKeyformClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := SxsDeploymentKeyform(Assembly.identity);
  end);
end;

procedure TMainForm.miCopyHashClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := IntToHex(SxsHashIdentity(Assembly.identity), 16);
  end);
end;

procedure TMainForm.miCopyVersionlessHashClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := IntToHex(SxsHashIdentity(Assembly.identity, true), 16);
  end);
end;

procedure TMainForm.miJumpToComponentKeyClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  if GetSelectedSingleAssembly(Assembly) then
    RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsComponentsKey+'\'+Assembly.manifestName);
end;

procedure TMainForm.miJumpToDeploymentKeyClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  if GetSelectedSingleAssembly(Assembly) then
    RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsDeploymentsKey+'\'+SxsDeploymentKeyform(Assembly.identity));
end;

procedure TMainForm.miGetAssemblySizeClick(Sender: TObject);
var Assembly: TAssemblyData;
  ACache: IAssemblyCache;
  AInfo: ASSEMBLY_INFO;
begin
  if not GetSelectedSingleAssembly(Assembly) then
    exit;

  OleCheck(CreateAssemblyCache(ACache, 0));
  FillChar(AInfo, SizeOf(AInfo), 0);
  AInfo.cbAssemblyInfo := SizeOf(AInfo);
  AInfo.dwAssemblyFlags := QUERYASMINFO_FLAG_GETSIZE;
  OleCheck(ACache.QueryAssemblyInfo(0, PChar(Assembly.identity.ToStrongName), @AInfo));

  MessageBox(Self.Handle, PChar('Assembly size: '+IntToStr(AInfo.uliAssemblySizeInKB.QuadPart)+' Kb'),
    PChar('Assembly info'), MB_OK);
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

procedure TMainForm.miUninstallAssemblyClick(Sender: TObject);
var Assembly: TAssemblyData;
  ACache: IAssemblyCache;
  AssemblyNames: string;
  ResultText: string;
  uresult: ULong;
  IsDeployment: boolean;
begin
  AssemblyNames := '';
  for Assembly in SelectedAssemblies do
    AssemblyNames := AssemblyNames+'  '+Assembly.identity.name+#13;
  if AssemblyNames = '' then exit; //no assemblies selected

  if MessageBox(Self.Handle, PChar('You are going to uninstall these assemblies: '#13
    +AssemblyNames
    +'Do you really want to continue?'),
    PChar('Confirm uninstall'), MB_ICONQUESTION + MB_YESNO) <> ID_YES then
    exit;

  OleCheck(CreateAssemblyCache(ACache, 0));

  if not IsComponentsHiveLoaded then
    LoadComponentsHive();

  ResultText := '';
  for Assembly in SelectedAssemblies do begin
    IsDeployment := SxsIsDeployment(Assembly.identity, Assembly.manifestName);
    if (not IsDeployment) and miForceUninstall.Checked then begin
      SxsConvertIntoDeployment(Assembly.identity, Assembly.manifestName);
      IsDeployment := true;
    end;
    if IsDeployment then
      SxsDeploymentAddUninstallSource(Assembly.identity, Assembly.manifestName);
    OleCheck(ACache.UninstallAssembly(0, PChar(Assembly.identity.ToStrongName), nil, @uresult));
    ResultText := ResultText+'  '+Assembly.identity.name+': '+IntToStr(uresult)+#13;
  end;

  MessageBox(Self.Handle, PChar('Uninstall results:'#13 + ResultText
    +'SxS uninstall results often do not reflect actual success or failure.'),
    PChar('Done'), MB_OK);
end;

procedure TMainForm.miConvertIntoDeploymentClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  if not IsComponentsHiveLoaded then
    LoadComponentsHive();
  for Assembly in SelectedAssemblies do begin
    SxsConvertIntoDeployment(Assembly.identity, Assembly.manifestName);
    SxsDeploymentAddUninstallSource(Assembly.identity, Assembly.manifestName);
  end;
  MessageBox(Self.Handle, PChar('Conversion completed'), PChar('Done'), MB_OK);
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

end.
