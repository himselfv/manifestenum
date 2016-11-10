unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Generics.Collections, Vcl.Menus, AssemblyDb, Vcl.ExtCtrls, Vcl.Buttons,
  AssemblyDetails, FileBrowser, RegistryBrowser, TaskBrowser, CategoryBrowser, AssemblyBrowser,
  AssemblyDb.Assemblies;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    F1: TMenuItem;
    Exit1: TMenuItem;
    pmRebuildAssemblyDatabase: TMenuItem;
    N1: TMenuItem;
    Reload1: TMenuItem;
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
    cbCopyComponentKeyform: TMenuItem;
    cbCopyDeploymentKeyform: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Loadmanifestfile1Click(Sender: TObject);
    procedure miExportManifestClick(Sender: TObject);
    procedure miUninstallAssemblyClick(Sender: TObject);
    procedure miGetAssemblySizeClick(Sender: TObject);
    procedure miCopyAssemblyNameClick(Sender: TObject);
    procedure miCopyAssemblyDisplayNameClick(Sender: TObject);
    procedure miCopyAssemblyStrongNameClick(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure Installassembly1Click(Sender: TObject);
    procedure Expandfile1Click(Sender: TObject);
    procedure miExportPackageDataClick(Sender: TObject);
    procedure miCopyAssemblyManifestNameClick(Sender: TObject);
    procedure miJumpToComponentKeyClick(Sender: TObject);
    procedure miJumpToDeploymentKeyClick(Sender: TObject);
    procedure miConvertIntoDeploymentClick(Sender: TObject);
    procedure cbCopyComponentKeyformClick(Sender: TObject);
    procedure cbCopyDeploymentKeyformClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblyBrowser: TAssemblyBrowserForm;
    FAssemblyDetails: TAssemblyDetailsForm;
    FCategoryBrowser: TCategoryBrowserForm;
    FFileBrowser: TFileBrowserForm;
    FRegistryBrowser: TRegistryBrowserForm;
    FTaskBrowser: TTaskBrowserForm;
    procedure AddPage(const AForm: TForm);
    procedure AssemblyBrowserAssemblySelected(Sender: TObject; AAssembly: TAssemblyId);
    procedure SaveManifest(const AManifestName: string; const ATargetName: string);
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, OsUtils, SxsUtils, AclHelpers, AssemblyDbBuilder, ManifestParser, SxSExpand,
  DelayLoadTree, AutorunsBrowser, ShellExtBrowser, winsxs, ComObj, Clipbrd,
  IOUtils, Types;

{$R *.dfm}
{$WARN SYMBOL_PLATFORM OFF}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  InitAssemblyDb(FDb, AppFolder+'\assembly.db', true);

  FAssemblyBrowser := TAssemblyBrowserForm.Create(Application);
  FAssemblyBrowser.OnAssemblySelected := Self.AssemblyBrowserAssemblySelected;
  FAssemblyBrowser.Tree.PopupMenu := Self.PopupMenu;
  AddPage(FAssemblyBrowser);

  FCategoryBrowser := TCategoryBrowserForm.Create(Application);
  FCategoryBrowser.Db := FDb;
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
  FAssemblyBrowser.Reload;
end;

procedure TMainForm.Reload1Click(Sender: TObject);
begin
  FAssemblyBrowser.Reload;
end;

procedure TMainForm.AssemblyBrowserAssemblySelected(Sender: TObject; AAssembly: TAssemblyId);
begin
  FAssemblyDetails.AssemblyId := AAssembly;
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
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  if AAssemblyData.manifestName = '' then begin
    MessageBox(Self.Handle, PChar('This assembly has no associated manifest'), PChar('Cannot save manifest'), MB_OK + MB_ICONEXCLAMATION);
    exit;
  end;

  SaveManifestDialog.Filename := AAssemblyData.manifestName+'.manifest';
  if not SaveManifestDialog.Execute then
    exit;

  SaveManifest(AAssemblyData.manifestName, SaveManifestDialog.Filename);
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
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  ATargetFolder: string;
  AFileDir: string;
  AFiles: TStringDynArray;
  AInFile, AOutFile: string;
  AStream: TStream;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  if AAssemblyData.manifestName = '' then begin
    MessageBox(Self.Handle, PChar('This assembly has no associated manifest'), PChar('Cannot save data'), MB_OK + MB_ICONEXCLAMATION);
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

  SaveManifest(AAssemblyData.manifestName, ATargetFolder+'\'+AAssemblyData.manifestName+'.manifest');

  AFileDir := GetWindowsDir()+'\WinSxS\'+AAssemblyData.manifestName;
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


procedure TMainForm.miCopyAssemblyNameClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.name;
end;

procedure TMainForm.miCopyAssemblyDisplayNameClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToString;
end;

procedure TMainForm.miCopyAssemblyStrongNameClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToStrongName;
end;

procedure TMainForm.miCopyAssemblyManifestNameClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.manifestName;
end;

procedure TMainForm.cbCopyComponentKeyformClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := SxsComponentKeyform(AAssemblyData.identity, SxsExtractHash(AAssemblyData.manifestName));
end;

procedure TMainForm.cbCopyDeploymentKeyformClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := SxsDeploymentKeyform(AAssemblyData.identity, SxsExtractHash(AAssemblyData.manifestName));
end;


procedure TMainForm.miJumpToComponentKeyClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsComponentsKey+'\'+AAssemblyData.manifestName);
end;

procedure TMainForm.miJumpToDeploymentKeyClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsDeploymentsKey+'\'+SxsDeploymentKeyform(AAssemblyData.identity, SxsExtractHash(AAssemblyData.manifestName)));
end;

procedure TMainForm.miGetAssemblySizeClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  ACache: IAssemblyCache;
  AInfo: ASSEMBLY_INFO;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  OleCheck(CreateAssemblyCache(ACache, 0));
  FillChar(AInfo, SizeOf(AInfo), 0);
  AInfo.cbAssemblyInfo := SizeOf(AInfo);
  AInfo.dwAssemblyFlags := QUERYASMINFO_FLAG_GETSIZE;
  OleCheck(ACache.QueryAssemblyInfo(0, PChar(AAssemblyData.identity.ToStrongName), @AInfo));

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
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
  ACache: IAssemblyCache;
  uresult: ULong;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  if MessageBox(Self.Handle, PChar('You are going to uninstall '+AAssemblyData.identity.name+'.'#13
    +'Do you really want to continue?'),
    PChar('Confirm uninstall'), MB_ICONQUESTION + MB_YESNO) <> ID_YES then
    exit;

  if not IsComponentsHiveLoaded then
    LoadComponentsHive();
  if SxsIsDeployment(AAssemblyData.identity, AAssemblyData.manifestName) then
    SxsDeploymentAddUninstallSource(AAssemblyData.identity, AAssemblyData.manifestName);
  OleCheck(CreateAssemblyCache(ACache, 0));
  OleCheck(ACache.UninstallAssembly(0, PChar(AAssemblyData.identity.ToStrongName), nil, @uresult));
  MessageBox(Self.Handle, PChar('Uninstall result: '+IntToStr(uresult)+#13
    +'Note though that this result often does not reflect actual success or failure.'),
    PChar('Done'), MB_OK);
end;


procedure TMainForm.miConvertIntoDeploymentClick(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);

  if not IsComponentsHiveLoaded then
    LoadComponentsHive();
  SxsConvertIntoDeployment(AAssemblyData.identity, AAssemblyData.manifestName);
  SxsDeploymentAddUninstallSource(AAssemblyData.identity, AAssemblyData.manifestName);
  MessageBox(Self.Handle, PChar('Done'), PChar('Done'), MB_OK);
end;

end.
