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
    Savemanifest1: TMenuItem;
    SaveManifestDialog: TSaveDialog;
    Uninstallassembly1: TMenuItem;
    Getassemblysize1: TMenuItem;
    Copy1: TMenuItem;
    Assemblyname1: TMenuItem;
    Assemblystrongname1: TMenuItem;
    Assemblydisplayname1: TMenuItem;
    Splitter1: TSplitter;
    Installassembly1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Loadmanifestfile1Click(Sender: TObject);
    procedure Savemanifest1Click(Sender: TObject);
    procedure Uninstallassembly1Click(Sender: TObject);
    procedure Getassemblysize1Click(Sender: TObject);
    procedure Assemblyname1Click(Sender: TObject);
    procedure Assemblydisplayname1Click(Sender: TObject);
    procedure Assemblystrongname1Click(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
    procedure Installassembly1Click(Sender: TObject);
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
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, AssemblyDbBuilder, ManifestParser, SxSExpand,
  DelayLoadTree, AutorunsBrowser, ShellExtBrowser, winsxs, ComObj, Clipbrd;

{$R *.dfm}

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
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.name;
end;

procedure TMainForm.Assemblydisplayname1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToString;
end;


procedure TMainForm.Assemblystrongname1Click(Sender: TObject);
var AAssemblyId: TAssemblyId;
  AAssemblyData: TAssemblyData;
begin
  AAssemblyId := FAssemblyBrowser.SelectedAssembly;
  if AAssemblyId < 0 then
    exit;
  AAssemblyData := FDb.Assemblies.GetAssembly(AAssemblyId);
  Clipboard.AsText := AAssemblyData.identity.ToStrongName;
end;

procedure TMainForm.Getassemblysize1Click(Sender: TObject);
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

procedure TMainForm.Uninstallassembly1Click(Sender: TObject);
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

  OleCheck(CreateAssemblyCache(ACache, 0));
  OleCheck(ACache.UninstallAssembly(0, PChar(AAssemblyData.identity.ToStrongName), nil, @uresult));
  MessageBox(Self.Handle, PChar('Uninstall result: '+IntToStr(uresult)), PChar('Done'), MB_OK);
end;

end.
