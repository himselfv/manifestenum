unit ManifestEnum.AssemblyActions;

interface

uses
  SysUtils, Classes, Vcl.Menus, Vcl.Dialogs, AssemblyDb, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TAssemblyProc = reference to procedure(const Assembly: TAssemblyData);
  TAssemblyStrFunc = reference to function(const Assembly: TAssemblyData): string;

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

  TAssemblyActions = class(TDataModule)
    PopupMenu: TPopupMenu;
    miCopy: TMenuItem;
    miCopyAssemblyName: TMenuItem;
    miCopyAssemblyDisplayName: TMenuItem;
    miCopyAssemblyStrongName: TMenuItem;
    miCopyAssemblyManifestName: TMenuItem;
    miCopyComponentKeyform: TMenuItem;
    miCopyDeploymentKeyform: TMenuItem;
    miCopyHash: TMenuItem;
    miCopyVersionlessHash: TMenuItem;
    miExport: TMenuItem;
    miExportManifest: TMenuItem;
    miExportPackageData: TMenuItem;
    miJumpTo: TMenuItem;
    miJumpToComponentKey: TMenuItem;
    miJumpToDeploymentKey: TMenuItem;
    miDebug: TMenuItem;
    miGetAssemblySize: TMenuItem;
    miProbeInstallation: TMenuItem;
    miConvertIntoDeployment: TMenuItem;
    miUninstallAssembly: TMenuItem;
    SaveManifestDialog: TSaveDialog;
    miJumpToSxsStore: TMenuItem;
    procedure miCopyAssemblyNameClick(Sender: TObject);
    procedure miCopyAssemblyDisplayNameClick(Sender: TObject);
    procedure miCopyAssemblyStrongNameClick(Sender: TObject);
    procedure miCopyAssemblyManifestNameClick(Sender: TObject);
    procedure miCopyComponentKeyformClick(Sender: TObject);
    procedure miCopyDeploymentKeyformClick(Sender: TObject);
    procedure miCopyHashClick(Sender: TObject);
    procedure miCopyVersionlessHashClick(Sender: TObject);
    procedure miJumpToComponentKeyClick(Sender: TObject);
    procedure miJumpToDeploymentKeyClick(Sender: TObject);
    procedure miExportManifestClick(Sender: TObject);
    procedure miExportPackageDataClick(Sender: TObject);
    procedure miGetAssemblySizeClick(Sender: TObject);
    procedure miConvertIntoDeploymentClick(Sender: TObject);
    procedure miUninstallAssemblyClick(Sender: TObject);
    procedure miProbeInstallationClick(Sender: TObject);
    procedure miJumpToSxsStoreClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FSelectedAssemblyIDs: TArray<TAssemblyId>;
    FForceUninstall: boolean;
    function Handle: THandle;
    procedure SaveManifest(const AManifestName: string; const ATargetName: string);
  public
    procedure SetSelectedAssembly(Item: TAssemblyId);
    procedure SetSelectedAssemblies(Items: TArray<TAssemblyId>);
    function SelectedAssemblies: TAssemblyEnum;
    function SelectedSingleAssemblyID: TAssemblyId;
    function GetSelectedSingleAssembly(out AAssembly: TAssemblyData): boolean;
    procedure ForEachSelected(AFunc: TAssemblyProc);
    function ForEachSelectedJoin(AFunc: TAssemblyStrFunc; ASep: string = #13): string;
    property Db: TAssemblyDb read FDb write FDb;
    property SelectedAssemblyIDs: TArray<TAssemblyId> read FSelectedAssemblyIDs;
    property ForceUninstall: boolean read FForceUninstall write FForceUninstall;
  end;


var
  AssemblyActions: TAssemblyActions;


resourcestring
  sConfirmUninstallNonDeployments =
    'You''re trying to uninstall some assemblies that aren''t deployments.'#13
    +'Only deployment assemblies can be uninstalled with SxS. The program can automatically convert '
    +'assemblies into deployments for you if you enable Force Uninstall in Options.'#13
    +'Do you want to continue anyway and try to uninstall all assemblies?';

implementation
uses Windows, Types, Clipbrd, IOUtils, ComObj, OsUtils, SxsUtils, SxSExpand, WinSxS,
  ManifestEnum.Log;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}


//Returns a handle to a window over which message boxes and other UI should pop up.
//On a normal form, that's a form itself. DataModule has no such Handle and we have to have a custom
//mechanic for guessing the form we're currently working with, or using main form.
function TAssemblyActions.Handle: THandle;
begin
  Result := 0; //TODO
end;


procedure TAssemblyActions.SetSelectedAssemblies(Items: TArray<TAssemblyId>);
begin
  FSelectedAssemblyIDs := Items;
end;

procedure TAssemblyActions.SetSelectedAssembly(Item: TAssemblyId);
var arr: TArray<TAssemblyId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedAssemblies(arr);
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

//Returns a enumerator of all selected assemblies
function TAssemblyActions.SelectedAssemblies: TAssemblyEnum;
begin
  Result.FIds := SelectedAssemblyIDs;
  Result.FDb := Self.FDb;
end;

//Returns ID of the only selected assembly, or 0 if none or multiple are selected
function TAssemblyActions.SelectedSingleAssemblyID: TAssemblyId;
var AIds: TArray<TAssemblyId>;
begin
  AIDs := SelectedAssemblyIDs;
  if Length(AIDs) = 1 then
    Result := AIDs[0]
  else
    Result := 0;
end;

function TAssemblyActions.GetSelectedSingleAssembly(out AAssembly: TAssemblyData): boolean;
var AId: TAssemblyId;
begin
  AId := SelectedSingleAssemblyId;
  Result := AId > 0;
  if Result then
    AAssembly := FDb.Assemblies.GetAssembly(AId);
end;

//Calls AFunc for each selected Assembly
procedure TAssemblyActions.ForEachSelected(AFunc: TAssemblyProc);
var AIds: TArray<TAssemblyId>;
  i: integer;
begin
  AIDs := SelectedAssemblyIDs;
  for i := 0 to Length(AIds)-1 do
    AFunc(FDb.Assemblies.GetAssembly(AIds[i]));
end;

//Calls AFunc for each selected assembly and joins the results with ASep
function TAssemblyActions.ForEachSelectedJoin(AFunc: TAssemblyStrFunc; ASep: string = #13): string;
var AIds: TArray<TAssemblyId>;
  i: integer;
begin
  Result := '';
  AIDs := SelectedAssemblyIDs;
  for i := 0 to Length(AIds)-1 do
    Result := Result + AFunc(FDb.Assemblies.GetAssembly(AIds[i])) + ASep;
  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result)-Length(ASep));
end;


procedure TAssemblyActions.miCopyAssemblyNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.name;
  end);
end;

procedure TAssemblyActions.miCopyAssemblyDisplayNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.ToString;
  end);
end;

procedure TAssemblyActions.miCopyAssemblyStrongNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.identity.ToStrongName;
  end);
end;

procedure TAssemblyActions.miCopyAssemblyManifestNameClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := Assembly.manifestName;
  end);
end;

procedure TAssemblyActions.miCopyComponentKeyformClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := SxsComponentKeyform(Assembly.identity);
  end);
end;

procedure TAssemblyActions.miCopyDeploymentKeyformClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := SxsDeploymentKeyform(Assembly.identity);
  end);
end;

procedure TAssemblyActions.miCopyHashClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := IntToHex(SxsHashIdentity(Assembly.identity), 16);
  end);
end;

procedure TAssemblyActions.miCopyVersionlessHashClick(Sender: TObject);
begin
  Clipboard.AsText := ForEachSelectedJoin(function(const Assembly: TAssemblyData): string begin
    Result := IntToHex(SxsHashIdentity(Assembly.identity, true), 16);
  end);
end;

procedure TAssemblyActions.miJumpToComponentKeyClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  if GetSelectedSingleAssembly(Assembly) then
    RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsComponentsKey+'\'+Assembly.manifestName);
end;

procedure TAssemblyActions.miJumpToDeploymentKeyClick(Sender: TObject);
var Assembly: TAssemblyData;
begin
  if GetSelectedSingleAssembly(Assembly) then
    RegeditOpenAndNavigate('HKEY_LOCAL_MACHINE\'+sSxsDeploymentsKey+'\'+SxsDeploymentKeyform(Assembly.identity));
end;

procedure TAssemblyActions.miJumpToSxsStoreClick(Sender: TObject);
var Assembly: TAssemblyData;
  Path: string;
begin
  if GetSelectedSingleAssembly(Assembly) then
    OsUtils.ShellOpen(SxsDir() + '\' + Assembly.manifestName + '\');
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

procedure TAssemblyActions.SaveManifest(const AManifestName: string; const ATargetName: string);
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

procedure TAssemblyActions.miExportManifestClick(Sender: TObject);
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

procedure TAssemblyActions.miExportPackageDataClick(Sender: TObject);
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




procedure TAssemblyActions.miGetAssemblySizeClick(Sender: TObject);
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

procedure TAssemblyActions.miProbeInstallationClick(Sender: TObject);
var Assembly: TAssemblyData;
  Buf: array[0..4095] of byte;
begin
  FillChar(Buf[0], Length(Buf), $CF);
  for Assembly in SelectedAssemblies do begin
    OleCheck(SxsProbeAssemblyInstallation(0, PChar(Assembly.identity.ToStrongName), @Buf[0]));
  end;
end;

procedure TAssemblyActions.miConvertIntoDeploymentClick(Sender: TObject);
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

procedure TAssemblyActions.miUninstallAssemblyClick(Sender: TObject);
var Assembly: TAssemblyData;
  ACache: IAssemblyCache;
  AssemblyNames: string;
  ResultText: string;
  uresult: ULong;
  IsDeployment: boolean;
  hr: HRESULT;
begin
  AssemblyNames := '';
  for Assembly in SelectedAssemblies do
    AssemblyNames := AssemblyNames+'  '+Assembly.identity.name+#13;
  if AssemblyNames = '' then exit; //no assemblies selected

  isDeployment := true;
  for Assembly in SelectedAssemblies do
    if not Assembly.isDeployment then
      isDeployment := false;
  if (not isDeployment) and (MessageBox(Self.Handle,
    PChar(sConfirmUninstallNonDeployments),
    PChar('Confirm uninstall'), MB_YESNO or MB_ICONQUESTION) <> ID_YES) then
    exit;

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
    if (not IsDeployment) and ForceUninstall then begin
      SxsConvertIntoDeployment(Assembly.identity, Assembly.manifestName);
      IsDeployment := true;
    end;
    if IsDeployment then
      SxsDeploymentAddUninstallSource(Assembly.identity, Assembly.manifestName);
    hr := ACache.UninstallAssembly(0, PChar(Assembly.identity.ToStrongName), nil, @uresult);
    OleCheck(hr);
    ResultText := ResultText+'  '+Assembly.identity.name+': '+IntToStr(uresult)+#13;
    LogForm.Log('UninstallAssembly('+Assembly.identity.name+'): 0x'+IntToHex(hr, 8)+' (state '+IntToStr(uresult)+')');
  end;

  MessageBox(Self.Handle, PChar('Uninstall results:'#13 + ResultText
    +'SxS uninstall results often do not reflect actual success or failure.'),
    PChar('Done'), MB_OK);
end;


end.
