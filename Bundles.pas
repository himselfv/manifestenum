unit Bundles;
{
Manages assembly "bundles".
Bundles are groupings one level above components and assemblies, introduced manually.
}

interface
uses SysUtils, Classes, Generics.Collections, AssemblyDb.Assemblies;

type
  TBundleManager = class;

  TBundle = class
  protected
    FName: string;
    FMasks: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const AFilename: string);
    function ContainsAssembly(const Id: TAssemblyIdentity): boolean;
    property Name: string read FName;
  end;

  TBundleFolder = class
  protected
    FName: string;
    FSubfolders: TList<TBundleFolder>;
    FBundles: TList<TBundle>;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Subfolders: TList<TBundleFolder> read FSubfolders;
    property Bundles: TList<TBundle> read FBundles;
  end;

  TBundleManager = class
  protected
    FFolders: TObjectList<TBundleFolder>;
    FBundles: TObjectList<TBundle>;
    FRoot: TBundleFolder;
    FAllPatterns: TDictionary<string, TBundle>;
    FAllAssemblies: TDictionary<TAssemblyId, TBundle>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ResetAssemblies;
    procedure Load(const ADir: string); overload;
    function LoadFolder(const ADir: string): TBundleFolder;
    function LoadBundle(const AFilename: string): TBundle;
    function MatchAssembly(const Id: TAssemblyIdentity): TBundle;
    procedure RegisterAssembly(AId: TAssemblyId; const AIdentity: TAssemblyIdentity);
    function GetAssemblyBundle(AId: TAssemblyId): TBundle;
    property Folders: TObjectList<TBundleFolder> read FFolders;
    property Bundles: TObjectList<TBundle> read FBundles;
  end;

var
  BundleMgr: TBundleManager;


implementation
uses WildcardMatching;

const
  BUNDLE_EXT = '.txt';

constructor TBundle.Create;
begin
  inherited;
  FMasks := TStringList.Create;
end;

destructor TBundle.Destroy;
begin
  FreeAndNil(FMasks);
  inherited;
end;

procedure TBundle.Load(const AFilename: string);
var i, i_pos: integer;
  ln: string;
begin
  FMasks.LoadFromFile(AFilename);

  for i := FMasks.Count-1 downto 0 do begin
    ln := FMasks[i];
    //Remove comments
    i_pos := pos('#', ln);
    if i_pos > 0 then begin
      SetLength(ln, i_pos-1);
    end;
    ln := ln.Trim;

    if ln = '' then begin
      FMasks.Delete(i);
      continue;
    end;
    FMasks[i] := ln; //without comments
  end;

  FName := ChangeFileExt(ExtractFilename(AFilename), '');
end;

function TBundle.ContainsAssembly(const Id: TAssemblyIdentity): boolean;
var i: integer;
begin
  for i := 0 to FMasks.Count-1 do
   //For now we only match by name. Versions and cultures are ignored. Most of the time that's what we want anyway.
    if WildcardMatchCase(PChar(Id.name), PChar(FMasks[i])) then begin
      Result := true;
      exit;
    end;
  Result := false;
end;

constructor TBundleFolder.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FSubfolders := TList<TBundleFolder>.Create;
  FBundles := TList<TBundle>.Create;
end;

destructor TBundleFolder.Destroy;
begin
  FreeAndNil(FBundles);
  FreeAndNil(FSubfolders);
  inherited;
end;

constructor TBundleManager.Create;
begin
  inherited;
  FFolders := TObjectList<TBundleFolder>.Create;
  FBundles := TObjectList<TBundle>.Create;
  FAllPatterns := TDictionary<string, TBundle>.Create;
  FAllAssemblies := TDictionary<TAssemblyId, TBundle>.Create;
end;

destructor TBundleManager.Destroy;
begin
  FreeAndNil(FAllAssemblies);
  FreeAndNil(FAllPatterns);
  FreeAndNil(FBundles);
  FreeAndNil(FFolders);
  inherited;
end;

procedure TBundleManager.Clear;
begin
  FAllAssemblies.Clear;
  FAllPatterns.Clear;
  FFolders.Clear;
  FBundles.Clear;
end;

procedure TBundleManager.ResetAssemblies;
begin
  FAllAssemblies.Clear;
end;

procedure TBundleManager.Load(const ADir: string);
begin
  Clear;
  FRoot := LoadFolder(ADir);
end;

function TBundleManager.LoadFolder(const ADir: string): TBundleFolder;
var sr: TSearchRec;
  res: integer;
begin
  Result := nil;
  res := FindFirst(ADir+'\*.*', faAnyFile, sr);
  if res <> 0 then exit;
  try
    Result := TBundleFolder.Create(ExtractFilename(ADir));
    Self.Folders.Add(Result);
    while res = 0 do begin
      //File
      if (sr.Attr and faDirectory = 0) then begin
        if ExtractFileExt(sr.Name)=BUNDLE_EXT then
          Result.Bundles.Add(LoadBundle(ADir+'\'+sr.Name));
        //else ignore file
      end else
      //Directory
      if (sr.Name <> '.') and (sr.Name <> '..') then
        Result.Subfolders.Add(LoadFolder(ADir+'\'+sr.Name));
      res := FindNext(sr);
    end;
  finally
    SysUtils.FindClose(sr);
  end;
end;

function TBundleManager.LoadBundle(const AFilename: string): TBundle;
begin
  Result := TBundle.Create;
  Self.Bundles.Add(Result);
  Result.Load(AFilename);
end;

function TBundleManager.MatchAssembly(const Id: TAssemblyIdentity): TBundle;
var Bundle: TBundle;
begin
  for Bundle in Bundles do
    if Bundle.ContainsAssembly(Id) then begin
      Result := Bundle;
      exit;
    end;
  Result := nil;
end;

procedure TBundleManager.RegisterAssembly(AId: TAssemblyId; const AIdentity: TAssemblyIdentity);
var Bundle: TBundle;
begin
  Bundle := MatchAssembly(AIdentity);
  FAllAssemblies.AddOrSetValue(AId, Bundle); //even if nil
end;

function TBundleManager.GetAssemblyBundle(AId: TAssemblyId): TBundle;
begin
  Result := FAllAssemblies[AId];
end;

end.
