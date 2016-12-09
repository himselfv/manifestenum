unit AssemblyDbBuilder;

{$DEFINE PROFILE}

{$DEFINE SAX}
//Use SAX parser instead of the DOM one

interface
uses AssemblyDb, {$IFDEF SAX}ManifestSaxParser{$ELSE}ManifestParser{$ENDIF};

{$IFDEF SAX}
type
  TManifestParser = TSaxManifestParser;
{$ENDIF}

procedure InitAssemblyDb(ADb: TAssemblyDb; const AFilename: string; AAutoRebuild: boolean = true);
procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);

implementation
uses Windows, SysUtils, Classes, FilenameUtils, ManifestEnum_Progress, AssemblyDb.Assemblies,
  AssemblyDb.Bundles, Generics.Collections, WinSxS, ComObj, SxSUtils;

procedure InitAssemblyDb(ADb: TAssemblyDb; const AFilename: string; AAutoRebuild: boolean);
begin
  if AAutoRebuild and not FileExists(AFilename) then
    RebuildAssemblyDatabase(ADb, AFilename)
  else
    ADb.Open(AFilename);
end;

//Creates TStringList and populates it with file names by mask
function FilesByMask(const AMask: string): TStringList;
var sr: TSearchRec;
  res: integer;
begin
  Result := TStringList.Create;
  res := FindFirst(AMask, faAnyFile and not faDirectory, sr);
  while res = 0 do begin
    Result.Add(sr.Name);
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;


procedure GetInstalledAssemblies(AList: TStringList);
var s: string;
begin
  for s in SxSGetWinners do
    AList.Add(s);
end;


type
 //Simple boolean bit set class
  TFlagSet = record
    Bits: array of cardinal;
    procedure Reset; inline;
    procedure SetSize(const Value: integer); inline;
    function GetItem(const Index: integer): boolean; inline;
    procedure SetItem(const Index: integer; const Value: boolean); inline;
    property Size: integer write SetSize; //no GetSize because we don't store it
    property Item[const Index: integer]: boolean read GetItem write SetItem; default;
  const
    bitno = sizeof(integer)*8;
  end;

procedure TFlagSet.Reset;
var i: integer;
begin
  for i := 0 to Length(Bits)-1 do
    Bits[i] := 0;
end;

procedure TFlagSet.SetSize(const Value: integer);
begin
  SetLength(Bits, (Value div bitno) + 1);
end;

function TFlagSet.GetItem(const Index: integer): boolean;
begin
  Result := Bits[Index div bitno] and (1 shl ((Index mod bitno)-1)) <> 0;
end;

procedure TFlagSet.SetItem(const Index: integer; const Value: boolean);
begin
  if Value then
    Bits[Index div bitno] := Bits[Index div bitno] or (1 shl ((Index mod bitno)-1))
  else
    Bits[Index div bitno] := Bits[Index div bitno] and not (1 shl ((Index mod bitno)-1));
end;



{
Parses all manifests in WinSxS\Manifests and adds their data in the database. If the manifest
is known, it's assumed to be unchanged (they almost always are).
Displays progress form.
Designed to be fast when updating.
}

type
 //Service object which preserves useful caches between operations
  TDatabaseMaintenance = class
  protected
    Db: TAssemblyDb;
    Progress: TProgressForm;
    DbAssemblies: TAssemblyList;  //all assemblies in the DB
    procedure LoadAssemblyList;
    procedure ProcessManifests(MissingManifests: TStringList);
    procedure RefreshAssemblyState(MissingManifests: TStringList);
    procedure RefreshBundles;
    function ImportNewBundle(const BundleFile: TBundle): TBundleId;
    procedure UpdateBundleContents(Id: TBundleId; const BundleFile: TBundle; SkipClear: boolean = false);
    procedure DeleteBundle(Id: TBundleId);
  public
    constructor Create(ADb: TAssemblyDb);
    destructor Destroy; override;
    procedure RefreshAll;

  end;

constructor TDatabaseMaintenance.Create(ADb: TAssemblyDb);
begin
  inherited Create;
  Db := ADb;
  progress := TProgressForm.Create(nil);
  progress.Show;
end;

destructor TDatabaseMaintenance.Destroy;
begin
  FreeAndNil(DbAssemblies);
  FreeAndNil(progress);
  inherited;
end;

{
Builds the lookup list of all assemblies present in the database.
Produces:
  Self.DbAssemblies:  a list of assemblies
}
procedure TDatabaseMaintenance.LoadAssemblyList;
begin
  progress.Start('Building assembly list');
  DbAssemblies := TAssemblyList.Create;
  Db.Assemblies.GetAllAssemblies(DbAssemblies);
end;

{
Processes manifests present in the filesystem and imports their data into the database.
Produces:
  MissingManifests: populated with the set of manifest names which are missing from disk
}
procedure TDatabaseMaintenance.ProcessManifests(MissingManifests: TStringList);
var baseDir: string;
  fnames: TStringList;
  parser: TManifestParser;
  i, idx: integer;
  manifestMap: TStringList;   //maps manifestName -> assemblyId
  found: TFlagSet;  //true if the manifest from manifestMap is present in FS
  ad: TAssemblyData;
begin
  baseDir := SxSManifestDir()+'\';
  parser := nil;
  manifestMap := nil;

  try
    progress.Start('Building manifest map');
    manifestMap := TStringList.Create;
    manifestMap.Sorted := true;
    for ad in DbAssemblies.Values do
      manifestMap.AddObject(ad.manifestName, TObject(ad.id));

    found.SetSize(manifestMap.Count);
    found.Reset;

    //Build the list of manifests to import
    progress.Start('Building file list');
    fnames := FilesByMask(baseDir+'\*.manifest');

    //Parse the files
    progress.Start('Reading manifests', fnames.Count-1);
    parser := TManifestParser.Create(Db);
    for i := 0 to fnames.Count-1 do begin
      idx := manifestMap.IndexOf(ChangeFileExt(fnames[i], ''));
      if idx >= 0 then
        found[idx] := true
      else
        parser.ImportManifest(baseDir+'\'+fnames[i]);
      progress.Step();
    end;

    if MissingManifests <> nil then begin
      for i := 0 to manifestMap.Count-1 do
        if not found[i] then
          MissingManifests.AddObject(manifestMap[i], manifestMap.Objects[i]);
    end;

  finally
    FreeAndNil(parser);
    FreeAndNil(fnames);
    FreeAndNil(manifestMap);
  end;
end;

{
Queries SxS for the state of all assemblies in DbAssemblies list. Updates that state in the database.
Requires:
  MissingManifests: list of all manifest names which are missing from disk
}
procedure TDatabaseMaintenance.RefreshAssemblyState(MissingManifests: TStringList);
var winners: TStringList;
  ad: TAssemblyData;
  newState: TAssemblyState;
begin
  progress.Start('Updating assembly state');
  winners := TStringList.Create;
  try
    winners.Sorted := true;
    GetInstalledAssemblies(winners);

    //Mark assemblies as missing and present.
    //We have to touch all assemblies since they can change both ways (go missing / apear after being missing)
    for ad in DbAssemblies.Values do begin
      if MissingManifests.IndexOf(ad.manifestName) >= 0 then
        newState := asMissing
      else
      if winners.IndexOf(SxsWinnerKeyform(ad.identity)) >= 0 then
        newState := asInstalled
      else
        newState := asPresent;

      if ad.state <> newState then
        Db.Assemblies.SetState(ad.id, newState)
    end;
  finally
    FreeAndNil(winners);
  end;
end;

{
Reloads the list of bundle files from the disk, then refreshes the state in the database
Requires:
  Self.DbAssemblies: a list of all assemblies in the database
}
procedure TDatabaseMaintenance.RefreshBundles;
var bundles: TBundleList;
  found: TFlagSet;
  i, idx: integer;
  Data: TBundleData;
begin
  //Load actual bundle files from disk
  progress.Start('Reloading bundles', 0);
  BundleFiles.Clear;
  BundleFiles.Load(AppFolder() + '\BundleData');

  //Load existing bundles from the db
  bundles := TBundleList.Create;
  try
    Db.Bundles.GetAll(bundles);

    progress.Start('Importing bundles', BundleFiles.Count);
    found.SetSize(bundles.Count);
    found.Reset;
    for i := 0 to BundleFiles.Count-1 do begin
      idx := bundles.Find(BundleFiles[i].Name, BundleFiles[i].Data.path);
      if idx >= 0 then begin
        found[idx] := true;
        if bundles[idx].hash <> BundleFiles[i].Data.hash then begin
          UpdateBundleContents(bundles[idx].id, BundleFiles[i]);
          Data := BundleFiles[i].Data;
          Data.id := bundles[idx].id; //associated id with a bundle file
          BundleFiles[i].Data := Data;
          Db.Bundles.Update(Data); //update hash
        end;
      end else
        ImportNewBundle(BundleFiles[i]);
      progress.Step;
    end;
    for i := bundles.Count-1 downto 0 do
      if not found[i] then
        DeleteBundle(bundles[i].id);
  finally
    FreeAndNil(bundles);
  end;
end;

function TDatabaseMaintenance.ImportNewBundle(const BundleFile: TBundle): TBundleId;
var Data: TBundleData;
begin
  Data := BundleFile.Data;
  Result := Db.Bundles.Add(Data.name, Data.path, Data.hash);
  Data.id := Result;
  BundleFile.Data := Data;
  UpdateBundleContents(Result, BundleFile, true);
end;

procedure TDatabaseMaintenance.UpdateBundleContents(Id: TBundleId; const BundleFile: TBundle; SkipClear: boolean);
var ad: TAssemblyData;
begin
  if not SkipClear then
    Db.Bundles.ResetAssemblies(Id);

  for ad in DbAssemblies.Values do
    if BundleFile.ContainsAssembly(ad.identity) then
      Db.Bundles.AddAssembly(Id, ad.id);
end;

procedure TDatabaseMaintenance.DeleteBundle(Id: TBundleId);
begin
  Db.Bundles.ResetAssemblies(Id);
  Db.Bundles.Delete(Id);
end;


procedure TDatabaseMaintenance.RefreshAll;
var MissingManifests: TStringList;
{$IFDEF PROFILE}
  tm1: cardinal;
{$ENDIF}
begin
 {$IFDEF PROFILE}
  tm1 := GetTickCount();
 {$ENDIF}

  MissingManifests := TStringList.Create;
  MissingManifests.Sorted := true;
  try

    LoadAssemblyList;
    Db.BeginTransaction;
    try
      ProcessManifests(MissingManifests);
      Db.CommitTransaction;
      Db.BeginTransaction;

      LoadAssemblyList; //might have been changed after import
      RefreshAssemblyState(MissingManifests);
      RefreshBundles;
      Db.CommitTransaction;
    except
      Db.AbortTransaction;
      raise;
    end;

  finally
    FreeAndNil(MissingManifests);
  end;

 {$IFDEF PROFILE}
  tm1 := GetTickCount-tm1;
  MessageBox(0, PChar('Total time: '+IntToStr(tm1)), PChar('Import completed'), MB_OK);
 {$ENDIF}
end;


procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
var maint: TDatabaseMaintenance;
begin
  maint := TDatabaseMaintenance.Create(ADb);
  try
    maint.RefreshAll;
  finally
    FreeAndNil(maint);
  end;
end;

procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);
begin
  ADb.Close;
  DeleteFile(AFilename);
  ADb.Open(AFilename);
  RefreshAssemblyDatabase(ADb);
end;

end.
