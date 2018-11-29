unit AssemblyDb.Bundles;
{
Manages assembly "bundles".
Bundles are groupings one level above components and assemblies, introduced manually. They are
stored as files, imported and linked to assemblies in the database.
}

{$DEFINE HASHNAMES}

interface
uses SysUtils, Classes, Generics.Collections, sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TBundleId = int64;
  TBundleHash = cardinal;

  TBundleData = record
    id: TBundleId;
    name: string;
    path: string;
    hash: TBundleHash;
  end;
  TBundleList = class(TList<TBundleData>)
    function Find(const AId: TBundleId): integer; overload;
    function Find(const AName, APath: string): integer; overload;
  end;

  TBundleAssociationDict = class(TDictionary<TAssemblyId, TBundleId>);

  TBundleAssociation = record
    bundle: TBundleId;
    assembly: TAssemblyId;
  end;
  TBundleAssociationList = TList<TBundleAssociation>;

  TAssemblyBundles = class(TAssemblyDbModule)
  protected
    Assemblies: TAssemblyAssemblies;
    StmTouch: PSQLite3Stmt;
    StmFind: PSQLite3Stmt;
    StmUpdate: PSQLite3Stmt;
    StmAddAssembly: PSQLite3Stmt;
    StmAddHostedFacility: PSQLite3Stmt;
    StmAddProvidedFacility: PSQLite3Stmt;
    procedure Initialize; override;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadBundleData(stmt: PSQLite3Stmt): TBundleData;
  public
    function Add(const AName, APath: string; AHash: TBundleHash): TBundleId;
    function Find(const AName, APath: string): TBundleId;
    procedure Update(Data: TBundleData);
    function Get(Id: TBundleId): TBundleData;
    procedure Delete(Id: TBundleId);
    procedure QueryBundles(AStmt: PSQLite3Stmt; AList: TBundleList);
    procedure GetAll(AList: TBundleList);

    procedure ResetAssemblies(Bundle: TBundleId);
    procedure AddAssembly(Bundle: TBundleId; Assembly: TAssemblyId);
    procedure GetAssemblies(Bundle: TBundleId; AList: TAssemblyList);
    procedure GetAssemblyBundles(Assembly: TAssemblyId; AList: TBundleList);

    procedure GetAllAssemblyAssociations(AList: TBundleAssociationList); overload;
    procedure GetAllAssemblyAssociations(AList: TBundleAssociationDict); overload;

    procedure ResetHostedFacilities(Bundle: TBundleId);
    procedure AddHostedFacility(Bundle: TBundleId; const AFacility: string);

    procedure ResetProvidedFacilities(Bundle: TBundleId);
    procedure AddProvidedFacility(Bundle: TBundleId; const AFacility: string);

  end;


{
 Next are the classes that manage the underlying files.
 Files are loaded but not normally linked to database bundle records. Database
 records are superior and are the ones we use for most operations.
 Files might differ from database records until we run a database refresh,
 so we only refer to it when we cannot do something otherwise.
}
type
  TMaskList = class(TList<TAssemblyIdentity>)
  public
    function MatchesAnyLowercased(const id: TAssemblyIdentity): boolean;
  end;

  TBundle = class
  protected
    FData: TBundleData;
    FMasks: TMaskList;
    FExclusionMasks: TMaskList;
    FHostedFacilities: TStringList;
    FProvidedFacilities: TStringList;
    function GetName: string; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(const ABase, AFilename: string);
    function ContainsAssembly(const Id: TAssemblyIdentity): boolean;
    function ContainsAssemblyLowercased(const Id: TAssemblyIdentity): boolean; inline;
    property Name: string read GetName;
    property Data: TBundleData read FData write FData;
    property HostedFacilities: TStringList read FHostedFacilities;
    property ProvidedFacilities: TStringList read FProvidedFacilities;
  end;

  TBundleManager = class(TObjectList<TBundle>)
  public
    procedure Load(const ABase: string); overload;
    procedure LoadFolder(const ABase, ADir: string);
    function LoadBundle(const ABase, AFilename: string): TBundle;
    function Find(const AId: TBundleId): TBundle;
    function MatchAssembly(const Id: TAssemblyIdentity): TBundle;
  end;

  EBundleException = class(Exception);


var
  BundleFiles: TBundleManager;

function BundleDir(): string;


implementation
uses Windows, FilenameUtils, WildcardMatching;

//Returns the default directory for storing bundle definitions
function BundleDir(): string;
begin
  Result := AppFolder() + 'BundleData';
end;

procedure TAssemblyBundles.Initialize;
begin
  inherited;
  Assemblies := TAssemblyAssemblies(Db.Modules.Find(TAssemblyAssemblies));
  Assert(Assemblies <> nil);
end;

procedure TAssemblyBundles.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS bundles ('
    +'id INTEGER PRIMARY KEY,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'path TEXT NOT NULL COLLATE NOCASE,'
    +'hash INTEGER,'
    +'CONSTRAINT identity UNIQUE(name,path)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS bundleAssemblies ('
    +'bundleId INTEGER NOT NULL,'
    +'assemblyId INTEGER NOT NULL,'
    +'CONSTRAINT identity UNIQUE(bundleId,assemblyId)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS bundleHostedFacilities ('
    +'bundleId INTEGER NOT NULL,'
    +'facility TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(bundleId,facility)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS bundleProvidedFacilities ('
    +'bundleId INTEGER NOT NULL,'
    +'facility TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(bundleId,facility)'
    +')');
end;

procedure TAssemblyBundles.InitStatements;
begin
  StmTouch := Db.PrepareStatement('INSERT OR IGNORE INTO bundles '
    +'(name,path) VALUES (?,?)');
  StmFind := Db.PrepareStatement('SELECT id FROM bundles WHERE '
    +'name=? AND path=?');
  StmUpdate := Db.PrepareStatement('UPDATE bundles SET hash=? '
    +'WHERE id=?');
  StmAddAssembly := Db.PrepareStatement('INSERT OR IGNORE INTO bundleAssemblies '
    +'(bundleId,assemblyId) VALUES (?,?)');
  StmAddHostedFacility := Db.PrepareStatement('INSERT OR IGNORE INTO bundleHostedFacilities '
    +'(bundleId,facility) VALUES (?,?)');
  StmAddProvidedFacility := Db.PrepareStatement('INSERT OR IGNORE INTO bundleProvidedFacilities '
    +'(bundleId,facility) VALUES (?,?)');
end;

function TAssemblyBundles.Add(const AName, APath: string; AHash: TBundleHash): TBundleId;
var Data: TBundleData;
begin
  //Touch
  sqlite3_bind_str(StmTouch, 1, AName);
  sqlite3_bind_str(StmTouch, 2, APath);
  if sqlite3_step(StmTouch) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmTouch);

  //Find id
  Result := Find(AName, APath);

  Data.id := Result;
  Data.name := AName;
  Data.path := APath;
  Data.hash := AHash;
  Self.Update(Data);
end;

function TAssemblyBundles.Find(const AName, APath: string): TBundleId;
begin
  //Find id
  sqlite3_bind_str(StmFind, 1, AName);
  sqlite3_bind_str(StmFind, 2, APath);
  if sqlite3_step(StmFind) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFind, 0);
  sqlite3_reset(StmFind);
end;

procedure TAssemblyBundles.Update(Data: TBundleData);
begin
  sqlite3_bind_int64(StmUpdate, 1, Data.hash);
  sqlite3_bind_int64(StmUpdate, 2, Data.id);
  if sqlite3_step(StmUpdate) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmUpdate);
end;

function TAssemblyBundles.SqlReadBundleData(stmt: PSQLite3Stmt): TBundleData;
begin
  Result.id := sqlite3_column_int64(stmt, 0); //rowid
  Result.name := sqlite3_column_text16(stmt, 1);
  Result.path := sqlite3_column_text16(stmt, 2);
  Result.hash := sqlite3_column_int(stmt, 3);
end;

//Parses the results of SELECT * FROM bundles
procedure TAssemblyBundles.QueryBundles(AStmt: PSQLite3Stmt; AList: TBundleList);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadBundleData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(AStmt);
end;

function TAssemblyBundles.Get(Id: TBundleId): TBundleData;
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT * FROM bundles WHERE id=?');
  sqlite3_bind_int64(stmt, 1, Id);
  if sqlite3_step(stmt) <> SQLITE_ROW then
    Db.RaiseLastSqliteError();
  Result := SqlReadBundleData(stmt);
  sqlite3_reset(stmt);
end;

procedure TAssemblyBundles.Delete(Id: TBundleId);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('DELETE FROM bundles WHERE id=?');
  sqlite3_bind_int64(stmt, 1, Id);
  Db.ExecAndReset(stmt);
end;

procedure TAssemblyBundles.GetAll(AList: TBundleList);
begin
  QueryBundles(Db.PrepareStatement('SELECT * FROM bundles'), AList);
end;

procedure TAssemblyBundles.ResetAssemblies(Bundle: TBundleId);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('DELETE FROM bundleAssemblies WHERE bundleId=?');
  sqlite3_bind_int64(stmt, 1, Bundle);
  Db.ExecAndReset(stmt);
end;

procedure TAssemblyBundles.AddAssembly(Bundle: TBundleId; Assembly: TAssemblyId);
begin
  sqlite3_bind_int64(StmAddAssembly, 1, Bundle);
  sqlite3_bind_int64(StmAddAssembly, 2, Assembly);
  if sqlite3_step(StmAddAssembly) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddAssembly);
end;

procedure TAssemblyBundles.GetAssemblies(Bundle: TBundleId; AList: TAssemblyList);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT * FROM assemblies WHERE id IN (SELECT assemblyId FROM bundleAssemblies WHERE bundleId=?)');
  sqlite3_bind_int64(stmt, 1, Bundle);
  Assemblies.QueryAssemblies(stmt, AList);
end;

procedure TAssemblyBundles.GetAssemblyBundles(Assembly: TAssemblyId; AList: TBundleList);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT * FROM bundleAssemblies WHERE assemblyId=?');
  sqlite3_bind_int64(stmt, 1, Assembly);
  QueryBundles(stmt, AList);
end;

//Returns a list of all associations
procedure TAssemblyBundles.GetAllAssemblyAssociations(AList: TBundleAssociationList);
var stmt: PSQLite3Stmt;
  res: integer;
  rec: TBundleAssociation;
begin
  stmt := Db.PrepareStatement('SELECT * FROM bundleAssemblies');
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    rec.bundle := sqlite3_column_int64(stmt, 0);
    rec.assembly := sqlite3_column_int64(stmt, 1);
    AList.Add(rec);
    res := sqlite3_step(stmt);
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSqliteError;
  sqlite3_reset(stmt);
end;

//Returns a list of all associations where each assembly is assigned only one bundle
procedure TAssemblyBundles.GetAllAssemblyAssociations(AList: TBundleAssociationDict);
var stmt: PSQLite3Stmt;
  res: integer;
  bundle: TBundleId;
  assembly: TAssemblyId;
begin
  stmt := Db.PrepareStatement('SELECT * FROM bundleAssemblies');
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    bundle := sqlite3_column_int64(stmt, 0);
    assembly := sqlite3_column_int64(stmt, 1);
    AList.AddOrSetValue(assembly, bundle);
    res := sqlite3_step(stmt);
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSqliteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyBundles.ResetHostedFacilities(Bundle: TBundleId);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('DELETE FROM bundleHostedFacilities WHERE bundleId=?');
  sqlite3_bind_int64(stmt, 1, Bundle);
  Db.ExecAndReset(stmt);
end;

procedure TAssemblyBundles.AddHostedFacility(Bundle: TBundleId; const AFacility: string);
begin
  sqlite3_bind_int64(StmAddHostedFacility, 1, Bundle);
  sqlite3_bind_str(StmAddHostedFacility, 2, AFacility);
  if sqlite3_step(StmAddHostedFacility) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddHostedFacility);
end;

procedure TAssemblyBundles.ResetProvidedFacilities(Bundle: TBundleId);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('DELETE FROM bundleProvidedFacilities WHERE bundleId=?');
  sqlite3_bind_int64(stmt, 1, Bundle);
  Db.ExecAndReset(stmt);
end;

procedure TAssemblyBundles.AddProvidedFacility(Bundle: TBundleId; const AFacility: string);
begin
  sqlite3_bind_int64(StmAddProvidedFacility, 1, Bundle);
  sqlite3_bind_str(StmAddProvidedFacility, 2, AFacility);
  if sqlite3_step(StmAddProvidedFacility) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddProvidedFacility);
end;

function TBundleList.Find(const AId: TBundleId): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Count-1 do
    if Self[i].id = AId then begin
      Result := i;
      break;
    end;
end;

function TBundleList.Find(const AName, APath: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Count-1 do
    if (Self[i].name = AName) and (Self[i].path = APath) then begin
      Result := i;
      break;
    end;
end;


{
Bundle files
Line format: name[,param=value[, ...]]
Any part can have wildcards.
}

const
  BUNDLE_EXT = '.txt';

constructor TBundle.Create;
begin
  inherited;
  FMasks := TMaskList.Create;
  FExclusionMasks := TMaskList.Create;
  FHostedFacilities := TStringList.Create;
  FProvidedFacilities := TStringList.Create;
end;

destructor TBundle.Destroy;
begin
  FreeAndNil(FProvidedFacilities);
  FreeAndNil(FHostedFacilities);
  FreeAndNil(FExclusionMasks);
  FreeAndNil(FMasks);
  inherited;
end;

function TBundle.GetName: string;
begin
  Result := FData.name;
end;

function EatPartname(var APart: string): string; inline;
var i_pos: integer;
begin
  i_pos := pos('=', APart);
  if i_pos > 0 then begin
    Result := Trim(copy(APart, 1, i_pos-1));
    delete(APart, 1, i_pos);
    APart := Trim(APart);
  end else begin
    Result := '';
  end;
end;

//Eats one command in the form "@command" or "@command:" and leaves the rest of the line intact.
//Does not verify that @ is indeed @.
//Also eats any spaces after the command.
function EatCmd(var ALine: string): string;
var i_pos, i_r: integer;
begin
  i_pos := pos(':', ALine);
  if i_pos > 0 then begin
    //Skip any spaces before ':'
    i_r := i_pos;
    while (i_r > 1) and (ALine[i_r] = ' ') do
      Dec(i_r);
    //Copy the command (omitting the @)
    Result := copy(ALine, 2, i_pos-2);
    //Skip any spaces after ':'
    while (i_pos < Length(ALine)) and (ALine[i_pos+1] = ' ') do
      Inc(i_pos);
    delete(ALine, 1, i_pos);
  end else begin
    Result := ALine;
    ALine := '';
    delete(Result, 1, 1); //"@"
  end;
end;

procedure TBundle.Load(const ABase, AFilename: string);
var i, i_pos: integer;
  ln, part, partname: string;
  attrs: WIN32_FILE_ATTRIBUTE_DATA;
  lines: TStringList;
  mask: TAssemblyIdentity;
  exclude: boolean;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ABase+'\'+AFilename);

    for i := Lines.Count-1 downto 0 do begin
      ln := Lines[i];
      //Remove comments
      i_pos := pos('#', ln);
      if i_pos > 0 then begin
        SetLength(ln, i_pos-1);
      end;
      ln := AnsiLowercase(ln.Trim);
      if ln = '' then continue;

      //Process sections
      //At the moment we just skip them (to allow defining them before support is introduced)
      if (ln[1] = '[') and (ln[Length(ln)] = ']') then begin
        continue;
      end;

      //Process instructions
      if ln[1] = '@' then begin
        part := EatCmd(ln);
        if SameStr(part, 'hosts') then begin
          if ln <> '' then
            Self.FHostedFacilities.Add(ln)
        end else
        if SameStr(part, 'provides') then begin
          if ln <> '' then
            Self.FProvidedFacilities.Add(ln);
        end;
        //Other commands are unsupported
        continue;
      end;

      //Handle exclude commands
      if ln[1]='-' then begin
        delete(ln, 1, 1);
        if ln = '' then continue; //empty
        exclude := true;
      end else
        exclude := false;

      //Parse the identity mask
      mask.Clear;
      repeat
        i_pos := pos(',', ln);
        if i_pos > 0 then begin
          part := copy(ln, 1, i_pos-1).Trim;
          delete(ln, 1, i_pos);
        end else
          part := ln;

        partname := EatPartname(part);

        if SameStr(partname, '') then
          mask.name := part
        else
        if SameStr(partname, 'processorarchitecture') then
          mask.processorArchitecture := part
        else
        if SameStr(partname, 'publickeytoken') then
          mask.publicKeyToken := part
        else
        if SameStr(partname, 'version') then
          mask.version := part
        else
        if SameStr(partname, 'language')
        or SameStr(partname, 'culture') then
          mask.language := part
        else
        if SameStr(partname, 'type') then
          mask.type_ := part
        else
        if SameStr(partname, 'versionscope') then
          mask.versionscope := part
        else
          raise EBundleException.Create('Unsupported filter name: "'+partname+'"');

      until i_pos <= 0;

      if not exclude then
        FMasks.Add(mask)
      else
        FExclusionMasks.Add(mask);
    end;
  finally
    FreeAndNil(lines);
  end;

  FData.name := ChangeFileExt(ExtractFilename(AFilename), '');
  FData.path := ExtractFilePath(AFilename);
  if (Length(FData.path) > 0) and (FData.path[Length(FData.path)]='\') then
    Delete(FData.path, Length(Data.path), 1);
  if (Length(FData.path) > 0) and (FData.path[1]='\') then
    Delete(FData.path, 1, 1);

  if not GetFileAttributesEx(PChar(ABase+'\'+AFilename), GetFileExInfoStandard, @attrs) then
    RaiseLastOsError();
  FData.hash := PInt64(@attrs.ftLastWriteTime)^;
end;

function TBundle.ContainsAssembly(const Id: TAssemblyIdentity): boolean;
begin
  Result := ContainsAssemblyLowercased(Id.ToLowercase);
end;

function TBundle.ContainsAssemblyLowercased(const Id: TAssemblyIdentity): boolean;
begin
  Result := FMasks.MatchesAnyLowercased(Id) and not FExclusionMasks.MatchesAnyLowercased(Id);
end;

function TMaskList.MatchesAnyLowercased(const id: TAssemblyIdentity): boolean;
var mask: TAssemblyIdentity;
begin
  for mask in Self do begin
    if mask.name <> '' then
      if pos('*', mask.name) <= 0 then begin
        if not SameStr(id.name, mask.name)
        and not SameStr(id.name, mask.name+'.resources')
        and not SameStr(id.name, mask.name+'-languagepack') then
          continue;
      end else
        if not WildcardMatchCase(PChar(Id.name), PChar(mask.name)) then
          continue;

    if (mask.version <> '')
    and not WildcardMatchCase(PChar(Id.version), PChar(mask.version)) then
      continue;

    if (mask.language <> '')
    and not WildcardMatchCase(PChar(Id.language), PChar(mask.language)) then
      continue;

    if (mask.publicKeyToken <> '')
    and not SameStr(Id.publicKeyToken, mask.publicKeyToken) then
      continue;

    if (mask.processorArchitecture <> '')
    and not SameStr(Id.processorArchitecture, mask.processorArchitecture) then
      continue;

    if (mask.versionScope <> '')
    and not SameStr(Id.versionScope, mask.versionScope) then
      continue;

    if (mask.type_ <> '')
    and not SameStr(Id.type_, mask.type_) then
      continue;

    Result := true;
    exit;
  end;
  Result := false;
end;



procedure TBundleManager.Load(const ABase: string);
begin
  Clear;
  LoadFolder(ABase, '');
end;

procedure TBundleManager.LoadFolder(const ABase, ADir: string);
var sr: TSearchRec;
  res: integer;
begin
  res := FindFirst(ABase+'\'+ADir+'\*.*', faAnyFile, sr);
  if res <> 0 then exit;
  try
    while res = 0 do begin
      //File
      if (sr.Attr and faDirectory = 0) then begin
        if ExtractFileExt(sr.Name)=BUNDLE_EXT then
          LoadBundle(ABase, ADir+'\'+sr.Name);
        //else ignore file
      end else
      //Directory
        if (sr.Name <> '.') and (sr.Name <> '..') then
          LoadFolder(ABase, ADir+'\'+sr.Name);
      res := FindNext(sr);
    end;
  finally
    SysUtils.FindClose(sr);
  end;
end;

function TBundleManager.LoadBundle(const ABase, AFilename: string): TBundle;
begin
  Result := TBundle.Create;
  Self.Add(Result);
  Result.Load(ABase, AFilename);
end;

function TBundleManager.Find(const AId: TBundleId): TBundle;
var Bundle: TBundle;
begin
  Result := nil;
  for Bundle in Self do
    if Bundle.FData.id = AId then begin
      Result := Bundle;
      break;
    end;
end;

function TBundleManager.MatchAssembly(const Id: TAssemblyIdentity): TBundle;
var Bundle: TBundle;
begin
  for Bundle in Self do
    if Bundle.ContainsAssembly(Id) then begin
      Result := Bundle;
      exit;
    end;
  Result := nil;
end;

initialization
  BundleFiles := TBundleManager.Create;

finalization
 {$IFDEF DEBUG}
  FreeAndNil(BundleFiles);
 {$ENDIF}

end.
