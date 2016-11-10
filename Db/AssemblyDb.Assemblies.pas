unit AssemblyDb.Assemblies;

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core;

type
  TAssemblyId = int64;
  TAssemblyIdentity = record
    name: string;
    type_: string;
    language: string;
    buildType: string;
    processorArchitecture: string;
    version: string;
    publicKeyToken: string;
    versionScope: string;
    procedure Clear;
    function ToString: string;
    function ToStrongName: string;
    function ToStrongNameNETStyle: string;
  end;
  TAssemblyData = record
    id: TAssemblyId;
    identity: TAssemblyIdentity;
    manifestName: string;
    isDeployment: boolean;
  end;

  TAssemblyList = TDictionary<TAssemblyId, TAssemblyData>;

  TAssemblyAssemblies = class(TAssemblyDbModule)
  protected
    FIdCache: TDictionary<TAssemblyIdentity, TAssemblyId>;
    StmTouch: PSQLite3Stmt;
    StmFind: PSQLite3Stmt;
    StmUpdate: PSQLite3Stmt;
    StmGet: PSQLite3Stmt;
    procedure Initialize; override;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;

  public
    destructor Destroy; override;

    function AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string;
      AIsDeployment: boolean): TAssemblyId;
    function NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
    function GetAssembly(AAssembly: TAssemblyId): TAssemblyData;
    procedure GetAllAssemblies(AList: TAssemblyList);
    procedure QueryAssemblies(const ASql: string; AList: TAssemblyList); overload;
    procedure QueryAssemblies(const AStmt: PSQLite3Stmt; AList: TAssemblyList); overload;

  end;

implementation
uses SysUtils;

procedure TAssemblyIdentity.Clear;
begin
  Self.name := '';
  Self.type_ := '';
  Self.language := '';
  Self.buildType := '';
  Self.processorArchitecture := '';
  Self.version := '';
  Self.publicKeyToken := '';
  Self.versionScope := '';
end;

function TAssemblyIdentity.ToString: string;
begin
  Result := Self.name + '-' + Self.language + '-' + Self.buildType + '-' + Self.processorArchitecture
    + '-' + Self.version + '-' + Self.publicKeyToken;
end;

//Strong name type 1:
//  Microsoft.VC90.ATL,version="9.0.30729.1",publicKeyToken="1fc8b3b9a1e18e3b",processorArchitecture="amd64",type="win32"
//Required by SxS.dll routines. Be very, very compliant, as it fails at the
//slightest deviations.
function TAssemblyIdentity.ToStrongName: string;
begin
  Result := Self.name;
  if Self.type_ <> '' then
    Result := Result + ',type="'+Self.type_+'"';
  if Self.version <> '' then
    Result := Result + ',version="'+Self.version+'"';
  if Self.PublicKeyToken <> '' then
    Result := Result + ',publicKeyToken="'+Self.publicKeyToken+'"';
  if Self.processorArchitecture <> '' then
    Result := Result + ',processorArchitecture="'+Self.processorArchitecture+'"';
  if (Self.language <> '') and (Self.language <> 'neutral') and (Self.language <> '*') then
    Result := Result +',language="'+Self.language+'"';
  if Self.versionScope <> '' then
    Result := Result + ',versionScope="'+Self.versionScope+'"';
{ Attrtype is supported but breaks the match:
  if Self.buildType <> '' then
    Result := Result + ',buildType="'+Self.buildType+'"'; }
end;

//Strong name type 2:
//  Microsoft.VC90.ATL, Culture=neutral, Version=9.0.30729.1, PublicKeyToken=1fc8b3b9a1e18e3b, ProcessorArchitecture=amd64
//Used in the COMPONENTS hive in some places, also in .NET (unrelated to this app)
function TAssemblyIdentity.ToStrongNameNETStyle: string;
begin
  Result := Self.name;
  if (Self.language <> '') and (Self.language <> 'neutral') and (Self.language <> '*') then
    Result := Result +', Culture='+Self.language
  else
    Result := Result +', Culture=Neutral';
  if Self.version <> '' then
    Result := Result + ', Version='+Self.version;
  if Self.PublicKeyToken <> '' then
    Result := Result + ', PublicKeyToken='+Self.publicKeyToken;
  if Self.processorArchitecture <> '' then
    Result := Result + ', ProcessorArchitecture='+Self.processorArchitecture;
  if Self.versionScope <> '' then
    Result := Result + ', versionScope='+Self.versionScope; //sic, starts with lowercase
end;

procedure TAssemblyAssemblies.Initialize;
begin
  inherited;
  FIdCache := TDictionary<TAssemblyIdentity, TAssemblyId>.Create;
end;

destructor TAssemblyAssemblies.Destroy;
begin
  FreeAndNil(FIdCache);
  inherited;
end;

procedure TAssemblyAssemblies.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS assemblies ('
    +'id INTEGER PRIMARY KEY,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'type TEXT NOT NULL COLLATE NOCASE,'
    +'language TEXT NOT NULL COLLATE NOCASE,'
    +'buildType TEXT NOT NULL COLLATE NOCASE,'
    +'processorArchitecture TEXT NOT NULL COLLATE NOCASE,'
    +'version TEXT NOT NULL COLLATE NOCASE,'
    +'publicKeyToken TEXT NOT NULL,'
    +'versionScope TEXT NOT NULL COLLATE NOCASE,'
    +'manifestName TEXT COLLATE NOCASE,'
    +'isDeployment BOOL,'
    +'CONSTRAINT identity UNIQUE(name,type,language,buildType,processorArchitecture,version,publicKeyToken,versionScope)'
    +')');
end;

procedure TAssemblyAssemblies.InitStatements;
begin
  FIdCache.Clear;
  StmTouch := Db.PrepareStatement('INSERT OR IGNORE INTO assemblies '
    +'(name,type,language,buildType,processorArchitecture,version,publicKeyToken,versionScope) '
    +'VALUES (?,?,?,?,?,?,?,?)');
  StmFind := Db.PrepareStatement('SELECT id FROM assemblies WHERE '
    +'name=? AND type=? AND language=? AND buildType=? AND processorArchitecture=? AND version=? AND publicKeyToken=? AND versionScope=?');
  StmUpdate := Db.PrepareStatement('UPDATE assemblies SET manifestName=?, isDeployment=? '
    +'WHERE id=? ');
  StmGet := Db.PrepareStatement('SELECT * FROM assemblies WHERE id=?');
end;

function TAssemblyAssemblies.AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string; AIsDeployment: boolean): TAssemblyId;
begin
  Result := NeedAssembly(AEntry);
  //Update optional fields
  sqlite3_bind_str(StmUpdate, 1, AManifestName);
  sqlite3_bind_int(StmUpdate, 2, integer(AIsDeployment));
  sqlite3_bind_int64(StmUpdate, 3, Result);
  if sqlite3_step(StmUpdate) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmUpdate);
end;

function TAssemblyAssemblies.NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
var res: integer;
begin
  if FIdCache.TryGetValue(AEntry, Result) then
    exit;

  //Touch assembly
  sqlite3_bind_str(StmTouch, 1, AEntry.name);
  sqlite3_bind_str(StmTouch, 2, AEntry.type_);
  sqlite3_bind_str(StmTouch, 3, AEntry.language);
  sqlite3_bind_str(StmTouch, 4, AEntry.buildType);
  sqlite3_bind_str(StmTouch, 5, AEntry.processorArchitecture);
  sqlite3_bind_str(StmTouch, 6, AEntry.version);
  sqlite3_bind_str(StmTouch, 7, AEntry.publicKeyToken);
  sqlite3_bind_str(StmTouch, 8, AEntry.versionScope);
  if sqlite3_step(StmTouch) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_last_insert_rowid(FDb);
  sqlite3_reset(StmTouch);

  //Find assembly ID
  sqlite3_bind_str(StmFind, 1, AEntry.name);
  sqlite3_bind_str(StmFind, 2, AEntry.type_);
  sqlite3_bind_str(StmFind, 3, AEntry.language);
  sqlite3_bind_str(StmFind, 4, AEntry.buildType);
  sqlite3_bind_str(StmFind, 5, AEntry.processorArchitecture);
  sqlite3_bind_str(StmFind, 6, AEntry.version);
  sqlite3_bind_str(StmFind, 7, AEntry.publicKeyToken);
  sqlite3_bind_str(StmFind, 8, AEntry.versionScope);
  res := sqlite3_step(StmFind);
  if res <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFind, 0);
  sqlite3_reset(StmFind);

  FIdCache.Add(AEntry, Result);
end;

//Parses a row from the assembles table into the record
function TAssemblyAssemblies.SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;
begin
  Result.id := sqlite3_column_int64(stmt, 0);
  Result.identity.name := sqlite3_column_text16(stmt, 1);
  Result.identity.type_ := sqlite3_column_text16(stmt, 2);
  Result.identity.language := sqlite3_column_text16(stmt, 3);
  Result.identity.buildType := sqlite3_column_text16(stmt, 4);
  Result.identity.processorArchitecture := sqlite3_column_text16(stmt, 5);
  Result.identity.version := sqlite3_column_text16(stmt, 6);
  Result.identity.publicKeyToken := sqlite3_column_text16(stmt, 7);
  Result.identity.versionScope := sqlite3_column_text16(stmt, 8);
  Result.manifestName := sqlite3_column_text16(stmt, 9);
  Result.isDeployment := boolean(sqlite3_column_int(stmt, 10));
end;

function TAssemblyAssemblies.GetAssembly(AAssembly: TAssemblyId): TAssemblyData;
begin
  sqlite3_bind_int64(StmGet, 1, AAssembly);
  if sqlite3_step(StmGet) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := SqlReadAssemblyData(StmGet);
  sqlite3_reset(StmGet);
end;

//Makes an SQL query which returns a set of assembly table records.
procedure TAssemblyAssemblies.QueryAssemblies(const ASql: string; AList: TAssemblyList);
begin
  QueryAssemblies(Db.PrepareStatement(ASql), AList);
end;

procedure TAssemblyAssemblies.QueryAssemblies(const AStmt: PSQLite3Stmt; AList: TAssemblyList);
var res: integer;
  AId: TAssemblyId;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AId := sqlite3_column_int64(AStmt, 0);
    if not AList.ContainsKey(AId) then
      AList.Add(AId, SqlReadAssemblyData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyAssemblies.GetAllAssemblies(AList: TAssemblyList);
begin
  QueryAssemblies('SELECT * FROM assemblies', AList);
end;


end.
