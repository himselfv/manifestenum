unit AssemblyDb.Assemblies;

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core;

type
  TAssemblyId = int64;
  TAssemblyIdentity = record
    name: string;
    language: string;
    buildType: string;
    processorArchitecture: string;
    version: string;
    publicKeyToken: string;
    function ToString: string;
  end;
  TAssemblyData = record
    id: TAssemblyId;
    identity: TAssemblyIdentity;
    manifestName: string;
  end;

  TAssemblyList = TDictionary<TAssemblyId, TAssemblyData>;

  TAssemblyAssemblies = class(TAssemblyDbModule)
  protected
    StmTouch: PSQLite3Stmt;
    StmFind: PSQLite3Stmt;
    StmUpdate: PSQLite3Stmt;
    StmGet: PSQLite3Stmt;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;

  public
    function AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
    function NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
    function GetAssembly(AAssembly: TAssemblyId): TAssemblyData;
    procedure GetAllAssemblies(AList: TAssemblyList);
    procedure QueryAssemblies(const ASql: string; AList: TAssemblyList); overload;
    procedure QueryAssemblies(const AStmt: PSQLite3Stmt; AList: TAssemblyList); overload;

  end;

implementation

function TAssemblyIdentity.ToString: string;
begin
  Result := Self.name + '-' + Self.language + '-' + Self.buildType + '-' + Self.processorArchitecture
    + '-' + Self.version + '-' + Self.publicKeyToken;
end;

procedure TAssemblyAssemblies.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS assemblies ('
    +'id INTEGER PRIMARY KEY,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'language TEXT NOT NULL COLLATE NOCASE,'
    +'buildType TEXT NOT NULL COLLATE NOCASE,'
    +'processorArchitecture TEXT NOT NULL COLLATE NOCASE,'
    +'version TEXT NOT NULL COLLATE NOCASE,'
    +'publicKeyToken TEXT NOT NULL,'
    +'manifestName TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(name,language,buildType,processorArchitecture,version,publicKeyToken)'
    +')');
end;

procedure TAssemblyAssemblies.InitStatements;
begin
  StmTouch := Db.PrepareStatement('INSERT OR IGNORE INTO assemblies '
    +'(name,language,buildType,processorArchitecture,version,publicKeyToken) '
    +'VALUES (?,?,?,?,?,?)');
  StmFind := Db.PrepareStatement('SELECT id FROM assemblies WHERE '
    +'name=? AND language=? AND buildType=? AND processorArchitecture=? AND version=? AND publicKeyToken=?');
  StmUpdate := Db.PrepareStatement('UPDATE assemblies SET manifestName=? '
    +'WHERE id=? ');
  StmGet := Db.PrepareStatement('SELECT * FROM assemblies WHERE id=?');
end;

function TAssemblyAssemblies.AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
begin
  Result := NeedAssembly(AEntry);
  //Update optional fields
  sqlite3_bind_str(StmUpdate, 1, AManifestName);
  sqlite3_bind_int64(StmUpdate, 2, Result);
  if sqlite3_step(StmUpdate) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmUpdate);
end;

function TAssemblyAssemblies.NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
var res: integer;
begin
  //Touch assembly
  sqlite3_bind_str(StmTouch, 1, AEntry.name);
  sqlite3_bind_str(StmTouch, 2, AEntry.language);
  sqlite3_bind_str(StmTouch, 3, AEntry.buildType);
  sqlite3_bind_str(StmTouch, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmTouch, 5, AEntry.version);
  sqlite3_bind_str(StmTouch, 6, AEntry.publicKeyToken);
  if sqlite3_step(StmTouch) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_last_insert_rowid(FDb);
  sqlite3_reset(StmTouch);

  //Find assembly ID
  sqlite3_bind_str(StmFind, 1, AEntry.name);
  sqlite3_bind_str(StmFind, 2, AEntry.language);
  sqlite3_bind_str(StmFind, 3, AEntry.buildType);
  sqlite3_bind_str(StmFind, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmFind, 5, AEntry.version);
  sqlite3_bind_str(StmFind, 6, AEntry.publicKeyToken);
  res := sqlite3_step(StmFind);
  if res <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFind, 0);
  sqlite3_reset(StmFind);
end;

//Parses a row from the assembles table into the record
function TAssemblyAssemblies.SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;
begin
  Result.id := sqlite3_column_int64(stmt, 0);
  Result.identity.name := sqlite3_column_text16(stmt, 1);
  Result.identity.language := sqlite3_column_text16(stmt, 2);
  Result.identity.buildType := sqlite3_column_text16(stmt, 3);
  Result.identity.processorArchitecture := sqlite3_column_text16(stmt, 4);
  Result.identity.version := sqlite3_column_text16(stmt, 5);
  Result.identity.publicKeyToken := sqlite3_column_text16(stmt, 6);
  Result.manifestName := sqlite3_column_text16(stmt, 7);
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
