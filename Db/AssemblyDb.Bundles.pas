unit AssemblyDb.Bundles;
//Stores bundle data

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TBundleId = int64;
  TBundleHash = cardinal;

  TBundleData = record
    id: TBundleId;
    name: string;
    path: string;
    hash: TBundleHash;
  end;
  TBundleList = TList<TBundleData>;

  TAssemblyBundles = class(TAssemblyDbModule)
  protected
    Assemblies: TAssemblyAssemblies;
    StmTouch: PSQLite3Stmt;
    StmFind: PSQLite3Stmt;
    StmUpdate: PSQLite3Stmt;
    StmAddAssembly: PSQLite3Stmt;
    procedure Initialize; override;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadBundleData(stmt: PSQLite3Stmt): TBundleData;
  public
    function Add(const AName, APath: string; AHash: TBundleHash): TBundleId;
    function Find(const AName, APath: string): TBundleId;
    procedure Update(Data: TBundleData);
    function Get(Id: TBundleId): TBundleData;
    procedure QueryBundles(AStmt: PSQLite3Stmt; AList: TBundleList);
    procedure GetAll(AList: TBundleList);

    procedure ResetContents(Bundle: TBundleId);
    procedure AddAssembly(Bundle: TBundleId; Assembly: TAssemblyId);
    procedure GetAssemblies(Bundle: TBundleId; AList: TAssemblyList);
    procedure GetAssemblyBundles(Assembly: TAssemblyId; AList: TBundleList);

  end;


implementation
uses SysUtils, Windows;

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
    +'hash INTEGER NOT NULL,'
    +'CONSTRAINT identity UNIQUE(name,path)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS bundleAssemblies ('
    +'bundleId INTEGER NOT NULL,'
    +'assemblyId INTEGER NOT NULL,'
    +'CONSTRAINT identity UNIQUE(bundleId,assemblyId)'
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

procedure TAssemblyBundles.GetAll(AList: TBundleList);
begin
  QueryBundles(Db.PrepareStatement('SELECT * FROM bundles'), AList);
end;

procedure TAssemblyBundles.ResetContents(Bundle: TBundleId);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('DELETE * FROM bundleAssemblies WHERE bundleId=?');
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
  stmt := Db.PrepareStatement('SELECT * FROM assemblies WHERE assemblyId IN (SELECT assemblyId FROM bundles WHERE bundleId=?)');
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

end.
