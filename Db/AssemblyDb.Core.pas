unit AssemblyDb.Core;
//Database core. Contains only the generic functions, no actual tables or schema.

interface
uses SysUtils, Classes, sqlite3, Generics.Collections;

type
  ESqliteError = class(Exception);
  EDatabaseError = class(Exception);

type
  TAssemblyDbModule = class;
  TAssemblyDbModuleList = class;

  TRowFunction = reference to procedure;

  TAssemblyDbCore = class
  protected
    FDb: PSQLite3;
    FPreparedStatements: TDictionary<string, PSQLite3Stmt>;
    procedure CreateTables; virtual;
    procedure InitStatements; virtual;
    procedure FreeStatements; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AFilename: string): boolean;
    procedure Close;
    function GetLastSqliteError: string;
    procedure RaiseLastSqliteError;
    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure AbortTransaction;
    procedure Exec(const ASql: string);
    procedure ExecAndReset(stmt: PSQLite3Stmt);
    procedure QueryAndReset(stmt: PSQLite3Stmt; rowFunc: TRowFunction);
    function PrepareStatement(const ASql: string): PSQLite3Stmt;

  //Modules provide a way to extend the database object with additions to its key functions
  protected
    FModules: TAssemblyDbModuleList;
    procedure FreeModules;
  public
    function AddModule(AModule: TAssemblyDbModule): TAssemblyDbModule;
    property Modules: TAssemblyDbModuleList read FModules;

  end;

  TAssemblyDbModule = class
  protected
    FDb: TAssemblyDbCore;
    procedure Initialize; virtual;
    procedure CreateTables; virtual;
    procedure InitStatements; virtual;
    procedure Close; virtual;
  public
    constructor Create(ADb: TAssemblyDbCore);
    destructor Destroy; override;
    property Db: TAssemblyDbCore read FDb;
  end;

  CAssemblyDbModule = class of TAssemblyDbModule;
  TAssemblyDbModuleList = class(TObjectList<TAssemblyDbModule>)
  public
    function Find(AClass: CAssemblyDbModule): TAssemblyDbModule;
  end;

function sqlite3_bind_str(pStmt: PSQLite3Stmt; i: Integer; const zData: string): integer; inline;

implementation

function sqlite3_bind_str(pStmt: PSQLite3Stmt; i: Integer; const zData: string): integer;
begin
  Result := sqlite3_bind_text16(pStmt, i, PChar(zData), -1, nil);
end;

constructor TAssemblyDbCore.Create;
begin
  inherited;
  FDb := nil;
  FModules := TAssemblyDbModuleList.Create;
end;

destructor TAssemblyDbCore.Destroy;
begin
  Close;
  FreeAndNil(FModules);
  inherited;
end;

//Retrieves text description for the last error that happened with the connection
function TAssemblyDbCore.GetLastSqliteError: string;
begin
  Result := sqlite3_errmsg16(FDb);
end;

//Raises ESqliteError for the last error that happened with the connection
procedure TAssemblyDbCore.RaiseLastSqliteError;
begin
  raise ESqliteError.Create(GetLastSqliteError);
end;

//Opens existing or creates a new database.
//If you need to force recreation, delete the database file before calling this.
function TAssemblyDbCore.Open(const AFilename: string): boolean;
var res: integer;
begin
  res := sqlite3_open16(PChar(AFilename), FDb);
  if res <> 0 then begin
    sqlite3_close(FDb); //handle is always returned
    FDb := nil;
    Result := false;
    exit;
  end;

  Exec('PRAGMA cache_size=200000');
  Exec('PRAGMA synchronous=OFF');
  Exec('PRAGMA count_changes=OFF');
  Exec('PRAGMA temp_store=2');

  CreateTables;
  InitStatements;

  Result := true;
end;

procedure TAssemblyDbCore.Close;
var AModule: TAssemblyDbModule;
begin
  for AModule in FModules do
    AModule.Close;
  if FDb <> nil then begin
    FreeStatements;
    sqlite3_close(FDb);
    FDb := nil;
  end;
end;

//Executes an instruction or throws an error
procedure TAssemblyDbCore.Exec(const ASql: string);
begin
  if sqlite3_exec(FDb, PAnsiChar(Utf8String(ASql)), nil, nil, nil) <> 0 then
    RaiseLastSqliteError();
end;

procedure TAssemblyDbCore.ExecAndReset(stmt: PSQLite3Stmt);
var res: integer;
begin
  res := sqlite3_step(stmt);
  if (res <> SQLITE_DONE) and (res <> SQLITE_ROW) then
    RaiseLastSqliteError();
  sqlite3_reset(stmt);
end;

procedure TAssemblyDbCore.QueryAndReset(stmt: PSQLite3Stmt; rowFunc: TRowFunction);
var res: integer;
begin
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    rowFunc();
    res := sqlite3_step(stmt);
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyDbCore.BeginTransaction;
begin
  Exec('BEGIN');
end;

procedure TAssemblyDbCore.CommitTransaction;
begin
  Exec('COMMIT');
end;

procedure TAssemblyDbCore.AbortTransaction;
begin
  Exec('ROLLBACK');
end;

procedure TAssemblyDbCore.CreateTables;
var AModule: TAssemblyDbModule;
begin
  for AModule in FModules do
    AModule.CreateTables;
end;

procedure TAssemblyDbCore.InitStatements;
var AModule: TAssemblyDbModule;
begin
  FPreparedStatements := TDictionary<string, PSQLite3Stmt>.Create;
  for AModule in FModules do
    AModule.InitStatements;
end;

procedure TAssemblyDbCore.FreeStatements;
var stmt: PSQLite3Stmt;
begin
  for stmt in FPreparedStatements.Values do
    sqlite3_finalize(stmt);
  FreeAndNil(FPreparedStatements);
end;

function TAssemblyDbCore.PrepareStatement(const ASql: string): PSQLite3Stmt;
begin
  if FPreparedStatements.TryGetValue(ASql, Result) then
    exit;
  if sqlite3_prepare16_v2(FDb, PChar(ASql), -1, Result, nil) <> 0 then
    RaiseLastSQLiteError();
  FPreparedStatements.Add(ASql, Result);
end;

function TAssemblyDbCore.AddModule(AModule: TAssemblyDbModule): TAssemblyDbModule;
begin
  FModules.Add(AModule);
  Result := AModule;
end;

procedure TAssemblyDbCore.FreeModules;
begin
  FModules.Clear;
end;

function TAssemblyDbModuleList.Find(AClass: CAssemblyDbModule): TAssemblyDbModule;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if Self[i] is AClass then begin
      Result := Self[i];
      break;
    end;
end;

constructor TAssemblyDbModule.Create(ADb: TAssemblyDbCore);
begin
  inherited Create;
  FDb := ADb;
  Initialize;
end;

//Override to do creation initialization
procedure TAssemblyDbModule.Initialize;
begin
end;

destructor TAssemblyDbModule.Destroy;
begin
  inherited;
end;

//Override to create tables which are missing from the DB
procedure TAssemblyDbModule.CreateTables;
begin
end;

//Override to initialize precompiled statements
procedure TAssemblyDbModule.InitStatements;
begin
end;

//Override to free any resources which are not free automatically due to TAssemblyDbCore unloading.
//Often nothing to do.
procedure TAssemblyDbModule.Close;
begin
end;

end.
