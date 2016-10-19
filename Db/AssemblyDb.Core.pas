unit AssemblyDb.Core;
//Database core. Contains only the generic functions, no actual tables or schema.

interface
uses SysUtils, Classes, sqlite3, Generics.Collections;

type
  ESqliteError = class(Exception);

type
  TAssemblyDbModule = class;

  TAssemblyDbCore = class
  protected
    FDb: PSQLite3;
    FPreparedStatements: TDictionary<string, PSQLite3Stmt>;
    function GetLastSqliteError: string;
    procedure RaiseLastSqliteError;
    procedure Exec(const ASql: string);
    procedure CreateTables; virtual;
    function PrepareStatement(const ASql: string): PSQLite3Stmt;
    procedure InitStatements; virtual;
    procedure FreeStatements; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AFilename: string): boolean;
    procedure Close;
    procedure BeginTransaction;
    procedure CommitTransaction;

  //Modules provide a way to extend the database object with additions to its key functions
  protected
    FModules: TObjectList<TAssemblyDbModule>;
    procedure FreeModules;
  public
    function AddModule(AModule: TAssemblyDbModule): TAssemblyDbModule;
    property Modules: TObjectList<TAssemblyDbModule> read FModules;

  end;

  TAssemblyDbModule = class
  protected
    FDb: TAssemblyDbCore;
    procedure CreateTables; virtual;
    procedure InitStatements; virtual;
  public
    constructor Create(ADb: TAssemblyDbCore);
    destructor Destroy;
    property Db: TAssemblyDbCore read FDb;
  end;


implementation


constructor TAssemblyDbCore.Create;
begin
  inherited;
  FDb := nil;
  FModules := TObjectList<TAssemblyDbModule>.Create;
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

  CreateTables;
  InitStatements;

  Result := true;
end;

procedure TAssemblyDbCore.Close;
begin
  FreeModules;
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

procedure TAssemblyDbCore.BeginTransaction;
begin
  Exec('BEGIN');
end;

procedure TAssemblyDbCore.CommitTransaction;
begin
  Exec('COMMIT');
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


constructor TAssemblyDbModule.Create(ADb: TAssemblyDbCore);
begin
  inherited Create;
  FDb := ADb;
end;

destructor TAssemblyDbModule.Destroy;
begin
  inherited;
end;

//Override to create tables
procedure TAssemblyDbModule.CreateTables;
begin
end;

//Override to initialize precompiled statements
procedure TAssemblyDbModule.InitStatements;
begin
end;

end.
