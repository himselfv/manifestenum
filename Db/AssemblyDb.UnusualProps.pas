unit AssemblyDb.UnusualProps;
//Enumerates unusual assembly XML properties encountered during import

interface
uses sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

const
  PROPTYPE_UNKNOWN  = 0;
  PROPTYPE_NODENAME = 1;

type
  TAssemblyUnusualProps = class(TAssemblyDbModule)
  protected
    StmAdd: PSQLite3Stmt;
    procedure CreateTables; override;
    procedure InitStatements; override;
  public
    procedure Add(AAssembly: TAssemblyId; AType: integer; const AValue: string);
  end;

implementation

procedure TAssemblyUnusualProps.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS unusualProperties ('
    +'assemblyId INTEGER NOT NULL,'
    +'type INTEGER NOT NULL,'
    +'value TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(assemblyId,type,value)'
    +')');
end;

procedure TAssemblyUnusualProps.InitStatements;
begin
  StmAdd := Db.PrepareStatement('INSERT OR IGNORE INTO unusualProperties '
    +'(assemblyId,type,value) VALUES (?,?,?)');
end;

procedure TAssemblyUnusualProps.Add(AAssembly: TAssemblyId; AType: integer; const AValue: string);
begin
  sqlite3_bind_int64(StmAdd, 1, AAssembly);
  sqlite3_bind_int(StmAdd, 2, AType);
  sqlite3_bind_str(StmAdd, 3, AValue);
  if sqlite3_step(StmAdd) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAdd);
end;

end.
