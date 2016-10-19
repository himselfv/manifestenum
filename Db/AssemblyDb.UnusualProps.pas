unit AssemblyDb.UnusualProps;
//Enumerates unusual assembly XML properties encountered during import

interface
uses sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TAssemblyUnusualProps = class(TAssemblyDbModule)
  protected
    StmAdd: PSQLite3Stmt;
    procedure CreateTables; override;
    procedure InitStatements; override;
  public
    procedure Add(AAssembly: TAssemblyId; const ANodeName: string);
  end;

implementation

procedure TAssemblyUnusualProps.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS unusualProperties ('
    +'assemblyId INTEGER NOT NULL,'
    +'nodeName TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(assemblyId,nodeName)'
    +')');
end;

procedure TAssemblyUnusualProps.InitStatements;
begin
  StmAdd := Db.PrepareStatement('INSERT OR IGNORE INTO unusualProperties '
    +'(assemblyId,nodeName) VALUES (?,?)');
end;

procedure TAssemblyUnusualProps.Add(AAssembly: TAssemblyId; const ANodeName: string);
begin
  sqlite3_bind_int64(StmAdd, 1, AAssembly);
  sqlite3_bind_str(StmAdd, 2, ANodeName);
  if sqlite3_step(StmAdd) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAdd);
end;

end.
