unit AssemblyDb.Services;
//Stores service registrations
//Services are registered with special categorymembership entries

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TServiceId = int64;
  TServiceEntryData = record
    assemblyId: TAssemblyId;
    name: string;
    displayName: string;
    errorControl: string;
    imagePath: string;
    start: string;
    type_: string;
    description: string;
    objectName: string;
    sidType: string;
    requiredPrivileges: string;
  end;

  TServiceList = TDictionary<TServiceId, TServiceEntryData>;

  TAssemblyServices = class(TAssemblyDbModule)
  protected
    StmTouchService: PSQLite3Stmt;
    StmFindService: PSQLite3Stmt;
    StmUpdateService: PSQLite3Stmt;
    StmAddServiceGroupEntry: PSQLite3Stmt;
    procedure CreateTables; override;
    procedure InitStatements; override;
  public
    function AddService(const AData: TServiceEntryData): TServiceId;
    procedure UpdateService(AService: TServiceId; const AData: TServiceEntryData);

    procedure AddServiceGroupEntry(const AGroupName: string; const AName: string; const APosition: string);

    procedure QueryServices(const AStmt: PSQLite3Stmt; AList: TServiceList); overload;
    procedure GetAllServices(AList: TServiceList);

  end;

implementation
uses SysUtils, Windows;

procedure TAssemblyServices.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS services ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'displayName TEXT COLLATE NOCASE,'
    +'errorControl TEXT COLLATE NOCASE,'
    +'imagePath TEXT COLLATE NOCASE,'
    +'start TEXT COLLATE NOCASE,'
    +'type TEXT COLLATE NOCASE,'
    +'description TEXT COLLATE NOCASE,'
    +'objectName TEXT COLLATE NOCASE,'
    +'sidType TEXT COLLATE NOCASE,'
    +'requiredPrivileges TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId,name)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS serviceGroupEntries ('
    +'groupName TEXT NOT NULL COLLATE NOCASE,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'position TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(groupName,name)'
    +')');
end;

procedure TAssemblyServices.InitStatements;
begin
  StmTouchService := Db.PrepareStatement('INSERT OR IGNORE INTO services '
    +'(parentId,name) VALUES (?,?)');
  StmFindService := Db.PrepareStatement('SELECT id FROM services WHERE '
    +'parentId=? AND name=?');
  StmUpdateService := Db.PrepareStatement('UPDATE services SET displayName=?, errorControl=?, '
    +'imagePath=?, start=?, type=?, description=?, objectName=?, sidType=?, requiredPrivileges=? '
    +'WHERE id=? ');
  StmAddServiceGroupEntry := Db.PrepareStatement('INSERT OR IGNORE INTO serviceGroupEntries '
    +'(groupName,name,position) VALUES (?,?,?)');
end;


function TAssemblyServices.AddService(const AData: TServiceEntryData): TServiceId;
begin
  //Touch
  sqlite3_bind_int64(StmTouchService, 1, AData.assemblyId);
  sqlite3_bind_str(StmTouchService, 2, AData.name);
  if sqlite3_step(StmTouchService) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmTouchService);

  //Find id
  sqlite3_bind_int64(StmFindService, 1, AData.assemblyId);
  sqlite3_bind_str(StmFindService, 2, AData.name);
  if sqlite3_step(StmFindService) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindService, 0);
  sqlite3_reset(StmFindService);

  //Fill the rest of the fields
  UpdateService(Result, AData);
end;

procedure TAssemblyServices.UpdateService(AService: TServiceId; const AData: TServiceEntryData);
begin
  sqlite3_bind_str(StmUpdateService, 1, AData.displayName);
  sqlite3_bind_str(StmUpdateService, 2, AData.errorControl);
  sqlite3_bind_str(StmUpdateService, 3, AData.imagePath);
  sqlite3_bind_str(StmUpdateService, 4, AData.start);
  sqlite3_bind_str(StmUpdateService, 5, AData.type_);
  sqlite3_bind_str(StmUpdateService, 6, AData.description);
  sqlite3_bind_str(StmUpdateService, 7, AData.objectName);
  sqlite3_bind_str(StmUpdateService, 8, AData.sidType);
  sqlite3_bind_str(StmUpdateService, 9, AData.requiredPrivileges);
  sqlite3_bind_int64(StmUpdateService, 10, AService);
  if sqlite3_step(StmUpdateService) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmUpdateService);
end;

procedure TAssemblyServices.AddServiceGroupEntry(const AGroupName: string; const AName: string; const APosition: string);
begin
  sqlite3_bind_str(StmAddServiceGroupEntry, 1, AGroupName);
  sqlite3_bind_str(StmAddServiceGroupEntry, 2, AName);
  sqlite3_bind_str(StmAddServiceGroupEntry, 3, APosition);
  if sqlite3_step(StmAddServiceGroupEntry) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddServiceGroupEntry);
end;

//Parses the results of SELECT * FROM services
procedure TAssemblyServices.QueryServices(const AStmt: PSQLite3Stmt; AList: TServiceList);
var res: integer;
  AKey: TServiceId;
  AData: TServiceEntryData;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AKey := sqlite3_column_int64(AStmt, 0);
    AData.assemblyId := sqlite3_column_int64(AStmt, 1);
    AData.name := sqlite3_column_text16(AStmt, 2);
    AData.displayName := sqlite3_column_text16(AStmt, 3);
    AData.errorControl := sqlite3_column_text16(AStmt, 4);
    AData.imagePath := sqlite3_column_text16(AStmt, 5);
    AData.start := sqlite3_column_text16(AStmt, 6);
    AData.type_ := sqlite3_column_text16(AStmt, 7);
    AData.description := sqlite3_column_text16(AStmt, 8);
    AData.objectName := sqlite3_column_text16(AStmt, 9);
    AData.sidType := sqlite3_column_text16(AStmt, 10);
    AData.requiredPrivileges := sqlite3_column_text16(AStmt, 11);
    AList.AddOrSetValue(AKey, AData);
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(AStmt);
end;

procedure TAssemblyServices.GetAllServices(AList: TServiceList);
begin
  QueryServices(Db.PrepareStatement('SELECT * FROM services'), AList);
end;

end.
