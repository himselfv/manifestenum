unit AssemblyDb;

interface
uses SysUtils, Classes, sqlite3, Generics.Collections, AssemblyDb.Core, AssemblyDb.Assemblies,
  AssemblyDb.Registry, AssemblyDb.Services, AssemblyDb.UnusualProps;

type
  TDependencyEntryData = record
    discoverable: boolean;
    resourceType: string;
    dependentAssembly: TAssemblyId;
    dependencyType: string;
  end;


  TCategoryMembershipData = record
    name: string;
    version: string;
    publicKeyToken: string;
    typeName: string;
  end;
  TCategoryMemberships = TDictionary<TAssemblyId, TCategoryMembershipData>;


  TFolderId = int64;
  TFolderList = TDictionary<TFolderId, string>;

  TFolderReferenceData = record
    owner: boolean;
  end;
  TFolderReferees = TDictionary<TAssemblyId, TFolderReferenceData>;
  TFolderReferences = TDictionary<TFolderId, TFolderReferenceData>;

  TFileEntryData = record
    assembly: TAssemblyId;
    folder: TFolderId;
    name: string;
    sourceName: string;
    sourcePath: string;
    importPath: string;
  end;


  TTaskFolderId = int64;
  TTaskEntryData = record
    assemblyId: TAssemblyId;
    folderId: TTaskFolderId;
    name: string;
  end;

  TAssemblyDb = class(TAssemblyDbCore)
  protected
    StmAddDependency: PSQLite3Stmt;
    StmAddCategoryMembership: PSQLite3Stmt;

    StmTouchFolder: PSQLite3Stmt;
    StmFindFolder: PSQLite3Stmt;
    StmAddFolderReference: PSQLite3Stmt;
    StmAddFile: PSQLite3Stmt;

    StmTouchTaskFolder: PSQLite3Stmt;
    StmFindTaskFolder: PSQLite3Stmt;
    StmTouchTask: PSQLite3Stmt;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
    function SqlReadTaskData(stmt: PSQLite3Stmt): TTaskEntryData;
  public
    Assemblies: TAssemblyAssemblies;
    Registry: TAssemblyRegistry;
    Services: TAssemblyServices;
    UnusualProps: TAssemblyUnusualProps;
    constructor Create;

    procedure AddDependency(AAssembly: TAssemblyId; const AProperties: TDependencyEntryData);
    procedure GetDependencies(AAssembly: TAssemblyId; AList: TAssemblyList);
    procedure GetDependents(AAssembly: TAssemblyId; AList: TAssemblyList);

    procedure AddCategoryMembership(AAssembly: TAssemblyId; const AData: TCategoryMembershipData);
    procedure GetCategories(AList: TStringList);
    procedure GetCategoryMemberships(AList: TCategoryMemberships);

    function AddFolder(const AName: string; AParent: TFolderId = 0): TFolderId; overload;
    function AddFolderPath(APath: string): TFolderId;
    procedure AddFolderReference(AAssembly: TAssemblyId; AFolder: TFolderId; const AData: TFolderReferenceData);
    function AddFolder(AAssembly: TAssemblyId; APath: string; const AData: TFolderReferenceData): TFolderId; overload;
    procedure AddFile(const AData: TFileEntryData);
    procedure GetFolders(const AParent: TFolderId; AList: TFolderList);
    procedure GetFolderReferees(AFolder: TFolderId; AList: TFolderReferees);
    function GetFolderName(AFolder: TFolderId): string;
    function GetFolderPath(AFolder: TFolderId): string;
    function GetFileFullDestinationName(const AFile: TFileEntryData): string;
    procedure GetAssemblyFolders(AAssembly: TAssemblyId; AList: TFolderReferences);
    procedure QueryFiles(AStmt: PSQLite3Stmt; AList: TList<TFileEntryData>);
    procedure GetFiles(AFolder: TFolderId; AList: TList<TFileEntryData>);
    procedure GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);

    function AddTaskFolder(const AName: string; AParent: TTaskFolderId = 0): TTaskFolderId;
    procedure AddTask(AAssembly: TAssemblyId; AFolder: TTaskFolderId; const AName: string); overload;
    procedure AddTask(AAssembly: TAssemblyId; AURI: string); overload;
    function GetTaskFolderName(AKey: TTaskFolderId): string;
    function GetTaskFolderPath(AKey: TTaskFolderId): string;
    procedure QueryTasks(AStmt: PSQLite3Stmt; AList: TList<TTaskEntryData>);
    procedure GetAssemblyTasks(AAssembly: TAssemblyId; AList: TList<TTaskEntryData>);
    procedure GetTaskFolders(const AParent: TTaskFolderId; AList: TList<TTaskFolderId>);
    procedure GetTasks(const AParent: TTaskFolderId; AList: TList<TTaskEntryData>);

    procedure FilterAssemblyByName(const AFilter: string; AList: TAssemblyList);
    procedure FilterAssemblyByFile(const AFilter: string; AList: TAssemblyList);

  end;

function OpenAssemblyDb(const AFilename: string): TAssemblyDb;
procedure ResetAssemblyDb(const AFilename: string);

implementation

function OpenAssemblyDb(const AFilename: string): TAssemblyDb;
begin
  Result := TAssemblyDb.Create;
  Result.Open(AFilename);
end;

procedure ResetAssemblyDb(const AFilename: string);
begin
  DeleteFile(AFilename);
end;

constructor TAssemblyDb.Create;
begin
  inherited;
  Assemblies := TAssemblyAssemblies.Create(Self);
  AddModule(Assemblies);

  Registry := TAssemblyRegistry.Create(Self);
  AddModule(Registry);

  Services := TAssemblyServices.Create(Self);
  AddModule(Services);

  UnusualProps := TAssemblyUnusualProps.Create(Self);
  AddModule(UnusualProps);
end;


//Ensures all the tables are present
procedure TAssemblyDb.CreateTables;
begin
  inherited;
  Exec('CREATE TABLE IF NOT EXISTS dependencies ('
    +'assemblyId INTEGER NOT NULL,'
    +'discoverable BOOLEAN,'
    +'resourceType TEXT COLLATE NOCASE,'
    +'dependentAssemblyId INTEGER NOT NULL,'
    +'dependencyType TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(assemblyId,dependentAssemblyId)'
    +')');


  Exec('CREATE TABLE IF NOT EXISTS folders ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId, name)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS folderReferences ('
    +'assemblyId INTEGER NOT NULL,'
    +'folderId INTEGER NOT NULL,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,folderId)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS files ('
    +'assemblyId INTEGER NOT NULL,'
    +'folderId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'sourceName TEXT COLLATE NOCASE,'
    +'sourcePath TEXT COLLATE NOCASE,'
    +'importPath TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE (assemblyId,folderId,name)'
    +')');


  Exec('CREATE TABLE IF NOT EXISTS categoryMemberships ('
    +'assemblyId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'version TEXT NOT NULL COLLATE NOCASE,'
    +'publicKeyToken TEXT NOT NULL,'
    +'typeName TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(assemblyId,name,version,publicKeyToken,typeName)'
    +')');


  Exec('CREATE TABLE IF NOT EXISTS taskFolders ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId,name)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS tasks ('
    +'assemblyId INTEGER NOT NULL,'
    +'folderId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(folderId,name)'
    +')');
end;


{
Throughout the unit, three kinds of sql requests are used:
  Touch: create the entry with this identity if it does not exist.
  Find: find the entry with this identity and return its rowid/id
  Update: write optional fields by the rowid

Here's how they combine into:
  Add: Touch+Update = update the entry or create a new one
  Need: Touch+Find = find the entry or create a new one without optional fields

Notes:

When INSERT OR IGNORE ends in IGNORE, last rowid is not updated, so we have to query it explicitly.

INSERT OR IGNORES does silent IGNORE even when INSERT fails for reasons other than existing record.

SQLite cannot do INSERT OR UPDATE natively. The closes eqivalent, INSERT OR REPLACE, deletes and
recreates the record, losing all the ommited fields and the unique ID.
So when we need INSERT OR UPDATE, we will first INSERT OR IGNORE, to ensure the record is present,
then UPDATE it.
}


procedure TAssemblyDb.InitStatements;
begin
  inherited;
  StmAddDependency := PrepareStatement('INSERT OR REPLACE INTO dependencies '
    +'(assemblyId,discoverable,resourceType,dependentAssemblyId,dependencyType) '
    +'VALUES (?,?,?,?,?)');
  StmAddCategoryMembership := PrepareStatement('INSERT OR REPLACE INTO categoryMemberships '
    +'(assemblyId,name,version,publicKeyToken,typeName) '
    +'VALUES (?,?,?,?,?)');

  StmTouchFolder := PrepareStatement('INSERT OR IGNORE INTO folders '
    +'(parentId,name) VALUES (?,?)');
  StmFindFolder := PrepareStatement('SELECT id FROM folders WHERE '
    +'parentId=? AND name=?');
  StmAddFolderReference := PrepareStatement('INSERT OR IGNORE INTO folderReferences '
    +'(assemblyId,folderId,owner) VALUES (?,?,?)');
  StmAddFile := PrepareStatement('INSERT OR IGNORE INTO files '
    +'(assemblyId,folderId,name,sourceName,sourcePath,importPath) '
    +'VALUES (?,?,?,?,?,?)');

  StmTouchTaskFolder := PrepareStatement('INSERT OR IGNORE INTO taskFolders '
    +'(parentId,name) VALUES (?,?)');
  StmFindTaskFolder := PrepareStatement('SELECT id FROM taskFolders WHERE '
    +'parentId=? AND name=?');
  StmTouchTask := PrepareStatement('INSERT OR IGNORE INTO tasks '
    +'(assemblyId,folderId,name) VALUES (?,?,?)');
end;



procedure TAssemblyDb.AddDependency(AAssembly: TAssemblyId; const AProperties: TDependencyEntryData);
begin
  sqlite3_bind_int64(StmAddDependency, 1, AAssembly);
  sqlite3_bind_int(StmAddDependency, 2, integer(AProperties.discoverable));
  sqlite3_bind_str(StmAddDependency, 3, AProperties.resourceType);
  sqlite3_bind_int64(StmAddDependency, 4, AProperties.dependentAssembly);
  sqlite3_bind_str(StmAddDependency, 5, AProperties.dependencyType);
  if sqlite3_step(StmAddDependency) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddDependency);
end;

procedure TAssemblyDb.GetDependencies(AAssembly: TAssemblyId; AList: TAssemblyList);
var stmt: PSQLite3Stmt;
begin
  stmt := PrepareStatement('SELECT * FROM assemblies WHERE id IN (SELECT dependentAssemblyId FROM dependencies WHERE assemblyId=?)');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  Assemblies.QueryAssemblies(stmt, AList);
end;

procedure TAssemblyDb.GetDependents(AAssembly: TAssemblyId; AList: TAssemblyList);
var stmt: PSQLite3Stmt;
begin
  stmt := PrepareStatement('SELECT * FROM assemblies WHERE id IN (SELECT assemblyId FROM dependencies WHERE dependentAssemblyId=?)');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  Assemblies.QueryAssemblies(stmt, AList);
end;


procedure TAssemblyDb.AddCategoryMembership(AAssembly: TAssemblyId; const AData: TCategoryMembershipData);
begin
  sqlite3_bind_int64(StmAddCategoryMembership, 1, AAssembly);
  sqlite3_bind_str(StmAddCategoryMembership, 2, AData.name);
  sqlite3_bind_str(StmAddCategoryMembership, 3, AData.version);
  sqlite3_bind_str(StmAddCategoryMembership, 4, AData.publicKeyToken);
  sqlite3_bind_str(StmAddCategoryMembership, 5, AData.typeName);
  if sqlite3_step(StmAddCategoryMembership) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddCategoryMembership);
end;

procedure TAssemblyDb.GetCategories(AList: TStringList);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT DISTINCT(name) FROM categoryMemberships');
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AList.Add(sqlite3_column_text16(stmt, 0));
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyDb.GetCategoryMemberships(AList: TCategoryMemberships);
var stmt: PSQLite3Stmt;
  res: integer;
  AId: TAssemblyId;
  AData: TCategoryMembershipData;
begin
  stmt := PrepareStatement('SELECT * FROM categoryMemberships');
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AId := sqlite3_column_int64(stmt, 0);
    AData.name := sqlite3_column_text16(stmt, 1);
    AData.version := sqlite3_column_text16(stmt, 2);
    AData.publicKeyToken := sqlite3_column_text16(stmt, 3);
    AData.typeName := sqlite3_column_text16(stmt, 4);
    AList.AddOrSetValue(AId, AData);
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;


function TAssemblyDb.AddFolder(const AName: string; AParent: TFolderId = 0): TFolderId;
begin
  //Touch
  sqlite3_bind_int64(StmTouchFolder, 1, AParent);
  sqlite3_bind_str(StmTouchFolder, 2, AName);
  if sqlite3_step(StmTouchFolder) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmTouchFolder);

  //Find id
  sqlite3_bind_int64(StmFindFolder, 1, AParent);
  sqlite3_bind_str(StmFindFolder, 2, AName);
  if sqlite3_step(StmFindFolder) <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindFolder, 0);
  sqlite3_reset(StmFindFolder);
end;

//Overloaded version which parses the path and adds it folder by folder. Returns the leaf folder id.
function TAssemblyDb.AddFolderPath(APath: string): TFolderId;
var idx: integer;
begin
  Result := 0;
  while APath <> '' do begin
    idx := pos('\', APath);
    if idx <= 0 then begin
      Result := AddFolder(APath, Result);
      break;
    end;

    if idx = 1 then begin
      APath := copy(APath, 2, MaxInt);
      continue;
    end;

    Result := AddFolder(copy(APath, 1, idx-1), Result);
    APath := copy(APath, idx+1, MaxInt);
  end;
end;

procedure TAssemblyDb.AddFolderReference(AAssembly: TAssemblyId; AFolder: TFolderId; const AData: TFolderReferenceData);
begin
  sqlite3_bind_int64(StmAddFolderReference, 1, AAssembly);
  sqlite3_bind_int64(StmAddFolderReference, 2, AFolder);
  sqlite3_bind_int(StmAddFolderReference, 3, integer(AData.owner));
  if sqlite3_step(StmAddFolderReference) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddFolderReference);
end;

//Overloaded version which parses the path, adds it and links the leaf folder with the assembly.
function TAssemblyDb.AddFolder(AAssembly: TAssemblyId; APath: string; const AData: TFolderReferenceData): TFolderId;
begin
  Result := AddFolderPath(APath);
  if Result <> 0 then
    AddFolderReference(AAssembly, Result, AData);
end;

procedure TAssemblyDb.AddFile(const AData: TFileEntryData);
begin
  sqlite3_bind_int64(StmAddFile, 1, AData.assembly);
  sqlite3_bind_int64(StmAddFile, 2, AData.folder);
  sqlite3_bind_str(StmAddFile, 3, AData.name);
  sqlite3_bind_str(StmAddFile, 4, AData.sourceName);
  sqlite3_bind_str(StmAddFile, 5, AData.sourcePath);
  sqlite3_bind_str(StmAddFile, 6, AData.importPath);
  if sqlite3_step(StmAddFile) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddFile);
end;

//Retrieves the list of children folders for a given parent (0 for root)
procedure TAssemblyDb.GetFolders(const AParent: TFolderId; AList: TFolderList);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT id, name FROM folders WHERE parentId=?');
  sqlite3_bind_int64(stmt, 1, AParent);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AList.Add(sqlite3_column_int64(stmt, 0), sqlite3_column_text16(stmt, 1));
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyDb.GetFolderReferees(AFolder: TFolderId; AList: TFolderReferees);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TFolderReferenceData;
begin
  stmt := PrepareStatement('SELECT assemblyId FROM folderReferences WHERE folderId=?');
  sqlite3_bind_int64(stmt, 1, AFolder);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AData.owner := boolean(sqlite3_column_int(stmt, 1));
    AList.Add(sqlite3_column_int64(stmt, 0), AData);
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

function TAssemblyDb.GetFolderName(AFolder: TFolderId): string;
var stmt: PSQLite3Stmt;
begin
  stmt := PrepareStatement('SELECT name FROM folders WHERE id=?');
  sqlite3_bind_int64(stmt, 1, AFolder);
  if sqlite3_step(stmt) <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := sqlite3_column_text16(stmt, 0);
end;

function TAssemblyDb.GetFolderPath(AFolder: TFolderId): string;
var stmt: PSQLite3Stmt;
begin
  Result := '';
  stmt := PrepareStatement('SELECT parentId,name FROM folders WHERE id=?');
  while AFolder > 0 do begin
    sqlite3_bind_int64(stmt, 1, AFolder);
    if sqlite3_step(stmt) <> SQLITE_ROW then
      RaiseLastSQLiteError();
    AFolder := sqlite3_column_int64(stmt, 0); //parent
    if Result = '' then
      Result := sqlite3_column_text16(stmt, 1)
    else
      Result := sqlite3_column_text16(stmt, 1) + '\' + Result;
    sqlite3_reset(stmt);
  end;
end;

function TAssemblyDb.GetFileFullDestinationName(const AFile: TFileEntryData): string;
begin
  if AFile.folder = 0 then
    Result := AFile.name
  else
    Result := GetFolderPath(AFile.folder) + '\' + AFile.name;
end;

procedure TAssemblyDb.GetAssemblyFolders(AAssembly: TAssemblyId; AList: TFolderReferences);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TFolderReferenceData;
begin
  stmt := PrepareStatement('SELECT folderId FROM folderReferences WHERE assemblyId=?');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AData.owner := boolean(sqlite3_column_int(stmt, 1));
    AList.Add(sqlite3_column_int64(stmt, 0), AData);
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

function TAssemblyDb.SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
begin
  Result.assembly := sqlite3_column_int64(stmt, 0);
  Result.folder := sqlite3_column_int64(stmt, 1);
  Result.name := sqlite3_column_text16(stmt, 2);
  Result.sourceName := sqlite3_column_text16(stmt, 3);
  Result.sourcePath := sqlite3_column_text16(stmt, 4);
  Result.importPath := sqlite3_column_text16(stmt, 5);
end;

procedure TAssemblyDb.QueryFiles(AStmt: PSQLite3Stmt; AList: TList<TFileEntryData>);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadFileData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyDb.GetFiles(AFolder: TFolderId; AList: TList<TFileEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM files WHERE folderId=?');
  sqlite3_bind_int64(AStmt, 1, AFolder);
  QueryFiles(AStmt, AList);
end;

procedure TAssemblyDb.GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM files WHERE assemblyId=?');
  sqlite3_bind_int64(AStmt, 1, AAssembly);
  QueryFiles(AStmt, AList);
end;



function TAssemblyDb.AddTaskFolder(const AName: string; AParent: TTaskFolderId = 0): TTaskFolderId;
begin
  //Touch
  sqlite3_bind_int64(StmTouchTaskFolder, 1, AParent);
  sqlite3_bind_str(StmTouchTaskFolder, 2, AName);
  if sqlite3_step(StmTouchTaskFolder) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmTouchTaskFolder);

  //Find id
  sqlite3_bind_int64(StmFindTaskFolder, 1, AParent);
  sqlite3_bind_str(StmFindTaskFolder, 2, AName);
  if sqlite3_step(StmFindTaskFolder) <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindTaskFolder, 0);
  sqlite3_reset(StmFindTaskFolder);
end;

procedure TAssemblyDb.AddTask(AAssembly: TAssemblyId; AFolder: TTaskFolderId; const AName: string);
begin
  sqlite3_bind_int64(StmTouchTask, 1, AAssembly);
  sqlite3_bind_int64(StmTouchTask, 2, AFolder);
  sqlite3_bind_str(StmTouchTask, 3, AName);
  if sqlite3_step(StmTouchTask) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmTouchTask);
end;

procedure TAssemblyDb.AddTask(AAssembly: TAssemblyId; AURI: string);
var idx: integer;
  AFolder: TTaskFolderId;
begin
  AFolder := 0;
  while AURI <> '' do begin
    idx := pos('\', AURI);
    if idx <= 0 then begin
      AddTask(AAssembly, AFolder, AURI);
      break;
    end;

    if idx = 1 then begin
      AURI := copy(AURI, 2, MaxInt);
      continue;
    end;

    AFolder := AddTaskFolder(copy(AURI, 1, idx-1), AFolder);
    AURI := copy(AURI, idx+1, MaxInt);
  end;
end;

function TAssemblyDb.GetTaskFolderName(AKey: TTaskFolderId): string;
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT parentId,name FROM taskFolders WHERE id=?');
  sqlite3_bind_int64(stmt, 1, AKey);
  res := sqlite3_step(stmt);
  if res = SQLITE_ROW then
    Result := sqlite3_column_text16(stmt, 1)
  else begin
    if res <> SQLITE_DONE then
      RaiseLastSQLiteError();
    Result := '';
  end;
  sqlite3_reset(stmt);
end;

function TAssemblyDb.GetTaskFolderPath(AKey: TRegistryKeyId): string;
var stmt: PSQLite3Stmt;
begin
  Result := '';
  stmt := PrepareStatement('SELECT parentId,name FROM taskFolders WHERE id=?');
  while AKey > 0 do begin
    sqlite3_bind_int64(stmt, 1, AKey);
    if sqlite3_step(stmt) <> SQLITE_ROW then
      RaiseLastSQLiteError();
    AKey := sqlite3_column_int64(stmt, 0); //parent
    if Result = '' then
      Result := sqlite3_column_text16(stmt, 1)
    else
      Result := sqlite3_column_text16(stmt, 1) + '\' + Result;
    sqlite3_reset(stmt);
  end;
end;

function TAssemblyDb.SqlReadTaskData(stmt: PSQLite3Stmt): TTaskEntryData;
begin
  Result.assemblyId := sqlite3_column_int64(stmt, 0);
  Result.folderId := sqlite3_column_int64(stmt, 1);
  Result.name := sqlite3_column_text16(stmt, 2);
end;

procedure TAssemblyDb.QueryTasks(AStmt: PSQLite3Stmt; AList: TList<TTaskEntryData>);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadTaskData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyDb.GetAssemblyTasks(AAssembly: TAssemblyId; AList: TList<TTaskEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM tasks WHERE assemblyId=?');
  sqlite3_bind_int64(AStmt, 1, AAssembly);
  QueryTasks(AStmt, AList);
end;

procedure TAssemblyDb.GetTaskFolders(const AParent: TTaskFolderId; AList: TList<TTaskFolderId>);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT id FROM taskFolders WHERE parentId=?');
  sqlite3_bind_int64(stmt, 1, AParent);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AList.Add(sqlite3_column_int64(stmt, 0));
    res := sqlite3_step(stmt);
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyDb.GetTasks(const AParent: TTaskFolderId; AList: TList<TTaskEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM tasks WHERE folderId=?');
  sqlite3_bind_int64(AStmt, 1, AParent);
  QueryTasks(AStmt, AList);
end;




procedure TAssemblyDb.FilterAssemblyByName(const AFilter: string; AList: TAssemblyList);
begin
  Assemblies.QueryAssemblies('SELECT * FROM assemblies WHERE assemblies.name LIKE "%'+AFilter+'%"', AList);
end;

procedure TAssemblyDb.FilterAssemblyByFile(const AFilter: string; AList: TAssemblyList);
begin
  Assemblies.QueryAssemblies('SELECT * FROM assemblies WHERE assemblies.id IN (SELECT assemblyId FROM files WHERE files.name LIKE "%'+AFilter+'%")', AList);
end;



end.
