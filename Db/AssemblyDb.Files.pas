unit AssemblyDb.Files;

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TFolderId = int64;
  TFolderList = TDictionary<TFolderId, string>;

  TFolderReferenceData = record
    owner: boolean;
  end;
  TFolderReferees = TDictionary<TAssemblyId, TFolderReferenceData>;
  TFolderReferences = TDictionary<TFolderId, TFolderReferenceData>;

  TFileEntryId = int64;

  TFileEntryData = record
    id: TFileEntryId;
    assembly: TAssemblyId;
    folder: TFolderId;
    name: string;
    sourceName: string;
    sourcePath: string;
    importPath: string;
  end;
  PFileEntryData = ^TFileEntryData;

  TAssemblyFiles = class(TAssemblyDbModule)
  protected
    StmTouchFolder: PSQLite3Stmt;
    StmFindFolder: PSQLite3Stmt;
    StmAddFolderReference: PSQLite3Stmt;
    StmAddFile: PSQLite3Stmt;

    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
  public
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
    function QueryFile(AStmt: PSQLite3Stmt; out AData: TFileEntryData): boolean;
    procedure GetFiles(AFolder: TFolderId; AList: TList<TFileEntryData>);
    procedure GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);
    function GetFileEntryById(AId: TFileEntryId): TFileEntryData;

  end;


implementation

procedure TAssemblyFiles.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS folders ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId, name)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS folderReferences ('
    +'assemblyId INTEGER NOT NULL,'
    +'folderId INTEGER NOT NULL,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,folderId)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS files ('
    +'assemblyId INTEGER NOT NULL,'
    +'folderId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'sourceName TEXT COLLATE NOCASE,'
    +'sourcePath TEXT COLLATE NOCASE,'
    +'importPath TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE (assemblyId,folderId,name)'
    +')');
end;

procedure TAssemblyFiles.InitStatements;
begin
  StmTouchFolder := Db.PrepareStatement('INSERT OR IGNORE INTO folders '
    +'(parentId,name) VALUES (?,?)');
  StmFindFolder := Db.PrepareStatement('SELECT id FROM folders WHERE '
    +'parentId=? AND name=?');
  StmAddFolderReference := Db.PrepareStatement('INSERT OR IGNORE INTO folderReferences '
    +'(assemblyId,folderId,owner) VALUES (?,?,?)');
  StmAddFile := Db.PrepareStatement('INSERT OR IGNORE INTO files '
    +'(assemblyId,folderId,name,sourceName,sourcePath,importPath) '
    +'VALUES (?,?,?,?,?,?)');
end;



function TAssemblyFiles.AddFolder(const AName: string; AParent: TFolderId = 0): TFolderId;
begin
  //Touch
  sqlite3_bind_int64(StmTouchFolder, 1, AParent);
  sqlite3_bind_str(StmTouchFolder, 2, AName);
  if sqlite3_step(StmTouchFolder) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmTouchFolder);

  //Find id
  sqlite3_bind_int64(StmFindFolder, 1, AParent);
  sqlite3_bind_str(StmFindFolder, 2, AName);
  if sqlite3_step(StmFindFolder) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindFolder, 0);
  sqlite3_reset(StmFindFolder);
end;

//Overloaded version which parses the path and adds it folder by folder. Returns the leaf folder id.
function TAssemblyFiles.AddFolderPath(APath: string): TFolderId;
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

procedure TAssemblyFiles.AddFolderReference(AAssembly: TAssemblyId; AFolder: TFolderId; const AData: TFolderReferenceData);
begin
  sqlite3_bind_int64(StmAddFolderReference, 1, AAssembly);
  sqlite3_bind_int64(StmAddFolderReference, 2, AFolder);
  sqlite3_bind_int(StmAddFolderReference, 3, integer(AData.owner));
  if sqlite3_step(StmAddFolderReference) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddFolderReference);
end;

//Overloaded version which parses the path, adds it and links the leaf folder with the assembly.
function TAssemblyFiles.AddFolder(AAssembly: TAssemblyId; APath: string; const AData: TFolderReferenceData): TFolderId;
begin
  Result := AddFolderPath(APath);
  if Result <> 0 then
    AddFolderReference(AAssembly, Result, AData);
end;

procedure TAssemblyFiles.AddFile(const AData: TFileEntryData);
begin
  sqlite3_bind_int64(StmAddFile, 1, AData.assembly);
  sqlite3_bind_int64(StmAddFile, 2, AData.folder);
  sqlite3_bind_str(StmAddFile, 3, AData.name);
  sqlite3_bind_str(StmAddFile, 4, AData.sourceName);
  sqlite3_bind_str(StmAddFile, 5, AData.sourcePath);
  sqlite3_bind_str(StmAddFile, 6, AData.importPath);
  if sqlite3_step(StmAddFile) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddFile);
end;

//Retrieves the list of children folders for a given parent (0 for root)
procedure TAssemblyFiles.GetFolders(const AParent: TFolderId; AList: TFolderList);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := Db.PrepareStatement('SELECT id, name FROM folders WHERE parentId=?');
  sqlite3_bind_int64(stmt, 1, AParent);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AList.AddOrSetValue(sqlite3_column_int64(stmt, 0), sqlite3_column_text16(stmt, 1));
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

procedure TAssemblyFiles.GetFolderReferees(AFolder: TFolderId; AList: TFolderReferees);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TFolderReferenceData;
begin
  stmt := Db.PrepareStatement('SELECT assemblyId FROM folderReferences WHERE folderId=?');
  sqlite3_bind_int64(stmt, 1, AFolder);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AData.owner := boolean(sqlite3_column_int(stmt, 1));
    AList.AddOrSetValue(sqlite3_column_int64(stmt, 0), AData);
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

function TAssemblyFiles.GetFolderName(AFolder: TFolderId): string;
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT name FROM folders WHERE id=?');
  sqlite3_bind_int64(stmt, 1, AFolder);
  if sqlite3_step(stmt) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_text16(stmt, 0);
  sqlite3_reset(stmt);
end;

function TAssemblyFiles.GetFolderPath(AFolder: TFolderId): string;
var stmt: PSQLite3Stmt;
begin
  Result := '';
  stmt := Db.PrepareStatement('SELECT parentId,name FROM folders WHERE id=?');
  while AFolder > 0 do begin
    sqlite3_bind_int64(stmt, 1, AFolder);
    if sqlite3_step(stmt) <> SQLITE_ROW then
      Db.RaiseLastSQLiteError();
    AFolder := sqlite3_column_int64(stmt, 0); //parent
    if Result = '' then
      Result := sqlite3_column_text16(stmt, 1)
    else
      Result := sqlite3_column_text16(stmt, 1) + '\' + Result;
    sqlite3_reset(stmt);
  end;
end;

function TAssemblyFiles.GetFileFullDestinationName(const AFile: TFileEntryData): string;
begin
  if AFile.folder = 0 then
    Result := AFile.name
  else
    Result := GetFolderPath(AFile.folder) + '\' + AFile.name;
end;

procedure TAssemblyFiles.GetAssemblyFolders(AAssembly: TAssemblyId; AList: TFolderReferences);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TFolderReferenceData;
begin
  stmt := Db.PrepareStatement('SELECT folderId FROM folderReferences WHERE assemblyId=?');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AData.owner := boolean(sqlite3_column_int(stmt, 1));
    AList.Add(sqlite3_column_int64(stmt, 0), AData);
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;

function TAssemblyFiles.SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
begin
  Result.id := sqlite3_column_int64(stmt, 0);
  Result.assembly := sqlite3_column_int64(stmt, 1);
  Result.folder := sqlite3_column_int64(stmt, 2);
  Result.name := sqlite3_column_text16(stmt, 3);
  Result.sourceName := sqlite3_column_text16(stmt, 4);
  Result.sourcePath := sqlite3_column_text16(stmt, 5);
  Result.importPath := sqlite3_column_text16(stmt, 6);
end;

procedure TAssemblyFiles.QueryFiles(AStmt: PSQLite3Stmt; AList: TList<TFileEntryData>);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadFileData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

function TAssemblyFiles.QueryFile(AStmt: PSQLite3Stmt; out AData: TFileEntryData): boolean;
var res: integer;
begin
  res := sqlite3_step(AStmt);
  Result := (res = SQLITE_ROW);
  if Result then
    AData := SqlReadFileData(AStmt)
  else
    if res <> SQLITE_DONE then
      Db.RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyFiles.GetFiles(AFolder: TFolderId; AList: TList<TFileEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := Db.PrepareStatement('SELECT rowid, * FROM files WHERE folderId=?');
  sqlite3_bind_int64(AStmt, 1, AFolder);
  QueryFiles(AStmt, AList);
end;

procedure TAssemblyFiles.GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := Db.PrepareStatement('SELECT rowid, * FROM files WHERE assemblyId=?');
  sqlite3_bind_int64(AStmt, 1, AAssembly);
  QueryFiles(AStmt, AList);
end;

function TAssemblyFiles.GetFileEntryById(AId: TFileEntryId): TFileEntryData;
var AStmt: PSQLite3Stmt;
begin
  AStmt := Db.PrepareStatement('SELECT rowid, * FROM files WHERE rowid=?');
  sqlite3_bind_int64(AStmt, 1, AId);
  if not QueryFile(AStmt, Result) then
    raise EDatabaseError.Create('Item not found');
end;



end.
