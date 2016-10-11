unit AssemblyDb;

//{$DEFINE XML_OMNI}
//Использовать OmniXML вместо MSXML

interface
uses SysUtils, Classes, sqlite3, SxsExpand,
  {$IFDEF XML_OMNI}OmniXML{$ELSE}ComObj, MSXML{$ENDIF},
  Generics.Collections;

{$IFNDEF XML_OMNI}
//MSXML использует чуть иные названия и GUID для интерфейсов, но по функциям всё почти совместимо,
//так что заложусь на Omni, а MS переопределю.
type
  IXMLDocument = IXMLDOMDocument;
  IXMLNodeList = IXMLDOMNodeList;
  IXMLNode = IXMLDOMNode;
{$ENDIF}

type
  ESqliteError = class(Exception);

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

  TDependencyEntryData = record
    discoverable: boolean;
    resourceType: string;
    dependentAssembly: TAssemblyId;
    dependencyType: string;
  end;

  TFileEntryData = record
    name: string;
    destinationPath: string;
    sourceName: string;
    sourcePath: string;
    importPath: string;
    function fullDestinationName: string; inline;
  end;

  TDirectoryEntryData = record
    destinationPath: string;
    owner: boolean;
  end;

  TRegistryKeyId = int64;
  TRegistryKeyList = TDictionary<TRegistryKeyId, string>;

  TRegistryKeyReferenceData = record
    owner: boolean;
  end;
  TRegistryKeyReferees = TDictionary<TAssemblyId, TRegistryKeyReferenceData>;

  TRegistryValueData = record
    key: TRegistryKeyId;
    name: string;
    valueType: string;
    value: string;
    operationHint: string;
    owner: boolean;
  end;

  TCategoryMembershipData = record
    name: string;
    version: string;
    publicKeyToken: string;
    typeName: string;
  end;

  TTaskFolderId = int64;
  TTaskEntryData = record
    folderId: TTaskFolderId;
    name: string;
  end;

  TAssemblyDb = class
  protected
    FDb: PSQLite3;
    function GetLastSqliteError: string;
    procedure RaiseLastSqliteError;
    procedure Exec(const ASql: string);
    function PrepareStatement(const ASql: string): PSQLite3Stmt;
    procedure InitDb;
  public
    constructor Create;
    destructor Destroy; override;
    function Open(const AFilename: string): boolean;
    procedure Close;
    procedure BeginTransaction;
    procedure CommitTransaction;

  protected
    FPreparedStatements: TDictionary<string, PSQLite3Stmt>;
    StmTouchAssembly: PSQLite3Stmt;
    StmFindAssembly: PSQLite3Stmt;
    StmUpdateAssembly: PSQLite3Stmt;
    StmGetAssembly: PSQLite3Stmt;

    StmAddDependency: PSQLite3Stmt;
    StmAddCategoryMembership: PSQLite3Stmt;

    StmAddFile: PSQLite3Stmt;
    StmAddDirectory: PSQLite3Stmt;

    StmTouchRegistryKey: PSQLite3Stmt;
    StmFindRegistryKey: PSQLite3Stmt;
    StmAddRegistryKeyReference: PSQLite3Stmt;
    StmAddRegistryValue: PSQLite3Stmt;

    StmTouchTaskFolder: PSQLite3Stmt;
    StmFindTaskFolder: PSQLite3Stmt;
    StmTouchTask: PSQLite3Stmt;
    procedure InitStatements;
    procedure FreeStatements;
    function SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;
    function SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
    function SqlReadDirectoryData(stmt: PSQLite3Stmt): TDirectoryEntryData;
    function SqlReadRegistryValueData(stmt: PSQLite3Stmt): TRegistryValueData;
    function SqlReadTaskData(stmt: PSQLite3Stmt): TTaskEntryData;
  public
    function AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
    function NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
    function GetAssembly(AAssembly: TAssemblyId): TAssemblyData;
    procedure GetAllAssemblies(AList: TAssemblyList);
    procedure QueryAssemblies(const ASql: string; AList: TAssemblyList); overload;
    procedure QueryAssemblies(const AStmt: PSQLite3Stmt; AList: TAssemblyList); overload;

    procedure AddDependency(AAssembly: TAssemblyId; const AProperties: TDependencyEntryData);
    procedure GetDependencies(AAssembly: TAssemblyId; AList: TAssemblyList);
    procedure GetDependents(AAssembly: TAssemblyId; AList: TAssemblyList);
    procedure AddCategoryMembership(AAssembly: TAssemblyId; const AData: TCategoryMembershipData);

    procedure AddFile(AAssembly: TAssemblyId; const AFileData: TFileEntryData);
    procedure AddDirectory(AAssembly: TAssemblyId; const ADirectoryData: TDirectoryEntryData);
    procedure QueryFiles(AStmt: PSQLite3Stmt; AList: TList<TFileEntryData>);
    procedure GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);
    procedure QueryDirectories(AStmt: PSQLite3Stmt; AList: TList<TDirectoryEntryData>);
    procedure GetAssemblyDirectories(AAssembly: TAssemblyId; AList: TList<TDirectoryEntryData>);

    function AddRegistryKey(AName: string; AParent: TRegistryKeyId = 0): TRegistryKeyId; overload;
    procedure AddRegistryKeyReference(AAssembly: TAssemblyId; AKey: TRegistryKeyId; const AData: TRegistryKeyReferenceData);
    function AddRegistryKey(AAssembly: TAssemblyId; AName: string; const AData: TRegistryKeyReferenceData): TRegistryKeyId; overload;
    procedure AddRegistryValue(AAssembly: TAssemblyId; const AData: TRegistryValueData);
    procedure GetRegistryKeys(const AParent: TRegistryKeyId; AList: TRegistryKeyList);
    procedure GetRegistryKeyReferees(AKey: TRegistryKeyId; AList: TRegistryKeyReferees);
    function GetRegistryKeyPath(AKey: TRegistryKeyId): string;
    procedure GetAssemblyKeys(AAssembly: TAssemblyId; AList: TList<TRegistryValueData>);

    function AddTaskFolder(AName: string; AParent: TTaskFolderId = 0): TTaskFolderId;
    procedure AddTask(AAssembly: TAssemblyId; AFolder: TTaskFolderId; AName: string); overload;
    procedure AddTask(AAssembly: TAssemblyId; AURI: string); overload;
    function GetTaskFolderPath(AKey: TTaskFolderId): string;
    procedure QueryTasks(AStmt: PSQLite3Stmt; AList: TList<TTaskEntryData>);
    procedure GetAssemblyTasks(AAssembly: TAssemblyId; AList: TList<TTaskEntryData>);

    procedure FilterAssemblyByName(const AFilter: string; AList: TAssemblyList);
    procedure FilterAssemblyByFile(const AFilter: string; AList: TAssemblyList);

  protected
    FXml: IXMLDocument;
    procedure InitXmlParser;
    procedure FreeXmlParser;
    function XmlReadAssemblyIdentityData(const ANode: IXmlNode): TAssemblyIdentity;
    function XmlReadDependencyData(const ANode: IXmlNode): TDependencyEntryData;
    function XmlReadFileData(const ANode: IXmlNode): TFileEntryData;
    function XmlReadDirectoryData(const ANode: IXmlNode): TDirectoryEntryData;
    procedure ImportRegistryKeyNode(const AAssembly: TAssemblyId; ANode: IXmlNode);
    function XmlReadRegistryValueData(const AKeyId: TRegistryKeyId; const ANode: IXmlNode): TRegistryValueData;
    function XmlReadCategoryMembership(const ANode: IXmlNode): TCategoryMembershipData;
  public
    procedure ImportManifest(const AManifestFile: string);

  end;

function OpenAssemblyDb(const AFilename: string): TAssemblyDb;
procedure ResetAssemblyDb(const AFilename: string);

implementation

function TAssemblyIdentity.ToString: string;
begin
  Result := Self.name + '-' + Self.language + '-' + Self.buildType + '-' + Self.processorArchitecture
    + '-' + Self.version + '-' + Self.publicKeyToken;
end;

function TFileEntryData.fullDestinationName: string;
begin
  if (Length(Self.destinationPath) > 0) and (Self.destinationPath[Length(Self.destinationPath)]<>'\') then
    Result := Self.destinationPath+'\'+Self.name
  else
    Result := Self.destinationPath+Self.name;
end;

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
  FDb := nil;
end;

destructor TAssemblyDb.Destroy;
begin
  Close;
end;

//Retrieves text description for the last error that happened with the connection
function TAssemblyDb.GetLastSqliteError: string;
begin
  Result := sqlite3_errmsg16(FDb);
end;

//Raises ESqliteError for the last error that happened with the connection
procedure TAssemblyDb.RaiseLastSqliteError;
begin
  raise ESqliteError.Create(GetLastSqliteError);
end;

//Opens existing or creates a new database.
//If you need to force recreation, delete the database file before calling this.
function TAssemblyDb.Open(const AFilename: string): boolean;
var res: integer;
begin
  res := sqlite3_open16(PChar(AFilename), FDb);
  if res <> 0 then begin
    sqlite3_close(FDb); //handle is always returned
    FDb := nil;
    Result := false;
    exit;
  end;

  InitDb;
  InitStatements;
  InitXmlParser;

  Result := true;
end;

procedure TAssemblyDb.Close;
begin
  FreeXmlParser;
  if FDb <> nil then begin
    FreeStatements;
    sqlite3_close(FDb);
    FDb := nil;
  end;
end;

//Executes an instruction or throws an error
procedure TAssemblyDb.Exec(const ASql: string);
begin
  if sqlite3_exec(FDb, PAnsiChar(Utf8String(ASql)), nil, nil, nil) <> 0 then
    RaiseLastSqliteError();
end;

procedure TAssemblyDb.BeginTransaction;
begin
  Exec('BEGIN');
end;

procedure TAssemblyDb.CommitTransaction;
begin
  Exec('COMMIT');
end;

//Ensures all the tables are present
procedure TAssemblyDb.InitDb;
begin
  Exec('CREATE TABLE IF NOT EXISTS assemblies ('
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

  Exec('CREATE TABLE IF NOT EXISTS dependencies ('
    +'assemblyId INTEGER NOT NULL,'
    +'discoverable BOOLEAN,'
    +'resourceType TEXT COLLATE NOCASE,'
    +'dependentAssemblyId INTEGER NOT NULL,'
    +'dependencyType TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(assemblyId,dependentAssemblyId)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS files ('
    +'assemblyId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'destinationPath TEXT NOT NULL COLLATE NOCASE,'
    +'sourceName TEXT COLLATE NOCASE,'
    +'sourcePath TEXT COLLATE NOCASE,'
    +'importPath TEXT COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE (assemblyId,name,destinationPath)' //maybe all fields need to be included?
    +')');

  Exec('CREATE TABLE IF NOT EXISTS directories ('
    +'assemblyId INTEGER NOT NULL,'
    +'destinationPath TEXT NOT NULL COLLATE NOCASE,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,destinationPath)'
    +')');


  Exec('CREATE TABLE IF NOT EXISTS registryKeys ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'keyName TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId,keyName)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS registryKeyReferences ('
    +'assemblyId INTEGER NOT NULL,'
    +'keyId INTEGER NOT NULL,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,keyId)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS registryValues ('
    +'assemblyId INTEGER NOT NULL,'
    +'keyId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'valueType TEXT NOT NULL COLLATE NOCASE,'
    +'value TEXT NOT NULL COLLATE NOCASE,'
    +'operationHint TEXT NOT NULL COLLATE NOCASE,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,keyId,name)'
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
  FPreparedStatements := TDictionary<string, PSQLite3Stmt>.Create;

  StmTouchAssembly := PrepareStatement('INSERT OR IGNORE INTO assemblies '
    +'(name,language,buildType,processorArchitecture,version,publicKeyToken) '
    +'VALUES (?,?,?,?,?,?)');
  StmFindAssembly := PrepareStatement('SELECT id FROM assemblies WHERE '
    +'name=? AND language=? AND buildType=? AND processorArchitecture=? AND version=? AND publicKeyToken=?');
  StmUpdateAssembly := PrepareStatement('UPDATE assemblies SET manifestName=? '
    +'WHERE id=? ');
  StmGetAssembly := PrepareStatement('SELECT * FROM assemblies WHERE id=?');

  StmAddDependency := PrepareStatement('INSERT OR REPLACE INTO dependencies '
    +'(assemblyId,discoverable,resourceType,dependentAssemblyId,dependencyType) '
    +'VALUES (?,?,?,?,?)');
  StmAddCategoryMembership := PrepareStatement('INSERT OR REPLACE INTO categoryMemberships '
    +'(assemblyId,name,version,publicKeyToken,typeName) '
    +'VALUES (?,?,?,?,?)');

  StmAddFile := PrepareStatement('INSERT OR REPLACE INTO files '
    +'(assemblyId,name,destinationPath,sourceName,sourcePath,importPath) '
    +'VALUES (?,?,?,?,?,?)');
  StmAddDirectory := PrepareStatement('INSERT OR REPLACE INTO directories '
    +'(assemblyId,destinationPath,owner) '
    +'VALUES (?,?,?)');


  StmTouchRegistryKey := PrepareStatement('INSERT OR IGNORE INTO registryKeys '
    +'(parentId,keyName) VALUES (?,?)');
  StmFindRegistryKey := PrepareStatement('SELECT id FROM registryKeys WHERE '
    +'parentId=? AND keyName=?');
  StmAddRegistryKeyReference := PrepareStatement('INSERT OR IGNORE INTO registryKeyReferences '
    +'(assemblyId,keyId,owner) VALUES (?,?,?)');
  StmAddRegistryValue := PrepareStatement('INSERT OR IGNORE INTO registryValues '
    +'(assemblyId,keyId,name,valueType,value,operationHint,owner) '
    +'VALUES (?,?,?,?,?,?,?)');

  StmTouchTaskFolder := PrepareStatement('INSERT OR IGNORE INTO taskFolders '
    +'(parentId,name) VALUES (?,?)');
  StmFindTaskFolder := PrepareStatement('SELECT id FROM taskFolders WHERE '
    +'parentId=? AND name=?');
  StmTouchTask := PrepareStatement('INSERT OR IGNORE INTO tasks '
    +'(assemblyId,folderId,name) VALUES (?,?,?)');
end;

procedure TAssemblyDb.FreeStatements;
var stmt: PSQLite3Stmt;
begin
  for stmt in FPreparedStatements.Values do
    sqlite3_finalize(stmt);
  FreeAndNil(FPreparedStatements);
end;

function TAssemblyDb.PrepareStatement(const ASql: string): PSQLite3Stmt;
begin
  if FPreparedStatements.TryGetValue(ASql, Result) then
    exit;
  if sqlite3_prepare16_v2(FDb, PChar(ASql), -1, Result, nil) <> 0 then
    RaiseLastSQLiteError();
  FPreparedStatements.Add(ASql, Result);
end;

function sqlite3_bind_str(pStmt: PSQLite3Stmt; i: Integer; const zData: string): integer; inline;
begin
  Result := sqlite3_bind_text16(pStmt, i, PChar(zData), -1, nil);
end;


function TAssemblyDb.AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
begin
  Result := NeedAssembly(AEntry);
  //Update optional fields
  sqlite3_bind_str(StmUpdateAssembly, 1, AManifestName);
  sqlite3_bind_int64(StmUpdateAssembly, 2, Result);
  if sqlite3_step(StmUpdateAssembly) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmUpdateAssembly);
end;

function TAssemblyDb.NeedAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
var res: integer;
begin
  //Touch assembly
  sqlite3_bind_str(StmTouchAssembly, 1, AEntry.name);
  sqlite3_bind_str(StmTouchAssembly, 2, AEntry.language);
  sqlite3_bind_str(StmTouchAssembly, 3, AEntry.buildType);
  sqlite3_bind_str(StmTouchAssembly, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmTouchAssembly, 5, AEntry.version);
  sqlite3_bind_str(StmTouchAssembly, 6, AEntry.publicKeyToken);
  if sqlite3_step(StmTouchAssembly) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  Result := sqlite3_last_insert_rowid(FDb);
  sqlite3_reset(StmTouchAssembly);

  //Find assembly ID
  sqlite3_bind_str(StmFindAssembly, 1, AEntry.name);
  sqlite3_bind_str(StmFindAssembly, 2, AEntry.language);
  sqlite3_bind_str(StmFindAssembly, 3, AEntry.buildType);
  sqlite3_bind_str(StmFindAssembly, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmFindAssembly, 5, AEntry.version);
  sqlite3_bind_str(StmFindAssembly, 6, AEntry.publicKeyToken);
  res := sqlite3_step(StmFindAssembly);
  if res <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindAssembly, 0);
  sqlite3_reset(StmFindAssembly);
end;

//Parses a row from the assembles table into the record
function TAssemblyDb.SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;
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

function TAssemblyDb.GetAssembly(AAssembly: TAssemblyId): TAssemblyData;
begin
  sqlite3_bind_int64(StmGetAssembly, 1, AAssembly);
  if sqlite3_step(StmGetAssembly) <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := SqlReadAssemblyData(StmGetAssembly);
  sqlite3_reset(StmGetAssembly);
end;

//Makes an SQL query which returns a set of assembly table records.
procedure TAssemblyDb.QueryAssemblies(const ASql: string; AList: TAssemblyList);
begin
  QueryAssemblies(PrepareStatement(ASql), AList);
end;

procedure TAssemblyDb.QueryAssemblies(const AStmt: PSQLite3Stmt; AList: TAssemblyList);
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
    RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyDb.GetAllAssemblies(AList: TAssemblyList);
begin
  QueryAssemblies('SELECT * FROM assemblies', AList);
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
  QueryAssemblies(stmt, AList);
end;

procedure TAssemblyDb.GetDependents(AAssembly: TAssemblyId; AList: TAssemblyList);
var stmt: PSQLite3Stmt;
begin
  stmt := PrepareStatement('SELECT * FROM assemblies WHERE id IN (SELECT assemblyId FROM dependencies WHERE dependentAssemblyId=?)');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  QueryAssemblies(stmt, AList);
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


procedure TAssemblyDb.AddFile(AAssembly: TAssemblyId; const AFileData: TFileEntryData);
begin
  sqlite3_bind_int64(StmAddFile, 1, AAssembly);
  sqlite3_bind_str(StmAddFile, 2, AFileData.name);
  sqlite3_bind_str(StmAddFile, 3, AFileData.destinationPath);
  sqlite3_bind_str(StmAddFile, 4, AFileData.sourceName);
  sqlite3_bind_str(StmAddFile, 5, AFileData.sourcePath);
  sqlite3_bind_str(StmAddFile, 6, AFileData.importPath);
  if sqlite3_step(StmAddFile) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddFile);
end;

procedure TAssemblyDb.AddDirectory(AAssembly: TAssemblyId; const ADirectoryData: TDirectoryEntryData);
begin
  sqlite3_bind_int64(StmAddDirectory, 1, AAssembly);
  sqlite3_bind_str(StmAddDirectory, 2, ADirectoryData.destinationPath);
  sqlite3_bind_int(StmAddDirectory, 3, integer(ADirectoryData.owner));
  if sqlite3_step(StmAddDirectory) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddDirectory);
end;

function TAssemblyDb.SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
begin
  Result.name := sqlite3_column_text16(stmt, 1);
  Result.destinationPath := sqlite3_column_text16(stmt, 2);
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

procedure TAssemblyDb.GetAssemblyFiles(AAssembly: TAssemblyId; AList: TList<TFileEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM files WHERE assemblyId=?');
  sqlite3_bind_int64(AStmt, 1, AAssembly);
  QueryFiles(AStmt, AList);
end;

function TAssemblyDb.SqlReadDirectoryData(stmt: PSQLite3Stmt): TDirectoryEntryData;
begin
  Result.destinationPath := sqlite3_column_text16(stmt, 1);
  Result.owner := boolean(sqlite3_column_int(stmt, 2));
end;

procedure TAssemblyDb.QueryDirectories(AStmt: PSQLite3Stmt; AList: TList<TDirectoryEntryData>);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadDirectoryData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyDb.GetAssemblyDirectories(AAssembly: TAssemblyId; AList: TList<TDirectoryEntryData>);
var AStmt: PSQLite3Stmt;
begin
  AStmt := PrepareStatement('SELECT * FROM directories WHERE assemblyId=?');
  sqlite3_bind_int64(AStmt, 1, AAssembly);
  QueryDirectories(AStmt, AList);
end;


function TAssemblyDb.AddRegistryKey(AName: string; AParent: TRegistryKeyId = 0): TRegistryKeyId;
begin
  //Touch
  sqlite3_bind_int64(StmTouchRegistryKey, 1, AParent);
  sqlite3_bind_str(StmTouchRegistryKey, 2, AName);
  if sqlite3_step(StmTouchRegistryKey) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmTouchRegistryKey);

  //Find id
  sqlite3_bind_int64(StmFindRegistryKey, 1, AParent);
  sqlite3_bind_str(StmFindRegistryKey, 2, AName);
  if sqlite3_step(StmFindRegistryKey) <> SQLITE_ROW then
    RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindRegistryKey, 0);
  sqlite3_reset(StmFindRegistryKey);
end;

procedure TAssemblyDb.AddRegistryKeyReference(AAssembly: TAssemblyId; AKey: TRegistryKeyId; const AData: TRegistryKeyReferenceData);
begin
  sqlite3_bind_int64(StmAddRegistryKeyReference, 1, AAssembly);
  sqlite3_bind_int64(StmAddRegistryKeyReference, 2, AKey);
  sqlite3_bind_int(StmAddRegistryKeyReference, 3, integer(AData.owner));
  if sqlite3_step(StmAddRegistryKeyReference) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddRegistryKeyReference);
end;

//Overloaded version which parses Registry key name, adds all neccessary key entries and then links
//the final part with the assembly.
function TAssemblyDb.AddRegistryKey(AAssembly: TAssemblyId; AName: string; const AData: TRegistryKeyReferenceData): TRegistryKeyId;
var idx: integer;
begin
  Result := 0;
  while AName <> '' do begin
    idx := pos('\', AName);
    if idx <= 0 then begin
      Result := AddRegistryKey(AName, Result);
      break;
    end;

    if idx = 1 then begin
      AName := copy(AName, 2, MaxInt);
      continue;
    end;

    Result := AddRegistryKey(copy(AName, 1, idx-1), Result);
    AName := copy(AName, idx+1, MaxInt);
  end;

  if Result <> 0 then
    AddRegistryKeyReference(AAssembly, Result, AData);
end;

procedure TAssemblyDb.AddRegistryValue(AAssembly: TAssemblyId; const AData: TRegistryValueData);
begin
  sqlite3_bind_int64(StmAddRegistryValue, 1, AAssembly);
  sqlite3_bind_int64(StmAddRegistryValue, 2, AData.key);
  sqlite3_bind_str(StmAddRegistryValue, 3, AData.name);
  sqlite3_bind_str(StmAddRegistryValue, 4, AData.valueType);
  sqlite3_bind_str(StmAddRegistryValue, 5, AData.value);
  sqlite3_bind_str(StmAddRegistryValue, 6, AData.operationHint);
  sqlite3_bind_int(StmAddRegistryValue, 7, integer(AData.owner));
  if sqlite3_step(StmAddRegistryValue) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  sqlite3_reset(StmAddRegistryValue);
end;

//Retrieves the list of children key names for a given parent key id (0 for root)
procedure TAssemblyDb.GetRegistryKeys(const AParent: TRegistryKeyId; AList: TRegistryKeyList);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT id, keyName FROM registryKeys WHERE parentId=?');
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

procedure TAssemblyDb.GetRegistryKeyReferees(AKey: TRegistryKeyId; AList: TRegistryKeyReferees);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TRegistryKeyReferenceData;
begin
  stmt := PrepareStatement('SELECT assemblyId FROM registryKeyReferences WHERE keyId=?');
  sqlite3_bind_int64(stmt, 1, AKey);
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

function TAssemblyDb.GetRegistryKeyPath(AKey: TRegistryKeyId): string;
var stmt: PSQLite3Stmt;
begin
  Result := '';
  stmt := PrepareStatement('SELECT parentId,keyName FROM registryKeys WHERE id=?');
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

function TAssemblyDb.SqlReadRegistryValueData(stmt: PSQLite3Stmt): TRegistryValueData;
begin
  Result.key := sqlite3_column_int64(stmt, 1);
  Result.name := sqlite3_column_text16(stmt, 2);
  Result.valueType := sqlite3_column_text16(stmt, 3);
  Result.value := sqlite3_column_text16(stmt, 4);
  Result.operationHint := sqlite3_column_text16(stmt, 5);
  Result.owner := boolean(sqlite3_column_int(stmt, 6));
end;

procedure TAssemblyDb.GetAssemblyKeys(AAssembly: TAssemblyId; AList: TList<TRegistryValueData>);
var stmt: PSQLite3Stmt;
  res: integer;
begin
  stmt := PrepareStatement('SELECT * FROM registryValues WHERE assemblyId=?');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  res := sqlite3_step(stmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadRegistryValueData(stmt));
    res := sqlite3_step(stmt)
  end;
  if res <> SQLITE_DONE then
    RaiseLastSQLiteError;
  sqlite3_reset(stmt);
end;



function TAssemblyDb.AddTaskFolder(AName: string; AParent: TTaskFolderId = 0): TTaskFolderId;
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

procedure TAssemblyDb.AddTask(AAssembly: TAssemblyId; AFolder: TTaskFolderId; AName: string);
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




procedure TAssemblyDb.FilterAssemblyByName(const AFilter: string; AList: TAssemblyList);
begin
  QueryAssemblies('SELECT * FROM assemblies WHERE assemblies.name LIKE "%'+AFilter+'%"', AList);
end;

procedure TAssemblyDb.FilterAssemblyByFile(const AFilter: string; AList: TAssemblyList);
begin
  QueryAssemblies('SELECT * FROM assemblies WHERE assemblies.id IN (SELECT assemblyId FROM files WHERE files.name LIKE "%'+AFilter+'%")', AList);
end;



// Importing manifests

procedure TAssemblyDb.InitXmlParser;
begin
  // Nothing, we'll create it once we need it
end;

procedure TAssemblyDb.FreeXmlParser;
begin
  FXml := nil;
end;

procedure TAssemblyDb.ImportManifest(const AManifestFile: string);
var node: IXmlNode;
  nodes: IXMLNodeList;
  aId: TAssemblyId;
  i: integer;
  xmlStr: string;
begin
  if FXml = nil then begin
   {$IFDEF XML_OMNI}
    FXml := OmniXml.TXMLDocument.Create as IXMLDocument;
   {$ELSE}
    FXml := CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument;
   {$ENDIF}
  end;

  xmlStr := LoadManifestFile(AManifestFile);
  FXml.loadXML(xmlStr);

  node := FXml.selectSingleNode('/assembly/assemblyIdentity');
  Assert(node <> nil);
  aId := AddAssembly(XmlReadAssemblyIdentityData(node), ChangeFileExt(ExtractFilename(AManifestFile), ''));

  nodes := FXml.selectNodes('/assembly/dependency');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      AddDependency(aId, XmlReadDependencyData(nodes.item[i]));
  end;

  nodes := FXml.selectNodes('/assembly/file');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      AddFile(aId, XmlReadFileData(nodes.item[i]));
  end;

  nodes := FXml.selectNodes('/assembly/directories/directory');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      AddDirectory(aId, XmlReadDirectoryData(nodes.item[i]));
  end;

  nodes := FXml.selectNodes('/assembly/registryKeys/registryKey');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      ImportRegistryKeyNode(aId, nodes.item[i]);
  end;

  nodes := FXml.selectNodes('/assembly/memberships/categoryMembership/id');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      AddCategoryMembership(aId, XmlReadCategoryMembership(nodes.item[i]));
  end;

  nodes := FXml.selectNodes('/assembly/taskScheduler/Task/RegistrationInfo/URI');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      Self.AddTask(aId, nodes.item[i].text);
  end;
end;

function textAttribute(const ANode: IXmlNode; const AAttribName: string): string; inline;
var AAttrib: IXmlDomNode;
begin
  AAttrib := ANode.attributes.getNamedItem(AAttribName);
  if AAttrib <> nil then
    Result := AAttrib.text
  else
    Result := '';
end;

function boolAttribute(const ANode: IXmlNode; const AAttribName: string): boolean; inline;
begin
  Result := StrToBoolDef(textAttribute(ANode, AAttribName), false);
end;

//Parses a given assemblyIdentity node, extracting all the fields that identify an assembly
function TAssemblyDb.XmlReadAssemblyIdentityData(const ANode: IXmlNode): TAssemblyIdentity;
begin
  Result.name := textAttribute(ANode, 'name');
  Result.language := textAttribute(ANode, 'language');
  Result.buildType := textAttribute(ANode, 'buildType');
  Result.processorArchitecture := textAttribute(ANode, 'processorArchitecture');
  Result.version := textAttribute(ANode, 'version');
  Result.publicKeyToken := textAttribute(ANode, 'publicKeyToken');
end;

function TAssemblyDb.XmlReadDependencyData(const ANode: IXmlNode): TDependencyEntryData;
var depAss: IXmlNode;
begin
  Result.discoverable := boolAttribute(ANode, 'discoverable');
  Result.resourceType := textAttribute(ANode, 'resourceType');
  depAss := ANode.selectSingleNode('dependentAssembly/assemblyIdentity');
  if depAss = nil then begin
    Result.dependentAssembly := 0;
    Result.dependencyType := '';
  end else begin
    Result.dependentAssembly := NeedAssembly(XmlReadAssemblyIdentityData(depAss));
    Result.dependencyType := textAttribute(ANode.selectSingleNode('dependentAssembly'), 'dependencyType');
  end;
end;

function TAssemblyDb.XmlReadFileData(const ANode: IXmlNode): TFileEntryData;
begin
  Result.name := textAttribute(ANode, 'name');
  Result.destinationPath := textAttribute(ANode, 'destinationPath');
  Result.sourceName := textAttribute(ANode, 'sourceName');
  Result.sourcePath := textAttribute(ANode, 'sourcePath');
  Result.importPath := textAttribute(ANode, 'importPath');
end;

function TAssemblyDb.XmlReadDirectoryData(const ANode: IXmlNode): TDirectoryEntryData;
begin
  Result.destinationPath := textAttribute(ANode, 'destinationPath');
  Result.owner := boolAttribute(ANode, 'owner');
end;

procedure TAssemblyDb.ImportRegistryKeyNode(const AAssembly: TAssemblyId; ANode: IXmlNode);
var AKeyName: string;
  AKeyData: TRegistryKeyReferenceData;
  AKeyId: TRegistryKeyId;
  nodes: IXmlNodeList;
  i: integer;
begin
  AKeyName := textAttribute(ANode, 'keyName');
  AKeyData.owner := boolAttribute(ANode, 'owner');
  AKeyId := AddRegistryKey(AAssembly, AKeyName, AKeyData);

  nodes := ANode.selectNodes('registryValue');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      AddRegistryValue(AAssembly, XmlReadRegistryValueData(AKeyId, nodes.item[i]));
  end;
end;

function TAssemblyDb.XmlReadRegistryValueData(const AKeyId: TRegistryKeyId; const ANode: IXmlNode): TRegistryValueData;
begin
  Result.key := AKeyId;
  Result.name := textAttribute(ANode, 'name');
  Result.valueType := textAttribute(ANode, 'valueType');
  Result.value := textAttribute(ANode, 'value');
  Result.operationHint := textAttribute(ANode, 'operationHint');
  Result.owner := boolAttribute(ANode, 'owner');
end;

function TAssemblyDb.XmlReadCategoryMembership(const ANode: IXmlNode): TCategoryMembershipData;
begin
  Result.name := textAttribute(ANode, 'name');
  Result.version := textAttribute(ANode, 'version');
  Result.publicKeyToken := textAttribute(ANode, 'publicKeyToken');
  Result.typeName := textAttribute(ANode, 'typeName');
end;

end.
