unit AssemblyDb;

//{$DEFINE XML_OMNI}
//Использовать OmniXML вместо MSXML

interface
uses SysUtils, sqlite3,
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
  end;
  TAssemblyData = record
    id: TAssemblyId;
    identity: TAssemblyIdentity;
    manifestName: string;
  end;

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
  end;

  TDirectoryEntryData = record
    destinationPath: string;
    owner: boolean;
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
    StmAddAssembly: PSQLite3Stmt;
    StmFindAssembly: PSQLite3Stmt;
    StmAddDependency: PSQLite3Stmt;
    StmAddFile: PSQLite3Stmt;
    StmAddDirectory: PSQLite3Stmt;
    procedure InitStatements;
    procedure FreeStatements;
    function SqlReadAssemblyData(stmt: PSQLite3Stmt): TAssemblyData;
    function SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
  public
    function AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
    function FindAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
    procedure AddDependency(AAssembly: TAssemblyId; const AProperties: TDependencyEntryData);
    procedure AddFile(AAssembly: TAssemblyId; const AFileData: TFileEntryData);
    procedure AddDirectory(AAssembly: TAssemblyId; const ADirectoryData: TDirectoryEntryData);

    function QueryAssemblies(const ASql: string): TList<TAssemblyData>;
    function GetAllAssemblies: TList<TAssemblyData>;
    function FindAssemblyByFile(const AFilter: string): TList<TAssemblyData>;

    function QueryFiles(const ASql: string): TList<TFileEntryData>;
    function GetAssemblyFiles(AAssembly: TAssemblyId): TList<TFileEntryData>;

  protected
    FXml: IXMLDocument;
    procedure InitXmlParser;
    procedure FreeXmlParser;
    function XmlReadAssemblyIdentityData(const ANode: IXmlNode): TAssemblyIdentity;
    function XmlReadDependencyData(const ANode: IXmlNode): TDependencyEntryData;
    function XmlReadFileData(const ANode: IXmlNode): TFileEntryData;
    function XmlReadDirectoryData(const ANode: IXmlNode): TDirectoryEntryData;
  public
    procedure ImportManifest(const AManifestFile: string);

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
    +'name TEXT NOT NULL,'
    +'language TEXT NOT NULL,'
    +'buildType TEXT NOT NULL,'
    +'processorArchitecture TEXT NOT NULL,'
    +'version TEXT NOT NULL,'
    +'publicKeyToken TEXT NOT NULL,'
    +'manifestName TEXT UNIQUE NOT NULL,'
    +'CONSTRAINT identity UNIQUE(name,language,buildType,processorArchitecture,version,publicKeyToken)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS dependencies ('
    +'assemblyId INTEGER NOT NULL,'
    +'discoverable BOOLEAN,'
    +'resourceType TEXT,'
    +'dependentAssemblyId INTEGER NOT NULL,'
    +'dependencyType TEXT,'
    +'CONSTRAINT identity UNIQUE(assemblyId,dependentAssemblyId)'
    +')');

  Exec('CREATE TABLE IF NOT EXISTS files ('
    +'assemblyId INTEGER NOT NULL,'
    +'name TEXT NOT NULL,'
    +'destinationPath TEXT NOT NULL,'
    +'sourceName TEXT,'
    +'sourcePath TEXT,'
    +'importPath TEXT,'
    +'CONSTRAINT identity UNIQUE (assemblyId,name,destinationPath)' //maybe all fields need to be included?
    +')');

  Exec('CREATE TABLE IF NOT EXISTS directories ('
    +'assemblyId INTEGER NOT NULL,'
    +'destinationPath TEXT NOT NULL,'
    +'owner BOOLEAN,'
    +'CONSTRAINT identity UNIQUE(assemblyId,destinationPath)'
    +')');
end;

function TAssemblyDb.PrepareStatement(const ASql: string): PSQLite3Stmt;
begin
  if sqlite3_prepare16_v2(FDb, PChar(ASql), -1, Result, nil) <> 0 then
    RaiseLastSQLiteError();
end;

procedure TAssemblyDb.InitStatements;
begin
  StmAddAssembly := PrepareStatement('INSERT OR REPLACE INTO assemblies '
    +'(name,language,buildType,processorArchitecture,version,publicKeyToken,manifestName) '
    +'VALUES (?,?,?,?,?,?,?)');
  StmFindAssembly := PrepareStatement('INSERT OR IGNORE INTO assemblies '
    +'(name,language,buildType,processorArchitecture,version,publicKeyToken) '
    +'VALUES (?,?,?,?,?,?)');
  StmAddDependency := PrepareStatement('INSERT OR REPLACE INTO dependencies '
    +'(assemblyId,discoverable,resourceType,dependentAssemblyId,dependencyType) '
    +'VALUES (?,?,?,?,?)');
  StmAddFile := PrepareStatement('INSERT OR REPLACE INTO files '
    +'(assemblyId,name,destinationPath,sourceName,sourcePath,importPath) '
    +'VALUES (?,?,?,?,?,?)');
  StmAddDirectory := PrepareStatement('INSERT OR REPLACE INTO directories '
    +'(assemblyId,destinationPath,owner) '
    +'VALUES (?,?,?)');
end;

procedure TAssemblyDb.FreeStatements;
begin
  sqlite3_finalize(StmAddAssembly);
  sqlite3_finalize(StmFindAssembly);
  sqlite3_finalize(StmAddDependency);
  sqlite3_finalize(StmAddFile);
  sqlite3_finalize(StmAddDirectory);
end;

function sqlite3_bind_str(pStmt: PSQLite3Stmt; i: Integer; const zData: string): integer; inline;
begin
  Result := sqlite3_bind_text16(pStmt, i, PChar(zData), -1, nil);
end;


function TAssemblyDb.AddAssembly(const AEntry: TAssemblyIdentity; const AManifestName: string): TAssemblyId;
begin
  sqlite3_bind_str(StmAddAssembly, 1, AEntry.name);
  sqlite3_bind_str(StmAddAssembly, 2, AEntry.language);
  sqlite3_bind_str(StmAddAssembly, 3, AEntry.buildType);
  sqlite3_bind_str(StmAddAssembly, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmAddAssembly, 5, AEntry.version);
  sqlite3_bind_str(StmAddAssembly, 6, AEntry.publicKeyToken);
  sqlite3_bind_str(StmAddAssembly, 7, AManifestName);
  if sqlite3_step(StmAddAssembly) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  Result := sqlite3_last_insert_rowid(FDb);
  sqlite3_reset(StmAddAssembly);
end;

function TAssemblyDb.FindAssembly(const AEntry: TAssemblyIdentity): TAssemblyId;
begin
  sqlite3_bind_str(StmFindAssembly, 1, AEntry.name);
  sqlite3_bind_str(StmFindAssembly, 2, AEntry.language);
  sqlite3_bind_str(StmFindAssembly, 3, AEntry.buildType);
  sqlite3_bind_str(StmFindAssembly, 4, AEntry.processorArchitecture);
  sqlite3_bind_str(StmFindAssembly, 5, AEntry.version);
  sqlite3_bind_str(StmFindAssembly, 6, AEntry.publicKeyToken);
  if sqlite3_step(StmFindAssembly) <> SQLITE_DONE then
    RaiseLastSQLiteError();
  Result := sqlite3_last_insert_rowid(FDb);
  sqlite3_reset(StmFindAssembly);
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

//Makes an SQL query which returns a set of assembly table records.
//The returned object must be destroyed by the caller.
function TAssemblyDb.QueryAssemblies(const ASql: string): TList<TAssemblyData>;
var stmt: PSQLite3Stmt;
  res: integer;
begin
  Result := TList<TAssemblyData>.Create;
  stmt := PrepareStatement(ASql);
  try
    res := sqlite3_step(stmt);
    while res = SQLITE_ROW do begin
      Result.Add(SqlReadAssemblyData(stmt));
      res := sqlite3_step(stmt)
    end;
    if res <> SQLITE_DONE then
      RaiseLastSQLiteError;
  finally
    sqlite3_finalize(stmt);
  end;
end;

function TAssemblyDb.GetAllAssemblies: TList<TAssemblyData>;
begin
  Result := QueryAssemblies('SELECT * FROM assemblies');
end;

function TAssemblyDb.FindAssemblyByFile(const AFilter: string): TList<TAssemblyData>;
begin
  Result := QueryAssemblies('SELECT * FROM assemblies WHERE assemblies.id IN (SELECT assemblyId FROM files WHERE files.name LIKE "%'+AFilter+'%")');
end;

function TAssemblyDb.SqlReadFileData(stmt: PSQLite3Stmt): TFileEntryData;
begin
  Result.name := sqlite3_column_text16(stmt, 1);
  Result.destinationPath := sqlite3_column_text16(stmt, 2);
  Result.sourceName := sqlite3_column_text16(stmt, 3);
  Result.sourcePath := sqlite3_column_text16(stmt, 4);
  Result.importPath := sqlite3_column_text16(stmt, 5);
end;

function TAssemblyDb.QueryFiles(const ASql: string): TList<TFileEntryData>;
var stmt: PSQLite3Stmt;
  res: integer;
begin
  Result := TList<TFileEntryData>.Create;
  stmt := PrepareStatement(ASql);
  try
    res := sqlite3_step(stmt);
    while res = SQLITE_ROW do begin
      Result.Add(SqlReadFileData(stmt));
      res := sqlite3_step(stmt)
    end;
    if res <> SQLITE_DONE then
      RaiseLastSQLiteError;
  finally
    sqlite3_finalize(stmt);
  end;
end;

function TAssemblyDb.GetAssemblyFiles(AAssembly: TAssemblyId): TList<TFileEntryData>;
begin
  Result := QueryFiles('SELECT * FROM files WHERE assemblyId='+IntToStr(AAssembly));
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
begin
  if FXml = nil then begin
   {$IFDEF XML_OMNI}
    FXml := OmniXml.TXMLDocument.Create as IXMLDocument;
   {$ELSE}
    FXml := CreateOleObject('Microsoft.XMLDOM') as IXMLDOMDocument;
   {$ENDIF}
  end;

  FXml.load(AManifestFile);

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
  Result.discoverable := StrToBoolDef(textAttribute(ANode, 'discoverable'), false);
  Result.resourceType := textAttribute(ANode, 'resourceType');
  depAss := ANode.selectSingleNode('dependentAssembly/assemblyIdentity');
  if depAss = nil then begin
    Result.dependentAssembly := 0;
    Result.dependencyType := '';
  end else begin
    Result.dependentAssembly := FindAssembly(XmlReadAssemblyIdentityData(depAss));
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
  Result.owner := StrToBoolDef(textAttribute(ANode, 'owner'), false);
end;

end.
