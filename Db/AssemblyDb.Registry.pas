unit AssemblyDb.Registry;
//Stores registry data

interface
uses Generics.Collections, sqlite3, AssemblyDb.Core, AssemblyDb.Assemblies;

type
  TRegistryValueType = cardinal; //= windows.pas REG_*

const //Some Delphi versions lack some of these. Add conditional defines as needed
  REG_RESOURCE_LIST              = 8;
  REG_FULL_RESOURCE_DESCRIPTOR   = 9;
  REG_RESOURCE_REQUIREMENTS_LIST = 10;
  REG_QWORD                      = 11;
  REG_QWORD_LITTLE_ENDIAN        = 11;

type
  TRegistryKeyId = int64;
  TRegistryKeyList = TDictionary<TRegistryKeyId, string>;

  TRegistryKeyReferenceData = record
    owner: boolean;
  end;
  TRegistryKeyReferees = TDictionary<TAssemblyId, TRegistryKeyReferenceData>;

  TRegistryValueId = int64;

  TRegistryValueData = record
    id: TRegistryValueId;
    assembly: TAssemblyId;
    key: TRegistryKeyId;
    name: string;
    valueType: TRegistryValueType;
    value: string;
    operationHint: string;
    owner: boolean;
  end;
  PRegistryValueData = ^TRegistryValueData;

  TRegistryValueList = TList<TRegistryValueData>;

  TAssemblyRegistry = class(TAssemblyDbModule)
  protected
    FKeyCache: TDictionary<string, TRegistryKeyId>;
    StmTouchKey: PSQLite3Stmt;
    StmFindKey: PSQLite3Stmt;
    StmAddKeyReference: PSQLite3Stmt;
    StmAddValue: PSQLite3Stmt;
    procedure Initialize; override;
    procedure CreateTables; override;
    procedure InitStatements; override;
    function SqlReadValueData(stmt: PSQLite3Stmt): TRegistryValueData;
  public
    destructor Destroy; override;

    function AddKey(const AName: string; AParent: TRegistryKeyId = 0): TRegistryKeyId; overload;
    function AddKeyPath(APath: string): TRegistryKeyId; overload;
    procedure AddKeyReference(AAssembly: TAssemblyId; AKey: TRegistryKeyId; const AData: TRegistryKeyReferenceData);
    function AddKey(AAssembly: TAssemblyId; APath: string; const AData: TRegistryKeyReferenceData): TRegistryKeyId; overload;
    procedure AddValue(AAssembly: TAssemblyId; const AData: TRegistryValueData);
    procedure QueryKeys(AStmt: PSQLite3Stmt; AList: TRegistryKeyList); overload;
    procedure GetKeys(const AParent: TRegistryKeyId; AList: TRegistryKeyList);
    procedure GetKeyReferees(AKey: TRegistryKeyId; AList: TRegistryKeyReferees);
    function GetKeyName(AKey: TRegistryKeyId): string;
    function GetKeyPath(AKey: TRegistryKeyId): string;
    function FindKeyByPath(const APath: string; ARoot: TRegistryKeyId = 0): TRegistryKeyId;

    procedure QueryValues(AStmt: PSQLite3Stmt; AList: TRegistryValueList);
    function QueryValue(AStmt: PSQLite3Stmt; out AData: TRegistryValueData): boolean;
    procedure GetKeyValues(AKey: TRegistryKeyId; AList: TRegistryValueList);
    procedure GetAssemblyValues(AAssembly: TAssemblyId; AList: TRegistryValueList);
    function GetValueById(AId: TRegistryValueId): TRegistryValueData;

  end;

function GetRegistryValueTypeName(const AValueType: TRegistryValueType): string;
function DecodeRegistryValueType(AText: string): TRegistryValueType;

implementation
uses SysUtils, Windows;

//Returns text name for a registry value type
function GetRegistryValueTypeName(const AValueType: TRegistryValueType): string;
begin
  case AValueType of
    REG_NONE: Result := 'REG_NONE';
    REG_SZ: Result := 'REG_SZ';
    REG_EXPAND_SZ: Result := 'REG_EXPAND_SZ';
    REG_BINARY: Result := 'REG_BINARY';
    REG_DWORD: Result := 'REG_DWORD';
    REG_DWORD_BIG_ENDIAN: Result := 'REG_DWORD_BIG_ENDIAN';
    REG_LINK: Result := 'REG_LINK';
    REG_MULTI_SZ: Result := 'REG_MULTI_SZ';
    REG_RESOURCE_LIST: Result := 'REG_RESOURCE_LIST';
    REG_FULL_RESOURCE_DESCRIPTOR: Result := 'REG_FULL_RESOURCE_DESCRIPTOR';
    REG_RESOURCE_REQUIREMENTS_LIST: Result := 'REG_RESOURCE_REQUIREMENTS_LIST';
    REG_QWORD: Result := 'REG_QWORD';
  else Result := '';
  end;
end;

function cregpref(const AStr: string): boolean; inline;
begin
  Result := (AStr[1]='R') and (AStr[2]='E') and (AStr[3]='G') and (AStr[4]='_');
end;

function cregcmp(const AStr: string; const AVal: string): boolean;
var i: integer;
begin
  Result := false;
  if Length(AStr) <> Length(AVal) then
    exit;
  for i := 1 to Length(AStr) do
    if (AStr[i] <> AVal[i]) then exit;
  Result := true;
end;

//Decodes textual representation of a registry value type. Based on how it's used in manifests.
function DecodeRegistryValueType(AText: string): TRegistryValueType;
begin
  AText := AText.ToUpper;

  //Note: we can do a hash map here for faster search

  if (Length(AText) >= 4) and cregpref(AText) then begin
    if cregcmp(AText, 'REG_NONE') then Result := REG_NONE else
    if cregcmp(AText, 'REG_SZ') then Result := REG_SZ else
    if cregcmp(AText, 'REG_EXPAND_SZ') then Result := REG_EXPAND_SZ else
    if cregcmp(AText, 'REG_BINARY') then Result := REG_BINARY else
    if cregcmp(AText, 'REG_DWORD') then Result := REG_DWORD else
    if cregcmp(AText, 'REG_DWORD_LITTLE_ENDIAN') then Result := REG_DWORD_LITTLE_ENDIAN else
    if cregcmp(AText, 'REG_DWORD_BIG_ENDIAN') then Result := REG_DWORD_BIG_ENDIAN else
    if cregcmp(AText, 'REG_LINK') then Result := REG_LINK else
    if cregcmp(AText, 'REG_MULTI_SZ') then Result := REG_MULTI_SZ else
    if cregcmp(AText, 'REG_RESOURCE_LIST') then Result := REG_RESOURCE_LIST else
    if cregcmp(AText, 'REG_FULL_RESOURCE_DESCRIPTOR') then Result := REG_FULL_RESOURCE_DESCRIPTOR else
    if cregcmp(AText, 'REG_RESOURCE_REQUIREMENTS_LIST') then Result := REG_RESOURCE_REQUIREMENTS_LIST else
    if cregcmp(AText, 'REG_QWORD') then Result := REG_QWORD else
    if cregcmp(AText, 'REG_QWORD_LITTLE_ENDIAN') then Result := REG_QWORD_LITTLE_ENDIAN else
      Result := REG_NONE;
    exit; //can't parse REG_*
  end;

  if not TryStrToInt('0x'+AText, integer(Result)) then
    Result := REG_NONE;
end;

procedure TAssemblyRegistry.Initialize;
begin
  inherited;
  FKeyCache := TDictionary<string, TRegistryKeyId>.Create();
end;

destructor TAssemblyRegistry.Destroy;
begin
  FreeAndNil(FKeyCache);
  inherited;
end;

procedure TAssemblyRegistry.CreateTables;
begin
  Db.Exec('CREATE TABLE IF NOT EXISTS registryKeys ('
    +'id INTEGER PRIMARY KEY,'
    +'parentId INTEGER NOT NULL,'
    +'keyName TEXT NOT NULL COLLATE NOCASE,'
    +'CONSTRAINT identity UNIQUE(parentId,keyName)'
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS registryKeyReferences ('
    +'assemblyId INTEGER NOT NULL,'
    +'keyId INTEGER NOT NULL,'
    +'owner BOOLEAN'
    +',CONSTRAINT identity UNIQUE(assemblyId,keyId)'    //keeping for now, because AddKeyReference may be called multiple times for a key atm
    +')');

  Db.Exec('CREATE TABLE IF NOT EXISTS registryValues ('
    +'assemblyId INTEGER NOT NULL,'
    +'keyId INTEGER NOT NULL,'
    +'name TEXT NOT NULL COLLATE NOCASE,'
    +'valueType INTEGER NOT NULL,'
    +'value TEXT NOT NULL COLLATE NOCASE,'
    +'operationHint TEXT NOT NULL COLLATE NOCASE,'
    +'owner BOOLEAN'
    +',CONSTRAINT identity UNIQUE(assemblyId,keyId,name)'
    +')');
end;

procedure TAssemblyRegistry.InitStatements;
begin
  StmTouchKey := Db.PrepareStatement('INSERT OR IGNORE INTO registryKeys '
    +'(parentId,keyName) VALUES (?,?)');
  StmFindKey := Db.PrepareStatement('SELECT id FROM registryKeys WHERE '
    +'parentId=? AND keyName=?');
  StmAddKeyReference := Db.PrepareStatement('INSERT OR IGNORE INTO registryKeyReferences '
    +'(assemblyId,keyId,owner) VALUES (?,?,?)');
  StmAddValue := Db.PrepareStatement('INSERT OR IGNORE INTO registryValues '
    +'(assemblyId,keyId,name,valueType,value,operationHint,owner) '
    +'VALUES (?,?,?,?,?,?,?)');
  FKeyCache.Clear;
end;


function TAssemblyRegistry.AddKey(const AName: string; AParent: TRegistryKeyId = 0): TRegistryKeyId;
var ANameLo: string;
begin
  //Check in the local cache
  ANameLo := AName.ToLower;
  if FKeyCache.TryGetValue(ANameLo, Result) then
    exit;

  //Touch
  sqlite3_bind_int64(StmTouchKey, 1, AParent);
  sqlite3_bind_str(StmTouchKey, 2, AName);
  if sqlite3_step(StmTouchKey) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmTouchKey);

  //Find id
  sqlite3_bind_int64(StmFindKey, 1, AParent);
  sqlite3_bind_str(StmFindKey, 2, AName);
  if sqlite3_step(StmFindKey) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_int64(StmFindKey, 0);
  sqlite3_reset(StmFindKey);

  FKeyCache.Add(ANameLo, Result);
end;

//Overloaded version which parses Registry path and creates all neccessary key entries. Returns the leaf key id.
function TAssemblyRegistry.AddKeyPath(APath: string): TRegistryKeyId;
var idx: integer;
begin
  Result := 0;
  while APath <> '' do begin
    idx := pos('\', APath);
    if idx <= 0 then begin
      Result := AddKey(APath, Result);
      break;
    end;

    if idx = 1 then begin
      APath := copy(APath, 2, MaxInt);
      continue;
    end;

    Result := AddKey(copy(APath, 1, idx-1), Result);
    APath := copy(APath, idx+1, MaxInt);
  end;
end;

procedure TAssemblyRegistry.AddKeyReference(AAssembly: TAssemblyId; AKey: TRegistryKeyId; const AData: TRegistryKeyReferenceData);
begin
  sqlite3_bind_int64(StmAddKeyReference, 1, AAssembly);
  sqlite3_bind_int64(StmAddKeyReference, 2, AKey);
  sqlite3_bind_int(StmAddKeyReference, 3, integer(AData.owner));
  if sqlite3_step(StmAddKeyReference) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddKeyReference);
end;

//Overloaded version which parses Registry path, adds it and links the leaf key with the assembly.
function TAssemblyRegistry.AddKey(AAssembly: TAssemblyId; APath: string; const AData: TRegistryKeyReferenceData): TRegistryKeyId;
begin
  Result := AddKeyPath(APath);
  if Result <> 0 then
    AddKeyReference(AAssembly, Result, AData);
end;

procedure TAssemblyRegistry.AddValue(AAssembly: TAssemblyId; const AData: TRegistryValueData);
begin
  sqlite3_bind_int64(StmAddValue, 1, AAssembly);
  sqlite3_bind_int64(StmAddValue, 2, AData.key);
  sqlite3_bind_str(StmAddValue, 3, AData.name);
  sqlite3_bind_int(StmAddValue, 4, AData.valueType);
  sqlite3_bind_str(StmAddValue, 5, AData.value);
  sqlite3_bind_str(StmAddValue, 6, AData.operationHint);
  sqlite3_bind_int(StmAddValue, 7, integer(AData.owner));
  if sqlite3_step(StmAddValue) <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(StmAddValue);
end;

//Parses the results of SELECT * FROM registryKeys
procedure TAssemblyRegistry.QueryKeys(AStmt: PSQLite3Stmt; AList: TRegistryKeyList);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(sqlite3_column_int64(AStmt, 0), sqlite3_column_text16(AStmt, 1));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError();
  sqlite3_reset(AStmt);
end;

//Retrieves the list of children key names for a given parent key id (0 for root)
procedure TAssemblyRegistry.GetKeys(const AParent: TRegistryKeyId; AList: TRegistryKeyList);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT id, keyName FROM registryKeys WHERE parentId=?');
  sqlite3_bind_int64(stmt, 1, AParent);
  QueryKeys(stmt, AList);
end;

procedure TAssemblyRegistry.GetKeyReferees(AKey: TRegistryKeyId; AList: TRegistryKeyReferees);
var stmt: PSQLite3Stmt;
  res: integer;
  AData: TRegistryKeyReferenceData;
begin
  stmt := Db.PrepareStatement('SELECT assemblyId, owner FROM registryKeyReferences WHERE keyId=?');
  sqlite3_bind_int64(stmt, 1, AKey);
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

function TAssemblyRegistry.GetKeyName(AKey: TRegistryKeyId): string;
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT parentId,keyName FROM registryKeys WHERE id=?');
  sqlite3_bind_int64(stmt, 1, AKey);
  if sqlite3_step(stmt) <> SQLITE_ROW then
    Db.RaiseLastSQLiteError();
  Result := sqlite3_column_text16(stmt, 1);
  sqlite3_reset(stmt);
end;

function TAssemblyRegistry.GetKeyPath(AKey: TRegistryKeyId): string;
var stmt: PSQLite3Stmt;
begin
  Result := '';
  stmt := Db.PrepareStatement('SELECT parentId,keyName FROM registryKeys WHERE id=?');
  while AKey > 0 do begin
    sqlite3_bind_int64(stmt, 1, AKey);
    if sqlite3_step(stmt) <> SQLITE_ROW then
      Db.RaiseLastSQLiteError();
    AKey := sqlite3_column_int64(stmt, 0); //parent
    if Result = '' then
      Result := sqlite3_column_text16(stmt, 1)
    else
      Result := sqlite3_column_text16(stmt, 1) + '\' + Result;
    sqlite3_reset(stmt);
  end;
end;

//Locates registry key ID by it's path. If the key is not found, returns 0.
function TAssemblyRegistry.FindKeyByPath(const APath: string; ARoot: TRegistryKeyId): TRegistryKeyId;
var stmt: PSQLite3Stmt;
  pos_b, pos_e: integer;
  res: integer;
begin
  stmt := Db.PrepareStatement('SELECT id FROM registryKeys WHERE parentId=? AND keyName=?');
  pos_b := 1;
  Result := ARoot;
  while pos_b <= Length(APath) do begin
    pos_e := pos('\', APath, pos_b);
    if pos_e <= 0 then pos_e := Length(APath)+1;
    sqlite3_bind_int64(stmt, 1, Result);
    sqlite3_bind_str(stmt, 2, copy(APath, pos_b, pos_e-pos_b));
    res := sqlite3_step(stmt);
    if res = SQLITE_DONE then begin
      Result := 0;
      exit;
    end;
    if res <> SQLITE_ROW then
      Db.RaiseLastSQLiteError();
    Result := sqlite3_column_int64(stmt, 0);
    sqlite3_reset(stmt);
    pos_b := pos_e + 1;
  end;
end;

function TAssemblyRegistry.SqlReadValueData(stmt: PSQLite3Stmt): TRegistryValueData;
begin
  Result.id := sqlite3_column_int64(stmt, 0); //rowid
  Result.assembly := sqlite3_column_int64(stmt, 1);
  Result.key := sqlite3_column_int64(stmt, 2);
  Result.name := sqlite3_column_text16(stmt, 3);
  Result.valueType := sqlite3_column_int(stmt, 4);
  Result.value := sqlite3_column_text16(stmt, 5);
  Result.operationHint := sqlite3_column_text16(stmt, 6);
  Result.owner := boolean(sqlite3_column_int(stmt, 7));
end;


procedure TAssemblyRegistry.QueryValues(AStmt: PSQLite3Stmt; AList: TRegistryValueList);
var res: integer;
begin
  res := sqlite3_step(AStmt);
  while res = SQLITE_ROW do begin
    AList.Add(SqlReadValueData(AStmt));
    res := sqlite3_step(AStmt)
  end;
  if res <> SQLITE_DONE then
    Db.RaiseLastSQLiteError;
  sqlite3_reset(AStmt);
end;

function TAssemblyRegistry.QueryValue(AStmt: PSQLite3Stmt; out AData: TRegistryValueData): boolean;
var res: integer;
begin
  res := sqlite3_step(AStmt);
  Result := (res = SQLITE_ROW);
  if Result then
    AData := SqlReadValueData(AStmt)
  else
    if res <> SQLITE_DONE then
      Db.RaiseLastSqliteError;
  sqlite3_reset(AStmt);
end;

procedure TAssemblyRegistry.GetKeyValues(AKey: TRegistryKeyId; AList: TList<TRegistryValueData>);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT rowid, * FROM registryValues WHERE keyId=?');
  sqlite3_bind_int64(stmt, 1, AKey);
  QueryValues(stmt, AList);
end;

procedure TAssemblyRegistry.GetAssemblyValues(AAssembly: TAssemblyId; AList: TRegistryValueList);
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT rowid, * FROM registryValues WHERE assemblyId=?');
  sqlite3_bind_int64(stmt, 1, AAssembly);
  QueryValues(stmt, AList);
end;

function TAssemblyRegistry.GetValueById(AId: TRegistryValueId): TRegistryValueData;
var stmt: PSQLite3Stmt;
begin
  stmt := Db.PrepareStatement('SELECT rowid, * FROM registryValues WHERE rowid=?');
  sqlite3_bind_int64(stmt, 1, AId);
  if not QueryValue(stmt, Result) then
    raise Exception.Create('Item not found');
end;


end.
