unit ManifestSaxParser;

//{$DEFINE NOLOAD}
//Do not even read the manifest from the disk

//{$DEFINE NOPARSE}
//Read the manifest but do not parse it

//{$DEFINE NOPROCESS}
//Parse the XML but do not handle the parsed data (tests the raw parsing)

//{$DEFINE NOCOMMIT}
//Do not commit parsed data to the database

//{$DEFINE UNUSUALPROPS}
//Record unusual encountered properties in a special table (slower but helps figuring out manifest
//peculiarities)

interface
uses SysUtils, Classes, sqlite3, SxsExpand, AssemblyDb, AssemblyDb.Assemblies, AssemblyDb.Files,
  AssemblyDb.Registry, AssemblyDb.Services, ComObj, ActiveX, Generics.Collections, MSXML;

type
  EParsingException = class(Exception);

  //Really quick list for our uses
  TQuickList<T> = class
  type PT = ^T;
  protected
    FData: array of T;
    FUsed: integer;
    function GetPItem(const AIndex: integer): PT;
  public
    procedure Clear; inline;
    function Add: PT; inline;
    function Last: PT; inline;
    property Count: integer read FUsed;
    property PItem[const Index: integer]: PT read GetPItem; default;
  end;

  TParserRoutine = procedure(const ANode: string; const AAttributes: ISAXAttributes) of object;
  TCharRoutine = procedure(const AData: string) of object;
  //ANode is always lowercase

  TStackFrame = record
    parser: TParserRoutine;
    char: TCharRoutine;
  end;

  TSaxManifestParser = class(TInterfacedObject, ISAXContentHandler, ISAXErrorHandler)
  protected
    FDb: TAssemblyDb;
    FReader: ISAXXMLReader;
  public
    constructor Create(ADb: TAssemblyDb);
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure ImportManifest(const AManifestFile: string);
    property Db: TAssemblyDb read FDb;

  protected // IVBSAXErrorHandler
    FLastError: record //set on error
      Message: string;
      Code: integer;
    end;
    function error(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
    function fatalError(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
    function ignorableWarning(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
    procedure raiseParsingException(hr: HRESULT);


  protected // IVBSAXContentHandler
    function putDocumentLocator(const pLocator: ISAXLocator): HResult; stdcall;
    function startDocument: HResult; stdcall;
    function endDocument: HResult; stdcall;
    function startPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT; var pwchUri: Word;
                                cchUri: SYSINT): HResult; stdcall;
    function endPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT): HResult; stdcall;
    function startElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                          var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                          cchQName: SYSINT; const pAttributes: ISAXAttributes): HResult; stdcall;
    function endElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                        var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                        cchQName: SYSINT): HResult; stdcall;
    function characters(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function ignorableWhitespace(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function processingInstruction(var pwchTarget: Word; cchTarget: SYSINT; var pwchData: Word;
                                   cchData: SYSINT): HResult; stdcall;
    function skippedEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;

 {
  Prefixes:
    dispNODE    = handles all child nodes for node NODE
    handleNODE  = handles instance of node NODE (parsing and storing the data)
    readNODE    = parses instance of node NODE and returns the data
 }
  protected // Parser stack
   //Each node level adds one entry to the stack. If it's not nil, it'll be called for the children items.
    FParsingStack: TStack<TStackFrame>;
    FParserRoutine: TParserRoutine;
    FCharRoutine: TCharRoutine;
    procedure pushNode(const ARoutine: TParserRoutine; const AChar: TCharRoutine = nil);
    procedure popNode;

    procedure dispRoot(const ANode: string; const AAttributes: ISAXAttributes);

    procedure dispAssembly(const ANode: string; const AAttributes: ISAXAttributes);
    function readAssemblyIdentity(const AAttributes: ISAXAttributes): TAssemblyIdentity;

    procedure handleFile(const AAttributes: ISAXAttributes);

    procedure dispDirectories(const ANode: string; const AAttributes: ISAXAttributes);

    procedure dispRegistryKeys(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispRegistryKey(const ANode: string; const AAttributes: ISAXAttributes);

    procedure handleDependency(const AAttributes: ISAXAttributes);
    procedure dispDependency(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispDependentAssembly(const ANode: string; const AAttributes: ISAXAttributes);

    procedure dispMemberships(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispCategoryMembership(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispCategoryInstance(const ANode: string; const AAttributes: ISAXAttributes);

    procedure dispTaskScheduler(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispTask(const ANode: string; const AAttributes: ISAXAttributes);
    procedure dispTaskRegistrationInfo(const ANode: string; const AAttributes: ISAXAttributes);
    procedure charTaskUri(const AData: string);


  protected // Parsing state
  type
    TDependencyList = TQuickList<record
      identity: TAssemblyIdentity;
      entry: TDependencyEntryData;
    end>;
    TFileList = TQuickList<record
      path: string;
      entry: TFileEntryData;
    end>;
    TDirectoryList = TQuickList<record
      folder: string;
      ref: TFolderReferenceData;
    end>;
    TRegistryKeyList = TQuickList<record
      path: string;
      ref: TRegistryKeyReferenceData;
      id: TRegistryKeyId;
    end>;
    TRegistryValueList = TQuickList<record
      keyIndex: integer;
      data: TRegistryValueData;
    end>;
    TMembershipList = TQuickList<TCategoryMembershipData>;
    TTaskList = TQuickList<string>;
    TServiceList = TQuickList<TServiceEntryData>;
    TServiceGroupEntryList = TQuickList<record
      groupName: string;
      serviceName: string;
      position: string;
    end>;

  protected
    FIdentity: TAssemblyIdentity;
    FManifestName: string;
    FIsDeployment: boolean;
    FDependencies: TDependencyList;
    FFiles: TFileList;
    FDirectories: TDirectoryList;
    FRegistryKeys: TRegistryKeyList;
    FRegistryValues: TRegistryValueList;
    FMemberships: TMembershipList;
    FTasks: TTaskList;
    FServices: TServiceList;
    FServiceGroupEntries: TServiceGroupEntryList;
    FLastCategoryInstance: record
      Subcategory: string;
    end;
    procedure resetState;
    procedure commitState;

  end;

implementation
uses Windows, UniStrUtils;

function TQuickList<T>.GetPItem(const AIndex: integer): PT;
begin
  Result := @FData[AIndex];
end;

procedure TQuickList<T>.Clear;
begin
  FUsed := 0;
end;

function TQuickList<T>.Add: PT;
begin
  if FUsed >= Length(FData) then
    SetLength(FData, Length(FData) * 2  + 10);
  Result := @FData[FUsed];
  Inc(FUsed);
end;

function TQuickList<T>.Last: PT;
begin
  Result := @FData[FUsed-1];
end;


constructor TSaxManifestParser.Create(ADb: TAssemblyDb);
begin
  Inc(FRefCount); //so that nothing destroys us
  inherited Create();

  FParsingStack := TStack<TStackFrame>.Create;

  FDependencies := TDependencyList.Create;
  FFiles := TFileList.Create;
  FDirectories := TDirectoryList.Create;
  FRegistryKeys := TRegistryKeyList.Create;
  FRegistryValues := TRegistryValueList.Create;
  FMemberships := TMembershipList.Create;
  FTasks := TTaskList.Create;
  FServices := TServiceList.Create;
  FServiceGroupEntries := TServiceGroupEntryList.Create;

  FDb := ADb;
  FReader := CoSAXXMLReader.Create as ISaxXMLReader;
 {$IFNDEF NOPROCESS}
  FReader.putContentHandler(Self);
  FReader.putErrorHandler(Self);
 {$ENDIF}
end;

procedure TSaxManifestParser.BeforeDestruction;
begin
 //Inherited implementation checks for 0 but we keep 1 of our own
 //+ any number from yet undestroyed FReader
end;

destructor TSaxManifestParser.Destroy;
begin
  FReader := nil;
  FreeAndNil(FDependencies);
  FreeAndNil(FFiles);
  FreeAndNil(FDirectories);
  FreeAndNil(FRegistryKeys);
  FreeAndNil(FRegistryValues);
  FreeAndNil(FMemberships);
  FreeAndNil(FTasks);
  FreeAndNil(FServices);
  FreeAndNil(FServiceGroupEntries);
  FreeAndNil(FParsingStack);
  inherited;
end;

procedure TSaxManifestParser.ImportManifest(const AManifestFile: string);
var xmlStr: string;
  hr: HRESULT;
begin
 {$IFDEF NOLOAD}exit;{$ENDIF}
  xmlStr := LoadManifestFile(AManifestFile);

 {$IFNDEF NOPROCESS}
  resetState;
  FManifestName := ChangeFileExt(ExtractFilename(AManifestFile), '');
 {$ENDIF}

  FParsingStack.Clear;
  FParserRoutine := dispRoot;
  FCharRoutine := nil;

 {$IFNDEF NOPARSE}
  hr := FReader.parse(xmlStr);
  if FAILED(hr) then
    raiseParsingException(hr);

 {$IFNDEF NOCOMMIT}
  commitState;
 {$ENDIF}
 {$ENDIF}
end;

procedure TSaxManifestParser.raiseParsingException(hr: HRESULT);
var msg: string;
begin
  msg := 'Cannot parse manifest: error 0x'+IntToHex(hr, 8);
  if FLastError.Message <> '' then
    msg := msg + #13 + FLastError.Message;
  if FLastError.Code <> 0 then
    msg := msg + #13 + 'Code: '+IntToStr(FLastError.Code);
  raise EParsingException.Create(msg);
end;



// Service functions

//Copies at most ALen characters from WideChar
function wcstr(const AWChar: PWideChar; const ALen: integer): string; overload; inline;
begin
  if ALen <= 0 then
    Result := ''
  else begin
    SetLength(Result, ALen);
    StrLCopy(@Result[1], AWChar, ALen);
  end;
end;

function wcstr(const AWChar: pointer; const ALen: integer): string; overload; inline;
begin
  Result := wcstr(PWideChar(AWChar), ALen);
end;

function attr(const AAttributes: ISAXAttributes; const AAttrName: string): string;
var AValue: PWord1;
  AValueLen: integer;
  i: integer;
  len: integer;
begin
  AAttributes.getLength(len);
  for i := 0 to len-1 do begin
    AAttributes.getLocalName(i, AValue, AValueLen);
    if (AValueLen = Length(AAttrName))
    and (StrLComp(PWideChar(AAttrName), PWideChar(AValue), AValueLen) = 0) then begin
      AAttributes.getValue(i, AValue, AValueLen);
      Result := wcstr(AValue, AValueLen);
      exit;
    end;
  end;
  Result := '';
end;

function boolattr(const AAttributes: ISAXAttributes; const AAttrName: string): boolean;
var AValue: string;
begin
  AValue := attr(AAttributes, AAttrName);
  if not TryStrToBool(AValue, Result) then
    Result := false;
end;


// IVBSAXErrorHandler

function TSaxManifestParser.error(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
begin
  FLastError.Message := PWideChar(pwchErrorMessage);
  FLastError.Code := hrErrorCode;
  Result := S_OK;
end;

function TSaxManifestParser.fatalError(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
begin
  FLastError.Message := PWideChar(pwchErrorMessage);
  FLastError.Code := hrErrorCode;
  Result := S_OK;
end;

function TSaxManifestParser.ignorableWarning(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
begin
  //No one cares
  Result := S_OK;
end;


// IVBSAXContentHandler

function TSaxManifestParser.putDocumentLocator(const pLocator: ISAXLocator): HResult; stdcall;
begin
  //Do not need it
  Result := S_OK;
end;

function TSaxManifestParser.startDocument: HResult; stdcall;
begin
  //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.endDocument: HResult; stdcall;
begin
  //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.startPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT; var pwchUri: Word; cchUri: SYSINT): HResult; stdcall;
begin
 //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.endPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT): HResult; stdcall;
begin
 //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.startElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
  var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word; cchQName: SYSINT;
  const pAttributes: ISAXAttributes): HResult; stdcall;
var lowercaseName: string;
begin
  if @FParserRoutine <> nil then begin
    lowercaseName := wcstr(@pwchLocalName, cchLocalName).ToLower;
    FParserRoutine(lowercaseName, pAttributes)
  end else
    pushNode(nil);
  Result := S_OK;
end;

function TSaxManifestParser.endElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
  var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word; cchQName: SYSINT): HResult; stdcall;
begin
  popNode;
  Result := S_OK;
end;

function TSaxManifestParser.characters(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
begin
  if @FCharRoutine <> nil then
    FCharRoutine(wcstr(@pwchChars, cchChars));
  Result := S_OK;
end;

function TSaxManifestParser.ignorableWhitespace(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
begin
  //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.processingInstruction(var pwchTarget: Word; cchTarget: SYSINT; var pwchData: Word;
  cchData: SYSINT): HResult; stdcall;
begin
  //Whatever
  Result := S_OK;
end;

function TSaxManifestParser.skippedEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;
begin
  //Whatever
  Result := S_OK;
end;


// Parser stack

procedure TSaxManifestParser.pushNode(const ARoutine: TParserRoutine; const AChar: TCharRoutine = nil);
var frame: TStackFrame;
begin
  frame.parser := FParserRoutine;
  frame.char := FCharRoutine;
  FParsingStack.Push(frame);
  FParserRoutine := ARoutine;
  FCharRoutine := AChar;
end;

procedure TSaxManifestParser.popNode;
var frame: TStackFrame;
begin
  frame := FParsingStack.Pop();
  FParserRoutine := frame.parser;
  FCharRoutine := frame.char;
end;


// Dispatchers

procedure TSaxManifestParser.dispRoot(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'assembly') then
    pushNode(dispAssembly)
  else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispAssembly(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'assemblyidentity') then begin
    FIdentity := readAssemblyIdentity(AAttributes);
    pushNode(nil);
  end else
  if SameStr(ANode, 'deployment') then begin
    FIsDeployment := true;
    pushNode(nil);
  end else
  if SameStr(ANode, 'dependency') then begin
    handleDependency(AAttributes);
  end else
  if SameStr(ANode, 'file') then begin
    handleFile(AAttributes);
  end else
  if SameStr(ANode, 'directories') then begin
    pushNode(dispDirectories);
  end else
  if SameStr(ANode, 'registrykeys') then begin
    pushNode(dispRegistryKeys);
  end else
  if SameStr(ANode, 'memberships') then begin
    pushNode(dispMemberships);
  end else
  if SameStr(ANode, 'taskscheduler') then begin
    pushNode(dispTaskScheduler);
  end else begin
   {$IFDEF UNUSUALPROPX}
    Db.UnusualProps.Add(aId, PROP_NODENAME, ANode);
   {$ENDIF}
    pushNode(nil);
  end;
end;


function TSaxManifestParser.readAssemblyIdentity(const AAttributes: ISAXAttributes): TAssemblyIdentity;
begin
  Result.name := attr(AAttributes, 'name');
  Result.type_ := attr(AAttributes, 'type');
  Result.language := attr(AAttributes, 'language');
  Result.buildType := attr(AAttributes, 'buildType');
  Result.processorArchitecture := attr(AAttributes, 'processorArchitecture');
  Result.version := attr(AAttributes, 'version');
  Result.publicKeyToken := attr(AAttributes, 'publicKeyToken');
  Result.versionScope := attr(AAttributes, 'versionScope');
end;


procedure TSaxManifestParser.handleDependency(const AAttributes: ISAXAttributes);
begin
  with FDependencies.Add^ do begin
    entry.discoverable := boolAttr(AAttributes, 'discoverable');
    entry.resourceType := attr(AAttributes, 'resourceType');
    entry.dependencyType := '';
    identity.Clear;
  end;
  pushNode(dispDependency);
end;

procedure TSaxManifestParser.dispDependency(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'dependentassembly') then begin
    FDependencies.Last^.entry.dependencyType := attr(AAttributes, 'dependencyType');
    pushNode(dispDependentAssembly);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispDependentAssembly(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'assemblyidentity') then begin
    FDependencies.last^.identity := ReadAssemblyIdentity(AAttributes);
    pushNode(nil);
  end else
    pushNode(nil);
end;


procedure TSaxManifestParser.handleFile(const AAttributes: ISAXAttributes);
begin
  with FFiles.Add^ do begin
    path := attr(AAttributes, 'destinationPath');
    entry.name := attr(AAttributes, 'name');
    entry.sourceName := attr(AAttributes, 'sourceName');
    entry.sourcePath := attr(AAttributes, 'sourcePath');
    entry.importPath := attr(AAttributes, 'importPath');
  end;
  pushNode(nil);
end;


procedure TSaxManifestParser.dispDirectories(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'directory') then
  with FDirectories.Add^ do begin
    folder := attr(AAttributes, 'destinationPath');
    ref.owner := boolattr(AAttributes, 'owner');
  end;
  pushNode(nil);
end;


procedure TSaxManifestParser.dispRegistryKeys(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'registrykey') then begin
    with FRegistryKeys.Add^ do begin
      path := attr(AAttributes, 'keyName');
      ref.owner := boolattr(AAttributes, 'owner');
    end;
    pushNode(dispRegistryKey);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispRegistryKey(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'registryvalue') then
  with FRegistryValues.Add^ do begin
    keyIndex := FRegistryKeys.Count-1;
    data.name := attr(AAttributes, 'name');
    data.valueType := DecodeRegistryValueType(attr(AAttributes, 'valueType'));
    data.value := attr(AAttributes, 'value');
    data.operationHint := attr(AAttributes, 'operationHint');
    data.owner := boolattr(AAttributes, 'owner');
  end;
  pushNode(nil);
end;

procedure TSaxManifestParser.dispMemberships(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'categorymembership') then begin
    pushNode(dispCategoryMembership);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispCategoryMembership(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'id') then begin
    with FMemberships.Add^ do begin
      name := attr(AAttributes, 'name');
      version := attr(AAttributes, 'version');
      publicKeyToken := attr(AAttributes, 'publicKeyToken');
      typeName := attr(AAttributes, 'typeName');
    end;
    pushNode(nil);
  end else
  if SameStr(ANode, 'categoryinstance') then begin
    FLastCategoryInstance.Subcategory := attr(AAttributes, 'subcategory'); //used by some children functions
    pushNode(dispCategoryInstance);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispCategoryInstance(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'servicedata') then
    with FServices.Add^ do begin
      name := attr(AAttributes, 'name');
      displayName := attr(AAttributes, 'displayName');
      errorControl := attr(AAttributes, 'errorControl');
      imagePath := attr(AAttributes, 'imagePath');
      start := attr(AAttributes, 'start');
      type_ := attr(AAttributes, 'type');
      description := attr(AAttributes, 'description');
      objectName := attr(AAttributes, 'objectName');
      sidType := attr(AAttributes, 'sidType');
      requiredPrivileges := attr(AAttributes, 'requiredPrivileges');
    end
  else
  if SameStr(ANode, 'servicegroup') then
    with FServiceGroupEntries.Add^ do begin
      groupName := FLastCategoryInstance.Subcategory;
      serviceName := attr(AAttributes, 'serviceName');
      position := attr(AAttributes, 'position');
    end;

  pushNode(nil);
end;

procedure TSaxManifestParser.dispTaskScheduler(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'task') then begin
    pushNode(dispTask);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispTask(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameStr(ANode, 'registrationinfo') then begin
    pushNode(dispTaskRegistrationInfo);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.dispTaskRegistrationInfo(const ANode: string; const AAttributes: ISAXAttributes);
begin
  if SameText(ANode, 'uri') then begin
    FTasks.Add^ := '';
    pushNode(nil, charTaskUri);
  end else
    pushNode(nil);
end;

procedure TSaxManifestParser.charTaskUri(const AData: string);
begin
  FTasks.Last^ := FTasks.Last^ + AData;
end;


// Parsing state

procedure TSaxManifestParser.resetState;
begin
  FLastError.Code := 0;
  FLastError.Message := '';
  FIdentity.Clear;
  FIsDeployment := false;
  FDependencies.Clear;
  FFiles.Clear;
  FDirectories.Clear;
  FRegistryKeys.Clear;
  FRegistryValues.Clear;
  FMemberships.Clear;
  FTasks.Clear;
  FServices.Clear;
  FServiceGroupEntries.Clear;
end;

//Commits the collected state to the database
procedure TSaxManifestParser.commitState;
var AId: TAssemblyId;
  i, j: integer;
begin
  AId := Db.Assemblies.AddAssembly(FIdentity, FManifestName, FIsDeployment, TAssemblyState.asInstalled);

  for i := 0 to FDependencies.Count-1 do begin
    FDependencies[i]^.entry.dependentAssembly := Db.Assemblies.NeedAssembly(FDependencies[i]^.identity);
    Db.AddDependency(AId, FDependencies[i]^.entry);
  end;

  for i := 0 to FFiles.Count-1 do begin
    FFiles[i]^.entry.assembly := AId;
    FFiles[i]^.entry.folder := Db.Files.AddFolderPath(FFiles[i]^.path);
    Db.Files.AddFile(FFiles[i]^.entry);
  end;

  for i := 0 to FDirectories.Count-1 do
    Db.Files.AddFolder(AId, FDirectories[i].folder, FDirectories[i].ref);

  for i := 0 to FRegistryKeys.Count-1 do
    FRegistryKeys[i].id := Db.Registry.AddKey(AId, FRegistryKeys[i].path, FRegistryKeys[i].ref);

  for i := 0 to FRegistryValues.Count-1 do begin
    j := FRegistryValues[i].keyIndex;
    FRegistryValues[i].data.assembly := AId;
    FRegistryValues[i].data.key := FRegistryKeys[j].id;
    Db.Registry.AddValue(AId, FRegistryValues[i].data);
  end;

  for i := 0 to FMemberships.Count-1 do
    Db.AddCategoryMembership(AId, FMemberships[i]^);

  for i := 0 to FTasks.Count-1 do
    Db.AddTask(AId, FTasks[i]^);

  for i := 0 to FServices.Count-1 do begin
    FServices[i].assemblyId := AId;
    Db.Services.AddService(FServices[i]^);
  end;

  for i := 0 to FServiceGroupEntries.Count-1 do
    Db.Services.AddServiceGroupEntry(
      FServiceGroupEntries[i].groupName,
      FServiceGroupEntries[i].serviceName,
      FServiceGroupEntries[i].position
    );

end;

end.
