unit ManifestParser;
//Parses assembly manifests and imports their contents into Assembly DB

//{$DEFINE XML_OMNI}
//Использовать OmniXML вместо MSXML

//{$DEFINE DRYRUN}
//Do not add anything to DB, just parse the Xml

//{$DEFINE SUPERDRY}
//Do not even parse the xml, just read it

//{$DEFINE UNUSUALPROPS}
//Record unusual encountered properties in a special table (slower but helps figuring out manifest
//peculiarities)


interface
uses SysUtils, Classes, sqlite3, SxsExpand, AssemblyDb, AssemblyDb.Assemblies, AssemblyDb.Registry,
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
  TManifestParser = class
  protected
    FDb: TAssemblyDb;
    FXml: IXMLDocument;
    procedure InitXmlParser;
    procedure FreeXmlParser;
    function XmlReadAssemblyIdentityData(const ANode: IXmlNode): TAssemblyIdentity;
    function XmlReadDependencyData(const ANode: IXmlNode): TDependencyEntryData;
    procedure ImportDirectories(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportDirectoryNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportFileNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportRegistryKeys(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportRegistryKeyNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportMemberships(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    procedure ImportTaskScheduler(const AAssembly: TAssemblyId; const ANode: IXmlNode);
    function XmlReadRegistryValueData(const AKeyId: TRegistryKeyId; const ANode: IXmlNode): TRegistryValueData;
    function XmlReadCategoryMembership(const ANode: IXmlNode): TCategoryMembershipData;
  public
    constructor Create(ADb: TAssemblyDb);
    destructor Destroy; override;
    procedure ImportManifest(const AManifestFile: string);
    property Db: TAssemblyDb read FDb;
  end;

implementation

constructor TManifestParser.Create(ADb: TAssemblyDb);
begin
  inherited Create;
  FDb := ADb;
  InitXmlParser;
end;

destructor TManifestParser.Destroy;
begin
  FreeXmlParser;
  inherited;
end;

procedure TManifestParser.InitXmlParser;
begin
  // Nothing, we'll create it once we need it
end;

procedure TManifestParser.FreeXmlParser;
begin
  FXml := nil;
end;

procedure TManifestParser.ImportManifest(const AManifestFile: string);
var root, node: IXmlNode;
  children: IXMLNodeList;
  aId: TAssemblyId;
  nodeName: string;
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
 {$IFDEF DRYRUN}{$IFDEF SUPERDRY}exit;{$ENDIF}{$ENDIF}
  FXml.loadXML(xmlStr);

  node := FXml.selectSingleNode('/assembly/assemblyIdentity');
  Assert(node <> nil);
 {$IFNDEF DRYRUN}aId := Db.Assemblies.AddAssembly({$ENDIF}XmlReadAssemblyIdentityData(node) {$IFNDEF DRYRUN}, ChangeFileExt(ExtractFilename(AManifestFile), '')){$ENDIF};


  root := FXml.selectSingleNode('/assembly');
  children := root.childNodes;
  for i := 0 to children.length-1 do begin
    node := children.item[i];
    nodeName := node.nodeName;
    if nodeName = 'dependency' then
     {$IFNDEF DRYRUN}Db.AddDependency(aId,{$ENDIF} XmlReadDependencyData(node){$IFNDEF DRYRUN}){$ENDIF}
    else
    if nodeName = 'file' then
      ImportFileNode(aId, node)
    else
    if nodeName = 'directories' then
      ImportDirectories(aId, node)
    else
    if nodeName = 'registryKeys' then
      ImportRegistryKeys(aId, node)
    else
    if nodeName = 'memberships' then
      ImportMemberships(aId, node)
    else
    if nodeName = 'taskScheduler' then
      ImportTaskScheduler(aId, node)
    else begin
     {$IFDEF UNUSUALPROPX}
      Db.UnusualProps.Add(aId, PROP_NODENAME, nodeName);
     {$ENDIF}
    end;
  end;
end;

function textAttribute(const ANode: IXmlNode; const AAttribName: string): string; inline;
var AAttrib: IXmlNode;
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
function TManifestParser.XmlReadAssemblyIdentityData(const ANode: IXmlNode): TAssemblyIdentity;
begin
  Result.name := textAttribute(ANode, 'name');
  Result.type_ := textAttribute(ANode, 'type');
  Result.language := textAttribute(ANode, 'language');
  Result.buildType := textAttribute(ANode, 'buildType');
  Result.processorArchitecture := textAttribute(ANode, 'processorArchitecture');
  Result.version := textAttribute(ANode, 'version');
  Result.publicKeyToken := textAttribute(ANode, 'publicKeyToken');
  Result.versionScope := textAttribute(ANode, 'versionScope');
end;

function TManifestParser.XmlReadDependencyData(const ANode: IXmlNode): TDependencyEntryData;
var depAss: IXmlNode;
begin
  Result.discoverable := boolAttribute(ANode, 'discoverable');
  Result.resourceType := textAttribute(ANode, 'resourceType');
  depAss := ANode.selectSingleNode('dependentAssembly/assemblyIdentity');
  if depAss = nil then begin
    Result.dependentAssembly := 0;
    Result.dependencyType := '';
  end else begin
    {$IFNDEF DRYRUN}Result.dependentAssembly := Db.Assemblies.NeedAssembly({$ENDIF}XmlReadAssemblyIdentityData(depAss){$IFNDEF DRYRUN}){$ENDIF};
    Result.dependencyType := textAttribute(ANode.selectSingleNode('dependentAssembly'), 'dependencyType');
  end;
end;

procedure TManifestParser.ImportDirectories(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var i: integer;
begin
  for i := 0 to ANode.childNodes.length-1 do
    if ANode.childNodes.Item[i].nodeName = 'directory' then
      ImportDirectoryNode(AAssembly, ANode.childNodes.Item[i]);
end;

procedure TManifestParser.ImportDirectoryNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var AData: TFolderReferenceData;
begin
  AData.owner := boolAttribute(ANode, 'owner');
{$IFNDEF DRYRUN}  Db.AddFolder(AAssembly,
    textAttribute(ANode, 'destinationPath'),
    AData);{$ENDIF}
end;

procedure TManifestParser.ImportFileNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var AData: TFileEntryData;
  ADestinationPath: string;
begin
  ADestinationPath := textAttribute(ANode, 'destinationPath');
  AData.assembly := AAssembly;
{$IFNDEF DRYRUN}  AData.folder := Db.AddFolderPath(ADestinationPath);{$ENDIF}
  AData.name := textAttribute(ANode, 'name');
  AData.sourceName := textAttribute(ANode, 'sourceName');
  AData.sourcePath := textAttribute(ANode, 'sourcePath');
  AData.importPath := textAttribute(ANode, 'importPath');
{$IFNDEF DRYRUN}  Db.AddFile(AData);{$ENDIF}
end;

procedure TManifestParser.ImportRegistryKeys(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var i: integer;
begin
  for i := 0 to ANode.childNodes.length-1 do
    if ANode.childNodes.Item[i].nodeName = 'registryKey' then
      ImportRegistryKeyNode(AAssembly, ANode.childNodes.Item[i]);
end;

procedure TManifestParser.ImportRegistryKeyNode(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var AKeyName: string;
  AKeyData: TRegistryKeyReferenceData;
  AKeyId: TRegistryKeyId;
  nodes: IXmlNodeList;
  i: integer;
begin
  AKeyName := textAttribute(ANode, 'keyName');
  AKeyData.owner := boolAttribute(ANode, 'owner');
{$IFNDEF DRYRUN}  AKeyId := Db.Registry.AddKey(AAssembly, AKeyName, AKeyData);{$ENDIF}

  nodes := ANode.selectNodes('registryValue');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      {$IFNDEF DRYRUN}Db.Registry.AddValue(AAssembly, {$ENDIF}XmlReadRegistryValueData(AKeyId, nodes.item[i]){$IFNDEF DRYRUN}){$ENDIF};
  end;
end;

function TManifestParser.XmlReadRegistryValueData(const AKeyId: TRegistryKeyId; const ANode: IXmlNode): TRegistryValueData;
begin
  Result.key := AKeyId;
  Result.name := textAttribute(ANode, 'name');
  Result.valueType := DecodeRegistryValueType(textAttribute(ANode, 'valueType'));
  Result.value := textAttribute(ANode, 'value');
  Result.operationHint := textAttribute(ANode, 'operationHint');
  Result.owner := boolAttribute(ANode, 'owner');
end;

procedure TManifestParser.ImportMemberships(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var nodes: IXmlNodeList;
  i: integer;
begin
  nodes := ANode.selectNodes('categoryMembership/id');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
     {$IFNDEF DRYRUN} Db.AddCategoryMembership(AAssembly,{$ENDIF} XmlReadCategoryMembership(nodes.item[i]){$IFNDEF DRYRUN}){$ENDIF};
  end;
end;

procedure TManifestParser.ImportTaskScheduler(const AAssembly: TAssemblyId; const ANode: IXmlNode);
var nodes: IXmlNodeList;
  i: integer;
begin
  nodes := ANode.selectNodes('Task/RegistrationInfo/URI');
  if nodes <> nil then begin
    for i := 0 to nodes.length-1 do
      {$IFNDEF DRYRUN}Db.AddTask(AAssembly,{$ENDIF} nodes.item[i].text{$IFNDEF DRYRUN}){$ENDIF};
  end;
end;

function TManifestParser.XmlReadCategoryMembership(const ANode: IXmlNode): TCategoryMembershipData;
begin
  Result.name := textAttribute(ANode, 'name');
  Result.version := textAttribute(ANode, 'version');
  Result.publicKeyToken := textAttribute(ANode, 'publicKeyToken');
  Result.typeName := textAttribute(ANode, 'typeName');
end;

end.
