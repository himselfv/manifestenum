unit SxsUtils;

interface
uses SysUtils, AssemblyDb.Assemblies;

type
  ESxsException = class(Exception);

{
Keyform management

SxS sometimes uses keyforms to refer to assemblies, deployments and components.
A keyform is a string built from several parts of identity, shortened if they are too long, e.g.:
  amd64_microsoft-windows-c..yment_10.10530.0_neutral_a21b34f6
A hash of identity is added at the end.

Different places use somewhat different keyforms (shortened length is different), and the format is
known to have changed at least once between versions.

https://blogs.msdn.microsoft.com/jonwis/2005/12/28/whats-that-awful-directory-name-under-windowswinsxs/
}

function SxsSanitize(const AText: string): string;
function SxsTruncate(const AName: string; ALen: integer): string;
function SxsValueOrNone(const AValue: string): string; inline;
function SxsExtractHash(const AKeyform: string): string;
function SxsDeploymentKeyform(const id: TAssemblyIdentity; const hash: string): string;
function SxsComponentKeyform(const id: TAssemblyIdentity; const hash: string): string;


const
  sSxsComponentsHive = 'COMPONENTS';
  sSxsDeploymentsKey = 'COMPONENTS\CanonicalData\Deployments';
  sSxsComponentsKey = 'COMPONENTS\DerivedData\Components';

function IsComponentsHiveLoaded: boolean;
procedure LoadComponentsHive;
function UnloadComponentsHive: integer;

function SxsIsDeployment(const id: TAssemblyIdentity; const AComponentKeyform: string): boolean;
procedure SxsDeploymentAddUninstallSource(const id: TAssemblyIdentity; const AComponentKeyform: string);
procedure SxsConvertIntoDeployment(const id: TAssemblyIdentity; const AComponentKeyform: string);

implementation
uses Windows, AclHelpers, OsUtils;

{
Sanitizes the text in exactly the way SxS does it (see the link in the interface comments).
At this point only one algorithm is known, if there are different ones we'll have to add a param.
}
function SxsSanitize(const AText: string): string;
var i, len: integer;
  ch: char;
begin
 //"Characters not in the group “A-Za-z0-9.\-_” are removed"
  SetLength(Result, Length(AText));
  len := 0;
  for i := 1 to Length(AText) do begin
    ch := AText[i];
    if ((Ord(ch) >= Ord('A')) and (Ord(ch) <= Ord('Z')))
    or ((Ord(ch) >= Ord('a')) and (Ord(ch) <= Ord('z')))
    or ((Ord(ch) >= Ord('0')) and (Ord(ch) <= Ord('9')))
    or (ch='.') or (ch='\') or (ch='-') or (ch='_') then begin
      Result[1+len] := ch;
      Inc(len);
    end;
  end;
  SetLength(Result, len); //truncate
end;

{
Truncates the string the way SxS does it:
  Some-Long-Component-Name -> Some-L..nt-Name
Result will not exceed ALen (SxS uses different lengths for different cases)
}
function SxsTruncate(const AName: string; ALen: integer): string;
begin
  if Length(AName) < ALen then begin
    Result := AName;
    exit;
  end;

  // SxS cuts exactly in the middle, leaving two equal parts by sides.
  // 2 additional chars needed for ..
  Result := Copy(AName, 1, ALen div 2 - 1) + '..' + Copy(AName, Length(AName) - ALen div 2 + 2, MaxInt);
end;

//A helper function which returns AValue or "none" if AValue was empty
function SxsValueOrNone(const AValue: string): string;
begin
  if (AValue <> '') and (AValue <> 'neutral') then
    Result := AValue
  else
    Result := 'none';
end;


{
Extracts a 8 byte hash part from the manifest name (or other keyform).
The hash algorithm is not documented and we don't know it and it may be different between versions
of Windows so at the moment we've decided to just extract it.
}
function SxsExtractHash(const AKeyform: string): string;
begin
  if Length(AKeyform) < 9 then begin
    Result := '';
    exit;
  end;

  if AKeyform[Length(AKeyform)-16] <> '_' then
    raise Exception.CreateFmt('The string "%s" does not seem to be a keyform ending in hash', [AKeyform]);

  Result := Copy(AKeyform, Length(AKeyform)-15, 16);
end;

{
Deployment keyforms have the following format on Windows 10 AU:
  name<24>_publicKey_version_hash
Hash has to be given (extract it from the manifest name)
}
function SxsDeploymentKeyform(const id: TAssemblyIdentity; const hash: string): string;
begin
  Result := SxsTruncate(SxsSanitize(id.name), 24)+'_'
    +id.publicKeyToken+'_'
    +id.version+'_'
    +hash;
  Result := Result.ToLowerInvariant;
end;

{
Component keyforms have the following format on Windows 7 .. Windows 10 AU:
  processorArchitecture<8>_name<64>_publicKey_version_culture<12>_hash
If the culture or processorArchitecture is missing, it's replaced with "none".

This is what a manifestName uses, so if you have a manifestName, just use that.
}
function SxsComponentKeyform(const id: TAssemblyIdentity; const hash: string): string;
begin
 //We'll hope processorArchitecture and language don't need SxsSanitize and SxsTruncate,
 //because in practice they never do.
  Result := SxsValueOrNone(id.processorArchitecture)+'_'
    +SxsTruncate(SxsSanitize(id.name), 64)+'_'
    +id.publicKeyToken+'_'
    +id.version+'_'
    +SxsValueOrNone(id.language)+'_'
    +hash;
  Result := Result.ToLowerInvariant;
end;


// COMPONENTS hive

//True if the COMPONENTS hive is loaded
function IsComponentsHiveLoaded: boolean;
var hk: HKEY;
begin
  Result := RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar('COMPONENTS'), 0, GENERIC_READ, hk) = 0;
  if Result then
    RegCloseKey(hk);
end;

//Loads COMPONENTS hive or verifies that it's already loaded. Exception on any problems.
procedure LoadComponentsHive;
var hProcToken: THandle;
  err: integer;
begin
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, hProcToken)
  or not SetPrivilege(hProcToken, SE_RESTORE_NAME, true)
  or not SetPrivilege(hProcToken, SE_BACKUP_NAME, true) then
    RaiseLastOsError();
  try
    err := RegLoadKey(HKEY_LOCAL_MACHINE, PChar('COMPONENTS'), PChar(GetWindowsDir+'\System32\Config\COMPONENTS'));
    if err <> 0 then begin
     //Maybe it's already loaded in which case we don't care
      if IsComponentsHiveLoaded then
        exit;
      RaiseLastOsError(err);
    end;
  finally
   //Do not clear the privileges, we haven't checked if they were enabled before. Whatever.
    CloseHandle(hProcToken);
  end;
end;

{
Unloads COMPONENTS hive. 0 on success, exception if no rights,
error code if enough rights but the key is not loaded / cannot be unloaded.
Sometimes someone else has loaded the key and it cannot be unloaded by us. You may decide to
ignore error code if you care enough only to try.
}
function UnloadComponentsHive: integer;
var hProcToken: THandle;
begin
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, hProcToken)
  or not SetPrivilege(hProcToken, SE_RESTORE_NAME, true)
  or not SetPrivilege(hProcToken, SE_BACKUP_NAME, true) then
    RaiseLastOsError();
  try
    Result := RegUnloadKey(HKEY_LOCAL_MACHINE, PChar('COMPONENTS'));
  finally
   //Do not clear the privileges, we haven't checked if they were enabled before. Whatever.
    CloseHandle(hProcToken);
  end;
end;

{
DerivedData\Components subkeys contain the following values:
  Key             OS        Description
  identity        7-10AU    assembly strong name in the .net style
  S256H           7-10AU    apparently, SHA256 of manifest?
  f!filename      7-10AU    for every included file. Value=1 apparently means owned by this assembly
  c!depkf         7-10AU    deployment keyform for the parent deployment assembly (or own one, if deployment)


CanonicalData\Deployments subkeys contain the following values
  appid           7-10AU    assembly strong name in the .net style
  CatalogThumbprint 7-10AU

  i!pref_data     7-10AU    "installed as a part of".
    The following prefixes are known:
      i!S_appname             string identity
      i!CBS_pkgname           package name from CBS
      i!SIAW_                 can be uninstalled by passing nil for a source
    The contents is 8 opaque bytes (usually 0c [7 times x]00) + same text as appid

  p!              7-10AU    ?
  s!              7-10AU    ?

  CF               -10AU    ? 00, 0c or other values
}


//True if the specified assembly is registered in the registry as a deployment
//This is different from what it's manifest says. This can be overriden (see SxsConvertIntoDeployment)
function SxsIsDeployment(const id: TAssemblyIdentity; const AComponentKeyform: string): boolean;
var hk: HKEY;
  dkf: string;
begin
  dkf := SxsDeploymentKeyform(id, SxsExtractHash(AComponentKeyform));

  Result := RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(sSxsDeploymentsKey+'\'+dkf), 0, GENERIC_READ, hk) = 0;
  if Result then
    RegCloseKey(hk);
end;

{
SxS deployments have "install sources" and you can only uninstall a deployment by passing a source
which installed it.
This adds a "generic" install source which allows to uninstall a deployment by passing nil.
}
procedure SxsDeploymentAddUninstallSource(const id: TAssemblyIdentity; const AComponentKeyform: string);
var hk: HKEY;
  dkf: string;
  err: integer;
  val: AnsiString;
begin
{
  Install sources are tracked in CanonicalData\Deployments\ subkeys as i! keys.
    i!S_srcname  means installed by source "srcname"
    i!SIAW_ is a generic source
  The binary value is unclear but it usually starts with 0C + [7 times] 00, then assembly strong
  name (as in appid).
}
  dkf := SxsDeploymentKeyform(id, SxsExtractHash(AComponentKeyform));

  err := RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(sSxsDeploymentsKey+'\'+dkf), 0, GENERIC_READ or GENERIC_WRITE, hk);
  if err <> 0 then
    raise ESxsException.CreateFmt('Deployment configuration key is unaccessible. '
      +'Are you running the app with the administrator privileges? '
      +'Maybe the assembly is not a deployment?'#13
      +'Error %d: %s',
      [err, SysErrorMessage(err)]);
  try
    val := '12345678'+AnsiString(id.ToStrongNameNETStyle); //hey... I hope it's Ansi
    val[1] := Chr($0C);
    val[2] := Chr($00);
    val[3] := Chr($00);
    val[4] := Chr($00);
    val[5] := Chr($00);
    val[6] := Chr($00);
    val[7] := Chr($00);
    val[8] := Chr($00);
    err := RegSetValueEx(hk, PChar('i!SIAW_'), 0, REG_BINARY, @val[1], length(val));
    if err <> 0 then
      raise ESxsException.CreateFmt('Cannot add install source to the deployment configuration key. '
        +'Error %d: %s', [err, SysErrorMessage(err)]);
  finally
    RegCloseKey(hk);
  end;
end;

function EnumRegistryValueNames(hk: HKEY): TArray<string>;
var i: integer;
  valName: string;
  valNameLen: cardinal;
  err: integer;
begin
  SetLength(Result, 0);

  valNameLen := 255; //since it's a c!+keyform, it should fit
  SetLength(valName, valNameLen+1);

  i := 0;
  repeat
    valNameLen := Length(valName)-1;
    err := RegEnumValue(hk, i, @valName[1], valNameLen, nil, nil, nil, nil);
    if err = ERROR_NO_MORE_ITEMS then
      break;
    if err <> 0 then
      raise ESxsException.CreateFmt('Cannot enumerate registry values. '
        +'Error %d: %s', [err, SysErrorMessage(err)]);
    SetLength(Result, i+1);
    Result[i] := Copy(valName, 1, valNameLen);
    Inc(i);
  until false;
end;

{
Normal assemblies are linked to deployment assemblies and cannot be uninstalled.
This takes a normal assembly and converts it into deployment assembly as far as registry is concerned:
- creates a Deployments\ subkey for it
- sets the assembly as its own deployment parent in its Components\ subkey
This does not:
- add default uninstall source (use SxsDeploymentAddUninstallSource on your assembly)
}
procedure SxsConvertIntoDeployment(const id: TAssemblyIdentity; const AComponentKeyform: string);
var hk: HKEY;
  dkf: string;
  err: integer;
  val: AnsiString;
  valName: string;
begin
  dkf := SxsDeploymentKeyform(id, SxsExtractHash(AComponentKeyform));

  //Create a deployment configuration key
  err := RegCreateKeyEx(HKEY_LOCAL_MACHINE, PChar(sSxsDeploymentsKey+'\'+dkf), 0, nil, 0,
    KEY_ALL_ACCESS, nil, hk, nil); //Creates or opens a key
  if err <> 0 then
    raise ESxsException.CreateFmt('Cannot create a deployment configuration key. '
      +'Are you running the app with the administrator privileges? '
      +'Error %d: %s',
      [err, SysErrorMessage(err)]);
  try
    val := AnsiString(id.ToStrongNameNETStyle);
    err := RegSetValueEx(hk, PChar('appid'), 0, REG_BINARY, @val[1], length(val));
    if err <> 0 then
      raise ESxsException.CreateFmt('Cannot initialize a deployment configuration key. '
        +'Error %d: %s', [err, SysErrorMessage(err)]);

   //That's it. A i!* key is also needed for uninstall, call SxsDeploymentAddUninstallSource for that
  finally
    RegCloseKey(hk);
  end;

  //Reparent a component configuration key to this deployment
  err := RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(sSxsComponentsKey+'\'+AComponentKeyform), 0,
    GENERIC_READ or GENERIC_WRITE, hk);
  if err <> 0 then
    raise ESxsException.CreateFmt('Cannot open a component configuration key. '
      +'Are you running the app with the administrator privileges? '
      +'Error %d: %s',
      [err, SysErrorMessage(err)]);
  try
    //Delete all c!-s
    for valName in EnumRegistryValueNames(hk) do
      if valName.StartsWith('c!') then begin
        err := RegDeleteValue(hk, PChar(@valName[1]));
        if err <> 0 then
          RaiseLastOsError();
      end;

    //Add our own c!
    err := RegSetValueEx(hk, PChar('c!'+dkf), 0, REG_BINARY, nil, 0);
    if err <> 0 then
      raise ESxsException.CreateFmt('Cannot add a self-parent record. '
        +'Are you running the app with the administrator privileges? '
        +'Error %d: %s',
        [err, SysErrorMessage(err)]);

  finally
    RegCloseKey(hk);
  end;
end;

end.
