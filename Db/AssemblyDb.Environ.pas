unit AssemblyDb.Environ;
{
Folder paths and registry values often contain placeholders such as $(Runtime.System32).
Note that these can appear basically anywhere in text data, so strictly speaking, this is not just
scoped to Assembly.Files.
}

interface
uses Classes;

type
  TEnvironment = TStringList;
  PEnvironment = ^TEnvironment;

//Generates an environment block for a model x64 PC with default installation configuration
function GetModelEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;

//Generates an environment block for the local PC. Useful to locate local equivalents of paths
function GetLocalEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;


function GetEnvironmentValue(env: TEnvironment; const name: string): string;
function ExpandEnvironmentVariables(env: TEnvironment; const text: string): string;

implementation
uses SysUtils, Windows, ShlObj;

{
All environment variables in SxS are in the form $(runtime.ENVNAME)
}

var
  envModelx64: TEnvironment;
  envModelx86: TEnvironment;
  envLocalx64: TEnvironment;
  envLocalx86: TEnvironment;

function ArcIsX86(const processorArchitecture: string): boolean;
begin
  Result := (processorArchitecture='x86') or (processorArchitecture='wow64');
end;

// iswow64 = 32 bit assembly on 64 bit PC
// all other modes are non-wow64 and are equal
function CreateModelEnvironmentBlock(const iswow64: boolean): TEnvironment;
begin
  Result := TEnvironment.Create;

  Result.Values[''] := 'C:\';
  Result.Values['bootdrive'] := 'C:\';

  Result.Values['windows'] := 'C:\Windows';
  Result.Values['windir'] := 'C:\Windows';
  Result.Values['systemroot'] := 'C:\Windows';

  Result.Values['apppatch'] := 'C:\Windows\AppPatch';
  Result.Values['fonts'] := 'C:\Windows\Fonts';
  Result.Values['help'] := 'C:\Windows\Help';
  Result.Values['inf'] := 'C:\Windows\Inf';

  Result.Values['syswow64'] := 'C:\Windows\SysWOW64';
  if iswow64 then
    Result.Values['system32'] := 'C:\Windows\SysWOW64'
  else
    Result.Values['system32'] := 'C:\Windows\System32';
  Result.Values['system'] := Result.Values['system32'];

  Result.Values['drivers'] := Result.Values['system32']+'\drivers';
  Result.Values['wbem'] := Result.Values['system32']+'\wbem';

  if iswow64 then
    Result.Values['programfiles'] := 'C:\Program Files (x86)'
  else
    Result.Values['programfiles'] := 'C:\Program Files';
  Result.Values['programfilesx86'] := 'C:\Program Files (x86)';
  Result.Values['commonfiles'] := Result.Values['programfiles'] + '\Common Files';

  Result.Values['programdata'] := 'C:\ProgramData';
  Result.Values['documentssettings'] := 'C:\Users';
  Result.Values['public'] := 'C:\Users\Public';
  Result.Values['userprofile'] := 'C:\Users\Default';
  Result.Values['startmenu'] := 'C:\Users\Default\Start Menu';
end;

function GetModelEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;
var isx86: boolean;
  ptr: PEnvironment;
begin
  isx86 := ArcIsX86(processorArchitecture);
  if isx86 then
    ptr := @envModelx86
  else
    ptr := @envModelx64;
  Result := ptr^;
  if Result = nil then begin
    Result := CreateModelEnvironmentBlock(isx86);
    if InterlockedCompareExchangePointer(pointer(ptr^), Result, nil) <> nil then begin
      FreeAndNil(Result);
      Result := ptr^;
    end;
  end;
end;

function GetKnownFolder(nFolder: integer; DefaultUser: boolean = false): string;
var hToken: THandle;
  hr: HRESULT;
begin
  SetLength(Result, MAX_PATH);
  if DefaultUser then
    hToken := THandle(-1)
  else
    hToken := THandle(0);
  hr := SHGetFolderPath(0, nFolder, hToken, SHGFP_TYPE_CURRENT, PChar(Result));
  if FAILED(hr) then
    RaiseLastOsError();
  SetLength(Result, strlen(PChar(Result)));
end;

function CreateLocalEnvironmentBlock(const isx86: boolean): TEnvironment;
begin
  Result := TEnvironment.Create;

  Result.Values['windows'] := GetKnownFolder(CSIDL_WINDOWS);
  Result.Values['windir'] := Result.Values['windows'];
  Result.Values['systemroot'] := Result.Values['windows'];

  Result.Values[''] := ExtractFileDrive(Result.Values['windows']);
  Result.Values['bootdrive'] := Result.Values[''];

  Result.Values['apppatch'] := Result.Values['windows']+'\AppPatch';
  Result.Values['fonts'] := Result.Values['windows']+'\Fonts';
  Result.Values['help'] := Result.Values['windows']+'\Help';
  Result.Values['inf'] := Result.Values['windows']+'\Inf';

  Result.Values['syswow64'] := GetKnownFolder(CSIDL_SYSTEMX86);
  if isx86 then
    Result.Values['system'] := GetKnownFolder(CSIDL_SYSTEMX86)
  else
    Result.Values['system'] := GetKnownFolder(CSIDL_SYSTEM);
  Result.Values['system32'] := Result.Values['system'];

  Result.Values['drivers'] := Result.Values['system32']+'\drivers';
  Result.Values['wbem'] := Result.Values['system32']+'\wbem';

  Result.Values['programfilesx86'] := GetKnownFolder(CSIDL_PROGRAM_FILESX86);
  if isx86 then begin
    Result.Values['programfiles'] := GetKnownFolder(CSIDL_PROGRAM_FILESX86);
    Result.Values['commonfiles'] := GetKnownFolder(CSIDL_PROGRAM_FILES_COMMONX86)
  end else begin
    Result.Values['programfiles'] := GetKnownFolder(CSIDL_PROGRAM_FILES);
    Result.Values['commonfiles'] := GetKnownFolder(CSIDL_PROGRAM_FILES_COMMON);
  end;

  Result.Values['programdata'] := GetKnownFolder(CSIDL_COMMON_APPDATA);
  Result.Values['public'] := ExtractFileDir(GetKnownFolder(CSIDL_COMMON_DOCUMENTS)); //best guess
  Result.Values['documentssettings'] := ExtractFileDir(Result.Values['public']); //best guess
  Result.Values['userprofile'] := GetKnownFolder(CSIDL_PROFILE);
  Result.Values['startmenu'] := GetKnownFolder(CSIDL_STARTMENU);
end;

function GetLocalEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;
var isx86: boolean;
  ptr: PEnvironment;
begin
  isx86 := ArcIsX86(processorArchitecture);
  if isx86 then
    ptr := @envLocalx86
  else
    ptr := @envLocalx64;
  Result := ptr^;
  if Result = nil then begin
    Result := CreateLocalEnvironmentBlock(isx86);
    if InterlockedCompareExchangePointer(pointer(ptr^), Result, nil) <> nil then begin
      FreeAndNil(Result);
      Result := ptr^;
    end;
  end;
end;


function GetEnvironmentValue(env: TEnvironment; const name: string): string;
begin
  Result := env.Values[name.ToLower];
end;

function ExpandEnvironmentVariables(env: TEnvironment; const text: string): string;
var pos_s, pos_e: integer;
begin
  Result := '';
  pos_e := 0;
  repeat
    pos_s := pos('$(runtime.', text, pos_e+1);
    if pos_s <= 0 then break;

    Result := Result + copy(text, pos_e, pos_s-pos_e-1);

    pos_e := pos(')', text, pos_s+Length('$(runtime.'));
    if pos_e <= 0 then begin //unterminated, do not decode
      pos_e := pos_s; //the tail is text
      break;
    end;

    pos_s := pos_s + Length('$(runtime.');
    Result := Result + GetEnvironmentValue(env, copy(text, pos_s, pos_e-pos_s));
  until false;

  //Copy the rest
  Result := Result + copy(text, pos_e+1, MaxInt);
end;


initialization
  envModelx64 := nil;
  envModelx86 := nil;
  envLocalx64 := nil;
  envLocalx86 := nil;

finalization
 {$IFDEF DEBUG}
  FreeAndNil(envModelx64);
  FreeAndNil(envModelx86);
  FreeAndNil(envLocalx64);
  FreeAndNil(envLocalx86);
 {$ENDIF}

end.
