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
uses SysUtils, Windows;

// All environment variables in SxS are in the form $(runtime.ENVNAME)

var
  envModelx64: TEnvironment;
  envModelx86: TEnvironment;
  envLocalx64: TEnvironment;
  envLocalx86: TEnvironment;

function ArcIsX64(const processorArchitecture: string): boolean;
begin
  Result := (processorArchitecture='') or (processorArchitecture='x64') or (processorArchitecture='amd64');
end;

function CreateModelEnvironmentBlock(const isX64: boolean): TEnvironment;
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

  if isx64 then
    Result.Values['system32'] := 'C:\Windows\System32'
  else
    Result.Values['system32'] := 'C:\Windows\SysWOW64';
  Result.Values['syswow64'] := 'C:\Windows\SysWOW64';
  Result.Values['system'] := Result.Values['system32'];

  Result.Values['drivers'] := Result.Values['system32']+'\drivers';
  Result.Values['wbem'] := Result.Values['system32']+'\wbem';

  if isx64 then
    Result.Values['programfiles'] := 'C:\Program Files'
  else
    Result.Values['programfiles'] := 'C:\Program Files (x86)';
  Result.Values['programfilesx86'] := 'C:\Program Files (x86)';
  Result.Values['commonfiles'] := Result.Values['programfilesx86'] + '\Common Files';

  Result.Values['programdata'] := 'C:\ProgramData';
  Result.Values['documentssettings'] := 'C:\Users';
  Result.Values['public'] := 'C:\Users\Public';
  Result.Values['userprofile'] := 'C:\Users\Default';
  Result.Values['startmenu'] := 'C:\Users\Default\Start Menu';
end;

function GetModelEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;
var isx64: boolean;
  ptr: PEnvironment;
begin
  isx64 := ArcIsX64(processorArchitecture);
  if isx64 then
    ptr := @envModelx64
  else
    ptr := @envModelx86;
  Result := ptr^;
  if Result = nil then begin
    Result := CreateModelEnvironmentBlock(isx64);
    if InterlockedCompareExchangePointer(pointer(ptr^), Result, nil) <> nil then begin
      FreeAndNil(Result);
      Result := ptr^;
    end;
  end;
end;

function CreateLocalEnvironmentBlock(const isX64: boolean): TEnvironment;
begin
  Result := TEnvironment.Create;
  //TODO:
end;

function GetLocalEnvironmentBlock(const processorArchitecture: string = ''): TEnvironment;
var isx64: boolean;
  ptr: PEnvironment;
begin
  isx64 := ArcIsX64(processorArchitecture);
  if isx64 then
    ptr := @envLocalx64
  else
    ptr := @envLocalx86;
  Result := ptr^;
  if Result = nil then begin
    Result := CreateLocalEnvironmentBlock(isx64);
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
