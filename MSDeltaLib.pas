unit MSDeltaLib;

interface
uses Windows;

const
  MSDELTADLL = 'MSDelta.dll';

type
  DELTA_FLAG_TYPE = int64;

const
  DELTA_FLAG_NONE = 0;
  DELTA_APPLY_FLAG_ALLOW_PA19 = 1;

type
  DELTA_INPUT = packed record
    lpStart: PByte;
    uSize: size_t;
    Editable: BOOL;
  end;
  PDELTA_INPUT = ^DELTA_INPUT;

  DELTA_OUTPUT = packed record
    lpStart: PByte;
    uSize: size_t;
  end;
  PDELTA_OUTPUT = ^DELTA_OUTPUT;

  TApplyDeltaA = function(ApplyFlags: DELTA_FLAG_TYPE; lpSourceName: PAnsiChar; lpDeltaName: PAnsiChar; lpTargetName: PAnsiChar): BOOL; stdcall;
  TApplyDeltaW = function(ApplyFlags: DELTA_FLAG_TYPE; lpSourceName: PWideChar; lpDeltaName: PWideChar; lpTargetName: PWideChar): BOOL; stdcall;
  TApplyDeltaB = function(ApplyFlags: DELTA_FLAG_TYPE; Source: DELTA_INPUT; Delta: DELTA_INPUT; var lpTarget: DELTA_OUTPUT): BOOL; stdcall;
  TDeltaFree = function(lpMemory: pointer): BOOL; stdcall;

  TMSDeltaDLL = class
  protected
    FLib: HMODULE;
  public
    ApplyDeltaA: TApplyDeltaA;
    ApplyDeltaW: TApplyDeltaW;
    ApplyDeltaB: TApplyDeltaB;
    DeltaFree: TDeltaFree;
    constructor Create;
    destructor Destroy; override;
  end;

function MSDelta: TMSDeltaDLL;

implementation
uses SysUtils;

// We only load MSDelta.dll if it's needed,
// to not be dependent on it on older versions of Windows.

var
  _MSDelta: TMSDeltaDLL = nil;

procedure LoadMSDelta;
var tmp: TMSDeltaDLL;
begin
  tmp := TMSDeltaDLL.Create;
  if InterlockedCompareExchangePointer(pointer(_MSDelta), tmp, nil) <> nil then
    FreeAndNil(tmp); //someone already created this
end;

function MSDelta: TMSDeltaDLL;
begin
  if _MSDelta = nil then LoadMSDelta;
  Result := _MSDelta;
end;

constructor TMSDeltaDLL.Create;
begin
  inherited;
  FLib := LoadLibrary(MSDELTADLL);
  if FLib = 0 then RaiseLastOsError();

  ApplyDeltaA := GetProcAddress(FLib, 'ApplyDeltaA');
  ApplyDeltaW := GetProcAddress(FLib, 'ApplyDeltaW');
  ApplyDeltaB := GetProcAddress(FLib, 'ApplyDeltaB');
  DeltaFree := GetProcAddress(FLib, 'DeltaFree');
end;

destructor TMSDeltaDLL.Destroy;
begin
  ApplyDeltaA := nil;
  ApplyDeltaW := nil;
  ApplyDeltaB := nil;
  DeltaFree := nil;

  if FLib <> 0 then
    FreeLibrary(FLib);
  FLib := 0;
  inherited;
end;


initialization
finalization
 {$IFDEF DEBUG}
  FreeAndNil(_MSDelta);
 {$ENDIF}
end.
