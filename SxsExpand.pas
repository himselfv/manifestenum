unit SxsExpand;

interface
uses Classes;

function OpenManifestFile(const AFilename: string): TStream; overload;
function LoadManifestFile(const AFilename: string): string; overload;

procedure DecodeDCM1(AData: TMemoryStream);


implementation
uses SysUtils, Windows, FilenameUtils, MSDeltaLib;

type
  TPackageSignature = integer;

const
  SIG_DCM1: TPackageSignature = $014D4344; //DCM$01
  SIG_DCN1: TPackageSignature = $014E4344; //DCN$01
  SIG_DCD1: TPackageSignature = $01444344; //DCD$01
  SIG_DCS1: TPackageSignature = $01534344; //DCS$01

function OpenManifestFile(const AFilename: string): TStream;
var AFileData: TMemoryStream;
  ASignature: TPackageSignature;
begin
 //System files are moderate, often tiny, and we need to load thousands of them fast.
 //Peeking a header wastes kernel calls. Caching layer would increase complexity and not
 //neccessarily work well for tiny files.
 //Realistically, the best we can do is to read the whole file in one go.
  AFileData := TMemoryStream.Create;
  AFileData.LoadFromFile(AFilename);

  if AFileData.Read(ASignature, SizeOf(ASignature)) = SizeOf(ASignature) then begin
    if ASignature = SIG_DCM1 then begin
      DecodeDCM1(AFileData);
    end;
  end;
  AFileData.Seek(0, soFromBeginning);
  Result := AFileData;
end;

function LoadManifestFile(const AFilename: string): string;
var AData: TStream;
 AReader: TStreamReader;
begin
  AReader := nil;
  AData := OpenManifestFile(AFilename);
  try
    AReader := TStreamReader.Create(AData);
    Result := AReader.ReadToEnd;
  finally
    FreeAndNil(AReader);
    FreeAndNil(AData);
  end;
end;


var
  _DCM1Base: PDELTA_INPUT = nil;

procedure FreeDCM1Base(tmp: PDELTA_INPUT);
begin
  if tmp <> nil then begin
    if tmp.lpStart <> nil then
      FreeMem(tmp.lpStart);
    FreeMem(tmp);
  end;
end;

procedure LoadDCM1Base;
var hFile: THandle;
  tmp: PDELTA_INPUT;
  bytesRead: cardinal;
begin
  hFile := CreateFile(PChar(AppFolder+'\w10-base.manifest'), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if hFile = INVALID_HANDLE_VALUE then RaiseLastOsError();

  GetMem(tmp, sizeof(DELTA_INPUT));
  tmp^.Editable := false;
  tmp^.uSize := GetFileSize(hFile, nil); //to hell with 2Gb+ files
  GetMem(tmp^.lpStart, tmp^.uSize);

  try
    if not ReadFile(hFile, tmp^.lpStart^, tmp^.uSize, bytesRead, nil) then
      RaiseLastOsError();
  except
    FreeDCM1Base(tmp);
    raise;
  end;

  tmp^.uSize := bytesRead;
  if InterlockedCompareExchangePointer(pointer(_DCM1Base), tmp, nil) <> nil then
    FreeDCM1Base(tmp);
end;

function DCM1Base: PDELTA_INPUT; inline;
begin
  if _DCM1Base = nil then LoadDCM1Base;
  Result := _DCM1Base;
end;

procedure DecodeDCM1(AData: TMemoryStream);
var in1: DELTA_INPUT;
  outp: DELTA_OUTPUT;
begin
  in1.lpStart := PByte(AData.Memory) + 4; //skip header
  in1.uSize := AData.Size - 4;
  in1.Editable := false;
  outp.lpStart := nil;
  outp.uSize := 0;
  if not MSDelta.ApplyDeltaB(0, DCM1Base^, in1, outp) then
    RaiseLastOsError();
  AData.Seek(0, soFromBeginning);
  AData.Write(outp.lpStart^, outp.uSize);
  AData.SetSize(outp.uSize);
  MSDelta.DeltaFree(outp.lpStart);
end;


initialization
finalization
 {$IFDEF DEBUG}
  FreeDCM1Base(_DCM1Base);
 {$ENDIF}
end.
