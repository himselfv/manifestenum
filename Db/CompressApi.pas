unit CompressApi;
{
Conversion of CompressApi.h / Cabinet.dll, available since Windows 8 and
Windows Server 2012
}

interface
uses Windows;

const
  CABINETDLL = 'cabinet.dll';

type
  COMPRESSOR_HANDLE = type pointer;
  PCOMPRESSOR_HANDLE = ^COMPRESSOR_HANDLE;
  DECOMPRESSOR_HANDLE = type pointer;
  PDECOMPRESSOR_HANDLE = ^DECOMPRESSOR_HANDLE;

const
  COMPRESS_ALGORITHM_INVALID      = 0;
  COMPRESS_ALGORITHM_NULL         = 1;
  COMPRESS_ALGORITHM_MSZIP        = 2;
  COMPRESS_ALGORITHM_XPRESS       = 3;
  COMPRESS_ALGORITHM_XPRESS_HUFF  = 4;
  COMPRESS_ALGORITHM_LZMS         = 5;
  COMPRESS_ALGORITHM_MAX          = 6;

  COMPRESS_RAW                    = 1 shl 29;

type
  PFN_COMPRESS_ALLOCATE = function(UserContext: pointer; Size: NativeUInt): pointer; cdecl;
  PFN_COMPRESS_FREE = procedure(UserContext: pointer; Memory: pointer); cdecl;

  COMPRESS_ALLOCATION_ROUTINES = packed record
    Allocate: PFN_COMPRESS_ALLOCATE;
    Free: PFN_COMPRESS_FREE;
    UserContext: pointer;
  end;
  PCOMPRESS_ALLOCATION_ROUTINES = ^COMPRESS_ALLOCATION_ROUTINES;

type
  COMPRESS_INFORMATION_CLASS = (
    COMPRESS_INFORMATION_CLASS_INVALID = 0,
    COMPRESS_INFORMATION_CLASS_BLOCK_SIZE,
    COMPRESS_INFORMATION_CLASS_LEVEL
  );


// Compression routines
type
  TCreateCompressor = function(
    Algorithm: DWORD;
    AllocationRoutines: PCOMPRESS_ALLOCATION_ROUTINES;
    CompressorHandle: PCOMPRESSOR_HANDLE
  ): BOOL; stdcall;

  TSetCompressorInformation = function(
    CompressorHandle: COMPRESSOR_HANDLE;
    CompressInformationClass: COMPRESS_INFORMATION_CLASS;
    CompressInformation: pointer;
    CompressInformationSize: NativeUInt
  ): BOOL; stdcall;

  TQueryCompressorInformation = function(
    CompressorHandle: COMPRESSOR_HANDLE;
    CompressInformationClass: COMPRESS_INFORMATION_CLASS;
    CompressInformation: pointer;
    CompressInformationSize: SIZE_T
  ): BOOL; stdcall;

  TCompress = function(
    CompressorHandle: COMPRESSOR_HANDLE;
    UncompressedData: pointer;
    UncompressedDataSize: NativeUInt;
    CompressedBuffer: pointer;
    CompressedBufferSize: NativeUInt;
    out CompressedDataSize: NativeUInt
  ): BOOL; stdcall;

  TResetCompressor = function(
    CompressorHandle: COMPRESSOR_HANDLE
  ): BOOL; stdcall;

  TCloseCompressor = function(
    CompressorHandle: COMPRESSOR_HANDLE
  ): BOOL; stdcall;


// Decompression routines
type
  TCreateDecompressor = function(
    Algorithm: DWORD;
    AllocationRoutines: PCOMPRESS_ALLOCATION_ROUTINES;
    DecompressorHandle: PDECOMPRESSOR_HANDLE
  ): BOOL; stdcall;

  TSetDecompressorInformation = function(
    DecompressorHandle: DECOMPRESSOR_HANDLE;
    CompressInformationClass: COMPRESS_INFORMATION_CLASS;
    CompressInformation: pointer;
    CompressInformationSize: NativeUInt
  ): BOOL; stdcall;

  TQueryDecompressorInformation = function(
    DecompressorHandle: DECOMPRESSOR_HANDLE;
    CompressInformationClass: COMPRESS_INFORMATION_CLASS;
    CompressInformation: pointer;
    CompressInformationSize: NativeUInt
  ): BOOL; stdcall;

  TDecompress = function(
    DecompressorHandle: DECOMPRESSOR_HANDLE;
    CompressedData: pointer;
    CompressedDataSize: NativeUInt;
    UncompressedBuffer: pointer;
    UncompressedBufferSize: NativeUInt;
    out UncompressedDataSize: NativeUInt
  ): BOOL; stdcall;

  TResetDecompressor = function(
    DecompressorHandle: DECOMPRESSOR_HANDLE
  ): BOOL; stdcall;

  TCloseDecompressor = function(
    DecompressorHandle: DECOMPRESSOR_HANDLE
  ): BOOL; stdcall;

var
  CreateCompressor: TCreateCompressor = nil;
  SetCompressorInformation: TSetCompressorInformation = nil;
  QueryCompressorInformation: TQueryCompressorInformation = nil;
  Compress: TCompress = nil;
  ResetCompressor: TResetCompressor = nil;
  CloseCompressor: TCloseCompressor = nil;
  CreateDecompressor: TCreateDecompressor = nil;
  SetDecompressorInformation: TSetDecompressorInformation = nil;
  QueryDecompressorInformation: TQueryDecompressorInformation = nil;
  Decompress: TDecompress = nil;
  ResetDecompressor: TResetDecompressor = nil;
  CloseDecompressor: TCloseDecompressor = nil;

function LoadCompressApi: boolean;

//Simple default allocation routines
function CompressSimpleAllocate(UserContext: pointer; Size: NativeUInt): pointer; cdecl;
procedure CompressSimpleFree(UserContext: pointer; Memory: pointer); cdecl;

var
  DefaultAllocationRoutines: COMPRESS_ALLOCATION_ROUTINES;


implementation

var
  CabinetLib: THandle = 0;

function LoadCompressApi: boolean;
var ALib: THandle;
begin
  //We only try to load it if it's 0. INVALID_HANDLE_VALUE means we've tried and failed
  if CabinetLib = 0 then begin
    ALib := LoadLibrary(CABINETDLL);
    if ALib <> 0 then begin
      CreateCompressor := GetProcAddress(ALib, 'CreateCompressor');
      SetCompressorInformation := GetProcAddress(ALib, 'SetCompressorInformation');
      QueryCompressorInformation := GetProcAddress(ALib, 'QueryCompressorInformation');
      Compress := GetProcAddress(ALib, 'Compress');
      ResetCompressor := GetProcAddress(ALib, 'ResetCompressor');
      CloseCompressor := GetProcAddress(ALib, 'CloseCompressor');
      CreateDecompressor := GetProcAddress(ALib, 'CreateDecompressor');
      SetDecompressorInformation := GetProcAddress(ALib, 'SetDecompressorInformation');
      QueryDecompressorInformation := GetProcAddress(ALib, 'QueryDecompressorInformation');
      Decompress := GetProcAddress(ALib, 'Decompress');
      ResetDecompressor := GetProcAddress(ALib, 'ResetDecompressor');
      CloseDecompressor := GetProcAddress(ALib, 'CloseDecompressor');
    end else
      ALib := INVALID_HANDLE_VALUE;
    if InterlockedCompareExchangePointer(pointer(CabinetLib), pointer(ALib), nil) <> nil then
      if ALib <> INVALID_HANDLE_VALUE then
        FreeLibrary(ALib);
  end;
  Result := CabinetLib <> INVALID_HANDLE_VALUE;
end;


function CompressSimpleAllocate(UserContext: pointer; Size: NativeUInt): pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure CompressSimpleFree(UserContext: pointer; Memory: pointer); cdecl;
begin
  FreeMem(Memory);
end;

initialization
  DefaultAllocationRoutines.Allocate := @CompressSimpleAllocate;
  DefaultAllocationRoutines.Free := @CompressSimpleFree;
  DefaultAllocationRoutines.UserContext := nil;

end.
