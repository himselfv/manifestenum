unit WinSxS;

interface
uses Windows, ActiveX;

const
  SXSDLL = 'sxs.dll';

const
  QUERYASMINFO_FLAG_VALIDATE = $00000001;
  QUERYASMINFO_FLAG_GETSIZE  = $00000002;

type
  PULARGE_INTEGER = ^ULARGE_INTEGER;

  FUSION_INSTALL_REFERENCE = packed record
    cbSize: DWORD;
    dwFlags: DWORD;
    guidScheme: TGUID;
    szIdentifier: LPCWSTR;
    szNonCannonicalData: LPCWSTR;
  end;
  LPFUSION_INSTALL_REFERENCE = ^FUSION_INSTALL_REFERENCE;

  ASSEMBLY_INFO = packed record
    cbAssemblyInfo: ULONG;
    dwAssemblyFlags: DWORD;
    uliAssemblySizeInKB: ULARGE_INTEGER;
    pszCurrentAssemblyPathBuf: LPWSTR;
    cchBuf: ULONG;
  end;
  PASSEMBLY_INFO = ^ASSEMBLY_INFO;

  IAssemblyCacheItem = interface['{9e3aaeb4-d1cd-11d2-bab9-00c04f8eceae}']
    function CreateStream(
        flags: DWORD;
        name: LPCWSTR;
        format: DWORD;
        format_flags: DWORD;
        out stream: IStream;
        max_size: PULARGE_INTEGER): HRESULT; stdcall;

    function Commit(
        flags: DWORD;
        disp: PULONG): HRESULT; stdcall;

    function AbortItem(): HRESULT; stdcall;
  end;

  IAssemblyCache = interface['{e707dcde-d1cd-11d2-bab9-00c04f8eceae}']
    function UninstallAssembly(
        flags: DWORD;
        name: LPCWSTR;
        ref: LPFUSION_INSTALL_REFERENCE;
        disp: PULONG): HRESULT; stdcall;

    function QueryAssemblyInfo(
        flags: DWORD;
        name: LPCWSTR;
        info: PASSEMBLY_INFO): HRESULT; stdcall;

    function CreateAssemblyCacheItem(
        flags: DWORD;
        reserved: pointer;
        out item: IAssemblyCacheItem;
        name: LPCWSTR): HRESULT; stdcall;

    function Reserved(
        out reserved: IUnknown): HRESULT; stdcall;

    function InstallAssembly(
        flags: DWORD;
        path: LPCWSTR;
        ref: LPFUSION_INSTALL_REFERENCE): HRESULT; stdcall;
  end;

function CreateAssemblyCache(out ppAsmCache: IAssemblyCache; dwReserved: DWORD): HRESULT; stdcall; external SXSDLL;

implementation

end.
