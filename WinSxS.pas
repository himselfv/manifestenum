unit WinSxS;

interface
uses Windows, ActiveX;

const
  SXSDLL = 'sxs.dll';

const
  QUERYASMINFO_FLAG_VALIDATE = $00000001;
  QUERYASMINFO_FLAG_GETSIZE  = $00000002;

  FUSION_REFCOUNT_UNINSTALL_SUBKEY_GUID: TGUID = '{8cedc215-ac4b-488b-93c0-a50a49cb2fb8}';
  FUSION_REFCOUNT_FILEPATH_GUID: TGUID = '{b02f9d65-fb77-4f7a-afa5-b391309f11c9}';
  FUSION_REFCOUNT_OPAQUE_STRING_GUID: TGUID = '{2ec93463-b0c3-45e1-8364-327e96aea856}';
  FUSION_REFCOUNT_MSI_GUID: TGUID = '{25df0fc1-7f97-4070-add7-4b13bbfd7cb8}';
  FUSION_REFCOUNT_OSINSTALL_GUID: TGUID = '{d16d444c-56d8-11d5-882d-0080c847b195}';

  IASSEMBLYCACHE_INSTALL_FLAG_REFRESH = $00000001;
  IASSEMBLYCACHE_INSTALL_FLAG_FORCE_REFRESH = $00000002;

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


  ASM_NAME = (
    ASM_NAME_PUBLIC_KEY,
    ASM_NAME_PUBLIC_KEY_TOKEN,
    ASM_NAME_HASH_VALUE,
    ASM_NAME_NAME,
    ASM_NAME_MAJOR_VERSION,
    ASM_NAME_MINOR_VERSION,
    ASM_NAME_BUILD_NUMBER,
    ASM_NAME_REVISION_NUMBER,
    ASM_NAME_CULTURE,
    ASM_NAME_PROCESSOR_ID_ARRAY,
    ASM_NAME_OSINFO_ARRAY,
    ASM_NAME_HASH_ALGID,
    ASM_NAME_ALIAS,
    ASM_NAME_CODEBASE_URL,
    ASM_NAME_CODEBASE_LASTMOD,
    ASM_NAME_NULL_PUBLIC_KEY,
    ASM_NAME_NULL_PUBLIC_KEY_TOKEN,
    ASM_NAME_CUSTOM,
    ASM_NAME_NULL_CUSTOM,
    ASM_NAME_MVID,
    ASM_NAME_MAX_PARAMS
  );

  ASM_DISPLAY_FLAGS = (
    ASM_DISPLAYF_VERSION               = $1,
    ASM_DISPLAYF_CULTURE               = $2,
    ASM_DISPLAYF_PUBLIC_KEY_TOKEN      = $4,
    ASM_DISPLAYF_PUBLIC_KEY            = $8,
    ASM_DISPLAYF_CUSTOM                = $10,
    ASM_DISPLAYF_PROCESSORARCHITECTURE = $20,
    ASM_DISPLAYF_LANGUAGEID            = $40
  );

  IAssemblyName = interface['{cd193bc0-b4bc-11D2-9833-00c04fc31d2e}']
    function SetProperty(
        id: DWORD;
        prop: LPVOID;
        size: DWORD): HRESULT; stdcall;

    function GetProperty(
        id: DWORD;
        buffer: LPVOID;
        buflen: LPDWORD): HRESULT; stdcall;

    function Finalize(): HRESULT; stdcall;

    function GetDisplayName(
        buffer: LPWSTR;
        buflen: LPDWORD;
        flags: DWORD): HRESULT; stdcall;

    function Reserved(
        riid: TGUID;
        pUnkReserved1: IUnknown;
        pUnkReserved2: IUnknown;
        szReserved: POleStr;
        llReserved: LONGLONG;
        pvReserved: LPVOID;
        cbReserved: DWORD;
        ppReserved: pointer): HRESULT; stdcall;

    function GetName(
        buflen: LPDWORD;
        buffer: LPWSTR): HRESULT; stdcall;

    function GetVersion(
        hi: LPDWORD;
        low: LPDWORD): HRESULT; stdcall;

    function IsEqual(
        name: IAssemblyName;
        flags: DWORD): HRESULT; stdcall;

    function Clone(
        out name: IAssemblyName): HRESULT; stdcall;
  end;

  CREATE_ASM_NAME_OBJ_FLAGS = (
    CANOF_PARSE_DISPLAY_NAME = $1,
    CANOF_SET_DEFAULT_VALUES = $2
  );

function CreateAssemblyCache(out ppAsmCache: IAssemblyCache; dwReserved: DWORD): HRESULT; stdcall; external SXSDLL;
function CreateAssemblyNameObject(out ppAssemblyNameObj: IAssemblyName; szAssemblyName: LPCWSTR;
    dwFlags: DWORD; pvReserved: LPVOID): HRESULT; stdcall; external SXSDLL;


implementation

end.
