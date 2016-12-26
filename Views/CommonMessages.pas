unit CommonMessages;

interface
uses Messages, Windows, AssemblyDb.Assemblies;

const
 //Sent by the main form to every affected child form on quick filter text change
  WM_SET_QUICKFILTER = WM_USER + 2000 + 1;

type
  TWmSetQuickFilter = record
    Msg: Cardinal;
    MsgFiller: TDwordFiller;
    FilterText: PString;
    Unused: LPARAM;
    Result: LRESULT;
  end;

procedure SetQuickFilter(hWnd: HWND; const AFilter: string);


const
 //Sent by browser forms to their parent forms. Contains the list of assemblies related to what is
 //selected in the browser form.
 //E.g. when a file is selected, File Browser will send a list assemblies which contain that file.
  WM_SET_ASSEMBLY_SELECTION = WM_USER + 2100 + 1;

type
  TWmSetAssemblySelection = record
    Msg: Cardinal;
    MsgFiller: TDwordFiller;
    Assemblies: TArray<TAssemblyId>;
    Unused: LPARAM;
    Result: LRESULT;
  end;

procedure SetAssemblySelection(hWnd: HWND; const AAssembly: TAssemblyId); overload;
procedure SetAssemblySelection(hWnd: HWND; const AAssemblies: TArray<TAssemblyId>); overload;

implementation

procedure SetQuickFilter(hWnd: HWND; const AFilter: string);
begin
  SendMessage(hWnd, WM_SET_QUICKFILTER, NativeUInt(@AFilter), 0);
end;

procedure SetAssemblySelection(hWnd: HWND; const AAssembly: TAssemblyId);
var AIds: TArray<TAssemblyId>;
begin
  SetLength(AIDs, 1);
  AIDs[0] := AAssembly;
  SetAssemblySelection(hWnd, AIDs);
end;

procedure SetAssemblySelection(hWnd: HWND; const AAssemblies: TArray<TAssemblyId>);
begin
  SendMessage(hWnd, WM_SET_ASSEMBLY_SELECTION, NativeUInt(AAssemblies), 0);
end;

end.
