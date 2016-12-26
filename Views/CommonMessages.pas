unit CommonMessages;

interface
uses Messages, Windows;

const
  WM_SET_QUICKFILTER = WM_USER + 2000 + 1;  //wParam: filterString

type
  TWmSetQuickFilter = record
    Msg: Cardinal;
    MsgFiller: TDwordFiller;
    FilterText: PString;
    Unused: LPARAM;
    Result: LRESULT;
  end;

procedure SetQuickFilter(hWnd: HWND; const AFilter: string);

implementation

procedure SetQuickFilter(hWnd: HWND; const AFilter: string);
begin
  SendMessage(hWnd, WM_SET_QUICKFILTER, NativeUInt(@AFilter), 0);
end;

end.
