unit ManifestEnum.RegistryActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, AssemblyDb, AssemblyDb.Registry;

type
  TRegistryActions = class(TDataModule)
    KeyPopupMenu: TPopupMenu;
    miKeyCopy: TMenuItem;
    miKeyCopyName: TMenuItem;
    miKeyCopyPath: TMenuItem;
    miKeyJumpTo: TMenuItem;
    miKeyJumpToLocalRegistry: TMenuItem;
    ValuePopupMenu: TPopupMenu;
    miValueCopy: TMenuItem;
    miValueCopyNameType: TMenuItem;
    miValueCopyValue: TMenuItem;
    miValueCopyNameValue: TMenuItem;
    miValueCopyFullEntry: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure KeyPopupMenuPopup(Sender: TObject);
    procedure ValuePopupMenuPopup(Sender: TObject);
    procedure miValueCopyFullEntryClick(Sender: TObject);
    procedure miValueCopyNameValueClick(Sender: TObject);
    procedure miValueCopyNameTypeClick(Sender: TObject);
    procedure miValueCopyValueClick(Sender: TObject);
    procedure miKeyCopyNameClick(Sender: TObject);
    procedure miKeyCopyPathClick(Sender: TObject);
    procedure miKeyJumpToLocalRegistryClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FSelectedKeys: TArray<TRegistryKeyId>;
    FSelectedValues: TArray<PRegistryValueData>;
    FInternalValueData: TArray<TRegistryValueData>; //stores value data if we have been given IDs
  public
    procedure SetSelectedKey(Item: TRegistryKeyId);
    procedure SetSelectedKeys(Items: TArray<TRegistryKeyId>);
    procedure SetSelectedValue(Item: PRegistryValueData); overload;
    procedure SetSelectedValues(Items: TArray<PRegistryValueData>);
    procedure SetSelectedValue(Item: TRegistryValueId); overload;
    procedure SetSelectedValueIds(Items: TArray<TRegistryValueId>);
    property Db: TAssemblyDb read FDb write FDb;
  end;

var
  RegistryActions: TRegistryActions;

implementation
uses Clipbrd, OsUtils;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TRegistryActions.DataModuleCreate(Sender: TObject);
var mi: TMenuItem;
begin
  //Programmatically add Keys popup menu to Values popup menu, to cover values parent keys
  //This is hackish, we might be better off copying the menu and enabling-disabling both copies
  //via actions
  mi := TMenuItem.Create(ValuePopupMenu);
  mi.Caption := '-';
  ValuePopupMenu.Items.Add(mi);
  ValuePopupMenu.Items.Add(KeyPopupMenu.Items);
  KeyPopupMenu.Items.Caption := 'Key';
end;

procedure TRegistryActions.DataModuleDestroy(Sender: TObject);
begin
  ValuePopupMenu.Items.Remove(KeyPopupMenu.Items); //or it'll be destroyed with parent
end;

procedure TRegistryActions.SetSelectedKeys(Items: TArray<TRegistryKeyId>);
begin
  FSelectedKeys := Items;
end;

procedure TRegistryActions.SetSelectedKey(Item: TRegistryKeyId);
var arr: TArray<TRegistryKeyId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedKeys(arr);
end;

//Sets selected RegistryValues by value. You must not free the pointer while the menu is active
procedure TRegistryActions.SetSelectedValues(Items: TArray<PRegistryValueData>);
var Value: PRegistryValueData;
  Keys: TArray<TRegistryKeyId>;
begin
  FSelectedValues := Items;

  SetLength(Keys, 0);
  for Value in FSelectedValues do begin
    SetLength(Keys, Length(Keys)+1);
    Keys[Length(Keys)-1] := Value.key;
  end;
  SetSelectedKeys(Keys);
end;

procedure TRegistryActions.SetSelectedValue(Item: PRegistryValueData);
var arr: TArray<PRegistryValueData>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedValues(arr);
end;

//Sets selected RegistryValues by Ids
procedure TRegistryActions.SetSelectedValueIds(Items: TArray<TRegistryValueId>);
var DataRefs: TArray<PRegistryValueData>;
  i: integer;
begin
  SetLength(Self.FInternalValueData, Length(Items));
  SetLength(DataRefs, Length(Items));
  for i := 0 to Length(Items)-1 do begin
    Self.FInternalValueData[i] := Db.Registry.GetValueById(Items[i]);
    DataRefs[i] := @Self.FInternalValueData[i];
  end;
  Self.SetSelectedValues(DataRefs);
end;

procedure TRegistryActions.SetSelectedValue(Item: TRegistryValueId);
var arr: TArray<TRegistryValueId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedValueIds(arr);
end;


procedure TRegistryActions.KeyPopupMenuPopup(Sender: TObject);
begin
  miKeyCopyName.Visible := Length(FSelectedKeys) > 0;
  miKeyCopyPath.Visible := Length(FSelectedKeys) > 0;
  miKeyCopy.Visible := miKeyCopyName.Visible or miKeyCopyPath.Visible;

  miKeyJumpToLocalRegistry.Visible := Length(FSelectedKeys) = 1;
  miKeyJumpTo.Visible := miKeyJumpToLocalRegistry.Visible;
end;

procedure TRegistryActions.ValuePopupMenuPopup(Sender: TObject);
begin
  miValueCopyFullEntry.Visible := Length(FSelectedValues) > 0;
  miValueCopyNameValue.Visible := Length(FSelectedValues) > 0;
  miValueCopyNameType.Visible := Length(FSelectedValues) > 0;
  miValueCopyValue.Visible := Length(FSelectedValues) > 0;
  miValueCopy.Visible := miValueCopyFullEntry.Visible or miValueCopyNameValue.Visible
    or miValueCopyNameType.Visible or miValueCopyValue.Visible;
end;


procedure TRegistryActions.miKeyCopyNameClick(Sender: TObject);
var Id: TRegistryKeyId;
  Text: string;
begin
  Text := '';
  for Id in FSelectedKeys do
    Text := Text + FDb.Registry.GetKeyName(Id) + #13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miKeyCopyPathClick(Sender: TObject);
var Id: TRegistryKeyId;
  Text: string;
begin
  Text := '';
  for Id in FSelectedKeys do
    Text := Text + FDb.Registry.GetKeyPath(Id) + #13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miKeyJumpToLocalRegistryClick(Sender: TObject);
var Path: string;
begin
  if Length(FSelectedKeys) <> 1 then exit;
  Path := FDb.Registry.GetKeyPath(FSelectedKeys[0]);
  OsUtils.RegeditOpenAndNavigate(Path);
end;


procedure TRegistryActions.miValueCopyFullEntryClick(Sender: TObject);
var Entry: PRegistryValueData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedValues do
    Text := Text + FDb.Registry.GetKeyPath(Entry.key) + '\' + Entry.name
      + ' (' + GetRegistryValueTypeName(Entry.valueType) + ')'
      + ' = ' + Entry.value + #13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miValueCopyNameValueClick(Sender: TObject);
var Entry: PRegistryValueData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedValues do
    Text := Text + Entry.name + ' = ' + Entry.value + #13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miValueCopyNameTypeClick(Sender: TObject);
var Entry: PRegistryValueData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedValues do
    Text := Text + Entry.name + ' (' + GetRegistryValueTypeName(Entry.valueType) + ')'#13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miValueCopyValueClick(Sender: TObject);
var Entry: PRegistryValueData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedValues do
    Text := Text + Entry.Value + #13;
  Clipboard.AsText := Text;
end;


end.
