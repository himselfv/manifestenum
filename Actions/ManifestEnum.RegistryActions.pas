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
    miValueCopyName: TMenuItem;
    miValueCopyValue: TMenuItem;
    miValueCopyPair: TMenuItem;
    procedure KeyPopupMenuPopup(Sender: TObject);
    procedure ValuePopupMenuPopup(Sender: TObject);
    procedure miValueCopyPairClick(Sender: TObject);
    procedure miValueCopyNameClick(Sender: TObject);
    procedure miValueCopyValueClick(Sender: TObject);
    procedure miKeyCopyNameClick(Sender: TObject);
    procedure miKeyCopyPathClick(Sender: TObject);
    procedure miKeyJumpToLocalRegistryClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FSelectedKeys: TArray<TRegistryKeyId>;
    FSelectedValues: TArray<PRegistryValueData>;
  public
    procedure SetSelectedKey(Item: TRegistryKeyId);
    procedure SetSelectedKeys(Items: TArray<TRegistryKeyId>);
    procedure SetSelectedValue(Item: PRegistryValueData);
    procedure SetSelectedValues(Items: TArray<PRegistryValueData>);
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
  miValueCopyPair.Visible := Length(FSelectedValues) > 0;
  miValueCopyName.Visible := Length(FSelectedValues) > 0;
  miValueCopyValue.Visible := Length(FSelectedValues) > 0;
  miValueCopy.Visible := miValueCopyPair.Visible or miValueCopyName.Visible or miValueCopyValue.Visible;
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


procedure TRegistryActions.miValueCopyPairClick(Sender: TObject);
var Entry: PRegistryValueData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedValues do
    Text := Text + Entry.name + ' = ' + Entry.value + #13;
  Clipboard.AsText := Text;
end;

procedure TRegistryActions.miValueCopyNameClick(Sender: TObject);
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
