unit ManifestEnum.RegistryActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, AssemblyDb.Registry;

type
  TRegistryActions = class(TDataModule)
    PopupMenu: TPopupMenu;
    miCopy: TMenuItem;
    miCopyKeyName: TMenuItem;
    miCopyKeyPath: TMenuItem;
    miJumpTo: TMenuItem;
    miJumpToLocalRegistry: TMenuItem;
    procedure PopupMenuPopup(Sender: TObject);
  protected
    FSelectedKeys: TArray<TRegistryKeyId>;
  public
    procedure SetSelectedKeys(var Items: TArray<TRegistryKeyId>);
  end;

var
  RegistryActions: TRegistryActions;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TRegistryActions.SetSelectedKeys(var Items: TArray<TRegistryKeyId>);
begin
  FSelectedKeys := Items;
end;

procedure TRegistryActions.PopupMenuPopup(Sender: TObject);
begin
  miCopyKeyName.Visible := Length(FSelectedKeys) > 0;
  miCopyKeyPath.Visible := Length(FSelectedKeys) > 0;
  miCopy.Visible := miCopyKeyName.Visible or miCopyKeyPath.Visible;

  miJumpToLocalRegistry.Visible := Length(FSelectedKeys) = 1;
  miJumpTo.Visible := miJumpToLocalRegistry.Visible;
end;


end.
