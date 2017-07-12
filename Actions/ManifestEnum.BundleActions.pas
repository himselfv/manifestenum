unit ManifestEnum.BundleActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, AssemblyDb, AssemblyDb.Core, AssemblyDb.Assemblies,
  AssemblyDb.Bundles, ManifestEnum.AssemblyActions;

type
 //Either bundles or bundle folders or both types of items can be selected.
 //The action list will contain only items appropriate for all selected items.
  TBundleActions = class(TDataModule)
    PopupMenu: TPopupMenu;
    miCopyName: TMenuItem;
    miEditDefinition: TMenuItem;
    miJumpTo: TMenuItem;
    miCopy: TMenuItem;
    miCopyFullPath: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure miCopyNameClick(Sender: TObject);
    procedure miEditDefinitionClick(Sender: TObject);
    procedure miJumpToClick(Sender: TObject);
    procedure miCopyFullPathClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FSelectedFolders: TArray<string>;      // A list of selected bundle folders (full paths from root)
    FSelectedBundles: TArray<TBundleId>;   // A list of selected bundles
    FAssemblyActions: TMenuItem;
    procedure UpdateAvailability;
  public
    procedure SetSelectedFolder(Item: string); overload;
    procedure SetSelectedFolders(Items: TArray<string>); overload;
    procedure SetSelectedBundle(Item: TBundleId); overload;
    procedure SetSelectedBundles(Items: TArray<TBundleId>); overload;
    property Db: TAssemblyDb read FDb write FDb;
  end;

var
  BundleActions: TBundleActions;

implementation
uses Clipbrd, OsUtils;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TBundleActions.DataModuleCreate(Sender: TObject);
begin
  //Programmatically add Assemblies popup menu contents
  FAssemblyActions := AssemblyActions.PopupMenu.Items;
  PopupMenu.Items.Add(FAssemblyActions);
  FAssemblyActions.Caption := 'Assemblies';
end;

procedure TBundleActions.DataModuleDestroy(Sender: TObject);
begin
  PopupMenu.Items.Remove(AssemblyActions.PopupMenu.Items);
end;


procedure TBundleActions.SetSelectedFolders(Items: TArray<string>);
begin
  FSelectedFolders := Items;
  UpdateAvailability;
end;

procedure TBundleActions.SetSelectedFolder(Item: string);
var arr: TArray<string>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedFolders(arr);
end;

procedure TBundleActions.SetSelectedBundles(Items: TArray<TBundleId>);
var bundleId: TBundleId;
  AList: TAssemblyList;
  assemblyIds: TArray<TAssemblyId>;
  assemblyId: TAssemblyData;
begin
  FSelectedBundles := Items;

 //SetSelectedAssemblies() on all bundle assemblies
 //Clients might want to enhance this set by including, say, all plugins
 //for a hosted facility.
 //That's their right; just re-call SetSelectedAssemblies after you call us.
  SetLength(assemblyIds, 0);
  AList := TAssemblyList.Create();
  try
    for bundleId in Items do begin
      Db.Bundles.GetAssemblies(bundleId, AList);
      for assemblyId in AList.Values do begin
        SetLength(assemblyIds, Length(assemblyIds)+1);
        assemblyIds[Length(assemblyIds)-1] := assemblyId.id;
      end;
    end;
  finally
    FreeAndNil(AList);
  end;
  AssemblyActions.SetSelectedAssemblies(assemblyIds);
  UpdateAvailability;
end;

procedure TBundleActions.SetSelectedBundle(Item: TBundleId);
var arr: TArray<TBundleId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedBundles(arr);
end;

procedure TBundleActions.UpdateAvailability;
var HaveFolders, HaveBundles: boolean;
begin
  HaveFolders := Length(FSelectedFolders) > 0;
  HaveBundles := Length(FSelectedBundles) > 0;

  miCopyName.Visible := HaveFolders or HaveBundles;
  miCopyFullPath.Visible := HaveFolders or HaveBundles;
  miCopy.Visible := miCopyName.Visible or miCopyFullPath.Visible;

  miEditDefinition.Visible := (not HaveFolders) and HaveBundles;
  miJumpTo.Visible := HaveFolders or HaveBundles;

  FAssemblyActions.Visible := HaveBundles;
end;


procedure TBundleActions.miCopyNameClick(Sender: TObject);
var copyStr: string;
  folderPath: string;
  bundleId: TBundleId;
begin
  copyStr := '';
  //We can copy both folder names and bundle names
  for folderPath in Self.FSelectedFolders do
    copyStr := copyStr + ExtractFilename(folderPath) + #13#10;
  for bundleId in Self.FSelectedBundles do
    copyStr := copyStr + Db.Bundles.Get(bundleId).name + #13#10;
  if copyStr <> '' then
    SetLength(copyStr, Length(copyStr)-2);
  Clipboard.AsText := copyStr;
end;

procedure TBundleActions.miCopyFullPathClick(Sender: TObject);
var copyStr: string;
  folderPath: string;
  bundleId: TBundleId;
  bundle: TBundleData;
begin
  copyStr := '';
  //We can copy both folder names and bundle names
  for folderPath in Self.FSelectedFolders do
    copyStr := copyStr + folderPath + #13#10;
  for bundleId in Self.FSelectedBundles do begin
    bundle := Db.Bundles.Get(bundleId);
    if bundle.path <> '' then
      copyStr := copyStr + bundle.path + '\' + bundle.name + #13#10
    else
      copyStr := copyStr + bundle.name + #13#10
  end;
  if copyStr <> '' then
    SetLength(copyStr, Length(copyStr)-2);
  Clipboard.AsText := copyStr;
end;

procedure TBundleActions.miEditDefinitionClick(Sender: TObject);
var bundleId: TBundleId;
  bundle: TBundleData;
begin
  //For now we can open definition files only for bundles
  for bundleId in Self.FSelectedBundles do begin
    bundle := Db.Bundles.Get(bundleId);
    ShellOpen(BundleDir()+'\'+bundle.path+'\'+bundle.name+'.txt');
  end;
end;

procedure TBundleActions.miJumpToClick(Sender: TObject);
var bundleId: TBundleId;
  bundle: TBundleData;
  folderPath: string;
begin
  //We can jump to folders itself and to bundle parent folders
  for folderPath in Self.FSelectedFolders do
    ShellOpen(BundleDir()+'\'+folderPath);  //Open the folder

  for bundleId in Self.FSelectedBundles do begin
    bundle := Db.Bundles.Get(bundleId);
    ExplorerAtFile(BundleDir()+'\'+bundle.path+'\'+bundle.name+'.txt'); //Highlight the file in folder
  end;
end;

end.
