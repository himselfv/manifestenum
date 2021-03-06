unit ManifestEnum.FileActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.Menus, AssemblyDb, AssemblyDb.Files;

type
  TFileActions = class(TDataModule)
    FolderPopupMenu: TPopupMenu;
    FilePopupMenu: TPopupMenu;
    miFolderCopy: TMenuItem;
    miFolderCopyName: TMenuItem;
    miFolderCopyPath: TMenuItem;
    miFolderJumpTo: TMenuItem;
    miFolderJumpToLocal: TMenuItem;
    miFileCopy: TMenuItem;
    miFileJumpTo: TMenuItem;
    miFileCopyName: TMenuItem;
    miFileCopyNameAndPath: TMenuItem;
    miFileJumpToLocal: TMenuItem;
    miFileJumpToSxs: TMenuItem;
    miFolderCopyModelPath: TMenuItem;
    miFileOpen: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure miFolderCopyNameClick(Sender: TObject);
    procedure miFolderCopyPathClick(Sender: TObject);
    procedure miFolderCopyModelPathClick(Sender: TObject);
    procedure miFolderJumpToLocalClick(Sender: TObject);
    procedure miFileCopyNameClick(Sender: TObject);
    procedure miFileCopyNameAndPathClick(Sender: TObject);
    procedure miFileJumpToLocalClick(Sender: TObject);
    procedure miFileJumpToSxsClick(Sender: TObject);
    procedure FolderPopupMenuPopup(Sender: TObject);
    procedure FilePopupMenuPopup(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FSelectedFolders: TArray<TFolderId>;
    FSelectedFiles: TArray<PFileEntryData>;
    FInternalFileData: TArray<TFileEntryData>; //stores value data if we have been given IDs
  public
    procedure SetSelectedFolder(Item: TFolderId);
    procedure SetSelectedFolders(Items: TArray<TFolderId>);
    procedure SetSelectedFile(Item: PFileEntryData); overload;
    procedure SetSelectedFiles(Items: TArray<PFileEntryData>);
    procedure SetSelectedFile(Item: TFileEntryId); overload;
    procedure SetSelectedFileIds(Items: TArray<TFileEntryId>);
    property Db: TAssemblyDb read FDb write FDb;
  end;

var
  FileActions: TFileActions;

implementation
uses Clipbrd, OsUtils, SxsUtils, AssemblyDb.Assemblies, AssemblyDb.Environ;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TFileActions.DataModuleCreate(Sender: TObject);
var mi: TMenuItem;
begin
  //Programmatically add Folder popup menu to File popup menu
  mi := TMenuItem.Create(FilePopupMenu);
  mi.Caption := '-';
  FilePopupMenu.Items.Add(mi);
  FilePopupMenu.Items.Add(FolderPopupMenu.Items);
  FolderPopupMenu.Items.Caption := 'Folder';
end;

procedure TFileActions.DataModuleDestroy(Sender: TObject);
begin
  FilePopupMenu.Items.Remove(FolderPopupMenu.Items); //or it'll be destroyed with parent
end;


procedure TFileActions.SetSelectedFolders(Items: TArray<TFolderId>);
begin
  FSelectedFolders := Items;
end;

procedure TFileActions.SetSelectedFolder(Item: TFolderId);
var arr: TArray<TFolderId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedFolders(arr);
end;

//Sets selected RegistryValues by value. You must not free the pointer while the menu is active
procedure TFileActions.SetSelectedFiles(Items: TArray<PFileEntryData>);
var Value: PFileEntryData;
  Folders: TArray<TFolderId>;
begin
  FSelectedFiles := Items;

  SetLength(Folders, 0);
  for Value in FSelectedFiles do begin
    SetLength(Folders, Length(Folders)+1);
    Folders[Length(Folders)-1] := Value.folder;
  end;
  SetSelectedFolders(Folders);
end;

procedure TFileActions.SetSelectedFile(Item: PFileEntryData);
var arr: TArray<PFileEntryData>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedFiles(arr);
end;

//Sets selected RegistryValues by Ids
procedure TFileActions.SetSelectedFileIds(Items: TArray<TFileEntryId>);
var DataRefs: TArray<PFileEntryData>;
  i: integer;
begin
  SetLength(Self.FInternalFileData, Length(Items));
  SetLength(DataRefs, Length(Items));
  for i := 0 to Length(Items)-1 do begin
    Self.FInternalFileData[i] := Db.Files.GetFileEntryById(Items[i]);
    DataRefs[i] := @Self.FInternalFileData[i];
  end;
  Self.SetSelectedFiles(DataRefs);
end;

procedure TFileActions.SetSelectedFile(Item: TFileEntryId);
var arr: TArray<TFileEntryId>;
begin
  SetLength(arr, 1);
  arr[0] := Item;
  SetSelectedFileIds(arr);
end;


procedure TFileActions.FolderPopupMenuPopup(Sender: TObject);
begin
  miFolderCopyName.Visible := Length(FSelectedFolders) > 0;
  miFolderCopyPath.Visible := Length(FSelectedFolders) > 0;
  miFolderCopyModelPath.Visible := Length(FSelectedFolders) > 0;
  miFolderCopy.Visible := miFolderCopyName.Visible or miFolderCopyPath.Visible
    or miFolderCopyModelPath.Visible;

  miFolderJumpToLocal.Visible := Length(FSelectedFolders) = 1;
  miFolderJumpTo.Visible := miFolderJumpToLocal.Visible;
end;

procedure TFileActions.FilePopupMenuPopup(Sender: TObject);
begin
  miFileCopyName.Visible := Length(FSelectedFiles) > 0;
  miFileCopyNameAndPath.Visible := Length(FSelectedFiles) > 0;
  miFileCopy.Visible := miFileCopyName.Visible or miFileCopyNameAndPath.Visible;

  miFileJumpToLocal.Visible := Length(FSelectedFiles) = 1;
  miFileJumpToSxs.Visible := Length(FSelectedFiles) = 1;
  miFileJumpTo.Visible := miFileJumpToLocal.Visible or miFileJumpToSxs.Visible;
end;


procedure TFileActions.miFolderCopyNameClick(Sender: TObject);
var Id: TFolderId;
  Text: string;
begin
  Text := '';
  for Id in FSelectedFolders do
    Text := Text + Db.Files.GetFolderName(Id) + #13;
  Clipboard.AsText := Text;
end;

procedure TFileActions.miFolderCopyPathClick(Sender: TObject);
var Id: TFolderId;
  Text: string;
begin
  Text := '';
  for Id in FSelectedFolders do
    Text := Text + Db.Files.GetFolderPath(Id) + #13;
  Clipboard.AsText := Text;
end;

procedure TFileActions.miFolderCopyModelPathClick(Sender: TObject);
var Id: TFolderId;
  Text: string;
  env: TEnvironment;
begin
  env := GetModelEnvironmentBlock(); //expand with default architecture (x64)
  Text := '';
  for Id in FSelectedFolders do
    Text := Text + ExpandEnvironmentVariables(env, Db.Files.GetFolderPath(Id)) + #13;
  Clipboard.AsText := Text;
end;

procedure TFileActions.miFolderJumpToLocalClick(Sender: TObject);
var Path: string;
begin
  if Length(FSelectedFolders) <> 1 then exit;
  Path := Db.Files.GetFolderPath(FSelectedFolders[0]);
  Path := ExpandEnvironmentVariables(GetLocalEnvironmentBlock(), Path); //TODO: x64/x86? Two different jumps? Separate in list?
  OsUtils.ShellOpen(Path);
end;


procedure TFileActions.miFileCopyNameClick(Sender: TObject);
var Entry: PFileEntryData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedFiles do
    Text := Text + Entry.name + #13;
  Clipboard.AsText := Text;
end;

procedure TFileActions.miFileCopyNameAndPathClick(Sender: TObject);
var Entry: PFileEntryData;
  Text: string;
begin
  Text := '';
  for Entry in FSelectedFiles do
    Text := Text + Db.Files.GetFolderPath(Entry.folder)+'\'+Entry.name + #13;
  Clipboard.AsText := Text;
end;

procedure TFileActions.miFileJumpToLocalClick(Sender: TObject);
var Path: string;
begin
  if Length(FSelectedFiles) <> 1 then exit;
  Path := Db.Files.GetFolderPath(FSelectedFiles[0].folder)+'\'+FSelectedFiles[0].name;
  Path := ExpandEnvironmentVariables(GetLocalEnvironmentBlock(), Path); //TODO: x64/x86?
  OsUtils.ExplorerAtFile(Path);
end;

procedure TFileActions.miFileJumpToSxsClick(Sender: TObject);
var Path: string;
  Assembly: TAssemblyData;
begin
  if Length(FSelectedFolders) <> 1 then exit;

  if FSelectedFiles[0].assembly <= 0 then exit;
  Assembly := FDb.Assemblies.GetAssembly(FSelectedFiles[0].assembly);

  Path := SxsDir() + '\' + Assembly.manifestName + '\';
  if FSelectedFiles[0].sourceName <> '' then
    Path := Path + FSelectedFiles[0].sourceName
  else
    Path := Path + FSelectedFiles[0].name;
  OsUtils.ExplorerAtFile(Path);
end;

procedure TFileActions.miFileOpenClick(Sender: TObject);
var Path: string;
  Assembly: TAssemblyData;
begin
  if Length(FSelectedFiles) <> 1 then exit;

  if FSelectedFiles[0].assembly <= 0 then exit;
  Assembly := FDb.Assemblies.GetAssembly(FSelectedFiles[0].assembly);

  Path := SxsDir() + '\' + Assembly.manifestName + '\';
  if FSelectedFiles[0].sourceName <> '' then
    Path := Path + FSelectedFiles[0].sourceName
  else
    Path := Path + FSelectedFiles[0].name;

  ShellOpen(Path);
end;

end.
