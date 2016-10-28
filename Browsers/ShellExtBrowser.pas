unit ShellExtBrowser;
// Lists Explorer extensions in the system registry

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RegistryBrowser, Vcl.StdCtrls, VirtualTrees, AssemblyDb,
  Vcl.ExtCtrls;

type
  TShellExtensionBrowserForm = class(TRegistryBrowserForm)
    procedure FormCreate(Sender: TObject);
  protected
    procedure SetDb(ADb: TAssemblyDb); override;
    procedure AddRootKeys;
    procedure AddRoot(const APath: string);
  public
    procedure Reload; override;
  end;

var
  ShellExtensionBrowserForm: TShellExtensionBrowserForm;

implementation
uses sqlite3, AssemblyDb.Core, AssemblyDb.Registry;

{$R *.dfm}

procedure TShellExtensionBrowserForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMode := rmKeys;
  FValuePresentation := vpPanel;
end;

procedure TShellExtensionBrowserForm.SetDb(ADb: TAssemblyDb);
begin
  if FDb <> ADb then
    FRootKeys.Clear; //need to rediscover the key IDs
  inherited;
end;

procedure TShellExtensionBrowserForm.Reload;
begin
  if FRootKeys.Count = 0 then //not yet populated
    AddRootKeys;
  inherited;
end;

procedure TShellExtensionBrowserForm.AddRoot(const APath: string);
var AKeyId: TRegistryKeyId;
  AExpandedPath: string;
begin
  AExpandedPath := APath
    .Replace('HKLM', 'HKEY_LOCAL_MACHINE')
    .Replace('HKCU', 'HKEY_CURRENT_USER');
  AKeyId := FDb.Registry.FindKeyByPath(AExpandedPath);
  if AKeyId > 0 then
    FRootKeys.Add(AKeyId, APath);
end;

procedure TShellExtensionBrowserForm.AddRootKeys;
var stmt: PSQLite3Stmt;
  AList: TRegistryKeyList;
  AKey: TRegistryKeyId;
  AKeyPath: string;
begin
 //See comments to AutorunsBrowser's AddRootKeys

  AddRoot('HKCU\SOFTWARE\Classes\Protocols\Filter');
  AddRoot('HKLM\SOFTWARE\Classes\Protocols\Filter');
  AddRoot('HKCU\SOFTWARE\Classes\Protocols\Handler');
  AddRoot('HKLM\SOFTWARE\Classes\Protocols\Handler');

  AddRoot('HKCU\SOFTWARE\Microsoft\Internet Explorer\Desktop\Components');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\SharedTaskScheduler');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Explorer\SharedTaskScheduler');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\ShellServiceObjects');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Explorer\ShellServiceObjects');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\ShellServiceObjects');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\ShellServiceObjectDelayLoad');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\ShellServiceObjectDelayLoad');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\ShellServiceObjectDelayLoad');

  AddRoot('HKLM\Software\Microsoft\Windows\CurrentVersion\Explorer\ShellExecuteHooks');
  AddRoot('HKLM\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Explorer\ShellExecuteHooks');

  AddRoot('HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\ShellIconOverlayIdentifiers');
  AddRoot('HKLM\Software\Microsoft\Windows\CurrentVersion\Explorer\ShellIconOverlayIdentifiers');
  AddRoot('HKLM\Software\Wow6432Node\Microsoft\Windows\CurrentVersion\Explorer\ShellIconOverlayIdentifiers');

 {
   We dynamically search for the following subnodes of ShellEx:
     \ContextMenuHandlers
     \PropertySheetHandlers
     \DragDropHandlers
     \CopyHookHandlers
     \ColumnHandlers
     \ExtShellFolderViews
   In the following locations:
     HKCU\Software\Classes\[class]
     HKLM\Software\Classes\[class]
     HKCU\Software\Wow6432Node\Classes\[class]
     HKLM\Software\Wow6432Node\Classes\[class]
   We won't check though because it's slow.
 }

  stmt := FDb.PrepareStatement('SELECT * FROM registryKeys WHERE keyName IN (?, ?, ?, ?, ?, ?)');
  sqlite3_bind_str(stmt, 1, 'ContextMenuHandlers');
  sqlite3_bind_str(stmt, 2, 'PropertySheetHandlers');
  sqlite3_bind_str(stmt, 3, 'DragDropHandlers');
  sqlite3_bind_str(stmt, 4, 'CopyHookHandlers');
  sqlite3_bind_str(stmt, 5, 'ColumnHandlers');
  sqlite3_bind_str(stmt, 6, 'ExtShellFolderViews');

  AList := TRegistryKeyList.Create;
  try
    FDb.Registry.QueryKeys(stmt, AList);
    for AKey in AList.Keys do begin
      AKeyPath := FDb.Registry.GetKeyPath(AKey)
        .Replace('HKEY_LOCAL_MACHINE', 'HKLM')
        .Replace('HKEY_CURRENT_USER', 'HKCU')
        .Replace('HKEY_CLASSES_ROOT', 'HKCR');
      FRootKeys.Add(AKey, AKeyPath);
    end;
  finally
    FreeAndNil(AList);
  end;
end;


end.
