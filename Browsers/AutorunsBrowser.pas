unit AutorunsBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RegistryBrowser, Vcl.StdCtrls, VirtualTrees, AssemblyDb;

type
  TAutorunsBrowserForm = class(TRegistryBrowserForm)
    procedure FormCreate(Sender: TObject);
  protected
    procedure SetDb(ADb: TAssemblyDb); override;
    procedure AddRootKeys;
    procedure AddRoot(const APath: string);
  public
    procedure Reload; override;
  end;

var
  AutorunsBrowserForm: TAutorunsBrowserForm;

implementation

{$R *.dfm}

procedure TAutorunsBrowserForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMode := rmKeys;
end;

procedure TAutorunsBrowserForm.SetDb(ADb: TAssemblyDb);
begin
  if FDb <> ADb then
    FKeys.Clear; //need to rediscover the key IDs
  inherited;
end;

procedure TAutorunsBrowserForm.Reload;
begin
  if FKeys.Count = 0 then //not yet populated
    AddRootKeys;
  inherited;
end;

procedure TAutorunsBrowserForm.AddRoot(const APath: string);
var AKeyId: TRegistryKeyId;
  AExpandedPath: string;
begin
  AExpandedPath := APath
    .Replace('HKLM', 'HKEY_LOCAL_MACHINE')
    .Replace('HKCU', 'HKEY_CURRENT_USER');
  AKeyId := FDb.FindRegistryKeyByPath(AExpandedPath);
  if AKeyId > 0 then
    FKeys.Add(AKeyId, APath);
end;

procedure TAutorunsBrowserForm.AddRootKeys;
begin
 //We'd do much faster manually: find the key for HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion,
 //then find the subkeys for Run, RunOnce and RunOnceEx.
 //Maybe we'll implement an automated way of doing this, if things run slow: add all locations,
 //then do a ResolveLocations() which does this in an optimal way.

 //Run* keys
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKCU\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnce');
  AddRoot('HKCU\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnce');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnceEx');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnceEx');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\RunOnceEx');
  AddRoot('HKCU\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\RunOnceEx');

  AddRoot('HKCU\Software\Microsoft\Windows NT\CurrentVersion\Windows\Load');
  AddRoot('HKCU\Software\Microsoft\Windows NT\CurrentVersion\Windows\Run');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');

 //Scripts
  AddRoot('HKLM\Software\Policies\Microsoft\Windows\System\Scripts\Startup');
  AddRoot('HKCU\Software\Policies\Microsoft\Windows\System\Scripts\Logon');
  AddRoot('HKLM\Software\Policies\Microsoft\Windows\System\Scripts\Logon');
  AddRoot('HKLM\Software\Policies\Microsoft\Windows\System\Scripts\Shutdown');
  AddRoot('HKCU\Software\Policies\Microsoft\Windows\System\Scripts\Logoff');
  AddRoot('HKLM\Software\Policies\Microsoft\Windows\System\Scripts\Logoff');
  AddRoot('HKLM\Software\Microsoft\Windows\CurrentVersion\Group Policy\Scripts\Startup');
  AddRoot('HKLM\Software\Microsoft\Windows\CurrentVersion\Group Policy\Scripts\Shutdown');

 //Winlogon and shell
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\AppSetup');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\Userinit');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\VmApplet');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\Shell');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\Shell');
  AddRoot('HKCU\Software\Microsoft\Windows\CurrentVersion\Policies\System\Shell');
  AddRoot('HKLM\Software\Microsoft\Windows\CurrentVersion\Policies\System\Shell');
  AddRoot('HKLM\SYSTEM\CurrentControlSet\Control\SafeBoot\AlternateShell');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\Taskman');
  AddRoot('HKLM\Software\Microsoft\Windows NT\CurrentVersion\Winlogon\AlternateShells\AvailableShells');

 //Terminal services
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\Runonce');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\RunonceEx');
  AddRoot('HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\Runonce');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\RunonceEx');
  AddRoot('HKCU\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Terminal Server\Install\Software\Microsoft\Windows\CurrentVersion\Run');
  AddRoot('HKLM\System\CurrentControlSet\Control\Terminal Server\Wds\rdpwd\StartupPrograms');
  AddRoot('HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp\InitialProgram');

 //Active setup
  AddRoot('HKLM\SOFTWARE\Microsoft\Active Setup\Installed Components');
  AddRoot('HKLM\SOFTWARE\Wow6432Node\Microsoft\Active Setup\Installed Components');
end;

end.

