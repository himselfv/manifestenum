unit AutorunsBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RegistryBrowser, Vcl.StdCtrls, VirtualTrees, AssemblyDb;

type
  TAutorunsForm = class(TRegistryBrowserForm)
    procedure FormCreate(Sender: TObject);
  protected
    procedure SetDb(ADb: TAssemblyDb); override;
    procedure AddRootKeys;
    procedure AddRoot(const APath: string);
  public
    procedure Reload; override;
  end;

var
  AutorunsForm: TAutorunsForm;

implementation

{$R *.dfm}

procedure TAutorunsForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMode := rmKeys;
end;

procedure TAutorunsForm.SetDb(ADb: TAssemblyDb);
begin
  if FDb <> ADb then
    FKeys.Clear; //need to rediscover the key IDs
  inherited;
end;

procedure TAutorunsForm.Reload;
begin
  if FKeys.Count = 0 then //not yet populated
    AddRootKeys;
  inherited;
end;

procedure TAutorunsForm.AddRootKeys;
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

end;

procedure TAutorunsForm.AddRoot(const APath: string);
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

end.

