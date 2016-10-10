program ManifestEnum;

uses
  Vcl.Forms,
  ManifestEnum_Main in 'ManifestEnum_Main.pas' {MainForm},
  ManifestEnum_Progress in 'ManifestEnum_Progress.pas' {ProgressForm},
  AssemblyDb in 'AssemblyDb.pas',
  SxsExpand in 'SxsExpand.pas',
  MSDeltaLib in 'MSDeltaLib.pas',
  RegistryBrowser in 'RegistryBrowser.pas' {RegistryBrowserForm},
  AssemblyDetails in 'AssemblyDetails.pas' {AssemblyDetailsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.Run;
end.
