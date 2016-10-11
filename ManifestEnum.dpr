program ManifestEnum;

uses
  Vcl.Forms,
  ManifestEnum_Main in 'ManifestEnum_Main.pas' {MainForm},
  ManifestEnum_Progress in 'ManifestEnum_Progress.pas' {ProgressForm},
  AssemblyDb in 'AssemblyDb.pas',
  SxsExpand in 'SxsExpand.pas',
  MSDeltaLib in 'MSDeltaLib.pas',
  AssemblyDbBuilder in 'AssemblyDbBuilder.pas',
  AssemblyDetails in 'AssemblyDetails.pas' {AssemblyDetailsForm},
  RegistryBrowser in 'Views\RegistryBrowser.pas' {RegistryBrowserForm},
  AssemblyFilesView in 'Views\AssemblyFilesView.pas' {AssemblyFilesForm},
  DelayLoadTree in 'Views\DelayLoadTree.pas' {DelayLoadTree},
  AssemblyTree in 'Views\AssemblyTree.pas' {AssemblyTreeForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TAssemblyTreeForm, AssemblyTreeForm);
  Application.Run;
end.
