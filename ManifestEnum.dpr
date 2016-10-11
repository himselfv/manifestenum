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
  AssemblyFilesView in 'Views\AssemblyFilesView.pas' {AssemblyFilesForm},
  DelayLoadTree in 'Views\DelayLoadTree.pas' {DelayLoadTree},
  AssemblyTree in 'Views\AssemblyTree.pas' {AssemblyTreeForm},
  AssemblyResourcesView in 'Views\AssemblyResourcesView.pas' {AssemblyResourcesForm},
  RegistryBrowser in 'Browsers\RegistryBrowser.pas' {RegistryBrowserForm},
  TaskBrowser in 'Browsers\TaskBrowser.pas' {TaskBrowserForm},
  CommonResources in 'CommonResources.pas' {ResourceModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TResourceModule, ResourceModule);
  Application.Run;
end.
