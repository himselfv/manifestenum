program ManifestEnum;

uses
  Vcl.Forms,
  ManifestEnum_Main in 'ManifestEnum_Main.pas' {MainForm},
  ManifestEnum_Progress in 'ManifestEnum_Progress.pas' {ProgressForm},
  ManifestEnum.Log in 'ManifestEnum.Log.pas' {LogForm},
  CommonFilters in 'CommonFilters.pas',
  AssemblyDb in 'Db\AssemblyDb.pas',
  AssemblyDb.Core in 'Db\AssemblyDb.Core.pas',
  AssemblyDb.Assemblies in 'Db\AssemblyDb.Assemblies.pas',
  AssemblyDb.Registry in 'Db\AssemblyDb.Registry.pas',
  AssemblyDb.Services in 'Db\AssemblyDb.Services.pas',
  AssemblyDb.UnusualProps in 'Db\AssemblyDb.UnusualProps.pas',
  SxsExpand in 'Db\SxsExpand.pas',
  MSDeltaLib in 'Db\MSDeltaLib.pas',
  CompressApi in 'Db\CompressApi.pas',
  ManifestParser in 'Db\ManifestParser.pas',
  ManifestSaxParser in 'Db\ManifestSaxParser.pas',
  AssemblyDbBuilder in 'Db\AssemblyDbBuilder.pas',
  ManifestEnum.RegistryActions in 'Actions\ManifestEnum.RegistryActions.pas' {RegistryActions: TDataModule},
  ManifestEnum.FileActions in 'Actions\ManifestEnum.FileActions.pas' {FileActions: TDataModule},
  AssemblyBrowser in 'Browsers\AssemblyBrowser.pas' {AssemblyBrowserForm},
  AssemblyDetails in 'AssemblyDetails.pas' {AssemblyDetailsForm},
  AssemblyFilesView in 'Views\AssemblyFilesView.pas' {AssemblyFilesForm},
  DelayLoadTree in 'Views\DelayLoadTree.pas' {DelayLoadTree},
  AssemblyTree in 'Views\AssemblyTree.pas' {AssemblyTreeForm},
  AssemblyResourcesView in 'Views\AssemblyResourcesView.pas' {AssemblyResourcesForm},
  RegistryBrowser in 'Browsers\RegistryBrowser.pas' {RegistryBrowserForm},
  ServiceBrowser in 'Browsers\ServiceBrowser.pas' {ServiceBrowserForm},
  TaskBrowser in 'Browsers\TaskBrowser.pas' {TaskBrowserForm},
  CommonResources in 'CommonResources.pas' {ResourceModule: TDataModule},
  FileBrowser in 'Browsers\FileBrowser.pas' {FileBrowserForm},
  CategoryBrowser in 'Browsers\CategoryBrowser.pas' {CategoryBrowserForm},
  AutorunsBrowser in 'Browsers\AutorunsBrowser.pas' {AutorunsBrowserForm},
  ShellExtBrowser in 'Browsers\ShellExtBrowser.pas' {ShellExtensionBrowserForm},
  WinSxS in 'WinSxS.pas',
  SxsUtils in 'SxsUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TResourceModule, ResourceModule);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TRegistryActions, RegistryActions);
  Application.CreateForm(TFileActions, FileActions);
  Application.Run;
end.
