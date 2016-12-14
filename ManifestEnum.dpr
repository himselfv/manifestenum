program ManifestEnum;

uses
  Vcl.Forms,
  ManifestEnum_Main in 'ManifestEnum_Main.pas' {MainForm},
  ManifestEnum_Progress in 'ManifestEnum_Progress.pas' {ProgressForm},
  ManifestEnum.Log in 'ManifestEnum.Log.pas' {LogForm},
  CommonResources in 'CommonResources.pas' {ResourceModule: TDataModule},
  CommonFilters in 'CommonFilters.pas' {FiltersForm},
  AssemblyDb in 'Db\AssemblyDb.pas',
  AssemblyDb.Core in 'Db\AssemblyDb.Core.pas',
  AssemblyDb.Environ in 'Db\AssemblyDb.Environ.pas',
  AssemblyDb.Assemblies in 'Db\AssemblyDb.Assemblies.pas',
  AssemblyDb.Bundles in 'Db\AssemblyDb.Bundles.pas',
  AssemblyDb.Files in 'Db\AssemblyDb.Files.pas',
  AssemblyDb.Registry in 'Db\AssemblyDb.Registry.pas',
  AssemblyDb.Services in 'Db\AssemblyDb.Services.pas',
  AssemblyDb.UnusualProps in 'Db\AssemblyDb.UnusualProps.pas',
  SxsExpand in 'Sxs\SxsExpand.pas',
  MSDeltaLib in 'Sxs\MSDeltaLib.pas',
  CompressApi in 'Sxs\CompressApi.pas',
  WinSxS in 'Sxs\WinSxS.pas',
  SxsUtils in 'Sxs\SxsUtils.pas',
  ManifestParser in 'Db\ManifestParser.pas',
  ManifestSaxParser in 'Db\ManifestSaxParser.pas',
  AssemblyDbBuilder in 'Db\AssemblyDbBuilder.pas',
  ManifestEnum.AssemblyActions in 'Actions\ManifestEnum.AssemblyActions.pas' {AssemblyActions: TDataModule},
  ManifestEnum.RegistryActions in 'Actions\ManifestEnum.RegistryActions.pas' {RegistryActions: TDataModule},
  ManifestEnum.FileActions in 'Actions\ManifestEnum.FileActions.pas' {FileActions: TDataModule},
  DelayLoadTree in 'Views\DelayLoadTree.pas' {DelayLoadTree},
  AssemblyDetails in 'AssemblyDetails.pas' {AssemblyDetailsForm},
  AssemblyBrowser in 'Browsers\AssemblyBrowser.pas' {AssemblyBrowserForm},
  AssemblyTree in 'Views\AssemblyTree.pas' {AssemblyTreeForm},
  AssemblyFilesView in 'Views\AssemblyFilesView.pas' {AssemblyFilesForm},
  AssemblyResourcesView in 'Views\AssemblyResourcesView.pas' {AssemblyResourcesForm},
  RegistryBrowser in 'Browsers\RegistryBrowser.pas' {RegistryBrowserForm},
  ServiceBrowser in 'Browsers\ServiceBrowser.pas' {ServiceBrowserForm},
  TaskBrowser in 'Browsers\TaskBrowser.pas' {TaskBrowserForm},
  FileBrowser in 'Browsers\FileBrowser.pas' {FileBrowserForm},
  CategoryBrowser in 'Browsers\CategoryBrowser.pas' {CategoryBrowserForm},
  AutorunsBrowser in 'Browsers\AutorunsBrowser.pas' {AutorunsBrowserForm},
  ShellExtBrowser in 'Browsers\ShellExtBrowser.pas' {ShellExtensionBrowserForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TResourceModule, ResourceModule);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TAssemblyActions, AssemblyActions);
  Application.CreateForm(TRegistryActions, RegistryActions);
  Application.CreateForm(TFileActions, FileActions);
  Application.CreateForm(TFiltersForm, FiltersForm);
  Application.Run;
end.
