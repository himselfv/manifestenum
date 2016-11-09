program ManifestEnum;

uses
  Vcl.Forms,
  ManifestEnum_Main in 'ManifestEnum_Main.pas' {MainForm},
  ManifestEnum_Progress in 'ManifestEnum_Progress.pas' {ProgressForm},
  AssemblyDb in 'Db\AssemblyDb.pas',
  AssemblyDb.Core in 'Db\AssemblyDb.Core.pas',
  AssemblyDb.Registry in 'Db\AssemblyDb.Registry.pas',
  SxsExpand in 'Db\SxsExpand.pas',
  MSDeltaLib in 'Db\MSDeltaLib.pas',
  CompressApi in 'Db\CompressApi.pas',
  ManifestParser in 'Db\ManifestParser.pas',
  ManifestSaxParser in 'Db\ManifestSaxParser.pas',
  AssemblyDbBuilder in 'Db\AssemblyDbBuilder.pas',
  AssemblyBrowser in 'Browsers\AssemblyBrowser.pas' {AssemblyBrowserForm},
  AssemblyDetails in 'AssemblyDetails.pas' {AssemblyDetailsForm},
  AssemblyFilesView in 'Views\AssemblyFilesView.pas' {AssemblyFilesForm},
  DelayLoadTree in 'Views\DelayLoadTree.pas' {DelayLoadTree},
  AssemblyTree in 'Views\AssemblyTree.pas' {AssemblyTreeForm},
  AssemblyResourcesView in 'Views\AssemblyResourcesView.pas' {AssemblyResourcesForm},
  RegistryBrowser in 'Browsers\RegistryBrowser.pas' {RegistryBrowserForm},
  TaskBrowser in 'Browsers\TaskBrowser.pas' {TaskBrowserForm},
  CommonResources in 'CommonResources.pas' {ResourceModule: TDataModule},
  FileBrowser in 'Browsers\FileBrowser.pas' {FileBrowserForm},
  CategoryBrowser in 'Browsers\CategoryBrowser.pas' {CategoryBrowserForm},
  AssemblyDb.UnusualProps in 'Db\AssemblyDb.UnusualProps.pas',
  AssemblyDb.Assemblies in 'Db\AssemblyDb.Assemblies.pas',
  AutorunsBrowser in 'Browsers\AutorunsBrowser.pas' {AutorunsBrowserForm},
  ShellExtBrowser in 'Browsers\ShellExtBrowser.pas' {ShellExtensionBrowserForm},
  WinSxS in 'WinSxS.pas',
  OsUtils in 'OsUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TResourceModule, ResourceModule);
  Application.CreateForm(TFileBrowserForm, FileBrowserForm);
  Application.CreateForm(TCategoryBrowserForm, CategoryBrowserForm);
  Application.CreateForm(TAutorunsBrowserForm, AutorunsBrowserForm);
  Application.CreateForm(TShellExtensionBrowserForm, ShellExtensionBrowserForm);
  Application.CreateForm(TAssemblyBrowserForm, AssemblyBrowserForm);
  Application.Run;
end.
