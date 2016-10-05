unit ManifestEnum_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Generics.Collections, Vcl.Menus, AssemblyDb;

type
  TAssemblyDetails = class
    name: string;
    files: array of string;
    dependencies: array of string;
  end;

  TMainForm = class(TForm)
    lbComponents: TListBox;
    edtQuickFilter: TEdit;
    pcDetails: TPageControl;
    tsGeneral: TTabSheet;
    tsFiles: TTabSheet;
    lbAssemblyFiles: TListBox;
    tsDependencies: TTabSheet;
    lbAssemblyDependencies: TListBox;
    MainMenu: TMainMenu;
    F1: TMenuItem;
    Exit1: TMenuItem;
    pmRebuildAssemblyDatabase: TMenuItem;
    N1: TMenuItem;
    Reload1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure lbComponentsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure pmRebuildAssemblyDatabaseClick(Sender: TObject);
    procedure Reload1Click(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    procedure ProcessManifests;
    procedure RebuildAssemblyDatabase;
  public
    procedure UpdateAssemblyList;
    procedure ReloadAssemblyDetails;
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, ManifestEnum_Progress;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDb := TAssemblyDb.Create;
  if not FileExists(AppFolder+'\assembly.db') then
    RebuildAssemblyDatabase
  else
    FDb.Open(AppFolder+'\assembly.db');
  UpdateAssemblyList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDb);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

//Создаёт TStringList и заполняет его файлами из папки, по маске
function FilesByMask(const AMask: string): TStringList;
var sr: TSearchRec;
  res: integer;
begin
  Result := TStringList.Create;
  res := FindFirst(AMask, faAnyFile and not faDirectory, sr);
  while res = 0 do begin
    Result.Add(sr.Name);
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

//Parses all manifests in WinSxS\Manifests and adds/updates their data in the database.
//Displays progress form.
procedure TMainForm.ProcessManifests;
var baseDir: string;
  fnames: TStringList;
  i: integer;
  progress: TProgressForm;  
begin
  baseDir := GetWindowsDir()+'\WinSxS\Manifests';
  fnames := nil;

  progress := TProgressForm.Create(Self);
  try
    progress.Show;

    //Составляем список файлов
    progress.Start('Building file list');
    fnames := FilesByMask(baseDir+'\*.manifest');

    FDb.BeginTransaction;

    //Теперь загружаем содержимое.
    progress.Start('Reading manifests', fnames.Count-1);
    for i := 0 to fnames.Count-1 do begin
      FDb.ImportManifest(baseDir+'\'+fnames[i]);
      progress.Step();
    end;

    FDb.CommitTransaction;
  finally
    FreeAndNil(fnames);
    FreeAndNil(progress);
  end;
end;

procedure TMainForm.RebuildAssemblyDatabase;
begin
  FDb.Close;
  DeleteFile(AppFolder+'\assembly.db');
  FDb.Open(AppFolder+'\assembly.db');
  ProcessManifests;
end;

procedure TMainForm.pmRebuildAssemblyDatabaseClick(Sender: TObject);
begin
  RebuildAssemblyDatabase;
  UpdateAssemblyList;
end;

//Показывает новый отфильтрованный список пакетов
procedure TMainForm.UpdateAssemblyList;
var i: integer;
  filter: string;
  list: TList<TAssemblyData>;
begin
  lbComponents.Items.BeginUpdate;
  try
    lbComponents.Items.Clear;

    filter := edtQuickFilter.Text;
    filter := filter.ToLower();

    if filter <> '' then
      list := FDb.FindAssemblyByFile(filter)
    else
      list := FDb.GetAllAssemblies;
    for i := 0 to list.Count-1 do
      lbComponents.AddItem(list[i].identity.name, TObject(list[i].id));
  finally
    lbComponents.Items.EndUpdate;
  end;
end;

procedure TMainForm.edtQuickFilterChange(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.Reload1Click(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.lbComponentsClick(Sender: TObject);
begin
  ReloadAssemblyDetails;
end;

procedure TMainForm.ReloadAssemblyDetails;
var id: TAssemblyId;
  files: TList<TFileEntryData>;
  i: integer;
begin
  lbAssemblyFiles.Clear;
  lbAssemblyDependencies.Clear;
  if lbComponents.ItemIndex < 0 then exit;

  id := int64(lbComponents.Items.Objects[lbComponents.ItemIndex]);

  files := FDb.GetAssemblyFiles(id);
  for i := 0 to files.Count-1 do
    lbAssemblyFiles.Items.Add(files[i].name);

//  for i := 0 to Length(ad.dependencies)-1 do
//    lbAssemblyDependencies.Items.Add(ad.dependencies[i]);
end;


end.
