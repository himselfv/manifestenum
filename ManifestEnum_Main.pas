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
    Components1: TMenuItem;
    Rebuildassemblydatabase1: TMenuItem;
    Loadmanifests1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure lbComponentsClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Rebuildassemblydatabase1Click(Sender: TObject);
    procedure Loadmanifests1Click(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    FAssemblies: TObjectList<TAssemblyDetails>;
    FFileRefs: TDictionary<string, TAssemblyDetails>;
  public
    procedure Reload;
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
  FAssemblies := TObjectList<TAssemblyDetails>.Create;
  FFileRefs := TDictionary<string, TAssemblyDetails>.Create;
  FDb := TAssemblyDb.Create;
  FDb.Open(AppFolder+'\assembly.db');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFileRefs);
  FreeAndNil(FAssemblies);
  FreeAndNil(FDb);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Loadmanifests1Click(Sender: TObject);
begin
  Reload;
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

procedure TMainForm.Reload;
var baseDir: string;
  fnames: TStringList;
  i: integer;
  ad: TAssemblyDetails;
  progress: TProgressForm;
begin
  lbComponents.Clear;
  FFileRefs.Clear;
  FAssemblies.Clear;

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
      ad := TAssemblyDetails.Create;
      ad.name := fnames[i];

      FDb.ImportManifest(baseDir+'\'+fnames[i]);

      FAssemblies.Add(ad);
      progress.Step();
    end;

    FDb.CommitTransaction;

    {
    //Build a list of all files and which assemblies contain those
    progress.Start('Indexing file entries', FAssemblies.Count);
    for i := 0 to FAssemblies.Count-1 do begin
      ad := FAssemblies[i];
      for j := 0 to Length(ad.files)-1 do
        FFileRefs.AddOrSetValue(ad.files[j].ToLower(), ad);
      progress.Step();
    end;

    }

  finally
    FreeAndNil(fnames);
    FreeAndNil(progress);
  end;

  UpdateAssemblyList;
end;

//Показывает новый отфильтрованный список пакетов
procedure TMainForm.UpdateAssemblyList;
var i, j: integer;
  found: boolean;
  filter: string;
begin
  lbComponents.Items.BeginUpdate;
  try
    lbComponents.Items.Clear;

    filter := edtQuickFilter.Text;
    filter := filter.ToLower();
    for i := 0 to FAssemblies.Count-1 do begin

      if filter <> '' then begin
        found := false;
        for j := 0 to Length(FAssemblies[i].files)-1 do
          if FAssemblies[i].files[j].Contains(filter) then begin
            found := true;
            break;
          end;
        if not found then continue;
      end;

      lbComponents.AddItem(FAssemblies[i].name, FAssemblies[i]);
    end;
  finally
    lbComponents.Items.EndUpdate;
  end;
end;

procedure TMainForm.edtQuickFilterChange(Sender: TObject);
begin
  UpdateAssemblyList;
end;

procedure TMainForm.lbComponentsClick(Sender: TObject);
begin
  ReloadAssemblyDetails;
end;

procedure TMainForm.ReloadAssemblyDetails;
var ad: TAssemblyDetails;
  i: integer;
begin
  lbAssemblyFiles.Clear;
  lbAssemblyDependencies.Clear;
  if lbComponents.ItemIndex < 0 then exit;

  ad := TAssemblyDetails(lbComponents.Items.Objects[lbComponents.ItemIndex]);

  for i := 0 to Length(ad.files)-1 do
    lbAssemblyFiles.Items.Add(ad.files[i]);
  for i := 0 to Length(ad.dependencies)-1 do
    lbAssemblyDependencies.Items.Add(ad.dependencies[i]);
end;

procedure TMainForm.Rebuildassemblydatabase1Click(Sender: TObject);
begin
//
end;

end.
