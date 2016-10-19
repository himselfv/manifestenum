unit AssemblyDbBuilder;

interface
uses AssemblyDb, ManifestParser;

function SxSDir: string;
function SxSManifestDir: string;

procedure InitAssemblyDb(ADb: TAssemblyDb; const AFilename: string; AAutoRebuild: boolean = true);
procedure ImportAssemblyManifests(ADb: TAssemblyDb);
procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);

implementation
uses SysUtils, Classes, FilenameUtils, ManifestEnum_Progress;

function SxSDir: string;
begin
  Result := GetWindowsDir()+'\WinSxS';
end;

function SxSManifestDir: string;
begin
  Result := GetWindowsDir()+'\WinSxS\Manifests';
end;

procedure InitAssemblyDb(ADb: TAssemblyDb; const AFilename: string; AAutoRebuild: boolean);
begin
  if AAutoRebuild and not FileExists(AFilename) then
    RebuildAssemblyDatabase(ADb, AFilename)
  else
    ADb.Open(AFilename);
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
procedure ImportAssemblyManifests(ADb: TAssemblyDb);
var baseDir: string;
  fnames: TStringList;
  i: integer;
  progress: TProgressForm;
  parser: TManifestParser;
begin
  baseDir := SxSManifestDir()+'\';
  fnames := nil;
  parser := nil;

  progress := TProgressForm.Create(nil);
  try
    progress.Show;

    //Составляем список файлов
    progress.Start('Building file list');
    fnames := FilesByMask(baseDir+'\*.manifest');

    ADb.BeginTransaction;
    parser := TManifestParser.Create(ADb);

    //Теперь загружаем содержимое.
    progress.Start('Reading manifests', fnames.Count-1);
    for i := 0 to fnames.Count-1 do begin
      parser.ImportManifest(baseDir+'\'+fnames[i]);
      progress.Step();
    end;

    ADb.CommitTransaction;
  finally
    FreeAndNil(parser);
    FreeAndNil(fnames);
    FreeAndNil(progress);
  end;
end;

procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);
begin
  ADb.Close;
  DeleteFile(AFilename);
  ADb.Open(AFilename);
  ImportAssemblyManifests(ADb);
end;

end.
