unit AssemblyDbBuilder;

//{$DEFINE PROFILE}

{$DEFINE SAX}
//Use SAX parser instead of the DOM one

interface
uses AssemblyDb, {$IFDEF SAX}ManifestSaxParser{$ELSE}ManifestParser{$ENDIF};

{$IFDEF SAX}
type
  TManifestParser = TSaxManifestParser;
{$ENDIF}

function SxSDir: string;
function SxSManifestDir: string;

procedure InitAssemblyDb(ADb: TAssemblyDb; const AFilename: string; AAutoRebuild: boolean = true);
procedure ImportAssemblyManifests(ADb: TAssemblyDb);
procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);

implementation
uses Windows, SysUtils, Classes, FilenameUtils, ManifestEnum_Progress, AssemblyDb.Assemblies;

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
 {$IFDEF PROFILE}
  tm1: cardinal;
 {$ENDIF}
begin
  baseDir := SxSManifestDir()+'\';
  fnames := nil;
  parser := nil;

  progress := TProgressForm.Create(nil);
  try
    progress.Show;

   {$IFDEF PROFILE}
    tm1 := GetTickCount();
   {$ENDIF}

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

   {$IFDEF PROFILE}
    tm1 := GetTickCount-tm1;
    MessageBox(0, PChar('Total time: '+IntToStr(tm1)), PChar('Import completed'), MB_OK);
   {$ENDIF}

    ADb.CommitTransaction;
  finally
    FreeAndNil(parser);
    FreeAndNil(fnames);
    FreeAndNil(progress);
  end;
end;

type
  TFlagSet = record
    Bits: array of cardinal;
    procedure SetSize(const Value: integer); inline;
    function GetItem(const Index: integer): boolean; inline;
    procedure SetItem(const Index: integer; const Value: boolean); inline;
    property Item[const Index: integer]: boolean read GetItem write SetItem; default;
  const
    bitno = sizeof(integer)*8;
  end;

procedure TFlagSet.SetSize(const Value: integer);
begin
  SetLength(Bits, (Value div bitno) + 1);
end;

function TFlagSet.GetItem(const Index: integer): boolean;
begin
  Result := (Bits[Index div bitno] shr ((Index mod bitno)-1)) and $01;
end;

procedure TFlagSet.SetItem(const Index: integer; const Value: boolean);
begin
  if Value then
    Bits[Index div bitno] := Bits[Index div bitno] or (1 shl ((Index mod bitno)-1))
  else
    Bits[Index div bitno] := Bits[Index div bitno] and not (1 shl ((Index mod bitno)-1));
end;


//Rescans available manifests and updates the database. If the manifest is known, it's assumed to
//be unchanged (they usually are).
procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
const bitno = sizeof(integer)*8;
var baseDir: string;
  fnames: TStringList;
  i, idx: integer;
  progress: TProgressForm;
  parser: TManifestParser;
  ass: TAssemblyList;
  ad: TAssemblyData;
  hash: TStringList;
  found: array of integer;
begin
  baseDir := SxSManifestDir()+'\';
  fnames := nil;
  parser := nil;

  hash := TStringList.Create;
  progress := TProgressForm.Create(nil);
  try
    progress.Show;

    hash.Sorted := true;

    progress.Start('Building assembly list');
    ass := TAssemblyList.Create;
    try
      ADb.Assemblies.GetAllAssemblies(ass);
      for ad in ass.Values do
        hash.Add(ad.manifestName);
    finally
      FreeAndNil(ass);
    end;

    SetLength(found, hash.Count div bitno + 1);
    for i := 0 to Length(found)-1 do
      found[i] := 0;

    //Составляем список файлов
    progress.Start('Building file list');
    fnames := FilesByMask(baseDir+'\*.manifest');

    ADb.BeginTransaction;
    parser := TManifestParser.Create(ADb);

    //Теперь загружаем содержимое.
    progress.Start('Reading manifests', fnames.Count-1);
    for i := 0 to fnames.Count-1 do begin
      idx := hash.IndexOf(ChangeFileExt(fnames[i], ''));
      if idx >= 0 then
        found[idx div bitno] := found[idx div bitno] or (1 shl ((idx mod bitno)-1))
      else
        parser.ImportManifest(baseDir+'\'+fnames[i]);
      progress.Step();
    end;

    progress.Start('Removing assemblies');
    for i := 0 to hash.Count-1 do begin

    end;

    ADb.CommitTransaction;
  finally
    FreeAndNil(parser);
    FreeAndNil(fnames);
    FreeAndNil(progress);
    FreeAndNil(hash);
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
