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
procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);

implementation
uses Windows, SysUtils, Classes, FilenameUtils, ManifestEnum_Progress, AssemblyDb.Assemblies,
 Generics.Collections, WinSxS, ComObj;

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

//Creates TStringList and populates it with file names by mask
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


type
 //Simple boolean bit set class
  TFlagSet = record
    Bits: array of cardinal;
    procedure Reset; inline;
    procedure SetSize(const Value: integer); inline;
    function GetItem(const Index: integer): boolean; inline;
    procedure SetItem(const Index: integer; const Value: boolean); inline;
    property Item[const Index: integer]: boolean read GetItem write SetItem; default;
  const
    bitno = sizeof(integer)*8;
  end;

procedure TFlagSet.Reset;
var i: integer;
begin
  for i := 0 to Length(Bits)-1 do
    Bits[i] := 0;
end;

procedure TFlagSet.SetSize(const Value: integer);
begin
  SetLength(Bits, (Value div bitno) + 1);
end;

function TFlagSet.GetItem(const Index: integer): boolean;
begin
  Result := Bits[Index div bitno] and ((Index mod bitno)-1)  <> 0;
end;

procedure TFlagSet.SetItem(const Index: integer; const Value: boolean);
begin
  if Value then
    Bits[Index div bitno] := Bits[Index div bitno] or (1 shl ((Index mod bitno)-1))
  else
    Bits[Index div bitno] := Bits[Index div bitno] and not (1 shl ((Index mod bitno)-1));
end;


{
Parses all manifests in WinSxS\Manifests and adds their data in the database. If the manifest
is known, it's assumed to be unchanged (they almost always are).
Displays progress form.
Designed to be fast when updating.
}
procedure RefreshAssemblyDatabase(ADb: TAssemblyDb);
var baseDir: string;
  fnames: TStringList;
  i, idx: integer;
  progress: TProgressForm;
  parser: TManifestParser;
  ass: TAssemblyList;
  ad: TAssemblyData;
  hash: TStringList;  //stores known manifest names
  found: TFlagSet;
  cache: IAssemblyCache;
  ai: ASSEMBLY_INFO;
  hr: HRESULT;
  tmp: string;
 {$IFDEF PROFILE}
  tm1: cardinal;
 {$ENDIF}
begin
  baseDir := SxSManifestDir()+'\';
  fnames := nil;
  parser := nil;
  hash := nil;
  ass := nil;

  SetLength(tmp, 500);

  progress := TProgressForm.Create(nil);
  try
    progress.Show;

   {$IFDEF PROFILE}
    tm1 := GetTickCount();
   {$ENDIF}

    //Build the lookup list of assemblies which we need not import
    hash := TStringList.Create;
    hash.Sorted := true;

    progress.Start('Building assembly list');
    ass := TAssemblyList.Create;
    try
      ADb.Assemblies.GetAllAssemblies(ass);
      for ad in ass.Values do begin
        hash.AddObject(ad.manifestName, TObject(ad.id));
      end;
    finally

    end;

    found.SetSize(hash.Count);
    found.Reset;


    //Build the list of manifests to import
    progress.Start('Building file list');
    fnames := FilesByMask(baseDir+'\*.manifest');

    ADb.BeginTransaction;
    try
      parser := TManifestParser.Create(ADb);

      //Parse the files
      progress.Start('Reading manifests', fnames.Count-1);
      for i := 0 to fnames.Count-1 do begin
        idx := hash.IndexOf(ChangeFileExt(fnames[i], ''));
        if idx >= 0 then
          found[idx] := true
        else
          parser.ImportManifest(baseDir+'\'+fnames[i]);
        progress.Step();
      end;

      OleCheck(CreateAssemblyCache(cache, 0));

      //Mark assemblies as missing and present.
      //We have to touch all assemblies since they can change both ways (go missing / apear after being missing)
      progress.Start('Updating assembly state');
      for i := 0 to hash.Count-1 do
        if found[i] then begin
          ad := ass[TAssemblyId(hash.Objects[i])];
          fillchar(ai, sizeof(ai), 0);
          ai.cbAssemblyInfo := sizeof(ai);
//          ai.pszCurrentAssemblyPathBuf := @tmp;
//          ai.cchBuf := Length(tmp);
          fillchar(tmp[1], Length(tmp)*SizeOf(WideChar), 0);

          hr := cache.QueryAssemblyInfo(0, PChar(ad.identity.ToStrongName), @ai);
          if hr <> HRESULT($80070490) then
            OleCheck(hr);
          if ai.dwAssemblyFlags and ASSEMBLYINFO_FLAG_INSTALLED <> 0 then
            ADb.Assemblies.SetState(TAssemblyId(hash.Objects[i]), asInstalled)
          else
            ADb.Assemblies.SetState(TAssemblyId(hash.Objects[i]), asPresent)
        end else
          ADb.Assemblies.SetState(TAssemblyId(hash.Objects[i]), asMissing);

     {$IFDEF PROFILE}
      tm1 := GetTickCount-tm1;
      MessageBox(0, PChar('Total time: '+IntToStr(tm1)), PChar('Import completed'), MB_OK);
     {$ENDIF}

      ADb.CommitTransaction;
    except
      ADb.AbortTransaction;
      raise;
    end;
  finally
    FreeAndNil(parser);
    FreeAndNil(fnames);
    FreeAndNil(progress);
    FreeAndNil(hash);
    FreeAndNil(ass);
  end;
end;

procedure RebuildAssemblyDatabase(ADb: TAssemblyDb; const AFilename: string);
begin
  ADb.Close;
  DeleteFile(AFilename);
  ADb.Open(AFilename);
  RefreshAssemblyDatabase(ADb);
end;

end.
