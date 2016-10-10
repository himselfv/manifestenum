unit AssemblyDetails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Generics.Collections,
  AssemblyDb;

type
  TAssemblyDetailsForm = class(TForm)
    pcDetails: TPageControl;
    tsGeneral: TTabSheet;
    tsFiles: TTabSheet;
    lbFiles: TListBox;
    tsRegistryKeys: TTabSheet;
    tsDependencies: TTabSheet;
    lbDependencies: TListBox;
    tsDependents: TTabSheet;
    tsCategories: TTabSheet;
    tsAdditionalGear: TTabSheet;
    lbRegistryKeys: TListBox;
    lbDependents: TListBox;
  protected
    FDb: TAssemblyDb;
    FAssemblyId: TAssemblyId;
    procedure SetAssemblyId(const AValue: TAssemblyId);
    procedure LoadAssemblyData;
    procedure LoadFiles;
    procedure LoadRegistryKeys;
    procedure LoadDependencies;
    procedure LoadDependents;
  public
    procedure Clear;
    procedure Reload;
    property Db: TAssemblyDb read FDb write FDb;
    property AssemblyId: TAssemblyId read FAssemblyId write SetAssemblyId;
  end;

var
  AssemblyDetailsForm: TAssemblyDetailsForm;

implementation

{$R *.dfm}

procedure TAssemblyDetailsForm.SetAssemblyId(const AValue: TAssemblyId);
begin
  if AValue <> FAssemblyId then begin
    FAssemblyId := AValue;
    Reload;
  end;
end;

procedure TAssemblyDetailsForm.Clear;
begin
  lbFiles.Clear;
  lbRegistryKeys.Clear;
  lbDependencies.Clear;
  lbDependents.Clear;
end;

procedure TAssemblyDetailsForm.Reload;
begin
  Clear;
  if FAssemblyId <= 0 then exit;

  LoadAssemblyData;
  LoadFiles;
  LoadRegistryKeys;
  LoadDependencies;
  LoadDependents;
end;

procedure TAssemblyDetailsForm.LoadAssemblyData;
begin

end;

procedure TAssemblyDetailsForm.LoadFiles;
var files: TList<TFileEntryData>;
  i: integer;
begin
  files := FDb.GetAssemblyFiles(Self.FAssemblyId);
  try
    for i := 0 to files.Count-1 do
      lbFiles.Items.Add(files[i].name);
  finally
    FreeAndNil(files);
  end;
end;

procedure TAssemblyDetailsForm.LoadRegistryKeys;
var keys: TList<TRegistryValueData>;
  i: integer;
begin
  keys := TList<TRegistryValueData>.Create;
  try
    FDb.GetAssemblyKeys(Self.FAssemblyId, keys);
    for i := 0 to keys.Count-1 do
      lbRegistryKeys.Items.Add(FDb.GetRegistryKeyPath(keys[i].key)+'\'+keys[i].name+'='+keys[i].value);
  finally
    FreeAndNil(keys);
  end;
end;

procedure TAssemblyDetailsForm.LoadDependencies;
var list: TAssemblyList;
  key: TAssemblyId;
begin
  list := TAssemblyList.Create();
  try
    FDb.GetDependencies(Self.FAssemblyId, list);
    for key in list.Keys do
      lbDependencies.Items.Add(list[key].identity.ToString);
  finally
    FreeAndNil(list);
  end;
end;

procedure TAssemblyDetailsForm.LoadDependents;
var list: TAssemblyList;
  key: TAssemblyId;
begin
  list := TAssemblyList.Create();
  try
    FDb.GetDependents(Self.FAssemblyId, list);
    for key in list.Keys do
      lbDependents.Items.Add(list[key].identity.ToString);
  finally
    FreeAndNil(list);
  end;
end;

end.
