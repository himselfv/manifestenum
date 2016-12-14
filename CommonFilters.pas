unit CommonFilters;
// Common filters which affect all views in the application

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  CheckLst, ExtCtrls, ComCtrls, Generics.Collections, AssemblyDb, AssemblyDb.Assemblies;

type
  TFiltersForm = class(TForm)
    pnlDialogButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    PageControl: TPageControl;
    tsVersions: TTabSheet;
    rbVersionsAll: TRadioButton;
    rbVersionsSelected: TRadioButton;
    lbVersions: TCheckListBox;
    tsArchitectures: TTabSheet;
    rbArchitecturesAll: TRadioButton;
    rbArchitecturesSelected: TRadioButton;
    lbArchitectures: TCheckListBox;
    tsLanguages: TTabSheet;
    rbLanguagesAll: TRadioButton;
    rbLanguagesSelected: TRadioButton;
    lbLanguages: TCheckListBox;
    tsPublicKeys: TTabSheet;
    rbPublicKeysAll: TRadioButton;
    rbPublicKeysSelected: TRadioButton;
    lbPublicKeys: TCheckListBox;
    btnReset: TButton;
    procedure FormShow(Sender: TObject);
    procedure lbVersionsClickCheck(Sender: TObject);
    procedure lbArchitecturesClickCheck(Sender: TObject);
    procedure lbLanguagesClickCheck(Sender: TObject);
    procedure lbPublicKeysClickCheck(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  protected
    FDb: TAssemblyDb;
    procedure ReloadOptions;
    function ClbGetChecked(lb: TCheckListBox): TArray<string>;
    procedure ClbSetChecked(lb: TCheckListBox; Values: TArray<string>);
    function ClbGetConfig(lb: TCheckListBox; rbAll: TRadioButton): TArray<string>;
    procedure ClbConfigure(lb: TCheckListBox; rbAll, rbSelected: TRadioButton; Values: TArray<string>);
  public
    property Db: TAssemblyDb read FDb write FDb;
  end;

var
  FiltersForm: TFiltersForm;

type
  TFilters = record
    ShowInstalledOnly: boolean;       //Hide assemblies which are not present in cache / not installed
    ShowDeploymentsOnly: boolean;     //Hide assemblies which are not deployments
    Versions: TArray<string>;
    ProcessorArchitectures: TArray<string>;
    PublicKeyTokens: TArray<string>;
    Languages: TArray<string>;
    function Test(const Id: TAssemblyIdentity): boolean; overload;
    function Test(const Assembly: TAssemblyData): boolean; overload;
  end;
var
  Filters: TFilters;
  OnFilterChanged: TList<TNotifyEvent>;

procedure FilterChanged(Sender: TObject);

implementation

{$R *.dfm}

procedure FilterChanged(Sender: TObject);
var event: TNotifyEvent;
begin
  for event in OnFilterChanged do
    event(Sender);
end;

type
  TStringArrayHelper = record helper for TArray<string>
  public
    function IndexOf(const Value: string): integer;
  end;

function TStringArrayHelper.IndexOf(const Value: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(Self)-1 do
    if Self[i] = Value then begin
      Result := i;
      break;
    end;
end;

function TFilters.Test(const Id: TAssemblyIdentity): boolean;
begin
  Result :=
         ((Self.Versions = nil) or (Self.Versions.IndexOf(Id.version) >= 0))
    and  ((Self.ProcessorArchitectures = nil) or (Self.ProcessorArchitectures.IndexOf(Id.processorArchitecture) >= 0))
    and  ((Self.Languages = nil) or (Self.Languages.IndexOf(Id.language) >= 0))
    and  ((Self.PublicKeyTokens = nil) or (Self.PublicKeyTokens.IndexOf(Id.publicKeyToken) >= 0));
end;

function TFilters.Test(const Assembly: TAssemblyData): boolean;
begin
  Result :=
         ((not Self.ShowInstalledOnly) or (Assembly.state = asInstalled))
    and  ((not Self.ShowDeploymentsOnly) or Assembly.isDeployment)
    and  Self.Test(Assembly.identity);
end;


procedure TFiltersForm.FormShow(Sender: TObject);
begin
  ReloadOptions;
  PageControl.ActivePageIndex := 0;
end;

procedure TFiltersForm.ReloadOptions;
var Items: TStringList;
begin
  Items := TStringList.Create;
  try
    Items.Sorted := true;

    Db.Assemblies.GetDistinctVersions(Items);
    lbVersions.Items.Assign(Items);
    ClbConfigure(lbVersions, rbVersionsAll, rbVersionsSelected, Filters.Versions);

    Items.Clear;
    Db.Assemblies.GetDistinctProcessorArchitectures(Items);
    lbArchitectures.Items.Assign(Items);
    ClbConfigure(lbArchitectures, rbArchitecturesAll, rbArchitecturesSelected, Filters.ProcessorArchitectures);

    Items.Clear;
    Db.Assemblies.GetDistinctLanguages(Items);
    lbLanguages.Items.Assign(Items);
    ClbConfigure(lbLanguages, rbLanguagesAll, rbLanguagesSelected, Filters.Languages);

    Items.Clear;
    Db.Assemblies.GetDistinctPublicKeyTokens(Items);
    lbPublicKeys.Items.Assign(Items);
    ClbConfigure(lbPublicKeys, rbPublicKeysAll, rbPublicKeysSelected, Filters.PublicKeyTokens);

  finally
    FreeAndNil(Items);
  end;
end;

procedure TFiltersForm.btnOkClick(Sender: TObject);
begin
  Filters.Versions := ClbGetConfig(lbVersions, rbVersionsAll);
  Filters.ProcessorArchitectures := ClbGetConfig(lbArchitectures, rbArchitecturesALl);
  Filters.Languages := ClbGetConfig(lbLanguages, rbLanguagesAll);
  Filters.PublicKeyTokens := ClbGetConfig(lbPublicKeys, rbPublicKeysAll);
  ModalResult := mrOk;
end;

procedure TFiltersForm.btnResetClick(Sender: TObject);
begin
  ClbConfigure(lbVersions, rbVersionsAll, rbVersionsSelected, nil);
  ClbConfigure(lbArchitectures, rbArchitecturesAll, rbArchitecturesSelected, nil);
  ClbConfigure(lbLanguages, rbLanguagesAll, rbLanguagesSelected, nil);
  ClbConfigure(lbPublicKeys, rbPublicKeysAll, rbPublicKeysSelected, nil);
end;

function TFiltersForm.ClbGetChecked(lb: TCheckListBox): TArray<string>;
var i: integer;
begin
  SetLength(Result, 0);
  for i := 0 to lb.Count-1 do
    if lb.Checked[i] then begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := lb.Items[i];
    end;
end;

procedure TFiltersForm.ClbSetChecked(lb: TCheckListBox; Values: TArray<string>);
var i, idx: integer;
begin
  lb.CheckAll(cbUnchecked);
  for i := 0 to Length(Values)-1 do begin
    idx := lb.Items.IndexOf(Values[i]);
    if idx>=0 then
      lb.Checked[idx] := true;
  end;
end;

function TFiltersForm.ClbGetConfig(lb: TCheckListBox; rbAll: TRadioButton): TArray<string>;
begin
  if rbAll.Checked then
    Result := nil
  else
    Result := ClbGetChecked(lb);
end;

procedure TFiltersForm.ClbConfigure(lb: TCheckListBox; rbAll, rbSelected: TRadioButton; Values: TArray<string>);
begin
  ClbSetChecked(lb, Values);
  if Values = nil then
    rbAll.Checked := true
  else
    rbSelected.Checked := true;
end;

procedure TFiltersForm.lbVersionsClickCheck(Sender: TObject);
begin
  rbVersionsSelected.Checked := true;
end;

procedure TFiltersForm.lbArchitecturesClickCheck(Sender: TObject);
begin
  rbArchitecturesSelected.Checked := true;
end;

procedure TFiltersForm.lbLanguagesClickCheck(Sender: TObject);
begin
  rbLanguagesSelected.Checked := true;
end;

procedure TFiltersForm.lbPublicKeysClickCheck(Sender: TObject);
begin
  rbPublicKeysSelected.Checked := true;
end;


initialization
  OnFilterChanged := TList<TNotifyEvent>.Create;

finalization
{$IFDEF DEBUG}
  FreeAndNil(OnFilterChanged);
{$ENDIF}

end.
