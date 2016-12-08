unit ManifestEnum_Progress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TProgressForm = class(TForm)
    lblOperation: TLabel;
    pbProgress: TProgressBar;
  protected
    FLastRepaint: cardinal;
  public
    procedure Start(const AOperation: string = ''; AMax: int64 = 0);
    procedure Step();
    procedure Update(); reintroduce;
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.Start(const AOperation: string = ''; AMax: int64 = 0);
begin
  Self.lblOperation.Caption := AOperation;
  Self.pbProgress.Max := AMax;
  Update();
end;

procedure TProgressForm.Step();
begin
  if Self.pbProgress.Max > 0 then
    Self.pbProgress.Position := Self.pbProgress.Position + 1;
  Update();
end;

procedure TProgressForm.Update();
var tm: cardinal;
begin
  tm := GetTickCount;
  if FLastRepaint + 200 < tm then begin
    Application.ProcessMessages;
    FLastRepaint := tm;
  end;
end;

end.
