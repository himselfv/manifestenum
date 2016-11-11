unit ManifestEnum.Log;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLogForm = class(TForm)
    memo: TMemo;
  public
    procedure Clear;
    procedure Log(const AMessage: string); overload;
    procedure Log(const AMessage: string; const AArgs: array of const); overload;
  end;

var
  LogForm: TLogForm;

implementation

{$R *.dfm}

procedure TLogForm.Clear;
begin
  memo.Clear;
end;

procedure TLogForm.Log(const AMessage: string);
begin
  memo.Lines.Add(AMessage);
end;

procedure TLogForm.Log(const AMessage: string; const AArgs: array of const);
begin
  self.Log(Format(AMessage, AArgs));
end;

end.
