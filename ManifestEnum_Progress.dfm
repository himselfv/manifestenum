object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  Margins.Left = 12
  Margins.Top = 12
  Margins.Right = 12
  Margins.Bottom = 12
  Caption = 'Action in progress'
  ClientHeight = 54
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblOperation: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 364
    Height = 13
    Align = alTop
    Caption = 'Please wait...'
    ExplicitWidth = 66
  end
  object pbProgress: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 364
    Height = 29
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 104
    ExplicitTop = 88
    ExplicitWidth = 150
    ExplicitHeight = 17
  end
end
