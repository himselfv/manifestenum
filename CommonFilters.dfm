object FiltersForm: TFiltersForm
  Left = 0
  Top = 0
  Caption = 'Filters'
  ClientHeight = 394
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 3
  Padding.Top = 3
  Padding.Right = 3
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDialogButtons: TPanel
    Left = 3
    Top = 363
    Width = 386
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 3
    Padding.Top = 3
    Padding.Right = 3
    Padding.Bottom = 3
    TabOrder = 0
    object btnOk: TButton
      AlignWithMargins = True
      Left = 191
      Top = 3
      Width = 92
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 291
      Top = 3
      Width = 92
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnReset: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 92
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'Reset'
      TabOrder = 2
      OnClick = btnResetClick
    end
  end
  object PageControl: TPageControl
    Left = 3
    Top = 3
    Width = 386
    Height = 360
    ActivePage = tsVersions
    Align = alClient
    TabOrder = 1
    object tsVersions: TTabSheet
      Caption = 'Versions'
      DesignSize = (
        378
        332)
      object rbVersionsAll: TRadioButton
        Left = 10
        Top = 10
        Width = 331
        Height = 17
        Caption = 'All'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbVersionsSelected: TRadioButton
        Left = 10
        Top = 33
        Width = 331
        Height = 17
        Caption = 'Selected:'
        TabOrder = 1
      end
      object lbVersions: TCheckListBox
        Left = 20
        Top = 58
        Width = 346
        Height = 261
        OnClickCheck = lbVersionsClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object tsArchitectures: TTabSheet
      Caption = 'Platforms'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 373
      ExplicitHeight = 165
      DesignSize = (
        378
        332)
      object rbArchitecturesAll: TRadioButton
        Left = 10
        Top = 10
        Width = 331
        Height = 17
        Caption = 'All'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbArchitecturesSelected: TRadioButton
        Left = 10
        Top = 33
        Width = 331
        Height = 17
        Caption = 'Selected:'
        TabOrder = 1
      end
      object lbArchitectures: TCheckListBox
        Left = 20
        Top = 58
        Width = 346
        Height = 261
        OnClickCheck = lbArchitecturesClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object tsLanguages: TTabSheet
      Caption = 'Languages'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 373
      ExplicitHeight = 165
      DesignSize = (
        378
        332)
      object rbLanguagesAll: TRadioButton
        Left = 10
        Top = 10
        Width = 331
        Height = 17
        Caption = 'All'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbLanguagesSelected: TRadioButton
        Left = 10
        Top = 33
        Width = 331
        Height = 17
        Caption = 'Selected:'
        TabOrder = 1
      end
      object lbLanguages: TCheckListBox
        Left = 20
        Top = 58
        Width = 346
        Height = 261
        OnClickCheck = lbLanguagesClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object tsPublicKeys: TTabSheet
      Caption = 'Vendors'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 373
      ExplicitHeight = 166
      DesignSize = (
        378
        332)
      object rbPublicKeysAll: TRadioButton
        Left = 10
        Top = 10
        Width = 331
        Height = 17
        Caption = 'All'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbPublicKeysSelected: TRadioButton
        Left = 10
        Top = 33
        Width = 331
        Height = 17
        Caption = 'Selected:'
        TabOrder = 1
      end
      object lbPublicKeys: TCheckListBox
        Left = 20
        Top = 58
        Width = 346
        Height = 261
        OnClickCheck = lbPublicKeysClickCheck
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
end
