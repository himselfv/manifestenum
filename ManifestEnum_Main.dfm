object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 407
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbComponents: TListBox
    Left = 0
    Top = 21
    Width = 553
    Height = 193
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbComponentsClick
  end
  object edtQuickFilter: TEdit
    Left = 0
    Top = 0
    Width = 553
    Height = 21
    Align = alTop
    TabOrder = 1
    OnChange = edtQuickFilterChange
  end
  object pcDetails: TPageControl
    Left = 0
    Top = 214
    Width = 553
    Height = 193
    ActivePage = tsGeneral
    Align = alBottom
    TabOrder = 2
    object tsGeneral: TTabSheet
      Caption = 'General'
    end
    object tsFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      object lbAssemblyFiles: TListBox
        Left = 0
        Top = 0
        Width = 545
        Height = 165
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsDependencies: TTabSheet
      Caption = 'Dependencies'
      ImageIndex = 2
      object lbAssemblyDependencies: TListBox
        Left = 0
        Top = 0
        Width = 545
        Height = 165
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 32
    object F1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Components1: TMenuItem
      Caption = 'Components'
      object Rebuildassemblydatabase1: TMenuItem
        Caption = 'Rebuild assembly database'
        OnClick = Rebuildassemblydatabase1Click
      end
      object Loadmanifests1: TMenuItem
        Caption = 'Reload manifests'
        OnClick = Loadmanifests1Click
      end
    end
  end
end
