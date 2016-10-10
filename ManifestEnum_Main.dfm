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
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 553
    Height = 407
    ActivePage = tsAssemblies
    Align = alClient
    TabOrder = 0
    object tsAssemblies: TTabSheet
      Caption = 'Assemblies'
      object lbComponents: TListBox
        Left = 0
        Top = 44
        Width = 545
        Height = 335
        Align = alClient
        ItemHeight = 13
        PopupMenu = PopupMenu
        TabOrder = 0
        OnClick = lbComponentsClick
      end
      object pnlFilter: TPanel
        Left = 0
        Top = 21
        Width = 545
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object sbFilterSettings: TSpeedButton
          Left = 496
          Top = 0
          Width = 49
          Height = 23
          Align = alRight
          AllowAllUp = True
          GroupIndex = 10
          Caption = 'Filters'
          OnClick = sbFilterSettingsClick
          ExplicitLeft = 504
        end
        object edtQuickFilter: TEdit
          Left = 0
          Top = 0
          Width = 496
          Height = 23
          Align = alClient
          TabOrder = 0
          OnChange = edtQuickFilterChange
          ExplicitHeight = 21
        end
      end
      object pnlFilterSettings: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 21
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 2
        Visible = False
        object cbFilterByName: TCheckBox
          Left = 0
          Top = 0
          Width = 97
          Height = 21
          Align = alLeft
          Caption = 'Name'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbFilterByNameClick
        end
        object cbFilterByFiles: TCheckBox
          Left = 97
          Top = 0
          Width = 97
          Height = 21
          Align = alLeft
          Caption = 'Files'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbFilterByNameClick
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 72
    object F1: TMenuItem
      Caption = 'File'
      object Reload1: TMenuItem
        Caption = 'Reload'
        OnClick = Reload1Click
      end
      object pmRebuildAssemblyDatabase: TMenuItem
        Caption = 'Rebuild assembly database'
        OnClick = pmRebuildAssemblyDatabaseClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object Loadmanifestfile1: TMenuItem
        Caption = 'Load manifest file...'
        OnClick = Loadmanifestfile1Click
      end
    end
  end
  object OpenManifestDialog: TOpenDialog
    DefaultExt = '*.manifest'
    Filter = '*.manifest'
    Title = 'Open manifest file...'
    Left = 104
    Top = 80
  end
  object PopupMenu: TPopupMenu
    Left = 248
    Top = 80
    object Savemanifest1: TMenuItem
      Caption = 'Save manifest...'
      OnClick = Savemanifest1Click
    end
  end
  object SaveManifestDialog: TSaveDialog
    DefaultExt = '*.manifest'
    Filter = '*.manifest'
    Left = 104
    Top = 136
  end
end
