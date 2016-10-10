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
    ExplicitLeft = 104
    ExplicitTop = 8
    ExplicitWidth = 289
    ExplicitHeight = 193
    object tsAssemblies: TTabSheet
      Caption = 'Assemblies'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object lbComponents: TListBox
        Left = 0
        Top = 44
        Width = 545
        Height = 142
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbComponentsClick
        ExplicitWidth = 553
        ExplicitHeight = 170
      end
      object pcDetails: TPageControl
        Left = 0
        Top = 186
        Width = 545
        Height = 193
        ActivePage = tsGeneral
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 214
        ExplicitWidth = 553
        object tsGeneral: TTabSheet
          Caption = 'General'
          ExplicitWidth = 545
        end
        object tsFiles: TTabSheet
          Caption = 'Files'
          ImageIndex = 1
          object lbAssemblyFiles: TListBox
            Left = 0
            Top = 0
            Width = 537
            Height = 165
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
          end
        end
        object tsRegistryKeys: TTabSheet
          Caption = 'Registry keys'
          ImageIndex = 4
          ExplicitWidth = 545
        end
        object tsDependencies: TTabSheet
          Caption = 'Depends on'
          ImageIndex = 2
          object lbAssemblyDependencies: TListBox
            Left = 0
            Top = 0
            Width = 537
            Height = 165
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
          end
        end
        object tsDependents: TTabSheet
          Caption = 'Required by'
          ImageIndex = 3
          ExplicitWidth = 545
        end
        object tsCategories: TTabSheet
          Caption = 'Categories'
          ImageIndex = 5
          ExplicitWidth = 545
        end
        object tsAdditionalGear: TTabSheet
          Caption = 'Additional'
          ImageIndex = 6
          ExplicitWidth = 545
        end
      end
      object pnlFilter: TPanel
        Left = 0
        Top = 21
        Width = 545
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitTop = 0
        ExplicitWidth = 553
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
          ExplicitWidth = 504
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
        TabOrder = 3
        Visible = False
        ExplicitWidth = 553
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
end
