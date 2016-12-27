object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 558
  ClientWidth = 729
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 555
    Width = 729
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 377
    ExplicitWidth = 545
  end
  object pcMain: TPageControl
    Left = 0
    Top = 21
    Width = 729
    Height = 534
    Align = alClient
    TabOrder = 1
    OnChange = pcMainChange
  end
  object edtQuickFilter: TEdit
    Left = 0
    Top = 0
    Width = 729
    Height = 21
    Align = alTop
    TabOrder = 0
    TextHint = 'Quick Search'
    OnChange = edtQuickFilterChange
    OnKeyDown = edtQuickFilterKeyDown
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 72
    object F1: TMenuItem
      Caption = 'File'
      object miAssemblyDatabase: TMenuItem
        Caption = 'Assembly database'
        object miRefreshAssemblyDatabase: TMenuItem
          Caption = 'Refresh'
          OnClick = miRefreshAssemblyDatabaseClick
        end
        object pmRebuildAssemblyDatabase: TMenuItem
          Caption = 'Rebuild from scratch'
          OnClick = pmRebuildAssemblyDatabaseClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miUninstallByList: TMenuItem
        Caption = 'Uninstall by list...'
        OnClick = miUninstallByListClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object miFilters: TMenuItem
        Caption = 'Filters...'
        OnClick = miFiltersClick
      end
      object miShowInstalledOnly: TMenuItem
        AutoCheck = True
        Caption = 'Show only installed'
        Checked = True
        OnClick = miShowInstalledOnlyClick
      end
      object miShowDeploymentsOnly: TMenuItem
        AutoCheck = True
        Caption = 'Show only deployments'
        OnClick = miShowDeploymentsOnlyClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miForceUninstall: TMenuItem
        AutoCheck = True
        Caption = 'Force uninstall'
        Hint = 'Automatically turn assemblies into deployments if needed'
        OnClick = miForceUninstallClick
      end
    end
    object miService: TMenuItem
      Caption = 'Service'
      object miOpenComponentsKey: TMenuItem
        Caption = 'Open Components key...'
        OnClick = miOpenComponentsKeyClick
      end
      object miOpenDeploymentsKey: TMenuItem
        Caption = 'Open Deployments key...'
        OnClick = miOpenDeploymentsKeyClick
      end
      object miOpenSxSFolder: TMenuItem
        Caption = 'Open SxS folder...'
        OnClick = miOpenSxSFolderClick
      end
      object miDISMImageCleanup: TMenuItem
        Caption = 'DISM image cleanup...'
        OnClick = miDISMImageCleanupClick
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object miShowLog: TMenuItem
        Caption = 'Show log'
        OnClick = miShowLogClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Loadmanifestfile1: TMenuItem
        Caption = 'Add manifest file to DB...'
        OnClick = Loadmanifestfile1Click
      end
      object Expandfile1: TMenuItem
        Caption = 'Expand SxS file...'
        OnClick = Expandfile1Click
      end
      object Installassembly1: TMenuItem
        Caption = 'Install assembly...'
        OnClick = Installassembly1Click
      end
      object miVerifyHashes: TMenuItem
        Caption = 'Verify identity hashes'
        Hint = 'Verify manifest name hashes by recalculating them'
        OnClick = miVerifyHashesClick
      end
      object Queryassemblyscavener1: TMenuItem
        Caption = 'Query assembly scavener'
        OnClick = Queryassemblyscavener1Click
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
  object OpenAnyFileDialog: TOpenDialog
    DefaultExt = '*.*'
    Filter = '*.*'
    Title = 'Open file...'
    Left = 408
    Top = 88
  end
  object SaveAnyFileDialog: TSaveDialog
    DefaultExt = '*.*'
    Filter = '*.*'
    Left = 408
    Top = 136
  end
  object OpenListDialog: TOpenDialog
    DefaultExt = '*.txt'
    Filter = '*.txt'
    Title = 'Open assembly list...'
    Left = 104
    Top = 216
  end
end
