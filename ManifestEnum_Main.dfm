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
    Top = 0
    Width = 729
    Height = 555
    Align = alClient
    TabOrder = 0
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
      object miForceUninstall: TMenuItem
        AutoCheck = True
        Caption = 'Force uninstall'
        Hint = 'Automatically turn assemblies into deployments if needed'
        OnClick = SettingsChanged
      end
      object N4: TMenuItem
        Caption = '-'
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
    object Copy1: TMenuItem
      Caption = 'Copy'
      object miCopyAssemblyName: TMenuItem
        Caption = 'Name'
        OnClick = miCopyAssemblyNameClick
      end
      object miCopyAssemblyDisplayName: TMenuItem
        Caption = 'Display name'
        OnClick = miCopyAssemblyDisplayNameClick
      end
      object miCopyAssemblyStrongName: TMenuItem
        Caption = 'Strong name'
        OnClick = miCopyAssemblyStrongNameClick
      end
      object miCopyAssemblyManifestName: TMenuItem
        Caption = 'Manifest name'
        OnClick = miCopyAssemblyManifestNameClick
      end
      object miCopyComponentKeyform: TMenuItem
        Caption = 'Component keyform'
        OnClick = miCopyComponentKeyformClick
      end
      object miCopyDeploymentKeyform: TMenuItem
        Caption = 'Deployment keyform'
        OnClick = miCopyDeploymentKeyformClick
      end
      object miCopyHash: TMenuItem
        Caption = 'Hash'
        OnClick = miCopyHashClick
      end
      object miCopyVersionlessHash: TMenuItem
        Caption = 'Versionless hash'
        OnClick = miCopyVersionlessHashClick
      end
    end
    object Export1: TMenuItem
      Caption = 'Export'
      object miExportManifest: TMenuItem
        Caption = 'Manifest...'
        OnClick = miExportManifestClick
      end
      object miExportPackageData: TMenuItem
        Caption = 'Package data...'
        OnClick = miExportPackageDataClick
      end
    end
    object Open1: TMenuItem
      Caption = 'Open'
      object miJumpToComponentKey: TMenuItem
        Caption = 'Component key'
        Hint = 
          'Opens the associated Component subkey in the COMPONENTS hive, if' +
          ' it'#39's loaded'
        OnClick = miJumpToComponentKeyClick
      end
      object miJumpToDeploymentKey: TMenuItem
        Caption = 'Deployment key'
        Hint = 
          'Opens the associated Deployment subkey in the COMPONENTS hive, i' +
          'f it'#39's loaded and the component is a deployment'
        OnClick = miJumpToDeploymentKeyClick
      end
    end
    object miGetAssemblySize: TMenuItem
      Caption = 'Get assembly size'
      OnClick = miGetAssemblySizeClick
    end
    object miConvertIntoDeployment: TMenuItem
      Caption = 'Convert into deployment'
      OnClick = miConvertIntoDeploymentClick
    end
    object miUninstallAssembly: TMenuItem
      Caption = 'Uninstall assembly'
      Hint = 'Uninstalls assembly from the assembly cache'
      OnClick = miUninstallAssemblyClick
    end
  end
  object SaveManifestDialog: TSaveDialog
    DefaultExt = '*.manifest'
    Filter = '*.manifest'
    Left = 104
    Top = 136
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
