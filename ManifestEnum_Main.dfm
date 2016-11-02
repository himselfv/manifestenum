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
        Caption = 'Add manifest file to DB...'
        OnClick = Loadmanifestfile1Click
      end
      object Expandfile1: TMenuItem
        Caption = 'Expand SxS file...'
        OnClick = Expandfile1Click
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
      object Assemblyname1: TMenuItem
        Caption = 'Name only'
        OnClick = Assemblyname1Click
      end
      object Assemblydisplayname1: TMenuItem
        Caption = 'Display name'
        OnClick = Assemblydisplayname1Click
      end
      object Assemblystrongname1: TMenuItem
        Caption = 'Strong name'
        OnClick = Assemblystrongname1Click
      end
      object Manifestname1: TMenuItem
        Caption = 'Manifest name'
        OnClick = Manifestname1Click
      end
    end
    object Export1: TMenuItem
      Caption = 'Export'
      object Savemanifest1: TMenuItem
        Caption = 'Manifest...'
        OnClick = Savemanifest1Click
      end
      object ExportPackageData1: TMenuItem
        Caption = 'Package data...'
        OnClick = ExportPackageData1Click
      end
    end
    object Getassemblysize1: TMenuItem
      Caption = 'Get assembly size'
      OnClick = Getassemblysize1Click
    end
    object Uninstallassembly1: TMenuItem
      Caption = 'Uninstall assembly'
      OnClick = Uninstallassembly1Click
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
end
