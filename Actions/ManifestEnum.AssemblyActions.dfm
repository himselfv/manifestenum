object AssemblyActions: TAssemblyActions
  OldCreateOrder = False
  Height = 150
  Width = 215
  object PopupMenu: TPopupMenu
    Left = 23
    Top = 8
    object miCopy: TMenuItem
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
    object miExport: TMenuItem
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
    object miJumpTo: TMenuItem
      Caption = 'Jump to'
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
    object miDebug: TMenuItem
      Caption = 'Debug'
      object miGetAssemblySize: TMenuItem
        Caption = 'Get assembly size'
        OnClick = miGetAssemblySizeClick
      end
      object miProbeInstallation: TMenuItem
        Caption = 'Probe installation'
        OnClick = miProbeInstallationClick
      end
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
    Left = 112
    Top = 8
  end
end
