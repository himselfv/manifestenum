object BundleActions: TBundleActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object PopupMenu: TPopupMenu
    Left = 23
    Top = 8
    object miCopy: TMenuItem
      Caption = 'Copy'
      object miCopyName: TMenuItem
        Caption = 'Name'
        OnClick = miCopyNameClick
      end
      object miCopyFullPath: TMenuItem
        Caption = 'Full path'
        OnClick = miCopyFullPathClick
      end
    end
    object miEditDefinition: TMenuItem
      Caption = 'Edit definition...'
      OnClick = miEditDefinitionClick
    end
    object miJumpTo: TMenuItem
      Caption = 'Jump to folder'
      OnClick = miJumpToClick
    end
  end
end
