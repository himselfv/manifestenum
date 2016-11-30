object RegistryActions: TRegistryActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object KeyPopupMenu: TPopupMenu
    OnPopup = KeyPopupMenuPopup
    Left = 32
    Top = 16
    object miKeyCopy: TMenuItem
      Caption = 'Copy'
      object miKeyCopyName: TMenuItem
        Caption = 'Name'
        OnClick = miKeyCopyNameClick
      end
      object miKeyCopyPath: TMenuItem
        Caption = 'Path'
        OnClick = miKeyCopyPathClick
      end
    end
    object miKeyJumpTo: TMenuItem
      Caption = 'Jump to'
      object miKeyJumpToLocalRegistry: TMenuItem
        Caption = 'Local registry'
        OnClick = miKeyJumpToLocalRegistryClick
      end
    end
  end
  object ValuePopupMenu: TPopupMenu
    OnPopup = ValuePopupMenuPopup
    Left = 120
    Top = 16
    object miValueCopy: TMenuItem
      Caption = 'Copy'
      object miValueCopyFullEntry: TMenuItem
        Caption = 'Full entry'
        OnClick = miValueCopyFullEntryClick
      end
      object miValueCopyNameValue: TMenuItem
        Caption = 'Name = Value'
        OnClick = miValueCopyNameValueClick
      end
      object miValueCopyNameType: TMenuItem
        Caption = 'Name and type'
        OnClick = miValueCopyNameTypeClick
      end
      object miValueCopyValue: TMenuItem
        Caption = 'Value'
        OnClick = miValueCopyValueClick
      end
    end
  end
end
