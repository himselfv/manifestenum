object RegistryActions: TRegistryActions
  OldCreateOrder = False
  Height = 150
  Width = 215
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 24
    Top = 16
    object miCopy: TMenuItem
      Caption = 'Copy'
      object miCopyKeyName: TMenuItem
        Caption = 'Key name'
      end
      object miCopyKeyPath: TMenuItem
        Caption = 'Key path'
      end
    end
    object miJumpTo: TMenuItem
      Caption = 'Jump to'
      object miJumpToLocalRegistry: TMenuItem
        Caption = 'Local registry'
      end
    end
  end
end
