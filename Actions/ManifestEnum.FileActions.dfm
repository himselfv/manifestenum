object FileActions: TFileActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object FolderPopupMenu: TPopupMenu
    Left = 32
    Top = 16
    object miFolderCopy: TMenuItem
      Caption = 'Copy'
      object miFolderCopyName: TMenuItem
        Caption = 'Name'
        OnClick = miFolderCopyNameClick
      end
      object miFolderCopyPath: TMenuItem
        Caption = 'Path'
        OnClick = miFolderCopyPathClick
      end
    end
    object miFolderJumpTo: TMenuItem
      Caption = 'Jump to'
      object miFolderJumpToLocal: TMenuItem
        Caption = 'Local folder'
        OnClick = miFolderJumpToLocalClick
      end
    end
  end
  object FilePopupMenu: TPopupMenu
    Left = 112
    Top = 16
    object miFileCopy: TMenuItem
      Caption = 'Copy'
      object miFileCopyName: TMenuItem
        Caption = 'File name'
        OnClick = miFileCopyNameClick
      end
      object miFileCopyNameAndPath: TMenuItem
        Caption = 'Name and path'
        OnClick = miFileCopyNameAndPathClick
      end
    end
    object miFileJumpTo: TMenuItem
      Caption = 'Jump to'
      object miFileJumpToLocal: TMenuItem
        Caption = 'Local file'
        OnClick = miFileJumpToLocalClick
      end
      object miFileJumpToSxs: TMenuItem
        Caption = 'File in SxS store'
        OnClick = miFileJumpToSxsClick
      end
    end
  end
end
