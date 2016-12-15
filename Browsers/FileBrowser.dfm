inherited FileBrowserForm: TFileBrowserForm
  Caption = 'Files'
  ExplicitWidth = 480
  ExplicitHeight = 306
  PixelsPerInch = 96
  TextHeight = 13
  object lblWhoAdded: TLabel [0]
    Left = 0
    Top = 255
    Width = 464
    Height = 13
    Align = alBottom
    ExplicitWidth = 3
  end
  inherited Tree: TVirtualStringTree
    Top = 21
    Height = 234
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetPopupMenu = TreeGetPopupMenu
    OnInitNode = TreeInitNode
    ExplicitTop = 17
    ExplicitHeight = 238
    Columns = <
      item
        Position = 0
        Width = 458
        WideText = 'Name'
      end>
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 17
    object cbViewMode: TComboBox
      Left = 319
      Top = 0
      Width = 145
      Height = 21
      Align = alRight
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Non-expanded'
      OnChange = cbViewModeChange
      Items.Strings = (
        'Non-expanded'
        'Model PC')
    end
  end
end
