inherited FileBrowserForm: TFileBrowserForm
  Caption = 'Files'
  ExplicitWidth = 480
  ExplicitHeight = 306
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Height = 247
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
    ExplicitTop = 21
    ExplicitHeight = 174
    Columns = <
      item
        Position = 0
        Width = 458
        WideText = 'Name'
      end>
  end
  object Panel: TPanel
    Left = 0
    Top = 247
    Width = 464
    Height = 21
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 0
    object cbViewMode: TComboBox
      Left = 319
      Top = 0
      Width = 145
      Height = 21
      Align = alRight
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 0
      Text = 'Model PC'
      OnChange = cbViewModeChange
      Items.Strings = (
        'Non-expanded'
        'Model PC')
    end
  end
end
