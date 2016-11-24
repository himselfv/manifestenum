inherited ServiceBrowserForm: TServiceBrowserForm
  Caption = 'Services'
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
    Height = 255
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    ExplicitHeight = 255
    Columns = <
      item
        Position = 0
        Width = 458
        WideText = 'Name'
      end>
  end
end
