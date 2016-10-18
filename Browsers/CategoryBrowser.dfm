object CategoryBrowserForm: TCategoryBrowserForm
  Left = 0
  Top = 0
  Caption = 'Categories'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 635
    Height = 300
    Align = alClient
    BorderWidth = 1
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = ResourceModule.SmallImages
    TabOrder = 0
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnHeaderClick = TreeHeaderClick
    OnInitNode = TreeInitNode
    Columns = <
      item
        Position = 0
        Width = 479
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 150
        WideText = 'Type'
      end>
  end
end
