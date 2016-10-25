inherited RegistryBrowserForm: TRegistryBrowserForm
  Caption = 'Registry'
  ClientHeight = 393
  ClientWidth = 635
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 0
    Top = 256
    Width = 635
    Height = 13
    Align = alBottom
    Caption = 'Components which use this key:'
    ExplicitWidth = 154
  end
  object splValues: TSplitter [1]
    Left = 181
    Top = 0
    Height = 256
    Align = alRight
    ExplicitLeft = 496
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  inherited Tree: TVirtualStringTree
    Width = 181
    Height = 256
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    ExplicitWidth = 635
    ExplicitHeight = 256
    Columns = <
      item
        Position = 0
        Width = 175
        WideText = 'Name'
      end>
  end
  object lbComponents: TListBox
    Left = 0
    Top = 269
    Width = 635
    Height = 124
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
  end
  object vtValues: TVirtualStringTree
    Left = 184
    Top = 0
    Width = 451
    Height = 256
    Align = alRight
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 2
    OnFreeNode = vtValuesFreeNode
    OnGetText = vtValuesGetText
    OnGetImageIndexEx = vtValuesGetImageIndexEx
    OnGetNodeDataSize = vtValuesGetNodeDataSize
    OnInitNode = vtValuesInitNode
    Columns = <>
  end
end
