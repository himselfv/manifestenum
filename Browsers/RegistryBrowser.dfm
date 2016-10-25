inherited RegistryBrowserForm: TRegistryBrowserForm
  Caption = 'Registry'
  ClientHeight = 579
  ClientWidth = 788
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 804
  ExplicitHeight = 617
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 0
    Top = 442
    Width = 788
    Height = 13
    Align = alBottom
    Caption = 'Components which use this key:'
    ExplicitWidth = 154
  end
  object splValues: TSplitter [1]
    Left = 269
    Top = 0
    Height = 442
    Align = alRight
    ExplicitLeft = 496
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  inherited Tree: TVirtualStringTree
    Width = 269
    Height = 442
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
    ExplicitWidth = 269
    ExplicitHeight = 442
    Columns = <
      item
        Position = 0
        Width = 263
        WideText = 'Name'
      end>
  end
  object lbComponents: TListBox
    Left = 0
    Top = 455
    Width = 788
    Height = 124
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
  end
  object vtValues: TVirtualStringTree
    Left = 272
    Top = 0
    Width = 516
    Height = 442
    Align = alRight
    BorderWidth = 1
    Header.AutoSizeIndex = 2
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    TabOrder = 2
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    OnCompareNodes = vtValuesCompareNodes
    OnFreeNode = vtValuesFreeNode
    OnGetText = vtValuesGetText
    OnGetImageIndexEx = vtValuesGetImageIndexEx
    OnGetNodeDataSize = vtValuesGetNodeDataSize
    OnInitNode = vtValuesInitNode
    Columns = <
      item
        Position = 0
        Width = 192
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 128
        WideText = 'Type'
      end
      item
        Position = 2
        Width = 190
        WideText = 'Value'
      end>
  end
end
