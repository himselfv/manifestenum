inherited RegistryBrowserForm: TRegistryBrowserForm
  Caption = 'Registry'
  ClientHeight = 579
  ClientWidth = 788
  ExplicitWidth = 804
  ExplicitHeight = 617
  PixelsPerInch = 96
  TextHeight = 13
  object splValues: TSplitter [0]
    Left = 285
    Top = 0
    Height = 579
    ExplicitLeft = 496
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  inherited Tree: TVirtualStringTree
    Width = 285
    Height = 579
    Align = alLeft
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetPopupMenu = TreeGetPopupMenu
    OnInitNode = TreeInitNode
    ExplicitWidth = 285
    ExplicitHeight = 442
    Columns = <
      item
        Position = 0
        Width = 29
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 128
        WideText = 'Type'
      end
      item
        Position = 2
        Width = 128
        WideText = 'Value'
      end>
  end
  object vtValues: TVirtualStringTree
    Left = 288
    Top = 0
    Width = 500
    Height = 579
    Align = alClient
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
    TabOrder = 1
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
    OnCompareNodes = vtValuesCompareNodes
    OnFocusChanged = vtValuesFocusChanged
    OnFreeNode = vtValuesFreeNode
    OnGetText = vtValuesGetText
    OnGetImageIndexEx = vtValuesGetImageIndexEx
    OnGetNodeDataSize = vtValuesGetNodeDataSize
    OnGetPopupMenu = vtValuesGetPopupMenu
    OnInitNode = vtValuesInitNode
    ExplicitHeight = 442
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
        Width = 180
        WideText = 'Value'
      end>
  end
end
