inherited AssemblyBrowserForm: TAssemblyBrowserForm
  Caption = 'Assemblies'
  ClientHeight = 349
  ClientWidth = 512
  ExplicitWidth = 528
  ExplicitHeight = 388
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Top = 21
    Width = 512
    Height = 328
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect]
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnPaintText = TreePaintText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    ExplicitTop = 21
    ExplicitWidth = 512
    ExplicitHeight = 328
    Columns = <
      item
        Position = 0
        Width = 506
        WideText = 'Name'
      end>
  end
  object pnlFilterSettings: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    object cbFilterByName: TCheckBox
      Left = 0
      Top = 0
      Width = 97
      Height = 21
      Align = alLeft
      Caption = 'Name'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbFilterByNameClick
    end
    object cbFilterByFiles: TCheckBox
      Left = 97
      Top = 0
      Width = 97
      Height = 21
      Align = alLeft
      Caption = 'Files'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbFilterByNameClick
    end
  end
end
