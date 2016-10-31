inherited AssemblyBrowserForm: TAssemblyBrowserForm
  Caption = 'Assemblies'
  ClientHeight = 349
  ClientWidth = 512
  ExplicitWidth = 528
  ExplicitHeight = 387
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Top = 44
    Width = 512
    Height = 305
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Header.SortColumn = 0
    Images = ResourceModule.SmallImages
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnPaintText = TreePaintText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    Columns = <
      item
        Position = 0
        Width = 506
        WideText = 'Name'
      end>
  end
  object pnlFilter: TPanel
    Left = 0
    Top = 21
    Width = 512
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -209
    ExplicitWidth = 721
    object sbFilterSettings: TSpeedButton
      Left = 463
      Top = 0
      Width = 49
      Height = 23
      Align = alRight
      AllowAllUp = True
      GroupIndex = 10
      Caption = 'Filters'
      OnClick = sbFilterSettingsClick
      ExplicitLeft = 504
    end
    object edtQuickFilter: TEdit
      Left = 0
      Top = 0
      Width = 463
      Height = 23
      Align = alClient
      TabOrder = 0
      OnChange = edtQuickFilterChange
      ExplicitWidth = 672
      ExplicitHeight = 21
    end
  end
  object pnlFilterSettings: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    ExplicitLeft = -209
    ExplicitWidth = 721
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
