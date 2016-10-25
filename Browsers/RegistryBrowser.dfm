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
  inherited Tree: TVirtualStringTree
    Width = 635
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
        Width = 629
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
end
