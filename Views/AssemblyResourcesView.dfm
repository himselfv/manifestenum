inherited AssemblyResourcesForm: TAssemblyResourcesForm
  Caption = 'Resources'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 480
  ExplicitHeight = 306
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Header.MainColumn = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = ResourceModule.SmallImages
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    Columns = <
      item
        Position = 0
        Width = 384
        WideText = 'Resource'
      end
      item
        Position = 1
        Width = 80
        WideText = 'Type'
      end>
  end
end
