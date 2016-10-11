inherited AssemblyTreeForm: TAssemblyTreeForm
  Caption = 'AssemblyTreeForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnInitNode = TreeInitNode
  end
end
