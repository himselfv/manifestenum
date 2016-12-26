inherited TaskBrowserForm: TTaskBrowserForm
  Caption = 'Tasks'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
    ExplicitHeight = 255
  end
end
