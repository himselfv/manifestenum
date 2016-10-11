inherited TaskBrowserForm: TTaskBrowserForm
  Caption = 'Tasks'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
  end
end
