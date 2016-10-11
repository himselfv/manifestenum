inherited TaskBrowserForm: TTaskBrowserForm
  Caption = 'Tasks'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    OnCompareNodes = TreeCompareNodes
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnInitNode = TreeInitNode
  end
end
