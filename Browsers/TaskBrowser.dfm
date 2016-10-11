inherited TaskBrowserForm: TTaskBrowserForm
  Caption = 'Tasks'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblWhoAdded: TLabel [0]
    Left = 0
    Top = 255
    Width = 464
    Height = 13
    Align = alBottom
    ExplicitWidth = 3
  end
  inherited Tree: TVirtualStringTree
    Height = 255
    Images = ResourceModule.SmallImages
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnInitNode = TreeInitNode
  end
end
