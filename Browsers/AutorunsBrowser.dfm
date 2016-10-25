inherited AutorunsBrowserForm: TAutorunsBrowserForm
  Caption = 'Autoruns'
  PixelsPerInch = 96
  TextHeight = 13
  inherited splValues: TSplitter
    Align = alRight
    Visible = False
  end
  inherited Tree: TVirtualStringTree
    Align = alClient
  end
  inherited vtValues: TVirtualStringTree
    Align = alRight
    Visible = False
    ExplicitLeft = 288
  end
end
