inherited ShellExtensionBrowserForm: TShellExtensionBrowserForm
  Caption = 'Shell extensions'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Tree: TVirtualStringTree
    Columns = <
      item
        Position = 0
        Width = 23
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 128
        WideText = 'Type'
      end
      item
        Position = 2
        Width = 128
        WideText = 'Value'
      end>
  end
  inherited vtValues: TVirtualStringTree
    ExplicitLeft = 288
    Columns = <
      item
        Position = 0
        Width = 192
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 128
        WideText = 'Type'
      end
      item
        Position = 2
        Width = 174
        WideText = 'Value'
      end>
  end
end
