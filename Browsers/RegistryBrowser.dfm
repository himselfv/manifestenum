object RegistryBrowserForm: TRegistryBrowserForm
  Left = 0
  Top = 0
  Caption = 'Registry'
  ClientHeight = 393
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 256
    Width = 635
    Height = 13
    Align = alBottom
    Caption = 'Components which use this key:'
    ExplicitWidth = 154
  end
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 635
    Height = 256
    Align = alClient
    BorderWidth = 1
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = ResourceModule.SmallImages
    TabOrder = 0
    OnExpanding = TreeExpanding
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnInitNode = TreeInitNode
    Columns = <>
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