object AssemblyDetailsForm: TAssemblyDetailsForm
  Left = 0
  Top = 0
  Caption = 'Assembly details'
  ClientHeight = 166
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pcDetails: TPageControl
    Left = 0
    Top = 0
    Width = 461
    Height = 166
    ActivePage = tsCategories
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'General'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tsDependencies: TTabSheet
      Caption = 'Depends on'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbDependencies: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsDependents: TTabSheet
      Caption = 'Required by'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbDependents: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsCategories: TTabSheet
      Caption = 'Categories'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
