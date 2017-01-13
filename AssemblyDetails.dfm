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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcDetails: TPageControl
    Left = 0
    Top = 21
    Width = 461
    Height = 145
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 1
    OnChange = pcDetailsChange
    ExplicitTop = 0
    ExplicitHeight = 166
    object tsGeneral: TTabSheet
      Caption = 'General'
      ExplicitHeight = 138
    end
    object tsDependencies: TTabSheet
      Caption = 'Depends on'
      ImageIndex = 2
      ExplicitHeight = 138
      object lbDependencies: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 117
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitHeight = 138
      end
    end
    object tsDependents: TTabSheet
      Caption = 'Required by'
      ImageIndex = 3
      ExplicitHeight = 138
      object lbDependents: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 117
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitHeight = 138
      end
    end
    object tsCategories: TTabSheet
      Caption = 'Categories'
      ImageIndex = 5
      ExplicitHeight = 138
    end
  end
  object cbAssemblyName: TComboBox
    Left = 0
    Top = 0
    Width = 461
    Height = 21
    Align = alTop
    Style = csDropDownList
    TabOrder = 0
    OnChange = cbAssemblyNameChange
  end
end
