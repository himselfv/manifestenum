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
  PixelsPerInch = 96
  TextHeight = 13
  object pcDetails: TPageControl
    Left = 0
    Top = 0
    Width = 461
    Height = 166
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    ExplicitTop = 107
    ExplicitWidth = 545
    ExplicitHeight = 193
    object tsGeneral: TTabSheet
      Caption = 'General'
      ExplicitWidth = 537
      ExplicitHeight = 165
    end
    object tsFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      ExplicitWidth = 627
      ExplicitHeight = 272
      object lbFiles: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 627
        ExplicitHeight = 272
      end
    end
    object tsRegistryKeys: TTabSheet
      Caption = 'Registry keys'
      ImageIndex = 4
      ExplicitWidth = 627
      ExplicitHeight = 272
      object lbRegistryKeys: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 627
        ExplicitHeight = 272
      end
    end
    object tsDependencies: TTabSheet
      Caption = 'Depends on'
      ImageIndex = 2
      ExplicitWidth = 627
      ExplicitHeight = 272
      object lbDependencies: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 627
        ExplicitHeight = 272
      end
    end
    object tsDependents: TTabSheet
      Caption = 'Required by'
      ImageIndex = 3
      ExplicitWidth = 627
      ExplicitHeight = 272
      object lbDependents: TListBox
        Left = 0
        Top = 0
        Width = 453
        Height = 138
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 627
        ExplicitHeight = 272
      end
    end
    object tsCategories: TTabSheet
      Caption = 'Categories'
      ImageIndex = 5
      ExplicitWidth = 627
      ExplicitHeight = 272
    end
    object tsAdditionalGear: TTabSheet
      Caption = 'Additional'
      ImageIndex = 6
      ExplicitWidth = 627
      ExplicitHeight = 272
    end
  end
end
