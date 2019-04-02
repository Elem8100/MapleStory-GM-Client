object SaveMapForm: TSaveMapForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Save Map'
  ClientHeight = 281
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 48
    Top = 32
    Width = 99
    Height = 18
    Caption = 'Back Y Position'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 48
    Top = 65
    Width = 32
    Height = 18
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 48
    Top = 98
    Width = 68
    Height = 18
    Caption = 'Back Color'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 48
    Top = 131
    Width = 47
    Height = 18
    Caption = 'Format'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ComboBox1: TComboBox
    Left = 153
    Top = 31
    Width = 80
    Height = 22
    Style = csOwnerDrawFixed
    DropDownCount = 10
    ItemIndex = 0
    TabOrder = 0
    Text = '0'
    Items.Strings = (
      '0'
      '-50'
      '-100'
      '-150'
      '-200'
      '-250'
      '-300'
      '-350'
      '-400'
      '-450'
      '-500'
      '50'
      '100'
      '150'
      '200')
  end
  object ComboBox2: TComboBox
    Left = 85
    Top = 65
    Width = 60
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 0
    TabOrder = 1
    Text = '1'
    Items.Strings = (
      '1'
      '-1'
      '-1.5'
      '-2')
  end
  object ComboBox3: TComboBox
    Left = 125
    Top = 97
    Width = 70
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 0
    TabOrder = 2
    Text = 'Gray'
    Items.Strings = (
      'Gray'
      'White'
      'Black'
      'Blue')
  end
  object Button1: TButton
    Left = 74
    Top = 184
    Width = 121
    Height = 41
    Caption = 'Save'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object ComboBox4: TComboBox
    Left = 100
    Top = 130
    Width = 70
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 0
    TabOrder = 4
    Text = 'JPG'
    Items.Strings = (
      'JPG'
      'PNG'
      'BMP')
  end
end
