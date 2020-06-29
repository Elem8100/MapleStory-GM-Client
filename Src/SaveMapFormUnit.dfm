object SaveMapForm: TSaveMapForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Save Map'
  ClientHeight = 212
  ClientWidth = 304
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
    Left = 60
    Top = 31
    Width = 111
    Height = 18
    Caption = 'Return Y Position'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 108
    Top = 72
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
  object ComboBox1: TComboBox
    Left = 180
    Top = 30
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
      '-550'
      '-600'
      '-650'
      '-700'
      '-750'
      '-800'
      '-850'
      '-900'
      '-950'
      '-1000'
      '-1050'
      '-1100'
      '-1150'
      '-1200'
      '-1250'
      '-1300'
      '-1350'
      '-1400'
      '-1450'
      '-1500'
      '-1550'
      '-1600'
      '50'
      '100'
      '150'
      '200')
  end
  object ComboBox2: TComboBox
    Left = 180
    Top = 71
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
  object Button1: TButton
    Left = 93
    Top = 128
    Width = 121
    Height = 41
    Caption = 'Save'
    TabOrder = 2
    OnClick = Button1Click
  end
end
