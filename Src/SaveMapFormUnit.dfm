object SaveMapForm: TSaveMapForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20786#23384#22320#22294
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
    Left = 48
    Top = 32
    Width = 120
    Height = 18
    Caption = #32972#26223#19978#19979#20301#32622#35519#25972
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 48
    Top = 72
    Width = 75
    Height = 16
    Caption = #32972#26223#25844#25955#29575
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
      '50'
      '100'
      '150'
      '200')
  end
  object ComboBox2: TComboBox
    Left = 133
    Top = 68
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
    Caption = #20786#23384
    TabOrder = 2
    OnClick = Button1Click
  end
end
