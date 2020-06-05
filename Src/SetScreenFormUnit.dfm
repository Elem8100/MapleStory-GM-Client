object SetScreenForm: TSetScreenForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Scale'
  ClientHeight = 602
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnActivate = FormActivate
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 10
    Top = 29
    Width = 177
    Height = 28
    Caption = '800X600 ->1024X768'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Tag = 1
    Left = 10
    Top = 63
    Width = 177
    Height = 28
    Caption = '800X600 ->1366X768'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Tag = 5
    Left = 10
    Top = 192
    Width = 177
    Height = 28
    Caption = '800X600 ->2560X1440'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button4: TButton
    Tag = 2
    Left = 10
    Top = 96
    Width = 177
    Height = 28
    Caption = '800X600 ->1600X900'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button5: TButton
    Tag = 3
    Left = 10
    Top = 128
    Width = 177
    Height = 28
    Caption = '800X600 ->1600X1200'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button6: TButton
    Tag = 4
    Left = 10
    Top = 160
    Width = 177
    Height = 28
    Caption = '800X600 ->1920X1080'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button7: TButton
    Tag = 12
    Left = 10
    Top = 420
    Width = 177
    Height = 28
    Caption = '1366X768 ->1600X900'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button8: TButton
    Tag = 13
    Left = 10
    Top = 452
    Width = 177
    Height = 28
    Caption = '1366X768->1920X1080'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button9: TButton
    Tag = 14
    Left = 10
    Top = 485
    Width = 177
    Height = 28
    Caption = '1366X768 ->2560X1440'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button10: TButton
    Tag = 15
    Left = 10
    Top = 518
    Width = 177
    Height = 28
    Caption = '1600X900 ->1920X1080'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = Button1Click
  end
  object Button11: TButton
    Tag = 9
    Left = 10
    Top = 325
    Width = 177
    Height = 28
    Caption = '1280X720 ->1600X900'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = Button1Click
  end
  object Button12: TButton
    Tag = 10
    Left = 10
    Top = 357
    Width = 177
    Height = 28
    Caption = '1280X720 ->1920X1080'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = Button1Click
  end
  object Button13: TButton
    Tag = 11
    Left = 10
    Top = 388
    Width = 177
    Height = 28
    Caption = '1280X720 ->2560X1440'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    OnClick = Button1Click
  end
  object Button14: TButton
    Tag = 6
    Left = 10
    Top = 225
    Width = 177
    Height = 28
    Caption = '1024X768 ->1600X900'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = Button1Click
  end
  object Button15: TButton
    Tag = 7
    Left = 10
    Top = 258
    Width = 177
    Height = 28
    Caption = '1024X768 ->1920X1080'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = Button1Click
  end
  object Button16: TButton
    Tag = 8
    Left = 10
    Top = 292
    Width = 177
    Height = 28
    Caption = '1024X768 ->2560X1440'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 15
    OnClick = Button1Click
  end
  object Button17: TButton
    Tag = 16
    Left = 10
    Top = 550
    Width = 177
    Height = 28
    Caption = '1600X900 ->2560X1440'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 16
    OnClick = Button1Click
  end
  object ScanlineCheckBox: TCheckBox
    Left = 56
    Top = 4
    Width = 89
    Height = 25
    Caption = 'Scanline'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 17
  end
end
