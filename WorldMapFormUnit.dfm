object WorldMapForm: TWorldMapForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'World Map'
  ClientHeight = 305
  ClientWidth = 529
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
  OnActivate = FormActivate
  OnClick = FormClick
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 120
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 529
    Height = 305
    Align = alClient
    ExplicitLeft = 128
    ExplicitTop = 104
    ExplicitWidth = 225
    ExplicitHeight = 121
  end
end
