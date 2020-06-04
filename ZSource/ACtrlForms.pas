// ----------------------------------------------------------------------------
// ACtrlForms.pas            Modified: 02-10-2010                 Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAForm and TAForm.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAForm e TAForm.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlForms;

interface

uses
  SysUtils, Classes, Controls, Windows,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, AsphyreImages, AsphyreTypes, Vectors2,
  // Asphyre GUI Engine
  ZGameFonts, ZGameFontHelpers, AControls, ACtrlTypes;

type
  TCustomAForm = class(TWControl)
  private
    FCanMove: Boolean;
    FIsMoving: Boolean;
    FHAlign: THAlign;
    FModal: Boolean;
    FVAlign: TVAlign;
    FPLine: Boolean;
    FShadowColor: Cardinal;
    FShadowWidth: Word;
    FShowShadow: Boolean;
    procedure SetCanMove(Value: Boolean);
    procedure SetShadowColor(Value: Cardinal);
    procedure SetShadowWidth(Value: Word);
    procedure SetShowShadow(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint(DC: HDC); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsModal: Boolean;
    procedure Close;
    procedure Show(Modal: Boolean = False);
    property CanMove: Boolean read FCanMove write SetCanMove;
    property IsMoving: Boolean read FIsMoving write FIsMoving;
    property ParagraphLine: Boolean read FPLine write FPLine;
    property ShadowColor: Cardinal read FShadowColor write SetShadowColor;
    property ShadowWidth: Word read FShadowWidth write SetShadowWidth;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow;
    property TextHorizontalAlign: THAlign read FHAlign write FHAlign;
    property TextVerticalAlign: TVAlign read FVAlign write FVAlign;
  end;

  TAForm = class(TCustomAForm)
  published
    property CanMove;
    property ParagraphLine;
    property ShadowColor;
    property ShadowWidth;
    property ShowShadow;
    property TextHorizontalAlign;
    property TextVerticalAlign;
    property BorderColor;
    property BorderWidth;
    //property Color;
    property Enabled;
    property Font;
    property FontColor;
    property Height;
    //property Image;
    property ImageAlpha;
    property Left;
    property Margin;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property TabOrder;
    property TabStop;
    property Text;
    property Top;
    property Visible;
    property Width;
  end;

  TAFormClass = class of TAForm;

implementation

uses
  PXT.Graphics, PXT.Types, PXT.Canvas, Global,UI.Utils;
// ----------------------------------------------------------------------------

var
  XOffSet, YOffSet: Integer;

  { TCustomAForm }

procedure TCustomAForm.MouseDown;
begin
  if (Button = mbLeft) and (FCanMove) then
  begin
    XOffSet := X - Left;
    YOffSet := Y - Top;
    FIsMoving := True;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomAForm.MouseEnter;
begin
  inherited;
end;

procedure TCustomAForm.MouseLeave;
begin
  inherited;
end;

procedure TCustomAForm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FIsMoving = True then
  begin
    Left := X - XOffSet;
    Top := Y - YOffSet;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomAForm.MouseUp;
begin
  if (Button = mbLeft) and (FIsMoving) then
    FIsMoving := False;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomAForm.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAForm then
    with TCustomAForm(Dest) do
    begin
      CanMove := Self.CanMove;
      IsMoving := Self.IsMoving;
      TextHorizontalAlign := Self.TextHorizontalAlign;
      TextVerticalAlign := Self.TextVerticalAlign;
      ParagraphLine := Self.ParagraphLine;
      ShadowColor := Self.ShadowColor;
      ShadowWidth := Self.ShadowWidth;
      ShowShadow := Self.ShowShadow;
    end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomAForm.Close;
begin
  FModal := False;
  Visible := False;
end;

constructor TCustomAForm.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    while AOwner.FindComponent('Form' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'Form' + IntToStr(Num);
  end;
  UIOwner:= Name;
  // fields
  FCanMove := True;
  FIsMoving := False;
  FModal := False;
  FPLine := False;
  FShadowColor := $40000000;
  FShadowWidth := 3;
  FShowShadow := True;
  FHAlign := hCenter;
  FVAlign := vMiddle;

  // properties
  Left := 0;
  Top := 0;
  Width := 320;
  Height := 240;
  BorderColor := clBlack1;
  BorderWidth := 1;
  //Color.SetFillColor($FFA6CAF0, $FFA6CAF0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;
  Margin := 3;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomAForm.Destroy;
begin
  inherited Destroy;
end;

function TCustomAForm.IsModal: Boolean;
begin
  Result := FModal;
end;

procedure TCustomAForm.Paint(DC: HDC);
var
  X, Y: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;
  if (AImage.Initialized) and (ImageEntry<> nil) then
     AEngine.Canvas.Draw(AImage,X,Y);
//  else
  //   AEngine.Canvas.FillRect(FloatRect(X, Y, Width,Height),cRGB1(255,0,0,100));
  inherited Paint(DC);
end;

procedure TCustomAForm.SetCanMove(Value: Boolean);
begin
  FCanMove := Value;
end;

procedure TCustomAForm.SetShadowColor(Value: Cardinal);
begin
  FShadowColor := Value;
end;

procedure TCustomAForm.SetShadowWidth(Value: Word);
begin
  FShadowWidth := Value;
end;

procedure TCustomAForm.SetShowShadow(Value: Boolean);
begin
  FShowShadow := Value;
end;

procedure TCustomAForm.Show(Modal: Boolean = False);
begin
  FModal := Modal;
  Visible := True;
  BringToFront;
  SetFocus;
  SelectFirst;
end;

initialization
  RegisterClasses([TCustomAForm, TAForm]);

finalization
  UnRegisterClasses([TCustomAForm, TAForm]);

end.

