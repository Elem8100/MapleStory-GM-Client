// ----------------------------------------------------------------------------
// ACtrlLabels.pas             Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomALabel and TALabel.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomALabel e TALabel.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlLabels;

interface

uses
  pxt.types, Classes, Controls, SysUtils, Windows,
  // Aspryre units
  // Asphyre GUI Engine
  AControls, ACtrlForms, ACtrlTypes;

type
  TCustomALabel = class(TAControl)
  private
    FCanMoveHandle: Boolean;
    FFocusControl: string;
    FPLine: Boolean;
    FTransparent: Boolean;
    FFontColor: TColorPair;
    procedure SetCanMoveHandle(Value: Boolean); virtual;
    procedure SetFocusControl(Value: string); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure Paint(DC: HDC); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CanMoveHandle: Boolean read FCanMoveHandle write SetCanMoveHandle;
    property FocusControl: string read FFocusControl write SetFocusControl;
    property ParagraphLine: Boolean read FPLine write FPLine;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property FontColor: TColorPair read FFontColor write FFontColor;
  end;

  TALabel = class(TCustomALabel)
  published
    property CanMoveHandle;
    property FocusControl;
    property ParagraphLine;
    property Transparent;
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
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property Text;
    property Top;
    property Visible;
    property Width;
  end;

  TALabelClass = class of TALabel;

implementation

uses
  PXT.Graphics, Global;

var
  XOffSet, YOffSet: Integer;

  { TCustomALabel }

procedure TCustomALabel.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomALabel then
    with TCustomALabel(Dest) do
    begin
      CanMoveHandle := Self.CanMoveHandle;

      FocusControl := Self.FocusControl;
      ParagraphLine := Self.ParagraphLine;
      Transparent := Self.Transparent;
    end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomALabel.Click;
var
  Control: TAControl;
begin
  Control := Self.Handle.FindChildControl(FFocusControl, True);
  if Control <> nil then
    if Control is TWControl then
      TWControl(Control).SetFocus;

  inherited Click;
end;

constructor TCustomALabel.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    begin
      while AOwner.FindComponent('Label' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'Label' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 80;
  Height := 26;

  BorderColor := $80FFFFFF;
  BorderWidth := 0;
  //Color.SetFillColor($FFA6CAF0, $FFA6CAF0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  //FontColor.SetFontColor(clWhite2);
  Margin := 2;
  Text := Name;
  Visible := True;

  // Fields
  FCanMoveHandle := True;
  FPLine := False;
  FTransparent := True;

  ControlState := ControlState - [csCreating];
  var FontSettings: TFontSettings;
  if ISKMS then
    FontSettings := TFontSettings.Create('Tahoma', 11, TFontWeight.Normal)
  else
    FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);

  FontSettings.Effect.BorderType := TFontBorder.None;
  FontSettings.Weight := TFontWeight.Light;
  GameFont.FontSettings := FontSettings;
end;

destructor TCustomALabel.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomALabel.MouseDown;
begin
  // Start move the form Handle
  if (FCanMoveHandle) and (Handle is TAForm) then
  begin
    if (Button = mbLeft) and (TAForm(Handle).CanMove) then
    begin
      XOffSet := X - TAForm(Handle).Left;
      YOffSet := Y - TAForm(Handle).Top;
      TAForm(Handle).IsMoving := True;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomALabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Move the form Handle
  if (FCanMoveHandle) and (Handle is TAForm) then
  begin
    if TAForm(Handle).IsMoving = True then
    begin
      TAForm(Handle).Left := X - XOffSet;
      TAForm(Handle).Top := Y - YOffSet;
    end;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomALabel.MouseUp;
begin
  // Stop move the form Handle
  if (FCanMoveHandle) and (Handle is TAForm) then
  begin
    if (Button = mbLeft) and (TAForm(Handle).IsMoving) then
      TAForm(Handle).IsMoving := False;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomALabel.Paint(DC: HDC);
var
  X, Y: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;
  var FontSettings: TFontSettings;
  if ISKMS then
    FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
  else
    FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);

  FontSettings.Effect.BorderType := TFontBorder.None;
  FontSettings.Effect.BorderOpacity := 1;
  FontSettings.Weight := TFontWeight.Thin;
  GameFont.FontSettings := FontSettings;
  GameFont.Draw(Point2f(X, Y), Text, FontColor);

end;

procedure TCustomALabel.SetCanMoveHandle(Value: Boolean);
begin
  if FCanMoveHandle <> Value then
    FCanMoveHandle := Value;
end;

procedure TCustomALabel.SetFocusControl(Value: string);
begin
  if FFocusControl <> Value then
    FFocusControl := Value;
end;

procedure TCustomALabel.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
    FTransparent := Value;
end;

initialization
  RegisterClasses([TCustomALabel, TALabel]);

finalization
  UnRegisterClasses([TCustomALabel, TALabel]);

end.

