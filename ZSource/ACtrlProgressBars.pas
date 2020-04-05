// ----------------------------------------------------------------------------
// ACtrlProgressBars.pas          Modified: 02-10-2010            Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAProgressBar and TAProgressBar.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAProgressBar e TAProgressBar.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlProgressBars;

interface

uses
  SysUtils, Classes, Controls,Windows,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, AsphyreImages, AsphyreTypes, Vectors2,
  ZGameFonts, ZGameFontHelpers,
  // Asphyre GUI Engine
  AControls, ACtrlForms, ACtrlTypes;

type
  TCustomAProgressBar = class(TAControl)
  private
    FCanMoveHandle: Boolean;
    FMax: Word;
    FMin: Word;
    FPosition: Word;
    FProgressColor: TFillColor;
    FShowPercentage: Boolean;
    procedure SetCanMoveHandle(Value: Boolean); virtual;
    procedure SetMax(Value: Word);
    procedure SetMin(Value: Word);
    procedure SetPosition(Value: Word);
    procedure SetProgressColor(Value: TFillColor);
    procedure SetShowPercentage(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint(DC: HDC); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CanMoveHandle: Boolean read FCanMoveHandle write SetCanMoveHandle;
    property Max: Word read FMax write SetMax;
    property Min: Word read FMin write SetMin;
    property Position: Word read FPosition write SetPosition;
    property ProgressColor
      : TFillColor read FProgressColor write SetProgressColor;
    property ShowPercentage
      : Boolean read FShowPercentage write SetShowPercentage;
  end;

  TAProgressBar = class(TCustomAProgressBar)
  published
    property CanMoveHandle;
    property Max;
    property Min;
    property Position;
    property ProgressColor;
    property ShowPercentage;

    property BorderColor;
    property BorderWidth;
    property Color;
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
    property Top;
    property Visible;
    property Width;
  end;

implementation
   uses PXT.Graphics,PXT.Types;
var
  XOffSet, YOffSet: Integer;

  { TCustomAProgressBar }

procedure TCustomAProgressBar.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAProgressBar then
    with TCustomAProgressBar(Dest) do
    begin
      CanMoveHandle := Self.CanMoveHandle;
      Max := Self.Max;
      Min := Self.Min;
      Position := Self.Position;
      ProgressColor := Self.ProgressColor;
      ShowPercentage := Self.ShowPercentage;
    end;

  ControlState := ControlState - [csReadingState];
end;

constructor TCustomAProgressBar.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    while AOwner.FindComponent('ProgressBar' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'ProgressBar' + IntToStr(Num);
  end;

  // Set Fields
  FCanMoveHandle := True;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FProgressColor := TFillColor.Create;
  FProgressColor.SetFillColor($FFFFD040, $FFFFD040, $FFFF8020, $FFFF8020);
  FShowPercentage := True;

  // Set Properties
  Left := 0;
  Top := 0;
  Height := 26;
  Width := 102;

  BorderWidth := 1;
  BorderColor := $80FFFFFF;

  Color.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;
  Margin := 1;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomAProgressBar.Destroy;
begin
  FreeAndNil(FProgressColor);

  inherited Destroy;
end;

procedure TCustomAProgressBar.MouseDown;
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

procedure TCustomAProgressBar.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TCustomAProgressBar.MouseUp;
begin
  // Stop move the form Handle
  if (FCanMoveHandle) and (Handle is TAForm) then
  begin
    if (Button = mbLeft) and (TAForm(Handle).IsMoving) then
      TAForm(Handle).IsMoving := False;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomAProgressBar.Paint(DC: HDC);
var
  X, Y: Integer;
  AText: string;
  L, W: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

  AText := format('%d%%', [Round(FPosition * (1 / (FMax)) * 100)]);

  // Draw Border
  if BorderWidth > 0 then
  begin
    AEngine.Canvas.FillRect(FloatRect(X, Y, X + Width, Y + BorderWidth),
      BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X, Y + BorderWidth, X + BorderWidth,
        Y + Height - BorderWidth), BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X, Y + Height - BorderWidth, X + Width,
        Y + Height), BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth, Y + BorderWidth,
        X + Width, Y + Height - BorderWidth), BorderColor);
  end;

  // Draw Background
  if AImage.Initialized then
  begin
  {
    AEngine.Canvas.UseTexturePx(AImage,
      pxBounds4(0 + BorderWidth, 0 + BorderWidth,
        AImage.Width - (BorderWidth * 2),
        AImage.Height - (BorderWidth * 2)));
    AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth, Y + BorderWidth,
          X + Width - BorderWidth, Y + Height - BorderWidth)),
      cAlpha4(ImageAlpha), deNormal);

      }

       var TexCoord := Quad(0 + BorderWidth, 0 + BorderWidth, AImage.Parameters.Width - (BorderWidth *
      2), AImage.Parameters.Height - (BorderWidth * 2));
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth, Y + BorderWidth, X + Width -
      BorderWidth, Y + Height - BorderWidth)), TexCoord,$FFFFFFFF);
  end
  else
  begin
    AEngine.Canvas.FillRect(FloatRect(X + BorderWidth, Y + BorderWidth,
        X + Width - BorderWidth, Y + Height - BorderWidth), Cardinal(Color)
      );
  end;

  // Draw progress
  L := X + BorderWidth + Margin;
  W := Width - BorderWidth - Margin;
  if Round(W * (FPosition * (1 / (FMax)))) > 0 then
  begin
    AEngine.Canvas.FillRect(FloatRect(L, Y + BorderWidth + Margin,
        X + Round(W * (FPosition * (1 / (FMax)))),
        Y + Height - BorderWidth - Margin), Cardinal(FProgressColor));
  end;
  // Draw Text
  {
  if AFont <> nil then
  begin
    // Draw Text
    if FShowPercentage = True then
      AFont.TextRectEx(Point2(X + BorderWidth + Margin,
          Y + BorderWidth + Margin+1),
        Point2(Width - (BorderWidth * 2) - (Margin * 2),
          Height - (BorderWidth * 2) - (Margin * 2)), AText,
        cColor2(FontColor), 1.0, hCenter, vMiddle, False);
  end;
  }

  // Draw Text New!
  if FZFont <> nil then
  begin
    // Draw Text
    if FShowPercentage = True then
    begin
    //  FZFont.Color   := cColor4(FontColor.Top,FontColor.Top,FontColor.Bottom,FontColor.Bottom);
      FZFont.TextOutRect(DC,
                         Point2(X + BorderWidth + Margin,Y + BorderWidth + Margin+1),
                         Point2(Width-(BorderWidth*2)-(Margin*2),Height-(BorderWidth*2)-(Margin*2)),
                         AText, 0, 0, False, GetZHAlign(hCenter), GetZVAlign(vMiddle));
    end;
  end;{ else
  if AFont <> nil then
  begin
    // Draw Text
    if FShowPercentage = True then
      AFont.TextRectEx(Point2(X + BorderWidth + Margin,
          Y + BorderWidth + Margin+1),
        Point2(Width - (BorderWidth * 2) - (Margin * 2),
          Height - (BorderWidth * 2) - (Margin * 2)), AText,
        cColor2(FontColor), 1.0, hCenter, vMiddle, False);
  end;
       }
end;

procedure TCustomAProgressBar.SetCanMoveHandle(Value: Boolean);
begin
  FCanMoveHandle := Value;
end;

procedure TCustomAProgressBar.SetMax(Value: Word);
begin
  if Value < 1 then
    Value := 1;

  FMax := Value;

  if FMax < FMin then
    FMin := FMax;
  if FPosition > FMax then
    FPosition := FMax;
end;

procedure TCustomAProgressBar.SetMin(Value: Word);
begin
  if Value > FMax then
    Value := FMax;

  FMin := Value;

  if FPosition < FMin then
    FPosition := FMin;
end;

procedure TCustomAProgressBar.SetPosition(Value: Word);
begin
  if Value < FMin then
    Value := FMin;

  if Value > FMax then
    Value := FMax;

  FPosition := Value;
end;

procedure TCustomAProgressBar.SetProgressColor(Value: TFillColor);
begin
  FProgressColor.Assign(Value);
end;

procedure TCustomAProgressBar.SetShowPercentage(Value: Boolean);
begin
  FShowPercentage := Value;
end;

initialization

RegisterClasses([TCustomAProgressBar, TAProgressBar]);

finalization

UnRegisterClasses([TCustomAProgressBar, TAProgressBar]);

end.
