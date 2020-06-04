// ----------------------------------------------------------------------------
// ACtrlSliders.pas             Modified: 02-10-2010              Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomASlider and TASlider.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomASlider e TASlider.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlSliders;

interface

uses
  Windows, SysUtils, Classes, Controls,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, PXT.Graphics, AsphyreTypes, Vectors2,
  // Asphyre GUI Engine
  AControls, ACtrlForms, ACtrlTypes;

type
  TCustomASlider = class(TWControl)
  private
    FMax: Word;
    FMin: Word;
    FPosition: Word;
    FButtonSlider: TUpDownButton;
    FTransparent: Boolean;
    procedure ButtonImageChange(Sender: TObject);
    procedure SetButtonSlider(Value: TUpDownButton);
    procedure SetMax(Value: Word);
    procedure SetMin(Value: Word);
    procedure SetPosition(Value: Word);
    procedure SetTransparent(Value: Boolean); virtual;
  protected
    function MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint(DC: HDC); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonSlider: TUpDownButton read FButtonSlider write SetButtonSlider;
    property Max: Word read FMax write SetMax;
    property Min: Word read FMin write SetMin;
    property Position: Word read FPosition write SetPosition;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TASlider = class(TCustomASlider)
  published
    property ButtonSlider;
    property Max;
    property Min;
    property Position;
    property Transparent;
    property BorderColor;
    property BorderWidth;
    //property Color;
    property Enabled;
    //property Font;
    //property FontColor;
    property Height;
   // property Image;
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
    property Top;
    property Visible;
    property Width;
  end;

implementation

uses
  PXT.Types;

var
  XPress: Integer;

  { TCustomASlider }

procedure TCustomASlider.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomASlider then
    with TCustomASlider(Dest) do
    begin
      Max := Self.Max;
      Min := Self.Min;
      Position := Self.Position;
      ButtonSlider := Self.ButtonSlider;
      Transparent := Self.Transparent;
    end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomASlider.ButtonImageChange(Sender: TObject);
begin
  if AEngine <> nil then
  begin
    (TUpDownButton(Sender)).FAImage := AEngine.ImageLib[(TUpDownButton(Sender).Image)];
    (TUpDownButton(Sender)).FAImageHover := AEngine.ImageLib[((TUpDownButton(Sender)).ImageHover)];
    (TUpDownButton(Sender)).FAImagePressed := AEngine.ImageLib[(TUpDownButton(Sender).ImagePressed)];
  end;
end;

constructor TCustomASlider.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    while AOwner.FindComponent('Slider' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'Slider' + IntToStr(Num);
  end;

  // Set Fields
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FTransparent := False;

  FButtonSlider := TUpDownButton.Create;
  FButtonSlider.OnImageChange := ButtonImageChange;
  FButtonSlider.IsEnabled := False;
  FButtonSlider.Width := 8;

  // Set Properties
  Left := 0;
  Top := 0;
  Height := 26;
  Width := 102;

  BorderWidth := 1;
  BorderColor := $80FFFFFF;

  //Color.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;
  Margin := 2;
  TabStop := True;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomASlider.Destroy;
begin
  FreeAndNil(FButtonSlider);

  inherited;
end;

procedure TCustomASlider.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RIGHT then
  begin
    Position := Position + FMax div 20;
  end;

  if Key = VK_LEFT then
  begin
    if (Position - FMax div 20) < 0 then
      Position := 0
    else
      Position := Position - FMax div 20;
  end;

  if Key = VK_HOME then
  begin
    Position := FMin;
  end;

  if Key = VK_END then
  begin
    Position := FMax;
  end;

  inherited;
end;

procedure TCustomASlider.MouseDown;
var
  VPos, VWidth: Integer;
  XIni, XEnd, YIni, YEnd: Integer;
begin
  VPos := Round((FPosition * (Width - (BorderWidth * 2) - (Margin * 2) - FButtonSlider.Width)) / FMax);
  XIni := ClientLeft + BorderWidth + Margin + VPos;
  XEnd := ClientLeft + BorderWidth + Margin + VPos + FButtonSlider.Width;
  YIni := ClientTop + BorderWidth + Margin;
  YEnd := ClientTop + Height - BorderWidth - Margin;

  if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
  begin
    FButtonSlider.IsPressed := True;
    XPress := X - VPos;
  end
  else
  begin
    FButtonSlider.IsPressed := False;

    // divide clientwidth in 8 parts
    VWidth := Width div 8;

    // Position is 0
    XIni := ClientLeft;
    XEnd := ClientLeft + BorderWidth + Margin + VWidth;
    if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
      Position := 0;

    // Position is in 1/4
    XIni := ClientLeft + BorderWidth + Margin + VWidth;
    XEnd := ClientLeft + BorderWidth + Margin + VWidth * 3;
    if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
      Position := FMax div 4;

    // Position is in 1/2
    XIni := ClientLeft + BorderWidth + Margin + VWidth * 3;
    XEnd := ClientLeft + VWidth * 5;
    if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
      Position := FMax div 2;

    // Position is in 3/4
    XIni := ClientLeft + VWidth * 5;
    XEnd := ClientLeft + VWidth * 7;
    if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
      Position := (FMax div 4) * 3;

    // Position is in 4/4
    XIni := ClientLeft + VWidth * 7;
    XEnd := ClientLeft + VWidth * 8;
    if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
      Position := FMax;

  end;

  inherited;
end;

procedure TCustomASlider.MouseLeave;
begin
  if FButtonSlider.IsHover then
    FButtonSlider.IsHover := False;
  if FButtonSlider.IsPressed then
    FButtonSlider.IsPressed := False;

  inherited;
end;

procedure TCustomASlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  VPos, VPosInv: Integer;
  XIni, XEnd, YIni, YEnd: Integer;
begin
  VPos := Round((FPosition * (Width - (BorderWidth * 2) - (Margin * 2) - FButtonSlider.Width)) / FMax);
  XIni := ClientLeft + BorderWidth + Margin + VPos;
  XEnd := ClientLeft + BorderWidth + Margin + VPos + FButtonSlider.Width;
  YIni := ClientTop + BorderWidth + Margin;
  YEnd := ClientTop + Height - BorderWidth - Margin;

  if (X >= XIni) and (X < XEnd) and (Y >= YIni) and (Y < YEnd) then
  begin
    FButtonSlider.IsHover := True;
  end
  else
  begin
    FButtonSlider.IsHover := False;
  end;

  if FButtonSlider.IsPressed then
  begin
    VPosInv := Round((X - XPress) * (FMax / (Width - (BorderWidth * 2) - (Margin * 2) - FButtonSlider.Width)));
    if VPosInv < 0 then
      Position := 0
    else
      Position := VPosInv;
  end;

  inherited;
end;

procedure TCustomASlider.MouseUp;
begin
  FButtonSlider.IsPressed := False;

  inherited;
end;

function TCustomASlider.MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Position := Position + FMax div 20;

  Result := True;
end;

function TCustomASlider.MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if (Position - FMax div 20) < 0 then
    Position := 0
  else
    Position := Position - FMax div 20;

  Result := True;
end;

procedure TCustomASlider.Paint(DC: HDC);
var
  X, Y: Integer;
  VPos: Integer;
  // Button Variables
  XIni, XEnd, YIni, YEnd: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

  // Draw Border
  if BorderWidth > 0 then
  begin
    AEngine.Canvas.FillRect(floatRect(X, Y, X + Width, Y + BorderWidth), BorderColor);
    AEngine.Canvas.FillRect(floatRect(X, Y + BorderWidth, X + BorderWidth, Y + Height - BorderWidth), BorderColor);
    AEngine.Canvas.FillRect(floatRect(X, Y + Height - BorderWidth, X + Width, Y + Height), BorderColor);
    AEngine.Canvas.FillRect(floatRect(X + Width - BorderWidth, Y + BorderWidth, X + Width, Y +
      Height - BorderWidth), BorderColor);
  end;

  // Draw Background
  if AImage.Initialized then
  begin

   // AEngine.Canvas.UseTexturePx(AImage, pxBounds4(0 + BorderWidth, 0 + BorderWidth, AImage.Width - (BorderWidth
    //  * 2), AImage.Height - (BorderWidth * 2)));
    var TexCoord := Quad(0 + BorderWidth, 0 + BorderWidth, AImage.Parameters.Width - (BorderWidth *
      2), AImage.Parameters.Height - (BorderWidth * 2));
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth, Y + BorderWidth, X + Width -
      BorderWidth, Y + Height - BorderWidth)), TexCoord,$FFFFFFFF);

   // AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth, Y +
     // Height - BorderWidth)), cAlpha4(ImageAlpha), deNormal);
  end
  else
  begin
    if not FTransparent then
    begin
      AEngine.Canvas.FillRect(floatRect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth, Y +
        Height - BorderWidth), ARGB(255,0,255,0));
    end;
    AEngine.Canvas.FrameRect(floatRect(X + BorderWidth + Margin + FButtonSlider.Width div 2, (Y + (Height
      div 2) - 1), X + Width - BorderWidth - Margin - FButtonSlider.Width div 2, (Y + (Height div 2)
      + 1)), $20000000,1);
  end;

  // draw slider
  VPos := Round((FPosition * (Width - (BorderWidth * 2) - (Margin * 2) - FButtonSlider.Width)) / FMax);

  XIni := X + BorderWidth + Margin + VPos;
  XEnd := X + BorderWidth + Margin + VPos + FButtonSlider.Width;
  YIni := Y + BorderWidth + Margin;
  YEnd := Y + Height - BorderWidth - Margin;

  if FButtonSlider.AImage.Initialized then
  begin
  //  AEngine.Canvas.UseImagePx(FButtonSlider.AImage, pxBounds4(0, 0, FButtonSlider.AImage.PatternSize.X,
   //   FButtonSlider.AImage.PatternSize.Y));
      var TexCoord := Quad( 0 ,0,
        AImage.Parameters.Width,
        AImage.Parameters.Height);
      AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(Xini,Yini,XEnd,YEnd)),TexCoord,$FFFFFFFF);
   // AEngine.Canvas.TexMap(pRect4(Rect(XIni, YIni, XEnd, YEnd)), cAlpha4(ImageAlpha), deNormal);
  end
  else
  begin
    if (FButtonSlider.IsHover) and not (FButtonSlider.IsPressed) then
    begin
      AEngine.Canvas.FillRect(floatRect(XIni, YIni, XEnd, YEnd), $FFFFFFFF);
    end
    else if FButtonSlider.IsPressed then
    begin
      AEngine.Canvas.FillRect(FloatRect(XIni, YIni, XEnd, YEnd), ARGB(255,255,0,0));
    end
    else
    begin
      AEngine.Canvas.FillRect(floatRect(XIni, YIni, XEnd, YEnd), cardinal(FButtonSlider.Color));
    end;
    AEngine.Canvas.FrameRect(floatRect(XIni, YIni, XEnd, YEnd), $60FFFFFF,1);
  end;

end;

procedure TCustomASlider.SetButtonSlider(Value: TUpDownButton);
begin
  FButtonSlider.Assign(Value);
end;

procedure TCustomASlider.SetMax(Value: Word);
begin
  if Value < 1 then
    Value := 1;

  FMax := Value;

  if FMax < FMin then
    FMin := FMax;
  if FPosition > FMax then
    FPosition := FMax;
end;

procedure TCustomASlider.SetMin(Value: Word);
begin
  if Value > FMax then
    Value := FMax;

  FMin := Value;

  if FPosition < FMin then
    FPosition := FMin;
end;

procedure TCustomASlider.SetPosition(Value: Word);
begin
  if Value < FMin then
    Value := FMin;

  if Value > FMax then
    Value := FMax;

  FPosition := Value;
end;

procedure TCustomASlider.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
end;

initialization
  RegisterClasses([TCustomASlider, TASlider]);

finalization
  UnRegisterClasses([TCustomASlider, TASlider]);

end.

