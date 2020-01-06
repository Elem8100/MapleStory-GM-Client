// ----------------------------------------------------------------------------
// ACtrlButtons.pas             Modified: 02-10-2010              Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAButton and TAbutton.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAButton e TAButton.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlButtons;

interface

uses
  Classes, Controls, SysUtils, Types, Windows,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, AsphyreImages, AsphyreTypes, Vectors2, Vectors2px,
  // Asphyre GUI Engine
  ZGameFonts, ZGameFontHelpers,
  AControls, ACtrlTypes, AbstractTextures,WZIMGFile;

type
  TCustomAButton = class(TAControl)
  private
    FAImageHover: TAsphyreLockableTexture;
    FAImagePressed: TAsphyreLockableTexture;
    FAImageDisabled: TAsphyreLockableTexture;
    FHAlign: THAlign;
    FVAlign: TVAlign;
    FColorHover: TFillColor;
    FColorPressed: TFillColor;
    FFontColorHover: TFontColor;
    FFontColorPressed: TFontColor;
    FHover: Boolean;
    FImageHover: TWZIMGEntry;
    FImagePressed: TWZIMGEntry;
    FImageDisabled: TWZIMGEntry;
    FPressed: Boolean;
    FShadowColor: Cardinal;
    FTransparent: Boolean;

    procedure SetColorHover(Value: TFillColor); virtual;
    procedure SetColorPressed(Value: TFillColor); virtual;
    procedure SetFontColorHover(Value: TFontColor); virtual;
    procedure SetFontColorPressed(Value: TFontColor); virtual;
    procedure SetImageHover(Value: TWZIMGEntry); virtual;
    procedure SetImagePressed(Value: TWZIMGEntry); virtual;
    procedure SetImageDisabled(Value: TWZIMGEntry); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(DC: HDC); override;
  public
    // ZEdit

    // ZEdit
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AImage: TAsphyreLockableTexture; override;
    function IsHover: Boolean;
    function IsPressed: Boolean;

    procedure SetAEngine(AEngine: TCustomEngine); override;
    property ColorHover: TFillColor read FColorHover write SetColorHover;
    property ColorPressed: TFillColor read FColorPressed write SetColorPressed;
    property FontColorHover: TFontColor read FFontColorHover write SetFontColorHover;
    property FontColorPressed: TFontColor read FFontColorPressed write SetFontColorPressed;
    property ImageHover: TWZIMGEntry read FImageHover write SetImageHover;
    property ImagePressed: TWZIMGEntry read FImagePressed write SetImagePressed;
    property ImageDisabled: TWZIMGEntry read FImageDisabled write SetImageDisabled;
    property TextHorizontalAlign: THAlign read FHAlign write FHAlign;
    property TextVerticalAlign: TVAlign read FVAlign write FVAlign;
    property Transparent: Boolean read FTransparent write SetTransparent;

  end;

  TAButton = class(TCustomAButton)
  published
    property ColorHover;
    property ColorPressed;
    property FontColorHover;
    property FontColorPressed;
    property ImageHover;
    property ImagePressed;
    property TextHorizontalAlign;
    property TextVerticalAlign;
    property Transparent;

    property BorderColor;
    property BorderWidth;
    property Color;
    property Enabled;
    property Font;
    property FontColor;
    property Height;
    // property Image;
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

  TAButtonClass = class of TAButton;

implementation

{ TCustomAButton }

function TCustomAButton.AImage: TAsphyreLockableTexture;
begin
  Result := inherited AImage;

  if FHover then
  begin
     Result := FAImageHover;
  end;

  if FPressed then
  begin
     Result := FAImagePressed;

  end;
  if Enabled = false then
    Result:=   FAImageDisabled;


end;

procedure TCustomAButton.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAButton then
    with TCustomAButton(Dest) do
    begin
      // FAImageHover - pointer
      // FAImagePressed - pointer
      TextHorizontalAlign := Self.TextHorizontalAlign;
      TextVerticalAlign := Self.TextVerticalAlign;
      ColorHover.Assign(Self.ColorHover);
      ColorPressed.Assign(Self.ColorPressed);
      FontColorHover.Assign(Self.FontColorHover);
      FontColorPressed.Assign(Self.FontColorPressed);
      // FHover - is dynamic field
      ImageHover := Self.ImageHover;
      ImagePressed := Self.ImagePressed;
      // FPressed - is dynamic field
      // FShadowColor - is not implemented
      Transparent := Self.Transparent;
    end;

  ControlState := ControlState - [csReadingState];
end;

constructor TCustomAButton.Create(AOwner: TComponent);
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
      while AOwner.FindComponent('Button' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'Button' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 80;
  Height := 26;

  BorderColor := clWhite1;
  BorderWidth := 0;
  Color.SetFillColor($FFA6CAF0, $FFA6CAF0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  FontColor.SetFontColor(clWhite2);
  Margin := 3;
  Text := Name;
  Visible := True;

  // Fields
  FHAlign := hCenter;
  FVAlign := vMiddle;
  FHover := False;
  FPressed := False;
  FTransparent := False;

  FColorHover := TFillColor.Create;
  FColorHover.SetFillColor($FFB6DAF0, $FFB6DAF0, $FF409AF0, $FF409AF0);

  FColorPressed := TFillColor.Create;
  FColorPressed.SetFillColor($FF4090F0, $FF4090F0, $FFA6CAF0, $FFA6CAF0);

  FFontColorHover := TFontColor.Create;
  FFontColorHover.SetFontColor(clWhite2);

  FFontColorPressed := TFontColor.Create;
  FFontColorPressed.SetFontColor($FFFFD040, $FFFFFFFF);

  FImageHover := nil;
  FImagePressed := nil;
 // FShadowColor := $40000000;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomAButton.Destroy;
begin
  FreeAndNil(FColorHover);
  FreeAndNil(FColorPressed);
  FreeAndNil(FFontColorHover);
  FreeAndNil(FFontColorPressed);

  inherited Destroy;
end;

function TCustomAButton.IsHover: Boolean;
begin
  Result := FHover;
end;

function TCustomAButton.IsPressed: Boolean;
begin
  Result := FPressed;
end;

procedure TCustomAButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPressed := True;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomAButton.MouseEnter;
begin
  FHover := True;
  inherited MouseEnter;
end;

procedure TCustomAButton.MouseLeave;
begin
  FHover := False;
  inherited MouseLeave;
end;

procedure TCustomAButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomAButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPressed := False;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomAButton.Paint(DC: HDC);
var
  X, Y: Integer;
  AColor: TColor4;
  AFontColor: TColor2;

begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;
  AColor := cColor4(Color);
  AFontColor := cColor2(FontColor);

  if FHover then
  begin
    AColor := cColor4(ColorHover);
    AFontColor := cColor2(FontColorHover);
  end;

  if FPressed then
  begin
    X := ClientLeft + 1;
    Y := ClientTop + 1;
    AColor := cColor4(ColorPressed);
    AFontColor := cColor2(FontColorPressed);
  end;

  // Draw Border
  if BorderWidth > 0 then
  begin
    AEngine.Canvas.FillRect(Rect(X, Y, X + Width, Y + BorderWidth), BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X, Y + BorderWidth, X + BorderWidth, Y + Height - BorderWidth), BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X, Y + Height - BorderWidth, X + Width, Y + Height), BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X + Width - BorderWidth, Y + BorderWidth, X + Width, Y + Height - BorderWidth),
      BorderColor, deNormal);
  end;

  // Draw Background
  if not FTransparent then
  begin
    if AImage <> nil then
    begin
      AEngine.Canvas.UseTexturePx(AImage, pxBounds4(0 + BorderWidth, 0 + BorderWidth, AImage.Width - (BorderWidth * 2),
        AImage.Height - (BorderWidth * 2)));
      AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth,
        Y + Height - BorderWidth)), cAlpha4(ImageAlpha), deNormal);
    end
    else
    begin
      AEngine.Canvas.FillRect(Rect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth, Y + Height - BorderWidth),
        AColor, deNormal);
    end;
  end;

  // Draw Text
  {
    if AFont <> nil then
    begin
    if Text <> '' then
    AFont.TextRectEx(Point2(X + BorderWidth + Margin,
    Y + BorderWidth + Margin+1),
    Point2(Width - (BorderWidth * 2) - (Margin * 2),
    Height - (BorderWidth * 2) - (Margin * 2)), Text,
    AFontColor, 1.0, FHAlign, FVAlign, False);
    end;
  }

  // Draw Text New
  if FZFont <> nil then
  begin
    FZFont.Color := cColor4(AFontColor[0], AFontColor[0], AFontColor[1], AFontColor[1]);
    if Text <> '' then
      FZFont.TextOutRect(DC, Point2(X + BorderWidth + Margin, Y + BorderWidth + Margin + 1),
        Point2(Width - (BorderWidth * 2) - (Margin * 2), Height - (BorderWidth * 2) - (Margin * 2)), Text, 0, 0, True,
        GetZHAlign(FHAlign), GetZVAlign(FVAlign));
  end; { else
    if AFont <> nil then
    begin
    if Text <> '' then
    AFont.TextRectEx(Point2(X + BorderWidth + Margin,
    Y + BorderWidth + Margin+1),
    Point2(Width - (BorderWidth * 2) - (Margin * 2),
    Height - (BorderWidth * 2) - (Margin * 2)), Text,
    AFontColor, 1.0, FHAlign, FVAlign, False);
    end;
  }
  // Draw Shadow
  AEngine.Canvas.FillRect(Rect(X + Width, Y + 1, X + Width + 1, Y + Height), FShadowColor, deShadow);
  AEngine.Canvas.FillRect(Rect(X + 1, Y + Height, X + Width + 1, Y + Height + 1), FShadowColor, deShadow);
end;

procedure TCustomAButton.SetAEngine(AEngine: TCustomEngine);
begin
  inherited;

  if AEngine <> nil then
  begin
    // FAImageHover := AEngine.ImageLib[WideString(FImageHover)];
    // FAImagePressed := AEngine.ImageLib[WideString(FImageHover)];
  end;
end;

procedure TCustomAButton.SetColorHover(Value: TFillColor);
begin
  FColorHover.Assign(Value);
end;

procedure TCustomAButton.SetColorPressed(Value: TFillColor);
begin
  FColorPressed.Assign(Value);
end;

procedure TCustomAButton.SetFontColorHover(Value: TFontColor);
begin
  FFontColorHover.Assign(Value);
end;

procedure TCustomAButton.SetFontColorPressed(Value: TFontColor);
begin
  FFontColorPressed.Assign(Value);
end;

procedure TCustomAButton.SetImageHover(Value: TWZIMGEntry);
begin
  FImageHover := Value;

  if AEngine <> nil then
  begin
    if not AEngine.ImageLib.ContainsKey(Value) then
      FAImageHover := nil
    else
      FAImageHover := AEngine.ImageLib[Value];
  end;
end;

procedure TCustomAButton.SetImagePressed(Value: TWZIMGEntry);
begin
  FImagePressed := Value;

  if AEngine <> nil then
  begin
    if not AEngine.ImageLib.ContainsKey(Value) then
      FAImagePressed := nil
    else
      FAImagePressed := AEngine.ImageLib[Value];
  end;

end;

procedure TCustomAButton.SetImageDisabled(Value: TWZIMGEntry);
begin
  FImageDisabled := Value;

  if AEngine <> nil then
  begin
    if not AEngine.ImageLib.ContainsKey(Value) then
      FAImageDisabled := nil
    else
      FAImageDisabled := AEngine.ImageLib[Value];
  end;

end;


procedure TCustomAButton.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
    FTransparent := Value;
end;

initialization

RegisterClasses([TCustomAButton, TAButton]);

finalization

UnRegisterClasses([TCustomAButton, TAButton]);

end.
