// ----------------------------------------------------------------------------
// ACtrlCheckBoxes.pas         Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomACheckBox and TACheckBox.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomACheckBox e TACheckBox.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlCheckBoxes;

interface

uses
  Windows, SysUtils, Classes, Controls,
  // Asphyre units
   // Asphyre GUI engine
  AControls, ACtrlTypes, WZIMGFile,PXT.Graphics;

type
  TCustomACheckBox = class(TWControl)
  private
    FAImageChecked: TTexture;
    FAImageUnChecked: TTexture;
    FBoxBorderColor: Cardinal;
    FBoxCheckColor: Cardinal;
    FBoxColor: TFillColor;
    FBoxImageAlpha: Cardinal;
    FBoxImageChecked: TWZIMGEntry;
    FBoxImageUnChecked: TWZIMGEntry;
    FBoxSize: Byte;
    FChecked: Boolean;
    FReadOnly: Boolean;
    FTransparent: Boolean;
    procedure SetBoxBorderColor(Value: Cardinal);
    procedure SetBoxCheckColor(Value: Cardinal);
    procedure SetBoxColor(Value: TFillColor);
    procedure SetBoxImageAlpha(Value: Cardinal);
    procedure SetBoxImageChecked(Value: TWZIMGEntry);
    procedure SetBoxImageUnChecked(Value: TWZIMGEntry);
    procedure SetBoxSize(Value: Byte);
    procedure SetChecked(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTransparent(Value: Boolean); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint(DC: HDC); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AImageChecked: TTexture;
    function AImageUnChecked: TTexture;
    property BoxBorderColor: Cardinal read FBoxBorderColor write SetBoxBorderColor;
    property BoxCheckColor: Cardinal read FBoxCheckColor write SetBoxCheckColor;
    property BoxColor: TFillColor read FBoxColor write SetBoxColor;
    property BoxImageAlpha: Cardinal read FBoxImageAlpha write SetBoxImageAlpha;
    property BoxImageChecked: TWZIMGEntry read FBoxImageChecked write SetBoxImageChecked;
    property BoxImageUnChecked: TWZIMGEntry read FBoxImageUnChecked write SetBoxImageUnChecked;
    property BoxSize: Byte read FBoxSize write SetBoxSize;
    property Checked: Boolean read FChecked write SetChecked;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TACheckBox = class(TCustomACheckBox)
  published
    property BoxBorderColor;
    property BoxCheckColor;
    property BoxColor;
    property BoxImageAlpha;
    property BoxImageChecked;
    property BoxImageUnChecked;
    property BoxSize;
    property Checked;
    property ReadOnly;
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

  TACheckBoxClass = class of TACheckBox;

implementation

uses
   PXT.Types;
{ TCustomACheckBox }

function TCustomACheckBox.AImageChecked: TTexture;
begin
  Result := FAImageChecked;
end;

function TCustomACheckBox.AImageUnChecked: TTexture;
begin
  Result := FAImageUnChecked;
end;

procedure TCustomACheckBox.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomACheckBox then
    with TCustomACheckBox(Dest) do
    begin
      // FAImageChecked
      // FAImageUnChecked
      BoxBorderColor := Self.BoxBorderColor;
      BoxCheckColor := Self.BoxCheckColor;
      BoxColor := Self.BoxColor;
      BoxImageAlpha := Self.BoxImageAlpha;
      BoxImageChecked := Self.BoxImageChecked;
      BoxImageUnChecked := Self.BoxImageUnChecked;
      BoxSize := Self.BoxSize;
      Checked := Self.Checked;
      ReadOnly := Self.FReadOnly;
      Transparent := Self.Transparent;
    end;

  ControlState := ControlState - [csReadingState];
end;

constructor TCustomACheckBox.Create(AOwner: TComponent);
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
      while AOwner.FindComponent('CheckBox' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'CheckBox' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 120;
  Height := 24;
  BorderColor := $80FFFFFF;
  BorderWidth := 0;
  //Color.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);
  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;
  Margin := 3;
  TabStop := True;
  Text := Name;
  Visible := True;

  // Fields
  FBoxBorderColor := $FF666666;
  FBoxCheckColor := $FF4090F0;
  FBoxColor := TFillColor.Create;
  FBoxColor.SetFillColor($FFBBBBBB, $FFBBBBBB, $FFEEEEEE, $FFEEEEEE);
  FBoxImageAlpha := $FFFFFFFF;
  FBoxImageChecked := nil;
  FBoxImageUnChecked := nil;
  FBoxSize := 16;
  FChecked := True;
  FReadOnly := False;
  FTransparent := True;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomACheckBox.Destroy;
begin
  FreeAndNil(FBoxColor);
  inherited Destroy;
end;

procedure TCustomACheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not (FReadOnly) then
  begin
    if (Key = VK_SPACE) then
    begin
      FChecked := not FChecked;
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TCustomACheckBox.MouseUp;
var
  L, T, H, W: Integer;
begin
  T := ClientTop + BorderWidth + Margin;
  H := Height - (BorderWidth * 2) - (Margin * 2);
  T := T + (H div 2 - FBoxSize div 2);

  L := ClientLeft + BorderWidth + Margin;
  W := ClientLeft + BorderWidth + Margin + FBoxSize;
  H := T + FBoxSize;

  if not (FReadOnly) then
  begin
    if (X >= L) and (X < W) and (Y >= T) and (Y < H) then
    begin
      FChecked := not FChecked;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomACheckBox.Paint(DC: HDC);
var
  X, Y: Integer;
  L, T, H, W: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

  // Draw Border
  if BorderWidth > 0 then
  begin
    AEngine.Canvas.FillRect(FloatRect(X, Y, X + Width, Y + BorderWidth), BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X, Y + BorderWidth, X + BorderWidth, Y + Height - BorderWidth), BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X, Y + Height - BorderWidth, X + Width, Y + Height), BorderColor);
    AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth, Y + BorderWidth, X + Width, Y +
      Height - BorderWidth), BorderColor);
  end;

  // Draw Background
  if not FTransparent then
  begin
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
      var TexCoord := Quad(0 + BorderWidth, 0 + BorderWidth, AImage.Parameters.Width - (BorderWidth
        * 2), AImage.Parameters.Height - (BorderWidth * 2));
      AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth, Y + BorderWidth, X + Width -
        BorderWidth, Y + Height - BorderWidth)), TexCoord, $FFFFFFFF);

    end
    else
    begin
    //  AEngine.Canvas.FillRect(FloatRect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth, Y
     //   + Height - BorderWidth), Cardinal(Color));
    end;
  end;

  // Draw Box
  T := Y + BorderWidth + Margin;
  H := Height - (BorderWidth * 2) - (Margin * 2);
  T := T + (H div 2 - FBoxSize div 2);

  // Draw focus
  if AEngine.ActiveControl = Self then
  begin
    AEngine.Canvas.FrameRect(FloatRect(X + BorderWidth + Margin - 1, T - 1, X + BorderWidth + Margin
      + FBoxSize + 1, T + FBoxSize + 1), ($20000000), 1, TBlendingEffect.Shadow);
  end;

  if AImageChecked.Initialized then
  begin
  //  AEngine.Canvas.UseImagePx(AImageChecked, pxBounds4(0, 0, AImageChecked.PatternSize.X, AImageChecked.PatternSize.Y));
    //AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth + Margin, T, X + BorderWidth + Margin +
      //FBoxSize, T + FBoxSize)), cAlpha4(FBoxImageAlpha), deNormal);
    var TexCoord := Quad(0, 0, AImage.Parameters.Width, AImage.Parameters.Height);
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth + Margin, T, X + BorderWidth +
      Margin + FBoxSize, T + FBoxSize)), TexCoord, $FFFFFFFF);

  end
  else
  begin
    AEngine.Canvas.FrameRect(FloatRect(X + BorderWidth + Margin, T, X + BorderWidth + Margin +
      FBoxSize, T + FBoxSize), Cardinal(FBoxBorderColor), 1);
    AEngine.Canvas.FillRect(FloatRect(X + BorderWidth + Margin + 1, T + 1, X + BorderWidth + Margin
      + FBoxSize - 1, T + FBoxSize - 1), Cardinal(FBoxColor));
  end;

  if FChecked then
  begin
    if AImageUnChecked.Initialized then
    begin
  //    AEngine.Canvas.UseImagePx(AImageUnChecked, pxBounds4(0, 0, AImageUnChecked.PatternSize.X,
    //    AImageUnChecked.PatternSize.Y));
    //  AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth + Margin, T, X + BorderWidth + Margin +
      //  FBoxSize, T + FBoxSize)), cAlpha4(FBoxImageAlpha), deNormal);

      var TexCoord := Quad(0, 0, AImage.Parameters.Width, AImage.Parameters.Height);
      AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth + Margin, T, X + BorderWidth +
        Margin + FBoxSize, T + FBoxSize)), TexCoord, $FFFFFFFF);

    end
    else
    begin
      L := X + BorderWidth + Margin + 1 + 1;
      W := X + BorderWidth + Margin + FBoxSize - 2;
      T := T + 1;
      H := T + FBoxSize - 3;
      var TexCoord := Quad(Point2f(L + 1, T + ((H - T) / 2) - 1), Point2f(L + ((W - L) / 2) - 1, H -
        3), Point2f(L + ((W - L) / 2) - 1, H + 1), Point2f(L, T + ((H - T) / 2) + 2));
      AEngine.Canvas.Quad(TexCoord, Cardinal(FBoxCheckColor));

     // AEngine.Canvas.FillQuad(Point4(Point2(L + ((W - L) / 2) - 1, H - 4), Point2(W, T + 1), Point2(W,
     //   T + 5), Point2(L + ((W - L) / 2) - 1, H)), cColor4(FBoxCheckColor), deNormal);
    end;
  end;

  // Draw Text
  {
  if AFont <> nil then
  begin
    if Text <> '' then
      AFont.TextRectEx(Point2(X + BorderWidth + Margin + FBoxSize + 4,
          Y + BorderWidth + Margin + 1),
        Point2(Width - (BorderWidth * 2) - (Margin * 2),
          Height - (BorderWidth * 2) - (Margin * 2)), Text,
        cColor2(FontColor), 1.0, hLeft, vMiddle, False);
  end;
  }

  // Draw Text New

 
end;

procedure TCustomACheckBox.SetBoxBorderColor(Value: Cardinal);
begin
  FBoxBorderColor := Value;
end;

procedure TCustomACheckBox.SetBoxCheckColor(Value: Cardinal);
begin
  FBoxCheckColor := Value;
end;

procedure TCustomACheckBox.SetBoxColor(Value: TFillColor);
begin
  FBoxColor.Assign(Value);
end;

procedure TCustomACheckBox.SetBoxImageAlpha(Value: Cardinal);
begin
  FBoxImageAlpha := Value;
end;

procedure TCustomACheckBox.SetBoxImageChecked(Value: TWZIMGEntry);
begin
  FBoxImageChecked := Value;
  if AEngine <> nil then
    FAImageChecked := AEngine.ImageLib[Value];
end;

procedure TCustomACheckBox.SetBoxImageUnChecked(Value: TWZIMGEntry);
begin
  FBoxImageUnChecked := Value;
  if AEngine <> nil then
    FAImageUnChecked := AEngine.ImageLib[Value];
end;

procedure TCustomACheckBox.SetBoxSize(Value: Byte);
begin
  FBoxSize := Value;
end;

procedure TCustomACheckBox.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TCustomACheckBox.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCustomACheckBox.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
end;

initialization
  RegisterClasses([TCustomACheckBox, TACheckBox]);

finalization
  UnRegisterClasses([TCustomACheckBox, TACheckBox]);

end.

