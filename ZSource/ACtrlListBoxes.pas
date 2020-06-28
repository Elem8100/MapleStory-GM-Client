// ----------------------------------------------------------------------------
// ACtrlListBoxes.pas            Modified: 02-10-2010             Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAListBox and TAListBox.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAListBox e TAListBox.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlListBoxes;

interface

uses
  Windows, SysUtils, Classes, Controls,
  // Aspryre units
   // Asphyre GUI Engine
  AControls, ACtrlButtons, ACtrlTypes, WZIMGFile;

type
  TCustomAListBox = class(TWControl)
  private
    FScrollWidth: Integer;
    FButtonDown: TUpDownButton;
    FButtonSlider: TUpDownButton;
    FButtonUp: TUpDownButton;
    FBSliderPos: Integer;
    FBSliderHeight: Integer;
    FIndex: Integer;
    FLineHeight: Integer;
    FSelectColor: TFontColor;
    FSelectFontColor: TFontColor;
    FStrings: TStringList;
    FVirtualHeight: Integer;
    FVirtualPosition: Integer;
    procedure ButtonImageChange(Sender: TObject);
    procedure SetButtonDown(Value: TUpDownButton);
    procedure SetButtonSlider(Value: TUpDownButton);
    procedure SetButtonUp(Value: TUpDownButton);
    procedure SetIndex(Value: Integer);
    procedure SetLineHeight(Value: Integer);
    procedure SetStrings(Value: TStringList);
    procedure SetVirtualHeight(Value: Integer);
    procedure SetVirtualPosition(Value: Integer);
    procedure StringsChange(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Paint(DC: HDC); override;
    procedure PaintScrollBar;
    procedure SetSelectColor(Value: TFontColor); virtual;
    procedure SetSelectFontColor(Value: TFontColor); virtual;
    property VirtualHeight: Integer read FVirtualHeight write SetVirtualHeight;
    property VirtualPosition: Integer read FVirtualPosition write SetVirtualPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetAEngine(AEngine: TCustomEngine); override;
    property ButtonDown: TUpDownButton read FButtonDown write SetButtonDown;
    property ButtonSlider: TUpDownButton read FButtonSlider write SetButtonSlider;
    property ButtonUp: TUpDownButton read FButtonUp write SetButtonUp;
    property ItemIndex: Integer read FIndex write SetIndex;
    property LineHeight: Integer read FLineHeight write SetLineHeight;
    property Lines: TStringList read FStrings write SetStrings;
    property SelectColor: TFontColor read FSelectColor write SetSelectColor;
    property SelectFontColor: TFontColor read FSelectFontColor write SetSelectFontColor;
  end;

  TAListBox = class(TCustomAListBox)
  published
    property ButtonDown;
    property ButtonSlider;
    property ButtonUp;
    property ItemIndex;
    property LineHeight;
    property Lines;
    property SelectColor;
    property SelectFontColor;
    property BorderColor;
    property BorderWidth;
   // property Color;
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

implementation

uses
  PXT.Types, PXT.Graphics;
{ TACustomListBox }

var
  YPress: Integer;

procedure TCustomAListBox.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAListBox then
    with TCustomAListBox(Dest) do
    begin
      // FScrollWidth - for future implementations
      // FIndex - Dynamic
      // FVirtualHeight - Dynamic
      // FVirtualPosition - Dynamic

      ButtonDown := Self.ButtonDown;
      ButtonSlider := Self.ButtonSlider;
      ButtonUp := Self.ButtonUp;
      LineHeight := Self.LineHeight;
      Lines := Self.Lines;
      SelectColor := Self.SelectColor;
      SelectFontColor := Self.SelectFontColor;
    end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomAListBox.ButtonImageChange(Sender: TObject);
begin
  if AEngine <> nil then
  begin
    (TUpDownButton(Sender)).FAImage := AEngine.ImageLib[(TUpDownButton(Sender).Image)];
    (TUpDownButton(Sender)).FAImageHover := AEngine.ImageLib[(TUpDownButton(Sender).ImageHover)];
    (TUpDownButton(Sender)).FAImagePressed := AEngine.ImageLib[(TUpDownButton(Sender).ImagePressed)];
  end;
end;

constructor TCustomAListBox.Create(AOwner: TComponent);
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
      while AOwner.FindComponent('ListBox' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'ListBox' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 120;
  Height := 120;

  BorderColor := $80FFFFFF;
  BorderWidth := 1;

  //Color.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);

  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;

  Margin := 2;

  TabStop := True;

  Visible := True;

  // Fields

  FButtonDown := TUpDownButton.Create;
  FButtonDown.OnImageChange := ButtonImageChange;
  FButtonDown.ButtonType := udDown;
  FButtonDown.IsEnabled := False;

  FButtonSlider := TUpDownButton.Create;
  FButtonSlider.OnImageChange := ButtonImageChange;
  FButtonSlider.IsEnabled := False;

  FButtonUp := TUpDownButton.Create;
  FButtonUp.OnImageChange := ButtonImageChange;
  FButtonUp.ButtonType := udUp;
  FButtonUp.IsEnabled := False;

  FIndex := -1;
  FLineHeight := 18;

  FScrollWidth := 16;

  FSelectColor := TFontColor.Create;
  FSelectColor.SetFontColor($FFFFD040, $FFFF8020);

  FSelectFontColor := TFontColor.Create; // white by default

  FStrings := TStringList.Create;
  FStrings.OnChange := StringsChange;

  FVirtualPosition := 0;
  FVirtualHeight := 0;

  FBSliderPos := 0;
  FBSliderHeight := 0;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomAListBox.Destroy;
begin
  FreeAndNil(FButtonDown);
  FreeAndNil(FButtonSlider);
  FreeAndNil(FButtonUp);
  FreeAndNil(FStrings);
  FreeAndNil(FSelectColor);
  FreeAndNil(FSelectFontColor);

  inherited;
end;

procedure TCustomAListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then
  begin
    if (FStrings.Count > 0) and (FIndex > 0) then
    begin
      FIndex := FIndex - 1;

      if (ClientTop + BorderWidth + Margin + VirtualPosition + (FLineHeight * FIndex)) < ClientTop +
        BorderWidth + Margin then
      begin
        VirtualPosition := -(FLineHeight * FIndex);
      end;

      if (ClientTop + Height - BorderWidth - Margin) < ((ClientTop + BorderWidth + Margin +
        VirtualPosition + (FLineHeight * FIndex)) + FLineHeight) then
      begin
        VirtualPosition := -(FLineHeight * FIndex) + (Height - (BorderWidth * 2) - (Margin * 2) -
          FLineHeight) - ((Height - (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);
      end;
    end;
    if (FStrings.Count > 0) and (FIndex < 0) then
    begin
      FIndex := 0;

      if (ClientTop + BorderWidth + Margin + VirtualPosition + (FLineHeight * FIndex)) < ClientTop +
        BorderWidth + Margin then
      begin
        VirtualPosition := -(FLineHeight * FIndex);
      end;

      if (ClientTop + Height - BorderWidth - Margin) < ((ClientTop + BorderWidth + Margin +
        VirtualPosition + (FLineHeight * FIndex)) + FLineHeight) then
      begin
        VirtualPosition := -(FLineHeight * FIndex) + (Height - (BorderWidth * 2) - (Margin * 2) -
          FLineHeight) - ((Height - (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);
      end;
    end;
  end;

  if Key = VK_DOWN then
  begin
    if (FStrings.Count > 0) and (FIndex < FStrings.Count - 1) then
    begin
      FIndex := FIndex + 1;

      if (ClientTop + BorderWidth + Margin + VirtualPosition + (FLineHeight * FIndex)) < ClientTop +
        BorderWidth + Margin then
      begin
        VirtualPosition := -(FLineHeight * FIndex);
      end;

      if (ClientTop + Height - BorderWidth - Margin) < ((ClientTop + BorderWidth + Margin +
        VirtualPosition + (FLineHeight * FIndex)) + FLineHeight) then
      begin
        VirtualPosition := -(FLineHeight * FIndex) + (Height - (BorderWidth * 2) - (Margin * 2) -
          FLineHeight) - ((Height - (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);
      end;
    end;
  end;

  // scroll to Top
  if Key = VK_HOME then
  begin
    VirtualPosition := 0;
  end;

  // scroll to bottom
  if Key = VK_END then
  begin
    if VirtualHeight > (Height - (BorderWidth * 2) - (Margin * 2)) then
      VirtualPosition := (Height - (BorderWidth * 2) - (Margin * 2)) - VirtualHeight;
  end;

  // Page_Up pressed
  if Key = VK_PRIOR then
  begin
    if VirtualHeight > (Height - (BorderWidth * 2) - (Margin * 2)) then
    begin
      VirtualPosition := VirtualPosition + (Height - (BorderWidth * 2) - (Margin * 2)) - ((Height -
        (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);

      if (VirtualPosition > 0) then
        VirtualPosition := 0;
    end;
  end;

  // Page_Down pressed
  if Key = VK_NEXT then
  begin
    if VirtualHeight > (Height - (BorderWidth * 2) - (Margin * 2)) then
    begin
      VirtualPosition := VirtualPosition - (Height - (BorderWidth * 2) - (Margin * 2)) + ((Height -
        (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);

      if (VirtualPosition + VirtualHeight) < ((Height - (BorderWidth * 2) - (Margin * 2))) then
        VirtualPosition := (Height - (BorderWidth * 2) - (Margin * 2)) - VirtualHeight;
    end;
  end;

  inherited;
end;

procedure TCustomAListBox.MouseDown;
var
  y1: Integer;
begin

  if Button = mbLeft then
  begin
    // ButtonDown
    if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft +
      Width - BorderWidth - Margin)) and (Y >= ClientTop + Height - BorderWidth - Margin - 16) and (Y
      < ClientTop + Height - BorderWidth - Margin) and (FButtonDown.IsEnabled) then
    begin
      if not FButtonDown.IsPressed then
        FButtonDown.IsPressed := True;
    end
    else
    begin
      if FButtonDown.IsPressed then
        FButtonDown.IsPressed := False;
    end;

    // ButtonUp
    if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft +
      Width - BorderWidth - Margin)) and (Y >= ClientTop + BorderWidth + Margin) and (Y < ClientTop
      + BorderWidth + Margin + 16) and (FButtonUp.IsEnabled) then
    begin
      if not FButtonUp.IsPressed then
        FButtonUp.IsPressed := True;
    end
    else
    begin
      if FButtonUp.IsPressed then
        FButtonUp.IsPressed := False;
    end;

    // ButtonSlider
    if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft +
      Width - BorderWidth - Margin)) and (Y >= ClientTop + BorderWidth + Margin + 16 - FBSliderPos)
      and (Y < ClientTop + BorderWidth + Margin + 16 - FBSliderPos + FBSliderHeight) and (FButtonSlider.IsEnabled) then
    begin
      if not FButtonSlider.IsPressed then
        FButtonSlider.IsPressed := True;
      YPress := Y + FBSliderPos;
    end
    else
    begin
      if FButtonSlider.IsPressed then
        FButtonSlider.IsPressed := False;
    end;
  end;

  if (X >= (ClientLeft + BorderWidth + Margin)) and (X < (ClientLeft + Width - BorderWidth - Margin
    - FScrollWidth)) and (Y >= ClientTop + BorderWidth + Margin) and (Y < ClientTop + Height -
    BorderWidth - Margin) and (FStrings.Count > 0) then
  begin
    y1 := Y - ClientTop - BorderWidth - Margin;
    if (y1 - VirtualPosition) div FLineHeight > FStrings.Count - 1 then
    begin
      FIndex := FStrings.Count - 1;
    end
    else
    begin
      FIndex := (y1 - VirtualPosition) div FLineHeight;
    end;
  end;

  inherited;
end;

procedure TCustomAListBox.MouseLeave;
begin
  if FButtonDown.IsHover then
    FButtonDown.IsHover := False;
  if FButtonDown.IsPressed then
    FButtonDown.IsPressed := False;

  if FButtonUp.IsHover then
    FButtonUp.IsHover := False;
  if FButtonUp.IsPressed then
    FButtonUp.IsPressed := False;

  if FButtonSlider.IsHover then
    FButtonSlider.IsHover := False;
  if FButtonSlider.IsPressed then
    FButtonSlider.IsPressed := False;

  inherited;
end;

procedure TCustomAListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // ButtonDown
  if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft + Width
    - BorderWidth - Margin)) and (Y >= ClientTop + Height - BorderWidth - Margin - 16) and (Y <
    ClientTop + Height - BorderWidth - Margin) and (FButtonDown.IsEnabled) then
  begin
    if not FButtonDown.IsHover then
      FButtonDown.IsHover := True;
  end
  else
  begin
    if FButtonDown.IsHover then
      FButtonDown.IsHover := False;
  end;

  // ButtonUp
  if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft + Width
    - BorderWidth - Margin)) and (Y >= ClientTop + BorderWidth + Margin) and (Y < ClientTop +
    BorderWidth + Margin + 16) and (FButtonUp.IsEnabled) then
  begin
    if not FButtonUp.IsHover then
      FButtonUp.IsHover := True;
  end
  else
  begin
    if FButtonUp.IsHover then
      FButtonUp.IsHover := False;
  end;

  // ButtonSlider
  if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft + Width
    - BorderWidth - Margin)) and (Y >= ClientTop + BorderWidth + Margin + 16 - FBSliderPos) and (Y <
    ClientTop + BorderWidth + Margin + 16 - FBSliderPos + FBSliderHeight) and (FButtonSlider.IsEnabled) then
  begin
    if not FButtonSlider.IsHover then
      FButtonSlider.IsHover := True;
  end
  else
  begin
    if FButtonSlider.IsHover then
      FButtonSlider.IsHover := False;
  end;

  // ButtonSlider pressed
  if FButtonSlider.IsPressed then
  begin

    VirtualPosition := Round((YPress - Y) * (FVirtualHeight / (Height - (BorderWidth * 2) - (Margin * 2) - 32)));

    if VirtualPosition > 0 then
      VirtualPosition := 0;

    if (VirtualPosition + VirtualHeight) < ((Height - (BorderWidth * 2) - (Margin * 2))) then
      VirtualPosition := (Height - (BorderWidth * 2) - (Margin * 2)) - VirtualHeight;
  end;

  inherited;
end;

procedure TCustomAListBox.MouseUp;
begin
  if Button = mbLeft then
  begin
    // ButtonDown
    if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft +
      Width - BorderWidth - Margin)) and (Y >= ClientTop + Height - BorderWidth - Margin - 16) and (Y
      < ClientTop + Height - BorderWidth - Margin) and (FButtonDown.IsEnabled) then
    begin
      if FButtonDown.IsPressed then
      begin
        // do line down
        VirtualPosition := VirtualPosition - FLineHeight;
        // FButtonDown.IsPressed := False;
      end;
    end;

    // ButtonUp
    if (X >= (ClientLeft + Width - BorderWidth - Margin - FScrollWidth)) and (X < (ClientLeft +
      Width - BorderWidth - Margin)) and (Y >= ClientTop + BorderWidth + Margin) and (Y < ClientTop
      + BorderWidth + Margin + 16) and (FButtonUp.IsEnabled) then
    begin
      if FButtonUp.IsPressed then
      begin
        // do line up
        VirtualPosition := VirtualPosition + FLineHeight;
        // FButtonUp.IsPressed := False;
      end;
    end;

    FButtonDown.IsPressed := False;
    FButtonUp.IsPressed := False;
    FButtonSlider.IsPressed := False;
  end;

  inherited;
end;

function TCustomAListBox.MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;

  if FVirtualPosition + FVirtualHeight > Height - (BorderWidth * 2) - (Margin * 2) then
  begin
    VirtualPosition := VirtualPosition - FLineHeight;

    if VirtualPosition > 0 then
      VirtualPosition := 0;

    if (VirtualPosition + VirtualHeight) < ((Height - (BorderWidth * 2) - (Margin * 2))) then
      VirtualPosition := (Height - (BorderWidth * 2) - (Margin * 2)) - VirtualHeight;

    Result := True;
  end;
end;

function TCustomAListBox.MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;

  if FVirtualPosition < 0 then
  begin
    VirtualPosition := VirtualPosition + FLineHeight;

    if VirtualPosition > 0 then
      VirtualPosition := 0;

    if (VirtualPosition + VirtualHeight) < ((Height - (BorderWidth * 2) - (Margin * 2))) then
      VirtualPosition := (Height - (BorderWidth * 2) - (Margin * 2)) - VirtualHeight;

    Result := True;
  end;
end;

procedure TCustomAListBox.Paint(DC: HDC);
var
  X, Y: Integer;
  Index: Integer;
  ARect: TIntRect;
  // constraint variables for line draw
  YTop, YBottom: Integer;
begin
  // Get size Canvas
  ARect := AEngine.Canvas.ClipRect;

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
      BorderWidth, Y + Height - BorderWidth)), TexCoord, $FFFFFFFF);
  end
  else
  begin
   // AEngine.Canvas.FillRect(FloatRect(X + BorderWidth, Y + BorderWidth, X + Width - BorderWidth, Y +
    //  Height - BorderWidth), Cardinal(Color));
  end;

  // draw scroll bar
  PaintScrollBar;

  // Set Rect Canvas
  AEngine.Canvas.ClipRect := intRect(X + BorderWidth + Margin, Y + BorderWidth + Margin, X + Width -
    BorderWidth - Margin - FScrollWidth, Y + Height - BorderWidth - Margin);

  // Draw lines
  if FStrings.Count > 0 then
  begin
    for Index := 0 to FStrings.Count - 1 do
    begin
      YTop := Y + BorderWidth + Margin + VirtualPosition + (FLineHeight * Index);
      YBottom := Y + BorderWidth + Margin + VirtualPosition + (FLineHeight * Index) + FLineHeight;

      if (AEngine.Canvas.ClipRect.Top <= YBottom) and (AEngine.Canvas.ClipRect.Bottom > YTop) then
      begin
        if FIndex = Index then
        begin
          AEngine.Canvas.FillRect(FloatRect(X + BorderWidth + Margin, Y + BorderWidth + Margin +
            VirtualPosition + (FLineHeight * Index), X + Width - BorderWidth - Margin - FScrollWidth,
            Y + BorderWidth + Margin + VirtualPosition + (FLineHeight * Index) + FLineHeight),
            ColorRect(SelectColor.Top, SelectColor.Top, SelectColor.Bottom, SelectColor.Bottom));

            {
          AFont.TextOut(Point2(X + BorderWidth + Margin + 1,
              Y + BorderWidth + Margin + VirtualPosition +
                (FLineHeight * Index)), FStrings[Index],
            cColor2(SelectFontColor), 1.0);
            }

          // draw unselected shader
          if AEngine.ActiveControl <> Self then
          begin
            AEngine.Canvas.FillRect(FloatRect(X + BorderWidth + Margin, Y + BorderWidth + Margin +
              VirtualPosition + (FLineHeight * Index), X + Width - BorderWidth - Margin -
              FScrollWidth, Y + BorderWidth + Margin + VirtualPosition + (FLineHeight * Index) +
              FLineHeight), ($20000000));
          end;
        end
        else
        begin
          {
          AFont.TextOut(Point2(X + BorderWidth + Margin + 1,
              Y + BorderWidth + Margin + VirtualPosition +
                (FLineHeight * Index)), FStrings[Index],
            cColor2(FontColor), 1.0);
          }
        
        end;
      end;
    end;
  end;

  // Set Rect Canvas
  AEngine.Canvas.ClipRect := ARect;
end;

procedure TCustomAListBox.PaintScrollBar;
var
  X, Y: Integer;
  YIni, YEnd, YMin, YMax: Integer;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

  YMin := Y + BorderWidth + Margin + 16;
  YMax := Y + Height - BorderWidth - Margin - 16;

  // Draw scroll area
  AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
    + Margin, X + Width - BorderWidth - Margin, Y + Height - BorderWidth - Margin), $20000000, TBlendingEffect.Shadow);

  // Draw up button
  if FButtonUp.AImage.Initialized then
  begin
  {
    AEngine.Canvas.UseImagePx(FButtonUp.AImage,
      pxBounds4(0, 0, FButtonUp.AImage.PatternSize.X,
        FButtonUp.AImage.PatternSize.Y));
    AEngine.Canvas.TexMap
      (pRect4(Rect(X + Width - BorderWidth - Margin - FScrollWidth,
          Y + BorderWidth + Margin, X + Width - BorderWidth - Margin,
          Y + BorderWidth + Margin + 16)), cAlpha4(ImageAlpha), deNormal);

          }

    var TexCoord := Quad(0, 0, FButtonUp.AImage.Parameters.Width, FButtonUp.AImage.Parameters.Height);
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + Width - BorderWidth - Margin - FscrollWidth, Y +
      BorderWidth + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16)), TexCoord, $FFFFFFFF);

    if not (FButtonUp.IsEnabled) then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
        + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16), ($20000000));
    end;
  end
  else
  begin
    if (FButtonUp.IsHover) and not (FButtonUp.IsPressed) then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
        + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16),cardinal(FButtonUp.ColorHover));
    end
    else if FButtonUp.IsPressed then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
        + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16), Cardinal(FButtonUp.ColorPressed)
        );
    end
    else
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
        + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16), Cardinal(FButtonUp.Color));
    end;

   // AEngine.Canvas.Triangles(Point2f(X + Width - BorderWidth - Margin - FScrollWidth / 2, Y +
     // BorderWidth + Margin + 3), Point2f(X + Width - BorderWidth - Margin - FScrollWidth + 2, Y +
    //  BorderWidth + Margin + 10), Point2f(X + Width - BorderWidth - Margin - 2, Y + BorderWidth +
      //Margin + 10), FButtonUp.IconColor, FButtonUp.IconColor, FButtonUp.IconColor);

    AEngine.Canvas.FrameRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth +
      Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16), ($60FFFFFF),1);

    if not (FButtonUp.IsEnabled) then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + BorderWidth
        + Margin, X + Width - BorderWidth - Margin, Y + BorderWidth + Margin + 16), ($20000000));
    end;
  end;

  // Draw down button
  if FButtonDown.AImage.Initialized then
  begin
  {
    AEngine.Canvas.UseImagePx(FButtonDown.AImage, pxBounds4(0, 0, FButtonDown.AImage.PatternSize.X,
      FButtonDown.AImage.PatternSize.Y));
    AEngine.Canvas.TexMap(pRect4(Rect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
      BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth - Margin)),
      cAlpha4(ImageAlpha), deNormal);
   }
    var TexCoord := Quad(0, 0, FButtonUp.AImage.Parameters.Width, FButtonUp.AImage.Parameters.Height);
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + Width - BorderWidth - Margin - FscrollWidth,Y + Height -
      BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth - Margin)), TexCoord, $FFFFFFFF);

    if not (FButtonDown.IsEnabled) then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
        BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth -
        Margin), ($20000000));
    end;
  end
  else
  begin
    if (FButtonDown.IsHover) and not (FButtonDown.IsPressed) then
    begin
      AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
        BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth -
        Margin), Cardinal(FButtonDown.ColorHover));
    end
    else if FButtonDown.IsPressed then
    begin
      AEngine.Canvas.FillRect(floatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
        BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth -
        Margin), cardinal(FButtonDown.ColorPressed));
    end
    else
    begin
      AEngine.Canvas.FillRect(floatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
        BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth -
        Margin), cardinal(FButtonDown.Color));
    end;

  //  AEngine.Canvas.FillTri(Point2(X + Width - BorderWidth - Margin - FScrollWidth / 2, Y + Height -
    //  BorderWidth - Margin - 3), Point2(X + Width - BorderWidth - Margin - FScrollWidth + 2, Y +
    //  Height - BorderWidth - Margin - 10), Point2(X + Width - BorderWidth - Margin - 2, Y + Height -
    //  BorderWidth - Margin - 10), FButtonDown.IconColor, FButtonDown.IconColor, FButtonDown.IconColor);
    AEngine.Canvas.FrameRect(floatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
      BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth - Margin),
     ($60FFFFFF),1);

    if not (FButtonDown.IsEnabled) then
    begin
      AEngine.Canvas.FillRect(floatRect(X + Width - BorderWidth - Margin - FScrollWidth, Y + Height -
        BorderWidth - Margin - 16, X + Width - BorderWidth - Margin, Y + Height - BorderWidth -
        Margin), ($20000000));
    end;
  end;

  // Draw Slide Button
  if FButtonSlider.IsEnabled then
  begin
    // set values
    YIni := YMin - FBSliderPos;

    // small adjust because round could not get exact value
    if YIni > YMax - FBSliderHeight then
      YIni := YMax - FBSliderHeight;

    YEnd := YIni + FBSliderHeight;

    if FButtonSlider.AImage.Initialized then
    begin
    {
      AEngine.Canvas.UseImagePx(FButtonSlider.AImage, pxBounds4(0, 0, FButtonSlider.AImage.PatternSize.X,
        FButtonSlider.AImage.PatternSize.Y));
      AEngine.Canvas.TexMap(pRect4(Rect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
        Width - BorderWidth - Margin, YEnd)), cAlpha4(ImageAlpha), deNormal);
     }
       var TexCoord := Quad(0, 0, FButtonUp.AImage.Parameters.Width, FButtonUp.AImage.Parameters.Height);
    AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + Width - BorderWidth - Margin - FscrollWidth, YIni, X +
        Width - BorderWidth - Margin, YEnd)), TexCoord, $FFFFFFFF);

    end
    else
    begin
      if (FButtonSlider.IsHover) and not (FButtonSlider.IsPressed) then
      begin
        AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal(FButtonSlider.ColorHover));
        AEngine.Canvas.FrameRect(floatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal($60FFFFFF),1);
      end
      else if FButtonSlider.IsPressed then
      begin
        AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal(FButtonSlider.ColorPressed));
        AEngine.Canvas.FrameRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal($60FFFFFF),1);
      end
      else
      begin
        AEngine.Canvas.FillRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal(FButtonSlider.Color));
        AEngine.Canvas.FrameRect(FloatRect(X + Width - BorderWidth - Margin - FScrollWidth, YIni, X +
          Width - BorderWidth - Margin, YEnd), cardinal($60FFFFFF),1);
      end;
    end;
  end;

end;

procedure TCustomAListBox.SetAEngine(AEngine: TCustomEngine);
begin
  inherited;

  if FButtonDown <> nil then
    ButtonImageChange(FButtonDown);
  if FButtonSlider <> nil then
    ButtonImageChange(FButtonSlider);
  if FButtonUp <> nil then
    ButtonImageChange(FButtonUp);
end;

procedure TCustomAListBox.SetButtonDown(Value: TUpDownButton);
begin
  FButtonDown.Assign(Value);
end;

procedure TCustomAListBox.SetButtonSlider(Value: TUpDownButton);
begin
  FButtonSlider.Assign(Value);
end;

procedure TCustomAListBox.SetButtonUp(Value: TUpDownButton);
begin
  FButtonUp.Assign(Value);
end;

procedure TCustomAListBox.SetIndex(Value: Integer);
begin
  if (Value > FStrings.Count - 1) or (FStrings.Count = 0) or (Value < -1) then
  begin
    FIndex := -1;
  end
  else
  begin
    FIndex := Value;
  end;
end;

procedure TCustomAListBox.SetLineHeight(Value: Integer);
begin
  FLineHeight := Value;

  if FLineHeight < 2 then
    FLineHeight := 2;

  VirtualHeight := FLineHeight * FStrings.Count;
end;

procedure TCustomAListBox.SetSelectColor(Value: TFontColor);
begin
  FSelectColor.Assign(Value);
end;

procedure TCustomAListBox.SetSelectFontColor(Value: TFontColor);
begin
  FSelectFontColor.Assign(Value);
end;

procedure TCustomAListBox.SetStrings(Value: TStringList);
begin
  FStrings.Assign(Value);
end;

procedure TCustomAListBox.SetVirtualHeight(Value: Integer);
begin
  if (Value > 0) then
  begin
    Value := Value + ((Height - (BorderWidth * 2) - (Margin * 2)) mod FLineHeight);
  end;

  FVirtualHeight := Value;

  // Set FBSliderHeight
  if FVirtualHeight > 0 then
    FBSliderHeight := Round((Height - (BorderWidth * 2) - (Margin * 2)) * ((Height - (BorderWidth *
      2) - (Margin * 2) - 32) / FVirtualHeight));

  // Set slider min height
  if FBSliderHeight < 6 then
    FBSliderHeight := 6;

  if FVirtualPosition + FVirtualHeight > Height - (BorderWidth * 2) - (Margin * 2) then
  begin
    ButtonDown.IsEnabled := True;
  end
  else
  begin
    ButtonDown.IsEnabled := False;
  end;

  if FVirtualHeight > Height - (BorderWidth * 2) - (Margin * 2) then
  begin
    ButtonSlider.IsEnabled := True;
  end
  else
  begin
    ButtonSlider.IsEnabled := False;
  end;
end;

procedure TCustomAListBox.SetVirtualPosition(Value: Integer);
var
  nPos: Integer;
begin
  FVirtualPosition := Value;

  // Set FBSliderPos
  if FVirtualHeight > 0 then
  begin
    FBSliderPos := Round(FVirtualPosition * ((Height - (BorderWidth * 2) - (Margin * 2) - 32) / FVirtualHeight));

    // get diference of min size and original value
    nPos := FBSliderHeight - Round((Height - (BorderWidth * 2) - (Margin * 2)) * ((Height - (BorderWidth
      * 2) - (Margin * 2) - 32) / FVirtualHeight));

    if nPos > 0 then
      FBSliderPos := Round(FVirtualPosition * ((Height - (BorderWidth * 2) - (Margin * 2) - 32 -
        nPos) / FVirtualHeight));
  end;

  if FVirtualPosition < 0 then
  begin
    ButtonUp.IsEnabled := True;
  end
  else
  begin
    ButtonUp.IsEnabled := False;
  end;

  if FVirtualPosition + FVirtualHeight > Height - (BorderWidth * 2) - (Margin * 2) then
  begin
    ButtonDown.IsEnabled := True;
  end
  else
  begin
    ButtonDown.IsEnabled := False;
  end;
end;

procedure TCustomAListBox.StringsChange(Sender: TObject);
begin
  VirtualHeight := FLineHeight * FStrings.Count;
end;

initialization
  RegisterClasses([TCustomAListBox, TAListBox]);

finalization
  UnRegisterClasses([TCustomAListBox, TAListBox]);

end.

