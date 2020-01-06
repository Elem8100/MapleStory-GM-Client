// ----------------------------------------------------------------------------
// ACtrlEditBoxes.pas           Modified: 02-10-2010              Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAEditBox and TAEditBox.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAEditBox e TAEditBox.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlEditBoxes;

interface

uses
  Windows, SysUtils, Classes, Clipbrd, Math, Controls,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, AsphyreImages, AsphyreTypes, Vectors2, Vectors2px,
  // Asphyre GUI Engine
  AControls, ACtrlTypes;

type
  { TCustomAEditBox }
  TCustomAEditBox = class(TWControl)
  private
    FHAlign: THAlign;
    FReadOnly: Boolean;
    FAutoSelect: Boolean;
    FMaxLength: Integer;
    FOnChange: TNotifyEvent;
    FSelection: TSelection;
    FSelectColor: TFontColor;
    FSelectFontColor: TFontColor;
    FVirtualPosition: Integer;
    function GetTic: Integer;
    procedure SetReadOnly(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Paint(DC: HDC); override;

    procedure Change; dynamic;
    function GetSelLength: Integer; virtual;
    function GetSelStart: Integer; virtual;
    function GetSelText: string; virtual;
    procedure SetMaxLength(Value: Integer); virtual;
    procedure SetSelectColor(Value: TFontColor); virtual;
    procedure SetSelectFontColor(Value: TFontColor); virtual;
    procedure SetSelLength(Value: Integer); virtual;
    procedure SetSelStart(Value: Integer); virtual;
    procedure SetSelText(Value: String); virtual;
    procedure SetText(Value: String); override;

    property AutoSelect
      : Boolean read FAutoSelect write FAutoSelect default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    procedure ClearSelection;

    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;

    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property SelectColor: TFontColor read FSelectColor write SetSelectColor;
    property SelectFontColor: TFontColor read FSelectFontColor write
      SetSelectFontColor;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property TabStop;
  end;

  { TAEditBox }
  TAEditBox = class(TCustomAEditBox)
  published
    property AutoSelect;
    property OnChange;
    property ReadOnly;
    property MaxLength;
    property SelectColor;
    property SelectFontColor;

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

  TAEditBoxClass = class of TAEditBox;

implementation

// ----------------------------------------------------------------------------
var
  Tic: Byte;
  Counter: Cardinal;

  // ----------------------------------------------------------------------------

  { TCustomAEditBox }

procedure TCustomAEditBox.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAEditBox then
    with TCustomAEditBox(Dest) do
    begin
      // FHAlign - not implemented, return always hLeft
      ReadOnly := Self.ReadOnly;
      AutoSelect := Self.AutoSelect;
      MaxLength := Self.MaxLength;
      OnChange := Self.OnChange;
      SelectColor := Self.SelectColor;
      SelectFontColor := Self.SelectFontColor;
      // FSelection - is dynamic field
      // FVirtualPosition - is dynamic field
    end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomAEditBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomAEditBox.Clear;
begin
  Text := '';
  Change;
end;

procedure TCustomAEditBox.ClearSelection;
begin
  FSelection.StartPos := 0;
  FSelection.EndPos := 0;
end;

procedure TCustomAEditBox.Click;
begin
  inherited;
end;

procedure TCustomAEditBox.CopyToClipboard;
begin
  Clipboard.SetTextBuf(PChar(Self.SelText));
end;

constructor TCustomAEditBox.Create(AOwner: TComponent);
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
      while AOwner.FindComponent('EditBox' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'EditBox' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 120;
  Height := 26;

  BorderColor := $80FFFFFF;
  BorderWidth := 1;

  Color.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);

  Font := 'tahoma10b';
  FontColor.SetFontColor(clWhite2);

  Margin := 3;

  TabStop := True;

  Visible := True;

  // Fields
  FHAlign := hLeft;

  FSelectColor := TFontColor.Create;
  FSelectColor.SetFontColor($FFFFD040, $FFFF8020);

  FSelectFontColor := TFontColor.Create; // white by default

  FSelection.StartPos := 0;
  FSelection.EndPos := 0;

  FAutoSelect := False;

  // FOldSelLength := -1;
  // FOldSelStart := -1;

  FReadOnly := False;

  Tic := 0;
  Counter := GetTickCount;

  ControlState := ControlState - [csCreating];
end;

procedure TCustomAEditBox.CutToClipboard;
var
  AText: String;
  AMin, AMax, ALength: Integer;
begin
  // Set initial values
  AText := Text;

  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  ALength := AMax - AMin;

  // Copy to Clipboard
  CopyToClipboard;

  Delete(AText, AMin + 1, ALength);
  Text := AText;
  // Execute OnChange Event
  Change;

  FSelection.StartPos := AMin;
  FSelection.EndPos := AMin;
end;

destructor TCustomAEditBox.Destroy;
begin
  FreeAndNil(FSelectColor);
  FreeAndNil(FSelectFontColor);

  inherited Destroy;
end;

procedure TCustomAEditBox.DoEnter;
begin
  Tic := 0;

  if FAutoSelect then
    SelectAll;

  inherited;
end;

procedure TCustomAEditBox.DoExit;
begin
  ClearSelection;
  inherited;
end;

function TCustomAEditBox.GetSelLength: Integer;
var
  AMin, AMax: Integer;
begin
  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  Result := AMax - AMin;
end;

function TCustomAEditBox.GetSelStart: Integer;
var
  AMin: Integer;
begin
  AMin := Min(FSelection.StartPos, FSelection.EndPos);

  Result := AMin;
end;

function TCustomAEditBox.GetSelText: string;
var
  AMin, AMax, ALength: Integer;
begin
  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);
  ALength := AMax - AMin;

  Result := Copy(Text, AMin + 1, ALength);
end;

function TCustomAEditBox.GetTic: Integer;
begin
  // When the system run continuously for 49.7 days, GetTickCount=0
  if GetTickCount < Counter then
    Counter := GetTickCount;

  if (GetTickCount - Counter) >= 300 then
  begin
    Counter := GetTickCount;
    Tic := Tic + 1;
  end;

  if Tic = 4 then
    Tic := 0;

  Result := Tic;
end;

procedure TCustomAEditBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  AText: String;
  AMin, AMax, ALength: Integer;
begin
  AText := Text;

  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  ALength := AMax - AMin;

  // shift pressed
  if Shift = [ssShift] then
  begin
    // user press right key
    if Key = vk_Right then
    begin
      if FSelection.EndPos < Length(Text) then
        Inc(FSelection.EndPos);
    end;

    // user press left key
    if Key = vk_Left then
    begin
      if FSelection.EndPos > 0 then
        Dec(FSelection.EndPos);
    end;

    // user press home key
    if Key = VK_Home then
    begin
      FSelection.EndPos := 0;
    end;

    // user press end key
    if Key = VK_End then
    begin
      FSelection.EndPos := Length(Text);
    end;
  end;

  // Shift not pressed
  if Shift <> [ssShift] then
  begin
    // User press left key
    if (Key = vk_Left) then
    begin
      if FSelection.StartPos = FSelection.EndPos then
      begin
        if FSelection.StartPos > 0 then
        begin
          Dec(FSelection.StartPos);
          FSelection.EndPos := FSelection.StartPos;
        end;
      end
      else if FSelection.StartPos > FSelection.EndPos then
      begin
        FSelection.StartPos := FSelection.EndPos;
      end
      else if FSelection.StartPos < FSelection.EndPos then
      begin
        FSelection.EndPos := FSelection.StartPos;
      end;
    end;

    // User press right key
    if (Key = vk_Right) then
    begin
      if FSelection.StartPos = FSelection.EndPos then
      begin
        if FSelection.EndPos < Length(Text) then
        begin
          Inc(FSelection.EndPos);
          FSelection.StartPos := FSelection.EndPos;
        end;
      end
      else if FSelection.StartPos > FSelection.EndPos then
      begin
        FSelection.EndPos := FSelection.StartPos;
      end
      else if FSelection.StartPos < FSelection.EndPos then
      begin
        FSelection.StartPos := FSelection.EndPos;
      end;
    end;

    // User press Delete or Backspace
    if ((Key = vk_Back) or (Key = vk_Delete)) and not(ReadOnly) then
    begin
      if ALength > 0 then
      begin
        case Key of
          vk_Back:
            Delete(AText, AMin + 1, ALength);
          vk_Delete:
            Delete(AText, AMin + 1, ALength);
        end;
      end
      else
      begin
        case Key of
          vk_Back:
            begin
              Delete(AText, AMin, 1);
              if AMin > 0 then
                Dec(AMin);
            end;
          vk_Delete:
            Delete(AText, AMin + 1, 1);
        end;
      end;

      Text := AText;
      // Execute OnChange Event
      Change;

      FSelection.StartPos := AMin;
      FSelection.EndPos := AMin;
    end;

    // user press home key
    if Key = VK_Home then
    begin
      FSelection.StartPos := 0;
      FSelection.EndPos := 0;
    end;

    // user press end key
    if Key = VK_End then
    begin
      FSelection.StartPos := Length(Text);
      FSelection.EndPos := Length(Text);
    end;
  end;

  Tic := 0;

  inherited KeyDown(Key, Shift);
end;

procedure TCustomAEditBox.KeyPress(var Key: Char);
var
  AText: String;
  AMin, AMax, ALength: Integer;
begin
  AText := Text;

  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  ALength := AMax - AMin;

  // Insert Key
  if (Key > #31) and not ReadOnly then
  begin
    Delete(AText, AMin + 1, ALength);

    Inc(AMin);
    Insert(Key, AText, AMin);

    Text := AText;
    // Execute OnChange Event
    Change;

    if AMin > Length(Text) then
      AMin := Length(Text);

    FSelection.StartPos := AMin;
    FSelection.EndPos := AMin;
  end;

  // Copy to Clipboard
  if Key = #3 then
    CopyToClipboard;

  // Paste from Clipboard
  if Key = #22 then
    PasteFromClipboard;

  // Cut to Clipboard
  if Key = #24 then
    CutToClipboard;

  inherited KeyPress(Key);
end;

procedure TCustomAEditBox.KeyUp(var Key: Word; Shift: TShiftState);
begin

  inherited KeyUp(Key, Shift);
end;

procedure TCustomAEditBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index, XPos, AVirtualCursor: Integer;
  AChars: TAChars;
begin
  {
  if AFont <> nil then
  begin
    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char := Text[Index + 1];
      AChars[Index].Width := Round(AFont.TextWidth(Text[Index + 1])
          + AFont.Kerning);
      Inc(Index);
    end;

    // Set position to 0
    FSelection.StartPos := 0;
    FSelection.EndPos := 0;

    // Get virtual Bounds
    XPos := ClientLeft + BorderWidth + Margin + FVirtualPosition;

    // Set virtual Pos
    AVirtualCursor := XPos;
    for Index := 0 to High(AChars) do
    begin
      if (X > AVirtualCursor) and (X <= AVirtualCursor + AChars[Index].Width)
        then
      begin
        if Index < Length(Text) then
        begin
          FSelection.StartPos := Index + 1;
          FSelection.EndPos := Index + 1;
        end;
        Break;
      end;
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

      if (Index = High(AChars)) and (X >= AVirtualCursor) then
      begin
        FSelection.StartPos := Index;
        FSelection.EndPos := Index;
      end;
    end;
  end;
  }
  if ZFont <> nil then
  begin
    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char  := Text[Index + 1];
      AChars[Index].Width := Round(ZFont.CharWidth(0,Text[Index + 1]));
      //AChars[Index].Width := Round(ZFont.GetTextLength(0,Text[Index + 1],1,1) + ZFont.Spacing);
      Inc(Index);
    end;

    // Set position to 0
    FSelection.StartPos := 0;
    FSelection.EndPos := 0;

    // Get virtual Bounds
    XPos := ClientLeft + BorderWidth + Margin + FVirtualPosition;

    // Set virtual Pos
    AVirtualCursor := XPos;
    for Index := 0 to High(AChars) do
    begin
      if (X > AVirtualCursor) and (X <= AVirtualCursor + AChars[Index].Width)
        then
      begin
        if Index < Length(Text) then
        begin
          FSelection.StartPos := Index + 1;
          FSelection.EndPos := Index + 1;
        end;
        Break;
      end;
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

      if (Index = High(AChars)) and (X >= AVirtualCursor) then
      begin
        FSelection.StartPos := Index;
        FSelection.EndPos := Index;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomAEditBox.MouseEnter;
begin
  // Change the cursor
  AEngine.Parent.Cursor := crIBeam;

  inherited MouseEnter;
end;

procedure TCustomAEditBox.MouseLeave;
begin
  // Change the cursor
  AEngine.Parent.Cursor := crDefault;

  inherited MouseLeave;
end;

procedure TCustomAEditBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index, XPos, AVirtualCursor: Integer;
  AChars: TAChars;
begin
  {
  if (AFont <> nil) and (Shift = [ssLeft]) then
  begin
    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char := Text[Index + 1];
      AChars[Index].Width := Round(AFont.TextWidth(Text[Index + 1])
          + AFont.Kerning);
      Inc(Index);
    end;

    // Set position to 0
    FSelection.EndPos := 0;

    // Get virtual Bounds
    XPos := ClientLeft + BorderWidth + Margin + FVirtualPosition;

    // Set virtual Pos
    AVirtualCursor := XPos;
    for Index := 0 to High(AChars) do
    begin
      if (X > AVirtualCursor) and (X <= AVirtualCursor + AChars[Index].Width)
        then
      begin
        if Index < Length(Text) then
        begin
          FSelection.EndPos := Index + 1;
        end;
        Break;
      end;
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

      if (Index = High(AChars)) and (X >= AVirtualCursor) then
      begin
        FSelection.EndPos := Index;
      end;
    end;
  end;
  }

  if (ZFont <> nil) and (Shift = [ssLeft]) then
  begin
    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char := Text[Index + 1];
      AChars[Index].Width := Round(ZFont.CharWidth(0,Text[Index + 1]));
      Inc(Index);
    end;

    // Set position to 0
    FSelection.EndPos := 0;

    // Get virtual Bounds
    XPos := ClientLeft + BorderWidth + Margin + FVirtualPosition;

    // Set virtual Pos
    AVirtualCursor := XPos;
    for Index := 0 to High(AChars) do
    begin
      if (X > AVirtualCursor) and (X <= AVirtualCursor + AChars[Index].Width)
        then
      begin
        if Index < Length(Text) then
        begin
          FSelection.EndPos := Index + 1;
        end;
        Break;
      end;
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

      if (Index = High(AChars)) and (X >= AVirtualCursor) then
      begin
        FSelection.EndPos := Index;
      end;
    end;
  end;

  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomAEditBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomAEditBox.Paint(DC: HDC);
var
  Index, X, Y, AWidth, AHeight, AVirtualCursor: Integer;
  AMin, AMax: Integer;
  AFontColor: TColor2;
  ASelectColor: TColor4;
  AChars: TAChars;
  ARect: TRect;
begin
  // Get size Canvas
  ARect := AEngine.Canvas.ClipRect;

  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

  // Draw Border
  if BorderWidth > 0 then
  begin
    AEngine.Canvas.FillRect(Rect(X, Y, X + Width, Y + BorderWidth),
      BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X, Y + BorderWidth, X + BorderWidth,
        Y + Height - BorderWidth), BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X, Y + Height - BorderWidth, X + Width,
        Y + Height), BorderColor, deNormal);
    AEngine.Canvas.FillRect(Rect(X + Width - BorderWidth, Y + BorderWidth,
        X + Width, Y + Height - BorderWidth), BorderColor, deNormal);
  end;

  // Set Bounds
  X := X + BorderWidth;
  Y := Y + BorderWidth;
  AWidth := Width - BorderWidth * 2;
  AHeight := Height - BorderWidth * 2;

  // Draw Background
  if AImage <> nil then
  begin
    AEngine.Canvas.UseTexturePx(AImage,
      pxBounds4(0 + BorderWidth, 0 + BorderWidth,
        AImage.Width - (BorderWidth * 2),
        AImage.Height - (BorderWidth * 2)));
    AEngine.Canvas.TexMap(pRect4(Rect(X, Y, X + AWidth, Y + AHeight)),
      cAlpha4(ImageAlpha), deNormal);
  end
  else
  begin
    AEngine.Canvas.FillRect(Rect(X, Y, X + AWidth, Y + AHeight),
      cColor4(Color), deNormal);
  end;

  // Set Bounds
  X := X + Margin;
  Y := Y + Margin;
  AWidth := AWidth - Margin * 2;
  AHeight := AHeight - Margin * 2;

  {
  // Get chars from text and draw all
  if AFont <> nil then
  begin
    // Set Rect Canvas
    AEngine.Canvas.ClipRect := Rect(X - 1, Y, X + AWidth, Y + AHeight);

    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char := Text[Index + 1];
      AChars[Index].Width := Round(AFont.TextWidth(Text[Index + 1])
          + AFont.Kerning);
      Inc(Index);
    end;

    // Set virtual Pos
    AVirtualCursor := 0;
    FVirtualPosition := 0;
    for Index := 0 to FSelection.EndPos - 1 do
    begin
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

    end;
    if AVirtualCursor > AWidth then
    begin
      FVirtualPosition := AWidth - AVirtualCursor;
      X := X + FVirtualPosition;
    end;

    AMin := Min(FSelection.StartPos, FSelection.EndPos);
    AMax := Max(FSelection.StartPos, FSelection.EndPos);

    ASelectColor := cColor4(FSelectColor.Top, FSelectColor.Top,
      FSelectColor.Bottom, FSelectColor.Bottom);

    // Draw Text char by char
    for Index := 0 to High(AChars) do
    begin

      // Draw Selection
      if (AMin < AMax) then
      begin
        if (Index >= AMin) and (Index < AMax) then
          AEngine.Canvas.FillRect(Rect(X, Y, X + AChars[Index].Width,
              Y + AHeight), ASelectColor, deNormal);
        AFontColor := cColor2(FSelectFontColor);
      end;

      // Set Font Color
      if (AMin < AMax) then
      begin
        if (Index >= AMin) and (Index < AMax) then
          AFontColor := cColor2(FSelectFontColor)
        else
          AFontColor := cColor2(FontColor);
      end
      else
        AFontColor := cColor2(FontColor);

      // Draw char
      AFont.TextOut(Point2(X, Y), AChars[Index].Char, AFontColor, 1.0);

      // Draw Tic
      if (GetTic <= 1) and (AEngine.ActiveControl = Self) then
      begin
        if Index = FSelection.EndPos then
          AEngine.Canvas.Line(Point2(X - 1, Y), Point2(X - 1, Y + AHeight),
            clBlack1);
      end;

      // Set Next X position
      X := X + AChars[Index].Width;
    end;

    // Set Rect Canvas
    AEngine.Canvas.ClipRect := ARect;
  end;
  }

  if ZFont <> nil then
  begin
    // Set Rect Canvas
    AEngine.Canvas.ClipRect := Rect(X - 1, Y, X + AWidth, Y + AHeight);

    // Get chars from text
    Index := 0;
    SetLength(AChars, Length(Text) + 1);
    while Index < Length(Text) do
    begin
      AChars[Index].Char  := Text[Index + 1];
      AChars[Index].Width := Round(ZFont.CharWidth(DC,Text[Index + 1]));
      //AChars[Index].Width := Round(ZFont.GetTextLength(DC,Text[Index + 1],1,1) + ZFont.Spacing);
      Inc(Index);
    end;

    // Set virtual Pos
    AVirtualCursor := 0;
    FVirtualPosition := 0;
    for Index := 0 to FSelection.EndPos - 1 do
    begin
      AVirtualCursor := AVirtualCursor + AChars[Index].Width;

    end;
    if AVirtualCursor > AWidth then
    begin
      FVirtualPosition := AWidth - AVirtualCursor;
      X := X + FVirtualPosition;
    end;

    AMin := Min(FSelection.StartPos, FSelection.EndPos);
    AMax := Max(FSelection.StartPos, FSelection.EndPos);

    ASelectColor := cColor4(FSelectColor.Top, FSelectColor.Top,
      FSelectColor.Bottom, FSelectColor.Bottom);

    // Draw Text char by char
    Y := Y + AHeight - ZFont.MaxHeight - Margin;

    for Index := 0 to High(AChars) do
    begin

      // Draw Selection
      if (AMin < AMax) then
      begin
        if (Index >= AMin) and (Index < AMax) then
          AEngine.Canvas.FillRect(Rect(X, Y, X + AChars[Index].Width,
              Y + AHeight), ASelectColor, deNormal);
        AFontColor := cColor2(FSelectFontColor);
      end;

      // Set Font Color
      if (AMin < AMax) then
      begin
        if (Index >= AMin) and (Index < AMax) then
          AFontColor := cColor2(FSelectFontColor)
        else
          AFontColor := cColor2(FontColor);
      end
      else
        AFontColor := cColor2(FontColor);

      // Draw char
      //AFont.TextOut(Point2(X, Y), AChars[Index].Char, AFontColor, 1.0);
      ZFont.Color := cColor4(AFontColor[0],AFontColor[0],AFontColor[1],AFontColor[1]);
      //if AChars[Index].Char <> #0 then
        ZFont.TextOut(DC,Point2px(X,Y),AChars[Index].Char);
      //ZCEdit

      // Draw Tic
      if (GetTic <= 1) and (AEngine.ActiveControl = Self) then
      begin
        if Index = FSelection.EndPos then
          AEngine.Canvas.Line(Point2(X-1 , Y), Point2(X-1 , Y + ZFont.MaxHeight),
            clBlack1);
      end;

      // Set Next X position
      X := X + AChars[Index].Width;
      //if Index = High(AChars) then ZFont.Flush(DC);
    end;

    // Set Rect Canvas
    AEngine.Canvas.ClipRect := ARect;
  end;
end;

procedure TCustomAEditBox.PasteFromClipboard;
var
  AText, CText: String;
  AMin, AMax, ALength: Integer;
begin
  AText := Text;

  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  ALength := AMax - AMin;

  Delete(AText, AMin + 1, ALength);
  CText := Clipboard.AsText;

  Inc(AMin);
  Insert(CText, AText, AMin);

  Text := AText;
  // Execute OnChange Event
  Change;

  Inc(AMin, Length(CText));

  if AMin > Length(Text) then
    AMin := Length(Text);

  FSelection.StartPos := AMin;
  FSelection.EndPos := AMin;
end;

procedure TCustomAEditBox.SelectAll;
begin
  if Enabled then
  begin
    FSelection.StartPos := 0;
    FSelection.EndPos := Length(Text);
  end;
end;

procedure TCustomAEditBox.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
    if (Value < Length(Text)) and (Value > 0) then
    begin
      Text := Copy(Text, 0, Value);
    end;
  end;
end;

procedure TCustomAEditBox.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
    FReadOnly := Value;
end;

procedure TCustomAEditBox.SetSelectColor(Value: TFontColor);
begin
  FSelectColor.Assign(Value);
end;

procedure TCustomAEditBox.SetSelectFontColor(Value: TFontColor);
begin
  FSelectFontColor.Assign(Value);
end;

procedure TCustomAEditBox.SetSelLength(Value: Integer);
begin
  FSelection.EndPos := FSelection.StartPos + Value;

  if FSelection.EndPos > Length(Text) then
    FSelection.EndPos := Length(Text);

  if FSelection.EndPos < 0 then
    FSelection.EndPos := 0;
end;

procedure TCustomAEditBox.SetSelStart(Value: Integer);
begin
  if not((Value < 0) and (Value > Length(Text))) then
  begin
    FSelection.StartPos := Value;
    FSelection.EndPos := Value;
  end;
end;

procedure TCustomAEditBox.SetSelText(Value: String);
var
  AText: String;
  AMin, AMax, ALength: Integer;
begin
  AText := Text;

  AMin := Min(FSelection.StartPos, FSelection.EndPos);
  AMax := Max(FSelection.StartPos, FSelection.EndPos);

  ALength := AMax - AMin;

  // Insert Key
  Delete(AText, AMin + 1, ALength);

  Inc(AMin);
  Insert(Value, AText, AMin);

  Text := AText;
  Change;

  if AMin > Length(Text) then
    AMin := Length(Text);

  FSelection.StartPos := AMin;
  FSelection.EndPos := AMin + Length(Value);
end;

procedure TCustomAEditBox.SetText(Value: String);
begin
  if (FMaxLength < Length(Value)) and (FMaxLength > 0) then
  begin
    Value := Copy(Value, 0, FMaxLength);
  end;

  inherited SetText(Value);
end;

initialization

RegisterClasses([TCustomAEditBox, TAEditBox]);

finalization

UnRegisterClasses([TCustomAEditBox, TAEditBox]);

end.
