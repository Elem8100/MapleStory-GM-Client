// ----------------------------------------------------------------------------
// ACtrlTypes.pas            Modified: 02-10-2010                 Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of helper functions and procedures.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de funções e rotinas de apoio.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlTypes;

interface

uses
  Classes, controls,
  // Asphyre units
  PXT.Graphics,  WZIMGFile;

type
  // --------------------------------------------------------------------------
  // TSelection, TAChar and TAChars
  // --------------------------------------------------------------------------
  TANotifyEvent = reference to procedure(Sender: TObject);

  TAMouseEvent = reference to procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  TAMouseMoveEvent = reference to procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  TAKeyEvent = reference to procedure(Sender: TObject; var Key: Word; Shift: TShiftState);

  TSelection = record
    StartPos, EndPos: Integer;
  end;

  TAChar = record
    Char: Char;
    Width: Integer;
  end;

  TAChars = array of TAChar;
  // TEditCharCase = (ecNormal, ecUpperCase, ecLowerCase);


  // --------------------------------------------------------------------------
  // TColorType, TAlphaColor, TBorderColor, TFontColor and TFillColor
  // --------------------------------------------------------------------------

  // TColorType

  TColorType = (ctSingle, ctLeftRight, ctTopBottom, ctFull);

  // TAlphaColor
  TAlphaColor = Cardinal;

  // TBorderColor
  TBorderColor = Cardinal;

  // TFontColor
  TFontColor = class(TPersistent)
  private
    fa: Cardinal;
    fb: Cardinal;
    procedure SetA(c: Cardinal);
    procedure SetB(c: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    procedure SetFontColor(a, b: Cardinal); overload;
   // procedure SetFontColor(c: TColor2); overload;
  published
    property Top: Cardinal read fa write SetA;
    property Bottom: Cardinal read fb write SetB;
  end;

  // TFillColor
  TFillColor = class(TPersistent)
  private
    fa: Cardinal;
    fb: Cardinal;
    fc: Cardinal;
    fd: Cardinal;
    procedure SetA(c: Cardinal);
    procedure SetB(c: Cardinal);
    procedure SetC(c: Cardinal);
    procedure SetD(c: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    procedure SetFillColor(a, b, c, d: Cardinal); overload;
   // procedure SetFillColor(c: TColor4); overload;
  published
    property TopLeft: Cardinal read fa write SetA;
    property TopRight: Cardinal read fb write SetB;
    property BottomRight: Cardinal read fc write SetC;
    property BottomLeft: Cardinal read fd write SetD;
  end;

  // --------------------------------------------------------------------------
  // TUpDownType, TUpDownButton
  // --------------------------------------------------------------------------

  // TUpDownType
  TUpDownType = (udUp, udDown, udNone);

  // TUpDownButton
  TUpDownButton = class(TPersistent)
  private
    FColor: TFillColor;
    FColorHover: TFillColor;
    FColorPressed: TFillColor;
    FEnabled: Boolean;
    FHover: Boolean;
    FIconColor: Cardinal;
    FImage: TWZIMGEntry;
    FImageHover: TWZIMGEntry;
    FImagePressed: TWZIMGEntry;
    FPressed: Boolean;
    FOnImageChange: TNotifyEvent;
    FType: TUpDownType;
    FWidth: Word;
    procedure SetColor(Value: TFillColor);
    procedure SetColorHover(Value: TFillColor);
    procedure SetColorPressed(Value: TFillColor);
    procedure SetIconColor(Value: Cardinal);
    procedure SetImage(Value: TWZIMGEntry);
    procedure SetImageHover(Value: TWZIMGEntry);
    procedure SetImagePressed(Value: TWZIMGEntry);
    procedure SetType(Value: TUpDownType);
    procedure SetHover(Value: Boolean);
    procedure SetPressed(Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    FAImage: TTExture;
    FAImageHover: TTExture;
    FAImagePressed: TTexture;
    constructor Create;
    destructor Destroy; override;
    function AImage: TTexture;
    property ButtonType: TUpDownType read FType write SetType;
    property IsEnabled: Boolean read FEnabled write FEnabled;
    property IsHover: Boolean read FHover write SetHover;
    property IsPressed: Boolean read FPressed write SetPressed;
    property OnImageChange: TNotifyEvent read FOnImageChange write FOnImageChange;
  published
    property Color: TFillColor read FColor write SetColor;
    property ColorHover: TFillColor read FColorHover write SetColorHover;
    property ColorPressed: TFillColor read FColorPressed write SetColorPressed;
    property IconColor: Cardinal read FIconColor write SetIconColor;
    property Image: TWZIMGEntry read FImage write SetImage;
    property ImageHover: TWZIMGEntry read FImageHover write SetImageHover;
    property ImagePressed: TWZIMGEntry read FImagePressed write SetImagePressed;
    property Width: Word read FWidth write FWidth;
  end;

  // --------------------------------------------------------------------------


// ----------------------------------------------------------------------------

implementation

// ----------------------------------------------------------------------------
{ TFontColor }

procedure TFontColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TFontColor then
    with TFontColor(Dest) do
    begin
      fa := Self.fa;
      fb := Self.fb;
    end
  else
    inherited;
end;

constructor TFontColor.Create;
begin
  inherited;

  // Set white color for all subcolors
  fa := $FFFFFFFF;
  fb := $FFFFFFFF;
end;

procedure TFontColor.SetA(c: Cardinal);
begin
  if fa <> c then
    fa := c;
end;

procedure TFontColor.SetB(c: Cardinal);
begin
  if fb <> c then
    fb := c;
end;

procedure TFontColor.SetFontColor(a, b: Cardinal);
begin
  fa := a;
  fb := b;
end;



// ----------------------------------------------------------------------------
{ TFillColor }

procedure TFillColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TFillColor then
    with TFillColor(Dest) do
    begin
      fa := Self.fa;
      fb := Self.fb;
      fc := Self.fc;
      fd := Self.fd;
    end
  else
    inherited;
end;

constructor TFillColor.Create;
begin
  inherited Create;

  // Set white color for all subcolors
  fa := $FFFFFFFF;
  fb := $FFFFFFFF;
  fc := $FFFFFFFF;
  fd := $FFFFFFFF;
end;

procedure TFillColor.SetA(c: Cardinal);
begin
  if fa <> c then
    fa := c;
end;

procedure TFillColor.SetB(c: Cardinal);
begin
  if fb <> c then
    fb := c;
end;

procedure TFillColor.SetC(c: Cardinal);
begin
  if fc <> c then
    fc := c;
end;

procedure TFillColor.SetD(c: Cardinal);
begin
  if fd <> c then
    fd := c;
end;

procedure TFillColor.SetFillColor(a, b, c, d: Cardinal);
begin
  fa := a;
  fb := b;
  fc := c;
  fd := d;
end;



// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------

{ TUpDownButton }

function TUpDownButton.AImage: TTexture;
begin
  Result := FAImage;

  if FHover then
  begin
    Result := FAImageHover;
  end;

  if FPressed then
  begin
    Result := FAImagePressed;
  end;
end;

procedure TUpDownButton.AssignTo(Dest: TPersistent);
begin
  if Dest is TUpDownButton then
    with TUpDownButton(Dest) do
    begin
      // FAImage  - is set by Image (OnImageChange must be defined)
      // FAImageHover  - is set by Image (OnImageChange must be defined)
      // FAImagePressed  - is set by Image (OnImageChange must be defined)
      // FEnabled - dynamic
      // FHover  - dynamic
      // FPressed  - dynamic

      Color := Self.Color;
      ColorHover := Self.ColorHover;
      ColorPressed := Self.ColorPressed;
      IconColor := Self.IconColor;
      Image := Self.Image;
      ImageHover := Self.ImageHover;
      ImagePressed := Self.ImagePressed;
      ButtonType := Self.ButtonType;
      Width := Self.Width;
    end
  else
    inherited;
end;

constructor TUpDownButton.Create;
begin
  // Set Fields
  //FAImage := nil;
 // FAImageHover := nil;
  //FAImagePressed := nil;

  FColor := TFillColor.Create;
  FColor.SetFillColor($FFA6CAF0, $FF4090F0, $FF4090F0, $FFA6CAF0);

  FColorHover := TFillColor.Create;
  FColorHover.SetFillColor($FFB6DAF0, $FF409AF0, $FF409AF0, $FFB6DAF0);

  FColorPressed := TFillColor.Create;
  FColorPressed.SetFillColor($FF2060F0, $FF2060F0, $FF2060F0, $FF2060F0);

  FIconColor := $FFB6DAF0;

  FImage := nil;
  FImageHover := nil;
  FImagePressed := nil;

  FEnabled := True;
  FHover := False;
  FPressed := False;

  FType := udNone;
  FWidth := 16;
end;

destructor TUpDownButton.Destroy;
begin
  FColor.Free;
  FColorHover.Free;
  FColorPressed.Free;

  inherited;
end;

procedure TUpDownButton.SetColor(Value: TFillColor);
begin
  FColor.Assign(Value);
end;

procedure TUpDownButton.SetColorHover(Value: TFillColor);
begin
  FColorHover.Assign(Value);
end;

procedure TUpDownButton.SetColorPressed(Value: TFillColor);
begin
  FColorPressed.Assign(Value);
end;

procedure TUpDownButton.SetHover(Value: Boolean);
begin
  FHover := Value;

  if assigned(FOnImageChange) then
    FOnImageChange(Self);
end;

procedure TUpDownButton.SetIconColor(Value: Cardinal);
begin
  FIconColor := Value;
end;

procedure TUpDownButton.SetImage(Value: TWZIMGEntry);
begin
  FImage := Value;

  if assigned(FOnImageChange) then
    FOnImageChange(Self);
end;

procedure TUpDownButton.SetImageHover(Value: TWZIMGEntry);
begin
  FImageHover := Value;

  if assigned(FOnImageChange) then
    FOnImageChange(Self);
end;

procedure TUpDownButton.SetImagePressed(Value: TWZIMGEntry);
begin
  FImagePressed := Value;

  if assigned(FOnImageChange) then
    FOnImageChange(Self);
end;

procedure TUpDownButton.SetPressed(Value: Boolean);
begin
  FPressed := Value;

  if assigned(FOnImageChange) then
    FOnImageChange(Self);
end;

procedure TUpDownButton.SetType(Value: TUpDownType);
begin
  FType := Value;
end;

initialization
  RegisterClasses([TFillColor, TFontColor, TUpDownButton]);

finalization
  UnRegisterClasses([TFillColor, TFontColor, TUpDownButton]);

end.

