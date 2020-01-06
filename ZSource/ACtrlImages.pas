// ----------------------------------------------------------------------------
// ACtrlImages.pas             Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomAImage and TAImage.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomAImage e TAImage.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlImages;

interface

uses
  Classes, Controls, SysUtils, Windows,
  // Aspryre units
  AbstractCanvas, AsphyreFonts, AsphyreImages, AsphyreTypes, Vectors2,
  // Asphyre GUI Engine
  AControls, ACtrlForms;

type
  TCustomAImage = class(TAControl)
  private
    FCanMoveHandle: Boolean;
    FFocusControl: string;
    FID: string;
    procedure SetCanMoveHandle(Value: Boolean); virtual;
    procedure SetFocusControl(Value: string); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; override;
    procedure Paint(DC: HDC); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ID: string read FID write FID;
    property CanMoveHandle: Boolean read FCanMoveHandle write SetCanMoveHandle;
    property FocusControl: string read FFocusControl write SetFocusControl;
  end;

  TAImage = class(TCustomAImage)
  published
    property CanMoveHandle;
    property FocusControl;

    property BorderColor;
    property BorderWidth;
    property ClientRect;
    property Enabled;
    property Height;
    //property Image;
    property ImageAlpha;
    property Left;
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

  TAImageClass = class of TAImage;

implementation

var
  XOffSet, YOffSet: Integer;

  { TCustomAImage }

procedure TCustomAImage.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomAImage then
  with TCustomAImage(Dest) do
  begin
    CanMoveHandle := Self.CanMoveHandle;
    FocusControl := Self.FocusControl;
  end;

  ControlState := ControlState - [csReadingState];
end;

procedure TCustomAImage.Click;
var
  Control: TAControl;
begin
  Control := Self.Handle.FindChildControl(FFocusControl, True);
  if Control <> nil then
    if Control is TWControl then
      TWControl(Control).SetFocus;

  inherited Click;
end;

constructor TCustomAImage.Create(AOwner: TComponent);
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
      while AOwner.FindComponent('Image' + IntToStr(Num)) <> nil do
        Inc(Num);
      Name := 'Image' + IntToStr(Num);
    end;
  end;

  // Properties
  Left := 0;
  Top := 0;
  Width := 80;
  Height := 80;

  BorderColor := $80FFFFFF;
  BorderWidth := 1;
  Visible := True;

  // Fields
  FCanMoveHandle := True;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomAImage.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomAImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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

procedure TCustomAImage.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TCustomAImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  // Stop move the form Handle
  if (FCanMoveHandle) and (Handle is TAForm) then
  begin
    if (Button = mbLeft) and (TAForm(Handle).IsMoving) then
      TAForm(Handle).IsMoving := False;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomAImage.Paint(DC: HDC);
var
  X, Y: Integer;
begin
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

  if AImage <> nil then
  begin
    AEngine.Canvas.UseTexturePx(AImage,
      pxBounds4(0 + BorderWidth, 0 + BorderWidth,
        AImage.Width - (BorderWidth * 2),
        AImage.Height - (BorderWidth * 2)));
    AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth, Y + BorderWidth,
          X + Width - BorderWidth, Y + Height - BorderWidth)),
      cAlpha4(ImageAlpha), deNormal);
  end;
end;

procedure TCustomAImage.SetCanMoveHandle(Value: Boolean);
begin
  if FCanMoveHandle <> Value then
    FCanMoveHandle := Value;
end;

procedure TCustomAImage.SetFocusControl(Value: string);
begin
  if FFocusControl <> Value then
    FFocusControl := Value;
end;

initialization

RegisterClasses([TCustomAImage, TAImage]);

finalization

UnRegisterClasses([TCustomAImage, TAImage]);

end.
