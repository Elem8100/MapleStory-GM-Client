// ----------------------------------------------------------------------------
// ACtrlLabels.pas             Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TCustomADropPanel and TADropPanel.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TCustomADropPanel e TADropPanel.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlDropPanels;

interface

uses
  pxt.types,SysUtils, Types, Classes, Controls,Windows,
  // Aspryre units
    // Asphyre GUI Engine
  AControls, ACtrlTypes, WZIMGFile;

type
  PItem = ^TItem;

  TItem = record
    ID: Integer;
    Image: TWZIMGEntry;
    Quantity: Word;
  end;

  TItems = array of TItem;

  TCustomADropPanel = class(TWControl)
  private
    FCanDrag: Boolean;
    FColumns: Integer;
    FItems: TItems;
    FItemColor: TFillColor;
    FRows: Integer;
    FRowHeight: Integer;
    FRowWidth: Integer;
    FDragItem: Boolean;
    function GetItem(Index: Integer): PItem;
    procedure SetItemColor(Value: TFillColor);
    procedure SetColumns(Value: Integer);
    procedure SetRows(Value: Integer);
    procedure SetRowHeight(Value: Integer);
    procedure SetRowWidth(Value: Integer);
  protected
    function CellAtPos(const X, Y: Integer): TPoint;
    function ColAtPos(const X: Integer): Integer;
    function RowAtPos(const Y: Integer): Integer;
    function CellRect(const ACol, ARow: Integer): TIntRect;
    function ItemAtPos(const X, Y: Integer): PItem;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Paint(DC: HDC); override;
    procedure SetBorderWidth(Value: Word); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetMargin(Value: Word); override;
    procedure SetWidth(Value: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearAll;
    procedure ClearItem(Index: Integer);

    property CanDrag: Boolean read FCanDrag write FCanDrag;
    property DragItem: Boolean read FDragItem write FDragItem;
    property Items[Index: Integer]: PItem read GetItem;
    property ItemColor: TFillColor read FItemColor write SetItemColor;
    property Columns: Integer read FColumns write SetColumns;
    property Rows: Integer read FRows write SetRows;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property RowWidth: Integer read FRowWidth write SetRowWidth;
  end;

  TADropPanel = class(TCustomADropPanel)
  published
    property CanDrag;
    property Columns;
    property ItemColor;
    property Rows;
    property RowHeight;
    property RowWidth;

    property BorderColor;
    property BorderWidth;
    //property Color;
    property Enabled;
    property Font;
    property FontColor;
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
  end;

  TADropPanelClass = class of TADropPanel;

implementation
     uses PXT.Graphics;
var
  SelItem: PItem;
  XOffSet, YOffSet: Integer;

  { TCustomADropPanel }

constructor TCustomADropPanel.Create(AOwner: TComponent);
var
  Num, size: Integer;
begin
  ControlState := ControlState + [csCreating];

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    while AOwner.FindComponent('DropPanel' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'DropPanel' + IntToStr(Num);
  end;

  // fields
  FCanDrag := True;
  FColumns := 100;
  FRows := 100;
  FRowHeight := 32;
  FRowWidth := 32;

  FItemColor := TFillColor.Create;
  FItemColor.SetFillColor($FF4090F0, $FF4090F0, $FF4090F0, $FF4090F0);

  FDragItem := False;

  size := (FColumns * FRows);

  SetLength(FItems, size);
  ClearAll;

  // properties
  Left := 10;
  Top := 10;
  BorderColor := $80FFFFFF;
  BorderWidth := 0;
  //Color.SetFillColor($FFA6CAF0, $FFA6CAF0, $FFA6CAF0, $FFA6CAF0);
  Font := 'tahoma10b';
  FontColor:=ColorPairWhite;
  Margin := 1;
  //TabStop := False;

  ControlState := ControlState - [csCreating];
end;

destructor TCustomADropPanel.Destroy;
begin
  FItems := nil;
  FreeAndNil(FItemColor);

  inherited Destroy;
end;

function TCustomADropPanel.GetItem(Index: Integer): PItem;
begin
  Result := nil;

  if (Index < 0) or (Index >= Length(FItems)) then
    Exit;

  Result := @FItems[Index];
end;

function TCustomADropPanel.ItemAtPos(const X, Y: Integer): PItem;
var
  P: TPoint;
  Index: Integer;
begin
  Result := nil;
  P := CellAtPos(X, Y);

  if (P.X < 0) or (P.Y < 0) then
    Exit;

  Index := P.X + (P.Y * FColumns);

  Result := @FItems[Index];
end;

procedure TCustomADropPanel.MouseDown;
var
  Item: PItem;
begin
  Self.BringToFront;

  if Button = mbLeft then
  begin
    Item := ItemAtPos(X, Y);

    if Item <> nil then
    begin
      if Item.ID <> -1 then
      begin
        SelItem := Item;
      end
      else
        SelItem := nil;
    end
    else
      SelItem := nil;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomADropPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (SelItem <> nil) and (Shift = [ssLeft]) and (CanDrag) then
  begin
    if not(DragItem) then
      DragItem := True;
    XOffSet := X - 4;
    YOffSet := Y - 4;
  end;

  inherited;
end;

procedure TCustomADropPanel.MouseUp;
var
  Item: PItem;
  PrevItem: TItem;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (DragItem) or ((AEngine.PreviousControl as TCustomADropPanel).DragItem) then
  begin
    Item := ItemAtPos(X, Y);

    if SelItem = Item then
      SelItem := nil;

    if (Item <> nil) and (SelItem <> nil) then
    begin
      if Item.ID <> -1 then
      begin
        PrevItem.ID := Item.ID;
        PrevItem.Image := Item.Image;
        PrevItem.Quantity := Item.Quantity;

        Item.ID := SelItem.ID;
        Item.Image := SelItem.Image;
        Item.Quantity := SelItem.Quantity;

        SelItem.ID := PrevItem.ID;
        SelItem.Image := PrevItem.Image;
        SelItem.Quantity := PrevItem.Quantity;
      end
      else
      begin
        Item.ID := SelItem.ID;
        Item.Image := SelItem.Image;
        Item.Quantity := SelItem.Quantity;

        SelItem.ID := -1;
        SelItem.Image := nil;
        SelItem.Quantity := 0;
      end;
    end;

    SelItem := nil;
    DragItem := False;
  end;
end;

procedure TCustomADropPanel.AssignTo(Dest: TPersistent);
begin
  ControlState := ControlState + [csReadingState];

  inherited AssignTo(Dest);

  if Dest is TCustomADropPanel then
    with TCustomADropPanel(Dest) do
    begin
      Columns := Self.Columns;
      ItemColor := Self.ItemColor;
      Rows := Self.Rows;
      RowHeight := Self.RowHeight;
      RowWidth := Self.RowWidth;
    end;

  ControlState := ControlState - [csReadingState];
end;

function TCustomADropPanel.CellAtPos(const X, Y: Integer): TPoint;
begin
  Result := Point(ColAtPos(X), RowAtPos(Y));
end;

function TCustomADropPanel.CellRect(const ACol, ARow: Integer): TIntRect;
var
  CL, CT: Integer;
  L, T, W, H: Integer;
begin
  Result := IntRectBDS(0, 0, 0, 0);

  if (ACol < 0) or (ACol >= FColumns) then
    Exit;
  if (ARow < 0) or (ARow >= FRows) then
    Exit;

  CL := ClientLeft + BorderWidth + Margin;
  CT := ClientTop + BorderWidth + Margin;

  // Calc all values
  L := CL + (ACol * Margin) + (FRowWidth * ACol);
  T := CT + (ARow * Margin) + (FRowHeight * ARow);
  W := (CL + (ACol * Margin) + (FRowWidth * ACol)) + (FRowWidth);
  H := (CT + (ARow * Margin) + (FRowHeight * ARow)) + (FRowHeight);

  Result := IntRectBDS(L, T, W, H);
end;

procedure TCustomADropPanel.ClearAll;
var
  I: Integer;
begin
  if Length(FItems) = 0 then
    Exit;

  for I := 0 to Length(FItems) - 1 do
  begin
    FItems[I].ID := -1;
    FItems[I].Image := nil;
    FItems[I].Quantity := 0;
  end;
end;

procedure TCustomADropPanel.ClearItem(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FItems)) then
    Exit;

  FItems[Index].ID := -1;
  FItems[Index].Image := nil;
  FItems[Index].Quantity := 0;
end;

function TCustomADropPanel.ColAtPos(const X: Integer): Integer;
var
  Col, I: Integer;
begin
  Col := -1;

  for I := 0 to FColumns - 1 do
  begin
    if ((X - ClientLeft) >= (FRowWidth * I)
        + BorderWidth + Margin + I * Margin) and
      ((X - ClientLeft) < (FRowWidth * (I + 1)) + BorderWidth +
        (I + 1) * Margin) then
    begin
      Col := I;
      Break;
    end;
  end;

  Result := Col;
end;

function TCustomADropPanel.RowAtPos(const Y: Integer): Integer;
var
  Row, I: Integer;
begin
  Row := -1;

  for I := 0 to FRows - 1 do
  begin
    if ((Y - ClientTop) >= (FRowHeight * I)
        + BorderWidth + Margin + I * Margin) and
      ((Y - ClientTop) < (FRowHeight * (I + 1)) + BorderWidth +
        (I + 1) * Margin) then
    begin
      Row := I;
      Break;
    end;
  end;

  Result := Row;
end;

procedure TCustomADropPanel.Paint(DC: HDC);
var
  X, Y: Integer;
  I, J, Index: Integer;
 // Img: TAsphyreImage;
  Img: TTexture;
begin
  // Set initial values
  X := ClientLeft;
  Y := ClientTop;

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
    //AEngine.Canvas.UseTexturePx(AImage,
     // pxBounds4(0 + BorderWidth, 0 + BorderWidth,
   //     AImage.Width - (BorderWidth * 2),
 //       AImage.Height - (BorderWidth * 2)));

        var TexCoord := Quad( 0 + BorderWidth, 0 + BorderWidth,
        AImage.Parameters.Width - (BorderWidth * 2),
        AImage.Parameters.Height - (BorderWidth * 2));
         AEngine.Canvas.Quad(AImage, Quad(IntRectBDS(X + BorderWidth, Y + BorderWidth, X + Width -
      BorderWidth, Y + Height - BorderWidth)), TexCoord,$FFFFFFFF);

  //  AEngine.Canvas.TexMap(pRect4(Rect(X + BorderWidth, Y + BorderWidth,
    //      X + Width - BorderWidth, Y + Height - BorderWidth)),
    //  cAlpha4(ImageAlpha), deNormal);
  end
  else
  begin
  //  AEngine.Canvas.FillRect(FloatRect(X + BorderWidth, Y + BorderWidth,
   //     X + Width - BorderWidth, Y + Height - BorderWidth), Cardinal(Color));
  end;

  // Draw Panels
  Index := 0;
  for I := 0 to FRows - 1 do
  begin
    for J := 0 to FColumns - 1 do
    begin
      if FItems[Index].ID <> -1 then
      begin
        //Img := AEngine.Images.Image[string(FItems[Index].Image)];
         Img := AEngine.ImageLib[FItems[Index].Image];
        if Img.Initialized then
        begin
        //  AEngine.Canvas.UseTexturePx(Img, pxBounds4(0, 0, Img.Width, Img.Height));
          var TexCoord := Quad(0, 0, AImage.Parameters.Width, AImage.Parameters.Height);


          if (@FItems[Index] = SelItem) and (DragItem) then
          //  AEngine.Canvas.TexMap(pRect4(CellRect(J, I)), cColor4($FFBBBBBB),
            //  deNormal)
             AEngine.Canvas.Quad(AImage, Quad(CellRect(J, I)), TexCoord,$FFBBBBBB)
          else
             AEngine.Canvas.Quad(AImage, Quad(CellRect(J, I)), TexCoord,$FFFFFFFF);
           // AEngine.Canvas.TexMap(pRect4(CellRect(J, I)), cColor4($FFFFFFFF),
             // deNormal);



        end
        else
        begin
          if AImage.Initialized then
            AEngine.Canvas.FillRect(CellRect(J, I), cardinal(FItemColor));
        end;
      end
      else
      begin
        if AImage.Initialized then
          AEngine.Canvas.FillRect(CellRect(J, I), cardinal(FItemColor));
      end;
      Index := Index + 1;
    end;
  end;

  if (SelItem <> nil) and (DragItem) then
  begin
    //Img := AEngine.Images.Image[string(SelItem.Image)];
    Img := AEngine.ImageLib[(SelItem.Image)];
    if Img.Initialized then
    begin
     // AEngine.Canvas.UseTexturePx(Img, pxBounds4(0, 0, Img.Width, Img.Height));
      //AEngine.Canvas.TexMap(pRect4(Rect(XOffSet, YOffSet, XOffSet + FRowWidth,
          //  YOffSet + FRowHeight)), cColor4($FFFFFFFF), deNormal);
       var TexCoord := Quad(0, 0, AImage.Parameters.Width, AImage.Parameters.Height);
        AEngine.Canvas.Quad(AImage,Quad(IntRectBDS(XOffSet, YOffSet, XOffSet + FRowWidth,
            YOffSet + FRowHeight)) , TexCoord,$FFFFFFFF);
    end;
  end;
end;

procedure TCustomADropPanel.SetBorderWidth(Value: Word);
begin
  inherited SetBorderWidth(Value);

  // Update Height
  Height := (FRowHeight * FRows) + ((FRows - 1) * Margin) + (BorderWidth * 2) +
    (Margin * 2);

  // Update Width
  Width := (FRowWidth * FColumns) + ((FColumns - 1) * Margin) +
    (BorderWidth * 2) + (Margin * 2);
end;

procedure TCustomADropPanel.SetColumns(Value: Integer);
var
  size: Integer;
begin
  if Value < 1 then
    Value := 1;

  FColumns := Value;

  size := (FColumns * FRows);

  SetLength(FItems, size);
  ClearAll;

  // Update Width
  Width := (FRowWidth * FColumns) + ((FColumns - 1) * Margin) +
    (BorderWidth * 2) + (Margin * 2);
end;

procedure TCustomADropPanel.SetHeight(Value: Integer);
begin
  // Set Default Value
  Value := (FRowHeight * FRows) + ((FRows - 1) * Margin) + (BorderWidth * 2) +
    (Margin * 2);

  inherited SetHeight(Value);
end;

procedure TCustomADropPanel.SetItemColor(Value: TFillColor);
begin
  FItemColor.Assign(Value);
end;

procedure TCustomADropPanel.SetMargin(Value: Word);
begin
  if Value < 1 then
    Value := 1;

  inherited SetMargin(Value);

  // Update Height
  Height := (FRowHeight * FRows) + ((FRows - 1) * Margin) + (BorderWidth * 2) +
    (Margin * 2);

  // Update Width
  Width := (FRowWidth * FColumns) + ((FColumns - 1) * Margin) +
    (BorderWidth * 2) + (Margin * 2);
end;

procedure TCustomADropPanel.SetRowHeight(Value: Integer);
begin
  if Value < 8 then
    Value := 8;

  FRowHeight := Value;
  // Update Height
  Height := (FRowHeight * FRows) + ((FRows - 1) * Margin) + (BorderWidth * 2) +
    (Margin * 2);
end;

procedure TCustomADropPanel.SetRows(Value: Integer);
var
  size: Integer;
begin
  if Value < 1 then
    Value := 1;

  FRows := Value;
  ClearAll;

  size := (FColumns * FRows);

  SetLength(FItems, size);

  // Update Height
  Height := (FRowHeight * FRows) + ((FRows - 1) * Margin) + (BorderWidth * 2) +
    (Margin * 2);
end;

procedure TCustomADropPanel.SetRowWidth(Value: Integer);
begin
  if Value < 8 then
    Value := 8;

  FRowWidth := Value;
  // Update Width
  Width := (FRowWidth * FColumns) + ((FColumns - 1) * Margin) +
    (BorderWidth * 2) + (Margin * 2);
end;

procedure TCustomADropPanel.SetWidth(Value: Integer);
begin
  // Set Default Value
  Value := (FRowWidth * FColumns) + ((FColumns - 1) * Margin) +
    (BorderWidth * 2) + (Margin * 2);

  inherited SetWidth(Value);
end;

initialization

RegisterClasses([TCustomADropPanel, TADropPanel]);

finalization

UnRegisterClasses([TCustomADropPanel, TADropPanel]);

end.
