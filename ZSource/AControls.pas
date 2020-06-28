// ----------------------------------------------------------------------------
// AControls.pas             Modified: 02-10-2010                 Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TControlEngine, TAControl and TWControl.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TControlEngine, TAControl e TWControl.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit AControls;

interface

uses
  PXT.Types,Windows, SysUtils, Classes, Controls,
  // Asphyre units
   Generics.Collections,WZIMGFile,
  // Asphyre GUI Engine
  ACtrlTypes,PXT.Graphics,PXT.Canvas;

type
  { Forward declarations }
  TCustomEngine = class;
  TAControl = class;
  TWControl = class;

  { TComponentEngine }
  TCustomEngine = class
  private
    { Private declarations }
    FDevice: TDevice;
    FCanvas: TGameCanvas;
    //FImages: TAsphyreImages;
    //FFonts: TAsphyreFonts;
    FImageLib: TDictionary<TWZIMGEntry, TTexture>;
    FParent: TControl;

    FActiveControl: TWControl;
    FPrevControl: TAControl;
    FRoot: TWControl;
    FActive: Boolean;
    FLoading: Boolean;
    FDesign: Boolean;

    FOwnerClick: TNotifyEvent;
    FOwnerDblClick: TNotifyEvent;
    FOwnerKeyDown: TKeyEvent;
    FOwnerKeyPress: TKeyPressEvent;
    FOwnerKeyUp: TKeyEvent;
    FOwnerMouseLeave: TNotifyEvent;
    FOwnerMouseEnter: TNotifyEvent;
    FOwnerMouseDown: TMouseEvent;
    FOwnerMouseUp: TMouseEvent;
    FOwnerMouseMove: TMouseMoveEvent;
    FOwnerMouseWheel: TMouseWheelEvent;
    FOwnerMouseWheelDown: TMouseWheelUpDownEvent;
    FOwnerMouseWheelUp: TMouseWheelUpDownEvent;

    procedure AcquireEvents;
    procedure RestoreEvents;
    procedure SetActiveControl(const Control: TWControl);
    procedure SetDesign(const Value: Boolean);
    procedure SetLoading(const Value: Boolean);
    procedure SetParent(const Control: TControl);
  protected
    procedure Click(Sender: TObject);
    procedure DblClick(Sender: TObject);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseEnter(Sender: TObject);
    procedure MouseLeave(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(const AOwner: TControl; const ADevice: TDevice;
      const ACanvas: TGameCanvas);

    destructor Destroy; override;

    //function LoadFontsFromFile(const FileName: string): Boolean;
    //function LoadImagesFromFile(const FileName: string): Boolean;

    procedure Render(DC: HDC);
    procedure Clear;

    property Active: Boolean read FActive write FActive;
    property ActiveControl
      : TWControl read FActiveControl write SetActiveControl;
    property PreviousControl: TAControl read FPrevControl write FPrevControl;

    property DesignMode: Boolean read FDesign write SetDesign;
    property Loading: Boolean read FLoading write SetLoading;

    property Root: TWControl read FRoot; // The root container that contains all the components

    property Device: TDevice read FDevice;
    property Canvas: TGameCanvas read FCanvas;
    //property Fonts: TAsphyreFonts read FFonts;
    //property Images: TAsphyreImages read FImages;
    property ImageLib: TDictionary<TWZIMGEntry, TTexture> read FImageLib write FImageLib;
    property Parent: TControl read FParent write SetParent;
  end;

  { TAControl }

  TAControl = class(TComponent)
  private
    FAEngine: Pointer;
    //FAFont: Pointer;
    FAImage: TTexture;
    FEnabled: Boolean;
    FBorderColor: TBorderColor;
    FBorderWidth: Word;
   // FColor: TFillColor;
    FControlState: TControlState;
    FFont: ShortString;
    FFontColor: TColorPair;
    FHandle: Pointer;
    FHeight: Integer;
    //FImage: ShortString;
    FImageEntry: TWZIMGEntry;
    FImageAlpha: TAlphaColor;
    FLeft: Integer;
    FMargin: Word;
    FOnClick: TANotifyEvent;
    FOnDblClick: TANotifyEvent;
    FOnMouseLeave: TANotifyEvent;
    FOnMouseEnter: TANotifyEvent;
    FOnMouseDown: TAMouseEvent;
    FOnMouseMove: TAMouseMoveEvent;
    FOnMouseUp: TAMouseEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnResize: TANotifyEvent;
    FParent: TWControl;
    FText: String;
    FTop: Integer;
    FWheelAccumulator: Integer;
    FWidth: Integer;
    FVisible: Boolean;

    procedure SetBorderColor(Color: TBorderColor);
    procedure SetColor(Color: TFillColor);
    procedure SetFont(Name: ShortString);

    procedure SetImage(Entry: TWZIMGEntry);
    procedure SetImageAlpha(Color: TAlphaColor);
    procedure SetLeft(Value: Integer);

    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);

    procedure SetZOrderPosition(Position: Integer);
  protected
    //ZC Edit

    procedure SetBorderWidth(Value: Word); virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetMargin(Value: Word); virtual;
    procedure SetWidth(Value: Integer); virtual;

    function GetClientLeft: Integer; virtual;
    function GetClientRect: TRect; virtual;
    function GetClientTop: Integer; virtual;
    function GetEnabled: Boolean; virtual;

    function GetHandle: TWControl; virtual;
    function IsVisible: Boolean; virtual;

    function MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; dynamic;
    function MouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      dynamic;
    function MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      dynamic;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Click; dynamic;
    procedure DblClick; dynamic;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      dynamic;
    procedure Paint(DC: HDC); dynamic; abstract;
    procedure ReadState(Reader: TReader); override;
    procedure Resize; dynamic;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetParent(AParent: TWControl); virtual;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetText(Text: String); virtual;
    procedure SetZOrder(TopMost: Boolean); dynamic;

    property OnClick: TANotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TANotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TAMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TANotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TANotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TAMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TAMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel
      : TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown
      : TMouseWheelUpDownEvent read FOnMouseWheelDown write
      FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write
      FOnMouseWheelUp;
    property OnResize: TANotifyEvent read FOnResize write FOnResize;
    property Text: String read FText write SetText;
    property WheelAccumulator
      : Integer read FWheelAccumulator write FWheelAccumulator;
  public


    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



  //  function AFont: TAsphyreFont; dynamic;
    function AImage: TTexture; dynamic;
    function AEngine: TCustomEngine;

    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    function GetTextLen: Integer;

    procedure Assign(Source: TPersistent); override;
    procedure BringToFront;
    procedure SendToBack;
    procedure SetAEngine(AEngine: TCustomEngine); virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;

    property BorderColor: TBorderColor read FBorderColor write SetBorderColor;
    property BorderWidth: Word read FBorderWidth write SetBorderWidth;
    property ClientLeft: Integer read GetClientLeft;
    property ClientTop: Integer read GetClientTop;
    property ClientRect: TRect read GetClientRect;
    //property Color: TFillColor read FColor write SetColor;
    property ControlState: TControlState read FControlState write FControlState;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Engine: TCustomEngine read AEngine write SetAEngine;
    property Font: ShortString read FFont write SetFont;
    property FontColor: TColorPair read FFontColor write FFontColor;
    property Handle: TWControl read GetHandle;
    //property Image: ShortString read FImage write SetImage;
    property ImageEntry: TWZIMGEntry read FImageEntry write FImageEntry;
    property ImageAlpha: TAlphaColor read FImageAlpha write SetImageAlpha;
    property Margin: Word read FMargin write SetMargin;
    property Parent: TWControl read FParent write SetParent;
    property Visible: Boolean read FVisible write SetVisible;
  published
    property Height: Integer read FHeight write SetHeight;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  end;

  TAControlClass = class of TAControl;

  { TWControl }

  TWControl = class(TAControl)
  private
    FControls: TList;
    FTabList: TList;
    FWControls: TList;
    FTabStop: Boolean;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TAKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    function GetControl(Index: Integer): TAControl;
    function GetControlCount: Integer;
    function GetTabOrder: TTabOrder;
    procedure Insert(AControl: TAControl);
    procedure Remove(AControl: TAControl);
    procedure SetTabOrder(Value: TTabOrder);
    procedure SetTabStop(Value: Boolean);
    procedure SetZOrderPosition(Position: Integer);
    procedure UpdateTabOrder(Value: TTabOrder);
  protected
    function FindNextControl(CurControl: TWControl; GoForward, CheckTabStop,
      CheckParent: Boolean): TWControl;
    function GetChildOwner: TComponent; override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoEnter; dynamic;
    procedure DoExit; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: Char); dynamic;
    procedure Paint(DC: HDC); override;

    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetZOrder(TopMost: Boolean); override;

    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TAKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanFocus: Boolean; dynamic;
    function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean;
      AllowWControls: Boolean = False; AllLevels: Boolean = False): TAControl;
    function FindChildControl(const ControlName: string;
      AllLevels: Boolean = False): TAControl;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure GetTabOrderList(List: TList); dynamic;
    procedure InsertControl(AControl: TAControl);
    procedure RemoveControl(AControl: TAControl);
    procedure SelectFirst;
    procedure SelectNext(CurControl: TWControl;
      GoForward, CheckTabStop: Boolean);
    procedure SetFocus; virtual;

    property Controls[Index: Integer]: TAControl read GetControl;
    property ControlCount: Integer read GetControlCount;

    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder;
    property TabStop: Boolean read FTabStop write SetTabStop;
  end;

  TWControlClass = class of TWControl;

implementation

uses
  Forms,
  // Asphyre GUI Engine
  ACtrlForms, ACtrlDropPanels, ACtrlHelpers,Global;

var
  VirtualPoint: TPoint;

  // ----------------------------------------------------------------------------
  { TCustomEngine }

procedure TCustomEngine.AcquireEvents;
begin
  if Parent is TForm then
  begin
    FOwnerMouseEnter := TForm(Parent).OnMouseEnter;
    FOwnerMouseLeave := TForm(Parent).OnMouseLeave;
    FOwnerMouseDown := TForm(Parent).OnMouseDown;
    FOwnerMouseUp := TForm(Parent).OnMouseUp;
    FOwnerMouseMove := TForm(Parent).OnMouseMove;
    FOwnerMouseWheel := TForm(Parent).OnMouseWheel;
    FOwnerMouseWheelDown := TForm(Parent).OnMouseWheelDown;
    FOwnerMouseWheelUp := TForm(Parent).OnMouseWheelUp;

    FOwnerKeyDown := TForm(Parent).OnKeyDown;
    FOwnerKeyUp := TForm(Parent).OnKeyUp;
    FOwnerKeyPress := TForm(Parent).OnKeyPress;

    FOwnerClick := TForm(Parent).OnClick;
    FOwnerDblClick := TForm(Parent).OnDblClick;

    TForm(Parent).OnMouseDown := MouseDown;
    TForm(Parent).OnMouseUp := MouseUp;
    TForm(Parent).OnMouseMove := MouseMove;
    TForm(Parent).OnMouseEnter := MouseEnter;
    TForm(Parent).OnMouseLeave := MouseLeave;
    TForm(Parent).OnMouseWheel := MouseWheel;
    TForm(Parent).OnMouseWheelDown := MouseWheelDown;
    TForm(Parent).OnMouseWheelUp := MouseWheelUp;

    TForm(Parent).OnKeyDown := KeyDown;
    TForm(Parent).OnKeyUp := KeyUp;
    TForm(Parent).OnKeyPress := KeyPress;

    TForm(Parent).OnClick := Click;
    TForm(Parent).OnDblClick := DblClick;
  end;
end;

procedure TCustomEngine.Clear;
var
  I: Integer;
  Instance: TAControl;
begin
  FActiveControl := nil;
  FPrevControl := nil;

  I := FRoot.ControlCount;
  while I <> 0 do
  begin
    Instance := FRoot.Controls[I - 1];
    FRoot.Remove(Instance);
    FreeAndNil(Instance);
    I := FRoot.ControlCount;
  end;
end;

procedure TCustomEngine.Click(Sender: TObject);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerClick)) then
    FOwnerClick(Sender);
  if (FActive) then
  begin
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.Click;
  end;
end;

constructor TCustomEngine.Create(const AOwner: TControl;
  const ADevice: TDevice; const ACanvas: TGameCanvas);
begin

  FDevice := ADevice;
  FCanvas := ACanvas;
  //FFonts := TAsphyreFonts.Create;
  //FFonts.Images := TAsphyreImages.Create;
  //FImages := TAsphyreImages.Create;
  Parent := AOwner;

  FRoot := TWControl.Create(nil);
  FRoot.Left := -1000;
  FRoot.Top := -1000;
  FRoot.Width :=3000;// ADevice.Size.X;
  FRoot.Height := 3000;
  FRoot.Engine := Self;

  FDesign := False;

  FActive := True;
  FLoading := False;
end;

procedure TCustomEngine.DblClick(Sender: TObject);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerDblClick)) then
    FOwnerDblClick(Sender);
  if (FActive) then
  begin
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.DblClick;
  end;
end;

destructor TCustomEngine.Destroy;
begin
  RestoreEvents;

  //FDevice := nil;
  //FCanvas := nil;
  FParent := nil;

  //FFonts.Images.RemoveAll;
  //FFonts.Images.Free;
  //FreeAndNil(FFonts);
  //FreeAndNil(FImages);
  FreeAndNil(FRoot);

  inherited Destroy;
end;

procedure TCustomEngine.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Control: TWControl;
begin
  if (Assigned(FOwnerKeyDown)) then
    FOwnerKeyDown(Sender, Key, Shift);
  if (FActive) then
  begin
    Control := ActiveControl;
    if Control <> nil then
    begin
      if (Control.Enabled) and (Control.Visible) then
        Control.KeyDown(Key, Shift);
    end
    else if FRoot.FWControls <> nil then
      TWControl(FRoot.FWControls[FRoot.FWControls.Count - 1]).KeyDown(Key,
        Shift)
    else
      FRoot.KeyDown(Key, Shift);
  end;
end;

procedure TCustomEngine.KeyPress(Sender: TObject; var Key: Char);
var
  Control: TWControl;
begin
  if (Assigned(FOwnerKeyPress)) then
    FOwnerKeyPress(Sender, Key);
  if (FActive) then
  begin
    Control := ActiveControl;
    if Control <> nil then
    begin
      if (Control.Enabled) and (Control.Visible) then
        Control.KeyPress(Key);
    end
    else if FRoot.FWControls <> nil then
      TWControl(FRoot.FWControls[FRoot.FWControls.Count - 1]).KeyPress(Key)
    else
      FRoot.KeyPress(Key);
  end;
end;

procedure TCustomEngine.KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Control: TWControl;
begin
  if (Assigned(FOwnerKeyUp)) then
    FOwnerKeyUp(Sender, Key, Shift);
  if (FActive) then
  begin
    Control := ActiveControl;
    if Control <> nil then
    begin
      if (Control.Enabled) and (Control.Visible) then
        Control.KeyUp(Key, Shift);
    end
    else if FRoot.FWControls <> nil then
      TWControl(FRoot.FWControls[FRoot.FWControls.Count - 1]).KeyUp(Key, Shift)
    else
      FRoot.KeyUp(Key, Shift);
  end;
end;

{
function TCustomEngine.LoadFontsFromFile(const FileName: string): Boolean;
var
  I: Integer;
  AName: string;
  Media: TASDb;
begin
  // free current fonts
  FFonts.RemoveAll;
  FFonts.Images.RemoveAll;

  FFonts.Canvas := FCanvas;

  Media := TASDb.Create;
  Media.FileName := FileName;
  Media.OpenMode := opReadOnly;
  Media.Update;

  if Media.RecordCount > 0 then
    for I := 0 to Media.RecordCount - 1 do
    begin
      if (Media.RecordType[I] in [recGraphics]) then
      begin
        // Create a Name for the image
        if Pos('.image', Media.RecordKey[I]) > 0 then
          AName := Copy(Media.RecordKey[I], 0,
            Pos('.image', Media.RecordKey[I]) - 1)
        else
          AName := Media.RecordKey[I];

        FFonts.Images.AddFromASDb(Media.RecordKey[I], Media, AName);
        FFonts.Insert(Media.FileName + ' | ' + AName + '.xml',
          ShortString(AName));
      end;
    end;

  // apply font patches
  if FFonts.FindFont('tahoma10') <> nil then
    FFonts.FindFont('tahoma10').Kerning := 1;

  Media.Free;

  Result := True;
end;

function TCustomEngine.LoadImagesFromFile(const FileName: string): Boolean;
var
  I: Integer;
  AName: string;
  Media: TASDb;
begin

  FImages.RemoveAll;

  Media := TASDb.Create;
  Media.FileName := FileName;
  Media.OpenMode := opReadOnly;
  Media.Update;

  if Media.RecordCount > 0 then
    for I := 0 to Media.RecordCount - 1 do
    begin
      if (Media.RecordType[I] in [recGraphics]) then
      begin

        if Pos('.image', Media.RecordKey[I]) > 0 then
          AName := Copy(Media.RecordKey[I], 0,
            Pos('.image', Media.RecordKey[I]) - 1)
        else
          AName := Media.RecordKey[I];

        FImages.AddFromASDb(Media.RecordKey[I], Media, AName);
      end;
    end;

  Media.Free;

  Result := True;
end;

}

procedure TCustomEngine.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Control: TAControl;
begin
  // fixes coordinates when scaled
  if Global.DisplaySize.X <> Self.Parent.ClientWidth then
   X := Trunc(X * (Global.DisplaySize.X / Self.Parent.ClientWidth));
  if Global.DisplaySize.Y <> Self.Parent.ClientHeight then
   Y := Trunc(Y * (Global.DisplaySize.Y / Self.Parent.ClientHeight));

  if (Assigned(FOwnerMouseDown)) then
    FOwnerMouseDown(Sender, Button, Shift, X, Y);
  if (FActive) then
  begin
    Control := FRoot.ControlAtPos(Point(X, Y), True, True, True);

    // Check if is Modal
    if FActiveControl <> nil then
    begin
      if FActiveControl.Handle <> nil then
      begin
        if FActiveControl.Handle is TAForm then
        begin
          if (TAForm(FActiveControl.Handle).IsModal) and
            (FActiveControl.Handle <> Control) then
          begin
            if (Control <> nil) then
            begin
              if FActiveControl.Handle.FindChildControl(Control.Name, True)
                = nil then
                Exit;
            end
            else
            begin
              Exit;
            end;
          end;
        end;
      end;
    end;

    if (Control <> nil) then
    begin
      if (Control.Enabled = True) then
        Control.MouseDown(Button, Shift, X, Y);
      FPrevControl := Control;
    end;
  end;
end;

procedure TCustomEngine.MouseEnter(Sender: TObject);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerMouseEnter)) then
    FOwnerMouseEnter(Sender);
  if (FActive) then
  begin
    FRoot.MouseEnter;
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseEnter;
  end;
end;

procedure TCustomEngine.MouseLeave(Sender: TObject);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerMouseLeave)) then
    FOwnerMouseLeave(Sender);
  if (FActive) then
  begin
    FRoot.MouseLeave;
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseLeave;
  end;
end;

procedure TCustomEngine.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Control: TAControl;
begin
  // fixes coordinates when scaled
  if Global.DisplaySize.X <> Self.Parent.ClientWidth then
   X := Trunc(X * (Global.DisplaySize.X / Self.Parent.ClientWidth));
  if Global.DisplaySize.Y <> Self.Parent.ClientHeight then
   Y := Trunc(Y * (Global.DisplaySize.Y / Self.Parent.ClientHeight));

  if (Assigned(FOwnerMouseMove)) then
    FOwnerMouseMove(Sender, Shift, X, Y);

  if (FActive) then
  begin
    if Shift = [ssLeft] then
      Control := FPrevControl
    else
      Control := FRoot.ControlAtPos(Point(X, Y), True, True, True);

    // Check if is Modal
    if FActiveControl <> nil then
    begin
      if FActiveControl.Handle <> nil then
      begin
        if FActiveControl.Handle is TAForm then
        begin
          if (TAForm(FActiveControl.Handle).IsModal) and
            (FActiveControl.Handle <> Control) then
          begin
            if (Control <> nil) then
            begin
              if FActiveControl.Handle.FindChildControl(Control.Name, True)
                = nil then
                Exit;
            end
            else
            begin
              Exit;
            end;
          end;
        end;
      end;
    end;

    if (FPrevControl <> Control) then
    begin
      if (FPrevControl <> nil) then
      begin
        if (FPrevControl.Enabled) and (FPrevControl.Visible) then
          if FPrevControl is TWControl then
          begin
            if Control <> nil then
            begin
              if TWControl(FPrevControl).FindChildControl(Control.Name)
                = nil then
                FPrevControl.MouseLeave;
            end
            else
              FPrevControl.MouseLeave;
          end
          else
            FPrevControl.MouseLeave;
      end;
      if (Control <> nil) then
      begin
        if (Control.Enabled) and (Control.Visible) then
          Control.MouseEnter;
      end;
      FPrevControl := Control;
    end;

    if (Control <> nil) then
    begin
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseMove(Shift, X, Y);
    end;

    // used for tests only
    {
      if Control <> nil then
      TForm(Parent).caption := '(' + Inttostr(X) + ',' + Inttostr(Y)
      + ') - ' + Control.Name
      else
      TForm(Parent).caption := '(' + Inttostr(X) + ',' + Inttostr(Y) + ')';
      }
  end;
end;

procedure TCustomEngine.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Control: TAControl;
begin
  // fixes coordinates when scaled
  if Global.DisplaySize.X <> Self.Parent.ClientWidth then
   X := Trunc(X * (Global.DisplaySize.X / Self.Parent.ClientWidth));
  if Global.DisplaySize.Y <> Self.Parent.ClientHeight then
   Y := Trunc(Y * (Global.DisplaySize.Y / Self.Parent.ClientHeight));

  if (Assigned(FOwnerMouseUp)) then
    FOwnerMouseUp(Sender, Button, Shift, X, Y);
  if (FActive) then
  begin
    if Button = mbLeft then
    begin
      Control := FPrevControl;

      if (FPrevControl is TADropPanel) then
      begin
        Control := FRoot.ControlAtPos(Point(X, Y), True, True, True);

        if not(Control is TADropPanel) then
        begin
          Control := FPrevControl;
        end;
      end;
    end
    else
      Control := FRoot.ControlAtPos(Point(X, Y), True, True, True);

    if (Control <> nil) then
    begin
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseUp(Button, Shift, X, Y);

      if (FPrevControl <> Control) and (FPrevControl is TADropPanel) then
        TADropPanel(FPrevControl).DragItem := False;

      FPrevControl := Control;
    end;
  end;
end;

procedure TCustomEngine.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerMouseWheel)) then
    FOwnerMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
  if (FActive) then
  begin
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseWheel(Shift, WheelDelta, MousePos);
  end;
end;

procedure TCustomEngine.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerMouseWheelDown)) then
    FOwnerMouseWheelDown(Sender, Shift, MousePos, Handled);
  if (FActive) then
  begin
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseWheelDown(Shift, MousePos);
  end;
end;

procedure TCustomEngine.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  Control: TAControl;
begin
  if (Assigned(FOwnerMouseWheelUp)) then
    FOwnerMouseWheelUp(Sender, Shift, MousePos, Handled);
  if (FActive) then
  begin
    Control := FPrevControl;
    if (Control <> nil) then
      if (Control.Enabled) and (Control.Visible) then
        Control.MouseWheelUp(Shift, MousePos);
  end;
end;

procedure TCustomEngine.Render(DC: HDC);
begin
  if FLoading then
    Exit;

  FRoot.Paint(DC);
end;

procedure TCustomEngine.RestoreEvents;
begin
  if Parent is TForm then
  begin
    TForm(Parent).OnMouseDown := FOwnerMouseDown;
    TForm(Parent).OnMouseUp := FOwnerMouseUp;
    TForm(Parent).OnMouseMove := FOwnerMouseMove;
    TForm(Parent).OnMouseEnter := FOwnerMouseEnter;
    TForm(Parent).OnMouseLeave := FOwnerMouseLeave;
    TForm(Parent).OnMouseWheel := FOwnerMouseWheel;
    TForm(Parent).OnMouseWheelDown := FOwnerMouseWheelDown;
    TForm(Parent).OnMouseWheelUp := FOwnerMouseWheelUp;

    TForm(Parent).OnKeyDown := FOwnerKeyDown;
    TForm(Parent).OnKeyUp := FOwnerKeyUp;
    TForm(Parent).OnKeyPress := FOwnerKeyPress;

    TForm(Parent).OnClick := FOwnerClick;
    TForm(Parent).OnDblClick := FOwnerDblClick;
  end;
end;

procedure TCustomEngine.SetActiveControl(const Control: TWControl);
begin
  if FActiveControl <> Control then
    FActiveControl := Control;
end;

procedure TCustomEngine.SetDesign(const Value: Boolean);
begin
  if FDesign <> Value then
    FDesign := Value;
end;

procedure TCustomEngine.SetLoading(const Value: Boolean);
begin
  if FLoading <> Value then
    FLoading := Value;
end;

procedure TCustomEngine.SetParent(const Control: TControl);
begin
  if FParent <> Control then
  begin
    if FParent <> nil then
      RestoreEvents;

    FParent := Control;
    AcquireEvents;
  end;
end;

// ----------------------------------------------------------------------------
{ TAControl }

function TAControl.AEngine: TCustomEngine;
begin
  Result := FAEngine;
end;

//function TAControl.AFont: TAsphyreFont;
//begin
 // Result := FAFont;
//end;

function TAControl.AImage: TTexture;
begin
  if {not} AEngine.FImageLib.ContainsKey(FImageEntry) then
 //   Result := nil
 // else
    Result := AEngine.FImageLib[FImageEntry];
end;

procedure TAControl.Assign(Source: TPersistent);
begin
  ControlState := ControlState + [csReadingState];
  inherited;
  ControlState := ControlState - [csReadingState];
end;

procedure TAControl.AssignTo(Dest: TPersistent);
begin

  if Dest is TAControl then
    with TAControl(Dest) do
    begin
      // inherited properties
      try
        Name := Self.Name;
      except
        on EComponentError do
        begin
        end;
      end;
      Tag := Self.Tag;

      // FAEngine - is set on Create
      // FHandle - is set on Create
      // FParent - is set on Create

      // FAFont - is set by Font
      // FAImage - is set by Image

      Enabled := Self.Enabled;
      BorderColor := Self.BorderColor;
      BorderWidth := Self.BorderWidth;
      //Color := Self.Color;
      Font := Self.Font; // this set FAFont
      FontColor := Self.FontColor;
      Height := Self.Height;
      //Image := Self.Image; // this set FAImage
      ImageEntry := Self.ImageEntry;
      ImageAlpha := Self.ImageAlpha;
      Left := Self.Left;
      Margin := Self.Margin;
      OnClick := Self.OnClick;
      OnDblClick := Self.OnDblClick;
      OnMouseLeave := Self.OnMouseLeave;
      OnMouseEnter := Self.OnMouseEnter;
      OnMouseDown := Self.OnMouseDown;
      OnMouseMove := Self.OnMouseMove;
      OnMouseUp := Self.OnMouseUp;
      OnMouseWheel := Self.OnMouseWheel;
      OnMouseWheelDown := Self.OnMouseWheelDown;
      OnMouseWheelUp := Self.OnMouseWheelUp;
      OnResize := Self.OnResize;
      Text := Self.Text;
      Top := Self.Top;
      WheelAccumulator := Self.WheelAccumulator;
      Width := Self.Width;
      Visible := Self.Visible;
    end
    else
      inherited AssignTo(Dest);

end;

procedure TAControl.BringToFront;
begin
  SetZOrder(True);
end;

procedure TAControl.Click;
var
  BoundsRect: TRect;
begin
  BoundsRect := Rect(ClientLeft, ClientTop, ClientLeft + Width,
    ClientTop + Height);

  if (PtInRect(BoundsRect, VirtualPoint)) then
    if Assigned(FOnClick) then
      FOnClick(Self);
end;

constructor TAControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHandle := Self;
  FText := '';
 // FColor := TFillColor.Create;
  FImageAlpha := $FFFFFFFF;
  FEnabled := True;
  FVisible := True;

  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    Engine := TWControl(AOwner).Engine;
    TWControl(AOwner).InsertControl(Self);
  end;

end;

procedure TAControl.DblClick;
var
  BoundsRect: TRect;
begin
  BoundsRect := Rect(ClientLeft, ClientTop, ClientLeft + Width,
    ClientTop + Height);

  if (PtInRect(BoundsRect, VirtualPoint)) then
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
end;

destructor TAControl.Destroy;
begin
  //FreeAndNil(FColor);
  //FreeAndNil(FFontColor);

  inherited Destroy;
end;

function TAControl.GetClientLeft: Integer;
var
  Temp: TWControl;
begin
  Temp := FParent;
  Result := FLeft;
  while Temp <> nil do
  begin
    Result := Result + Temp.FLeft;
    Temp := Temp.FParent;
  end;
end;

function TAControl.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

function TAControl.GetClientTop: Integer;
var
  Temp: TWControl;
begin
  Temp := Parent;
  Result := FTop;
  while Temp <> nil do
  begin
    Result := Result + Temp.FTop;
    Temp := Temp.Parent;
  end;
end;

function TAControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TAControl.GetHandle: TWControl;
begin
  Result := FHandle;
end;

function TAControl.GetParentComponent: TComponent;
begin
  Result := FParent as TComponent;
end;

function TAControl.GetTextLen: Integer;
begin
  Result := Length(FText);
end;

function TAControl.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

function TAControl.IsVisible: Boolean;
var
  Temp: TAControl;
begin
  Temp := Self;
  Result := FVisible;
  while Temp.Parent <> nil do
  begin
    Result := (Result and Temp.FVisible);
    Temp := Temp.Parent;
  end;
end;

procedure TAControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Include(FControlState, csClicked);
  // get the down point
  VirtualPoint.X := X;
  VirtualPoint.Y := Y;

  if AEngine.ActiveControl <> Self then
  begin
    // Bring Handle to front and set focus
    Handle.BringToFront;

    if AEngine.ActiveControl <> nil then
    begin
      if (Self is TWControl) then
      begin
        if TWControl(Self).FindChildControl(AEngine.ActiveControl.Name)
          = nil then
        begin
          TWControl(Self).SetFocus;
          TWControl(Self).SelectFirst;
        end;
      end
      else
      begin
        if Handle.FindChildControl(AEngine.ActiveControl.Name) = nil then
        begin
          Handle.SetFocus;
          Handle.SelectFirst;
        end;
      end;
    end
    else
    begin
      Handle.SetFocus;
      Handle.SelectFirst;
    end;
  end;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TAControl.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAControl.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // get the current point
  VirtualPoint.X := X;
  VirtualPoint.Y := Y;

  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TAControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Exclude(FControlState, csClicked);

  // get the release point
  VirtualPoint.X := X;
  VirtualPoint.Y := Y;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

function TAControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  IsNeg: Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    Inc(FWheelAccumulator, WheelDelta);
    while Abs(FWheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := FWheelAccumulator < 0;
      FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        if FWheelAccumulator <> 0 then
          FWheelAccumulator := -FWheelAccumulator;
        Result := MouseWheelDown(Shift, MousePos);
      end
      else
        Result := MouseWheelUp(Shift, MousePos);
    end;
  end;
end;

function TAControl.MouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

function TAControl.MouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

procedure TAControl.ReadState(Reader: TReader);
begin
  Include(FControlState, csReadingState);
  inherited ReadState(Reader);
  Exclude(FControlState, csReadingState);
end;

procedure TAControl.Resize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TAControl.SendToBack;
begin
  SetZOrder(False);
end;

procedure TAControl.SetAEngine(AEngine: TCustomEngine);
begin
  FAEngine := AEngine;

  if AEngine <> nil then
  begin
   // FAFont := AEngine.Fonts.FindFont(FFont);
   //if  FImageName <> '' then

   // FAImage := AEngine.ImageLib[imagename];
  end;
end;

procedure TAControl.SetBorderColor(Color: TBorderColor);
begin
  FBorderColor := Color;
end;

procedure TAControl.SetBorderWidth(Value: Word);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
  end;
end;

procedure TAControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if ((ALeft <> FLeft) or (ATop <> FTop) or (AWidth <> FWidth) or
      (AHeight <> FHeight)) then
  begin
    FLeft := ALeft;
    FTop := ATop;
    FWidth := AWidth;
    FHeight := AHeight;
    if not(csLoading in ComponentState) then
      Resize;
  end;
end;

procedure TAControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
  end;
end;

procedure TAControl.SetFont(Name: ShortString);
begin
  FFont := Name;
 // if AEngine <> nil then
  //  FAFont := AEngine.Fonts.FindFont(Name);
end;

procedure TAControl.SetColor(Color: TFillColor);
begin
  //FColor.Assign(Color);
end;

procedure TAControl.SetHeight(Value: Integer);
begin
  if (Parent <> nil) and not((csReadingState in ControlState) or
      (csCreating in ControlState)) then
  begin
    if Value > Parent.Height - Parent.BorderWidth - FTop then
      Value := Parent.Height - Parent.BorderWidth - FTop;
  end;

  SetBounds(FLeft, FTop, FWidth, Value);
end;

procedure TAControl.SetImage(Entry: TWZIMGEntry);
begin
  FImageEntry := Entry;
  if AEngine <> nil then
    FAImage := AEngine.ImageLib[Entry];
end;

procedure TAControl.SetImageAlpha(Color: TAlphaColor);
begin
  if FImageAlpha <> Color then
    FImageAlpha := Color;
end;

procedure TAControl.SetLeft(Value: Integer);
begin
  if (FParent <> nil) and not((csReadingState in ControlState) or
      (csCreating in ControlState)) then
  begin
    if Value > FParent.Width - FParent.BorderWidth - FWidth+500 then
      Value := FParent.Width - FParent.BorderWidth - FWidth+500;

    if Value < FParent.BorderWidth then
      Value := Parent.BorderWidth;
  end;

  SetBounds(Value, FTop, FWidth, FHeight);
end;

procedure TAControl.SetMargin(Value: Word);
begin
  if FMargin <> Value then
    FMargin := Value;
end;

procedure TAControl.SetParent(AParent: TWControl);
begin
  if FParent <> AParent then
  begin
    if AParent = Self then
      Exit;
    if FParent <> nil then
      FParent.RemoveControl(Self);
    if AParent <> nil then
    begin
      AParent.InsertControl(Self);
    end;
  end;
end;

procedure TAControl.SetParentComponent(Value: TComponent);
begin
  if (Parent <> Value) and (Value is TWControl) then
    SetParent(TWControl(Value));
end;

procedure TAControl.SetText(Text: String);
begin
  FText := Text;
end;

procedure TAControl.SetTop(Value: Integer);
begin
  if (Parent <> nil) and not((csReadingState in ControlState) or
      (csCreating in ControlState)) then
  begin
    if Value > Parent.Height - Parent.BorderWidth - Height+500 then
      Value := Parent.Height - Parent.BorderWidth - Height+500;

    if Value < Parent.BorderWidth then
      Value := Parent.BorderWidth;
  end;

  SetBounds(FLeft, Value, FWidth, FHeight);
end;

procedure TAControl.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;

    // Execute events that possible haven't been executed yet.
    if Value = False then
    begin
      MouseLeave;
      if Self is TWControl then
        TWControl(Self).DoExit;
    end;
  end;
end;

procedure TAControl.SetWidth(Value: Integer);
begin
  if (Parent <> nil) and not((csReadingState in ControlState) or
      (csCreating in ControlState)) then
  begin
    if Value > Parent.Width - Parent.BorderWidth - FLeft then
      Value := Parent.Width - Parent.BorderWidth - FLeft;
  end;

  SetBounds(FLeft, FTop, Value, FHeight);
end;

procedure TAControl.SetZOrder(TopMost: Boolean);
begin
  if FParent <> nil then
  begin
    if TopMost then
    begin
      SetZOrderPosition(FParent.FControls.Count - 1)
    end
    else
      SetZOrderPosition(0);
  end;
end;

procedure TAControl.SetZOrderPosition(Position: Integer);
var
  I, Count: Integer;
begin
  if FParent <> nil then
  begin
    I := FParent.FControls.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FParent.FControls.Count;
      if Position < 0 then
        Position := 0;
      if Position >= Count then
        Position := Count - 1;
      if Position <> I then
      begin
        FParent.FControls.Delete(I);
        FParent.FControls.Insert(Position, Self);
      end;
    end;
  end;
end;

// ----------------------------------------------------------------------------
{ TWControl }

procedure TWControl.AssignTo(Dest: TPersistent);
var
  I: Integer;
  AClass: TAControlClass;
  Control: TAControl;
begin
  inherited AssignTo(Dest);

  if Dest is TWControl then
    with TWControl(Dest) do
    begin
      // FControls
      // FTabList
      // FWControls
      TabStop := Self.TabStop;
      OnEnter := Self.OnEnter;
      OnExit := Self.OnExit;
      OnKeyDown := Self.OnKeyDown;
      OnKeyPress := Self.OnKeyPress;
      OnKeyUp := Self.OnKeyUp;

      if Self.ControlCount <> 0 then
      begin
        for I := 0 to Self.ControlCount - 1 do
        begin
          AClass := TAControlClass(Self.Controls[I].ClassType);
          Control := AClass.Create(TWControl(Dest));
          Control.Assign(Self.Controls[I]);
        end;
      end;
    end;
end;

function TWControl.CanFocus: Boolean;
var
  Control: TWControl;
begin
  Result := False;

  Control := Self;
  while Control.Parent <> AEngine.Root do
  begin
    if not(Control.FVisible and Control.Enabled) then
      Exit;
    Control := Control.Parent;
  end;
  Result := True;
end;

function TWControl.ControlAtPos(const Pos: TPoint;
  AllowDisabled, AllowWControls, AllLevels: Boolean): TAControl;
var
  I: Integer;
  P: TPoint;
  LControl: TAControl;

  function GetControlAtPos(AControl: TAControl): Boolean;
  begin
    with AControl do
    begin
      P := Point(Pos.X - ClientLeft, Pos.Y - ClientTop);
      Result := (PtInRect(ClientRect,
          P) and (IsVisible or (IsVisible and (Enabled or AllowDisabled)))) or
        (PtInRect(ClientRect, P) and (AEngine.DesignMode));
      if Result then
        LControl := AControl;
    end;
  end;

begin
  LControl := nil;

  if AllowWControls and (FWControls <> nil) then
    for I := FWControls.Count - 1 downto 0 do
    begin
      if AllLevels then
        if TWControl(FWControls[I]).FWControls <> nil then
          LControl := TWControl(FWControls[I]).ControlAtPos(Pos, AllowDisabled,
            True, True);

      // if found a WControl on Sub Level
      if (LControl <> nil) then
      begin
        Break;
      end;

      // Not found on sub Level, check curent level
      if (LControl = nil) and GetControlAtPos(TWControl(FWControls[I])) then
        Break;
    end;

  // find FControls on result WControl
  if (LControl <> nil) and (LControl is TWControl) then
    if (TWControl(LControl).FControls <> nil) then
      for I := TWControl(LControl).FControls.Count - 1 downto 0 do
        if GetControlAtPos(TWControl(LControl).FControls[I]) then
          Break;

  // if nothing found and has FControls, search in FControls list
  if (FControls <> nil) and (LControl = nil) then
  begin
    for I := FControls.Count - 1 downto 0 do
      if GetControlAtPos(FControls[I]) then
        Break;
  end;

  Result := LControl;
end;

constructor TWControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTabStop := False;
end;

destructor TWControl.Destroy;
var
  I: Integer;
  Instance: TAControl;
begin
  Destroying;

  I := ControlCount;
  while I <> 0 do
  begin
    Instance := Controls[I - 1];
    Remove(Instance);
    FreeAndNil(Instance);
    I := ControlCount;
  end;

  inherited Destroy;
end;

procedure TWControl.DoEnter;
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TWControl.DoExit;
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

function TWControl.FindChildControl(const ControlName: string;
  AllLevels: Boolean = False): TAControl;
var
  I: Integer;
begin
  Result := nil;

  if FControls <> nil then
    for I := 0 to FControls.Count - 1 do
      if CompareText(TWControl(FControls[I]).Name, ControlName) = 0 then
      begin
        Result := TAControl(FControls[I]);
        Exit;
      end;

  if FWControls <> nil then
    for I := 0 to FWControls.Count - 1 do
    begin
      if CompareText(TWControl(FWControls[I]).Name, ControlName) = 0 then
      begin
        Result := TAControl(FWControls[I]);
        Exit;
      end;
      if AllLevels = True then
        if TWControl(FWControls[I]).FindChildControl(ControlName, AllLevels)
          <> nil then
        begin
          Result := TWControl(FWControls[I]).FindChildControl(ControlName,
            AllLevels);
          Exit;
        end;
    end;
end;

function TWControl.FindNextControl(CurControl: TWControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWControl;
var
  I, StartIndex: Integer;
  List: TList;
begin
  Result := nil;
  List := TList.Create;
  try
    GetTabOrderList(List);
    if List.Count > 0 then
    begin
      StartIndex := List.IndexOf(CurControl);
      if StartIndex = -1 then
        if GoForward then
          StartIndex := List.Count - 1
        else
          StartIndex := 0;
      I := StartIndex;
      repeat
        if GoForward then
        begin
          Inc(I);
          if I = List.Count then
            I := 0;
        end
        else
        begin
          if I = 0 then
            I := List.Count;
          Dec(I);
        end;
        CurControl := TWControl(List[I]);
        if CurControl.CanFocus and (not CheckTabStop or CurControl.TabStop) and
          (not CheckParent or (CurControl.Parent = Self)) then
          Result := CurControl;
      until (Result <> nil) or (I = StartIndex);
    end;
  finally
    List.Free;
  end;
end;

function TWControl.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TWControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TAControl;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    // if Control.Owner = Root then
    Proc(Control);
  end;
end;

function TWControl.GetControl(Index: Integer): TAControl;
var
  N: Integer;
begin
  if FControls <> nil then
    N := FControls.Count
  else
    N := 0;
  if Index < N then
    Result := FControls[Index]
  else
    Result := FWControls[Index - N];
end;

function TWControl.GetControlCount: Integer;
begin
  Result := 0;
  if FControls <> nil then
    Inc(Result, FControls.Count);
  if FWControls <> nil then
    Inc(Result, FWControls.Count);
end;

function TWControl.GetTabOrder: TTabOrder;
begin
  if FParent <> nil then
    Result := FParent.FTabList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TWControl.GetTabOrderList(List: TList);
var
  I: Integer;
  Control: TWControl;
begin
  if FTabList <> nil then
    for I := 0 to FTabList.Count - 1 do
    begin
      Control := TWControl(FTabList[I]);
      List.Add(Control);
      Control.GetTabOrderList(List);
    end;
end;

procedure TWControl.Insert(AControl: TAControl);
var
  Form: TWControl;
begin
  if AControl <> nil then
  begin
    if AControl is TWControl then
    begin
      ListAdd(FWControls, AControl);
      ListAdd(FTabList, AControl);
    end
    else
      ListAdd(FControls, AControl);
    AControl.FParent := Self;

    // Get the parent Form from Engine.Root and set it as handler
    Form := Self;
    if (AEngine <> nil) then
    begin
      if not(Form = AEngine.Root) then
      begin
        while Form.Parent <> AEngine.Root do
          Form := Form.Parent;
        AControl.FHandle := Form;
      end
      else
      begin
        AControl.FHandle := AControl;
      end;
    end;
  end;
end;

procedure TWControl.InsertControl(AControl: TAControl);
begin
  AControl.ValidateContainer(Self);
  Insert(AControl);
end;

procedure TWControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssShift, ssCtrl]) or (Shift = [ssShift]) then
  begin
    case Key of
      vk_Tab:
        Self.Handle.SelectNext(Self, False, True);
    end;
  end;

  if (Shift = [ssCtrl]) or (Shift = []) then
  begin
    case Key of
      vk_Tab:
        Self.Handle.SelectNext(Self, True, True);
    end;
  end;

  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TWControl.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TWControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure TWControl.Paint(DC: HDC);
var
  Control, WControl: Integer;
begin
  if FControls <> nil then
  begin
    for Control := 0 to FControls.Count - 1 do
    begin
      with TAControl(FControls[Control]) do
        if (Visible) or (Self.AEngine.FDesign) then
          Paint(DC);
    end;
  end;
  if FWControls <> nil then
  begin
    for WControl := 0 to FWControls.Count - 1 do
    begin
      with TWControl(FWControls[WControl]) do
        if (Visible) or (Self.AEngine.FDesign) then
          Paint(DC);
    end;
  end;
end;

procedure TWControl.Remove(AControl: TAControl);
begin
  if AControl is TWControl then
  begin
    ListRemove(FTabList, AControl);
    ListRemove(FWControls, AControl);
  end
  else
    ListRemove(FControls, AControl);
  AControl.FParent := nil;
  AControl.FHandle := AControl;
end;

procedure TWControl.RemoveControl(AControl: TAControl);
begin
  Remove(AControl);
end;

procedure TWControl.SelectFirst;
var
  Control: TWControl;
begin
  Control := FindNextControl(nil, True, True, False);
  if Control = nil then
    Control := FindNextControl(nil, True, False, False);
  if Control <> nil then
  begin
    Control.SetFocus;
  end;
end;

procedure TWControl.SelectNext(CurControl: TWControl;
  GoForward, CheckTabStop: Boolean);
begin
  CurControl := FindNextControl(CurControl, GoForward, CheckTabStop,
    not CheckTabStop);
  if CurControl <> nil then
    CurControl.SetFocus;
end;

procedure TWControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  if Child is TWControl then
    TWControl(Child).SetZOrderPosition(Order)
  else if Child is TAControl then
    TAControl(Child).SetZOrderPosition(Order);
end;

procedure TWControl.SetFocus;
var
  Control: TWControl;
begin
  Control := Self;
  if Control.CanFocus then
  begin
    if AEngine.ActiveControl <> nil then
      if AEngine.ActiveControl <> Control then
        AEngine.ActiveControl.DoExit;
    AEngine.ActiveControl := Control;
    Control.DoEnter;
  end;
end;

procedure TWControl.SetTabOrder(Value: TTabOrder);
begin
  UpdateTabOrder(Value);
end;

procedure TWControl.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
  end;
end;

procedure TWControl.SetZOrder(TopMost: Boolean);
var
  N, M: Integer;
begin
  if FParent <> nil then
  begin
    if TopMost then
      N := FParent.FWControls.Count - 1
    else
      N := 0;
    M := 0;
    if FParent.FControls <> nil then
      M := FParent.FControls.Count;
    SetZOrderPosition(M + N);
  end;
end;

procedure TWControl.SetZOrderPosition(Position: Integer);
var
  I, Count: Integer;
begin
  if FParent <> nil then
  begin
    if FParent.FControls <> nil then
      Dec(Position, FParent.FControls.Count);
    I := FParent.FWControls.IndexOf(Self);
    if I >= 0 then
    begin
      Count := FParent.FWControls.Count;
      if Position < 0 then
        Position := 0;
      if Position >= Count then
        Position := Count - 1;
      if Position <> I then
      begin
        FParent.FWControls.Delete(I);
        FParent.FWControls.Insert(Position, Self);
      end;
    end;
  end;
end;

procedure TWControl.UpdateTabOrder(Value: TTabOrder);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetTabOrder;
  if CurIndex >= 0 then
  begin
    Count := FParent.FTabList.Count;
    if Value < 0 then
      Value := 0;
    if Value >= Count then
      Value := Count - 1;
    if Value <> CurIndex then
    begin
      FParent.FTabList.Delete(CurIndex);
      FParent.FTabList.Insert(Value, Self);
    end;
  end;
end;

initialization

RegisterClasses([TAControl, TWControl]);

finalization

UnRegisterClasses([TAControl, TWControl]);

end.
