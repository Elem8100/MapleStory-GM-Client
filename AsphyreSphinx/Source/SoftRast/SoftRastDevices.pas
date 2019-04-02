unit SoftRastDevices;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, Graphics, SysUtils, AbstractDevices, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TSoftRastDevice = class(TAsphyreDevice)
 private
  FSurface: TSystemSurface;
  Bitmap : TBitmap;

  ModeChanged: Boolean;

  function ChangeVideoMode(): Boolean;
  procedure RestoreDisplayMode();
 protected
  function InitDevice(): Boolean; override;
  procedure DoneDevice(); override;
  procedure ResetDevice(); override;

  procedure UpdateParams(ParamType: TParamChangeType); override;
  function MayRender(): Boolean; override;
  procedure RenderWith(hWnd: THandle; Handler: TNotifyEvent;
   Background: Cardinal); override;
  procedure RenderToTarget(Handler: TNotifyEvent;
   Background: Cardinal; FillBk: Boolean); override;
 public
  property Surface: TSystemSurface read FSurface;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 WinAdapterInfo, AsphyreErrors, SoftRastTypes;

//---------------------------------------------------------------------------
constructor TSoftRastDevice.Create();
begin
 inherited;

 FSurface:= nil;
 Bitmap  := nil;
 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
function TSoftRastDevice.ChangeVideoMode(): Boolean;
var
 Mode: Integer;
 CurMode: TWinAdapterMode;
begin
 Result:= False;

 // (1) Retreive current video mode.
 CurMode:= WinAdapterHelper.CurrentMode(Adapter);
 if (CurMode.Width < 1)or(CurMode.Height < 1) then
  begin
   Errors.Insert(errEnumDisplayAdapters, Self, ClassName, 'ChangeVideoMode');
   Exit;
  end;

 // (2) Retreive all possible video modes.
 Result:= WinAdapterHelper.RefreshModes(Adapter);
 if (not Result) then
  begin
   Errors.Insert(errEnumDisplayMode, Self, ClassName, 'ChangeVideoMode');
   Exit;
  end;

 // (2) Find the best approximate video mode match.
 if (HighBitDepth) then
  begin
   Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, CurMode.Refresh);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 75);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 60);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 0);

   if (Mode = -1) then
    Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, CurMode.Refresh);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 75);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 60);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 0);

   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 0, 0);
  end else
  begin
   Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, CurMode.Refresh);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 75);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 60);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 16, 0);

   if (Mode = -1) then
    Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, CurMode.Refresh);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 75);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 60);
   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 32, 0);

   if (Mode = -1) then Mode:= WinAdapterHelper.MatchMode(Size.x, Size.y, 0, 0);
  end;

 Result:= Mode <> -1;
 if (not Result) then
  begin
   Errors.Insert(errUnsupportedDisplayMode, Self, ClassName, 'ChangeVideoMode');
   Exit;
  end;

 // (3) Change the resolution to the desired mode.
 Result:= WinAdapterHelper.SetTempMode(Adapter, Mode);
 if (not Result) then
  begin
   Errors.Insert(errChangeDisplayMode, Self, ClassName, 'ChangeVideoMode');
   Exit;
  end;

 // (4) Update patameters.
 ModeChanged:= True;
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.RestoreDisplayMode();
begin
 WinAdapterHelper.RestoreTempMode(Adapter);
 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
function TSoftRastDevice.InitDevice(): Boolean;
begin
 if (Size.x < 1)or(Size.y < 1) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'InitDevice');
   Result:= False;
   Exit;
  end;

 if (not Windowed) then
  begin
   Result:= ChangeVideoMode();
   if (not Result) then Exit;
  end;

 FSurface:= TSystemSurface.Create();
 FSurface.SetSize(Size.x, Size.y);
 FSurface.Clear(0);

 Bitmap:= TBitmap.Create();
 Bitmap.PixelFormat:= pf32bit;

 TargetSurface:= nil;
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.DoneDevice();
begin
 TargetSurface:= nil;

 if (Bitmap <> nil) then FreeAndNil(Bitmap);
 if (FSurface <> nil) then FreeAndNil(FSurface);

 if (ModeChanged) then RestoreDisplayMode();
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.ResetDevice();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TSoftRastDevice.MayRender(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.UpdateParams(ParamType: TParamChangeType);
begin
 FSurface.SetSize(Size.x, Size.y);
 FSurface.Clear(0);
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.RenderWith(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
var
 i: Integer;
 DestDC: THandle;
begin
 TargetSurface:= FSurface;

 FSurface.Clear(Background);

 EventBeginScene.Notify(Self, nil);
 Handler(Self);
 EventEndScene.Notify(Self, nil);

 if (Bitmap.Width <> FSurface.Width)or(Bitmap.Height <> FSurface.Height) then
  Bitmap.SetSize(FSurface.Width, FSurface.Height);

 for i:= 0 to FSurface.Height - 1 do
  Move(FSurface.Scanline[i]^, Bitmap.Scanline[i]^, FSurface.Width * 4);

 DestDC:= GetDC(hWnd);
 if (DestDC <> 0) then
  begin
   BitBlt(DestDC, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle,
    0, 0, SRCCOPY);
   ReleaseDC(hWnd, DestDC);
  end;

 TargetSurface:= nil;
end;

//---------------------------------------------------------------------------
procedure TSoftRastDevice.RenderToTarget(Handler: TNotifyEvent; Background: Cardinal;
 FillBk: Boolean);
begin
 Errors.Insert(errUnsupportedOperation, Self, ClassName, 'RenderToTarget');
end;

//---------------------------------------------------------------------------
end.
