unit GLESDevices;
//---------------------------------------------------------------------------
// GLESDevices.pas                                      Modified: 12-Nov-2010
// OpenGL ES device and window handling                           Version 1.0
//---------------------------------------------------------------------------
// Important Notice:
//
// If you modify/use this code or one of its parts either in original or
// modified form, you must comply with Mozilla Public License v1.1,
// specifically section 3, "Distribution Obligations". Failure to do so will
// result in the license breach, which will be resolved in the court.
// Remember that violating author's rights is considered a serious crime in
// many countries. Thank you!
//
// !! Please *read* Mozilla Public License 1.1 document located at:
//  http://www.mozilla.org/MPL/
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is GLESDevices.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2010,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SysUtils, Classes, Graphics, AbstractDevices, AsphyreGLES,
 AsphyreEGL, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TGLESDevice = class(TAsphyreDevice)
 private
  Display : EGLDisplay;
  DispConf: EGLConfig;
  Context : EGLContext;
  Surface : EGLSurface;

  ModeChanged: Boolean;
  FPixBuffer : TSystemSurface;
  FPixBitmap : TBitmap;

  function ChangeVideoMode(): Boolean;
  procedure RestoreDisplayMode();
  procedure SaveWindowState();
  procedure RestoreWindowState();

  procedure StartEGL();
  procedure EndEGL();

  procedure StartEGLContext();
  procedure EndEGLContext();

  procedure RenderScene(Handler: TNotifyEvent; Background: Cardinal);
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
  property PixBuffer: TSystemSurface read FPixBuffer;
  property PixBitmap: TBitmap read FPixBitmap;

  procedure Clear(Color: Cardinal);
  procedure Flip(Handle: THandle);

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 UsingFrameBuffer: Boolean = False;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors, WinAdapterInfo;

//---------------------------------------------------------------------------
constructor TGLESDevice.Create();
begin
 inherited;

 FPixBuffer:= TSystemSurface.Create();

 FPixBitmap:= TBitmap.Create();
 FPixBitmap.PixelFormat:= pf32bit;

 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
destructor TGLESDevice.Destroy();
begin
 FreeAndNil(FPixBitmap);
 FreeAndNil(FPixBuffer);

 inherited;
end;

//---------------------------------------------------------------------------
function TGLESDevice.ChangeVideoMode(): Boolean;
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

 // (3) Save the current window state.
 SaveWindowState();

 // (4) Change the resolution to the desired mode.
 Result:= WinAdapterHelper.SetTempMode(Adapter, Mode);
 if (not Result) then
  begin
   Errors.Insert(errChangeDisplayMode, Self, ClassName, 'ChangeVideoMode');
   Exit;
  end;

 // (5) Update patameters.
 ModeChanged:= True;
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.RestoreDisplayMode();
begin
 WinAdapterHelper.RestoreTempMode(Adapter);

 RestoreWindowState();

 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.SaveWindowState();
begin
 // to-do
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.RestoreWindowState();
begin
 // to-do
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.StartEGL();
var
 Config: array[0..14] of EGLint;
 MajorVersion: EGLint;
 MinorVersion: EGLint;
 NumConfigs: EGLint;
begin
 Config[0] := EGL_RED_SIZE;
 Config[1] := 8;
 Config[2] := EGL_GREEN_SIZE;
 Config[3] := 8;
 Config[4] := EGL_BLUE_SIZE;
 Config[5] := 8;
 Config[6] := EGL_ALPHA_SIZE;
 Config[7] := 8;

 Config[8] := EGL_DEPTH_SIZE;
 Config[9] := 24;
 Config[10]:= EGL_STENCIL_SIZE;
 Config[11]:= 8;
 Config[12]:= EGL_SURFACE_TYPE;
 Config[13]:= EGL_PBUFFER_BIT;
 Config[14]:= EGL_NONE;

 Display:= eglGetDisplay(EGL_DEFAULT_DISPLAY);
 eglInitialize(Display, @MajorVersion, @MinorVersion);
 eglGetConfigs(Display, nil, 0, @NumConfigs);
 eglChooseConfig(Display, @Config[0], @DispConf, 1, @NumConfigs);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.EndEGL();
begin
 eglTerminate(Display);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.StartEGLContext();
var
 Config: array[0..4] of EGLint;
begin
 Config[0]:= EGL_WIDTH;
 Config[1]:= Size.x;
 Config[2]:=	EGL_HEIGHT;
 Config[3]:= Size.y;
 Config[4]:=	EGL_NONE;

 Context:= eglCreateContext(Display, DispConf, nil, nil);
 Surface:= eglCreatePbufferSurface(Display, DispConf, @Config[0]);
 eglMakeCurrent(Display, Surface, Surface, Context);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.EndEGLContext();
begin
 eglMakeCurrent(Display, nil, nil, nil);
 eglDestroyContext(Display, Context);
 eglDestroySurface(Display, Surface);
end;

//---------------------------------------------------------------------------
function TGLESDevice.InitDevice(): Boolean;
begin
 // (1) Setup the required display mode.
 if (not Windowed) then
  begin
   Result:= ChangeVideoMode();
   if (not Result) then Exit;
  end;

 // (2) Create the context for the main window.
 StartEGL();
 StartEGLContext();

 FPixBuffer.SetSize(Size.x, Size.y);
 FPixBuffer.Clear(0);

 FPixBitmap.SetSize(Size.x, Size.y);

 // (3) Specify some default OpenGL states.
 glEnable(GL_TEXTURE_2D);
 glShadeModel(GL_SMOOTH);
 glClearColor(0.0, 0.0, 0.0, 1.0);
 glClearDepthf(1.0);
 glEnable(GL_DEPTH_TEST);
 glDepthFunc(GL_LEQUAL);
 glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.DoneDevice();
begin
 // (1) Restore display mode, if was set during initialization.
 if (ModeChanged) then RestoreDisplayMode();

 // (2) Remove any previously cached contexts.
 EndEGLContext();
 EndEGL();
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.Flip(Handle: THandle);
var
 i: Integer;
begin
 glReadPixels(0, 0, FPixBuffer.Width, FPixBuffer.Height, GL_RGBA,
  GL_UNSIGNED_BYTE, FPixBuffer.Bits);

 for i:= 0 to FPixBuffer.Height - 1 do
  Move(FPixBuffer.Scanline[i]^, FPixBitmap.ScanLine[i]^, FPixBuffer.Width * 4);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.Clear(Color: Cardinal);
begin
 glClearColor((Color and $FF) / 255.0,
  ((Color shr 8) and $FF) / 255.0,
  ((Color shr 16) and $FF) / 255.0,
  ((Color shr 24) and $FF) / 255.0);

 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

 glClearDepthf(0.0);

end;

//---------------------------------------------------------------------------
procedure TGLESDevice.ResetDevice();
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.UpdateParams(ParamType: TParamChangeType);
begin
 case ParamType of
  pctWindowed,
  pctSize:
   begin
    Finalize();
    Initialize();
   end;
 end;
end;

//---------------------------------------------------------------------------
function TGLESDevice.MayRender(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.RenderScene(Handler: TNotifyEvent; Background: Cardinal);
begin
 glViewport(0, 0, Size.x, Size.y);

 Clear(Background);

 EventBeginScene.Notify(Self, nil);
 Handler(Self);
 EventEndScene.Notify(Self, nil);

 Flip(WindowHandle);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.RenderWith(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
begin
 if (hWnd <> WindowHandle) then Exit;

 RenderScene(Handler, Background);
end;

//---------------------------------------------------------------------------
procedure TGLESDevice.RenderToTarget(Handler: TNotifyEvent;
 Background: Cardinal; FillBk: Boolean);
begin
 UsingFrameBuffer:= True;

 if (FillBk) then Clear(Background);

 EventBeginScene.Notify(Self, nil);
 Handler(Self);
 EventEndScene.Notify(Self, nil);

 UsingFrameBuffer:= False;
end;

//---------------------------------------------------------------------------
end.
