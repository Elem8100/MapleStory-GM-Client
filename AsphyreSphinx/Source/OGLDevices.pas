unit OGLDevices;
//---------------------------------------------------------------------------
// OGLDevices.pas                                       Modified: 24-Feb-2009
// OpenGL device and window handling                             Version 1.03
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
// The Original Code is OGLDevices.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SysUtils, Classes, AbstractDevices, OGLContexts;

//---------------------------------------------------------------------------
type
 TOGLDevice = class(TAsphyreDevice)
 private
  ModeChanged: Boolean;
  FContexts  : TOGLContexts;

  function ChangeVideoMode(): Boolean;
  procedure RestoreDisplayMode();
  procedure SaveWindowState();
  procedure RestoreWindowState();

  procedure UpdateVSync();

  procedure Clear(Color: Cardinal);
  procedure Flip(Handle: THandle);
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
  property Contexts: TOGLContexts read FContexts;

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
 AsphyreGL, AsphyreErrors, WinAdapterInfo;

//---------------------------------------------------------------------------
var
 OpenGLCreated: Boolean = False;

//---------------------------------------------------------------------------
constructor TOGLDevice.Create();
begin
 inherited;

 FContexts  := TOGLContexts.Create();
 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
destructor TOGLDevice.Destroy();
begin
 FreeAndNil(FContexts);

 inherited;
end;

//---------------------------------------------------------------------------
function TOGLDevice.ChangeVideoMode(): Boolean;
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
 FContexts.ColorBits:= WinAdapterHelper.Mode[Mode]^.BitDepth;
 ModeChanged:= True;
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.RestoreDisplayMode();
begin
 WinAdapterHelper.RestoreTempMode(Adapter);

 RestoreWindowState();

 ModeChanged:= False;
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.SaveWindowState();
begin
 // to-do
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.RestoreWindowState();
begin
 // to-do
end;

//---------------------------------------------------------------------------
function TOGLDevice.InitDevice(): Boolean;
begin
 // (1) Check whether OpenGL has been created.
 Result:= OpenGLCreated;
 if (not Result) then Exit;

 // (2) Setup the required display mode.
 if (not Windowed) then
  begin
   Result:= ChangeVideoMode();
   if (not Result) then Exit;
  end;

 // (3) Create the context for the main window.
 Result:= FContexts.Activate(WindowHandle, 0);
 if (not Result) then
  begin
   RestoreDisplayMode();
   Exit;
  end;

 // (4) Retreive OpenGL extensions.
 ReadExtensions();
 ReadImplementationProperties();

 // (5) Set VSync parameter.
 UpdateVSync();

 // (6) Specify some default OpenGL states.
 glEnable(GL_TEXTURE_2D);
 glShadeModel(GL_SMOOTH);
 glClearColor(0.0, 0.0, 0.0, 1.0);
 glClearDepth(1.0);
 glEnable(GL_DEPTH_TEST);
 glDepthFunc(GL_LEQUAL);
 glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.DoneDevice();
begin
 // (1) Restore display mode, if was set during initialization.
 if (ModeChanged) then RestoreDisplayMode();

 // (2) Remove any previously cached contexts.
 if (FContexts <> nil) then FContexts.RemoveAll();
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.UpdateVSync();
var
 Interval: Integer;
begin
 if (WGL_EXT_swap_control) then
  begin
   Interval:= wglGetSwapIntervalEXT();

   if (VSync)and(Interval <> 1) then wglSwapIntervalEXT(1);
   if (not VSync)and(Interval <> 0) then wglSwapIntervalEXT(0);
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.Flip(Handle: THandle);
var
 Index: Integer;
begin
 Index:= FContexts.IndexOf(Handle);
 if (Index <> -1) then SwapBuffers(FContexts[Index]^.WinDC);
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.Clear(Color: Cardinal);
begin
 glClearColor(
  ((Color shr 16) and $FF) / 255.0,
  ((Color shr 8) and $FF) / 255.0,
  (Color and $FF) / 255.0, ((Color shr 24) and $FF) / 255.0);

 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.ResetDevice();
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.UpdateParams(ParamType: TParamChangeType);
begin
 case ParamType of
  pctVSync:
   UpdateVSync();

  pctWindowed:
   begin
    Finalize();
    Initialize();
   end;
 end;
end;

//---------------------------------------------------------------------------
function TOGLDevice.MayRender(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.RenderScene(Handler: TNotifyEvent; Background: Cardinal);
begin
 glViewport(0, 0, Size.x, Size.y);

 Clear(Background);

 EventBeginScene.Notify(Self, nil);
 Handler(Self);
 EventEndScene.Notify(Self, nil);

 Flip(WindowHandle);
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.RenderWith(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
begin
 if (hWnd = WindowHandle) then
  begin
   RenderScene(Handler, Background);
   Exit;
  end;

 if (not FContexts.Activate(hWnd, WindowHandle)) then Exit;

 UpdateVSync();

 Clear(Background);

 EventBeginScene.Notify(Self, nil);
 Handler(Self);
 EventEndScene.Notify(Self, nil);

 Flip(hWnd);
end;

//---------------------------------------------------------------------------
procedure TOGLDevice.RenderToTarget(Handler: TNotifyEvent;
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
initialization
 OpenGLCreated:= InitOpenGL();

//---------------------------------------------------------------------------
end.
