unit DX7Devices;
//---------------------------------------------------------------------------
// DX7Devices.pas                                       Modified: 17-Sep-2008
// DirectDraw + Direct3D devices using DirectX 7.0               Version 1.04
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
// The Original Code is DX7Textures.pas.
//
// The Initial Developer of the Original Code is M. Sc. Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007,
// M. Sc. Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, DirectDraw7, Direct3D7, Classes, AbstractDevices;

//---------------------------------------------------------------------------

// Remove the dot to prevent any window changes made by DirectDraw.
{.$define NoWindowChanges}

// Remove the dot to preserve FPU state.
{$define PreserveFPU}

// Remove the dot to enable multi-threading mode.
{.$define EnableMultithread}

//---------------------------------------------------------------------------
type
 TCurrentDeviceState = (cdsOkay, cdsLost, cdsNeedReset);

//---------------------------------------------------------------------------
 TDX7Device = class(TAsphyreDevice)
 private
  FrontBuffer: IDirectDrawSurface7;
  FBackBuffer: IDirectDrawSurface7;
  FrontDesc  : TDDSurfaceDesc2;
  DestMonitor: HMonitor;


  LostState: Boolean;

  function SetCooperativeLevel(): Boolean;
  function SetDisplayMode(): Boolean;
  function CreateFrontBuffer(): Boolean;
  function CreateBackBuffer(): Boolean;
  function CreateClipper(): Boolean;

  procedure UpdateClipper(hWnd: THandle);
  procedure FlipWindowed(hWnd: THandle);
  procedure Flip();
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
  property BackBuffer: IDirectDrawSurface7 read FBackBuffer;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors, DX7Types, DX7AdapterInfo;

//---------------------------------------------------------------------------
type
 TWindowInfo = packed record
  cbSize         : DWORD;
  rcWindow       : TRect;
  rcClient       : TRect;
  dwStyle        : DWORD;
  dwExStyle      : DWORD;
  dwOtherStuff   : DWORD;
  cxWindowBorders: UINT;
  cyWindowBorders: UINT;
  atomWindowType : TAtom;
  wCreatorVersion: WORD;
 end;

//---------------------------------------------------------------------------
 PMonitorInfoEx = ^TMonitorInfoEx;
 TMonitorInfoEx = record
  Size    : Longword;
  MnRect  : TRect;
  WorkArea: TRect;
  Flags   : Longword;
  Device  : array[0..CCHDEVICENAME - 1] of Char;
 end;

//---------------------------------------------------------------------------
function GetWindowInfo(hwnd: HWND;
 var pwi: TWindowInfo): BOOL; stdcall; external 'user32.dll';

function GetMonitorInfoA(Monitor: HMonitor;
 Info: PMonitorInfoEx): LongBool; stdcall; external 'user32.dll';

//---------------------------------------------------------------------------
constructor TDX7Device.Create();
begin
 inherited;

 FrontBuffer:= nil;
 FBackBuffer := nil;

 FillChar(FrontDesc, SizeOf(TDDSurfaceDesc2), 0);
end;

//---------------------------------------------------------------------------
function TDX7Device.SetCooperativeLevel(): Boolean;
var
 Flags: Cardinal;
begin
 if (Windowed) then Flags:= DDSCL_NORMAL
  else Flags:= DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN or DDSCL_ALLOWREBOOT;

 {$ifdef NoWindowChanges}
 Flags:= Flags or DDSCL_NOWINDOWCHANGES;
 {$endif}

 {$ifdef PreserveFPU}
 Flags:= Flags or DDSCL_FPUPRESERVE;
 {$endif}

 {$ifdef EnableMultithread}
 Flags:= Flags or DDSCL_MULTITHREADED;
 {$endif}

 Result:= Succeeded(DirectDraw.SetCooperativeLevel(WindowHandle, Flags));

 if (not Result) then
  Errors.Insert(errCooperativeLevel, Self, ClassName, 'SetCooperativeLevel');
end;

//---------------------------------------------------------------------------
function TDX7Device.SetDisplayMode(): Boolean;
var
 BitCount: Integer;
begin
 if (not Windowed) then
  begin
   BitCount:= 32;
   if (not HighBitDepth) then BitCount:= 16;

   Result:= Succeeded(DirectDraw.SetDisplayMode(Size.x, Size.y, BitCount, 0, 0));

   // -> If failed setting 32-bit video mode, try 16-bit just in case.
   if (not Result)and(BitCount = 32) then
    Result:= Succeeded(DirectDraw.SetDisplayMode(Size.x, Size.y, 16, 0, 0));
  end else Result:= True;

 if (not Result) then
  Errors.Insert(errSetDisplayMode, Self, ClassName, 'SetDisplayMode');
end;

//---------------------------------------------------------------------------
function TDX7Device.CreateFrontBuffer(): Boolean;
begin
 FillChar(FrontDesc, SizeOf(TDDSurfaceDesc2), 0);

 FrontDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 if (Windowed) then
  begin
   FrontDesc.dwFlags:= DDSD_CAPS;
   FrontDesc.ddsCaps.dwCaps:= DDSCAPS_PRIMARYSURFACE;
  end else
  begin
   FrontDesc.dwFlags:= DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
   FrontDesc.ddsCaps.dwCaps:= DDSCAPS_PRIMARYSURFACE or DDSCAPS_FLIP or
    DDSCAPS_COMPLEX or DDSCAPS_3DDEVICE;
   FrontDesc.dwBackBufferCount:= 1;
  end;

 Result:= Succeeded(DirectDraw.CreateSurface(FrontDesc, FrontBuffer, nil));

 if (not Result) then
  Errors.Insert(errCreateSurface, Self, ClassName, 'CreateFrontBuffer');

 if (Result) then
  begin
   Result:= Succeeded(FrontBuffer.GetSurfaceDesc(FrontDesc));

   if (not Result) then
    Errors.Insert(errGetSurfaceDesc, Self, ClassName, 'CreateFrontBuffer');
  end;
end;

//---------------------------------------------------------------------------
function TDX7Device.CreateBackBuffer(): Boolean;
var
 BackDesc: TDDSurfaceDesc2;
 Caps: TDDSCaps2;
begin
 FillChar(BackDesc, SizeOf(TDDSurfaceDesc2), 0);

 BackDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 if (Windowed) then
  begin
   BackDesc.dwFlags:= DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH;
   BackDesc.ddsCaps.dwCaps:= DDSCAPS_OFFSCREENPLAIN or DDSCAPS_3DDEVICE;
   BackDesc.dwWidth := Size.x;
   BackDesc.dwHeight:= Size.y;

   Result:= Succeeded(DirectDraw.CreateSurface(BackDesc, FBackBuffer, nil));
  end else
  begin
   FillChar(Caps, SizeOf(TDDSCaps2), 0);

   Caps.dwCaps:= DDSCAPS_BACKBUFFER;

   Result:= Succeeded(FrontBuffer.GetAttachedSurface(Caps, FBackBuffer));
  end;

 if (not Result) then
  Errors.Insert(errCreateSurface, Self, ClassName, 'CreateBackBuffer');
end;

//---------------------------------------------------------------------------
function TDX7Device.CreateClipper(): Boolean;
var
 Clipper: IDirectDrawClipper;
begin
 Result:= Succeeded(DirectDraw.CreateClipper(0, Clipper, nil));
 if (not Result) then
  begin
   Errors.Insert(errCreateClipper, Self, ClassName, 'CreateClipper');
   Exit;
  end;

 Clipper.SetHWnd(0, WindowHandle);
 FrontBuffer.SetClipper(Clipper);

 Clipper:= nil;
 Result:= True;
end;

//---------------------------------------------------------------------------
function TDX7Device.InitDevice(): Boolean;
var
 Guid: PGuid;
 Info: PDX7AdapterInfo;
begin
 Result:= (DirectDraw = nil)and(Direct3D = nil)and(Device7 = nil);
 if (not Result) then Exit;

 // Step 1. Find GUID for the specified adapter.
 Guid:= nil;
 DestMonitor:= 0;

 if (DX7AdapterHelper.Update()) then
  begin
   Info:= DX7AdapterHelper.AdapterInfo[Adapter];
   if (Info <> nil) then
    begin
     DestMonitor:= Info^.Monitor;
     Guid:= @Info^.Guid;
    end;
  end;

 // Step 2. Create DirectDraw interface.
 Result:= Succeeded(DirectDrawCreateEx(Guid, DirectDraw, IID_IDirectDraw7, nil));
 if (not Result) then
  begin
   Errors.Insert(errCreateDirectDraw, Self, ClassName, 'InitDevice');
   Exit;
  end;

 // Step 3. Create Direct3D interface.
 Result:= Succeeded(DirectDraw.QueryInterface(IID_IDirect3D7, Direct3D));
 if (not Result) then
  begin
   DirectDraw:= nil;
   Errors.Insert(errCreateDirect3D, Self, ClassName, 'InitDevice');
   Exit;
  end;

 // Step 4. Set the particular cooperative mode.
 Result:= SetCooperativeLevel();
 if (not Result) then
  begin
   Direct3D  := nil;
   DirectDraw:= nil;
   Exit;
  end;

 // Step 5. Change the current display mode.
 Result:= SetDisplayMode();
 if (not Result) then
  begin
   Direct3D  := nil;
   DirectDraw:= nil;
   Exit;
  end;

 // Step 6. Create primary surface as a Front Buffer.
 Result:= CreateFrontBuffer();
 if (not Result) then
  begin
   Direct3D  := nil;
   DirectDraw:= nil;
   Exit;
  end;

 // Step 7. Create offscreen surface as a Back Buffer.
 Result:= CreateBackBuffer();
 if (not Result) then
  begin
   FrontBuffer:= nil;
   Direct3D   := nil;
   DirectDraw := nil;
   Exit;
  end;

 // Step 8. Create Clipper object.
 Result:= CreateClipper();
 if (not Result) then
  begin
   FBackBuffer:= nil;
   FrontBuffer:= nil;
   Direct3D   := nil;
   DirectDraw := nil;
   Exit;
  end;

 // Step 9. Create Direct3D device for 3D rendering.
 Result:= Succeeded(Direct3D.CreateDevice(IID_IDirect3DHALDevice, FBackBuffer,
  Device7));
 if (not Result) then
  begin
   FBackBuffer:= nil;
   FrontBuffer:= nil;
   Direct3D   := nil;
   DirectDraw := nil;
   Errors.Insert(errCreateDirect3DDevice, Self, ClassName, 'InitDevice');
   Exit;
  end;

 LostState:= False;
end;

//---------------------------------------------------------------------------
procedure TDX7Device.DoneDevice();
begin
 Device7    := nil;
 FBackBuffer:= nil;
 FrontBuffer:= nil;

 if (DirectDraw <> nil)and(not Windowed) then DirectDraw.RestoreDisplayMode();

 Direct3D  := nil;
 DirectDraw:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Device.ResetDevice();
begin
 FrontBuffer._Restore();
 FBackBuffer._Restore();
end;

//---------------------------------------------------------------------------
function TDX7Device.MayRender(): Boolean;
var
 Res: HResult;
 IsLost, NeedReset: Boolean;
begin
 Res   := DirectDraw.TestCooperativeLevel();
 IsLost:= Failed(Res);

 NeedReset:= ((LostState)and(not IsLost))or((Windowed)and(Res = DDERR_WRONGMODE));

 // Case 1. The device has been lost.
 if (IsLost)and(not LostState) then
  begin
   LostState:= True;
   Result:= False;
   Exit;
  end;

 // Case 2. The device has been recovered.
 if (LostState)and(not IsLost) then
  begin
   Reset();
   LostState:= False;
   Result:= True;
   Exit;
  end;

 // Case 3. The device is lost, but may be recovered (later on).
 if (IsLost)and(NeedReset) then
  begin
   Reset();
   LostState:= False;
   Result:= False;
   Exit;
  end;

 // Case 4. The device is still lost.
 if (IsLost) then
  begin
   Result:= False;
   Exit;
  end;

 // Case 5. The device is operational.
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TDX7Device.UpdateParams(ParamType: TParamChangeType);
begin
 Finalize();
 Initialize();
end;

//---------------------------------------------------------------------------
procedure TDX7Device.UpdateClipper(hWnd: THandle);
var
 Clipper: IDirectDrawClipper;
begin
 if (Failed(FrontBuffer.GetClipper(Clipper))) then
  begin
   Errors.Insert(errRetreiveClipper, Self, ClassName, 'UpdateClipper');
   Exit;
  end;

 Clipper.SetHWnd(0, hWnd);
 Clipper:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Device.FlipWindowed(hWnd: THandle);
var
 Info: TWindowInfo;
 Rect: TRect;
 MonInfo: TMonitorInfoEx;
begin
 if (hWnd <> WindowHandle) then UpdateClipper(hWnd);

 FillChar(Info, SizeOf(TWindowInfo), 0);
 Info.cbSize:= SizeOf(TWindowInfo);

 if (not GetWindowInfo(hWnd, Info)) then
  begin
   Errors.Insert(errWindowInformation, Self, ClassName, 'FlipWindowed');
   Exit;
  end;

 Rect:= Info.rcClient;

 if (DestMonitor <> 0) then
  begin
   FillChar(MonInfo, SizeOf(TMonitorInfoEx), 0);
   MonInfo.Size:= SizeOf(TMonitorInfoEx);

   if (not GetMonitorInfoA(DestMonitor, @MonInfo)) then
    begin
     Errors.Insert(errMonitorInformation, Self, ClassName, 'FlipWindowed');
     Exit;
    end;

   Dec(Rect.Left, MonInfo.MnRect.Left);
   Dec(Rect.Top, MonInfo.MnRect.Top);
   Dec(Rect.Right, MonInfo.MnRect.Left);
   Dec(Rect.Bottom, MonInfo.MnRect.Top);
  end;

 if (VSync) then DirectDraw.WaitForVerticalBlank(DDWAITVB_BLOCKBEGIN, 0);

 FrontBuffer.Blt(@Rect, FBackBuffer, nil, DDBLT_WAIT, nil);
end;

//---------------------------------------------------------------------------
procedure TDX7Device.Flip();
begin
 if (VSync) then FrontBuffer.Flip(nil, DDFLIP_WAIT)
  else FrontBuffer.Flip(nil, DDFLIP_WAIT or DDFLIP_NOVSYNC);
end;

//---------------------------------------------------------------------------
procedure TDX7Device.RenderWith(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
begin
 Device7.Clear(0, nil, D3DCLEAR_TARGET, Background, 0.0, 0);

 if (Succeeded(Device7.BeginScene())) then
  begin
   EventBeginScene.Notify(Self, nil);

   Handler(Self);

   EventEndScene.Notify(Self, nil);
   Device7.EndScene();
  end;

 if (Windowed) then FlipWindowed(hWnd) else Flip();
end;

//---------------------------------------------------------------------------
procedure TDX7Device.RenderToTarget(Handler: TNotifyEvent; Background: Cardinal;
 FillBk: Boolean);
begin
 if (FillBk) then
  Device7.Clear(0, nil, D3DCLEAR_TARGET, Background, 0.0, 0);

 if (Succeeded(Device7.BeginScene())) then
  begin
   EventBeginScene.Notify(Self, nil);

   Handler(Self);

   EventEndScene.Notify(Self, nil);
   Device7.EndScene();
  end;
end;

//---------------------------------------------------------------------------
end.
