unit DX9Devices;
//---------------------------------------------------------------------------
// DX9Devices.pas                                       Modified: 18-Feb-2009
// DirectX 9.0 device management for Asphyre                     Version 1.02
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
// The Original Code is DX9Devices.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Direct3D9, Classes, AbstractDevices, AbstractTextures;

//---------------------------------------------------------------------------

// Remove the dot to prevent any window changes made by Direct3D.
{.$define NoWindowChanges}

// Remove the dot to preserve FPU state.
{$define PreserveFPU}

// Remove the dot to enable multi-threading mode.
{.$define EnableMultithread}

// Remove the dot to enable back-buffer alpha hack (this will try to set the
// back buffer to A8R8G8B8, so screenshots can be taken with alpha-channel on
// some ATI video cards).
{.$define BackBufferAlphaHack}

//---------------------------------------------------------------------------
type
 TDX9Device = class(TAsphyreDevice)
 private
  UsingDepthBuf: Boolean;
  UsingStencil : Boolean;
  IsLostState  : Boolean;

  function FindBackFormat(HiDepth: Boolean; ReqAdapter, ReqWidth,
   ReqHeight: Integer): TD3DFormat;
  function FindDepthFormat(Depth: TDepthStencilType; BackFormat: TD3DFormat;
   ReqAdapter: Integer): TD3DFormat;
  function FindNearestMultisamples(ReqSamples: Integer; ReqAdapter: Cardinal;
   SurfaceFormat, DepthFormat: TD3DFormat;
   ReqWindowed: Boolean): TD3DMultisampleType;
  procedure DefineParams(out Params: TD3DPresentParameters);
  procedure MoveIntoLostState();
  function AttemptRecoverState(): Boolean;
  function HandleDriverError(): Boolean;
  function CheckLostScenario(): Boolean;
  procedure Clear(Color: Cardinal);
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
  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 DX9Types, AsphyreErrors;

//---------------------------------------------------------------------------
const
 BackFormats: array[0..4] of TD3DFormat = (
  {  0 } D3DFMT_A8R8G8B8,
  {  1 } D3DFMT_X8R8G8B8,
  {  2 } D3DFMT_A1R5G5B5,
  {  3 } D3DFMT_X1R5G5B5,
  {  4 } D3DFMT_R5G6B5);

//---------------------------------------------------------------------------
 DepthStencilFormats: array[0..5] of TD3DFormat = (
  {  0 } D3DFMT_D24S8,
  {  1 } D3DFMT_D24X4S4,
  {  2 } D3DFMT_D15S1,
  {  3 } D3DFMT_D32,
  {  4 } D3DFMT_D24X8,
  {  5 } D3DFMT_D16);

//---------------------------------------------------------------------------
constructor TDX9Device.Create();
begin
 inherited;

 Direct3D:= nil;
end;

//---------------------------------------------------------------------------
function TDX9Device.FindBackFormat(HiDepth: Boolean; ReqAdapter, ReqWidth,
 ReqHeight: Integer): TD3DFormat;
const
 HighIndexes: array[0..4] of Integer = (0, 1, 4, 2, 3);
 LowIndexes : array[0..4] of Integer = (4, 2, 3, 0, 1);
var
 IndexPtr : PInteger;
 FormatNo : Integer;
 Format   : TD3DFormat;
 ModeCount: Integer;
 ModeNo   : Integer;
 Mode     : TD3DDisplayMode;
begin
 Result:= D3DFMT_UNKNOWN;
 if (Direct3D = nil) then Exit;

 IndexPtr:= @HighIndexes[0];
 if (not HiDepth) then IndexPtr:= @LowIndexes[0];

 for FormatNo:= 0 to 4 do
  begin
   Format:= BackFormats[IndexPtr^];

   ModeCount:= Direct3D.GetAdapterModeCount(ReqAdapter, Format);
   for ModeNo:= 0 to ModeCount - 1 do
    begin
     if (Succeeded(Direct3D.EnumAdapterModes(ReqAdapter, Format, ModeNo, Mode)))and
      (Integer(Mode.Width) = ReqWidth)and(Integer(Mode.Height) = ReqHeight) then
      begin
       Result:= Format;
       Exit;
      end;
    end;

   Inc(IndexPtr);
  end;
end;

//---------------------------------------------------------------------------
function TDX9Device.FindDepthFormat(Depth: TDepthStencilType;
 BackFormat: TD3DFormat; ReqAdapter: Integer): TD3DFormat;
const
 FormatIndexes: array[TDepthStencilType, 0..5] of Integer = (
  (-1, -1, -1, -1, -1, -1), (3, 0, 1, 4, 5, 2), (0, 1, 2, 3, 4, 5));
var
 FormatNo : Integer;
 Format   : TD3DFormat;
 ModeCount: Integer;
 ModeNo   : Integer;
begin
 Result:= D3DFMT_UNKNOWN;
 if (Direct3D = nil)or(Depth = dsNone) then Exit;

 for FormatNo:= 0 to 5 do
  begin
   Format:= DepthStencilFormats[FormatIndexes[Depth, FormatNo]];

   ModeCount:= Direct3D.GetAdapterModeCount(ReqAdapter, BackFormat);
   for ModeNo:= 0 to ModeCount - 1 do
    if (Succeeded(Direct3D.CheckDeviceFormat(ReqAdapter, D3DDEVTYPE_HAL,
     BackFormat, D3DUSAGE_DEPTHSTENCIL, D3DRTYPE_SURFACE, Format))) then
     begin
      Result:= Format;
      Exit;
     end;
  end;
end;

//---------------------------------------------------------------------------
function TDX9Device.FindNearestMultisamples(ReqSamples: Integer;
 ReqAdapter: Cardinal; SurfaceFormat, DepthFormat: TD3DFormat;
 ReqWindowed: Boolean): TD3DMultisampleType;
var
 MType: TD3DMultisampleType;
 Allowed: Boolean;
 i: Integer;
begin
 Result:= D3DMULTISAMPLE_NONE;
 if (Direct3D = nil) then Exit;

 for i:= ReqSamples downto 2 do
  begin
   MType:= TD3DMultisampleType(i);
   Allowed:= Succeeded(Direct3D.CheckDeviceMultiSampleType(ReqAdapter,
    D3DDEVTYPE_HAL, SurfaceFormat, ReqWindowed, MType, nil));

   if (Allowed)and(DepthFormat <> D3DFMT_UNKNOWN) then
    Allowed:= Succeeded(Direct3D.CheckDeviceMultiSampleType(ReqAdapter,
     D3DDEVTYPE_HAL, DepthFormat, ReqWindowed, MType, nil));

   if (Allowed) then
    begin
     Result:= MType;
     Break;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Device.DefineParams(out Params: TD3DPresentParameters);
var
 Mode: TD3DDisplayMode;
begin
 FillChar(Params, SizeOf(TD3DPresentParameters), 0);

 Params.BackBufferWidth := Size.x;
 Params.BackBufferHeight:= Size.y;
 Params.Windowed        := Windowed;
 Params.hDeviceWindow   := WindowHandle;
 Params.SwapEffect      := D3DSWAPEFFECT_DISCARD;

 Params.PresentationInterval:= D3DPRESENT_INTERVAL_IMMEDIATE;
 if (VSync) then Params.PresentationInterval:= D3DPRESENT_INTERVAL_ONE;

 if (Windowed) then
  begin
   Params.BackBufferFormat:= D3DFMT_UNKNOWN;

   if (Direct3D <> nil)and
    (Succeeded(Direct3D.GetAdapterDisplayMode(Adapter, Mode))) then
    Params.BackBufferFormat:= Mode.Format;
  end else Params.BackBufferFormat:= FindBackFormat(HighBitDepth, Adapter,
   Size.x, Size.y);

 if (DepthStencil <> dsNone) then
  begin
   Params.EnableAutoDepthStencil:= True;
   Params.Flags:= D3DPRESENTFLAG_DISCARD_DEPTHSTENCIL;
   Params.AutoDepthStencilFormat:= FindDepthFormat(DepthStencil,
    Params.BackBufferFormat, Adapter);
  end;

 if (DepthStencil <> dsNone) then
  begin
   Params.MultiSampleType:= FindNearestMultisamples(Multisamples,
    Adapter, Params.BackBufferFormat, Params.AutoDepthStencilFormat,
    Params.Windowed);
  end else
  begin
   Params.MultiSampleType:= FindNearestMultisamples(Multisamples,
    Adapter, Params.BackBufferFormat, D3DFMT_UNKNOWN, Params.Windowed);
  end;

 {$ifdef BackBufferAlphaHack}
 Params.BackBufferFormat:= D3DFMT_A8R8G8B8;
 {$endif}
end;

//---------------------------------------------------------------------------
function TDX9Device.InitDevice(): Boolean;
var
 Flags: Cardinal;
begin
 Result:= (Direct3D = nil)and(Device9 = nil);
 if (not Result) then Exit;

 Direct3D:= Direct3DCreate9(D3D_SDK_VERSION);
 if (Direct3D = nil) then
  begin
   Errors.Insert(errCreateDirect3D, Self, ClassName, 'InitDevice');
   Result:= False;
   Exit;
  end;

 DefineParams(Params9);

 UsingDepthBuf := (DepthStencil <> dsNone);
 UsingStencil  := (DepthStencil = dsDepthStencil);

 Flags:= 0;

 {$ifdef NoWindowChanges}
 Flags:= Flags or D3DCREATE_NOWINDOWCHANGES;
 {$endif}

 {$ifdef PreserveFPU}
 Flags:= Flags or D3DCREATE_FPU_PRESERVE;
 {$endif}

 {$ifdef EnableMultithread}
 Flags:= Flags or D3DCREATE_MULTITHREADED;
 {$endif}

 case VertexProcessing of
  vptMixed:
   Result:= Succeeded(Direct3D.CreateDevice(Adapter, D3DDEVTYPE_HAL,
    WindowHandle, Flags or D3DCREATE_MIXED_VERTEXPROCESSING, @Params9,
    Device9));

  vptHardware:
   begin
    Result:= Succeeded(Direct3D.CreateDevice(Adapter, D3DDEVTYPE_HAL,
     WindowHandle, Flags or D3DCREATE_HARDWARE_VERTEXPROCESSING, @Params9,
     Device9));

    if (not Result) then
     Result:= Succeeded(Direct3D.CreateDevice(Adapter, D3DDEVTYPE_HAL,
      WindowHandle, Flags or D3DCREATE_SOFTWARE_VERTEXPROCESSING, @Params9,
      Device9));
   end;

  vptSoftware:
   Result:= Succeeded(Direct3D.CreateDevice(Adapter, D3DDEVTYPE_HAL,
    WindowHandle, Flags or D3DCREATE_SOFTWARE_VERTEXPROCESSING, @Params9,
    Device9));
 end;

 if (Result) then
  begin
   Result:= Succeeded(Device9.GetDeviceCaps(Caps9));
   if (not Result) then
    Errors.Insert(errRetreiveDeviceCaps, Self, ClassName, 'InitDevice');
  end else Errors.Insert(errCreateDirect3DDevice, Self, ClassName, 'InitDevice');

 Adapter9:= Adapter;
 IsLostState:= False;
end;

//---------------------------------------------------------------------------
procedure TDX9Device.DoneDevice();
begin
 if (Device9 <> nil) then Device9:= nil;
 if (Direct3D <> nil) then Direct3D:= nil;
 
 FillChar(Caps9, SizeOf(TD3DCaps9), 0);
 FillChar(Params9, SizeOf(TD3DPresentParameters), 0);
 Adapter9:= 0;
end;

//---------------------------------------------------------------------------
procedure TDX9Device.MoveIntoLostState();
begin
 if (not IsLostState) then
  begin
   EventDeviceLost.Notify(Self, nil);
   IsLostState:= True;
  end;
end;

//---------------------------------------------------------------------------
function TDX9Device.AttemptRecoverState(): Boolean;
begin
 Result:= Device9 <> nil;
 if (not Result) then Exit;

 if (IsLostState) then
  begin
   Result:= Succeeded(Device9.Reset(Params9));
   if (Result) then
    begin
     IsLostState:= False;
     EventDeviceReset.Notify(Self, nil);
    end;
  end;
end;

//---------------------------------------------------------------------------
function TDX9Device.HandleDriverError(): Boolean;
begin
 MoveIntoLostState();
 Result:= AttemptRecoverState();
end;

//---------------------------------------------------------------------------
function TDX9Device.CheckLostScenario(): Boolean;
var
 Res: HResult;
begin
 Result:= (Device9 <> nil);
 if (not Result) then Exit;

 Res:= Device9.TestCooperativeLevel();

 case Res of
  D3DERR_DEVICELOST:
   begin
    MoveIntoLostState();
    Result:= False;
   end;

  D3DERR_DEVICENOTRESET:
   begin
    if (not IsLostState) then MoveIntoLostState();
    Result:= AttemptRecoverState();
   end;

  D3DERR_DRIVERINTERNALERROR:
   Result:= HandleDriverError();

  D3D_OK:
   Result:= True;

  else Result:= False;
 end;
end;

//---------------------------------------------------------------------------
procedure TDX9Device.ResetDevice();
begin
 MoveIntoLostState();
 AttemptRecoverState();
end;

//---------------------------------------------------------------------------
function TDX9Device.MayRender(): Boolean;
begin
 Result:= CheckLostScenario();
end;

//---------------------------------------------------------------------------
procedure TDX9Device.UpdateParams(ParamType: TParamChangeType);
begin
 MoveIntoLostState();

 Params9.BackBufferWidth := Size.x;
 Params9.BackBufferHeight:= Size.y;
 Params9.Windowed        := Windowed;

 Params9.PresentationInterval:= D3DPRESENT_INTERVAL_IMMEDIATE;
 if (VSync) then Params9.PresentationInterval:= D3DPRESENT_INTERVAL_ONE;

 AttemptRecoverState();
end;

//---------------------------------------------------------------------------
procedure TDX9Device.Clear(Color: Cardinal);
var
 ClearFlags: Cardinal;
begin
 ClearFlags:= D3DCLEAR_TARGET;

 if (UsingDepthBuf) then
  begin
   ClearFlags:= ClearFlags or D3DCLEAR_ZBUFFER;
   if (UsingStencil) then ClearFlags:= ClearFlags or D3DCLEAR_STENCIL;
  end;

 Device9.Clear(0, nil, ClearFlags, Color, FillDepthValue, FillStencilValue);
end;

//---------------------------------------------------------------------------
procedure TDX9Device.RenderWith(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
begin
 if (Device9 = nil) then Exit;

 Clear(Background);

 if (Succeeded(Device9.BeginScene())) then
  begin
   EventBeginScene.Notify(Self, nil);

   Handler(Self);

   EventEndScene.Notify(Self, nil);
   Device9.EndScene();
  end;

 Device9.Present(nil, nil, hWnd, nil);
end;

//---------------------------------------------------------------------------
procedure TDX9Device.RenderToTarget(Handler: TNotifyEvent; Background: Cardinal;
 FillBk: Boolean);
begin
 if (FillBk) then
  Clear(Background);

 if (Succeeded(Device9.BeginScene())) then
  begin
   EventBeginScene.Notify(Self, nil);

   Handler(Self);

   EventEndScene.Notify(Self, nil);
   Device9.EndScene();
  end;
end;

//---------------------------------------------------------------------------
end.
