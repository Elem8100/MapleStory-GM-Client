unit AsphyreJoystick;
//---------------------------------------------------------------------------
// AsphyreJoystick.pas                                  Modified: 15-Dec-2008
// Joystick/Gamepad DirectInput wrapper for Asphyre              Version 1.05
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
//
// If you require any clarifications about the license, feel free to contact
// us or post your question on our forums at: http://www.afterwarp.net
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
// The Original Code is AsphyreJoystick.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, DirectInput;

//---------------------------------------------------------------------------
type
 TAsphyreJoysticks = class;

//---------------------------------------------------------------------------
 TAsphyreJoystick = class
 private
  FOwner: TAsphyreJoysticks;
  
  FInitialized: Boolean;
  FInputDevice: IDirectInputDevice7;
  FJoyState   : TDIJoyState2;
  FButtonCount: Integer;
  FAxisCount  : Integer;
  FPOVCount   : Integer;
  FDeviceCaps : TDIDevCaps;
  FForeground : Boolean;

  procedure SetForeground(const Value: Boolean);
 public
  property Owner: TAsphyreJoysticks read FOwner;

  property Initialized: Boolean read FInitialized;
  property InputDevice: IDirectInputDevice7 read FInputDevice;
  property DeviceCaps : TDIDevCaps read FDeviceCaps;
  property JoyState   : TDIJoyState2 read FJoyState;
  property Foreground : Boolean read FForeground write SetForeground;

  property ButtonCount: Integer read FButtonCount;
  property AxisCount  : Integer read FAxisCount;
  property POVCount   : Integer read FPOVCount;

  function Initialize(ddi: PDIDeviceInstance; hWnd: Integer): Boolean;
  procedure Finalize();

  function Poll(): Boolean;

  constructor Create(AOwner: TAsphyreJoysticks);
  destructor Destroy(); override;
 published
 end;

//---------------------------------------------------------------------------
 TAsphyreJoysticks = class
 private
  Data: array of TAsphyreJoystick;
  FForeground : Boolean;
  FInitialized: Boolean;
  FWindowHandle: THandle;

  function GetCount(): Integer;
  function GetJoystick(Num: Integer): TAsphyreJoystick;
  procedure ReleaseJoysticks();

  function CreateDirectInput(): Boolean;
 protected
  function AddJoy(): TAsphyreJoystick;
 public
  property WindowHandle: THandle read FWindowHandle write FWindowHandle;

  // Indicates whether the component has been initialized properly.
  property Initialized: Boolean read FInitialized;

  // This indicates whether the component should have joysticks acquired
  // even when the application has no focus.
  property Foreground: Boolean read FForeground write FForeground;

  property Count: Integer read GetCount;
  property Joystick[Num: Integer]: TAsphyreJoystick read GetJoystick; default;

  function Update(): Boolean;
  function Initialize(): Boolean;
  procedure Finalize();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 DX7Types;

//---------------------------------------------------------------------------
type
 PJoysEnumRef = ^TJoysEnumRef;
 TJoysEnumRef = record
  Referrer: TAsphyreJoysticks;
  Success : Boolean;
 end;

//---------------------------------------------------------------------------
 PJoyEnumRef = ^TJoyEnumRef;
 TJoyEnumRef = record
  Referrer: TAsphyreJoystick;
  Success : Boolean;
 end;

//---------------------------------------------------------------------------
function AxisEnumCallback(var ddoi: TDIDeviceObjectInstance;
 Ref: Pointer): LongBool; stdcall;
var
 DIPropRange: TDIPropRange;
 EnumRef    : PJoyEnumRef;
 Res        : Integer;
begin
 // (1) Retreive enumeration reference for further use.
 EnumRef:= Ref;

 // (2) Configure the axis.
 DIPropRange.diph.dwSize:= SizeOf(TDIPropRange);
 DIPropRange.diph.dwHeaderSize:= SizeOf(TDIPropHeader);
 DIPropRange.diph.dwHow:= DIPH_BYID;
 DIPropRange.diph.dwObj:= ddoi.dwType;

 // -> use range [-32768..32767]
 DIPropRange.lMin:= Low(SmallInt);
 DIPropRange.lMax:= High(SmallInt);

 // (3) Set axis properties.
 Res:= EnumRef^.Referrer.InputDevice.SetProperty(DIPROP_RANGE,
  DIPropRange.diph);
 if (Res <> DI_OK) then
  begin
   Result:= DIENUM_STOP;
   EnumRef^.Success:= False;
  end else Result:= DIENUM_CONTINUE;
end;

//---------------------------------------------------------------------------
function JoyEnumCallback(var ddi: TDIDeviceInstance;
 Ref: Pointer): LongBool; stdcall;
var
 EnumRef : PJoysEnumRef;
 Joystick: TAsphyreJoystick;
begin
 // (1) Retreive enumeration reference for further use.
 EnumRef:= Ref;

 // (2) Create new TJoystick class.
 Joystick:= EnumRef^.Referrer.AddJoy();

 // (3) Initialize the created joystick.
 EnumRef^.Success:= Joystick.Initialize(@ddi, EnumRef^.Referrer.WindowHandle);

 if (not EnumRef^.Success) then
  Result:= DIENUM_STOP else Result:= DIENUM_CONTINUE;
end;

//---------------------------------------------------------------------------
constructor TAsphyreJoystick.Create(AOwner: TAsphyreJoysticks);
begin
 inherited Create();

 FOwner:= AOwner;

 FInitialized:= False;
 FForeground := True;
end;

//---------------------------------------------------------------------------
destructor TAsphyreJoystick.Destroy();
begin
 if (FInitialized) then Finalize();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreJoystick.SetForeground(const Value: Boolean);
begin
 if (not FInitialized) then FForeground:= Value;
end;

//---------------------------------------------------------------------------
function TAsphyreJoystick.Initialize(ddi: PDIDeviceInstance;
 hWnd: Integer): Boolean;
var
 EnumRef: TJoyEnumRef;
 Flags  : Cardinal;
begin
 // (2) Create input device.
 Result:= Succeeded(DInput7.CreateDeviceEx(ddi^.guidInstance,
  IID_IDirectInputDevice7, Pointer(FInputDevice), nil));
 if (not Result) then Exit;

 // (3) Set data format.
 Result:= Succeeded(FInputDevice.SetDataFormat(c_dfDIJoystick2));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (4) Prepare cooperative flags.
 Flags:= DISCL_FOREGROUND or DISCL_EXCLUSIVE;
 if (not FForeground) then Flags:= DISCL_BACKGROUND or DISCL_NONEXCLUSIVE;

 // (5) Set joystick cooperative level.
 Result:= Succeeded(FInputDevice.SetCooperativeLevel(hWnd, Flags));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (6) Enumerate joystick axes.
 EnumRef.Referrer:= Self;
 EnumRef.Success := True;

 Result:= Succeeded(FInputDevice.EnumObjects(@AxisEnumCallback, @EnumRef,
  DIDFT_AXIS))and(EnumRef.Success);
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (7) Get device capabilities.
 FillChar(FDeviceCaps, SizeOf(TDIDevCaps), 0);
 FDeviceCaps.dwSize:= SizeOf(TDIDevCaps);
 Result:= Succeeded(FInputDevice.GetCapabilities(FDeviceCaps));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (8) Retreive useful info.
 FButtonCount:= FDeviceCaps.dwButtons;
 FAxisCount  := FDeviceCaps.dwAxes;
 FPOVCount   := FDeviceCaps.dwPOVs;

 // (9) Set status to [Initialized].
 FInitialized:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreJoystick.Finalize();
begin
 if (FInputDevice <> nil) then
  begin
   FInputDevice.Unacquire();
   FInputDevice:= nil;
  end;

 FInitialized:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreJoystick.Poll(): Boolean;
var
 Res: Integer;
begin
 Result:= True;
 
 // (1) Attempt polling Joystick.
 Res:= FInputDevice.Poll();

 // failures?
 if (Res <> DI_OK)and(Res <> DI_NOEFFECT) then
  begin
   // we can handle Lost Input & Non-Acquired problems
   if (Res <> DIERR_INPUTLOST)and(Res <> DIERR_NOTACQUIRED) then
    begin
     Result:= False;
     Exit;
    end;

   // Acquire the device!
   Result:= Succeeded(FInputDevice.Acquire());
   if (Result) then
    begin
     Res:= FInputDevice.Poll();
     if (Res <> DI_OK)and(Res <> DI_NOEFFECT) then
      begin
       Result:= False;
       Exit;
      end;
    end else Exit;
  end;

 // (2) Retreive joystick state.
 Res:= FInputDevice.GetDeviceState(SizeOf(TDIJoyState2), @FJoyState);
 if (Res <> DI_OK) then
  begin
   // we can handle Lost Input & Non-Acquired problems
   if (Res <> DIERR_INPUTLOST)and(Res <> DIERR_NOTACQUIRED) then
    begin
     Result:= False;
     Exit;
    end;

   // Again, try to acquire the device.
   Result:= Succeeded(FInputDevice.Acquire());
   if (Result) then
    begin
     Result:= Succeeded(FInputDevice.GetDeviceState(SizeOf(TDIJoyState2),
      @FJoyState));
     if (not Result) then Exit;
    end;
  end;
end;

//---------------------------------------------------------------------------
constructor TAsphyreJoysticks.Create();
begin
 inherited;

 FForeground := True;
 FInitialized:= False;
end;

//---------------------------------------------------------------------------
destructor TAsphyreJoysticks.Destroy();
begin
 if (FInitialized) then Finalize();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.CreateDirectInput(): Boolean;
begin
 if (DInput7 <> nil) then
  begin
   Result:= True;
   Exit;
  end;

 Result:= Succeeded(DirectInputCreateEx(hInstance, DIRECTINPUT_VERSION,
  IID_IDirectInput7, DInput7, nil));
end;

//---------------------------------------------------------------------------
procedure TAsphyreJoysticks.ReleaseJoysticks();
var
 i: Integer;
begin
 for i:= 0 to Length(Data) - 1 do
  if (Data[i] <> nil) then
   begin
    Data[i].Free();
    Data[i]:= nil;
   end;
   
 SetLength(Data, 0);
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.GetCount(): Integer;
begin
 Result:= Length(Data);
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.GetJoystick(Num: Integer): TAsphyreJoystick;
begin
 if (Num >= 0)and(Num < Length(Data)) then
  Result:= Data[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.AddJoy(): TAsphyreJoystick;
var
 Index: Integer;
begin
 Index:= Length(Data);
 SetLength(Data, Length(Data) + 1);

 Data[Index]:= TAsphyreJoystick.Create(Self);
 Data[Index].Foreground:= FForeground;
 Result:= Data[Index];
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.Initialize(): Boolean;
var
 EnumRef: TJoysEnumRef;
begin
 Result:= CreateDirectInput();
 if (not Result) then Exit;

 // (2) Release any previously created joysticks.
 ReleaseJoysticks();

 // (3) Enumerate joysticks.
 EnumRef.Referrer:= Self;
 EnumRef.Success := False;
 Result:= Succeeded(DInput7.EnumDevices(DIDEVTYPE_JOYSTICK,
  @JoyEnumCallback, @EnumRef, DIEDFL_ATTACHEDONLY))and(EnumRef.Success);
 if (not Result) then ReleaseJoysticks();

 // (4) Set status to [Initialized]
 FInitialized:= Result;
end;

//---------------------------------------------------------------------------
procedure TAsphyreJoysticks.Finalize();
begin
 ReleaseJoysticks();
 FInitialized:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreJoysticks.Update(): Boolean;
var
 i: Integer;
begin
 Result:= True;

 if (not FInitialized) then
  begin
   Result:= Initialize();
   if (not Result) then Exit;
  end;

 for i:= 0 to Length(Data) - 1 do
  if (Data[i] <> nil)and(Data[i].Initialized) then
   begin
    Result:= Data[i].Poll();
    if (not Result) then Break;
   end;
end;

//---------------------------------------------------------------------------
end.
