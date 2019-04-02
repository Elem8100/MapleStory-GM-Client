unit AsphyreMouse;
//---------------------------------------------------------------------------
// AsphyreMouse.pas                                     Modified: 14-Mar-2008
// Mouse DirectInput wrapper for Asphyre                         Version 1.05
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
// The Original Code is AsphyreMouse.pas.
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
 TAsphyreMouse = class
 private
  FInputDevice : IDirectInputDevice7;
  FInitialized : Boolean;
  FForeground  : Boolean;
  FBufferSize  : Integer;
  FDeltaWheel  : Integer;
  FExclusive   : Boolean;
  FWindowHandle: THandle;

  FDeltaX: Integer;
  FDeltaY: Integer;
  MEvent : THandle;

  FStationaryBuffers: Boolean;

  function CreateDirectInput(): Boolean;
  procedure SetForeground(const Value: Boolean);
  procedure SetExclusive(const Value: Boolean);
  procedure SetBufferSize(const Value: Integer);
  function GetPressed(Button: Integer): Boolean;
  function GetReleased(Button: Integer): Boolean;
 protected
  BufClick  : array[0..7] of Integer;
  BufRelease: array[0..7] of Integer;

  procedure ResetButtonState(); virtual;
 public
  // Interface to DirectInput 8 device.
  property InputDevice: IDirectInputDevice7 read FInputDevice;

  // Indicates whether the component has been initialized properly.
  property Initialized: Boolean read FInitialized;

  // This indicates whether the component should have mouse acquired
  // even when the application has no focus.
  property Foreground: Boolean read FForeground write SetForeground;

  // The buffer where mouse events are cached.
  property BufferSize: Integer read FBufferSize write SetBufferSize;

  // This property determines whether the mouse is to be dedicated
  // exclusively to the application.
  property Exclusive: Boolean read FExclusive write SetExclusive;

  // Determines whether the values stored in buffers are preserved
  // throughout Update() calls.
  property StationaryBuffers: Boolean read FStationaryBuffers
   write FStationaryBuffers;

  // Mouse displacement information
  property DeltaX: Integer read FDeltaX;
  property DeltaY: Integer read FDeltaY;
  property DeltaWheel: Integer read FDeltaWheel;

  // Mouse button status
  property Pressed[Button: Integer]: Boolean read GetPressed;
  property Released[Button: Integer]: Boolean read GetReleased;

  // The handle of parent window.
  property WindowHandle: THandle read FWindowHandle write FWindowHandle;

  function Initialize(): Boolean;
  procedure Finalize();
  function Update(): Boolean;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 DX7Types;

//---------------------------------------------------------------------------
constructor TAsphyreMouse.Create();
begin
 inherited;

 FInitialized:= False;
 FForeground := True;
 FBufferSize := 512;
 FExclusive  := True;

 FDeltaWheel := 0;
 FDeltaX:= 0;
 FDeltaY:= 0;

 FStationaryBuffers:= False;
end;

//---------------------------------------------------------------------------
destructor TAsphyreMouse.Destroy();
begin
 if (FInitialized) then Finalize();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMouse.SetForeground(const Value: Boolean);
begin
 if (not FInitialized) then FForeground:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMouse.SetBufferSize(const Value: Integer);
begin
 if (not FInitialized) then FBufferSize:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMouse.SetExclusive(const Value: Boolean);
begin
 if (not FInitialized) then FExclusive:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMouse.ResetButtonState();
var
 i: Integer;
begin
 for i:= Low(BufClick) to High(BufClick) do
  begin
   BufClick[i]  := 0;
   BufRelease[i]:= 0;
  end;
end;

//---------------------------------------------------------------------------
function TAsphyreMouse.CreateDirectInput(): Boolean;
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
function TAsphyreMouse.Initialize(): Boolean;
var
 DIProp: TDIPropDWord;
 Flags : Cardinal;
begin
 Result:= CreateDirectInput();
 if (not Result) then Exit;

 if (FInitialized) then Finalize();

 // (1) Create Mouse DirectInput device.
 Result:= Succeeded(DInput7.CreateDeviceEx(GUID_SysMouse,
  IID_IDirectInputDevice7, Pointer(FInputDevice), nil));
 if (not Result) then Exit;

 // (2) Set Keyboard data format.
 Result:= Succeeded(FInputDevice.SetDataFormat(c_dfDIMouse));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (3) Define device flags.
 Flags:= DISCL_FOREGROUND;
 if (not FForeground) then Flags:= DISCL_BACKGROUND;
 if (FExclusive) then Flags:= Flags or DISCL_EXCLUSIVE
  else Flags:= Flags or DISCL_NONEXCLUSIVE;

 // (4) Set cooperative level.
 Result:= Succeeded(FInputDevice.SetCooperativeLevel(FWindowHandle, Flags));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (5) Create a new event.
 MEvent:= CreateEvent(nil, False, False, nil);
 if (MEvent = 0) then
  begin
   FInputDevice:= nil;
   Result:= False;
   Exit;
  end;

 // (6) Set the recently created event for mouse notifications.
 Result:= Succeeded(FInputDevice.SetEventNotification(MEvent));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (7) Setup property info for mouse buffer size.
 FillChar(DIProp, SizeOf(DIProp), 0);
 with DIProp do
  begin
   diph.dwSize:= SizeOf(TDIPropDWord);
   diph.dwHeaderSize:= SizeOf(TDIPropHeader);
   diph.dwObj:= 0;
   diph.dwHow:= DIPH_DEVICE;
   dwData:= FBufferSize;
  end;

 // (8) Update mouse buffer size.
 Result:= Succeeded(FInputDevice.SetProperty(DIPROP_BUFFERSIZE, DIProp.diph));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 ResetButtonState();
 FInitialized:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMouse.Finalize();
begin
 if (FInputDevice <> nil) then
  begin
   FInputDevice.Unacquire();
   FInputDevice:= nil;
  end;

 FInitialized:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreMouse.Update(): Boolean;
var
 Res: Integer;
 EvCount: Cardinal;
 ObjData: TDIDeviceObjectData;
 EvClick: Integer;
 BtnIndx: Integer;
 EvRelease: Integer;
begin
 Result:= True;

 // (1) Make sure the component is initialized.
 if (not FInitialized) then
  begin
   Result:= Initialize();
   if (not Result) then Exit;
  end;

 FDeltaX:= 0;
 FDeltaY:= 0;
 FDeltaWheel:= 0;
 if (not FStationaryBuffers) then ResetButtonState();

 repeat
  EvCount:= 1;

  // (2) Retreive Mouse Data.
  Res:= FInputDevice.GetDeviceData(SizeOf(TDIDeviceObjectData), @ObjData,
   EvCount, 0);
  if (Res <> DI_OK)and(Res <> DI_BUFFEROVERFLOW) then
   begin
    if (Res <> DIERR_INPUTLOST)and(Res <> DIERR_NOTACQUIRED) then
     begin
      Result:= False;
      Exit;
     end;

    // -> attempt acquiring mouse
    Res:= FInputDevice.Acquire();
    if (Res = DI_OK) then
     begin
      // acquired successfully, attempt retreiving data again
      Res:= FInputDevice.GetDeviceData(SizeOf(TDIDeviceObjectData), @ObjData,
       EvCount, 0);
      if (Res <> DI_OK)and(Res <> DI_BUFFEROVERFLOW) then
       begin
        Result:= False;
        Exit;
       end;
     end else
     begin
      Result:= False;
      Exit;
     end;
   end; // if (Res <> DI_OK)

  // (3) Verify if there's anything in mouse buffer.
  if (EvCount < 1) then Break;

  // (4) Determine event type.
  case ObjData.dwOfs of
   DIMOFS_X: Inc(FDeltaX, Integer(ObjData.dwData));
   DIMOFS_Y: Inc(FDeltaY, Integer(ObjData.dwData));
   DIMOFS_Z: Inc(FDeltaWheel, Integer(ObjData.dwData));

   DIMOFS_BUTTON0..DIMOFS_BUTTON7:
    begin
     // -> determine click - release type
     EvClick  := 0;
     EvRelease:= 1;
     if ((ObjData.dwData and $80) = $80) then
      begin
       EvClick  := 1;
       EvRelease:= 0;
      end;

     BtnIndx:= ObjData.dwOfs - DIMOFS_BUTTON0;
     BufClick[BtnIndx]  := EvClick;
     BufRelease[BtnIndx]:= EvRelease;
    end;
  end;
 until (EvCount < 1);
end;

//---------------------------------------------------------------------------
function TAsphyreMouse.GetPressed(Button: Integer): Boolean;
begin
 if (Button >= 0)and(Button < 8) then
  Result:= (BufClick[Button] > 0) else Result:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreMouse.GetReleased(Button: Integer): Boolean;
begin
 if (Button >= 0)and(Button < 8) then
  Result:= (BufRelease[Button] > 0) else Result:= False;
end;

//---------------------------------------------------------------------------
end.
