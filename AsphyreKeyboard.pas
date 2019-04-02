unit AsphyreKeyboard;
//---------------------------------------------------------------------------
// AsphyreKeyboard.pas                                  Modified: 10-Oct-2005
// Copyright (c) 2000 - 2005  Afterwarp Interactive              Version 1.01
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
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Types, Classes, SysUtils, Forms, DirectInput;

//---------------------------------------------------------------------------
type
 TDIKeyBuf = array[0..255] of Byte;

//---------------------------------------------------------------------------
 TAsphyreKeyboard = class(TComponent)
 private
  FInputDevice: IDirectInputDevice8;
  FForeground : Boolean;
  FInitialized: Boolean;

  Buffer : TDIKeyBuf;
  PrevBuf: TDIKeyBuf;

  function CheckDirectInput(): Boolean;
  procedure SetForeground(const Value: Boolean);
  function GetKeyBuffer(): Pointer;
  function GetKey(KeyNum: Integer): Boolean;
  function GetKeyPressed(KeyNum: Integer): Boolean;
  function GetKeyReleased(KeyNum: Integer): Boolean;
  function GetKeyName(KeyNum: Integer): string;
  function VKeyToNum(VCode: Cardinal): Integer;
  function GetVKey(VCode: Cardinal): Boolean;
  function GetVKeyPressed(VCode: Cardinal): Boolean;
  function GetVKeyReleased(VCode: Cardinal): Boolean;
  function GetVKeyName(VCode: Cardinal): string;
 public
  property InputDevice: IDirectInputDevice8 read FInputDevice;
  property KeyBuffer  : Pointer read GetKeyBuffer;
  property Initialized: Boolean read FInitialized;

  //================================================================
  // Scancode Key Status (use DIK_[key] constants to access these)
  //================================================================

  // keyboard key status (DIK_[key] constants)
  property Key[KeyNum: Integer]: Boolean read GetKey;
  property KeyName[KeyNum: Integer]: string read GetKeyName;
  property KeyPressed[KeyNum: Integer]: Boolean read GetKeyPressed;
  property KeyReleased[KeyNum: Integer]: Boolean read GetKeyReleased;

  //================================================================
  // Virtual Keys (use VK_[key] constants to access these)
  //================================================================
  property VKey[VCode: Cardinal]: Boolean read GetVKey;
  property VKeyName[VCode: Cardinal]: string read GetVKeyName;
  property VKeyPressed[VCode: Cardinal]: Boolean read GetVKeyPressed;
  property VKeyReleased[VCode: Cardinal]: Boolean read GetVKeyReleased;

  function Update(): Boolean;
  function Initialize(): Boolean;
  procedure Finalize();

  constructor Create(AOwner: TComponent); override;
  destructor Destroy(); override;
 published
  property Foreground: Boolean read FForeground write SetForeground;
 end;

//---------------------------------------------------------------------------
implementation
var
   DirectInput8  : IDirectInput8    = nil;
//---------------------------------------------------------------------------
constructor TAsphyreKeyboard.Create(AOwner: TComponent);
begin
 inherited;

 if (not (AOwner is TCustomForm)) then
  raise Exception.Create(ClassName + ': This component must be dropped onto the form!');

 FForeground := True;
 FInitialized:= False;
end;

//---------------------------------------------------------------------------
destructor TAsphyreKeyboard.Destroy();
begin
 if (FInitialized) then Finalize();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreKeyboard.SetForeground(const Value: Boolean);
begin
 if (not FInitialized) then
  FForeground:= Value;
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.CheckDirectInput(): Boolean;
begin
 Result:= (DirectInput8 <> nil)or(Succeeded(DirectInput8Create(hInstance,
  DIRECTINPUT_VERSION, IID_IDirectInput8, DirectInput8, nil)));
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.Initialize(): Boolean;
begin
 // (1) Verify conditions.
 if (FInitialized) then
  begin
   Result:= False;
   Exit;
  end;

 // (2) Verify that DirectInput interface exists.
 Result:= CheckDirectInput();
 if (not Result) then Exit;

 // (3) Create Keyboard device
 Result:= Succeeded(DirectInput8.CreateDevice(GUID_SysKeyboard,
  FInputDevice, nil));
 if (not Result) then Exit;

 // (4) Set Keyboard data format.
 Result:= Succeeded(FInputDevice.SetDataFormat(c_dfDIKeyboard));
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 // (5) Set cooperative level.
 if (FForeground) then
  begin // foreground cooperative level
   Result:= Succeeded(FInputDevice.SetCooperativeLevel(TCustomForm(Owner).Handle,
    DISCL_FOREGROUND or DISCL_NONEXCLUSIVE));
  end else
  begin // background cooperative level
   Result:= Succeeded(FInputDevice.SetCooperativeLevel(TCustomForm(Owner).Handle,
    DISCL_BACKGROUND or DISCL_NONEXCLUSIVE));
  end;
 if (not Result) then
  begin
   FInputDevice:= nil;
   Exit;
  end;

 FillChar(Buffer, SizeOf(TDIKeyBuf), 0);
 FillChar(PrevBuf, SizeOf(TDIKeyBuf), 0);
 FInitialized:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreKeyboard.Finalize();
begin
 if (FInputDevice <> nil) then
  begin
   FInputDevice.Unacquire();
   FInputDevice:= nil;
  end;

 FInitialized:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetKeyBuffer(): Pointer;
begin
 Result:= @Buffer;
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.Update(): Boolean;
var
 Res: Integer;
begin
 Result:= True;

 // (1) Verify initial conditions.
 if (not FInitialized) then
  begin
   Result:= Initialize();
   if (not Result) then Exit;
  end;

 // (2) Save current buffer state.
 Move(Buffer, PrevBuf, SizeOf(TDIKeyBuf));

 // (3) Attempt to retreive device state.
 Res:= FInputDevice.GetDeviceState(SizeOf(TDIKeyBuf), @Buffer);
 if (Res <> DI_OK) then
  begin
   // -> can the error be corrected?
   if (Res <> DIERR_INPUTLOST)and(Res <> DIERR_NOTACQUIRED) then
    begin
     Result:= False;
     Exit;
    end;

   // -> device might not be acquired!
   Res:= FInputDevice.Acquire();
   if (Res = DI_OK) then
    begin
     // acquired successfully, now try retreiving the state again
     Res:= FInputDevice.GetDeviceState(SizeOf(TDIKeyBuf), @Buffer);
     if (Res <> DI_OK) then Result:= False;
    end else Result:= False;
  end; // if (Res <> DI_OK)
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetKey(KeyNum: Integer): Boolean;
begin
 Result:= (Buffer[KeyNum] and $80) = $80;
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetKeyPressed(KeyNum: Integer): Boolean;
begin
 Result:= (PrevBuf[KeyNum] and $80 <> $80) and (Buffer[KeyNum] and $80 = $80);
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetKeyReleased(KeyNum: Integer): Boolean;
begin
 Result:= (PrevBuf[KeyNum] and $80 = $80) and (Buffer[KeyNum] and $80 <> $80);
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetKeyName(KeyNum: Integer): string;
var
 KeyName: array[0..255] of Char;
begin
 GetKeyNameText(KeyNum or $800000, @KeyName, 255);
 Result:= string(KeyName);
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.VKeyToNum(VCode: Cardinal): Integer;
begin
 Result:= MapVirtualKey(VCode, 0);
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetVKey(VCode: Cardinal): Boolean;
begin
 Result:= GetKey(VKeyToNum(VCode));
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetVKeyPressed(VCode: Cardinal): Boolean;
begin
 Result:= GetKeyPressed(VKeyToNum(VCode));
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetVKeyReleased(VCode: Cardinal): Boolean;
begin
 Result:= GetKeyReleased(VKeyToNum(VCode));
end;

//---------------------------------------------------------------------------
function TAsphyreKeyboard.GetVKeyName(VCode: Cardinal): string;
begin
 Result:= GetKeyName(VKeyToNum(VCode));
end;

//---------------------------------------------------------------------------
end.
