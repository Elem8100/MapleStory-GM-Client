unit WinAdapterInfo;
//---------------------------------------------------------------------------
// WinAdapterInfo.pas                                   Modified: 29-Dec-2010
// Adapter Information in Windows                                Version 1.01
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
// The Original Code is WinAdapterInfo.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
{$ifdef fpc}{$h+}{$endif}

//---------------------------------------------------------------------------
uses
 Windows, SysUtils;

//---------------------------------------------------------------------------
type
 HMonitor = THandle;

//---------------------------------------------------------------------------
 PWinAdapterInfo = ^TWinAdapterInfo;
 TWinAdapterInfo = record
  DeviceName: AnsiString;
  DeviceText: AnsiString;
  MnRect    : TRect;
  Monitor   : HMonitor;
 end;

//---------------------------------------------------------------------------
 PWinAdapterMode = ^TWinAdapterMode;
 TWinAdapterMode = record
  Width   : Integer;
  Height  : Integer;
  BitDepth: Integer;
  Refresh : Integer;
  DevMode : TDeviceModeA;
 end;

//---------------------------------------------------------------------------
 TWinAdapterHelper = class
 private
  Adapters: array of TWinAdapterInfo;
  Modes   : array of TWinAdapterMode;

  function InsertAdapter(const AAdapter: TWinAdapterInfo): Integer;

  function GetAdapterCount(): Integer;
  function GetAdapter(Index: Integer): PWinAdapterInfo;
  function GetMode(Index: Integer): PWinAdapterMode;
  function GetModeCount(): Integer;
 public
  property AdapterCount: Integer read GetAdapterCount;
  property Adapter[Index: Integer]: PWinAdapterInfo read GetAdapter;

  property ModeCount: Integer read GetModeCount;
  property Mode[Index: Integer]: PWinAdapterMode read GetMode;
  
  function RefreshAdapters(): Boolean;
  function RefreshModes(AdapterNo: Integer): Boolean;

  function MatchMode(Width, Height, BitDepth, Refresh: Integer): Integer;
  function CurrentMode(AdapterNo: Integer): TWinAdapterMode;

  function SetTempMode(AdapterNo, NewMode: Integer): Boolean;
  procedure RestoreTempMode(AdapterNo: Integer);
  
  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 WinAdapterHelper: TWinAdapterHelper = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
const
 ENUM_CURRENT_SETTINGS  = Cardinal(-1);
 ENUM_REGISTRY_SETTINGS = Cardinal(-2);

//---------------------------------------------------------------------------
type
 PMonitorInfoEx = ^TMonitorInfoEx;
 TMonitorInfoEx = packed record
  Size    : Longword;
  MnRect  : TRect;
  WorkArea: TRect;
  Flags   : Longword;
  Device  : array[0..CCHDEVICENAME - 1] of Char;
 end;

//---------------------------------------------------------------------------
 TMonitorEnumProc = function(Monitor: HMonitor; MonitorDC: HDC;
  MonitorRect: PRect; Context: Pointer): LongBool; stdcall;

//---------------------------------------------------------------------------
{$ifdef fpc}
 TDisplayDeviceA = packed record
  cb          : Longword;
  DeviceName  : array[0..31] of AnsiChar;
  DeviceString: array[0..127] of AnsiChar;
  StateFlags  : Longword;
 end;
{$endif}

//---------------------------------------------------------------------------
function GetMonitorInfoA(Monitor: HMonitor;
 Info: PMonitorInfoEx): LongBool; stdcall; external 'user32.dll';

//---------------------------------------------------------------------------
function EnumDisplayMonitors(DC: HDC; Clip: PRect; EnumProc: TMonitorEnumProc;
 Context: Pointer): LongBool; stdcall; external 'user32.dll';

//---------------------------------------------------------------------------
function ChangeDisplaySettingsEx(DeviceName: PAnsiChar; DevMode: PDeviceModeA;
 Handle: HWnd; Flags: Longword; Param: Pointer): Integer; stdcall;
 external user32 name 'ChangeDisplaySettingsExA';

//---------------------------------------------------------------------------
{$ifdef fpc}
function EnumDisplayDevicesA(DeviceName: PAnsiChar; DevNum: Longword;
 const DisplayDevice: TDisplayDeviceA; Flags: Longword): LongBool; stdcall;
 external user32 name 'EnumDisplayDevicesA';
{$endif}

//---------------------------------------------------------------------------
function EnumCallback(Monitor: HMonitor; MonitorDC: HDC; MonitorRect: PRect;
 Context: Pointer): LongBool; stdcall;
var
 InfoItem: TWinAdapterInfo;
 Helper  : TWinAdapterHelper;
 Info    : TMonitorInfoEx;
 DpDevice: TDisplayDeviceA;
begin
 FillChar(Info, SizeOf(TMonitorInfoEx), 0);
 Info.Size:= SizeOf(TMonitorInfoEx);

 if (GetMonitorInfoA(Monitor, @Info)) then
  begin
   InfoItem.DeviceName:= Info.Device;
   InfoItem.MnRect    := Info.MnRect;
   InfoItem.Monitor   := Monitor;

   FillChar(DpDevice, SizeOf(TDisplayDeviceA), 0);
   DpDevice.cb:= SizeOf(TDisplayDeviceA);

   if (EnumDisplayDevicesA(PAnsiChar(InfoItem.DeviceName), 0, DpDevice, 0)) then
    InfoItem.DeviceText:= DpDevice.DeviceString
     else InfoItem.DeviceText:= InfoItem.DeviceName;

   Helper:= TWinAdapterHelper(Context);
   Helper.InsertAdapter(InfoItem);
  end;

 Result:= True;
end;

//---------------------------------------------------------------------------
constructor TWinAdapterHelper.Create();
begin
 inherited;

end;

//---------------------------------------------------------------------------
destructor TWinAdapterHelper.Destroy();
begin

 inherited;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.RefreshAdapters(): Boolean;
begin
 SetLength(Adapters, 0);

 {$ifdef fpc}
 Result:= EnumDisplayMonitors(0, nil, @EnumCallback, Self);
 {$else}
 Result:= EnumDisplayMonitors(0, nil, EnumCallback, Self);
 {$endif}
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.InsertAdapter(
 const AAdapter: TWinAdapterInfo): Integer;
begin
 Result:= Length(Adapters);
 SetLength(Adapters, Result + 1);

 Adapters[Result]:= AAdapter;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.GetAdapterCount(): Integer;
begin
 Result:= Length(Adapters);
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.GetAdapter(Index: Integer): PWinAdapterInfo;
begin
 if (Index >= 0)and(Index < Length(Adapters)) then
  Result:= @Adapters[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.GetModeCount(): Integer;
begin
 Result:= Length(Modes);
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.GetMode(Index: Integer): PWinAdapterMode;
begin
 if (Index >= 0)and(Index < Length(Modes)) then
  Result:= @Modes[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.RefreshModes(AdapterNo: Integer): Boolean;
var
 iMode, Index: Integer;
 DevMode: TDeviceModeA;
begin
 if (AdapterNo <= Length(Adapters)) then
  begin
   Result:= RefreshAdapters();
   if (not Result) then Exit;
  end;

 SetLength(Modes, 0);

 iMode:= 0;
 while (EnumDisplaySettingsA(PAnsiChar(Adapters[AdapterNo].DeviceName), iMode,
  DevMode)) do
  begin
   Index:= Length(Modes);
   SetLength(Modes, Index + 1);

   Modes[Index].Width   := DevMode.dmPelsWidth;
   Modes[Index].Height  := DevMode.dmPelsHeight;
   Modes[Index].BitDepth:= DevMode.dmBitsPerPel;
   Modes[Index].Refresh := DevMode.dmDisplayFrequency;
   Move(DevMode, Modes[Index].DevMode, SizeOf(TDeviceModeA));
   
   Inc(iMode);
  end;
  
 Result:= Length(Modes) > 0; 
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.MatchMode(Width, Height, BitDepth,
 Refresh: Integer): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Modes) - 1 do
  if (Modes[i].Width = Width)and(Modes[i].Height = Height) then
   begin
    if (BitDepth > 0)and(Modes[i].BitDepth <> BitDepth) then Continue;
    if (Refresh > 0)and(Modes[i].Refresh <> Refresh) then Continue;

    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.CurrentMode(AdapterNo: Integer): TWinAdapterMode;
var
 DevMode: TDeviceModeA;
begin
 FillChar(Result, SizeOf(TWinAdapterMode), 0);

 if (AdapterNo >= Length(Adapters))and(not RefreshAdapters()) then Exit;
 if (AdapterNo >= Length(Adapters)) then Exit;
 
 if (not EnumDisplaySettingsA(PAnsiChar(Adapters[AdapterNo].DeviceName),
  ENUM_CURRENT_SETTINGS, 
  DevMode)) then Exit;
  
 Result.Width   := DevMode.dmPelsWidth;
 Result.Height  := DevMode.dmPelsHeight;
 Result.BitDepth:= DevMode.dmBitsPerPel;
 Result.Refresh := DevMode.dmDisplayFrequency;
end;

//---------------------------------------------------------------------------
function TWinAdapterHelper.SetTempMode(AdapterNo, NewMode: Integer): Boolean;
var
 DevMode: TDeviceModeA;
begin
 if (NewMode < 0)or(NewMode >= Length(Modes))or(AdapterNo < 0)or
  (AdapterNo >= Length(Adapters)) then
  begin
   Result:= False;
   Exit;
  end;

 Move(Modes[NewMode].DevMode, DevMode, SizeOf(TDeviceModeA));

 Result:=
  ChangeDisplaySettingsEx(PAnsiChar(Adapters[AdapterNo].DeviceName), @DevMode, 0,
   CDS_FULLSCREEN, nil) = DISP_CHANGE_SUCCESSFUL;
end;

//---------------------------------------------------------------------------
procedure TWinAdapterHelper.RestoreTempMode(AdapterNo: Integer);
begin
 if (AdapterNo < 0)or(AdapterNo >= Length(Adapters)) then Exit;

 ChangeDisplaySettingsEx(PAnsiChar(Adapters[AdapterNo].DeviceName), nil, 0, 0,
  nil);
end;

//---------------------------------------------------------------------------
initialization
 WinAdapterHelper:= TWinAdapterHelper.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(WinAdapterHelper);

//---------------------------------------------------------------------------
end.
