unit WinKeyb;
//---------------------------------------------------------------------------
// WinKeyb.pas                                          Modified: 15-Dec-2008
// Keyboard key handler for Asphyre                              Version 1.01
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
// The Original Code is WinKeyb.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SysUtils;

//---------------------------------------------------------------------------
const
 WinKeybLocale = '00000409'; // U.S. English

//---------------------------------------------------------------------------
type
 TWinKeyb = class
 private
  Locale: HKL;

  function GetKey(vKey: Integer): Boolean;
  function GetKeyName(vKey: Integer): ShortString;
  function GetKeyPressed(vKey: Integer): Boolean;
 public
  property Key[vKey: Integer]: Boolean read GetKey;
  property KeyName[vKey: Integer]: ShortString read GetKeyName;
  property KeyPressed[vKey: Integer]: Boolean read GetKeyPressed;

  function CharKey(Ch: Char): Integer;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 Keyb: TWinKeyb = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TWinKeyb.Create();
begin
 inherited;

 Locale:= LoadKeyboardLayout(PChar(WinKeybLocale), 0);
end;

//---------------------------------------------------------------------------
destructor TWinKeyb.Destroy();
begin
 UnloadKeyboardLayout(Locale);

 inherited;
end;

//---------------------------------------------------------------------------
function TWinKeyb.GetKey(vKey: Integer): Boolean;
var
 State: SmallInt;
begin
 State:= GetAsyncKeyState(vKey);
 Result:= State and $8000 <> 0;
end;

//---------------------------------------------------------------------------
function TWinKeyb.GetKeyName(vKey: Integer): ShortString;
var
 KeyText : array[0..254] of AnsiChar;
 ScanCode: Longword;
begin
 ScanCode:= MapVirtualKey(vKey, 0);

 if (ScanCode <> 0) then
  begin
   GetKeyNameText(ScanCode or $800000, @KeyText, 255);
   Result:= string(KeyText);
  end else Result:= '';
end;

//---------------------------------------------------------------------------
function TWinKeyb.GetKeyPressed(vKey: Integer): Boolean;
var
 State: SmallInt;
begin
 State:= GetAsyncKeyState(vKey);
 Result:= State and $1 <> 0;
end;

//---------------------------------------------------------------------------
function TWinKeyb.CharKey(Ch: Char): Integer;
begin
 Result:= VkKeyScanEx(Ch, Locale) and $FF;
end;

//---------------------------------------------------------------------------
initialization
 Keyb:= TWinKeyb.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(Keyb);

//---------------------------------------------------------------------------
end.
