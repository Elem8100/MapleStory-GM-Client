unit AsphyreRenderTargets;
//---------------------------------------------------------------------------
// AsphyreRenderTargets.pas                             Modified: 02-Feb-2009
// Render Target storage container for Asphyre                   Version 1.01
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
// The Original Code is AsphyreRenderTargets.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreTypes, AbstractTextures;

//---------------------------------------------------------------------------
type
 TAsphyreRenderTargets = class
 private
  Textures: array of TAsphyreRenderTargetTexture;

  DestroyHandle : Cardinal;
  ResetHandle   : Cardinal;
  LostHandle    : Cardinal;

  function GetTexture(Index: Integer): TAsphyreRenderTargetTexture;
  function GetCount(): Integer;

  procedure OnDeviceDestroy(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnDeviceReset(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnDeviceLost(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
 public
  property Texture[Index: Integer]: TAsphyreRenderTargetTexture
   read GetTexture; default;
  property Count: Integer read GetCount;

  function Insert(): Integer;
  function IndexOf(Element: TAsphyreRenderTargetTexture): Integer; overload;
  procedure Remove(Index: Integer);

  function Add(AddCount, Width, Height: Integer; Format: TAsphyrePixelFormat;
   DepthStencil: Boolean = False; MipMapping: Boolean = False): Integer;

  procedure RemoveAll();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AbstractDevices, AsphyreFactory;

//---------------------------------------------------------------------------
constructor TAsphyreRenderTargets.Create();
begin
 inherited;

 {$ifdef fpc}
 DestroyHandle:= EventDeviceDestroy.Subscribe(@OnDeviceDestroy, -1);
 ResetHandle  := EventDeviceReset.Subscribe(@OnDeviceReset, -1);
 LostHandle   := EventDeviceLost.Subscribe(@OnDeviceLost, -1);
 {$else}
 DestroyHandle:= EventDeviceDestroy.Subscribe(OnDeviceDestroy, -1);
 ResetHandle  := EventDeviceReset.Subscribe(OnDeviceReset, -1);
 LostHandle   := EventDeviceLost.Subscribe(OnDeviceLost, -1);
 {$endif}
end;

//---------------------------------------------------------------------------
destructor TAsphyreRenderTargets.Destroy();
begin
 EventDeviceDestroy.Unsubscribe(LostHandle);
 EventDeviceDestroy.Unsubscribe(ResetHandle);
 EventDeviceDestroy.Unsubscribe(DestroyHandle);

 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreRenderTargets.GetTexture(
 Index: Integer): TAsphyreRenderTargetTexture;
begin
 if (Index >= 0)and(Index < Length(Textures)) then
  Result:= Textures[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TAsphyreRenderTargets.GetCount(): Integer;
begin
 Result:= Length(Textures);
end;

//---------------------------------------------------------------------------
function TAsphyreRenderTargets.Insert(): Integer;
var
 TexItem: TAsphyreRenderTargetTexture;
 Index  : Integer;
begin
 TexItem:= Factory.CreateRenderTargetTexture();
 if (TexItem = nil) then
  begin
   Result:= -1;
   Exit;
  end;

 Index:= Length(Textures);
 SetLength(Textures, Index + 1);

 Textures[Index]:= TexItem;
 Result:= Index;
end;

//---------------------------------------------------------------------------
function TAsphyreRenderTargets.IndexOf(
 Element: TAsphyreRenderTargetTexture): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Textures) - 1 do
  if (Textures[i] = Element) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargets.RemoveAll();
var
 i: Integer;
begin
 for i:= 0 to Length(Textures) - 1 do
  if (Textures[i] <> nil) then
   FreeAndNil(Textures[i]);

 SetLength(Textures, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargets.OnDeviceDestroy(Sender: TObject;
 Param: Pointer; var Handled: Boolean);
begin
 RemoveAll();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargets.OnDeviceReset(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
var
 i: Integer;
begin
 for i:= 0 to Length(Textures) - 1 do
  if (Textures[i] <> nil) then Textures[i].HandleDeviceReset();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargets.OnDeviceLost(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
var
 i: Integer;
begin
 for i:= 0 to Length(Textures) - 1 do
  if (Textures[i] <> nil) then Textures[i].HandleDeviceLost();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargets.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Textures)) then Exit;

 if (Textures[Index] <> nil) then FreeAndNil(Textures[Index]);

 for i:= Index to Length(Textures) - 2 do
  Textures[i]:= Textures[i + 1];

 SetLength(Textures, Length(Textures) - 1);
end;

//---------------------------------------------------------------------------
function TAsphyreRenderTargets.Add(AddCount, Width, Height: Integer;
 Format: TAsphyrePixelFormat; DepthStencil, MipMapping: Boolean): Integer;
var
 i, Index: Integer;
begin
 Result:= -1;

 for i:= 0 to AddCount - 1 do
  begin
   Index:= Insert();
   if (Index = -1) then Break;
   if (Result = -1) then Result:= Index;

   Textures[Index].Width := Width;
   Textures[Index].Height:= Height;
   Textures[Index].Format:= Format;
   Textures[Index].DepthStencil:= DepthStencil;
   Textures[Index].Mipmapping  := MipMapping;

   if (not Textures[Index].Initialize()) then
    begin
     if (Result = Index) then Result:= -1;
     Remove(Index);
     Break;
    end;
  end;
end;

//---------------------------------------------------------------------------
end.
