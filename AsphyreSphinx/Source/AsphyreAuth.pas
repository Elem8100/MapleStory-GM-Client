unit AsphyreAuth;
//---------------------------------------------------------------------------
// AsphyreAuth.pas                                      Modified: 29-Dec-2010
// Asphyre Authorization provider                                 Version 1.0
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
// The Original Code is AsphyreAuth.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2011,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreDb;

//---------------------------------------------------------------------------
type
 TAuthType = (atProvideKey, atBurnKey);

//---------------------------------------------------------------------------
 TAsphyreAuth = class;

//---------------------------------------------------------------------------
 TAuthCallback = procedure(Sender: TObject; Auth: TAsphyreAuth;
  Archive: TASDb; AuthType: TAuthType) of object;

//---------------------------------------------------------------------------
 TAuthItem = record
  Callback: TAuthCallback;
  ItemID  : Cardinal;
 end;

//---------------------------------------------------------------------------
 TAsphyreAuth = class
 private
  Items: array of TAuthItem;
  CurrentID: Cardinal;

  Authorized : Boolean;
  AuthArchive: TASDb;
  AuthSender : TObject;
  AuthIndex  : Integer;

  function NextID(): Cardinal;
  function IndexOf(ItemID: Cardinal): Integer;
  procedure Remove(Index: Integer);
 public
  function Subscribe(Callback: TAuthCallback): Cardinal;
  procedure Unsubscribe(EventID: Cardinal);

  function Authorize(Sender: TObject; Archive: TASDb): Boolean;
  procedure Unauthorize();

  procedure ProvideKey(Key: Pointer);
 end;

//---------------------------------------------------------------------------
var
 Auth: TAsphyreAuth = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function TAsphyreAuth.NextID(): Cardinal;
begin
 Result:= CurrentID;
 Inc(CurrentID);
end;

//---------------------------------------------------------------------------
function TAsphyreAuth.IndexOf(ItemID: Cardinal): Integer;
var
 i: Integer;
begin
 Result:= -1;
 for i:= 0 to Length(Items) - 1 do
  if (Items[i].ItemID = ItemID) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreAuth.Subscribe(Callback: TAuthCallback): Cardinal;
var
 Index: Integer;
begin
 Result:= NextID();

 Index:= Length(Items);
 SetLength(Items, Length(Items) + 1);

 Items[Index].Callback:= Callback;
 Items[Index].ItemID  := Result;
end;

//---------------------------------------------------------------------------
procedure TAsphyreAuth.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Items)) then Exit;

 for i:= Index to Length(Items) - 2 do
  Items[i]:= Items[i + 1];

 SetLength(Items, Length(Items) - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreAuth.Unsubscribe(EventID: Cardinal);
begin
 Unauthorize();
 Remove(IndexOf(EventID));
end;

//---------------------------------------------------------------------------
function TAsphyreAuth.Authorize(Sender: TObject; Archive: TASDb): Boolean;
var
 i: Integer;
begin
 Authorized := False;
 AuthArchive:= Archive;
 AuthSender := Sender;
 AuthIndex  := -1;

 for i:= 0 to Length(Items) - 1 do
  begin
   Items[i].Callback(AuthSender, Self, AuthArchive, atProvideKey);
   if (Authorized) then
    begin
     AuthIndex:= i;
     Break;
    end;
  end;

 Result:= Authorized;
 if (not Result) then
  begin
   AuthArchive:= nil;
   AuthSender := nil;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreAuth.Unauthorize();
begin
 if (Authorized)and(AuthIndex >= 0)and(AuthIndex < Length(Items)) then
  Items[AuthIndex].Callback(AuthSender, Self, AuthArchive, atBurnKey);

 Authorized := False;
 AuthIndex  := -1;
 AuthArchive:= nil;
end;

//---------------------------------------------------------------------------
procedure TAsphyreAuth.ProvideKey(Key: Pointer);
begin
 if (AuthArchive <> nil) then
  begin
   AuthArchive.Password:= Key;
   Authorized:= True;
  end;
end;

//---------------------------------------------------------------------------
initialization
 Auth:= TAsphyreAuth.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(Auth);

//---------------------------------------------------------------------------
end.
