unit AsphyreFactory;
//---------------------------------------------------------------------------
// AsphyreFactory.pas                                   Modified: 08-Jan-2009
// Asphyre Component Factory class                               Version 1.02
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
// The Original Code is AsphyreFactory.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AbstractDevices, AbstractCanvas, AbstractTextures,
 AbstractRasterizer;

//---------------------------------------------------------------------------
type
 TAsphyreProvider = class
 protected
  FProviderID: Cardinal;
 public
  property ProviderID: Cardinal read FProviderID;

  function CreateDevice(): TAsphyreDevice; virtual; abstract;
  function CreateCanvas(): TAsphyreCanvas; virtual; abstract;
  function CreateRasterizer(): TAsphyreRasterizer; virtual; abstract;
  function CreateLockableTexture(): TAsphyreLockableTexture; virtual; abstract;
  function CreateRenderTargetTexture(): TAsphyreRenderTargetTexture; virtual; abstract;
 end;

//---------------------------------------------------------------------------
 TAsphyreFactory = class
 private
  Providers: array of TAsphyreProvider;
  Provider : TAsphyreProvider;

  function IndexOf(AProvider: TAsphyreProvider): Integer;
  function Insert(AProvider: TAsphyreProvider): Integer;
  procedure Remove(Index: Integer; NoFree: Boolean);
  procedure RemoveAll();
  function FindProvider(ID: Cardinal): TAsphyreProvider;
 public
  function CreateDevice(): TAsphyreDevice;
  function CreateCanvas(): TAsphyreCanvas;
  function CreateRasterizer(): TAsphyreRasterizer;
  function CreateLockableTexture(): TAsphyreLockableTexture;
  function CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;

  procedure Subscribe(AProvider: TAsphyreProvider);
  procedure Unsubscribe(AProvider: TAsphyreProvider; NoFree: Boolean = False);

  procedure UseProvider(ProviderID: Cardinal);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 Factory: TAsphyreFactory = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreFactory.Create();
begin
 inherited;

 Provider:= nil;
end;

//---------------------------------------------------------------------------
destructor TAsphyreFactory.Destroy();
begin
 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.IndexOf(AProvider: TAsphyreProvider): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Providers) - 1 do
  if (Providers[i] = AProvider) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.Insert(AProvider: TAsphyreProvider): Integer;
var
 Index: Integer;
begin
 Index:= Length(Providers);
 SetLength(Providers, Index + 1);

 Providers[Index]:= AProvider;
 Result:= Index;
end;

//---------------------------------------------------------------------------
procedure TAsphyreFactory.RemoveAll();
var
 i: Integer;
begin
 for i:= 0 to Length(Providers) - 1 do
  if (Providers[i] <> nil) then FreeAndNil(Providers[i]);

 SetLength(Providers, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFactory.Remove(Index: Integer; NoFree: Boolean);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Providers)) then Exit;

 if (Providers[Index] <> nil)and(not NoFree) then
  FreeAndNil(Providers[Index]);

 for i:= Index to Length(Providers) - 2 do
  Providers[i]:= Providers[i + 1];

 SetLength(Providers, Length(Providers) - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFactory.Subscribe(AProvider: TAsphyreProvider);
var
 Index: Integer;
begin
 Index:= IndexOf(AProvider);
 if (Index = -1) then Insert(AProvider);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFactory.Unsubscribe(AProvider: TAsphyreProvider;
 NoFree: Boolean);
begin
 Remove(IndexOf(AProvider), NoFree);
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.FindProvider(ID: Cardinal): TAsphyreProvider;
var
 Index, i: Integer;
begin
 Index:= -1;

 for i:= 0 to Length(Providers) - 1 do
  if (Providers[i].ProviderID = ID) then
   begin
    Index:= i;
    Break;
   end;

 if (Index = -1)and(Length(Providers) > 0) then Index:= 0;

 Result:= nil;
 if (Index <> -1) then Result:= Providers[Index];
end;

//---------------------------------------------------------------------------
procedure TAsphyreFactory.UseProvider(ProviderID: Cardinal);
begin
 Provider:= FindProvider(ProviderID);
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.CreateDevice(): TAsphyreDevice;
begin
 Result:= nil;

 if (Provider <> nil) then
  Result:= Provider.CreateDevice();
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.CreateCanvas(): TAsphyreCanvas;
begin
 Result:= nil;

 if (Provider <> nil) then
  Result:= Provider.CreateCanvas();
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.CreateRasterizer(): TAsphyreRasterizer;
begin
 Result:= nil;

 if (Provider <> nil) then
  Result:= Provider.CreateRasterizer();
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.CreateLockableTexture(): TAsphyreLockableTexture;
begin
 Result:= nil;

 if (Provider <> nil) then
  Result:= Provider.CreateLockableTexture();
end;

//---------------------------------------------------------------------------
function TAsphyreFactory.CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;
begin
 Result:= nil;

 if (Provider <> nil) then
  Result:= Provider.CreateRenderTargetTexture();
end;

//---------------------------------------------------------------------------
initialization
 Factory:= TAsphyreFactory.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(Factory);

//---------------------------------------------------------------------------
end.
