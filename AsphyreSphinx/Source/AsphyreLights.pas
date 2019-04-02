unit AsphyreLights;
//---------------------------------------------------------------------------
// AsphyreLights.pas                                    Modified: 24-Jan-2009
// Asphyre 3D abstract light declaration                          Version 1.0
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
// The Original Code is AsphyreLights.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, Math, Vectors3, AsphyreColors;

//---------------------------------------------------------------------------
const
 LightWhiteColor: TAsphyreColor = (r: 65535; g: 65535; b: 65535; a: 0);
 LightBlackColor: TAsphyreColor = (r: 0; g: 0; b: 0; a: 0);

//---------------------------------------------------------------------------
type
 TAsphyreCustomLight = class
 private
 public
  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); virtual; abstract;
 end;

//---------------------------------------------------------------------------
 TAsphyreAmbientLight = class(TAsphyreCustomLight)
 private
  FColor: TAsphyreColor;
 public
  property Color: TAsphyreColor read FColor write FColor;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TAsphyreDirectionalLight = class(TAsphyreCustomLight)
 protected
  FDirection: TVector3;
 public
  property Direction: TVector3 read FDirection write FDirection;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TAsphyrePointLight = class(TAsphyreCustomLight)
 protected
  FPosition: TVector3;
 public
  property Position: TVector3 read FPosition write FPosition;

  constructor Create();
 end;

 //---------------------------------------------------------------------------
 TAsphyreLights = class
 private
  Lights: array of TAsphyreCustomLight;

  function GetCount(): Integer;
  function GetLight(Index: Integer): TAsphyreCustomLight;
  procedure SetLight(Index: Integer; const Value: TAsphyreCustomLight);
 public
  property Count: Integer read GetCount;
  property Light[Index: Integer]: TAsphyreCustomLight read GetLight
   write SetLight; default;

  function Insert(Item: TAsphyreCustomLight): Integer;
  function IndexOf(Item: TAsphyreCustomLight): Integer;
  procedure Remove(Index: Integer);

  function Include(Item: TAsphyreCustomLight): Integer;
  procedure Exclude(Item: TAsphyreCustomLight);

  procedure RemoveAll();
  procedure ExcludeAll();

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor);

  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreAmbientLight.Create();
begin
 inherited;

 FColor:= cColor(48, 48, 48, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreAmbientLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
begin
 DiffuseColor := FColor;
 SpecularColor:= lightBlackColor;
end;

//---------------------------------------------------------------------------
constructor TAsphyreDirectionalLight.Create();
begin
 inherited;

 FDirection:= Norm3(Vector3(1.0, 1.0, 1.0));
end;

//---------------------------------------------------------------------------
constructor TAsphyrePointLight.Create();
begin
 inherited;

 FPosition:= Vector3(100.0, 100.0, -100.0);
end;

//---------------------------------------------------------------------------
destructor TAsphyreLights.Destroy();
begin
 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreLights.GetCount(): Integer;
begin
 Result:= Length(Lights);
end;

//---------------------------------------------------------------------------
function TAsphyreLights.GetLight(Index: Integer): TAsphyreCustomLight;
begin
 if (Index >= 0)and(Index < Length(Lights)) then
  Result:= Lights[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.SetLight(Index: Integer;
 const Value: TAsphyreCustomLight);
begin
 if (Index >= 0)and(Index < Length(Lights)) then
  Lights[Index]:= Value;
end;

//---------------------------------------------------------------------------
function TAsphyreLights.Insert(Item: TAsphyreCustomLight): Integer;
begin
 Result:= Length(Lights);
 SetLength(Lights, Result + 1);
 Lights[Result]:= Item;
end;

//---------------------------------------------------------------------------
function TAsphyreLights.IndexOf(Item: TAsphyreCustomLight): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Lights) - 1 do
  if (Lights[i] = Item) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Lights)) then Exit;

 if (Lights[Index] <> nil) then FreeAndNil(Lights[Index]);

 for i:= Index to Length(Lights) - 2 do
  Lights[i]:= Lights[i + 1];

 SetLength(Lights, Length(Lights) - 1);
end;

//---------------------------------------------------------------------------
function TAsphyreLights.Include(Item: TAsphyreCustomLight): Integer;
begin
 Result:= IndexOf(Item);
 if (Result = -1) then Result:= Insert(Item);
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.Exclude(Item: TAsphyreCustomLight);
begin
 Remove(IndexOf(Item));
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.RemoveAll();
var
 i: Integer;
begin
 for i:= Length(Lights) - 1 downto 0 do
  if (Lights[i] <> nil) then FreeAndNil(Lights[i]);

 SetLength(Lights, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.ExcludeAll();
begin
 SetLength(Lights, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreLights.Illuminate(const WorldPos, Normal, EyePos: TVector3;
 out DiffuseColor, SpecularColor: TAsphyreColor);
var
 i: Integer;
 DiffuseTerm, SpecularTerm: TAsphyreColor;
begin
 DiffuseColor := cColor(0, 0, 0, 0);
 SpecularColor:= cColor(0, 0, 0, 0);

 for i:= 0 to Length(Lights) - 1 do
  begin
   Lights[i].Illuminate(WorldPos, Normal, EyePos, DiffuseTerm, SpecularTerm);

   DiffuseColor := DiffuseColor + DiffuseTerm;
   SpecularColor:= SpecularColor + SpecularTerm;
  end;

 DiffuseColor := cClamp(DiffuseColor);
 SpecularColor:= cClamp(SpecularColor);
end;

//---------------------------------------------------------------------------
end.
