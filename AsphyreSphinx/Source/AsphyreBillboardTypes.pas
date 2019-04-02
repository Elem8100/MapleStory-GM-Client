unit AsphyreBillboardTypes;
//---------------------------------------------------------------------------
// AsphyreBillboardTypes.pas                            Modified: 25-Jan-2009
// Billboard storage type for Asphyre                             Version 1.0
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
// The Original Code is AsphyreBillboardTypes.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Math, Vectors2, AsphyreTypes, AbstractTextures;

//---------------------------------------------------------------------------
type
 PBillboardEntry = ^TBillboardEntry;
 TBillboardEntry = record
  Texture: TAsphyreCustomTexture;
  Mapping: TPoint4;
  Size   : TPoint2;
  Phi    : Single;
  Color  : Longword;
 end;

//---------------------------------------------------------------------------
 TBillboardEntries = class
 private
  Entries: array of TBillboardEntry;
  EntryCount: Integer;

  function GetItem(Index: Integer): PBillboardEntry;
  procedure Request(Amount: Integer);
 public
  property Count: Integer read EntryCount;
  property Items[Index: Integer]: PBillboardEntry read GetItem; default;

  function Insert(const Item: TBillboardEntry): Integer;
  procedure Remove(Index: Integer);
  procedure RemoveAll();
  procedure ReleaseAll();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
const
 CacheSize = 512;

//---------------------------------------------------------------------------
constructor TBillboardEntries.Create();
begin
 inherited;

 EntryCount:= 0;
end;

//---------------------------------------------------------------------------
destructor TBillboardEntries.Destroy();
begin
 SetLength(Entries, 0);
 EntryCount:= 0;

 inherited;
end;

//---------------------------------------------------------------------------
function TBillboardEntries.GetItem(Index: Integer): PBillboardEntry;
begin
 if (Index >= 0)and(Index < EntryCount) then
  Result:= @Entries[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TBillboardEntries.Request(Amount: Integer);
var
 Required: Integer;
begin
 Required:= Ceil(Amount / CacheSize) * CacheSize;
 if (Length(Entries) < Required) then SetLength(Entries, Required);
end;

//---------------------------------------------------------------------------
function TBillboardEntries.Insert(const Item: TBillboardEntry): Integer;
var
 Index: Integer;
begin
 Request(EntryCount + 1);

 Index:= EntryCount;
 Entries[Index]:= Item;

 Inc(EntryCount);
 Result:= Index;
end;

//---------------------------------------------------------------------------
procedure TBillboardEntries.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= EntryCount) then Exit;

 for i:= Index to EntryCount - 2 do
  Entries[i]:= Entries[i + 1];

 Dec(EntryCount);
end;

//---------------------------------------------------------------------------
procedure TBillboardEntries.RemoveAll();
begin
 EntryCount:= 0;
end;

//---------------------------------------------------------------------------
procedure TBillboardEntries.ReleaseAll();
begin
 SetLength(Entries, 0);
 EntryCount:= 0;
end;

//---------------------------------------------------------------------------
end.

