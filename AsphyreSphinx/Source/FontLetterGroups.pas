unit FontLetterGroups;
//---------------------------------------------------------------------------
// FontLetterGroups.pas                                 Modified: 21-Dec-2010
// 2-letter displacement combination holder                       Version 1.0
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
// The Original Code is FontLetterGroups.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2010,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
{$ifdef fpc}{$mode delphi}{$endif}

interface

//---------------------------------------------------------------------------
uses
 SysUtils, Vectors2px, HelperSets;

//---------------------------------------------------------------------------
type
 TFontLetterGroups = class
 private
  HashArray: packed array[0..255, 0..255] of Shortint;
  ExtArray : TPointList;

  function GetShift(Code1, Code2: Integer): Integer;
 public
  property Shift[Code1, Code2: Integer]: Integer read GetShift; default;

  procedure Spec(Code1, Code2, Shift: Integer); overload;
  procedure Spec(Code1, Code2: AnsiChar; Shift: Integer); overload;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreUtils;

//---------------------------------------------------------------------------
constructor TFontLetterGroups.Create();
begin
 inherited;

 ExtArray:= TPointList.Create();

 FillChar(HashArray, SizeOf(HashArray), 0);
end;

//---------------------------------------------------------------------------
destructor TFontLetterGroups.Destroy();
begin
 FreeAndNil(ExtArray);

 inherited;
end;

//---------------------------------------------------------------------------
procedure TFontLetterGroups.Spec(Code1, Code2, Shift: Integer);
var
 Pos: TPoint2px;
 Index: Integer;
begin
 if (Code1 < 0)or(Code2 < 0) then Exit;

 Shift:= MinMax2(Shift, -128, 127);

 if (Code1 <= 255)and(Code2 <= 255) then
  begin
   HashArray[Code1, Code2]:= Shift;
   Exit;
  end;

 Pos.x:= Code1;
 Pos.y:= Code2;

 Index:= ExtArray.IndexOf(Pos);
 if (Index = -1) then Index:= ExtArray.Insert(Pos);

 ExtArray[Index].Data:= Pointer(Shift);
end;

//---------------------------------------------------------------------------
function TFontLetterGroups.GetShift(Code1, Code2: Integer): Integer;
var
 Index: Integer;
begin
 Result:= 0;
 if (Code1 < 0)or(Code2 < 0) then Exit;

 if (Code1 <= 255)and(Code2 <= 255) then
  begin
   Result:= HashArray[Code1, Code2];
   Exit;
  end;

 Index:= ExtArray.IndexOf(Point2px(Code1, Code2));

 if (Index <> -1) then
  Result:= Integer(ExtArray[Index].Data);
end;

//---------------------------------------------------------------------------
procedure TFontLetterGroups.Spec(Code1, Code2: AnsiChar; Shift: Integer);
begin
 Spec(Integer(Code1), Integer(Code2), Shift);
end;

//---------------------------------------------------------------------------
end.
