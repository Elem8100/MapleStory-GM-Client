unit MediaFonts;
//---------------------------------------------------------------------------
// MediaFonts.pas                                       Modified: 20-May-2010
// Resource management utility for Asphyre fonts                  Version 1.0
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
// The Original Code is MediaFonts.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2010,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreXML, MediaUtils;

//---------------------------------------------------------------------------
type
 PFontDesc = ^TFontDesc;
 TFontDesc = record
  Name      : ShortString;
  Kerning   : Single;
  Whitespace: Single;
  Linespace : Single;
  DataLink  : AnsiString;
  ImageName : AnsiString;
 end;

//---------------------------------------------------------------------------
 TFontGroup = class
 private
  Elements: array of TFontDesc;

  FName  : ShortString;
  FOption: ShortString;

  function GetCount(): Integer;
  function GetItem(Num: Integer): PFontDesc;
  function NewItem(): PFontDesc;
  procedure ParseItem(Node: TXMLNode);
 public
  property Name  : ShortString read FName;
  property Option: ShortString read FOption;

  property Count: Integer read GetCount;
  property Item[Num: Integer]: PFontDesc read GetItem; default;

  function Find(const Text: ShortString): PFontDesc;
  procedure ParseXML(Node: TXMLNode);

  constructor Create(const AName: ShortString);
 end;

//---------------------------------------------------------------------------
 TFontGroups = class
 private
  Groups: array of TFontGroup;
  FOption: ShortString;

  function GetCount(): Integer;
  function GetItem(Num: Integer): TFontGroup;
  function GetGroup(const Name: ShortString): TFontGroup;
  function NewGroup(const Name: ShortString): TFontGroup;
  function GetTotalElements(): Integer;
 public
  property Count: Integer read GetCount;
  property Item[Num: Integer]: TFontGroup read GetItem;
  property Group[const Name: ShortString]: TFontGroup read GetGroup;

  property Option: ShortString read FOption write FOption;

  property TotalElements: Integer read GetTotalElements;

  function IndexOf(const Name: ShortString): Integer;
  procedure Clear();

  function Find(const Name: ShortString): PFontDesc;
  procedure ParseLink(const Link: AnsiString);
  procedure ParseFolder(Link: AnsiString);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 FontGroups: TFontGroups = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TFontGroup.Create(const AName: ShortString);
begin
 inherited Create();

 FName:= AName;
end;

//---------------------------------------------------------------------------
function TFontGroup.GetCount(): Integer;
begin
 Result:= Length(Elements);
end;

//---------------------------------------------------------------------------
function TFontGroup.GetItem(Num: Integer): PFontDesc;
begin
 if (Num >= 0)and(Num < Length(Elements)) then
  Result:= @Elements[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TFontGroup.Find(const Text: ShortString): PFontDesc;
var
 i: Integer;
begin
 Result:= nil;

 for i:= 0 to Length(Elements) - 1 do
  if (SameText(Elements[i].Name, Text)) then
   begin
    Result:= @Elements[i];
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TFontGroup.NewItem(): PFontDesc;
var
 Index: Integer;
begin
 Index:= Length(Elements);
 SetLength(Elements, Index + 1);

 FillChar(Elements[Index], SizeOf(TFontDesc), 0);

 Elements[Index].Kerning   := 0.0;
 Elements[Index].Whitespace:= 8.0;
 Elements[Index].Linespace := 0.0;

 Result:= @Elements[Index];
end;

//---------------------------------------------------------------------------
procedure TFontGroup.ParseItem(Node: TXMLNode);
var
 Desc: PFontDesc;
 Aux : TXMLNode;
begin
 // -> node "font"
 if (not SameText(Node.Name, 'font')) then Exit;

 // -> attributes "name", "link" and "image"
 Desc:= NewItem();
 Desc^.Name:= Node.FieldValue['name'];
 Desc^.DataLink := Node.FieldValue['link'];
 Desc^.ImageName:= Node.FieldValue['image'];

 // -> "attrib" node
 Aux:= Node.ChildNode['attrib'];
 if (Aux <> nil) then
  begin
   Desc^.Kerning   := ParseFloat(Aux.FieldValue['kerning']);
   Desc^.Whitespace:= ParseFloat(Aux.FieldValue['whitespace']);
   Desc^.Linespace := ParseFloat(Aux.FieldValue['linespace']);
  end;
end;

//---------------------------------------------------------------------------
procedure TFontGroup.ParseXML(Node: TXMLNode);
var
 i: Integer;
begin
 if (not SameText(Node.Name, 'font-group')) then Exit;

 FName  := LowerCase(Node.FieldValue['name']);
 FOption:= LowerCase(Node.FieldValue['option']);

 for i:= 0 to Node.ChildCount - 1 do
  ParseItem(Node.Child[i]);
end;

//---------------------------------------------------------------------------
constructor TFontGroups.Create();
begin
 inherited;

 FOption:= '';
end;

//---------------------------------------------------------------------------
destructor TFontGroups.Destroy();
begin
 Clear();

 inherited;
end;

//---------------------------------------------------------------------------
function TFontGroups.GetCount: Integer;
begin
 Result:= Length(Groups);
end;

//---------------------------------------------------------------------------
function TFontGroups.GetItem(Num: Integer): TFontGroup;
begin
 if (Num >= 0)and(Num < Length(Groups)) then
  Result:= Groups[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TFontGroups.IndexOf(const Name: ShortString): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Groups) - 1 do
  if (SameText(Groups[i].Name, Name)) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TFontGroups.GetGroup(const Name: ShortString): TFontGroup;
var
 Index: Integer;
begin
 Result:= nil;
 Index := IndexOf(Name);

 if (Index <> -1) then Result:= Groups[Index];
end;

//---------------------------------------------------------------------------
procedure TFontGroups.Clear();
var
 i: Integer;
begin
 for i:= 0 to Length(Groups) - 1 do
  if (Groups[i] <> nil) then
   begin
    Groups[i].Free();
    Groups[i]:= nil;
   end;

 SetLength(Groups, 0);
end;

//---------------------------------------------------------------------------
function TFontGroups.GetTotalElements(): Integer;
var
 i: Integer;
begin
 Result:= 0;

 for i:= 0 to Length(Groups) - 1 do
  Inc(Result, Groups[i].Count);
end;

//---------------------------------------------------------------------------
function TFontGroups.Find(const Name: ShortString): PFontDesc;
var
 i: Integer;
begin
 Result:= nil;

 for i:= 0 to Length(Groups) - 1 do
  if (FOption = '')or(Groups[i].Option = '')or
   (SameText(Groups[i].Option, FOption)) then
   begin
    Result:= Groups[i].Find(Name);
    if (Result <> nil) then Break;
   end;
end;

//---------------------------------------------------------------------------
function TFontGroups.NewGroup(const Name: ShortString): TFontGroup;
var
 Index: Integer;
begin
 Index:= Length(Groups);
 SetLength(Groups, Index + 1);

 Groups[Index]:= TFontGroup.Create(Name);
 Result:= Groups[Index];
end;

//---------------------------------------------------------------------------
procedure TFontGroups.ParseLink(const Link: AnsiString);
var
 Root : TXMLNode;
 Child: TXMLNode;
 GroupItem: TFontGroup;
 Name : AnsiString;
 i: Integer;
begin
 Root:= LoadLinkXML(Link);
 if (Root = nil) then Exit;

 for i:= 0 to Root.ChildCount - 1 do
  begin
   Child:= Root.Child[i];

   // -> "font-group" node
   if (SameText(Child.Name, 'font-group')) then
    begin
     Name:= Child.FieldValue['name'];
     if (Length(Name) > 0) then
      begin
       GroupItem:= GetGroup(Name);
       if (GroupItem = nil) then GroupItem:= NewGroup(Name);

       GroupItem.ParseXML(Child);
      end;
    end;

   // -> "resource" node
   if (SameText(Child.Name, 'resource')) then
    begin
     Name:= Child.FieldValue['source'];
     if (Length(Name) > 0) then ParseLink(Name);
    end;
  end;

 Root.Free();
end;

//---------------------------------------------------------------------------
procedure TFontGroups.ParseFolder(Link: AnsiString);
var
 Path : AnsiString;
 Rec  : TSearchRec;
 Found: Boolean;
begin
 if (Length(Link) < 1)or(Link[Length(Link)] <> '\') then
  Link:= Link + '/';

 Path:= ExtractArchiveName(Link);

 if (Length(Path) > 0)and(Path[Length(Path)] <> '\') then
  Path:= Path + '\';

 if (FindFirst(Path + '*.xml', faReadOnly or faArchive, Rec) <> 0) then
  begin
   FindClose(Rec);
   Exit;
  end;

 repeat
  ParseLink(Link + Rec.Name);
  Found:= FindNext(Rec) = 0;
 until (not Found);

 FindClose(Rec);
end;

//---------------------------------------------------------------------------
initialization
 FontGroups:= TFontGroups.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(FontGroups);

//---------------------------------------------------------------------------
end.
