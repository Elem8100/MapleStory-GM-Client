unit MediaImages;
//---------------------------------------------------------------------------
// MediaImages.pas                                      Modified: 20-May-2010
// Resource management utility for Asphyre images                Version 1.11
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
// The Original Code is MediaImages.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, SysUtils, Vectors2px, AsphyreTypes, AsphyreXML, MediaUtils;

//---------------------------------------------------------------------------
type
 PImageDesc = ^TImageDesc;
 TImageDesc = record
  Name        : ShortString;         // unique image identifier
  Format      : TAsphyrePixelFormat; // preferred pixel format
  MipMapping  : Boolean;             // whether to enable mipmapping
  PatternSize : TPoint2px;           // pattern size
  PatternCount: Integer;             // number of patterns
  VisibleSize : TPoint2px;           // visible size
  MediaLink   : AnsiString;          // link to image media
 end;

//---------------------------------------------------------------------------
 TImageGroup = class
 private
  Elements: array of TImageDesc;

  FName  : ShortString;
  FOption: ShortString;

  function GetCount(): Integer;
  function GetItem(Num: Integer): PImageDesc;
  function NewItem(): PImageDesc;
  procedure ParseItem(Node: TXMLNode);
 public
  property Name  : ShortString read FName;
  property Option: ShortString read FOption;

  property Count: Integer read GetCount;
  property Item[Num: Integer]: PImageDesc read GetItem; default;

  function Find(const Text: ShortString): PImageDesc;
  procedure ParseXML(Node: TXMLNode);

  constructor Create(const AName: ShortString);
 end;

//---------------------------------------------------------------------------
 TImageGroups = class
 private
  Groups: array of TImageGroup;
  FOption: ShortString;

  function GetCount(): Integer;
  function GetItem(Num: Integer): TImageGroup;
  function GetGroup(const Name: ShortString): TImageGroup;
  function NewGroup(const Name: ShortString): TImageGroup;
  function GetTotalElements(): Integer;
 public
  property Count: Integer read GetCount;
  property Item[Num: Integer]: TImageGroup read GetItem;
  property Group[const Name: ShortString]: TImageGroup read GetGroup;

  property Option: ShortString read FOption write FOption;

  property TotalElements: Integer read GetTotalElements;

  function IndexOf(const Name: ShortString): Integer;
  procedure Clear();

  function Find(const Name: ShortString): PImageDesc;
  procedure ParseLink(const Link: AnsiString);
  procedure ParseFolder(Link: AnsiString);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 ImageGroups: TImageGroups = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreImages, AsphyreFormatInfo;

//---------------------------------------------------------------------------
constructor TImageGroup.Create(const AName: ShortString);
begin
 inherited Create();

 FName:= AName;
end;

//---------------------------------------------------------------------------
function TImageGroup.GetCount(): Integer;
begin
 Result:= Length(Elements);
end;

//---------------------------------------------------------------------------
function TImageGroup.GetItem(Num: Integer): PImageDesc;
begin
 if (Num >= 0)and(Num < Length(Elements)) then
  Result:= @Elements[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TImageGroup.Find(const Text: ShortString): PImageDesc;
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
function TImageGroup.NewItem(): PImageDesc;
var
 Index: Integer;
begin
 Index:= Length(Elements);
 SetLength(Elements, Index + 1);

 FillChar(Elements[Index], SizeOf(TImageDesc), 0);

 Elements[Index].Format      := apf_Unknown;
 Elements[Index].MipMapping  := False;
 Elements[Index].PatternCount:= 0;

 Result:= @Elements[Index];
end;

//---------------------------------------------------------------------------
procedure TImageGroup.ParseItem(Node: TXMLNode);
var
 Desc: PImageDesc;
 Aux : TXMLNode;
begin
 // -> node "image"
 if (not SameText(Node.Name, 'image')) then Exit;

 // -> attributes "name" and "link"
 Desc:= NewItem();
 Desc^.Name:= Node.FieldValue['name'];
 Desc^.MediaLink := Node.FieldValue['link'];

 // -> "format" node
 Aux:= Node.ChildNode['format'];
 if (Aux <> nil) then
  begin
   Desc^.Format    := StrToFormat(Aux.FieldValue['type']);
   Desc^.MipMapping:= ParseBoolean(Aux.FieldValue['mipmapping'], False);
  end;

 // -> "pattern" node
 Aux:= Node.ChildNode['pattern'];
 if (Aux <> nil) then
  begin
   Desc^.PatternSize.x:= ParseInt(Aux.FieldValue['width'], 0);
   Desc^.PatternSize.y:= ParseInt(Aux.FieldValue['height'], 0);
   Desc^.PatternCount := ParseInt(Aux.FieldValue['count'], 1);
   Desc^.VisibleSize.x:= ParseInt(Aux.FieldValue['viewx'], 0);
   Desc^.VisibleSize.y:= ParseInt(Aux.FieldValue['viewy'], 0);
  end;
end;

//---------------------------------------------------------------------------
procedure TImageGroup.ParseXML(Node: TXMLNode);
var
 i: Integer;
begin
 if (not SameText(Node.Name, 'image-group')) then Exit;

 FName  := LowerCase(Node.FieldValue['name']);
 FOption:= LowerCase(Node.FieldValue['option']);

 for i:= 0 to Node.ChildCount - 1 do
  ParseItem(Node.Child[i]);
end;

//---------------------------------------------------------------------------
constructor TImageGroups.Create();
begin
 inherited;

 FOption:= '';
end;

//---------------------------------------------------------------------------
destructor TImageGroups.Destroy();
begin
 Clear();

 inherited;
end;

//---------------------------------------------------------------------------
function TImageGroups.GetCount: Integer;
begin
 Result:= Length(Groups);
end;

//---------------------------------------------------------------------------
function TImageGroups.GetItem(Num: Integer): TImageGroup;
begin
 if (Num >= 0)and(Num < Length(Groups)) then
  Result:= Groups[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TImageGroups.IndexOf(const Name: ShortString): Integer;
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
function TImageGroups.GetGroup(const Name: ShortString): TImageGroup;
var
 Index: Integer;
begin
 Result:= nil;
 Index := IndexOf(Name);

 if (Index <> -1) then Result:= Groups[Index];
end;

//---------------------------------------------------------------------------
procedure TImageGroups.Clear();
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
function TImageGroups.GetTotalElements(): Integer;
var
 i: Integer;
begin
 Result:= 0;

 for i:= 0 to Length(Groups) - 1 do
  Inc(Result, Groups[i].Count);
end;

//---------------------------------------------------------------------------
function TImageGroups.Find(const Name: ShortString): PImageDesc;
var
 i: Integer;
begin
 Result:= nil;

 for i:= 0 to Length(Groups) - 1 do
  if (FOption = '')or(Groups[i].Option = '')or(SameText(Groups[i].Option, FOption)) then
   begin
    Result:= Groups[i].Find(Name);
    if (Result <> nil) then Break;
   end;
end;

//---------------------------------------------------------------------------
function TImageGroups.NewGroup(const Name: ShortString): TImageGroup;
var
 Index: Integer;
begin
 Index:= Length(Groups);
 SetLength(Groups, Index + 1);

 Groups[Index]:= TImageGroup.Create(Name);
 Result:= Groups[Index];
end;

//---------------------------------------------------------------------------
procedure TImageGroups.ParseLink(const Link: AnsiString);
var
 Root : TXMLNode;
 Child: TXMLNode;
 GroupItem: TImageGroup;
 Name : AnsiString;
 i: Integer;
begin
 Root:= LoadLinkXML(Link);
 if (Root = nil) then Exit;

 for i:= 0 to Root.ChildCount - 1 do
  begin
   Child:= Root.Child[i];

   // -> "image-group" node
   if (SameText(Child.Name, 'image-group')) then
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
procedure TImageGroups.ParseFolder(Link: AnsiString);
var
 Rec  : TSearchRec;
 Path : AnsiString;
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
 ImageGroups:= TImageGroups.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(ImageGroups);

//---------------------------------------------------------------------------
end.
