unit AsphyreXML;
//---------------------------------------------------------------------------
// AsphyreXML.pas                                       Modified: 29-Dec-2010
// Asphyre XML wrapper                                           Version 1.03
//---------------------------------------------------------------------------
// Note: This component doesn't read or write data parts of XML and is
// primarily used to read nodes and their attributes only. This is because
// Asphyre does not use data parts of XML files.
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
// The Original Code is AsphyreXML.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2011,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
{$ifdef fpc}{$mode delphi}{$endif}
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, LibXMLParser, AsphyreDb, AsphyreAuth;

//---------------------------------------------------------------------------
type
 PXMLNodeField = ^TXMLNodeField;
 TXMLNodeField = record
  Name : AnsiString;
  Value: AnsiString;
 end;

//---------------------------------------------------------------------------
 TXMLNode = class;

//---------------------------------------------------------------------------
 TXMLNodeEnumerator = class
 private
  FNode: TXMLNode;
  Index: Integer;

  function GetCurrent(): TXMLNode;
 public
  property Current: TXMLNode read GetCurrent;

  function MoveNext(): Boolean;
  constructor Create(Node: TXMLNode);
 end;

//---------------------------------------------------------------------------
 TXMLNode = class
 private
  FName : AnsiString;
  Nodes : array of TXMLNode;
  Fields: array of TXMLNodeField;

  function GetChildCount(): Integer;
  function GetChild(Num: Integer): TXMLNode;
  function GetChildNode(const Name: AnsiString): TXMLNode;
  function GetFieldCount(): Integer;
  function GetField(Num: Integer): PXMLNodeField;
  function GetFieldValue(const Name: AnsiString): Variant;
  procedure SetFieldValue(const Name: AnsiString; const Value: Variant);
  function SubCode(Spacing: Integer): AnsiString;
 public
  property Name: AnsiString read FName;

  property ChildCount: Integer read GetChildCount;
  property Child[Num: Integer]: TXMLNode read GetChild;
  property ChildNode[const Name: AnsiString]: TXMLNode read GetChildNode;

  property FieldCount: Integer read GetFieldCount;
  property Field[Num: Integer]: PXMLNodeField read GetField;
  property FieldValue[const Name: AnsiString]: Variant read GetFieldValue
   write SetFieldValue;

  function AddChild(const Name: AnsiString): TXMLNode;
  function FindChildByName(const Name: AnsiString): Integer;

  function AddField(const Name: AnsiString;
   const Value: Variant): PXMLNodeField;
  function FindFieldByName(const Name: AnsiString): Integer;

  function GetCode(): AnsiString;

  procedure SaveToFile(const FileName: AnsiString);
  function SaveToStream(OutStream: TStream): Boolean;

  function SaveToASDb(const Key: ShortString;
   Archive: TASDb): Boolean; overload;
  function SaveToASDb(const Key: ShortString;
   const ArchiveName: AnsiString): Boolean; overload;

  function GetEnumerator(): TXMLNodeEnumerator;

  constructor Create(const AName: AnsiString);
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
function LoadXMLFromFile(const FileName: AnsiString): TXMLNode;
function LoadXMLFromStream(InStream: TStream): TXMLNode;
function LoadXMLFromASDb(const Key: ShortString;
 Archive: TASDb): TXMLNode; overload;
function LoadXMLFromASDb(const Key: ShortString;
 const ArchiveName: AnsiString): TXMLNode; overload;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function Spaces(Num: Integer): AnsiString;
var
 i: Integer;
begin
 Result:= '';
 for i:= 0 to Num - 1 do
  Result:= Result + ' ';
end;

//---------------------------------------------------------------------------
constructor TXMLNode.Create(const AName: AnsiString);
begin
 inherited Create();

 FName:= AName;
end;

//---------------------------------------------------------------------------
destructor TXMLNode.Destroy();
var
 i: Integer;
begin
 for i:= 0 to Length(Nodes) - 1 do
  if (Nodes[i] <> nil) then
   FreeAndNil(Nodes[i]);
   
 SetLength(Nodes, 0);

 inherited;
end;

//---------------------------------------------------------------------------
function TXMLNode.GetChildCount(): Integer;
begin
 Result:= Length(Nodes);
end;

//---------------------------------------------------------------------------
function TXMLNode.GetChild(Num: Integer): TXMLNode;
begin
 if (Num >= 0)and(Num < Length(Nodes)) then
  Result:= Nodes[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TXMLNode.FindChildByName(const Name: AnsiString): Integer;
var
 i: Integer;
begin
 Result:= -1;
 for i:= 0 to Length(Nodes) - 1 do
  if (Nodes[i].Name = Name) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TXMLNode.GetChildNode(const Name: AnsiString): TXMLNode;
var
 Index: Integer;
begin
 Index:= FindChildByName(Name);
 if (Index <> -1) then Result:= Nodes[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TXMLNode.GetFieldCount(): Integer;
begin
 Result:= Length(Fields);
end;

//---------------------------------------------------------------------------
function TXMLNode.GetField(Num: Integer): PXMLNodeField;
begin
 if (Num >= 0)and(Num < Length(Fields)) then
  Result:= @Fields[Num] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TXMLNode.FindFieldByName(const Name: AnsiString): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Fields) - 1 do
  if (SameText(Fields[i].Name, Name)) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TXMLNode.GetFieldValue(const Name: AnsiString): Variant;
var
 Index: Integer;
begin
 Index:= FindFieldByName(Name);
 if (Index <> -1) then Result:= Fields[Index].Value else Result:= '';
end;

//---------------------------------------------------------------------------
procedure TXMLNode.SetFieldValue(const Name: AnsiString; const Value: Variant);
var
 Index: Integer;
begin
 Index:= FindFieldByName(Name);
 if (Index <> -1) then Fields[Index].Value:= Value else AddField(Name, Value);
end;

//---------------------------------------------------------------------------
function TXMLNode.AddChild(const Name: AnsiString): TXMLNode;
var
 Index: Integer;
begin
 Index:= Length(Nodes);
 SetLength(Nodes, Index + 1);

 Nodes[Index]:= TXMLNode.Create(Name);
 Result:= Nodes[Index];
end;

//---------------------------------------------------------------------------
function TXMLNode.AddField(const Name: AnsiString;
 const Value: Variant): PXMLNodeField;
var
 Index: Integer;
begin
 Index:= Length(Fields);
 SetLength(Fields, Index + 1);

 Fields[Index].Name := Name;
 Fields[Index].Value:= Value;
 Result:= @Fields[Index];
end;

//---------------------------------------------------------------------------
function TXMLNode.SubCode(Spacing: Integer): AnsiString;
var
 st: AnsiString;
 i: Integer;
begin
 st:= Spaces(Spacing) + '<' + FName;
 if (Length(Fields) > 0) then
  begin
   st:= st + ' ';
   for i:= 0 to Length(Fields) - 1 do
    begin
     st:= st + Fields[i].Name + '="' + Fields[i].Value + '"';
     if (i < Length(Fields) - 1) then st:= st + ' ';
    end;
  end;

 if (Length(Nodes) > 0) then
  begin
   st:= st + '>'#13#10;
   for i:= 0 to Length(Nodes) - 1 do
    st:= st + Nodes[i].SubCode(Spacing + 1);
   st:= st + Spaces(Spacing) + '</' + FName + '>'#13#10; 
  end else st:= st + ' />'#13#10;

 Result:= st; 
end;

//---------------------------------------------------------------------------
function TXMLNode.GetCode(): AnsiString;
begin
 Result:= SubCode(0);
end;

//---------------------------------------------------------------------------
procedure TXMLNode.SaveToFile(const FileName: AnsiString);
var
 Strings: TStrings;
begin
 Strings:= TStringList.Create();
 Strings.Text:= GetCode();

 try
  Strings.SaveToFile(FileName);
 finally
  FreeAndNil(Strings);
 end;
end;

//---------------------------------------------------------------------------
function TXMLNode.SaveToStream(OutStream: TStream): Boolean;
var
 Strings: TStrings;
begin
 Strings:= TStringList.Create();
 Strings.Text:= GetCode();

 Result:= True;
 try
  try
   Strings.SaveToStream(OutStream);
  except
   Result:= False;
  end;
 finally
  FreeAndNil(Strings);
 end;
end;

//---------------------------------------------------------------------------
function TXMLNode.SaveToASDb(const Key: ShortString; Archive: TASDb): Boolean;
var
 Stream: TMemoryStream;
begin
 if (Archive = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Result:= Archive.UpdateOnce();
 if (not Result) then Exit;

 Stream:= TMemoryStream.Create();

 Result:= SaveToStream(Stream);
 if (not Result) then
  begin
   FreeAndNil(Stream);
   Exit;
  end;

 Auth.Authorize(Self, Archive);

 Result:= Archive.WriteRecord(Key, Stream.Memory, Stream.Size, recFile);

 Auth.Unauthorize();

 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TXMLNode.SaveToASDb(const Key: ShortString;
 const ArchiveName: AnsiString): Boolean;
var
 Archive: TASDb;
begin
 Archive:= TASDb.Create();

 Archive.FileName:= ArchiveName;
 Archive.OpenMode:= opUpdate;

 Result:= SaveToASDb(Key, Archive);

 FreeAndNil(Archive);
end;

//---------------------------------------------------------------------------
function TXMLNode.GetEnumerator(): TXMLNodeEnumerator;
begin
 Result:= TXMLNodeEnumerator.Create(Self);
end;

//---------------------------------------------------------------------------
constructor TXMLNodeEnumerator.Create(Node: TXMLNode);
begin
 inherited Create();

 FNode:= Node;
 Index:= -1;
end;

//---------------------------------------------------------------------------
function TXMLNodeEnumerator.GetCurrent(): TXMLNode;
begin
 Result:= FNode.Child[Index];
end;

//---------------------------------------------------------------------------
function TXMLNodeEnumerator.MoveNext(): Boolean;
begin
 Result:= Index < FNode.ChildCount - 1;
 if (Result) then Inc(Index);
end;

//--------------------------------------------------------------------------
function LoadEmptyRootNode(Parser: TXMLParser): TXMLNode;
var
 i: Integer;
begin
 Result:= TXMLNode.Create(Parser.CurName);

 with Parser.CurAttr do
  for i:= 0 to Count - 1 do
   Result.AddField(Name(i), Value(i));
end;

//---------------------------------------------------------------------------
procedure LoadNodeBody(TopNode: TXMLNode; Parser: TXMLParser);
var
 Aux: TXMLNode;
 i: Integer;
begin
 with Parser.CurAttr do
  for i:= 0 to Count - 1 do
   TopNode.AddField(Name(i), Value(i));

 while (Parser.Scan()) do
  case Parser.CurPartType of
   ptEndTag:
    Break;

   ptEmptyTag:
    begin
     Aux:= TopNode.AddChild(Parser.CurName);

     with Parser.CurAttr do
      for i:= 0 to Count - 1 do
       Aux.AddField(Name(i), Value(i));
    end;

   ptStartTag:
    begin
     Aux:= TopNode.AddChild(Parser.CurName);
     LoadNodeBody(Aux, Parser);
    end;
  end;
end;

//---------------------------------------------------------------------------
function LoadRootNode(Parser: TXMLParser): TXMLNode;
var
 Aux: TXMLNode;
 i: Integer;
begin
 Result:= TXMLNode.Create(Parser.CurName);

 // -> read attributes of root node
 with Parser.CurAttr do
  for i:= 0 to Count - 1 do
   Result.AddField(Name(i), Value(i));

 // -> parse the body
 while (Parser.Scan()) do
  case Parser.CurPartType of
   // exit out of root node
   ptEndTag:
    Break;

   // empty node inside of root node
   ptEmptyTag:
    begin
     Aux:= Result.AddChild(Parser.CurName);

     with Parser.CurAttr do
      for i:= 0 to Count - 1 do
       Aux.AddField(Name(i), Value(i));
    end;

   // new node owned by root node
   ptStartTag:
    begin
     Aux:= Result.AddChild(Parser.CurName);
     LoadNodeBody(Aux, Parser);
    end;
  end;
end;

//---------------------------------------------------------------------------
function LoadXMLFromFile(const FileName: AnsiString): TXMLNode;
var
 Parser: TXMLParser;
begin
 Result:= nil;

 Parser:= TXMLParser.Create();
 try
  Parser.LoadFromFile(FileName, fmOpenRead or fmShareDenyWrite);

  Parser.Normalize:= False;
  Parser.StartScan();

  while (Parser.Scan()) do
   case Parser.CurPartType of
    ptEmptyTag:
     begin
      Result:= LoadEmptyRootNode(Parser);
      Break;
     end;

    ptStartTag:
     begin
      Result:= LoadRootNode(Parser);
      Break;
     end;
   end;

 finally
  FreeAndNil(Parser);
 end;
end;

//---------------------------------------------------------------------------
function LoadXMLFromStream(InStream: TStream): TXMLNode;
var
 Strings: TStrings;
 Parser : TXMLParser;
begin
 Result:= nil;

 Strings:= TStringList.Create();
 Parser:= TXMLParser.Create();
 try
  try
   Strings.LoadFromStream(InStream);

   Parser.LoadFromBuffer(PAnsiChar(AnsiString(Strings.Text)));

   Parser.Normalize:= False;
   Parser.StartScan();

   while (Parser.Scan()) do
    case Parser.CurPartType of
     ptEmptyTag:
      begin
       Result:= LoadEmptyRootNode(Parser);
       Break;
      end;

     ptStartTag:
      begin
       Result:= LoadRootNode(Parser);
       Break;
      end;
    end;
  except
   FreeAndNil(Result);
  end;

 finally
  FreeAndNil(Strings);
  FreeAndNil(Parser);
 end;
end;

//---------------------------------------------------------------------------
function LoadXMLFromASDb(const Key: ShortString;
 Archive: TASDb): TXMLNode; overload;
var
 Stream: TMemoryStream;
begin
 Result:= nil;
 if (Archive = nil)or(not Archive.UpdateOnce()) then Exit;

 Stream:= TMemoryStream.Create();

 Auth.Authorize(nil, Archive);

 if (not Archive.ReadStream(Key, Stream)) then
  begin
   FreeAndNil(Stream);
   Exit;
  end;

 Auth.Unauthorize();

 Stream.Seek(0, soFromBeginning);

 Result:= LoadXMLFromStream(Stream);

 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function LoadXMLFromASDb(const Key: ShortString;
 const ArchiveName: AnsiString): TXMLNode; overload;
var
 Archive: TASDb;
begin
 Archive:= TASDb.Create();

 Archive.FileName:= ArchiveName;
 Archive.OpenMode:= opReadOnly;

 Result:= LoadXMLFromASDb(Key, Archive);

 FreeAndNil(Archive);
end;

//---------------------------------------------------------------------------
end.
