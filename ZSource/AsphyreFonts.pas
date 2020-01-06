unit AsphyreFonts;
// ---------------------------------------------------------------------------
// AsphyreFonts.pas                                     Modified: 09-Ago-2010
// Asphyre Bitmap fonts with Unicode support                     Version 2.02
// ---------------------------------------------------------------------------
// This file is unoffical
// Changed by Marcos Gomes
// ---------------------------------------------------------------------------
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
// http://www.mozilla.org/MPL/
// ---------------------------------------------------------------------------
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
// The Original Code is AsphyreFonts.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
// ---------------------------------------------------------------------------
//
// Special notes:
//
// * Unicode is only partially supported in text tags. The unicode tags
// are translated into current user locale before lookup.
//
// * TextRect also supports Unicode partially (using locale translation)
// unless TntUnicode option is enabled below.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Remove the dot to enable full Unicode support for TextRect method. This
// will require TNT Unicode components to be present in the library path.
// ---------------------------------------------------------------------------
{ .$define TntUnicode }

// ---------------------------------------------------------------------------
{$IFDEF fpc}{$MODE delphi}{$ENDIF}

// ---------------------------------------------------------------------------
interface

// ---------------------------------------------------------------------------
uses
  Types, Classes, {$IFDEF TntUnicode} TntClasses, {$ENDIF} SysUtils, Contnrs,
  Math, Vectors2px, Vectors2, AsphyreXML, MediaUtils, AsphyreColors,
  AsphyreTypes, AsphyreUtils, AsphyreImages, AbstractCanvas, HelperSets;

// ---------------------------------------------------------------------------
type
  PLetterEntry = ^TLetterEntry;

  TLetterEntry = record
    Top: Integer;
    Pos: TPoint2px;
    Size: TPoint2px;
    Leading: Integer;
    Trailing: Integer;
  end;

  // ---------------------------------------------------------------------------
  TAsphyreFonts = class;

  // ---------------------------------------------------------------------------
  PFontStyle = ^TFontStyle;

  TFontStyle = record
    Colors: array [0 .. 1] of TAsphyreColor;
    Style: Cardinal;
  end;

  // ---------------------------------------------------------------------------
  PFontTag = ^TFontTag;

  TFontTag = record
    Name: ShortString;
    Colors: TColor2;
    Style: Cardinal;
  end;

  // ---------------------------------------------------------------------------
  PFontState = ^TFontState;

  TFontState = record
    ImageIndex: Integer;
    FontSize: TPoint2px;
    DivSet: WideString;
    ParaSet: WideString;
    Scale: Single;
    Kerning: Single;
    Whitespace: Single;
    Linespace: Single;
  end;

  // ---------------------------------------------------------------------------
  THAlign = (hLeft, hCenter, hRight, hJustify);

  // ---------------------------------------------------------------------------
  TVAlign = (vTop, vMiddle, vBottom);

  // ---------------------------------------------------------------------------
  TCustomTextEvent = procedure(Sender: TObject; Image: TAsphyreImage;
    const SrcRect, DestRect: TRect; const Colors: TColor4; User: Pointer);

  // ---------------------------------------------------------------------------
  TAsphyreFont = class
  private
    FOwner: TAsphyreFonts;
    FName: ShortString;

    Entries: array [0 .. 65535] of TLetterEntry;
    FImageIndex: Integer;

    StyleStack: TStack;
    StateStack: TStack;
{$IFDEF TntUnicode}
    Words: TTntStringList;
{$ELSE}
    Words: TStringList;
{$ENDIF}
    FParaSet: WideString;
    FDivSet: WideString;
    FFontSize: TPoint2px;

    FScale: Single;
    FKerning: Single;
    FWhitespace: Single;
    FLinespace: Single;

    function ParseTag(const Text: WideString; var CurPos: Integer;
      NoStyle: Boolean): Boolean;

    procedure ParseEntry(Node: TXMLNode);
    procedure ClearStyles();
    procedure PushStyle(const Colors: TColor2; Style: Cardinal);
    function PeekStyle(): PFontStyle;
    procedure PopStyle();
    procedure DisplayText(const Pos: TPoint2; const Text: WideString;
      Alpha: Single);

    function IsDivChar(Ch: WideChar): Boolean;
    function ExtractWord(const Text: WideString; var Step, Para: Integer;
      out Segment: WideString): Boolean;
    procedure SplitText(const Text: WideString);
  public
    // -------------------------------------------------------------------------
    // PROPERTIES
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // The owner of this class must be a valid instance of TBitmapFonts.
    // -------------------------------------------------------------------------
    property Owner: TAsphyreFonts read FOwner;

    // -------------------------------------------------------------------------
    // Represents the font name.
    // -------------------------------------------------------------------------
    property Name: ShortString read FName write FName;

    // -------------------------------------------------------------------------
    // This must be a valid index to images inside TAsphyreDevice of the owner.
    // -------------------------------------------------------------------------
    property ImageIndex: Integer read FImageIndex write FImageIndex;

    // -------------------------------------------------------------------------
    // This is the maximum "window" covering all letters as initially rendered
    // by the FontTool. It is used for drawing letter gradients correctly.
    // -------------------------------------------------------------------------
    property FontSize: TPoint2px read FFontSize write FFontSize;

    // -------------------------------------------------------------------------
    // These are "divisors" that are used to split words when using TextRect.
    // -------------------------------------------------------------------------
    property DivSet: WideString read FDivSet write FDivSet;

    // -------------------------------------------------------------------------
    // These are "paragraph" characters that indicate the beginning of the
    // next paragraph, when using TextRect.
    // -------------------------------------------------------------------------
    property ParaSet: WideString read FParaSet write FParaSet;

    // -------------------------------------------------------------------------
    // This property indicates how big or small the font is. Notice that
    // the kerning and whitespace are also rescaled by this value.
    // -------------------------------------------------------------------------
    property Scale: Single read FScale write FScale;

    // -------------------------------------------------------------------------
    // This is the additional space added to every character's width. It can
    // compress or expand the rendered text.
    // -------------------------------------------------------------------------
    property Kerning: Single read FKerning write FKerning;

    // -------------------------------------------------------------------------
    // This is the space used for blank characters.
    // -------------------------------------------------------------------------
    property Whitespace: Single read FWhitespace write FWhitespace;

    // -------------------------------------------------------------------------
    // This is the space added between individual lines drawn with TextRect.
    // -------------------------------------------------------------------------
    property Linespace: Single read FLinespace write FLinespace;

    // -------------------------------------------------------------------------
    // METHODS
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Loads the font description from external link.
    // -------------------------------------------------------------------------
    function ParseLink(const Link: string): Boolean;

    // -------------------------------------------------------------------------
    // Saves the state of all properties (e.g. scale, kerning, etc.)
    // -------------------------------------------------------------------------
    procedure SaveState();

    // -------------------------------------------------------------------------
    // Restores the state of all properties (e.g. scale, kerning, etc.)
    // -------------------------------------------------------------------------
    procedure RestoreState();

    // -------------------------------------------------------------------------
    // Removes all state entries saved by SaveState().
    // -------------------------------------------------------------------------
    procedure ClearStates();

    procedure TextOut(const Pos: TPoint2; const Text: WideString;
      const Colors: TColor2; Alpha: Single = 1.0); overload;
    procedure CustomOut(const Pos: TPoint2; const Text: WideString;
      const Color: TColor2; Alpha: Single; Event: TCustomTextEvent;
      User: Pointer);

    procedure TextMid(const Pos: TPoint2px; const Text: WideString;
      const Colors: TColor2; Alpha: Single = 1.0);

    procedure TextMidF(const Pos: TPoint2; const Text: WideString;
      const Colors: TColor2; Alpha: Single = 1.0);
    procedure TextMidFF(const Pos: TPoint2; const Text: WideString;
      const Colors: TColor2; Alpha: Single = 1.0);

    function TextExtent(const Text: WideString): TPoint2;
    function TextWidth(const Text: WideString): Single;
    function TextHeight(const Text: WideString): Single;

    procedure TextRect(const Pos, Size, Paragraph: TPoint2;
      const Text: WideString; const Colors: TColor2; Alpha: Single;
      Cut: Boolean = False);

    // ------------------------------------------------------------------------
    // The following method draws aligned text in a Rect
    // ------------------------------------------------------------------------
    procedure TextRectEx(const Pos, Size: TPoint2; const Text: WideString;
      const Colors: TColor2; Alpha: Single; const HAlign: THAlign = hLeft;
      const VAlign: TVAlign = vTop; PLine: Boolean = True);

    // -------------------------------------------------------------------------
    // The following method estimates the rectangles occupied by individual
    // letters when rendered on the screen.
    // -------------------------------------------------------------------------
    procedure TextRects(const Text: WideString; List: TRectList);

    constructor Create(AOwner: TAsphyreFonts);
    destructor Destroy(); override;
  end;

  // ---------------------------------------------------------------------------
  TAsphyreFonts = class
  private
    DestroyHandle: Cardinal;

    Fonts: array of TAsphyreFont;
    Tags: array of TFontTag;
    TagsDirty: Boolean;

    FImages: TAsphyreImages;
    FCanvas: TAsphyreCanvas;

    procedure DeviceDestroy(Sender: TObject; Param: Pointer;
      var Handled: Boolean);

    function GetCount(): Integer;
    function GetFont(Num: Integer): TAsphyreFont;

    function InsertFont(): Integer;
    function IndexOfTag(const Name: ShortString): Integer;
    procedure DeleteTag(Index: Integer);

    procedure Swap(Index1, Index2: Integer);
    function SortSplit(Start, Stop: Integer): Integer;
    procedure Quicksort(Start, Stop: Integer);
  public
    property Images: TAsphyreImages read FImages write FImages;
    property Canvas: TAsphyreCanvas read FCanvas write FCanvas;

    property Font[Num: Integer]: TAsphyreFont read GetFont; default;
    property Count: Integer read GetCount;

    function Insert(const DescLink: string; const ImageName: ShortString): Integer;

    procedure RemoveFont(Num: Integer);
    procedure RemoveAll();

    procedure InsertTag(const Name: ShortString; const Colors: TColor2;
      Style: Cardinal = 0);
    procedure RemoveTags();
    procedure RemoveTag(const Name: ShortString);
    function FindTag(const Name: ShortString): PFontTag;
    function FindFont(const Name: ShortString): TAsphyreFont;

    constructor Create();
    destructor Destroy(); override;
  end;

  // ---------------------------------------------------------------------------
implementation

// ---------------------------------------------------------------------------
uses
  AbstractDevices;

// ---------------------------------------------------------------------------
constructor TAsphyreFont.Create(AOwner: TAsphyreFonts);
begin
  inherited Create();

  FOwner := AOwner;
{$IFDEF TntUnicode}
  Words := TTntStringList.Create();
{$ELSE}
  Words := TStringList.Create();
{$ENDIF}
  StyleStack := TStack.Create();
  StateStack := TStack.Create();

  FImageIndex := -1;
  FScale := 1.0;
  FKerning := -1.0;
  FWhitespace := 5.0;
  FLinespace := 2.0;

  FParaSet := #10;
  FDivSet := #13 + #32 + #8;
end;

// ---------------------------------------------------------------------------
destructor TAsphyreFont.Destroy();
begin
  ClearStates();
  StateStack.Free();

  ClearStyles();
  StyleStack.Free();

  Words.Free();

  inherited;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.ParseEntry(Node: TXMLNode);
var
  CharCode: Integer;
begin
  CharCode := ParseInt(Node.FieldValue['ascii'], -1);
  if (CharCode < 0) or (CharCode > 255) then
  begin
    CharCode := ParseInt(Node.FieldValue['ucode'], -1);
    if (CharCode < 0) then
      Exit;
  end;

  with Entries[CharCode] do
  begin
    Top := ParseInt(Node.FieldValue['top'], 0);
    Pos.x := ParseInt(Node.FieldValue['x'], 0);
    Pos.y := ParseInt(Node.FieldValue['y'], 0);
    Size.x := ParseInt(Node.FieldValue['width'], 0);
    Size.y := ParseInt(Node.FieldValue['height'], 0);
    Leading := ParseInt(Node.FieldValue['leading'], 0);
    Trailing := ParseInt(Node.FieldValue['trailing'], 0);
  end;
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.ParseLink(const Link: string): Boolean;
var
  Node, Child: TXMLNode;
{$IFDEF fpc}
  i: Integer;
{$ENDIF}
begin
  Node := LoadLinkXML(Link);

  Result := Node <> nil;
  if (not Result) then
    Exit;

  FFontSize.x := ParseInt(Node.FieldValue['width'], 0);
  FFontSize.y := ParseInt(Node.FieldValue['height'], 0);
{$IFDEF fpc}
  for i := 0 to Node.ChildCount - 1 do
  begin
    Child := Node.Child[i];
{$ELSE}
    for Child in Node do
{$ENDIF}
      if (LowerCase(Child.Name) = 'item') then
        ParseEntry(Child);
{$IFDEF fpc}
  end;
{$ENDIF}
  Node.Free();
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.ClearStyles();
var
  State: PFontStyle;
begin
  while (StyleStack.Count > 0) do
  begin
    State := StyleStack.Pop();
    FreeMem(State);
  end;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.PushStyle(const Colors: TColor2; Style: Cardinal);
var
  State: PFontStyle;
begin
  State := AllocMem(SizeOf(TFontStyle));

  State.Colors[0] := Colors[0];
  State.Colors[1] := Colors[1];
  State.Style := Style;

  StyleStack.Push(State);
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.PeekStyle(): PFontStyle;
begin
  if (StyleStack.Count > 0) then
  begin
    Result := StyleStack.Peek();
  end
  else
    Result := nil;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.PopStyle();
var
  Style: PFontStyle;
begin
  if (StyleStack.AtLeast(1)) then
  begin
    Style := StyleStack.Pop();
    FreeMem(Style);
  end;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.ClearStates();
var
  State: PFontState;
begin
  while (StateStack.Count > 0) do
  begin
    State := StateStack.Pop();
    FreeMem(State);
  end;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.SaveState();
var
  State: PFontState;
begin
  State := AllocMem(SizeOf(TFontState));

  State.ImageIndex := FImageIndex;
  State.FontSize := FFontSize;
  State.DivSet := FDivSet;
  State.ParaSet := FParaSet;
  State.Scale := FScale;
  State.Kerning := FKerning;
  State.Whitespace := FWhitespace;
  State.Linespace := FLinespace;

  StateStack.Push(State);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.RestoreState();
var
  State: PFontState;
begin
  if (StyleStack.AtLeast(1)) then
  begin
    State := StyleStack.Pop();

    FImageIndex := State.ImageIndex;
    FFontSize := State.FontSize;
    FDivSet := State.DivSet;
    FParaSet := State.ParaSet;
    FScale := State.Scale;
    FKerning := State.Kerning;
    FWhitespace := State.Whitespace;
    FLinespace := State.Linespace;

    FreeMem(State);
  end;
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.ParseTag(const Text: WideString; var CurPos: Integer;
  NoStyle: Boolean): Boolean;
var
  TagPos, TagSize, PreCurPos: Integer;
  TagName: WideString;
  Tag: PFontTag;
begin
  PreCurPos := CurPos;

  // -> Check whether there is a tag.
  Result := Text[CurPos] = '<';
  if (not Result) then
    Exit;

  // -> Check for invalid "<" tag at the end of string.
  if (CurPos >= Length(Text)) then
  begin
    Result := False;
    Exit;
  end;

  // -> Mark the beginning of tag text.
  Inc(CurPos);

  TagPos := CurPos;
  TagSize := 0;

  // -> Scan for the end of the tag.
  while (CurPos <= Length(Text)) and (Text[CurPos] <> '>') do
  begin
    Inc(TagSize);
    Inc(CurPos);
  end;

  // -> Check if tag was not closed at the end of string.
  if (CurPos > Length(Text)) then
  begin
    Result := False;
    CurPos := PreCurPos;
    Exit;
  end;

  // -> Skip ">" letter.
  Inc(CurPos);
  if (NoStyle) then
    Exit;

  // -> Extract tag name from the string.
  TagName := Copy(Text, TagPos, TagSize);

  if (TagName = '/') then
  begin
    if (StyleStack.Count > 1) then
      PopStyle();
    Exit;
  end;

  Tag := FOwner.FindTag(ShortString(TagName));
  if (Tag <> nil) then
    PushStyle(Tag.Colors, Tag.Style);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.DisplayText(const Pos: TPoint2; const Text: WideString;
  Alpha: Single);
var
  CharNo: Integer;
  uCode: Integer;
  xPos: Single;
  Style: PFontStyle;
  Entry: PLetterEntry;
  Image: TAsphyreImage;
  Canvas: TAsphyreCanvas;
  iAlpha: Integer;
  DrawPos: TPoint2;
  DrawSize: TPoint2;
  Colors: TColor2;
begin
  if (FOwner.Images = nil) or (FOwner.Canvas = nil) then
    Exit;

  Image := FOwner.Images[FImageIndex];
  if (Image = nil) then
    Exit;

  Canvas := FOwner.Canvas;

  // -> Start processing text.
  xPos := Pos.x;
  CharNo := 1;
  iAlpha := MinMax2(Round(Alpha * 255.0), 0, 255);

  while (CharNo <= Length(Text)) do
  begin
    // -> Check if there are any tags in the text.
    if (ParseTag(Text, CharNo, False)) then
      Continue;

    // -> Retreive letter, its numerical code, and the current style.
    uCode := Word(Text[CharNo]);
    Entry := @Entries[uCode];
    Style := PeekStyle();

    // -> Check whether the letter has its drawing information.
    if (Entry.Size.x < 1) or (Entry.Size.y < 1) or (Style = nil) then
    begin
      Inc(CharNo);
      xPos := xPos + FWhitespace * FScale;
      Continue;
    end;

    if (Style = nil) then
      Continue;

    // -> Include leading space.
    xPos := xPos + Entry.Leading * FScale;

    // -> Compute drawing position and size.
    DrawPos.x := xPos;
    DrawPos.y := Pos.y + (Entry.Top * FScale);

    DrawSize.x := Entry.Size.x * FScale;
    DrawSize.y := Entry.Size.y * FScale;

    // -> Interpolate drawing colors.
    Colors[0] := cLerp(Style.Colors[0], Style.Colors[1],
      Entry.Top / FFontSize.y);
    Colors[1] := cLerp(Style.Colors[0], Style.Colors[1],
      (Entry.Top + Entry.Size.y) / FFontSize.y);

    // -> Display the letter.
    Canvas.UseImagePx(Image, pxBounds4(Entry.Pos.x, Entry.Pos.y, Entry.Size.x,
        Entry.Size.y));

    Canvas.TexMap(pBounds4(DrawPos.x, DrawPos.y, DrawSize.x, DrawSize.y),
      cColorAlpha4(Colors[0], Colors[0], Colors[1], Colors[1], iAlpha, iAlpha,
        iAlpha, iAlpha));

    // -> Keep going horizontally.
    Inc(CharNo);
    xPos := xPos + (Entry.Size.x + Entry.Trailing + FKerning) * FScale;
  end;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.CustomOut(const Pos: TPoint2; const Text: WideString;
  const Color: TColor2; Alpha: Single; Event: TCustomTextEvent; User: Pointer);
var
  CharNo: Integer;
  Ascii: Integer;
  xPos: Single;
  Style: PFontStyle;
  Entry: PLetterEntry;
  Image: TAsphyreImage;
  iAlpha: Integer;
  DrawPos: TPoint2;
  DrawSize: TPoint2;
  Colors: TColor2;
begin
  if (FOwner.Images = nil) or (FOwner.Canvas = nil) then
    Exit;

  ClearStyles();
  PushStyle(Color, 0);

  Image := FOwner.Images[FImageIndex];
  if (Image = nil) then
    Exit;

  xPos := Pos.x;
  CharNo := 1;
  iAlpha := MinMax2(Round(Alpha * 255.0), 0, 255);

  while (CharNo <= Length(Text)) do
  begin
    if (ParseTag(Text, CharNo, False)) then
      Continue;

    Ascii := Ord(Text[CharNo]);
    Entry := @Entries[Ascii];
    Style := PeekStyle();

    if (Entry.Size.x < 1) or (Entry.Size.y < 1) or (Style = nil) then
    begin
      Inc(CharNo);
      xPos := xPos + FWhitespace * FScale;
      Continue;
    end;

    if (Style = nil) then
      Continue;

    xPos := xPos + Entry.Leading * FScale;

    DrawPos.x := xPos;
    DrawPos.y := Pos.y + (Entry.Top * FScale);

    DrawSize.x := Entry.Size.x * FScale;
    DrawSize.y := Entry.Size.y * FScale;

    Colors[0] := cLerp(Style.Colors[0], Style.Colors[1],
      Entry.Top / FFontSize.y);
    Colors[1] := cLerp(Style.Colors[0], Style.Colors[1],
      (Entry.Top + Entry.Size.y) / FFontSize.y);

    Event(Self, Image, Bounds(Entry.Pos.x, Entry.Pos.y, Entry.Size.x,
        Entry.Size.y), Bounds(Round(DrawPos.x), Round(DrawPos.y),
        Round(DrawSize.x), Round(DrawSize.y)),
      cColorAlpha4(Colors[0], Colors[0], Colors[1], Colors[1], iAlpha, iAlpha,
        iAlpha, iAlpha), User);

    Inc(CharNo);
    xPos := xPos + (Entry.Size.x + Entry.Trailing + FKerning) * FScale;
  end;

  ClearStyles();
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.TextExtent(const Text: WideString): TPoint2;
var
  CharNo: Integer;
  Ascii: Integer;
  Entry: PLetterEntry;
  KernSub: Single;
begin
  CharNo := 1;

  Result.x := 0.0;
  Result.y := FFontSize.y * FScale;
  KernSub := 0.0;

  while (CharNo <= Length(Text)) do
  begin
    if (ParseTag(Text, CharNo, True)) then
      Continue;

    Ascii := Ord(Text[CharNo]);
    Entry := @Entries[Ascii];

    if (Entry.Size.x < 1) or (Entry.Size.y < 1) then
    begin
      Inc(CharNo);

      Result.x := Result.x + FWhitespace * FScale;
      Continue;
    end;

    Inc(CharNo);

    Result.x := Result.x +
      (Entry.Size.x + Entry.Leading + Entry.Trailing + FKerning) * FScale;
    KernSub := FKerning * FScale;
  end;

  Result.x := Result.x - KernSub;
end;

// ---------------------------------------------------------------------------

procedure TAsphyreFont.TextRects(const Text: WideString; List: TRectList);
var
  CharNo: Integer;
  uCode: Integer;
  xPos: Single;
  Entry: PLetterEntry;
  Rect: TRect;
begin
  xPos := 0;
  CharNo := 1;

  Rect.Top := 0;
  Rect.Bottom := Round(FFontSize.y * FScale);

  while (CharNo <= Length(Text)) do
  begin
    if (ParseTag(Text, CharNo, False)) then
      Continue;

    uCode := Word(Text[CharNo]);
    Entry := @Entries[uCode];

    if (Entry.Size.x < 1) or (Entry.Size.y < 1) then
    begin
      Inc(CharNo);

      Rect.Left := Round(xPos);
      Rect.Right := Round(xPos + FWhitespace * FScale);
      List.Add(Rect);

      xPos := xPos + FWhitespace * FScale;
      Continue;
    end;

    xPos := xPos + Entry.Leading * FScale;

    Rect.Left := Round(xPos);
    Rect.Right := Round(xPos + (Entry.Size.x + Entry.Trailing) * FScale) + 1;
    List.Add(Rect);

    // -> Keep going horizontally.
    Inc(CharNo);
    xPos := xPos + (Entry.Size.x + Entry.Trailing + FKerning) * FScale;
  end;
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.TextWidth(const Text: WideString): Single;
begin
  Result := TextExtent(Text).x;
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.TextHeight(const Text: WideString): Single;
begin
  Result := TextExtent(Text).y;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextOut(const Pos: TPoint2; const Text: WideString;
  const Colors: TColor2; Alpha: Single);
begin
  ClearStyles();
  PushStyle(Colors, 0);

  DisplayText(Pos, Text, Alpha);

  ClearStyles();
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextMid(const Pos: TPoint2px; const Text: WideString;
  const Colors: TColor2; Alpha: Single);
var
  TextSize: TPoint2;
  DrawPos: TPoint2px;
begin
  TextSize := TextExtent(Text);

  DrawPos.x := Pos.x - Round(TextSize.x * 0.5);
  DrawPos.y := Pos.y - Round(TextSize.y * 0.5);

  TextOut(DrawPos, Text, Colors, Alpha);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextMidF(const Pos: TPoint2; const Text: WideString;
  const Colors: TColor2; Alpha: Single);
var
  TextSize: TPoint2;
  DrawPos: TPoint2;
begin
  TextSize := TextExtent(Text);

  DrawPos.x := Pos.x - Round(TextSize.x * 0.5);
  DrawPos.y := Pos.y - Round(TextSize.y * 0.5);

  TextOut(DrawPos, Text, Colors, Alpha);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextMidFF(const Pos: TPoint2; const Text: WideString;
  const Colors: TColor2; Alpha: Single);
var
  TextSize: TPoint2;
  DrawPos: TPoint2;
begin
  TextSize := TextExtent(Text);

  DrawPos.x := Pos.x - TextSize.x * 0.5;
  DrawPos.y := Pos.y - TextSize.y * 0.5;

  TextOut(DrawPos, Text, Colors, Alpha);
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.IsDivChar(Ch: WideChar): Boolean;
var
  IsDiv: Boolean;
begin
  IsDiv := Pos(Ch, FDivSet) <> 0;

  Result := (IsDiv) or (Ch = #32);
end;

// ---------------------------------------------------------------------------
function TAsphyreFont.ExtractWord(const Text: WideString;
  var Step, Para: Integer; out Segment: WideString): Boolean;
var
  SegPos, SegSize: Integer;
begin
  Segment := '';

  // -> Skip all unused characters.
  while (Step <= Length(Text)) and (IsDivChar(Text[Step])) do
    Inc(Step);

  // -> Check for end of text.
  if (Step > Length(Text)) then
  begin
    Result := False;
    Exit;
  end;

  // -> Check for next paragraph.
  if (Pos(Text[Step], FParaSet) <> 0) then
  begin
    Inc(Para);
    Inc(Step);
    Result := True;
    Exit;
  end;

  // -> Start parsing the word.
  SegPos := Step;
  SegSize := 0;

  while (Step <= Length(Text)) and (Pos(Text[Step], FDivSet) = 0) do
  begin
    Inc(Step);
    Inc(SegSize);
  end;

  // -> Extract text segment.
  Segment := Copy(Text, SegPos, SegSize);

  Result := (SegSize > 0);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.SplitText(const Text: WideString);
var
  Step, Para: Integer;
  Segment: WideString;
begin
  Words.Clear();

  Step := 1;
  Para := 0;

  while (ExtractWord(Text, Step, Para, Segment)) do
    if (Length(Segment) > 0) then
      Words.AddObject(Segment, TObject(Para));
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextRect(const Pos, Size, Paragraph: TPoint2;
  const Text: WideString; const Colors: TColor2; Alpha: Single;
  Cut: Boolean = False);
var
  Para, ParaTo: Integer;
  WordNo, WordTo, NoWords, Index: Integer;
  PreSize, CurSize, BlnkSpace, MaxSize, Ident, Height, PosAdd: Single;
  CurPos, TextSize: TPoint2;
begin
  SplitText(Text);

  Para := -1;
  WordNo := 0;

  ClearStyles();
  PushStyle(Colors, 0);

  CurPos.x := Pos.x;

  Height := 0;

  while (WordNo < Words.Count) do
  begin
    PreSize := 0.0;
    CurSize := 0.0;
    BlnkSpace := 0.0;

    MaxSize := Size.x - (CurPos.x - Pos.x);

    WordTo := WordNo;
    ParaTo := Para;
    while (CurSize + BlnkSpace < MaxSize) and (WordTo < Words.Count) and
      (ParaTo = Para) do
    begin
      PreSize := CurSize;
      CurSize := CurSize + TextWidth(Words[WordTo]);
      BlnkSpace := BlnkSpace + FWhitespace * FScale;
      ParaTo := Integer(Words.Objects[WordTo]);

      Inc(WordTo);
    end;

    NoWords := (WordTo - WordNo) - 1;
    if (WordTo >= Words.Count) and (CurSize + BlnkSpace < MaxSize) then
    begin
      Inc(NoWords);
      PreSize := CurSize;
    end;

    if (NoWords < 1) then
    begin
      // Case 1. New paragraph.
      if (ParaTo <> Para) then
      begin
        CurPos.x := Pos.x + Paragraph.x;
        if (WordNo < 1) then
          CurPos.y := Pos.y
        else
          CurPos.y := CurPos.y + Paragraph.y;

        Para := ParaTo;

        Continue;
      end
      else
        // Case 2. Exhausted words or size doesn't fit.
        Break;
    end;

    if (NoWords > 1) { and(WordNo + NoWords < Words.Count) } then
      Ident := (MaxSize - PreSize) / (NoWords - 1)
    else
      Ident := 0.0;

    if ((ParaTo <> Para) and (NoWords > 1)) or
      (WordNo + NoWords >= Words.Count) then
      Ident := FWhitespace * FScale;

    PosAdd := 0.0;

    // Calculate Vertical offset
    // only draw text inside the rect
    if not(Cut) or ((CurPos.y - Pos.y) + (Height + FLinespace) <= Size.y) then
    begin
      for Index := WordNo to WordNo + NoWords - 1 do
      begin
        DisplayText(Point2(CurPos.x + Round(PosAdd), CurPos.y), Words[Index],
          Alpha);
        TextSize := TextExtent(Words[Index]);

        // CurPos.x:= CurPos.x + TextSize.x + Ident;
        PosAdd := PosAdd + TextSize.x + Ident;
        Height := Max(Height, TextSize.y);
      end;
    end
    else
      Break;

    CurPos.x := Pos.x;
    CurPos.y := CurPos.y + Height + FLinespace;

    Inc(WordNo, NoWords);
  end;

  ClearStyles();
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFont.TextRectEx(const Pos, Size: TPoint2;
  const Text: WideString; const Colors: TColor2; Alpha: Single;
  const HAlign: THAlign = hLeft; const VAlign: TVAlign = vTop;
  PLine: Boolean = True);
var
  Para, ParaTo: Integer;
  WordNo, WordTo, NoWords, Index: Integer;
  CurSize, PreSize, BlnkSpace, MaxSize, Height, Ident, PosAdd: Single;
  CurPos: TPoint2;

  i, ParaNo: Integer;
  EmptySizeX, EmptySizeY, xPos, YPos: Single;
  LineText: WideString;
  Lines, ParaList: TStringList;

begin
  Lines := TStringList.Create;
  ParaList := TStringList.Create;

  SplitText(Text);

  Para := -1;
  ParaNo := 0;
  WordNo := 0;

  ClearStyles();
  PushStyle(Colors, 0);

  CurPos.x := Pos.x;
  CurPos.y := Pos.y;
  Height := 0;
  MaxSize := Size.x;

  while (WordNo < Words.Count) do
  begin
    CurSize := 0;
    BlnkSpace := 0;

    WordTo := WordNo;
    ParaTo := Para;

    while (CurSize + BlnkSpace < MaxSize) and (WordTo < Words.Count) and
      (ParaTo = Para) do
    begin
      CurSize := CurSize + TextWidth(Words[WordTo]);
      BlnkSpace := BlnkSpace + FWhitespace * FScale;
      ParaTo := Integer(Words.Objects[WordTo]);

      Inc(WordTo);
    end;

    NoWords := (WordTo - WordNo) - 1;
    if (WordTo >= Words.Count) and (CurSize + BlnkSpace < MaxSize) then
    begin
      Inc(NoWords);
    end;

    if (NoWords < 1) then
    begin
      // Case 1. New paragraph.
      if (ParaTo <> Para) then
      begin
        Para := ParaTo;

        if (WordNo >= 1) and (HAlign = hJustify) then
        begin
          ParaList.Add(IntToStr(ParaNo));
        end;

        if (WordNo >= 1) and (PLine) then
          Lines.Add('');

        Inc(ParaNo);
        Continue;
      end
      else
        // Case 2. Exhausted words or size doesn't fit.
        Break;
    end;

    if ((Height + FLinespace) * (Lines.Count + 1)) <= Size.y then
    begin
      LineText := '';
      for Index := WordNo to WordNo + NoWords - 1 do
      begin
        if Index = (WordNo + NoWords - 1) then
          LineText := LineText + Words[Index]
        else
          LineText := LineText + Words[Index] + ' ';
      end;
      Lines.Add(LineText);

      // Calculate max line height
      Height := Max(Height, TextHeight(LineText));
    end
    else
      Break;

    Inc(ParaNo);
    Inc(WordNo, NoWords);
  end;

  // Draw all lines
  for i := 0 to Lines.Count - 1 do
  begin
    EmptySizeX := Size.x - TextWidth(Lines[i]);
    EmptySizeY := Size.y - ((Height + FLinespace) * (Lines.Count));

    case VAlign of
      vTop:
        YPos := CurPos.y;
      vMiddle:
        YPos := CurPos.y + Round(EmptySizeY / 2);
      vBottom:
        YPos := CurPos.y + Round(EmptySizeY);
    else
      YPos := CurPos.y;
    end;

    case HAlign of
      hLeft:
        xPos := CurPos.x;
      hCenter:
        xPos := CurPos.x + Round(EmptySizeX / 2);
      hRight:
        xPos := CurPos.x + Round(EmptySizeX);
      hJustify:
        begin
          xPos := CurPos.x;

          SplitText(Lines[i]);

          PreSize := 0.0;
          for index := 0 to Words.Count - 1 do
          begin
            PreSize := PreSize + TextWidth(Words[Index]);
          end;

          if (Words.Count - 1) > 0 then
            Ident := (Size.x - PreSize) / (Words.Count - 1)
          else
            Ident := 0.0;

          // Next line is paragraph
          for index := 0 to ParaList.Count - 1 do
          begin
            if PLine then
              if (StrToInt(ParaList[index]) = (i + 2)) then
              begin
                Ident := FWhitespace * FScale;
                Break;
              end
              else if (StrToInt(ParaList[index]) = (i + 1)) then
              begin
                Ident := FWhitespace * FScale;
                Break;
              end;
          end;

          // Is the last Line
          if (i = (Lines.Count - 1)) then
            Ident := FWhitespace * FScale;

          // Draw word by word
          PosAdd := 0.0;
          for Index := 0 to Words.Count - 1 do
          begin
            DisplayText(Point2(xPos + Round(PosAdd), YPos), Words[Index],
              Alpha);
            PosAdd := PosAdd + TextWidth(Words[Index]) + Ident;
          end;
        end
      else
        xPos := CurPos.x;
    end;

    if HAlign <> hJustify then
      DisplayText(Point2(xPos, YPos), Lines[i], Alpha);

    CurPos.x := Pos.x;
    CurPos.y := CurPos.y + Height + FLinespace;
  end;

  ClearStyles();

  Lines.Free;
  ParaList.Free;
end;

// ---------------------------------------------------------------------------
constructor TAsphyreFonts.Create();
begin
  inherited;

  DestroyHandle := EventDeviceDestroy.Subscribe(DeviceDestroy, -1);

  TagsDirty := False;
end;

// ---------------------------------------------------------------------------
destructor TAsphyreFonts.Destroy();
begin
  EventDeviceDestroy.Unsubscribe(DestroyHandle);

  RemoveAll();

  inherited;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.DeviceDestroy(Sender: TObject; Param: Pointer;
  var Handled: Boolean);
begin
  RemoveAll();
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.GetCount(): Integer;
begin
  Result := Length(Fonts);
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.GetFont(Num: Integer): TAsphyreFont;
begin
  if (Num >= 0) and (Num < Length(Fonts)) then
    Result := Fonts[Num]
  else
    Result := nil;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.RemoveAll();
var
  i: Integer;
begin
  for i := 0 to Length(Fonts) - 1 do
    if (Fonts[i] <> nil) then
      FreeAndNil(Fonts[i]);

  SetLength(Fonts, 0);
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.InsertFont(): Integer;
begin
  Result := Length(Fonts);
  SetLength(Fonts, Result + 1);

  Fonts[Result] := TAsphyreFont.Create(Self);
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.RemoveFont(Num: Integer);
var
  i: Integer;
begin
  if (Num < 0) or (Num >= Length(Fonts)) then
    Exit;

  FreeAndNil(Fonts[Num]);

  for i := Num to Length(Fonts) - 2 do
    Fonts[i] := Fonts[i + 1];

  SetLength(Fonts, Length(Fonts) - 1);
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.Insert(const DescLink: string; const ImageName: ShortString): Integer;
var
  ImageIndex: Integer;
  AName: ShortString;
begin
  Result := -1;
  if Pos('.image', ImageName) > 0 then
    AName := Copy(ImageName, 0, Pos('.image', ImageName) - 1)
  else
    AName := Copy(ImageName, 0, Length(ImageName));

  // (1) Check whether a valid image list is provided.
  if (FImages = nil) then
    Exit;

  // (2) Resolve the bitmap font's graphics.
  ImageIndex := FImages.IndexOf(ImageName);
  if (ImageIndex = -1) then
    Exit;

  // (3) Create new font and try to parse its description.
  Result := InsertFont();
  if (not Fonts[Result].ParseLink(DescLink)) then
  begin
    RemoveFont(Result);
    Result := -1;
    Exit;
  end;

  // (4) Assign font attributes.
  Fonts[Result].ImageIndex := ImageIndex;
  Fonts[Result].Name := AName;

end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.InsertTag(const Name: ShortString;
  const Colors: TColor2; Style: Cardinal);
var
  Index: Integer;
begin
  Index := Length(Tags);
  SetLength(Tags, Index + 1);

  Tags[Index].Name := Name;
  Tags[Index].Colors := Colors;
  Tags[Index].Style := Style;

  TagsDirty := True;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.RemoveTags();
begin
  SetLength(Tags, 0);

  TagsDirty := False;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.DeleteTag(Index: Integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index >= Length(Tags)) then
    Exit;

  for i := Index to Length(Tags) - 2 do
    Tags[i] := Tags[i + 1];

  SetLength(Tags, Length(Tags) - 1);

  TagsDirty := True;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.Swap(Index1, Index2: Integer);
var
  Aux: TFontTag;
begin
  Aux := Tags[Index1];

  Tags[Index1] := Tags[Index2];
  Tags[Index2] := Aux;
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.SortSplit(Start, Stop: Integer): Integer;
var
  Left, Right: Integer;
  Pivot: ShortString;
begin
  Left := Start + 1;
  Right := Stop;
  Pivot := Tags[Start].Name;

  while (Left <= Right) do
  begin
    while (Left <= Stop) and (CompareText(Tags[Left].Name, Pivot) < 0) do
      Inc(Left);

    while (Right > Start) and (CompareText(Tags[Right].Name, Pivot) >= 0) do
      Dec(Right);

    if (Left < Right) then
      Swap(Left, Right);
  end;

  Swap(Start, Right);

  Result := Right;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.Quicksort(Start, Stop: Integer);
var
  SplitPt: Integer;
begin
  if (Start < Stop) then
  begin
    SplitPt := SortSplit(Start, Stop);

    Quicksort(Start, SplitPt - 1);
    Quicksort(SplitPt + 1, Stop);
  end;
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.IndexOfTag(const Name: ShortString): Integer;
var
  Lo, Hi, Mid: Integer;
begin
  if (TagsDirty) then
  begin
    Quicksort(0, Length(Tags) - 1);
    TagsDirty := False;
  end;

  Result := -1;

  Lo := 0;
  Hi := Length(Tags) - 1;

  while (Lo <= Hi) do
  begin
    Mid := (Lo + Hi) div 2;

    if (SameText(Tags[Mid].Name, Name)) then
    begin
      Result := Mid;
      Break;
    end;

    if (CompareText(string(Tags[Mid].Name), string(Name)) > 0) then
      Hi := Mid - 1
    else
      Lo := Mid + 1;
  end;
end;

// ---------------------------------------------------------------------------
procedure TAsphyreFonts.RemoveTag(const Name: ShortString);
begin
  DeleteTag(IndexOfTag(LowerCase(Name)));
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.FindFont(const Name: ShortString): TAsphyreFont;
var
  i: Integer;
begin
  Result := nil;
  if Length(Fonts) > 0 then
  begin
    for i := 0 to Length(Fonts) - 1 do
      if (CompareText(Fonts[i].FName, Name) = 0) then
      begin
        Result := Fonts[i];
        Exit;
      end;
  end;
end;

// ---------------------------------------------------------------------------
function TAsphyreFonts.FindTag(const Name: ShortString): PFontTag;
var
  Index: Integer;
begin
  Index := IndexOfTag(Name);

  if (Index <> -1) then
    Result := @Tags[Index]
  else
    Result := nil;
end;

// ---------------------------------------------------------------------------
end.
