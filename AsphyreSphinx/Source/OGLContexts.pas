unit OGLContexts;
//---------------------------------------------------------------------------
// OGLContexts.pas                                      Modified: 20-Feb-2009
// OpenGL rendering contexts                                      Version 1.0
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
// The Original Code is OGLContexts.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows;

//---------------------------------------------------------------------------
type
 POGLContext = ^TOGLContext;
 TOGLContext = record
  Handle : THandle;
  WinDC  : HDC;
  Palette: HPalette;
  Context: HGLRC;
 end;

//---------------------------------------------------------------------------
 TOGLContexts = class
 private
  Data: array of TOGLContext;
  SearchList : array of Integer;
  SearchDirty: Boolean;

  FColorBits: Integer;
  FDepthBits: Integer;

  function GetCount(): Integer;
  function GetItem(Index: Integer): POGLContext;
  procedure Release(Index: Integer);

  procedure InitSearchList();
  procedure SearchListSwap(Index1, Index2: Integer);
  function SearchListCompare(Index1, Index2: Integer): Integer;
  function SearchListSplit(Start, Stop: Integer): integer;
  procedure SearchListSort(Start, Stop: integer);
  procedure UpdateSearchList();

  function GetFormatDescriptor(): TPixelFormatDescriptor;
  function UpdatePixelFormat(DestDC: HDC): TPixelFormatDescriptor;
  function VerifyPalette(Index: Integer;
    const FormatDesc: TPixelFormatDescriptor): Boolean;
  function Prepare(Index: Integer; MainHandle: THandle): Boolean;
 public
  property ColorBits: Integer read FColorBits write FColorBits;
  property DepthBits: Integer read FDepthBits write FDepthBits;

  property Count: Integer read GetCount;
  property Items[Index: Integer]: POGLContext read GetItem; default;

  function Insert(Handle: THandle): Integer;
  function IndexOf(Handle: THandle): Integer;

  procedure Remove(Index: Integer);
  procedure RemoveAll();

  function Activate(SubHandle, MainHandle: THandle): Boolean;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors;

//---------------------------------------------------------------------------
constructor TOGLContexts.Create();
begin
 inherited;

 SearchDirty:= False;
 FColorBits:= 32;
 FDepthBits:= 32;
end;

//---------------------------------------------------------------------------
destructor TOGLContexts.Destroy();
begin
 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TOGLContexts.GetCount(): Integer;
begin
 Result:= Length(Data);
end;

//---------------------------------------------------------------------------
function TOGLContexts.GetItem(Index: Integer): POGLContext;
begin
 if (Index >= 0)and(Index < Length(Data)) then
  Result:= @Data[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.Release(Index: Integer);
begin
 if (Index < 0)or(Index >= Length(Data)) then Exit;

 if (Data[Index].Context <> 0) then
  begin
   if (wglGetCurrentContext() = Data[Index].Context) then wglMakeCurrent(0, 0);
   wglDeleteContext(Data[Index].Context);

   Data[Index].Context:= 0;
  end;

 if (Data[Index].Palette <> 0) then
  begin
   DeleteObject(Data[Index].Palette);
   Data[Index].Palette:= 0;
  end;

 if (Data[Index].WinDC <> 0) then
  begin
   ReleaseDC(Data[Index].Handle, Data[Index].WinDC);
   Data[Index].WinDC:= 0;
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Data)) then Exit;

 Release(Index);

 for i:= Index to Length(Data) - 2 do
  Data[i]:= Data[i + 1];

 SetLength(Data, Length(Data) - 1); 

 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.RemoveAll();
var
 i: Integer;
begin
 for i:= Length(Data) - 1 downto 0 do
  Release(i);

 SetLength(Data, 0);
 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
function TOGLContexts.Insert(Handle: THandle): Integer;
begin
 Result:= Length(Data);
 SetLength(Data, Result + 1);

 Data[Result].Handle:= Handle;
 Data[Result].WinDC := 0;
 Data[Result].Palette:= 0;
 Data[Result].Context:= 0;

 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.InitSearchList();
var
 i: Integer;
begin
 if (Length(SearchList) <> Length(Data)) then
  SetLength(SearchList, Length(Data));

 for i:= 0 to Length(SearchList) - 1 do
  SearchList[i]:= i;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.SearchListSwap(Index1, Index2: Integer);
var
 Aux: Integer;
begin
 Aux:= SearchList[Index1];

 SearchList[Index1]:= SearchList[Index2];
 SearchList[Index2]:= Aux;
end;

//---------------------------------------------------------------------------
function TOGLContexts.SearchListCompare(Index1, Index2: Integer): Integer;
begin
 Result:= 0;

 if (Data[Index1].Handle < Data[Index2].Handle) then Result:= -1;
 if (Data[Index1].Handle > Data[Index2].Handle) then Result:= 1;
end;

//---------------------------------------------------------------------------
function TOGLContexts.SearchListSplit(Start, Stop: Integer): integer;
var
 Left, Right, Pivot: Integer;
begin
 Left := Start + 1;
 Right:= Stop;
 Pivot:= SearchList[Start];

 while (Left <= Right) do
  begin
   while (Left <= Stop)and(SearchListCompare(SearchList[Left], Pivot) < 0) do
    Inc(Left);

   while (Right > Start)and(SearchListCompare(SearchList[Right], Pivot) >= 0) do
    Dec(Right);

   if (Left < Right) then SearchListSwap(Left, Right);
  end;

 SearchListSwap(Start, Right);

 Result:= Right;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.SearchListSort(Start, Stop: integer);
var
 SplitPt: integer;
begin
 if (Start < Stop) then
  begin
   SplitPt:= SearchListSplit(Start, Stop);

   SearchListSort(Start, SplitPt - 1);
   SearchListSort(SplitPt + 1, Stop);
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLContexts.UpdateSearchList();
begin
 InitSearchList();
 if (Length(SearchList) > 1) then SearchListSort(0, Length(SearchList) - 1);

 SearchDirty:= False;
end;

//---------------------------------------------------------------------------
function TOGLContexts.IndexOf(Handle: THandle): Integer;
var
 Lo, Hi, Mid: Integer;
begin
 if (SearchDirty) then UpdateSearchList();

 Result:= -1;

 Lo:= 0;
 Hi:= Length(SearchList) - 1;

 while (Lo <= Hi) do
  begin
   Mid:= (Lo + Hi) div 2;

   if (Data[SearchList[Mid]].Handle = Handle) then
    begin
     Result:= SearchList[Mid];
     Break;
    end;

   if (Data[SearchList[Mid]].Handle > Handle) then Hi:= Mid - 1
    else Lo:= Mid + 1;
 end;
end;

//---------------------------------------------------------------------------
function TOGLContexts.GetFormatDescriptor(): TPixelFormatDescriptor;
begin
 FillChar(Result, SizeOf(TPixelFormatDescriptor), 0);
 with Result do
  begin
   nSize     := SizeOf(TPixelFormatDescriptor);
   nVersion  := 1;
   dwFlags   := PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER;
   iPixelType:= PFD_TYPE_RGBA;
   cColorBits:= FColorBits;
   cDepthBits:= FDepthBits;
  end;
end;

//---------------------------------------------------------------------------
function TOGLContexts.UpdatePixelFormat(DestDC: HDC): TPixelFormatDescriptor;
var
 FormatIndex: Integer;
 FormatDesc : TPixelFormatDescriptor;
begin
 // Step 1. prepare some form of requested format
 FormatDesc:= GetFormatDescriptor();

 // Step 2. find a best format match
 FormatIndex:= ChoosePixelFormat(DestDC, @FormatDesc);

 // Step 3. update the format of the window
 SetPixelFormat(DestDC, FormatIndex, @FormatDesc);

 // Step 4. retreive information of updated format
 DescribePixelFormat(DestDC, FormatIndex, SizeOf(TPixelFormatDescriptor),
  FormatDesc);

 Result:= FormatDesc;
end;

//---------------------------------------------------------------------------
function TOGLContexts.VerifyPalette(Index: Integer;
 const FormatDesc: TPixelFormatDescriptor): Boolean;
var
 ColorCount: Integer;
 MyPal     : PLogPalette;
 RedMask   : Integer;
 GreenMask : Integer;
 BlueMask  : Integer;
 i         : Integer;
begin
 if ((FormatDesc.dwFlags and PFD_NEED_PALETTE) <> 0) then
  begin
   ColorCount:= 1 shl FormatDesc.cColorBits;
   MyPal:= AllocMem(SizeOf(TLogPalette) + (ColorCount * SizeOf(TPaletteEntry)));

   MyPal^.palVersion   := $300;
   MyPal^.palNumEntries:= ColorCount;

   RedMask  := (1 shl FormatDesc.cRedBits) - 1;
   GreenMask:= (1 shl FormatDesc.cGreenBits) - 1;
   BlueMask := (1 shl FormatDesc.cBlueBits) - 1;

   for i:= 0 to ColorCount - 1 do
    begin
     MyPal^.palPalEntry[i].peRed:=
      (((i shr FormatDesc.cRedShift) and RedMask) * 255) div RedMask;

     MyPal^.palPalEntry[i].peGreen:=
      (((i shr FormatDesc.cGreenShift) and GreenMask) * 255) div GreenMask;

     MyPal^.palPalEntry[i].peBlue:=
      (((i shr FormatDesc.cBlueShift) and BlueMask) * 255) div BlueMask;

     MyPal^.palPalEntry[i].peFlags:= 0;
    end;

   Data[Index].Palette:= CreatePalette(MyPal^);
   FreeMem(MyPal);

   Result:= Data[Index].Palette <> 0;
   if (not Result) then
    begin
     Errors.Insert(errCreateWindowPalette, Self, ClassName, 'VerifyPalette');
     Exit;
    end;

   SelectPalette(Data[Index].WinDC, Data[Index].Palette, False);
   RealizePalette(Data[Index].WinDC);
  end;

 Result:= True;
end;

//---------------------------------------------------------------------------
function TOGLContexts.Prepare(Index: Integer; MainHandle: THandle): Boolean;
var
 FormatDesc: TPixelFormatDescriptor;
 MainIndex : Integer;
begin
 // (1) Retreive DC from window handle.
 Data[Index].WinDC:= GetDC(Data[Index].Handle);
 Result:= Data[Index].WinDC <> 0;

 if (not Result) then
  begin
   Errors.Insert(errInvalidWindowHandle, Self, ClassName, 'Prepare');
   Exit;
  end;

 // (2) Prepare window's pixel format.
 FormatDesc:= UpdatePixelFormat(Data[Index].WinDC);

 Result:= VerifyPalette(Index, FormatDesc);
 if (not Result) then Exit;

 // (3) Create OpenGL rendering context.
 Data[Index].Context:= wglCreateContext(Data[Index].WinDC);

 Result:= Data[Index].Context <> 0;
 if (not Result) then Exit;

 // (4) Share resources with the new context.
 if (MainHandle <> 0)and(MainHandle <> Data[Index].Handle) then
  begin
   MainIndex:= IndexOf(MainHandle);
   if (MainIndex <> -1) then
    Result:= wglShareLists(Data[MainIndex].Context, Data[Index].Context);
  end;
end;

//---------------------------------------------------------------------------
function TOGLContexts.Activate(SubHandle, MainHandle: THandle): Boolean;
var
 Index: Integer;
begin
 Index:= IndexOf(SubHandle);
 if (Index = -1) then
  begin
   Index:= Insert(SubHandle);
   Result:= Prepare(Index, MainHandle);

   if (not Result) then
    begin
     Remove(Index);
     Exit;
    end;
  end;

 Result:= wglMakeCurrent(Data[Index].WinDC, Data[Index].Context);
end;

//---------------------------------------------------------------------------
end.
