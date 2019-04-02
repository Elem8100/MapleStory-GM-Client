unit NativeTGA;
//---------------------------------------------------------------------------
// NativeTGA.pas                                         Modified: 4-Nov-2007
// Truevision TARGA format support for Asphyre                    Version 1.1
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
// The Original Code is NativeTGA.pas.
//
// The Initial Developer of the Original Code is M. Sc. Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007,
// M. Sc. Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TTGAFlag  = (tfMirrored, tfFlipped, tfCompressed);
 TTGAFlags = set of TTGAFlag;

//---------------------------------------------------------------------------
// LoadTGAtoSystem()
//
// Loads 24-bit or 32-bit Truevision TARGA file from stream and stores all
// information in the destination surface.
//---------------------------------------------------------------------------
function LoadTGAtoSystem(Stream: TStream;
 Dest: TSystemSurface): Boolean; overload;

//---------------------------------------------------------------------------
// SaveSystemToTGA()
//
// Saves 24-bit or 32-bit bitmap as Truevision TARGA file to stream.
//---------------------------------------------------------------------------
function SaveSystemToTGA(Stream: TStream; Source: TSystemSurface;
 Flags: TTGAFlags): Boolean; overload;

//---------------------------------------------------------------------------
// Overloaded functions to save/load TGAs to/from external files.
//---------------------------------------------------------------------------
function LoadTGAtoSystem(const FileName: string;
 Dest: TSystemSurface): Boolean; overload;
function SaveSystemToTGA(const FileName: string;
 Source: TSystemSurface; Flags: TTGAFlags): Boolean; overload;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
type
 TTGAHeader = packed record
  tfIDLength     : Byte;
  tfColorMapType : Byte;
  tfImageType    : Byte;
  tfColorMapSpec : packed array[0..4] of Byte;
  tfOrigX        : Word;
  tfOrigY        : Word;
  tfWidth        : Word;
  tfHeight       : Word;
  tfBpp          : Byte;
  tfImageDesc    : Byte;
 end;

//---------------------------------------------------------------------------
procedure DecodeTargaRLE(Stream: TStream; Dest: Pointer; DestSize,
 tgaBpp: Integer);
var
 Write: Pointer;
 Count, bSize: Integer;
 RLEHeader,
 BlockLength: Byte;
 RLEBuf: Longword;
begin
 // bytes to write
 Count:= DestSize;
 // pointer to destination
 Write:= Dest;

 // read pixels
 while (Count > 0) do
  begin
   // read the RLE header
   Stream.ReadBuffer(RLEHeader, SizeOf(RLEHeader));
   // RLE Block length
   BlockLength:= (RLEHeader and $7F) + 1;
   if (RLEHeader and $80) = $80 then
    begin
     // if highest bit is set, the read one pixel and repeat it BlockLength times
     Stream.ReadBuffer(RLEBuf, tgaBpp);
     // write BlockLength pixels of RLEBuf
     while (BlockLength > 0) do
      begin
       Move(RLEBuf, Write^, tgaBpp); // repeat the pixel, one at a time
       Inc(Integer(Write), tgaBpp);
       Dec(Count, tgaBpp);
       Dec(BlockLength);
      end;
    end else
    begin
     // size of scanline to read
     bSize:= Integer(BlockLength) * tgaBpp;
     // read BlockLength pixels
     Stream.ReadBuffer(Write^, bSize);
     // increment destination pointer
     Inc(Integer(Write), bSize);
     // decrement the remaining byte count
     Dec(Count, bSize);
    end; // if RLEHeader
  end; // while
end;

//---------------------------------------------------------------------------
procedure Flip(Image: TSystemSurface);
var
 i, j: Integer;
 ScanBuf: Pointer;
 MyPitch: Integer;
begin
 MyPitch:= Image.Pitch;
 if (MyPitch < 1) then Exit;

 GetMem(ScanBuf, MyPitch);

 for i:= 0 to (Image.Height div 2) - 1 do
  begin
   j:= (Image.Height - 1) - i;

   Move(Image.Scanline[i]^, ScanBuf^, MyPitch);
   Move(Image.Scanline[j]^, Image.Scanline[i]^, MyPitch);
   Move(ScanBuf^, Image.Scanline[j]^, MyPitch);
  end;

 FreeMem(ScanBuf);
end;

//---------------------------------------------------------------------------
procedure Mirror(Image: TSystemSurface);
var
 i, j: Integer;
 ScanBuf, Dest, Source: Pointer;
 MyPitch: Integer;
begin
 MyPitch:= Image.Pitch;
 if (MyPitch < 1) then Exit;

 GetMem(ScanBuf, MyPitch);

 for j:= 0 to Image.Height - 1 do
  begin
   Move(Image.Scanline[j]^, ScanBuf^, MyPitch);

   Dest:= ScanBuf;
   Source:= Pointer(Cardinal(Image.Scanline[j]) + (MyPitch - 4));

   for i:= 0 to Image.Width - 1 do
    begin
     Longword(Dest^):= Longword(Source^);
     Dec(Integer(Source), 4);
     Inc(Integer(Dest), 4);
    end;

   Move(ScanBuf^, Image.Scanline[j]^, MyPitch);
  end;

 FreeMem(ScanBuf);
end;

//---------------------------------------------------------------------------
function LoadTGAtoSystem(Stream: TStream; Dest: TSystemSurface): Boolean;
var
 tgaHeader: TTGAHeader;
 tgaBpp, i, j: Integer;
 BufSize, ScanLength: Integer;
 PixBuffer, Read: Pointer;
 InPixel : Pointer;
 OutPixel: PCardinal;
 InData  : Cardinal;
begin
 Result:= False;
 PixBuffer:= nil;

 try
  Stream.ReadBuffer(tgaHeader, SizeOf(TTGAHeader));

  // check if the image is either True-Color or RLE encoded
  if (tgaHeader.tfImageType <> 2)and(tgaHeader.tfImageType <> 10) then Exit;
  if (tgaHeader.tfColorMapType <> 0) then Exit;

  tgaBpp:= tgaHeader.tfBpp;
  if (tgaBpp <> 32)and(tgaBpp <> 24) then Exit;

  // skip Image ID field
  if (tgaHeader.tfIDLength <> 0) then
   Stream.Seek(tgaHeader.tfIDLength, soFromCurrent);

  BufSize:= Integer(tgaHeader.tfWidth) * tgaHeader.tfHeight * (tgaBpp div 8);
  PixBuffer:= AllocMem(BufSize);

  if (tgaHeader.tfImageType <> 10) then
   begin // raw pixel data
    Stream.ReadBuffer(pixBuffer^, bufSize);
   end else
   begin // RLE-encoded
    DecodeTargaRLE(Stream, pixBuffer, bufSize, tgaBpp div 8);
   end;
 except
  if (PixBuffer <> nil) then FreeMem(PixBuffer);
  Exit;
 end;

 Dest.SetSize(tgaHeader.tfWidth, tgaHeader.tfHeight);

 Read:= pixBuffer;
 ScanLength:= Dest.Width * (tgaBpp div 8);

 if (tgaBpp = 32) then
  begin // 32-bit image
   for i:= 0 to Dest.Height - 1 do
    begin
     Move(Read^, Dest.Scanline[i]^, ScanLength);
     Inc(Integer(Read), ScanLength);
    end;
  end else
  begin // 24-bit image
   InData:= 0;

   for j:= 0 to Dest.Height - 1 do
    begin
     InPixel := Read;
     InData  := 0;
     OutPixel:= Dest.Scanline[j];

     for i:= 0 to Dest.Width - 1 do
      begin
       Move(InPixel^, InData, 3);
       OutPixel^:= InData or $FF000000;

       Inc(Integer(InPixel), 3);
       Inc(OutPixel);
      end;

     Inc(Integer(Read), ScanLength);
    end;
  end;

 if (tgaHeader.tfImageDesc and $10 = $10) then Mirror(Dest);
 if (tgaHeader.tfImageDesc and $20 <> $20) then Flip(Dest);

 FreeMem(PixBuffer);
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure ScanRLE(Data: Pointer; PixRemain, iBpp: Integer;
 out PixCount: Integer; out DoRepeat: Boolean);
var
 Pixels: array[0..2] of Longword;
 nPixel: Longword;
 i: Integer;
begin
 // case 0: less than 3 pixels to write
 if (PixRemain < 3) then
  begin
   PixCount:= PixRemain;
   DoRepeat:= False;
   Exit;
  end;
 // read next 3 pixels
 for i:= 0 to 2 do
  begin
   Pixels[i]:= 0;
   Move(Pointer(Cardinal(Data) + (iBpp * i))^, Pixels[i], iBpp);
  end;
 // case 1: repeating pixels
 nPixel:= 0;
 if (Pixels[0] = Pixels[1])and(Pixels[1] = Pixels[2]) then
  begin
   PixCount:= 3;
   nPixel:= Pixels[0];
   while (PixCount < PixRemain)and(PixCount < $80)and(nPixel = Pixels[0]) do
    begin
     // increment repeated pixel count
     Inc(PixCount);
     Move(Pointer(Cardinal(Data) + (iBpp * PixCount))^, nPixel, iBpp);
    end;
   DoRepeat:= True;
   Exit;
  end;
 // case 2: non-repeating pixels
 PixCount:= 2;
 while (PixCount < PixRemain - 1)and(PixCount < $80) do
  begin
   // read next 3 pixels
   for i:= 0 to 2 do
    begin
     Pixels[i]:= 0;
     Move(Pointer(Cardinal(Data) + (iBpp * (i + PixCount)))^, Pixels[i], iBpp);
    end;
   // check if the pixels are different
   if (Pixels[0] = Pixels[1])and(Pixels[1] = Pixels[2]) then Break
    else Inc(PixCount);
  end;
 DoRepeat:= False;
end;

//---------------------------------------------------------------------------
procedure EncodeTargaRLE(Stream: TStream; Source: Pointer;
 SourceSize, tgaBpp: Integer);
var
 Read: Pointer;
 Count, bSize: Integer;
 RLEHeader: Byte;
 PixCount: Integer;
 DoRepeat: Boolean;
begin
 // bytes to read
 Count:= SourceSize;
 // pointer to source
 Read:= Source;

 // write pixels
 while (Count > 0) do
  begin
   // scan repeating pixels
   ScanRLE(Read, Count div tgaBpp, tgaBpp, PixCount, DoRepeat);
   // calculate scanline size
   bSize:= PixCount * tgaBpp;
   // set # of pixels
   RLEHeader:= (PixCount - 1) and $7F;
   if (DoRepeat) then
    begin
     // update RLE header
     RLEHeader:= RLEHeader or $80; // set RLE bit
     // write updated RLE header
     Stream.WriteBuffer(RLEHeader, SizeOf(RLEHeader));
     // write the repeating pixel data
     Stream.WriteBuffer(Read^, tgaBpp);
    end else
    begin
     // write RLE header
     Stream.WriteBuffer(RLEHeader, SizeOf(RLEHeader));
     // write pixel data
     Stream.WriteBuffer(Read^, bSize);
    end;

   // increment source pointer by number of scanned pixels
   Inc(Integer(Read), bSize);
   // decrement bytes remaining
   Dec(Count, bSize);
  end; // while
end;

//---------------------------------------------------------------------------
function SaveSystemToTGA(Stream: TStream; Source: TSystemSurface;
 Flags: TTGAFlags): Boolean;
var
 tgaHeader: TTGAHeader;
 tgaBpp, i: Integer;
 BufSize, ScanLength: Integer;
 PixBuffer, Write: Pointer;
begin
 tgaBpp:= 32;

 // create pixel buffer
 BufSize:= Source.Width * Source.Height * (tgaBpp div 8);
 GetMem(PixBuffer, BufSize);

 // source pointer
 Write:= PixBuffer;
 // scanline width
 ScanLength:= Source.Width * (tgaBpp div 8);

 // apply flip & mirror attributes
 if (tfFlipped in Flags) then Flip(Source);
 if (tfMirrored in Flags) then Mirror(Source);

 // set pixel data
 for i:= 0 to Source.Height - 1 do
  begin
   Move(Source.Scanline[i]^, Write^, ScanLength);
   Inc(Integer(Write), ScanLength);
  end;

 // return image to normal state
 if (tfFlipped in Flags) then Flip(Source);
 if (tfMirrored in Flags) then Mirror(Source);

 // clear TARGA header
 FillChar(tgaHeader, SizeOf(TTGAHeader), 0);

 // create new TARGA header
 tgaHeader.tfImageType:= 2;    // True-color
 if (tfCompressed in Flags) then tgaHeader.tfImageType:= 10;    // RLE-encoded

 // set flip & mirror attributes
 tgaHeader.tfImageDesc:= $00;   // the image is flipped
 // mirrored
 if (tfFlipped in Flags) then
  tgaHeader.tfImageDesc:= tgaHeader.tfImageDesc or $20;
 // flipped
 if (tfMirrored in Flags) then
  tgaHeader.tfImageDesc:= tgaHeader.tfImageDesc or $10;

 tgaHeader.tfColorMapType := 0;     // no colormapping
 tgaHeader.tfWidth        := Source.Width; // image width
 tgaHeader.tfHeight       := Source.Height;// image height
 tgaHeader.tfBpp          := tgaBpp;// image bit-depth

 Result:= True;
 try
  // write new TARGA header
  Stream.WriteBuffer(tgaHeader, SizeOf(TTGAHeader));

  // encode pixel data
  if (tfCompressed in Flags) then
   begin
    EncodeTargaRLE(Stream, pixBuffer, bufSize, tgaBpp div 8);
   end else
   begin
    Stream.WriteBuffer(pixBuffer^, bufSize);
   end;
 except
  Result:= False;
 end; 

 // release the buffer memory and reading stream
 FreeMem(PixBuffer);
end;

//---------------------------------------------------------------------------
function LoadTGAtoSystem(const FileName: string;
 Dest: TSystemSurface): Boolean; overload;
var
 Stream: TStream;
begin
 try
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
 except
  Result:= False;
  Exit;
 end;

 try
  Result:= LoadTGAtoSystem(Stream, Dest);
 finally
  Stream.Free();
 end;
end;

//---------------------------------------------------------------------------
function SaveSystemtoTGA(const FileName: string; Source: TSystemSurface;
 Flags: TTGAFlags): Boolean;
var
 Stream: TStream;
begin
 try
  Stream:= TFileStream.Create(FileName, fmCreate or fmShareExclusive);
 except
  Result:= False;
  Exit;
 end;

 try
  Result:= SaveSystemtoTGA(Stream, Source, Flags);
 finally
  Stream.Free();
 end;
end;


//---------------------------------------------------------------------------
end.
