unit CommonUtils;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, Classes, SysUtils, Math;

//---------------------------------------------------------------------------
// Returns the next power of two of the specified value.
//---------------------------------------------------------------------------
function NextPowerOfTwo(Value: Integer): Integer;

//---------------------------------------------------------------------------
// The routines 'IsPowerOfTwo', 'CeilPowerOfTwo' and 'FloorPowerOfTwo' are
// converted from published code on FlipCode.com by Sebastian Schuberth.
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// Determines whether the specified value is a power of two.
//---------------------------------------------------------------------------
function IsPowerOfTwo(Value: Integer): Boolean;

//---------------------------------------------------------------------------
// The least power of two greater than or equal to the specified value.
// Note that for Value = 0 and for Value > 2147483648 the result is 0.
//---------------------------------------------------------------------------
function CeilPowerOfTwo(Value: Integer): Integer;

//---------------------------------------------------------------------------
// The greatest power of two less than or equal to the specified value.
// Note that for Value = 0 the result is 0.
//---------------------------------------------------------------------------
function FloorPowerOfTwo(Value: Integer): Integer;

//---------------------------------------------------------------------------
// Retreives a string representing quanity in kibibytes or mebibytes.
//---------------------------------------------------------------------------
function SizeToStr(Value: Integer): string;

//---------------------------------------------------------------------------
// Retreives the size of the specified file in bytes.
//---------------------------------------------------------------------------
function FileSize(Filename: string): Integer;

//---------------------------------------------------------------------------
// Determines the total size of bytes of the specified file list.
//---------------------------------------------------------------------------
function SizeOfFiles(List: TStrings): Integer;

//---------------------------------------------------------------------------
// Determines how many pixels are wasted if the specific number of patterns
// is placed onto one or several textures with the given size.
//---------------------------------------------------------------------------
function TexWastedSpace(const PatternSize, TextureSize: TPoint;
 PatternCount: Integer; out TextureCount: Integer): Integer;

//---------------------------------------------------------------------------
// Attempts to find the best texture size to match the specified pattern
// amount and dimensions. The heuristics try to optimize for performance
// first, then minimize waste and finally, gives preference to textures
// with less anisotropy (likes square textures).
//---------------------------------------------------------------------------
procedure BestTexSize(const PatternSize: TPoint; PatternCount: Integer;
 out TextureSize: TPoint);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function NextPowerOfTwo(Value: Integer): Integer;
begin
 Result:= 1;
 asm
  xor ecx, ecx
  bsr ecx, Value
  inc ecx
  shl Result, cl
 end;
end;

//---------------------------------------------------------------------------
function IsPowerOfTwo(Value: Integer): Boolean;
begin
 Result:= (Value >= 1)and((Value and (Value - 1)) = 0);
end;

//---------------------------------------------------------------------------
function CeilPowerOfTwo(Value: Integer): Integer; register;
asm
 mov ecx, eax
 xor eax, eax
 dec ecx
 bsr ecx, ecx
 cmovz ecx, eax
 setnz al
 inc eax
 shl eax, cl
end;

//---------------------------------------------------------------------------
function FloorPowerOfTwo(Value: Integer): Integer;
asm
 xor eax, eax
 bsr ecx, ecx
 setnz al
 shl eax, cl
end;

//---------------------------------------------------------------------------
function SizeToStr(Value: Integer): string;
var
 v: Real;
begin
 if (Value < 1024) then
  begin
   Result:= IntToStr(Value) + ' bytes';
   Exit;
  end;

 v:= Value / 1024.0;
 if (v < 8192) then
  begin
   Result:= FormatFloat('#,#.#', v) + ' KiB';
   Exit;
  end;

 v:= v / 1024.0;
 Result:= FormatFloat('#,#.#', v) + ' MiB';
end;

//---------------------------------------------------------------------------
function FileSize(Filename: string): Integer;
var
 Stream: TFileStream;
begin
 Result:= 0;

 try
  Stream:= TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
 except
  Exit;
 end;

 try
  Result:= Stream.Size;
 finally
  Stream.Free();
 end;
end;

//---------------------------------------------------------------------------
function SizeOfFiles(List: TStrings): Integer;
var
 i, Size: Integer;
begin
 Size:= 0;

 for i:= 0 to List.Count - 1 do
  Inc(Size, FileSize(List[i]));

 Result:= Size; 
end;

//---------------------------------------------------------------------------
function TexWastedSpace(const PatternSize, TextureSize: TPoint;
 PatternCount: Integer; out TextureCount: Integer): Integer;
var
 UsedWidth        : Integer;
 UsedHeight       : Integer;
 AvailablePatterns: Integer;
 PatternsInTexture: Integer;
 Wasted           : Integer;
 WastedWidth      : Integer;
 WastedHeight     : Integer;
 WastedCorner     : Integer;
 WastedPatterns   : Integer;
begin
 UsedWidth := (TextureSize.X div PatternSize.X) * PatternSize.X;
 UsedHeight:= (TextureSize.Y div PatternSize.Y) * PatternSize.Y;

 // how many patterns fit in one texture?
 PatternsInTexture:= ((TextureSize.X div PatternSize.X) *
  (TextureSize.Y div PatternSize.Y));

 // how many textures are needed to hold all patterns
 if (PatternsInTexture > 0) then
  TextureCount:= Ceil(PatternCount / PatternsInTexture)
   else TextureCount:= 0;

 // patterns in these textures
 AvailablePatterns:= TextureCount * PatternsInTexture;

 WastedWidth   := (TextureSize.X - UsedWidth) * TextureSize.Y;
 WastedHeight  := (TextureSize.Y - UsedHeight) * TextureSize.X;
 WastedCorner  := (TextureSize.X - UsedWidth) * (TextureSize.Y - UsedHeight);
 WastedPatterns:= (AvailablePatterns - PatternCount) * PatternSize.X * PatternSize.Y;

 // space wasted in ONE texture
 Wasted:= WastedWidth + WastedHeight - WastedCorner;

 // space wasted in ALL textures and missing patterns
 Result:= (Wasted * TextureCount) + WastedPatterns;
end;

//---------------------------------------------------------------------------
procedure BestTexSize(const PatternSize: TPoint; PatternCount: Integer;
 out TextureSize: TPoint);
const
 MaxSize: TPoint = (X: 512; Y: 512);
var
 MinSize: TPoint;
 Wasted : Integer;
 Attempt: TPoint;

 TexCount: Integer;
 NewCount: Integer;
 NewWaste: Integer;
 Delta   : Integer;
 NewDelta: Integer;
begin
 // (1) Minimal texture size.
 MinSize:= PatternSize;
 if (not IsPowerOfTwo(MinSize.X)) then
  MinSize.X:= NextPowerOfTwo(PatternSize.X);

 if (not IsPowerOfTwo(MinSize.Y)) then
  MinSize.Y:= NextPowerOfTwo(PatternSize.Y);

 // (2) If it's not within limits -> just use the minimal size.
 if (MinSize.X > MaxSize.X)or(MinSize.Y > MaxSize.Y) then
  begin
   TextureSize:= MinSize;
   Exit;
  end;

 // (3) Assume we are using maximum texture size.
 Wasted:= TexWastedSpace(PatternSize, MaxSize, PatternCount, TexCount);
 TextureSize:= MaxSize;
 Delta:= Abs(MaxSize.X - MaxSize.Y);

 Attempt.Y:= MaxSize.Y;
 while (Attempt.Y >= MinSize.Y) do
  begin
   Attempt.X:= MaxSize.X;
   while (Attempt.X >= MinSize.X) do
    begin
     NewWaste:= TexWastedSpace(PatternSize, Attempt, PatternCount, NewCount);
     NewDelta:= Abs(Attempt.X - Attempt.Y);
     if (NewCount < TexCount)or((NewCount = TexCount)and(NewWaste < Wasted))or
      ((NewCount = TexCount)and(NewWaste = Wasted)and(NewDelta < Delta)) then
      begin
       TextureSize:= Attempt;
       Wasted  := NewWaste;
       TexCount:= NewCount;
       Delta   := NewDelta;
      end;

     Attempt.X:= Attempt.X div 2;
    end;

   Attempt.Y:= Attempt.Y div 2;
  end;
end;

//---------------------------------------------------------------------------
end.
