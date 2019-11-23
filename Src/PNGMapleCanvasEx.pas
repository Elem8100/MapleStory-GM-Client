unit PNGMapleCanvasEx;

interface

uses
  Windows, Classes, SysUtils, WZReader, ZLIBex, Graphics, DX9Textures, PNGImage, AsphyreFactory,
  AsphyreTypes, System.Types, iexBitmaps, iesettings, hyiedefs, hyieutils, Math, ColorUtils;

type
  TBmpEx = class(TIEBitmap)
  public
    ID, Name: string;
  end;

  TPNGMapleCanvas = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FDataLength: Integer;
    FOffset: Int64;
    FFormat: Integer;
    FWZReader: TWZReader;
    function Parse1(Input: TMemoryStream): TDX9LockableTexture;
    function Parse2(Input: TMemoryStream): TDX9LockableTexture;
    function Parse513(Input: TMemoryStream): TDX9LockableTexture;
    function Parse517(Input: TMemoryStream): TDX9LockableTexture;
    function Parse1026(Input: TMemoryStream): TDX9LockableTexture;
    function Parse2050(Input: TMemoryStream): TDX9LockableTexture;
    function Parse1Bmp(Input: TMemoryStream): TBitmap;
    function Parse2Bmp(Input: TMemoryStream): TBitmap;
    function Parse1026Bmp(Input: TMemoryStream): TBitmap;
    function Parse2050Bmp(Input: TMemoryStream): TBitmap;
    function Parse1BmpEx(Input: TMemoryStream): TBmpEx;
    function Parse2BmpEx(Input: TMemoryStream): TBmpEx;
    function Parse1PNG(Input: TMemoryStream): TPngImage;
    function Parse2PNG(Input: TMemoryStream): TPngImage;
    function Parse513Png(Input: TMemoryStream): TPngImage;
    function Parse517Png(Input: TMemoryStream): TPngImage;
    function Parse1026Png(Input: TMemoryStream): TPngImage;
    function Parse2050Png(Input: TMemoryStream): TPngImage;
  public
    constructor Create(Width, Height, DataLength: Integer; Offset: Int64; Format: Integer; var WZReader: TWZReader);
    function Decompress: TMemoryStream;
    function Dump(ColorEffect: TColorEffect; Value: Integer): TDX9LockableTexture;
    function DumpBmp: TBitmap;
    function DumpBmpEx: TBmpEx;
    function DumpPNG: TPngImage;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    property Format: Integer read FFormat;
  end;

implementation

procedure DecompressDDS(RGBA: PByte; Width, Height: Integer; Blocks: PByte; Flags: Integer); stdcall;
  external 'libdds.dll' index 1;


{ TPNGMapleCanvas }

constructor TPNGMapleCanvas.Create(Width, Height, DataLength: Integer; Offset: Int64; Format:
  Integer; var WZReader: TWZReader);
begin
  FHeight := Height;
  FWidth := Width;
  FDataLength := DataLength;
  FOffset := Offset;
  FFormat := Format;
  FWZReader := WZReader;
end;

procedure StreamDecompression(mInputStream: TStream; var mOutputStream: TMemoryStream);
var
  Stream: TZDecompressionStream;
begin
  if not Assigned(mInputStream) then
  begin
    if Assigned(mOutputStream) then
      FreeAndNil(mOutputStream);
    Exit;
  end;

  if not Assigned(mOutputStream) then
    Exit;

  Stream := TZDecompressionStream.Create(mInputStream);
  try
    mOutputStream.CopyFrom(Stream, 0);
  finally
    Stream.Free;
  end;
end;

procedure StreamDecryption(var Stream: TMemoryStream; Key: TBytes);
var
  SubLen, i: Integer;
  Data: TBytes;
  New: TMemoryStream;
begin
  New := TMemoryStream.Create;
  try
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(SubLen, 4);
      SetLength(Data, SubLen);
      Stream.Read(Data[0], SubLen);

      for i := 0 to High(Data) do
        Data[i] := Data[i] xor Key[i];

      New.Write(Data[0], Length(Data));
    end;

    Stream.Clear;
    Stream.CopyFrom(New, 0);
  finally
    New.Free;
  end;
end;

function TPNGMapleCanvas.Decompress: TMemoryStream;
var
  vBuf: TMemoryStream;
begin
  // if (FFormat <> 1) and (FFormat <> 2) and (FFormat <> 513) and (FFormat <> 517) then
  // raise Exception.CreateFmt('Image format unsupported! [%d]', [FFormat]);

  Result := nil;

  vBuf := TMemoryStream.Create;

  FWZReader.Seek(FOffset, soBeginning);
  vBuf.CopyFrom(FWZReader.Stream, FDataLength);
  vBuf.Position := 0;

  Result := TMemoryStream.Create;
  try
    try
      StreamDecompression(vBuf, Result);
    except
      on EZDecompressionError do
      begin
        vBuf.Position := 0;
        StreamDecryption(vBuf, FWZReader.Key);
        StreamDecompression(vBuf, Result);
      end
      else
        FreeAndNil(Result);
    end;

    if not Assigned(Result) then
      Exit(nil);

    Result.Position := 0;
  finally
    vBuf.Free;
  end;
end;

function TPNGMapleCanvas.Dump(ColorEffect: TColorEffect; Value: Integer): TDX9LockableTexture;
var
  Decompressed: TMemoryStream;
  Texture: TDX9LockableTexture;
begin
  Result := nil;
  Decompressed := Decompress;
  try
    case FFormat of
      1:
        Texture := Parse1(Decompressed);
      2:
        Texture := Parse2(Decompressed);
      513:
        Texture := Parse513(Decompressed); // $201
      517:
        Texture := Parse517(Decompressed);
      1026:
        Texture := Parse1026(Decompressed);
      2050:
        Texture := Parse2050(Decompressed);
    end;
  finally
    Decompressed.Free;
  end;

  case ColorEffect of
    ceNone:
      begin
        Result := Texture;
      end;
    ceHue:
      begin
        HSVvar(Texture, Value, 1, 0);
        Result := Texture;
      end;
    ceSaturation:
      begin
        HSVvar(Texture, 0, Value, 0);
        Result := Texture;
      end;
    ceContrast1:
      begin
        Contrast3(Texture, 50, -90, True, False, False);
        Result := Texture;
      end;
    ceContrast2:
      begin
        Contrast3(Texture, 50, -90, False, True, False);
        Result := Texture;
      end;

    ceContrast3:
      begin
        Contrast3(Texture, 50, -90, False, False, True);
        Result := Texture;
      end;

    ceContrast4:
      begin
        Contrast3(Texture, 50, -90, True, True, False);
        Result := Texture;
      end;

    ceContrast5:
      begin
        Contrast3(Texture, 50, -90, True, False, True);
        Result := Texture;
      end;

    ceNegative:
      begin
        Negative(Texture);
        Result := Texture;
      end;

  end;
end;

function TPNGMapleCanvas.DumpBmp: TBitmap;
var
  Decompressed: TMemoryStream;
begin
  Result := nil;
  Decompressed := Decompress;
  try
    case FFormat of
      1:
        Result := Parse1Bmp(Decompressed);
      2:
        Result := Parse2Bmp(Decompressed);
      1026:
        Result := Parse1026Bmp(Decompressed); // $201
      2050:
        Result := Parse2050Bmp(Decompressed);
    end;
  finally
    Decompressed.Free;
  end;
end;

function TPNGMapleCanvas.DumpBmpEx: TBmpEx;
var
  Decompressed: TMemoryStream;
begin
  Result := nil;
  Decompressed := Decompress;
  try
    case FFormat of
      1:
        Result := Parse1BmpEx(Decompressed);
      2:
        Result := Parse2BmpEx(Decompressed);

    end;
  finally
    Decompressed.Free;
  end;
end;

function TPNGMapleCanvas.DumpPNG: TPngImage;
var
  Decompressed: TMemoryStream;
begin
  Result := nil;
  Decompressed := Decompress;
  try
    case FFormat of
      1:
        Result := Parse1PNG(Decompressed);
      2:
        Result := Parse2PNG(Decompressed);
      513:
        Result := Parse513Png(Decompressed); // $201
      517:
        Result := Parse517Png(Decompressed);
      1026:
        Result := Parse1026Png(Decompressed);
      2050:
        Result := Parse2050Png(Decompressed);
    end;
  finally
    Decompressed.Free;
  end;
end;

function TPNGMapleCanvas.Parse1(Input: TMemoryStream): TDX9LockableTexture;
var
  x, y: Integer;
  b1: array[0..1] of Byte;
  A, R, G, B: Word;
  P: PLongWord;
  pDest: Pointer;
  nPitch: Integer;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  P := pDest;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      Input.Read(b1[0], 2);
      B := b1[0] and 15;
      B := B or (B shl 4);

      G := b1[0] and 240;
      G := G or (G shr 4);

      R := b1[1] and 15;
      R := R or (R shl 4);

      A := b1[1] and 240;
      A := A or (A shr 4);

      P^ := cRGB1(R, G, B, A);
      Inc(P);
    end;
  end;
  Result.Unlock;

end;

function TPNGMapleCanvas.Parse2(Input: TMemoryStream): TDX9LockableTexture;
var
  x, y: Integer;
  b1, b2, b3, b4: Byte;
  A, R, G, B: Word;
  P: PLongWord;
  pDest: Pointer;
  nPitch: Integer;
  ARGB: PByte;
  bytes: array of PByte;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;
  // SetLength(bytes, Input.Size);

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  P := pDest;

  // for official wz--ultra fast
  {
   ARGB :=pDest;
   Input.Read(bytes[0], Input.Size);
   Move(bytes[0], ARGB[0], Input.Size);
  }

  // for custom-- fast
  {
    for y := 0 to FHeight - 1 do
    begin
    for x := 0 to FWidth - 1 do
    begin
    Input.Read(bytes[0], 4);
    P^ := Cardinal(bytes[0]);
    Inc(P);
    end;
    end;
  }
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);
      Input.Read(b3, 1);
      Input.Read(b4, 1);
      P^ := cRGB1(b3, b2, b1, b4);
      Inc(P);
    end;
  end;

  Result.Unlock;
end;

function TPNGMapleCanvas.Parse513(Input: TMemoryStream): TDX9LockableTexture;
var
  x, y: Integer;
  b1, b2: Byte;
  R, G, B, A: Word;
  P: PLongWord;
  pDest: Pointer;
  nPitch: Integer;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  P := pDest;
  for y := 0 to FHeight - 1 do
  begin

    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);
      B := (b1 and $1F) shl 3;
      B := B or (B shr 5);
      G := ((b2 and 7) shl 5) or ((b1 and $E0) shr 3);
      G := G or (G shr 6);
      R := b2 and $F8;
      R := R or (R shr 5);
      A := $FF;
      P^ := cRGB1(R, G, B, A);
      Inc(P);
    end;

  end;
  Result.Unlock;
end;

function TPNGMapleCanvas.Parse517(Input: TMemoryStream): TDX9LockableTexture;
var
  x, y: Integer;
  P: PLongWord;
  pDest: Pointer;
  nPitch: Integer;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  P := pDest;
  for y := 0 to FHeight - 1 do
  begin

    for x := 0 to FWidth - 1 do
    begin
      if FWidth = 128 then
        P^ := cRGB1(66, 159, 255, 255)
      else
        P^ := cRGB1(83, 134, 239, 255);
      Inc(P);
    end;

  end;
  Result.Unlock;
end;

function TPNGMapleCanvas.Parse1026(Input: TMemoryStream): TDX9LockableTexture;
var
  pDest: Pointer;
  nPitch: Integer;
  ARGB: PByte;
  bytes: array of PByte;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;

  SetLength(bytes, Input.Size);
  Input.Read(bytes[0], Input.Size);

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  ARGB := pDest;
  DecompressDDS(ARGB, Width, Height, PByte(bytes), 2);
  Result.Unlock;
end;

function TPNGMapleCanvas.Parse2050(Input: TMemoryStream): TDX9LockableTexture;
var
  pDest: Pointer;
  nPitch: Integer;
  ARGB: PByte;
  bytes: array of PByte;
begin
  Result := TDX9LockableTexture.Create;
  Result.Width := FWidth;
  Result.Height := FHeight;
  Result.Format := apf_A8R8G8B8;
  Result.Initialize;

  SetLength(bytes, Input.Size);
  Input.Read(bytes[0], Input.Size);

  Result.Lock(Rect(0, 0, FWidth, FHeight), pDest, nPitch);
  ARGB := pDest;
  DecompressDDS(ARGB, Width, Height, PByte(bytes), 4);
  Result.Unlock;
end;

function TPNGMapleCanvas.Parse1Bmp(Input: TMemoryStream): TBitmap;
var
  x, y: Integer;
  b1, b2: Byte;
  A, R, G, B: Word;
  Line: PRGB32Array;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.AlphaFormat := afPremultiplied;
  Result.Width := FWidth;
  Result.Height := FHeight;

  for y := 0 to FHeight - 1 do
  begin
    Line := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);

      B := b1 and 15;
      B := B or (B shl 4);
      G := b1 and 240;
      G := G or (G shr 4);
      R := b2 and 15;
      R := R or (R shl 4);
      A := b2 and 240;
      A := A or (A shr 4);
      Line[x].B := B;
      Line[x].G := G;
      Line[x].R := R;
      Line[x].A := A;
    end;
  end;
end;

function TPNGMapleCanvas.Parse2Bmp(Input: TMemoryStream): TBitmap;
var
  x, y: Integer;
  b1, b2, b3, b4: Byte;
  Line: PRGB32Array;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.AlphaFormat := afPremultiplied;
  Result.Width := FWidth;
  Result.Height := FHeight;
  for y := 0 to FHeight - 1 do
  begin
    Line := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);
      Input.Read(b3, 1);
      Input.Read(b4, 1);
      Line[x].A := b4;
      Line[x].R := b3;
      Line[x].G := b2;
      Line[x].B := b1;
    end;
  end;
end;

function TPNGMapleCanvas.Parse1BmpEx(Input: TMemoryStream): TBmpEx;
var
  x, y: Integer;
  b1, b2: Byte;
  A, R, G, B: Word;
  Line: PRGB32Array;
begin
  Result := TBmpEx.Create;
  Result.PixelFormat := ie32RGB;
  Result.Width := FWidth;
  Result.Height := FHeight;

  for y := 0 to FHeight - 1 do
  begin
    Line := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);

      B := b1 and 15;
      B := B or (B shl 4);
      G := b1 and 240;
      G := G or (G shr 4);
      R := b2 and 15;
      R := R or (R shl 4);
      A := b2 and 240;
      A := A or (A shr 4);
      Line[x].B := B;
      Line[x].G := G;
      Line[x].R := R;
    // Line[x].A := A;
      Result.Alpha[x, y] := A;
    end;
  end;
end;

function TPNGMapleCanvas.Parse2BmpEx(Input: TMemoryStream): TBmpEx;
var
  x, y: Integer;
  b1, b2, b3, b4: Byte;
  Line: PRGB32Array;
begin
  Result := TBmpEx.Create;
  Result.PixelFormat := ie32RGB;

  Result.Width := FWidth;
  Result.Height := FHeight;
  for y := 0 to FHeight - 1 do
  begin
    Line := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);
      Input.Read(b3, 1);
      Input.Read(b4, 1);
     // Line[x].A := b4;
      Result.Alpha[x, y] := b4;
      Line[x].R := b3;
      Line[x].G := b2;
      Line[x].B := b1;
    end;
  end;
end;

function TPNGMapleCanvas.Parse1026Bmp(Input: TMemoryStream): TBitmap;
var
  ARGB: PByte;
  bytes: array of PByte;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.AlphaFormat := afPremultiplied;
  Result.Width := FWidth;
  Result.Height := -FHeight;
  SetLength(bytes, Input.Size);

  ARGB := Result.Scanline[Result.Height - 1];
  // index := -1;
  {
    for y := 0 to FHeight - 1 do
    begin
    if y mod 4 = 0 then
    begin
    for x := 0 to FWidth - 1 do
    begin
    begin
    Inc(Index);
    Input.Read(bytes[Index], 4);

    end;
    end;
    end;
    end;
  }
  Input.Read(bytes[0], Input.Size);

  DecompressDDS(ARGB, Width, Height, PByte(bytes), 2);

end;

function TPNGMapleCanvas.Parse2050Bmp(Input: TMemoryStream): TBitmap;
var
  ARGB: PByte;
  bytes: array of PByte;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.AlphaFormat := afPremultiplied;
  Result.Width := FWidth;
  Result.Height := -FHeight;
  SetLength(bytes, Input.Size);

  ARGB := Result.Scanline[Result.Height - 1];
  // index := -1;
  {
    for y := 0 to FHeight - 1 do
    begin
    if y mod 4 = 0 then
    begin
    for x := 0 to FWidth - 1 do
    begin
    begin
    Inc(Index);
    Input.Read(bytes[Index], 4);

    end;
    end;
    end;
    end;
  }
  Input.Read(bytes[0], Input.Size);

  // DXTDecompressImageFlip(ARGB, Width, Height, Pbyte(Bytes), 4);
  DecompressDDS(ARGB, Width, Height, PByte(bytes), 4);
end;

function TPNGMapleCanvas.Parse1PNG(Input: TMemoryStream): TPngImage;
var
  x, y: Integer;
  b1, b2: Byte;
  A, R, G, B: Word;
  RGBLine: pRGBLine;
  AlphaLine: PByteArray;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    RGBLine := Result.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);

      B := b1 and 15;
      B := B or (B shl 4);
      G := b1 and 240;
      G := G or (G shr 4);
      R := b2 and 15;
      R := R or (R shl 4);
      A := b2 and 240;
      A := A or (A shr 4);

      RGBLine[x].rgbtBlue := B;
      RGBLine[x].rgbtGreen := G;
      RGBLine[x].rgbtRed := R;
      AlphaLine[x] := A;
    end;
  end;
end;

function TPNGMapleCanvas.Parse2PNG(Input: TMemoryStream): TPngImage;
var
  x, y: Integer;
  RGBLine: pRGBLine;
  AlphaLine: PByteArray;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    RGBLine := Result.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(RGBLine[x], 3);
      Input.Read(AlphaLine[x], 1);
    end;
  end;
end;

function TPNGMapleCanvas.Parse513Png(Input: TMemoryStream): TPngImage;
var
  x, y: Integer;
  b1, b2: Byte;
  R, G, B: Word;
  RGBLine: pRGBLine;
  AlphaLine: PByteArray;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    RGBLine := Result.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);

      B := (b1 and $1F) shl 3;
      RGBLine[x].rgbtBlue := B or (B shr 5);
      G := ((b2 and 7) shl 5) or ((b1 and $E0) shr 3);
      RGBLine[x].rgbtGreen := G or (G shr 6);
      R := b2 and $F8;
      RGBLine[x].rgbtRed := R or (R shr 5);

      AlphaLine[x] := $FF;
    end;
  end;
end;

function TPNGMapleCanvas.Parse517Png(Input: TMemoryStream): TPngImage;
var
  j, k, x, y: Integer;
  B, Col: Byte;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  x := 0;
  y := 0;
  while Input.Position < Input.Size do
  begin
    Input.Read(B, 1);
    for j := 0 to 7 do
    begin
      Col := ((B and (1 shl (7 - j))) shr (7 - j)) * $FF;
      for k := 0 to 15 do
      begin
        if x = FWidth then
        begin
          x := 0;
          Inc(y);
        end;
        Result.Pixels[x, y] := RGB(Col, Col, Col);
        Result.AlphaScanline[y][x] := $FF;
        Inc(x);
      end;
    end;
  end;
end;

function TPNGMapleCanvas.Parse1026Png(Input: TMemoryStream): TPngImage;
var
  ARGB: PRGB32Array;
  Bytes: array of PByte;
  Bmp: TBitmap;
  x, y: Integer;
  RGBLine: pRGBLine;
  AlphaLine: PByteArray;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.AlphaFormat := afPremultiplied;
  Bmp.Width := FWidth;
  Bmp.Height := FHeight;
  SetLength(Bytes, Input.Size);

  ARGB := Bmp.Scanline[Bmp.Height - 1];

  Input.Read(Bytes[0], Input.Size);
  DecompressDDS(PByte(ARGB), Width, Height, PByte(Bytes), 2);

  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    ARGB := Bmp.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    RGBLine := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      RGBLine[x].rgbtBlue := ARGB[x].B;
      RGBLine[x].rgbtGreen := ARGB[x].G;
      RGBLine[x].rgbtRed := ARGB[x].R;
      AlphaLine[x] := ARGB[x].A;
    end;
  end;

  Bmp.Free;

end;

function TPNGMapleCanvas.Parse2050Png(Input: TMemoryStream): TPngImage;
var
  ARGB: PRGB32Array;
  Bytes: array of PByte;
  Bmp: TBitmap;
  x, y: Integer;
  RGBLine: pRGBLine;
  AlphaLine: PByteArray;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.AlphaFormat := afPremultiplied;
  Bmp.Width := FWidth;
  Bmp.Height := FHeight;
  SetLength(Bytes, Input.Size);

  ARGB := Bmp.Scanline[Bmp.Height - 1];

  Input.Read(Bytes[0], Input.Size);
  DecompressDDS(PByte(ARGB), Width, Height, PByte(Bytes), 4);

  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    ARGB := Bmp.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    RGBLine := Result.Scanline[y];
    for x := 0 to FWidth - 1 do
    begin
      RGBLine[x].rgbtBlue := ARGB[x].B;
      RGBLine[x].rgbtGreen := ARGB[x].G;
      RGBLine[x].rgbtRed := ARGB[x].R;
      AlphaLine[x] := ARGB[x].A;
    end;
  end;

  Bmp.Free;

end;

end.

