unit DecodeDXT;

interface

procedure DXTDecompressImage(ARGB: PByte; Width, Height: Integer; Blocks: PByte; Flags: Integer);
procedure DXTDecompressImageFlip(ARGB: PByte; Width, Height: Integer; Blocks: PByte; Flags: Integer);

implementation

const
  kDXT1 = 1 shl 0;
  kDXT3 = 1 shl 1;
  kDXT5 = 1 shl 2;
  kColourClusterFit = 1 shl 3;
  kColourRangeFit = 1 shl 4;
  kColourMetricPerceptual = 1 shl 5;
  kColourMetricUniform = 1 shl 6;
  kWeightColourByAlpha = 1 shl 7;
  kColourIterativeClusterFit = 1 shl 8;

procedure DXTAlphaDXT3(ARGB, Block: PByte);
var
  Bytes: PByte;
  quant, Lo, Hi: Byte;
  i: Integer;
begin
  Bytes := Block;
  for i := 0 to 7 do
  begin
    quant := Bytes[i];
    Lo := quant and $0F;
    Hi := quant and $F0;
    ARGB[8 * i + 3] := Lo or (Lo shl 4);
    ARGB[8 * i + 7] := Hi or (Hi shr 4);
  end;
end;

procedure DXTAlphaDXT5(ARGB, Block: PByte);
var
  Bytes, Src, Dest: PByte;
  Codes: array of Byte; // 8
  Indices: array of Byte; // 16
  i, j, Alpha0, Alpha1, Value, Byte_, Index: Integer;
begin
  Bytes := Block;
  Alpha0 := Bytes[0];
  Alpha1 := Bytes[1];
  Src := Bytes + 2;
  SetLength(Codes, 8);
  SetLength(Indices, 16);

  Dest := PByte(Indices);

  Codes[0] := Alpha0;
  Codes[1] := Alpha1;

  if (Alpha0 <= Alpha1) then
  begin
    for i := 0 to 4 do
      Codes[1 + i] := ((5 - i) * Alpha0 + i * Alpha1) div 5;

    Codes[6] := 0;
    Codes[7] := 255;
  end
  else
  begin
    for i := 0 to 6 do
      Codes[1 + i] := ((7 - i) * Alpha0 + i * Alpha1) div 7;
  end;

  for i := 0 to 1 do
  begin
    Value := 0;
    for j := 0 to 2 do
    begin
      Byte_ := Src^;
      Inc(Src);

      Value := Value or (Byte_ shl 8 * j);
    end;

    for j := 0 to 7 do
    begin
      Index := (Value shr 3 * j) and $7;
      Dest^ := Index;
      Inc(Dest);
    end;

  end;

  for i := 0 to 15 do
    ARGB[4 * i + 3] := Codes[Indices[i]];
end;

function DXTUnpack565(Packe, Colour: PByte): Integer;
var
  Value: Integer;
  Red, Green, Blue: Byte;
begin
  Value := Packe[0] or (Packe[1] shl 8);
  Red := (Value shr 11) and $1F;
  Green := (Value shr 5) and $3F;
  Blue := Value and $1F;

  Colour[0] := (Blue shl 3) or (Blue shr 2);
  Colour[1] := (Green shl 2) or (Green shr 4);
  Colour[2] := (Red shl 3) or (Red shr 2);
  Colour[3] := 255;
  Result := Value;
end;

procedure DXTColour(ARGB, Block: PByte; IsDXT1: Integer);
var
  Bytes, Index: PByte;
  Codes: array of Byte;
  Indices: array of Byte;
  A, B, i, j, c, d: Integer;
  Packeds, Offset: Byte;
begin
  Bytes := Block;
  SetLength(Codes, 16);
  SetLength(Indices, 16);
  A := DXTUnpack565(Bytes, PByte(Codes));
  B := DXTUnpack565(Bytes + 2, PByte(Codes) + 4);

  for i := 0 to 2 do
  begin
    c := Codes[i];
    d := Codes[4 + i];
    if (IsDXT1 and A) >= B then
    begin
      Codes[8 + i] := (c + d) shr 1;
      Codes[12 + i] := 0;
    end
    else
    begin
      Codes[8 + i] := (2 * c + d) div 3;
      Codes[12 + i] := (c + 2 * d) div 3;
    end;
  end;

  Codes[8 + 3] := 255;
  if (IsDXT1 and A) <= B then
    Codes[12 + 3] := 0
  else
    Codes[12 + 3] := 255;

  for i := 0 to 3 do
  begin
    Index := PByte(Indices) + (i shl 2);
    Packeds := Bytes[4 + i];
    Index[0] := Packeds and $3;
    Index[1] := (Packeds shr 2) and $3;
    Index[2] := (Packeds shr 4) and $3;
    Index[3] := (Packeds shr 6) and $3;
  end;

  for i := 0 to 15 do
  begin
    Offset := Indices[i] shl 2;
    for j := 0 to 3 do
      ARGB[(i shl 2) + j] := Codes[Offset + j];
  end;

end;

function DXTFixFlags(Flags: Integer): Integer;
var
  Method, Fit, Metric, Extra: Integer;
begin
  Method := Flags and (kDXT1 or kDXT3 or kDXT5);
  Fit := Flags and (kColourIterativeClusterFit or kColourClusterFit or kColourRangeFit);
  Metric := Flags and (kColourMetricPerceptual or kColourMetricUniform);
  Extra := Flags and kWeightColourByAlpha;

  if (Method <> kDXT3) and (Method <> kDXT5) then
    Method := kDXT1;
  if (Fit <> kColourRangeFit) then
    Fit := kColourClusterFit;
  if (Metric <> kColourMetricUniform) then
    Metric := kColourMetricPerceptual;

  Result := Method or Fit or Metric or Extra;
end;

procedure DXTDecompress(ARGB, Block: PByte; Flags: Integer);
var
  ColourBlock, Alphabock: PByte;
begin
  ColourBlock := Block;
  Alphabock := Block;

  Flags := DXTFixFlags(Flags);

  if Boolean(Flags and (kDXT3 or kDXT5)) then
    ColourBlock := Block + 8;
  // decompress colour
  DXTColour(ARGB, ColourBlock, Flags and kDXT1);
  if Boolean(Flags and kDXT3) then
    DXTAlphaDXT3(ARGB, Alphabock)
  else if Boolean(Flags and kDXT5) then
    DXTAlphaDXT5(ARGB, Alphabock);

end;

procedure DXTDecompressImage(ARGB: PByte; Width, Height: Integer; Blocks: PByte; Flags: Integer);
var
  SourceBlock, SourcePixel, TargetPixel: PByte;
  i, x, y, Px, Py, BytesPerBlock, Sx, Sy: Integer;
  TargetRGBA: array of Byte;
begin
  SourceBlock := Blocks;
  SetLength(TargetRGBA, 16 shl 2);

  if Boolean(Flags and kDXT1) then
    BytesPerBlock := 8
  else
    BytesPerBlock := 16;

  Flags := DXTFixFlags(Flags);

  for y := 0 to Height - 1  do
  begin

    if y mod 4 = 0 then
    begin
      for x := 0 to Width - 1 do
      begin

        if x mod 4 = 0 then
        begin

          SourcePixel := PByte(TargetRGBA);
          DXTDecompress(PByte(TargetRGBA), SourceBlock, Flags);

          for Py := 0 to 3 do
          begin
            for Px := 0 to 3 do
            begin
              Sx := x + Px;
              Sy := y + Py;
              if (Sx < Width) and (Sy < Height) then
              begin
                TargetPixel := ARGB + 4 * (Width * Sy + Sx);
                for i := 0 to 3 do
                begin
                  TargetPixel^ := SourcePixel^;
                  Inc(TargetPixel);
                  Inc(SourcePixel);
                end;

              end
              else
              begin
                SourcePixel := SourcePixel + 4;
              end;

            end;
          end;

          SourceBlock := SourceBlock + BytesPerBlock;

        end;

      end;
    end;

  end;

end;

procedure DXTDecompressImageFlip(ARGB: PByte; Width, Height: Integer; Blocks: PByte; Flags: Integer);
var
  SourceBlock, SourcePixel, TargetPixel: PByte;
  i, x, y, Px, Py, BytesPerBlock, Sx, Sy: Integer;
  TargetRGBA: array of Byte;
begin
  SourceBlock := Blocks;
  SetLength(TargetRGBA, 16 shl 2);

  if Boolean(Flags and kDXT1) then
    BytesPerBlock := 8
  else
    BytesPerBlock := 16;

  Flags := DXTFixFlags(Flags);

  for y :=  Height - 1 downto 0 do
  begin

    if y mod 4 = 0 then
    begin
      for x := 0 to Width - 1 do
      begin

        if x mod 4 = 0 then
        begin

          SourcePixel := PByte(TargetRGBA);
          DXTDecompress(PByte(TargetRGBA), SourceBlock, Flags);

          for Py :=  3 downto 0 do
          begin
            for Px := 0 to 3 do
            begin
              Sx := x + Px;
              Sy := y + Py;
              if (Sx < Width) and (Sy < Height) then
              begin
                TargetPixel := ARGB + 4 * (Width * Sy + Sx);
                for i := 0 to 3 do
                begin
                  TargetPixel^ := SourcePixel^;
                  Inc(TargetPixel);
                  Inc(SourcePixel);
                end;

              end
              else
              begin
                SourcePixel := SourcePixel + 4;
              end;

            end;
          end;

          SourceBlock := SourceBlock + BytesPerBlock;

        end;

      end;
    end;

  end;

end;


end.
