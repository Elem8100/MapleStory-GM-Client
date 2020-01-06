unit ZGameFonts;

interface

uses
  Windows, SysUtils, Graphics, Classes, Vectors2px, AsphyreFactory, AbstractTextures, AsphyreTypes,
  AbstractCanvas, Generics.Collections;

const
  MinFontTextureWidth  = 256;
  MinFontTextureHeight = 256;
  DefaultWordSpacing   = 0;
  DefaultLineSpacing   = 0;
  DefaultTextsCached   = 100;

type

  PZLetterGlyph = ^TZLetterGlyph;
  TZLetterGlyph = record
    Code:   WideChar;      // 字符代码
    Pos:    TPoint2px;     // 纹理行列
    bUsed:  Boolean;       // 是否激活使用
    pPrev:  PZLetterGlyph;
    pNext:  PZLetterGlyph;
  end;

  PZLetterEntry = ^TZLetterEntry;
  TZLetterEntry = record
    TexPos:   TPoint2px;  //  字符纹理坐标
    Top:      TPoint2px;  //  字符绘制位置偏移
    Size:     TPoint2px;  //  字符有效纹理大小
    Tail:     Integer;
    Scanned:  Boolean;    //  字符信息是否存储
    Prepared: Boolean;    //  字符是否被预缓存
    pGlyph:   PZLetterGlyph;  //  字符纹理缓存信息
  end;

  TZTextInfo = record
    Color:   TColor4;
    Pos:     TPoint2px;
    Compact: Boolean;
    Spacing: Integer;
    Text:    WideString;
  end;

  TZHAlign = (zhLeft, zhCenter, zhRight);

  TZVAlign = (zvTop, zvMiddle, zvBottom);

  TZGameFont = class
  private
    FFont: TFont;
    FOutLine: Boolean;
    FRows, FCols: Integer;
    FMaxHeight, FMaxWidth:  Integer;
    FtmAscent: Integer;
    FTexture:  TAsphyreLockableTexture;
    FEntries:  array[0..65535] of TZLetterEntry;
    FPrepares: TList<WideChar>;
    FGlyphs:   array of TZLetterGlyph;
    FFreeCount: Integer;
    FFreeHead: TZLetterGlyph;
    FFreeTail: TZLetterGlyph;
    FUsedHead: TZLetterGlyph;
    FUsedTail: TZLetterGlyph;
    FCanvas:   TAsphyreCanvas;
    FColor:    TColor4;
    FTextList: TList<TZTextInfo>;
    FBuffWidth:  Integer;
    FBuffHeight: Integer;
    FWordBuff:   PByte;

    procedure ScanChar(DC:HDC; Code:WideChar); inline;
    procedure ResetGlyphsLists;
    procedure PrepareChar(Code: WideChar);
    procedure CachePrepares(DC:HDC);
    procedure CacheChar(DC:HDC; Code:WideChar; pDest:Pointer; Pitch: Integer);
    procedure CleanCharTexture(pDest:Pointer; Pitch: Integer);
    function GetCharWidth(DC: HDC; Code: WideChar; bDrawSize: Boolean = True):
        Integer;
    function  GetTextWidth(DC: HDC; Text: WideString; nFirstPos, nLastPos: Integer; Spacing: Integer): Integer;
    procedure ResetCacheData;
    function TextOutCached(Pos: TPoint2px; const Text: WideString; nStart, nEnd:
        Integer; const Color: TColor4; bCompact: Boolean = False; Spacing: Integer
        = DefaultWordSpacing): TPoint2px;
    procedure doFlush(DC:HDC);
    procedure OutLineChar(SrcBuff: PByte; SrcPitch: Integer; DestBuff: PByte;
        DestPitch: Integer);
  public

    property MaxHeight: Integer read FMaxHeight;
    property Canvas: TAsphyreCanvas read FCanvas write FCanvas;
    property Color: TColor4 read FColor write FColor;

    function CharWidth(DC: HDC; Code: WideChar; bDrawSize: Boolean = True): Integer;
    function TextWidth(DC: HDC; Text: WideString; nFirstPos, nLastPos, Spacing: Integer): Integer;
    procedure TextOut(DC: HDC; Pos: TPoint2px; const Text: WideString; bCompact:
        Boolean = False; WordSpacing: Integer = DefaultWordSpacing; bFlush: Boolean
        = True); overload;
    procedure TextOutRect(DC: HDC; Pos, Size: TPoint2px; const Text: WideString;
        WordSpacing: Integer = DefaultWordSpacing; LineSpacing: Integer =
        DefaultLineSpacing; AutoReturn: Boolean = True; const HAlign: TZHAlign =
        zhLeft; const VAlign: TZValign = zvTop);
    procedure Flush(DC: HDC);

    constructor Create(DC: HDC; Factory: TAsphyreFactory; TexWidth: Integer = 512;
        TexHeight: Integer = 512; const Name: WideString = '宋体'; Size: Cardinal =
        9; const Style: TFontStyles = []; bOutLine: Boolean = False); overload;
    destructor  Destroy(); override;



    // Debug Function
    procedure DrawFontTexture(const Pos: TPoint2px; const Colors: TColor4);
    // Test  Function
    procedure TestSetFontTexture(DC:HDC);
  end;

implementation

{ TZGameFont }

procedure TZGameFont.PrepareChar(Code: WideChar);
var
  pEntry: PZLetterEntry;
  pGlyph: PZLetterGlyph;
begin
  pEntry := @FEntries[Integer(Code)];
  if not Assigned(pEntry.pGlyph) then
  begin
    if not pEntry.Prepared then
    begin
      FPrepares.Add(Code);
      pEntry.Prepared := True;
    end;
  end else
  begin
    pGlyph := pEntry.pGlyph;
    pGlyph.pPrev.pNext := pGlyph.pNext;
    pGlyph.pNext.pPrev := pGlyph.pPrev;
    pGlyph.pPrev := FUsedTail.pPrev;
    pGlyph.pPrev.pNext := pGlyph;
    pGlyph.pNext := @FUsedTail;
    FUsedTail.pPrev := pGlyph;
    if not pGlyph.bUsed then
    begin
      Dec(FFreeCount);
      pGlyph.bUsed := True;
    end;
  end;
end;

function TZGameFont.TextOutCached(Pos: TPoint2px; const Text: WideString;
    nStart, nEnd: Integer; const Color: TColor4; bCompact: Boolean = False;
    Spacing: Integer = DefaultWordSpacing): TPoint2px;
var
  nStep: Integer;
  pEntry: PZLetterEntry;
  Code: WideChar;
begin
  if not Assigned(FCanvas) then Exit(Pos);
  for nStep := nStart to nEnd do
  begin
    Code   := Text[nStep];
    pEntry := @FEntries[Integer(Code)];
    FCanvas.UseTexturePx(FTexture, pxBounds4( pEntry.TexPos.x, pEntry.TexPos.y ,FMaxWidth,FMaxHeight));

    if bCompact then
    begin
      FCanvas.TexMap( pBounds4(Pos.x,Pos.y+pEntry.Top.y,FMaxWidth,FMaxHeight), Color);
      Inc(Pos.x, pEntry.Size.x+Spacing);
    end else
    begin
      FCanvas.TexMap( pBounds4(Pos.x+pEntry.Top.x,Pos.y+pEntry.Top.y,FMaxWidth,FMaxHeight), Color);
      Inc(Pos.x, pEntry.Top.x+pEntry.Size.x+pEntry.Tail+Spacing);
    end;
  end;

  Result := Pos;
end;

procedure TZGameFont.TextOutRect(DC: HDC; Pos, Size: TPoint2px; const Text:
    WideString; WordSpacing: Integer = DefaultWordSpacing; LineSpacing: Integer
    = DefaultLineSpacing; AutoReturn: Boolean = True; const HAlign: TZHAlign =
    zhLeft; const VAlign: TZValign = zvTop);
var
  oldFont: HGDIOBJ;
  Lines:  TStringList;
  PLines: TStringList;
  LineIndex: Integer;
  StrIndex:  Integer;
  WordLen: Integer;
  LineText: WideString;
  LineLen: Integer;
  LinesHeight: Integer;
  bEndParse: Boolean;
  xPos,yPos: Integer;
begin
  bEndParse := False;
  Lines := TStringList.Create;
  Lines.Text := Text;
  PLines := TStringList.Create;
  LineText := '';
  LineLen  := 0;
  LinesHeight := 0;

  oldFont := SelectObject(DC,FFont.Handle);

  for LineIndex := 0 to Lines.Count-1 do
  begin
    if LineIndex = 0 then
    begin
      if FMaxHeight > Size.y then Break;
    end else
    begin
      if LinesHeight+FMaxHeight+LineSpacing > Size.y then Break;
    end;

    if Length(Lines[LineIndex]) = 0 then
    begin
      Inc(LinesHeight,FMaxHeight+LineSpacing);
    end;

    for StrIndex := 1 to Length(Lines[LineIndex]) do
    begin
      WordLen := GetCharWidth(DC,Lines[LineIndex][StrIndex]);
      if LineLen = 0 then
      begin
        if WordLen <= Size.x then
        begin
          LineLen  := WordLen;
          LineText := Lines[LineIndex][StrIndex];
        end else
        begin
          bEndParse := True;
          Break;
        end;
      end else
      begin
        if LineLen+WordLen <= Size.x then
        begin
          LineText := LineText + Lines[LineIndex][StrIndex];
          Inc(LineLen,WordLen);
        end else
        begin
          PLines.Add(LineText);
          LineLen := 0;

          if LineIndex = 0 then
            Inc(LinesHeight,FMaxHeight)
          else
            Inc(LinesHeight,FMaxHeight+LineSpacing);

          if LinesHeight+FMaxHeight+LineSpacing > Size.y then
          begin
            bEndParse := True;
            Break;
          end else
          begin
            if not AutoReturn then Break;
          end;

          LineLen  := WordLen;
          LineText := Lines[LineIndex][StrIndex];
        end;
      end;
    end;
    if bEndParse then Break;

    if LineLen > 0 then
    begin
      PLines.Add(LineText);
      Inc(LinesHeight,FMaxHeight+LineSpacing);
      LineLen := 0;
      if LinesHeight+FMaxHeight+LineSpacing > Size.y then bEndParse := True;
    end;
    if bEndParse then Break;
  end;

  case VAlign of
    zvMiddle: yPos := Pos.y + (Size.y-LinesHeight) div 2;
    zvBottom: yPos := Pos.y + Size.y-LinesHeight;
  else
    yPos := Pos.y;
  end;

  xPos := Pos.x;
  for LineIndex := 0 to PLines.Count-1 do
  begin
    case HAlign of
      zhCenter:
        xPos := Pos.x+(Size.x-GetTextWidth(DC,PLines[LineIndex],1,Length(PLines[LineIndex]),WordSpacing)) div 2;
      zhRight:
        xPos := Pos.x+Size.x-GetTextWidth(DC,PLines[LineIndex],1,Length(PLines[LineIndex]),WordSpacing);
    end;

    TextOut(DC,Point2px(xPos,yPos),PLines.Strings[LineIndex]);
    Inc(yPos,FMaxHeight+LineSpacing);
  end;
  //Flush(DC);

  SelectObject(DC,oldFont);

  Lines.Free;
  PLines.Free;
end;

function TZGameFont.TextWidth(DC: HDC; Text: WideString; nFirstPos, nLastPos, Spacing: Integer): Integer;
var
  oldFont: HGDIOBJ;
begin
  oldFont := SelectObject(DC,FFont.Handle);
  Result := GetTextWidth(DC,Text,nFirstPos,nLastPos,Spacing);
  SelectObject(DC,oldFont);
end;

procedure TZGameFont.TextOut(DC: HDC; Pos: TPoint2px; const Text: WideString;
    bCompact: Boolean = False; WordSpacing: Integer = DefaultWordSpacing;
    bFlush: Boolean = True);
var
  oldFont: HGDIOBJ;
  nStep: Integer;
  nStart: Integer;
  nEnd: Integer;
  ListIndex: Integer;
  tmpInfo: TZTextInfo;
begin
  oldFont := SelectObject(DC,FFont.Handle);
  nStart  := 1;
  nEnd    := 1;
  for nStep := 1 to Length(Text) do
  begin
    if Text[nStep] = #0 then Break;
    nEnd  := nStep;
    PrepareChar(Text[nStep]);
    if FFreeCount = FPrepares.Count then
    begin
      CachePrepares(DC);
      ResetCacheData;
      for ListIndex := 0 to FTextList.Count-1 do
      begin
        tmpInfo := FTextList[ListIndex];
        TextOutCached(tmpInfo.Pos,tmpInfo.Text,1,Length(tmpInfo.Text),tmpInfo.Color,tmpInfo.Compact,tmpInfo.Spacing);
      end;
      FTextList.Clear;
      Pos := TextOutCached(Pos,Text,nStart,nEnd,FColor,bCompact,WordSpacing);
      FCanvas.Flush;
      nStart := nStep + 1;
    end;
  end;

  if (nStart <= Length(Text)) and (Text[nStart] <> #0) then
  begin
    tmpInfo.Pos     := Pos;
    tmpInfo.Color   := FColor;
    tmpInfo.Compact := bCompact;
    tmpInfo.Spacing := WordSpacing;
    tmpInfo.Text    := Copy(Text,nStart,nEnd-nStart+1);
    FTextList.Add(tmpInfo);
  end;
  if bFlush then doFlush(DC);
  
  SelectObject(DC,oldFont);
end;

procedure TZGameFont.ResetCacheData;
var
  pGlyph: PZLetterGlyph;
begin
  FPrepares.Clear;
  FFreeCount := FRows*FCols;
  if FUsedHead.pNext <> @FUsedTail then
  begin
    pGlyph := FUsedHead.pNext;
    while pGlyph <> @FUsedTail do
    begin
      pGlyph.bUsed := False;
      FEntries[Integer(pGlyph.Code)].Prepared := False;
      pGlyph := pGlyph.pNext;
    end;

    FFreeTail.pPrev.pNext := FUsedHead.pNext;
    FUsedHead.pNext.pPrev := FFreeTail.pPrev;
    FUsedTail.pPrev.pNext := @FFreeTail;
    FFreeTail.pPrev := FUsedTail.pPrev;

    FUsedHead.pNext := @FUsedTail;
    FUsedTail.pPrev := @FUsedHead;
  end;
end;

procedure TZGameFont.CleanCharTexture(pDest:Pointer; Pitch: Integer);
var
  i,j: Integer;
  pDestW: PWord;
begin
  for i := 0 to FMaxHeight-1 do
  begin
    pDestW  := PWord( PByte(pDest) + i * Pitch );
    for j := 0 to FMaxWidth-1 do
    begin
      pDestW^ := 0;
      Inc(pDestW);
    end;
  end;
end;

constructor TZGameFont.Create(DC: HDC; Factory: TAsphyreFactory; TexWidth:
    Integer = 512; TexHeight: Integer = 512; const Name: WideString = '宋体';
    Size: Cardinal = 9; const Style: TFontStyles = []; bOutLine: Boolean =
    False);
var
  tm: TTextMetric;
  oldFont: HGDIOBJ;
begin
  FFont := TFont.Create;
  FFont.Name  := Name;
  FFont.Size  := Size;
  FFont.Style := Style;
  FOutLine  := bOutLine;
  FColor    := cColor4($FFFFFFFF);

  oldFont := SelectObject(DC,FFont.Handle);
  if GetTextMetrics(DC,tm) then
  begin
    FMaxWidth  := tm.tmMaxCharWidth;
    FMaxHeight := tm.tmHeight;
    FtmAscent  := tm.tmAscent;
  end else
  begin
    FMaxWidth   := 12;
    FMaxHeight  := 12;
    FtmAscent   := 10;
  end;
  SelectObject(DC,oldFont);

  // 无法提前知道TrueType斜体或者粗体时字体的最大宽度
  // 把最大宽度放大一点,保证能显示完整.
  if (fsItalic in Style) or (fsBold in Style) then
  begin
    Inc(FMaxWidth,FMaxWidth div 5);
    if fsBold in Style then
    begin
      Inc(FMaxWidth,2);
      Inc(FMaxHeight,2);
    end;
  end;


  if bOutLine then
  begin
    Inc(FMaxWidth,2);
    Inc(FMaxHeight,2);
    FBuffWidth  := FMaxWidth  + 2;
    FBuffHeight := FMaxHeight + 2;
    FWordBuff := AllocMem(FBuffWidth*FBuffHeight*Sizeof(WORD));
  end;

  if TexWidth<MinFontTextureWidth    then TexWidth  := MinFontTextureWidth;
  if TexHeight<MinFontTextureHeight  then TexHeight := MinFontTextureHeight;
  FTexture := Factory.CreateLockableTexture;
  FTexture.Width  := TexWidth;
  FTexture.Height := TexHeight;
  FTexture.Format := apf_A1R5G5B5;
  FTexture.Initialize;

  FRows := FTexture.Height div FMaxHeight;
  FCols := FTexture.Width div FMaxWidth;
  FPrepares := TList<WideChar>.Create;
  FPrepares.Capacity := FRows*FCols+1;
  FFreeCount := FRows*FCols;
  SetLength(FGlyphs,FFreeCount);
  ResetGlyphsLists();

  FTextList := TList<TZTextInfo>.Create;
  FTextList.Capacity := DefaultTextsCached;
end;

destructor TZGameFont.Destroy;
begin
  if Assigned(FWordBuff) then FreeMem(FWordBuff);
  FreeAndNil(FTextList);
  FreeAndNil(FFont);
  FreeAndNil(FPrepares);
  FreeAndNil(FTexture);
  FGlyphs := nil;
  inherited;
end;

procedure TZGameFont.doFlush(DC: HDC);
var
  ListIndex: Integer;
  tmpInfo: TZTextInfo;
  PrepareCount: Integer;
begin
  PrepareCount := FPrepares.Count;
  if FTextList.Count > 0 then
  begin
    CachePrepares(DC);
    ResetCacheData;
    for ListIndex := 0 to FTextList.Count-1 do
    begin
      tmpInfo := FTextList[ListIndex];
      TextOutCached(tmpInfo.Pos,tmpInfo.Text,1,Length(tmpInfo.Text),tmpInfo.Color,tmpInfo.Compact,tmpInfo.Spacing);
    end;
  end;
  FTextList.Clear;
  if (PrepareCount>0) and Assigned(FCanvas) then FCanvas.Flush;
end;

procedure TZGameFont.DrawFontTexture(const Pos: TPoint2px; const Colors: TColor4);
begin
  if Assigned(FCanvas) then
  begin
    FCanvas.UseTexturePx(FTexture,pxBounds4(0,0,FTexture.Width,FTexture.Height));
    FCanvas.TexMap( pBounds4(Pos.x,Pos.y,FTexture.Width,FTexture.Height), Colors );
  end;
end;

procedure TZGameFont.Flush(DC: HDC);
var
  oldFont: HGDIOBJ;
begin
  oldFont := SelectObject(DC,FFont.Handle);
  doFlush(DC);
  SelectObject(DC,oldFont);
end;

function TZGameFont.GetTextWidth(DC: HDC; Text: WideString; nFirstPos, nLastPos, Spacing: Integer): Integer;
var
  ChIndex: Integer;
  ChWidth: Integer;
begin
  Result := 0;
  for ChIndex := nFirstPos to nLastPos do
  begin
    ChWidth := GetCharWidth(DC,Text[ChIndex]);
    if ChIndex>nFirstPos then Inc(Result,Spacing);
    Result := Result + ChWidth;
  end;
end;

procedure TZGameFont.ResetGlyphsLists;
var
  nStep: Integer;
  pPreGlyph: PZLetterGlyph;
  pGlyph: PZLetterGlyph;
  EntryIndex: Cardinal;
begin
  FEntries[0].pGlyph   := nil;
  FEntries[0].Prepared := False;
  pPreGlyph := @FFreeHead;
  pPreGlyph.pPrev := nil;
  for nStep := Low(FGlyphs) to High(FGlyphs) do
  begin
    pGlyph := @FGlyphs[nStep];
    pPreGlyph.pNext := pGlyph;
    EntryIndex := Cardinal(pGlyph.Code);
    if EntryIndex <> 0 then
    begin
      FEntries[EntryIndex].pGlyph   := nil;
      FEntries[EntryIndex].Prepared := False;
    end;
    pGlyph.pPrev  := pPreGlyph;
    pGlyph.Code   := #0;
    pGlyph.Pos    := Point2px( nStep mod FCols, nStep div FCols );
    pGlyph.bUsed  := False;
    pPreGlyph     := @FGlyphs[nStep];
  end;
  pPreGlyph.pNext := @FFreeTail;
  FFreeTail.pPrev := pPreGlyph;

  FUsedHead.pNext := @FUsedTail;
  FUsedTail.pPrev := @FUsedHead;
end;

procedure TZGameFont.ScanChar(DC:HDC; Code:WideChar);
var
  pEntry: PZLetterEntry;
  gm: TGLYPHMETRICS;
  mt: TMat2;
begin
  pEntry := @FEntries[Integer(Code)];
  if not pEntry.Scanned then
  begin
    ZeroMemory(@mt,SizeOf(TMat2));
    mt.eM11.value := 1;
    mt.eM22.value := 1;
    GetGlyphOutline( DC,Cardinal(Code),GGO_METRICS,gm,0,nil,mt );


    if FOutLine then
      pEntry.Size := Point2px(gm.gmBlackBoxX+2,gm.gmBlackBoxY+2)
    else
      pEntry.Size := Point2px(gm.gmBlackBoxX,gm.gmBlackBoxY);
    pEntry.Top  := Point2px(gm.gmptGlyphOrigin.X,FtmAscent-gm.gmptGlyphOrigin.Y);
    pEntry.Tail := gm.gmCellIncX - gm.gmBlackBoxX - gm.gmptGlyphOrigin.X;

    pEntry.Scanned := True;
  end;
end;

procedure TZGameFont.CacheChar(DC:HDC; Code:WideChar; pDest:Pointer; Pitch:
    Integer);
var
  pEntry: PZLetterEntry;
  gm: TGLYPHMETRICS;
  mt: TMat2;
  cbBuff: Cardinal;
  pBuff:  array of Byte;
  GlyphW: Integer;
  fixW, fixH: Integer;
  i,j: Integer;
  nBuffPos: Integer;
  BuffByte: Byte;
  shlStep: Integer;
  pDestW: PWord;
begin
  BuffByte := 0;
  ZeroMemory(@mt,SizeOf(TMat2));
  mt.eM11.value := 1;
  mt.eM22.value := 1;
  cbBuff := GetGlyphOutline( DC,Cardinal(Code),GGO_BITMAP,gm,0,nil,mt );
  SetLength(pBuff,cbBuff+1);
  GetGlyphOutline( DC,Cardinal(Code),GGO_BITMAP,gm,cbBuff,@pBuff[0],mt );

  pEntry := @FEntries[Integer(Code)];
  if not pEntry.Scanned then
  begin
    if FOutLine then
      pEntry.Size := Point2px(gm.gmBlackBoxX+2,gm.gmBlackBoxY+2)
    else
      pEntry.Size := Point2px(gm.gmBlackBoxX,gm.gmBlackBoxY);
    pEntry.Top  := Point2px(gm.gmptGlyphOrigin.X,FtmAscent-gm.gmptGlyphOrigin.Y);
    pEntry.Tail := gm.gmCellIncX - gm.gmBlackBoxX - gm.gmptGlyphOrigin.X;
    pEntry.Scanned := True;
  end;

  if Integer(gm.gmBlackBoxX) > FMaxWidth then
    fixW := FMaxWidth
  else
    fixW := gm.gmBlackBoxX;

  if Integer(gm.gmBlackBoxY) > FMaxHeight then
    fixH := FMaxHeight
  else
    fixH := gm.gmBlackBoxY;

  GlyphW := ((gm.gmBlackBoxX + 31) div 32) * 4;

  CleanCharTexture(pDest,Pitch);

  for i := 0 to fixH-1 do
  begin
    nBuffPos := i * GlyphW;
    pDestW  := PWord( PByte(pDest) + i * Pitch );
    for j := 0 to fixW-1 do
    begin
      shlStep := j mod 8;
      if shlStep = 0 then
      begin
        BuffByte := pBuff[nBuffPos];
        Inc(nBuffPos);
      end;
      if ( (BuffByte shl shlStep) and $80) <> 0 then
        pDestW^ := $FFFF
      else
        pDestW^ := 0;

      Inc(pDestW);
    end;
  end;

  pBuff := nil;
end;

procedure TZGameFont.TestSetFontTexture(DC:HDC);
var
  rect: TRect;
  pDest: Pointer;
  nPitch: Integer;
  i, j : Integer;
  ch: WideChar;
  pRowDest: Pointer;
  oldFont: HGDIOBJ;
begin
  oldFont := SelectObject(DC,FFont.Handle);
  if Assigned(FTexture) and FTexture.Active then
  begin
    rect.Left := 0;
    rect.Top  := 0;
    rect.Right  := FTexture.Width;
    rect.Bottom := FTexture.Height;
    try
      FTexture.Lock(rect,pDest,nPitch);
    except
      exit;
    end;

    ch := #0;

    for i := 0 to FRows-1 do
    begin
      pRowDest := Pointer( PByte(pDest) + nPitch*i*FMaxHeight );
      for j := 0 to FCols-1 do
      begin
        CacheChar( DC, ch, Pointer(PByte(pRowDest) + j*FMaxWidth*Sizeof(WORD)) ,nPitch );
        Inc(ch);
      end;
    end;

    FTexture.Unlock;
  end;
  SelectObject(DC,oldFont);
end;

procedure TZGameFont.CachePrepares(DC:HDC);
var
  rect: TRect;
  pDest: Pointer;
  pRowDest: Pointer;
  nPitch: Integer;
  nStep: Integer;
  pGlyph: PZLetterGlyph;
  pEntry: PZLetterEntry;
begin
  if FPrepares.Count > 0 then
  begin
    if Assigned(FTexture) and FTexture.Active then
    begin
      rect.Left := 0;
      rect.Top  := 0;
      rect.Right  := FTexture.Width;
      rect.Bottom := FTexture.Height;
      try
        FTexture.Lock(rect,pDest,nPitch);
      except
        exit;
      end;

      for nStep := 0 to FPrepares.Count-1 do
      begin
        pGlyph := FFreeHead.pNext;
        FFreeHead.pNext := pGlyph.pNext;
        pGlyph.pNext.pPrev := @FFreeHead;

        pGlyph.pPrev := FFreeTail.pPrev;
        pGlyph.pPrev.pNext := pGlyph;
        pGlyph.pNext := @FFreeTail;
        FFreeTail.pPrev := pGlyph;

        FEntries[Integer(pGlyph.Code)].pGlyph   := nil;
        FEntries[Integer(pGlyph.Code)].Prepared := False;
        pGlyph.Code := FPrepares.Items[nStep];

        pRowDest := Pointer( PByte(pDest) + nPitch*pGlyph.Pos.y*FMaxHeight );
        if FOutLine then
        begin
          ZeroMemory(FWordBuff,FBuffWidth*FBuffHeight*Sizeof(WORD));
          CacheChar(DC, pGlyph.Code, FWordBuff+(2*FBuffWidth+2)*Sizeof(WORD), FBuffWidth*Sizeof(WORD));
          OutLineChar(FWordBuff+(FBuffWidth+1)*Sizeof(WORD),
                      FBuffWidth*Sizeof(WORD),
                      PByte(pRowDest) + pGlyph.Pos.x*FMaxWidth*Sizeof(WORD),
                      nPitch);
        end else
          CacheChar( DC, pGlyph.Code, Pointer(PByte(pRowDest) + pGlyph.Pos.x*FMaxWidth*Sizeof(WORD)) ,nPitch );

        pEntry := @FEntries[Integer(pGlyph.Code)];
        pEntry.TexPos   := Point2px(pGlyph.Pos.x*FMaxWidth,pGlyph.Pos.y*FMaxHeight);
        pEntry.pGlyph   := pGlyph;
        pEntry.Prepared := False;
      end;

      FTexture.Unlock;
    end;
  end;
end;

function TZGameFont.CharWidth(DC: HDC; Code: WideChar; bDrawSize: Boolean =
    True): Integer;
var
  oldFont: HGDIOBJ;
  pEntry: PZLetterEntry;
begin
  pEntry := @FEntries[Integer(Code)];
  if pEntry.Scanned then
  begin
    Result := pEntry.Size.x;
    if bDrawSize then Inc(Result,pEntry.Top.x+pEntry.Tail);
  end else
  begin
    oldFont := SelectObject(DC,FFont.Handle);
    Result  := GetCharWidth(DC,Code);
    SelectObject(DC,oldFont);
  end;
end;

function TZGameFont.GetCharWidth(DC: HDC; Code: WideChar; bDrawSize: Boolean =
    True): Integer;
var
  pEntry: PZLetterEntry;
begin
  ScanChar(DC,Code);
  pEntry := @FEntries[Integer(Code)];
  Result := pEntry.Size.x;
  if bDrawSize then Inc(Result,pEntry.Top.x+pEntry.Tail);
end;

procedure TZGameFont.OutLineChar(SrcBuff: PByte; SrcPitch: Integer; DestBuff:
    PByte; DestPitch: Integer);
var
  nRowIndex, nColIndex: Integer;
  pSrcRow, pDestRow: PByte;
  pSrcW, pDestW: PWORD;
  pUpW, pDownW: PWORD;
  SrcWord: Word;
  MixWord: WORD;
begin
  for nRowIndex := 0 to FMaxHeight-1 do
  begin
    pSrcRow  := SrcBuff  + nRowIndex*SrcPitch;
    pDestRow := DestBuff + nRowIndex*DestPitch;
    pSrcW  := PWord(pSrcRow);
    pDestW := PWord(pDestRow);
    for nColIndex := 0 to FMaxWidth-1 do
    begin
      pUpW   := PWORD(pByte(pSrcW) - SrcPitch);
      pDownW := PWORD(PByte(pSrcW) + SrcPitch);
      SrcWord := pSrcW^;
      if SrcWord > 0 then
      begin
        pDestW^ := SrcWord;
      end else
      begin
        MixWord := PWORD(PByte(pSrcW)+Sizeof(WORD))^ or PWORD(PByte(pSrcW)-Sizeof(WORD))^ or
                   pUpW^ or PWORD(PByte(pUpW)+Sizeof(WORD))^  or PWORD(PByte(pUpW)-Sizeof(WORD))^ or
                   pDownW^ or PWORD(PByte(pDownW)+Sizeof(WORD))^ or PWORD(PByte(pDownW)-Sizeof(WORD))^;

        if MixWord > 0 then pDestW^ := $8000;
      end;

      Inc(pDestW);
      Inc(pSrcW);
    end;
  end;

end;

end.
