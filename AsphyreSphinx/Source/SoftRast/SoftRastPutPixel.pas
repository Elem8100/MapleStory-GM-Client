unit SoftRastPutPixel;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SoftRastTypes;

//---------------------------------------------------------------------------
procedure SRPutPixel(Addr: TSRAddress; ClipRect: TRect; x, y: Integer;
 Color: Cardinal; Op: Integer); stdcall;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure SRPutPixel(Addr: TSRAddress; ClipRect: TRect; x, y: Integer; Color: Cardinal;
 Op: Integer); stdcall;
var
 DestPtr: Pointer;
begin
 if (x < ClipRect.Left)or(y < ClipRect.Top)or(x >= ClipRect.Right)or(y >= ClipRect.Bottom) then Exit;

 DestPtr:= Pointer(Integer(Addr.Bits) + (y * Addr.Pitch) + (x * 4));

 if (Op and srMove > 0) then
  begin
   {$DEFINE OpMove}
   {$include include\srPutPixel.inc}
  end else
  begin
   if (Op and srAdd > 0) then
    begin
     if (Op and srInvAlpha > 0) then
      begin
       if (Op and srSrcAlpha > 0) then
        begin
         if (Op and srDestAlpha > 0) then
          begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE InvertSourceAlpha}
             {$DEFINE SourceMulAlpha}
             {$DEFINE DestMulAlpha}
             {$include include\srPutPixel.inc}
          end else
          begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE InvertSourceAlpha}
             {$DEFINE SourceMulAlpha}
             {$include include\srPutPixel.inc}
          end;
        end else
        begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE InvertSourceAlpha}
             {$DEFINE DestMulAlpha}
             {$include include\srPutPixel.inc}
        end;
      end else
      begin
       if (Op and srSrcAlpha > 0) then
        begin
         if (Op and srDestAlpha > 0) then
          begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE SourceMulAlpha}
             {$DEFINE DestMulAlpha}
             {$include include\srPutPixel.inc}
          end else
          begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE SourceMulAlpha}
             {$include include\srPutPixel.inc}
          end;
        end else
        begin
         if (Op and srDestAlpha > 0) then
          begin
             {$DEFINE LoadSourceAlpha}
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE DestMulAlpha}
             {$include include\srPutPixel.inc}
          end else
          begin
           if (Op and srDestSrc > 0) then
            begin
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$DEFINE DestMulSource}
             {$include include\srPutPixel.inc}
            end else
            begin
             {$DEFINE LoadDestPixel}
             {$DEFINE OpAdd}
             {$include include\srPutPixel.inc}
            end;
          end;
        end;
      end;
    end else
    begin
     if (Op and srInvAlpha > 0) then
      begin
       {$DEFINE LoadSourceAlpha}
       {$DEFINE LoadDestPixel}
       {$DEFINE OpDest}
       {$DEFINE InvertSourceAlpha}
       {$DEFINE DestMulAlpha}
       {$include include\srPutPixel.inc}
      end else
      begin
       if (Op and srDestAlpha > 0) then
        begin
         {$DEFINE LoadSourceAlpha}
         {$DEFINE LoadDestPixel}
         {$DEFINE OpDest}
         {$DEFINE DestMulAlpha}
         {$include include\srPutPixel.inc}
        end else
        begin
         if (Op and srDestSrc > 0) then
          begin
           {$DEFINE LoadDestPixel}
           {$DEFINE OpDest}
           {$DEFINE DestMulSource}
           {$include include\srPutPixel.inc}
          end else
          begin
           {$DEFINE LoadDestPixel}
           {$DEFINE OpDest}
           {$include include\srPutPixel.inc}
          end;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
end.
