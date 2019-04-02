 for i:= 0 to Length(PolyData) - 1 do
  begin
   Left:= PolyData[i].Left;
   Width:= (PolyData[i].Right - PolyData[i].Left) + 1;

   if (Width > 0) then
    begin
     // 1. set texture coordinates
     u:= PolyData[i].TxCoords.u1 * $10000;
     v:= PolyData[i].TxCoords.v1 * $10000;
     uDelta:= (Int64(PolyData[i].TxCoords.u2 - PolyData[i].TxCoords.u1) * $10000) div Width;
     vDelta:= (Int64(PolyData[i].TxCoords.v2 - PolyData[i].TxCoords.v1) * $10000) div Width;

     // 2. perform clipping
     if (Left < ClipRect.Left) then
      begin
       Diff:= ClipRect.Left - Left;
       Left:= ClipRect.Left;
       Width:= Width - Diff;
       u:= u + (uDelta * Diff);
       v:= v + (vDelta * Diff);
      end;
     if (Left + Width > ClipRect.Right) then Width:= ClipRect.Right - Left;
     if (Width < 1) then Continue;

     // 3. calculate pointers
     DestBits:= Pointer(Integer(Address.Bits) + ((i + TopY) * Address.Pitch) + (Left * 4));
