unit AsphyreFormats;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
// The following option allows the conversion of luminance formats to RGB.
// This is particularly useful for displaying fonts with format like A8L8 -
// in this case, if no luminance formats are supported, an RGB replacement
// will be used, such as A8R8G8B8.
{$define ConvertLuminanceToRGB}

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, AsphyreTypes;

//---------------------------------------------------------------------------
type
 TAsphyreFormatList = class
 private
  Items: array of TAsphyrePixelFormat;

  SortReqFormat: TAsphyrePixelFormat;

  function GetCount(): Integer;
  function GetFormat(Index: Integer): TAsphyrePixelFormat;
  procedure ItemSortSwap(Index1, Index2: Integer);
  function ItemSortCompare(Item1, Item2: TAsphyrePixelFormat): Integer;
  function ItemSortSplit(Start, Stop: Integer): integer;
  procedure ItemSortExec(Start, Stop: integer);
 public
  property Count: Integer read GetCount;
  property Format[Index: Integer]: TAsphyrePixelFormat read GetFormat; default;

  function Insert(AFormat: TAsphyrePixelFormat): Integer;
  function IndexOf(AFormat: TAsphyrePixelFormat): Integer;
  function Include(AFormat: TAsphyrePixelFormat): Integer;
  procedure Remove(Index: Integer);
  procedure Clear();
  procedure SortBestMatch(AFormat: TAsphyrePixelFormat);
  procedure InsertAll();

  constructor Create();
 end;

//---------------------------------------------------------------------------
var
 FormatList: TAsphyreFormatList = nil;

//---------------------------------------------------------------------------
function FindClosestFormat(Format: TAsphyrePixelFormat;
 ExistingFormats: TAsphyreFormatList): TAsphyrePixelFormat;
procedure GetFormatCompatibilityList(Format: TAsphyrePixelFormat;
 Strings: TStrings);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreFormatInfo;

//---------------------------------------------------------------------------
function CanAcceptFormat(SampleFormat,
 ReqFormat: TAsphyrePixelFormat): Boolean;
var
 SampleInfo, ReqInfo: TPixelFormatInfo;
 Index: TChannelTypeIndex;
 Matched: Boolean;
 i, DestNo: Integer;
begin
 SampleInfo:= GetPixelFormatInfo(SampleFormat);
 ReqInfo   := GetPixelFormatInfo(ReqFormat);

 Matched:= False;

 for i:= 0 to ReqInfo.NoChannels - 1 do
  begin
   Index:= ReqInfo.Channels[i].Index;
   if (not IsUsefulChannel(Index)) then Continue;

   // -> Sample format needs to contain at least those channels present in
   // the requested format. In addition, they must have the same storage type.
   DestNo:= FindChannelAt(Index, SampleInfo);
   if (DestNo = -1)or
    (SampleInfo.Channels[DestNo].Storage <> ReqInfo.Channels[i].Storage) then
    begin
     Result:= False;
     Exit;
    end;

   Matched:= True;
  end;

 Result:= (Matched)or((SampleFormat = ReqFormat)and
  (SampleFormat <> apf_Unknown));
end;

//---------------------------------------------------------------------------
function GetChannelDistance(SrcFormat,
 DestFormat: TAsphyrePixelFormat): Integer;
var
 SrcInfo, DestInfo: TPixelFormatInfo;
 Index: TChannelTypeIndex;
 SrcNo, DestNo: Integer;
begin
 SrcInfo := GetPixelFormatInfo(SrcFormat);
 DestInfo:= GetPixelFormatInfo(DestFormat);

 Result:= 0;

 for SrcNo:= 0 to SrcInfo.NoChannels - 1 do
  begin
   Index:= SrcInfo.Channels[SrcNo].Index;
   if (not IsUsefulChannel(Index)) then Continue;

   DestNo:= FindChannelAt(Index, DestInfo);
   if (DestNo = -1) then Continue;

   Inc(Result, Sqr(SrcInfo.Channels[SrcNo].Bits -
    DestInfo.Channels[DestNo].Bits));
  end;
end;

//---------------------------------------------------------------------------
function GetChannelExtraBits(SampleFormat,
 ReqFormat: TAsphyrePixelFormat): Integer;
var
 SampleInfo, ReqInfo: TPixelFormatInfo;
 Index: TChannelTypeIndex;
 i: Integer;
begin
 SampleInfo:= GetPixelFormatInfo(SampleFormat);
 ReqInfo   := GetPixelFormatInfo(ReqFormat);

 Result:= 0;

 for i:= 0 to SampleInfo.NoChannels - 1 do
  begin
   Index:= SampleInfo.Channels[i].Index;

   if (not IsUsefulChannel(Index))or(FindChannelAt(Index, ReqInfo) = -1) then
    Inc(Result, SampleInfo.Channels[i].Bits);
  end;
end;

//---------------------------------------------------------------------------
function GetChannelPosDistance(SrcFormat,
 DestFormat: TAsphyrePixelFormat): Integer;
var
 SrcInfo, DestInfo: TPixelFormatInfo;
 Index: TChannelTypeIndex;
 SrcNo, DestNo, SrcAt, DestAt: Integer;
 Sum: Single;
begin
 SrcInfo := GetPixelFormatInfo(SrcFormat);
 DestInfo:= GetPixelFormatInfo(DestFormat);

 Sum:= 0.0;

 for SrcNo:= 0 to SrcInfo.NoChannels - 1 do
  begin
   Index:= SrcInfo.Channels[SrcNo].Index;
   if (not IsUsefulChannel(Index)) then Continue;

   DestNo:= FindChannelAt(Index, DestInfo);
   if (DestNo = -1) then Continue;

   SrcAt := (SrcInfo.NoChannels - 1) - SrcNo;
   DestAt:= (DestInfo.NoChannels - 1) - DestNo;

   Sum:= Sum + Sqr(SrcAt - DestAt);
  end;

 Result:= Round(Sqrt(Sum));
end;

//---------------------------------------------------------------------------
function FindClosestFormatGeneric(Format: TAsphyrePixelFormat;
 ExistingFormats: TAsphyreFormatList): TAsphyrePixelFormat;
var
 Accepted: TAsphyreFormatList;
 i: Integer;
begin
 Accepted:= TAsphyreFormatList.Create();

 for i:= 0 to ExistingFormats.Count - 1 do
  if (CanAcceptFormat(ExistingFormats[i], Format)) then
   Accepted.Insert(ExistingFormats[i]);

 Accepted.SortBestMatch(Format);

 if (Accepted.Count > 0) then
  Result:= Accepted[0] else Result:= apf_Unknown;

 FreeAndNil(Accepted);
end;

//---------------------------------------------------------------------------
function FindClosestFormat(Format: TAsphyrePixelFormat;
 ExistingFormats: TAsphyreFormatList): TAsphyrePixelFormat;
begin
 Result:= FindClosestFormatGeneric(Format, ExistingFormats);

 {$ifdef ConvertLuminanceToRGB}
 case Format of
  apf_L8:
   if (Result = apf_Unknown) then
    Result:= FindClosestFormatGeneric(apf_R8G8B8, ExistingFormats);

  apf_A8L8:
   if (Result = apf_Unknown) then
    Result:= FindClosestFormatGeneric(apf_A8R8G8B8, ExistingFormats);

  apf_A4L4:
   if (Result = apf_Unknown) then
    Result:= FindClosestFormatGeneric(apf_A4R4G4B4, ExistingFormats);
 end;
 {$endif}
end;

//---------------------------------------------------------------------------
procedure GetFormatCompatibilityList(Format: TAsphyrePixelFormat;
 Strings: TStrings);
var
 Accepted: TAsphyreFormatList;
 Sample  : TAsphyrePixelFormat;
 i: Integer;
begin
 Accepted:= TAsphyreFormatList.Create();

 for Sample:= Low(TAsphyrePixelFormat) to High(TAsphyrePixelFormat) do
  if (CanAcceptFormat(Sample, Format)) then
   Accepted.Insert(Sample);

 Accepted.SortBestMatch(Format);

 for i:= 0 to Accepted.Count - 1 do
  Strings.Add(FormatToStr(Accepted[i]));

 FreeAndNil(Accepted);

 {$ifdef ConvertLuminanceToRGB}
 case Format of
  apf_L8:
   GetFormatCompatibilityList(apf_R8G8B8, Strings);

  apf_A8L8:
   GetFormatCompatibilityList(apf_A8R8G8B8, Strings);

  apf_A4L4:
   GetFormatCompatibilityList(apf_A4R4G4B4, Strings);
 end;
 {$endif}
end;

//---------------------------------------------------------------------------
constructor TAsphyreFormatList.Create();
begin
 inherited;

end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.GetCount(): Integer;
begin
 Result:= Length(Items);
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.GetFormat(Index: Integer): TAsphyrePixelFormat;
begin
 if (Index >= 0)and(Index < Length(Items)) then
  Result:= Items[Index] else Result:= apf_Unknown;
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.Insert(AFormat: TAsphyrePixelFormat): Integer;
begin
 Result:= Length(Items);
 SetLength(Items, Result + 1);

 Items[Result]:= AFormat;
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.IndexOf(AFormat: TAsphyrePixelFormat): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Items) - 1 do
  if (Items[i] = AFormat) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.Include(AFormat: TAsphyrePixelFormat): Integer;
begin
 Result:= IndexOf(AFormat);
 if (Result = -1) then Result:= Insert(AFormat);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Items)) then Exit;

 for i:= Index to Length(Items) - 2 do
  Items[i]:= Items[i + 1];

 SetLength(Items, Length(Items) - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.Clear();
begin
 SetLength(Items, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.ItemSortSwap(Index1, Index2: Integer);
var
 Aux: TAsphyrePixelFormat;
begin
 Aux:= Items[Index1];

 Items[Index1]:= Items[Index2];
 Items[Index2]:= Aux;
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.ItemSortCompare(Item1,
 Item2: TAsphyrePixelFormat): Integer;
var
 Delta1, Delta2: Integer;
begin
 Delta1:= GetChannelDistance(Item1, SortReqFormat);
 Delta2:= GetChannelDistance(Item2, SortReqFormat);

 if (Delta1 = Delta2) then
  begin
   Delta1:= GetChannelExtraBits(Item1, SortReqFormat);
   Delta2:= GetChannelExtraBits(Item2, SortReqFormat);
  end;

 if (Delta1 = Delta2) then
  begin
   Delta1:= GetChannelPosDistance(Item1, SortReqFormat);
   Delta2:= GetChannelPosDistance(Item2, SortReqFormat);
  end;

 if (Delta1 = Delta2) then
  begin
   Delta1:= GetChannelCount(GetPixelFormatInfo(Item1));
   Delta2:= GetChannelCount(GetPixelFormatInfo(Item2));
  end;

 if (Delta1 = Delta2) then
  begin
   Delta1:= Abs(AsphyrePixelFormatBits[Item1] -
    AsphyrePixelFormatBits[SortReqFormat]);
   Delta2:= Abs(AsphyrePixelFormatBits[Item2] -
    AsphyrePixelFormatBits[SortReqFormat]);
  end;

 Result:= 0;

 if (Delta1 > Delta2) then Result:= 1;
 if (Delta1 < Delta2) then Result:= -1;
end;

//---------------------------------------------------------------------------
function TAsphyreFormatList.ItemSortSplit(Start, Stop: Integer): integer;
var
 Left, Right: Integer;
 Pivot: TAsphyrePixelFormat;
begin
 Left := Start + 1;
 Right:= Stop;
 Pivot:= Items[Start];

 while (Left <= Right) do
  begin
   while (Left <= Stop)and(ItemSortCompare(Items[Left], Pivot) < 0) do
    Inc(Left);

   while (Right > Start)and(ItemSortCompare(Items[Right], Pivot) >= 0) do
    Dec(Right);

   if (Left < Right) then ItemSortSwap(Left, Right);
  end;

 ItemSortSwap(Start, Right);

 Result:= Right;
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.ItemSortExec(Start, Stop: integer);
var
 SplitPt: integer;
begin
 if (Start < Stop) then
  begin
   SplitPt:= ItemSortSplit(Start, Stop);

   ItemSortExec(Start, SplitPt - 1);
   ItemSortExec(SplitPt + 1, Stop);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.SortBestMatch(AFormat: TAsphyrePixelFormat);
begin
 SortReqFormat:= AFormat;

 if (Length(Items) > 0) then ItemSortExec(0, Length(Items) - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreFormatList.InsertAll();
var
 i: TAsphyrePixelFormat;
begin
 Clear();

 for i:= Low(TAsphyrePixelFormat) to High(TAsphyrePixelFormat) do
  Insert(i);
end;

//---------------------------------------------------------------------------
initialization
 FormatList:= TAsphyreFormatList.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(FormatList);

//---------------------------------------------------------------------------
end.
