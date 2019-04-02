unit AsphyreFormatInfo;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreTypes;

//---------------------------------------------------------------------------
type
 TChannelTypeIndex = (ctiR, ctiG, ctiB, ctiA, ctiL, ctiU, ctiV, ctiW, ctiQ,
  ctiX, ctiUnknown);

//---------------------------------------------------------------------------
 TChannelStorage = (csUnsigned, csSigned, csFloat, csUnknown);

//---------------------------------------------------------------------------
 TChannelInfo = record
  Index  : TChannelTypeIndex;
  Storage: TChannelStorage;
  Bits   : Integer;
  Pos    : Integer;
 end;

//---------------------------------------------------------------------------
 TPixelFormatInfo = record
  NoChannels: Integer;
  Channels  : array[0..3] of TChannelInfo;
 end;

//---------------------------------------------------------------------------
 PFormatBitInfo = ^TFormatBitInfo;
 TFormatBitInfo = record
  rAt, rNo: Integer;
  gAt, gNo: Integer;
  bAt, bNo: Integer;
  aAt, aNo: Integer;
 end;

//---------------------------------------------------------------------------
 TPixelFormatCategory = (pfc_RGB, pfc_Luminance, pfc_BumpMap, pfc_DXT,
  pfc_RGBf, pfc_Misc);

//---------------------------------------------------------------------------
const
 FormatInfo: array[TAsphyrePixelFormat] of TFormatBitInfo = (
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_Unknown
  (rAt: 16; rNo:  8; gAt:  8; gNo:  8; bAt:  0; bNo:  8; aAt: -1; aNo:  0), // apf_R8G8B8
  (rAt: 16; rNo:  8; gAt:  8; gNo:  8; bAt:  0; bNo:  8; aAt: 24; aNo:  8), // apf_A8R8G8B8
  (rAt: 16; rNo:  8; gAt:  8; gNo:  8; bAt:  0; bNo:  8; aAt: -1; aNo:  0), // apf_X8R8G8B8
  (rAt: 11; rNo:  5; gAt:  5; gNo:  6; bAt:  0; bNo:  5; aAt: -1; aNo:  0), // apf_R5G6B5
  (rAt: 10; rNo:  5; gAt:  5; gNo:  5; bAt:  0; bNo:  5; aAt: -1; aNo:  0), // apf_X1R5G5B5
  (rAt: 10; rNo:  5; gAt:  5; gNo:  5; bAt:  0; bNo:  5; aAt: 15; aNo:  1), // apf_A1R5G5B5
  (rAt:  8; rNo:  4; gAt:  4; gNo:  4; bAt:  0; bNo:  4; aAt: 12; aNo:  4), // apf_A4R4G4B4
  (rAt:  5; rNo:  3; gAt:  2; gNo:  3; bAt:  0; bNo:  2; aAt: -1; aNo:  0), // apf_R3G3B2
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt:  0; aNo:  8), // apf_A8
  (rAt:  5; rNo:  3; gAt:  2; gNo:  3; bAt:  0; bNo:  2; aAt:  8; aNo:  8), // apf_A8R3G3B2
  (rAt:  8; rNo:  4; gAt:  4; gNo:  4; bAt:  0; bNo:  4; aAt: -1; aNo:  0), // apf_X4R4G4B4
  (rAt:  0; rNo: 10; gAt: 10; gNo: 10; bAt: 20; bNo: 10; aAt: 30; aNo:  2), // apf_A2B10G10R10
  (rAt:  0; rNo: 16; gAt: 16; gNo: 16; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_G16R16
  (rAt: 20; rNo: 10; gAt: 10; gNo: 10; bAt:  0; bNo: 10; aAt: 30; aNo:  2), // apf_A2R10G10B10
  (rAt:  0; rNo: 16; gAt: 16; gNo: 16; bAt: 32; bNo: 16; aAt: 48; aNo: 16), // apf_A16B16G16R16
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_L8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt:  8; aNo:  8), // apf_A8L8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt:  4; aNo:  4), // apf_A4L4
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_V8U8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_L6V5U5
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_X8L8V8U8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_Q8W8V8U8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_V16U16
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: 30; aNo:  2), // apf_A2W10V10U10
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_UYVY
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_R8G8_B8G8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_YUY2
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_G8R8_G8B8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_DXT1
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_DXT2
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_DXT3
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_DXT4
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_DXT5
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_L16
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_Q16W16V16U16
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_R16F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_G16R16F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_A16B16G16R16F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_R32F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_G32R32F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_A32B32G32R32F
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_CxV8U8
  (rAt:  0; rNo:  8; gAt:  8; gNo:  8; bAt: 16; bNo:  8; aAt: 24; aNo:  8), // apf_A8B8G8R8
  (rAt:  0; rNo:  8; gAt:  8; gNo:  8; bAt: 16; bNo:  8; aAt: -1; aNo:  0), // apf_X8B8G8R8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: 24; aNo:  8), // apf_A8X8V8U8
  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt: -1; aNo:  0), // apf_L8X8V8U8

  (rAt: -1; rNo:  0; gAt: -1; gNo:  0; bAt: -1; bNo:  0; aAt:  2; aNo:  6), // apf_A6L2
  (rAt:  4; rNo:  2; gAt:  2; gNo:  2; bAt:  0; bNo:  2; aAt:  6; aNo:  2), // apf_A2R2G2B2
  (rAt: 18; rNo:  9; gAt:  9; gNo:  9; bAt:  0; bNo:  9; aAt: 27; aNo:  5)  // apf_A5R9G9B9
 );

//---------------------------------------------------------------------------
 PixelFormatCategory: array[TAsphyrePixelFormat] of TPixelFormatCategory = (
  pfc_Misc, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB,
  pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB, pfc_RGB,
  pfc_Luminance, pfc_Luminance, pfc_Luminance, pfc_BumpMap, pfc_BumpMap,
  pfc_BumpMap, pfc_BumpMap, pfc_BumpMap, pfc_BumpMap, pfc_Misc, pfc_Misc,
  pfc_Misc, pfc_Misc, pfc_DXT, pfc_DXT, pfc_DXT, pfc_DXT, pfc_DXT,
  pfc_Luminance, pfc_BumpMap, pfc_RGBf, pfc_RGBf, pfc_RGBf, pfc_RGBf, pfc_RGBf,
  pfc_RGBf, pfc_Misc, pfc_RGB, pfc_RGB, pfc_BumpMap, pfc_BumpMap,
  pfc_Luminance, pfc_RGB, pfc_RGB);

//---------------------------------------------------------------------------
  ChannelTextLetter: array[TChannelTypeIndex] of Char = ('R', 'G', 'B', 'A',
   'L', 'U', 'V', 'W', 'Q', 'X', '-');

//---------------------------------------------------------------------------
procedure ResetFormatInfo(var Info: TPixelFormatInfo);
function AddChannel(var Info: TPixelFormatInfo; Index: TChannelTypeIndex;
 Storage: TChannelStorage; Bits, Pos: Integer): Integer;
function SameChannel(const Sample1, Sample2: TChannelInfo): Boolean;
function SameFormat(const Sample1, Sample2: TPixelFormatInfo): Boolean;

//---------------------------------------------------------------------------
function GetPixelFormatInfo(Format: TAsphyrePixelFormat): TPixelFormatInfo;
function InfoToPixelFormat(const Info: TPixelFormatInfo): TAsphyrePixelFormat;

//---------------------------------------------------------------------------
function IsUsefulChannel(Index: TChannelTypeIndex): Boolean;
function GetChannelCount(const Info: TPixelFormatInfo): Integer;
function FindChannelAt(Index: TChannelTypeIndex;
 const Info: TPixelFormatInfo): Integer;
function FormatToStr(Format: TAsphyrePixelFormat): string;
function StrToFormat(const Text: string): TAsphyrePixelFormat;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure ResetFormatInfo(var Info: TPixelFormatInfo);
var
 i: Integer;
begin
 Info.NoChannels:= 0;

 for i:= 0 to High(Info.Channels) do
  begin
   Info.Channels[i].Index  := ctiUnknown;
   Info.Channels[i].Storage:= csUnknown;
   Info.Channels[i].Bits   := 0;
   Info.Channels[i].Pos    := -1;
  end;
end;

//---------------------------------------------------------------------------
function AddChannel(var Info: TPixelFormatInfo; Index: TChannelTypeIndex;
 Storage: TChannelStorage; Bits, Pos: Integer): Integer;
begin
 if (Info.NoChannels < 4) then
  begin
   Result:= Info.NoChannels;

   Info.Channels[Result].Index  := Index;
   Info.Channels[Result].Storage:= Storage;
   Info.Channels[Result].Bits   := Bits;
   Info.Channels[Result].Pos    := Pos;
   Inc(Info.NoChannels);
  end else Result:= -1;
end;

//---------------------------------------------------------------------------
function SameChannel(const Sample1, Sample2: TChannelInfo): Boolean;
begin
 Result:=
  (Sample1.Index = Sample2.Index)and
  (Sample1.Storage = Sample2.Storage)and
  (Sample1.Bits = Sample2.Bits)and
  (Sample1.Pos = Sample2.Pos);
end;

//---------------------------------------------------------------------------
function SameFormat(const Sample1, Sample2: TPixelFormatInfo): Boolean;
var
 j, i : Integer;
 Match: Boolean;
begin
 Result:= True;

 for j:= 0 to Sample1.NoChannels - 1 do
  begin
   Match:= False;

   for i:= 0 to Sample2.NoChannels - 1 do
    if (SameChannel(Sample1.Channels[j], Sample2.Channels[i])) then
     begin
      Match:= True;
      Break;
     end;

   if (not Match) then
    begin
     Result:= False;
     Break;
    end;
  end;
end;

//---------------------------------------------------------------------------
function GetPixelFormatInfo(Format: TAsphyrePixelFormat): TPixelFormatInfo;
begin
 ResetFormatInfo(Result);

 case Format of
  apf_R8G8B8:
   begin
    AddChannel(Result, ctiR, csUnsigned, 8, 16);
    AddChannel(Result, ctiG, csUnsigned, 8,  8);
    AddChannel(Result, ctiB, csUnsigned, 8,  0);
   end;

  apf_A8R8G8B8:
   begin
    AddChannel(Result, ctiA, csUnsigned, 8, 24);
    AddChannel(Result, ctiR, csUnsigned, 8, 16);
    AddChannel(Result, ctiG, csUnsigned, 8,  8);
    AddChannel(Result, ctiB, csUnsigned, 8,  0);
   end;

  apf_X8R8G8B8:
   begin
    AddChannel(Result, ctiX, csUnsigned, 8, 24);
    AddChannel(Result, ctiR, csUnsigned, 8, 16);
    AddChannel(Result, ctiG, csUnsigned, 8,  8);
    AddChannel(Result, ctiB, csUnsigned, 8,  0);
   end;

  apf_R5G6B5:
   begin
    AddChannel(Result, ctiR, csUnsigned, 5, 11);
    AddChannel(Result, ctiG, csUnsigned, 6,  5);
    AddChannel(Result, ctiB, csUnsigned, 5,  0);
   end;

  apf_X1R5G5B5:
   begin
    AddChannel(Result, ctiX, csUnsigned, 1, 15);
    AddChannel(Result, ctiR, csUnsigned, 5, 10);
    AddChannel(Result, ctiG, csUnsigned, 5,  5);
    AddChannel(Result, ctiB, csUnsigned, 5,  0);
   end;

  apf_A1R5G5B5:
   begin
    AddChannel(Result, ctiA, csUnsigned, 1, 15);
    AddChannel(Result, ctiR, csUnsigned, 5, 10);
    AddChannel(Result, ctiG, csUnsigned, 5,  5);
    AddChannel(Result, ctiB, csUnsigned, 5,  0);
   end;

  apf_A4R4G4B4:
   begin
    AddChannel(Result, ctiA, csUnsigned, 4, 12);
    AddChannel(Result, ctiR, csUnsigned, 4,  8);
    AddChannel(Result, ctiG, csUnsigned, 4,  4);
    AddChannel(Result, ctiB, csUnsigned, 4,  0);
   end;

  apf_R3G3B2:
   begin
    AddChannel(Result, ctiR, csUnsigned, 3, 5);
    AddChannel(Result, ctiG, csUnsigned, 3, 2);
    AddChannel(Result, ctiB, csUnsigned, 2, 0);
   end;

  apf_A8:
   AddChannel(Result, ctiA, csUnsigned, 8, 0);

  apf_A8R3G3B2:
   begin
    AddChannel(Result, ctiA, csUnsigned, 8, 8);
    AddChannel(Result, ctiR, csUnsigned, 3, 5);
    AddChannel(Result, ctiG, csUnsigned, 3, 2);
    AddChannel(Result, ctiB, csUnsigned, 2, 0);
   end;

  apf_X4R4G4B4:
   begin
    AddChannel(Result, ctiX, csUnsigned, 4, 12);
    AddChannel(Result, ctiR, csUnsigned, 4,  8);
    AddChannel(Result, ctiG, csUnsigned, 4,  4);
    AddChannel(Result, ctiB, csUnsigned, 4,  0);
   end;

  apf_A2B10G10R10:
   begin
    AddChannel(Result, ctiA, csUnsigned,  2, 30);
    AddChannel(Result, ctiB, csUnsigned, 10, 20);
    AddChannel(Result, ctiG, csUnsigned, 10, 10);
    AddChannel(Result, ctiR, csUnsigned, 10,  0);
   end;

  apf_G16R16:
   begin
    AddChannel(Result, ctiG, csUnsigned, 16, 16);
    AddChannel(Result, ctiR, csUnsigned, 16,  0);
   end;

  apf_A2R10G10B10:
   begin
    AddChannel(Result, ctiA, csUnsigned,  2, 30);
    AddChannel(Result, ctiR, csUnsigned, 10, 20);
    AddChannel(Result, ctiG, csUnsigned, 10, 10);
    AddChannel(Result, ctiB, csUnsigned, 10,  0);
   end;

  apf_A16B16G16R16:
   begin
    AddChannel(Result, ctiA, csUnsigned, 16, 48);
    AddChannel(Result, ctiB, csUnsigned, 16, 32);
    AddChannel(Result, ctiG, csUnsigned, 16, 16);
    AddChannel(Result, ctiR, csUnsigned, 16,  0);
   end;

  apf_L8:
   AddChannel(Result, ctiL, csUnsigned, 8, 0);

  apf_A8L8:
   begin
    AddChannel(Result, ctiA, csUnsigned, 8, 8);
    AddChannel(Result, ctiL, csUnsigned, 8, 0);
   end;

  apf_A4L4:
   begin
    AddChannel(Result, ctiA, csUnsigned, 4, 4);
    AddChannel(Result, ctiL, csUnsigned, 4, 0);
   end;

  apf_V8U8:
   begin
    AddChannel(Result, ctiV, csSigned, 8, 8);
    AddChannel(Result, ctiU, csSigned, 8, 0);
   end;

  apf_L6V5U5:
   begin
    AddChannel(Result, ctiL, csUnsigned, 6, 10);
    AddChannel(Result, ctiV,   csSigned, 5,  5);
    AddChannel(Result, ctiU,   csSigned, 5,  0);
   end;

  apf_X8L8V8U8:
   begin
    AddChannel(Result, ctiX, csUnsigned, 8, 24);
    AddChannel(Result, ctiL, csUnsigned, 8, 16);
    AddChannel(Result, ctiV,   csSigned, 8,  8);
    AddChannel(Result, ctiU,   csSigned, 8,  0);
   end;

  apf_Q8W8V8U8:
   begin
    AddChannel(Result, ctiQ, csSigned, 8, 24);
    AddChannel(Result, ctiW, csSigned, 8, 16);
    AddChannel(Result, ctiV, csSigned, 8,  8);
    AddChannel(Result, ctiU, csSigned, 8,  0);
   end;

  apf_V16U16:
   begin
    AddChannel(Result, ctiV, csSigned, 16, 16);
    AddChannel(Result, ctiU, csSigned, 16,  0);
   end;

  apf_A2W10V10U10:
   begin
    AddChannel(Result, ctiA, csUnsigned,  2, 30);
    AddChannel(Result, ctiW,   csSigned, 10, 20);
    AddChannel(Result, ctiV,   csSigned, 10, 10);
    AddChannel(Result, ctiU,   csSigned, 10,  0);
   end;

  apf_L16:
   AddChannel(Result, ctiL, csUnsigned, 16, 0);

  apf_Q16W16V16U16:
   begin
    AddChannel(Result, ctiQ, csSigned, 16, 48);
    AddChannel(Result, ctiW, csSigned, 16, 32);
    AddChannel(Result, ctiV, csSigned, 16, 16);
    AddChannel(Result, ctiU, csSigned, 16,  0);
   end;

  apf_R16F:
   AddChannel(Result, ctiR, csFloat, 16, 0);

  apf_G16R16F:
   begin
    AddChannel(Result, ctiG, csFloat, 16, 16);
    AddChannel(Result, ctiR, csFloat, 16,  0);
   end;

  apf_A16B16G16R16F:
   begin
    AddChannel(Result, ctiA, csFloat, 16, 48);
    AddChannel(Result, ctiB, csFloat, 16, 32);
    AddChannel(Result, ctiG, csFloat, 16, 16);
    AddChannel(Result, ctiR, csFloat, 16,  0);
   end;

  apf_R32F:
   AddChannel(Result, ctiR, csFloat, 32, 0);

  apf_G32R32F:
   begin
    AddChannel(Result, ctiG, csFloat, 32, 32);
    AddChannel(Result, ctiR, csFloat, 32,  0);
   end;

  apf_A32B32G32R32F:
   begin
    AddChannel(Result, ctiA, csFloat, 32, 96);
    AddChannel(Result, ctiB, csFloat, 32, 64);
    AddChannel(Result, ctiG, csFloat, 32, 32);
    AddChannel(Result, ctiR, csFloat, 32,  0);
   end;

  apf_A8B8G8R8:
   begin
    AddChannel(Result, ctiA, csUnsigned, 8, 24);
    AddChannel(Result, ctiB, csUnsigned, 8, 16);
    AddChannel(Result, ctiG, csUnsigned, 8,  8);
    AddChannel(Result, ctiR, csUnsigned, 8,  0);
   end;

  apf_X8B8G8R8:
   begin
    AddChannel(Result, ctiX, csUnsigned, 8, 24);
    AddChannel(Result, ctiB, csUnsigned, 8, 16);
    AddChannel(Result, ctiG, csUnsigned, 8,  8);
    AddChannel(Result, ctiR, csUnsigned, 8,  0);
   end;

  apf_A8X8V8U8:
   begin
    AddChannel(Result, ctiA, csUnsigned, 8, 24);
    AddChannel(Result, ctiX, csUnsigned, 8, 16);
    AddChannel(Result, ctiV, csSigned,   8,  8);
    AddChannel(Result, ctiU, csSigned,   8,  0);
   end;

  apf_L8X8V8U8:
   begin
    AddChannel(Result, ctiL, csUnsigned, 8, 24);
    AddChannel(Result, ctiX, csUnsigned, 8, 16);
    AddChannel(Result, ctiV, csSigned,   8,  8);
    AddChannel(Result, ctiU, csSigned,   8,  0);
   end;

  apf_A6L2:
   begin
    AddChannel(Result, ctiA, csUnsigned, 6, 2);
    AddChannel(Result, ctiL, csUnsigned, 2, 0);
   end;

  apf_A2R2G2B2:
   begin
    AddChannel(Result, ctiA, csUnsigned, 2, 6);
    AddChannel(Result, ctiR, csUnsigned, 2, 4);
    AddChannel(Result, ctiG, csUnsigned, 2, 2);
    AddChannel(Result, ctiB, csUnsigned, 2, 0);
   end;

  apf_A5R9G9B9:
   begin
    AddChannel(Result, ctiA, csUnsigned, 5, 27);
    AddChannel(Result, ctiR, csUnsigned, 9, 18);
    AddChannel(Result, ctiG, csUnsigned, 9,  9);
    AddChannel(Result, ctiB, csUnsigned, 9,  0);
   end;
 end;
end;

//---------------------------------------------------------------------------
function InfoToPixelFormat(const Info: TPixelFormatInfo): TAsphyrePixelFormat;
var
 Sample: TAsphyrePixelFormat;
 SaInfo: TPixelFormatInfo;
begin
 Result:= apf_Unknown;

 for Sample:= Low(TAsphyrePixelFormat) to High(TAsphyrePixelFormat) do
  begin
   SaInfo:= GetPixelFormatInfo(Sample);
   if (SameFormat(Info, SaInfo)) then
    begin
     Result:= Sample;
     Break;
    end;
  end;
end;

//---------------------------------------------------------------------------
function IsUsefulChannel(Index: TChannelTypeIndex): Boolean;
begin
 Result:= Index in [ctiR, ctiG, ctiB, ctiA, ctiL, ctiU, ctiV, ctiW, ctiQ];
end;

//---------------------------------------------------------------------------
function GetChannelCount(const Info: TPixelFormatInfo): Integer;
var
 i: Integer;
begin
 Result:= 0;

 for i:= 0 to Info.NoChannels - 1 do
  if (IsUsefulChannel(Info.Channels[i].Index)) then Inc(Result);
end;

//---------------------------------------------------------------------------
function FindChannelAt(Index: TChannelTypeIndex;
 const Info: TPixelFormatInfo): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Info.NoChannels - 1 do
  if (Info.Channels[i].Index = Index) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function FormatToStrGeneric(Format: TAsphyrePixelFormat): string;
var
 Info : TPixelFormatInfo;
 Index: TChannelTypeIndex;
 Added: Boolean;
 Float: Boolean;
 i: Integer;
begin
 Result:= 'apf_';

 Info := GetPixelFormatInfo(Format);
 Added:= False;
 Float:= False;

 for i:= 0 to Info.NoChannels - 1 do
  begin
   Index:= Info.Channels[i].Index;

   if (Index <> ctiUnknown) then
    begin
     Result:= Result + ChannelTextLetter[Index] +
      IntToStr(Info.Channels[i].Bits);

     if (Info.Channels[i].Storage = csFloat) then
      Float:= True;

     Added:= True;
    end;
  end;

 if (Float) then Result:= Result + 'F';
 if (not Added) then Result:= Result + 'Unknown';
end;

//---------------------------------------------------------------------------
function FormatToStr(Format: TAsphyrePixelFormat): string;
begin
 case Format of
  apf_UYVY:
   Result:= 'apf_UYVY';

  apf_R8G8_B8G8:
   Result:= 'apf_R8G8_B8G8';

  apf_YUY2:
   Result:= 'apf_YUY2';

  apf_G8R8_G8B8:
   Result:= 'apf_G8R8_G8B8';

  apf_DXT1:
   Result:= 'apf_DXT1';

  apf_DXT2:
   Result:= 'apf_DXT2';

  apf_DXT3:
   Result:= 'apf_DXT3';

  apf_DXT4:
   Result:= 'apf_DXT4';

  apf_DXT5:
   Result:= 'apf_DXT5';

  apf_CxV8U8:
   Result:= 'apf_CxV8U8';

  else Result:= FormatToStrGeneric(Format);
 end;
end;

//---------------------------------------------------------------------------
function StrToFormat(const Text: string): TAsphyrePixelFormat;
var
 HiText: string;
begin
 Result:= apf_Unknown;

 HiText:= UpperCase(Text);

 if (Pos('APF_', HiText) = 1) then Delete(HiText, 1, 4);
 if (Pos('D3DFMT_', HiText) = 1) then Delete(HiText, 1, 7);
 if (Pos('COLOR_', HiText) = 1) then Delete(HiText, 1, 6);

 if (HiText = 'R8G8B8') then Result:= apf_R8G8B8;
 if (HiText = 'A8R8G8B8') then Result:= apf_A8R8G8B8;
 if (HiText = 'X8R8G8B8') then Result:= apf_X8R8G8B8;
 if (HiText = 'R5G6B5') then Result:= apf_R5G6B5;
 if (HiText = 'X1R5G5B5') then Result:= apf_X1R5G5B5;
 if (HiText = 'A1R5G5B5') then Result:= apf_A1R5G5B5;
 if (HiText = 'A4R4G4B4') then Result:= apf_A4R4G4B4;
 if (HiText = 'R3G3B2') then Result:= apf_R3G3B2;
 if (HiText = 'A8') then Result:= apf_A8;
 if (HiText = 'A8R3G3B2') then Result:= apf_A8R3G3B2;
 if (HiText = 'X4R4G4B4') then Result:= apf_X4R4G4B4;
 if (HiText = 'A2B10G10R10') then Result:= apf_A2B10G10R10;
 if (HiText = 'G16R16') then Result:= apf_G16R16;
 if (HiText = 'A2R10G10B10') then Result:= apf_A2R10G10B10;
 if (HiText = 'A16B16G16R16') then Result:= apf_A16B16G16R16;
 if (HiText = 'L8') then Result:= apf_L8;
 if (HiText = 'A8L8') then Result:= apf_A8L8;
 if (HiText = 'A4L4') then Result:= apf_A4L4;
 if (HiText = 'V8U8') then Result:= apf_V8U8;
 if (HiText = 'L6V5U5') then Result:= apf_L6V5U5;
 if (HiText = 'X8L8V8U8') then Result:= apf_X8L8V8U8;
 if (HiText = 'Q8W8V8U8') then Result:= apf_Q8W8V8U8;
 if (HiText = 'V16U16') then Result:= apf_V16U16;
 if (HiText = 'A2W10V10U10') then Result:= apf_A2W10V10U10;
 if (HiText = 'UYVY') then Result:= apf_UYVY;
 if (HiText = 'R8G8_B8G8') then Result:= apf_R8G8_B8G8;
 if (HiText = 'YUY2') then Result:= apf_YUY2;
 if (HiText = 'G8R8_G8B8') then Result:= apf_G8R8_G8B8;
 if (HiText = 'DXT1') then Result:= apf_DXT1;
 if (HiText = 'DXT2') then Result:= apf_DXT2;
 if (HiText = 'DXT3') then Result:= apf_DXT3;
 if (HiText = 'DXT4') then Result:= apf_DXT4;
 if (HiText = 'DXT5') then Result:= apf_DXT5;
 if (HiText = 'L16') then Result:= apf_L16;
 if (HiText = 'Q16W16V16U16') then Result:= apf_Q16W16V16U16;
 if (HiText = 'R16F') then Result:= apf_R16F;
 if (HiText = 'G16R16F') then Result:= apf_G16R16F;
 if (HiText = 'A16B16G16R16F') then Result:= apf_A16B16G16R16F;
 if (HiText = 'R32F') then Result:= apf_R32F;
 if (HiText = 'G32R32F') then Result:= apf_G32R32F;
 if (HiText = 'A32B32G32R32F') then Result:= apf_A32B32G32R32F;
 if (HiText = 'CxV8U8') then Result:= apf_CxV8U8;
 if (HiText = 'A8B8G8R8') then Result:= apf_A8B8G8R8;
 if (HiText = 'X8B8G8R8') then Result:= apf_X8B8G8R8;
 if (HiText = 'A6L2') then Result:= apf_A6L2;
 if (HiText = 'A2R2G2B') then Result:= apf_A2R2G2B2;
 if (HiText = 'A5R9G9B9') then Result:= apf_A5R9G9B9;
end;

//---------------------------------------------------------------------------
end.
