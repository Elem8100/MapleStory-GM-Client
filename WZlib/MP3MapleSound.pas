unit MP3MapleSound;

interface

uses
  Classes, SysUtils, WZReader;

type
  TPCMWaveFormat = record
    FormatTag: Word;
    Channels: Word;
    SamplesPerSec: Cardinal;
    AvgBytesPerSec: Cardinal;
    BlockAlign: Word;
    BitsPerSample: Word;
  end;

  TWZSound = class
  private
    FDataLength, FDuration, FHeaderLength: Integer;
    FPCMFormat: TPCMWaveFormat;
    FOffset: Int64;
    FWZReader: TWZReader;
    function GetIsMP3: Boolean;
  public
    constructor Create(DataLength, Duration, HeaderLength: Integer; WZReader: TWZReader);
    function Dump: TMemoryStream;
    function ToString: string; override;
    property Offset: Int64 read FOffset;
    property DataLength: Integer read FDataLength;
    property Duration: Integer read FDuration;
    property IsMP3: Boolean read GetIsMP3;
    property PCMFormat: TPCMWaveFormat read FPCMFormat;
  end;

implementation

{ TWZSound }

constructor TWZSound.Create(DataLength, Duration, HeaderLength: Integer; WZReader: TWZReader);
begin
  FDataLength := DataLength;
  FDuration := Duration;
  FHeaderLength := HeaderLength;
  FWZReader := WZReader;

  FOffset := FWZReader.Position + FHeaderLength;

  if FHeaderLength = 70 then
  begin
    FWZReader.Seek(52, soCurrent);
    FWZReader.Stream.Read(FPCMFormat, 16);
  end;
end;

function TWZSound.Dump: TMemoryStream;
begin
  FWZReader.Seek(FOffset, soBeginning);
  Result := TMemoryStream.Create;
  Result.CopyFrom(FWZReader.Stream, FDataLength);
end;

function TWZSound.GetIsMP3: Boolean;
begin
  // MP3 is mostly 82, but not always
  // BgmGL.img/Amorianchallenge is 51 in earlier GMS versions and has an ID3 tag lol
  Result := FHeaderLength <> 70;
end;

function TWZSound.ToString: string;
var
  Fmt: string;
begin
  if IsMP3 then
    Fmt := 'MP3'
  else if FPCMFormat.FormatTag = 1 then
    Fmt := SysUtils.Format('PCM %d Hz %d Ch.', [FPCMFormat.SamplesPerSec, FPCMFormat.Channels])
  else
    Fmt := SysUtils.Format('UNK %d Hz %d Ch.', [FPCMFormat.SamplesPerSec, FPCMFormat.Channels]);

  Result := SysUtils.Format('Sound [%s, %d ms, %d bytes]', [Fmt, FDuration, FDataLength]);
end;

end.

