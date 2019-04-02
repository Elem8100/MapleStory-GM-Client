unit MP3MapleSound;

interface

uses Classes, SysUtils, WZReader;

type
  TMP3MapleSound = class
  private
    FDataLength: Integer;
    FOffset: Int64;
    FWZReader: TWZReader;
  public
    constructor Create(DataLength, Offset: Integer; var WZReader: TWZReader);

    function Dump: TMemoryStream;

    property Offset: Int64 read FOffset;
    property DataLength: Integer read FDataLength;
  end;

implementation

{ TMP3MapleSound }

constructor TMP3MapleSound.Create(DataLength, Offset: Integer; var WZReader: TWZReader);
begin
  FDataLength := DataLength;
  FOffset := Offset;
  FWZReader := WZReader;
end;

function TMP3MapleSound.Dump: TMemoryStream;
begin
  if FWZReader = nil then
    raise Exception.Create('WZReader instance isn''t active anymore');

  FWZReader.Seek(FOffset, soBeginning);
  Result := TMemoryStream.Create;
  Result.CopyFrom(FWZReader.Stream, FDataLength);
end;

end.
