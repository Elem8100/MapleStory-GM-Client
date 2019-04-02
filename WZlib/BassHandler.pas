unit BassHandler;

interface

uses Windows, Classes, SysUtils, Bass;

type
  TBassHandler = class
  private
    MWND: HSTREAM;
    MS: TMemoryStream;
    FStart: Boolean;
   //procedure BassInit;
    function GetIsPlaying: Boolean;
    function GetPosition: Double;
    function GetLength: Double;
  public
    constructor Create(Media: TMemoryStream); overload;
    constructor Create(Media: TStream; Offset, Size: Int64); overload;
    destructor Destroy; override;

    procedure Play;
    procedure PlayLoop;
    property Position: Double read GetPosition;
    property Length: Double read GetLength;
    property IsPlaying: Boolean read GetIsPlaying;
    property Start: Boolean read FStart write FStart;
  end;

  procedure BassInit;
  procedure BassFree;

implementation

{ TBassHandler }

procedure BassInit;
begin
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
    raise Exception.Create('Bass.dll version not supported!');

  if not BASS_Init(-1, 44100, 0, 0, nil) then
    raise Exception.Create('Bass.dll couldn''t be initialized!');
end;

procedure BassFree;
begin
  BASS_Free;
end;

constructor TBassHandler.Create(Media: TMemoryStream);
begin
 // BassInit;

  MS := Media;

  MWND := BASS_StreamCreateFile(True, MS.Memory, 0, MS.Size, BASS_STREAM_PRESCAN);

end;

constructor TBassHandler.Create(Media: TStream; Offset, Size: Int64);
{ Direct playing from archive, faster than first dumping the Media }
begin
 // BassInit;

  MS := TMemoryStream.Create;
  Media.Seek(Offset, soBeginning);
  MS.CopyFrom(Media, Size);

  MWND := BASS_StreamCreateFile(True, MS.Memory, Offset, Size, BASS_STREAM_PRESCAN);
end;

destructor TBassHandler.Destroy;
begin
  BASS_ChannelStop(MWND);
  BASS_StreamFree(MWND);
  //BASS_Free;
  FreeAndNil(MS);
  MWND := 0;

  inherited;
end;

procedure TBassHandler.Play;
begin
  BASS_ChannelPlay(MWND, true);
end;

procedure TBassHandler.PlayLoop;
begin
  BASS_ChannelPlay(MWND, False);
  BASS_ChannelFlags(MWND,BASS_SAMPLE_LOOP , BASS_SAMPLE_LOOP);
end;

function TBassHandler.GetLength: Double;
var
  ByteLen: Int64;
  Seconds: Double;
begin
  ByteLen := BASS_ChannelGetLength(MWND, BASS_POS_BYTE);
  if ByteLen = -1 then
    raise Exception.Create('BASS_ERROR: ' + IntToStr(BASS_ErrorGetCode));

  Seconds := BASS_ChannelBytes2Seconds(MWND, ByteLen);
  Result := Seconds;
end;

function TBassHandler.GetPosition: Double;
var
  BytePos: Int64;
  Seconds: Double;
begin
  BytePos := BASS_ChannelGetPosition(MWND, BASS_POS_BYTE);
  if BytePos = -1 then
    raise Exception.Create('BASS_ERROR: ' + IntToStr(BASS_ErrorGetCode));

  Seconds := BASS_ChannelBytes2Seconds(MWND, BytePos);
  Result := Seconds;
end;

function TBassHandler.GetIsPlaying;
begin
  Result := False;
  if BASS_ChannelIsActive(MWND) = BASS_ACTIVE_PLAYING then
    Result:= True;

end;

end.
