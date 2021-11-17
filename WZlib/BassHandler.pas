unit BassHandler;

interface

uses
  Windows, Classes, SysUtils, Bass,MP3MapleSound;

type
  TBassHandler = class
  private
    FChannel: HCHANNEL;
    MS: TMemoryStream;
    FSample: HSAMPLE;
    FStream: HSTREAM;
    FStart: Boolean;
    Volume: FLOAT;
   //procedure BassInit;
    function GetIsPlaying: Boolean;
    function GetPosition: Double;
    function GetLength: Double;
  public
    constructor Create(Sound: TWZSound);
    destructor Destroy; override;
    procedure Play;
    procedure PlayLoop;
    procedure Mute;
    procedure ReStart;
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
  if (HiWord(BASS_GetVersion) <> BASSVERSION) then
    raise Exception.Create('Bass.dll version not supported!');

  if not BASS_Init(-1, 44100, 0, 0, nil) then
    raise Exception.Create('Bass.dll couldn''t be initialized!');
end;

procedure BassFree;
begin
  BASS_Free;
end;


constructor TBassHandler.Create(Sound: TWZSound);
begin
 // BassInit;

  MS := Sound.Dump;

  if Sound.IsMP3 then
  begin
    FStream := BASS_StreamCreateFile(True, MS.Memory, 0, MS.Size, BASS_STREAM_PRESCAN);
    FChannel := FStream;
  end
  else if Sound.PCMFormat.FormatTag = 1 then // PCM
  begin
    FSample := BASS_SampleCreate(MS.Size, Sound.PCMFormat.SamplesPerSec, Sound.PCMFormat.Channels, 1, 0);
    if FSample = 0 then
      raise Exception.Create('Creating BASS sample failed');

    BASS_SampleSetData(FSample, MS.Memory);
    FChannel := BASS_SampleGetChannel(FSample, False);
    FStream := 0;
  end
  else
  begin
    raise Exception.Create('Unsupported format: ' + IntToStr(Sound.PCMFormat.FormatTag));
  end;
end;

destructor TBassHandler.Destroy;
begin
  BASS_ChannelStop(FChannel);
  if FStream <> 0 then
    BASS_StreamFree(FStream)
  else
    BASS_SampleFree(FSample);
  // BASS_Free;
  MS.Free;
  inherited;
end;

procedure TBassHandler.Play;
begin
  BASS_ChannelPlay(FChannel, True);
end;

procedure TBassHandler.PlayLoop;
begin
  BASS_ChannelPlay(FChannel, False);
  BASS_ChannelFlags(FChannel, BASS_SAMPLE_LOOP, BASS_SAMPLE_LOOP);
end;

procedure TBassHandler.Mute;
begin
  BASS_Pause;
end;

procedure TBassHandler.ReStart;
begin
  BASS_Start;
  PlayLoop;
end;

function TBassHandler.GetLength: Double;
var
  ByteLen: Int64;
begin
  ByteLen := BASS_ChannelGetLength(FChannel, BASS_POS_BYTE);
  if ByteLen = -1 then
    raise Exception.Create('BASS_ERROR: ' + IntToStr(BASS_ErrorGetCode));

  Result := BASS_ChannelBytes2Seconds(FChannel, ByteLen);
end;

function TBassHandler.GetPosition: Double;
var
  BytePos: Int64;
begin
  BytePos := BASS_ChannelGetPosition(FChannel, BASS_POS_BYTE);
  if BytePos = -1 then
    raise Exception.Create('BASS_ERROR: ' + IntToStr(BASS_ErrorGetCode));

  Result := BASS_ChannelBytes2Seconds(FChannel, BytePos);
end;


function TBassHandler.GetIsPlaying;
begin
  Result := False;
  if BASS_ChannelIsActive(FChannel) = BASS_ACTIVE_PLAYING then
    Result := True;

end;

end.

