unit KeyHandler;

interface

uses Classes, SysUtils, DECCipher;

const
  KEY: array[0..31] of Byte = (
    $13, $00, $00, $00, $08, $00, $00, $00, $06, $00, $00, $00, $B4, $00, $00, $00,
    $1B, $00, $00, $00, $0F, $00, $00, $00, $33, $00, $00, $00, $52, $00, $00, $00);

  GMS_IV = $2BC7234D;
  GENERAL_IV = $E9637DB9;

type
  TKeyCreator = class
  private
    FCipher: TCipher_Rijndael;
  public
    constructor Create;
    destructor Destroy; override;

    function GetKey(IV: Cardinal): TBytes;
  end;

var
  KeyCreator: TKeyCreator;  // Singleton

implementation

{ TKeyCreator }

function dupe32(V, Count: Cardinal): TBytes;
var
  x: Integer;
begin
  SetLength(Result, Count * 4);
  for x := 0 to Count - 1 do
    PCardinal(@Result[x * 4])^ := V;
end;

constructor TKeyCreator.Create;
const
  a: Pointer = nil;
begin
  FCipher := TCipher_Rijndael.Create;
  FCipher.Mode := cmECBx;
  FCipher.Init(KEY, 32, a, 0);
end;

destructor TKeyCreator.Destroy;
begin
  FCipher.Free;

  inherited;
end;

function TKeyCreator.GetKey(IV: Cardinal): TBytes;
var
  Buffer: TBytes;
  i: Integer;
begin
  SetLength(Result, $FFFF);
  Buffer := dupe32(IV, 4);

  for i := 0 to (Length(Result) div 16) - 1 do
  begin
    FCipher.Encode(Buffer[0], Buffer[0], 16);
    Move(Buffer[0], Result[i * 16], 16);
    FCipher.Done;
  end;

  // Last bytes
  FCipher.Encode(Buffer[0], Buffer[0], 16);
  Move(Buffer[0], Result[Length(Result) - 15], 15);
  FCipher.Done;

  Buffer := nil;
end;

initialization
  KeyCreator := TKeyCreator.Create;

finalization
  KeyCreator.Free;

end.

