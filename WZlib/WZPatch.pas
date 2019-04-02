unit WZPatch;

interface

uses Classes, SysUtils, ZLib, Dialogs;

type
  TWZPatch = class
  private
    FPatch: TFileStream;
    FChecksum: Cardinal;

    function VerifyHeader: Boolean;

    procedure ParseType1(Decomp: TMemoryStream);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    function ProcessZLib: TMemoryStream;
    procedure ParseContents(Decomp: TMemoryStream);
  end;

implementation

{ TWZPatch }

constructor TWZPatch.Create(const FileName: string);
begin
  FPatch := TFileStream.Create(FileName, fmOpenRead);

  if not VerifyHeader then
    raise Exception.Create('Patch file header incorrect!');
end;

destructor TWZPatch.Destroy;
begin
  FreeAndNil(FPatch);

  inherited;
end;

function TWZPatch.VerifyHeader: Boolean;
const
  CorrectHeader = 'WzPatch'#26#2#0#0#0;
var
  Header: AnsiString;
begin
  SetLength(Header, Length(CorrectHeader));
  FPatch.Read(Header[1], Length(CorrectHeader));

  Result := Header = CorrectHeader;
end;

procedure TWZPatch.ParseContents(Decomp: TMemoryStream);
var
  FN: string;
  FNList: TStringList;
  b: Byte;
begin
  FNList := TStringList.Create;

  Decomp.Position := 0;
  while Decomp.Position < Decomp.Size do
  begin
    Decomp.Read(b, 1);
    case b of
      0, 2:
      begin
        Showmessage('Types 0 & 2 can''t be parsed yet');
        Exit;
      end;

      1:
      begin
        ParseType1(Decomp);
        FNList.Add(FN);
        FN := '';
      end;

      else FN := FN + Chr(b);
    end;
  end;

  Showmessage(FNList.Text);
end;

procedure TWZPatch.ParseType1(Decomp: TMemoryStream);
var
  OldSum, NewSum, Command, LengthOfBlock, BaseOffset: Cardinal;
begin
  Decomp.Read(OldSum, 4);
  Decomp.Read(NewSum, 4);

  Command := 1;
  while Command <> 0 do
  begin
    Decomp.Read(Command, 4);

    case Command shr 24 of
      $80:  // Write
      begin
        LengthOfBlock := Command and $07FFFFFF;
        Decomp.Seek(LengthOfBlock, soCurrent);
      end;

      $C0:  // Insert multiple bytes
      begin
        LengthOfBlock := (Command shr 8) and $3FFFFF;
      end;

      // Read
      else if Command <> 0 then
      begin
        LengthOfBlock := Command;
        Decomp.Read(BaseOffset, 4);
      end;
    end;
  end;
end;

function TWZPatch.ProcessZLib: TMemoryStream;
var
  ZDecomp: TZDecompressionStream;
  Src: TMemoryStream;
begin
  FPatch.Read(FChecksum, 4);

  Src := TMemoryStream.Create;
  try
    Src.CopyFrom(FPatch, FPatch.Size - FPatch.Position);
    ZDecomp := TZDecompressionStream.Create(Src);
    try
      Result := TMemoryStream.Create;
      Result.CopyFrom(ZDecomp, 0);
    finally
      ZDecomp.Free;
    end;
  finally
    Src.Free;
  end;
end;

end.
