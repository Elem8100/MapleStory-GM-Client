unit Tools;

interface

uses SysUtils;

type
  TStringArray = array of string;

function Explode(const Separator, s: string; Limit: Integer = 0): TStringArray;

implementation

function Explode(const Separator, s: string; Limit: Integer = 0): TStringArray;
var
  SepLen: Integer;
  F, P: PChar;
  ALen, Index: Integer;
begin
  SetLength(Result, 0);
  if (S = '') or (Limit < 0) then Exit;
  if Separator = '' then
  begin
    SetLength(Result, 1);
    Result[0] := S;
    Exit;
  end;
  SepLen := Length(Separator);
  ALen := Limit;
  SetLength(Result, ALen);


  Index := 0;
  P := PChar(S);
  while P^ <> #0 do
  begin
    F := P;
    P := StrPos(P, PChar(Separator));
    if (P = nil) or ((Limit > 0) and (Index = Limit - 1)) then P := StrEnd(F);
    if Index >= ALen then
    begin
      Inc(ALen, 5); // mehrere auf einmal um schneller arbeiten zu können
      SetLength(Result, ALen);
    end;
    SetString(Result[Index], F, P - F);
    Inc(Index);
    if P^ <> #0 then Inc(P, SepLen);
  end;
  if Index < ALen then SetLength(Result, Index); // wirkliche Länge festlegen
end;

end.
