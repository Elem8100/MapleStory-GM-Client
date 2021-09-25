unit Morph;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, WZIMGFile,
  Classes, Global, WzUtils;

type
  TMorph = class(TSpriteEx)
  public
    UpPath: string;
    Path: string;
    State: string;
    Frame: Integer;
    MorphNum: string;
    FTime: Integer;
    Delay: Integer;
    Flip: Integer;
    Origin: TPoint;
    class var
      Entry: TWZIMGEntry;
      IsUse: Boolean;
    procedure DoMove(const MoveCount: Single); override;
    class procedure Delete;
    class procedure Create(MorphNum: string); overload;
  end;

implementation

uses
  MapleCharacter, Footholds,
  AsphyreKeyboard, DirectInput, WZDirectory;

function IDToInt(ID: string): string;
begin
  var S := ID.ToInteger;
  Result := S.ToString;
end;

class procedure TMorph.Delete;
begin
  for var Iter in SpriteEngine.SpriteList do
    if Iter is TMorph then
      Iter.Dead;

  for var Iter in EquipImages.Keys do
    if LeftStr(Iter.GetPath, 8)= 'Morph'  then
    begin
       EquipImages.Remove(Iter);
       EquipData.Remove(Iter.GetPath);
    end;

end;


class procedure TMorph.Create(MorphNum: string);
begin
  Entry := GetImgEntry('Morph/' + MorphNum + '/');
  DumpData(Entry, EquipData, EquipImages);

  for var Iter in EquipData[Entry.GetPath].Children do
    for var Iter2 in EquipData[Iter.GetPath].Children do
      if (Iter2.Name[1] in ['0'..'9']) then
      begin
        if (Iter.Name = 'walk') and (Iter2.Name = '0') then
          with TMorph.Create(SpriteEngine) do
          begin
            ImageLib := EquipImages;
            TruncMove := True;
            Tag := 1;
            State := Iter.Name;
            Frame := Iter2.Name.ToInteger;
            MorphNum := LeftStr(Entry.Name, 4);
            UpPath := Entry.GetPath;
            ImageEntry := EquipData[Iter2.GetPath];
          end;
      end;

end;

procedure TMorph.DoMove(const MoveCount: Single);
begin
  inherited;


  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString) then
  begin
    Path := UpPath + '/' + State + '/' + Frame.ToString;
    ImageEntry := EquipData[Path];
    //Visible := True;
  end;
 // else
   // Visible := False;

  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString + '/delay') then
    Delay := EquipData[UpPath + '/' + State + '/' + Frame.ToString + '/delay'].Data
  else
    Delay := 100;


  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not HasEntryE(UpPath + '/' + State + '/' + Frame.ToString) then
      Frame := 0;
    FTime := 0;
  end;

  if HasEntryE(Path + '/z') then
    Z := Player.Z - Integer(EquipData[Path + '/z'].Data)
  else
    Z := Player.Z;

 // MirrorX := Player.MirrorX;
  if (Keyboard.Key[DIK_LEFT]) then
   Mirrorx:=False;
  if  (Keyboard.Key[DIK_RIGHT])  then
   Mirrorx:=True;
  if (Player.JumpState <> jsNone) then
  begin
    Frame := 0;
    State := 'jump';
  end;

  if Player.JumpState = jsNone then
  begin
    State := 'stand';
    if (Keyboard.Key[DIK_LEFT]) or (Keyboard.Key[DIK_RIGHT]) then
      State := 'walk';
    if Keyboard.Key[DIK_DOWN] then
      State := 'prone';
    if (State = 'prone') and (Keyboard.KeyReleased[DIK_DOWN]) then
      State := 'stand';
    if (Keyboard.KeyReleased[DIK_LEFT]) or (Keyboard.KeyReleased[DIK_RIGHT]) then
      State := 'stand';
  end;


  if (Player.InLadder) then
  begin
    case Player.LadderType of
      rtLadder:
        begin
          if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
            State := 'ladder'
          else
          begin
            State := 'ladder';
            Frame := 0;
          end;
        end;

      rtRope:
        begin
          if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
            State := 'rope'
          else
          begin
            State := 'rope';
            Frame := 0;
          end;
        end;
    end;
  end;


  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  Y := Trunc(Player.Y);
  case MirrorX of
    True:
      begin
        X := Trunc(Player.X + 1);
        Offset.X := Origin.X - PatternWidth;
        Flip := -1;
      end;
    False:
      begin
        X := Trunc(Player.X);
        Offset.X := -Origin.X;
        Flip := 1;
      end;
  end;
  Offset.Y := -Origin.Y;

end;

end.

