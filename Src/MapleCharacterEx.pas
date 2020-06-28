unit MapleCharacterEx;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, Math, PXT.Sprites, Footholds, LadderRopes,
  ChatBalloon, Classes, Global, Tools, MapleMap, MapleCharacter;

type
  TMoveDirection = (mdLeft, mdRight, mdNone, mdNone2);

  TMoveType = (mtStand, mtMove, mtJump, mtFly);

  TPlayerEx = class(TPlayer)
  private
    FJumpCount: Integer;
    FJumpSpeed: Single;
    FJumpHeight: Single;
    FMaxFallSpeed: Single;
    FDoJump: Boolean;
    FJumpState: TJumpState;
    FVelocityY: Single;
    procedure SetJumpState(Value: TJumpState);
  public
    MoveDirection: TMoveDirection;
    MoveSpeed: Single;
    JumpEdge: Integer;
    MoveType: TMoveType;
    property VelocityY: Single read FVelocityY write FVelocityY;
    property JumpCount: Integer read FJumpCount write FJumpCount;
    property JumpState: TJumpState read FJumpState write SetJumpState;
    property JumpSpeed: Single read FJumpSpeed write FJumpSpeed;
    property JumpHeight: Single read FJumpHeight write FJumpHeight;
    property MaxFallSpeed: Single read FMaxFallSpeed write FMaxFallSpeed;
    property DoJump: Boolean read FDoJump write FDoJump;
    procedure UpdateJump;
    class procedure Spawn(IDList: string);
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

  TAvatarPartEx = class(TAvatarParts)
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

var
  PlayerExList: TList<TPlayerEx>;

implementation

uses
  MapleChair, WZIMGFile, WzUtils,PXT.TypesEx;

class procedure TPlayerEx.Spawn(IDList: string);
var
  BelowFH: TFoothold;
  Below: TPoint;
begin
  Inc(TPlayer._NewZ);
  var PlayerEx := TPlayerEx.Create(SpriteEngine);
  PlayerEx.ImageLib := EquipImages;
  PlayerEx.NewZ := TPlayer._NewZ;
  PlayerEx.OtherPlayer := True;
  PlayerEx.X := Player.X;
  PlayerEx.Y := Player.Y - 100;
  Below := TFootholdTree.This.FindBelow(Point(Round(Player.X), Round(Player.Y) - 2), BelowFH);
  PlayerEx.FH := BelowFH;
  PlayerEx.JumpSpeed := 0.6;
  PlayerEx.JumpHeight := 9.5;
  PlayerEx.MaxFallSpeed := 8;
  PlayerEx.JumpState := jsFalling;
  PlayerEx.MoveSpeed := 1.8;
  PlayerEx.MoveType := mtJump;
  PlayerEx.MoveDirection := mdNone;
  PlayerExList.Add(PlayerEx);
  var Explode: TArray<string>;
  Explode := IDList.Split(['-']);

  var List := TList<string>.Create;
  for var i := 0 to High(Explode) - 1 do
    List.Add(Explode[i]);
  List.Sort;

  for var I := 0 to List.Count - 1 do
    PlayerEx.CreateEquip(List[i], SpriteEngine);
  List.Free;
end;

procedure TPlayerEx.SetJumpState(Value: TJumpState);
begin
  if FJumpState <> Value then
  begin
    FJumpState := Value;
    case Value of
      jsNone, jsFalling:
        begin
          FVelocityY := 0;
        end;
    end;
  end;
end;

procedure TPlayerEx.UpdateJump;
begin
  case FJumpState of
    jsNone:
      begin
        if DoJump then
        begin
          FJumpState := jsJumping;
          VelocityY := -FJumpHeight;
        end;
      end;
    jsJumping:
      begin
        Y := Y + FVelocityY * 1;
        VelocityY := FVelocityY + FJumpSpeed;
        if VelocityY > 0 then
          FJumpState := jsFalling;
      end;
    jsFalling:
      begin
        Y := Y + FVelocityY * 1;
        VelocityY := VelocityY + FJumpSpeed;
        if VelocityY > FMaxFallSpeed then
          VelocityY := FMaxFallSpeed;
      end;
  end;
  DoJump := False;
end;

procedure TPlayerEx.DoMove(const Movecount: Single);
var
  Direction, AnimDelay: Integer;
  X1, Y1, X2, Y2, FallEdge: Integer;
  NewAction: string;
  Below: TPoint;
  BelowFH, WallFH: TFoothold;
  LadderPos: TPoint;
  LadderRope: TLadderRope;
  PX, PY, Delay: Integer;
begin
  UpdateJump;
  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;

  case Random(300) of

    100:
      begin
        MirrorX := False;
        MoveDirection := mdLeft;

      end;

    150:
      begin
        MirrorX := True;
        MoveDirection := mdRight;
      end;

    200:
      begin
        MoveDirection := mdNone;
      end;

   // 250:
    //  begin
      //  MoveDirection := mdNone2;
     // end;

    290:
      begin

        DoJump := True;

      end;

  end;

  if JumpState <> jsNone then
  begin

    Action := 'jump';
      //Frame := 0;
  end
  else
  begin
    case MoveDirection of
      mdLeft, mdRight:
        begin

          Action := WalkType;
        end;
      mdNone:
        begin

          Action := StandType;
        end;
      mdNone2:
        begin

          Action := 'prone';
        end;
    end;

  end;

  if (JumpState = jsFalling) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 2)), BelowFH);
    if Y >= Below.Y - 3 then
    begin
      Y := Below.Y;
      // MaxFallSpeed :=10;
      JumpState := jsNone;
      FH := BelowFH;
      Z := FH.Z * 100000 + 6000;
    end;
  end;
  // if (FH.Prev=0) and (FH.Next=0) then Exit;

  case MoveDirection of
    mdLeft:
      begin

        Direction := GetAngle256(X1, Y1, X2, Y2);
        if (not FH.IsWall) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        FallEdge := -999999;
        JumpEdge := -999999;
        if MoveType = mtMove then
        begin
          // no fh
          if FH.Prev = nil then
            FallEdge := FH.X1;
          // Wall's edge down
          if (FH.Prev <> nil) and (FH.Prev.IsWall) then
            FallEdge := FH.X1;

          if X < FallEdge then
          begin
            X := FallEdge;
            MirrorX := True;
            MoveDirection := mdRight;
          end;
        end;

        if MoveType = mtJump then
        begin
          if (X < TMap.Left + 20) then
          begin
            X := TMap.Left + 20;
            MirrorX := True;
            MoveDirection := mdRight;
          end;
          // .--------.
          if FH.Prev = nil then
            JumpEdge := FH.X1;
          // ¢z--- <--
          if (FH.Prev <> nil) and (FH.Prev.IsWall) and (FH.Prev.Y1 > Y) then
            FallEdge := FH.X1;

          if X < FallEdge then
            JumpState := jsFalling;
          if X < JumpEdge then
            DoJump := True;
          // -->  ---¢{  <--
          WallFH := TFootholdTree.This.FindWallR(Point(Round(X + 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if X < WallFH.X1 + 30 then
              DoJump := True;
            if X <= WallFH.X1 then
            begin
              X := WallFH.X1 + MoveSpeed;
              if JumpState = jsNone then
              begin
                MirrorX := True;
                MoveDirection := mdRight;
              end;
            end;
          end;
        end;

        // walk left
        if (X <= FH.X1) and (FH.PrevID <> 0) and (not FH.IsWall) and (not FH.Prev.IsWall) then
        begin
          if (JumpState = jsNone) then
          begin
            FH := FH.Prev;
            X := FH.X2;
            Y := FH.Y2;
            Z := FH.Z * 100000 + 6000;
          end;
        end;

      end;

    mdRight:
      begin

        Direction := GetAngle256(X2, Y2, X1, Y1);
        if (not FH.IsWall) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        FallEdge := 999999;
        JumpEdge := 999999;
        if MoveType = mtMove then
        begin
          if (FH.Next = nil) then
            FallEdge := FH.X2 + 5;
          // Wall down
          if (FH.Next <> nil) and (FH.Next.IsWall) { and (FH.Next.Y2 > Y) } then
            FallEdge := FH.X2;
          if (X > FallEdge) then
          begin
            X := FallEdge;
            MirrorX := False;
            MoveDirection := mdLeft;
          end;
        end;

        if MoveType = mtJump then
        begin
          if (X > TMap.Right - 20) then
          begin
            X := TMap.Right - 20;
            MirrorX := False;
            MoveDirection := mdLeft;
          end;
          if FH.Next = nil then // .--------.
            JumpEdge := FH.X2;
          // -->  ----¢{
          if (FH.Next <> nil) and (FH.Next.IsWall) and (FH.Next.Y2 > Y) then
            FallEdge := FH.X2;

          if X > FallEdge then
            JumpState := jsFalling;
          if X > JumpEdge then
            DoJump := True;
          // -->  ¢z.....
          WallFH := TFootholdTree.This.FindWallL(Point(Round(X - 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if X > WallFH.X1 - 30 then
              DoJump := True;
            if X >= WallFH.X1 then
            begin
              X := WallFH.X2 - MoveSpeed;
              if JumpState = jsNone then
              begin
                MirrorX := False;
                MoveDirection := mdLeft;
              end;
            end;

          end;
        end;

        // walk right
        if (X >= FH.X2) and (FH.NextID <> 0) and (not FH.IsWall) and (not FH.Next.IsWall) then
        begin
          if (JumpState = jsNone) then
          begin
            FH := FH.Next;
            X := FH.X1;
            Y := FH.Y1;
            Z := FH.Z * 100000 + 6000;
          end;
        end;

      end;

  end;

end;

procedure TPlayerEx.DoDraw;
begin
  inherited;
end;

procedure TAvatarPartEx.DoMove(const Movecount: Single);
begin
  if Image <> 'hand' then
    if (Alpha = 0) or (Visible = False) then
      Exit;

  X := Trunc(Owner.X);
  Y := Trunc(Owner.Y);

  MirrorX := Owner.MirrorX;
  State := Owner.action;

  case Random(500) of
    250:
      begin
        FaceFrame := 0;
        Expression := 'smile';
      end;
    400:
      begin
        Expression := 'blink';
      end;
  end;
  UpdateFrame;

end;

procedure TAvatarPartEx.DoDraw;
begin
  if (Alpha = 0) or (Visible = False) then
     Exit;
  inherited;

  if ChangeFrame then
    ChangeFrame := False;

  if Visible then
    Moved := True;

end;

initialization
  PlayerExList := TList<TPlayerEx>.Create;

finalization
  PlayerExList.Free;

end.

