unit UI.UIWindow2.Item;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TPickType = (ptNone, ptItem, ptChar);

  TSlots = class
    class var
      PickUp: Boolean;
      PickUpItem: TWZIMGEntry;
      PickType: TPickType;
      SlotA: TAImage;
  private
    FSlots: array[0..95] of TAImage;
    procedure DrawArea;
    procedure ExChange(Index1, Index2: TAImage);
  public
    class procedure Swap(var S1, S2: TAImage);
    procedure ReDraw;
    procedure Show;
    procedure Hide;
    procedure Move(Y: Integer);
    procedure AddItem(ID: string);
    constructor Create;
  end;

var
  EquipSlot, ConsumeSlot, EtcSlot, InstallSlot, CashSlot: TSlots;

procedure CreateItemForm;

procedure AddItemToSlot(ID: string);

implementation

uses
  UI.Utils, ACtrlLabels, MainUnit, MapleCharacter, MapleChair;

var
  HasLoad: Boolean;

procedure TSlots.ReDraw;
var
  i, X, Y: Integer;
begin

  X := -2;
  Y := 15;
  for i := 0 to 95 do
  begin
    Inc(X, 36);
    if (i mod 4 = 0) then
    begin
      X := 10;
      Inc(Y, 35);
    end;
    FSlots[i].Left := X;
    FSlots[i].Top := Y;
    FSlots[i].Tag := i
  end;
end;

procedure TSlots.ExChange(Index1, Index2: TAImage);
var
  Left, Top, Tag: Integer;
  Temp: TAImage;
begin
  Temp := FSlots[Index1.Tag];
  FSlots[Index1.Tag] := FSlots[Index2.Tag];
  FSlots[Index2.Tag] := Temp;

  Left := Index1.Left;
  Index1.Left := Index2.Left;
  Index2.Left := Left;
  Top := Index1.Top;
  Index1.Top := Index2.Top;
  Index2.Top := Top;

  Tag := Index1.Tag;
  Index1.Tag := Index2.Tag;
  Index2.Tag := Tag;
end;

class procedure TSlots.Swap(var S1, S2: TAImage);
var
  Temp: TAImage;
begin
  Temp := TAImage.Create(nil);
  Temp.ImageEntry := S1.ImageEntry;
  S1.ImageEntry := S2.ImageEntry;
  S2.ImageEntry := Temp.ImageEntry;

  Temp.Width := S1.Width;
  S1.Width := S2.Width;
  S2.Width := Temp.Width;

  Temp.Height := S1.Height;
  S1.Height := S2.Height;
  S2.Height := Temp.Height;

  Temp.Free;
end;

constructor TSlots.Create;
var
  i, X, Y: Integer;
  b: TAImage;
begin
  X := -2;
  Y := 15;
  for i := 0 to 95 do
  begin
    Inc(X, 36);
    if (i mod 4 = 0) then
    begin
      X := 10;
      Inc(Y, 35);
    end;
    FSlots[i] := TAImage.Create(UIEngine.AForm(UIOwner));
    FSlots[i].ImageEntry := nil;
    FSlots[i].Width := 35;
    FSlots[i].Height := 35;
    FSlots[i].BorderWidth := 0;
    FSlots[i].CanMoveHandle := False;
    FSlots[i].Left := X;
    FSlots[i].Top := Y;
    FSlots[i].Tag := i;
    FSlots[i].OnMouseDown :=
      procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
      begin
   //   mainform.Caption := variant(TAImage(Sender).ID);
        if (PickType = ptNone) and (TAImage(Sender).ImageEntry <> nil) then
        begin
          SlotA := TAImage(Sender);
          PickUpItem := SlotA.ImageEntry;
          PickType := ptItem;
          PickUpItem := SlotA.ImageEntry;
        end
        else if (PickType = ptItem) then
        begin
          b := TAImage(Sender);
          ExChange(SlotA, b);
          PickType := ptNone;
          PickUpItem := nil;
        end;
     {
      if (PickType = ptChar) and (PickUpItem <> nil) then
      begin
        if TAImage(Sender).ImageEntry = nil then
          Swap(TAImage(Sender), TCharSlot.SlotA);
        PickType := ptNone;
        PickUpItem := nil;
      end;
      }

      end;

  end;

  DrawArea;
end;

procedure TSlots.Show;
var
  i: Integer;
begin
  for i := 0 to 95 do
    FSlots[i].Visible := True;

end;

procedure TSlots.Hide;
var
  i: Integer;
begin
  for i := 0 to 95 do
    FSlots[i].Visible := False;
end;

procedure TSlots.DrawArea;
var
  i: Integer;
begin
  for i := 0 to 95 do
  begin
    FSlots[i].Visible := False;
    if (FSlots[i].Top > 20) and (FSlots[i].Top < 250) then
      FSlots[i].Visible := True;
  end;
end;

function GetItemDir(ID: string): string;
begin
  case StrToInt(ID) div 1000000 of
    5:
      Result := 'Cash';
    2:
      Result := 'Consume';
    4:
      Result := 'Etc';
    3:
      Result := 'Install';
    9:
      Result := 'Special';
  end;
end;

procedure TSlots.AddItem(ID: string);
var
  IconPath, Dir: string;
  Entry: TWZIMGEntry;
  i: Integer;
  p: Boolean;
  a: TAImage;
begin
  case ID.ToInteger div 1000000 of
    2, 3, 4, 5, 9:
      begin
        Dir := GetItemDir(ID);
        IconPath := 'Item/' + Dir + '/' + LeftStr(ID, 4) + '.img/' + ID + '/info/icon';
        Entry := GetImgEntry(IconPath);
      end
  else
    begin
      Dir := GetDir(ID);
      Entry := GetImgFile('Character/'+Dir + ID + '.img').Root.Get('info/icon');
    end;
  end;

  DumpData(Entry, UIData, UIImages);
  for i := 0 to 95 do
  begin
    if FSlots[i].ImageEntry = nil then
    begin
      FSlots[i].ImageEntry := Entry;
      FSlots[i].Width := TWZIMGEntry(Entry.Parent).Get2('icon').Canvas.Width;
      FSlots[i].Height := TWZIMGEntry(Entry.Parent).Get2('icon').Canvas.Height;
      FSlots[i].ID := ID;
      Exit;
    end;
  end;
end;

procedure AddItemToSlot(ID: string);
begin
  case ID.ToInteger div 1000000 of
    2:
      ConsumeSlot.AddItem(ID);
    3:
      InstallSlot.AddItem(ID);
    4:
      EtcSlot.AddItem(ID);
    5:
      CashSlot.AddItem(ID);
  else
    EquipSlot.AddItem(ID);
  end;
end;

procedure TSlots.Move(Y: Integer);
var
  py, i: Integer;
begin
  py := 15;
  for i := 0 to 95 do
  begin
    if (i mod 4 = 0) then
      Inc(py, 35);
    FSlots[i].Top := py + Y * 35;
  end;
  DrawArea;
end;

var
  TabIndex: Integer;
  Pos: array of Integer;

procedure SelectTab(Index: Integer);
begin
  const Path = 'UI/UIWindow2.img/Item/Tab/enabled/';
  for var i := 0 to 4 do
    UIImage[Path + i.ToString].Visible := False;

  UIImage[Path + Index.ToString].Visible := True;
  EquipSlot.Hide;
  ConsumeSlot.Hide;
  EtcSlot.Hide;
  InstallSlot.Hide;
  CashSlot.Hide;

  if UIImage.ContainsKey('ItemSlotVScr/thumb0') then
    UIImage['ItemSlotVScr/thumb0'].Top := Pos[Index];

  case Index of
    0:
      begin
        TabIndex := 0;
        Equipslot.PickType := ptNone;
        EquipSlot.DrawArea;
      end;
    1:
      begin
        TabIndex := 1;
        ConsumeSlot.PickType := ptNone;
        ConsumeSlot.DrawArea;
      end;
    2:
      begin
        TabIndex := 2;
        EtcSlot.PickType := ptNone;
        EtcSlot.DrawArea;
      end;
    3:
      begin
        TabIndex := 3;
        InstallSlot.PickType := ptNone;
        InstallSlot.DrawArea;
      end;
    4:
      begin
        TabIndex := 4;
        CashSlot.PickType := ptNone;
        CashSlot.DrawArea;
      end;
  end;
end;

procedure CreateItemForm;
begin
  const Path = 'UI/UIWindow2.img/Item/';
  CreateForm(Path + 'backgrnd', 417, 200);

  CreateButton('ItemFormClose', 'UI/Basic.img/BtClose3', 150, 5);
  UIButton['ItemFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow2.img/Item/backgrnd'].Visible := False;
    end;
  CreateImage(Path + 'backgrnd2');
  CreateImage(Path + 'backgrnd3');

  for var i := 0 to 4 do
  begin
    CreateImage(Path + 'Tab/disabled/' + i.ToString);
    CreateImage(Path + 'Tab/enabled/' + i.ToString);
  end;

  if not HasLoad then
  begin
    Pos := [58, 58, 58, 58, 58];
    EquipSlot := TSlots.Create;
    ConsumeSlot := TSlots.Create;
    EtcSlot := TSlots.Create;
    InstallSlot := TSlots.Create;
    CashSlot := TSlots.Create;
    SelectTab(0);
    AddItemToSlot('05010000');
    AddItemToSlot('05010001');
    AddItemToSlot('05010000');
    AddItemToSlot('02010009');
    AddItemToSlot('02000005');
    AddItemToSlot('02000001');
    AddItemToSlot('01302001');
    HasLoad := True;
  end;

  CreateButton(Path + 'BtCoin3');
  CreateButton(Path + 'BtFull3');
  CreateButton(Path + 'BtSmall3');
  CreateButton(Path + 'BtPoint0');

  CreateButton(Path + 'BtBits3');
  CreateButton(Path + 'BtDisassemble3');
  CreateButton(Path + 'BtExtract3');
  CreateButton(Path + 'BtGather3');
  CreateButton(Path + 'BtSort3');
  CreateButton(Path + 'BtToad3');
  CreateButton(Path + 'BtUpgrade3');
  CreateButton(Path + 'BtAppraise3');
  CreateButton(Path + 'BtPot3');

  CreateImage('ItemSlotVScr', 'UI/Basic.img/VScr9/enabled/base', 1, 17.7, 153, 47);
  CreateImage('ItemSlotVScr/prev0', 'UI/Basic.img/VScr9/enabled/prev0', 1, 1, 153, 47);
  CreateImage('ItemSlotVScr/next0', 'UI/Basic.img/VScr9/enabled/next0', 1, 1, 153, 247);
  CreateImage('ItemSlotVScr/thumb0', 'UI/Basic.img/VScr9/enabled/thumb0', 1, 1, 153, 58);
  UIImage['ItemSlotVScr/thumb0'].Width := 11;
  UIImage['ItemSlotVScr/thumb0'].Height := 26;
  var OnDrag: Boolean;
  var MouseDownY: Integer;
  UIImage['ItemSlotVScr/thumb0'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      MouseDownY := GetMouseDownY('ItemSlotVScr/thumb0', Y);
      OnDrag := True;
    end;

  UIImage['ItemSlotVScr/thumb0'].OnMouseMove :=
    procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer)
    begin
      if OnDrag then
      begin
        MoveImage('ItemSlotVScr/thumb0', Y, MouseDownY);
        var Thumb := UIImage['ItemSlotVScr/thumb0'];
        if Thumb.Top < 58 then
          Thumb.Top := 58;

        if Thumb.Top > 223 then
          Thumb.Top := 223;

        case TabIndex of
          0:
            begin
              Pos[0] := Thumb.Top;
              EquipSlot.Move((50 - Thumb.Top) div 10);
            end;
          1:
            begin
              pos[1] := Thumb.Top;
              ConsumeSlot.Move((50 - Thumb.Top) div 10);
            end;

          2:
            begin
              Pos[2] := Thumb.Top;
              EtcSlot.Move((50 - Thumb.Top) div 10);
            end;
          3:
            begin
              pos[3] := Thumb.Top;
              InstallSlot.Move((50 - Thumb.Top) div 10);
            end;
          4:
            begin
              pos[4] := Thumb.Top;
              CashSlot.Move((50 - Thumb.Top) div 10);
            end;

        end;
      end;
    end;

  UIImage['ItemSlotVScr/thumb0'].OnMouseUp :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      OnDrag := False;
    end;

  UIImage[Path + 'Tab/disabled/0'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(0);
    end;
  UIImage[Path + 'Tab/disabled/1'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(1);
    end;
  UIImage[Path + 'Tab/disabled/2'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(2);
    end;
  UIImage[Path + 'Tab/disabled/3'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(3);
    end;
  UIImage[Path + 'Tab/disabled/4'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(4);
    end;
  CreateLabel('ItemMeso', '991,332,550', 52, 268);
  CreateLabel('ItemMaplePoints', '601,888', 97, 285);
end;

initialization

finalization
  EquipSlot.Free;
  ConsumeSlot.Free;
  EtcSlot.Free;
  InstallSlot.Free;
  CashSlot.Free;

end.

