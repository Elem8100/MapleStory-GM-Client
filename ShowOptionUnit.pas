unit ShowOptionUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TShowOptionForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    c2: TCheckBox;
    CheckBox13: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ShowOptionForm: TShowOptionForm;

implementation
      uses MainUnit,MapleMap,MobInfo;
{$R *.dfm}

procedure TShowOptionForm.CheckBox1Click(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    0: TMap.ShowTile := not TMap.ShowTile;
    1: TMap.ShowObj := not TMap.ShowObj;
    2: TMap.ShowBack := not TMap.ShowBack;
    3: TMap.ShowNPC := not TMap.ShowNPC;
    4: TMap.ShowMob := not TMap.ShowMob;
    5: TMap.ShowPortal := not TMap.ShowPortal;
    6: TMap.ShowMobName := not TMap.ShowMobName;
    7: TMap.ShowID := not TMap.ShowID;
    8: TMap.ShowPortalInfo := not TMap.ShowPortalInfo;
    9:
      begin
        TMap.ShowMobInfo := not TMap.ShowMobInfo;
        TMobInfo.ReDrawTarget;
      end;
    10: TMap.ShowFPS := not TMap.ShowFPS;
    11: TMap.ShowChar := not TMap.ShowChar;
    12: TMap.ShowFoothold := not TMap.ShowFoothold;
    13: TMap.ShowMusic := not TMap.ShowMusic;
  end;
  ActiveControl:= nil;
end;

procedure TShowOptionForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TShowOptionForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2-200;
end;

procedure TShowOptionForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.
