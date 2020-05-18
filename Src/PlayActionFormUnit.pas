unit PlayActionFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TPlayActionForm = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    DoPlay: Boolean;
    Playing: Boolean;
    { Public declarations }
  end;

var
  PlayActionForm: TPlayActionForm;

implementation

uses
  WZArchive, Global;
{$R *.dfm}

procedure TPlayActionForm.Button1Click(Sender: TObject);
begin
  DoPlay := True;
  Playing := True;
end;

procedure TPlayActionForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  for var Iter in CharacterWZ.GetImgFile('00002000.img').Root.Children do
    ListBox1.Items.Add(Iter.Name);
end;

procedure TPlayActionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Playing := False;
end;

end.

