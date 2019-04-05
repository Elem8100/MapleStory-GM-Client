unit SkillFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid;

type
  TSkillForm = class(TForm)
    SelectGrid: TAdvStringGrid;
    HotKeySetGrid: TAdvStringGrid;
    SkillGrid: TAdvStringGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkillForm: TSkillForm;

implementation

{$R *.dfm}

end.
