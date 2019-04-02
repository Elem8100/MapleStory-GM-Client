program DelphiMaple;
    {$SetPEFlags $20}
uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  AddMobUnit in 'C:\MapleStory Emulator2.6\AddMobUnit.pas' {AddMobForm},
  ImageInfoUnit in 'C:\MapleStory Emulator2.6\ImageInfoUnit.pas' {ImageInfoForm},
  SaveMapUnit in 'C:\MapleStory Emulator2.6\SaveMapUnit.pas' {SaveMapForm},
  RenderFormUnit in 'C:\MapleStory Emulator2.6\RenderFormUnit.pas' {RenderForm},
  AvatarUnit in 'C:\MapleStory Emulator2.6\AvatarUnit.pas' {AvatarForm},
  ShowOptionUnit in 'C:\MapleStory Emulator2.6\ShowOptionUnit.pas' {ShowOptionForm},
  AddNpcFormUnit in 'C:\MapleStory Emulator2.6\AddNpcFormUnit.pas' {AddNpcForm},
  DamageSkinFormUnit in 'C:\MapleStory Emulator2.6\DamageSkinFormUnit.pas' {DamageSkinForm},
  ChairformUnit in 'C:\MapleStory Emulator2.6\ChairformUnit.pas' {ChairForm},
  WorldMapFormUnit in 'C:\MapleStory Emulator2.6\WorldMapFormUnit.pas' {WorldMapForm},
  CashFormUnit in 'C:\MapleStory Emulator2.6\CashFormUnit.pas' {CashForm},
  TamingMobFormUnit in 'C:\MapleStory Emulator2.6\TamingMobFormUnit.pas' {TamingMobForm},
  MorphFormUnit in 'C:\MapleStory Emulator2.6\MorphFormUnit.pas' {MorphForm},
  MedalTagFormUnit in 'C:\MapleStory Emulator2.6\MedalTagFormUnit.pas' {MedalTagForm},
  NickNameTagFormUnit in 'C:\MapleStory Emulator2.6\NickNameTagFormUnit.pas' {NickNameForm},
  LabelRingFormUnit in 'C:\MapleStory Emulator2.6\LabelRingFormUnit.pas' {LabelRingForm},
  PetFormUnit in 'C:\MapleStory Emulator2.6\PetFormUnit.pas' {PetForm},
  FamiliarFormUnit in 'C:\MapleStory Emulator2.6\FamiliarFormUnit.pas' {FamiliarForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRenderForm, RenderForm);
  Application.CreateForm(TAddMobForm, AddMobForm);
  Application.CreateForm(TImageInfoForm, ImageInfoForm);
  Application.CreateForm(TSaveMapForm, SaveMapForm);
  Application.CreateForm(TAvatarForm, AvatarForm);
  Application.CreateForm(TShowOptionForm, ShowOptionForm);
  Application.CreateForm(TAddNpcForm, AddNpcForm);
  Application.CreateForm(TDamageSkinForm, DamageSkinForm);
  Application.CreateForm(TChairForm, ChairForm);
  Application.CreateForm(TWorldMapForm, WorldMapForm);
  Application.CreateForm(TCashForm, CashForm);
  Application.CreateForm(TTamingMobForm, TamingMobForm);
  Application.CreateForm(TMorphForm, MorphForm);
  Application.CreateForm(TMedalTagForm, MedalTagForm);
  Application.CreateForm(TNickNameForm, NickNameForm);
  Application.CreateForm(TLabelRingForm, LabelRingForm);
  Application.CreateForm(TPetForm, PetForm);
  Application.CreateForm(TFamiliarForm, FamiliarForm);
  Application.Run;
end.
