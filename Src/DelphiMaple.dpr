program DelphiMaple;
    {$SetPEFlags $20}
uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  AddMobUnit in 'AddMobUnit.pas' {AddMobForm},
  ImageInfoUnit in 'ImageInfoUnit.pas' {ImageInfoForm},
  SaveMapUnit in 'SaveMapUnit.pas' {SaveMapForm},
  RenderFormUnit in 'RenderFormUnit.pas' {RenderForm},
  AvatarUnit in 'AvatarUnit.pas' {AvatarForm},
  ShowOptionUnit in 'ShowOptionUnit.pas' {ShowOptionForm},
  AddNpcFormUnit in 'AddNpcFormUnit.pas' {AddNpcForm},
  DamageSkinFormUnit in 'DamageSkinFormUnit.pas' {DamageSkinForm},
  ChairformUnit in 'ChairformUnit.pas' {ChairForm},
  WorldMapFormUnit in 'WorldMapFormUnit.pas' {WorldMapForm},
  CashFormUnit in 'CashFormUnit.pas' {CashForm},
  TamingMobFormUnit in 'TamingMobFormUnit.pas' {TamingMobForm},
  MorphFormUnit in 'MorphFormUnit.pas' {MorphForm},
  MedalTagFormUnit in 'MedalTagFormUnit.pas' {MedalTagForm},
  NickNameTagFormUnit in 'NickNameTagFormUnit.pas' {NickNameForm},
  LabelRingFormUnit in 'LabelRingFormUnit.pas' {LabelRingForm},
  PetFormUnit in 'PetFormUnit.pas' {PetForm},
  FamiliarFormUnit in 'FamiliarFormUnit.pas' {FamiliarForm},
  SkillFormUnit in 'SkillFormUnit.pas' {SkillForm};

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
  Application.CreateForm(TSkillForm, SkillForm);
  Application.Run;
end.
