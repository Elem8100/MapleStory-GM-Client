program MapleStoryGM;
    {$SetPEFlags $20}
uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  MobFormUnit in 'MobFormUnit.pas' {AddMobForm},
  ImageInfoUnit in 'ImageInfoUnit.pas' {ImageInfoForm},
  SaveMapFormUnit in 'SaveMapFormUnit.pas' {SaveMapForm},
  AvatarFormUnit in 'AvatarFormUnit.pas' {AvatarForm},
  ShowOptionUnit in 'ShowOptionUnit.pas' {ShowOptionForm},
  NpcFormUnit in 'NpcFormUnit.pas' {AddNpcForm},
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
  SkillFormUnit in 'SkillFormUnit.pas' {SkillForm},
  OptionsFormUnit in 'OptionsFormUnit.pas' {OptionsForm},
  AndroidFormUnit in 'AndroidFormUnit.pas' {AndroidForm},
  RenderFormUnit in 'RenderFormUnit.pas' {RenderForm},
  SetScreenFormUnit in 'SetScreenFormUnit.pas' {SetScreenForm},
  ConsumeFormUnit in 'ConsumeFormUnit.pas' {ConsumeForm},
  CashForm2Unit in 'CashForm2Unit.pas' {CashForm2},
  EtcFormUnit in 'EtcFormUnit.pas' {EtcForm},
  PlayActionFormUnit in 'PlayActionFormUnit.pas' {PlayActionForm},
  SelectFolderFormUnit in 'SelectFolderFormUnit.pas' {SelectFolderForm},
  TotemEffectFormUnit in 'TotemEffectFormUnit.pas' {TotemEffectForm},
  SoulEffectFormUnit in 'SoulEffectFormUnit.pas' {SoulEffectForm},
  ReactorFormUnit in 'ReactorFormUnit.pas' {ReactorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
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
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAndroidForm, AndroidForm);
  Application.CreateForm(TRenderForm, RenderForm);
  Application.CreateForm(TSetScreenForm, SetScreenForm);
  Application.CreateForm(TConsumeForm, ConsumeForm);
  Application.CreateForm(TCashForm2, CashForm2);
  Application.CreateForm(TEtcForm, EtcForm);
  Application.CreateForm(TPlayActionForm, PlayActionForm);
  Application.CreateForm(TSelectFolderForm, SelectFolderForm);
  Application.CreateForm(TTotemEffectForm, TotemEffectForm);
  Application.CreateForm(TSoulEffectForm, SoulEffectForm);
  Application.CreateForm(TReactorForm, ReactorForm);
  Application.Run;
end.
