unit AsphyreDirectionalLights;
//---------------------------------------------------------------------------
// AsphyreDirectionalLights.pas                         Modified: 24-Jan-2009
// A variety of directional lights for Asphyre                    Version 1.0
//---------------------------------------------------------------------------
// Important Notice:
//
// If you modify/use this code or one of its parts either in original or
// modified form, you must comply with Mozilla Public License v1.1,
// specifically section 3, "Distribution Obligations". Failure to do so will
// result in the license breach, which will be resolved in the court.
// Remember that violating author's rights is considered a serious crime in
// many countries. Thank you!
//
// !! Please *read* Mozilla Public License 1.1 document located at:
//  http://www.mozilla.org/MPL/
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is AsphyreDirectionalLights.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Math, AsphyreColors, Vectors3, AsphyreLights;

//---------------------------------------------------------------------------
type
 TDirectionalPhongLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse : TAsphyreColor;
  FSpecular: TAsphyreColor;
  FPower   : Single;
 public
  property Diffuse : TAsphyreColor read FDiffuse write FDiffuse;
  property Specular: TAsphyreColor read FSpecular write FSpecular;
  property Power   : Single read FPower write FPower;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalBlinnPhongLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse : TAsphyreColor;
  FSpecular: TAsphyreColor;
  FPower   : Single;
 public
  property Diffuse : TAsphyreColor read FDiffuse write FDiffuse;
  property Specular: TAsphyreColor read FSpecular write FSpecular;
  property Power   : Single read FPower write FPower;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalMinneartLight = class(TAsphyreDirectionalLight)
 private
  FColor    : TAsphyreColor;
  FRoughness: Single;
 public
  property Color    : TAsphyreColor read FColor write FColor;
  property Roughness: Single read FRoughness write FRoughness;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalCookTorranceLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse   : TAsphyreColor;
  FSpecular  : TAsphyreColor;
  FRoughness1: Single;
  FRoughness2: Single;
  FSpecPower : Single;
 public
  property Diffuse : TAsphyreColor read FDiffuse write FDiffuse;
  property Specular: TAsphyreColor read FSpecular write FSpecular;

  property Roughness1: Single read FRoughness1 write FRoughness1;
  property Roughness2: Single read FRoughness2 write FRoughness2;
  property SpecPower : Single read FSpecPower write FSpecPower;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalIsotropicWardLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse  : TAsphyreColor;
  FSpecular : TAsphyreColor;
  FRoughness: Single;
 public
  property Diffuse  : TAsphyreColor read FDiffuse write FDiffuse;
  property Specular : TAsphyreColor read FSpecular write FSpecular;
  property Roughness: Single read FRoughness write FRoughness;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalAnisotropicWardLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse   : TAsphyreColor;
  FSpecular  : TAsphyreColor;
  FRoughness1: Single;
  FRoughness2: Single;
  FWardTo    : TVector3;
 public
  property Diffuse   : TAsphyreColor read FDiffuse write FDiffuse;
  property Specular  : TAsphyreColor read FSpecular write FSpecular;
  property Roughness1: Single read FRoughness1 write FRoughness1;
  property Roughness2: Single read FRoughness2 write FRoughness2;
  property WardTo    : TVector3 read FWardTo write FWardTo;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TDirectionalOrenNayerLight = class(TAsphyreDirectionalLight)
 private
  FDiffuse  : TAsphyreColor;
  FRoughness: Single;
 public
  property Diffuse  : TAsphyreColor read FDiffuse write FDiffuse;
  property Roughness: Single read FRoughness write FRoughness;

  procedure Illuminate(const WorldPos, Normal, EyePos: TVector3;
   out DiffuseColor, SpecularColor: TAsphyreColor); override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function OneDivSqrt(Value: Single): Single;
begin
 if (Value > 0.0) then Result:= 1.0 / Sqrt(Value)
  else Result:= 0.0;
end;

//---------------------------------------------------------------------------
constructor TDirectionalPhongLight.Create();
begin
 inherited;

 FDiffuse := LightWhiteColor;
 FSpecular:= LightWhiteColor;
 FPower   := 8.0;
end;

//---------------------------------------------------------------------------
procedure TDirectionalPhongLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ToEye, Reflect: TVector3;
 DiffuseTerm, SpecularTerm: Single;
begin
 ToEye  := Norm3(EyePos - WorldPos);
 Reflect:= -Reflect3(FDirection, Normal);

 DiffuseTerm := Max(Dot3(FDirection, Normal), 0.0);
 SpecularTerm:= Math.Power(Max(Dot3(Reflect, ToEye), 0.0), FPower);

 DiffuseColor := FDiffuse * DiffuseTerm;
 SpecularColor:= FSpecular * SpecularTerm;
end;

//---------------------------------------------------------------------------
constructor TDirectionalBlinnPhongLight.Create();
begin
 inherited;

 FDiffuse := LightWhiteColor;
 FSpecular:= LightWhiteColor;
 FPower   := 8.0;
end;

//---------------------------------------------------------------------------
procedure TDirectionalBlinnPhongLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ToEye, HalfVec: TVector3;
 DiffuseTerm, SpecularTerm: Single;
begin
 ToEye  := Norm3(EyePos - WorldPos);
 HalfVec:= Norm3(FDirection + ToEye);

 DiffuseTerm := Max(Dot3(FDirection, Normal), 0.0);
 SpecularTerm:= Math.Power(Max(Dot3(Normal, HalfVec), 0.0), FPower);

 DiffuseColor := FDiffuse * DiffuseTerm;
 SpecularColor:= FSpecular * SpecularTerm;
end;

//---------------------------------------------------------------------------
constructor TDirectionalMinneartLight.Create();
begin
 inherited;

 FColor    := LightWhiteColor;
 FRoughness:= 0.5;
end;

//---------------------------------------------------------------------------
procedure TDirectionalMinneartLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ToEye: TVector3;
 VdotN, LdotN, Irradiance: Single;
begin
 ToEye  := Norm3(EyePos - WorldPos);

 VdotN:= Max(Dot3(ToEye, Normal), 0.0);
 LdotN:= Max(Dot3(FDirection, Normal), 0.0);
 Irradiance:= Power(VdotN * LdotN, FRoughness) * LdotN;

 DiffuseColor := FColor * Irradiance;
 SpecularColor:= LightBlackColor;
end;

//---------------------------------------------------------------------------
constructor TDirectionalCookTorranceLight.Create();
begin
 inherited;

 FRoughness1:= 0.8;
 FRoughness2:= 1.2;
 FSpecPower := 1.5;
 FDiffuse := LightWhiteColor;
 FSpecular:= LightWhiteColor;
end;

//---------------------------------------------------------------------------
procedure TDirectionalCookTorranceLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ViewDir, ViewHalf: TVector3;
 NormalDotHalf, ViewDotHalf, NormalDotView, NormalDotLight: Single;
 G1, G2, G, F, R_2, NDotH_2, A, B, R, Irradiance, SpecularTerm: Single;
begin
 ViewDir := Norm3(EyePos - WorldPos);
 ViewHalf:= Norm3(FDirection + ViewDir);

 NormalDotHalf := Dot3(Normal, ViewHalf);
 ViewDotHalf   := Dot3(ViewHalf, ViewDir);
 NormalDotView := Dot3(Normal, ViewDir);
 NormalDotLight:= Dot3(Normal, FDirection);

 G1:= (2.0 * NormalDotHalf * NormalDotView) / ViewDotHalf;
 G2:= (2.0 * NormalDotHalf * NormalDotLight) / ViewDotHalf;
 G := Min(1.0, Max(0.0, Min(G1, G2)));

 F:= FRoughness2 + (1.0 - FRoughness2) * Power(1.0 - NormalDotView, 5.0);

 R_2    := FRoughness1 * FRoughness1;
 NDotH_2:= NormalDotHalf * NormalDotHalf;
 A      := 1.0 / (4.0 * R_2 * NDotH_2 * NDotH_2);
 B      := Exp(-(1.0 - NDotH_2) / (R_2 * NDotH_2));
 R      := A * B;

 Irradiance:= Max(0.0, NormalDotLight);

 SpecularTerm:= FSpecPower * Irradiance * G * F * R /
  (NormalDotLight * NormalDotView) - 0.4;

 DiffuseColor := FDiffuse * Irradiance;
 SpecularColor:= FSpecular * SpecularTerm;
end;

//---------------------------------------------------------------------------
constructor TDirectionalIsotropicWardLight.Create();
begin
 inherited;

 FDiffuse  := LightWhiteColor;
 FSpecular := LightWhiteColor;
 FRoughness:= 0.5;
end;

//---------------------------------------------------------------------------
procedure TDirectionalIsotropicWardLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ToEye, HalfVec: TVector3;
 RMS2, Den, Tan2nh, FirstTerm: Single;
 CosTheta, CosDelta, SecondTerm: Single;
begin
 ToEye  := Norm3(EyePos - WorldPos);
 HalfVec:= Norm3(FDirection + ToEye);

 RMS2:= Sqr(FRoughness);
 Den := 6.28 * RMS2;
 Tan2nh:= -Sqr(Tan(ArcCos(Dot3(Normal, HalfVec))));
 FirstTerm:= Exp(Tan2nh / RMS2) / Den;

 CosTheta:= Max(Dot3(Normal, FDirection), 0.0);
 CosDelta:= Dot3(Normal, ToEye);

 SecondTerm:= OneDivSqrt(CosTheta * CosDelta);

 DiffuseColor := FDiffuse * CosTheta;
 SpecularColor:= FSpecular * (CosTheta * FirstTerm * SecondTerm);  
end;

//---------------------------------------------------------------------------
constructor TDirectionalAnisotropicWardLight.Create();
begin
 inherited;

 FDiffuse   := LightWhiteColor;
 FSpecular  := LightWhiteColor;
 FRoughness1:= 0.3;
 FRoughness2:= 0.4;
 FWardTo    := Vector3(0.0, 0.0, 1.0);
end;

//---------------------------------------------------------------------------
procedure TDirectionalAnisotropicWardLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 ToEye, HalfVec: TVector3;
 CosTheta, CosDelta, FirstTerm: Single;
 SecondTerm, HdotX, HdotY, HdotN, A, B, ThirdTerm: Single;
 X, Y: TVector3;
begin
 ToEye  := Norm3(EyePos - WorldPos);
 HalfVec:= Norm3(FDirection + ToEye);

 CosTheta:= Max(Dot3(Normal, FDirection), 0.0);
 CosDelta:= Dot3(Normal, ToEye);

 FirstTerm := OneDivSqrt(CosTheta * CosDelta);
 SecondTerm:= 1.0 / (12.56 * FRoughness1 * FRoughness2);

 X:= Norm3(Cross3(Normal, FWardTo));
 Y:= Norm3(Cross3(Normal, X));

 HdotX:= Dot3(HalfVec, X);
 HdotY:= Dot3(HalfVec, Y);
 HdotN:= Dot3(HalfVec, Normal);

 A:= -2.0 * (Sqr((HdotX / FRoughness1)) + Sqr((HdotY / FRoughness2)));
 B:= 1.0 + HdotN;

 ThirdTerm:= Exp(A / B);

 DiffuseColor := FDiffuse * CosTheta;
 SpecularColor:= FSpecular * (CosTheta * FirstTerm * SecondTerm * ThirdTerm);
end;

//---------------------------------------------------------------------------
constructor TDirectionalOrenNayerLight.Create();
begin
 inherited;

 FDiffuse  := LightWhiteColor;
 FRoughness:= Sqrt(4.9);
end;

//---------------------------------------------------------------------------
procedure TDirectionalOrenNayerLight.Illuminate(const WorldPos, Normal,
 EyePos: TVector3; out DiffuseColor, SpecularColor: TAsphyreColor);
var
 Rough2, A, B: Single;
 EyeVector: TVector3;
 NdotLight, NdotEye: Single;
 SinTheta_r, CosTheta_r, TanTheta_r: Single;
 SinTheta_i, CosTheta_i, TanTheta_i: Single;
 Eye_p, Light_p: TVector3;
 CosAzimuth, Inten: Single;
begin
	Rough2:= Sqr(FRoughness);
	A:= 1.0 - (0.5 * Rough2) / (Rough2 + 0.33);
	B:= (0.45 * Rough2) / (Rough2 + 0.09);

 EyeVector:= Norm3(EyePos);

 NdotLight:= Dot3(Normal, FDirection);
 NdotEye  := Dot3(Normal, EyeVector);

 SinTheta_r:= Length3(Cross3(EyeVector, Normal));
 CosTheta_r:= Max(NdotEye, 0.001);
 SinTheta_i:= Length3(Cross3(FDirection, Normal));
 CosTheta_i:= Max(NdotLight, 0.001);
 TanTheta_i:= SinTheta_i / CosTheta_i;
 TanTheta_r:= SinTheta_r / CosTheta_r;

 Eye_p  := Norm3(EyeVector - NdotEye * Normal);
 Light_p:= Norm3(FDirection - NdotLight * Normal);
 CosAzimuth:= Dot3(Eye_p, Light_p);

 Inten:= CosTheta_i * (A + B * Max(0.0, CosAzimuth) *
  Max(SinTheta_r, SinTheta_i) * Min(TanTheta_i, TanTheta_r));

 DiffuseColor := FDiffuse * Min(Max(Inten, 0.0), 1.0);
 SpecularColor:= LightBlackColor;
end;

//---------------------------------------------------------------------------
end.
