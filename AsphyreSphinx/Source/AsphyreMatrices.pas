unit AsphyreMatrices;
//---------------------------------------------------------------------------
// AsphyreMatrices.pas                                  Modified: 26-Dec-2008
// High-level implementation of 4x4 matrix w/32-byte alignment   Version 1.02
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
//
// If you require any clarifications about the license, feel free to contact
// us or post your question on our forums at: http://www.afterwarp.net
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
// The Original Code is AsphyreMatrices.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Vectors3, Matrices4;

//---------------------------------------------------------------------------
type
 TAsphyreMatrix = class
 private
  MemAddr: Pointer;
  FRawMtx: PMatrix4;

  function GetWorldPos(): TVector3;
  function GetEyePos(): TVector3;
 public
  property RawMtx: PMatrix4 read FRawMtx;

  property WorldPos: TVector3 read GetWorldPos;
  property EyePos  : TVector3 read GetEyePos;

  procedure LoadMtx(Source: PMatrix4); overload;
  procedure LoadMtx(const Source: TMatrix4); overload;
  procedure LoadIdentity();
  procedure LoadZero();

  procedure Translate(dx, dy, dz: Single); overload;
  procedure Translate(const v: TVector3); overload;

  procedure Scale(const v: TVector3); overload;
  procedure Scale(dx, dy, dz: Single); overload;
  procedure Scale(Delta: Single); overload;
  procedure RotateX(Phi: Single);
  procedure RotateY(Phi: Single);
  procedure RotateZ(Phi: Single);

  procedure RotateXLocal(Phi: Single);
  procedure RotateYLocal(Phi: Single);
  procedure RotateZLocal(Phi: Single);

  procedure Multiply(SrcMtx: PMatrix4); overload;
  procedure Multiply(const SrcMtx: TMatrix4); overload;
  procedure Multiply(Source: TAsphyreMatrix); overload;

  procedure LoadRotation(SrcMtx: PMatrix4);

  procedure LookAt(const Origin, Target, Roof: TVector3);

  procedure PerspectiveFovY(FieldOfView, AspectRatio, MinRange, MaxRange: Single);
  procedure PerspectiveFovX(FieldOfView, AspectRatio, MinRange, MaxRange: Single);
  procedure PerspectiveVOL(Width, Height, MinRange, MaxRange: Single);
  procedure PerspectiveBDS(Left, Right, Top, Bottom, MinRange, MaxRange: Single);
  procedure OrthogonalVOL(Width, Height, MinRange, MaxRange: Single);
  procedure OrthogonalBDS(Left, Right, Top, Bottom, MinRange, MaxRange: Single);

  procedure HeadingPitchBank(const v: TVector3); overload;
  procedure HeadingPitchBank(Heading, Pitch, Bank: Single); overload;

  procedure YawPitchRoll(Yaw, Pitch, Roll: Single); overload;
  procedure YawPitchRoll(const v: TVector3); overload;

  procedure Inverse();
  procedure Transpose();
  procedure InverseTranspose();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreMatrix.Create();
begin
 inherited;

 MemAddr:= AllocMem(SizeOf(TMatrix4) + 16);
 FRawMtx:= Pointer(Integer(MemAddr) + ($10 - (Integer(MemAddr) and $0F)));

 LoadIdentity();
end;

//---------------------------------------------------------------------------
destructor TAsphyreMatrix.Destroy();
begin
 FreeMem(MemAddr);

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreMatrix.GetWorldPos(): TVector3;
begin
 Result:= GetWorldPos4(FRawMtx^);
end;

//---------------------------------------------------------------------------
function TAsphyreMatrix.GetEyePos(): TVector3;
begin
 Result:= GetEyePos4(FRawMtx^);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.LoadIdentity();
begin
 Move(IdentityMtx4, FRawMtx^, SizeOf(TMatrix4));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.LoadZero();
begin
 FillChar(FRawMtx^, SizeOf(TMatrix4), 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.LoadMtx(Source: PMatrix4);
begin
 Move(Source^, FRawMtx^, SizeOf(TMatrix4));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.LoadMtx(const Source: TMatrix4);
begin
 Move(Source, FRawMtx^, SizeOf(TMatrix4));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Translate(dx, dy, dz: Single);
begin
 FRawMtx^:= FRawMtx^ * TranslateMtx4(Vector3(dx, dy, dz));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Translate(const v: TVector3);
begin
 FRawMtx^:= FRawMtx^ * TranslateMtx4(v);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateX(Phi: Single);
begin
 FRawMtx^:= FRawMtx^ * RotateXMtx4(Phi);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateY(Phi: Single);
begin
 FRawMtx^:= FRawMtx^ * RotateYMtx4(Phi);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateZ(Phi: Single);
begin
 FRawMtx^:= FRawMtx^ * RotateZMtx4(Phi);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Scale(const v: TVector3);
begin
 FRawMtx^:= FRawMtx^ * ScaleMtx4(v);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Scale(dx, dy, dz: Single);
begin
 FRawMtx^:= FRawMtx^ * ScaleMtx4(Vector3(dx, dy, dz));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Scale(Delta: Single);
begin
 FRawMtx^:= FRawMtx^ * ScaleMtx4(Vector3(Delta, Delta, Delta));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateXLocal(Phi: Single);
var
 Axis: TVector3;
begin
 Axis.x:= FRawMtx^.Data[0, 0];
 Axis.y:= FRawMtx^.Data[0, 1];
 Axis.z:= FRawMtx^.Data[0, 2];

 FRawMtx^:= FRawMtx^ * RotateMtx4(Axis, Phi);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateYLocal(Phi: Single);
var
 Axis: TVector3;
begin
 Axis.x:= FRawMtx^.Data[1, 0];
 Axis.y:= FRawMtx^.Data[1, 1];
 Axis.z:= FRawMtx^.Data[1, 2];

 FRawMtx^:= FRawMtx^ * RotateMtx4(Axis, Phi);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.RotateZLocal(Phi: Single);
var
 Axis: TVector3;
begin
 Axis.x:= FRawMtx^.Data[2, 0];
 Axis.y:= FRawMtx^.Data[2, 1];
 Axis.z:= FRawMtx^.Data[2, 2];

 FRawMtx^:= FRawMtx^ * RotateMtx4(Axis, Phi);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.Multiply(SrcMtx: PMatrix4);
begin
 FRawMtx^:= FRawMtx^ * SrcMtx^;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Multiply(const SrcMtx: TMatrix4);
begin
 Multiply(@SrcMtx);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Multiply(Source: TAsphyreMatrix);
begin
 Multiply(Source.RawMtx);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.LookAt(const Origin, Target, Roof: TVector3);
var
 Aux: TMatrix4;
begin
 Aux:= LookAtMtx4(Origin, Target, Roof);
 Multiply(@Aux);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.PerspectiveFovY(FieldOfView, AspectRatio, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= PerspectiveFOVYMtx4(FieldOfView, AspectRatio, MinRange, MaxRange);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.PerspectiveFovX(FieldOfView, AspectRatio, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= PerspectiveFOVXMtx4(FieldOfView, AspectRatio, MinRange, MaxRange);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.PerspectiveVOL(Width, Height, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= PerspectiveVOLMtx4(Width, Height, MinRange, MaxRange);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.PerspectiveBDS(Left, Right, Top, Bottom, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= PerspectiveBDSMtx4(Left, Right, Top, Bottom, MinRange, MaxRange);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.OrthogonalVOL(Width, Height, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= OrthogonalVOLMtx4(Width, Height, MinRange, MaxRange);
end;

//--------------------------------------------------------------------------
procedure TAsphyreMatrix.OrthogonalBDS(Left, Right, Top, Bottom, MinRange,
 MaxRange: Single);
begin
 FRawMtx^:= OrthogonalBDSMtx4(Left, Right, Top, Bottom, MinRange, MaxRange);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.LoadRotation(SrcMtx: PMatrix4);
var
 i: Integer;
begin
 Move(SrcMtx^, FRawMtx^, SizeOf(TMatrix4));

 for i:= 0 to 3 do
  begin
   FRawMtx^.Data[3, i]:= 0.0;
   FRawMtx^.Data[i, 3]:= 0.0;
  end;

 FRawMtx^.Data[3, 3]:= 1.0;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.HeadingPitchBank(Heading, Pitch, Bank: Single);
var
 Aux: TMatrix4;
begin
 Aux:= HeadingPitchBankMtx4(Heading, Pitch, Bank);
 Multiply(@Aux);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.HeadingPitchBank(const v: TVector3);
var
 Aux: TMatrix4;
begin
 Aux:= HeadingPitchBankMtx4(v);
 Multiply(@Aux);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.YawPitchRoll(Yaw, Pitch, Roll: Single);
begin
 Multiply(YawPitchRollMtx4(Yaw, Pitch, Roll));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.YawPitchRoll(const v: TVector3);
begin
 YawPitchRoll(v.y, v.x, v.z);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.InverseTranspose();
begin
 FRawMtx^:= TransposeMtx4(InvertMtx4(FRawMtx^));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Inverse();
begin
 FRawMtx^:= InvertMtx4(FRawMtx^);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMatrix.Transpose();
begin
 FRawMtx^:= TransposeMtx4(FrawMtx^);
end;

//---------------------------------------------------------------------------
end.
