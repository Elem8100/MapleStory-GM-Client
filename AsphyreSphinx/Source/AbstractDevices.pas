unit AbstractDevices;
//---------------------------------------------------------------------------
// AbstractDevices.pas                                  Modified: 05-May-2008
// Asphyre Device Abstract declaration                           Version 1.02
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
// The Original Code is AbstractDevices.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, SysUtils, EventProviders, Vectors2px, AsphyreUtils,
 AbstractTextures;

//---------------------------------------------------------------------------
type
 TDepthStencilType = (dsNone, dsDepthOnly, dsDepthStencil);

//---------------------------------------------------------------------------
 TVertexProcessingType = (vptSoftware, vptHardware, vptMixed);

//---------------------------------------------------------------------------
 TParamChangeType = (pctSize, pctWindowed, pctVSync);

//---------------------------------------------------------------------------
 TAsphyreDevice = class
 private
  FSize    : TPoint2px;
  FWindowed: Boolean;
  FVSync   : Boolean;
  FActive  : Boolean;
  FAdapter : Integer;

  FWindowHandle: THandle;
  FHighBitDepth: Boolean;
  FMultisamples: Integer;
  FDepthStencil: TDepthStencilType;
  FFillDepthValue  : Single;
  FFillStencilValue: Cardinal;
  FVertexProcessing: TVertexProcessingType;

  procedure SetSize(const Value: TPoint2px);
  procedure SetVSync(const Value: Boolean);
  procedure SetWindowed(const Value: Boolean);
  procedure SetWindowHandle(const Value: THandle);
  procedure SetHighBitDepth(const Value: Boolean);
  procedure SetAdapter(const Value: Integer);
  procedure SetMultisamples(const Value: Integer);
  procedure SetDepthStencil(const Value: TDepthStencilType);
 protected
  function InitDevice(): Boolean; virtual; abstract;
  procedure DoneDevice(); virtual; abstract;
  procedure ResetDevice(); virtual; abstract;

  procedure UpdateParams(ParamType: TParamChangeType); virtual; abstract;
  function MayRender(): Boolean; virtual; abstract;
  procedure RenderWith(hWnd: THandle; Handler: TNotifyEvent;
   Background: Cardinal); virtual; abstract;
  procedure RenderToTarget(Handler: TNotifyEvent;
   Background: Cardinal; FillBk: Boolean); virtual; abstract;
 public
  property Active: Boolean read FActive;

  property Adapter : Integer read FAdapter write SetAdapter;
  property Size    : TPoint2px read FSize write SetSize;
  property Windowed: Boolean read FWindowed write SetWindowed;
  property VSync   : Boolean read FVSync write SetVSync;

  property WindowHandle: THandle read FWindowHandle write SetWindowHandle;
  property HighBitDepth: Boolean read FHighBitDepth write SetHighBitDepth;

  property Multisamples: Integer read FMultisamples write SetMultisamples;
  property DepthStencil: TDepthStencilType read FDepthStencil
   write SetDepthStencil;

  property FillDepthValue  : Single read FFillDepthValue write FFillDepthValue;
  property FillStencilValue: Cardinal read FFillStencilValue
   write FFillStencilValue;

  property VertexProcessing: TVertexProcessingType read FVertexProcessing
   write FVertexProcessing;

  function Initialize(): Boolean;
  procedure Finalize();

  procedure Render(Handler: TNotifyEvent; Background: Cardinal); overload;
  procedure Render(hWnd: THandle; Handler: TNotifyEvent;
   Background: Cardinal); overload;

  procedure RenderTo(Handler: TNotifyEvent; Background: Cardinal;
   FillBk: Boolean; Texture: TAsphyreRenderTargetTexture);
  procedure Reset();

  constructor Create(); virtual;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
// Events related to device function.
//---------------------------------------------------------------------------
var
 EventDeviceCreate : TEventProvider = nil;
 EventDeviceDestroy: TEventProvider = nil;

//---------------------------------------------------------------------------
 EventDeviceReset  : TEventProvider = nil;
 EventDeviceLost   : TEventProvider = nil;

//---------------------------------------------------------------------------
 EventBeginScene   : TEventProvider = nil;
 EventEndScene     : TEventProvider = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreDevice.Create();
begin
 inherited;

 FSize    := Point2px(800, 600);
 FWindowed:= True;
 FVSync   := False;
 FAdapter := 0;

 FMultisamples:= 1;
 FDepthStencil:= dsDepthOnly;

 FWindowHandle:= 0;
 FHighBitDepth:= True;

 FFillDepthValue  := 1.0;
 FFillStencilValue:= 0;
 FVertexProcessing:= vptHardware;
end;

//---------------------------------------------------------------------------
destructor TAsphyreDevice.Destroy();
begin
 if (FActive) then Finalize();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreDevice.Initialize(): Boolean;
begin
 Result:= (not FActive)and(FWindowHandle <> 0);
 if (not Result) then Exit;

 Result:= InitDevice();
 if (not Result) then Exit;

 FActive:= True;

 EventDeviceCreate.Notify(Self, @Result);
 if (not Result) then
  begin
   Finalize();
   Exit;
  end;

 EventDeviceReset.Notify(Self, @Result);
 if (not Result) then
  begin
   Finalize();
   Exit;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.Finalize();
begin
 if (not FActive) then Exit;

 EventDeviceLost.Notify(Self, nil);
 EventDeviceDestroy.Notify(Self, nil);

 DoneDevice();

 FActive:= False;
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetSize(const Value: TPoint2px);
begin
 FSize.x:= Max2(Value.x, 1);
 FSize.y:= Max2(Value.y, 1);

 if (FActive) then UpdateParams(pctSize);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetVSync(const Value: Boolean);
begin
 FVSync:= Value;

 if (FActive) then UpdateParams(pctVSync);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetWindowed(const Value: Boolean);
begin
 FWindowed:= Value;

 if (FActive) then UpdateParams(pctWindowed);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetWindowHandle(const Value: THandle);
begin
 if (not FActive) then FWindowHandle:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetHighBitDepth(const Value: Boolean);
begin
 if (not FActive) then FHighBitDepth:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetAdapter(const Value: Integer);
begin
 if (not FActive) then
  FAdapter:= Max2(Value, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetMultisamples(const Value: Integer);
begin
 if (not FActive) then
  FMultisamples:= Max2(Value, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.SetDepthStencil(const Value: TDepthStencilType);
begin
 if (not FActive) then
  FDepthStencil:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.Render(Handler: TNotifyEvent; Background: Cardinal);
begin
 if (FActive)and(MayRender()) then
  RenderWith(FWindowHandle, Handler, Background)
   else SleepEx(5, True);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.Render(hWnd: THandle; Handler: TNotifyEvent;
 Background: Cardinal);
begin
 if (FActive)and(MayRender()) then
  RenderWith(hWnd, Handler, Background)
   else SleepEx(5, True);
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.RenderTo(Handler: TNotifyEvent; Background: Cardinal;
 FillBk: Boolean; Texture: TAsphyreRenderTargetTexture);
begin
 if (not FActive)or(not MayRender())or(Texture = nil) then Exit;

 if (not Texture.BeginDrawTo()) then Exit;

 RenderToTarget(Handler, Background, FillBk);

 Texture.EndDrawTo();
end;

//---------------------------------------------------------------------------
procedure TAsphyreDevice.Reset();
begin
 EventDeviceLost.Notify(Self, nil);

 ResetDevice();

 EventDeviceReset.Notify(Self, nil);
end;

//---------------------------------------------------------------------------
initialization
 EventDeviceCreate := TEventProvider.Create();
 EventDeviceDestroy:= TEventProvider.Create();
 EventDeviceReset  := TEventProvider.Create();
 EventDeviceLost   := TEventProvider.Create();
 EventBeginScene   := TEventProvider.Create();
 EventEndScene     := TEventProvider.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(EventEndScene);
 FreeAndNil(EventBeginScene);
 FreeAndNil(EventDeviceLost);
 FreeAndNil(EventDeviceReset);
 FreeAndNil(EventDeviceDestroy);
 FreeAndNil(EventDeviceCreate);

//---------------------------------------------------------------------------
end.
