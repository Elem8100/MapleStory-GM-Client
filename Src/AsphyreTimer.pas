unit AsphyreTimer;
//---------------------------------------------------------------------------
// AsphyreTimer.pas                                     Modified: 13-Oct-2009
// Single-Core Timer based on Idle event                         Version 3.11
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
//---------------------------------------------------------------------------
// Changes since v2.0:
//
//   * Working internally with latency instead of frame-rate, giving much
//     higher accuracy and avoiding some division problems.
//   * Changed event name from OnRender to OnTimer, to avoid confusion
//     with TAsphyreDevice events.
//   + Added MaxFPS property which will limit OnTimer occurence to certain
//     fixed speed. This is useful to prevent the application using all
//     system's resources just to render a scene like 1000 FPS, which is
//     not necessary anyway.
//   * Changed Speed type to floating-point. This allows choosing fractional
//     processing speeds like "74.2".
//   * Changed MayRender property name to Enabled.
//   + Added Precision property which will inform the user about the
//     precision system used.
//   + Added Delta property which informs the actual ratio between running
//     speed (OnTimer event) and the specified one in Speed property.
//   + Added Latency property, which informs the time elapsed since
//     previous OnTimer event (in milliseconds).
//
// Changes since v3.0:
//
//   * Using 12:20 fixed-point instead of floating-point numbers now, which
//     reduces the consequential errors and prevents any unnecessary jumps
//     in OnProcess event.
//   - Removed Theta parameter from Process() function, because it wasn't of
//     much use and it doesn't work well with fixed-point math.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, Forms, Math, MMSystem;

//---------------------------------------------------------------------------
type
 TPerformancePrecision = (ppLow, ppHigh);

//---------------------------------------------------------------------------
 TAsphyreTimer = class
 private
  FMaxFPS : Integer;
  FSpeed  : Single;
  FEnabled: Boolean;
  FOnTimer: TNotifyEvent;

  FFrameRate: Integer;

  FPrecision : TPerformancePrecision;
  PrevTime  : Cardinal;
  PrevTime64: Int64;
  FOnProcess: TNotifyEvent;
  Processed : Boolean;

  LatencyFP : Integer;
  DeltaFP   : Integer;
  HighFreq  : Int64;
  MinLatency: Integer;
  SpeedLatcy: Integer;
  FixedDelta: Integer;

  SampleLatency: Integer;
  SampleIndex: Integer;
  FSingleCallOnly: Boolean;

  function RetreiveLatency(): Integer;
  procedure AppIdle(Sender: TObject; var Done: Boolean);
  procedure SetSpeed(const Value: Single);
  procedure SetMaxFPS(const Value: Integer);
  function GetDelta(): Single;
  function GetLatency(): Single;
 public
  property Delta  : Single read GetDelta;
  property Latency: Single read GetLatency;
  property FrameRate: Integer read FFrameRate;

  // The speed at which processing will be made.
  property Speed: Single read FSpeed write SetSpeed;
  // The maximum allowed frame rate.
  property MaxFPS: Integer read FMaxFPS write SetMaxFPS;
  // Whether this timer is active or not.
  property Enabled: Boolean read FEnabled write FEnabled;
  // The precision of timer's calculations.
  property Precision: TPerformancePrecision read FPrecision;

  // If set to True, this property prevents Timer from trying to fix situations
  // where the processing cannot be made faster than the frame rate. This is
  // useful on dedicated servers where no rendering is made.
  property SingleCallOnly: Boolean read FSingleCallOnly write FSingleCallOnly;

  property OnTimer  : TNotifyEvent read FOnTimer write FOnTimer;
  property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;

  procedure Process();

  procedure Reset();

  constructor Create();
 end;

//---------------------------------------------------------------------------
var
 Timer: TAsphyreTimer = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
const
 FixedHigh = $100000;
 DeltaLimit = {32}8 * FixedHigh;

//---------------------------------------------------------------------------
constructor TAsphyreTimer.Create();
begin
 inherited;

 Speed := 60.0;
 MaxFPS:= 100;

 FPrecision:= ppLow;
 if (QueryPerformanceFrequency(HighFreq)) then FPrecision:= ppHigh;

 if (FPrecision = ppHigh) then
  QueryPerformanceCounter(PrevTime64) else PrevTime:= GetTickCount;

 {$ifdef fpc}
 Application.OnIdle:= @AppIdle;
 {$else}
 Application.OnIdle:= AppIdle;
 {$endif}

 timeBeginPeriod(1);

 FixedDelta := 0;
 FFrameRate := 0;
 SampleLatency:= 0;
 SampleIndex  := 0;
 Processed    := False;
 FSingleCallOnly:= False;
end;

//---------------------------------------------------------------------------
procedure TAsphyreTimer.SetSpeed(const Value: Single);
begin
 FSpeed:= Value;
 if (FSpeed < 1.0) then FSpeed:= 1.0;
 SpeedLatcy:= Round(FixedHigh * 1000.0 / FSpeed);
end;

//---------------------------------------------------------------------------
procedure TAsphyreTimer.SetMaxFPS(const Value: Integer);
begin
 FMaxFPS:= Value;
 if (FMaxFPS < 1) then FMaxFPS:= 1;
 MinLatency:= Round(FixedHigh * 1000.0 / FMaxFPS);
end;

//---------------------------------------------------------------------------
function TAsphyreTimer.GetDelta(): Single;
begin
 Result:= DeltaFP / FixedHigh;
end;

//---------------------------------------------------------------------------
function TAsphyreTimer.GetLatency(): Single;
begin
 Result:= LatencyFP / FixedHigh;
end;

//---------------------------------------------------------------------------
function TAsphyreTimer.RetreiveLatency(): Integer;
var
 CurTime  : Cardinal;
 CurTime64: Int64;
begin
 if (FPrecision = ppHigh) then
  begin
   QueryPerformanceCounter(CurTime64);
   Result:= ((CurTime64 - PrevTime64) * FixedHigh * 1000) div HighFreq;
   PrevTime64:= CurTime64;
  end else
  begin
   CurTime := GetTickCount;
   Result  := (CurTime - PrevTime) * FixedHigh;
   PrevTime:= CurTime;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreTimer.AppIdle(Sender: TObject; var Done: Boolean);
var
 WaitAmount: Integer;
 SampleMax : Integer;
begin
 Done:= False;
                
 // (1) Retreive current latency.
 LatencyFP:= RetreiveLatency();

 // (2) If Timer is disabled, wait a little to avoid using 100% of CPU.
 if (not FEnabled) then
  begin
  // SleepEx(5, True);
   Exit;
  end;

 // (3) Adjust to maximum FPS, if necessary.
 if (LatencyFP < MinLatency) then
  begin
   WaitAmount:= (MinLatency - LatencyFP) div FixedHigh;
  // SleepEx(WaitAmount, True);
  end else WaitAmount:= 0;

 // (4) The running speed ratio.
 DeltaFP:= (Int64(LatencyFP) * FixedHigh) div SpeedLatcy;
 // -> provide Delta limit to prevent auto-loop lockup.
 if (DeltaFP > DeltaLimit) then DeltaFP:= DeltaLimit;

 // (5) Calculate Frame Rate every second.
 SampleLatency:= SampleLatency + LatencyFP + (WaitAmount * FixedHigh);
 if (LatencyFP <= 0) then SampleMax:= 4
  else SampleMax:= (Int64(FixedHigh) * 1000) div LatencyFP;

 Inc(SampleIndex);
 if (SampleIndex >= SampleMax) then
  begin
   FFrameRate   := (Int64(SampleIndex) * FixedHigh * 1000) div SampleLatency;
   SampleLatency:= 0;
   SampleIndex  := 0;
  end;

 // (6) Increase processing queque, if processing was made last time.
 if (Processed) then
  begin
   Inc(FixedDelta, DeltaFP);
   Processed:= False;
  end;

 // (7) Call timer event.
 if (Assigned(FOnTimer)) then FOnTimer(Self);
end;

//---------------------------------------------------------------------------
procedure TAsphyreTimer.Process();
var
 i, Amount: Integer;
begin
 Processed:= True;

 Amount:= FixedDelta div FixedHigh;
 if (Amount < 1) then Exit;

 if (FSingleCallOnly) then
  begin
   Amount:= 1;
   FixedDelta:= 0;
  end;

 if (Assigned(FOnProcess)) then
  for i:= 1 to Amount do
   FOnProcess(Self);

 FixedDelta:= FixedDelta and (FixedHigh - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreTimer.Reset();
begin
 FixedDelta:= 0;
 DeltaFP:= 0;
 RetreiveLatency();
end;

//---------------------------------------------------------------------------
initialization
 Timer:= TAsphyreTimer.Create();

//---------------------------------------------------------------------------
finalization
 Timer.Free();

//---------------------------------------------------------------------------
end.

