unit PXT.Controls;
(*
 * Copyright (c) 2015 - 2020 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
{< Helper control component that aids rendering in FreePascal/Lazarus. }
interface

{$INCLUDE PXT.Config.inc}

uses
{$IFDEF FPC}
  LCLType, LMessages,
{$ELSE}
  Messages,
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
{$ENDIF}
  Classes, Controls, PXT.Types;

type
  // Helper control that serves as rendering surface.
  TCustomRenderingControl = class(TWinControl)
  private
    FOnPaint: TNotifyEvent;
    function GetWindowHandle: THandle;
  protected
  {$IFDEF FPC}
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  {$ELSE}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure PaintWindow(DC: HDC); override;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

  {$IFDEF FPC}
    // Prevents erasing background to avoid flicker.
    procedure EraseBackground(DC: HDC); override;
  {$ENDIF}

    // Returns handle of the window for device association.
    property WindowHandle: THandle read GetWindowHandle;

    // This event is invoked to repaint the control.
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TRenderingControl = class(TCustomRenderingControl)
  published
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnPaint;
    property OnClick;
    property OnDblClick;
  end;

// Returns handle that can be used for creating graphics device associated with a particular control.
function HandleToWindowControlHandle(const AHandle: THandle): THandle;

implementation

{$IFDEF LINUX}
uses
  gtk2, gdk2x, gtk2proc, Graphics;
{$ENDIF}

{$REGION 'TRenderingControl'}

constructor TCustomRenderingControl.Create(AOwner: TComponent);
begin
  inherited;
  ParentDoubleBuffered := False;
  FDoubleBuffered := False;
{$IFDEF FPC}
  SetInitialBounds(0, 0, 256, 256);
{$ELSE}
  ControlState := ControlState + [csCustomPaint];
{$ENDIF}
end;

function TCustomRenderingControl.GetWindowHandle: THandle;
begin
  Result := HandleToWindowControlHandle(Handle);
end;

{$IFDEF FPC}
procedure TCustomRenderingControl.EraseBackground(DC: HDC);
begin
end;

procedure TCustomRenderingControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if Assigned(FOnPaint) then
    FOnPaint(Self);
  Exclude(FControlState, csCustomPaint);
end;
{$ELSE}
procedure TCustomRenderingControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomRenderingControl.PaintWindow(DC: HDC);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;
{$ENDIF}

{$ENDREGION}
{$REGION 'Global functions'}

function HandleToWindowControlHandle(const AHandle: THandle): THandle;
{$IFDEF LINUX}
var
  LWidget: PGtkWidget;
{$ENDIF}
begin
{$IFDEF LINUX}
  LWidget := GetFixedWidget(PGtkWidget(AHandle));
  gtk_widget_realize(LWidget);
  Result := THandle(GDK_WINDOW_XID(LWidget.window));
{$ELSE}
  Result := AHandle;
{$ENDIF}
end;

{$ENDREGION}

end.
