// ----------------------------------------------------------------------------
// ACtrlEngine.pas             Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// Definition of TControlEngine.
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Definição de TControlEngine.
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlEngine;

interface

uses
  SysUtils, Classes,
  // Asphyre GUI Engine
  AControls, ACtrlForms, ACtrlLabels, ACtrlEditBoxes, ACtrlButtons,
  ACtrlImages, ACtrlProgressBars, ACtrlListBoxes, ACtrlCheckBoxes,
  ACtrlDropPanels, ACtrlSliders;

type
  TControlEngine = class(TCustomEngine)
  public
    function AButton(const Parent, Name: String): TAButton;
    function ACheckBox(const Parent, Name: String): TACheckBox;
    function AEdit(const Parent, Name: String): TAEditBox;
    function AForm(const Name: String): TAForm;
    function AImage(const Parent, Name: String): TAImage;
    function ALabel(const Parent, Name: String): TALabel;
    function AListBox(const Parent, Name: String): TAListBox;
    function AProgressBar(const Parent, Name: String): TAProgressBar;
    function ASlider(const Parent, Name: String): TASlider;
    function ADropPanel(const Parent, Name: String): TADropPanel;

    procedure LoadFromFile(const FileName: string);
  end;

implementation

{ TControlEngine }

function TControlEngine.AButton(const Parent, Name: String): TAButton;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TAButton then
      Result := TAButton(AControl);
end;

function TControlEngine.ACheckBox(const Parent, Name: String): TACheckBox;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TACheckBox then
      Result := TACheckBox(AControl);
end;

function TControlEngine.ADropPanel(const Parent, Name: String): TADropPanel;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TADropPanel then
      Result := TADropPanel(AControl);
end;

function TControlEngine.AEdit(const Parent, Name: String): TAEditBox;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TAEditBox then
      Result := TAEditBox(AControl);
end;

function TControlEngine.AForm(const Name: String): TAForm;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Name, False);

  if AControl <> nil then
    if AControl is TAForm then
      Result := TAForm(AControl);
end;

function TControlEngine.AImage(const Parent, Name: String): TAImage;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TAImage then
      Result := TAImage(AControl);
end;

function TControlEngine.ALabel(const Parent, Name: String): TALabel;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TALabel then
      Result := TALabel(AControl);
end;

function TControlEngine.AListBox(const Parent, Name: String): TAListBox;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TAListBox then
      Result := TAListBox(AControl);
end;

function TControlEngine.AProgressBar(const Parent, Name: String): TAProgressBar;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TAProgressBar then
      Result := TAProgressBar(AControl);
end;

function TControlEngine.ASlider(const Parent, Name: String): TASlider;
var
  AControl: TAControl;
begin
  Result := nil;

  AControl := Self.Root.FindChildControl(Parent, False);

  if AControl <> nil then
    if AControl is TAForm then
      AControl := TAForm(AControl).FindChildControl(Name, True);

  if AControl <> nil then
    if AControl is TASlider then
      Result := TASlider(AControl);
end;

procedure TControlEngine.LoadFromFile(const FileName: string);
var
  AControl: TWControl;
begin
  try
    AControl := ReadComponentResFile(FileName, nil) as TWControl;
    Root.Assign(AControl);
  finally
    FreeAndNil(AControl);
  end;
end;

end.
