// ----------------------------------------------------------------------------
// ACtrlHelpers.pas            Modified: 02-10-2010               Version: 0.8
// ----------------------------------------------------------------------------
// Original: English
// List Helpers
// ----------------------------------------------------------------------------
// Translation: Portuguese
// Rotinas de apoio a Listas
// ----------------------------------------------------------------------------
// Created by Marcos Gomes.
// ----------------------------------------------------------------------------
unit ACtrlHelpers;

interface

uses
  Classes;

// ----------------------------------------------------------------------------
// List Helpers
// ----------------------------------------------------------------------------
procedure ListAdd(var List: TList; Item: Pointer);
procedure ListRemove(var List: TList; Item: Pointer);
// ----------------------------------------------------------------------------
implementation

// ----------------------------------------------------------------------------
// List Helpers
// ----------------------------------------------------------------------------
procedure ListAdd(var List: TList; Item: Pointer);
begin
  if List = nil then
    List := TList.Create;
  List.Add(Item);
end;

procedure ListRemove(var List: TList; Item: Pointer);
var
  Count: Integer;
begin
  Count := List.Count;
  if Count > 0 then
  begin
    { On destruction usually the last item is deleted first }
    if List[Count - 1] = Item then
      List.Delete(Count - 1)
    else
      List.Remove(Item);
  end;
  if List.Count = 0 then
  begin
    List.Free;
    List := nil;
  end;
end;
// ----------------------------------------------------------------------------
end.
