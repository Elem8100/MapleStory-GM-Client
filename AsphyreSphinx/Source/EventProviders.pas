unit EventProviders;
//---------------------------------------------------------------------------
// EventProviders.pas                                   Modified: 30-Dec-2010
//---------------------------------------------------------------------------
//
// Observer Pattern implementation, also called Subscriber-Publisher.
//
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
type
 TEventCallback = procedure(Sender: TObject; Param: Pointer;
  var Handled: Boolean) of object;

//---------------------------------------------------------------------------
 PEventItem = ^TEventItem;
 TEventItem = record
  EventID : Cardinal;
  Callback: TEventCallback;
  Priority: Integer;
 end;

//---------------------------------------------------------------------------
type
 TEventProvider = class
 private
  CurrentID : Cardinal;
  EventItems: array of TEventItem;
  OrderDirty: Boolean;

  function NextID(): Cardinal;
  function IndexOf(EventID: Cardinal): Integer;
  procedure Remove(Index: Integer);

  procedure EventListSwap(Index1, Index2: Integer);
  function EventListCompare(Item1, Item2: PEventItem): Integer;
  function EventListSplit(Start, Stop: Integer): Integer;
  procedure EventListSort(Start, Stop: Integer);
  procedure SortCallbacks();
 public
  function Subscribe(Callback: TEventCallback; Priority: Integer): Cardinal;
  procedure Unsubscribe(EventID: Cardinal);

  function Notify(Sender: TObject; Param: Pointer): Boolean;

  constructor Create();
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TEventProvider.Create();
begin
 inherited;

 CurrentID:= 0;
end;

//---------------------------------------------------------------------------
function TEventProvider.NextID(): Cardinal;
begin
 Result:= CurrentID;
 Inc(CurrentID);
end;

//---------------------------------------------------------------------------
function TEventProvider.IndexOf(EventID: Cardinal): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(EventItems) - 1 do
  if (EventItems[i].EventID = EventID) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TEventProvider.Subscribe(Callback: TEventCallback;
 Priority: Integer): Cardinal;
var
 Index: Integer;
begin
 Result:= NextID();

 Index:= Length(EventItems);
 SetLength(EventItems, Index + 1);

 EventItems[Index].EventID := Result;
 EventItems[Index].Callback:= Callback;
 EventItems[Index].Priority:= Priority;

 OrderDirty:= True;
end;

//---------------------------------------------------------------------------
procedure TEventProvider.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(EventItems)) then Exit;

 for i:= Index to Length(EventItems) - 2 do
  EventItems[i]:= EventItems[i + 1];

 SetLength(EventItems, Length(EventItems) - 1);

 OrderDirty:= True;
end;

//---------------------------------------------------------------------------
procedure TEventProvider.Unsubscribe(EventID: Cardinal);
begin
 Remove(IndexOf(EventID));
end;

//---------------------------------------------------------------------------
procedure TEventProvider.EventListSwap(Index1, Index2: Integer);
var
 Aux: TEventItem;
begin
 Aux:= EventItems[Index1];

 EventItems[Index1]:= EventItems[Index2];
 EventItems[Index2]:= Aux;
end;

//---------------------------------------------------------------------------
function TEventProvider.EventListCompare(Item1, Item2: PEventItem): Integer;
begin
 Result:= 0;

 if (Item1.Priority > Item2.Priority) then Result:= 1;
 if (Item1.Priority < Item2.Priority) then Result:= -1;

 if (Result = 0) then
  begin
   if (Item1.EventID > Item2.EventID) then Result:= 1;
   if (Item1.EventID < Item2.EventID) then Result:= -1;
  end;
end;

//---------------------------------------------------------------------------
function TEventProvider.EventListSplit(Start, Stop: Integer): Integer;
var
 Left, Right: Integer;
 Pivot: PEventItem;
begin
 Left := Start + 1;
 Right:= Stop;
 Pivot:= @EventItems[Start];

 while (Left <= Right) do
  begin
   while (Left <= Stop)and(EventListCompare(@EventItems[Left], Pivot) < 0) do
    Inc(Left);

   while (Right > Start)and(EventListCompare(@EventItems[Right], Pivot) >= 0) do
    Dec(Right);

   if (Left < Right) then EventListSwap(Left, Right);
  end;

 EventListSwap(Start, Right);

 Result:= Right;
end;

//---------------------------------------------------------------------------
procedure TEventProvider.EventListSort(Start, Stop: Integer);
var
 SplitPt: Integer;
begin
 if (Start < Stop) then
  begin
   SplitPt:= EventListSplit(Start, Stop);

   EventListSort(Start, SplitPt - 1);
   EventListSort(SplitPt + 1, Stop);
  end;
end;

//---------------------------------------------------------------------------
procedure TEventProvider.SortCallbacks();
begin
 if (Length(EventItems) > 1) then EventListSort(0, Length(EventItems) - 1);
 OrderDirty:= False;
end;

//---------------------------------------------------------------------------
function TEventProvider.Notify(Sender: TObject; Param: Pointer): Boolean;
var
 i: Integer;
begin
 if (OrderDirty) then SortCallbacks();

 Result:= False;
 for i:= 0 to Length(EventItems) - 1 do
  begin
   EventItems[i].Callback(Sender, Param, Result);
   if (Result) then Break;
  end;
end;

//---------------------------------------------------------------------------
end.
