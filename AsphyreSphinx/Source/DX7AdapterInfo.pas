unit DX7AdapterInfo;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SysUtils, DirectDraw7;

//---------------------------------------------------------------------------
type
 PDX7AdapterInfo = ^TDX7AdapterInfo;
 TDX7AdapterInfo = record
  Guid      : TGuid;
  DriverDesc: string;
  DriverName: string;
  Monitor   : HMonitor;
 end;

//---------------------------------------------------------------------------
 TDX7AdapterHelper = class
 private
  FUpdated: Boolean;
  InfoItems: array of TDX7AdapterInfo;

  function InsertInfoItem(const InfoItem: TDX7AdapterInfo): Integer;

  function GetAdapterCount(): Integer;
  function GetAdapterInfo(Index: Integer): PDX7AdapterInfo;
 public
  property Updated: Boolean read FUpdated;

  property AdapterCount: Integer read GetAdapterCount;
  property AdapterInfo[Index: Integer]: PDX7AdapterInfo read GetAdapterInfo;

  function Update(): Boolean;
  procedure Release();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 DX7AdapterHelper: TDX7AdapterHelper = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function EnumCallback(Guid: PGUID; DriverDesc: PAnsiChar; DriverName: PAnsiChar;
 Context: Pointer; Monitor: HMonitor): LongBool; stdcall;
 var
 InfoItem: TDX7AdapterInfo;
 Helper  : TDX7AdapterHelper;
begin
 if (Monitor <> 0) then
  begin
   Move(Guid^, InfoItem.Guid, SizeOf(TGuid));

   InfoItem.DriverDesc:= DriverDesc;
   InfoItem.DriverName:= DriverName;
   InfoItem.Monitor   := Monitor;

   Helper:= TDX7AdapterHelper(Context);
   Helper.InsertInfoItem(InfoItem);
  end;

 Result:= True;
end;

//---------------------------------------------------------------------------
constructor TDX7AdapterHelper.Create();
begin
 inherited;

end;

//---------------------------------------------------------------------------
destructor TDX7AdapterHelper.Destroy();
begin

 inherited;
end;

//---------------------------------------------------------------------------
function TDX7AdapterHelper.Update(): Boolean;
begin
 Result:= FUpdated;
 if (Result) then Exit;
 
 Release();

 {$ifdef fpc}
 Result:= Succeeded(DirectDrawEnumerateEx(@EnumCallback, Pointer(Self),
  DDENUM_ATTACHEDSECONDARYDEVICES));
 {$else}
 Result:= Succeeded(DirectDrawEnumerateEx(EnumCallback, Pointer(Self),
  DDENUM_ATTACHEDSECONDARYDEVICES));
 {$endif}

 FUpdated:= Result; 
end;

//---------------------------------------------------------------------------
function TDX7AdapterHelper.InsertInfoItem(
 const InfoItem: TDX7AdapterInfo): Integer;
begin
 Result:= Length(InfoItems);
 SetLength(InfoItems, Result + 1);

 InfoItems[Result]:= InfoItem;
end;

//---------------------------------------------------------------------------
function TDX7AdapterHelper.GetAdapterCount(): Integer;
begin
 Result:= Length(InfoItems);
end;

//---------------------------------------------------------------------------
function TDX7AdapterHelper.GetAdapterInfo(Index: Integer): PDX7AdapterInfo;
begin
 if (Index >= 0)and(Index < Length(InfoItems)) then
  Result:= @InfoItems[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7AdapterHelper.Release();
begin
 SetLength(InfoItems, 0);
 FUpdated:= False;
end;

//---------------------------------------------------------------------------
initialization
 DX7AdapterHelper:= TDX7AdapterHelper.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(DX7AdapterHelper);

//---------------------------------------------------------------------------
end.
