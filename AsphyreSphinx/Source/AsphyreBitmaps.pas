unit AsphyreBitmaps;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TAsphyreCustomBitmap = class
 protected
  FDesc: ShortString;
 public
  property Desc: ShortString read FDesc;

  function LoadFromStream(const Extension: ShortString; Stream: TStream;
   Dest: TSystemSurface): Boolean; virtual; abstract;
  function SaveToStream(const Extension: ShortString; Stream: TStream;
   Source: TSystemSurface): Boolean; virtual; abstract;

  constructor Create();
 end;

//---------------------------------------------------------------------------
 TBitmapAssociation = record
  Extension: ShortString;
  Handler  : TAsphyreCustomBitmap;
 end;

//---------------------------------------------------------------------------
 TAsphyreBitmapManager = class
 private
  Associations: array of TBitmapAssociation;

  function FindExtension(const Extension: ShortString): Integer;
  procedure RemoveAssociation(AsIndex: Integer);
 public
  function RegisterExt(const Extension: ShortString;
   Handler: TAsphyreCustomBitmap): Boolean;
  procedure UnregisterExt(const Extension: ShortString);

  function AssociatedHandler(const Extension: ShortString): TAsphyreCustomBitmap;

  function LoadFromStream(const Extension: ShortString; Stream: TStream;
   Dest: TSystemSurface): Boolean;
  function SaveToStream(const Extension: ShortString; Stream: TStream;
   Source: TSystemSurface): Boolean;

  function LoadFromFile(const FileName: AnsiString;
   Dest: TSystemSurface): Boolean;
  function SaveToFile(const FileName: AnsiString;
   Source: TSystemSurface): Boolean;

  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 BitmapManager: TAsphyreBitmapManager = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors;

//---------------------------------------------------------------------------
constructor TAsphyreCustomBitmap.Create();
begin
 inherited;

 FDesc:= 'Unknown Bitmap';
end;

//---------------------------------------------------------------------------
destructor TAsphyreBitmapManager.Destroy();
begin

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.FindExtension(
 const Extension: ShortString): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Associations) - 1 do
  if (SameText(Associations[i].Extension, Extension)) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.RegisterExt(const Extension: ShortString;
 Handler: TAsphyreCustomBitmap): Boolean;
var
 AsIndex: Integer;
begin
 AsIndex:= FindExtension(Extension);
 Result:= (AsIndex = -1);
 if (not Result) then Exit;

 AsIndex:= Length(Associations);
 SetLength(Associations, AsIndex + 1);
 Associations[AsIndex].Extension:= Extension;
 Associations[AsIndex].Handler  := Handler;
end;

//---------------------------------------------------------------------------
procedure TAsphyreBitmapManager.RemoveAssociation(AsIndex: Integer);
var
 i: Integer;
begin
 for i:= AsIndex to Length(Associations) - 2 do
  Associations[i]:= Associations[i + 1];

 SetLength(Associations, Length(Associations) - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBitmapManager.UnregisterExt(const Extension: ShortString);
var
 AsIndex: Integer;
begin
 AsIndex:= FindExtension(Extension);
 if (AsIndex <> -1) then RemoveAssociation(AsIndex);
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.AssociatedHandler(
 const Extension: ShortString): TAsphyreCustomBitmap;
var
 Index: Integer;
begin
 Result:= nil;

 Index:= FindExtension(Extension);
 if (Index <> -1) then Result:= Associations[Index].Handler;
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.LoadFromStream(const Extension: ShortString;
 Stream: TStream; Dest: TSystemSurface): Boolean;
var
 Handler: TAsphyreCustomBitmap;
begin
 Handler:= AssociatedHandler(Extension);
 if (Handler = nil) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'LoadFromStream');
   Result:= False;
   Exit;
  end;

 Result:= Handler.LoadFromStream(Extension, Stream, Dest);
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.SaveToStream(const Extension: ShortString;
 Stream: TStream; Source: TSystemSurface): Boolean;
var
 Handler: TAsphyreCustomBitmap;
begin
 Handler:= AssociatedHandler(Extension);
 if (Handler = nil) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'SaveToStream');
   Result:= False;
   Exit;
  end;

 Result:= Handler.SaveToStream(Extension, Stream, Source);
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.LoadFromFile(const FileName: AnsiString;
 Dest: TSystemSurface): Boolean;
var
 Handler: TAsphyreCustomBitmap;
 InSt   : TFileStream;
begin
 Handler:= AssociatedHandler(ExtractFileExt(FileName));
 if (Handler = nil) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'LoadFromFile');
   Result:= False;
   Exit;
  end;

 try
  InSt:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
 except
  Errors.Insert(errOpenFile, Self, ClassName, 'LoadFromFile');
  Result:= False;
  Exit;
 end;

 Result:= Handler.LoadFromStream(ExtractFileExt(FileName), InSt, Dest);
 FreeAndNil(InSt);
end;

//---------------------------------------------------------------------------
function TAsphyreBitmapManager.SaveToFile(const FileName: AnsiString;
 Source: TSystemSurface): Boolean;
var
 Handler: TAsphyreCustomBitmap;
 OutSt  : TFileStream;
begin
 Handler:= AssociatedHandler(ExtractFileExt(FileName));
 if (Handler = nil) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'SaveToFile');
   Result:= False;
   Exit;
  end;

 try
  OutSt:= TFileStream.Create(FileName, fmCreate or fmShareExclusive);
 except
  Errors.Insert(errCreateFile, Self, ClassName, 'SaveToFile');
  Result:= False;
  Exit;
 end;

 Result:= Handler.SaveToStream(ExtractFileExt(FileName), OutSt, Source);
 FreeAndNil(OutSt);
end;

//---------------------------------------------------------------------------
initialization
 BitmapManager:= TAsphyreBitmapManager.Create();

//---------------------------------------------------------------------------
finalization
 BitmapManager.Free();
 BitmapManager:= nil;

//---------------------------------------------------------------------------
end.
