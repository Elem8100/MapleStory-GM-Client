unit SoftRastTextures;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, SysUtils, AsphyreTypes, AbstractTextures, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TSRLockableTexture = class(TAsphyreLockableTexture)
 private
  FSurface: TSystemSurface;
 protected
  procedure UpdateSize(); override;
  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Surface: TSystemSurface read FSurface;

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); override;
  procedure Unlock(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors;

//---------------------------------------------------------------------------
constructor TSRLockableTexture.Create();
begin
 inherited;

 FSurface:= nil;
end;

//---------------------------------------------------------------------------
function TSRLockableTexture.CreateTexture(): Boolean;
begin
 if (Width < 1)or(Height < 1) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'CreateTexture');
   Result:= False;
   Exit;
  end;

 FSurface:= TSystemSurface.Create();
 FSurface.SetSize(Width, Height);
 FSurface.Clear(0);

 FFormat:= apf_A8R8G8B8;

 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TSRLockableTexture.DestroyTexture();
begin
 if (FSurface <> nil) then FreeAndNil(FSurface);
end;

//---------------------------------------------------------------------------
procedure TSRLockableTexture.Lock(const Rect: TRect; out Bits: Pointer;
 out Pitch: Integer);
begin
 if (Rect.Left < 0)or(Rect.Top < 0)or(Rect.Right > FSurface.Width)or
  (Rect.Bottom > FSurface.Height)or(FSurface = nil) then
  begin
   Bits := nil;
   Pitch:= 0;
   Errors.Insert(errInvalidCall, Self, ClassName, 'Lock');
   Exit;
  end;

 Bits:= Pointer(Integer(FSurface.Bits) + (Rect.Top * FSurface.Pitch) +
  (Rect.Left * 4));
 Pitch:= FSurface.Pitch;
end;

//---------------------------------------------------------------------------
procedure TSRLockableTexture.Unlock();
begin
end;

//---------------------------------------------------------------------------
procedure TSRLockableTexture.UpdateSize();
begin
 FSurface.SetSize(Width, Height);
 FSurface.Clear(0);
end;

//---------------------------------------------------------------------------
end.
