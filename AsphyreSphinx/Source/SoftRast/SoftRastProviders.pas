unit SoftRastProviders;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreFactory, AbstractDevices, AbstractCanvas, AbstractTextures,
 AbstractRasterizer;

//---------------------------------------------------------------------------
const
 idSoftRast = $00C00C00;

//---------------------------------------------------------------------------
type
 TSoftRastProvider = class(TAsphyreProvider)
 private
 public
  function CreateDevice(): TAsphyreDevice; override;
  function CreateCanvas(): TAsphyreCanvas; override;
  function CreateRasterizer(): TAsphyreRasterizer; override;
  function CreateLockableTexture(): TAsphyreLockableTexture; override;
  function CreateRenderTargetTexture(): TAsphyreRenderTargetTexture; override;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 SoftRastProvider: TSoftRastProvider = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 SoftRastDevices, SoftRastCanvas, SoftRastTextures;

//---------------------------------------------------------------------------
constructor TSoftRastProvider.Create();
begin
 inherited;

 FProviderID:= idSoftRast;

 Factory.Subscribe(Self);
end;

//---------------------------------------------------------------------------
destructor TSoftRastProvider.Destroy();
begin
 Factory.Unsubscribe(Self, True);

 inherited;
end;

//---------------------------------------------------------------------------
function TSoftRastProvider.CreateDevice(): TAsphyreDevice;
begin
 Result:= TSoftRastDevice.Create();
end;

//---------------------------------------------------------------------------
function TSoftRastProvider.CreateCanvas(): TAsphyreCanvas;
begin
 Result:= TSoftRastCanvas.Create();
end;

//---------------------------------------------------------------------------
function TSoftRastProvider.CreateRasterizer(): TAsphyreRasterizer;
begin
 Result:= nil;
end;

//---------------------------------------------------------------------------
function TSoftRastProvider.CreateLockableTexture(): TAsphyreLockableTexture;
begin
 Result:= TSRLockableTexture.Create();
end;

//---------------------------------------------------------------------------
function TSoftRastProvider.CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;
begin
 Result:= nil
end;

//---------------------------------------------------------------------------
initialization
 SoftRastProvider:= TSoftRastProvider.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(SoftRastProvider);

//---------------------------------------------------------------------------
end.
