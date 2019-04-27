unit LockRenderTarget;

interface

uses
  winapi.Messages, system.SysUtils, system.Classes, vcl.Controls, vcl.Forms, vcl.Dialogs, vcl.ExtCtrls,
  DX9Textures, windows, math, Graphics, vcl.StdCtrls, AsphyreRenderTargets,System.Types;

type

  TLockableRenderTarget = class(TAsphyreRenderTargets)
  private
    FWidth: Integer;
    FHeight: Integer;
    FOutputTexture: TDX9LockableTexture;
  public
    property OutputTexture: TDX9LockableTexture read FOutputTexture write FOutputTexture;
    procedure SetSize(Width, Height: Integer);
    procedure Lock(TextureIndex: Integer; out Bits: Pointer; out Pitch: Integer);
    procedure UnLock;
    destructor Destroy(); override;
  end;

implementation

uses
  Vectors2, Vectors2px, AsphyreTimer, AsphyreFactory, AsphyreTypes,
  AsphyreDb, AbstractDevices, AsphyreImages, AsphyreFonts, DX9Providers,
  AbstractCanvas, DX9Types, Direct3d9;

procedure TLockableRenderTarget.SetSize(Width, Height: Integer);
begin
  FOutputTexture := TDX9LockableTexture.Create;
  FOutputTexture.Width := Width;
  FOutputTexture.Height := Height;
  FWidth := Width;
  FHeight := Height;
  FOutputTexture.Format := apf_A8R8G8B8;
  FOutputTexture.SystemPool := True;
  FOutputTexture.Initialize;
end;

procedure TLockableRenderTarget.Lock(TextureIndex: Integer; out Bits: Pointer; out Pitch: Integer);
var
  SrcSurface, DestSurface: IDIRECT3DSURFACE9;
begin
  FOutputTexture.Texture.GetSurfaceLevel(0, DestSurface);
  if TDX9RenderTargetTexture(Texture[TextureIndex]).Texture.GetSurfaceLevel(0, SrcSurface) = D3DERR_INVALIDCALL then
    ShowMessage('error get');
  if Device9.GetRenderTargetData(SrcSurface, DestSurface) = D3DERR_INVALIDCALL then
    ShowMessage('error 1');
  FOutputTexture.Lock(Rect(0, 0, FWidth, FHeight), Bits, Pitch);
end;

procedure TLockableRenderTarget.UnLock;
begin
  FOutputTexture.UnLock;
end;

destructor TLockableRenderTarget.Destroy;
begin
  FreeAndNil(FOutputTexture);
  inherited;
end;
end.
