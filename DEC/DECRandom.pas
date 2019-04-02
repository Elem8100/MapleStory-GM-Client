{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Author:  (c) 2006 Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

  Description:      Secure protected Random Number Generator based on Yarrow

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*****************************************************************************}

unit DECRandom;

interface

implementation

uses SysUtils, DECUtil, DECFmt, DECHash;

var
  RandomClass: TDECHashClass = THash_SHA1;
  FRegister: array[0..127] of Byte;
  FCounter: Cardinal;
  FHash: TDECHash = nil;

function DoProcess: Byte;
begin
  if not IsObject(FHash, RandomClass) then
    FHash := RandomClass.Create;
  FHash.Init;
  FHash.Calc(FCounter, SizeOf(FCounter));
  FHash.Calc(FRegister, SizeOf(FRegister));
  FHash.Done;
  FRegister[FCounter mod SizeOf(FRegister)] := FRegister[FCounter mod SizeOf(FRegister)] xor FHash.Digest[0];
  Inc(FCounter);
  Result := FHash.Digest[1]; // no real predictable dependency to above FHash.Digest[0] !
end;

procedure DoBuffer(var Buffer; Size: Integer);
var
  I: Integer;
begin
  for I := 0 to Size -1 do
    TByteArray(Buffer)[I] := DoProcess;
end;

procedure DoSeed(const Buffer; Size: Integer);
var
  I: Integer;
  T: Cardinal;
begin
  if Size >= 0 then              
  begin
    // initalize a repeatable Seed  
    FillChar(FRegister, SizeOf(FRegister), 0);
    FCounter := 0;
    for I := 0 to Size -1 do
      FRegister[I mod SizeOf(FRegister)] := FRegister[I mod SizeOf(FRegister)] xor TByteArray(Buffer)[I];
  end else
  begin
    // initalize a non-repeatable Seed based on Timers,
    // ATTENTION! this way isn't secure inpredictable,
    // the user should call RandomSeed(Data, SizeOf(Data)) instead,
    // where Date contains as example collected informations based on Human inputs.
    T := RandomSystemTime;
    for I := Low(FRegister) to High(FRegister) do
    begin
      FRegister[I] := FRegister[I] xor Byte(T);
      T := T shl 1 or T shr 31;
    end;
  end;
  for I := Low(FRegister) to High(FRegister) do
    DoProcess;
  FCounter := 0;
end;

procedure DoInit;
begin
  DoRandomBuffer := DoBuffer;
  DoRandomSeed := DoSeed;
  DoSeed('', 0);
end;

procedure DoDone;
begin
  try
    FHash.Free;
  except
  end;
  FHash := nil;
  FillChar(FRegister, SizeOf(FRegister), 0);
  FCounter := 0;
end;

initialization
  DoInit;

finalization
  DoDone;

end.
