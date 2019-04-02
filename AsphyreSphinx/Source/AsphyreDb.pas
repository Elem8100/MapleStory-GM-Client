unit AsphyreDb;
//---------------------------------------------------------------------------
// AsphyreDb.pas                                        Modified: 29-Dec-2010
// Asphyre Secure Database (ASDb) implementation                 Version 1.14
//---------------------------------------------------------------------------
// Important Notice:
//
// If you modify/use this code or one of its parts either in original or
// modified form, you must comply with Mozilla Public License v1.1,
// specifically section 3, "Distribution Obligations". Failure to do so will
// result in the license breach, which will be resolved in the court.
// Remember that violating author's rights is considered a serious crime in
// many countries. Thank you!
//
// !! Please *read* Mozilla Public License 1.1 document located at:
//  http://www.mozilla.org/MPL/
//
// If you require any clarifications about the license, feel free to contact
// us or post your question on our forums at: http://www.afterwarp.net
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is AsphyreDb.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2011,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
{$ifdef fpc}{$mode delphi}{$endif}
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, Math, SysUtils, AsphyreMD5, AsphyreXTEA;

//---------------------------------------------------------------------------
const
 ASDbSignature = $62445341; // 'ASDb'

 // record type enumerations
 recUnknown    = 0;
 recGraphics   = 1;
 recFile       = 2;

//---------------------------------------------------------------------------
type
 PASDbHeader = ^TASDbHeader;
 TASDbHeader = packed record 
  Signature  : Longword; // signature ('ASDb' = 62445341h)
  RecordCount: Longword; // number of records in the archive
  TableOffset: Longword; // table offset
 end;

//---------------------------------------------------------------------------
{
 ASDb Table structure:
  Key Name      -  4+ bytes (dword: length; [length-bytes]: string chars)
  Offset        -  unsigned dword

 ASDb Record structure:
  RecordType    -  word

  OrigSize      -  unsigned dword
  PhysSize      -  unsigned dword
  DateTime      -  double (unsigned qword)

  Checksum      -  16 bytes (MD5 message-digest)

  Encoding      -  word
  IV            -  unsigned qword (8 bytes) as IV

  DataBlock     -  DataSize bytes
}

//---------------------------------------------------------------------------
 TRecordInfo = record
  Key       : AnsiString; // record unique identifier
  Offset    : Longword;  // record offset in archive
  RecordType: Cardinal;  // type of the record (generic, file, graphics, etc)

  OrigSize: Cardinal;  // original data size
  PhysSize: Cardinal;  // physical data size
  DateTime: TDateTime; // record date & time
  Checksum: TKey128; // MD5 message-digest

  Secure : Boolean;   // whether record is encrypted
  InitVec: TBlock64;
 end;

//---------------------------------------------------------------------------
 TOpenModes = (opUpdate, opOverwrite, opReadOnly);

//---------------------------------------------------------------------------
 TRecordSortCriteria = (rscName, rscType, rscDate, rscOrigSize, rscPhysSize,
  rscSecurity);

//---------------------------------------------------------------------------
 TASDb = class
 private
  FUpdatedOnce: Boolean;
  FFileSize : Cardinal;
  FFileName : AnsiString;
  FOpenMode : TOpenModes;
  FRecords  : array of TRecordInfo;
  ASDbHeader: TASDbHeader;
  FPassword : Pointer;
  InsideKey : TKey128;

  function GetRecordDate(Num: Integer): TDateTime;
  function GetRecordCount(): Integer;
  function GetRecordPhysSize(Num: Integer): Integer;
  function GetRecordKey(Num: Integer): ShortString;
  function GetRecordNum(const Key: ShortString): Integer;
  function GetRecordOrigSize(Num: Integer): Integer;
  procedure SetFileName(const Value: AnsiString);
  function CreateEmtpyFile(): Boolean;
  function GetRecordType(Num: Integer): Integer;
  function GetRecordSecure(Num: Integer): Boolean;
  function GetRecordChecksum(Num: Integer): PKey128;

  function ReadASDbHeader(Stream: TStream; Header: PASDbHeader): Boolean;
  function ReadASDbInfo(Stream: TStream): Boolean;
  function WriteRecordTable(): Boolean;

  function CompressData(Source: Pointer; SourceSize: Cardinal;
   out Data: Pointer; out DataSize: Cardinal): Boolean;
  function DecompressData(Source: Pointer; SourceSize: Cardinal;
   out Data: Pointer; DataSize: Cardinal): Boolean;
  function CompareRecords(Index1, Index2: Integer;
   Criteria: TRecordSortCriteria): Integer; 
 public
  //=========================================================================
  // PUBLIC Properties
  //=========================================================================
  property UpdatedOnce: Boolean read FUpdatedOnce;

  // The name of the archive
  property FileName: AnsiString read FFileName write SetFileName;

  // open mode (e.g. WriteBuffer-only)
  property OpenMode: TOpenModes read FOpenMode write FOpeNmode;

  // Indicates the size of the file.
  property FileSize: Cardinal read FFileSize;

  // This pointer is used to store the secure password.
  property Password: Pointer read FPassword write FPassword;

  property RecordCount: Integer read GetRecordCount;
  property RecordKey[Num: Integer]: ShortString read GetRecordKey;
  property RecordPhysSize[Num: Integer]: Integer read GetRecordPhysSize;
  property RecordOrigSize[Num: Integer]: Integer read GetRecordOrigSize;
  property RecordNum[const Key: ShortString]: Integer read GetRecordNum;
  property RecordType[Num: Integer]: Integer read GetRecordType;
  property RecordDate[Num: Integer]: TDateTime read GetRecordDate;
  property RecordSecure[Num: Integer]: Boolean read GetRecordSecure;
  property RecordChecksum[Num: Integer]: PKey128 read GetRecordChecksum;

  //=========================================================================
  // PUBLIC Methods
  //=========================================================================
  procedure SetPassword(const Pass: ShortString);
  procedure BurnPassword();

  // writes the specific record to ASDb archive
  function WriteRecord(const Key: ShortString; Source: Pointer;
   SourceSize: Cardinal; RecType: Integer): Boolean;

  // writes the entire stream to ASDb archive
  function WriteStream(const Key: ShortString; Stream: TStream;
   RecType: Integer): Boolean;

  function WriteString(const Key: ShortString; const Text: AnsiString;
   RecType: Integer): Boolean;

  // reads the specified record from ASDb archive
  // NOTE: this method allocates memory which needs to be freed by FreeMem
  function ReadRecord(const Key: ShortString; out Data: Pointer;
   out DataSize: Cardinal): Boolean;

  // reads the record and stores it in the stream
  function ReadStream(const Key: ShortString; Stream: TStream): Boolean;

  // reads ASDb record contents as a text string
  function ReadString(const Key: ShortString; out Text: AnsiString): Boolean;

  // removes the record from archive
  function RemoveRecord(const Key: ShortString): Boolean;

  // changes the key of the record without physically moving it
  function RenameRecord(const Key, NewKey: ShortString): Boolean;

  // switches the positions of two records
  function SwitchRecords(Index1, Index2: Integer): Boolean;

  // sorts the records by type not affecting order of items with the same type
  function SortRecords(Criteria: TRecordSortCriteria; Invert: Boolean): Boolean;

  constructor Create();

  // updates the list of ASDb records
  function Update(): Boolean;

  // updates the record list only once
  function UpdateOnce(): Boolean;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 StreamUtils, AsphyreData;

//---------------------------------------------------------------------------
const
 // A record name returned when invalid index is specified
 invRecordName = '[invalid-record-#]';

 // When using compression, a temporary buffer is used to store the final
 // output. Under certain circumstances, the output data size is bigger than
 // the original. For these cases, output buffer is created slightly bigger
 // than the original. The additional percentage added is specified below.
 BufferGrow    = 5; // default: 5 (in %)

 // For the same purpose as BufferGrow, this value is simply added to the
 // buffer size previously increased by BufferGrow (for very short buffers).
 BufferGrowAdd = 256; // default: 256

 // In original record position, this offset determines where record data
 // is allocated. This is used for ReadRecord method to get directly to
 // record data. Also used for removing records.
 DataOffset    = 44;

 // Temporary archive name to be used when deleting or overwriting records
 TempFileText  = 'asdb.tmp';

//---------------------------------------------------------------------------
constructor TASDb.Create();
begin
 inherited;

 FUpdatedOnce:= False;
 FFileSize:= 0;
 FFileName:= '';
 FOpenMode:= opUpdate;
 FPassword:= nil;

 SetLength(FRecords, 0);
 FillChar(ASDbHeader, SizeOf(TASDbHeader), 0);
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordCount(): Integer;
begin
 Result:= Length(FRecords);
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordPhysSize(Num: Integer): Integer;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].PhysSize;
  end else Result:= 0;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordOrigSize(Num: Integer): Integer;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].OrigSize;
  end else Result:= 0;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordType(Num: Integer): Integer;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].RecordType;
  end else Result:= 0;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordKey(Num: Integer): ShortString;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].Key;
  end else Result:= invRecordName;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordDate(Num: Integer): TDateTime;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].DateTime;
  end else Result:= Now();
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordSecure(Num: Integer): Boolean;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= FRecords[Num].Secure;
  end else Result:= False;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordChecksum(Num: Integer): PKey128;
begin
 if (Num >= 0)and(Num < Length(FRecords)) then
  begin
   Result:= @FRecords[Num].Checksum;
  end else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TASDb.SetFileName(const Value: AnsiString);
begin
 FFileName:= Value;
 FUpdatedOnce:= False;
end;

//---------------------------------------------------------------------------
procedure TASDb.SetPassword(const Pass: ShortString);
begin
 MD5Checksum(@Pass[1], Length(Pass), @InsideKey);
 FPassword:= @InsideKey;
end;

//---------------------------------------------------------------------------
procedure TASDb.BurnPassword();
begin
 FillChar(InsideKey, SizeOf(TKey128), 0);
 FPassword:= nil;
end;

//---------------------------------------------------------------------------
function TASDb.GetRecordNum(const Key: ShortString): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(FRecords) - 1 do
  if (SameText(FRecords[i].Key, Key)) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TASDb.CreateEmtpyFile(): Boolean;
var
 fs: TStream;
begin
 // prepare empty header
 FillChar(ASDbHeader, SizeOf(TASDbHeader), 0);
 ASDbHeader.Signature:= ASDbSignature;
 ASDbHeader.RecordCount:= 0;
 // offset to non-existant table
 ASDbHeader.TableOffset:= SizeOf(TASDbHeader);

 // create file stream
 try
  fs:= TFileStream.Create(FFileName, fmCreate or fmShareExclusive);
 except
  Result:= False;
  Exit;
 end;

 // write header
 Result:= True;
 try
  fs.WriteBuffer(ASDbHeader, SizeOf(ASDbHeader));
 except
  Result:= False;
 end;

 // free file stream
 FreeAndNil(fs);

 // file size
 FFileSize:= SizeOf(ASDbHeader);

 // assume no records exist
 SetLength(FRecords, 0);
end;

//---------------------------------------------------------------------------
function TASDb.ReadASDbHeader(Stream: TStream; Header: PASDbHeader): Boolean;
begin
 if (Stream = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Result:= True;
 try
  // read ASDb Header
  Stream.Seek(0, soFromBeginning);
  Stream.ReadBuffer(Header^, SizeOf(TASDbHeader));
 except
  Result:= False;
 end;
end;

//---------------------------------------------------------------------------
function TASDb.ReadASDbInfo(Stream: TStream): Boolean;
var
 i: Integer;
 NoStream: Boolean;
begin
 // release records
 SetLength(FRecords, 0);
 NoStream:= (Stream = nil);

 if (NoStream) then
  begin
   // open the specified file
   try
    Stream:= TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
   except
    Result:= False;
    Exit;
   end;
  end;

 // read & validate ASDbHeader
 Result:= ReadASDbHeader(Stream, @ASDbHeader);
 if (not Result) then
  begin
   if (NoStream) then FreeAndNil(Stream);
   Exit;
  end;

 // retreive file size
 FFileSize:= Stream.Size;

 // seek record table in archive
 Stream.Seek(ASDbHeader.TableOffset, soFromBeginning);

 // specify record count
 SetLength(FRecords, ASDbHeader.RecordCount);

 // read record names and positions
 try
  for i:= 0 to Length(FRecords) - 1 do
   begin
    // read record name
    FRecords[i].Key:= StreamGetSimpleAnsiString(Stream);
    FRecords[i].Offset:= StreamGetLongword(Stream);
   end;
 except
  Result:= False;
 end;

 // check for Read errors
 if (not Result) then
  begin
   // no records are saved on error
   SetLength(FRecords, 0);
   if (NoStream) then FreeAndNil(Stream);
   Exit;
  end;

 // check if any records exist in archive
 if (ASDbHeader.RecordCount < 1) then
  begin
   if (NoStream) then FreeAndNil(Stream);
   Exit;
  end;

 // read record detailed information
 try
  for i:= 0 to Length(FRecords) - 1 do
   begin
    // seek record's position
    Stream.Seek(FRecords[i].Offset, soFromBeginning);

    // record type
    FRecords[i].RecordType:= StreamGetWord(Stream);

    // basic info
    FRecords[i].OrigSize:= StreamGetLongword(Stream);
    FRecords[i].PhysSize:= StreamGetLongword(Stream);
    FRecordS[i].DateTime:= StreamGetDouble(Stream);

    // MD5 message-digest of record's contents
    Stream.ReadBuffer(FRecords[i].Checksum, SizeOf(TKey128));

    // security information
    FRecords[i].Secure:= Boolean(StreamGetWord(Stream));
    Stream.ReadBuffer(FRecords[i].InitVec, SizeOf(TBlock64));
   end; // for
 except
  Result:= False;
 end;

 // release stream's memory
 if (NoStream) then FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TASDb.Update(): Boolean;
begin
 Result:= True;

 // act depending of opening mode
 case FOpenMode of
  // create new file
  opOverwrite:
   begin
    Result:= CreateEmtpyFile();
    if (Result) then FOpenMode:= opUpdate;
   end;

  // open file for reading
  opReadOnly:
   Result:= ReadASDbInfo(nil);

  // open file for update
  opUpdate:
   begin
    if (FileExists(FFileName)) then Result:= ReadASDbInfo(nil)
     else Result:= CreateEmtpyFile();
   end;
 end;

 FUpdatedOnce:= Result;
end;

//---------------------------------------------------------------------------
function TASDb.UpdateOnce(): Boolean;
begin
 Result:= FUpdatedOnce;
 if (not Result) then Result:= Update();
end;

//---------------------------------------------------------------------------
function TASDb.CompressData(Source: Pointer; SourceSize: Cardinal;
 out Data: Pointer; out DataSize: Cardinal): Boolean;
var
 CodeBuf   : Pointer;
 BufferSize: Cardinal;
begin
 Result:= True;

 // guaranteed buffer size
 BufferSize:= Ceil((SourceSize * (100 + BufferGrow)) / 100) + BufferGrowAdd;

 // allocate encoding buffer
 GetMem(CodeBuf, BufferSize);

 // inflate the buffer
 DataSize:= AsphyreData.CompressData(Source, CodeBuf, SourceSize, BufferSize,
  clHighest);
 if (DataSize = 0) then
  begin
   FreeMem(CodeBuf);
   Result:= False;
   Exit;
  end;

 // allocate real data container
 GetMem(Data, DataSize);

 // copy the compressed data
 Move(CodeBuf^, Data^, DataSize);

 // release encoding buffer
 FreeMem(CodeBuf);
end;

//---------------------------------------------------------------------------
function TASDb.DecompressData(Source: Pointer; SourceSize: Cardinal;
 out Data: Pointer; DataSize: Longword): Boolean;
var
 OutSize: Integer;
begin
 Result:= True;

 // allocate output buffer
 GetMem(Data, DataSize);

 // decompress the data stream
 OutSize:= AsphyreData.DecompressData(Source, Data, SourceSize, DataSize);
 if (OutSize = 0)or(Int64(OutSize) <> DataSize) then
  begin
   FreeMem(Data);
   Data:= nil;
   Result:= False;
  end;
end;

//---------------------------------------------------------------------------
function TASDb.WriteRecordTable(): Boolean;
var
 Stream: TFileStream;
 i: Integer;
begin
 Result:= True;

 // (1) OPEN THE ARCHIVE for *writing*
 try
  Stream:= TFileStream.Create(FFileName, fmOpenWrite or fmShareExclusive);
 except
  Result:= False;
  Exit;
 end;

 try
  // (2) go to the position of record table
  Stream.Seek(ASDbHeader.TableOffset, soFromBeginning);

  // (3) flush the record table
  for i:= 0 to Length(FRecords) - 1 do
   begin
    StreamPutSimpleAnsiString(Stream, FRecords[i].Key);
    StreamPutLongword(Stream, FRecords[i].Offset);
   end;
 except
  Result:= False;
 end;

 // (4) release the file stream
 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TASDb.WriteRecord(const Key: ShortString; Source: Pointer;
 SourceSize: Cardinal; RecType: Integer): Boolean;
var
 Data: Pointer;
 DataSize: Cardinal;
 i, NewIndex: Integer;
 Stream: TStream;
 RecordOffset: Cardinal;
 InitVec : TBlock64;
 Checksum: TKey128;
 CurDate : TDateTime;
begin
 Result := False;
 CurDate:= Now();

 // (1) verify open mode
 if (FOpenMode = opReadOnly) then Exit;

 // (2) if the record exists, remove it
 if (GetRecordNum(Key) <> -1) then RemoveRecord(Key);

 // (3) calculate checksum and digest
 MD5Checksum(Source, SourceSize, @Checksum);

 // (4) compress input data
 Result:= CompressData(Source, SourceSize, Data, DataSize);
 if (not Result) then Exit;

 // (5) Apply security
 if (FPassword <> nil) then
  begin
   // -> generate random IV keys
   InitVec[0]:= Round(Random * High(Longword));
   InitVec[1]:= Round(Random * High(Longword));
   // -> encrypt compressed data
   CipherDataXTEA(Data, Data, DataSize, FPassword, @InitVec);
  end else
  begin
   InitVec[0]:= 0;
   InitVec[1]:= 0;
  end;

 // (6) OPEN THE ARCHIVE for reading & writing
 try
  Stream:= TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive);
 except
  Result:= False;
  Exit;
 end;

 // (7) update ASDb info, in case it has been changed
 Result:= ReadASDbInfo(Stream);
 if (not Result) then
  begin
   FreeAndNil(Stream);
   Exit;
  end;

 // (8) if the record still exists, we cannot proceed
 if (GetRecordNum(Key) <> -1) then
  begin
   FreeAndNil(Stream);
   Result:= False;
   Exit;
  end;

 // (9) write the ENTIRE RECORD
 try
  // seek the record table position and write the record there!
  RecordOffset:= ASDbHeader.TableOffset;
  Stream.Seek(RecordOffset, soFromBeginning);

  // RECORD TYPE
  StreamPutWord(Stream, RecType);
  // ORIGINAL SIZE
  StreamPutLongword(Stream, SourceSize);
  // PHYSICAL SIZE
  StreamPutLongword(Stream, DataSize);
  // DATE & TIME
  StreamPutDouble(Stream, CurDate);

  // Checksum: MD5 message-digest
  Stream.WriteBuffer(Checksum, SizeOf(Checksum));

  // Security Information
  StreamPutWord(Stream, Word(FPassword <> nil));
  Stream.WriteBuffer(InitVec, SizeOf(TBlock64));

  // RECORD DATA
  Stream.WriteBuffer(Data^, DataSize);
 except
  Result:= False;
  FreeMem(Data);
  FreeAndNil(Stream);
  Exit;
 end;

 // (10) add new record to the record list
 NewIndex:= Length(FRecords);
 SetLength(FRecords, NewIndex + 1);
 FRecords[NewIndex].Key:= Key;
 Move(Checksum, FRecords[NewIndex].Checksum, SizeOf(Checksum));
 FRecords[NewIndex].RecordType:= RecType;
 FRecords[NewIndex].OrigSize:= SourceSize;
 FRecords[NewIndex].PhysSize:= DataSize;
 FRecords[NewIndex].Offset  := RecordOffset;
 FRecords[NewIndex].DateTime:= CurDate;
 FRecords[NewIndex].Secure  := (FPassword <> nil);
 FRecords[NewIndex].InitVec[0]:= InitVec[0];
 FRecords[NewIndex].InitVec[1]:= InitVec[1];

 // (11) update ASDb Header information
 ASDbHeader.TableOffset:= Stream.Position;
 ASDbHeader.RecordCount:= ASDbHeader.RecordCount + 1;

 try
  // (12) rewrite entire RECORD TABLE
  for i:= 0 to Length(FRecords) - 1 do
   begin
    StreamPutSimpleAnsiString(Stream, FRecords[i].Key);
    StreamPutLongword(Stream, FRecords[i].Offset);
   end;

 // (13) write down ASDb HEADER
 Stream.Seek(0, soFromBeginning);
 Stream.WriteBuffer(ASDbHeader, SizeOf(TASDbHeader));
 except
  Result:= False;
 end;

 // (14) Release the stream and memory
 FreeMem(Data);
 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TASDb.WriteStream(const Key: ShortString; Stream: TStream;
 RecType: Integer): Boolean;
var
 Data: Pointer;
 DataSize, ReadBytes: Integer;
begin
 Result:= False;

 // verify open mode
 if (FOpenMode = opReadOnly) then Exit;

 // allocate memory for stream data
 DataSize:= Stream.Size - Stream.Position;
 Data:= AllocMem(DataSize);

 // read the stream data
 ReadBytes:= Stream.Read(Data^, DataSize);
 if (ReadBytes <> DataSize) then
  begin
   FreeMem(Data);
   Exit;
  end;

 // write the data to ASDb
 Result:= WriteRecord(Key, Data, DataSize, RecType);

 // free the unused memory
 FreeMem(Data);
end;

//---------------------------------------------------------------------------
function TASDb.WriteString(const Key: ShortString; const Text: AnsiString;
 RecType: Integer): Boolean;
begin
 if (Length(Text) < 1) then
  begin
   Result:= False;
   Exit;
  end;

 Result:= WriteRecord(Key, @Text[1], Length(Text), RecType);
end;

//---------------------------------------------------------------------------
function TASDb.ReadRecord(const Key: ShortString; out Data: Pointer;
 out DataSize: Cardinal): Boolean;
var
 PreRelease: Boolean;
 PreBuf  : Pointer;
 PreSize : Cardinal;
 Index   : Integer;
 Stream  : TStream;
 Checksum: TKey128;
begin
 Result:= False;

 // (1) OPEN archive
 try
  Stream:= TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
 except
  Exit;
 end;

 // (2) update ASDb info, in case it has been changed
 Result:= ReadASDbInfo(Stream);
 if (not Result) then
  begin
   FreeAndNil(Stream);
   Exit;
  end;

 // (3) find record index
 Index:= GetRecordNum(Key);
 if (Index = -1) then
  begin
   FreeAndNil(Stream);
   Result:= False;
   Exit;
  end;

 // assign data size
 DataSize:= FRecords[Index].OrigSize;

 // (4) create temporary buffers
 PreSize:= FRecords[Index].PhysSize;
 GetMem(PreBuf, PreSize);
 PreRelease:= True;

 // (5) read the ENTIRE RECORD
 try
  // seek the record position in the file
  Stream.Seek(FRecords[Index].Offset + DataOffset, soFromBeginning);

  // read record data
  Stream.ReadBuffer(PreBuf^, PreSize);
 except
  Result:= False;
  FreeMem(PreBuf);
  FreeAndNil(Stream);
  Exit;
 end;

 // close the file stream
 FreeAndNil(Stream);

 // (6) Apply security
 if (FRecords[Index].Secure)and(FPassword <> nil) then
  begin
   DecipherDataXTEA(PreBuf, PreBuf, PreSize, FPassword,
    @FRecords[Index].InitVec);
  end;

 // (7) decompress the data stream
 Result:= DecompressData(PreBuf, PreSize, Data, DataSize);
 if (not Result) then
  begin
   FreeMem(PreBuf);
   Exit;
  end;

 // (8) release buffers
 if (PreRelease) then FreeMem(PreBuf);

 // (9) checksum verification
 MD5Checksum(Data, DataSize, @Checksum);
 Result:= CompareMem(@Checksum, @FRecords[Index].Checksum, SizeOf(Checksum));
end;

//---------------------------------------------------------------------------
function TASDb.ReadStream(const Key: ShortString; Stream: TStream): Boolean;
var
 Data: Pointer;
 DataSize, BytesWritten: Cardinal;
begin
 // read the record data
 Result:= ReadRecord(Key, Data, DataSize);

 // write the record data to stream
 if (Result) then
  begin
   BytesWritten:= Stream.Write(Data^, DataSize);
   Result:= (BytesWritten = DataSize);

   // free the unused memory
   FreeMem(Data);
  end;
end;

//---------------------------------------------------------------------------
function TASDb.ReadString(const Key: ShortString;
 out Text: AnsiString): Boolean;
var
 Data: Pointer;
 Size: Cardinal;
begin
 Result:= ReadRecord(Key, Data, Size);
 if (Result) then
  begin
   if (Size > 0) then
    begin
     SetLength(Text, Size);
     Move(Data^, (@Text[1])^, Size);
     FreeMem(Data);
    end else Text:= '';
  end;
end;

//---------------------------------------------------------------------------
function TASDb.RemoveRecord(const Key: ShortString): Boolean;
var
 InStream, OutStream: TFileStream;
 NewHeader: TASDbHeader;
 TempFileName: AnsiString;
 NewRecords: array of TRecordInfo;
 i, Index, NewIndex: Integer;
 Data: Pointer;
 DataSize: Cardinal;
begin
 SetLength(NewRecords, 0);
 Data:= nil;

 // (1) Update record list
 Result:= Update();
 if (not Result) then Exit;

 // (2) retreive record index
 Index:= GetRecordNum(Key);
 if (Index = -1) then
  begin
   Result:= False;
   Exit;
  end; 

 // (3) OPEN THE SOURCE for reading & writing
 try
  InStream:= TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
 except
  Exit;
 end;

 // (4) OPEN THE DESTINATION for writing
 TempFileName:= ExtractFilePath(FFileName) + TempFileText;

 try
  OutStream:= TFileStream.Create(TempFileName, fmCreate or fmShareExclusive);
 except
  Exit;
 end;

 // (5) update ASDb info, in case it has been changed
 Result:= ReadASDbInfo(InStream);
 if (not Result) then
  begin
   FreeAndNil(InStream);
   FreeAndNil(OutStream);
   Exit;
  end;

 // (6) create NEW HEADER
 Move(ASDbHeader, NewHeader, SizeOf(TASDbHeader));
 NewHeader.RecordCount:= ASDbHeader.RecordCount - 1;

 // (7) Write temporary ASDb header
 try
  OutStream.WriteBuffer(NewHeader, SizeOf(TASDbHeader));
 except
  Result:= False;
  FreeAndNil(InStream);
  FreeAndNil(OutStream);
  Exit;
 end;

 // (8) Completely rewrite RECORD LIST
 for i:= 0 to Length(FRecords) - 1 do
  if (i <> Index) then
   begin
    // create a copy of previous record
    NewIndex:= Length(NewRecords);
    SetLength(NewRecords, NewIndex + 1);
    NewRecords[NewIndex]:= FRecords[i];

    // update record offset
    NewRecords[NewIndex].Offset:= OutStream.Position;

    // allocate temporary buffers
    DataSize:= NewRecords[NewIndex].PhysSize + DataOffset;
    ReallocMem(Data, DataSize);

    // read the whole record block
    try
     InStream.Seek(FRecords[i].Offset, soFromBeginning);
     InStream.ReadBuffer(Data^, DataSize);
    except
     FreeAndNil(InStream);
     FreeAndNil(OutStream);
     FreeMem(Data);
     Result:= False;
     Exit;
    end;

    // write the whole record block
    try
     OutStream.WriteBuffer(Data^, DataSize);
    except
     FreeAndNil(InStream);
     FreeAndNil(OutStream);
     FreeMem(Data);
     Result:= False;
     Exit;
    end;
   end; // rewrite records

 // the record table follows, update ASDb header
 NewHeader.TableOffset:= OutStream.Position;

 // (9) write NEW RECORD table (and update the current one)
 SetLength(FRecords, Length(NewRecords));
 try
  for i:= 0 to Length(NewRecords) - 1 do
   begin
    // write record info
    StreamPutSimpleAnsiString(OutStream, NewRecords[i].Key);
    StreamPutLongword(OutStream, NewRecords[i].Offset);

    // update the record table
    FRecords[i]:= NewRecords[i];
   end;

  // (10) write updated ASDb header
  OutStream.Seek(0, soFromBeginning);
  OutStream.WriteBuffer(NewHeader, SizeOf(TASDbHeader));
 except
  Result:= False;
  FreeAndNil(InStream);
  FreeAndNil(OutStream);
  Exit;
 end;

 // update file size
 FFileSize:= OutStream.Size;

 // (11) Release allocated buffers
 if (Data <> nil) then FreeMem(Data);
 FreeAndNil(InStream);
 FreeAndNil(OutStream);

 // (12) Switch between temporary file and real one
 try
  DeleteFile(FFileName);
  RenameFile(TempFileName, FFileName);
 except
  Result:= False;
 end;
end;

//---------------------------------------------------------------------------
function TASDb.RenameRecord(const Key, NewKey: ShortString): Boolean;
var
 Index: Integer;
begin
 // (1) Check the validity of OpenMode.
 if (FOpenMode in [opOverwrite, opReadonly]) then
  begin
   Result:= False;
   Exit;
  end;

 // (2) Refresh record list.
 Result:= ReadASDbInfo(nil);
 if (not Result) then Exit;

 // (3) Check the validity of specified keys.
 Index:= GetRecordNum(Key);
 if (Index = -1)or(GetRecordNum(NewKey) <> -1) then
  begin
   Result:= False;
   Exit;
  end;

 // (4) Modify record table.
 FRecords[Index].Key:= NewKey;

 // (5) Write new record table.
 Result:= WriteRecordTable();
end;

//---------------------------------------------------------------------------
function TASDb.SwitchRecords(Index1, Index2: Integer): Boolean;
var
 Aux: TRecordInfo;
begin
 // (1) Check the validity of OpenMode.
 if (FOpenMode in [opOverwrite, opReadonly]) then
  begin
   Result:= False;
   Exit;
  end;

 // (2) Refresh record list.
 Result:= ReadASDbInfo(nil);
 if (not Result) then Exit;

 // (3) Validate indexes with updated list.
 if (Index1 < 0)or(Index2 < 0)or(Index1 >= Length(FRecords))or
  (Index2 >= Length(FRecords)) then
  begin
   Result:= False;
   Exit;
  end;

 // (4) Exchange two records.
 Aux:= FRecords[Index1];
 FRecords[Index1]:= FRecords[Index2];
 FRecords[Index2]:= Aux;

 // (5) Write new record table.
 Result:= WriteRecordTable();
end;

//---------------------------------------------------------------------------
function TASDb.CompareRecords(Index1, Index2: Integer;
 Criteria: TRecordSortCriteria): Integer;
begin
 Result:= 0;

 case Criteria of
  rscName:
   Result:= CompareText(FRecords[Index1].Key, FRecords[Index2].Key);

  rscType:
   begin
    if (FRecords[Index1].RecordType < FRecords[Index2].RecordType) then
     Result:= -1;

    if (FRecords[Index1].RecordType > FRecords[Index2].RecordType) then
     Result:= 1;
   end;

  rscDate:
   begin
    if (FRecords[Index1].DateTime < FRecords[Index2].DateTime) then
     Result:= -1;

    if (FRecords[Index1].DateTime > FRecords[Index2].DateTime) then
     Result:= 1;
   end;

  rscOrigSize:
   begin
    if (FRecords[Index1].OrigSize < FRecords[Index2].OrigSize) then
     Result:= -1;

    if (FRecords[Index1].OrigSize > FRecords[Index2].OrigSize) then
     Result:= 1;
   end;

  rscPhysSize:
   begin
    if (FRecords[Index1].PhysSize < FRecords[Index2].PhysSize) then
     Result:= -1;

    if (FRecords[Index1].PhysSize > FRecords[Index2].PhysSize) then
     Result:= 1;
   end;

  rscSecurity:
   begin
    if (FRecords[Index1].Secure < FRecords[Index2].Secure) then
     Result:= -1;

    if (FRecords[Index1].Secure > FRecords[Index2].Secure) then
     Result:= 1;
   end;
 end;
end;

//---------------------------------------------------------------------------
function TASDb.SortRecords(Criteria: TRecordSortCriteria;
 Invert: Boolean): Boolean;
var
 i, j: Integer;
 Aux: TRecordInfo;
begin
 if (not Invert) then
  begin
   for i:= 0 to Length(FRecords) - 2 do
    for j:= 0 to Length(FRecords) - i - 2 do
     if (CompareRecords(j, j + 1, Criteria) > 0) then
      begin
       Aux:= FRecords[j];
       FRecords[j]:= FRecords[j + 1];
       FRecords[j + 1]:= Aux;
      end;
  end else
  begin
   for i:= 0 to Length(FRecords) - 2 do
    for j:= 0 to Length(FRecords) - i - 2 do
     if (CompareRecords(j, j + 1, Criteria) < 0) then
      begin
       Aux:= FRecords[j];
       FRecords[j]:= FRecords[j + 1];
       FRecords[j + 1]:= Aux;
      end;
  end;



 Result:= WriteRecordTable();
end;

//---------------------------------------------------------------------------
end.
