unit AsphyreDebug;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils;

//---------------------------------------------------------------------------
{$i-}

//---------------------------------------------------------------------------
procedure DebugLog(const Text: ShortString);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
var
 LogFile: TextFile;

//---------------------------------------------------------------------------
procedure InitializeLog();
begin
 AssignFile(LogFile, ChangeFileExt(ParamStr(0), '.log'));
 ReWrite(LogFile);

 WriteLn(LogFile, 'Executed name: ' + ExtractFileName(ParamStr(0)));
 WriteLn(LogFile, 'Executed date: ' + DateTimeToStr(Now()));
 WriteLn(LogFile, '-------------------- ENTRY -----------------------');
 CloseFile(LogFile);
end;

//---------------------------------------------------------------------------
procedure FinalizeLog();
begin
 Append(LogFile);
 WriteLn(LogFile, '-------------------- EXIT ------------------------');
 CloseFile(LogFile);
end;

//---------------------------------------------------------------------------
procedure DebugLog(const Text: ShortString);
begin
 Append(LogFile);
 WriteLn(LogFile, Text);
 CloseFile(LogFile);
end;

//---------------------------------------------------------------------------
initialization
 InitializeLog();

//---------------------------------------------------------------------------
finalization
 FinalizeLog();

//---------------------------------------------------------------------------
end.
