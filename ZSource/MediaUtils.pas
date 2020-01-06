unit MediaUtils;
//---------------------------------------------------------------------------
// MediaUtils.pas                                       Modified: 25-Ago-2010
// Utility routines for handling media files                     Version 1.02
//---------------------------------------------------------------------------
// This file is unoffical
// Changed by Marcos Gomes
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
// The Original Code is MediaUtils.pas.
//
// The Initial Developer of the Original Code is M. Sc. Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007,
// Afterwarp Interactive. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, SysUtils, AsphyreXML;

//---------------------------------------------------------------------------
{$WARN SYMBOL_PLATFORM OFF}

//---------------------------------------------------------------------------
// IsArchiveLink()
//
// Validates if the specified link points to Archive.
// Example:
//   /data/media/map.zip | test.image
//---------------------------------------------------------------------------
function IsArchiveLink(const Text: string): Boolean;

//---------------------------------------------------------------------------
// ExtractArchiveName()
//
// Generates a valid archive file name with full path from the archive link.
//---------------------------------------------------------------------------
function ExtractArchiveName(const Text: string): string;

//---------------------------------------------------------------------------
// ExtractArchiveKey()
//
// Generates a valid key from the archive link.
//---------------------------------------------------------------------------
function ExtractArchiveKey(const Text: string): string;

//---------------------------------------------------------------------------
// ExtractArchiveKey()
//
// Extracts all paths from the specified link leaving only key name.
//---------------------------------------------------------------------------
function ExtractPureKey(const Text: string): string;

//---------------------------------------------------------------------------
// LoadLinkXML()
//
// Attempts to load a link pointing to XML file
//---------------------------------------------------------------------------
function LoadLinkXML(const Link: string): TXMLNode;

//---------------------------------------------------------------------------
// GetWindowsDir()
//
// Retreives Windows System path.
//---------------------------------------------------------------------------
function GetWindowsPath(): TFileName;

//---------------------------------------------------------------------------
// GetTempDir()
//
// Retreives Temporary path.
//---------------------------------------------------------------------------
function GetTempPath(): TFileName;

//---------------------------------------------------------------------------
// MakeValidPath()
//
// Assures that the specified path ends with "\", so a file name can be
// added to it.
//---------------------------------------------------------------------------
function MakeValidPath(const Path: string): string;

//---------------------------------------------------------------------------
// MakeValidFileName()
//
// Assures that the specified file name does not begin with "\", so a path
// can be added to it.
//---------------------------------------------------------------------------
function MakeValidFileName(const FileName: string): string;

//---------------------------------------------------------------------------
// ParseInt()
//
// Parses a signed integer value read from XML. If no AutoValue is provided,
// in case of empty or non-parseable text, -1 will be returned.
//---------------------------------------------------------------------------
function ParseInt(const Text: string): Integer; overload;
function ParseInt(const Text: string; AutoValue: Integer): Integer; overload;

//---------------------------------------------------------------------------
// ParseCardinal()
//
// Parses an unsigned integer value read from XML. If no AutoValue is provided,
// in case of empty or non-parseable text, High(Cardinal) will be returned.
//---------------------------------------------------------------------------
function ParseCardinal(const Text: string): Cardinal; overload;
function ParseCardinal(const Text: string;
 AutoValue: Cardinal): Cardinal; overload;

//---------------------------------------------------------------------------
// ParseFloat()
//
// Parses a floating-point  unsigned integer value read from XML. If no
// AutoValue is provided, in case of empty or non-parseable text,
// High(Cardinal) will be returned.
//---------------------------------------------------------------------------
function ParseFloat(const Text: string): Real; overload;
function ParseFloat(const Text: string; AutoValue: Real): Real; overload;

//---------------------------------------------------------------------------
// ParseBoolean()
//
// Parses Boolean text representation (true, false, yes, no).
//---------------------------------------------------------------------------
function ParseBoolean(const Text: string;
 AutoValue: Boolean): Boolean; overload;
function ParseBoolean(const Text: string): Boolean; overload;

//---------------------------------------------------------------------------
// ParseColor()
//
// Parses an HTML or hexadecimal color.
//  -> For HTML colors (#RRGGBB), alpha is always 255.
//  -> If no AutoValue is specified, unparseable text gives opaque white.
//---------------------------------------------------------------------------
function ParseColor(const Text: string): Cardinal; overload;
function ParseColor(const Text: string;
 AutoColor: Cardinal): Cardinal; overload;

//---------------------------------------------------------------------------
// BooleanToString()
//
// Returns string representation of boolean.
//---------------------------------------------------------------------------
function BooleanToString(Value: Boolean): string;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function IsArchiveLink(const Text: string): Boolean;
var
 xPos: Integer;
begin
 xPos:= Pos('|', Text);
 Result:= (Length(Text) >= 3)and(xPos > 1)and(xPos < Length(Text));
end;

//---------------------------------------------------------------------------
function ExtractArchiveName(const Text: string): string;
var
 xPos, i: Integer;
begin
 Result:= Text;

 // Step 1. Remove "key" part from the link.
 xPos:= Pos('|', Text);
 if (xPos <> 0) then
  Delete(Result, xPos, Length(Result) + 1 - xPos);

 // Step 2. Replace "/" with "\".
 for i:= 1 to Length(Result) do
  if (Result[i] = '/') then Result[i]:= '\';

 // Step 3. Trim all leading and trailing spaces.
 Result:= Trim(Result);

 // Step 4. Remove leading "\", if such exists.
 if (Length(Result) > 0)and(Result[1] = '\') then Delete(Result, 1, 1);

 // Step 5. Include program path
 //Result:= ExtractFilePath(ParamStr(0)) + Result;
end;

//---------------------------------------------------------------------------
function ExtractArchiveKey(const Text: string): string;
var
 xPos: Integer;
begin
 Result:= Text;

 // Step 1. Remove "archive" part from the link.
 xPos:= Pos('|', Text);
 if (xPos <> 0) then
  Delete(Result, 1, xPos);

 // Step 2. Trim all leading and trailing spaces.
 Result:= Trim(Result);
end;

//---------------------------------------------------------------------------
function ExtractPureKey(const Text: string): string;
var
 xPos, i: Integer;
begin
 Result:= Text;

 // Step 1. Remove "key" part from the link.
 xPos:= Pos('|', Text);
 if (xPos <> 0) then
  Delete(Result, xPos, Length(Result) + 1 - xPos);

 // Step 2. Replace "/" with "\".
 for i:= 1 to Length(Result) do
  if (Result[i] = '/') then Result[i]:= '\';

 // Step 3. Trim all leading and trailing spaces.
 Result:= Trim(Result);

 // Step 4. Remove leading "\", if such exists.
 if (Length(Result) > 0)and(Result[1] = '\') then Delete(Result, 1, 1);
end;

//---------------------------------------------------------------------------
function LoadLinkXML(const Link: string): TXMLNode;
begin
 if (IsArchiveLink(Link)) then
  begin
   Result:= LoadXMLFromASDb(ExtractArchiveKey(Link), ExtractArchiveName(Link));
  end else Result:= LoadXMLFromFile(ExtractArchiveName(Link));
end;

//---------------------------------------------------------------------------
function GetWindowsPath(): TFileName;
var
 WinDir: array [0..MAX_PATH - 1] of Char;
begin
 SetString(Result, WinDir, GetWindowsDirectory(WinDir, MAX_PATH));

 if (Result = '') then Result:= ExtractFilePath(ParamStr(0));
end;

//---------------------------------------------------------------------------
function GetTempPath(): TFileName;
var
 TempDir: array[0..MAX_PATH - 1] of Char;
begin
 try
  SetString(Result, TempDir, Windows.GetTempPath(MAX_PATH, TempDir));

  if (not DirectoryExists(Result)) then
   if (not CreateDirectory(PChar(Result), nil)) then
    begin
     Result:= IncludeTrailingBackslash(GetWindowsPath()) + 'TEMP';
     if (not DirectoryExists(Result)) then
      if (not CreateDirectory(Pointer(Result), nil)) then
       begin
        Result:= ExtractFileDrive(Result) + '\TEMP';
        if (not DirectoryExists(Result)) then
         if (not CreateDirectory(Pointer(Result), nil)) then
          begin
           Result:= ExtractFileDrive(Result) + '\TMP';
           if (not DirectoryExists(Result)) then
            if (not CreateDirectory(Pointer(Result), nil)) then
             Result:= ExtractFilePath(ParamStr(0));
          end;
       end;
    end;
  except
   Result:= ExtractFilePath(ParamStr(0));
 end;
end;

//---------------------------------------------------------------------------
function MakeValidPath(const Path: string): string;
begin
 Result:= Trim(Path);

 if (Length(Result) > 0)and(Result[Length(Result)] <> '\') then
  Result:= Result + '\';
end;

//---------------------------------------------------------------------------
function MakeValidFileName(const FileName: string): string;
begin
 Result:= Trim(FileName);
 while (Length(Result) > 0)and(Result[1] = '\') do Delete(Result, 1, 1);
end;

//---------------------------------------------------------------------------
function ParseInt(const Text: string): Integer;
begin
 Result:= StrToIntDef(Text, -1);
end;

//---------------------------------------------------------------------------
function ParseInt(const Text: string; AutoValue: Integer): Integer;
begin
 Result:= StrToIntDef(Text, AutoValue);
end;

//---------------------------------------------------------------------------
function ParseCardinal(const Text: string): Cardinal;
begin
 Result:= Cardinal(StrToIntDef(Text, Integer(High(Cardinal))));
end;

//---------------------------------------------------------------------------
function ParseCardinal(const Text: string; AutoValue: Cardinal): Cardinal;
begin
 Result:= Cardinal(StrToIntDef(Text, Integer(AutoValue)));
end;

//---------------------------------------------------------------------------
function ParseBoolean(const Text: string; AutoValue: Boolean): Boolean;
begin
 Result:= AutoValue;

 if (SameText(Text, 'no'))or(SameText(Text, 'false')) then Result:= False;
 if (SameText(Text, 'yes'))or(SameText(Text, 'true')) then Result:= True;
end;

//---------------------------------------------------------------------------
function ParseBoolean(const Text: string): Boolean;
begin
 Result:= ParseBoolean(Text, False);
end;

//---------------------------------------------------------------------------
function ParseFloat(const Text: string): Real;
begin
 Result:= ParseFloat(Text, 0.0);
end;

//---------------------------------------------------------------------------
function ParseFloat(const Text: string; AutoValue: Real): Real;
var
 PrevDecimalSpeparator: Char;
begin
 PrevDecimalSpeparator:= DecimalSeparator;
 try
   DecimalSeparator     := '.';
   Result:= StrToFloatDef(Text, AutoValue);
 finally
   DecimalSeparator     := PrevDecimalSpeparator;
 end;
end;

//---------------------------------------------------------------------------
function ParseColor(const Text: string; AutoColor: Cardinal): Cardinal;
begin
 if (SameText(Text, 'source'))or(SameText(Text, 'auto'))or
  (SameText(Text, 'none')) then
  begin
   Result:= AutoColor;
   Exit;
  end;

 Result:= $FFFFFFFF;
 if (Length(Text) < 2)or((Text[1] <> '#')and(Text[1] <> '$')) then Exit;

 if (Text[1] = '#') then
  begin
   Result:= Cardinal(StrToIntDef('$' + Copy(Text, 2, Length(Text) - 1),
    Integer(AutoColor))) or $FF000000;
  end else Result:= Cardinal(StrToIntDef(Text, Integer(AutoColor)));
end;

//---------------------------------------------------------------------------
function ParseColor(const Text: string): Cardinal;
begin
 Result:= ParseColor(Text, $FFFFFFFF);
end;

//---------------------------------------------------------------------------
function BooleanToString(Value: Boolean): string;
begin
 if (Value) then Result:= 'yes' else Result:= 'no';
end;

//---------------------------------------------------------------------------
end.
