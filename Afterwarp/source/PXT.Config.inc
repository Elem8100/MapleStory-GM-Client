(*
 * Copyright (c) 2015 - 2020 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)

{ Indicates that this config file has been processed (to avoid redeclaring or redefining of parameters in
  other configuration files). }
{$DEFINE PXT_CONFIG}

{.$DEFINE PXL_SCIENTIFIC_MODE}

{.$HINTS OFF}
{.$WARNINGS OFF}

{$MINENUMSIZE 1}
{$B-}
{$I-}

{$IFDEF FPC}
  // FreePascal compiler directives.

  {$MODE DELPHI}
  {$MODESWITCH TYPEHELPERS}
  {$PACKRECORDS C}
  {$CODEPAGE UTF8}

  {$IFDEF CPU32}
    {$IFDEF I386}
      {$DEFINE CPUX86}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPU64}
    {$DEFINE CPUX64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}

{$ELSE}
  // Delphi compiler directives.

  {$DEFINE DELPHI}
  {$ALIGN ON}

  {$IFDEF VER180} // Delphi 2006
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER185} // Delphi 2007
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER200} // Delphi 2009
    {$DEFINE DELPHI_PRE_XE2}
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER210} // Delphi 2010
    {$DEFINE DELPHI_PRE_XE2}
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER220} // Delphi XE
    {$DEFINE DELPHI_PRE_XE2}
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER230} // Delphi XE 2
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER240} // Delphi XE 3
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF VER250} // Delphi XE 4
    {$DEFINE DELPHI_NO_UTF8STRING}
  {$ENDIF}

  {$IFDEF NEXTGEN}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}

  {$IFDEF DELPHI_LEGACY}
    {$DEFINE CPUX86}
  {$ENDIF}

  {$IF (NOT DEFINED(DELPHI_PRE_XE2)) AND (NOT DEFINED(DELPHI_LEGACY))}
    {$DEFINE DELPHI_MODERN}
    {$IFNDEF PXL_SCIENTIFIC_MODE}
      {$EXCESSPRECISION OFF}
    {$ENDIF}
  {$IFEND}

{$ENDIF}

{$IFNDEF DELPHI_LEGACY}
  {$SCOPEDENUMS ON}
{$ENDIF}
