(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)

{ Indicates that this config file has been processed (to avoid redeclaring or redefining of parameters in other
  configuration files). }
{$DEFINE PXL_CONFIG}

{.$DEFINE PXL_CONSOLE}
{.$DEFINE SCF_SILENT}

{.$DEFINE PXL_SCIENTIFIC_MODE_MAX}
{.$DEFINE PXL_SCIENTIFIC_MODE}

{.$DEFINE DX11DEBUG}
{.$DEFINE D2DDEBUG}

{.$HINTS OFF}
{.$WARNINGS OFF}

{$IFDEF PXL_SCIENTIFIC_MODE_MAX}
  {$DEFINE PXL_SCIENTIFIC_MODE}
{$ENDIF}

{ Enable the following option to support antialiased lines even when multisampling is disabled.
  This is typically supported on Nvidia DirectX hardware. }
{$DEFINE PXL_NOMSAA_ANTIALIASED_LINES}

{$SCOPEDENUMS ON}
{$MINENUMSIZE 1}
{$B-}
{$I-}

{$IFDEF SINGLEBOARD}
  {$DEFINE PXL_CONSOLE}
{$ENDIF}

{$IFDEF FPC}
  // FreePascal compiler directives.

  {$MODE DELPHI}
  {$MODESWITCH TYPEHELPERS}
  {$PACKRECORDS C}

  {$IFDEF CPU32}
    {$IFDEF I386}
      {$DEFINE CPUX86}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPU64}
    {$DEFINE CPUX64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}

  {$IF DEFINED(CPUARM) AND DEFINED(FPC_ABI_EABI)}
    {$DEFINE FPC_MMAP2}
  {$ELSE}
    {$DEFINE FPC_OLDMMAP}
  {$ENDIF}

  {$IFDEF EMBEDDED}
    {$DEFINE SINGLEBOARD}
    {$ALIGN 2}
    {.$DEFINE MICROBOARD}
  {$ENDIF}

{$ELSE}
  // Delphi compiler directives.

  {$DEFINE DELPHI}
  {$ALIGN ON}

  {$IFDEF VER180} // Delphi 2006
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_2006}
  {$ENDIF}

  {$IFDEF VER185} // Delphi 2007
    {$DEFINE DELPHI_LEGACY}
    {$DEFINE DELPHI_2007}
  {$ENDIF}

  {$IFDEF VER200} // Delphi 2009
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2009}
  {$ENDIF}

  {$IFDEF VER210} // Delphi 2010
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_2010}
  {$ENDIF}

  {$IFDEF VER220} // Delphi XE
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE}
  {$ENDIF}

  {$IFDEF VER230} // Delphi XE 2
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE2}
  {$ENDIF}

  {$IFDEF VER240} // Delphi XE 3
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE3}
  {$ENDIF}

  {$IFDEF VER250} // Delphi XE 4
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE4}
  {$ENDIF}

  {$IFDEF VER260} // Delphi XE 5
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
  {$ENDIF}

  {$IFDEF VER270} // Delphi XE 6
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
  {$ENDIF}

  {$IFDEF VER280} // Delphi XE 7
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
  {$ENDIF}

  {$IFDEF VER290} // Delphi XE 8
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
    {$DEFINE DELPHI_XE8_UP}
    {$DEFINE DELPHI_XE8}
  {$ENDIF}

  {$IFDEF VER300} // Delphi Seattle
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
    {$DEFINE DELPHI_XE8_UP}
    {$DEFINE DELPHI_XE8}
    {$DEFINE DELPHI_SEATTLE_UP}
    {$DEFINE DELPHI_SEATTLE}
  {$ENDIF}

  {$IFDEF VER310} // Delphi Berlin
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
    {$DEFINE DELPHI_XE8_UP}
    {$DEFINE DELPHI_XE8}
    {$DEFINE DELPHI_SEATTLE_UP}
    {$DEFINE DELPHI_SEATTLE}
    {$DEFINE DELPHI_BERLIN_UP}
    {$DEFINE DELPHI_BERLIN}
  {$ENDIF}

  {$IFDEF VER320} // Delphi Tokyo
    {$DEFINE DELPHI_2009_UP}
    {$DEFINE DELPHI_2010_UP}
    {$DEFINE DELPHI_XE_UP}
    {$DEFINE DELPHI_XE2_UP}
    {$DEFINE DELPHI_XE3_UP}
    {$DEFINE DELPHI_XE4_UP}
    {$DEFINE DELPHI_XE5_UP}
    {$DEFINE DELPHI_XE5}
    {$DEFINE DELPHI_XE6_UP}
    {$DEFINE DELPHI_XE6}
    {$DEFINE DELPHI_XE7_UP}
    {$DEFINE DELPHI_XE7}
    {$DEFINE DELPHI_XE8_UP}
    {$DEFINE DELPHI_XE8}
    {$DEFINE DELPHI_SEATTLE_UP}
    {$DEFINE DELPHI_SEATTLE}
    {$DEFINE DELPHI_BERLIN_UP}
    {$DEFINE DELPHI_BERLIN}
    {$DEFINE DELPHI_TOKYO_UP}
    {$DEFINE DELPHI_TOKYO}
  {$ENDIF}

  {$IFDEF NEXTGEN}
    {$DEFINE DELPHI_NEXTGEN}
    {$DEFINE STANDARD_STRINGS_ONLY}
    {$ZEROBASEDSTRINGS OFF}
  {$ENDIF}

  {$IFDEF DELPHI_LEGACY}
    {$DEFINE CPUX86}
  {$ENDIF}

  {$IFDEF DELPHI_XE2_UP}
    {$IFNDEF PXL_SCIENTIFIC_MODE}
      {$EXCESSPRECISION OFF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF POSIX}
    {$DEFINE FIREMONKEY}
  {$ENDIF}

{$ENDIF}
