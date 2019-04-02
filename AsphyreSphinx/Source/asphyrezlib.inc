{*****************************************************************************
*  zlibex.inc                                                                *
*  copyright (c) 2006 base2 technologies                                     *
*                                                                            *
*  version information for delphi/c++ builder                                *
*****************************************************************************}

{** delphi ******************************************************************}

{$IFDEF VER80}              // delphi 1
  {$DEFINE Delphi}

  {$DEFINE Version1}
{$ENDIF}

{$IFDEF VER90}              // delphi 2
  {$DEFINE Delphi}

  {$DEFINE Version2}
{$ENDIF}

{$IFDEF VER100}             // delphi 3
  {$DEFINE Delphi}

  {$DEFINE Version3}
{$ENDIF}

{$IFDEF VER120}             // delphi 4
  {$DEFINE Delphi}

  {$DEFINE Version4}
{$ENDIF}

{** c++ builder *************************************************************}

{$IFDEF VER93}              // c++ builder 1
  {$DEFINE CBuilder}

  {$DEFINE Version1}
{$ENDIF}

{$IFDEF VER110}             // c++ builder 3
  {$DEFINE CBuilder}

  {$DEFINE Version3}
{$ENDIF}

{$IFDEF VER125}             // c++ builder 4
  {$DEFINE CBuilder}

  {$DEFINE Version4}
{$ENDIF}

{** delphi/c++ builder (common) *********************************************}

{$IFDEF VER130}             // delphi/c++ builder 5
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version5}

  {$DEFINE Version5Plus}
{$ENDIF}

{$IFDEF VER140}             // delphi 6
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version6}

  {$DEFINE Version5Plus}
  {$DEFINE Version6Plus}
{$ENDIF}

{$IFDEF VER150}             // delphi 7
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version7}

  {$DEFINE Version5Plus}
  {$DEFINE Version6Plus}
  {$DEFINE Version7Plus}
{$ENDIF}

{$IFDEF VER160}             // delphi 8 (.net)
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version8}

  {$DEFINE Version5Plus}
  {$DEFINE Version6Plus}
  {$DEFINE Version7Plus}
  {$DEFINE Version8Plus}
{$ENDIF}

{$IFDEF VER170}             // delphi 2005
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version2005}

  {$DEFINE Version5Plus}
  {$DEFINE Version6Plus}
  {$DEFINE Version7Plus}
  {$DEFINE Version8Plus}
  {$DEFINE Version2005Plus}
{$ENDIF}

{$IFDEF VER180}             // bds 2006
  {$IFDEF BCB}
    {$DEFINE CBuilder}
  {$ELSE}
    {$DEFINE Delphi}
  {$ENDIF}

  {$DEFINE Version2006}

  {$DEFINE Version5Plus}
  {$DEFINE Version6Plus}
  {$DEFINE Version7Plus}
  {$DEFINE Version8Plus}
  {$DEFINE Version2005Plus}
  {$DEFINE Version2006Plus}
{$ENDIF}
