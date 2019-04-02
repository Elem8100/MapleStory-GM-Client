{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Author:  (c) 2006 Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

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

{.$A+,B-,E-,F-,G+,H+,I-,J+,K-,N+,P+,Q-,R-,S-,T-,V+,W-,X+,Y-,G+}

{$DEFINE 486GE}         // Assembler Core only for >= 486 CPU
{$DEFINE UseASM}        // use Assembler optimated core

{$IFDEF VER80}  { Delphi 1.0     }
  {$DEFINE VER_D1}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER90}  { Delphi 2.0     }
  {$DEFINE VER_D2}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER100} {Borland Delphi 3.0 }
  {$DEFINE VER_D3}
  {$DEFINE VER_D3H}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER120} {Borland Delphi 4.0 }
  {$DEFINE VER_D4}
  {$DEFINE VER_D4H}
  {$DEFINE VER_D3H}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER130} {Borland Delphi 5.0 }
  {$DEFINE VER_D5}
  {$DEFINE VER_D5H}
  {$DEFINE VER_D4H}
  {$DEFINE VER_D3H}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER140} {Borland Delphi 6.0 }
  {$DEFINE VER_D6}
  {$DEFINE VER_D6H}
  {$DEFINE VER_D5H}
  {$DEFINE VER_D4H}
  {$DEFINE VER_D3H}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER150} {Borland Delphi 7.0 }
  {$DEFINE VER_D7}
  {$DEFINE VER_D7H}
  {$DEFINE VER_D6H}
  {$DEFINE VER_D5H}
  {$DEFINE VER_D4H}
  {$DEFINE VER_D3H}
  {$DEFINE VER_D2H}
  {$DEFINE VER_D1H}
{$ENDIF}

{$IFDEF VER93}  { Borland C++Builder 1.0 }
  {$DEFINE VER_BCB1}
  {$DEFINE VER_BCB1H}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE VER_BCB3}
  {$DEFINE VER_BCB3H}
  {$DEFINE VER_BCB1H}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE VER_BCB4}
  {$DEFINE VER_BCB4H}
  {$DEFINE VER_BCB3H}
  {$DEFINE VER_BCB1H}
{$ENDIF}

{$IFNDEF VER80}           { Delphi 1.0     }
 {$IFNDEF VER90}          { Delphi 2.0     }
  {$IFNDEF VER93}         { C++Builder 1.0 }
    {$DEFINE VER_D3H}     { Delphi 3.0 or higher }
    {$IFNDEF VER100}
      {$DEFINE VER_BCB3H} { C++Builder 3.0 or higher }
      {$IFNDEF VER110}
        {$DEFINE VER_D4H} { Delphi 4.0 or higher }
        {$IFNDEF VER120}
           {$DEFINE VER_D5H}
           {$IFNDEF VER130}
              {$DEFINE VER_D6H}
              {$IFNDEF VER140}
                 {$DEFINE VER_D7H}
              {$ENDIF}
           {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER_D7H}
  {$A4} 
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
