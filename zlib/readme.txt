-- notes -----------------------------------------------------------------------------------------

  the units included in this archive should work with delphi 5 through delphi xe3 for win32 and
  win64.

  please contact me if you find any errors, make any changes, add new functionality, or have any
  general suggestions so that i may incorporate them into my version.  i can be reached via my
  website at

    http://www.base2ti.com

  thanks.

  brent sherwood

-- disclaimer ------------------------------------------------------------------------------------

  this software is provided "as-is", without any express or implied warranty. in no event will the
  authors be held liable for any damages arising from the use of this software.

  permission is granted to anyone to use this software for any purpose, including commercial
  applications.  please do not misrepresent the origin of this software.  if you use this software
  in a product, an acknowledgment in the product documentation (readme, about box, help file,
  etc.) would be appreciated but is not required.

-- installation ----------------------------------------------------------------------------------

  first, copy all of the files into a folder (for example, c:\delphi\zlib). next, include the
  folder in the library path in the environment options. finally, "use" the zlibex and zlibexgz
  units as needed.

-- contents --------------------------------------------------------------------------------------

  delphi files

    zlibex.inc
    zlibex.pas
    zlibexapi.pas
    zlibexgz.pas

  objects files used by zlibex.pas

    win32\adler32.obj
    win32\compress.obj
    win32\crc32.obj
    win32\deflate.obj
    win32\infback.obj
    win32\inffast.obj
    win32\inflate.obj
    win32\inftrees.obj
    win32\trees.obj

    win64\adler32.obj
    win64\compress.obj
    win64\crc32.obj
    win64\deflate.obj
    win64\infback.obj
    win64\inffast.obj
    win64\inflate.obj
    win64\inftrees.obj
    win64\trees.obj

  zlib 1.2.8 source files (http://www.zlib.net)

    zlib\adler32.c
    zlib\compress.c
    zlib\crc32.c
    zlib\deflate.c
    zlib\infback.c
    zlib\inffast.c
    zlib\inflate.c
    zlib\inftrees.c
    zlib\trees.c
    zlib\zutil.c
    zlib\crc32.h
    zlib\deflate.h
    zlib\inffast.h
    zlib\inffixed.h
    zlib\inflate.h
    zlib\inftrees.h
    zlib\trees.h
    zlib\zconf.h
    zlib\zlib.h
    zlib\zutil.h

-- history ---------------------------------------------------------------------------------------

  2013.05.23  zlibexapi.pas
                updated to zlib version 1.2.8

              zlibex.inc
                updated for delphi xe3 (2013)

  2012.05.23  zlibexgz.pas
                updated for delphi xe2
                added overloaded GZCompressFile

  2012.05.21  zlibex.pas
                updated for win64 (delphi xe2)
                added NativeInt type for delphi 2007-
                added NativeUInt type for delphi 2007-

              zlibexapi.pas
                updated for win64 (delphi xe2)
                moved win32 obj files to win32 subfolder
                changed win32 obj options to exclude the underscore

  2012.05.07  zlibexapi.pas
                updated to zlib version 1.2.7

  2012.05.01  zlibex.inc
                updated for delphi xe2 (2012)

  2012.03.05  zliexapi.pas
                udpated to zlib version 1.2.6

  2011.07.21  zlibex.pas
                fixed routines to validate size before calling Move

              zlibexgz.pas
                fixed routines to validate size before calling Move

  2010.07.01  zlibex.pas
                hide overloaded Z*String* routines for delphi 5

  2010.05.02  zlibex.pas
                added ZDeflateEx and ZInflateEx

  2010.04.20  zlibex.pas
                added TZ*Buffer classes

              zlibexapi.pas
                updated to zlib version 1.2.5

  2010.04.15  zlibex.pas
                moved core zlib routines to separate unit (ZLibExApi.pas)

              zlibexapi.pas
                updated to zlib version 1.2.4

  2010.01.27  zlibex.pas
                updated for delphi 2010

              zlibexgz.pas
                updated for delphi 2010

              zlibex.inc
                updated for delphi 2010

  2009.04.14  zlibex.pas
                added overloaded string routines for AnsiString and
                  UnicodeString

              zlibexgz.pas
                added overloaded string routines for AnsiString and
                  UnicodeString
                removed deprecated Z*G routines

  2009.04.11  zlibex.inc
                updated to use CONDITIONALEXPRESSIONS and CompilerVersion

  2009.01.28  zlibex.pas
                updated for delphi 2009 String (UnicodeString)

              zlibexgz.pas
                updated for delphi 2009 String (UnicodeString)

              zlibex.inc
                updated for delphi 2009

  2008.05.15  zlibex.pas
                added TStreamPos type Stream.Position variants
                added TCustomZStream.Stream* methods

              zlibexgz.pas
                added TGZCompressionStream and TGZDecompressionStream

  2007.11.06  zlibexgz.pas
                changed TGZTrailer.Crc from Cardinal to Longint

  2007.10.01  zlibexgz.pas
                added GZDecompressStreamSize
                fixed GZDecompressStream position handling

              zlibex.inc
                updated for delphi 2007

  2007.08.17  zlibex.pas
                modified TZCompressionStream.Write to use Write instead of
                  WriteBuffer

  2007.07.18  zlibexgz.pas
                fixed GZCompressStr filename and comment processing

  2007.03.18  zlibexgz.pas
                modified naming convention for gzip routines GZ*
                deprecated previous gzip routines Z*G

  2007.03.15  zlibex.pas
                moved gzip routines to separate unit - zlibexgz.pas

              zlibexgz.pas
                added ZDecompressStreamG
                added overloaded ZCompressStrG
                added overloaded ZCompressStreamG

  2007.02.24  zlibex.pas
                added PWord declaration for delphi 5-

  2006.10.07  zlibex.pas
                fixed EZLibError constructor for c++ builder compatibility

  2006.08.10  zlibex.pas
                added ZDecompressStrG (simple gzip format)

  2006.06.02  zlibex.pas
                added DateTimeToUnix for delphi 5-

  2006.03.28  zlibex.pas
                moved Z_DEFLATED to interface section
                added custom compression levels zcLevel1 thru zcLevel9

  2006.03.27  zlibex.pas
                added ZCompressStreamWeb
                added ZCompressStreamG (simple gzip format)

  2006.03.24  zlibex.pas
                added ZCompressStrG (simple gzip format)
                added ZAdler32 and ZCrc32

  2005.11.29  zlibex.pas
                changed FStreamPos to Int64 for delphi 6+

  2005.07.25  zlibex.pas
                updated to zlib version 1.2.3

  2005.03.04  zlibex.pas
                modified ZInternalCompressStream loops
                modified ZInternalDecompressStream loops

  2005.02.07  zlibex.pas
                fixed ZInternalCompressStream loop conditions
                fixed ZInternalDecompressStream loop conditions

  2005.01.11  zlibex.pas
                updated to zlib version 1.2.2
                added ZCompressStrWeb

  2004.01.06  zlibex.pas
                updated to zlib version 1.2.1

  2003.04.14  zlibex.pas
                added ZCompress2 and ZDecompress2
                added ZCompressStr2 and ZDecompressStr2
                added ZCompressStream2 and ZDecompressStream2
                added overloaded T*Stream constructors to support InflateInit2
                  and DeflateInit2
                fixed ZDecompressStream to use ZDecompressCheck instead of
                  ZCompressCheck

  2002.03.15  zlibex.pas
                updated to zlib version 1.1.4

  2001.11.27  zlibex.pas
                enhanced TZDecompressionStream.Read to adjust source stream
                  position upon end of compression data
                fixed endless loop in TZDecompressionStream.Read when
                  destination count was greater than uncompressed data

  2001.10.26  zlibex.pas
                renamed unit to integrate "nicely" with delphi 6

  2000.11.24  zlib.pas
                added soFromEnd condition to TZDecompressionStream.Seek
                added ZCompressStream and ZDecompressStream

  2000.06.13  zlib.pas
                optimized, fixed, rewrote, and enhanced the zlib.pas unit
                  included on the delphi cd (zlib version 1.1.3)

-- acknowledgments -------------------------------------------------------------------------------

  erik turner - thanks for the enhancements and recommendations. specifically, the
    ZCompressionStream and ZDecompressionStream routines. my apologies for the delay in getting
    these in here.

  david bennion - thanks for finding that nasty little endless loop quirk with the
    TZDecompressionStream.Read method.

  burak kalayci - thanks for emailing to inform me about the zlib 1.1.4 update; and again for
    emailing about 1.2.1.

  vicente sánchez-alarcos - thanks for emailing to inform me about the zlib 1.2.2 update.

  luigi sandon - thanks for pointing out the missing loop condition (Z_STREAM_END) in
    ZInternalCompressStream and ZInternalDecompressStream.

  ferry van genderen - thanks for assisting me fine tune and beta test the ZInternalCompressStream
    and ZInternalDecompressStream routines.

  mathijs van veluw - thanks for emailing to inform me about the zlib 1.2.3 update.

  j. rathlev - thanks for pointing out the FStreamPos and TStream.Position type inconsistency.

  ralf wenske - thanks for prototyping and assisting with ZCompressStrG and ZCompressStreamG.

  roman krupicka - thanks for pointing out the DateUtils unit and the DateTimeToUnix function
    wasn't available prior to delphi 6.

  anders johansen - thanks for pointing out the ELibError constructor incompatibility with c++
    builder.

  marcin treffler - thanks for pointing out the missing PWord declaration for delphi 5.

  jean-jacques esquirol - thanks for pointing out the "result" address issue when processing
    filename and comment flags/content in GZCompressStr; and for pointing out the type differences
    with TGZTrailer.Crc (Cardinal) and ZCrc32 (Longint).

  graham wideman - thanks for beta testing GZDecompressStreamSize and pointing out the position
    handling issue in GZDecompressStream.

  marcin szafrański - thanks for beta testing the delphi 2009 changes.

  iztok kacin - thanks for the CONDITIONALEXPRESSIONS, CompilerVersion changes, and assisting me
    design and further improve support for delphi 2009.

  oleg matrozov - thanks for pointing out the missing loop condition (avail_in > 0) in
    ZInternalCompress and ZInternalDecompress; and for prototyping and assisting with the
    TZ*Buffer classes.

  edward koo - thanks for pointing out the delphi 5 incompatibility with the overloaded Z*String*
   routines.

  farshad mohajeri - thanks for the paypal donation.

  egron elbra - thanks for pointing out the range exception when moving empty strings.

  tommi prami - thanks for emailing to inform me about the zlib 1.2.6 udpate.

  marian pascalau - thanks for providing the win64 obj files and your win64 modifications.

  marat safin - thanks for the paypal donation.

  moacir schmidt - thanks for the paypal donation.

  roman ganz - thanks for the paypal donation and informing me about the zlib 1.2.8 update.
