{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Types;

interface

type
  PLongint = System.PLongint;
  {$EXTERNALSYM PLongint}
  PInteger = System.PInteger;
  {$EXTERNALSYM PInteger}
  PSmallInt = System.PSmallInt;
  {$EXTERNALSYM PSmallInt}
  PDouble = System.PDouble;
  {$EXTERNALSYM PDouble}
  PByte = System.PByte;
  {$EXTERNALSYM PByte}

  TIntegerDynArray      = array of Integer;
  {$EXTERNALSYM TIntegerDynArray}
  TCardinalDynArray     = array of Cardinal;
  {$EXTERNALSYM TCardinalDynArray}
  TWordDynArray         = array of Word;
  {$EXTERNALSYM TWordDynArray}
  TSmallIntDynArray     = array of SmallInt;
  {$EXTERNALSYM TSmallIntDynArray}
  TByteDynArray         = array of Byte;
  {$EXTERNALSYM TByteDynArray}
  TShortIntDynArray     = array of ShortInt;
  {$EXTERNALSYM TShortIntDynArray}
  TInt64DynArray        = array of Int64;
  {$EXTERNALSYM TInt64DynArray}
  TLongWordDynArray     = array of LongWord;
  {$EXTERNALSYM TLongWordDynArray}
  TSingleDynArray       = array of Single;
  {$EXTERNALSYM TSingleDynArray}
  TDoubleDynArray       = array of Double;
  {$EXTERNALSYM TDoubleDynArray}
  TBooleanDynArray      = array of Boolean;
  {$EXTERNALSYM TBooleanDynArray}
  TStringDynArray       = array of string;
  {$EXTERNALSYM TStringDynArray}
  TWideStringDynArray   = array of WideString;
  {$EXTERNALSYM TWideStringDynArray}

  PPoint = ^TPoint;
  TPoint = record
    X: Longint;
    Y: Longint;
  end;
  {$NODEFINE TPoint}
  tagPOINT = TPoint;
  {$NODEFINE tagPOINT}

  PRect = ^TRect;
  TRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft, BottomRight: TPoint);
  end;
  {$NODEFINE TRect}

  PSize = ^TSize;
  tagSIZE = record
    cx: Longint;
    cy: Longint;
  end;
  {$EXTERNALSYM tagSIZE}
  TSize = tagSIZE;
  SIZE = tagSIZE;
  {$EXTERNALSYM SIZE}

  PSmallPoint = ^TSmallPoint;
  TSmallPoint = record
    x: SmallInt;
    y: SmallInt;
  end;

  (*$HPPEMIT 'namespace Types'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '  struct TPoint : public POINT'*)
  (*$HPPEMIT '  {'*)
  (*$HPPEMIT '    TPoint() { x = y = 0; }'*)
  (*$HPPEMIT '    TPoint(int _x, int _y) { x=_x; y=_y; }'*)
  (*$HPPEMIT '    TPoint(const POINT& pt)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      x = pt.x;'*)
  (*$HPPEMIT '      y = pt.y;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool operator ==(const TPoint& pt) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return (x == pt.x) && (y == pt.y);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    bool operator !=(const TPoint& pt) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return !(pt == *this);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool IsEmpty() const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return !x && !y;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    void Offset(int DX, int DY)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      x += DX;'*)
  (*$HPPEMIT '      y += DY;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    static int __fastcall _sqr(int i) // Helper - private?'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '       return i*i;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    bool PtInCircle(const TPoint& CircleCenter, int Radius) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return (Radius > 0) && ((_sqr(CircleCenter.x-x)+_sqr(CircleCenter.y-y)) < _sqr(Radius));'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    __property LONG X = { read=x,   write=x  };'*)
  (*$HPPEMIT '    __property LONG Y = { read=y,   write=y  };'*)
  (*$HPPEMIT '  };'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '  typedef TPoint tagPoint;'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '  struct TRect : public RECT'*)
  (*$HPPEMIT '  {'*)
  (*$HPPEMIT '    TRect() { left=top=right=bottom=0; }'*)
  (*$HPPEMIT '    TRect(const TPoint& TL, const TPoint& BR) { left=TL.x; top=TL.y; right=BR.x; bottom=BR.y; }'*)
  (*$HPPEMIT '    TRect(int l, int t, int r, int b)         { left=l;    top=t;    right=r;    bottom=b;    }'*)
  (*$HPPEMIT '    TRect(const RECT& r)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      left    = r.left;'*)
  (*$HPPEMIT '      top     = r.top;'*)
  (*$HPPEMIT '      right   = r.right;'*)
  (*$HPPEMIT '      bottom  = r.bottom;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    int Width () const { return right  - left; }'*)
  (*$HPPEMIT '    int Height() const { return bottom - top ; }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    TPoint& TopLeft()                 { return *((TPoint* )this); }'*)
  (*$HPPEMIT '    TPoint& BottomRight()             { return *((TPoint* )this+1); }'*)
  (*$HPPEMIT '    const TPoint& TopLeft() const     { return *((TPoint* )this); }'*)
  (*$HPPEMIT '    const TPoint& BottomRight() const { return *((TPoint* )this+1); }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool operator ==(const TRect& rc) const '*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '       return left ==  rc.left  && top==rc.top &&     '*)
  (*$HPPEMIT '              right == rc.right && bottom==rc.bottom; '*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    bool operator !=(const TRect& rc) const '*)
  (*$HPPEMIT '    {  return !(rc==*this); }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool Contains(const TPoint& p) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '       return ((p.x >= left) && (p.y >= top) && (p.x < right) && (p.y < bottom));'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool IsEmpty() const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return (right <= left) || (bottom <= top);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool PtInRect(const TPoint &pt) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return Contains(pt);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool Intersects(const TRect &r) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return Contains(r.TopLeft()) ||'*)
  (*$HPPEMIT '             Contains(r.BottomRight());'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    bool Overlaps(const TRect &r) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return Contains(r.TopLeft()) &&'*)
  (*$HPPEMIT '             Contains(r.BottomRight());'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    void Offset(int DX, int DY)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      left   += DX;'*)
  (*$HPPEMIT '      right  += DY;'*)
  (*$HPPEMIT '      top    += DY;'*)
  (*$HPPEMIT '      bottom += DY;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    void Inflate(int DX, int DY)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      left   -= DX;'*)
  (*$HPPEMIT '      right  += DY;'*)
  (*$HPPEMIT '      top    -= DY;'*)
  (*$HPPEMIT '      bottom += DY;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    void Inflate(int l, int t, int r, int b)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      left   -= l;'*)
  (*$HPPEMIT '      right  += r;'*)
  (*$HPPEMIT '      top    -= t;'*)
  (*$HPPEMIT '      bottom += b;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    // NOTE: Several methods (Height, Width, IsEmpty, PtInRect, etc) assume normalized TRects'*)
  (*$HPPEMIT '    //       So use this method first if you have a TRect with (top > bottom) or (left > right).'*)
  (*$HPPEMIT '    void NormalizeRect()'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      int i;'*)
  (*$HPPEMIT '      if (left > right)'*)
  (*$HPPEMIT '      {'*)
  (*$HPPEMIT '        i = left;'*)
  (*$HPPEMIT '        left = right;'*)
  (*$HPPEMIT '        right = i;'*)
  (*$HPPEMIT '      }'*)
  (*$HPPEMIT '      if (top > bottom)'*)
  (*$HPPEMIT '      {'*)
  (*$HPPEMIT '        i = top;'*)
  (*$HPPEMIT '        top = bottom;'*)
  (*$HPPEMIT '        bottom = i;'*)
  (*$HPPEMIT '      }'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    TRect Normalize() const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      TRect result = *this;'*)
  (*$HPPEMIT '      result.NormalizeRect();'*)
  (*$HPPEMIT '      return result;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    TPoint CenterPoint() const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return TPoint((left+right)/2, (top+bottom)/2);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT '    TRect CenteredRect(const TRect &CenteredRect) const'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      int w = CenteredRect.Width();'*)
  (*$HPPEMIT '      int h = CenteredRect.Height();'*)
  (*$HPPEMIT '      int x = (right + left)/2;'*)
  (*$HPPEMIT '      int y = (top + bottom)/2;'*)
  (*$HPPEMIT '      return TRect(x-w/2, y-h/2, x+(w+1)/2, y+(h+1)/2);'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    bool  IntersectRect(const TRect &R1, const TRect &R2);'*)
  (*$HPPEMIT '    bool  UnionRect(const TRect &R1, const TRect &R2);'*)
  (*$HPPEMIT ' #if 0'*)
  (*$HPPEMIT '    TRect SplitRect(TSplitRectType SplitType, int Size) const;'*)
  (*$HPPEMIT '    TRect SplitRect(TSplitRectType SplitType, double Percent) const;'*)
  (*$HPPEMIT ' #endif'*)
  (*$HPPEMIT ' '*)
  (*$HPPEMIT ' #if defined(_Windows)'*)
  (*$HPPEMIT '    bool SubtractRect(const TRect &R1, const TRect &R2)'*)
  (*$HPPEMIT '    {'*)
  (*$HPPEMIT '      return ::SubtractRect(this, &R1, &R2) != 0;'*)
  (*$HPPEMIT '    }'*)
  (*$HPPEMIT ' #endif'*)  
  (*$HPPEMIT ' '*)
  (*$HPPEMIT '    __property LONG Left    = { read=left,   write=left   }; '*)
  (*$HPPEMIT '    __property LONG Top     = { read=top,    write=top    }; '*)
  (*$HPPEMIT '    __property LONG Right   = { read=right,  write=right  }; '*)
  (*$HPPEMIT '    __property LONG Bottom  = { read=bottom, write=bottom }; '*)
  (*$HPPEMIT '  };'*)
  (*$HPPEMIT '} /* namespace Types */ ;'*)
  
  (*$HPPEMIT END 'namespace Types {'*)
  (*$HPPEMIT END '    inline bool TRect::IntersectRect(const TRect &R1, const TRect &R2)'*)
  (*$HPPEMIT END '    {'*)
  (*$HPPEMIT END '      return Types::IntersectRect(*this, R1, R2) != 0;'*)
  (*$HPPEMIT END '    }'*)
  (*$HPPEMIT END '    inline bool TRect::UnionRect(const TRect &R1, const TRect &R2)'*)
  (*$HPPEMIT END '    {'*)
  (*$HPPEMIT END '      return Types::UnionRect(*this, R1, R2) != 0;'*)
  (*$HPPEMIT END '    }'*)
  (*$HPPEMIT END ' #if 0'*)
  (*$HPPEMIT END '    inline TRect SplitRect(TSplitRectType SplitType, int Size) const'*)
  (*$HPPEMIT END '    {'*)
  (*$HPPEMIT END '      return Types::SplitRect(*this, SplitType, Size);'*)
  (*$HPPEMIT END '    }'*)
  (*$HPPEMIT END '    inline TRect SplitRect(TSplitRectType SplitType, double Percent) const'*)
  (*$HPPEMIT END '    {'*)
  (*$HPPEMIT END '      return Types::SplitRect(*this, SplitType, Percent);'*)
  (*$HPPEMIT END '    }'*)
  (*$HPPEMIT END ' #endif'*)
  (*$HPPEMIT END '} /* namespace Types */ ;'*)
  

  DWORD = LongWord;
  {$EXTERNALSYM DWORD}
const
  RT_RCDATA       = PChar(10);
  {$EXTERNALSYM RT_RCDATA}

                                                                                                                                                                                                                                                                                                                                                       
{$IFNDEF MSWINDOWS}
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
type
  PDisplay = Pointer;
  PEvent = Pointer;
  TXrmOptionDescRec = record end;
  XrmOptionDescRec = TXrmOptionDescRec;
  PXrmOptionDescRec = ^TXrmOptionDescRec;
  Widget = Pointer;
  WidgetClass = Pointer;
  ArgList = Pointer;
  Region = Pointer;
  
 {$IFDEF MACOS}
   EventHandlerCallRef = Pointer;
   EventRef = Pointer;
   CGImageRef = Pointer;
   RgnHandle = Pointer;
   HIShapeRef = Pointer;
   HIMutableShapeRef = Pointer;
   OSMenuRef = Pointer;
 {$ENDIF MACOS}

const
  STGTY_STORAGE   = 1;
  STGTY_STREAM    = 2;
  STGTY_LOCKBYTES = 3;
  STGTY_PROPERTY  = 4;

  STREAM_SEEK_SET = 0;
  STREAM_SEEK_CUR = 1;
  STREAM_SEEK_END = 2;

  LOCK_WRITE     = 1;
  LOCK_EXCLUSIVE = 2;
  LOCK_ONLYONCE  = 4;

  { Unspecified error }
  E_FAIL                      = HRESULT($80004005);

  { Unable to perform requested operation. }
  STG_E_INVALIDFUNCTION       = HRESULT($80030001);

  { %l could not be found. }
  STG_E_FILENOTFOUND          = HRESULT($80030002);

  { The path %l could not be found. }
  STG_E_PATHNOTFOUND          = HRESULT($80030003);

  { There are insufficient resources to open another file. }
  STG_E_TOOMANYOPENFILES      = HRESULT($80030004);

  { Access Denied. }
  STG_E_ACCESSDENIED          = HRESULT($80030005);

  { Attempted an operation on an invalid object. }
  STG_E_INVALIDHANDLE         = HRESULT($80030006);

  { There is insufficient memory available to complete operation. }
  STG_E_INSUFFICIENTMEMORY    = HRESULT($80030008);

  { Invalid pointer error. }
  STG_E_INVALIDPOINTER        = HRESULT($80030009);

  { There are no more entries to return. }
  STG_E_NOMOREFILES           = HRESULT($80030012);

  { Disk is write-protected. }
  STG_E_DISKISWRITEPROTECTED  = HRESULT($80030013);

  { An error occurred during a seek operation. }
  STG_E_SEEKERROR             = HRESULT($80030019);

  { A disk error occurred during a write operation. }
  STG_E_WRITEFAULT            = HRESULT($8003001D);

  { A disk error occurred during a read operation. }
  STG_E_READFAULT             = HRESULT($8003001E);

  { A share violation has occurred. }
  STG_E_SHAREVIOLATION        = HRESULT($80030020);

  { A lock violation has occurred. }
  STG_E_LOCKVIOLATION         = HRESULT($80030021);

  { %l already exists. }
  STG_E_FILEALREADYEXISTS     = HRESULT($80030050);

  { Invalid parameter error. }
  STG_E_INVALIDPARAMETER      = HRESULT($80030057);

  { There is insufficient disk space to complete operation. }
  STG_E_MEDIUMFULL            = HRESULT($80030070);

  { Illegal write of non-simple property to simple property set. }
  STG_E_PROPSETMISMATCHED     = HRESULT($800300F0);

  { An API call exited abnormally. }
  STG_E_ABNORMALAPIEXIT       = HRESULT($800300FA);

  { The file %l is not a valid compound file. }
  STG_E_INVALIDHEADER         = HRESULT($800300FB);

  { The name %l is not valid. }
  STG_E_INVALIDNAME           = HRESULT($800300FC);

  { An unexpected error occurred. }
  STG_E_UNKNOWN               = HRESULT($800300FD);

  { That function is not implemented. }
  STG_E_UNIMPLEMENTEDFUNCTION = HRESULT($800300FE);

  { Invalid flag error. }
  STG_E_INVALIDFLAG           = HRESULT($800300FF);

  { Attempted to use an object that is busy. }
  STG_E_INUSE                 = HRESULT($80030100);

  { The storage has been changed since the last commit. }
  STG_E_NOTCURRENT            = HRESULT($80030101);

  { Attempted to use an object that has ceased to exist. }
  STG_E_REVERTED              = HRESULT($80030102);

  { Can't save. }
  STG_E_CANTSAVE              = HRESULT($80030103);

  { The compound file %l was produced with an incompatible version of storage. }
  STG_E_OLDFORMAT             = HRESULT($80030104);

  { The compound file %l was produced with a newer version of storage. }
  STG_E_OLDDLL                = HRESULT($80030105);

  { Share.exe or equivalent is required for operation. }
  STG_E_SHAREREQUIRED         = HRESULT($80030106);

  { Illegal operation called on non-file based storage. }
  STG_E_NOTFILEBASEDSTORAGE   = HRESULT($80030107);

  { Illegal operation called on object with extant marshallings. }
  STG_E_EXTANTMARSHALLINGS    = HRESULT($80030108);

  { The docfile has been corrupted. }
  STG_E_DOCFILECORRUPT        = HRESULT($80030109);

  { OLE32.DLL has been loaded at the wrong address. }
  STG_E_BADBASEADDRESS        = HRESULT($80030110);

  { The file download was aborted abnormally.  The file is incomplete. }
  STG_E_INCOMPLETE            = HRESULT($80030201);

  { The file download has been terminated. }
  STG_E_TERMINATED            = HRESULT($80030202);

  { The underlying file was converted to compound file format. }
  STG_S_CONVERTED             = HRESULT($00030200);

  { The storage operation should block until more data is available. }
  STG_S_BLOCK                 = HRESULT($00030201);

  { The storage operation should retry immediately. }
  STG_S_RETRYNOW              = HRESULT($00030202);

  { The notified event sink will not influence the storage operation. }
  STG_S_MONITORING            = HRESULT($00030203);


type
  TOleChar = WideChar;
  POleStr = PWideChar;
  PPOleStr = ^POleStr;

  PCLSID = PGUID;
  TCLSID = TGUID;

{ 64-bit large integer }

  Largeint = Int64;
  {$EXTERNALSYM Largeint}

//  DWORD = LongWord;
//  {$EXTERNALSYM DWORD}
  PDWORD = ^DWORD;
  {$EXTERNALSYM PDWORD}

  { File System time stamps are represented with the following structure: }
  PFileTime = ^TFileTime;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
  TFileTime = _FILETIME;
  FILETIME = _FILETIME;

{ IStream interface }

  PStatStg = ^TStatStg;
  tagSTATSTG = record
    pwcsName: POleStr;
    dwType: Longint;
    cbSize: Largeint;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    clsid: TCLSID;
    grfStateBits: Longint;
    reserved: Longint;
  end;
  TStatStg = tagSTATSTG;
  STATSTG = TStatStg;

  IClassFactory = interface(IUnknown)
    ['{00000001-0000-0000-C000-000000000046}']
    function CreateInstance(const unkOuter: IUnknown; const iid: TGUID;
      out obj): HResult; stdcall;
    function LockServer(fLock: LongBool): HResult; stdcall;
  end;

  ISequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
      stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
      stdcall;
  end;

  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;
{$ENDIF} { !MSWINDOWS }

type
  TSplitRectType = (
    srLeft,
    srRight,
    srTop,
    srBottom
  );

function EqualRect(const R1, R2: TRect): Boolean;
function Rect(Left, Top, Right, Bottom: Integer): TRect;
{$EXTERNALSYM Rect}
function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
{$EXTERNALSYM Bounds}
function Point(X, Y: Integer): TPoint; inline;
{$EXTERNALSYM Point}
function SmallPoint(X, Y: Integer): TSmallPoint; inline; overload;
function SmallPoint(XY: LongWord): TSmallPoint; overload;
function PtInRect(const Rect: TRect; const P: TPoint): Boolean;
function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean;
function IntersectRect(out Rect: TRect; const R1, R2: TRect): Boolean;
function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;
function IsRectEmpty(const Rect: TRect): Boolean;
function OffsetRect(var Rect: TRect; DX: Integer; DY: Integer): Boolean;
function CenterPoint(const Rect: TRect): TPoint;
function RectWidth(const Rect: TRect): Integer; inline;
function RectHeight(const Rect: TRect): Integer; inline;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect; overload;
function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect; overload;
function CenteredRect(const SourceRect: TRect; const CenteredRect: TRect): TRect;

type
  TValueRelationship = -1..1;

const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);

implementation

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect;
begin
  Result := Rect;
  case SplitType of
    srLeft:
      Result.Right := Rect.Left + Size;
    srRight:
      Result.Left := Rect.Right - Size;
    srTop:
      Result.Bottom := Rect.Top + Size;
    srBottom:
      Result.Top := Rect.Bottom - Size;
  end;
end;

function SplitRect(const Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect;
begin
  Result := Rect;
  case SplitType of
    srLeft:
      Result.Right := Rect.Left + Trunc(Percent * RectWidth(Rect));
    srRight:
      Result.Left := Rect.Right - Trunc(Percent * RectWidth(Rect));
    srTop:
      Result.Bottom := Rect.Top + Trunc(Percent * RectHeight(Rect));
    srBottom:
      Result.Top := Rect.Bottom - Trunc(Percent * RectHeight(Rect));
  end;
end;

function CenteredRect(const SourceRect: TRect; const CenteredRect: TRect): TRect;
var
  Width, Height: Integer;
  X, Y: Integer;
begin
  Width := RectWidth(CenteredRect);
  Height := RectHeight(CenteredRect);
  X := (SourceRect.Right + SourceRect.Left) div 2;
  Y := (SourceRect.Top + SourceRect.Bottom) div 2;
  Result := Rect(X - Width div 2, Y - Height div 2, X + (Width + 1) div 2, Y + (Height + 1) div 2);
end;

function EqualRect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
    (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom);
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end; 

function Point(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function SmallPoint(X, Y: Integer): TSmallPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function SmallPoint(XY: LongWord): TSmallPoint;
begin
  Result.X := SmallInt(XY and $0000FFFF);
  Result.Y := SmallInt(XY shr 16);
end;

function PtInRect(const Rect: TRect; const P: TPoint): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

function PtInCircle(const Point, Center: TPoint; Radius: Integer): Boolean;
begin
  if Radius > 0 then
  begin
    Result := Sqr((Point.X - Center.X) / Radius) +
      Sqr((Point.Y - Center.Y) / Radius) <= 1;
  end
  else
  begin
    Result := False;
  end;
end;

function IntersectRect(out Rect: TRect; const R1, R2: TRect): Boolean;
begin
  Rect := R1;
  if R2.Left > R1.Left then Rect.Left := R2.Left;
  if R2.Top > R1.Top then Rect.Top := R2.Top;
  if R2.Right < R1.Right then Rect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then Rect.Bottom := R2.Bottom;
  Result := not IsRectEmpty(Rect);
  if not Result then FillChar(Rect, SizeOf(Rect), 0);
end;

function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;
begin
  Rect := R1;
  if not IsRectEmpty(R2) then
  begin
    if R2.Left < R1.Left then Rect.Left := R2.Left;
    if R2.Top < R1.Top then Rect.Top := R2.Top;
    if R2.Right > R1.Right then Rect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then Rect.Bottom := R2.Bottom;
  end;
  Result := not IsRectEmpty(Rect);
  if not Result then FillChar(Rect, SizeOf(Rect), 0);
end;

function IsRectEmpty(const Rect: TRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

function OffsetRect(var Rect: TRect; DX: Integer; DY: Integer): Boolean;
begin
  if @Rect <> nil then // Test to increase compatiblity with Windows
  begin
    Inc(Rect.Left, DX);
    Inc(Rect.Right, DX);
    Inc(Rect.Top, DY);
    Inc(Rect.Bottom, DY);
    Result := True;
  end
  else
    Result := False;
end;

function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom :=  ATop + AHeight;
  end;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  with Rect do
  begin
    Result.X := (Right - Left) div 2 + Left;
    Result.Y := (Bottom - Top) div 2 + Top;
  end;
end;

end.


