unit gdip_gfx;

interface

uses Windows, Classes, SysUtils, Graphics, ShlObj, ShellAPI, PIDL, ActiveX,
  CommCtrl, GDIPAPI, toolu, dockh;

const
  HLSMAX = 240;
  RGBMAX = 255;
  UNDEFINED = (HLSMAX * 2) div 3;
  DEFAULT_COLOR_DATA = $3c803c00; // contrast, brightness, saturation, color_offset //
  DEFAULT_COLOR_OFFSET = $00;
  DEFAULT_SATURATION = $3c;
  DEFAULT_BRIGHTNESS = $80;
  DEFAULT_CONTRAST = $3c;

  LWA_COLORKEY = 1;
  LWA_ALPHA    = 2;
  ULW_COLORKEY = 1;
  ULW_ALPHA    = 2;
  ULW_OPAQUE   = 4;

type
  TRGBAData = array [0..3] of Byte;
  PRGBAData = ^TRGBAData;
  ARGB   = DWORD;
  PARGB  = ^ARGB;

  TGDIPColor = packed record
    case integer of
    0:(B, G, R, A: Byte);
    1:(I: integer)
  end;
  TGDIPColors = packed array [0..MaxLongint div 4 - 1] of TGDIPColor;
  PGDIPColors = ^TGDIPColors;

  TStretchStyle = (ssNone, ssStretch, ssTile, ssVStretch, ssHStretch,
    ssVTile, ssHTile, ssVStretchHTile, ssHStretchVTile);

  _SimpleBitmap = record
    topleft: windows.TPoint;
    width: integer;
    height: integer;
    dc: hdc;
    bi: windows.TBitmapInfo;
    BufferBits: Pointer;
    OldBitmap: HBitmap;
    BufferBitmap: HBitmap;
  end;

  _FontData = record
    name: array [0..255] of char;
    size: integer;
    color: uint;
    color_outline: uint;
    bold: boolean;
    italic: boolean;
    outline: boolean;
  end;

  ppixel = ^pixel;
  pixel = record
    b: byte;
    g: byte;
    r: byte;
    a: byte;
  end;

  PBlendFunction = ^TBlendFunction;
  _BLENDFUNCTION = record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
  end;
  {$EXTERNALSYM _BLENDFUNCTION}
  BLENDFUNCTION = _BLENDFUNCTION;
  {$EXTERNALSYM BLENDFUNCTION}
  LPBLENDFUNCTION = ^BLENDFUNCTION;
  {$EXTERNALSYM LPBLENDFUNCTION}
  TBlendFunction = _BLENDFUNCTION;

function UpdateLayeredWindow(hWnd: HWND; hdcDst: HDC; pptDst: LPPOINT;
    psize: LPSIZE; hdcSrc: HDC; pptSrc: LPPOINT; crKey: COLORREF;
    pblend: LPBLENDFUNCTION; dwFlags: DWORD): BOOL; stdcall; external 'user32.dll';

function CreateBitmap(var bmp: _SimpleBitmap): boolean;
procedure DeleteBitmap(var bmp: _SimpleBitmap);
procedure BitmapReflection(var bmp: _SimpleBitmap; XOffset, YOffset, size, ReflectionSize, Direction: integer);
function CreateRegionFromMask(bmp: _SimpleBitmap): HRGN;
procedure IdentityMatrix(var matrix: ColorMatrix);
procedure MultiplyMatrix(var matrix, matrix2: ColorMatrix);
procedure CreateColorMatrix(color_data: integer; var clMatrix: ColorMatrix);
procedure CreateLightnessMatrix(Lit: integer; var brMatrix: ColorMatrix);
procedure CreateAlphaMatrix(alpha: integer; var matrix: ColorMatrix);
function CreateGraphics(dc: hdc; color: uint = 0): Pointer;
procedure DeleteGraphics(hgdip: Pointer);
procedure DrawEx(dst, src: Pointer; W, H: uint; dstrect: windows.TRect;
  margins: windows.TRect; Style: TStretchStyle = ssStretch;
  color_data: integer = DEFAULT_COLOR_DATA);
function MeasureString(str: string; hfont: Pointer): TRectF;
procedure UpdateLWindow(hWnd: THandle; bmp: _SimpleBitmap; SrcAlpha: integer = 255);
procedure UpdateLWindowContent(hWnd: THandle; bmp: _SimpleBitmap; SrcAlpha: integer = 255);
procedure UpdateLWindowPosAlpha(hWnd: THandle; x, y: integer; SrcAlpha: integer = 255);
procedure UpdateLWindowPos(hWnd: THandle; x, y: integer);
procedure LoadImageFromPIDL(pidl: PItemIDList; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
procedure LoadImageFromHWnd(h: THandle; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint; timeout: uint);
procedure LoadImage(imagefile: string; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
function IconToGDIPBitmap(AIcon: HICON): Pointer;
function DownscaleImage(var image: pointer; MaxSize: integer; var srcwidth, srcheight: uint; DeleteSource: boolean): boolean;
procedure LoadGdipImageFromResource(res_name: pchar; var image: Pointer);
procedure OffsetRectF(var r: TRectF; offx, offy: integer);
procedure CopyFontData(var fFrom: _FontData; var fTo: _FontData);
function FontDataToString(var fData: _FontData): string;
procedure StringToFontData(str: string; var font: _FontData);
function SwapColor(color: uint): uint;
function FadeColorTo(color, fadeto: uint; fadeby: extended): uint;
procedure RGBtoHLS(color: uint; out h, l, s: integer);
function HLStoRGB(h, l, s: integer): uint;
function StringToStretchStyle(str: string): TStretchStyle;
function StretchStyleToString(ss: TStretchStyle): string;

var
  StartupInput: GdiplusStartupInput;
  gdiplusToken: ULONG;

implementation
//------------------------------------------------------------------------------
function min(a, b: integer): integer;
begin
  if a < b then result:= a else result:= b;
end;
//------------------------------------------------------------------------------
function max(a, b: integer): integer;
begin
  if a < b then result:= b else result:= a;
end;
//--------------------------------------------------------------------------------------------------
function CreateBitmap(var bmp: _SimpleBitmap): boolean;
begin
  result:= false;
  bmp.dc:= windows.CreateCompatibleDC(0);
  if bmp.dc <> 0 then
  begin
    Fillchar(bmp.bi, sizeof(bmp.bi), #0);
    bmp.bi.bmiHeader.biSize:= sizeof(windows.TBitmapInfoHeader);
    bmp.bi.bmiHeader.biPlanes:= 1;
    bmp.bi.bmiHeader.biBitCount:= 32;
    bmp.bi.bmiHeader.biCompression:= BI_RGB;
    bmp.bi.bmiHeader.biWidth:= bmp.width;
    bmp.bi.bmiHeader.biHeight:= -1 * bmp.height;
    bmp.BufferBitmap:= windows.CreateDIBSection(bmp.dc, bmp.bi, DIB_RGB_COLORS, bmp.BufferBits, 0, 0);
    if (bmp.BufferBitmap = 0) or (bmp.BufferBits = nil) then
    begin
      if bmp.BufferBitmap <> 0 then windows.DeleteObject(bmp.BufferBitmap);
      windows.DeleteDC(bmp.dc);
    end else
    begin
	    bmp.OldBitmap:= windows.SelectObject(bmp.dc, bmp.BufferBitmap);
      result:= true;
    end;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure DeleteBitmap(var bmp: _SimpleBitmap);
begin
  if bmp.dc > 0 then
  begin
    windows.SelectObject(bmp.dc, bmp.OldBitmap);
    windows.DeleteObject(bmp.BufferBitmap);
    windows.DeleteDC(bmp.dc);
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure BitmapReflection(var bmp: _SimpleBitmap; XOffset, YOffset, size, ReflectionSize, Direction: integer);
var
  xi, yi: integer;
begin
  if ReflectionSize > 0 then
  try
    if Direction = 3 then
      for yi:= YOffset + size to YOffset + size + ReflectionSize - 1 do
        for xi:= XOffset * 4 to (XOffset + size) * 4 - 1 do
        begin
          pbyte(uint(bmp.BufferBits) + xi + yi * bmp.width * 4)^:=
            max(
              round(
                pbyte(uint(bmp.BufferBits) + xi + ((YOffset + size) * 2 - yi - 1) * bmp.width * 4)^ -
                pbyte(uint(bmp.BufferBits) + xi + ((YOffset + size) * 2 - yi - 1) * bmp.width * 4)^ *
                (yi - size - YOffset + 1) / ReflectionSize
              ) * 2 div 3,
            1);
        end;
    if Direction = 1 then
      for yi:= 0 to ReflectionSize - 1 do
        for xi:= XOffset * 4 to (XOffset + size) * 4 - 1 do
        begin
          pbyte(uint(bmp.BufferBits) + xi + (YOffset - ReflectionSize + yi) * bmp.width * 4)^:=
            max(
              round(
                pbyte(uint(bmp.BufferBits) + xi + (YOffset + ReflectionSize - yi - 1) * bmp.width * 4)^ -
                pbyte(uint(bmp.BufferBits) + xi + (YOffset + ReflectionSize - yi - 1) * bmp.width * 4)^ * (ReflectionSize - yi) / ReflectionSize
              ) * 2 div 3,
            1);
        end;
    if Direction = 0 then
      for yi:= YOffset to YOffset + size - 1 do
        for xi:= 0 to ReflectionSize - 1 do
        begin
          ppixel(uint(bmp.BufferBits) + ((XOffset - ReflectionSize + xi) + yi * bmp.width) * 4)^.a :=
            max(round(ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.a -
            ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.a * (ReflectionSize - xi) div ReflectionSize ) * 2 div 3, 1);
          ppixel(uint(bmp.BufferBits) + ((XOffset - ReflectionSize + xi) + yi * bmp.width) * 4)^.r :=
            max(round(ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.r -
            ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.r * (ReflectionSize - xi) div ReflectionSize ) * 2 div 3, 0);
          ppixel(uint(bmp.BufferBits) + ((XOffset - ReflectionSize + xi) + yi * bmp.width) * 4)^.g :=
            max(round(ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.g -
            ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.g * (ReflectionSize - xi) div ReflectionSize ) * 2 div 3, 0);
          ppixel(uint(bmp.BufferBits) + ((XOffset - ReflectionSize + xi) + yi * bmp.width) * 4)^.b :=
            max(round(ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.b -
            ppixel(uint(bmp.BufferBits) + ((XOffset + ReflectionSize - xi - 1) + yi * bmp.width) * 4)^.b * (ReflectionSize - xi) div ReflectionSize ) * 2 div 3, 0);
        end;
    if Direction = 2 then
      for yi:= YOffset to YOffset + size - 1 do
        for xi:= XOffset + size to XOffset + size + ReflectionSize - 1 do
        begin
          ppixel(uint(bmp.BufferBits) + (xi + yi * bmp.width) * 4)^.a :=
            max(round(ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.a -
            ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.a *
            (xi - size - XOffset + 1) div ReflectionSize ) * 2 div 3, 1);
          ppixel(uint(bmp.BufferBits) + (xi + yi * bmp.width) * 4)^.r :=
            max(round(ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.r -
            ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.r *
            (xi - size - XOffset + 1) div ReflectionSize ) * 2 div 3, 0);
          ppixel(uint(bmp.BufferBits) + (xi + yi * bmp.width) * 4)^.g :=
            max(round(ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.g -
            ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.g *
            (xi - size - XOffset + 1) div ReflectionSize ) * 2 div 3, 0);
          ppixel(uint(bmp.BufferBits) + (xi + yi * bmp.width) * 4)^.b :=
            max(round(ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.b -
            ppixel(uint(bmp.BufferBits) + (((XOffset + size) * 2 - xi - 1) + yi * bmp.width) * 4)^.b *
            (xi - size - XOffset + 1) div ReflectionSize ) * 2 div 3, 0);
        end;
  except
    on e: Exception do raise Exception.Create('in BitmapReflection'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function CreateRegionFromMask(bmp: _SimpleBitmap): HRGN;
var
  X, Y, StartX: integer;
  Excl: THandle;
  TransparentColor: pixel;
begin
  result := CreateRectRGN(0, 0, bmp.width, bmp.height);

  for Y := 0 to bmp.height - 1 do
  begin
    X := 0;
    if Y = 0 then
    begin
      TransparentColor.r := ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.r;
      TransparentColor.g := ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.g;
      TransparentColor.b := ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.b;
    end;
    StartX := -1;

    for X := 0 to bmp.width - 1 do
    begin
      if (ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.r = TransparentColor.r) and
        (ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.g = TransparentColor.g) and
        (ppixel(uint(bmp.BufferBits) + (X + Y * bmp.width) * 4)^.b = TransparentColor.b) then
      begin
        if StartX = -1 then StartX := X;
      end else
      begin
        if StartX > -1 then
        begin
          Excl := CreateRectRGN(StartX, Y, X, Y + 1);
          try
            CombineRGN(Result, Result, Excl, RGN_DIFF);
            StartX := -1;
          finally
            DeleteObject(Excl);
          end;
        end;
      end;

      if StartX > -1 then
      begin
        Excl := CreateRectRGN(StartX, Y, X + 1, Y + 1);
        try
          CombineRGN(Result, Result, Excl, RGN_DIFF);
        finally
          DeleteObject(Excl);
        end;
      end;
      StartX := -1;
    end;
  end;
end;
//--------------------------------------------------------------------------------------------------
function CreateGraphics(dc: hdc; color: uint = 0): Pointer;
begin
  result:= nil;
  GdipCreateFromHDC(dc, result);
  GdipGraphicsClear(result, color);
end;
//--------------------------------------------------------------------------------------------------
procedure DeleteGraphics(hgdip: Pointer);
begin
  if hgdip <> nil then GdipDeleteGraphics(hgdip);
end;
//--------------------------------------------------------------------------------------------------
procedure OffsetRectF(var r: TRectF; offx, offy: integer);
begin
  r.x:= r.x + offx;
  r.y:= r.y + offy;
  r.width:= r.width + offx;
  r.height:= r.height + offy;
end;
//--------------------------------------------------------------------------------------------------
procedure IdentityMatrix(var matrix: ColorMatrix);
begin
  matrix[0, 0]:= 1;
  matrix[1, 0]:= 0;
  matrix[2, 0]:= 0;
  matrix[3, 0]:= 0;
  matrix[4, 0]:= 0;
  matrix[0, 1]:= 0;
  matrix[1, 1]:= 1;
  matrix[2, 1]:= 0;
  matrix[3, 1]:= 0;
  matrix[4, 1]:= 0;
  matrix[0, 2]:= 0;
  matrix[1, 2]:= 0;
  matrix[2, 2]:= 1;
  matrix[3, 2]:= 0;
  matrix[4, 2]:= 0;
  matrix[0, 3]:= 0;
  matrix[1, 3]:= 0;
  matrix[2, 3]:= 0;
  matrix[3, 3]:= 1;
  matrix[4, 3]:= 0;
  matrix[0, 4]:= 0;
  matrix[1, 4]:= 0;
  matrix[2, 4]:= 0;
  matrix[3, 4]:= 0;
  matrix[4, 4]:= 1;
end;
//--------------------------------------------------------------------------------------------------
procedure MultiplyMatrix(var matrix, matrix2: ColorMatrix);
var
  x, y: integer;            
begin                       
  for x:= 0 to 4 do         
    for y:= 0 to 4 do
      matrix[x, y]:= matrix[0, y] * matrix2[x, 0] +
        matrix[1, y] * matrix2[x, 1] + matrix[2, y] * matrix2[x, 2] +
        matrix[3, y] * matrix2[x, 3] + matrix[4, y] * matrix2[x, 4];
end;
//--------------------------------------------------------------------------------------------------
procedure CreateColorMatrix(color_data: integer; var clMatrix: ColorMatrix);
var
  satMatrix, brMatrix, coMatrix: ColorMatrix;
  cl, r, g, b: single;
  br, co: single;
  sat, satCompl, sr, sg, sb: single;
begin
  // color //

  IdentityMatrix(clMatrix);
  cl:= byte(color_data);
  if cl > 239 then cl:= cl - 240;
  if cl < 0 then cl:= cl + 240;
  if cl <> DEFAULT_COLOR_OFFSET then
  begin
    if (cl >= 0) and (cl < 80) then
    begin
      r:= 80 - cl;
      g:= cl;
      b:= 0;
    end;
    if (cl >= 80) and (cl < 160) then
    begin
      r:= 0;
      g:= 160 - cl;
      b:= cl - 80;
    end;
    if (cl >= 160) and (cl < 240) then
    begin
      r:= cl - 160;
      g:= 0;
      b:= 240 - cl;
    end;
    r:= r / 80;
    g:= g / 80;
    b:= b / 80;
    clMatrix[0, 0]:= r;
    clMatrix[1, 0]:= g;
    clMatrix[2, 0]:= b;
    clMatrix[0, 1]:= g;
    clMatrix[1, 1]:= b;
    clMatrix[2, 1]:= r;
    clMatrix[0, 2]:= b;
    clMatrix[1, 2]:= r;
    clMatrix[2, 2]:= g;
    MultiplyMatrix(satMatrix, clMatrix);
  end;

  // saturation //

  IdentityMatrix(satMatrix);
  sat := byte(color_data shr 8);
  if (sat <> DEFAULT_SATURATION) and (sat > 0) then
  begin
    sat := sat / 60;
    satCompl := 1 - sat;
    sr := 0.3086 * satCompl;
    sg := 0.6094 * satCompl;
    sb := 0.0820 * satCompl;
    satMatrix[0, 0] := sr + sat;
    satMatrix[0, 1] := sr;
    satMatrix[0, 2] := sr;
    satMatrix[1, 0] := sg;
    satMatrix[1, 1] := sg + sat;
    satMatrix[1, 2] := sg;
    satMatrix[2, 0] := sb;
    satMatrix[2, 1] := sb;
    satMatrix[2, 2] := sb + sat;
    MultiplyMatrix(clMatrix, satMatrix);
  end
  else
  if sat = 0 then // grayscale //
  begin
    sr := 0.3;
    sg := 0.59;
    sb := 0.11;
    clMatrix[0, 0] := sr;
    clMatrix[0, 1] := sr;
    clMatrix[0, 2] := sr;
    clMatrix[1, 0] := sg;
    clMatrix[1, 1] := sg;
    clMatrix[1, 2] := sg;
    clMatrix[2, 0] := sb;
    clMatrix[2, 1] := sb;
    clMatrix[2, 2] := sb;
  end;

  // lightness //

  IdentityMatrix(brMatrix);
  br:= byte(color_data shr 16);
  if br <> DEFAULT_BRIGHTNESS then
  begin
    br:= br / 128 - 1;
    brMatrix[4, 0]:= br;
    brMatrix[4, 1]:= br;
    brMatrix[4, 2]:= br;
    MultiplyMatrix(clMatrix, brMatrix);
  end;

  // contrast //

  IdentityMatrix(coMatrix);
  co:= byte(color_data shr 24);
  if co <> DEFAULT_CONTRAST then
  begin
    co:= co / 60;
    coMatrix[0, 0]:= co;
    coMatrix[1, 1]:= co;
    coMatrix[2, 2]:= co;
    coMatrix[4, 0]:= 0.1;
    coMatrix[4, 1]:= 0.1;
    coMatrix[4, 2]:= 0.1;
    MultiplyMatrix(clMatrix, coMatrix);
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure CreateLightnessMatrix(Lit: integer; var brMatrix: ColorMatrix);
var
  br: extended;
begin
  IdentityMatrix(brMatrix);
  if Lit > 255 then Lit:= 255;
  br:= Lit / 128 - 1;
  brMatrix[4, 0]:= br;
  brMatrix[4, 1]:= br;
  brMatrix[4, 2]:= br;
end;
//--------------------------------------------------------------------------------------------------
procedure CreateAlphaMatrix(alpha: integer; var matrix: ColorMatrix);
begin
  IdentityMatrix(matrix);
  if alpha > 255 then alpha:= 255;
  if alpha < 0 then alpha:= 0;
  matrix[3, 3]:= alpha / 255;
end;
//------------------------------------------------------------------------------
procedure DrawEx(dst, src: Pointer; W, H: uint; dstrect: windows.TRect;
  margins: windows.TRect; Style: TStretchStyle = ssStretch;
  color_data: integer = DEFAULT_COLOR_DATA);
var
  x, y: integer;
  srcwidth, srcheight: uint;
  dstwidth, dstheight: uint;
  part_w, part_h: uint;
  //
  hattr: Pointer;
  matrix: ColorMatrix;
begin
  if dst = nil then exit;

  try
    if src = nil then
    begin
      GdipGraphicsClear(dst, 0);
      exit;
    end;

    srcwidth := W;
    srcheight := H;
    dstwidth := dstrect.Right;
    dstheight := dstrect.Bottom;

    // colorization //

    hattr:= nil;
    if DEFAULT_COLOR_DATA <> color_data then
    begin
      CreateColorMatrix(color_data, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);
    end;

  except exit;
  end;

  if Margins.Left > srcwidth then margins.left:= srcwidth;
  if Margins.Right > srcwidth - Margins.Left then Margins.Right:= srcwidth - Margins.Left;
  if Margins.Top > srcheight then Margins.Top:= srcheight;
  if Margins.Bottom > srcheight - Margins.Top then Margins.Bottom:= srcheight - Margins.Top;
  if Margins.Left > dstwidth then Margins.Left:= dstwidth;
  if Margins.Right > dstwidth - Margins.Left then Margins.right:= dstwidth - Margins.Left;
  if Margins.Top > dstheight then Margins.Top:= dstheight;
  if Margins.Bottom > dstheight - Margins.Top then Margins.bottom:= dstheight - Margins.Top;

  try

  // stretch //
  if style = ssStretch then
  begin

    // center //
    GdipDrawImageRectRectI(dst, src, margins.left + dstrect.left, margins.top + dstrect.top,
      dstwidth - margins.left - margins.right, dstheight - margins.top - margins.bottom,
      margins.left, margins.top, srcwidth - margins.left - margins.right,
      srcheight - margins.top - margins.bottom, UnitPixel, hattr, nil, nil);

    // left line //
    if margins.left > 0 then
      GdipDrawImageRectRectI(dst, src,
        dstrect.left, margins.top + dstrect.top,
        margins.left, dstheight - margins.top - margins.bottom,
        0, margins.top, margins.left, srcheight - margins.top - margins.bottom,
        UnitPixel, hattr, nil, nil);

    // right line //
    if margins.right > 0 then
      GdipDrawImageRectRectI(dst, src,
        dstwidth - margins.right + dstrect.left, margins.top + dstrect.top,
        margins.right, dstheight - margins.top - margins.bottom,
        srcwidth - margins.right,
        margins.top, margins.right, srcheight - margins.top - margins.bottom,
        UnitPixel, hattr, nil, nil);

    // top line //
    if margins.top > 0 then
      GdipDrawImageRectRectI(dst, src,
        margins.left + dstrect.left, dstrect.top,
        dstwidth - margins.left - margins.right, margins.top,
        margins.left, 0, srcwidth - margins.left - margins.right, margins.top,
        UnitPixel, hattr, nil, nil);

    // bottom line //
    if margins.bottom > 0 then
      GdipDrawImageRectRectI(dst, src,
        margins.left + dstrect.left, dstheight - margins.bottom + dstrect.top,
        dstwidth - margins.left - margins.right, margins.bottom,
        margins.left, srcheight - margins.bottom,
        srcwidth - margins.left - margins.right, margins.bottom,
        UnitPixel, hattr, nil, nil);

  // tile //
  end else if style = ssTile then
  begin

    // center //
    x:= Margins.Left + dstrect.left;
    while x <= dstrect.left + dstWidth - Margins.Right do
    begin

      part_w:= srcwidth - margins.left - margins.right;
      if x + part_w > dstrect.left + dstWidth - Margins.right
      then part_w:= dstrect.left + dstWidth - Margins.right - x;

      y:= Margins.Top + dstrect.top;
      while y <= dstrect.top + dstHeight - Margins.Bottom do
      begin

        part_h:= srcheight - margins.top - margins.bottom;
        if y + part_h > dstrect.top + dstHeight - Margins.Bottom
        then part_h:= dstrect.top + dstHeight - Margins.Bottom - y;

        GdipDrawImageRectRectI(dst, src,
          x, y, part_w, part_h,
          margins.left, margins.top, part_w, part_h,
          UnitPixel, hattr, nil, nil);

        inc(y, srcHeight - Margins.Top - Margins.Bottom);
      end;

      inc(x, srcWidth - Margins.Left - Margins.Right);
    end;

    // left and right line //
    if (margins.left > 0) or (margins.right > 0) then
    begin
      y:= Margins.Top + dstrect.top;
      while y <= dstrect.top + dstHeight - Margins.Bottom do
      begin
        part_h:= srcheight - margins.top - margins.bottom;
        if y + part_h > dstrect.top + dstHeight - Margins.Bottom
        then part_h:= dstrect.top + dstHeight - Margins.Bottom - y;

        if margins.left > 0 then
        GdipDrawImageRectRectI(dst, src,
          dstrect.left, y, margins.left, part_h,
          0, margins.top, margins.left, part_h,
          UnitPixel, hattr, nil, nil);

        if margins.right > 0 then
        GdipDrawImageRectRectI(dst, src,
          dstwidth - margins.right + dstrect.left, y, margins.right, part_h,
          srcwidth - margins.right, margins.top, margins.right, part_h,
          UnitPixel, hattr, nil, nil);

        inc(y, srcHeight - Margins.Top - Margins.Bottom);
      end;
    end;

    // top and bottom line //
    if (margins.top > 0) or (margins.bottom > 0) then
    begin
      x:= Margins.Left + dstrect.left;
      while x <= dstrect.left + dstWidth - Margins.Right do
      begin
        part_w:= srcwidth - margins.left - margins.right;
        if x + part_w > dstrect.left + dstWidth - Margins.right
        then part_w:= dstrect.left + dstWidth - Margins.right - x;

        if margins.top > 0 then
        GdipDrawImageRectRectI(dst, src,
          x, dstrect.top, part_w, margins.top,
          margins.left, 0, part_w, margins.top,
          UnitPixel, hattr, nil, nil);

        if margins.bottom > 0 then
        GdipDrawImageRectRectI(dst, src,
          x, dstheight - margins.bottom + dstrect.top, part_w, margins.bottom,
          margins.left, srcheight - margins.bottom, part_w, margins.bottom,
          UnitPixel, hattr, nil, nil);

        inc(x, srcWidth - Margins.left - Margins.right);
      end;
    end;

  end;

  except
  end;

  // end of center area //

  // top-left corner //
  try
    if (margins.top > 0) and (margins.left > 0) then
      GdipDrawImageRectRectI(dst, src,
      dstrect.left, dstrect.top, margins.left, margins.top,
      0, 0, margins.left, margins.top, UnitPixel, hattr, nil, nil);
  except
  end;

  // top-right corner //
  try
    if (margins.top > 0) and (margins.right > 0) then
      GdipDrawImageRectRectI(dst, src,
      dstwidth - margins.right + dstrect.left, dstrect.top,
      margins.right, margins.top,
      srcwidth - margins.right, 0, margins.right, margins.top,
      UnitPixel, hattr, nil, nil);
  except
  end;

  // bottom-left corner //
  try
    if (margins.bottom > 0) and (margins.left > 0) then
      GdipDrawImageRectRectI(dst, src,
      dstrect.left, dstheight - margins.bottom + dstrect.top,
      margins.left, margins.bottom,
      0, srcheight - margins.bottom, margins.left, margins.bottom,
      UnitPixel, hattr, nil, nil);
  except
  end;

  // bottom-right corner //
  try
    if (margins.bottom > 0) and (margins.right > 0) then
      GdipDrawImageRectRectI(dst, src,
      dstwidth - margins.right + dstrect.left,
      dstheight - margins.bottom + dstrect.top,
      margins.right, margins.bottom,
      srcwidth - margins.right, srcheight - margins.bottom,
      margins.right, margins.bottom,
      UnitPixel, hattr, nil, nil);
  except
  end;

  if DEFAULT_COLOR_DATA <> color_data then GdipDisposeImageAttributes(hattr);
end;
//------------------------------------------------------------------------------
function MeasureString(str: string; hfont: Pointer): TRectF;
var
  dc: HDC;
  hgdip: Pointer;
begin
  result.x:= 0;
  result.y:= 0;
  result.width:= 0;
  result.height:= 0;
  dc:= CreateCompatibleDC(0);
  if dc = 0 then exit;
  GdipCreateFromHDC(dc, hgdip);
  GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
  GdipMeasureString(hgdip, PWideChar(WideString(str)), -1, hfont, @result, nil, @result, nil, nil);
  GdipDeleteGraphics(hgdip);
  DeleteDC(dc);
end;
//--------------------------------------------------------------------------------------------------
procedure UpdateLWindow(hWnd: THandle; bmp: _SimpleBitmap; SrcAlpha: integer = 255);
var
  Blend: TBlendFunction;
  TopLeft: windows.TPoint;
  Size: windows.TSize;
begin
  TopLeft:= Point(0, 0);
  size.cx:= bmp.width;
  size.cy:= bmp.height;
  Blend.BlendOp:= AC_SRC_OVER;
  Blend.BlendFlags:= 0;
  Blend.SourceConstantAlpha:= SrcAlpha;
  Blend.AlphaFormat:= AC_SRC_ALPHA;
  UpdateLayeredWindow(hWnd, 0, @bmp.topleft, @Size, bmp.dc, @TopLeft, $1fffffff, @Blend, ULW_ALPHA);
end;
//--------------------------------------------------------------------------------------------------
procedure UpdateLWindowContent(hWnd: THandle; bmp: _SimpleBitmap; SrcAlpha: integer = 255);
var
  Blend: TBlendFunction;
  TopLeft: windows.TPoint;
  Size: windows.TSize;
begin
  TopLeft:= Point(0, 0);
  size.cx:= bmp.width;
  size.cy:= bmp.height;
  Blend.BlendOp:= AC_SRC_OVER;
  Blend.BlendFlags:= 0;
  Blend.SourceConstantAlpha:= SrcAlpha;
  Blend.AlphaFormat:= AC_SRC_ALPHA;
  UpdateLayeredWindow(hWnd, 0, nil, @Size, bmp.dc, @TopLeft, $1fffffff, @Blend, ULW_ALPHA);
end;
//--------------------------------------------------------------------------------------------------
procedure UpdateLWindowPosAlpha(hWnd: THandle; x, y: integer; SrcAlpha: integer = 255);
var
  Blend: TBlendFunction;
  TopLeft: windows.TPoint;
begin
  TopLeft:= Point(x, y);
  Blend.BlendOp:= AC_SRC_OVER;
  Blend.BlendFlags:= 0;
  Blend.SourceConstantAlpha:= SrcAlpha;
  Blend.AlphaFormat:= AC_SRC_ALPHA;
  UpdateLayeredWindow(hWnd, 0, @topleft, nil, 0, nil, $1fffffff, @Blend, ULW_ALPHA);
end;
//--------------------------------------------------------------------------------------------------
procedure UpdateLWindowPos(hWnd: THandle; x, y: integer);
var
  TopLeft: windows.TPoint;
begin
  TopLeft:= Point(x, y);
  UpdateLayeredWindow(hWnd, 0, @topleft, 0, 0, 0, 0, 0, 0);
end;
//------------------------------------------------------------------------------
procedure LoadImageFromPIDL(pidl: PItemIDList; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
var
  sfi: TSHFileInfoA;
  hil: HImageList;
begin
  try if image <> nil then GdipDisposeImage(image);
  except end;
  image := nil;

  if not Assigned(pidl) then exit;

  hil := SHGetFileInfoA(pchar(pidl), -1, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SHELLICONSIZE);
  if hil > 32 then
  begin
    image := IconToGdipBitmap(ImageList_GetIcon(hil, sfi.iIcon, 0));
  end else begin
    image := IconToGdipBitmap(sfi.hIcon);
  end;
  try DestroyIcon(sfi.hIcon);
  except end;
  DownscaleImage(image, MaxSize, srcwidth, srcheight, true);
end;
//------------------------------------------------------------------------------
procedure LoadImageFromHWnd(h: THandle; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint; timeout: uint);
const
  ICON_SMALL2 = PtrUInt(2);
var
  hIcon: THandle;
begin
  try if image <> nil then GdipDisposeImage(image);
  except end;
  image := nil;

  if not IsWindow(h) then exit;
  SendMessageTimeout(h, WM_GETICON, ICON_BIG, 0, SMTO_ABORTIFHUNG + SMTO_BLOCK, timeout, hIcon);
  if hIcon = THandle(0) then SendMessageTimeout(h, WM_GETICON, ICON_SMALL2, 0, SMTO_ABORTIFHUNG + SMTO_BLOCK, timeout, hIcon);
  if hIcon = THandle(0) then hIcon := GetClassLongPtr(h, GCL_HICON);
  if (hIcon = THandle(0)) and default then hIcon := windows.LoadIcon(0, IDI_APPLICATION);
  if hIcon <> THandle(0) then
  begin
    image := IconToGdipBitmap(hIcon);
    DownscaleImage(image, MaxSize, srcwidth, srcheight, true);
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure LoadImage(imagefile: string; MaxSize: integer; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
var
  idx: word;
  icoIndex: uint;
  ext: string;
  fstream: TFileStream;
  stream: IStream;
  isFullyQalified: boolean;
begin
  try if image <> nil then GdipDisposeImage(image);
  except end;
  image := nil;

  try
    icoIndex := 0;
    try idx := strtoint(cutafterlast(imagefile, ','));
    except end;
    imagefile := UnzipPath(cut(imagefile, ','));
    isFullyQalified:= false;
    if length(imagefile) > 3 then
      if (imagefile[2] = ':') and (imagefile[3] = '\') then
        isFullyQalified := true;

    if not isFullyQalified or (not fileexists(imagefile) and not directoryexists(imagefile)) then
    begin
      if default then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\default.png'))), image);
    end else begin
      ext := AnsiLowerCase(ExtractFileExt(imagefile));
      try
        if (ext = '.png') or (ext = '.gif') then
        begin
          GdipLoadImageFromFile(PWideChar(WideString(cut(imagefile, ','))), image);
        end
        else begin
          icoIndex := windows.ExtractAssociatedIcon(hInstance, pchar(cut(imagefile, ',')), @idx);
          image := IconToGdipBitmap(icoIndex);
          DeleteObject(icoIndex);
        end;
      except
      end;
    end;

    DownscaleImage(image, MaxSize, srcwidth, srcheight, true);
  except
  end;
end;
//--------------------------------------------------------------------------------------------------
function DownscaleImage(var image: pointer; MaxSize: integer; var srcwidth, srcheight: uint; DeleteSource: boolean): boolean;
var
  imgTemp, g: pointer;
  w, h: uint;
begin
  result := false;
  srcwidth := 32;
  srcheight := 32;
  if image = nil then exit;

  try
    imgTemp:= nil;
    GdipGetImageWidth(image, w);
    GdipGetImageHeight(image, h);
    srcwidth := w;
    srcheight := h;
    // downscale image //
    if (w > MaxSize) and (w > 128) and (MaxSize <= 192) then
    begin
      imgTemp := image;
      if MaxSize <= 128 then srcwidth:= 128
      else if MaxSize <= 160 then srcwidth:= 160
      else if MaxSize <= 192 then srcwidth:= 192;
      srcheight:= srcwidth;
      GdipCreateBitmapFromScan0(srcwidth, srcheight, 0, PixelFormat32bppPARGB, nil, image);
      GdipGetImageGraphicsContext(image, g);
      GdipSetInterpolationMode(g, InterpolationModeHighQualityBicubic);
      GdipSetPixelOffsetMode(g, PixelOffsetModeHighQuality);
      GdipDrawImageRectRectI(g, imgTemp, 0, 0, srcwidth, srcheight, 0, 0, w, h, UnitPixel, nil, nil, nil);
      GdipDeleteGraphics(g);
      if DeleteSource then GdipDisposeImage(imgTemp);
      result := true;
    end;
  except
  end;
end;
//--------------------------------------------------------------------------------------------------
function IconToGDIPBitmap(AIcon: HICON): Pointer;
var
  ii: ICONINFO;
  biNew: BITMAPINFO;
  bi: BITMAPINFO;
  bmpData: PGDIPColors;
  dc: HDC;
  hMask: Pointer;
  xx, yy: Integer;
  cColor: cardinal;
  Alpha: Byte;
  bAlpha: Boolean;

function HasAlpha: Boolean;
var
  x, y: Integer;
begin
  Result := false;
  for x := 0 to biNew.bmiHeader.biHeight - 1 do
    for y := 0 to biNew.bmiHeader.biWidth - 1 do
      if bmpData[y * biNew.bmiHeader.biWidth + x].A <> 0 then
      begin
        result := true;
        exit;
      end;
end;

begin
  Result := nil;
  if GetIconInfo(AIcon, ii) then
  begin
    dc := GetDC(0);
    if dc <> 0 then
    begin
      // Get the bitmap informations
      biNew.bmiHeader.biSize := SizeOf(biNew);
      biNew.bmiHeader.biBitCount := 0;
      if 0 <> GetDIBits(dc, ii.hbmColor, 0, 0, nil, biNew, DIB_RGB_COLORS) then
      begin
        // Get the memory for the bits
        try GetMem(bmpData, biNew.bmiHeader.biWidth * biNew.bmiHeader.biHeight * SizeOf(TColor))
        except bmpData := nil;
        end;
        if bmpData <> nil then
        begin
          // Get the icon bits (pixels colors)
          bi.bmiHeader.biSize := SizeOf(bi);
          bi.bmiHeader.biWidth := biNew.bmiHeader.biWidth;
          bi.bmiHeader.biHeight := -biNew.bmiHeader.biHeight;
          bi.bmiHeader.biPlanes := 1;
          bi.bmiHeader.biBitCount := 32;
          bi.bmiHeader.biCompression := BI_RGB;
          bi.bmiHeader.biSizeImage := 0;
          bi.bmiHeader.biXPelsPerMeter := 0;
          bi.bmiHeader.biYPelsPerMeter := 0;
          bi.bmiHeader.biClrUsed := 0;
          bi.bmiHeader.biClrImportant := 0;
          if 0 <> GetDIBits(dc, ii.hbmColor, 0, biNew.bmiHeader.biHeight, bmpData, bi, DIB_RGB_COLORS) then
          begin
            try
              hMask := nil;

              try
                bAlpha := HasAlpha;
                GdipCreateBitmapFromScan0(biNew.bmiHeader.biWidth,
                  biNew.bmiHeader.biHeight, 0, PixelFormat32bppPARGB, nil, result);
                if bAlpha then
                begin
                  for yy := 0 to biNew.bmiHeader.biHeight - 1 do
                    for xx := 0 to biNew.bmiHeader.biWidth - 1 do
                      with bmpData[yy * biNew.bmiHeader.biWidth + xx] do
                        GdipBitmapSetPixel(result, xx, yy, MakeColor(A, R, G, B));
                end
                else
                begin
                  GdipCreateBitmapFromHBITMAP(ii.hbmMask, 0, hMask);
                  for yy := 0 to biNew.bmiHeader.biHeight - 1 do
                  begin
                    for xx := 0 to biNew.bmiHeader.biWidth - 1 do
                    begin
                      GdipBitmapGetPixel(hMask, xx, yy, cColor);
                      if cColor = $FFFFFFFF then Alpha := 0 else Alpha := 255;
                      with bmpData[yy * biNew.bmiHeader.biWidth + xx] do
                        GdipBitmapSetPixel(result, xx, yy, MakeColor(Alpha, R, G, B));
                    end;
                  end;
                end;
              finally
                GdipDisposeImage(hMask);
              end;

            except
              FreeAndNil(Result);
            end;
          end;
          FreeMem(bmpData);
        end;
      end;
      ReleaseDC(0, dc);
    end;
    DeleteObject(ii.hbmMask);
    DeleteObject(ii.hbmColor);
  end;
end;
//------------------------------------------------------------------------------
procedure LoadGdipImageFromResource(res_name: pchar; var image: Pointer);
var
  rs: TResourceStream;
  s: IStream;
begin
  image := nil;
  rs := TResourceStream.Create(hInstance, res_name, RT_RCDATA);
  s := TStreamAdapter.Create(rs, soReference) as IStream;
  GdipCreateBitmapFromStream(s, image);
  rs.free;
end;
//--------------------------------------------------------------------------------------------------
procedure CopyFontData(var fFrom: _FontData; var fTo: _FontData);
begin
  strcopy(@fTo.name, @fFrom.name);
  fTo.size:=          fFrom.size;
  fTo.color:=         fFrom.color;
  fTo.color_outline:= fFrom.color_outline;
  fTo.bold:=          fFrom.bold;
  fTo.italic:=        fFrom.italic;
  fTo.outline:=       fFrom.outline;
end;
//--------------------------------------------------------------------------------------------------
function FontDataToString(var fData: _FontData): string;
begin
  result:= 'name=' + pchar(@fData.name) + ';' +
    'size=' + inttostr(fData.size) + ';' +
    'color=$' + inttohex(fData.color, 8) + ';' +
    'color_o=$' + inttohex(fData.color_outline, 8) + ';' +
    'bold=' + inttostr(integer(fData.bold)) + ';' +
    'italic=' + inttostr(integer(fData.italic)) + ';' +
    'outline=' + inttostr(integer(fData.outline)) + ';';
end;
//--------------------------------------------------------------------------------------------------
procedure StringToFontData(str: string; var font: _FontData);
begin
  try strcopy(font.name, pchar(FetchValue(str, 'name=', ';')));
  except end;
  try font.size:= strtoint(FetchValue(str, 'size=', ';'));
  except end;
  try font.color:= StringToColor(FetchValue(str, 'color=', ';'));
  except end;
  try font.color_outline:= StringToColor(FetchValue(str, 'color_o=', ';'));
  except end;
  try font.bold:= boolean(strtoint(FetchValue(str, 'bold=', ';')));
  except end;
  try font.italic:= boolean(strtoint(FetchValue(str, 'italic=', ';')));
  except end;
  try font.outline:= boolean(strtoint(FetchValue(str, 'outline=', ';')));
  except end;
end;
//------------------------------------------------------------------------------
function SwapColor(color: uint): uint;
begin
  result:= color and $ff000000 + color shr 16 and $ff + color and $ff00 + color and $ff shl 16;
end;
//------------------------------------------------------------------------------
function FadeColorTo(color, fadeto: uint; fadeby: extended): uint;
var
  r, g, b: integer;
begin
  r:= integer(fadeto shr 16 and $ff) - round((integer(fadeto shr 16 and $ff) - integer(color shr 16 and $ff)) * fadeby);
  g:= integer(fadeto shr 8 and $ff) - round((integer(fadeto shr 8 and $ff) - integer(color shr 8 and $ff)) * fadeby);
  b:= integer(fadeto and $ff) - round((integer(fadeto and $ff) - integer(color and $ff)) * fadeby);
  result:= fadeto and $ff000000 + r shl 16 + g shl 8 + b;
end;
//------------------------------------------------------------------------------
procedure RGBtoHLS(color: uint; out h, l, s: integer);
var
  cMax, cMin: integer;
  r, g, b: integer;
  Rdelta, Gdelta, Bdelta: single;
begin
  r:= GetRValue(color);
  g:= GetGValue(color);
  b:= GetBValue(color);
  cMax:= max(max(R, G), B);
  cMin:= min(min(R, G), B);
  l:= round((((cMax + cMin) * HLSMAX) + RGBMAX ) / (2 * RGBMAX));
  if cMax = cMin then
  begin
    s:= 0;
    h:= UNDEFINED;
  end else begin
    if L <= (HLSMAX / 2) then
      s:= round((((cMax - cMin) * HLSMAX) + ((cMax + cMin) / 2) ) / (cMax + cMin))
    else s:= round((((cMax - cMin) * HLSMAX) +
      ((2 * RGBMAX - cMax - cMin) / 2)) / (2 * RGBMAX - cMax - cMin));
    Rdelta:= (((cMax - R) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Gdelta:= (((cMax - G) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Bdelta:= (((cMax - B) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    if R = cMax then H:= round(Bdelta - Gdelta)
    else if G = cMax then H:= round((HLSMAX / 3) + Rdelta - Bdelta)
    else H:= round(((2 * HLSMAX) / 3) + Gdelta - Rdelta);
    if H < 0 then inc(H, HLSMAX);
    if H > HLSMAX then dec(H, HLSMAX);
  end;
  if S < 0 then S:= 0;
  if S > HLSMAX then S:= HLSMAX;
  if L < 0 then L:= 0;
  if L > HLSMAX then L:= HLSMAX;
end;
//--------------------------------------------------------------------------------------------------
function HLStoRGB(h, l, s: integer): uint;

  function HueToRGB(n1, n2, hue: single): single;
  begin
    if hue < 0 then hue:= hue + HLSMAX;
    if hue > HLSMAX then hue:= hue - HLSMAX;
    if hue < HLSMAX / 6 then result:= (n1 + (((n2 - n1) * hue + (HLSMAX / 12)) / (HLSMAX / 6)))
    else if hue < HLSMAX / 2 then result:= n2
    else if hue < (HLSMAX * 2) / 3 then
      result:= (n1 + (((n2 - n1) * (((HLSMAX * 2) / 3) - hue) + (HLSMAX / 12)) / (HLSMAX / 6)))
    else result:= n1;
  end;

var
  r, g, b, Magic1, Magic2: single;
begin
  if S = 0 then
  begin
    b:= round((L * RGBMAX) / HLSMAX);
    R:= B;
    G:= B;
  end else begin
    if L <= (HLSMAX / 2) then Magic2:= (L * (HLSMAX + S) + (HLSMAX / 2)) / HLSMAX
    else Magic2:= L + S - ((L * S) + (HLSMAX / 2)) / HLSMAX;
    Magic1:= 2 * L - Magic2;
    R:= round((HueToRGB(Magic1, Magic2, H + (HLSMAX / 3)) * RGBMAX + (HLSMAX / 2)) / HLSMAX);
    G:= round((HueToRGB(Magic1, Magic2, H) * RGBMAX + (HLSMAX / 2)) / HLSMAX);
    B:= round((HueToRGB(Magic1, Magic2, H - (HLSMAX / 3)) * RGBMAX + (HLSMAX / 2)) / HLSMAX);
  end;
  if R < 0 then R:= 0;
  if R > RGBMAX then R:= RGBMAX;
  if G < 0 then G:= 0;
  if G > RGBMAX then G:= RGBMAX;
  if B < 0 then B:= 0;
  if B > RGBMAX then B:= RGBMAX;
  result:= RGB(round(r), round(g), round(b));
end;
//------------------------------------------------------------------------------
function StringToStretchStyle(str: string): TStretchStyle;
begin
  result := ssStretch;
  if AnsiLowerCase(str) = 'tile' then result:= ssTile
end;
//------------------------------------------------------------------------------
function StretchStyleToString(ss: TStretchStyle): string;
begin
  result := 'stretch';
  if ss = ssTile then result:= 'tile';
end;
//--------------------------------------------------------------------------------------------------
initialization
begin
  StartupInput.DebugEventCallback:= nil;
  StartupInput.SuppressBackgroundThread:= false;
  StartupInput.SuppressExternalCodecs:= false;
  StartupInput.GdiplusVersion:= 1;
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;
//--------------------------------------------------------------------------------------------------
finalization
begin
  GdiplusShutdown(gdiplusToken);
end;
//--------------------------------------------------------------------------------------------------
end.
