      {******************************************************************}
      { GDI+ API                                                         }
      {                                                                  }
      { home page : http://www.progdigy.com                              }
      { email     : hgourvest@progdigy.com                               }
      {                                                                  }
      { date      : 15-02-2002                                           }
      {                                                                  }
      { The contents of this file are used with permission, subject to   }
      { the Mozilla Public License Version 1.1 (the "License"); you may  }
      { not use this file except in compliance with the License. You may }
      { obtain a copy of the License at                                  }
      { http://www.mozilla.org/MPL/MPL-1.1.html                          }
      {                                                                  }
      { Software distributed under the License is distributed on an      }
      { "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
      { implied. See the License for the specific language governing     }
      { rights and limitations under the License.                        }
      {                                                                  }
      { *****************************************************************}

unit GDIPAPI;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

(**************************************************************************\
*
*   GDI+ public header file
*
\**************************************************************************)

uses
  Windows,
  ActiveX,
  DirectDraw,
  Math;

type
  INT16   = type Smallint;
  UINT16  = type Word;
  PUINT16 = ^UINT16;
  UINT32  = type Cardinal;
  TSingleDynArray = array of Single;

(**************************************************************************\
*
*   GDI+ Private Memory Management APIs
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Memory Allocation APIs
//----------------------------------------------------------------------------

{$EXTERNALSYM GdipAlloc}
function GdipAlloc(size: ULONG): pointer; stdcall; external 'gdiplus.dll';
{$EXTERNALSYM GdipFree}
procedure GdipFree(ptr: pointer); stdcall; external 'gdiplus.dll';

(**************************************************************************\
*
*   GDI+ base memory allocation class
*
\**************************************************************************)

type
  TGdiplusBase = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

(**************************************************************************\
*
*   GDI+ Enumeration Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Default bezier flattening tolerance in device pixels.
//--------------------------------------------------------------------------

const
  {$EXTERNALSYM FlatnessDefault}
  FlatnessDefault = 0.25;

//--------------------------------------------------------------------------
// Graphics and Container State cookies
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM GraphicsState}
  GraphicsState     = UINT;
  {$EXTERNALSYM GraphicsContainer}
  GraphicsContainer = UINT;

//--------------------------------------------------------------------------
// Fill mode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM FillMode}
  FillMode = (
    FillModeAlternate,        // 0
    FillModeWinding           // 1
  );
  TFillMode = FillMode;

//--------------------------------------------------------------------------
// Quality mode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM QualityMode}
  QualityMode = (
    QualityModeInvalid   = -1,
    QualityModeDefault   =  0,
    QualityModeLow       =  1, // Best performance
    QualityModeHigh      =  2  // Best rendering quality
  );
  TQualityMode = QualityMode;

//--------------------------------------------------------------------------
// Alpha Compositing mode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM CompositingMode}
  CompositingMode = (
    CompositingModeSourceOver,    // 0
    CompositingModeSourceCopy     // 1
  );
  TCompositingMode = CompositingMode;

//--------------------------------------------------------------------------
// Alpha Compositing quality constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM CompositingQuality}
  CompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );
  TCompositingQuality = CompositingQuality;

//--------------------------------------------------------------------------
// Unit constants
//--------------------------------------------------------------------------

 // {$EXTERNALSYM Unit}
  Unit_ = (
    UnitWorld,      // 0 -- World coordinate (non-physical unit)
    UnitDisplay,    // 1 -- Variable -- for PageTransform only
    UnitPixel,      // 2 -- Each unit is one device pixel.
    UnitPoint,      // 3 -- Each unit is a printer's point, or 1/72 inch.
    UnitInch,       // 4 -- Each unit is 1 inch.
    UnitDocument,   // 5 -- Each unit is 1/300 inch.
    UnitMillimeter  // 6 -- Each unit is 1 millimeter.
  );
  TUnit = Unit_;

//--------------------------------------------------------------------------
// MetafileFrameUnit
//
// The frameRect for creating a metafile can be specified in any of these
// units.  There is an extra frame unit value (MetafileFrameUnitGdi) so
// that units can be supplied in the same units that GDI expects for
// frame rects -- these units are in .01 (1/100ths) millimeter units
// as defined by GDI.
//--------------------------------------------------------------------------

  {$EXTERNALSYM MetafileFrameUnit}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel      = ord(UnitPixel),
    MetafileFrameUnitPoint      = ord(UnitPoint),
    MetafileFrameUnitInch       = ord(UnitInch),
    MetafileFrameUnitDocument   = ord(UnitDocument),
    MetafileFrameUnitMillimeter = ord(UnitMillimeter),
    MetafileFrameUnitGdi        // GDI compatible .01 MM units
  );
  TMetafileFrameUnit = MetafileFrameUnit;

//--------------------------------------------------------------------------
// Coordinate space identifiers
//--------------------------------------------------------------------------

  {$EXTERNALSYM CoordinateSpace}
  CoordinateSpace = (
    CoordinateSpaceWorld,     // 0
    CoordinateSpacePage,      // 1
    CoordinateSpaceDevice     // 2
  );
  TCoordinateSpace = CoordinateSpace;

//--------------------------------------------------------------------------
// Various wrap modes for brushes
//--------------------------------------------------------------------------

  {$EXTERNALSYM WrapMode}
  WrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );
  TWrapMode = WrapMode;

//--------------------------------------------------------------------------
// Various hatch styles
//--------------------------------------------------------------------------

  {$EXTERNALSYM HatchStyle}
  HatchStyle = (
    HatchStyleHorizontal                   = 0,
    HatchStyleVertical                     = 1,
    HatchStyleForwardDiagonal              = 2,
    HatchStyleBackwardDiagonal             = 3,
    HatchStyleCross                        = 4,
    HatchStyleDiagonalCross                = 5,
    HatchStyle05Percent                    = 6,
    HatchStyle10Percent                    = 7,
    HatchStyle20Percent                    = 8,
    HatchStyle25Percent                    = 9,
    HatchStyle30Percent                    = 10,
    HatchStyle40Percent                    = 11,
    HatchStyle50Percent                    = 12,
    HatchStyle60Percent                    = 13,
    HatchStyle70Percent                    = 14,
    HatchStyle75Percent                    = 15,
    HatchStyle80Percent                    = 16,
    HatchStyle90Percent                    = 17,
    HatchStyleLightDownwardDiagonal        = 18,
    HatchStyleLightUpwardDiagonal          = 19,
    HatchStyleDarkDownwardDiagonal         = 20,
    HatchStyleDarkUpwardDiagonal           = 21,
    HatchStyleWideDownwardDiagonal         = 22,
    HatchStyleWideUpwardDiagonal           = 23,
    HatchStyleLightVertical                = 24,
    HatchStyleLightHorizontal              = 25,
    HatchStyleNarrowVertical               = 26,
    HatchStyleNarrowHorizontal             = 27,
    HatchStyleDarkVertical                 = 28,
    HatchStyleDarkHorizontal               = 29,
    HatchStyleDashedDownwardDiagonal       = 30,
    HatchStyleDashedUpwardDiagonal         = 31,
    HatchStyleDashedHorizontal             = 32,
    HatchStyleDashedVertical               = 33,
    HatchStyleSmallConfetti                = 34,
    HatchStyleLargeConfetti                = 35,
    HatchStyleZigZag                       = 36,
    HatchStyleWave                         = 37,
    HatchStyleDiagonalBrick                = 38,
    HatchStyleHorizontalBrick              = 39,
    HatchStyleWeave                        = 40,
    HatchStylePlaid                        = 41,
    HatchStyleDivot                        = 42,
    HatchStyleDottedGrid                   = 43,
    HatchStyleDottedDiamond                = 44,
    HatchStyleShingle                      = 45,
    HatchStyleTrellis                      = 46,
    HatchStyleSphere                       = 47,
    HatchStyleSmallGrid                    = 48,
    HatchStyleSmallCheckerBoard            = 49,
    HatchStyleLargeCheckerBoard            = 50,
    HatchStyleOutlinedDiamond              = 51,
    HatchStyleSolidDiamond                 = 52,

    HatchStyleTotal                        = 53,
    HatchStyleLargeGrid = HatchStyleCross, // 4

    HatchStyleMin       = HatchStyleHorizontal,
    HatchStyleMax       = HatchStyleTotal - 1
  );
  THatchStyle = HatchStyle;

//--------------------------------------------------------------------------
// Dash style constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM DashStyle}
  DashStyle = (
    DashStyleSolid,          // 0
    DashStyleDash,           // 1
    DashStyleDot,            // 2
    DashStyleDashDot,        // 3
    DashStyleDashDotDot,     // 4
    DashStyleCustom          // 5
  );
  TDashStyle = DashStyle;

//--------------------------------------------------------------------------
// Dash cap constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM DashCap}
  DashCap = (
    DashCapFlat             = 0,
    DashCapRound            = 2,
    DashCapTriangle         = 3
  );
  TDashCap = DashCap;

//--------------------------------------------------------------------------
// Line cap constants (only the lowest 8 bits are used).
//--------------------------------------------------------------------------

  {$EXTERNALSYM LineCap}
  LineCap = (
    LineCapFlat             = 0,
    LineCapSquare           = 1,
    LineCapRound            = 2,
    LineCapTriangle         = 3,

    LineCapNoAnchor         = $10, // corresponds to flat cap
    LineCapSquareAnchor     = $11, // corresponds to square cap
    LineCapRoundAnchor      = $12, // corresponds to round cap
    LineCapDiamondAnchor    = $13, // corresponds to triangle cap
    LineCapArrowAnchor      = $14, // no correspondence

    LineCapCustom           = $ff, // custom cap

    LineCapAnchorMask       = $f0  // mask to check for anchor or not.
  );
  TLineCap = LineCap;

//--------------------------------------------------------------------------
// Custom Line cap type constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM CustomLineCapType}
  CustomLineCapType = (
    CustomLineCapTypeDefault         = 0,
    CustomLineCapTypeAdjustableArrow = 1
  );
  TCustomLineCapType = CustomLineCapType;

//--------------------------------------------------------------------------
// Line join constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM LineJoin}
  LineJoin = (
    LineJoinMiter        = 0,
    LineJoinBevel        = 1,
    LineJoinRound        = 2,
    LineJoinMiterClipped = 3
  );
  TLineJoin = LineJoin;

//--------------------------------------------------------------------------
// Path point types (only the lowest 8 bits are used.)
//  The lowest 3 bits are interpreted as point type
//  The higher 5 bits are reserved for flags.
//--------------------------------------------------------------------------
  {$Z1}
  {$EXTERNALSYM PathPointType}
  PathPointType = (
    PathPointTypeStart           = $00, // move
    PathPointTypeLine            = $01, // line
    PathPointTypeBezier          = $03, // default Bezier (= cubic Bezier)
    PathPointTypePathTypeMask    = $07, // type mask (lowest 3 bits).
    PathPointTypeDashMode        = $10, // currently in dash mode.
    PathPointTypePathMarker      = $20, // a marker for the path.
    PathPointTypeCloseSubpath    = $80, // closed flag

    // Path types used for advanced path.
    PathPointTypeBezier3         = $03  // cubic Bezier
  );
  TPathPointType = PathPointType;
  {$Z4}

//--------------------------------------------------------------------------
// WarpMode constants
//--------------------------------------------------------------------------

  {$EXTERNALSYM WarpMode}
  WarpMode = (
    WarpModePerspective,    // 0
    WarpModeBilinear        // 1
  );
  TWarpMode = WarpMode;

//--------------------------------------------------------------------------
// LineGradient Mode
//--------------------------------------------------------------------------

  {$EXTERNALSYM LinearGradientMode}
  LinearGradientMode = (
    LinearGradientModeHorizontal,         // 0
    LinearGradientModeVertical,           // 1
    LinearGradientModeForwardDiagonal,    // 2
    LinearGradientModeBackwardDiagonal    // 3
  );
  TLinearGradientMode = LinearGradientMode;

//--------------------------------------------------------------------------
// Region Comine Modes
//--------------------------------------------------------------------------

  {$EXTERNALSYM CombineMode}
  CombineMode = (
    CombineModeReplace,     // 0
    CombineModeIntersect,   // 1
    CombineModeUnion,       // 2
    CombineModeXor,         // 3
    CombineModeExclude,     // 4
    CombineModeComplement   // 5 (Exclude From)
  );
  TCombineMode = CombineMode;

//--------------------------------------------------------------------------
 // Image types
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageType}
  ImageType = (
    ImageTypeUnknown,   // 0
    ImageTypeBitmap,    // 1
    ImageTypeMetafile   // 2
  );
  TImageType = ImageType;

//--------------------------------------------------------------------------
// Interpolation modes
//--------------------------------------------------------------------------

  {$EXTERNALSYM InterpolationMode}
  InterpolationMode = (
    InterpolationModeInvalid          = ord(QualityModeInvalid),
    InterpolationModeDefault          = ord(QualityModeDefault),
    InterpolationModeLowQuality       = ord(QualityModeLow),
    InterpolationModeHighQuality      = ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );
  TInterpolationMode = InterpolationMode;

//--------------------------------------------------------------------------
// Pen types
//--------------------------------------------------------------------------

  {$EXTERNALSYM PenAlignment}
  PenAlignment = (
    PenAlignmentCenter       = 0,
    PenAlignmentInset        = 1
  );
  TPenAlignment = PenAlignment;

//--------------------------------------------------------------------------
// Brush types
//--------------------------------------------------------------------------

  {$EXTERNALSYM BrushType}
  BrushType = (
   BrushTypeSolidColor       = 0,
   BrushTypeHatchFill        = 1,
   BrushTypeTextureFill      = 2,
   BrushTypePathGradient     = 3,
   BrushTypeLinearGradient   = 4
  );
  TBrushType = BrushType;

//--------------------------------------------------------------------------
// Pen's Fill types
//--------------------------------------------------------------------------

  {$EXTERNALSYM PenType}
  PenType = (
   PenTypeSolidColor       =  ord(BrushTypeSolidColor),
   PenTypeHatchFill        =  ord(BrushTypeHatchFill),
   PenTypeTextureFill      =  ord(BrushTypeTextureFill),
   PenTypePathGradient     =  ord(BrushTypePathGradient),
   PenTypeLinearGradient   =  ord(BrushTypeLinearGradient),
   PenTypeUnknown          = -1
  );
  TPenType = PenType;

//--------------------------------------------------------------------------
// Matrix Order
//--------------------------------------------------------------------------

  {$EXTERNALSYM MatrixOrder}
  MatrixOrder = (
    MatrixOrderPrepend    = 0,
    MatrixOrderAppend     = 1
  );
  TMatrixOrder = MatrixOrder;

//--------------------------------------------------------------------------
// Generic font families
//--------------------------------------------------------------------------

  {$EXTERNALSYM GenericFontFamily}
  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace
  );
  TGenericFontFamily = GenericFontFamily;

//--------------------------------------------------------------------------
// FontStyle: face types and common styles
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM FontStyle}
  FontStyle = Integer;
  const
    FontStyleRegular    = Integer(0);
    FontStyleBold       = Integer(1);
    FontStyleItalic     = Integer(2);
    FontStyleBoldItalic = Integer(3);
    FontStyleUnderline  = Integer(4);
    FontStyleStrikeout  = Integer(8);
  Type
  TFontStyle = FontStyle;

//---------------------------------------------------------------------------
// Smoothing Mode
//---------------------------------------------------------------------------

  {$EXTERNALSYM SmoothingMode}
  SmoothingMode = (
    SmoothingModeInvalid     = ord(QualityModeInvalid),
    SmoothingModeDefault     = ord(QualityModeDefault),
    SmoothingModeHighSpeed   = ord(QualityModeLow),
    SmoothingModeHighQuality = ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias
  );
  TSmoothingMode = SmoothingMode;

//---------------------------------------------------------------------------
// Pixel Format Mode
//---------------------------------------------------------------------------

  {$EXTERNALSYM PixelOffsetMode}
  PixelOffsetMode = (
    PixelOffsetModeInvalid     = Ord(QualityModeInvalid),
    PixelOffsetModeDefault     = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed   = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,    // No pixel offset
    PixelOffsetModeHalf     // Offset by -0.5, -0.5 for fast anti-alias perf
  );
  TPixelOffsetMode = PixelOffsetMode;

//---------------------------------------------------------------------------
// Text Rendering Hint
//---------------------------------------------------------------------------

  {$EXTERNALSYM TextRenderingHint}
  TextRenderingHint = (
    TextRenderingHintSystemDefault = 0,            // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );
  TTextRenderingHint = TextRenderingHint;

//---------------------------------------------------------------------------
// Metafile Types
//---------------------------------------------------------------------------

  {$EXTERNALSYM MetafileType}
  MetafileType = (
    MetafileTypeInvalid,            // Invalid metafile
    MetafileTypeWmf,                // Standard WMF
    MetafileTypeWmfPlaceable,       // Placeable WMF
    MetafileTypeEmf,                // EMF (not EMF+)
    MetafileTypeEmfPlusOnly,        // EMF+ without dual, down-level records
    MetafileTypeEmfPlusDual         // EMF+ with dual, down-level records
  );
  TMetafileType = MetafileType;

//---------------------------------------------------------------------------
// Specifies the type of EMF to record
//---------------------------------------------------------------------------

  {$EXTERNALSYM EmfType}
  EmfType = (
    EmfTypeEmfOnly     = Ord(MetafileTypeEmf),          // no EMF+, only EMF
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),  // no EMF, only EMF+
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual)   // both EMF+ and EMF
  );
  TEmfType = EmfType;

//---------------------------------------------------------------------------
// EMF+ Persistent object types
//---------------------------------------------------------------------------

  {$EXTERNALSYM ObjectType}
  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap,

    ObjectTypeMax = ObjectTypeCustomLineCap,
    ObjectTypeMin = ObjectTypeBrush
  );
  TObjectType = ObjectType;

function ObjectTypeIsValid(type_: ObjectType): BOOL;

//---------------------------------------------------------------------------
// EMF+ Records
//---------------------------------------------------------------------------

  // We have to change the WMF record numbers so that they don't conflict with
  // the EMF and EMF+ record numbers.

const
  GDIP_EMFPLUS_RECORD_BASE      = $00004000;
  {$EXTERNALSYM GDIP_EMFPLUS_RECORD_BASE}
  GDIP_WMF_RECORD_BASE          = $00010000;
  {$EXTERNALSYM GDIP_WMF_RECORD_BASE}

// macros
function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;

const
// Metafile Functions

  META_SETBKCOLOR            = $0201;
  {$EXTERNALSYM META_SETBKCOLOR}
  META_SETBKMODE             = $0102;
  {$EXTERNALSYM META_SETBKMODE}
  META_SETMAPMODE            = $0103;
  {$EXTERNALSYM META_SETMAPMODE}
  META_SETROP2               = $0104;
  {$EXTERNALSYM META_SETROP2}
  META_SETRELABS             = $0105;
  {$EXTERNALSYM META_SETRELABS}
  META_SETPOLYFILLMODE       = $0106;
  {$EXTERNALSYM META_SETPOLYFILLMODE}
  META_SETSTRETCHBLTMODE     = $0107;
  {$EXTERNALSYM META_SETSTRETCHBLTMODE}
  META_SETTEXTCHAREXTRA      = $0108;
  {$EXTERNALSYM META_SETTEXTCHAREXTRA}
  META_SETTEXTCOLOR          = $0209;
  {$EXTERNALSYM META_SETTEXTCOLOR}
  META_SETTEXTJUSTIFICATION  = $020A;
  {$EXTERNALSYM META_SETTEXTJUSTIFICATION}
  META_SETWINDOWORG          = $020B;
  {$EXTERNALSYM META_SETWINDOWORG}
  META_SETWINDOWEXT          = $020C;
  {$EXTERNALSYM META_SETWINDOWEXT}
  META_SETVIEWPORTORG        = $020D;
  {$EXTERNALSYM META_SETVIEWPORTORG}
  META_SETVIEWPORTEXT        = $020E;
  {$EXTERNALSYM META_SETVIEWPORTEXT}
  META_OFFSETWINDOWORG       = $020F;
  {$EXTERNALSYM META_OFFSETWINDOWORG}
  META_SCALEWINDOWEXT        = $0410;
  {$EXTERNALSYM META_SCALEWINDOWEXT}
  META_OFFSETVIEWPORTORG     = $0211;
  {$EXTERNALSYM META_OFFSETVIEWPORTORG}
  META_SCALEVIEWPORTEXT      = $0412;
  {$EXTERNALSYM META_SCALEVIEWPORTEXT}
  META_LINETO                = $0213;
  {$EXTERNALSYM META_LINETO}
  META_MOVETO                = $0214;
  {$EXTERNALSYM META_MOVETO}
  META_EXCLUDECLIPRECT       = $0415;
  {$EXTERNALSYM META_EXCLUDECLIPRECT}
  META_INTERSECTCLIPRECT     = $0416;
  {$EXTERNALSYM META_INTERSECTCLIPRECT}
  META_ARC                   = $0817;
  {$EXTERNALSYM META_ARC}
  META_ELLIPSE               = $0418;
  {$EXTERNALSYM META_ELLIPSE}
  META_FLOODFILL             = $0419;
  {$EXTERNALSYM META_FLOODFILL}
  META_PIE                   = $081A;
  {$EXTERNALSYM META_PIE}
  META_RECTANGLE             = $041B;
  {$EXTERNALSYM META_RECTANGLE}
  META_ROUNDRECT             = $061C;
  {$EXTERNALSYM META_ROUNDRECT}
  META_PATBLT                = $061D;
  {$EXTERNALSYM META_PATBLT}
  META_SAVEDC                = $001E;
  {$EXTERNALSYM META_SAVEDC}
  META_SETPIXEL              = $041F;
  {$EXTERNALSYM META_SETPIXEL}
  META_OFFSETCLIPRGN         = $0220;
  {$EXTERNALSYM META_OFFSETCLIPRGN}
  META_TEXTOUT               = $0521;
  {$EXTERNALSYM META_TEXTOUT}
  META_BITBLT                = $0922;
  {$EXTERNALSYM META_BITBLT}
  META_STRETCHBLT            = $0B23;
  {$EXTERNALSYM META_STRETCHBLT}
  META_POLYGON               = $0324;
  {$EXTERNALSYM META_POLYGON}
  META_POLYLINE              = $0325;
  {$EXTERNALSYM META_POLYLINE}
  META_ESCAPE                = $0626;
  {$EXTERNALSYM META_ESCAPE}
  META_RESTOREDC             = $0127;
  {$EXTERNALSYM META_RESTOREDC}
  META_FILLREGION            = $0228;
  {$EXTERNALSYM META_FILLREGION}
  META_FRAMEREGION           = $0429;
  {$EXTERNALSYM META_FRAMEREGION}
  META_INVERTREGION          = $012A;
  {$EXTERNALSYM META_INVERTREGION}
  META_PAINTREGION           = $012B;
  {$EXTERNALSYM META_PAINTREGION}
  META_SELECTCLIPREGION      = $012C;
  {$EXTERNALSYM META_SELECTCLIPREGION}
  META_SELECTOBJECT          = $012D;
  {$EXTERNALSYM META_SELECTOBJECT}
  META_SETTEXTALIGN          = $012E;
  {$EXTERNALSYM META_SETTEXTALIGN}
  META_CHORD                 = $0830;
  {$EXTERNALSYM META_CHORD}
  META_SETMAPPERFLAGS        = $0231;
  {$EXTERNALSYM META_SETMAPPERFLAGS}
  META_EXTTEXTOUT            = $0a32;
  {$EXTERNALSYM META_EXTTEXTOUT}
  META_SETDIBTODEV           = $0d33;
  {$EXTERNALSYM META_SETDIBTODEV}
  META_SELECTPALETTE         = $0234;
  {$EXTERNALSYM META_SELECTPALETTE}
  META_REALIZEPALETTE        = $0035;
  {$EXTERNALSYM META_REALIZEPALETTE}
  META_ANIMATEPALETTE        = $0436;
  {$EXTERNALSYM META_ANIMATEPALETTE}
  META_SETPALENTRIES         = $0037;
  {$EXTERNALSYM META_SETPALENTRIES}
  META_POLYPOLYGON           = $0538;
  {$EXTERNALSYM META_POLYPOLYGON}
  META_RESIZEPALETTE         = $0139;
  {$EXTERNALSYM META_RESIZEPALETTE}
  META_DIBBITBLT             = $0940;
  {$EXTERNALSYM META_DIBBITBLT}
  META_DIBSTRETCHBLT         = $0b41;
  {$EXTERNALSYM META_DIBSTRETCHBLT}
  META_DIBCREATEPATTERNBRUSH = $0142;
  {$EXTERNALSYM META_DIBCREATEPATTERNBRUSH}
  META_STRETCHDIB            = $0f43;
  {$EXTERNALSYM META_STRETCHDIB}
  META_EXTFLOODFILL          = $0548;
  {$EXTERNALSYM META_EXTFLOODFILL}
  META_SETLAYOUT             = $0149;
  {$EXTERNALSYM META_SETLAYOUT}
  META_DELETEOBJECT          = $01f0;
  {$EXTERNALSYM META_DELETEOBJECT}
  META_CREATEPALETTE         = $00f7;
  {$EXTERNALSYM META_CREATEPALETTE}
  META_CREATEPATTERNBRUSH    = $01F9;
  {$EXTERNALSYM META_CREATEPATTERNBRUSH}
  META_CREATEPENINDIRECT     = $02FA;
  {$EXTERNALSYM META_CREATEPENINDIRECT}
  META_CREATEFONTINDIRECT    = $02FB;
  {$EXTERNALSYM META_CREATEFONTINDIRECT}
  META_CREATEBRUSHINDIRECT   = $02FC;
  {$EXTERNALSYM META_CREATEBRUSHINDIRECT}
  META_CREATEREGION          = $06FF;
  {$EXTERNALSYM META_CREATEREGION}

  // Enhanced metafile record types.

  EMR_HEADER               = 1;
  {$EXTERNALSYM EMR_HEADER}
  EMR_POLYBEZIER           = 2;
  {$EXTERNALSYM EMR_POLYBEZIER}
  EMR_POLYGON              = 3;
  {$EXTERNALSYM EMR_POLYGON}
  EMR_POLYLINE             = 4;
  {$EXTERNALSYM EMR_POLYLINE}
  EMR_POLYBEZIERTO         = 5;
  {$EXTERNALSYM EMR_POLYBEZIERTO}
  EMR_POLYLINETO           = 6;
  {$EXTERNALSYM EMR_POLYLINETO}
  EMR_POLYPOLYLINE         = 7;
  {$EXTERNALSYM EMR_POLYPOLYLINE}
  EMR_POLYPOLYGON          = 8;
  {$EXTERNALSYM EMR_POLYPOLYGON}
  EMR_SETWINDOWEXTEX       = 9;
  {$EXTERNALSYM EMR_SETWINDOWEXTEX}
  EMR_SETWINDOWORGEX       = 10;
  {$EXTERNALSYM EMR_SETWINDOWORGEX}
  EMR_SETVIEWPORTEXTEX     = 11;
  {$EXTERNALSYM EMR_SETVIEWPORTEXTEX}
  EMR_SETVIEWPORTORGEX     = 12;
  {$EXTERNALSYM EMR_SETVIEWPORTORGEX}
  EMR_SETBRUSHORGEX        = 13;
  {$EXTERNALSYM EMR_SETBRUSHORGEX}
  EMR_EOF                  = 14;
  {$EXTERNALSYM EMR_EOF}
  EMR_SETPIXELV            = 15;
  {$EXTERNALSYM EMR_SETPIXELV}
  EMR_SETMAPPERFLAGS       = 16;
  {$EXTERNALSYM EMR_SETMAPPERFLAGS}
  EMR_SETMAPMODE           = 17;
  {$EXTERNALSYM EMR_SETMAPMODE}
  EMR_SETBKMODE            = 18;
  {$EXTERNALSYM EMR_SETBKMODE}
  EMR_SETPOLYFILLMODE      = 19;
  {$EXTERNALSYM EMR_SETPOLYFILLMODE}
  EMR_SETROP2              = 20;
  {$EXTERNALSYM EMR_SETROP2}
  EMR_SETSTRETCHBLTMODE    = 21;
  {$EXTERNALSYM EMR_SETSTRETCHBLTMODE}
  EMR_SETTEXTALIGN         = 22;
  {$EXTERNALSYM EMR_SETTEXTALIGN}
  EMR_SETCOLORADJUSTMENT   = 23;
  {$EXTERNALSYM EMR_SETCOLORADJUSTMENT}
  EMR_SETTEXTCOLOR         = 24;
  {$EXTERNALSYM EMR_SETTEXTCOLOR}
  EMR_SETBKCOLOR           = 25;
  {$EXTERNALSYM EMR_SETBKCOLOR}
  EMR_OFFSETCLIPRGN        = 26;
  {$EXTERNALSYM EMR_OFFSETCLIPRGN}
  EMR_MOVETOEX             = 27;
  {$EXTERNALSYM EMR_MOVETOEX}
  EMR_SETMETARGN           = 28;
  {$EXTERNALSYM EMR_SETMETARGN}
  EMR_EXCLUDECLIPRECT      = 29;
  {$EXTERNALSYM EMR_EXCLUDECLIPRECT}
  EMR_INTERSECTCLIPRECT    = 30;
  {$EXTERNALSYM EMR_INTERSECTCLIPRECT}
  EMR_SCALEVIEWPORTEXTEX   = 31;
  {$EXTERNALSYM EMR_SCALEVIEWPORTEXTEX}
  EMR_SCALEWINDOWEXTEX     = 32;
  {$EXTERNALSYM EMR_SCALEWINDOWEXTEX}
  EMR_SAVEDC               = 33;
  {$EXTERNALSYM EMR_SAVEDC}
  EMR_RESTOREDC            = 34;
  {$EXTERNALSYM EMR_RESTOREDC}
  EMR_SETWORLDTRANSFORM    = 35;
  {$EXTERNALSYM EMR_SETWORLDTRANSFORM}
  EMR_MODIFYWORLDTRANSFORM = 36;
  {$EXTERNALSYM EMR_MODIFYWORLDTRANSFORM}
  EMR_SELECTOBJECT         = 37;
  {$EXTERNALSYM EMR_SELECTOBJECT}
  EMR_CREATEPEN            = 38;
  {$EXTERNALSYM EMR_CREATEPEN}
  EMR_CREATEBRUSHINDIRECT  = 39;
  {$EXTERNALSYM EMR_CREATEBRUSHINDIRECT}
  EMR_DELETEOBJECT         = 40;
  {$EXTERNALSYM EMR_DELETEOBJECT}
  EMR_ANGLEARC             = 41;
  {$EXTERNALSYM EMR_ANGLEARC}
  EMR_ELLIPSE              = 42;
  {$EXTERNALSYM EMR_ELLIPSE}
  EMR_RECTANGLE            = 43;
  {$EXTERNALSYM EMR_RECTANGLE}
  EMR_ROUNDRECT            = 44;
  {$EXTERNALSYM EMR_ROUNDRECT}
  EMR_ARC                  = 45;
  {$EXTERNALSYM EMR_ARC}
  EMR_CHORD                = 46;
  {$EXTERNALSYM EMR_CHORD}
  EMR_PIE                  = 47;
  {$EXTERNALSYM EMR_PIE}
  EMR_SELECTPALETTE        = 48;
  {$EXTERNALSYM EMR_SELECTPALETTE}
  EMR_CREATEPALETTE        = 49;
  {$EXTERNALSYM EMR_CREATEPALETTE}
  EMR_SETPALETTEENTRIES    = 50;
  {$EXTERNALSYM EMR_SETPALETTEENTRIES}
  EMR_RESIZEPALETTE        = 51;
  {$EXTERNALSYM EMR_RESIZEPALETTE}
  EMR_REALIZEPALETTE       = 52;
  {$EXTERNALSYM EMR_REALIZEPALETTE}
  EMR_EXTFLOODFILL         = 53;
  {$EXTERNALSYM EMR_EXTFLOODFILL}
  EMR_LINETO               = 54;
  {$EXTERNALSYM EMR_LINETO}
  EMR_ARCTO                = 55;
  {$EXTERNALSYM EMR_ARCTO}
  EMR_POLYDRAW             = 56;
  {$EXTERNALSYM EMR_POLYDRAW}
  EMR_SETARCDIRECTION      = 57;
  {$EXTERNALSYM EMR_SETARCDIRECTION}
  EMR_SETMITERLIMIT        = 58;
  {$EXTERNALSYM EMR_SETMITERLIMIT}
  EMR_BEGINPATH            = 59;
  {$EXTERNALSYM EMR_BEGINPATH}
  EMR_ENDPATH              = 60;
  {$EXTERNALSYM EMR_ENDPATH}
  EMR_CLOSEFIGURE          = 61;
  {$EXTERNALSYM EMR_CLOSEFIGURE}
  EMR_FILLPATH             = 62;
  {$EXTERNALSYM EMR_FILLPATH}
  EMR_STROKEANDFILLPATH    = 63;
  {$EXTERNALSYM EMR_STROKEANDFILLPATH}
  EMR_STROKEPATH           = 64;
  {$EXTERNALSYM EMR_STROKEPATH}
  EMR_FLATTENPATH          = 65;
  {$EXTERNALSYM EMR_FLATTENPATH}
  EMR_WIDENPATH            = 66;
  {$EXTERNALSYM EMR_WIDENPATH}
  EMR_SELECTCLIPPATH       = 67;
  {$EXTERNALSYM EMR_SELECTCLIPPATH}
  EMR_ABORTPATH            = 68;
  {$EXTERNALSYM EMR_ABORTPATH}

  EMR_GDICOMMENT              = 70;
  {$EXTERNALSYM EMR_GDICOMMENT}
  EMR_FILLRGN                 = 71;
  {$EXTERNALSYM EMR_FILLRGN}
  EMR_FRAMERGN                = 72;
  {$EXTERNALSYM EMR_FRAMERGN}
  EMR_INVERTRGN               = 73;
  {$EXTERNALSYM EMR_INVERTRGN}
  EMR_PAINTRGN                = 74;
  {$EXTERNALSYM EMR_PAINTRGN}
  EMR_EXTSELECTCLIPRGN        = 75;
  {$EXTERNALSYM EMR_EXTSELECTCLIPRGN}
  EMR_BITBLT                  = 76;
  {$EXTERNALSYM EMR_BITBLT}
  EMR_STRETCHBLT              = 77;
  {$EXTERNALSYM EMR_STRETCHBLT}
  EMR_MASKBLT                 = 78;
  {$EXTERNALSYM EMR_MASKBLT}
  EMR_PLGBLT                  = 79;
  {$EXTERNALSYM EMR_PLGBLT}
  EMR_SETDIBITSTODEVICE       = 80;
  {$EXTERNALSYM EMR_SETDIBITSTODEVICE}
  EMR_STRETCHDIBITS           = 81;
  {$EXTERNALSYM EMR_STRETCHDIBITS}
  EMR_EXTCREATEFONTINDIRECTW  = 82;
  {$EXTERNALSYM EMR_EXTCREATEFONTINDIRECTW}
  EMR_EXTTEXTOUTA             = 83;
  {$EXTERNALSYM EMR_EXTTEXTOUTA}
  EMR_EXTTEXTOUTW             = 84;
  {$EXTERNALSYM EMR_EXTTEXTOUTW}
  EMR_POLYBEZIER16            = 85;
  {$EXTERNALSYM EMR_POLYBEZIER16}
  EMR_POLYGON16               = 86;
  {$EXTERNALSYM EMR_POLYGON16}
  EMR_POLYLINE16              = 87;
  {$EXTERNALSYM EMR_POLYLINE16}
  EMR_POLYBEZIERTO16          = 88;
  {$EXTERNALSYM EMR_POLYBEZIERTO16}
  EMR_POLYLINETO16            = 89;
  {$EXTERNALSYM EMR_POLYLINETO16}
  EMR_POLYPOLYLINE16          = 90;
  {$EXTERNALSYM EMR_POLYPOLYLINE16}
  EMR_POLYPOLYGON16           = 91;
  {$EXTERNALSYM EMR_POLYPOLYGON16}
  EMR_POLYDRAW16              = 92;
  {$EXTERNALSYM EMR_POLYDRAW16}
  EMR_CREATEMONOBRUSH         = 93;
  {$EXTERNALSYM EMR_CREATEMONOBRUSH}
  EMR_CREATEDIBPATTERNBRUSHPT = 94;
  {$EXTERNALSYM EMR_CREATEDIBPATTERNBRUSHPT}
  EMR_EXTCREATEPEN            = 95;
  {$EXTERNALSYM EMR_EXTCREATEPEN}
  EMR_POLYTEXTOUTA            = 96;
  {$EXTERNALSYM EMR_POLYTEXTOUTA}
  EMR_POLYTEXTOUTW            = 97;
  {$EXTERNALSYM EMR_POLYTEXTOUTW}

  EMR_SETICMMODE       = 98;
  {$EXTERNALSYM EMR_SETICMMODE}
  EMR_CREATECOLORSPACE = 99;
  {$EXTERNALSYM EMR_CREATECOLORSPACE}
  EMR_SETCOLORSPACE    = 100;
  {$EXTERNALSYM EMR_SETCOLORSPACE}
  EMR_DELETECOLORSPACE = 101;
  {$EXTERNALSYM EMR_DELETECOLORSPACE}
  EMR_GLSRECORD        = 102;
  {$EXTERNALSYM EMR_GLSRECORD}
  EMR_GLSBOUNDEDRECORD = 103;
  {$EXTERNALSYM EMR_GLSBOUNDEDRECORD}
  EMR_PIXELFORMAT      = 104;
  {$EXTERNALSYM EMR_PIXELFORMAT}

  EMR_RESERVED_105        = 105;
  {$EXTERNALSYM EMR_RESERVED_105}
  EMR_RESERVED_106        = 106;
  {$EXTERNALSYM EMR_RESERVED_106}
  EMR_RESERVED_107        = 107;
  {$EXTERNALSYM EMR_RESERVED_107}
  EMR_RESERVED_108        = 108;
  {$EXTERNALSYM EMR_RESERVED_108}
  EMR_RESERVED_109        = 109;
  {$EXTERNALSYM EMR_RESERVED_109}
  EMR_RESERVED_110        = 110;
  {$EXTERNALSYM EMR_RESERVED_110}
  EMR_COLORCORRECTPALETTE = 111;
  {$EXTERNALSYM EMR_COLORCORRECTPALETTE}
  EMR_SETICMPROFILEA      = 112;
  {$EXTERNALSYM EMR_SETICMPROFILEA}
  EMR_SETICMPROFILEW      = 113;
  {$EXTERNALSYM EMR_SETICMPROFILEW}
  EMR_ALPHABLEND          = 114;
  {$EXTERNALSYM EMR_ALPHABLEND}
  EMR_SETLAYOUT           = 115;
  {$EXTERNALSYM EMR_SETLAYOUT}
  EMR_TRANSPARENTBLT      = 116;
  {$EXTERNALSYM EMR_TRANSPARENTBLT}
  EMR_RESERVED_117        = 117;
  {$EXTERNALSYM EMR_RESERVED_117}
  EMR_GRADIENTFILL        = 118;
  {$EXTERNALSYM EMR_GRADIENTFILL}
  EMR_RESERVED_119        = 119;
  {$EXTERNALSYM EMR_RESERVED_119}
  EMR_RESERVED_120        = 120;
  {$EXTERNALSYM EMR_RESERVED_120}
  EMR_COLORMATCHTOTARGETW = 121;
  {$EXTERNALSYM EMR_COLORMATCHTOTARGETW}
  EMR_CREATECOLORSPACEW   = 122;
  {$EXTERNALSYM EMR_CREATECOLORSPACEW}

type
  {$EXTERNALSYM EmfPlusRecordType}
  EmfPlusRecordType = (
   // Since we have to enumerate GDI records right along with GDI+ records,
   // We list all the GDI records here so that they can be part of the
   // same enumeration type which is used in the enumeration callback.

    WmfRecordTypeSetBkColor              = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetBkMode               = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapMode              = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2                 = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs               = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode         = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode       = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra        = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor            = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification    = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg            = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt            = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg          = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt          = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg         = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt          = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg       = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt        = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo                  = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo                  = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect         = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect       = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc                     = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse                 = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill               = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie                     = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle               = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect               = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt                  = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC                  = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel                = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn           = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut                 = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt                  = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt              = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon                 = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline                = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape                  = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC               = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion              = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion             = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion            = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion             = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion        = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject            = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign            = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText                = ($062F or GDIP_WMF_RECORD_BASE),  // META_DRAWTEXT
    WmfRecordTypeChord                   = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags          = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut              = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev             = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette           = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette          = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette          = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries           = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon             = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette           = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt               = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt           = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush   = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB              = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill            = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout               = ($0149 or GDIP_WMF_RECORD_BASE),  // META_SETLAYOUT
    WmfRecordTypeResetDC                 = ($014C or GDIP_WMF_RECORD_BASE),  // META_RESETDC
    WmfRecordTypeStartDoc                = ($014D or GDIP_WMF_RECORD_BASE),  // META_STARTDOC
    WmfRecordTypeStartPage               = ($004F or GDIP_WMF_RECORD_BASE),  // META_STARTPAGE
    WmfRecordTypeEndPage                 = ($0050 or GDIP_WMF_RECORD_BASE),  // META_ENDPAGE
    WmfRecordTypeAbortDoc                = ($0052 or GDIP_WMF_RECORD_BASE),  // META_ABORTDOC
    WmfRecordTypeEndDoc                  = ($005E or GDIP_WMF_RECORD_BASE),  // META_ENDDOC
    WmfRecordTypeDeleteObject            = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette           = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush             = ($00F8 or GDIP_WMF_RECORD_BASE),  // META_CREATEBRUSH
    WmfRecordTypeCreatePatternBrush      = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect       = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect      = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect     = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect    = ($02FD or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAPINDIRECT
    WmfRecordTypeCreateBitmap            = ($06FE or GDIP_WMF_RECORD_BASE),  // META_CREATEBITMAP
    WmfRecordTypeCreateRegion            = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),

    EmfRecordTypeHeader                  = EMR_HEADER,
    EmfRecordTypePolyBezier              = EMR_POLYBEZIER,
    EmfRecordTypePolygon                 = EMR_POLYGON,
    EmfRecordTypePolyline                = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo            = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo              = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline            = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon             = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx          = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx          = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx        = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx        = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx           = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF                     = EMR_EOF,
    EmfRecordTypeSetPixelV               = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags          = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode              = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode               = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode         = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2                 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode       = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign            = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment      = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor            = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor              = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn           = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx                = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn              = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect         = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect       = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx      = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx        = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC                  = EMR_SAVEDC,
    EmfRecordTypeRestoreDC               = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform       = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform    = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject            = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen               = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect     = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject            = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc                = EMR_ANGLEARC,
    EmfRecordTypeEllipse                 = EMR_ELLIPSE,
    EmfRecordTypeRectangle               = EMR_RECTANGLE,
    EmfRecordTypeRoundRect               = EMR_ROUNDRECT,
    EmfRecordTypeArc                     = EMR_ARC,
    EmfRecordTypeChord                   = EMR_CHORD,
    EmfRecordTypePie                     = EMR_PIE,
    EmfRecordTypeSelectPalette           = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette           = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries       = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette           = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette          = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill            = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo                  = EMR_LINETO,
    EmfRecordTypeArcTo                   = EMR_ARCTO,
    EmfRecordTypePolyDraw                = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection         = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit           = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath               = EMR_BEGINPATH,
    EmfRecordTypeEndPath                 = EMR_ENDPATH,
    EmfRecordTypeCloseFigure             = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath                = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath       = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath              = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath             = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath               = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath          = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath               = EMR_ABORTPATH,
    EmfRecordTypeReserved_069            = 69,  // Not Used
    EmfRecordTypeGdiComment              = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn                 = EMR_FILLRGN,
    EmfRecordTypeFrameRgn                = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn               = EMR_INVERTRGN,
    EmfRecordTypePaintRgn                = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn        = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt                  = EMR_BITBLT,
    EmfRecordTypeStretchBlt              = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt                 = EMR_MASKBLT,
    EmfRecordTypePlgBlt                  = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice       = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits           = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect   = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA             = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW             = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16            = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16               = EMR_POLYGON16,
    EmfRecordTypePolyline16              = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16          = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16            = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16          = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16           = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16              = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush         = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen            = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA            = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW            = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode              = 98,  // EMR_SETICMMODE,
    EmfRecordTypeCreateColorSpace        = 99,  // EMR_CREATECOLORSPACE,
    EmfRecordTypeSetColorSpace           = 100, // EMR_SETCOLORSPACE,
    EmfRecordTypeDeleteColorSpace        = 101, // EMR_DELETECOLORSPACE,
    EmfRecordTypeGLSRecord               = 102, // EMR_GLSRECORD,
    EmfRecordTypeGLSBoundedRecord        = 103, // EMR_GLSBOUNDEDRECORD,
    EmfRecordTypePixelFormat             = 104, // EMR_PIXELFORMAT,
    EmfRecordTypeDrawEscape              = 105, // EMR_RESERVED_105,
    EmfRecordTypeExtEscape               = 106, // EMR_RESERVED_106,
    EmfRecordTypeStartDoc                = 107, // EMR_RESERVED_107,
    EmfRecordTypeSmallTextOut            = 108, // EMR_RESERVED_108,
    EmfRecordTypeForceUFIMapping         = 109, // EMR_RESERVED_109,
    EmfRecordTypeNamedEscape             = 110, // EMR_RESERVED_110,
    EmfRecordTypeColorCorrectPalette     = 111, // EMR_COLORCORRECTPALETTE,
    EmfRecordTypeSetICMProfileA          = 112, // EMR_SETICMPROFILEA,
    EmfRecordTypeSetICMProfileW          = 113, // EMR_SETICMPROFILEW,
    EmfRecordTypeAlphaBlend              = 114, // EMR_ALPHABLEND,
    EmfRecordTypeSetLayout               = 115, // EMR_SETLAYOUT,
    EmfRecordTypeTransparentBlt          = 116, // EMR_TRANSPARENTBLT,
    EmfRecordTypeReserved_117            = 117, // Not Used
    EmfRecordTypeGradientFill            = 118, // EMR_GRADIENTFILL,
    EmfRecordTypeSetLinkedUFIs           = 119, // EMR_RESERVED_119,
    EmfRecordTypeSetTextJustification    = 120, // EMR_RESERVED_120,
    EmfRecordTypeColorMatchToTargetW     = 121, // EMR_COLORMATCHTOTARGETW,
    EmfRecordTypeCreateColorSpaceW       = 122, // EMR_CREATECOLORSPACEW,
    EmfRecordTypeMax                     = 122,
    EmfRecordTypeMin                     = 1,

    // That is the END of the GDI EMF records.

    // Now we start the list of EMF+ records.  We leave quite
    // a bit of room here for the addition of any new GDI
    // records that may be added later.

    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,

    EmfPlusRecordTypeComment,

    EmfPlusRecordTypeGetDC,

    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,

    // For all persistent objects

    EmfPlusRecordTypeObject,

    // Drawing Records

    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,

    // Graphics State Records

    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,

    EmfPlusRecordTypeDrawDriverString,

    EmfPlusRecordTotal,

    EmfPlusRecordTypeMax = EmfPlusRecordTotal-1,
    EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader
  );
  TEmfPlusRecordType = EmfPlusRecordType;

//---------------------------------------------------------------------------
// StringFormatFlags
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// String format flags
//
//  DirectionRightToLeft          - For horizontal text, the reading order is
//                                  right to left. This value is called
//                                  the base embedding level by the Unicode
//                                  bidirectional engine.
//                                  For vertical text, columns are read from
//                                  right to left.
//                                  By default, horizontal or vertical text is
//                                  read from left to right.
//
//  DirectionVertical             - Individual lines of text are vertical. In
//                                  each line, characters progress from top to
//                                  bottom.
//                                  By default, lines of text are horizontal,
//                                  each new line below the previous line.
//
//  NoFitBlackBox                 - Allows parts of glyphs to overhang the
//                                  bounding rectangle.
//                                  By default glyphs are first aligned
//                                  inside the margines, then any glyphs which
//                                  still overhang the bounding box are
//                                  repositioned to avoid any overhang.
//                                  For example when an italic
//                                  lower case letter f in a font such as
//                                  Garamond is aligned at the far left of a
//                                  rectangle, the lower part of the f will
//                                  reach slightly further left than the left
//                                  edge of the rectangle. Setting this flag
//                                  will ensure the character aligns visually
//                                  with the lines above and below, but may
//                                  cause some pixels outside the formatting
//                                  rectangle to be clipped or painted.
//
//  DisplayFormatControl          - Causes control characters such as the
//                                  left-to-right mark to be shown in the
//                                  output with a representative glyph.
//
//  NoFontFallback                - Disables fallback to alternate fonts for
//                                  characters not supported in the requested
//                                  font. Any missing characters will be
//                                  be displayed with the fonts missing glyph,
//                                  usually an open square.
//
//  NoWrap                        - Disables wrapping of text between lines
//                                  when formatting within a rectangle.
//                                  NoWrap is implied when a point is passed
//                                  instead of a rectangle, or when the
//                                  specified rectangle has a zero line length.
//
//  NoClip                        - By default text is clipped to the
//                                  formatting rectangle. Setting NoClip
//                                  allows overhanging pixels to affect the
//                                  device outside the formatting rectangle.
//                                  Pixels at the end of the line may be
//                                  affected if the glyphs overhang their
//                                  cells, and either the NoFitBlackBox flag
//                                  has been set, or the glyph extends to far
//                                  to be fitted.
//                                  Pixels above/before the first line or
//                                  below/after the last line may be affected
//                                  if the glyphs extend beyond their cell
//                                  ascent / descent. This can occur rarely
//                                  with unusual diacritic mark combinations.

//---------------------------------------------------------------------------

  {$EXTERNALSYM StringFormatFlags}
  StringFormatFlags = Integer;
  const
    StringFormatFlagsDirectionRightToLeft        = $00000001;
    StringFormatFlagsDirectionVertical           = $00000002;
    StringFormatFlagsNoFitBlackBox               = $00000004;
    StringFormatFlagsDisplayFormatControl        = $00000020;
    StringFormatFlagsNoFontFallback              = $00000400;
    StringFormatFlagsMeasureTrailingSpaces       = $00000800;
    StringFormatFlagsNoWrap                      = $00001000;
    StringFormatFlagsLineLimit                   = $00002000;

    StringFormatFlagsNoClip                      = $00004000;

Type
  TStringFormatFlags = StringFormatFlags;

//---------------------------------------------------------------------------
// StringTrimming
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringTrimming}
  StringTrimming = (
    StringTrimmingNone              = 0,
    StringTrimmingCharacter         = 1,
    StringTrimmingWord              = 2,
    StringTrimmingEllipsisCharacter = 3,
    StringTrimmingEllipsisWord      = 4,
    StringTrimmingEllipsisPath      = 5
  );
  TStringTrimming = StringTrimming;

//---------------------------------------------------------------------------
// National language digit substitution
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringDigitSubstitute}
  StringDigitSubstitute = (
    StringDigitSubstituteUser        = 0,  // As NLS setting
    StringDigitSubstituteNone        = 1,
    StringDigitSubstituteNational    = 2,
    StringDigitSubstituteTraditional = 3
  );
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;

//---------------------------------------------------------------------------
// Hotkey prefix interpretation
//---------------------------------------------------------------------------

  {$EXTERNALSYM HotkeyPrefix}
  HotkeyPrefix = (
    HotkeyPrefixNone        = 0,
    HotkeyPrefixShow        = 1,
    HotkeyPrefixHide        = 2
  );
  THotkeyPrefix = HotkeyPrefix;

//---------------------------------------------------------------------------
// String alignment flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM StringAlignment}
  StringAlignment = (
    // Left edge for left-to-right text,
    // right for right-to-left text,
    // and top for vertical
    StringAlignmentNear   = 0,
    StringAlignmentCenter = 1,
    StringAlignmentFar    = 2
  );
  TStringAlignment = StringAlignment;

//---------------------------------------------------------------------------
// DriverStringOptions
//---------------------------------------------------------------------------

  {$EXTERNALSYM DriverStringOptions}
  DriverStringOptions = Integer;
  const
    DriverStringOptionsCmapLookup             = 1;
    DriverStringOptionsVertical               = 2;
    DriverStringOptionsRealizedAdvance        = 4;
    DriverStringOptionsLimitSubpixel          = 8;

type
  TDriverStringOptions = DriverStringOptions;

//---------------------------------------------------------------------------
// Flush Intention flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM FlushIntention}
  FlushIntention = (
    FlushIntentionFlush = 0,        // Flush all batched rendering operations
    FlushIntentionSync = 1          // Flush all batched rendering operations
                                    // and wait for them to complete
  );
  TFlushIntention = FlushIntention;

//---------------------------------------------------------------------------
// Image encoder parameter related types
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameterValueType}
  EncoderParameterValueType = Integer;
  const
    EncoderParameterValueTypeByte          : Integer = 1;    // 8-bit unsigned int
    EncoderParameterValueTypeASCII         : Integer = 2;    // 8-bit byte containing one 7-bit ASCII
                                                             // code. NULL terminated.
    EncoderParameterValueTypeShort         : Integer = 3;    // 16-bit unsigned int
    EncoderParameterValueTypeLong          : Integer = 4;    // 32-bit unsigned int
    EncoderParameterValueTypeRational      : Integer = 5;    // Two Longs. The first Long is the
                                                             // numerator, the second Long expresses the
                                                             // denomintor.
    EncoderParameterValueTypeLongRange     : Integer = 6;    // Two longs which specify a range of
                                                             // integer values. The first Long specifies
                                                             // the lower end and the second one
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
    EncoderParameterValueTypeUndefined     : Integer = 7;    // 8-bit byte that can take any value
                                                             // depending on field definition
    EncoderParameterValueTypeRationalRange : Integer = 8;    // Two Rationals. The first Rational
                                                             // specifies the lower end and the second
                                                             // specifies the higher end. All values
                                                             // are inclusive at both ends
type
  TEncoderParameterValueType = EncoderParameterValueType;

//---------------------------------------------------------------------------
// Image encoder value types
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderValue}
  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage
  );
  TEncoderValue = EncoderValue;

//---------------------------------------------------------------------------
// Conversion of Emf To WMF Bits flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM EmfToWmfBitsFlags}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault          = $00000000,
    EmfToWmfBitsFlagsEmbedEmf         = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip        = $00000004
  );
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;

(**************************************************************************\
*
*   GDI+ Types
*
\**************************************************************************)

//--------------------------------------------------------------------------
// Callback functions
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageAbort}
  ImageAbort = function: BOOL; stdcall;
  {$EXTERNALSYM DrawImageAbort}
  DrawImageAbort         = ImageAbort;
  {$EXTERNALSYM GetThumbnailImageAbort}
  GetThumbnailImageAbort = ImageAbort;


  // Callback for EnumerateMetafile methods.  The parameters are:

  //      recordType      WMF, EMF, or EMF+ record type
  //      flags           (always 0 for WMF/EMF records)
  //      dataSize        size of the record data (in bytes), or 0 if no data
  //      data            pointer to the record data, or NULL if no data
  //      callbackData    pointer to callbackData, if any

  // This method can then call Metafile::PlayRecord to play the
  // record that was just enumerated.  If this method  returns
  // FALSE, the enumeration process is aborted.  Otherwise, it continues.

  {$EXTERNALSYM EnumerateMetafileProc}
  EnumerateMetafileProc = function(recordType: EmfPlusRecordType; flags: UINT;
    dataSize: UINT; data: PBYTE; callbackData: pointer): BOOL; stdcall;

//--------------------------------------------------------------------------
// Primitive data types
//
// NOTE:
//  Types already defined in standard header files:
//      INT8
//      UINT8
//      INT16
//      UINT16
//      INT32
//      UINT32
//      INT64
//      UINT64
//
//  Avoid using the following types:
//      LONG - use INT
//      ULONG - use UINT
//      DWORD - use UINT32
//--------------------------------------------------------------------------

const
  { from float.h }
  FLT_MAX =  3.402823466e+38; // max value
  FLT_MIN =  1.175494351e-38; // min positive value

  REAL_MAX           = FLT_MAX;
  {$EXTERNALSYM REAL_MAX}
  REAL_MIN           = FLT_MIN;
  {$EXTERNALSYM REAL_MIN}
  REAL_TOLERANCE     = (FLT_MIN * 100);
  {$EXTERNALSYM REAL_TOLERANCE}
  REAL_EPSILON       = 1.192092896e-07;        // FLT_EPSILON
  {$EXTERNALSYM REAL_EPSILON}

//--------------------------------------------------------------------------
// Status return values from GDI+ methods
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM Status}
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );
  TStatus = Status;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PSizeF = ^TSizeF;
  TSizeF = packed record
    Width  : Single;
    Height : Single;
  end;

  function MakeSize(Width, Height: Single): TSizeF; overload;

//--------------------------------------------------------------------------
// Represents a dimension in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PSize = ^TSize;
  TSize = packed record
    Width  : Integer;
    Height : Integer;
  end;

  function MakeSize(Width, Height: Integer): TSize; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PPointF = ^TPointF;
  TPointF = packed record
    X : Single;
    Y : Single;
  end;
  TPointFDynArray = array of TPointF;

  function MakePoint(X, Y: Single): TPointF; overload;

//--------------------------------------------------------------------------
// Represents a location in a 2D coordinate system (integer coordinates)
//--------------------------------------------------------------------------

type
  PPoint = ^TPoint;
  TPoint = packed record
    X : Integer;
    Y : Integer;
  end;
  TPointDynArray = array of TPoint;

  function MakePoint(X, Y: Integer): TPoint; overload;

//--------------------------------------------------------------------------
// Represents a rectangle in a 2D coordinate system (floating-point coordinates)
//--------------------------------------------------------------------------

type
  PRectF = ^TRectF;
  TRectF = packed record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;
  TRectFDynArray = array of TRectF;

  function MakeRect(x, y, width, height: Single): TRectF; overload;
  function MakeRect(location: TPointF; size: TSizeF): TRectF; overload;

type
  PRect = ^TRect;
  TRect = packed record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;
  TRectDynArray = array of TRect;

  function MakeRect(x, y, width, height: Integer): TRect; overload;
  function MakeRect(location: TPoint; size: TSize): Trect; overload;

type
  TPathData = packed class
  public
    Count  : Integer;
    Points : PPointF;
    Types  : PBYTE;
    constructor Create;
    destructor destroy; override;
  end;

  PCharacterRange = ^TCharacterRange;
  TCharacterRange = packed record
    First  : Integer;
    Length : Integer;
  end;

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;

(**************************************************************************
*
*   GDI+ Startup and Shutdown APIs
*
**************************************************************************)
type
  {$EXTERNALSYM DebugEventLevel}
  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning
  );
  TDebugEventLevel = DebugEventLevel;

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  {$EXTERNALSYM DebugEventProc}
  DebugEventProc = procedure(level: DebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  {$EXTERNALSYM NotificationHookProc}
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  {$EXTERNALSYM NotificationUnhookProc}
  NotificationUnhookProc = procedure(token: ULONG); stdcall;

  // Input structure for GdiplusStartup

  {$EXTERNALSYM GdiplusStartupInput}
  GdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;                                        // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  {$EXTERNALSYM GdiplusStartupOutput}
  GdiplusStartupOutput = packed record
    // The following 2 fields are NULL if SuppressBackgroundThread is FALSE.
    // Otherwise, they are functions which must be called appropriately to
    // replace the background thread.
    //
    // These should be called on the application's main message loop - i.e.
    // a message loop which is active for the lifetime of GDI+.
    // "NotificationHook" should be called before starting the loop,
    // and "NotificationUnhook" should be called after the loop ends.

    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  // GDI+ initialization. Must not be called from DllMain - can cause deadlock.
  //
  // Must be called before GDI+ API's or constructors are used.
  //
  // token  - may not be NULL - accepts a token to be passed in the corresponding
  //          GdiplusShutdown call.
  // input  - may not be NULL
  // output - may be NULL only if input->SuppressBackgroundThread is FALSE.

  {$EXTERNALSYM GdiplusStartup}
 function GdiplusStartup(out token: ULONG; input: PGdiplusStartupInput;
   output: PGdiplusStartupOutput): Status; stdcall; external 'gdiplus.dll';

  // GDI+ termination. Must be called before GDI+ is unloaded.
  // Must not be called from DllMain - can cause deadlock.
  //
  // GDI+ API's may not be called after GdiplusShutdown. Pay careful attention
  // to GDI+ object destructors.

  {$EXTERNALSYM GdiplusShutdown}
  procedure GdiplusShutdown(token: ULONG); stdcall; external 'gdiplus.dll';


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   Gdiplus Pixel Formats
* Abstract:
*   GDI+ Pixel Formats
*
\**************************************************************************)

type
  PARGB  = ^ARGB;
  ARGB   = DWORD;
  {$EXTERNALSYM ARGB}
  ARGB64 = Int64;
  {$EXTERNALSYM ARGB64}

const
  ALPHA_SHIFT = 24;
  {$EXTERNALSYM ALPHA_SHIFT}
  RED_SHIFT   = 16;
  {$EXTERNALSYM RED_SHIFT}
  GREEN_SHIFT = 8;
  {$EXTERNALSYM GREEN_SHIFT}
  BLUE_SHIFT  = 0;
  {$EXTERNALSYM BLUE_SHIFT}
  ALPHA_MASK  = (ARGB($ff) shl ALPHA_SHIFT);
  {$EXTERNALSYM ALPHA_MASK}

  // In-memory pixel data formats:
  // bits 0-7 = format index
  // bits 8-15 = pixel size (in bits)
  // bits 16-23 = flags
  // bits 24-31 = reserved

type
  PixelFormat = Integer;
  {$EXTERNALSYM PixelFormat}
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed     = $00010000; // Indexes into a palette
  {$EXTERNALSYM PixelFormatIndexed}
  PixelFormatGDI         = $00020000; // Is a GDI-supported format
  {$EXTERNALSYM PixelFormatGDI}
  PixelFormatAlpha       = $00040000; // Has an alpha component
  {$EXTERNALSYM PixelFormatAlpha}
  PixelFormatPAlpha      = $00080000; // Pre-multiplied alpha
  {$EXTERNALSYM PixelFormatPAlpha}
  PixelFormatExtended    = $00100000; // Extended color 16 bits/channel
  {$EXTERNALSYM PixelFormatExtended}
  PixelFormatCanonical   = $00200000;
  {$EXTERNALSYM PixelFormatCanonical}

  PixelFormatUndefined      = 0;
  {$EXTERNALSYM PixelFormatUndefined}
  PixelFormatDontCare       = 0;
  {$EXTERNALSYM PixelFormatDontCare}

  PixelFormat1bppIndexed    = (1  or ( 1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat1bppIndexed}
  PixelFormat4bppIndexed    = (2  or ( 4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat4bppIndexed}
  PixelFormat8bppIndexed    = (3  or ( 8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat8bppIndexed}
  PixelFormat16bppGrayScale = (4  or (16 shl 8) or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat16bppGrayScale}
  PixelFormat16bppRGB555    = (5  or (16 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppRGB555}
  PixelFormat16bppRGB565    = (6  or (16 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppRGB565}
  PixelFormat16bppARGB1555  = (7  or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat16bppARGB1555}
  PixelFormat24bppRGB       = (8  or (24 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat24bppRGB}
  PixelFormat32bppRGB       = (9  or (32 shl 8) or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat32bppRGB}
  PixelFormat32bppARGB      = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  {$EXTERNALSYM PixelFormat32bppARGB}
  PixelFormat32bppPARGB     = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  {$EXTERNALSYM PixelFormat32bppPARGB}
  PixelFormat48bppRGB       = (12 or (48 shl 8) or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat48bppRGB}
  PixelFormat64bppARGB      = (13 or (64 shl 8) or PixelFormatAlpha  or PixelFormatCanonical or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat64bppARGB}
  PixelFormat64bppPARGB     = (14 or (64 shl 8) or PixelFormatAlpha  or PixelFormatPAlpha or PixelFormatExtended);
  {$EXTERNALSYM PixelFormat64bppPARGB}
  PixelFormatMax            = 15;
  {$EXTERNALSYM PixelFormatMax}

{$EXTERNALSYM GetPixelFormatSize}
function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
{$EXTERNALSYM IsIndexedPixelFormat}
function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
{$EXTERNALSYM IsAlphaPixelFormat}
function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
{$EXTERNALSYM IsExtendedPixelFormat}
function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;

//--------------------------------------------------------------------------
// Determine if the Pixel Format is Canonical format:
//   PixelFormat32bppARGB
//   PixelFormat32bppPARGB
//   PixelFormat64bppARGB
//   PixelFormat64bppPARGB
//--------------------------------------------------------------------------

{$EXTERNALSYM IsCanonicalPixelFormat}
function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;

type
  {$EXTERNALSYM PaletteFlags}
  PaletteFlags = (
    PaletteFlagsHasAlpha    = $0001,
    PaletteFlagsGrayScale   = $0002,
    PaletteFlagsHalftone    = $0004
  );
  TPaletteFlags = PaletteFlags;


  {$EXTERNALSYM ColorPalette}
  ColorPalette = packed record
    Flags  : UINT ;                 // Palette flags
    Count  : UINT ;                 // Number of color entries
    Entries: array [0..0] of ARGB ; // Palette color entries
  end;

  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

(**************************************************************************\
*
*   GDI+ Color Object
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color mode
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMode}
  ColorMode = (
    ColorModeARGB32 = 0,
    ColorModeARGB64 = 1
  );
  TColorMode = ColorMode;

//----------------------------------------------------------------------------
// Color Channel flags 
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorChannelFlags}
  ColorChannelFlags = (
    ColorChannelFlagsC = 0,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast
  );
  TColorChannelFlags = ColorChannelFlags;

//----------------------------------------------------------------------------
// Color
//----------------------------------------------------------------------------

  // Common color constants
const
  aclAliceBlue            = $FFF0F8FF;
  aclAntiqueWhite         = $FFFAEBD7;
  aclAqua                 = $FF00FFFF;
  aclAquamarine           = $FF7FFFD4;
  aclAzure                = $FFF0FFFF;
  aclBeige                = $FFF5F5DC;
  aclBisque               = $FFFFE4C4;
  aclBlack                = $FF000000;
  aclBlanchedAlmond       = $FFFFEBCD;
  aclBlue                 = $FF0000FF;
  aclBlueViolet           = $FF8A2BE2;
  aclBrown                = $FFA52A2A;
  aclBurlyWood            = $FFDEB887;
  aclCadetBlue            = $FF5F9EA0;
  aclChartreuse           = $FF7FFF00;
  aclChocolate            = $FFD2691E;
  aclCoral                = $FFFF7F50;
  aclCornflowerBlue       = $FF6495ED;
  aclCornsilk             = $FFFFF8DC;
  aclCrimson              = $FFDC143C;
  aclCyan                 = $FF00FFFF;
  aclDarkBlue             = $FF00008B;
  aclDarkCyan             = $FF008B8B;
  aclDarkGoldenrod        = $FFB8860B;
  aclDarkGray             = $FFA9A9A9;
  aclDarkGreen            = $FF006400;
  aclDarkKhaki            = $FFBDB76B;
  aclDarkMagenta          = $FF8B008B;
  aclDarkOliveGreen       = $FF556B2F;
  aclDarkOrange           = $FFFF8C00;
  aclDarkOrchid           = $FF9932CC;
  aclDarkRed              = $FF8B0000;
  aclDarkSalmon           = $FFE9967A;
  aclDarkSeaGreen         = $FF8FBC8B;
  aclDarkSlateBlue        = $FF483D8B;
  aclDarkSlateGray        = $FF2F4F4F;
  aclDarkTurquoise        = $FF00CED1;
  aclDarkViolet           = $FF9400D3;
  aclDeepPink             = $FFFF1493;
  aclDeepSkyBlue          = $FF00BFFF;
  aclDimGray              = $FF696969;
  aclDodgerBlue           = $FF1E90FF;
  aclFirebrick            = $FFB22222;
  aclFloralWhite          = $FFFFFAF0;
  aclForestGreen          = $FF228B22;
  aclFuchsia              = $FFFF00FF;
  aclGainsboro            = $FFDCDCDC;
  aclGhostWhite           = $FFF8F8FF;
  aclGold                 = $FFFFD700;
  aclGoldenrod            = $FFDAA520;
  aclGray                 = $FF808080;
  aclGreen                = $FF008000;
  aclGreenYellow          = $FFADFF2F;
  aclHoneydew             = $FFF0FFF0;
  aclHotPink              = $FFFF69B4;
  aclIndianRed            = $FFCD5C5C;
  aclIndigo               = $FF4B0082;
  aclIvory                = $FFFFFFF0;
  aclKhaki                = $FFF0E68C;
  aclLavender             = $FFE6E6FA;
  aclLavenderBlush        = $FFFFF0F5;
  aclLawnGreen            = $FF7CFC00;
  aclLemonChiffon         = $FFFFFACD;
  aclLightBlue            = $FFADD8E6;
  aclLightCoral           = $FFF08080;
  aclLightCyan            = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray            = $FFD3D3D3;
  aclLightGreen           = $FF90EE90;
  aclLightPink            = $FFFFB6C1;
  aclLightSalmon          = $FFFFA07A;
  aclLightSeaGreen        = $FF20B2AA;
  aclLightSkyBlue         = $FF87CEFA;
  aclLightSlateGray       = $FF778899;
  aclLightSteelBlue       = $FFB0C4DE;
  aclLightYellow          = $FFFFFFE0;
  aclLime                 = $FF00FF00;
  aclLimeGreen            = $FF32CD32;
  aclLinen                = $FFFAF0E6;
  aclMagenta              = $FFFF00FF;
  aclMaroon               = $FF800000;
  aclMediumAquamarine     = $FF66CDAA;
  aclMediumBlue           = $FF0000CD;
  aclMediumOrchid         = $FFBA55D3;
  aclMediumPurple         = $FF9370DB;
  aclMediumSeaGreen       = $FF3CB371;
  aclMediumSlateBlue      = $FF7B68EE;
  aclMediumSpringGreen    = $FF00FA9A;
  aclMediumTurquoise      = $FF48D1CC;
  aclMediumVioletRed      = $FFC71585;
  aclMidnightBlue         = $FF191970;
  aclMintCream            = $FFF5FFFA;
  aclMistyRose            = $FFFFE4E1;
  aclMoccasin             = $FFFFE4B5;
  aclNavajoWhite          = $FFFFDEAD;
  aclNavy                 = $FF000080;
  aclOldLace              = $FFFDF5E6;
  aclOlive                = $FF808000;
  aclOliveDrab            = $FF6B8E23;
  aclOrange               = $FFFFA500;
  aclOrangeRed            = $FFFF4500;
  aclOrchid               = $FFDA70D6;
  aclPaleGoldenrod        = $FFEEE8AA;
  aclPaleGreen            = $FF98FB98;
  aclPaleTurquoise        = $FFAFEEEE;
  aclPaleVioletRed        = $FFDB7093;
  aclPapayaWhip           = $FFFFEFD5;
  aclPeachPuff            = $FFFFDAB9;
  aclPeru                 = $FFCD853F;
  aclPink                 = $FFFFC0CB;
  aclPlum                 = $FFDDA0DD;
  aclPowderBlue           = $FFB0E0E6;
  aclPurple               = $FF800080;
  aclRed                  = $FFFF0000;
  aclRosyBrown            = $FFBC8F8F;
  aclRoyalBlue            = $FF4169E1;
  aclSaddleBrown          = $FF8B4513;
  aclSalmon               = $FFFA8072;
  aclSandyBrown           = $FFF4A460;
  aclSeaGreen             = $FF2E8B57;
  aclSeaShell             = $FFFFF5EE;
  aclSienna               = $FFA0522D;
  aclSilver               = $FFC0C0C0;
  aclSkyBlue              = $FF87CEEB;
  aclSlateBlue            = $FF6A5ACD;
  aclSlateGray            = $FF708090;
  aclSnow                 = $FFFFFAFA;
  aclSpringGreen          = $FF00FF7F;
  aclSteelBlue            = $FF4682B4;
  aclTan                  = $FFD2B48C;
  aclTeal                 = $FF008080;
  aclThistle              = $FFD8BFD8;
  aclTomato               = $FFFF6347;
  aclTransparent          = $00FFFFFF;
  aclTurquoise            = $FF40E0D0;
  aclViolet               = $FFEE82EE;
  aclWheat                = $FFF5DEB3;
  aclWhite                = $FFFFFFFF;
  aclWhiteSmoke           = $FFF5F5F5;
  aclYellow               = $FFFFFF00;
  aclYellowGreen          = $FF9ACD32;

  // Shift count and bit mask for A, R, G, B components
  AlphaShift  = 24;
  {$EXTERNALSYM AlphaShift}
  RedShift    = 16;
  {$EXTERNALSYM RedShift}
  GreenShift  = 8;
  {$EXTERNALSYM GreenShift}
  BlueShift   = 0;
  {$EXTERNALSYM BlueShift}

  AlphaMask   = $ff000000;
  {$EXTERNALSYM AlphaMask}
  RedMask     = $00ff0000;
  {$EXTERNALSYM RedMask}
  GreenMask   = $0000ff00;
  {$EXTERNALSYM GreenMask}
  BlueMask    = $000000ff;
  {$EXTERNALSYM BlueMask}


type
{  TColor = class
  protected
     Argb: ARGB;
  public
    constructor Create; overload;
    constructor Create(r, g, b: Byte); overload;
    constructor Create(a, r, g, b: Byte); overload;
    constructor Create(Value: ARGB); overload;
    function GetAlpha: BYTE;
    function GetA: BYTE;
    function GetRed: BYTE;
    function GetR: BYTE;
    function GetGreen: Byte;
    function GetG: Byte;
    function GetBlue: Byte;
    function GetB: Byte;
    function GetValue: ARGB;
    procedure SetValue(Value: ARGB);
    procedure SetFromCOLORREF(rgb: COLORREF);
    function ToCOLORREF: COLORREF;
    function MakeARGB(a, r, g, b: Byte): ARGB;
  end;  }

  PColor = ^TColor;
  TColor = ARGB;
  TColorDynArray = array of TColor;

  function MakeColor(r, g, b: Byte): ARGB; overload;
  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  function GetAlpha(color: ARGB): BYTE;
  function GetRed(color: ARGB): BYTE;
  function GetGreen(color: ARGB): BYTE;
  function GetBlue(color: ARGB): BYTE;
  function ColorRefToARGB(rgb: COLORREF): ARGB;
  function ARGBToColorRef(Color: ARGB): COLORREF;


(**************************************************************************\
*
*   GDI+ Metafile Related Structures
*
\**************************************************************************)

type
  { from Windef.h }
  RECTL = Windows.Trect;
  SIZEL = Windows.TSize;

  {$EXTERNALSYM ENHMETAHEADER3}
  ENHMETAHEADER3 = packed record
    iType          : DWORD;  // Record type EMR_HEADER
    nSize          : DWORD;  // Record size in bytes.  This may be greater
                             // than the sizeof(ENHMETAHEADER).
    rclBounds      : RECTL;  // Inclusive-inclusive bounds in device units
    rclFrame       : RECTL;  // Inclusive-inclusive Picture Frame .01mm unit
    dSignature     : DWORD;  // Signature.  Must be ENHMETA_SIGNATURE.
    nVersion       : DWORD;  // Version number
    nBytes         : DWORD;  // Size of the metafile in bytes
    nRecords       : DWORD;  // Number of records in the metafile
    nHandles       : WORD;   // Number of handles in the handle table
                             // Handle index zero is reserved.
    sReserved      : WORD;   // Reserved.  Must be zero.
    nDescription   : DWORD;  // Number of chars in the unicode desc string
                             // This is 0 if there is no description string
    offDescription : DWORD;  // Offset to the metafile description record.
                             // This is 0 if there is no description string
    nPalEntries    : DWORD;  // Number of entries in the metafile palette.
    szlDevice      : SIZEL;  // Size of the reference device in pels
    szlMillimeters : SIZEL;  // Size of the reference device in millimeters
  end;
  TENHMETAHEADER3 = ENHMETAHEADER3;
  PENHMETAHEADER3 = ^TENHMETAHEADER3;

  // Placeable WMFs

  // Placeable Metafiles were created as a non-standard way of specifying how
  // a metafile is mapped and scaled on an output device.
  // Placeable metafiles are quite wide-spread, but not directly supported by
  // the Windows API. To playback a placeable metafile using the Windows API,
  // you will first need to strip the placeable metafile header from the file.
  // This is typically performed by copying the metafile to a temporary file
  // starting at file offset 22 (0x16). The contents of the temporary file may
  // then be used as input to the Windows GetMetaFile(), PlayMetaFile(),
  // CopyMetaFile(), etc. GDI functions.

  // Each placeable metafile begins with a 22-byte header,
  //  followed by a standard metafile:

  {$EXTERNALSYM PWMFRect16}
  PWMFRect16 = packed record
    Left   : INT16;
    Top    : INT16;
    Right  : INT16;
    Bottom : INT16;
  end;
  TPWMFRect16 = PWMFRect16;
  PPWMFRect16 = ^TPWMFRect16;

  {$EXTERNALSYM WmfPlaceableFileHeader}
  WmfPlaceableFileHeader = packed record
    Key         : UINT32;      // GDIP_WMF_PLACEABLEKEY
    Hmf         : INT16;       // Metafile HANDLE number (always 0)
    BoundingBox : PWMFRect16;  // Coordinates in metafile units
    Inch        : INT16;       // Number of metafile units per inch
    Reserved    : UINT32;      // Reserved (always 0)
    Checksum    : INT16;       // Checksum value for previous 10 WORDs
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

  // Key contains a special identification value that indicates the presence
  // of a placeable metafile header and is always 0x9AC6CDD7.

  // Handle is used to stored the handle of the metafile in memory. When written
  // to disk, this field is not used and will always contains the value 0.

  // Left, Top, Right, and Bottom contain the coordinates of the upper-left
  // and lower-right corners of the image on the output device. These are
  // measured in twips.

  // A twip (meaning "twentieth of a point") is the logical unit of measurement
  // used in Windows Metafiles. A twip is equal to 1/1440 of an inch. Thus 720
  // twips equal 1/2 inch, while 32,768 twips is 22.75 inches.

  // Inch contains the number of twips per inch used to represent the image.
  // Normally, there are 1440 twips per inch; however, this number may be
  // changed to scale the image. A value of 720 indicates that the image is
  // double its normal size, or scaled to a factor of 2:1. A value of 360
  // indicates a scale of 4:1, while a value of 2880 indicates that the image
  // is scaled down in size by a factor of two. A value of 1440 indicates
  // a 1:1 scale ratio.

  // Reserved is not used and is always set to 0.

  // Checksum contains a checksum value for the previous 10 WORDs in the header.
  // This value can be used in an attempt to detect if the metafile has become
  // corrupted. The checksum is calculated by XORing each WORD value to an
  // initial value of 0.

  // If the metafile was recorded with a reference Hdc that was a display.

const
  GDIP_EMFPLUSFLAGS_DISPLAY      = $00000001;
  {$EXTERNALSYM GDIP_EMFPLUSFLAGS_DISPLAY}

type
  TMetafileHeader = packed class
  public
    Type_        : TMetafileType;
    Size         : UINT;           // Size of the metafile (in bytes)
    Version      : UINT;           // EMF+, EMF, or WMF version
    EmfPlusFlags : UINT;
    DpiX         : Single;
    DpiY         : Single;
    X            : Integer;        // Bounds in device units
    Y            : Integer;
    Width        : Integer;
    Height       : Integer;
    Header       : record
    case integer of
      0: (WmfHeader: TMETAHEADER;);
      1: (EmfHeader: TENHMETAHEADER3);
    end;
    EmfPlusHeaderSize : Integer; // size of the EMF+ header in file
    LogicalDpiX       : Integer; // Logical Dpi of reference Hdc
    LogicalDpiY       : Integer; // usually valid only for EMF+
  public
    property GetType: TMetafileType read Type_;
    property GetMetafileSize: UINT read Size;
    // If IsEmfPlus, this is the EMF+ version; else it is the WMF or EMF ver
    property GetVersion: UINT read Version;
     // Get the EMF+ flags associated with the metafile
    property GetEmfPlusFlags: UINT read EmfPlusFlags;
    property GetDpiX: Single read DpiX;
    property GetDpiY: Single read DpiY;
    procedure GetBounds(out Rect: Trect);
    // Is it any type of WMF (standard or Placeable Metafile)?
    function IsWmf: BOOL;
    // Is this an Placeable Metafile?
    function IsWmfPlaceable: BOOL;
    // Is this an EMF (not an EMF+)?
    function IsEmf: BOOL;
    // Is this an EMF or EMF+ file?
    function IsEmfOrEmfPlus: BOOL;
    // Is this an EMF+ file?
    function IsEmfPlus: BOOL;
    // Is this an EMF+ dual (has dual, down-level records) file?
    function IsEmfPlusDual: BOOL;
    // Is this an EMF+ only (no dual records) file?
    function IsEmfPlusOnly: BOOL;
    // If it's an EMF+ file, was it recorded against a display Hdc?
    function IsDisplay: BOOL;
    // Get the WMF header of the metafile (if it is a WMF)
    function GetWmfHeader: PMetaHeader;
    // Get the EMF header of the metafile (if it is an EMF)
    function GetEmfHeader: PENHMETAHEADER3;
  end;

(**************************************************************************\
*
*   GDI+ Imaging GUIDs
*
\**************************************************************************)

//---------------------------------------------------------------------------
// Image file format identifiers
//---------------------------------------------------------------------------

const
  ImageFormatUndefined : TGUID = '{b96b3ca9-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatUndefined}
  ImageFormatMemoryBMP : TGUID = '{b96b3caa-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatMemoryBMP}
  ImageFormatBMP       : TGUID = '{b96b3cab-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatBMP}
  ImageFormatEMF       : TGUID = '{b96b3cac-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEMF}
  ImageFormatWMF       : TGUID = '{b96b3cad-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatWMF}
  ImageFormatJPEG      : TGUID = '{b96b3cae-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatJPEG}
  ImageFormatPNG       : TGUID = '{b96b3caf-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatPNG}
  ImageFormatGIF       : TGUID = '{b96b3cb0-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatGIF}
  ImageFormatTIFF      : TGUID = '{b96b3cb1-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatTIFF}
  ImageFormatEXIF      : TGUID = '{b96b3cb2-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatEXIF}
  ImageFormatIcon      : TGUID = '{b96b3cb5-0728-11d3-9d7b-0000f81ef32e}';
  {$EXTERNALSYM ImageFormatIcon}

//---------------------------------------------------------------------------
// Predefined multi-frame dimension IDs
//---------------------------------------------------------------------------

  FrameDimensionTime       : TGUID = '{6aedbd6d-3fb5-418a-83a6-7f45229dc872}';
  {$EXTERNALSYM FrameDimensionTime}
  FrameDimensionResolution : TGUID = '{84236f7b-3bd3-428f-8dab-4ea1439ca315}';
  {$EXTERNALSYM FrameDimensionResolution}
  FrameDimensionPage       : TGUID = '{7462dc86-6180-4c7e-8e3f-ee7333a7a483}';
  {$EXTERNALSYM FrameDimensionPage}

//---------------------------------------------------------------------------
// Property sets
//---------------------------------------------------------------------------

  FormatIDImageInformation : TGUID = '{e5836cbe-5eef-4f1d-acde-ae4c43b608ce}';
  {$EXTERNALSYM FormatIDImageInformation}
  FormatIDJpegAppHeaders   : TGUID = '{1c4afdcd-6177-43cf-abc7-5f51af39ee85}';
  {$EXTERNALSYM FormatIDJpegAppHeaders}

//---------------------------------------------------------------------------
// Encoder parameter sets
//---------------------------------------------------------------------------

  EncoderCompression      : TGUID = '{e09d739d-ccd4-44ee-8eba-3fbf8be4fc58}';
  {$EXTERNALSYM EncoderCompression}
  EncoderColorDepth       : TGUID = '{66087055-ad66-4c7c-9a18-38a2310b8337}';
  {$EXTERNALSYM EncoderColorDepth}
  EncoderScanMethod       : TGUID = '{3a4e2661-3109-4e56-8536-42c156e7dcfa}';
  {$EXTERNALSYM EncoderScanMethod}
  EncoderVersion          : TGUID = '{24d18c76-814a-41a4-bf53-1c219cccf797}';
  {$EXTERNALSYM EncoderVersion}
  EncoderRenderMethod     : TGUID = '{6d42c53a-229a-4825-8bb7-5c99e2b9a8b8}';
  {$EXTERNALSYM EncoderRenderMethod}
  EncoderQuality          : TGUID = '{1d5be4b5-fa4a-452d-9cdd-5db35105e7eb}';
  {$EXTERNALSYM EncoderQuality}
  EncoderTransformation   : TGUID = '{8d0eb2d1-a58e-4ea8-aa14-108074b7b6f9}';
  {$EXTERNALSYM EncoderTransformation}
  EncoderLuminanceTable   : TGUID = '{edb33bce-0266-4a77-b904-27216099e717}';
  {$EXTERNALSYM EncoderLuminanceTable}
  EncoderChrominanceTable : TGUID = '{f2e455dc-09b3-4316-8260-676ada32481c}';
  {$EXTERNALSYM EncoderChrominanceTable}
  EncoderSaveFlag         : TGUID = '{292266fc-ac40-47bf-8cfc-a85b89a655de}';
  {$EXTERNALSYM EncoderSaveFlag}

  CodecIImageBytes : TGUID = '{025d1823-6c7d-447b-bbdb-a3cbc3dfa2fc}';
  {$EXTERNALSYM CodecIImageBytes}

type
  {$EXTERNALSYM IImageBytes}
  IImageBytes = Interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    // Return total number of bytes in the IStream
    function CountBytes(out pcb: UINT): HRESULT; stdcall;
    // Locks "cb" bytes, starting from "ulOffset" in the stream, and returns the
    // pointer to the beginning of the locked memory chunk in "ppvBytes"
    function LockBytes(cb: UINT; ulOffset: ULONG; out ppvBytes: pointer): HRESULT; stdcall;
    // Unlocks "cb" bytes, pointed by "pvBytes", starting from "ulOffset" in the
    // stream
    function UnlockBytes(pvBytes: pointer; cb: UINT; ulOffset: ULONG): HRESULT; stdcall;
  end;

//--------------------------------------------------------------------------
// ImageCodecInfo structure
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageCodecInfo}
  ImageCodecInfo = packed record
    Clsid             : TGUID;
    FormatID          : TGUID;
    CodecName         : PWCHAR;
    DllName           : PWCHAR;
    FormatDescription : PWCHAR;
    FilenameExtension : PWCHAR;
    MimeType          : PWCHAR;
    Flags             : DWORD;
    Version           : DWORD;
    SigCount          : DWORD;
    SigSize           : DWORD;
    SigPattern        : PBYTE;
    SigMask           : PBYTE;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;

//--------------------------------------------------------------------------
// Information flags about image codecs
//--------------------------------------------------------------------------

  {$EXTERNALSYM ImageCodecFlags}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder            = $00000001,
    ImageCodecFlagsDecoder            = $00000002,
    ImageCodecFlagsSupportBitmap      = $00000004,
    ImageCodecFlagsSupportVector      = $00000008,
    ImageCodecFlagsSeekableEncode     = $00000010,
    ImageCodecFlagsBlockingDecode     = $00000020,

    ImageCodecFlagsBuiltin            = $00010000,
    ImageCodecFlagsSystem             = $00020000,
    ImageCodecFlagsUser               = $00040000
  );
  TImageCodecFlags = ImageCodecFlags;

//---------------------------------------------------------------------------
// Access modes used when calling Image::LockBits
//---------------------------------------------------------------------------

  {$EXTERNALSYM ImageLockMode}
  ImageLockMode = Integer;
  const
    ImageLockModeRead         = $0001;
    ImageLockModeWrite        = $0002;
    ImageLockModeUserInputBuf = $0004;
type
  TImageLockMode = ImageLockMode;

//---------------------------------------------------------------------------
// Information about image pixel data
//---------------------------------------------------------------------------

  {$EXTERNALSYM BitmapData}
  BitmapData = packed record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : PixelFormat;
    Scan0       : Pointer;
    Reserved    : UINT;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;

//---------------------------------------------------------------------------
// Image flags
//---------------------------------------------------------------------------

  {$EXTERNALSYM ImageFlags}
  ImageFlags = (
    ImageFlagsNone                = 0,

    // Low-word: shared with SINKFLAG_x

    ImageFlagsScalable            = $0001,
    ImageFlagsHasAlpha            = $0002,
    ImageFlagsHasTranslucent      = $0004,
    ImageFlagsPartiallyScalable   = $0008,

    // Low-word: color space definition

    ImageFlagsColorSpaceRGB       = $0010,
    ImageFlagsColorSpaceCMYK      = $0020,
    ImageFlagsColorSpaceGRAY      = $0040,
    ImageFlagsColorSpaceYCBCR     = $0080,
    ImageFlagsColorSpaceYCCK      = $0100,

    // Low-word: image size info

    ImageFlagsHasRealDPI          = $1000,
    ImageFlagsHasRealPixelSize    = $2000,

    // High-word

    ImageFlagsReadOnly            = $00010000,
    ImageFlagsCaching             = $00020000
  );
  TImageFlags = ImageFlags;

  {$EXTERNALSYM RotateFlipType}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone   = 1,
    Rotate180FlipNone  = 2,
    Rotate270FlipNone  = 3,

    RotateNoneFlipX    = 4,
    Rotate90FlipX      = 5,
    Rotate180FlipX     = 6,
    Rotate270FlipX     = 7,

    RotateNoneFlipY    = Rotate180FlipX,
    Rotate90FlipY      = Rotate270FlipX,
    Rotate180FlipY     = RotateNoneFlipX,
    Rotate270FlipY     = Rotate90FlipX,

    RotateNoneFlipXY   = Rotate180FlipNone,
    Rotate90FlipXY     = Rotate270FlipNone,
    Rotate180FlipXY    = RotateNoneFlipNone,
    Rotate270FlipXY    = Rotate90FlipNone
  );
  TRotateFlipType = RotateFlipType;

//---------------------------------------------------------------------------
// Encoder Parameter structure
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameter}
  EncoderParameter = packed record
    Guid           : TGUID;   // GUID of the parameter
    NumberOfValues : ULONG;   // Number of the parameter values
    Type_          : ULONG;   // Value type, like ValueTypeLONG  etc.
    Value          : Pointer; // A pointer to the parameter values
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

//---------------------------------------------------------------------------
// Encoder Parameters structure
//---------------------------------------------------------------------------

  {$EXTERNALSYM EncoderParameters}
  EncoderParameters = packed record
    Count     : UINT;               // Number of parameters in this structure
    Parameter : array[0..0] of TEncoderParameter;  // Parameter values
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

//---------------------------------------------------------------------------
// Property Item
//---------------------------------------------------------------------------

  {$EXTERNALSYM PropertyItem}
  PropertyItem = record // NOT PACKED !!
    id       : PROPID;  // ID of this property
    length   : ULONG;   // Length of the property value, in bytes
    type_    : WORD;    // Type of the value, as one of TAG_TYPE_XXX
    value    : Pointer; // property value
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

//---------------------------------------------------------------------------
// Image property types
//---------------------------------------------------------------------------

const
  PropertyTagTypeByte      : Integer =  1;
  {$EXTERNALSYM PropertyTagTypeByte}
  PropertyTagTypeASCII     : Integer =  2;
  {$EXTERNALSYM PropertyTagTypeASCII}
  PropertyTagTypeShort     : Integer =  3;
  {$EXTERNALSYM PropertyTagTypeShort}
  PropertyTagTypeLong      : Integer =  4;
  {$EXTERNALSYM PropertyTagTypeLong}
  PropertyTagTypeRational  : Integer =  5;
  {$EXTERNALSYM PropertyTagTypeRational}
  PropertyTagTypeUndefined : Integer =  7;
  {$EXTERNALSYM PropertyTagTypeUndefined}
  PropertyTagTypeSLONG     : Integer =  9;
  {$EXTERNALSYM PropertyTagTypeSLONG}
  PropertyTagTypeSRational : Integer = 10;
  {$EXTERNALSYM PropertyTagTypeSRational}

//---------------------------------------------------------------------------
// Image property ID tags
//---------------------------------------------------------------------------

  PropertyTagExifIFD            = $8769;
  {$EXTERNALSYM PropertyTagExifIFD}
  PropertyTagGpsIFD             = $8825;
  {$EXTERNALSYM PropertyTagGpsIFD}

  PropertyTagNewSubfileType     = $00FE;
  {$EXTERNALSYM PropertyTagNewSubfileType}
  PropertyTagSubfileType        = $00FF;
  {$EXTERNALSYM PropertyTagSubfileType}
  PropertyTagImageWidth         = $0100;
  {$EXTERNALSYM PropertyTagImageWidth}
  PropertyTagImageHeight        = $0101;
  {$EXTERNALSYM PropertyTagImageHeight}
  PropertyTagBitsPerSample      = $0102;
  {$EXTERNALSYM PropertyTagBitsPerSample}
  PropertyTagCompression        = $0103;
  {$EXTERNALSYM PropertyTagCompression}
  PropertyTagPhotometricInterp  = $0106;
  {$EXTERNALSYM PropertyTagPhotometricInterp}
  PropertyTagThreshHolding      = $0107;
  {$EXTERNALSYM PropertyTagThreshHolding}
  PropertyTagCellWidth          = $0108;
  {$EXTERNALSYM PropertyTagCellWidth}
  PropertyTagCellHeight         = $0109;
  {$EXTERNALSYM PropertyTagCellHeight}
  PropertyTagFillOrder          = $010A;
  {$EXTERNALSYM PropertyTagFillOrder}
  PropertyTagDocumentName       = $010D;
  {$EXTERNALSYM PropertyTagDocumentName}
  PropertyTagImageDescription   = $010E;
  {$EXTERNALSYM PropertyTagImageDescription}
  PropertyTagEquipMake          = $010F;
  {$EXTERNALSYM PropertyTagEquipMake}
  PropertyTagEquipModel         = $0110;
  {$EXTERNALSYM PropertyTagEquipModel}
  PropertyTagStripOffsets       = $0111;
  {$EXTERNALSYM PropertyTagStripOffsets}
  PropertyTagOrientation        = $0112;
  {$EXTERNALSYM PropertyTagOrientation}
  PropertyTagSamplesPerPixel    = $0115;
  {$EXTERNALSYM PropertyTagSamplesPerPixel}
  PropertyTagRowsPerStrip       = $0116;
  {$EXTERNALSYM PropertyTagRowsPerStrip}
  PropertyTagStripBytesCount    = $0117;
  {$EXTERNALSYM PropertyTagStripBytesCount}
  PropertyTagMinSampleValue     = $0118;
  {$EXTERNALSYM PropertyTagMinSampleValue}
  PropertyTagMaxSampleValue     = $0119;
  {$EXTERNALSYM PropertyTagMaxSampleValue}
  PropertyTagXResolution        = $011A;   // Image resolution in width direction
  {$EXTERNALSYM PropertyTagXResolution}
  PropertyTagYResolution        = $011B;   // Image resolution in height direction
  {$EXTERNALSYM PropertyTagYResolution}
  PropertyTagPlanarConfig       = $011C;   // Image data arrangement
  {$EXTERNALSYM PropertyTagPlanarConfig}
  PropertyTagPageName           = $011D;
  {$EXTERNALSYM PropertyTagPageName}
  PropertyTagXPosition          = $011E;
  {$EXTERNALSYM PropertyTagXPosition}
  PropertyTagYPosition          = $011F;
  {$EXTERNALSYM PropertyTagYPosition}
  PropertyTagFreeOffset         = $0120;
  {$EXTERNALSYM PropertyTagFreeOffset}
  PropertyTagFreeByteCounts     = $0121;
  {$EXTERNALSYM PropertyTagFreeByteCounts}
  PropertyTagGrayResponseUnit   = $0122;
  {$EXTERNALSYM PropertyTagGrayResponseUnit}
  PropertyTagGrayResponseCurve  = $0123;
  {$EXTERNALSYM PropertyTagGrayResponseCurve}
  PropertyTagT4Option           = $0124;
  {$EXTERNALSYM PropertyTagT4Option}
  PropertyTagT6Option           = $0125;
  {$EXTERNALSYM PropertyTagT6Option}
  PropertyTagResolutionUnit     = $0128;   // Unit of X and Y resolution
  {$EXTERNALSYM PropertyTagResolutionUnit}
  PropertyTagPageNumber         = $0129;
  {$EXTERNALSYM PropertyTagPageNumber}
  PropertyTagTransferFuncition  = $012D;
  {$EXTERNALSYM PropertyTagTransferFuncition}
  PropertyTagSoftwareUsed       = $0131;
  {$EXTERNALSYM PropertyTagSoftwareUsed}
  PropertyTagDateTime           = $0132;
  {$EXTERNALSYM PropertyTagDateTime}
  PropertyTagArtist             = $013B;
  {$EXTERNALSYM PropertyTagArtist}
  PropertyTagHostComputer       = $013C;
  {$EXTERNALSYM PropertyTagHostComputer}
  PropertyTagPredictor          = $013D;
  {$EXTERNALSYM PropertyTagPredictor}
  PropertyTagWhitePoint         = $013E;
  {$EXTERNALSYM PropertyTagWhitePoint}
  PropertyTagPrimaryChromaticities = $013F;
  {$EXTERNALSYM PropertyTagPrimaryChromaticities}
  PropertyTagColorMap           = $0140;
  {$EXTERNALSYM PropertyTagColorMap}
  PropertyTagHalftoneHints      = $0141;
  {$EXTERNALSYM PropertyTagHalftoneHints}
  PropertyTagTileWidth          = $0142;
  {$EXTERNALSYM PropertyTagTileWidth}
  PropertyTagTileLength         = $0143;
  {$EXTERNALSYM PropertyTagTileLength}
  PropertyTagTileOffset         = $0144;
  {$EXTERNALSYM PropertyTagTileOffset}
  PropertyTagTileByteCounts     = $0145;
  {$EXTERNALSYM PropertyTagTileByteCounts}
  PropertyTagInkSet             = $014C;
  {$EXTERNALSYM PropertyTagInkSet}
  PropertyTagInkNames           = $014D;
  {$EXTERNALSYM PropertyTagInkNames}
  PropertyTagNumberOfInks       = $014E;
  {$EXTERNALSYM PropertyTagNumberOfInks}
  PropertyTagDotRange           = $0150;
  {$EXTERNALSYM PropertyTagDotRange}
  PropertyTagTargetPrinter      = $0151;
  {$EXTERNALSYM PropertyTagTargetPrinter}
  PropertyTagExtraSamples       = $0152;
  {$EXTERNALSYM PropertyTagExtraSamples}
  PropertyTagSampleFormat       = $0153;
  {$EXTERNALSYM PropertyTagSampleFormat}
  PropertyTagSMinSampleValue    = $0154;
  {$EXTERNALSYM PropertyTagSMinSampleValue}
  PropertyTagSMaxSampleValue    = $0155;
  {$EXTERNALSYM PropertyTagSMaxSampleValue}
  PropertyTagTransferRange      = $0156;
  {$EXTERNALSYM PropertyTagTransferRange}

  PropertyTagJPEGProc               = $0200;
  {$EXTERNALSYM PropertyTagJPEGProc}
  PropertyTagJPEGInterFormat        = $0201;
  {$EXTERNALSYM PropertyTagJPEGInterFormat}
  PropertyTagJPEGInterLength        = $0202;
  {$EXTERNALSYM PropertyTagJPEGInterLength}
  PropertyTagJPEGRestartInterval    = $0203;
  {$EXTERNALSYM PropertyTagJPEGRestartInterval}
  PropertyTagJPEGLosslessPredictors = $0205;
  {$EXTERNALSYM PropertyTagJPEGLosslessPredictors}
  PropertyTagJPEGPointTransforms    = $0206;
  {$EXTERNALSYM PropertyTagJPEGPointTransforms}
  PropertyTagJPEGQTables            = $0207;
  {$EXTERNALSYM PropertyTagJPEGQTables}
  PropertyTagJPEGDCTables           = $0208;
  {$EXTERNALSYM PropertyTagJPEGDCTables}
  PropertyTagJPEGACTables           = $0209;
  {$EXTERNALSYM PropertyTagJPEGACTables}

  PropertyTagYCbCrCoefficients  = $0211;
  {$EXTERNALSYM PropertyTagYCbCrCoefficients}
  PropertyTagYCbCrSubsampling   = $0212;
  {$EXTERNALSYM PropertyTagYCbCrSubsampling}
  PropertyTagYCbCrPositioning   = $0213;
  {$EXTERNALSYM PropertyTagYCbCrPositioning}
  PropertyTagREFBlackWhite      = $0214;
  {$EXTERNALSYM PropertyTagREFBlackWhite}

  PropertyTagICCProfile         = $8773;   // This TAG is defined by ICC
  {$EXTERNALSYM PropertyTagICCProfile}
                                           // for embedded ICC in TIFF
  PropertyTagGamma                = $0301;
  {$EXTERNALSYM PropertyTagGamma}
  PropertyTagICCProfileDescriptor = $0302;
  {$EXTERNALSYM PropertyTagICCProfileDescriptor}
  PropertyTagSRGBRenderingIntent  = $0303;
  {$EXTERNALSYM PropertyTagSRGBRenderingIntent}

  PropertyTagImageTitle         = $0320;
  {$EXTERNALSYM PropertyTagImageTitle}
  PropertyTagCopyright          = $8298;
  {$EXTERNALSYM PropertyTagCopyright}

// Extra TAGs (Like Adobe Image Information tags etc.)

  PropertyTagResolutionXUnit           = $5001;
  {$EXTERNALSYM PropertyTagResolutionXUnit}
  PropertyTagResolutionYUnit           = $5002;
  {$EXTERNALSYM PropertyTagResolutionYUnit}
  PropertyTagResolutionXLengthUnit     = $5003;
  {$EXTERNALSYM PropertyTagResolutionXLengthUnit}
  PropertyTagResolutionYLengthUnit     = $5004;
  {$EXTERNALSYM PropertyTagResolutionYLengthUnit}
  PropertyTagPrintFlags                = $5005;
  {$EXTERNALSYM PropertyTagPrintFlags}
  PropertyTagPrintFlagsVersion         = $5006;
  {$EXTERNALSYM PropertyTagPrintFlagsVersion}
  PropertyTagPrintFlagsCrop            = $5007;
  {$EXTERNALSYM PropertyTagPrintFlagsCrop}
  PropertyTagPrintFlagsBleedWidth      = $5008;
  {$EXTERNALSYM PropertyTagPrintFlagsBleedWidth}
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  {$EXTERNALSYM PropertyTagPrintFlagsBleedWidthScale}
  PropertyTagHalftoneLPI               = $500A;
  {$EXTERNALSYM PropertyTagHalftoneLPI}
  PropertyTagHalftoneLPIUnit           = $500B;
  {$EXTERNALSYM PropertyTagHalftoneLPIUnit}
  PropertyTagHalftoneDegree            = $500C;
  {$EXTERNALSYM PropertyTagHalftoneDegree}
  PropertyTagHalftoneShape             = $500D;
  {$EXTERNALSYM PropertyTagHalftoneShape}
  PropertyTagHalftoneMisc              = $500E;
  {$EXTERNALSYM PropertyTagHalftoneMisc}
  PropertyTagHalftoneScreen            = $500F;
  {$EXTERNALSYM PropertyTagHalftoneScreen}
  PropertyTagJPEGQuality               = $5010;
  {$EXTERNALSYM PropertyTagJPEGQuality}
  PropertyTagGridSize                  = $5011;
  {$EXTERNALSYM PropertyTagGridSize}
  PropertyTagThumbnailFormat           = $5012;  // 1 = JPEG, 0 = RAW RGB
  {$EXTERNALSYM PropertyTagThumbnailFormat}
  PropertyTagThumbnailWidth            = $5013;
  {$EXTERNALSYM PropertyTagThumbnailWidth}
  PropertyTagThumbnailHeight           = $5014;
  {$EXTERNALSYM PropertyTagThumbnailHeight}
  PropertyTagThumbnailColorDepth       = $5015;
  {$EXTERNALSYM PropertyTagThumbnailColorDepth}
  PropertyTagThumbnailPlanes           = $5016;
  {$EXTERNALSYM PropertyTagThumbnailPlanes}
  PropertyTagThumbnailRawBytes         = $5017;
  {$EXTERNALSYM PropertyTagThumbnailRawBytes}
  PropertyTagThumbnailSize             = $5018;
  {$EXTERNALSYM PropertyTagThumbnailSize}
  PropertyTagThumbnailCompressedSize   = $5019;
  {$EXTERNALSYM PropertyTagThumbnailCompressedSize}
  PropertyTagColorTransferFunction     = $501A;
  {$EXTERNALSYM PropertyTagColorTransferFunction}
  PropertyTagThumbnailData             = $501B;    // RAW thumbnail bits in
  {$EXTERNALSYM PropertyTagThumbnailData}
                                                   // JPEG format or RGB format
                                                   // depends on
                                                   // PropertyTagThumbnailFormat

  // Thumbnail related TAGs

  PropertyTagThumbnailImageWidth        = $5020;   // Thumbnail width
  {$EXTERNALSYM PropertyTagThumbnailImageWidth}
  PropertyTagThumbnailImageHeight       = $5021;   // Thumbnail height
  {$EXTERNALSYM PropertyTagThumbnailImageHeight}
  PropertyTagThumbnailBitsPerSample     = $5022;   // Number of bits per
  {$EXTERNALSYM PropertyTagThumbnailBitsPerSample}
                                                   // component
  PropertyTagThumbnailCompression       = $5023;   // Compression Scheme
  {$EXTERNALSYM PropertyTagThumbnailCompression}
  PropertyTagThumbnailPhotometricInterp = $5024;   // Pixel composition
  {$EXTERNALSYM PropertyTagThumbnailPhotometricInterp}
  PropertyTagThumbnailImageDescription  = $5025;   // Image Tile
  {$EXTERNALSYM PropertyTagThumbnailImageDescription}
  PropertyTagThumbnailEquipMake         = $5026;   // Manufacturer of Image
  {$EXTERNALSYM PropertyTagThumbnailEquipMake}
                                                   // Input equipment
  PropertyTagThumbnailEquipModel        = $5027;   // Model of Image input
  {$EXTERNALSYM PropertyTagThumbnailEquipModel}
                                                   // equipment
  PropertyTagThumbnailStripOffsets    = $5028;  // Image data location
  {$EXTERNALSYM PropertyTagThumbnailStripOffsets}
  PropertyTagThumbnailOrientation     = $5029;  // Orientation of image
  {$EXTERNALSYM PropertyTagThumbnailOrientation}
  PropertyTagThumbnailSamplesPerPixel = $502A;  // Number of components
  {$EXTERNALSYM PropertyTagThumbnailSamplesPerPixel}
  PropertyTagThumbnailRowsPerStrip    = $502B;  // Number of rows per strip
  {$EXTERNALSYM PropertyTagThumbnailRowsPerStrip}
  PropertyTagThumbnailStripBytesCount = $502C;  // Bytes per compressed
  {$EXTERNALSYM PropertyTagThumbnailStripBytesCount}
                                                // strip
  PropertyTagThumbnailResolutionX     = $502D;  // Resolution in width
  {$EXTERNALSYM PropertyTagThumbnailResolutionX}
                                                // direction
  PropertyTagThumbnailResolutionY     = $502E;  // Resolution in height
  {$EXTERNALSYM PropertyTagThumbnailResolutionY}
                                                // direction
  PropertyTagThumbnailPlanarConfig    = $502F;  // Image data arrangement
  {$EXTERNALSYM PropertyTagThumbnailPlanarConfig}
  PropertyTagThumbnailResolutionUnit  = $5030;  // Unit of X and Y
  {$EXTERNALSYM PropertyTagThumbnailResolutionUnit}
                                                // Resolution
  PropertyTagThumbnailTransferFunction = $5031;  // Transfer function
  {$EXTERNALSYM PropertyTagThumbnailTransferFunction}
  PropertyTagThumbnailSoftwareUsed     = $5032;  // Software used
  {$EXTERNALSYM PropertyTagThumbnailSoftwareUsed}
  PropertyTagThumbnailDateTime         = $5033;  // File change date and
  {$EXTERNALSYM PropertyTagThumbnailDateTime}
                                                 // time
  PropertyTagThumbnailArtist          = $5034;  // Person who created the
  {$EXTERNALSYM PropertyTagThumbnailArtist}
                                                // image
  PropertyTagThumbnailWhitePoint      = $5035;  // White point chromaticity
  {$EXTERNALSYM PropertyTagThumbnailWhitePoint}
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  {$EXTERNALSYM PropertyTagThumbnailPrimaryChromaticities}
                                                    // Chromaticities of
                                                    // primaries
  PropertyTagThumbnailYCbCrCoefficients = $5037; // Color space transforma-
  {$EXTERNALSYM PropertyTagThumbnailYCbCrCoefficients}
                                                 // tion coefficients
  PropertyTagThumbnailYCbCrSubsampling = $5038;  // Subsampling ratio of Y
  {$EXTERNALSYM PropertyTagThumbnailYCbCrSubsampling}
                                                 // to C
  PropertyTagThumbnailYCbCrPositioning = $5039;  // Y and C position
  {$EXTERNALSYM PropertyTagThumbnailYCbCrPositioning}
  PropertyTagThumbnailRefBlackWhite    = $503A;  // Pair of black and white
  {$EXTERNALSYM PropertyTagThumbnailRefBlackWhite}
                                                 // reference values
  PropertyTagThumbnailCopyRight       = $503B;   // CopyRight holder
  {$EXTERNALSYM PropertyTagThumbnailCopyRight}

  PropertyTagLuminanceTable           = $5090;
  {$EXTERNALSYM PropertyTagLuminanceTable}
  PropertyTagChrominanceTable         = $5091;
  {$EXTERNALSYM PropertyTagChrominanceTable}

  PropertyTagFrameDelay               = $5100;
  {$EXTERNALSYM PropertyTagFrameDelay}
  PropertyTagLoopCount                = $5101;
  {$EXTERNALSYM PropertyTagLoopCount}

  PropertyTagPixelUnit         = $5110;  // Unit specifier for pixel/unit
  {$EXTERNALSYM PropertyTagPixelUnit}
  PropertyTagPixelPerUnitX     = $5111;  // Pixels per unit in X
  {$EXTERNALSYM PropertyTagPixelPerUnitX}
  PropertyTagPixelPerUnitY     = $5112;  // Pixels per unit in Y
  {$EXTERNALSYM PropertyTagPixelPerUnitY}
  PropertyTagPaletteHistogram  = $5113;  // Palette histogram
  {$EXTERNALSYM PropertyTagPaletteHistogram}

  // EXIF specific tag

  PropertyTagExifExposureTime  = $829A;
  {$EXTERNALSYM PropertyTagExifExposureTime}
  PropertyTagExifFNumber       = $829D;
  {$EXTERNALSYM PropertyTagExifFNumber}

  PropertyTagExifExposureProg  = $8822;
  {$EXTERNALSYM PropertyTagExifExposureProg}
  PropertyTagExifSpectralSense = $8824;
  {$EXTERNALSYM PropertyTagExifSpectralSense}
  PropertyTagExifISOSpeed      = $8827;
  {$EXTERNALSYM PropertyTagExifISOSpeed}
  PropertyTagExifOECF          = $8828;
  {$EXTERNALSYM PropertyTagExifOECF}

  PropertyTagExifVer           = $9000;
  {$EXTERNALSYM PropertyTagExifVer}
  PropertyTagExifDTOrig        = $9003; // Date & time of original
  {$EXTERNALSYM PropertyTagExifDTOrig}
  PropertyTagExifDTDigitized   = $9004; // Date & time of digital data generation
  {$EXTERNALSYM PropertyTagExifDTDigitized}

  PropertyTagExifCompConfig    = $9101;
  {$EXTERNALSYM PropertyTagExifCompConfig}
  PropertyTagExifCompBPP       = $9102;
  {$EXTERNALSYM PropertyTagExifCompBPP}

  PropertyTagExifShutterSpeed  = $9201;
  {$EXTERNALSYM PropertyTagExifShutterSpeed}
  PropertyTagExifAperture      = $9202;
  {$EXTERNALSYM PropertyTagExifAperture}
  PropertyTagExifBrightness    = $9203;
  {$EXTERNALSYM PropertyTagExifBrightness}
  PropertyTagExifExposureBias  = $9204;
  {$EXTERNALSYM PropertyTagExifExposureBias}
  PropertyTagExifMaxAperture   = $9205;
  {$EXTERNALSYM PropertyTagExifMaxAperture}
  PropertyTagExifSubjectDist   = $9206;
  {$EXTERNALSYM PropertyTagExifSubjectDist}
  PropertyTagExifMeteringMode  = $9207;
  {$EXTERNALSYM PropertyTagExifMeteringMode}
  PropertyTagExifLightSource   = $9208;
  {$EXTERNALSYM PropertyTagExifLightSource}
  PropertyTagExifFlash         = $9209;
  {$EXTERNALSYM PropertyTagExifFlash}
  PropertyTagExifFocalLength   = $920A;
  {$EXTERNALSYM PropertyTagExifFocalLength}
  PropertyTagExifMakerNote     = $927C;
  {$EXTERNALSYM PropertyTagExifMakerNote}
  PropertyTagExifUserComment   = $9286;
  {$EXTERNALSYM PropertyTagExifUserComment}
  PropertyTagExifDTSubsec      = $9290;  // Date & Time subseconds
  {$EXTERNALSYM PropertyTagExifDTSubsec}
  PropertyTagExifDTOrigSS      = $9291;  // Date & Time original subseconds
  {$EXTERNALSYM PropertyTagExifDTOrigSS}
  PropertyTagExifDTDigSS       = $9292;  // Date & TIme digitized subseconds
  {$EXTERNALSYM PropertyTagExifDTDigSS}

  PropertyTagExifFPXVer        = $A000;
  {$EXTERNALSYM PropertyTagExifFPXVer}
  PropertyTagExifColorSpace    = $A001;
  {$EXTERNALSYM PropertyTagExifColorSpace}
  PropertyTagExifPixXDim       = $A002;
  {$EXTERNALSYM PropertyTagExifPixXDim}
  PropertyTagExifPixYDim       = $A003;
  {$EXTERNALSYM PropertyTagExifPixYDim}
  PropertyTagExifRelatedWav    = $A004;  // related sound file
  {$EXTERNALSYM PropertyTagExifRelatedWav}
  PropertyTagExifInterop       = $A005;
  {$EXTERNALSYM PropertyTagExifInterop}
  PropertyTagExifFlashEnergy   = $A20B;
  {$EXTERNALSYM PropertyTagExifFlashEnergy}
  PropertyTagExifSpatialFR     = $A20C;  // Spatial Frequency Response
  {$EXTERNALSYM PropertyTagExifSpatialFR}
  PropertyTagExifFocalXRes     = $A20E;  // Focal Plane X Resolution
  {$EXTERNALSYM PropertyTagExifFocalXRes}
  PropertyTagExifFocalYRes     = $A20F;  // Focal Plane Y Resolution
  {$EXTERNALSYM PropertyTagExifFocalYRes}
  PropertyTagExifFocalResUnit  = $A210;  // Focal Plane Resolution Unit
  {$EXTERNALSYM PropertyTagExifFocalResUnit}
  PropertyTagExifSubjectLoc    = $A214;
  {$EXTERNALSYM PropertyTagExifSubjectLoc}
  PropertyTagExifExposureIndex = $A215;
  {$EXTERNALSYM PropertyTagExifExposureIndex}
  PropertyTagExifSensingMethod = $A217;
  {$EXTERNALSYM PropertyTagExifSensingMethod}
  PropertyTagExifFileSource    = $A300;
  {$EXTERNALSYM PropertyTagExifFileSource}
  PropertyTagExifSceneType     = $A301;
  {$EXTERNALSYM PropertyTagExifSceneType}
  PropertyTagExifCfaPattern    = $A302;
  {$EXTERNALSYM PropertyTagExifCfaPattern}

  PropertyTagGpsVer            = $0000;
  {$EXTERNALSYM PropertyTagGpsVer}
  PropertyTagGpsLatitudeRef    = $0001;
  {$EXTERNALSYM PropertyTagGpsLatitudeRef}
  PropertyTagGpsLatitude       = $0002;
  {$EXTERNALSYM PropertyTagGpsLatitude}
  PropertyTagGpsLongitudeRef   = $0003;
  {$EXTERNALSYM PropertyTagGpsLongitudeRef}
  PropertyTagGpsLongitude      = $0004;
  {$EXTERNALSYM PropertyTagGpsLongitude}
  PropertyTagGpsAltitudeRef    = $0005;
  {$EXTERNALSYM PropertyTagGpsAltitudeRef}
  PropertyTagGpsAltitude       = $0006;
  {$EXTERNALSYM PropertyTagGpsAltitude}
  PropertyTagGpsGpsTime        = $0007;
  {$EXTERNALSYM PropertyTagGpsGpsTime}
  PropertyTagGpsGpsSatellites  = $0008;
  {$EXTERNALSYM PropertyTagGpsGpsSatellites}
  PropertyTagGpsGpsStatus      = $0009;
  {$EXTERNALSYM PropertyTagGpsGpsStatus}
  PropertyTagGpsGpsMeasureMode = $00A;
  {$EXTERNALSYM PropertyTagGpsGpsMeasureMode}
  PropertyTagGpsGpsDop         = $000B;  // Measurement precision
  {$EXTERNALSYM PropertyTagGpsGpsDop}
  PropertyTagGpsSpeedRef       = $000C;
  {$EXTERNALSYM PropertyTagGpsSpeedRef}
  PropertyTagGpsSpeed          = $000D;
  {$EXTERNALSYM PropertyTagGpsSpeed}
  PropertyTagGpsTrackRef       = $000E;
  {$EXTERNALSYM PropertyTagGpsTrackRef}
  PropertyTagGpsTrack          = $000F;
  {$EXTERNALSYM PropertyTagGpsTrack}
  PropertyTagGpsImgDirRef      = $0010;
  {$EXTERNALSYM PropertyTagGpsImgDirRef}
  PropertyTagGpsImgDir         = $0011;
  {$EXTERNALSYM PropertyTagGpsImgDir}
  PropertyTagGpsMapDatum       = $0012;
  {$EXTERNALSYM PropertyTagGpsMapDatum}
  PropertyTagGpsDestLatRef     = $0013;
  {$EXTERNALSYM PropertyTagGpsDestLatRef}
  PropertyTagGpsDestLat        = $0014;
  {$EXTERNALSYM PropertyTagGpsDestLat}
  PropertyTagGpsDestLongRef    = $0015;
  {$EXTERNALSYM PropertyTagGpsDestLongRef}
  PropertyTagGpsDestLong       = $0016;
  {$EXTERNALSYM PropertyTagGpsDestLong}
  PropertyTagGpsDestBearRef    = $0017;
  {$EXTERNALSYM PropertyTagGpsDestBearRef}
  PropertyTagGpsDestBear       = $0018;
  {$EXTERNALSYM PropertyTagGpsDestBear}
  PropertyTagGpsDestDistRef    = $0019;
  {$EXTERNALSYM PropertyTagGpsDestDistRef}
  PropertyTagGpsDestDist       = $001A;
  {$EXTERNALSYM PropertyTagGpsDestDist}

(**************************************************************************\
*
*  GDI+ Color Matrix object, used with Graphics.DrawImage
*
\**************************************************************************)

//----------------------------------------------------------------------------
// Color matrix
//----------------------------------------------------------------------------

type
  {$EXTERNALSYM ColorMatrix}
  ColorMatrix = packed array[0..4, 0..4] of Single;
  TColorMatrix = ColorMatrix;
  PColorMatrix = ^TColorMatrix;

//----------------------------------------------------------------------------
// Color Matrix flags
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMatrixFlags}
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault   = 0,
    ColorMatrixFlagsSkipGrays = 1,
    ColorMatrixFlagsAltGray   = 2
  );
  TColorMatrixFlags = ColorMatrixFlags;

//----------------------------------------------------------------------------
// Color Adjust Type
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorAdjustType}
  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny      // Reserved
  );
  TColorAdjustType = ColorAdjustType;

//----------------------------------------------------------------------------
// Color Map
//----------------------------------------------------------------------------

  {$EXTERNALSYM ColorMap}
  ColorMap = packed record
    oldColor: TColor;
    newColor: TColor;
  end;
  TColorMap = ColorMap;
  PColorMap = ^TColorMap;

//---------------------------------------------------------------------------
// Private GDI+ classes for internal type checking
//---------------------------------------------------------------------------

  GpGraphics = Pointer;

  GpBrush = Pointer;
  GpTexture = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpPathGradient = Pointer;
  GpHatch =  Pointer;

  GpPen = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;

  GpImage = Pointer;
  GpBitmap = Pointer;
  GpMetafile = Pointer;
  GpImageAttributes = Pointer;

  GpPath = Pointer;
  GpRegion = Pointer;
  GpPathIterator = Pointer;

  GpFontFamily = Pointer;
  GpFont = Pointer;
  GpStringFormat = Pointer;
  GpFontCollection = Pointer;
  GpCachedBitmap = Pointer;

  GpStatus          = TStatus;
  GpFillMode        = TFillMode;
  GpWrapMode        = TWrapMode;
  GpUnit            = TUnit;
  GpCoordinateSpace = TCoordinateSpace;
  GpPointF          = PPointF;
  GpPoint           = PPoint;
  GpRectF           = PRectF;
  GpRect            = PRect;
  GpSizeF           = PSizeF;
  GpHatchStyle      = THatchStyle;
  GpDashStyle       = TDashStyle;
  GpLineCap         = TLineCap;
  GpDashCap         = TDashCap;

  GpPenAlignment    = TPenAlignment;

  GpLineJoin        = TLineJoin;
  GpPenType         = TPenType;

  GpMatrix          = Pointer; 
  GpBrushType       = TBrushType;
  GpMatrixOrder     = TMatrixOrder;
  GpFlushIntention  = TFlushIntention;
  GpPathData        = TPathData;

  // LOGFONT //

  PLogFontA = ^TLogFontA;
  tagLOGFONTA = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: BYTE;
    lfUnderline: BYTE;
    lfStrikeOut: BYTE;
    lfCharSet: BYTE;
    lfOutPrecision: BYTE;
    lfClipPrecision: BYTE;
    lfQuality: BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName: array [0..LF_FACESIZE - 1] of CHAR;
  end;
  {$EXTERNALSYM tagLOGFONTA}
  LOGFONTA = tagLOGFONTA;
  {$EXTERNALSYM LOGFONTA}
  LPLOGFONTA = ^LOGFONTA;
  {$EXTERNALSYM LPLOGFONTA}
  NPLOGFONTA = ^LOGFONTA;
  {$EXTERNALSYM NPLOGFONTA}
  TLogFontA = LOGFONTA;

  PLogFontW = ^TLogFontW;
  tagLOGFONTW = record
    lfHeight: LONG;
    lfWidth: LONG;
    lfEscapement: LONG;
    lfOrientation: LONG;
    lfWeight: LONG;
    lfItalic: BYTE;
    lfUnderline: BYTE;
    lfStrikeOut: BYTE;
    lfCharSet: BYTE;
    lfOutPrecision: BYTE;
    lfClipPrecision: BYTE;
    lfQuality: BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName: array [0..LF_FACESIZE - 1] of WCHAR;
  end;
  {$EXTERNALSYM tagLOGFONTW}
  LOGFONTW = tagLOGFONTW;
  {$EXTERNALSYM LOGFONTW}
  LPLOGFONTW = ^LOGFONTW;
  {$EXTERNALSYM LPLOGFONTW}
  NPLOGFONTW = ^LOGFONTW;
  {$EXTERNALSYM NPLOGFONTW}
  TLogFontW = LOGFONTW;

  {$IFDEF UNICODE}
  LOGFONT = LOGFONTW;
  {$EXTERNALSYM LOGFONT}
  PLOGFONT = PLOGFONTW;
  {$EXTERNALSYM PLOGFONT}
  NPLOGFONT = NPLOGFONTW;
  {$EXTERNALSYM NPLOGFONT}
  LPLOGFONT = LPLOGFONTW;
  {$EXTERNALSYM LPLOGFONT}
  TLogFont = TLogFontW;
  {$ELSE}
  LOGFONT = LOGFONTA;
  {$EXTERNALSYM LOGFONT}
  PLOGFONT = PLOGFONTA;
  {$EXTERNALSYM PLOGFONT}
  NPLOGFONT = NPLOGFONTA;
  {$EXTERNALSYM NPLOGFONT}
  LPLOGFONT = LPLOGFONTA;
  {$EXTERNALSYM LPLOGFONT}
  TLogFont = TLogFontA;
  {$ENDIF UNICODE}


(**************************************************************************\
*
* Copyright (c) 1998-2001, Microsoft Corp.  All Rights Reserved.
* Module Name:
*   GdiplusFlat.h
* Abstract:
*   Private GDI+ header file.
*
\**************************************************************************)

  function GdipCreatePath(brushMode: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePath}

  function GdipCreatePath2(v1: GPPOINTF; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePath2}

  function GdipCreatePath2I(v1: GPPOINT; v2: PBYTE; v3: Integer; v4: GPFILLMODE;
    out path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePath2I}

  function GdipClonePath(path: GPPATH;
    out clonePath: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipClonePath}

  function GdipDeletePath(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeletePath}

  function GdipResetPath(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetPath}

  function GdipGetPointCount(path: GPPATH;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPointCount}

  function GdipGetPathTypes(path: GPPATH; types: PBYTE;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathTypes}

  function GdipGetPathPoints(v1: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathPoints}

  function GdipGetPathPointsI(v1: GPPATH; points: GPPOINT;
             count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathPointsI}

  function GdipGetPathFillMode(path: GPPATH;
    var fillmode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathFillMode}

  function GdipSetPathFillMode(path: GPPATH;
    fillmode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathFillMode}

  function GdipGetPathData(path: GPPATH;
    pathData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathData}

  function GdipStartPathFigure(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipStartPathFigure}

  function GdipClosePathFigure(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipClosePathFigure}

  function GdipClosePathFigures(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipClosePathFigures}

  function GdipSetPathMarker(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathMarker}

  function GdipClearPathMarkers(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipClearPathMarkers}

  function GdipReversePath(path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipReversePath}

  function GdipGetPathLastPoint(path: GPPATH;
    lastPoint: GPPOINTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathLastPoint}

  function GdipAddPathLine(path: GPPATH;
    x1, y1, x2, y2: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathLine}

  function GdipAddPathLine2(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathLine2}

  function GdipAddPathArc(path: GPPATH; x, y, width, height, startAngle,
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathArc}

  function GdipAddPathBezier(path: GPPATH;
    x1, y1, x2, y2, x3, y3, x4, y4: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathBezier}

  function GdipAddPathBeziers(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathBeziers}

  function GdipAddPathCurve(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurve}

  function GdipAddPathCurve2(path: GPPATH; points: GPPOINTF; count: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurve2}

  function GdipAddPathCurve3(path: GPPATH; points: GPPOINTF; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurve3}

  function GdipAddPathClosedCurve(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathClosedCurve}

  function GdipAddPathClosedCurve2(path: GPPATH; points: GPPOINTF;
    count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathClosedCurve2}

  function GdipAddPathRectangle(path: GPPATH; x: Single; y: Single;
    width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathRectangle}

  function GdipAddPathRectangles(path: GPPATH; rects: GPRECTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathRectangles}

  function GdipAddPathEllipse(path: GPPATH;  x: Single; y: Single;
    width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathEllipse}

  function GdipAddPathPie(path: GPPATH; x: Single; y: Single; width: Single;
    height: Single; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathPie}

  function GdipAddPathPolygon(path: GPPATH; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathPolygon}

  function GdipAddPathPath(path: GPPATH; addingPath: GPPATH;
    connect: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathPath}

  function GdipAddPathString(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PRECTF;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathString}

  function GdipAddPathStringI(path: GPPATH; string_: PWCHAR; length: Integer;
    family: GPFONTFAMILY; style: Integer; emSize: Single; layoutRect: PRECT;
    format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathStringI}

  function GdipAddPathLineI(path: GPPATH; x1: Integer; y1: Integer; x2: Integer;
    y2: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathLineI}

  function GdipAddPathLine2I(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathLine2I}

  function GdipAddPathArcI(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathArcI}

  function GdipAddPathBezierI(path: GPPATH; x1: Integer; y1: Integer;
    x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer;
    y4: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathBezierI}

  function GdipAddPathBeziersI(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathBeziersI}

  function GdipAddPathCurveI(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurveI}

  function GdipAddPathCurve2I(path: GPPATH; points: GPPOINT; count: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurve2I}

  function GdipAddPathCurve3I(path: GPPATH; points: GPPOINT; count: Integer;
    offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathCurve3I}

  function GdipAddPathClosedCurveI(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathClosedCurveI}

  function GdipAddPathClosedCurve2I(path: GPPATH; points: GPPOINT;
    count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathClosedCurve2I}

  function GdipAddPathRectangleI(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathRectangleI}

  function GdipAddPathRectanglesI(path: GPPATH; rects: GPRECT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathRectanglesI}

  function GdipAddPathEllipseI(path: GPPATH; x: Integer; y: Integer;
    width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathEllipseI}

  function GdipAddPathPieI(path: GPPATH; x: Integer; y: Integer; width: Integer;
    height: Integer; startAngle: Single; sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathPieI}

  function GdipAddPathPolygonI(path: GPPATH; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipAddPathPolygonI}

  function GdipFlattenPath(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFlattenPath}

  function GdipWindingModeOutline(path: GPPATH; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipWindingModeOutline}

  function GdipWidenPath(nativePath: GPPATH; pen: GPPEN; matrix: GPMATRIX;
    flatness: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipWidenPath}

  function GdipWarpPath(path: GPPATH; matrix: GPMATRIX; points: GPPOINTF;
    count: Integer; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; warpMode: WARPMODE; flatness: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipWarpPath}

  function GdipTransformPath(path: GPPATH; matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformPath}

  function GdipGetPathWorldBounds(path: GPPATH; bounds: GPRECTF;
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathWorldBounds}

  function GdipGetPathWorldBoundsI(path: GPPATH; bounds: GPRECT;
    matrix: GPMATRIX; pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathWorldBoundsI}

  function GdipIsVisiblePathPoint(path: GPPATH; x: Single; y: Single;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisiblePathPoint}

  function GdipIsVisiblePathPointI(path: GPPATH; x: Integer; y: Integer;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisiblePathPointI}

  function GdipIsOutlineVisiblePathPoint(path: GPPATH; x: Single; y: Single;
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsOutlineVisiblePathPoint}

  function GdipIsOutlineVisiblePathPointI(path: GPPATH; x: Integer; y: Integer;
    pen: GPPEN; graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsOutlineVisiblePathPointI}

//----------------------------------------------------------------------------
// PathIterator APIs 
//----------------------------------------------------------------------------

  function GdipCreatePathIter(out iterator: GPPATHITERATOR;
    path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePathIter}

  function GdipDeletePathIter(iterator: GPPATHITERATOR): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeletePathIter}

  function GdipPathIterNextSubpath(iterator: GPPATHITERATOR;
    var resultCount: Integer; var startIndex: Integer; var endIndex: Integer;
    out isClosed: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterNextSubpath}

  function GdipPathIterNextSubpathPath(iterator: GPPATHITERATOR;
    var resultCount: Integer; path: GPPATH;
    out isClosed: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterNextSubpathPath}

  function GdipPathIterNextPathType(iterator: GPPATHITERATOR;
    var resultCount: Integer; pathType: PBYTE; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterNextPathType}

  function GdipPathIterNextMarker(iterator: GPPATHITERATOR;
    var resultCount: Integer; var startIndex: Integer;
    var endIndex: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterNextMarker}

  function GdipPathIterNextMarkerPath(iterator: GPPATHITERATOR;
    var resultCount: Integer; path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterNextMarkerPath}

  function GdipPathIterGetCount(iterator: GPPATHITERATOR;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterGetCount}

  function GdipPathIterGetSubpathCount(iterator: GPPATHITERATOR;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterGetSubpathCount}

  function GdipPathIterIsValid(iterator: GPPATHITERATOR;
    out valid: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterIsValid}

  function GdipPathIterHasCurve(iterator: GPPATHITERATOR;
    out hasCurve: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterHasCurve}

  function GdipPathIterRewind(iterator: GPPATHITERATOR): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterRewind}

  function GdipPathIterEnumerate(iterator: GPPATHITERATOR;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterEnumerate}

  function GdipPathIterCopyData(iterator: GPPATHITERATOR;
    var resultCount: Integer; points: GPPOINTF; types: PBYTE;
    startIndex: Integer; endIndex: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPathIterCopyData}

//----------------------------------------------------------------------------
// Matrix APIs
//----------------------------------------------------------------------------

  function GdipCreateMatrix(out matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMatrix}

  function GdipCreateMatrix2(m11: Single; m12: Single; m21: Single; m22: Single;
    dx: Single; dy: Single; out matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMatrix2}

  function GdipCreateMatrix3(rect: GPRECTF; dstplg: GPPOINTF;
    out matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMatrix3}

  function GdipCreateMatrix3I(rect: GPRECT; dstplg: GPPOINT;
    out matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMatrix3I}

  function GdipCloneMatrix(matrix: GPMATRIX;
    out cloneMatrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneMatrix}

  function GdipDeleteMatrix(matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteMatrix}

  function GdipSetMatrixElements(matrix: GPMATRIX; m11: Single; m12: Single;
    m21: Single; m22: Single; dx: Single; dy: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetMatrixElements}

  function GdipMultiplyMatrix(matrix: GPMATRIX; matrix2: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyMatrix}

  function GdipTranslateMatrix(matrix: GPMATRIX; offsetX: Single;
    offsetY: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateMatrix}

  function GdipScaleMatrix(matrix: GPMATRIX; scaleX: Single; scaleY: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScaleMatrix}

  function GdipRotateMatrix(matrix: GPMATRIX; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotateMatrix}

  function GdipShearMatrix(matrix: GPMATRIX; shearX: Single; shearY: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipShearMatrix}

  function GdipInvertMatrix(matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipInvertMatrix}

  function GdipTransformMatrixPoints(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformMatrixPoints}

  function GdipTransformMatrixPointsI(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformMatrixPointsI}

  function GdipVectorTransformMatrixPoints(matrix: GPMATRIX; pts: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipVectorTransformMatrixPoints}

  function GdipVectorTransformMatrixPointsI(matrix: GPMATRIX; pts: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipVectorTransformMatrixPointsI}

  function GdipGetMatrixElements(matrix: GPMATRIX;
    matrixOut: PSingle): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMatrixElements}

  function GdipIsMatrixInvertible(matrix: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsMatrixInvertible}

  function GdipIsMatrixIdentity(matrix: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsMatrixIdentity}

  function GdipIsMatrixEqual(matrix: GPMATRIX; matrix2: GPMATRIX;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsMatrixEqual}

//----------------------------------------------------------------------------
// Region APIs
//----------------------------------------------------------------------------

  function GdipCreateRegion(out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegion}

  function GdipCreateRegionRect(rect: GPRECTF;
    out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegionRect}

  function GdipCreateRegionRectI(rect: GPRECT;
    out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegionRectI}

  function GdipCreateRegionPath(path: GPPATH;
    out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegionPath}

  function GdipCreateRegionRgnData(regionData: PBYTE; size: Integer;
    out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegionRgnData}

  function GdipCreateRegionHrgn(hRgn: HRGN;
    out region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateRegionHrgn}

  function GdipCloneRegion(region: GPREGION;
    out cloneRegion: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneRegion}

  function GdipDeleteRegion(region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteRegion}

  function GdipSetInfinite(region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetInfinite}

  function GdipSetEmpty(region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetEmpty}

  function GdipCombineRegionRect(region: GPREGION; rect: GPRECTF;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCombineRegionRect}

  function GdipCombineRegionRectI(region: GPREGION; rect: GPRECT;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCombineRegionRectI}

  function GdipCombineRegionPath(region: GPREGION; path: GPPATH;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCombineRegionPath}

  function GdipCombineRegionRegion(region: GPREGION; region2: GPREGION;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCombineRegionRegion}

  function GdipTranslateRegion(region: GPREGION; dx: Single;
    dy: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateRegion}

  function GdipTranslateRegionI(region: GPREGION; dx: Integer;
    dy: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateRegionI}

  function GdipTransformRegion(region: GPREGION;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformRegion}

  function GdipGetRegionBounds(region: GPREGION; graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionBounds}

  function GdipGetRegionBoundsI(region: GPREGION; graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionBoundsI}

  function GdipGetRegionHRgn(region: GPREGION; graphics: GPGRAPHICS;
    out hRgn: HRGN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionHRgn}

  function GdipIsEmptyRegion(region: GPREGION; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsEmptyRegion}

  function GdipIsInfiniteRegion(region: GPREGION; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsInfiniteRegion}

  function GdipIsEqualRegion(region: GPREGION; region2: GPREGION;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsEqualRegion}

  function GdipGetRegionDataSize(region: GPREGION;
    out bufferSize: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionDataSize}

  function GdipGetRegionData(region: GPREGION; buffer: PBYTE;
    bufferSize: UINT; sizeFilled: PUINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionData}

  function GdipIsVisibleRegionPoint(region: GPREGION; x: Single; y: Single;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRegionPoint}

  function GdipIsVisibleRegionPointI(region: GPREGION; x: Integer; y: Integer;
    graphics: GPGRAPHICS; out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRegionPointI}

  function GdipIsVisibleRegionRect(region: GPREGION; x: Single; y: Single;
    width: Single; height: Single; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRegionRect}

  function GdipIsVisibleRegionRectI(region: GPREGION; x: Integer; y: Integer;
    width: Integer; height: Integer; graphics: GPGRAPHICS;
    out result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRegionRectI}

  function GdipGetRegionScansCount(region: GPREGION; out count: UINT;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionScansCount}

  function GdipGetRegionScans(region: GPREGION; rects: GPRECTF;
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionScans}

  function GdipGetRegionScansI(region: GPREGION; rects: GPRECT;
    out count: Integer; matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRegionScansI}

//----------------------------------------------------------------------------
// Brush APIs
//----------------------------------------------------------------------------

  function GdipCloneBrush(brush: GPBRUSH;
    out cloneBrush: GPBRUSH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneBrush}

  function GdipDeleteBrush(brush: GPBRUSH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteBrush}

  function GdipGetBrushType(brush: GPBRUSH;
    out type_: GPBRUSHTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetBrushType}

//----------------------------------------------------------------------------
// HatchBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateHatchBrush(hatchstyle: Integer; forecol: ARGB;
    backcol: ARGB; out brush: GPHATCH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateHatchBrush}

  function GdipGetHatchStyle(brush: GPHATCH;
    out hatchstyle: GPHATCHSTYLE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetHatchStyle}

  function GdipGetHatchForegroundColor(brush: GPHATCH;
    out forecol: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetHatchForegroundColor}

  function GdipGetHatchBackgroundColor(brush: GPHATCH;
    out backcol: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetHatchBackgroundColor}

//----------------------------------------------------------------------------
// TextureBrush APIs
//----------------------------------------------------------------------------


  function GdipCreateTexture(image: GPIMAGE; wrapmode: GPWRAPMODE;
    var texture: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateTexture}

  function GdipCreateTexture2(image: GPIMAGE; wrapmode: GPWRAPMODE;
    x: Single; y: Single; width: Single; height: Single;
    out texture: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateTexture2}

  function GdipCreateTextureIA(image: GPIMAGE;
    imageAttributes: GPIMAGEATTRIBUTES; x: Single; y: Single; width: Single;
    height: Single; out texture: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateTextureIA}

  function GdipCreateTexture2I(image: GPIMAGE; wrapmode: GPWRAPMODE; x: Integer;
    y: Integer; width: Integer; height: Integer;
    out texture: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateTexture2I}

  function GdipCreateTextureIAI(image: GPIMAGE;
    imageAttributes: GPIMAGEATTRIBUTES; x: Integer; y: Integer; width: Integer;
    height: Integer; out texture: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateTextureIAI}

  function GdipGetTextureTransform(brush: GPTEXTURE;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetTextureTransform}

  function GdipSetTextureTransform(brush: GPTEXTURE;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetTextureTransform}

  function GdipResetTextureTransform(brush: GPTEXTURE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetTextureTransform}

  function GdipMultiplyTextureTransform(brush: GPTEXTURE; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyTextureTransform}

  function GdipTranslateTextureTransform(brush: GPTEXTURE; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateTextureTransform}

  function GdipScaleTextureTransform(brush: GPTEXTURE; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScaleTextureTransform}

  function GdipRotateTextureTransform(brush: GPTEXTURE; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotateTextureTransform}

  function GdipSetTextureWrapMode(brush: GPTEXTURE;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetTextureWrapMode}

  function GdipGetTextureWrapMode(brush: GPTEXTURE;
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetTextureWrapMode}

  function GdipGetTextureImage(brush: GPTEXTURE;
    out image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetTextureImage}

//----------------------------------------------------------------------------
// SolidBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateSolidFill(color: ARGB;
    out brush: GPSOLIDFILL): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateSolidFill}

  function GdipSetSolidFillColor(brush: GPSOLIDFILL;
    color: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetSolidFillColor}

  function GdipGetSolidFillColor(brush: GPSOLIDFILL;
    out color: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetSolidFillColor}

//----------------------------------------------------------------------------
// LineBrush APIs
//----------------------------------------------------------------------------

  function GdipCreateLineBrush(point1: GPPOINTF; point2: GPPOINTF; color1: ARGB;
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrush}

  function GdipCreateLineBrushI(point1: GPPOINT; point2: GPPOINT; color1: ARGB;
    color2: ARGB; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrushI}

  function GdipCreateLineBrushFromRect(rect: GPRECTF; color1: ARGB;
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrushFromRect}

  function GdipCreateLineBrushFromRectI(rect: GPRECT; color1: ARGB;
    color2: ARGB; mode: LINEARGRADIENTMODE; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrushFromRectI}

  function GdipCreateLineBrushFromRectWithAngle(rect: GPRECTF; color1: ARGB;
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrushFromRectWithAngle}

  function GdipCreateLineBrushFromRectWithAngleI(rect: GPRECT; color1: ARGB;
    color2: ARGB; angle: Single; isAngleScalable: Bool; wrapMode: GPWRAPMODE;
    out lineGradient: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateLineBrushFromRectWithAngleI}

  function GdipSetLineColors(brush: GPLINEGRADIENT; color1: ARGB;
    color2: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineColors}

  function GdipGetLineColors(brush: GPLINEGRADIENT;
    colors: PARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineColors}

  function GdipGetLineRect(brush: GPLINEGRADIENT;
    rect: GPRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineRect}

  function GdipGetLineRectI(brush: GPLINEGRADIENT;
    rect: GPRECT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineRectI}

  function GdipSetLineGammaCorrection(brush: GPLINEGRADIENT;
    useGammaCorrection: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineGammaCorrection}

  function GdipGetLineGammaCorrection(brush: GPLINEGRADIENT;
    out useGammaCorrection: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineGammaCorrection}

  function GdipGetLineBlendCount(brush: GPLINEGRADIENT;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineBlendCount}

  function GdipGetLineBlend(brush: GPLINEGRADIENT; blend: PSingle;
    positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineBlend}

  function GdipSetLineBlend(brush: GPLINEGRADIENT; blend: PSingle;
    positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineBlend}

  function GdipGetLinePresetBlendCount(brush: GPLINEGRADIENT;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLinePresetBlendCount}

  function GdipGetLinePresetBlend(brush: GPLINEGRADIENT; blend: PARGB;
    positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLinePresetBlend}

  function GdipSetLinePresetBlend(brush: GPLINEGRADIENT; blend: PARGB;
    positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLinePresetBlend}

  function GdipSetLineSigmaBlend(brush: GPLINEGRADIENT; focus: Single;
    scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineSigmaBlend}

  function GdipSetLineLinearBlend(brush: GPLINEGRADIENT; focus: Single;
    scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineLinearBlend}

  function GdipSetLineWrapMode(brush: GPLINEGRADIENT;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineWrapMode}

  function GdipGetLineWrapMode(brush: GPLINEGRADIENT;
    out wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineWrapMode}

  function GdipGetLineTransform(brush: GPLINEGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineTransform}

  function GdipSetLineTransform(brush: GPLINEGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetLineTransform}

  function GdipResetLineTransform(brush: GPLINEGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetLineTransform}

  function GdipMultiplyLineTransform(brush: GPLINEGRADIENT; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyLineTransform}

  function GdipTranslateLineTransform(brush: GPLINEGRADIENT; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateLineTransform}

  function GdipScaleLineTransform(brush: GPLINEGRADIENT; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScaleLineTransform}

  function GdipRotateLineTransform(brush: GPLINEGRADIENT; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotateLineTransform}

//----------------------------------------------------------------------------
// PathGradientBrush APIs
//----------------------------------------------------------------------------

  function GdipCreatePathGradient(points: GPPOINTF; count: Integer;
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePathGradient}

  function GdipCreatePathGradientI(points: GPPOINT; count: Integer;
    wrapMode: GPWRAPMODE; out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePathGradientI}

  function GdipCreatePathGradientFromPath(path: GPPATH;
    out polyGradient: GPPATHGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePathGradientFromPath}

  function GdipGetPathGradientCenterColor(brush: GPPATHGRADIENT;
    out colors: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientCenterColor}

  function GdipSetPathGradientCenterColor(brush: GPPATHGRADIENT;
    colors: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientCenterColor}

  function GdipGetPathGradientSurroundColorsWithCount(brush: GPPATHGRADIENT;
    color: PARGB; var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientSurroundColorsWithCount}

  function GdipSetPathGradientSurroundColorsWithCount(brush: GPPATHGRADIENT;
    color: PARGB; var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientSurroundColorsWithCount}

  function GdipGetPathGradientPath(brush: GPPATHGRADIENT;
    path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientPath}

  function GdipSetPathGradientPath(brush: GPPATHGRADIENT;
    path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientPath}

  function GdipGetPathGradientCenterPoint(brush: GPPATHGRADIENT;
    points: GPPOINTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientCenterPoint}

  function GdipGetPathGradientCenterPointI(brush: GPPATHGRADIENT;
    points: GPPOINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientCenterPointI}

  function GdipSetPathGradientCenterPoint(brush: GPPATHGRADIENT;
    points: GPPOINTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientCenterPoint}

  function GdipSetPathGradientCenterPointI(brush: GPPATHGRADIENT;
    points: GPPOINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientCenterPointI}

  function GdipGetPathGradientRect(brush: GPPATHGRADIENT;
    rect: GPRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientRect}

  function GdipGetPathGradientRectI(brush: GPPATHGRADIENT;
    rect: GPRECT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientRectI}

  function GdipGetPathGradientPointCount(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientPointCount}

  function GdipGetPathGradientSurroundColorCount(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientSurroundColorCount}

  function GdipSetPathGradientGammaCorrection(brush: GPPATHGRADIENT;
    useGammaCorrection: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientGammaCorrection}

  function GdipGetPathGradientGammaCorrection(brush: GPPATHGRADIENT;
    var useGammaCorrection: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientGammaCorrection}

  function GdipGetPathGradientBlendCount(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientBlendCount}

  function GdipGetPathGradientBlend(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientBlend}

  function GdipSetPathGradientBlend(brush: GPPATHGRADIENT;
    blend: PSingle; positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientBlend}

  function GdipGetPathGradientPresetBlendCount(brush: GPPATHGRADIENT;
    var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientPresetBlendCount}

  function GdipGetPathGradientPresetBlend(brush: GPPATHGRADIENT;
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientPresetBlend}

  function GdipSetPathGradientPresetBlend(brush: GPPATHGRADIENT;
    blend: PARGB; positions: PSingle; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientPresetBlend}

  function GdipSetPathGradientSigmaBlend(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientSigmaBlend}

  function GdipSetPathGradientLinearBlend(brush: GPPATHGRADIENT;
    focus: Single; scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientLinearBlend}

  function GdipGetPathGradientWrapMode(brush: GPPATHGRADIENT;
    var wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientWrapMode}

  function GdipSetPathGradientWrapMode(brush: GPPATHGRADIENT;
    wrapmode: GPWRAPMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientWrapMode}

  function GdipGetPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientTransform}

  function GdipSetPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientTransform}

  function GdipResetPathGradientTransform(
    brush: GPPATHGRADIENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetPathGradientTransform}

  function GdipMultiplyPathGradientTransform(brush: GPPATHGRADIENT;
    matrix: GPMATRIX; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyPathGradientTransform}

  function GdipTranslatePathGradientTransform(brush: GPPATHGRADIENT;
    dx: Single; dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslatePathGradientTransform}

  function GdipScalePathGradientTransform(brush: GPPATHGRADIENT;
    sx: Single; sy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScalePathGradientTransform}

  function GdipRotatePathGradientTransform(brush: GPPATHGRADIENT;
    angle: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotatePathGradientTransform}

  function GdipGetPathGradientFocusScales(brush: GPPATHGRADIENT;
    var xScale: Single; var yScale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPathGradientFocusScales}

  function GdipSetPathGradientFocusScales(brush: GPPATHGRADIENT;
    xScale: Single; yScale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPathGradientFocusScales}

//----------------------------------------------------------------------------
// Pen APIs
//----------------------------------------------------------------------------

  function GdipCreatePen1(color: ARGB; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePen1}

  function GdipCreatePen2(brush: GPBRUSH; width: Single; unit_: GPUNIT;
    out pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreatePen2}

  function GdipClonePen(pen: GPPEN; out clonepen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipClonePen}

  function GdipDeletePen(pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeletePen}

  function GdipSetPenWidth(pen: GPPEN; width: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenWidth}

  function GdipGetPenWidth(pen: GPPEN; out width: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenWidth}

  function GdipSetPenUnit(pen: GPPEN; unit_: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenUnit}

  function GdipGetPenUnit(pen: GPPEN; var unit_: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenUnit}

  function GdipSetPenLineCap197819(pen: GPPEN; startCap: GPLINECAP;
    endCap: GPLINECAP; dashCap: GPDASHCAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenLineCap197819}

  function GdipSetPenStartCap(pen: GPPEN;
    startCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenStartCap}

  function GdipSetPenEndCap(pen: GPPEN; endCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenEndCap}

  function GdipSetPenDashCap197819(pen: GPPEN;
    dashCap: GPDASHCAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenDashCap197819}

  function GdipGetPenStartCap(pen: GPPEN;
    out startCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenStartCap}

  function GdipGetPenEndCap(pen: GPPEN;
    out endCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenEndCap}

  function GdipGetPenDashCap197819(pen: GPPEN;
    out dashCap: GPDASHCAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenDashCap197819}

  function GdipSetPenLineJoin(pen: GPPEN;
    lineJoin: GPLINEJOIN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenLineJoin}

  function GdipGetPenLineJoin(pen: GPPEN;
    var lineJoin: GPLINEJOIN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenLineJoin}

  function GdipSetPenCustomStartCap(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenCustomStartCap}

  function GdipGetPenCustomStartCap(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenCustomStartCap}

  function GdipSetPenCustomEndCap(pen: GPPEN;
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenCustomEndCap}

  function GdipGetPenCustomEndCap(pen: GPPEN;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenCustomEndCap}

  function GdipSetPenMiterLimit(pen: GPPEN;
    miterLimit: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenMiterLimit}

  function GdipGetPenMiterLimit(pen: GPPEN;
    out miterLimit: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenMiterLimit}

  function GdipSetPenMode(pen: GPPEN;
    penMode: GPPENALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenMode}

  function GdipGetPenMode(pen: GPPEN;
    var penMode: GPPENALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenMode}

  function GdipSetPenTransform(pen: GPPEN;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenTransform}

  function GdipGetPenTransform(pen: GPPEN;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenTransform}

  function GdipResetPenTransform(pen: GPPEN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetPenTransform}

  function GdipMultiplyPenTransform(pen: GPPEN; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyPenTransform}

  function GdipTranslatePenTransform(pen: GPPEN; dx: Single; dy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslatePenTransform}

  function GdipScalePenTransform(pen: GPPEN; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScalePenTransform}

  function GdipRotatePenTransform(pen: GPPEN; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotatePenTransform}

  function GdipSetPenColor(pen: GPPEN; argb: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenColor}

  function GdipGetPenColor(pen: GPPEN; out argb: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenColor}

  function GdipSetPenBrushFill(pen: GPPEN; brush: GPBRUSH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenBrushFill}

  function GdipGetPenBrushFill(pen: GPPEN;
    out brush: GPBRUSH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenBrushFill}

  function GdipGetPenFillType(pen: GPPEN;
    out type_: GPPENTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenFillType}

  function GdipGetPenDashStyle(pen: GPPEN;
    out dashstyle: GPDASHSTYLE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenDashStyle}

  function GdipSetPenDashStyle(pen: GPPEN;
    dashstyle: GPDASHSTYLE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenDashStyle}

  function GdipGetPenDashOffset(pen: GPPEN;
    out offset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenDashOffset}

  function GdipSetPenDashOffset(pen: GPPEN; offset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenDashOffset}

  function GdipGetPenDashCount(pen: GPPEN;
    var count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenDashCount}

  function GdipSetPenDashArray(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenDashArray}

  function GdipGetPenDashArray(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenDashArray}

  function GdipGetPenCompoundCount(pen: GPPEN;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenCompoundCount}

  function GdipSetPenCompoundArray(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPenCompoundArray}

  function GdipGetPenCompoundArray(pen: GPPEN; dash: PSingle;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPenCompoundArray}

//----------------------------------------------------------------------------
// CustomLineCap APIs
//----------------------------------------------------------------------------

  function GdipCreateCustomLineCap(fillPath: GPPATH; strokePath: GPPATH;
    baseCap: GPLINECAP; baseInset: Single;
    out customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateCustomLineCap}

  function GdipDeleteCustomLineCap(
    customCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteCustomLineCap}

  function GdipCloneCustomLineCap(customCap: GPCUSTOMLINECAP;
    out clonedCap: GPCUSTOMLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneCustomLineCap}

  function GdipGetCustomLineCapType(customCap: GPCUSTOMLINECAP;
    var capType: CUSTOMLINECAPTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapType}

  function GdipSetCustomLineCapStrokeCaps(customCap: GPCUSTOMLINECAP;
    startCap: GPLINECAP; endCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCustomLineCapStrokeCaps}

  function GdipGetCustomLineCapStrokeCaps(customCap: GPCUSTOMLINECAP;
    var startCap: GPLINECAP; var endCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapStrokeCaps}

  function GdipSetCustomLineCapStrokeJoin(customCap: GPCUSTOMLINECAP;
  lineJoin: GPLINEJOIN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCustomLineCapStrokeJoin}

  function GdipGetCustomLineCapStrokeJoin(customCap: GPCUSTOMLINECAP;
  var lineJoin: GPLINEJOIN): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapStrokeJoin}

  function GdipSetCustomLineCapBaseCap(customCap: GPCUSTOMLINECAP;
  baseCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCustomLineCapBaseCap}

  function GdipGetCustomLineCapBaseCap(customCap: GPCUSTOMLINECAP;
  var baseCap: GPLINECAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapBaseCap}

  function GdipSetCustomLineCapBaseInset(customCap: GPCUSTOMLINECAP;
  inset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCustomLineCapBaseInset}

  function GdipGetCustomLineCapBaseInset(customCap: GPCUSTOMLINECAP;
  var inset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapBaseInset}

  function GdipSetCustomLineCapWidthScale(customCap: GPCUSTOMLINECAP;
  widthScale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCustomLineCapWidthScale}

  function GdipGetCustomLineCapWidthScale(customCap: GPCUSTOMLINECAP;
  var widthScale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCustomLineCapWidthScale}

//----------------------------------------------------------------------------
// AdjustableArrowCap APIs
//----------------------------------------------------------------------------

  function GdipCreateAdjustableArrowCap(height: Single;
  width: Single;
  isFilled: Bool;
  out cap: GPADJUSTABLEARROWCAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateAdjustableArrowCap}

  function GdipSetAdjustableArrowCapHeight(cap: GPADJUSTABLEARROWCAP;
  height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetAdjustableArrowCapHeight}

  function GdipGetAdjustableArrowCapHeight(cap: GPADJUSTABLEARROWCAP;
  var height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetAdjustableArrowCapHeight}

  function GdipSetAdjustableArrowCapWidth(cap: GPADJUSTABLEARROWCAP;
  width: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetAdjustableArrowCapWidth}

  function GdipGetAdjustableArrowCapWidth(cap: GPADJUSTABLEARROWCAP;
  var width: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetAdjustableArrowCapWidth}

  function GdipSetAdjustableArrowCapMiddleInset(cap: GPADJUSTABLEARROWCAP;
  middleInset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetAdjustableArrowCapMiddleInset}

  function GdipGetAdjustableArrowCapMiddleInset(cap: GPADJUSTABLEARROWCAP;
  var middleInset: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetAdjustableArrowCapMiddleInset}

  function GdipSetAdjustableArrowCapFillState(cap: GPADJUSTABLEARROWCAP;
  fillState: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetAdjustableArrowCapFillState}

  function GdipGetAdjustableArrowCapFillState(cap: GPADJUSTABLEARROWCAP;
  var fillState: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetAdjustableArrowCapFillState}

//---------------------------------------------------------------------------- 
// Image APIs
//----------------------------------------------------------------------------

  function GdipLoadImageFromStream(stream: ISTREAM;
  out image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipLoadImageFromStream}

  function GdipLoadImageFromFile(filename: PWCHAR;
  out image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipLoadImageFromFile}

  function GdipLoadImageFromStreamICM(stream: ISTREAM;
  out image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipLoadImageFromStreamICM}

  function GdipLoadImageFromFileICM(filename: PWCHAR;
  out image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipLoadImageFromFileICM}

  function GdipCloneImage(image: GPIMAGE;
  out cloneImage: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneImage}

  function GdipDisposeImage(image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDisposeImage}

  function GdipSaveImageToFile(image: GPIMAGE;
  filename: PWCHAR;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSaveImageToFile}

  function GdipSaveImageToStream(image: GPIMAGE;
  stream: ISTREAM;
  clsidEncoder: PGUID;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSaveImageToStream}

  function GdipSaveAdd(image: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSaveAdd}

  function GdipSaveAddImage(image: GPIMAGE;
  newImage: GPIMAGE;
  encoderParams: PENCODERPARAMETERS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSaveAddImage}

  function GdipGetImageGraphicsContext(image: GPIMAGE;
  out graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageGraphicsContext}

  function GdipGetImageBounds(image: GPIMAGE;
  srcRect: GPRECTF;
  var srcUnit: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageBounds}

  function GdipGetImageDimension(image: GPIMAGE;
  var width: Single;
  var height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageDimension}

  function GdipGetImageType(image: GPIMAGE;
  var type_: IMAGETYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageType}

  function GdipGetImageWidth(image: GPIMAGE;
  var width: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageWidth}

  function GdipGetImageHeight(image: GPIMAGE;
  var height: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageHeight}

  function GdipGetImageHorizontalResolution(image: GPIMAGE;
  var resolution: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageHorizontalResolution}

  function GdipGetImageVerticalResolution(image: GPIMAGE;
  var resolution: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageVerticalResolution}

  function GdipGetImageFlags(image: GPIMAGE;
  var flags: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageFlags}

  function GdipGetImageRawFormat(image: GPIMAGE;
  format: PGUID): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageRawFormat}

  function GdipGetImagePixelFormat(image: GPIMAGE;
  out format: TPIXELFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImagePixelFormat}

  function GdipGetImageThumbnail(image: GPIMAGE; thumbWidth: UINT;
    thumbHeight: UINT; out thumbImage: GPIMAGE;
    callback: GETTHUMBNAILIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageThumbnail}

  function GdipGetEncoderParameterListSize(image: GPIMAGE;
    clsidEncoder: PGUID; out size: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetEncoderParameterListSize}

  function GdipGetEncoderParameterList(image: GPIMAGE; clsidEncoder: PGUID;
    size: UINT; buffer: PENCODERPARAMETERS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetEncoderParameterList}

  function GdipImageGetFrameDimensionsCount(image: GPIMAGE;
    var count: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageGetFrameDimensionsCount}

  function GdipImageGetFrameDimensionsList(image: GPIMAGE; dimensionIDs: PGUID;
    count: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageGetFrameDimensionsList}

  function GdipImageGetFrameCount(image: GPIMAGE; dimensionID: PGUID;
    var count: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageGetFrameCount}

  function GdipImageSelectActiveFrame(image: GPIMAGE; dimensionID: PGUID;
    frameIndex: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageSelectActiveFrame}

  function GdipImageRotateFlip(image: GPIMAGE;
    rfType: ROTATEFLIPTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageRotateFlip}

  function GdipGetImagePalette(image: GPIMAGE; palette: PCOLORPALETTE;
    size: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImagePalette}

  function GdipSetImagePalette(image: GPIMAGE;
    palette: PCOLORPALETTE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImagePalette}

  function GdipGetImagePaletteSize(image: GPIMAGE;
    var size: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImagePaletteSize}

  function GdipGetPropertyCount(image: GPIMAGE;
    var numOfProperty: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPropertyCount}

  function GdipGetPropertyIdList(image: GPIMAGE; numOfProperty: UINT;
    list: PPROPID): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPropertyIdList}

  function GdipGetPropertyItemSize(image: GPIMAGE; propId: PROPID;
    var size: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPropertyItemSize}

  function GdipGetPropertyItem(image: GPIMAGE; propId: PROPID; propSize: UINT;
    buffer: PPROPERTYITEM): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPropertyItem}

  function GdipGetPropertySize(image: GPIMAGE; var totalBufferSize: UINT;
    var numProperties: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPropertySize}

  function GdipGetAllPropertyItems(image: GPIMAGE; totalBufferSize: UINT;
    numProperties: UINT; allItems: PPROPERTYITEM): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetAllPropertyItems}

  function GdipRemovePropertyItem(image: GPIMAGE;
    propId: PROPID): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRemovePropertyItem}

  function GdipSetPropertyItem(image: GPIMAGE;
    item: PPROPERTYITEM): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPropertyItem}

  function GdipImageForceValidation(image: GPIMAGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipImageForceValidation}

//---------------------------------------------------------------------------- 
// Bitmap APIs
//----------------------------------------------------------------------------

  function GdipCreateBitmapFromStream(stream: ISTREAM;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromStream}

  function GdipCreateBitmapFromFile(filename: PWCHAR;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromFile}

  function GdipCreateBitmapFromStreamICM(stream: ISTREAM;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromStreamICM}

  function GdipCreateBitmapFromFileICM(filename: PWCHAR;
    var bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromFileICM}

  function GdipCreateBitmapFromScan0(width: Integer; height: Integer;
    stride: Integer; format: PIXELFORMAT; scan0: PBYTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromScan0}

  function GdipCreateBitmapFromGraphics(width: Integer; height: Integer;
    target: GPGRAPHICS; out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromGraphics}

  function GdipCreateBitmapFromDirectDrawSurface(surface: IDIRECTDRAWSURFACE7;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromDirectDrawSurface}

  function GdipCreateBitmapFromGdiDib(gdiBitmapInfo: PBitmapInfo;
    gdiBitmapData: Pointer; out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromGdiDib}

  function GdipCreateBitmapFromHBITMAP(hbm: HBITMAP; hpal: HPALETTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromHBITMAP}

  function GdipCreateHBITMAPFromBitmap(bitmap: GPBITMAP; out hbmReturn: HBITMAP;
    background: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateHBITMAPFromBitmap}

  function GdipCreateBitmapFromHICON(hicon: HICON;
    out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromHICON}

  function GdipCreateHICONFromBitmap(bitmap: GPBITMAP;
    out hbmReturn: HICON): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateHICONFromBitmap}

  function GdipCreateBitmapFromResource(hInstance: HMODULE;
    lpBitmapName: PWCHAR; out bitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateBitmapFromResource}

  function GdipCloneBitmapArea(x: Single; y: Single; width: Single;
    height: Single; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneBitmapArea}

  function GdipCloneBitmapAreaI(x: Integer; y: Integer; width: Integer;
    height: Integer; format: PIXELFORMAT; srcBitmap: GPBITMAP;
    out dstBitmap: GPBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneBitmapAreaI}

  function GdipBitmapLockBits(bitmap: GPBITMAP; rect: GPRECT; flags: UINT;
    format: PIXELFORMAT; lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBitmapLockBits}

  function GdipBitmapUnlockBits(bitmap: GPBITMAP;
    lockedBitmapData: PBITMAPDATA): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBitmapUnlockBits}

  function GdipBitmapGetPixel(bitmap: GPBITMAP; x: Integer; y: Integer;
    var color: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBitmapGetPixel}

  function GdipBitmapSetPixel(bitmap: GPBITMAP; x: Integer; y: Integer;
    color: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBitmapSetPixel}

  function GdipBitmapSetResolution(bitmap: GPBITMAP; xdpi: Single;
    ydpi: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBitmapSetResolution}

//----------------------------------------------------------------------------
// ImageAttributes APIs
//----------------------------------------------------------------------------

  function GdipCreateImageAttributes(
    out imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateImageAttributes}

  function GdipCloneImageAttributes(imageattr: GPIMAGEATTRIBUTES;
    out cloneImageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneImageAttributes}

  function GdipDisposeImageAttributes(
    imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDisposeImageAttributes}

  function GdipSetImageAttributesToIdentity(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesToIdentity}

  function GdipResetImageAttributes(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetImageAttributes}

  function GdipSetImageAttributesColorMatrix(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorMatrix: PCOLORMATRIX;
    grayMatrix: PCOLORMATRIX; flags: COLORMATRIXFLAGS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesColorMatrix}

  function GdipSetImageAttributesThreshold(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    threshold: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesThreshold}

  function GdipSetImageAttributesGamma(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; gamma: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesGamma}

  function GdipSetImageAttributesNoOp(imageattr: GPIMAGEATTRIBUTES;
  type_: COLORADJUSTTYPE; enableFlag: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesNoOp}

  function GdipSetImageAttributesColorKeys(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; colorLow: ARGB;
    colorHigh: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesColorKeys}

  function GdipSetImageAttributesOutputChannel(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    channelFlags: COLORCHANNELFLAGS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesOutputChannel}

  function GdipSetImageAttributesOutputChannelColorProfile(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool;
    colorProfileFilename: PWCHAR): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesOutputChannelColorProfile}

  function GdipSetImageAttributesRemapTable(imageattr: GPIMAGEATTRIBUTES;
    type_: COLORADJUSTTYPE; enableFlag: Bool; mapSize: UINT;
    map: PCOLORMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesRemapTable}

  function GdipSetImageAttributesWrapMode(imageAttr: GPIMAGEATTRIBUTES;
    wrap: WRAPMODE; argb: ARGB; clamp: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesWrapMode}

  function GdipSetImageAttributesICMMode(imageAttr: GPIMAGEATTRIBUTES;
    on_: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetImageAttributesICMMode}

  function GdipGetImageAttributesAdjustedPalette(imageAttr: GPIMAGEATTRIBUTES;
    colorPalette: PCOLORPALETTE;
    colorAdjustType: COLORADJUSTTYPE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageAttributesAdjustedPalette}

//----------------------------------------------------------------------------
// Graphics APIs
//----------------------------------------------------------------------------

  function GdipFlush(graphics: GPGRAPHICS;
    intention: GPFLUSHINTENTION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFlush}

  function GdipCreateFromHDC(hdc: HDC;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFromHDC}

  function GdipCreateFromHDC2(hdc: HDC; hDevice: THandle;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFromHDC2}

  function GdipCreateFromHWND(hwnd: HWND;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFromHWND}

  function GdipCreateFromHWNDICM(hwnd: HWND;
    out graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFromHWNDICM}

  function GdipDeleteGraphics(graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteGraphics}

  function GdipGetDC(graphics: GPGRAPHICS; var hdc: HDC): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetDC}

  function GdipReleaseDC(graphics: GPGRAPHICS; hdc: HDC): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipReleaseDC}

  function GdipSetCompositingMode(graphics: GPGRAPHICS;
    compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCompositingMode}

  function GdipGetCompositingMode(graphics: GPGRAPHICS;
    var compositingMode: COMPOSITINGMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCompositingMode}

  function GdipSetRenderingOrigin(graphics: GPGRAPHICS; x: Integer;
    y: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetRenderingOrigin}

  function GdipGetRenderingOrigin(graphics: GPGRAPHICS; var x: Integer;
    var y: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetRenderingOrigin}

  function GdipSetCompositingQuality(graphics: GPGRAPHICS;
    compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetCompositingQuality}

  function GdipGetCompositingQuality(graphics: GPGRAPHICS;
    var compositingQuality: COMPOSITINGQUALITY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCompositingQuality}

  function GdipSetSmoothingMode(graphics: GPGRAPHICS;
    smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetSmoothingMode}

  function GdipGetSmoothingMode(graphics: GPGRAPHICS;
    var smoothingMode: SMOOTHINGMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetSmoothingMode}

  function GdipSetPixelOffsetMode(graphics: GPGRAPHICS;
    pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPixelOffsetMode}

  function GdipGetPixelOffsetMode(graphics: GPGRAPHICS;
    var pixelOffsetMode: PIXELOFFSETMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPixelOffsetMode}

  function GdipSetTextRenderingHint(graphics: GPGRAPHICS;
    mode: TEXTRENDERINGHINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetTextRenderingHint}

  function GdipGetTextRenderingHint(graphics: GPGRAPHICS;
    var mode: TEXTRENDERINGHINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetTextRenderingHint}

  function GdipSetTextContrast(graphics: GPGRAPHICS;
    contrast: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetTextContrast}

  function GdipGetTextContrast(graphics: GPGRAPHICS;
    var contrast: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetTextContrast}

  function GdipSetInterpolationMode(graphics: GPGRAPHICS;
    interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetInterpolationMode}

  function GdipGetInterpolationMode(graphics: GPGRAPHICS;
    var interpolationMode: INTERPOLATIONMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetInterpolationMode}

  function GdipSetWorldTransform(graphics: GPGRAPHICS;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetWorldTransform}

  function GdipResetWorldTransform(graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetWorldTransform}

  function GdipMultiplyWorldTransform(graphics: GPGRAPHICS; matrix: GPMATRIX;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMultiplyWorldTransform}

  function GdipTranslateWorldTransform(graphics: GPGRAPHICS; dx: Single;
    dy: Single; order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateWorldTransform}

  function GdipScaleWorldTransform(graphics: GPGRAPHICS; sx: Single; sy: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipScaleWorldTransform}

  function GdipRotateWorldTransform(graphics: GPGRAPHICS; angle: Single;
    order: GPMATRIXORDER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRotateWorldTransform}

  function GdipGetWorldTransform(graphics: GPGRAPHICS;
    matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetWorldTransform}

  function GdipResetPageTransform(graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetPageTransform}

  function GdipGetPageUnit(graphics: GPGRAPHICS;
    var unit_: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPageUnit}

  function GdipGetPageScale(graphics: GPGRAPHICS;
    var scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetPageScale}

  function GdipSetPageUnit(graphics: GPGRAPHICS;
    unit_: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPageUnit}

  function GdipSetPageScale(graphics: GPGRAPHICS;
    scale: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetPageScale}

  function GdipGetDpiX(graphics: GPGRAPHICS;
    var dpi: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetDpiX}

  function GdipGetDpiY(graphics: GPGRAPHICS;
    var dpi: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetDpiY}

  function GdipTransformPoints(graphics: GPGRAPHICS;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformPoints}

  function GdipTransformPointsI(graphics: GPGRAPHICS;
    destSpace: GPCOORDINATESPACE; srcSpace: GPCOORDINATESPACE;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTransformPointsI}

  function GdipGetNearestColor(graphics: GPGRAPHICS;
    argb: PARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetNearestColor}

// Creates the Win9x Halftone Palette (even on NT) with correct Desktop colors

  function GdipCreateHalftonePalette: HPALETTE; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateHalftonePalette}

  function GdipDrawLine(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawLine}

  function GdipDrawLineI(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawLineI}

  function GdipDrawLines(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawLines}

  function GdipDrawLinesI(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawLinesI}

  function GdipDrawArc(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;
    width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawArc}

  function GdipDrawArcI(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawArcI}

  function GdipDrawBezier(graphics: GPGRAPHICS; pen: GPPEN; x1: Single;
    y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single;
    y4: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawBezier}

  function GdipDrawBezierI(graphics: GPGRAPHICS; pen: GPPEN; x1: Integer;
    y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer;
    x4: Integer; y4: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawBezierI}

  function GdipDrawBeziers(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawBeziers}

  function GdipDrawBeziersI(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawBeziersI}

  function GdipDrawRectangle(graphics: GPGRAPHICS; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawRectangle}

  function GdipDrawRectangleI(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawRectangleI}

  function GdipDrawRectangles(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawRectangles}

  function GdipDrawRectanglesI(graphics: GPGRAPHICS; pen: GPPEN; rects: GPRECT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawRectanglesI}

  function GdipDrawEllipse(graphics: GPGRAPHICS; pen: GPPEN; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawEllipse}

  function GdipDrawEllipseI(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawEllipseI}

  function GdipDrawPie(graphics: GPGRAPHICS; pen: GPPEN; x: Single; y: Single;
    width: Single;  height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawPie}

  function GdipDrawPieI(graphics: GPGRAPHICS; pen: GPPEN; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawPieI}

  function GdipDrawPolygon(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawPolygon}

  function GdipDrawPolygonI(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawPolygonI}

  function GdipDrawPath(graphics: GPGRAPHICS; pen: GPPEN;
    path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawPath}

  function GdipDrawCurve(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurve}

  function GdipDrawCurveI(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurveI}

  function GdipDrawCurve2(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurve2}

  function GdipDrawCurve2I(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurve2I}

  function GdipDrawCurve3(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINTF;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurve3}

  function GdipDrawCurve3I(graphics: GPGRAPHICS; pen: GPPEN; points: GPPOINT;
    count: Integer; offset: Integer; numberOfSegments: Integer;
    tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCurve3I}

  function GdipDrawClosedCurve(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawClosedCurve}

  function GdipDrawClosedCurveI(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawClosedCurveI}

  function GdipDrawClosedCurve2(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINTF; count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawClosedCurve2}

  function GdipDrawClosedCurve2I(graphics: GPGRAPHICS; pen: GPPEN;
    points: GPPOINT; count: Integer; tension: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawClosedCurve2I}

  function GdipGraphicsClear(graphics: GPGRAPHICS;
    color: ARGB): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGraphicsClear}

  function GdipFillRectangle(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillRectangle}

  function GdipFillRectangleI(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillRectangleI}

  function GdipFillRectangles(graphics: GPGRAPHICS; brush: GPBRUSH;
    rects: GPRECTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillRectangles}

  function GdipFillRectanglesI(graphics: GPGRAPHICS; brush: GPBRUSH;
    rects: GPRECT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillRectanglesI}

  function GdipFillPolygon(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPolygon}

  function GdipFillPolygonI(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer; fillMode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPolygonI}

  function GdipFillPolygon2(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPolygon2}

  function GdipFillPolygon2I(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPolygon2I}

  function GdipFillEllipse(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillEllipse}

  function GdipFillEllipseI(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillEllipseI}

  function GdipFillPie(graphics: GPGRAPHICS; brush: GPBRUSH; x: Single;
    y: Single; width: Single; height: Single; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPie}

  function GdipFillPieI(graphics: GPGRAPHICS; brush: GPBRUSH; x: Integer;
    y: Integer; width: Integer; height: Integer; startAngle: Single;
    sweepAngle: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPieI}

  function GdipFillPath(graphics: GPGRAPHICS; brush: GPBRUSH;
    path: GPPATH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillPath}

  function GdipFillClosedCurve(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillClosedCurve}

  function GdipFillClosedCurveI(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillClosedCurveI}

  function GdipFillClosedCurve2(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINTF; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillClosedCurve2}

  function GdipFillClosedCurve2I(graphics: GPGRAPHICS; brush: GPBRUSH;
    points: GPPOINT; count: Integer; tension: Single;
    fillMode: GPFILLMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillClosedCurve2I}

  function GdipFillRegion(graphics: GPGRAPHICS; brush: GPBRUSH;
    region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFillRegion}

  function GdipDrawImage(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;
    y: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImage}

  function GdipDrawImageI(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
    y: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImageI}

  function GdipDrawImageRect(graphics: GPGRAPHICS; image: GPIMAGE; x: Single;
    y: Single; width: Single; height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImageRect}

  function GdipDrawImageRectI(graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
    y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImageRectI}

  function GdipDrawImagePoints(graphics: GPGRAPHICS; image: GPIMAGE;
    dstpoints: GPPOINTF; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePoints}

  function GdipDrawImagePointsI(graphics: GPGRAPHICS; image: GPIMAGE;
    dstpoints: GPPOINT; count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePointsI}

  function GdipDrawImagePointRect(graphics: GPGRAPHICS; image: GPIMAGE;
    x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single;
    srcheight: Single; srcUnit: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePointRect}

  function GdipDrawImagePointRectI(graphics: GPGRAPHICS; image: GPIMAGE;
    x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer;
    srcheight: Integer; srcUnit: GPUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePointRectI}

  function GdipDrawImageRectRect(graphics: GPGRAPHICS; image: GPIMAGE;
    dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
    srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImageRectRect}

  function GdipDrawImageRectRectI(graphics: GPGRAPHICS; image: GPIMAGE;
    dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer;
    srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer;
    srcUnit: GPUNIT; imageAttributes: GPIMAGEATTRIBUTES;
    callback: DRAWIMAGEABORT; callbackData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImageRectRectI}

  function GdipDrawImagePointsRect(graphics: GPGRAPHICS; image: GPIMAGE;
    points: GPPOINTF; count: Integer; srcx: Single; srcy: Single;
    srcwidth: Single; srcheight: Single; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePointsRect}

  function GdipDrawImagePointsRectI(graphics: GPGRAPHICS; image: GPIMAGE;
    points: GPPOINT; count: Integer; srcx: Integer; srcy: Integer;
    srcwidth: Integer; srcheight: Integer; srcUnit: GPUNIT;
    imageAttributes: GPIMAGEATTRIBUTES; callback: DRAWIMAGEABORT;
    callbackData: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawImagePointsRectI}

  function GdipEnumerateMetafileDestPoint(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PPOINTF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestPoint}

  function GdipEnumerateMetafileDestPointI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PPOINT; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestPointI}

  function GdipEnumerateMetafileDestRect(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PRECTF; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestRect}

  function GdipEnumerateMetafileDestRectI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PRECT; callback: ENUMERATEMETAFILEPROC;
    callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestRectI}

  function GdipEnumerateMetafileDestPoints(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PPOINTF; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestPoints}

  function GdipEnumerateMetafileDestPointsI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PPOINT; count: Integer;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileDestPointsI}

  function GdipEnumerateMetafileSrcRectDestPoint(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PPOINTF; srcRect: PRECTF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoint}

  function GdipEnumerateMetafileSrcRectDestPointI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoint: PPOINT; srcRect: PRECT; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointI}

  function GdipEnumerateMetafileSrcRectDestRect(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PRECTF; srcRect: PRECTF; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRect}

  function GdipEnumerateMetafileSrcRectDestRectI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destRect: PRECT; srcRect: PRECT; srcUnit: TUNIT;
    callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestRectI}

  function GdipEnumerateMetafileSrcRectDestPoints(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PPOINTF; count: Integer; srcRect: PRECTF;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPoints}

  function GdipEnumerateMetafileSrcRectDestPointsI(graphics: GPGRAPHICS;
    metafile: GPMETAFILE; destPoints: PPOINT; count: Integer; srcRect: PRECT;
    srcUnit: TUNIT; callback: ENUMERATEMETAFILEPROC; callbackData: Pointer;
    imageAttributes: GPIMAGEATTRIBUTES): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEnumerateMetafileSrcRectDestPointsI}

  function GdipPlayMetafileRecord(metafile: GPMETAFILE;
    recordType: EMFPLUSRECORDTYPE; flags: UINT; dataSize: UINT;
    data: PBYTE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPlayMetafileRecord}

  function GdipSetClipGraphics(graphics: GPGRAPHICS; srcgraphics: GPGRAPHICS;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipGraphics}

  function GdipSetClipRect(graphics: GPGRAPHICS; x: Single; y: Single;
    width: Single; height: Single; combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipRect}

  function GdipSetClipRectI(graphics: GPGRAPHICS; x: Integer; y: Integer;
    width: Integer; height: Integer;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipRectI}

  function GdipSetClipPath(graphics: GPGRAPHICS; path: GPPATH;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipPath}

  function GdipSetClipRegion(graphics: GPGRAPHICS; region: GPREGION;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipRegion}

  function GdipSetClipHrgn(graphics: GPGRAPHICS; hRgn: HRGN;
    combineMode: COMBINEMODE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetClipHrgn}

  function GdipResetClip(graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipResetClip}

  function GdipTranslateClip(graphics: GPGRAPHICS; dx: Single;
    dy: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateClip}

  function GdipTranslateClipI(graphics: GPGRAPHICS; dx: Integer;
    dy: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipTranslateClipI}

  function GdipGetClip(graphics: GPGRAPHICS;
    region: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetClip}

  function GdipGetClipBounds(graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetClipBounds}

  function GdipGetClipBoundsI(graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetClipBoundsI}

  function GdipIsClipEmpty(graphics: GPGRAPHICS;
    result: PBool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsClipEmpty}

  function GdipGetVisibleClipBounds(graphics: GPGRAPHICS;
    rect: GPRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetVisibleClipBounds}

  function GdipGetVisibleClipBoundsI(graphics: GPGRAPHICS;
    rect: GPRECT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetVisibleClipBoundsI}

  function GdipIsVisibleClipEmpty(graphics: GPGRAPHICS;
    var result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleClipEmpty}

  function GdipIsVisiblePoint(graphics: GPGRAPHICS; x: Single; y: Single;
    var result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisiblePoint}

  function GdipIsVisiblePointI(graphics: GPGRAPHICS; x: Integer; y: Integer;
    var result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisiblePointI}

  function GdipIsVisibleRect(graphics: GPGRAPHICS; x: Single; y: Single;
    width: Single; height: Single; var result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRect}

  function GdipIsVisibleRectI(graphics: GPGRAPHICS; x: Integer; y: Integer;
    width: Integer; height: Integer; var result: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsVisibleRectI}

  function GdipSaveGraphics(graphics: GPGRAPHICS;
    var state: GRAPHICSSTATE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSaveGraphics}

  function GdipRestoreGraphics(graphics: GPGRAPHICS;
    state: GRAPHICSSTATE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRestoreGraphics}

  function GdipBeginContainer(graphics: GPGRAPHICS; dstrect: GPRECTF;
    srcrect: GPRECTF; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBeginContainer}

  function GdipBeginContainerI(graphics: GPGRAPHICS; dstrect: GPRECT;
    srcrect: GPRECT; unit_: GPUNIT;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBeginContainerI}

  function GdipBeginContainer2(graphics: GPGRAPHICS;
    var state: GRAPHICSCONTAINER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipBeginContainer2}

  function GdipEndContainer(graphics: GPGRAPHICS;
    state: GRAPHICSCONTAINER): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEndContainer}

  function GdipGetMetafileHeaderFromWmf(hWmf: HMETAFILE;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    header: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileHeaderFromWmf}

  function GdipGetMetafileHeaderFromEmf(hEmf: HENHMETAFILE;
    header: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileHeaderFromEmf}

  function GdipGetMetafileHeaderFromFile(filename: PWCHAR;
    header: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileHeaderFromFile}

  function GdipGetMetafileHeaderFromStream(stream: ISTREAM;
    header: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileHeaderFromStream}

  function GdipGetMetafileHeaderFromMetafile(metafile: GPMETAFILE;
    header: Pointer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileHeaderFromMetafile}

  function GdipGetHemfFromMetafile(metafile: GPMETAFILE;
    var hEmf: HENHMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetHemfFromMetafile}

  function GdipCreateStreamOnFile(filename: PWCHAR; access: UINT;
    out stream: ISTREAM): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateStreamOnFile}

  function GdipCreateMetafileFromWmf(hWmf: HMETAFILE; deleteWmf: Bool;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMetafileFromWmf}

  function GdipCreateMetafileFromEmf(hEmf: HENHMETAFILE; deleteEmf: Bool;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMetafileFromEmf}

  function GdipCreateMetafileFromFile(file_: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMetafileFromFile}

  function GdipCreateMetafileFromWmfFile(file_: PWCHAR;
    wmfPlaceableFileHeader: PWMFPLACEABLEFILEHEADER;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMetafileFromWmfFile}

  function GdipCreateMetafileFromStream(stream: ISTREAM;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateMetafileFromStream}

  function GdipRecordMetafile(referenceHdc: HDC; type_: EMFTYPE;
    frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafile}

  function GdipRecordMetafileI(referenceHdc: HDC; type_: EMFTYPE;
    frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT; description: PWCHAR;
    out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafileI}

  function GdipRecordMetafileFileName(fileName: PWCHAR; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafileFileName}

  function GdipRecordMetafileFileNameI(fileName: PWCHAR; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafileFileNameI}

  function GdipRecordMetafileStream(stream: ISTREAM; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECTF; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafileStream}

  function GdipRecordMetafileStreamI(stream: ISTREAM; referenceHdc: HDC;
    type_: EMFTYPE; frameRect: GPRECT; frameUnit: METAFILEFRAMEUNIT;
    description: PWCHAR; out metafile: GPMETAFILE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipRecordMetafileStreamI}

  function GdipSetMetafileDownLevelRasterizationLimit(metafile: GPMETAFILE;
    metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetMetafileDownLevelRasterizationLimit}

  function GdipGetMetafileDownLevelRasterizationLimit(metafile: GPMETAFILE;
    var metafileRasterizationLimitDpi: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetMetafileDownLevelRasterizationLimit}

  function GdipGetImageDecodersSize(out numDecoders: UINT;
    out size: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageDecodersSize}

  function GdipGetImageDecoders(numDecoders: UINT; size: UINT;
    decoders: PIMAGECODECINFO): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageDecoders}

  function GdipGetImageEncodersSize(out numEncoders: UINT;
    out size: UINT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageEncodersSize}

  function GdipGetImageEncoders(numEncoders: UINT; size: UINT;
    encoders: PIMAGECODECINFO): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetImageEncoders}

  function GdipComment(graphics: GPGRAPHICS; sizeData: UINT;
    data: PBYTE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipComment}

//----------------------------------------------------------------------------
// FontFamily APIs
//----------------------------------------------------------------------------

  function GdipCreateFontFamilyFromName(name: PWCHAR;
    fontCollection: GPFONTCOLLECTION;
    out FontFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFontFamilyFromName}

  function GdipDeleteFontFamily(FontFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteFontFamily}

  function GdipCloneFontFamily(FontFamily: GPFONTFAMILY;
    out clonedFontFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneFontFamily}

  function GdipGetGenericFontFamilySansSerif(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetGenericFontFamilySansSerif}

  function GdipGetGenericFontFamilySerif(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetGenericFontFamilySerif}

  function GdipGetGenericFontFamilyMonospace(
    out nativeFamily: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetGenericFontFamilyMonospace}

  function GdipGetFamilyName(family: GPFONTFAMILY; name: PWideChar;
    language: LANGID): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFamilyName}

  function GdipIsStyleAvailable(family: GPFONTFAMILY; style: Integer;
    var IsStyleAvailable: Bool): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipIsStyleAvailable}

  function GdipFontCollectionEnumerable(fontCollection: GPFONTCOLLECTION;
    graphics: GPGRAPHICS; var numFound: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFontCollectionEnumerable}

  function GdipFontCollectionEnumerate(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: array of GPFONTFAMILY;
    var numFound: Integer; graphics: GPGRAPHICS): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipFontCollectionEnumerate}

  function GdipGetEmHeight(family: GPFONTFAMILY; style: Integer;
    out EmHeight: UINT16): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetEmHeight}

  function GdipGetCellAscent(family: GPFONTFAMILY; style: Integer;
    var CellAscent: UINT16): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCellAscent}

  function GdipGetCellDescent(family: GPFONTFAMILY; style: Integer;
    var CellDescent: UINT16): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetCellDescent}

  function GdipGetLineSpacing(family: GPFONTFAMILY; style: Integer;
    var LineSpacing: UINT16): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLineSpacing}

//----------------------------------------------------------------------------
// Font APIs
//----------------------------------------------------------------------------

  function GdipCreateFontFromDC(hdc: HDC; out font: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFontFromDC}

  function GdipCreateFontFromLogfontA(hdc: HDC; logfont: PLOGFONTA;
    out font: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFontFromLogfontA}

  function GdipCreateFontFromLogfontW(hdc: HDC; logfont: PLOGFONTW;
    out font: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFontFromLogfontW}

  function GdipCreateFont(fontFamily: GPFONTFAMILY; emSize: Single;
    style: Integer; unit_: Integer; out font: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateFont}

  function GdipCloneFont(font: GPFONT;
    out cloneFont: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneFont}

  function GdipDeleteFont(font: GPFONT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteFont}

  function GdipGetFamily(font: GPFONT;
    out family: GPFONTFAMILY): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFamily}

  function GdipGetFontStyle(font: GPFONT;
    var style: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontStyle}

  function GdipGetFontSize(font: GPFONT; var size: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontSize}

  function GdipGetFontUnit(font: GPFONT; var unit_: TUNIT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontUnit}

  function GdipGetFontHeight(font: GPFONT; graphics: GPGRAPHICS;
    var height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontHeight}

  function GdipGetFontHeightGivenDPI(font: GPFONT; dpi: Single;
    var height: Single): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontHeightGivenDPI}

  function GdipGetLogFontA(font: GPFONT; graphics: GPGRAPHICS;
    var logfontA: LOGFONTA): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLogFontA}

  function GdipGetLogFontW(font: GPFONT; graphics: GPGRAPHICS;
    var logfontW: LOGFONTW): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetLogFontW}

  function GdipNewInstalledFontCollection(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipNewInstalledFontCollection}

  function GdipNewPrivateFontCollection(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipNewPrivateFontCollection}

  function GdipDeletePrivateFontCollection(
    out fontCollection: GPFONTCOLLECTION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeletePrivateFontCollection}

  function GdipGetFontCollectionFamilyCount(fontCollection: GPFONTCOLLECTION;
    var numFound: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontCollectionFamilyCount}

  function GdipGetFontCollectionFamilyList(fontCollection: GPFONTCOLLECTION;
    numSought: Integer; gpfamilies: GPFONTFAMILY;
    var numFound: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetFontCollectionFamilyList}

  function GdipPrivateAddFontFile(fontCollection: GPFONTCOLLECTION;
    filename: PWCHAR): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPrivateAddFontFile}

  function GdipPrivateAddMemoryFont(fontCollection: GPFONTCOLLECTION;
    memory: Pointer; length: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipPrivateAddMemoryFont}

//----------------------------------------------------------------------------
// Text APIs
//----------------------------------------------------------------------------

  function GdipDrawString(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PRECTF;
    stringFormat: GPSTRINGFORMAT; brush: GPBRUSH): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawString}

  function GdipMeasureString(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PRECTF;
    stringFormat: GPSTRINGFORMAT; boundingBox: PRECTF;
    codepointsFitted: PInteger; linesFilled: PInteger): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMeasureString}

  function GdipMeasureCharacterRanges(graphics: GPGRAPHICS; string_: PWCHAR;
    length: Integer; font: GPFONT; layoutRect: PRECTF;
    stringFormat: GPSTRINGFORMAT; regionCount: Integer;
    const regions: GPREGION): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMeasureCharacterRanges}

  function GdipDrawDriverString(graphics: GPGRAPHICS; const text: PUINT16;
    length: Integer; const font: GPFONT; const brush: GPBRUSH;
    const positions: PPOINTF; flags: Integer;
    const matrix: GPMATRIX): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawDriverString}

  function GdipMeasureDriverString(graphics: GPGRAPHICS; text: PUINT16;
    length: Integer; font: GPFONT; positions: PPOINTF; flags: Integer;
    matrix: GPMATRIX; boundingBox: PRECTF): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipMeasureDriverString}

//----------------------------------------------------------------------------
// String format APIs
//----------------------------------------------------------------------------

  function GdipCreateStringFormat(formatAttributes: Integer; language: LANGID;
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateStringFormat}

  function GdipStringFormatGetGenericDefault(
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipStringFormatGetGenericDefault}

  function GdipStringFormatGetGenericTypographic(
    out format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipStringFormatGetGenericTypographic}

  function GdipDeleteStringFormat(format: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteStringFormat}

  function GdipCloneStringFormat(format: GPSTRINGFORMAT;
    out newFormat: GPSTRINGFORMAT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCloneStringFormat}

  function GdipSetStringFormatFlags(format: GPSTRINGFORMAT;
    flags: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatFlags}

  function GdipGetStringFormatFlags(format: GPSTRINGFORMAT;
    out flags: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatFlags}

  function GdipSetStringFormatAlign(format: GPSTRINGFORMAT;
    align: STRINGALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatAlign}

  function GdipGetStringFormatAlign(format: GPSTRINGFORMAT;
    out align: STRINGALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatAlign}

  function GdipSetStringFormatLineAlign(format: GPSTRINGFORMAT;
    align: STRINGALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatLineAlign}

  function GdipGetStringFormatLineAlign(format: GPSTRINGFORMAT;
    out align: STRINGALIGNMENT): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatLineAlign}

  function GdipSetStringFormatTrimming(format: GPSTRINGFORMAT;
    trimming: STRINGTRIMMING): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatTrimming}

  function GdipGetStringFormatTrimming(format: GPSTRINGFORMAT;
    out trimming: STRINGTRIMMING): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatTrimming}

  function GdipSetStringFormatHotkeyPrefix(format: GPSTRINGFORMAT;
    hotkeyPrefix: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatHotkeyPrefix}

  function GdipGetStringFormatHotkeyPrefix(format: GPSTRINGFORMAT;
    out hotkeyPrefix: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatHotkeyPrefix}

  function GdipSetStringFormatTabStops(format: GPSTRINGFORMAT;
    firstTabOffset: Single; count: Integer;
    tabStops: PSingle): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatTabStops}

  function GdipGetStringFormatTabStops(format: GPSTRINGFORMAT;
    count: Integer; firstTabOffset: PSingle;
    tabStops: PSingle): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatTabStops}

  function GdipGetStringFormatTabStopCount(format: GPSTRINGFORMAT;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatTabStopCount}

  function GdipSetStringFormatDigitSubstitution(format: GPSTRINGFORMAT;
    language: LANGID;
    substitute: STRINGDIGITSUBSTITUTE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatDigitSubstitution}

  function GdipGetStringFormatDigitSubstitution(format: GPSTRINGFORMAT;
    language: PUINT; substitute: PSTRINGDIGITSUBSTITUTE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatDigitSubstitution}

  function GdipGetStringFormatMeasurableCharacterRangeCount(format: GPSTRINGFORMAT;
    out count: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipGetStringFormatMeasurableCharacterRangeCount}

  function GdipSetStringFormatMeasurableCharacterRanges(format: GPSTRINGFORMAT;
    rangeCount: Integer; ranges: PCHARACTERRANGE): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipSetStringFormatMeasurableCharacterRanges}

//----------------------------------------------------------------------------
// Cached Bitmap APIs
//----------------------------------------------------------------------------

  function GdipCreateCachedBitmap(bitmap: GPBITMAP; graphics: GPGRAPHICS;
    out cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipCreateCachedBitmap}

  function GdipDeleteCachedBitmap(
    cachedBitmap: GPCACHEDBITMAP): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDeleteCachedBitmap}

  function GdipDrawCachedBitmap(graphics: GPGRAPHICS;
    cachedBitmap: GPCACHEDBITMAP; x: Integer;
    y: Integer): GPSTATUS; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipDrawCachedBitmap}

  function GdipEmfToWmfBits(hemf: HENHMETAFILE; cbData16: UINT; pData16: PBYTE;
    iMapMode: Integer; eFlags: Integer): UINT; stdcall; external 'gdiplus.dll';
  {$EXTERNALSYM GdipEmfToWmfBits}

implementation

// -----------------------------------------------------------------------------
// TGdiplusBase class
// -----------------------------------------------------------------------------

  class function TGdiplusBase.NewInstance: TObject;
  begin
    Result := InitInstance(GdipAlloc(ULONG(instanceSize)));
  end;

  procedure TGdiplusBase.FreeInstance;
  begin
    CleanupInstance;
    GdipFree(Self);
  end;

// -----------------------------------------------------------------------------
// macros
// -----------------------------------------------------------------------------

function ObjectTypeIsValid(type_: ObjectType): BOOL;
begin
  result :=  ((type_ >= ObjectTypeMin) and (type_ <= ObjectTypeMax));
end;

function GDIP_WMF_RECORD_TO_EMFPLUS(n: integer): Integer;
begin
  result := (n or GDIP_WMF_RECORD_BASE);
end;

function GDIP_EMFPLUS_RECORD_TO_WMF(n: integer): Integer;
begin
  result := n and (not GDIP_WMF_RECORD_BASE);
end;

function GDIP_IS_WMF_RECORDTYPE(n: integer): BOOL;
begin
  result := ((n and GDIP_WMF_RECORD_BASE) <> 0);
end;


//--------------------------------------------------------------------------
// TPoint Util
//--------------------------------------------------------------------------

  function MakePoint(X, Y: Integer): TPoint;
  begin
    result.X := X;
    result.Y := Y;
  end;

  function MakePoint(X, Y: Single): TPointF;
  begin
    Result.X := X;
    result.Y := Y;
  end;

//--------------------------------------------------------------------------
// TSize Util
//--------------------------------------------------------------------------

  function MakeSize(Width, Height: Single): TSizeF;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

  function MakeSize(Width, Height: Integer): TSize;
  begin
    result.Width := Width;
    result.Height := Height;
  end;

//--------------------------------------------------------------------------
// TCharacterRange Util
//--------------------------------------------------------------------------

  function MakeCharacterRange(First, Length: Integer): TCharacterRange;
  begin
    result.First  := First;
    result.Length := Length;
  end;

// -----------------------------------------------------------------------------
// RectF class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Single): TRectF; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TPointF; size: TSizeF): TRectF; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

// -----------------------------------------------------------------------------
// Rect class
// -----------------------------------------------------------------------------

  function MakeRect(x, y, width, height: Integer): TRect; overload;
  begin
    Result.X      := x;
    Result.Y      := y;
    Result.Width  := width;
    Result.Height := height;
  end;

  function MakeRect(location: TPoint; size: TSize): Trect; overload;
  begin
    Result.X      := location.X;
    Result.Y      := location.Y;
    Result.Width  := size.Width;
    Result.Height := size.Height;
  end;

// -----------------------------------------------------------------------------
// PathData class
// -----------------------------------------------------------------------------

  constructor TPathData.Create;
  begin
    Count := 0;
    Points := nil;
    Types := nil;
  end;

  destructor TPathData.destroy;
  begin
    if assigned(Points) then freemem(Points);
    if assigned(Types) then freemem(Types);
  end;


function GetPixelFormatSize(pixfmt: PixelFormat): UINT;
begin
  result := (pixfmt shr 8) and $ff;
end;

function IsIndexedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(pixfmt: PixelFormat): BOOL;
begin
  result := (pixfmt and PixelFormatCanonical) <> 0;
end;

// -----------------------------------------------------------------------------
// Color class
// -----------------------------------------------------------------------------

{  constructor TColor.Create;
  begin
    Argb := DWORD(Black);
  end;

  // Construct an opaque Color object with
  // the specified Red, Green, Blue values.
  //
  // Color values are not premultiplied.

  constructor TColor.Create(r, g, b: Byte);
  begin
    Argb := MakeARGB(255, r, g, b);
  end;

  constructor TColor.Create(a, r, g, b: Byte);
  begin
    Argb := MakeARGB(a, r, g, b);
  end;

  constructor TColor.Create(Value: ARGB);
  begin
    Argb := Value;
  end;

  function TColor.GetAlpha: BYTE;
  begin
    result := BYTE(Argb shr AlphaShift);
  end;

  function TColor.GetA: BYTE;
  begin
    result := GetAlpha;
  end;

  function TColor.GetRed: BYTE;
  begin
    result := BYTE(Argb shr RedShift);
  end;

  function TColor.GetR: BYTE;
  begin
    result := GetRed;
  end;

  function TColor.GetGreen: Byte;
  begin
    result := BYTE(Argb shr GreenShift);
  end;

  function TColor.GetG: Byte;
  begin
    result := GetGreen;
  end;

  function TColor.GetBlue: Byte;
  begin
    result := BYTE(Argb shr BlueShift);
  end;

  function TColor.GetB: Byte;
  begin
    result := GetBlue;
  end;

  function TColor.GetValue: ARGB;
  begin
    result := Argb;
  end;

  procedure TColor.SetValue(Value: ARGB);
  begin
    Argb := Value;
  end;

  procedure TColor.SetFromCOLORREF(rgb: COLORREF);
  begin
    Argb := MakeARGB(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function TColor.ToCOLORREF: COLORREF;
  begin
    result := RGB(GetRed, GetGreen, GetBlue);
  end;

  function TColor.MakeARGB(a, r, g, b: Byte): ARGB;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;  }

  function MakeColor(r, g, b: Byte): ARGB; overload;
  begin
    result := MakeColor(255, r, g, b);
  end;

  function MakeColor(a, r, g, b: Byte): ARGB; overload;
  begin
    result := ((DWORD(b) shl  BlueShift) or
               (DWORD(g) shl GreenShift) or
               (DWORD(r) shl   RedShift) or
               (DWORD(a) shl AlphaShift));
  end;

  function GetAlpha(color: ARGB): BYTE;
  begin
    result := BYTE(color shr AlphaShift);
  end;

  function GetRed(color: ARGB): BYTE;
  begin
    result := BYTE(color shr RedShift);
  end;

  function GetGreen(color: ARGB): BYTE;
  begin
    result := BYTE(color shr GreenShift);
  end;

  function GetBlue(color: ARGB): BYTE;
  begin
    result := BYTE(color shr BlueShift);
  end;

  function ColorRefToARGB(rgb: COLORREF): ARGB;
  begin
    result := MakeColor(255, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
  end;

  function ARGBToColorRef(Color: ARGB): COLORREF;
  begin
    result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
  end;


// -----------------------------------------------------------------------------
// MetafileHeader class
// -----------------------------------------------------------------------------

  procedure TMetafileHeader.GetBounds(out Rect: Trect);
  begin
    rect.X      := X;
    rect.Y      := Y;
    rect.Width  := Width;
    rect.Height := Height;
  end;

  function TMetafileHeader.IsWmf: BOOL;
  begin
    result :=  ((Type_ = MetafileTypeWmf) or (Type_ = MetafileTypeWmfPlaceable));
  end;

  function TMetafileHeader.IsWmfPlaceable: BOOL;
  begin
    result := (Type_ = MetafileTypeWmfPlaceable);
  end;

  function TMetafileHeader.IsEmf: BOOL;
  begin
    result := (Type_ = MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfOrEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmf);
  end;

  function TMetafileHeader.IsEmfPlus: BOOL;
  begin
    result := (Type_ >= MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsEmfPlusDual: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusDual)
  end;

  function TMetafileHeader.IsEmfPlusOnly: BOOL;
  begin
    result := (Type_ = MetafileTypeEmfPlusOnly)
  end;

  function TMetafileHeader.IsDisplay: BOOL;
  begin
    result := (IsEmfPlus and ((EmfPlusFlags and GDIP_EMFPLUSFLAGS_DISPLAY) <> 0));
  end;

  function TMetafileHeader.GetWmfHeader: PMetaHeader;
  begin
    if IsWmf then result :=  @Header.WmfHeader
             else result := nil;
  end;

  function TMetafileHeader.GetEmfHeader: PENHMETAHEADER3;
  begin
    if IsEmfOrEmfPlus then result := @Header.EmfHeader
                      else result := nil;
  end;

end.



