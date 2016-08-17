library digital;

{$mode Delphi}

uses
  Windows,
  SysUtils,
  DockH in '..\..\dockh.pas',
  GDIPAPI;

const
  PLUGIN_NAME = 'Digital Clock';
  PLUGIN_VERSION = 100;
  PLUGIN_AUTHOR = 'Roman Dubinin';
  PLUGIN_NOTES = 'Digital clock';

type
  PData = ^TData;
  TData = record
	  hWnd: HANDLE;
	  hInstance: HANDLE;
    PluginRoot: array [0..MAX_PATH - 1] of char;
    BackFile: array [0..MAX_PATH - 1] of char;
    Background: Pointer;
  end;

//------------------------------------------------------------------------------
procedure err(msg: string);
begin
  messagebox(0, pchar(msg), PLUGIN_NAME, MB_ICONERROR);
end;
//------------------------------------------------------------------------------
function ColorToString(color: uint): string;
begin
  FmtStr(result, '%s%.8x', ['$', color]);
end;
//------------------------------------------------------------------------------
function SwapColor(color: uint): uint;
begin
  result:= color and $ff000000 + color shr 16 and $ff + color and $ff00 + color and $ff shl 16;
end;
//------------------------------------------------------------------------------
procedure ClockWork(lpData: PData);
var
  hgdip, image, brush, fontfamily, font, format: pointer;
  width, height: uint;
  rect: TRectF;
begin
  DockletSetLabel(lpData.hWnd, pchar(formatdatetime('hh:nn  d mmmm yyyy', now)));
  if lpData.Background = nil then exit;

  try
    GdipCloneImage(lpData.Background, image);
    GdipGetImageGraphicsContext(image, hgdip);
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
    GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
    GdipGetImageWidth(image, width);
    GdipGetImageHeight(image, height);

    // time //
    GdipCreateFontFamilyFromName('Tahoma', nil, fontfamily);
    GdipCreateFont(fontfamily, width * 40 div 128, 0, 2, font);
    GdipCreateStringFormat(0, 0, format);
    GdipSetStringFormatAlign(format, StringAlignmentCenter);
    GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
    GdipCreateSolidFill($ffffffff, brush);
    rect.x := 0;
    rect.y := 0;
    rect.width := width;
    rect.height := height;
    GdipDrawString(hgdip, PWideChar(WideString(formatdatetime('hh:nn', now))), -1, font, @rect, format, brush);
    GdipDeleteBrush(brush);
    GdipDeleteStringFormat(format);
    GdipDeleteFont(font);
    GdipDeleteFontFamily(fontfamily);

    // set plugin image //
    DockletSetImage(lpData.hwnd, image, true);
    GdipDeleteGraphics(hgdip);
  except
    messagebox(lpData.hwnd, pchar(SysErrorMessage(GetLastError)), PLUGIN_NAME, mb_iconexclamation)
  end;
end;
//------------------------------------------------------------------------------
procedure OnProcessMessage(lpData: PData; hwnd: HANDLE; uMsg: uint; wParam: WPARAM; lParam: LPARAM); stdcall;
begin
  if uMsg = WM_TIMER then
  try
    if DockletIsVisible(lpData.hwnd) then ClockWork(lpData);
  except
    messagebox(lpData^.hwnd, pchar(SysErrorMessage(GetLastError)), PLUGIN_NAME, mb_iconexclamation);
  end;
end;
//------------------------------------------------------------------------------
procedure OnGetInformation(szName, szAuthor: pchar; iVersion: PInteger; szNotes: pchar); stdcall;
begin
	strcopy(szName, PLUGIN_NAME);
	strcopy(szAuthor, PLUGIN_AUTHOR);
	iVersion^:= PLUGIN_VERSION;
	strcopy(szNotes, PLUGIN_NOTES);
end;
//------------------------------------------------------------------------------
procedure LoadBackground(lpData: PData);
var
  tmp: array [0..MAX_PATH - 1] of char;
begin
  if lpData.Background <> nil then GdipDisposeImage(lpData.Background);
  lpData.Background := nil;

  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, 'bg.png');
  lpData.Background := DockletLoadGDIPlusImage(@tmp);
end;
//------------------------------------------------------------------------------
function OnCreate(hwnd, hInstance: HANDLE; szIni, szIniGroup: pchar): PData; stdcall;
var
  szRet: array [0..1023] of char;
  path: string;
  b: pointer;
  Instance: PData;
begin
  result := nil;
  New(Instance);
  Instance.hwnd := hwnd;
	Instance.hInstance := hInstance;
  FillChar(Instance.PluginRoot, MAX_PATH, 0);
  Instance.Background := nil;

  DockletGetRootFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);
  DockletGetRelativeFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);

  result := Instance;

  LoadBackground(Instance);
  ClockWork(Instance);

  SetTimer(hWnd, 1, 1000, nil);
end;
//------------------------------------------------------------------------------
procedure OnDestroy(data: PData; hwnd: HANDLE); stdcall;
begin
  KillTimer(hWnd, 1);
  if data.Background <> nil then GdipDisposeImage(data.Background);
  Dispose(data);
end;
//------------------------------------------------------------------------------
exports OnGetInformation, OnCreate, OnDestroy, OnProcessMessage;
//------------------------------------------------------------------------------
begin
end.
 
