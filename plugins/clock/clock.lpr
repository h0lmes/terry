library clock;

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Interfaces,
  GDIPAPI,
  DockH in '..\..\dockh.pas';
  //, frmscheduleru in 'frmscheduleru.pas' {frmscheduler};

const
  PLUGIN_NAME = 'Clock';
  PLUGIN_VERSION = 304;
  PLUGIN_AUTHOR = 'Roman Dubinin';
  PLUGIN_NOTES = 'Analog clock, timer and scheduler';

type
  PData = ^TData;
  TData = record
	  hWnd: uint;
	  hInstance: uint;
    ArmColor: uint;
    SecArmColor: uint;
    Day: boolean;
    BackSelectMode: boolean;
    PluginRoot: array [0..MAX_PATH - 1] of char;
    BackFile: array [0..MAX_PATH - 1] of char;
    Clear: Pointer;
    Background: Pointer;
    Foreground: Pointer;
    ButtonNext: Pointer;
    ButtonPrevious: Pointer;
  end;
  //frmscheduler: Tfrmscheduler;

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
  hgdip, img, hPen, hBrush: pointer;
  pts: array [0..2] of TPointF;
  center: GDIPAPI.TPointF;
  iw, ih, fw, fh, fwx, fhx: uint;
  hourangle, minangle, secangle: extended;
  hourr, minr, secr, secbackr, dotr: extended;
  //
  hff, hfont: pointer;
  fsize: extended;
  day_rect: TRectF;
  systime: TSystemTime;
begin
  DockletSetLabel(lpData.hWnd, pchar(formatdatetime('hh:nn  d mmmm yyyy', now)));
  if lpData.Background = nil then exit;

  try
    GdipCloneImage(lpData.Background, img);
    GdipGetImageGraphicsContext(img, hgdip);
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
    GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
    GdipGetImageWidth(img, iw);
    GdipGetImageHeight(img, ih);
    center.x := iw / 2;
    center.y := ih / 2;
    hourr := iw / 5;
    minr := iw / 3;
    secr := iw / 3;
    secbackr := iw / 10;
    dotr := iw / 50;

    GetLocalTime(systime);
    hourangle:= (systime.wHour * 60 + systime.wMinute) * PI / 360 - (PI / 2);
    minangle:= (systime.wMinute + systime.wSecond / 60) * PI / 30 - (PI / 2);
    secangle:= systime.wSecond * PI / 30 - (PI / 2);

    // day number //
    if lpData.Day then
    begin
      fsize:= iw / 120 + 20;
      GdipCreateFontFamilyFromName('Tahoma', nil, hff);
      GdipCreateFont(hff, fsize, 0, 2, hfont);
      GdipCreateSolidFill(lpData.ArmColor, hbrush);
      day_rect.x:= iw * 43 / 64 - fsize + 1;
      day_rect.y:= center.y - fsize / 2 - 2;
      day_rect.width:= 50;
      day_rect.height:= 40;
      GdipDrawString(hgdip, PWideChar(WideString(inttostr(systime.wDay))),
        -1, hfont, @day_rect, nil, hbrush);
      GdipDeleteBrush(hbrush);
      GdipDeleteFont(hfont);
      GdipDeleteFontFamily(hff);
    end;

    // hour arm //
    pts[0].x:= center.x;
    pts[0].y:= center.y;
    pts[1].x:= center.x + hourr * cos(hourangle);
    pts[1].y:= center.y + hourr * sin(hourangle);
    GdipCreatePen1(lpData.ArmColor, iw / 32, UnitPixel, hPen);
    GdipDrawLine(hgdip, hPen, pts[0].x, pts[0].y, pts[1].x, pts[1].y);
    GdipDeletePen(hPen);

    // min arm //
    pts[0].x:= center.x;
    pts[0].y:= center.y;
    pts[1].x:= center.x + minr * cos(minangle);
    pts[1].y:= center.y + minr * sin(minangle);
    GdipCreatePen1(lpData.ArmColor, iw / 64, UnitPixel, hPen);
    GdipDrawLine(hgdip, hPen, pts[0].x, pts[0].y, pts[1].x, pts[1].y);
    GdipDeletePen(hPen);

    // sec arm //
    if true then
    begin
      GdipCreatePen1(lpData.SecArmColor, iw / 128, UnitPixel, hPen);
      GdipDrawLine(hgdip, hPen,
        center.x - secbackr * cos(secangle), center.y - secbackr * sin(secangle),
        center.x + secr * cos(secangle), center.y + secr * sin(secangle));
      GdipDeletePen(hPen);
    end;

    // center dot //
    GdipCreateSolidFill(lpData.ArmColor, hbrush);
    GdipFillEllipse(hgdip, hbrush, center.x - dotr, center.y - dotr, dotr * 2, dotr * 2);
    GdipDeleteBrush(hbrush);

    // foreground overlay //
    if lpData.foreground <> nil then
    begin
      GdipGetImageWidth(lpData.foreground, fw);
      GdipGetImageHeight(lpData.foreground, fh);
      GdipDrawImageRectRectI(hgdip, lpData.foreground, 0, 0, iw, ih, 0, 0, fw, fh, UnitPixel, nil, nil, nil);
    end;

    // buttons overlay //
    fwx := iw div 4;
    fhx := ih div 4;
    if lpData.BackSelectMode and (lpData.ButtonNext <> nil) then
    begin
      GdipGetImageWidth(lpData.ButtonNext, fw);
      GdipGetImageHeight(lpData.ButtonNext, fh);
      GdipDrawImageRectRectI(hgdip, lpData.ButtonNext,
        iw - fwx, ih - fhx, fwx, fhx, 0, 0, fw, fh,
        UnitPixel, nil, nil, nil);
    end;
    if lpData.BackSelectMode and (lpData.ButtonPrevious <> nil) then
    begin
      GdipGetImageWidth(lpData.ButtonPrevious, fw);
      GdipGetImageHeight(lpData.ButtonPrevious, fh);
      GdipDrawImageRectRectI(hgdip, lpData.ButtonPrevious,
        0, ih - fhx, fwx, fhx, 0, 0, fw, fh,
        UnitPixel, nil, nil, nil);
    end;

    // set plugin image //
    DockletSetImage(lpData.hwnd, img, true);
    GdipDeleteGraphics(hgdip);
  except
    messagebox(lpData.hwnd, pchar(SysErrorMessage(GetLastError)), PLUGIN_NAME, mb_iconexclamation)
  end;
end;
//------------------------------------------------------------------------------
procedure OnProcessMessage(lpData: PData; hwnd, uMsg: uint; wParam: WPARAM; lParam: LPARAM); stdcall;
begin
  if uMsg = WM_TIMER then
  try
    if DockletIsVisible(lpData.hwnd) then ClockWork(lpData);
    //if assigned(frmscheduler) then frmscheduler.TimerProc;
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
  if lpData.Foreground <> nil then GdipDisposeImage(lpData.Foreground);
  if lpData.ButtonNext <> nil then GdipDisposeImage(lpData.ButtonNext);
  if lpData.ButtonPrevious <> nil then GdipDisposeImage(lpData.ButtonPrevious);
  lpData.Background := nil;
  lpData.Foreground := nil;
  lpData.ButtonNext := nil;
  lpData.ButtonPrevious := nil;

  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, 'Backgrounds\');
  strcat(@tmp, @lpData.BackFile);
  lpData.Background := DockletLoadGDIPlusImage(@tmp);
  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, 'Foregrounds\');
  strcat(@tmp, @lpData.BackFile);
  lpData.Foreground := DockletLoadGDIPlusImage(@tmp);

  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, 'Next.png');
  lpData.ButtonNext := DockletLoadGDIPlusImage(@tmp);
  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, 'Previous.png');
  lpData.ButtonPrevious := DockletLoadGDIPlusImage(@tmp);
end;
//------------------------------------------------------------------------------
function OnCreate(hwnd, hInstance: uint; szIni, szIniGroup: pchar): PData; stdcall;
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
  Instance.ArmColor := $ff000000;
  Instance.SecArmColor := $ffff2000;
  StrCopy(Instance.BackFile, 'Default.png');
  FillChar(Instance.PluginRoot, MAX_PATH, 0);
  Instance.Background := nil;
  Instance.Foreground := nil;
  Instance.ButtonNext := nil;
  Instance.ButtonPrevious := nil;

  DockletGetRootFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);
  DockletGetRelativeFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);

  if (szIni <> nil) and (szIniGroup <> nil) then
  begin
    GetPrivateProfileString(szIniGroup, 'ArmColor', pchar(ColorToString(Instance.ArmColor)), @szRet, 11, szIni);
    try Instance.ArmColor := StrToInt(strpas(@szRet));
    except end;

    GetPrivateProfileString(szIniGroup, 'SecArmColor', pchar(ColorToString(Instance.SecArmColor)), @szRet, 11, szIni);
    try Instance.SecArmColor := StrToInt(strpas(@szRet));
    except end;

    GetPrivateProfileString(szIniGroup, 'Day', '1', @szRet, 5, szIni);
    try Instance.Day := szRet[0] <> '0';
    except end;

    GetPrivateProfileString(szIniGroup, 'Background', 'Default.png', @Instance.BackFile, MAX_PATH, szIni);

    //GetPrivateProfileString(szIniGroup, 'task0', nil, @szRet, 2048, szIni);
    //try frmscheduler.FromString(pchar(@szRet), 0);
    //except end;
  end;

  result := Instance;

  LoadBackground(Instance);
  ClockWork(Instance);

  //Instance.frmscheduler:= Tfrmscheduler.create(nil);

  SetTimer(hWnd, 1, 1000, nil);
end;
//------------------------------------------------------------------------------
procedure OnDestroy(data: PData; hwnd: uint); stdcall;
begin
  KillTimer(hWnd, 1);
  if data.Background <> nil then GdipDisposeImage(data.Background);
  if data.Foreground <> nil then GdipDisposeImage(data.Foreground);
  if data.ButtonNext <> nil then GdipDisposeImage(data.ButtonNext);
  if data.ButtonPrevious <> nil then GdipDisposeImage(data.ButtonPrevious);
  //data.frmscheduler.free;
  Dispose(data);
end;
//------------------------------------------------------------------------------
procedure OnSave(data: PData; szIni, szIniGroup: pchar; isForExport: boolean); stdcall;
begin
  if (szIni <> nil) and (szIniGroup <> nil) then
  begin
    WritePrivateProfileString(szIniGroup, 'ArmColor', pchar(ColorToString(data.ArmColor)), szIni);
    WritePrivateProfileString(szIniGroup, 'SecArmColor', pchar(ColorToString(data.SecArmColor)), szIni);
    WritePrivateProfileString(szIniGroup, 'Background', @data.BackFile, szIni);
    WritePrivateProfileString(szIniGroup, 'Day', pchar(inttostr(integer(data.Day))), szIni);
    //WritePrivateProfileString(szIniGroup, 'task0', pchar(data.frmscheduler.ToString(0)), szIni);
  end;
end;
//------------------------------------------------------------------------------
procedure OnConfigure(data: PData); stdcall;
begin
  //data^.frmscheduler.Show;
end;
//------------------------------------------------------------------------------
procedure FindImageFile(PathMask, FileName: pchar; Step: integer);
var
  fhandle: THandle;
  f: TWin32FindData;
  index: integer;
  list: TStrings;
begin
  list := TStringList.Create;
  try
    // search files //
    fhandle := FindFirstFile(PathMask, f);
    if fhandle <> DWORD(-1) then
    begin
      if (f.dwFileAttributes and $18) = 0 then list.Add(f.cFileName);
      while FindNextFile(fhandle, f) do
        if (f.dwFileAttributes and $18) = 0 then list.Add(f.cFileName);
      if fhandle <> DWORD(-1) then Windows.FindClose(fhandle);
    end;

    // select desired //
    if list.Count > 0 then
    begin
      index := list.IndexOf(strpas(FileName));
      if index >= 0 then
      begin
        inc(index, Step);
        if index < 0 then index := list.Count - 1;
        if index >= list.Count then index := 0;
        strpcopy(FileName, list.strings[index]);
      end;
    end;

  finally
    list.Free;
  end;
end;
//------------------------------------------------------------------------------
function OnLeftButtonClick(data: PData; ppt: windows.PPoint; size: windows.PSize): boolean; stdcall;
var
  path_mask: array [0..MAX_PATH - 1] of char;
  Step: integer;
begin
  result:= true;
  if data.BackSelectMode then
  begin
    FillChar(path_mask, MAX_PATH, 0);
    strcat(@path_mask, @data.PluginRoot);
    strcat(@path_mask, 'Backgrounds\*.png');
    Step := 1;
    if ppt.x < size.cx div 2 then Step := -1;
    FindImageFile(@path_mask, @data.BackFile, Step);
    LoadBackground(data);
    ClockWork(data);
  end;
end;
//------------------------------------------------------------------------------
function OnDoubleClick(data: PData; ppt: windows.PPoint; size: windows.PSize): boolean; stdcall;
begin
  OnConfigure(data);
  result:= true;
end;
//------------------------------------------------------------------------------
function OnRightButtonClick(data: PData; ppt: windows.PPoint; size: windows.PSize): boolean; stdcall;
var
  hMenu, color: uint;
  cmd: integer;
  pt: windows.TPoint;
  path_mask: array [0..MAX_PATH - 1] of char;
  mi: MenuItemInfo;
begin
  try
    result:= true;
	  GetCursorPos(pt);

    hMenu:= CreatePopupMenu;
    AppendMenu(hMenu, MF_STRING, 2, 'Цвет стрелок');
	  AppendMenu(hMenu, MF_STRING, 3, 'Цвет секундной стрелки');
	  AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.Day), 5, 'День месяца');
	  AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.BackSelectMode), 6, 'Режим выбора фона');
	  AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
	  AppendMenu(hMenu, MF_STRING, 1, 'Таймер-планировщик');
    AppendMenu(hMenu, MF_STRING, 4, 'Дата и время');

    DockletLockMouseEffect(data.hWnd, true);
    cmd:= integer(TrackPopupMenuEx(hMenu, TPM_RETURNCMD, pt.x, pt.y, data.hwnd, nil));
    DockletLockMouseEffect(data.hWnd, false);

    // execute plugin menu command
    if cmd = 1 then OnConfigure(data)
    else
    if cmd = 2 then
    begin
      color := SwapColor(data.ArmColor);
      if DockColorDialog(@color) then data.ArmColor := SwapColor(color) or $FF000000;
    end
    else
    if cmd = 3 then
    begin
      color := SwapColor(data.SecArmColor);
      if DockColorDialog(@color) then data.SecArmColor := SwapColor(color) or $FF000000;
    end
    else
    if cmd = 4 then DockExecute(data.hWnd, 'timedate.cpl', nil, nil, sw_shownormal)
    else
    if cmd = 5 then data.Day := not data.Day
    else
    if cmd = 6 then data.BackSelectMode := not data.BackSelectMode;

  finally
    DestroyMenu(hMenu);
  end;
end;
//------------------------------------------------------------------------------
exports OnGetInformation, OnCreate, OnDestroy, OnSave, OnProcessMessage,
  OnLeftButtonClick, OnRightButtonClick, OnDoubleClick, OnConfigure;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
begin
end.
 
