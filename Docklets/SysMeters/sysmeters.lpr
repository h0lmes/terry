library sysmeters;

{$MODE Delphi}

uses
  Windows,
  SysUtils,
  Math,
  DockH in '..\..\DockH.pas',
  GDIPAPI,
  adCpuUsage,
  cibufferu;

const
  PLUGIN_NAME = 'SysMeters';
  PLUGIN_AUTHOR = '';
  PLUGIN_VERSION = 400;
  PLUGIN_NOTES = 'System performance monitor';
  GRAPH_COLOR = $c0ffffff;
  BUF_SIZE = 28;

type
  TMeterMode = (mmCPU, mmRAM, mmDrive, mmBattery);

  PData = ^TData;
  TData = object
    hWnd: uint;
    mode: TMeterMode;
    letter: char;
    caption: string;
    buf: TCIBuffer;
    PluginRoot: array [0..MAX_PATH - 1] of char;
  end;

//------------------------------------------------------------------------------
procedure getDrive(letter: char; var loadPercent: integer; var freeGB: extended);
var
  free, total: Int64;
  freeMB, totalMB: integer;
  root: array [0..3] of char;
begin
  freeGB := 0;
  loadPercent := 0;
  root[0] := letter;
  root[1] := ':';
  root[2] := '\';
  root[3] := #0;
  if not GetDiskFreeSpaceEx(@root, free, total, nil) then exit;
  totalMB := total shr 20 and $fffffff;
  freeMB := free shr 20 and $fffffff;
  loadPercent := round((totalMB - freeMB) / totalMB * 100);
  freeGB := freeMB / 1024;
end;
//------------------------------------------------------------------------------
procedure getRam(var loadPercent, freeMB: integer);
var
  status: TMemoryStatus;
begin
  status.dwLength := sizeof(TMemoryStatus);
  GlobalMemoryStatus(status);
  loadPercent := status.dwMemoryLoad;
  freeMB := status.dwAvailPhys shr 20 and $fffffff;
end;
//------------------------------------------------------------------------------
procedure getCpu(var loadPercent: integer);
begin
  CollectCPUData;
  loadPercent := round(GetCPUUsage(GetCPUCount - 1) * 100);
  if loadPercent < 0 then loadPercent := 0;
  if loadPercent > 100 then loadPercent := 100;
end;
//------------------------------------------------------------------------------
procedure getBattery(var percent, lifetimeH, lifetimeM: integer; var charging, noBattery: boolean);
var
  SysPowerStatus: TSystemPowerStatus;
begin
  percent := -1;
  GetSystemPowerStatus(SysPowerStatus);
  percent := SysPowerStatus.BatteryLifePercent;
  if percent > 100 then percent := 100;
  lifetimeM := SysPowerStatus.BatteryLifeTime div 60;
  lifetimeH := lifetimeM div 60;
  lifetimeM := lifetimeM - lifetimeH * 60;
  charging := SysPowerStatus.ACLineStatus = 1;
  noBattery := SysPowerStatus.BatteryFlag and 128 <> 0;
end;
//------------------------------------------------------------------------------
procedure NewBitmap(out bitmap, graphics: Pointer);
begin
  GdipCreateBitmapFromScan0(128, 128, 0, PixelFormat32bppPARGB, nil, bitmap);
  GdipGetImageGraphicsContext(bitmap, graphics);
  GdipSetInterpolationMode(graphics, InterpolationModeHighQualityBicubic);
  GdipSetTextRenderingHint(graphics, TextRenderingHintAntiAlias);
  GdipSetPixelOffsetMode(graphics, PixelOffsetModeHighSpeed);
  GdipSetSmoothingMode(graphics, SmoothingModeAntiAlias);
  GdipSetCompositingQuality(graphics, CompositingQualityHighSpeed);
end;
//------------------------------------------------------------------------------
procedure DrawGraph(graphics: Pointer; var buf: TCIBuffer);
var
  i, value1, value2: integer;
  pen: pointer;
  x1, y1, x2, y2: integer;
begin
  GdipCreatePen1(GRAPH_COLOR, 4, UnitPixel, pen);
  for i := 0 to buf.size - 2 do
  begin
    CIBuffer_Get(buf, i, value1);
    CIBuffer_Get(buf, i + 1, value2);
    if (value1 > -1) and (value2 > -1) then
    begin
      x1 := 10 + i * 4 - 2;
      y1 := 114 - value1 - 2;
      x2 := 10 + (i + 1) * 4 - 2;
      y2 := 114 - value2 - 2;
      GdipDrawLineI(graphics, pen, x1, y1, x2, y2);
    end;
  end;
  GdipDeletePen(pen);
end;
//------------------------------------------------------------------------------
procedure DrawText(graphics: Pointer; txt: string);
var
  rect: TRectF;
  hff, hfont, hbrush: pointer;
begin
  rect.X := 6;
  rect.y := 8;
  rect.Width := 112;
  rect.Height := 64;
  GdipCreateFontFamilyFromName(PWideChar(WideString(PChar('Segoe UI'))), nil, hff);
  GdipCreateFont(hff, 20, 1, 2, hfont);
  GdipCreateSolidFill(GRAPH_COLOR, hbrush);
  GdipDrawString(graphics, PWideChar(WideString(txt)), -1, hfont, @rect, nil, hbrush);
  GdipDeleteBrush(hbrush);
  GdipDeleteFont(hfont);
  GdipDeleteFontFamily(hff);
end;
//------------------------------------------------------------------------------
procedure Battery(data: PData);
var
  percent, lifetimeH, lifetimeM: integer;
  charging, noBattery: boolean;
  caption: string;
  overlay, g: pointer;
begin
  getBattery(percent, lifetimeH, lifetimeM, charging, noBattery);
  if noBattery then percent := -1;
  CIBuffer_Put(data.buf, percent);

  if noBattery then caption := 'no battery'
  else
  begin
    caption := inttostr(lifetimeH) + ':' + inttostr(lifetimeM) + ' ' + inttostr(percent) + '%';
    if charging and (percent < 100) then caption := caption + ' (charging)';
    if charging and (percent = 100) then caption := caption + ' (AC power)';
  end;
  if data.caption <> caption then
  begin
    data.caption := caption;
    dockh.DockletSetLabel(data.hwnd, pchar(caption));
  end;

  NewBitmap(overlay, g);
  DrawGraph(g, data.buf);
  if noBattery then DrawText(g, 'N/A') else DrawText(g, 'Batt. ' + inttostr(percent) + '%');
  GdipDeleteGraphics(g);
  DockletSetImageOverlay(data.hwnd, overlay, true);
end;
//------------------------------------------------------------------------------
procedure CPU(data: PData);
var
  percent: integer;
  caption: string;
  overlay, g: pointer;
begin
  getCpu(percent);
  CIBuffer_Put(data.buf, percent);

  caption := 'CPU load ' + inttostr(percent) + '%';
  if data.caption <> caption then
  begin
    data.caption := caption;
    DockletSetLabel(data.hwnd, pchar(caption));
  end;

  NewBitmap(overlay, g);
  DrawGraph(g, data.buf);
  DrawText(g, 'CPU ' + inttostr(percent) + '%');
  GdipDeleteGraphics(g);
  DockletSetImageOverlay(data.hwnd, overlay, true);
end;
//------------------------------------------------------------------------------
procedure RAM(data: PData);
var
  percent, freeMB: integer;
  caption: string;
  overlay, g: pointer;
begin
  getRam(percent, freeMB);
  CIBuffer_Put(data.buf, percent);

  caption := 'RAM load ' + inttostr(percent) + '% (free ' + floattostr(freeMB) + ' MB)';
  if data.caption <> caption then
  begin
    data.caption := caption;
    DockletSetLabel(data.hwnd, pchar(caption));
  end;

  NewBitmap(overlay, g);
  DrawGraph(g, data.buf);
  DrawText(g, 'RAM ' + inttostr(percent) + '%');
  GdipDeleteGraphics(g);
  DockletSetImageOverlay(data.hwnd, overlay, true);
end;
//------------------------------------------------------------------------------
procedure Drive(data: PData);
var
  percent: integer;
  freeGB: extended;
  caption: string;
  overlay, g: pointer;
begin
  getDrive(data.letter, percent, freeGB);
  CIBuffer_Put(data.buf, percent);

  if percent >= 0 then caption := data.letter + ': load ' + inttostr(percent) + '% (free ' + formatfloat('### ##0.0#', freeGB) + ' GB)'
  else caption := data.letter + ': not found';
  if data.caption <> caption then
  begin
    data.caption := caption;
    DockletSetLabel(data.hwnd, pchar(caption));
  end;

  NewBitmap(overlay, g);
  DrawGraph(g, data.buf);
  if percent < 0 then DrawText(g, data.letter + ': N/A') else DrawText(g, data.letter + ': ' + formatfloat('### ##0.0#', freeGB) + ' GB');
  GdipDeleteGraphics(g);
  DockletSetImageOverlay(data.hwnd, overlay, true);
end;
//------------------------------------------------------------------------------
procedure Work(data: PData);
begin
  if data.mode = mmBattery then Battery(data)
  else if data.mode = mmCPU then CPU(data)
  else if data.mode = mmRAM then RAM(data)
  else if data.mode = mmDrive then Drive(data);
end;
//------------------------------------------------------------------------------
procedure SetBackground(lpData: PData);
var
  tmp: array [0..MAX_PATH - 1] of char;
begin
  FillChar(tmp, MAX_PATH, 0);
  strcat(@tmp, @lpData.PluginRoot);
  strcat(@tmp, pchar('bg.png'));
  DockletSetImageFile(lpData.hwnd, @tmp);
end;
//------------------------------------------------------------------------------
procedure OnProcessMessage(lpData: PData; hwnd, uMsg: uint; wParam: WPARAM; lParam: LPARAM); stdcall;
begin
  if uMsg = WM_TIMER then
  try
    if DockletIsVisible(lpData.hwnd) then Work(lpData);
  except
    messagebox(lpData^.hwnd, pchar(SysErrorMessage(GetLastError)), PLUGIN_NAME, mb_iconexclamation);
  end;
end;
//------------------------------------------------------------------------------
procedure OnGetInformation(szName, szAuthor: pchar; iVersion: PInteger; szNotes: pchar); stdcall;
begin
  strcopy(szName, PLUGIN_NAME);
  strcopy(szAuthor, PLUGIN_AUTHOR);
  iVersion^ := PLUGIN_VERSION;
  strcopy(szNotes, PLUGIN_NOTES);
end;
//------------------------------------------------------------------------------
function OnCreate(hwnd, hInstance: uint; szIni, szIniGroup: pchar): PData; stdcall;
var
  szRet: array [0..MAX_PATH - 1] of char;
  Instance: PData;
begin
  try
    result := nil;
    New(Instance);
    Instance.hwnd := hwnd;
    Instance.mode := mmCPU;
    CIBuffer_Init(Instance.buf, BUF_SIZE, -1);
    result := Instance;

    FillChar(Instance.PluginRoot, MAX_PATH, 0);
    DockletGetRootFolder(hWnd, @szRet);
    strcat(@Instance.PluginRoot, @szRet);
    DockletGetRelativeFolder(hWnd, @szRet);
    strcat(@Instance.PluginRoot, @szRet);
    SetBackground(Instance);

    if (szIni <> nil) and (szIniGroup <> nil) then
    begin
      GetPrivateProfileString(szIniGroup, 'mode', pchar(inttostr(integer(mmCPU))), @szRet, MAX_PATH, szIni);
      try Instance.mode := TMeterMode(strtoint(strpas(@szRet)));
      except end;
      GetPrivateProfileString(szIniGroup, 'letter', 'C', @szRet, MAX_PATH, szIni);
      Instance.letter := szRet[0];
    end;

    Work(Instance);
    SetTimer(hWnd, 1, 1000, nil);
  except
    messagebox(hwnd, pchar(SysErrorMessage(GetLastError)), PLUGIN_NAME, mb_iconexclamation);
  end;
end;
//------------------------------------------------------------------------------
procedure OnDestroy(data: PData; hwnd: uint); stdcall;
begin
  KillTimer(hWnd, 1);
  Dispose(data);
end;
//------------------------------------------------------------------------------
procedure OnSave(data: PData; szIni, szIniGroup: pchar); stdcall;
begin
  if (szIni <> nil) and (szIniGroup <> nil) then
  begin
    WritePrivateProfileString(szIniGroup, 'mode', pchar(inttostr(integer(data.mode))), szIni);
    if data.mode = mmDrive then WritePrivateProfileString(szIniGroup, 'letter', @data.letter, szIni);
  end;
end;
//------------------------------------------------------------------------------
function OnDoubleClick(data: PData; ptCursor: PPoint; size: PSize): boolean; stdcall;
begin
  result := true;
  dockh.DockletDoAttensionAnimation(data.hwnd);
  if data.mode = mmBattery then dockh.DockExecute(data.hwnd, '%sysdir%\control.exe', '-name Microsoft.PowerOptions', nil, sw_shownormal);
  if (data.mode = mmCPU) or (data.mode = mmRAM) then dockh.DockExecute(data.hwnd, 'taskmgr.exe', nil, nil, sw_shownormal);
  if data.mode = mmDrive then dockh.DockExecute(data.hwnd, pchar(data.letter + ':\'), nil, nil, sw_shownormal);
end;
//------------------------------------------------------------------------------
function OnRightButtonClick(data: PData; ptCursor: PPoint; size: PSize): boolean; stdcall;
const
  drives: array [3..26] of char =
    ('C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');
var
  hMenu, hDiskMenu: uint;
  cmd, i: integer;
  pt: windows.TPoint;
begin
  try
    result := true;
    GetCursorPos(pt);

    hDiskMenu := CreatePopupMenu;
    for i := 3 to 26 do
      AppendMenu(hDiskMenu, MF_STRING + MF_CHECKED *
        integer((data.letter = drives[i]) and (data.mode = mmDrive)), $100 + i,
        pchar(string(drives[i]) + ':'));


    hMenu := CreatePopupMenu;

	  AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.mode = mmBattery), 1, 'Battery');
	  AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.mode = mmCPU), 2, 'CPU');
	  AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.mode = mmRAM), 3, 'RAM');
    AppendMenu(hMenu, MF_POPUP + MF_CHECKED * integer(data.mode = mmDrive), hDiskMenu, 'Drive');

    DockletLockMouseEffect(data.hWnd, true);
    cmd := integer(TrackPopupMenuEx(hMenu, TPM_RETURNCMD, pt.x, pt.y, data.hwnd, nil));
    DockletLockMouseEffect(data.hWnd, false);

    if cmd = 1 then data.mode := mmBattery;
    if cmd = 2 then data.mode := mmCPU;
    if cmd = 3 then data.mode := mmRAM;
    if cmd > $100 then
    begin
      data.mode := mmDrive;
      data.letter := drives[cmd - $100];
    end;

    if cmd > 0 then
    begin
      CIBuffer_Init(data.buf, BUF_SIZE, -1); // reset buffer
      dockh.DockletSetImageOverlay(data.hwnd, nil, true); // reset overlay
    end;
  finally
    DestroyMenu(hMenu);
  end;
end;
//------------------------------------------------------------------------------
exports OnProcessMessage, OnGetInformation, OnCreate, OnDestroy, OnSave,
  OnRightButtonClick, OnDoubleClick;
//------------------------------------------------------------------------------
begin
end.
