library sysm;

{$MODE Delphi}

uses
  Windows,
  SysUtils,
  Math,
  DockH in '..\..\DockH.pas',
  adCpuUsage;

const
  //ONE_GBYTE = 1073741824;
  PLUGIN_NAME = 'SysMeters';
  PLUGIN_AUTHOR = '';
  PLUGIN_VERSION = 301;
  PLUGIN_NOTES = 'Системный монитор';

type
  PData = ^TData;
  TData = object
    hWnd: uint;
    hInstance: uint;
    meter_data: string;
    image: string;
    overlay: string;
    caption: string;
    //
    PluginRoot: array [0..MAX_PATH - 1] of char;
    BackFile: array [0..MAX_PATH - 1] of char;
    Background: Pointer;
    Foreground: Pointer;
  end;

//------------------------------------------------------------------------------
function drive(letter: char): integer;
var
  free, total: Int64;
  root: array [0..3] of char;
begin
  while byte(letter) > 90 do dec(byte(letter), 26);
  result := -1;
  root[0] := letter;
  root[1] := ':';
  root[2] := '\';
  root[3] := #0;
  try if not GetDiskFreeSpaceEx(@root[0], free, total, nil) then exit;
  except exit;
  end;
  result := round((total - free) * 100 / total);
end;
//------------------------------------------------------------------------------
function ram: integer;
var
  status: TMemoryStatus;
begin
  status.dwLength := sizeof(TMemoryStatus);
  GlobalMemoryStatus(status);
  result := status.dwMemoryLoad;
end;
//------------------------------------------------------------------------------
function cpu: integer;
begin
  CollectCPUData;
  result := round(GetCPUUsage(GetCPUCount - 1) * 100);
end;
//------------------------------------------------------------------------------
function power: integer;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  result := -1;
  GetSystemPowerStatus(SysPowerStatus);
  result := SysPowerStatus.BatteryLifePercent and $ff;
  if SysPowerStatus.ACLineStatus = 1 then result := result or $100; // charging
  if SysPowerStatus.BatteryFlag and 128 <> 0 then inc(result, $200); // N/A
end;
//------------------------------------------------------------------------------
procedure Work(data: PData);
var
  value: integer;
  image, overlay, caption: string;
  img: pointer;
begin
  // power meter //

  if data.meter_data = 'power' then
  begin
    value := power;

    image := inttostr(min((value and $ff) div 10, 8));
    if value and $200 <> 0 then image := 'na';
    image := 'power\' + image + '.png';
    overlay := '';
    if value and $100 <> 0 then overlay := 'power\charging.png';

    caption := 'Battery ' + inttostr(byte(value)) + '%';
    if (value and $100 <> 0) and (byte(value) < 100) then caption := caption + ' (charging)';
    if ((value and $100 <> 0) and (byte(value) = 100)) or (value and $200 <> 0) then caption := 'AC Power';

    if data.image <> image then
    begin
      data.image := image;
      DockletSetImageFile(data.hwnd, pchar(strpas(data.PluginRoot) + image));
    end;
    if data.overlay <> overlay then
    begin
      data.overlay := overlay;
      img := nil;
      if overlay <> '' then img := dockh.DockletLoadGDIPlusImage(pchar(strpas(data.PluginRoot) + overlay));
      dockh.DockletSetImageOverlay(data.hwnd, img, true);
    end;

    if data.caption <> caption then
    begin
      data.caption := caption;
      dockh.DockletSetLabel(data.hwnd, pchar(caption));
    end;
  end;

  // cpu meter //

  if data.meter_data = 'cpu' then
  begin
    value := cpu;
    if value < 0 then value := 0;
    if value > 100 then value := 100;
    image := 'cpu\' + inttostr(round((value + 1) / 10)) + '.png';
    caption := 'CPU load ' + inttostr(value) + '%';

    if data.image <> image then
    begin
      data.image := image;
      DockletSetImageFile(data.hwnd, pchar(strpas(data.PluginRoot) + image));
    end;
    if data.caption <> caption then
    begin
      data.caption := caption;
      DockletSetLabel(data.hwnd, pchar(caption));
    end;
  end;

  // ram meter //

  if data.meter_data = 'ram' then
  begin
    value := ram;
    if value < 0 then value := 0;
    if value > 100 then value := 100;
    image := 'ram\' + inttostr(round((value + 1) / 10)) + '.png';
    caption := 'RAM load ' + inttostr(value) + '%';

    if data.image <> image then
    begin
      data.image := image;
      DockletSetImageFile(data.hwnd, pchar(strpas(data.PluginRoot) + image));
    end;
    if data.caption <> caption then
    begin
      data.caption := caption;
      DockletSetLabel(data.hwnd, pchar(caption));
    end;
  end;

  // disk meter //

  if copy(data.meter_data, 1, 5) = 'drive' then
  begin
    value := drive(data.meter_data[7]);
    if value > 100 then value := 100;
    if value >= 0 then
    begin
      image := 'drive\' + inttostr(round((value + 1) / 10)) + '.png';
      caption := 'Drive ' + data.meter_data[7] + ': load ' + inttostr(value) + '%';
    end else begin
      image := 'drive\na.png';
      caption := 'Drive ' + data.meter_data[7] + ': not found';
    end;

    if data.image <> image then
    begin
      data.image := image;
      DockletSetImageFile(data.hwnd, pchar(strpas(data.PluginRoot) + image));
    end;
    if data.caption <> caption then
    begin
      data.caption := caption;
      DockletSetLabel(data.hwnd, pchar(caption));
    end;
  end;
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
  szRet: array [0..1023] of char;
  Instance: PData;
begin
  result := nil;
  New(Instance);
  Instance.hwnd := hwnd;
  Instance.hInstance := hInstance;
  Instance.meter_data := 'cpu';
  Instance.image := '';
  Instance.overlay := '';

  FillChar(Instance.PluginRoot, MAX_PATH, 0);
  DockletGetRootFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);
  DockletGetRelativeFolder(hWnd, @szRet);
  strcat(@Instance.PluginRoot, @szRet);

  if (szIni <> nil) and (szIniGroup <> nil) then
  begin
    GetPrivateProfileString(szIniGroup, 'Data', 'cpu', @szRet, MAX_PATH, szIni);
    try Instance.meter_data := strpas(@szRet);
    except end;
  end;

  result := Instance;

  Work(Instance);

  SetTimer(hWnd, 1, 1000, nil);
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
    WritePrivateProfileString(szIniGroup, 'Data', pchar(data^.meter_data), szIni);
end;
//------------------------------------------------------------------------------
function OnDoubleClick(data: PData; ptCursor: PPoint; size: PSize): boolean; stdcall;
begin
  result := true;
  dockh.DockletDoAttensionAnimation(data.hwnd);
  if data.meter_data = 'power' then dockh.DockExecute(data.hwnd, '%sysdir%\control.exe', '-name Microsoft.PowerOptions', nil, sw_shownormal);
  if data.meter_data = 'cpu' then dockh.DockExecute(data.hwnd, 'taskmgr.exe', nil, nil, sw_shownormal);
  if data.meter_data = 'ram' then dockh.DockExecute(data.hwnd, 'taskmgr.exe', nil, nil, sw_shownormal);
  if copy(data.meter_data, 1, 5) = 'drive' then dockh.DockExecute(data.hwnd, pchar(data.meter_data[7] + ':\'), nil, nil, sw_shownormal);
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
      integer(data^.meter_data[7] = drives[i]), $100 + i,
      pchar(string(drives[i]) + ':'));


  hMenu := CreatePopupMenu;

	AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.meter_data = 'power'), 1, 'Электропитание');
	AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.meter_data = 'cpu'), 2, 'Процессор');
	AppendMenu(hMenu, MF_STRING + MF_CHECKED * integer(data.meter_data = 'ram'), 3, 'Память');
  AppendMenu(hMenu, MF_POPUP + MF_CHECKED * integer(copy(data.meter_data, 1, 5) = 'drive'), hDiskMenu, 'Диск');

  DockletLockMouseEffect(data.hWnd, true);
  cmd := integer(TrackPopupMenuEx(hMenu, TPM_RETURNCMD, pt.x, pt.y, data.hwnd, nil));
  DockletLockMouseEffect(data.hWnd, false);

  if cmd = 1 then data.meter_data := 'power';
  if cmd = 2 then data.meter_data := 'cpu';
  if cmd = 3 then data.meter_data := 'ram';
  if cmd > $100 then
  begin
    data.meter_data := 'drive ' + drives[cmd - $100];
  end;
  if cmd > 0 then
  begin
    data.image := '';
    data.overlay := '';
    dockh.DockletSetImageOverlay(data.hwnd, nil, true); // reset overlay for "power"
  end;

  finally
    DestroyMenu(hMenu);
  end;
end;
//------------------------------------------------------------------------------
exports OnProcessMessage, OnGetInformation, OnCreate, OnDestroy, OnSave,
  OnRightButtonClick, OnDoubleClick;
//------------------------------------------------------------------------------
{$R *.res}
begin
end.
