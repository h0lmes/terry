unit processhlp;

{$mode delphi}

interface

uses Windows, jwaWindows, SysUtils, Classes, Forms, Syncobjs, toolu, declu;

type
  {TProcessHelper}

  TProcessHelper = class(TObject)
  private
    FReady: boolean;
    crsection: TCriticalSection;
    listProcess: TStrings; // process + PID
    listProcessFullName: TStrings; // process full module name + PID
    listAppWindows: TFPList; // app windows
    FWindowsCount: integer;
    FWindowsCountChanged: boolean;
    hKernel32: HMODULE;
    hUser32: HMODULE;
    hPowrprofDll: HMODULE;
    AllowSetForegroundWindow: function(dwProcess: dword): bool; stdcall;
    queryFullProcessImageName: function(hProcess: HANDLE; dwFlags: dword; lpExeName: PAnsiChar; var lpdwSize: dword): boolean; stdcall;
    // processes //
    function IndexOf(Name: string): integer;
    function IndexOfFullName(Name: string): integer;
    function IndexOfPID(pid: dword): integer;
    function IndexOfPIDFullName(pid: dword): integer;
    function GetName(index: integer): string;
    function GetFullName(index: integer): string;
    function GetFullNameByPID(pid: uint): string;
  public
    property Ready: boolean read FReady;
    property WindowsCountChanged: boolean read FWindowsCountChanged;
    // //
    class procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
    // processes //
    procedure EnumProc;
    procedure Kill(Name: string);
    function Exists(Name: string): boolean;
    function FullNameExists(Name: string): boolean;
    function GetWindowProcessName(h: THandle): string;
    function GetWindowProcessFullName(h: THandle): string;
    // windows //
    class function GetWindowText(h: THandle): string;
    procedure AllowSetForeground(hWnd: HWND);
    function WindowOnTop(wnd: THandle): boolean;
    procedure ActivateWindow(h: THandle);
    procedure CloseWindow(h: THandle);
    function ActivateProcessMainWindow(ProcessName: string; h: THandle; ItemRect: windows.TRect; Edge: integer): boolean;
    procedure EnumAppWindows;
    procedure SortAppWindows(list: TFPList);
    function GetAppWindowsCount: integer;
    function GetAppWindowHandle(index: integer): THandle;
    function GetAppWindowIndex(h: THandle): integer;
    function GetWindowClassName(h: THandle): string;
    function WindowsOnTheSameMonitor(h1, h2: THandle): boolean;
    // system //
    procedure SetSuspendState(Hibernate: boolean);
  end;

var
  ProcessHelper: TProcessHelper;

implementation
//------------------------------------------------------------------------------
class procedure TProcessHelper.Cleanup;
begin
  if assigned(ProcessHelper) then ProcessHelper.Free;
  ProcessHelper := nil;
end;
//------------------------------------------------------------------------------
constructor TProcessHelper.Create;
begin
  FReady := false;
  inherited Create;
  crsection := TCriticalSection.Create;
  listProcess := TStringList.Create;
  listProcessFullName := TStringList.Create;
  listAppWindows := TFPList.Create;
  FWindowsCount := 0;
  FWindowsCountChanged := false;

  hKernel32 := 0;
  hUser32 := 0;
  hPowrprofDll := 0;
  @QueryFullProcessImageName := nil;
  @AllowSetForegroundWindow := nil;

  if IsWindowsVista then
  begin
    hKernel32 := LoadLibrary('kernel32.dll');
    if hKernel32 <> 0 then @QueryFullProcessImageName := GetProcAddress(hKernel32, 'QueryFullProcessImageNameA');
  end;
  hUser32 := GetModuleHandle('USER32.DLL');
  if hUser32 = 0 then hUser32 := LoadLibrary('USER32.DLL');
  if hUser32 <> 0 then @AllowSetForegroundWindow := GetProcAddress(hUser32, 'AllowSetForegroundWindow');

  FReady := assigned(crsection) and assigned(listProcess) and assigned(listAppWindows);
end;
//------------------------------------------------------------------------------
destructor TProcessHelper.Destroy;
begin
  if hKernel32 <> 0 then FreeLibrary(hKernel32);
  if hUser32 <> 0 then FreeLibrary(hUser32);
  if hPowrprofDll <> 0 then FreeLibrary(hPowrprofDll);
  listProcess.free;
  listProcessFullName.free;
  listAppWindows.free;
  crsection.free;
  inherited;
end;
//------------------------------------------------------------------------------
//
//
//
// functions to work with processes
//
//
//
//------------------------------------------------------------------------------
procedure TProcessHelper.EnumProc;
var
  snap: HANDLE;
  f: bool;
  lp: TProcessEntry32;
  idx: integer;
begin
  crsection.Acquire;
  try
    if not FReady then exit;
    listProcess.Clear;
    snap := CreateToolhelp32Snapshot(2, GetCurrentProcessId);
    if snap < 32 then exit;
    lp.dwSize := sizeof(lp);
    f := Process32First(snap, lp);
    while longint(f) <> 0 do
    begin
      listProcess.AddObject(AnsiLowerCase(lp.szExeFile), TObject(lp.th32ProcessID));
      if listProcessFullName.IndexOfObject(tobject(lp.th32ProcessID)) < 0 then
        listProcessFullName.AddObject(AnsiLowerCase(GetFullNameByPID(lp.th32ProcessID)), TObject(lp.th32ProcessID));
      f := Process32Next(snap, lp);
    end;
    CloseHandle(snap);

    // delete non-existing //
    idx := listProcessFullName.Count - 1;
    while idx >= 0 do
    begin
      if listProcess.IndexOfObject(listProcessFullName.Objects[idx]) < 0 then listProcessFullName.Delete(idx);
      dec(idx);
    end;
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.Kill(Name: string);
var
  hProc: HANDLE;
begin
  if not FReady then exit;
  EnumProc;
  Name := AnsiLowerCase(Name);
  if listProcess.indexof(Name) < 0 then exit;
  hProc := OpenProcess(PROCESS_TERMINATE, true, dword(listProcess.Objects[listProcess.indexof(Name)]));
  TerminateProcess(hProc, 0);
end;
//------------------------------------------------------------------------------
function TProcessHelper.Exists(Name: string): boolean;
begin
  result := IndexOf(Name) >= 0;
end;
//------------------------------------------------------------------------------
function TProcessHelper.FullNameExists(Name: string): boolean;
begin
  result := IndexOfFullName(AnsiLowerCase(Name)) >= 0;
end;
//------------------------------------------------------------------------------
function TProcessHelper.IndexOf(Name: string): integer;
begin
  result := listProcess.IndexOf(AnsiLowerCase(Name));
end;
//------------------------------------------------------------------------------
function TProcessHelper.IndexOfFullName(Name: string): integer;
begin
  result := listProcessFullName.IndexOf(AnsiLowerCase(Name));
end;
//------------------------------------------------------------------------------
function TProcessHelper.IndexOfPID(pid: dword): integer;
begin
  result := listProcess.IndexOfObject(TObject(pid));
end;
//------------------------------------------------------------------------------
function TProcessHelper.IndexOfPIDFullName(pid: dword): integer;
begin
  result := listProcessFullName.IndexOfObject(TObject(pid));
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetName(index: integer): string;
begin
  result := '';
  if (index >= 0) and (index < listProcess.Count) then result := listProcess.strings[index];
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetFullName(index: integer): string;
begin
  result := '';
  if (index >= 0) and (index < listProcessFullName.Count) then result := listProcessFullName.strings[index];
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetFullNameByPID(pid: uint): string;
var
  hProcess: HANDLE;
  size: dword;
  buff: array [0..MAX_PATH - 1] of AnsiChar;
begin
  result := '';
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, pid);
  if hProcess > 32 then
  begin
      size := MAX_PATH;
      ZeroMemory(@buff, MAX_PATH);

      if assigned(QueryFullProcessImageName) then
      begin
          QueryFullProcessImageName(hProcess, 0, buff, size);
          GetLongPathName(buff, buff, MAX_PATH);
          result := strpas(pchar(@buff));
      end;

      if result = '' then
      begin
        GetModuleFileNameEx(hProcess, 0, buff, MAX_PATH);
        result := strpas(pchar(@buff));
      end;

      CloseHandle(hProcess);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
// functions to work with process windows
//
//
//
//------------------------------------------------------------------------------
function CmpWindows(Item1, Item2: Pointer): Integer;
var
  pid1, pid2: dword;
begin
  GetWindowThreadProcessId(THandle(Item1), @pid1);
  GetWindowThreadProcessId(THandle(Item2), @pid2);
  result := 0;
  if pid1 > pid2 then result := -1;
  if pid1 < pid2 then result := 1;
  if pid1 = pid2 then
  begin
    if THandle(Item1) > THandle(Item2) then result := -1;
    if THandle(Item1) < THandle(Item2) then result := 1;
  end;
end;
//------------------------------------------------------------------------------
function EnumWProc(h: THandle; l: LPARAM): bool; stdcall;
var
  helper: TProcessHelper absolute l;
  exstyle: PtrUInt;
  ch: array [0..10] of char;
begin
  result := true;
  inc(helper.FWindowsCount);

  if IsWindowVisible(h) then
  begin
    exstyle := GetWindowLongPtr(h, GWL_EXSTYLE);
    if exstyle and WS_EX_APPWINDOW = 0 then
    begin
      if GetWindow(h, GW_OWNER) <> THandle(0) then exit;
      if exstyle and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW then exit;
      if windows.GetWindowText(h, ch, 10) < 1 then exit;
    end;

    helper.listAppWindows.Add(pointer(h));
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.EnumAppWindows;
var
  oldWindowsCount: integer;
begin
  crsection.Acquire;
  try
    oldWindowsCount := FWindowsCount;
    FWindowsCountChanged := false;
    FWindowsCount := 0;
    if not FReady then exit;

    listAppWindows.Clear;
    EnumWindows(@EnumWProc, LPARAM(self));
    SortAppWindows(listAppWindows);

    FWindowsCountChanged := oldWindowsCount <> FWindowsCount;
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.SortAppWindows(list: TFPList);
begin
  list.Sort(CmpWindows);
end;
//------------------------------------------------------------------------------
class function TProcessHelper.GetWindowText(h: THandle): string;
var
  win_name: array [0..255] of char;
begin
  windows.GetWindowText(h, @win_name[0], 255);
  result := strpas(pchar(@win_name[0]));
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.AllowSetForeground(hWnd: HWND);
var
  dwProcess: dword;
begin
  if assigned(AllowSetForegroundWindow) then
  begin
    dwProcess := 0;
    GetWindowThreadProcessId(hWnd, @dwProcess);
    AllowSetForegroundWindow(dwProcess);
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.WindowOnTop(wnd: THandle): boolean;
  function ZOrderIndex(hWnd: uint): integer;
  var
    index: integer;
    h: THandle;
  begin
    result := 0;
    index := 0;
	  h := FindWindow('Progman', nil);
	  while (h <> 0) and (h <> hWnd) do
	  begin
		  inc(index);
		  h := GetWindow(h, GW_HWNDPREV);
	  end;
	  result := index;
  end;

var
  i, index: integer;
  h: THandle;
begin
  result := true;
  index := ZOrderIndex(wnd);
  i := 0;
  while i < listAppWindows.count do
  begin
    h := THandle(listAppWindows.items[i]);
    if h <> wnd then
      if IsWindowVisible(h) and not IsIconic(h) then
        if ZOrderIndex(h) > index then
        begin
          result := false;
          break;
        end;
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.ActivateWindow(h: THandle);
begin
  if IsWindowVisible(h) and not IsIconic(h) then
  begin
    if WindowOnTop(h) then
      PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0)
    else
    begin
      AllowSetForeground(h);
      SetForegroundWindow(h);
    end;
  end else begin
    PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
    AllowSetForeground(h);
    SetForegroundWindow(h);
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.CloseWindow(h: THandle);
begin
  postmessage(h, WM_SYSCOMMAND, SC_CLOSE, 0);
end;
//------------------------------------------------------------------------------
// ProcessName - path and file name
// h - item window handle
// ItemRect - screen rect of item to calc the position of a popup menu
// Edge - edge of screen the item is docked at
function TProcessHelper.ActivateProcessMainWindow(ProcessName: string; h: THandle; ItemRect: windows.TRect; Edge: integer): boolean;
var
  idx, cmd: integer;
  hMenu, menu_align: cardinal;
  wnd: THandle;
  wtpid: dword;
  wlist: TFPList;
begin
  result := false;

  EnumProc;

  if FullNameExists(ProcessName) then
  begin

    EnumAppWindows;
    wlist := TFPList.Create;
    idx := 0;
    while idx < listAppWindows.count do
    begin
      wnd := THandle(listAppWindows.items[idx]);
      GetWindowThreadProcessId(wnd, @wtpid);
      if GetFullName(IndexOfPIDFullName(wtpid)) = AnsiLowerCase(ProcessName) then wlist.Add(pointer(wnd));
      inc(idx);
    end;
    result := wlist.Count > 0;

    if wlist.Count = 1 then // in case of only one window //
    begin
      ActivateWindow(THandle(wlist.items[0]));
    end
    else if result then // in case of several windows //
    begin

      hMenu := CreatePopupMenu;
      idx := 0;
      while idx < wlist.Count do
      begin
        AppendMenu(hMenu, MF_STRING, $100 + idx, pchar(GetWindowText(THandle(wlist.items[idx]))));
        inc(idx);
      end;
      AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
      AppendMenu(hMenu, MF_STRING, $2, 'Run program');

      menu_align := TPM_LEFTALIGN + TPM_TOPALIGN;
      case Edge of
        0: ItemRect.left := ItemRect.right;
        1: ItemRect.top := ItemRect.bottom;
        2: menu_align := TPM_RIGHTALIGN + TPM_TOPALIGN;
        3: menu_align := TPM_LEFTALIGN + TPM_BOTTOMALIGN;
      end;
      cmd := integer(TrackPopupMenuEx(hMenu, menu_align + TPM_RIGHTBUTTON + TPM_RETURNCMD, ItemRect.Left, ItemRect.Top, h, nil));
      DestroyMenu(hMenu);

      if cmd = $2 then result := false
      else if cmd >= $100 then ActivateWindow(THandle(wlist.items[cmd - $100]));

    end; // end case of several windows //

    wlist.free;
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetAppWindowsCount: integer;
begin
  result := listAppWindows.count;
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetAppWindowHandle(index: integer): THandle;
begin
  result := THandle(listAppWindows.items[index]);
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetAppWindowIndex(h: THandle): integer;
begin
  result := listAppWindows.IndexOf(pointer(h));
end;
//------------------------------------------------------------------------------
function TProcessHelper.WindowsOnTheSameMonitor(h1, h2: THandle): boolean;
begin
  result := MonitorFromWindow(h1, MONITOR_DEFAULTTOPRIMARY) = MonitorFromWindow(h2, MONITOR_DEFAULTTOPRIMARY);
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetWindowProcessName(h: THandle): string;
var
  wtpid: dword;
begin
  GetWindowThreadProcessId(h, @wtpid);
  result := GetName(IndexOfPID(wtpid));
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetWindowProcessFullName(h: THandle): string;
var
  wtpid: dword;
begin
  GetWindowThreadProcessId(h, @wtpid);
  result := GetFullName(IndexOfPIDFullName(wtpid));
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetWindowClassName(h: THandle): string;
var
  cls: array [0..255] of char;
begin
  GetClassName(h, @cls, 255);
  result := strpas(pchar(@cls));
end;
//------------------------------------------------------------------------------
//
//
//
//
//
//
//
//------------------------------------------------------------------------------
procedure TProcessHelper.SetSuspendState(Hibernate: boolean);
var
  susp: function(Hibernate, ForceCritical, DisableWakeEvent: bool): bool;
begin
  if SetPrivilege('SeShutdownPrivilege') then
  begin
    @susp := nil;
    if hPowrprofDll = 0 then hPowrprofDll := GetModuleHandle('powrprof.dll');
    if hPowrprofDll = 0 then hPowrprofDll := LoadLibrary('powrprof.dll');
    if hPowrprofDll <> 0 then @susp := GetProcAddress(hPowrprofDll, 'SetSuspendState');
    if assigned(susp) then susp(Hibernate, false, false);
  end;
end;
//------------------------------------------------------------------------------
end.

