unit processhlp;

{$mode delphi}

interface

uses Windows, jwaWindows, SysUtils, Classes, Forms, Syncobjs, declu, dwm_unit;

type
  {TProcessHelper}

  TProcessHelper = class(TObject)
  private
    FReady: boolean;
    crsection: TCriticalSection;
    listProcess: TStrings; // process + PID
    listProcessFullName: TStrings; // process full module name + PID
    listAppWindows: TFPList; // app windows
    FParentHWnd: THandle; // if this handle <> 0, then every app window is being checked if it is on the same monitor
    FWindowsCount: integer;
    FWindowsCountChanged: boolean;
    hKernel32: HMODULE;
    hUser32: HMODULE;
    hPowrprofDll: HMODULE;
    AllowSetForegroundWindow: function(dwProcess: dword): bool; stdcall;
    queryFullProcessImageName: function(hProcess: HANDLE; dwFlags: dword; lpExeName: PAnsiChar; var lpdwSize: dword): boolean; stdcall;
    // processes //
    procedure GetProcessPIDs(Name: string; var pids: TFPList; OnlyFist: boolean = false);
    function GetFullNameByPID_Internal(pid: uint): string;
    function SetPrivilege(Name: string): boolean;
  public
    property Ready: boolean read FReady;
    property WindowsCountChanged: boolean read FWindowsCountChanged;
    // //
    class procedure Cleanup;
    constructor Create(bVistaOrGreater: boolean);
    destructor Destroy; override;
    procedure EnterCRS;
    procedure LeaveCRS;
    // processes //
    procedure EnumProc;
    procedure Kill(Name: string);
    function ProcessExists(Name: string): boolean;
    function GetWindowProcessName(h: THandle): string;
    procedure GetProcessWindows(Name: string; var AppList: TFPList); overload;
    procedure GetProcessWindows(pid: dword; var AppList: TFPList); overload;
    // windows //
    class function GetWindowText(h: THandle): string;
    procedure AllowSetForeground(hWnd: HWND);
    function WindowOnTop(wnd: THandle): boolean;
    procedure ActivateWindow(h: THandle);
    procedure ActivateWindowList(list: TFPList);
    procedure CloseWindow(h: THandle);
    function ActivateProcessMainWindow(ProcessName: string; h: THandle; ItemRect: windows.TRect; Edge: integer): boolean;
    procedure EnumAppWindows(ParentHWnd: THandle = 0);
    procedure SortAppWindows(list: TFPList);
    function GetAppWindowsCount: integer;
    function GetAppWindowHandle(index: integer): THandle;
    function WindowsOnTheSameMonitor(h1, h2: THandle): boolean;
    // system //
    procedure Shutdown(mode: integer);
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
constructor TProcessHelper.Create(bVistaOrGreater: boolean);
begin
  FReady := false;
  inherited Create;
  crsection := TCriticalSection.Create;
  listProcess := TStringList.Create;
  listProcessFullName := TStringList.Create;
  listAppWindows := TFPList.Create;
  FParentHWnd := 0;
  FWindowsCount := 0;
  FWindowsCountChanged := false;

  hKernel32 := 0;
  hUser32 := 0;
  hPowrprofDll := 0;
  @QueryFullProcessImageName := nil;
  @AllowSetForegroundWindow := nil;

  if bVistaOrGreater then
  begin
    hKernel32 := GetModuleHandle('KERNEL32.DLL');
    if hKernel32 = 0 then hKernel32 := LoadLibrary('KERNEL32.DLL');
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
procedure TProcessHelper.EnterCRS;
begin
  crsection.Acquire;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.LeaveCRS;
begin
  crsection.Leave;
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
        listProcessFullName.AddObject(AnsiLowerCase(GetFullNameByPID_Internal(lp.th32ProcessID)), TObject(lp.th32ProcessID));
      f := Process32Next(snap, lp);
    end;
    CloseHandle(snap);

    // delete non-existing processes from listProcessFullName //
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
function TProcessHelper.GetFullNameByPID_Internal(pid: uint): string;
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
// kill a process by its name
// name must be either a fully qualified pathname or just a filename.exe
procedure TProcessHelper.Kill(Name: string);
var
  index: integer;
  hProc: HANDLE;
  pids: TFPList;
begin
  EnumProc;
  pids := TFPList.Create;
  try
    GetProcessPIDs(Name, pids);
    index := 0;
    while index < pids.Count do
    begin
      hProc := OpenProcess(PROCESS_TERMINATE, true, dword(pids.Items[index]));
      TerminateProcess(hProc, 0);
      inc(index);
    end;
  finally
    pids.free;
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.ProcessExists(Name: string): boolean;
var
   pids: TFPList;
begin
  result := false;
  pids := TFPList.Create;
  try
      GetProcessPIDs(Name, pids, true);
      result := pids.Count > 0;
  finally
      pids.free;
  end;
end;
//------------------------------------------------------------------------------
// get main window handles belonging to a specified process.
// 'Name' could be either a fully qualified path + filename or just a filename.exe
procedure TProcessHelper.GetProcessWindows(Name: string; var AppList: TFPList);
var
  index: integer;
  wnd: THandle;
  pid, wpid: DWORD;
  pids, newAppList: TFPList;
begin
  if not FReady then exit;
  newAppList := TFPList.Create;
  pids := TFPList.Create;

  try
    GetProcessPIDs(Name, pids);
    if pids.Count > 0 then
    begin
      index := 0;
      while index < listAppWindows.count do
      begin
        wnd := THandle(listAppWindows.items[index]);
        GetWindowThreadProcessId(wnd, @wpid);
        if pids.IndexOf(Pointer(wpid)) >= 0 then newAppList.Add(pointer(wnd));
        inc(index);
      end;
    end;
  finally
    pids.free;
  end;

  try
    if newAppList.Count = 0 then AppList.Clear
    else begin
      index := 0;
      while index < newAppList.Count do
      begin
        if AppList.IndexOf(newAppList.Items[index]) < 0 then AppList.Add(newAppList.Items[index]);
        inc(index);
      end;
      index := AppList.Count - 1;
      while index >= 0 do
      begin
        if newAppList.IndexOf(AppList.Items[index]) < 0 then AppList.Delete(index);
        dec(index);
      end;
    end;
  finally
    newAppList.free;
  end;
end;
//------------------------------------------------------------------------------
// get main window handles belonging to a specified process.
// 'Name' could be either a fully qualified path + filename or just a filename.exe
procedure TProcessHelper.GetProcessWindows(pid: dword; var AppList: TFPList);
var
  index: integer;
  wnd: THandle;
  wpid: DWORD;
begin
  if not FReady then exit;
  AppList.Clear;
	index := 0;
	while index < listAppWindows.count do
	begin
      wnd := THandle(listAppWindows.items[index]);
      GetWindowThreadProcessId(wnd, @wpid);
	    if wpid = pid then AppList.Add(pointer(wnd));
	    inc(index);
	end;
end;
//------------------------------------------------------------------------------
// get list of PIDs (process identifiers) by a process name
// the name must be either a fully qualified pathname or just a filename.exe
procedure TProcessHelper.GetProcessPIDs(Name: string; var pids: TFPList; OnlyFist: boolean = false);
var
  index, found: integer;
  fullyQualified: boolean;
begin
  Name := AnsiLowerCase(Name);
  fullyQualified := Name <> ExtractFilename(Name);
  found := 0;

  if fullyQualified then
  begin
      index := 0;
      while index < listProcessFullName.Count do
      begin
          if listProcessFullName.Strings[index] = Name then
          begin
              pids.Add(Pointer(listProcessFullName.Objects[index]));
              if OnlyFist then exit;
              inc(found);
          end;
          inc(index);
      end;
  end;

  if found = 0 then
  begin
      index := 0;
      while index < listProcess.Count do
      begin
          if listProcess.Strings[index] = Name then
          begin
              pids.Add(Pointer(listProcess.Objects[index]));
              if OnlyFist then exit;
          end;
          inc(index);
      end;
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
  if pid1 < pid2 then result := -1;
  if pid1 > pid2 then result := 1;
  if pid1 = pid2 then
  begin
    if THandle(Item1) < THandle(Item2) then result := -1;
    if THandle(Item1) > THandle(Item2) then result := 1;
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.SortAppWindows(list: TFPList);
begin
  list.Sort(CmpWindows);
end;
//------------------------------------------------------------------------------
function EnumWProc(h: THandle; l: LPARAM): bool; stdcall;
var
  helper: TProcessHelper absolute l;
  exstyle: PtrUInt;
  ch: array [0..10] of char;
  pid: dword;
begin
  result := true;
  inc(helper.FWindowsCount);

  if IsWindowVisible(h) then
  begin
      exstyle := GetWindowLongPtr(h, GWL_EXSTYLE);
      if exstyle and WS_EX_APPWINDOW = 0 then
      begin
          if GetWindow(h, GW_OWNER) <> THandle(0) then exit;
          if exstyle and WS_EX_TOOLWINDOW <> 0 then exit;
          if windows.GetWindowText(h, ch, 10) < 1 then exit;
          if GetProp(h, 'ITaskList_Deleted') <> 0 then exit;
          if assigned(DWM) then
            if DWM.IsWindowCloaked(h) then exit;
      end;

      if not (helper.FParentHWnd = 0) then
          if not helper.WindowsOnTheSameMonitor(h, helper.FParentHWnd) then exit;

      helper.listAppWindows.Add(pointer(h));
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.EnumAppWindows(ParentHWnd: THandle = 0);
var
  oldWindowsCount: integer;
begin
  crsection.Acquire;
  try
    oldWindowsCount := FWindowsCount;
    FWindowsCountChanged := false;
    FWindowsCount := 0;
    if not FReady then exit;
    FParentHWnd := ParentHWnd;
    listAppWindows.Clear;
    EnumWindows(@EnumWProc, LPARAM(self));
    SortAppWindows(listAppWindows);
    FWindowsCountChanged := oldWindowsCount <> FWindowsCount;
  finally
    crsection.Leave;
  end;
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
	    if h = 0 then h := FindWindow('Dwm', nil);
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
  end
  else begin
      PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
      AllowSetForeground(h);
      SetForegroundWindow(h);
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.ActivateWindowList(list: TFPList);
var
  anyVisible: boolean;
  index: integer;
  h: THandle;
begin
  anyVisible := false;
  index := 0;
  while index < list.Count do
  begin
      h := THandle(list.Items[index]);
      if IsWindowVisible(h) and not IsIconic(h) then anyVisible := true;
      inc(index);
	end;

  if anyVisible then
  begin
      index := 0;
	    while index < list.Count do
	    begin
	        h := THandle(list.Items[index]);
	        PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0);
	        inc(index);
		  end;
  end
  else begin
      index := 0;
	    while index < list.Count do
	    begin
	        h := THandle(list.Items[index]);
	        PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
          AllowSetForeground(h);
          SetForegroundWindow(h);
	        inc(index);
		  end;
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
  wlist: TFPList;
begin
  result := false;

  EnumProc;

  if ProcessExists(ProcessName) then
  begin

    EnumAppWindows;
    wlist := TFPList.Create;
    idx := 0;
    while idx < listAppWindows.count do
    begin
      wnd := THandle(listAppWindows.items[idx]);
      if GetWindowProcessName(wnd) = AnsiLowerCase(ProcessName) then wlist.Add(pointer(wnd));
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
function TProcessHelper.WindowsOnTheSameMonitor(h1, h2: THandle): boolean;
begin
  result := MonitorFromWindow(h1, MONITOR_DEFAULTTOPRIMARY) = MonitorFromWindow(h2, MONITOR_DEFAULTTOPRIMARY);
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetWindowProcessName(h: THandle): string;
var
  pid: dword;
  index: integer;
begin
  GetWindowThreadProcessId(h, @pid);
  result := '';
  index := listProcessFullName.IndexOfObject(TObject(pid));
  if index >= 0 then
  begin
    result := listProcessFullName.Strings[index];
  end else begin
    index := listProcess.IndexOfObject(TObject(pid));
    if index >= 0 then result := listProcess.Strings[index];
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//
//
//
//
//----------------------------------------------------------------------
procedure TProcessHelper.Shutdown(mode: integer);
begin
  if SetPrivilege('SeShutdownPrivilege') then ExitWindowsEx(mode, 0);
end;
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
//----------------------------------------------------------------------
function TProcessHelper.SetPrivilege(Name: string): boolean;
var
  hToken: cardinal;
  tkp, tkpo: Windows.TTokenPrivileges;
  rl: dword;
begin
  Result := False;
  rl := 0;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
  if not Windows.LookupPrivilegeValue(nil, PChar(Name), tkp.Privileges[0].Luid) then exit;
  tkp.PrivilegeCount := 1;
  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
  Windows.AdjustTokenPrivileges(hToken, False, tkp, sizeof(TTokenPrivileges), tkpo, rl);
  Result := GetLastError() = 0;
end;
//------------------------------------------------------------------------------
end.

