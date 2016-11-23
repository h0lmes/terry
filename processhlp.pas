unit processhlp;

{$mode delphi}
{$undef EXT_DEBUG}

interface

uses Windows, jwaWindows, SysUtils, Classes, Forms, Syncobjs,
  declu, dwm_unit, loggeru;

type
  {TProcessHelper}

  TProcessHelper = class(TObject)
  private
    FReady: boolean;
    FVistaOrHigher: boolean;
    FWin7OrHigher: boolean;
    FWin10: boolean;
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
    QueryFullProcessImageName: function(hProcess: THandle; dwFlags: dword; lpExeName: PWChar; var lpdwSize: dword): boolean; stdcall;
    // processes //
    procedure EnumProc32;
    procedure EnumProc64;
    procedure AddProcessById(processId: DWORD);
    procedure GetProcessPIDs(Name: string; var pids: TFPList; OnlyFist: boolean = false);
    function GetFullNameByHProcess(hProcess: THandle): string;
    function SetPrivilege(Name: string): boolean;
    function GetProcesses: string;
    function GetProcessesFullName: string;
  public
    ForegroundWindowHandle: THandle;
    property Ready: boolean read FReady;
    property WindowsCountChanged: boolean read FWindowsCountChanged;
    property Processes: string read GetProcesses;
    property ProcessesFullName: string read GetProcessesFullName;
    // //
    class procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
    procedure EnterCRS;
    procedure LeaveCRS;
    // processes //
    procedure EnumProc;
    procedure Kill(Name: string);
    function ProcessExists(Name: string): boolean;
    function GetWindowProcessName(h: THandle): string;
    function GetProcessWindowsCount(Name: string): integer;
    procedure GetProcessWindows(Name: string; var AppList: TFPList); overload;
    procedure GetProcessWindows(pid: dword; var AppList: TFPList); overload;
    function GetModernAppProcessPathByHWND(hwnd: THandle): string;
    // windows //
    class function GetWindowText(wnd: THandle): WideString;
    procedure SetForegroundWindow(wnd: THandle);
    function IsForegroundWindow(wnd: THandle): boolean;
    procedure ActivateWindow(h: THandle; Force: boolean = false);
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
function windowEnumProc(h: THandle; lp: LPARAM): WINBOOL; stdcall;
var
  list: TFPList absolute lp;
begin
  list.Add(pointer(h));
  result := true;
end;
//------------------------------------------------------------------------------
class procedure TProcessHelper.Cleanup;
begin
  if assigned(ProcessHelper) then ProcessHelper.Free;
  ProcessHelper := nil;
end;
//------------------------------------------------------------------------------
constructor TProcessHelper.Create;
var
  VerInfo: windows.TOSVersioninfo;
begin
  FReady := false;
  inherited Create;

  ForegroundWindowHandle := 0;
  VerInfo.dwOSVersionInfoSize:= sizeof(TOSVersionInfo);
  windows.GetVersionEx(VerInfo);
  FVistaOrHigher := VerInfo.dwMajorVersion >= 6;
  FWin7OrHigher := (VerInfo.dwMajorVersion > 6) or ((VerInfo.dwMajorVersion = 6) and (VerInfo.dwMinorVersion >= 2));
  FWin10 := VerInfo.dwMajorVersion >= 10;

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

  if FVistaOrHigher then
  begin
    hKernel32 := GetModuleHandle('KERNEL32.DLL');
    if hKernel32 = 0 then hKernel32 := LoadLibrary('KERNEL32.DLL');
    if hKernel32 <> 0 then
      {$ifdef CPU64}
      @QueryFullProcessImageName := GetProcAddress(hKernel32, 'QueryFullProcessImageNameW');
      {$else CPU64}
      @QueryFullProcessImageName := GetProcAddress(hKernel32, 'QueryFullProcessImageNameA');
      {$endif CPU64}
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
function TProcessHelper.GetProcesses: string;
var
  i: integer = 0;
begin
  result := '';
  while i < listProcess.Count do
  begin
    result := result + listProcess.Strings[i] + LineEnding;
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetProcessesFullName: string;
var
  i: integer = 0;
begin
  result := '';
  while i < listProcessFullName.Count do
  begin
    result := result + listProcessFullName.Strings[i] + LineEnding;
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.EnumProc;
begin
  EnumProc64;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.EnumProc32;
var
  snapshotHandle: THandle;
  hProcess: THandle;
  processEntry: TProcessEntry32;
  flag: bool;
  idx: integer;
begin
  crsection.Acquire;
  try
    if not FReady then exit;
    listProcess.Clear;
    snapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    {$ifdef EXT_DEBUG} AddLog('___CreateToolhelp32Snapshot'); {$endif}
    if snapshotHandle < 32 then
    begin
      snapshotHandle := GetLastError;
      raise Exception.Create('CreateToolhelp32Snapshot error ' + inttostr(snapshotHandle));
      exit;
    end;
    processEntry.dwSize := sizeof(TProcessEntry32);
    flag := Process32First(snapshotHandle, processEntry);
    {$ifdef EXT_DEBUG} AddLog('___Process32First'); {$endif}
    while flag do
    begin
      {$ifdef EXT_DEBUG} AddLog('___Process = ' + LowerCase(processEntry.szExeFile)); {$endif}
      listProcess.AddObject(LowerCase(processEntry.szExeFile), TObject(processEntry.th32ProcessID));
      if listProcessFullName.IndexOfObject(tobject(processEntry.th32ProcessID)) < 0 then
      begin
        hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, processEntry.th32ProcessID);
        listProcessFullName.AddObject(LowerCase(GetFullNameByHProcess(hProcess)), TObject(processEntry.th32ProcessID));
        CloseHandle(hProcess);
      end;
      flag := Process32Next(snapshotHandle, processEntry);
      {$ifdef EXT_DEBUG} AddLog('___Process32Next'); {$endif}
    end;
    CloseHandle(snapshotHandle);

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
procedure TProcessHelper.EnumProc64;
var
  aProcesses: array [0..1023] of DWORD;
  cbNeeded: DWORD;
  cProcesses: DWORD;
  i: integer;
begin
  crsection.Acquire;
  try
    if not FReady then exit;
    listProcess.Clear;

    if not EnumProcesses(aProcesses, sizeof(aProcesses), cbNeeded) then
      raise Exception.Create('ProcessHelper.EnumProc64.EnumProcesses() failed');
    cProcesses := cbNeeded div sizeof(DWORD);
    i := 0;
    while i < cProcesses do
    begin
      if aProcesses[i] <> 0 then AddProcessById(aProcesses[i]);
      inc(i);
    end;

    // delete non-existing processes from listProcessFullName //
    i := listProcessFullName.Count - 1;
    while i >= 0 do
    begin
      if listProcess.IndexOfObject(listProcessFullName.Objects[i]) < 0 then listProcessFullName.Delete(i);
      dec(i);
    end;
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.AddProcessById(processId: DWORD);
var
  hProcess: THandle;
  hMod: HMODULE;
  cbNeeded: DWORD;
  szProcessName: array [0..MAX_PATH - 1] of TCHAR;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, processId);
  if hProcess <> 0 then
  begin
    if EnumProcessModules(hProcess, @hMod, sizeof(hMod), cbNeeded) then
    begin
      GetModuleBaseName(hProcess, hMod, @szProcessName, sizeof(szProcessName) div sizeof(TCHAR));
      listProcess.AddObject(LowerCase(strpas(szProcessName)), TObject(processId));
      if listProcessFullName.IndexOfObject(tobject(processId)) < 0 then
        listProcessFullName.AddObject(LowerCase(GetFullNameByHProcess(hProcess)), TObject(processId));
    end;
    CloseHandle(hProcess);
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetFullNameByHProcess(hProcess: THandle): string;
var
  size: dword;
  buff: array [0..MAX_PATH - 1] of WideChar;
begin
  result := '';
  size := MAX_PATH;
  ZeroMemory(@buff, MAX_PATH);

  if assigned(QueryFullProcessImageName) then
  begin
      QueryFullProcessImageName(hProcess, 0, buff, size);
      GetLongPathNameW(buff, buff, MAX_PATH);
      result := strpas(pwchar(@buff));
  end;

  if result = '' then
  begin
    GetModuleFileNameExW(hProcess, 0, buff, MAX_PATH);
    result := strpas(pwchar(@buff));
  end;
end;
//------------------------------------------------------------------------------
// kill a process by its name
// name must be either a fully qualified pathname or just a filename.exe
procedure TProcessHelper.Kill(Name: string);
var
  index: integer;
  hProc: THandle;
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
// returns main window handles count belonging to a specified process.
// 'Name' could be either a fully qualified path + filename or just a filename.exe
function TProcessHelper.GetProcessWindowsCount(Name: string): integer;
var
  index: integer;
  wnd: THandle;
  wpid: DWORD;
  pids, newAppList: TFPList;
begin
  result := 0;
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
  result := newAppList.Count;
  newAppList.free;
end;
//------------------------------------------------------------------------------
// returns main window handles belonging to a specified process.
// 'Name' could be either a fully qualified path + filename or just a filename.exe
procedure TProcessHelper.GetProcessWindows(Name: string; var AppList: TFPList);
var
  index: integer;
  wnd: THandle;
  wpid: DWORD;
  pids, newAppList: TFPList;
begin
  if not FReady then exit;
  newAppList := TFPList.Create;
  pids := TFPList.Create;
  {$ifdef EXT_DEBUG} AddLog('ProcessHelper.GetProcessWindows(' + Name + ')'); {$endif}

  try
    GetProcessPIDs(Name, pids);
    {$ifdef EXT_DEBUG} AddLog('pids.Count = ' + inttostr(pids.Count)); {$endif}
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
  Name := LowerCase(Name);
  fullyQualified := ExtractFilePath(Name) <> '';
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
      begin      TProcessHelper
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
function TProcessHelper.IsModernAppHWND(hwnd: THandle): boolean;
var
  className: array [0..20] of wchar;
begin
  result := false;
  if GetClassNameW(hwnd, @className, 20) > 0 then
  begin
    result := strpas(pwchar(@className)) = 'ApplicationFrameHost';
  end;
end;
//------------------------------------------------------------------------------
function TProcessHelper.GetModernAppProcessPathByHWND(hwnd: THandle): string;
var
  i: integer;
  pid: UINT = 0;
  childPid: UINT = 0;
  list: TFPList;
  hProcess: THandle;
begin
  result := '';
  GetWindowThreadProcessId(hwnd, out pid);
  list := TFPList.Create;
  windows.EnumChildWindows(hwnd, @windowEnumProc, LPARAM(list));
  i := 0;
  while i < list.Count do
  begin
    GetWindowThreadProcessId(THandle(list.Items[i]), out childPid);
    if childPid <> pid then
    begin
      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, childPid);
      result := GetFullNameByHProcess(hProcess);
      CloseHandle(hProcess);
      break;
    end;
    inc(i);
  end;
  list.free;
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
class function TProcessHelper.GetWindowText(wnd: THandle): WideString;
var
  name: array [0..255] of wchar;
begin
  windows.GetWindowTextW(wnd, @name[0], 255);
  result := strpas(pwchar(@name[0]));
end;
//------------------------------------------------------------------------------
procedure TProcessHelper.SetForegroundWindow(wnd: THandle);
var
  dwProcess: dword;
begin
  if assigned(AllowSetForegroundWindow) then
  begin
    dwProcess := 0;
    GetWindowThreadProcessId(wnd, @dwProcess);
    AllowSetForegroundWindow(dwProcess);
  end;
  windows.SetForegroundWindow(wnd);
  windows.SetActiveWindow(wnd);
end;
//------------------------------------------------------------------------------
function TProcessHelper.IsForegroundWindow(wnd: THandle): boolean;
    function ZOrderIndex(wnd: THandle): integer;
    var
      index: integer;
      h: HWND;
    begin
      result := 0;
      index := 0;
	    h := FindWindow('Progman', nil);
	    if h = 0 then h := FindWindow('Dwm', nil);
	    while (h <> 0) and (h <> wnd) do
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
  if ForegroundWindowHandle <> THandle(0) then
  begin
    result := wnd = ForegroundWindowHandle;
    exit;
  end;

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
procedure TProcessHelper.ActivateWindow(h: THandle; Force: boolean = false);
begin
  if Force then
  begin
    if IsIconic(h) then PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
    SetForegroundWindow(h);
    exit;
  end;

  if IsWindowVisible(h) and not IsIconic(h) then
  begin
      if IsForegroundWindow(h) then PostMessage(h, WM_SYSCOMMAND, SC_MINIMIZE, 0)
      else SetForegroundWindow(h);
  end
  else begin
      PostMessage(h, WM_SYSCOMMAND, SC_RESTORE, 0);
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
  hMenu: THandle;
  menu_align: cardinal;
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
      if GetWindowProcessName(wnd) = LowerCase(ProcessName) then wlist.Add(pointer(wnd));
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
        AppendMenuW(hMenu, MF_STRING, $100 + idx, pwchar(WideString(GetWindowText(THandle(wlist.items[idx])))));
        inc(idx);
      end;
      AppendMenuW(hMenu, MF_SEPARATOR, 0, '-');
      AppendMenuW(hMenu, MF_STRING, $2, 'Run program');

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
//------------------------------------------------------------------------------
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
  hToken: THandle;
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

