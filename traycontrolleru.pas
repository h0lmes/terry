unit traycontrolleru;

interface
uses Windows, Classes, SysUtils, Registry, declu;

type
  _TrayController = class
  private
    FSite: TBaseSite;
    Fx: integer;
    Fy: integer;
    FControl: boolean;
  public
    constructor Create;
    function AutoTrayEnabled: boolean;
    procedure SwitchAutoTray;
    procedure EnableAutoTray;
    procedure DisableAutoTray;
    procedure Show(site: TBaseSite; host_wnd: cardinal = 0);
    procedure Timer;
  end;

implementation
uses frmterryu;
//------------------------------------------------------------------------------
constructor _TrayController.Create;
begin
  inherited;
  FControl := false;
end;
//------------------------------------------------------------------------------
function _TrayController.AutoTrayEnabled: boolean;
begin
  result := false;
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_CURRENT_USER;
    result := 1 = ReadInteger('Software\Microsoft\Windows\CurrentVersion\Explorer', 'EnableAutoTray', 0);
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure _TrayController.SwitchAutoTray;
begin
  if AutoTrayEnabled then DisableAutoTray else EnableAutoTray;
end;
//------------------------------------------------------------------------------
procedure _TrayController.EnableAutoTray;
begin
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_CURRENT_USER;
    LazyWrite := False;
    WriteInteger('Software\Microsoft\Windows\CurrentVersion\Explorer', 'EnableAutoTray', 1);
    Free;
  end;

  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0, SMTO_ABORTIFHUNG, 5000, nil);
end;
//------------------------------------------------------------------------------
procedure _TrayController.DisableAutoTray;
begin
  with TRegIniFile.Create do
  begin
    RootKey := HKEY_CURRENT_USER;
    LazyWrite := False;
    WriteInteger('Software\Microsoft\Windows\CurrentVersion\Explorer', 'EnableAutoTray', 0);
    Free;
  end;

  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0, SMTO_ABORTIFHUNG, 5000, nil);
end;
//------------------------------------------------------------------------------
procedure _TrayController.Show(site: TBaseSite; host_wnd: cardinal = 0);
var
  HWnd: cardinal;
  baseRect, wRect, hostRect: windows.TRect;
  pt: windows.TPoint;
begin
  if not AutoTrayEnabled then
  begin
    messagebox(frmterry.handle, pchar(UTF8ToAnsi(XMsgNotificationAreaIcons)), '', MB_ICONEXCLAMATION);
    frmterry.Run('control.exe', '/name Microsoft.NotificationAreaIcons', '', sw_shownormal);
    exit;
  end;

  FSite := site;

  GetCursorPos(pt);

  hwnd := FindWindow('Window', 'TerryApp');
  GetWindowRect(hwnd, @baseRect);

  hwnd := FindWindow('Shell_TrayWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'TrayNotifyWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'Button', nil);
  SendMessage(hwnd, BM_CLICK, 0, 0);

  hwnd := findwindow('NotifyIconOverflowWindow', nil);
  GetWindowRect(hwnd, @wRect);

  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: pt.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: pt.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  if FSite = bsLeft then
  begin
    Fx := baseRect.Right + 20;
    Fy := pt.y - (wRect.Bottom - wRect.Top) div 2;
  end
  else
  if FSite = bsTop then
  begin
    Fx := pt.x - (wRect.Right - wRect.Left) div 2;
    Fy := baseRect.Bottom + 20;
  end
  else
  if FSite = bsRight then
  begin
    Fx := baseRect.Left - 20 - (wRect.Right - wRect.Left);
    Fy := pt.y - (wRect.Bottom - wRect.Top) div 2;
  end
  else
  begin
    Fx := pt.x - (wRect.Right - wRect.Left) div 2;
    Fy := baseRect.Top - 20 - (wRect.Bottom - wRect.Top);
  end;

  SetWindowPos(hwnd, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER + SWP_SHOWWINDOW);
  FControl := true;
end;
//------------------------------------------------------------------------------
procedure _TrayController.Timer;
var
  HWnd: cardinal;
  wRect: windows.TRect;
begin
  if FControl then
  begin
    HWnd := findwindow('NotifyIconOverflowWindow', nil);
    if IsWindowVisible(HWnd) then
    begin
      GetWindowRect(hwnd, @wRect);
      if (wRect.Left <> Fx) or (wRect.Top <> Fy) then SetWindowPos(HWnd, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
    end
    else FControl := false;
  end;
end;
//------------------------------------------------------------------------------
end.

