unit trayu;

interface
uses Windows, Classes, SysUtils, Registry, declu;

type
  TTrayController = class
  private
    FSite: TBaseSite;
    FNotifyIconOverflowWindow: HWND;
    Fx: integer;
    Fy: integer;
    FControl, FShown: boolean;
    FPoint: windows.TPoint;
    FBaseRect: windows.TRect;
  public
    constructor Create;
    function AutoTrayEnabled: boolean;
    procedure SwitchAutoTray;
    procedure EnableAutoTray;
    procedure DisableAutoTray;
    procedure Show(site: TBaseSite; host_wnd: cardinal; baseRect: windows.TRect);
    procedure Timer;
  end;

implementation
uses frmmainu;
//------------------------------------------------------------------------------
constructor TTrayController.Create;
begin
  inherited;
  FControl := false;
end;
//------------------------------------------------------------------------------
function TTrayController.AutoTrayEnabled: boolean;
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
procedure TTrayController.SwitchAutoTray;
begin
  if AutoTrayEnabled then DisableAutoTray else EnableAutoTray;
end;
//------------------------------------------------------------------------------
procedure TTrayController.EnableAutoTray;
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
procedure TTrayController.DisableAutoTray;
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
procedure TTrayController.Show(site: TBaseSite; host_wnd: cardinal; baseRect: windows.TRect);
var
  HWnd: cardinal;
  hostRect: windows.TRect;
begin
  if not AutoTrayEnabled then
  begin
    messagebox(frmmain.handle, pchar(UTF8ToAnsi(XMsgNotificationAreaIcons)), '', MB_ICONEXCLAMATION);
    frmmain.Run('control.exe', '/name Microsoft.NotificationAreaIcons', '', sw_shownormal);
    exit;
  end;

  FSite := site;
  FBaseRect := baseRect;
  GetCursorPos(FPoint);
  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: FPoint.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: FPoint.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  FNotifyIconOverflowWindow := findwindow('NotifyIconOverflowWindow', nil);
  hwnd := FindWindow('Shell_TrayWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'TrayNotifyWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'Button', nil);
  SetActiveWindow(hwnd);
  SetForegroundWindow(hwnd);
  SendMessage(hwnd, BM_CLICK, 0, 0);

  FShown := false;
  FControl := true;
end;
//------------------------------------------------------------------------------
procedure TTrayController.Timer;
var
  wRect: windows.TRect;
begin
  if FControl then
  begin
    if IsWindowVisible(FNotifyIconOverflowWindow) then
    begin
      FShown := true;
      GetWindowRect(FNotifyIconOverflowWindow, @wRect);
      if FSite = bsLeft then
      begin
        Fx := FBaseRect.Right + 20;
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      if FSite = bsTop then
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Bottom + 20;
      end
      else
      if FSite = bsRight then
      begin
        Fx := FBaseRect.Left - 20 - (wRect.Right - wRect.Left);
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Top - 20 - (wRect.Bottom - wRect.Top);
      end;
      if (wRect.Left <> Fx) or (wRect.Top <> Fy) then
        SetWindowPos(FNotifyIconOverflowWindow, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
    end
    else
      if FShown then FControl := false;
  end;
end;
//------------------------------------------------------------------------------
end.

