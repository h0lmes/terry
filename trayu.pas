unit trayu;

interface
uses Windows, Classes, SysUtils, Registry, declu, processhlp;

type
  TTCWindowType = (tcwtTrayOverflow, tcwtVolume, tcwtNetworks);

  { TTrayController }

  TTrayController = class
  private
    FSite: TBaseSite;
    FControlWindow: HWND;
    Fx: integer;
    Fy: integer;
    FControl, FShown: boolean;
    FPoint: windows.TPoint;
    FBaseRect, FMonitorRect: windows.TRect;
    procedure RunAvailableNetworks;
    procedure RunNotificationAreaIcons;
    procedure RunDateAndTime;
    procedure RunPowerOptions;
    procedure RunMobilityCenter;
  public
    constructor Create;
    function AutoTrayEnabled: boolean;
    procedure SwitchAutoTray;
    procedure EnableAutoTray;
    procedure DisableAutoTray;
    function GetLangIDString: string;
    procedure ShowTrayOverflow(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
    procedure ShowVolumeControl(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
    procedure ShowNetworks(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
    procedure ShowBattery(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
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
function TTrayController.GetLangIDString: string;
var
  langID, procID: dword;
begin
  result := '--';
  langID := $ffff and GetKeyboardLayout(GetWindowThreadProcessId(GetForegroundWindow(), procID));
  case langID of
    $0419: result := 'RU'; // ru-RU
    $046d: result := 'RU'; // ba-RU
    $0444: result := 'RU'; // tt-RU
    $0485: result := 'RU'; // sah-RU
    $0423: result := 'BY'; // be-BY
    $0409: result := 'EN';
    $042b: result := 'AM'; // hy-AM
    $0407: result := 'DE';
    $0410: result := 'IT';
    $040c: result := 'FR';
    $047e: result := 'FR'; // br-FR
    $0483: result := 'FR'; // co-FR
    $0403: result := 'ES'; // ca-ES
    $0405: result := 'CZ'; // cs-CZ
    $043b: result := 'NO';
    $103b: result := 'NO'; // smj-NO
  end;
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowTrayOverflow(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  HWnd: cardinal;
  hostRect: windows.TRect;
begin
  if not AutoTrayEnabled then
  begin
    messagebox(frmmain.handle, pchar(UTF8ToAnsi(XMsgNotificationAreaIcons)), '', MB_ICONEXCLAMATION);
    RunNotificationAreaIcons;
    exit;
  end;

  FSite := site;
  FBaseRect := baseRect;
  FMonitorRect := monitorRect;
  GetCursorPos(FPoint);
  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: FPoint.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: FPoint.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  FControlWindow := findwindow('NotifyIconOverflowWindow', nil);
  FShown := false;
  FControl := true;
  // open Tray overflow window
  hwnd := FindWindow('Shell_TrayWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'TrayNotifyWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'Button', nil);
  SetActiveWindow(hwnd);
  SetForegroundWindow(hwnd);
  SendMessage(hwnd, BM_CLICK, 0, 0);
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowVolumeControl(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  HWnd: cardinal;
  hostRect: windows.TRect;
begin
  FSite := site;
  FBaseRect := baseRect;
  FMonitorRect := monitorRect;
  GetCursorPos(FPoint);
  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: FPoint.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: FPoint.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  FControlWindow := 0;
  FShown := false;
  FControl := false;
  // open volume control
  frmmain.Run('sndvol.exe', '-f ' + inttostr(FPoint.x + FPoint.y * $10000), '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowNetworks(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  HWnd: cardinal;
  hostRect: windows.TRect;
begin
  FSite := site;
  FBaseRect := baseRect;
  FMonitorRect := monitorRect;
  GetCursorPos(FPoint);
  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: FPoint.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: FPoint.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  FControlWindow := findwindow('NativeHWNDHost', 'View Available Networks');
  FShown := false;
  FControl := true;
  // open View Available Networks
  RunAvailableNetworks;
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowBattery(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  HWnd: cardinal;
  hostRect: windows.TRect;
begin
  FSite := site;
  FBaseRect := baseRect;
  FMonitorRect := monitorRect;
  GetCursorPos(FPoint);
  if IsWindow(host_wnd) then
  begin
    GetWindowRect(host_wnd, @hostRect);
    case FSite of
      bsLeft, bsRight: FPoint.y := (hostRect.Top + hostRect.Bottom) div 2;
      bsTop, bsBottom: FPoint.x := (hostRect.Left + hostRect.Right) div 2;
    end;
  end;

  FControlWindow := findwindow('BatMeterFlyout', nil);
  FShown := false;
  FControl := true;
  // open Battery Meter Flyout
  showwindow(FControlWindow, sw_show);
  processhelper.AllowSetForeground(FControlWindow);
  SetActiveWindow(FControlWindow);
  SetForegroundWindow(FControlWindow);
end;
//------------------------------------------------------------------------------
procedure TTrayController.Timer;
var
  wRect: windows.TRect;
begin
  if FControl then
  begin
    if IsWindowVisible(FControlWindow) then
    begin
      FShown := true;
      GetWindowRect(FControlWindow, @wRect);
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
      if Fx < FMonitorRect.Left then Fx := FMonitorRect.Left;
      if Fx > FMonitorRect.Right - wRect.Right + wRect.Left then Fx := FMonitorRect.Right - wRect.Right + wRect.Left;
      if Fy < FMonitorRect.Top then Fy := FMonitorRect.Top;
      if Fy > FMonitorRect.Bottom - wRect.Bottom + wRect.Top then Fy := FMonitorRect.Bottom - wRect.Bottom + wRect.Top;

      if (wRect.Left <> Fx) or (wRect.Top <> Fy) then
        SetWindowPos(FControlWindow, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
    end
    else
      if FShown then FControl := false;
  end;
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunAvailableNetworks;
begin
  frmmain.Run('rundll32.exe', 'van.dll,RunVAN', '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunNotificationAreaIcons;
begin
  frmmain.Run('control.exe', '/name Microsoft.NotificationAreaIcons', '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunDateAndTime;
begin
  frmmain.Run('control.exe', '/name Microsoft.DateAndTime', '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunPowerOptions;
begin
  frmmain.Run('control.exe', '/name Microsoft.PowerOptions', '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunMobilityCenter;
begin
  frmmain.Run('control.exe', '/name Microsoft.MobilityCenter', '', sw_shownormal);
end;
//------------------------------------------------------------------------------
end.

