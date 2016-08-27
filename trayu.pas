//------------------------------------------------------------------------------
//
//
//
// This unit implements functionality to interact with
// shell tray and tray icons
//
//
//
//------------------------------------------------------------------------------

unit trayu;

interface
uses Windows, Classes, SysUtils, Registry, declu, processhlp;

const
  CTLW_OFFSET = 10;

type

  { TTrayController }

  TTrayController = class
  private
    FParentHWnd: THandle;
    FSite: TBaseSite;
    FControlWindow: THandle;
    Fx: integer;
    Fy: integer;
    FControl: boolean;
    FShown: boolean;
    FPoint: windows.TPoint;
    FBaseRect: windows.TRect;
    FMonitorRect: windows.TRect;
    FWin10: boolean;
    procedure RunAvailableNetworks;
    procedure RunNotificationAreaIcons;
    procedure RunDateAndTime;
    procedure RunPowerOptions;
    procedure RunMobilityCenter;
    procedure Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
  public
    constructor Create;
    function AutoTrayEnabled: boolean;
    procedure SwitchAutoTray;
    procedure EnableAutoTray;
    procedure DisableAutoTray;
    procedure ShowTrayOverflow(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
		procedure ShowActionCenter(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
    procedure ShowVolumeControl(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
    procedure ShowNetworks(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
    procedure ShowBattery(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
    procedure Timer;
  end;

implementation
//------------------------------------------------------------------------------
constructor TTrayController.Create;
var
  VerInfo: windows.TOSVersioninfo;
begin
  inherited;
  FControl := false;
  FParentHWnd := FindWindow('Window', PROGRAM_NAME);
  VerInfo.dwOSVersionInfoSize:= sizeof(TOSVersionInfo);
  windows.GetVersionEx(VerInfo);
  FWin10 := VerInfo.dwMajorVersion >= 10;
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
procedure TTrayController.ShowTrayOverflow(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
var
  HWnd: THandle;
  hostRect: windows.TRect;
begin
  if not AutoTrayEnabled then
  begin
    messageboxw(FParentHWnd, pwchar(UTF8Decode(XMsgNotificationAreaIcons)), '', MB_ICONEXCLAMATION);
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
// win 10 feature. not yet working. TODO
procedure TTrayController.ShowActionCenter(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
var
  HWnd: THandle;
begin
  FControlWindow := 0;
  FShown := false;
  FControl := false;
  // open Win 10 style Action Center window
  hwnd := FindWindow('Shell_TrayWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'TrayNotifyWnd', nil);
  hwnd := FindWindowEx(hwnd, 0, 'TrayButton', nil);
  SetActiveWindow(hwnd);
  SetForegroundWindow(hwnd);
  SendMessage(hwnd, BM_CLICK, 0, 0); // this for sure does a click but nothing happens
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowVolumeControl(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
var
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
  Run('sndvol.exe', '-f ' + inttostr(FPoint.x + FPoint.y * $10000), '', sw_shownormal);
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowNetworks(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
var
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

  // open View Available Networks
  RunAvailableNetworks;
  if FWin10 then
  begin
    FControlWindow := findwindow('NativeHWNDHost', 'View Available Networks');
    FShown := false;
    FControl := true;
	end else begin
    FControlWindow := 0;
    FShown := false;
    FControl := false;
	end;
end;
//------------------------------------------------------------------------------
procedure TTrayController.ShowBattery(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
var
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
        Fx := FBaseRect.Right + CTLW_OFFSET;
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      if FSite = bsTop then
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Bottom + CTLW_OFFSET;
      end
      else
      if FSite = bsRight then
      begin
        Fx := FBaseRect.Left - CTLW_OFFSET - (wRect.Right - wRect.Left);
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Top - CTLW_OFFSET - (wRect.Bottom - wRect.Top);
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
  if FWin10 then Run('ms-settings:network')
  else Run('rundll32.exe', 'van.dll,RunVAN');
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunNotificationAreaIcons;
begin
  Run('control.exe', '/name Microsoft.NotificationAreaIcons');
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunDateAndTime;
begin
  Run('control.exe', '/name Microsoft.DateAndTime');
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunPowerOptions;
begin
  Run('control.exe', '/name Microsoft.PowerOptions');
end;
//------------------------------------------------------------------------------
procedure TTrayController.RunMobilityCenter;
begin
  Run('control.exe', '/name Microsoft.MobilityCenter');
end;
//------------------------------------------------------------------------------
procedure TTrayController.Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
var
  pparams, pdir: pchar;
begin
  try
    pparams := nil;
    pdir := nil;
    if params <> '' then pparams := PChar(params);
    if dir <> '' then pdir := PChar(dir);
    shellexecute(FParentHWnd, nil, pchar(exename), pparams, pdir, showcmd);
  except
    on e: Exception do raise Exception.Create('TrayController.Run ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

