unit shelltraywndu;

interface
uses Windows, Classes, SysUtils, Forms, declu, GDIPAPI, dockh;

type

  { TShellTrayWndController }

  TShellTrayWndController = class
  private
    FIsEdgeReserved: boolean;
    FReservedSite: TBaseSite;
    FReservedMonitor: integer;
    FSite: TBaseSite;
    FPoint: windows.TPoint;
    FBaseRect, FMonitorRect: windows.TRect;
    Fx, FxPic: integer;
    Fy, FyPic: integer;
    FControl, FShown: boolean;
    FStartMenuWnd: THandle;
    FDesktopUserPictureWnd: THandle;
    function GetMonitorWorkareaRect(Monitor: integer): Windows.TRect;
    function GetMonitorBoundsRect(Monitor: integer): Windows.TRect;
    function GetRectCenter(rect: Windows.TRect): Windows.TPoint;
    function GetMaximizedWindows(monitorBounds: windows.TRect): TFPList;
    procedure SetWorkarea(NewWorkArea: windows.TRect);
  public
    class procedure Cleanup;
    constructor Create;
    procedure HideTaskbar(hide: boolean);
    procedure ReserveScreenEdge(DockMonitor: integer; DockSite: TBaseSite; Percent: integer; DockWindowRect: GDIPAPI.TRect; DockAutoHide: boolean);
    procedure UnreserveScreenEdge;
    procedure ShowStartMenu(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
    procedure Timer;
  end;

var ShellTrayWndController: TShellTrayWndController;

implementation
//------------------------------------------------------------------------------
class procedure TShellTrayWndController.Cleanup;
begin
  if assigned(ShellTrayWndController) then ShellTrayWndController.Free;
  ShellTrayWndController := nil;
end;
//------------------------------------------------------------------------------
constructor TShellTrayWndController.Create;
begin
  inherited;
  FControl := false;
  FIsEdgeReserved := false;
  FReservedMonitor := 0;
  FReservedSite := bsBottom;
end;
//------------------------------------------------------------------------------
function TShellTrayWndController.GetMonitorWorkareaRect(Monitor: integer): Windows.TRect;
begin
  if Monitor >= screen.MonitorCount then Monitor := screen.MonitorCount - 1;
  if Monitor >= 0 then Result := screen.Monitors[Monitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
function TShellTrayWndController.GetMonitorBoundsRect(Monitor: integer): Windows.TRect;
begin
  if Monitor >= screen.MonitorCount then Monitor := screen.MonitorCount - 1;
  if Monitor >= 0 then Result := screen.Monitors[Monitor].BoundsRect;
end;
//------------------------------------------------------------------------------
function TShellTrayWndController.GetRectCenter(rect: Windows.TRect): Windows.TPoint;
begin
  result.x := (rect.Left + rect.Right) div 2;
  result.y := (rect.Top + rect.Bottom) div 2;
end;
//------------------------------------------------------------------------------
// hide/show taskbar and start button
procedure TShellTrayWndController.HideTaskbar(hide: boolean);
var
  hwndTaskbar, hwndButton: THandle;
  buttonVisible, taskbarVisible, updateWorkarea: boolean;
  taskbarRect, taskbarMonitorWorkarea, taskbarMonitorBounds: Windows.TRect;
  monitorCenter, taskbarCenter: windows.TPoint;
  taskbarMonitorIndex: integer; // a monitor on which the taskbar is
  taskbarSite: TBaseSite;
begin
  try
    hwndTaskbar := FindWindow('Shell_TrayWnd', nil);
    hwndButton := FindWindowW('Button', pwchar(UTF8Decode(XStartButtonText)));
    taskbarVisible := IsWindowVisible(hwndTaskbar);
    buttonVisible := IsWindowVisible(hwndButton);
    updateWorkarea := false;

    // if workarea should be changed
    if hide = taskbarVisible then
    begin
      // get monitor index and site where taskbar is located
      taskbarMonitorIndex := 0;
      if GetWindowRect(hwndTaskbar, taskbarRect) then
      begin
        // monitor index
        taskbarCenter := GetRectCenter(taskbarRect);
        taskbarMonitorIndex := screen.MonitorFromPoint(taskbarCenter).MonitorNum;
        // monitor center point
        taskbarMonitorBounds := GetMonitorBoundsRect(taskbarMonitorIndex);
        taskbarMonitorWorkarea := GetMonitorWorkareaRect(taskbarMonitorIndex);
        monitorCenter := GetRectCenter(taskbarMonitorBounds);
        // taskbar site
        if taskbarRect.Bottom - taskbarRect.Top < taskbarRect.Right - taskbarRect.Left then
        begin
          if taskbarCenter.y > monitorCenter.y then taskbarSite := bsBottom else taskbarSite := bsTop;
        end else begin
          if taskbarCenter.x > monitorCenter.x then taskbarSite := bsRight else taskbarSite := bsLeft;
        end;
      end;

      // do not change WA if the taskbar and the dock occupy the same monitor/site and dock already controls WA
      updateWorkarea := not FIsEdgeReserved or (taskbarMonitorIndex <> FReservedMonitor) or (taskbarSite <> FReservedSite);

      // calculate new WA
      if updateWorkarea and hide then
      begin
        case taskbarSite of
          bsBottom: taskbarMonitorWorkarea.Bottom := taskbarMonitorBounds.Bottom;
          bsTop: taskbarMonitorWorkarea.Top := taskbarMonitorBounds.Top;
          bsLeft: taskbarMonitorWorkarea.Left := taskbarMonitorBounds.Left;
          bsRight: taskbarMonitorWorkarea.Right := taskbarMonitorBounds.Right;
        end;
      end;
      if updateWorkarea and not hide then
      begin
        case taskbarSite of
          bsBottom: taskbarMonitorWorkarea.Bottom := taskbarMonitorBounds.Bottom - taskbarRect.Bottom + taskbarRect.Top;
          bsTop: taskbarMonitorWorkarea.Top := taskbarMonitorBounds.Top + taskbarRect.Bottom - taskbarRect.Top;
          bsLeft: taskbarMonitorWorkarea.Left := taskbarMonitorBounds.Left + taskbarRect.Right - taskbarRect.Left;
          bsRight: taskbarMonitorWorkarea.Right := taskbarMonitorBounds.Right - taskbarRect.Right + taskbarRect.Left;
        end;
      end;
    end;

    // hide or show Start Button
    if hide and buttonVisible then         showwindow(hwndButton, SW_HIDE);
    if not hide and not buttonVisible then showwindow(hwndButton, SW_SHOWNORMAL);

    // hide or show Taskbar
    if hide and taskbarVisible then         showwindow(hwndTaskbar, SW_HIDE);
    if not hide and not taskbarVisible then showwindow(hwndTaskbar, SW_SHOWNORMAL);

    // update workarea
    if updateWorkarea then SetWorkarea(taskbarMonitorWorkarea);
  except
    on e: Exception do raise Exception.Create('TShellTrayWndController.HideTaskbar' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TShellTrayWndController.GetMaximizedWindows(monitorBounds: windows.TRect): TFPList;
var
  wnd: THandle;
  wp: WINDOWPLACEMENT;
  rect: windows.TRect;
  center: windows.TPoint;
begin
  result := TFPList.Create;
  wp.length := sizeof(wp);
  wnd := FindWindow('Progman', nil);
  wnd := GetWindow(wnd, GW_HWNDPREV);
  while wnd <> 0 do
  begin
    // exclude hidden and tool windows
    if IsWindowVisible(wnd) and (GetWindowLongPtr(wnd, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
    begin
      GetWindowPlacement(wnd, wp);
      GetWindowRect(wnd, rect);
      center.x := (rect.left + rect.right) div 2;
      center.y := (rect.top + rect.bottom) div 2;
      if (wp.showCmd = SW_SHOWMAXIMIZED) and PtInRect(monitorBounds, center) then result.Add(pointer(wnd));
    end;
    wnd := GetWindow(wnd, GW_HWNDPREV);
  end;
end;
//------------------------------------------------------------------------------
procedure TShellTrayWndController.SetWorkarea(NewWorkArea: windows.TRect);
var
  monitor: integer;
  bounds: windows.TRect;
  i: integer;
begin
  SystemParametersInfo(SPI_SETWORKAREA, 0, @NewWorkArea, SPIF_UPDATEINIFILE);
  monitor := screen.MonitorFromPoint(GetRectCenter(NewWorkArea)).MonitorNum;
  bounds := GetMonitorBoundsRect(monitor);
  with GetMaximizedWindows(bounds) do
  begin
    i := 0;
    while i < Count do // reposition maximized windows to match new workarea
    begin
      SetWindowPos(THandle(Items[i]), 0, NewWorkArea.Left, NewWorkArea.Top,
        NewWorkArea.Right - NewWorkArea.Left, NewWorkArea.Bottom - NewWorkArea.Top,
        SWP_NOACTIVATE + SWP_NOZORDER + SWP_NOOWNERZORDER);
      inc(i);
    end;
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure TShellTrayWndController.ReserveScreenEdge(DockMonitor: integer; DockSite: TBaseSite; Percent: integer; DockWindowRect: GDIPAPI.TRect; DockAutoHide: boolean);
var
  Changed: boolean;
  Position: integer;
  WorkArea, Bounds: Windows.TRect;
begin
  try
    FIsEdgeReserved := true;
    FReservedMonitor := DockMonitor;
    FReservedSite := DockSite;
    Changed := false;
    WorkArea := GetMonitorWorkareaRect(DockMonitor);
    Bounds := GetMonitorBoundsRect(DockMonitor);

    if DockSite = bsLeft then
    begin
      if DockAutoHide then Position := 0
      else Position := DockWindowRect.Width * Percent div 100;
      if WorkArea.Left <> Position then
      begin
        WorkArea.Left := Position;
        Changed := true;
      end;
    end
    else
    if DockSite = bsTop then
    begin
      if DockAutoHide then Position := 0
      else Position := DockWindowRect.Height * Percent div 100;
      if WorkArea.Top <> Position then
      begin
        WorkArea.Top := Position;
        Changed := true;
      end;
    end
    else
    if DockSite = bsRight then
    begin
      if DockAutoHide then Position := Bounds.Right
      else Position := Bounds.Right - DockWindowRect.Width * Percent div 100;
      if WorkArea.Right <> Position then
      begin
        WorkArea.Right := Position;
        Changed := true;
      end;
    end
    else
    if DockSite = bsBottom then
    begin
      if DockAutoHide then Position := Bounds.Bottom
      else Position := Bounds.Bottom - DockWindowRect.Height * Percent div 100;
      if WorkArea.Bottom <> Position then
      begin
        WorkArea.Bottom := Position;
        Changed := true;
      end;
    end;

    if Changed then SetWorkarea(WorkArea);
  except
    on e: Exception do raise Exception.Create('TShellTrayWndController.ReserveScreenEdge' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShellTrayWndController.UnreserveScreenEdge;
var
  Changed, MustRestoreShellWorkArea: boolean;
  Position: integer;
  WorkArea, Bounds, Shell_TrayRect: Windows.TRect;
  Shell_TrayWnd: THandle;
  Shell_TrayCenter, MonitorCenter: Windows.TPoint;
  Shell_TrayEdge: TBaseSite;
begin
  if FIsEdgeReserved then
  try
    Changed := false;
    WorkArea := GetMonitorWorkareaRect(FReservedMonitor);
    Bounds := GetMonitorBoundsRect(FReservedMonitor);

    Position := Bounds.Bottom;
    if FReservedSite = bsTop then Position := Bounds.Top
    else if FReservedSite = bsLeft then Position := Bounds.Left
    else if FReservedSite = bsRight then Position := Bounds.Right;

    // restore workarea for Shell_TrayWnd ? //
    Shell_TrayWnd := FindWindow('Shell_TrayWnd', nil);
    if IsWindowVisible(Shell_TrayWnd) then
    begin
      GetWindowRect(Shell_TrayWnd, Shell_TrayRect);
      Shell_TrayCenter := GetRectCenter(Shell_TrayRect);
      MonitorCenter := GetRectCenter(Bounds);
      Shell_TrayEdge := bsBottom;
      if Shell_TrayCenter.y < MonitorCenter.y - 10 then Shell_TrayEdge := bsTop
      else if Shell_TrayCenter.x < MonitorCenter.x - 10 then Shell_TrayEdge := bsLeft
      else if Shell_TrayCenter.x > MonitorCenter.x + 10 then Shell_TrayEdge := bsRight;
      MustRestoreShellWorkArea := ptinrect(Bounds, Shell_TrayCenter) and (Shell_TrayEdge = FReservedSite);
      if MustRestoreShellWorkArea then
      begin
        Position := Shell_TrayRect.Top;
        if Shell_TrayEdge = bsTop then Position := Shell_TrayRect.Bottom;
        if Shell_TrayEdge = bsLeft then Position := Shell_TrayRect.Right;
        if Shell_TrayEdge = bsRight then Position := Shell_TrayRect.Left;
      end;
    end;

    if FReservedSite = bsLeft then
    begin
      if WorkArea.Left <> Position then
      begin
        WorkArea.Left := Position;
        Changed := true;
      end;
    end
    else if FReservedSite = bsTop then
    begin
      if WorkArea.Top <> Position then
      begin
        WorkArea.Top := Position;
        Changed := true;
      end;
    end
    else if FReservedSite = bsRight then
    begin
      if WorkArea.Right <> Position then
      begin
        WorkArea.Right := Position;
        Changed := true;
      end;
    end
    else if FReservedSite = bsBottom then
    begin
      if WorkArea.Bottom <> Position then
      begin
        WorkArea.Bottom := Position;
        Changed := true;
      end;
    end;

    if Changed then SetWorkarea(WorkArea);
    FIsEdgeReserved := false;
  except
    on e: Exception do raise Exception.Create('TShellTrayWndController.UnreserveScreenEdge' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShellTrayWndController.ShowStartMenu(site: TBaseSite; host_wnd: THandle; baseRect, monitorRect: windows.TRect);
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

  FStartMenuWnd := findwindow('ImmersiveLauncher', nil);
  if FStartMenuWnd = 0 then FStartMenuWnd := findwindow('DV2ControlHost', nil);
  FDesktopUserPictureWnd := findwindow('Desktop User Picture', nil);
  //FDesktopSpecialFoldersWnd := findwindowex(FStartMenuWnd, 0, 'DesktopSpecialFolders', nil);

  sendmessage(host_wnd, WM_SYSCOMMAND, SC_TASKLIST, 0);
  FShown := false; // wait until start menu is shown
  FControl := true; // enable start menu position control
end;
//------------------------------------------------------------------------------
procedure TShellTrayWndController.Timer;
var
  wRect, picRect: windows.TRect;
begin
  if FControl then
  begin
    if IsWindowVisible(FStartMenuWnd) then
    begin
      FShown := true;
      GetWindowRect(FStartMenuWnd, @wRect);
      GetWindowRect(FDesktopUserPictureWnd, @picRect);
      FxPic := (wRect.Right - wRect.Left) * 3 div 4 - (picRect.Right - picRect.Left);
      FyPic := -(picRect.Bottom - picRect.Top) div 2;
      if FSite = bsLeft then
      begin
        Fx := FBaseRect.Right + 5;
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      if FSite = bsTop then
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Bottom + 5;
      end
      else
      if FSite = bsRight then
      begin
        Fx := FBaseRect.Left - 5 - (wRect.Right - wRect.Left);
        Fy := FPoint.y - (wRect.Bottom - wRect.Top) div 2;
      end
      else
      begin
        Fx := FPoint.x - (wRect.Right - wRect.Left) div 2;
        Fy := FBaseRect.Top - 5 - (wRect.Bottom - wRect.Top);
      end;
      if Fx < FMonitorRect.Left then Fx := FMonitorRect.Left;
      if Fx > FMonitorRect.Right - wRect.Right + wRect.Left then Fx := FMonitorRect.Right - wRect.Right + wRect.Left;
      if Fy < FMonitorRect.Top then Fy := FMonitorRect.Top;
      if Fy > FMonitorRect.Bottom - wRect.Bottom + wRect.Top then Fy := FMonitorRect.Bottom - wRect.Bottom + wRect.Top;

      if (wRect.Left <> Fx) or (wRect.Top <> Fy) then
      begin
        SetWindowPos(FStartMenuWnd, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
        SetWindowPos(FDesktopUserPictureWnd, 0, Fx + FxPic, Fy + FyPic, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
      end;
      //PostMessage(FDesktopSpecialFoldersWnd, WM_ERASEBKGND, 0, 0);
    end
    else
      if FShown then FControl := false;
  end;
end;
//------------------------------------------------------------------------------
end.

