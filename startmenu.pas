unit startmenu;

interface
uses Windows, Classes, SysUtils, declu;

type
  TStartMenuController = class
  private
    FSite: TBaseSite;
    FPoint: windows.TPoint;
    FBaseRect, FMonitorRect: windows.TRect;
    Fx, FxPic: integer;
    Fy, FyPic: integer;
    FControl, FShown: boolean;
    FStartMenuWnd: HWND;
    FDesktopUserPictureWnd: HWND;
    FDesktopSpecialFoldersWnd: HWND;
  public
    constructor Create;
    procedure Show(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
    procedure Timer;
  end;

implementation
//------------------------------------------------------------------------------
constructor TStartMenuController.Create;
begin
  inherited;
  FControl := false;
end;
//------------------------------------------------------------------------------
procedure TStartMenuController.Show(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  wRect, picRect, hostRect: windows.TRect;
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
procedure TStartMenuController.Timer;
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

