unit startmenu;

interface
uses Windows, Classes, SysUtils, declu;

type
  TStartMenuController = class
  private
    FSite: TBaseSite;
    Fx, FxPic: integer;
    Fy, FyPic: integer;
    FControl: boolean;
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
  FxPic := -1;
end;
//------------------------------------------------------------------------------
procedure TStartMenuController.Show(site: TBaseSite; host_wnd: cardinal; baseRect, monitorRect: windows.TRect);
var
  wRect, picRect, hostRect: windows.TRect;
  pt: windows.TPoint;
begin
  FSite := site;
  GetCursorPos(pt);

  FStartMenuWnd := findwindow('ImmersiveLauncher', nil);
  if FStartMenuWnd = 0 then FStartMenuWnd := findwindow('DV2ControlHost', nil);
  FDesktopUserPictureWnd := findwindow('Desktop User Picture', nil);
  //FDesktopSpecialFoldersWnd := findwindowex(FStartMenuWnd, 0, 'DesktopSpecialFolders', nil);
  GetWindowRect(FStartMenuWnd, @wRect);
  GetWindowRect(FDesktopUserPictureWnd, @picRect);
  FxPic := (wRect.Right - wRect.Left) * 3 div 4 - (picRect.Right - picRect.Left);
  FyPic := -(picRect.Bottom - picRect.Top) div 2;

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
    Fx := baseRect.Right + 5;
    Fy := pt.y - (wRect.Bottom - wRect.Top) div 2;
  end
  else
  if FSite = bsTop then
  begin
    Fx := pt.x - (wRect.Right - wRect.Left) div 2;
    Fy := baseRect.Bottom + 5;
  end
  else
  if FSite = bsRight then
  begin
    Fx := baseRect.Left - 5 - (wRect.Right - wRect.Left);
    Fy := pt.y - (wRect.Bottom - wRect.Top) div 2;
  end
  else
  begin
    Fx := pt.x - (wRect.Right - wRect.Left) div 2;
    Fy := baseRect.Top - 5 - (wRect.Bottom - wRect.Top);
  end;
  if Fx < monitorRect.Left then Fx := monitorRect.Left;
  if Fx > monitorRect.Right - wRect.Right + wRect.Left then Fx := monitorRect.Right - wRect.Right + wRect.Left;
  if Fy < monitorRect.Top then Fy := monitorRect.Top;
  if Fy > monitorRect.Bottom - wRect.Bottom + wRect.Top then Fy := monitorRect.Bottom - wRect.Bottom + wRect.Top;

  sendmessage(host_wnd, WM_SYSCOMMAND, SC_TASKLIST, 0);

  SetWindowPos(FStartMenuWnd, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER + SWP_SHOWWINDOW);
  SetWindowPos(FDesktopUserPictureWnd, 0, Fx + FxPic, Fy + FyPic, 0, 0, SWP_NOSIZE + SWP_NOZORDER + SWP_SHOWWINDOW);
  FControl := true;
end;
//------------------------------------------------------------------------------
procedure TStartMenuController.Timer;
var
  wRect: windows.TRect;
begin
  if FControl then
  begin
    if IsWindowVisible(FStartMenuWnd) then
    begin
      GetWindowRect(FStartMenuWnd, @wRect);
      if (wRect.Left <> Fx) or (wRect.Top <> Fy) then
      begin
        SetWindowPos(FStartMenuWnd, 0, Fx, Fy, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
        SetWindowPos(FDesktopUserPictureWnd, 0, Fx + FxPic, Fy + FyPic, 0, 0, SWP_NOSIZE + SWP_NOZORDER);
      end;
      //PostMessage(FDesktopSpecialFoldersWnd, WM_ERASEBKGND, 0, 0);
    end
    else FControl := false;
  end;
end;
//------------------------------------------------------------------------------
end.

