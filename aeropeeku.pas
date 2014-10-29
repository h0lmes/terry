unit aeropeeku;

interface

uses Windows, Messages, Classes, SysUtils, Forms, uxTheme, themes,
  declu, dwm_unit, GDIPAPI, gdip_gfx, toolu, processhlp;

type

  { TAeroPeekWindow }

  TAeroPeekWindow = class
  private
    FHWnd: uint;
    WindowClassInstance: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    VSplit, Border, Shadow, ThumbW, ThumbH: integer;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FWTarget: integer;
    FHTarget: integer;
    FActivating: boolean;
    FActive: boolean;
    FMonitor: integer;
    FAnimate: boolean;
    list: TFPList;
    listThumbnail: TFPList;
    procedure Paint;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    procedure Timer;
    procedure WindowProc(var msg: TMessage);
    procedure err(where: string; e: Exception);
  public
    property Handle: uint read FHWnd;
    property Active: boolean read FActive;

    class function Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    class procedure SetPosition(AX, AY: integer; AMonitor: integer);
    class procedure Close(Timeout: cardinal = 0);
    class function IsActive: boolean;
    class procedure Cleanup;

    constructor Create;
    destructor Destroy; override;
    function OpenWindow(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    procedure SetWindowPosition(AX, AY: integer; AMonitor: integer);
    procedure CloseWindow;
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
//------------------------------------------------------------------------------
// open (show) AeroPeekWindow
class function TAeroPeekWindow.Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.OpenWindow(AppList, AX, AY, AMonitor);
end;
//------------------------------------------------------------------------------
// set new position
class procedure TAeroPeekWindow.SetPosition(AX, AY: integer; AMonitor: integer);
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.SetWindowPosition(AX, AY, AMonitor);
end;
//------------------------------------------------------------------------------
// close AeroPeekWindow
// if Timeout set then close after timeout has elapsed
class procedure TAeroPeekWindow.Close(Timeout: cardinal = 0);
begin
  if assigned(AeroPeekWindow) then
  begin
    if Timeout = 0 then AeroPeekWindow.CloseWindow
    else SetTimer(AeroPeekWindow.Handle, ID_TIMER_CLOSE, Timeout, nil);
  end;
end;
//------------------------------------------------------------------------------
// check if AeroPeekWindow is visible
class function TAeroPeekWindow.IsActive: boolean;
begin
  result := false;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.Active;
end;
//------------------------------------------------------------------------------
// destroy window
class procedure TAeroPeekWindow.Cleanup;
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.Free;
  AeroPeekWindow := nil;
end;
//------------------------------------------------------------------------------
constructor TAeroPeekWindow.Create;
begin
  inherited;
  FActive := false;
  FAnimate := true;
  list := TFPList.Create;
  listThumbnail := TFPList.Create;

  // create window //
  FHWnd := 0;
  try
    FHWnd := CreateWindowEx(WS_EX_LAYERED + WS_EX_TOOLWINDOW, WINITEM_CLASS, nil, WS_POPUP + WS_BORDER + WS_THICKFRAME, -100, -100, 1, 1, 0, 0, hInstance, nil);
    if IsWindow(FHWnd) then
    begin
      SetWindowLong(FHWnd, GWL_USERDATA, cardinal(self));
      FWndInstance := MakeObjectInstance(WindowProc);
      FPrevWndProc := Pointer(GetWindowLong(FHWnd, GWL_WNDPROC));
      SetWindowLong(FHWnd, GWL_WNDPROC, LongInt(FWndInstance));

      //if dwm.CompositionEnabled then dwm.ExtendFrameIntoClientArea(FHWnd, rect(-1,-1,-1,-1));
      //SetLayeredWindowAttributes(FHWnd, 0, 255, LWA_ALPHA);
    end
    else err('AeroPeekWindow.Create.CreateWindowEx failed', nil);
  except
    on e: Exception do err('AeroPeekWindow.Create.CreateWindow', e);
  end;
end;
//------------------------------------------------------------------------------
destructor TAeroPeekWindow.Destroy;
begin
  try
    // restore window proc
    if assigned(FPrevWndProc) then SetWindowLong(FHWnd, GWL_WNDPROC, LongInt(FPrevWndProc));
    DestroyWindow(FHWnd);
    if assigned(list) then list.Free;
    if assigned(listThumbnail) then listThumbnail.Free;
    inherited;
  except
    on e: Exception do err('AeroPeekWindow.Destroy', e);
  end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.GetMonitorRect(AMonitor: integer): Windows.TRect;
begin
  result := screen.DesktopRect;
  if AMonitor >= screen.MonitorCount then AMonitor := screen.MonitorCount - 1;
  if AMonitor >= 0 then Result := screen.Monitors[AMonitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Paint;
  procedure AddPathRoundRect(path: pointer; x, y, w, h, radius: integer);
  begin
    GdipStartPathFigure(path);
    GdipAddPathLine(path, x + radius, y, x + w - radius - 1, y);
    GdipAddPathArc(path, x + w - radius * 2 - 1, y, radius * 2, radius * 2, 270, 90);

    GdipAddPathLine(path, x + w - 1, y + radius, x + w - 1, y + h - radius - 1);
    GdipAddPathArc(path, x + w - radius * 2 - 1, y + h - radius * 2 - 1, radius * 2, radius * 2, 0, 90);

    GdipAddPathLine(path, x + w - radius - 1, y + h - 1, x + radius, y + h - 1);
    GdipAddPathArc(path, x, y + h - radius * 2 - 1, radius * 2, radius * 2, 90, 90);

    GdipAddPathLine(path, x, y + h - radius - 1, x, y + radius);
    GdipAddPathArc(path, x, y, radius * 2, radius * 2, 180, 90);

    GdipClosePathFigure(path);
  end;

var
  bmp: _SimpleBitmap;
  hgdip, brush, path: Pointer;
  alpha: cardinal;
  rgn: HRGN;
begin
  // prepare //
  bmp.topleft.x := Fx;
  bmp.topleft.y := Fy;
  bmp.Width := FWidth;
  bmp.Height := FHeight;
  if not gdip_gfx.CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
  hgdip := CreateGraphics(bmp.dc, 0);
  if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
  GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
  GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);

  // draw background //
  if dwm.CompositionEnabled then alpha := $60000000 else alpha := $ff101010;
  GdipCreateSolidFill(alpha, brush);
  GdipCreatePath(FillModeWinding, path);
  AddPathRoundRect(path, Shadow, Shadow, FWidth - Shadow * 2, FHeight - Shadow * 2, Shadow);
  GdipFillPath(hgdip, brush, path);
  GdipDeletePath(path);
  GdipDeleteBrush(brush);

  // update window //
  UpdateLWindow(FHWnd, bmp, 255);
  GdipDeleteGraphics(hgdip);
  gdip_gfx.DeleteBitmap(bmp);

  if dwm.CompositionEnabled then
  begin
    rgn := CreateRoundRectRgn(Shadow, Shadow, FWidth - Shadow, FHeight - Shadow, Shadow * 2, Shadow * 2);
    dwm.EnableBlurBehindWindow(FHWnd, rgn);
    DeleteObject(rgn);
  end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.OpenWindow(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
var
  idx: integer;
  ThumbnailId: THandle;
  rect, wa: windows.TRect;
begin
  result := false;
  if not FActivating then
  try
    try
      FActivating := true;

      KillTimer(FHWnd, ID_TIMER_CLOSE);
      // unregister thumbnails
      for idx := 0 to listThumbnail.Count - 1 do dwm.UnregisterThumbnail(THandle(listThumbnail.Items[idx]));
      listThumbnail.Clear;
      list.Clear;
      if AppList.Count = 0 then
      begin
        CloseWindow;
        exit;
      end;

      //
      list.AddList(AppList);

      // calc size
      FMonitor := AMonitor;
      wa := GetMonitorRect(FMonitor);
      Border := 22;
      Shadow := 10;
      VSplit := 12;
      ThumbW := 200;
      ThumbH := round(ThumbW * (wa.Bottom - wa.Top) / (wa.Right - wa.Left));
      FWTarget := Border * 2 + list.Count * (ThumbW + VSplit) - VSplit;
      FHTarget := Border * 2 + ThumbH;
      if not FActive then
      begin
        FWidth := FWTarget;
        FHeight := FHTarget;
      end;

      // calc position //
      FXTarget := AX - FWTarget div 2;
      FYTarget := AY - FHTarget;
      if FAnimate then
      begin
        if not FActive then
        begin
          Fx := FXTarget;
          Fy := FYTarget + 20;
        end;
      end
      else
      begin
        Fx := FXTarget;
        Fy := FYTarget;
      end;

      // show AeroPeek window
      Paint;
      SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_showwindow);
      //SetWindowPos(FHWnd, $ffffffff, Fx, Fy, FWidth, FHeight, swp_noactivate + swp_showwindow);
      SetActiveWindow(FHWnd);
      if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
      FActive := true;

      // register thumbnails
      for idx := 0 to list.Count - 1 do
      begin
        rect := classes.rect(Border + idx * (ThumbW + VSplit), Border, Border + ThumbW + idx * (ThumbW + VSplit), Border + ThumbH);
        dwm.RegisterThumbnail(FHWnd, THandle(list.Items[idx]), rect, true, ThumbnailId);
        listThumbnail.Add(pointer(ThumbnailId));
      end;
    finally
      FActivating := false;
    end;
  except
    on e: Exception do err('AeroPeekWindow.Message', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.SetWindowPosition(AX, AY: integer; AMonitor: integer);
begin
  if FActive then
  begin
    FXTarget := AX - FWTarget div 2;
    FYTarget := AY - FHTarget;
    Fx := FXTarget;
    Fy := FYTarget;
    gdip_gfx.UpdateLWindowPosAlpha(FHWnd, Fx, Fy, 255);
    //SetWindowPos(FHWnd, $ffffffff, Fx, Fy, 0, 0, swp_nosize + swp_noactivate + swp_showwindow);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.CloseWindow;
var
  idx: integer;
begin
  try
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    // unregister thumbnails
    if listThumbnail.Count > 0 then
      for idx := 0 to listThumbnail.Count - 1 do
        dwm.UnregisterThumbnail(THandle(listThumbnail.Items[idx]));
    listThumbnail.clear;
    list.clear;
    KillTimer(FHWnd, ID_TIMER);
    ShowWindow(FHWnd, 0);
    FActive := False;
    TAeroPeekWindow.Cleanup;
  except
    on e: Exception do err('AeroPeekWindow.CloseI', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Timer;
var
  delta: integer;
  rect: windows.TRect;
begin
  if FActive then
  try
    if (FXTarget <> Fx) or (FYTarget <> Fy) or (FWTarget <> FWidth) or (FHTarget <> FHeight) then
    begin
      delta := abs(FXTarget - Fx) div 4;
      if delta < 1 then delta := 1;
      if Fx > FXTarget then Dec(Fx, delta);
      if Fx < FXTarget then Inc(Fx, delta);

      delta := abs(FYTarget - Fy) div 4;
      if delta < 1 then delta := 1;
      if Fy > FYTarget then Dec(Fy, delta);
      if Fy < FYTarget then Inc(Fy, delta);

      delta := abs(FWTarget - FWidth) div 4;
      if delta < 1 then delta := 1;
      if FWidth > FWTarget then Dec(FWidth, delta);
      if FWidth < FWTarget then Inc(FWidth, delta);

      delta := abs(FHTarget - FHeight) div 4;
      if delta < 1 then delta := 1;
      if FHeight > FHTarget then Dec(FHeight, delta);
      if FHeight < FHTarget then Inc(FHeight, delta);

      Paint;
      //SetWindowPos(FHWnd, $ffffffff, Fx, Fy, FWidth, FHeight, swp_noactivate + swp_showwindow);
    end;
  except
    on e: Exception do err('AeroPeekWindow.Timer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.WindowProc(var msg: TMessage);
var
  idx: integer;
  pt: windows.TPoint;
  rect: windows.TRect;
begin
  msg.Result := 0;

  // WM_LBUTTONUP
  if msg.msg = WM_LBUTTONUP then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for idx := 0 to list.Count - 1 do
    begin
      rect := classes.rect(Border + idx * (ThumbW + VSplit), Border, Border + ThumbW + idx * (ThumbW + VSplit), Border + ThumbH);
      if PtInRect(rect, pt) then ProcessHelper.ActivateWindow(THandle(list.Items[idx]));
    end;
    CloseWindow;
    exit;
  end
  // WM_TIMER
  else if msg.msg = WM_TIMER then
  begin
    if msg.wParam = ID_TIMER then Timer;

    if msg.wParam = ID_TIMER_CLOSE then
    begin
      GetCursorPos(pt);
      if WindowFromPoint(pt) <> FHWnd then CloseWindow;
    end;

    exit;
  end
  // override WM_NCHITTEST to disable window sizing
  else if msg.msg = WM_NCHITTEST then
  begin
    msg.Result := HTCLIENT;
    exit;
  end
  // disable WM_NCPAINT
  else if msg.msg = WM_NCPAINT then
  begin
    exit;
  end;

  msg.Result := DefWindowProc(FHWnd, msg.msg, msg.wParam, msg.lParam);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + #10#13 + e.message);
    messagebox(0, PChar(where + #10#13 + e.message), declu.PROGRAM_NAME, MB_ICONERROR)
  end else begin
    AddLog(where);
    messagebox(0, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.

