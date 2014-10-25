unit aeropeeku;

interface

uses Windows, Messages, Classes, SysUtils, Forms, declu, dwm_unit, GDIPAPI, gdip_gfx, toolu, processhlp;

type

  { TAeroPeekWindow }

  TAeroPeekWindow = class
  private
    FHWnd: uint;
    WindowClassInstance: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    VSplit, Border, ThumbW, ThumbH: integer;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FActivating: boolean;
    FActive: boolean;
    FMonitor: integer;
    FAnimate: boolean;
    list: TFPList;
    listThumbnail: TFPList;
    procedure err(where: string; e: Exception);
  public
    property Handle: uint read FHWnd;
    property Active: boolean read FActive;

    class function Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    class function IsActive: boolean;
    class procedure Close(Timeout: cardinal = 0);
    class procedure SetPosition(AX, AY: integer; AMonitor: integer);
    class procedure Cleanup;

    constructor Create;
    destructor Destroy; override;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    function OpenI(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    procedure SetPositionI(AX, AY: integer; AMonitor: integer);
    procedure OpenI2;
    procedure CloseI;
    procedure Timer;
    procedure WindowProc(var msg: TMessage);
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
//------------------------------------------------------------------------------
// open (show) AeroPeekWindow
class function TAeroPeekWindow.Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.OpenI(AppList, AX, AY, AMonitor);
end;
//------------------------------------------------------------------------------
// check if AeroPeekWindow is visible
class function TAeroPeekWindow.IsActive: boolean;
begin
  result := false;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.Active;
end;
//------------------------------------------------------------------------------
// close AeroPeekWindow
// if Timeout set then close after timeout has elapsed
class procedure TAeroPeekWindow.Close(Timeout: cardinal = 0);
begin
  if assigned(AeroPeekWindow) then
  begin
    if Timeout = 0 then AeroPeekWindow.CloseI
    else SetTimer(AeroPeekWindow.Handle, ID_TIMER_CLOSE, Timeout, nil);
  end;
end;
//------------------------------------------------------------------------------
class procedure TAeroPeekWindow.SetPosition(AX, AY: integer; AMonitor: integer);
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.SetPositionI(AX, AY, AMonitor);
end;
//------------------------------------------------------------------------------
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
    FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow, WINITEM_CLASS, nil, WS_POPUP, -100, -100, 50, 50, 0, 0, hInstance, nil);
    if IsWindow(FHWnd) then
    begin
      SetWindowLong(FHWnd, GWL_USERDATA, cardinal(self));
      FWndInstance := MakeObjectInstance(WindowProc);
      FPrevWndProc := Pointer(GetWindowLong(FHWnd, GWL_WNDPROC));
      SetWindowLong(FHWnd, GWL_WNDPROC, LongInt(FWndInstance));
      //if dwm.CompositingEnabled then dwm.ExtendFrameIntoClientArea(FHWnd, rect(-1,-1,-1,-1));
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
function TAeroPeekWindow.OpenI(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
var
  idx: integer;
  ThumbnailId: THandle;
  rect, wa: windows.TRect;
begin
  result := false;
  try
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    // unregister thumbnails
    for idx := 0 to listThumbnail.Count - 1 do dwm.UnregisterThumbnail(THandle(listThumbnail.Items[idx]));
    listThumbnail.Clear;
    list.Clear;
    list.AddList(AppList);
    if list.Count = 0 then
    begin
      CloseI;
      exit;
    end;

    // calc size
    FMonitor := AMonitor;
    wa := GetMonitorRect(FMonitor);
    Border := 16;
    VSplit := 12;
    ThumbH := 120;
    ThumbW := round(ThumbH * (wa.Right - wa.Left) / (wa.Bottom - wa.Top));
    FWidth := Border * 2 + list.Count * (ThumbW + VSplit) - VSplit;
    FHeight := Border * 2 + ThumbH;

    // calc position //
    FXTarget := AX - FWidth div 2;
    FYTarget := AY - FHeight;
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
    OpenI2;

    // register thumbnails
    for idx := 0 to list.Count - 1 do
    begin
      rect := classes.rect(Border + idx * (ThumbW + VSplit), Border, Border + (idx + 1) * ThumbW, Border + ThumbH);
      dwm.RegisterThumbnail(FHWnd, THandle(list.Items[idx]), rect, ThumbnailId);
      listThumbnail.Add(pointer(ThumbnailId));
    end;
  except
    on e: Exception do err('AeroPeekWindow.Message', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.SetPositionI(AX, AY: integer; AMonitor: integer);
begin
  if FActive then
  begin
    FXTarget := AX - FWidth div 2;
    FYTarget := AY - FHeight;
    Fx := FXTarget;
    Fy := FYTarget;
    gdip_gfx.UpdateLWindowPosAlpha(FHWnd, Fx, Fy, 255);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.OpenI2;
var
  hgdip, brush, font, family: Pointer;
  caption_rect: TRectF;
  bmp: _SimpleBitmap;
  alpha: uint;
  acoeff: integer;
begin
  if not FActivating then
  try
    FActivating := true;

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
    if dwm.CompositingEnabled then alpha := $80000000 else alpha := $ff101010;
    GdipCreateSolidFill(alpha, brush);
    GdipFillRectangle(hgdip, brush, 0, 0, FWidth, FHeight);
    GdipDeleteBrush(brush);

    // update window //
    acoeff := 255;
    if FAnimate then
    begin
      acoeff := 255 - (abs(Fx - FXTarget) * 510 div FWidth);
      if acoeff < 0 then acoeff := 0;
      if acoeff > 255 then acoeff := 255;
    end;
    gdip_gfx.UpdateLWindow(FHWnd, bmp, acoeff);
    SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_showwindow);
    GdipDeleteGraphics(hgdip);
    gdip_gfx.DeleteBitmap(bmp);

    if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
    FActive := true;
  finally
    FActivating := false;
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Timer;
var
  delta: integer;
  set_pos: boolean;
  acoeff: integer;
  pt: windows.TPoint;
begin
  if FActive then
  try
    acoeff := 255 - (abs(Fx - FXTarget) * 510 div FWidth);
    if acoeff < 0 then acoeff := 0;
    if acoeff > 255 then acoeff := 255;

    set_pos := (FXTarget <> Fx) or (FYTarget <> Fy);
    if set_pos then
    begin
      delta := abs(FXTarget - Fx) div 4;
      if delta < 1 then delta := 1;
      if Fx > FXTarget then Dec(Fx, delta);
      if Fx < FXTarget then Inc(Fx, delta);
      delta := abs(FYTarget - Fy) div 4;
      if delta < 1 then delta := 1;
      if Fy > FYTarget then Dec(Fy, delta);
      if Fy < FYTarget then Inc(Fy, delta);
      UpdateLWindowPosAlpha(FHWnd, Fx, Fy, acoeff);
    end;
  except
    on e: Exception do err('AeroPeekWindow.Timer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.CloseI;
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
  except
    on e: Exception do err('AeroPeekWindow.CloseI', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.WindowProc(var msg: TMessage);
var
  idx: integer;
  pt: windows.TPoint;
begin
  msg.Result := 0;

  // WM_LBUTTONUP
  if msg.msg = WM_LBUTTONUP then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for idx := 0 to list.Count - 1 do
      if PtInRect(classes.rect(Border + idx * (ThumbW + VSplit), Border, Border + (idx + 1) * ThumbW, Border + ThumbH), pt) then
        ProcessHelper.ActivateWindow(THandle(list.Items[idx]));
    CloseI;
    exit;
  end
  // WM_RBUTTONUP
  else if msg.msg = WM_RBUTTONUP then
  begin
    CloseI;
    exit;
  end
  // WM_TIMER
  else if msg.msg = WM_TIMER then
  begin
    if msg.wParam = ID_TIMER then Timer;

    if msg.wParam = ID_TIMER_CLOSE then
    begin
      GetCursorPos(pt);
      if WindowFromPoint(pt) <> FHWnd then CloseI;
    end;

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
    messagebox(FHWnd, PChar(where + #10#13 + e.message), declu.PROGRAM_NAME, MB_ICONERROR)
  end else begin
    AddLog(where);
    messagebox(FHWnd, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.

