unit aeropeeku;

interface

uses Windows, Messages, Classes, SysUtils, Forms, declu, dwm_unit, GDIPAPI, gdip_gfx, toolu, processhlp;

type
  TAeroPeekWindow = class
  private
    FHWnd: uint;
    WindowClassInstance: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    FActivating: boolean;
    VSplit, Border, ThumbW, ThumbH: integer;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FActive: boolean;
    FMonitor: integer;
    list: TFPList;
    listThumbnail: TFPList;
    procedure err(where: string; e: Exception);
  public
    class function Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    class procedure Close;
    class procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    function OpenI(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
    procedure OpenI2(animate: boolean = false);
    procedure CloseI;
    procedure Timer;
    procedure WindowProc(var msg: TMessage);
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
//------------------------------------------------------------------------------
class function TAeroPeekWindow.Open(AppList: TFPList; AX, AY: integer; AMonitor: integer): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.OpenI(AppList, AX, AY, AMonitor);
end;
//------------------------------------------------------------------------------
class procedure TAeroPeekWindow.Close;
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.CloseI;
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
  FActive := False;
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
      if dwm.CompositingEnabled then dwm.ExtendFrameIntoClientArea(FHWnd, rect(-1,-1,-1,-1));
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
  rect: windows.TRect;
begin
  result := false;
  try
    if FActive then CloseI;

    list.Clear;
    listThumbnail.Clear;
    list.AddList(AppList);

    Border := 10;
    VSplit := 3;
    ThumbH := 150;
    ThumbW := round(ThumbH * 1.6);

    Fx := AX;
    Fy := AY;
    FWidth := Border * 2 + list.Count * (ThumbW + VSplit) - VSplit;
    FHeight := Border * 2 + ThumbH;
    FMonitor := AMonitor;

    OpenI2;

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
procedure TAeroPeekWindow.OpenI2(animate: boolean = false);
var
  hgdip, path, hbrush, hpen: Pointer;
  caption_font, message_font, caption_font_family, message_font_family: Pointer;
  caption_rect, text_rect: TRectF;
  message_margin, wa: Windows.TRect;
  bmp: _SimpleBitmap;
  rgn: HRGN;
  alpha: uint;
  acoeff: integer;
begin
  if FActivating then exit;
  self.FMonitor := FMonitor;

  FActivating := True;

  // measure //
  try
    // calc position //
    wa := GetMonitorRect(FMonitor);
    FXTarget := Fx - FWidth div 2;
    Fx := FXTarget;
    FYTarget := Fy - FHeight;
    Fy := FYTarget + 20;
    if not animate then
    begin
      Fx := FXTarget;
      Fy := FYTarget;
    end;
  except
    on e: Exception do
    begin
      err('AeroPeekWindow.Message_Internal.Measure', e);
      FActivating := False;
      exit;
    end;
  end;

  // background //
  try
    bmp.topleft.x := Fx;
    bmp.topleft.y := Fy;
    bmp.Width := FWidth;
    bmp.Height := FHeight;
    if not gdip_gfx.CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
    //GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
    if dwm.CompositingEnabled then alpha := $80000000 else alpha := $ff101010;
    GdipCreateSolidFill(alpha, hbrush);
    GdipFillRectangle(hgdip, hbrush, 0, 0, FWidth, FHeight);
    GdipDeleteBrush(hbrush);
  except
    on e: Exception do
    begin
      err('AeroPeekWindow.Message_Internal.Backgroud', e);
      FActivating := False;
      exit;
    end;
  end;

  // Open //
  try
    acoeff := 255;
    if animate then
    begin
      acoeff := 255 - (abs(Fx - FXTarget) * 510 div FWidth);
      if acoeff < 0 then acoeff := 0;
      if acoeff > 255 then acoeff := 255;
    end;
    gdip_gfx.UpdateLWindow(FHWnd, bmp, acoeff);
    SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_showwindow);
    //SetWindowPos(FHWnd, $ffffffff, Fx, Fy, FWidth, FHeight, swp_noactivate + swp_showwindow);
    GdipDeleteGraphics(hgdip);
    gdip_gfx.DeleteBitmap(bmp);
  except
    on e: Exception do
    begin
      err('AeroPeekWindow.Message_Internal.Show', e);
      FActivating := False;
      exit;
    end;
  end;

  if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
  FActive := True;
  FActivating := False;
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
      delta := abs(FXTarget - Fx) div 6;
      if delta < 1 then delta := 1;
      if Fx > FXTarget then Dec(Fx, delta);
      if Fx < FXTarget then Inc(Fx, delta);
      delta := abs(FYTarget - Fy) div 6;
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
  bmp: _SimpleBitmap;
  idx: integer;
begin
  try
    // unregister thumbnails
    for idx := 0 to listThumbnail.Count - 1 do dwm.UnregisterThumbnail(THandle(listThumbnail.Items[idx]));
    listThumbnail.clear;
    list.clear;

    {bmp.topleft.x := -1;
    bmp.topleft.y := -1;
    bmp.Width := 1;
    bmp.Height := 1;
    if gdip_gfx.CreateBitmap(bmp) then
    begin
      gdip_gfx.UpdateLWindow(FHWnd, bmp, 255);
      gdip_gfx.DeleteBitmap(bmp);
    end;}

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
  if msg.msg = wm_lbuttonup then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for idx := 0 to list.Count - 1 do
      if PtInRect(classes.rect(Border + idx * (ThumbW + VSplit), Border, Border + (idx + 1) * ThumbW, Border + ThumbH), pt) then
        ProcessHelper.ActivateWindow(THandle(list.Items[idx]));
    CloseI;
    exit;
  end
  else if msg.msg = wm_rbuttonup then
  begin
    CloseI;
    exit;
  end
  else if msg.msg = WM_TIMER then
  begin
    Timer;
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

