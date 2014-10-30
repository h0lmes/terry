unit notifieru;

interface

uses Windows, Messages, Classes, SysUtils, Forms, declu, toolu, GDIPAPI, gdip_gfx, dwm_unit;

type
  TNotifier = class
  private
    hWnd: uint;
    WindowClassInstance: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    FActivating: boolean;
    x: integer;
    y: integer;
    need_x: integer;
    need_y: integer;
    awidth: integer;
    aheight: integer;
    showtime: uint;
    active: boolean;
    alert: boolean;
    timeout: cardinal;
    monitor: integer;
    current_text: string;
    procedure err(where: string; e: Exception);
  public
    texts: TStrings;
    class procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
    function GetMonitorRect(monitor: integer): Windows.TRect;
    procedure Message(Text: string; monitor: integer = 0; alert: boolean = false; silent: boolean = false);
    procedure MessageNoLog(Text: string; monitor: integer = 0; replace: boolean = false);
    procedure Message_Internal(Caption, Text: string; monitor: integer; animate: boolean = True);
    procedure Close;
    procedure Timer;
    procedure WindowProc(var msg: TMessage);
  end;

var Notifier: TNotifier;

implementation
//------------------------------------------------------------------------------
class procedure TNotifier.Cleanup;
begin
  if assigned(Notifier) then Notifier.Free;
  Notifier := nil;
end;
//------------------------------------------------------------------------------
constructor TNotifier.Create;
begin
  inherited;
  active := False;
  texts := TStringList.Create;
  current_text := '';

  // create window //
  hWnd := 0;
  try
    hWnd := CreateWindowEx(ws_ex_layered or ws_ex_toolwindow, WINITEM_CLASS, nil, ws_popup, 0, 0, 0, 0, 0, 0, hInstance, nil);
    if IsWindow(hWnd) then
    begin
      SetWindowLong(hWnd, GWL_USERDATA, cardinal(self));
      FWndInstance := MakeObjectInstance(WindowProc);
      FPrevWndProc := Pointer(GetWindowLong(hWnd, GWL_WNDPROC));
      SetWindowLong(hWnd, GWL_WNDPROC, LongInt(FWndInstance));
    end
    else err('Notifier.Create.CreateWindowEx failed', nil);
  except
    on e: Exception do err('Notifier.Create.CreateWindow', e);
  end;
end;
//------------------------------------------------------------------------------
destructor TNotifier.Destroy;
begin
  try
    // restore window proc
    if assigned(FPrevWndProc) then SetWindowLong(hWnd, GWL_WNDPROC, LongInt(FPrevWndProc));
    DestroyWindow(hWnd);
    if assigned(texts) then texts.Free;
    inherited;
  except
    on e: Exception do err('Notifier.Destroy', e);
  end;
end;
//------------------------------------------------------------------------------
function TNotifier.GetMonitorRect(monitor: integer): Windows.TRect;
begin
  result := screen.DesktopRect;
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Message(Text: string; monitor: integer = 0; alert: boolean = false; silent: boolean = false);
begin
  if alert then AddLog('!' + text) else AddLog(text);
  try
    texts.add('[' + formatdatetime('dd/MM/yyyy hh:nn:ss', now) + ']  ' + Text);
    self.alert := self.alert or alert;

    if not silent or alert then
    begin
      timeout := 8000;
      if length(Text) > 50 then timeout := 15000
      else if length(Text) > 30 then timeout := 11000;
      if current_text = '' then current_text := Text
      else current_text := current_text + #13#10#13#10 + Text;
      Message_Internal(declu.PROGRAM_NAME, current_text, monitor, false);
    end;
  except
    on e: Exception do err('Notifier.Message', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.MessageNoLog(Text: string; monitor: integer = 0; replace: boolean = false);
begin
  try
    timeout := 8000;
    if length(Text) > 50 then timeout := 15000
    else if length(Text) > 30 then timeout := 11000;
    if replace or (current_text = '') then current_text := Text
    else current_text := current_text + #13#10#13#10 + Text;
    Message_Internal(declu.PROGRAM_NAME, current_text, monitor, false);
  except
    on e: Exception do err('Notifier.MessageNoLog', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Message_Internal(Caption, Text: string; monitor: integer; animate: boolean = True);
var
  hgdip, path, hbrush, hpen: Pointer;
  caption_font, message_font, caption_font_family, message_font_family: Pointer;
  caption_rect, text_rect: TRectF;
  message_margin, wa: Windows.TRect;
  bmp: _SimpleBitmap;
  h_split, radius: integer;
  rgn: HRGN;
  alpha: uint;
  acoeff: integer;
begin
  if FActivating then exit;
  self.monitor := monitor;

  FActivating := True;
  radius := 3;
  h_split := 3;
  awidth := 240;
  message_margin.left := radius * 2 div 3 + 3;
  message_margin.right := radius * 2 div 3 + 3;
  message_margin.top := radius * 2 div 3 + 3;
  message_margin.bottom := radius * 2 div 3 + 3;

  // context //
  try
    bmp.dc := CreateCompatibleDC(0);
    if bmp.dc = 0 then raise Exception.Create('CreateCompatibleDC failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Context', e);
      FActivating := False;
      exit;
    end;
  end;

  // context //
  try
    GdipCreateFontFamilyFromName(PWideChar(WideString(GetFont)), nil, caption_font_family);
    GdipCreateFontFamilyFromName(PWideChar(WideString(GetContentFont)), nil, message_font_family);
    GdipCreateFont(caption_font_family, 16, 1, 2, caption_font);
    GdipCreateFont(message_font_family, 14, 0, 2, message_font);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Fonts', e);
      FActivating := False;
      exit;
    end;
  end;

  // measure //
  try
    caption_rect.x := 0;
    caption_rect.y := 0;
    caption_rect.Width := awidth - message_margin.left - message_margin.right;
    caption_rect.Height := 0;
    GdipMeasureString(hgdip, PWideChar(WideString(Caption)), -1, caption_font, @caption_rect, nil, @caption_rect, nil, nil);
    caption_rect.Height := caption_rect.Height + 1;

    text_rect.x := 0;
    text_rect.y := 0;
    text_rect.Width := awidth - message_margin.left - message_margin.right;
    text_rect.Height := 0;
    GdipMeasureString(hgdip, PWideChar(WideString(Text)), -1, message_font, @text_rect, nil, @text_rect, nil, nil);
    text_rect.Height := text_rect.Height + 1;

    caption_rect.x := message_margin.left;
    caption_rect.y := message_margin.top;

    text_rect.x := message_margin.left;
    text_rect.y := caption_rect.y + caption_rect.Height + h_split;

    if assigned(hgdip) then GdipDeleteGraphics(hgdip);
    if bmp.dc > 0 then DeleteDC(bmp.dc);

    aheight := message_margin.top + trunc(caption_rect.Height) + h_split +
      trunc(text_rect.Height) + message_margin.bottom;

    // calc position //
    wa := GetMonitorRect(monitor);
    need_x := wa.right - awidth - 2;
    x := wa.right - awidth div 2 - 2;
    need_y := wa.bottom - aheight - 2;
    y := need_y;
    if not animate then
    begin
      x := need_x;
      y := need_y;
    end;
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Measure', e);
      FActivating := False;
      exit;
    end;
  end;

  // prepare drawing //
  try
    bmp.topleft.x := x;
    bmp.topleft.y := y;
    bmp.Width := awidth;
    bmp.Height := aheight;
    if not gdip_gfx.CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
    GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Prepare', e);
      FActivating := False;
      exit;
    end;
  end;

  // background //
  try
    GdipCreatePath(FillModeAlternate, path);
    AddPathRoundRect(path, 0, 0, awidth, aheight, radius);
    if dwm.CompositionEnabled then alpha := $80000000 else alpha := $ff101010;
    // fill
    GdipCreateSolidFill(alpha, hbrush);
    GdipFillPath(hgdip, hbrush, path);
    GdipDeleteBrush(hbrush);
    // outline
    GdipCreatePen1($60ffffff, 1, UnitPixel, hpen);
    GdipDrawPath(hgdip, hpen, path);
    GdipDeletePen(hpen);
    // cleanup
    GdipDeletePath(path);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Backgroud', e);
      FActivating := False;
      exit;
    end;
  end;

  // message caption and text //
  try
    if alert then GdipCreateSolidFill($ffff5000, hbrush) else GdipCreateSolidFill($ffffffff, hbrush);
    GdipDrawString(hgdip, PWideChar(WideString(Caption)), -1, caption_font, @caption_rect, nil, hbrush);
    GdipDrawString(hgdip, PWideChar(WideString(Text)), -1, message_font, @text_rect, nil, hbrush);
    GdipDeleteBrush(hbrush);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.MessageCaptionAndText', e);
      FActivating := False;
      exit;
    end;
  end;

  // show //
  try
    acoeff := 255;
    if animate then
    begin
      acoeff := 255 - (abs(x - need_x) * 510 div awidth);
      if acoeff < 0 then acoeff := 0;
      if acoeff > 255 then acoeff := 255;
    end;
    gdip_gfx.UpdateLWindow(hWnd, bmp, acoeff);
    SetWindowPos(hWnd, $ffffffff, 0, 0, 0, 0, swp_noactivate + swp_nomove + swp_nosize + swp_showwindow);
    if dwm.CompositionEnabled then
    begin
      rgn := CreateRoundRectRgn(0, 0, awidth, aheight, radius * 2, radius * 2);
      DWM.EnableBlurBehindWindow(hWnd, rgn);
      DeleteObject(rgn);
    end
    else
      DWM.DisableBlurBehindWindow(hWnd);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Show', e);
      FActivating := False;
      exit;
    end;
  end;

  // cleanup //
  try
    GdipDeleteFont(caption_font);
    GdipDeleteFont(message_font);
    GdipDeleteFontFamily(caption_font_family);
    GdipDeleteFontFamily(message_font_family);
    GdipDeleteGraphics(hgdip);
    gdip_gfx.DeleteBitmap(bmp);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Cleanup', e);
      FActivating := False;
      exit;
    end;
  end;

  showtime := gettickcount;
  if not active then SetTimer(hWnd, ID_TIMER, 10, nil);
  active := True;
  FActivating := False;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Timer;
var
  delta: integer;
  set_pos: boolean;
  acoeff: integer;
  pt: windows.TPoint;
begin
  if active then
  try
    acoeff := 255 - (abs(x - need_x) * 510 div awidth);
    if acoeff < 0 then acoeff := 0;
    if acoeff > 255 then acoeff := 255;

    set_pos := (need_x <> x) or (need_y <> y);
    if set_pos then
    begin
      delta := abs(need_x - x) div 6;
      if delta < 1 then delta := 1;
      if x > need_x then Dec(x, delta);
      if x < need_x then Inc(x, delta);
      delta := abs(need_y - y) div 6;
      if delta < 1 then delta := 1;
      if y > need_y then Dec(y, delta);
      if y < need_y then Inc(y, delta);
      UpdateLWindowPosAlpha(hWnd, x, y, acoeff);
    end;

    if (x <> need_x) or (y <> need_y) then showtime := gettickcount
    else
    if not alert then
    begin
      GetCursorPos(pt);
      if WindowFromPoint(pt) <> hWnd then
        if gettickcount - showtime > timeout then Close;
    end;
  except
    on e: Exception do err('Notifier.Timer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Close;
var
  bmp: _SimpleBitmap;
begin
  try
    //Message_Internal('', '', self.monitor, false);
    bmp.topleft.x := -1;
    bmp.topleft.y := -1;
    bmp.Width := 1;
    bmp.Height := 1;
    if gdip_gfx.CreateBitmap(bmp) then
    begin
      gdip_gfx.UpdateLWindow(hWnd, bmp, 255);
      gdip_gfx.DeleteBitmap(bmp);
    end;

    DWM.DisableBlurBehindWindow(hWnd);
    KillTimer(hWnd, ID_TIMER);
    ShowWindow(hWnd, 0);
    active := False;
    alert := False;
    current_text := '';
  except
    on e: Exception do err('Notifier.Close', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.WindowProc(var msg: TMessage);
begin
  msg.Result := 0;
  if (msg.msg = wm_lbuttondown) or (msg.msg = wm_rbuttonup) then
  begin
    Close;
    exit;
  end
  else if msg.msg = WM_TIMER then
  begin
    Timer;
    exit;
  end;

  msg.Result := DefWindowProc(hWnd, msg.msg, msg.wParam, msg.lParam);
end;
//------------------------------------------------------------------------------
procedure TNotifier.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + #10#13 + e.message);
    messagebox(hWnd, PChar(where + #10#13 + e.message), declu.PROGRAM_NAME, MB_ICONERROR)
  end else begin
    AddLog(where);
    messagebox(hWnd, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.

