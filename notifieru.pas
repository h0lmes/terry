unit notifieru;

interface

uses Windows, Messages, Classes, SysUtils, Forms,
  declu, GDIPAPI, gfx, dwm_unit, loggeru, toolu;

const
  NOTIFIER_FONT_NAME = 'Lucida Console';
  NOTIFIER_FONT_NAME_ALT = 'Courier New';
  NOTIFIER_WCLASS = 'TDockNotifierWClass';

type
  TNotifier = class
  private
    FHWnd: HWND;
    FActivating: boolean;
    FX: integer;
    FY: integer;
    FXTarget: integer;
    FYTarget: integer;
    FW: integer;
    FH: integer;
    FShowTime: QWord;
    FActive: boolean;
    FAlert: boolean;
    FTimeout: QWord;
    FMonitorRect: windows.TRect;
    FText: WideString;
    procedure err(where: string; e: Exception);
  public
    class procedure Cleanup;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterWindowItemClass;
    procedure Message(Text: WideString; monitorRect: windows.TRect; alert: boolean = false; silent: boolean = false);
    procedure MessageNoLog(Text: WideString; monitorRect: windows.TRect; replace: boolean = false);
    procedure Message_Internal(Text: WideString; monitorRect: windows.TRect; animate: boolean = True);
    procedure Close;
    procedure Timer;
    function WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
  end;

var Notifier: TNotifier;

implementation
//------------------------------------------------------------------------------
function NotifierClassWindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  inst: TNotifier;
begin
  inst := TNotifier(GetWindowLongPtr(wnd, GWL_USERDATA));
  if assigned(inst) then
    result := inst.WindowProc(wnd, message, wParam, lParam)
  else
    result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
class procedure TNotifier.Cleanup;
begin
  if assigned(Notifier) then Notifier.Free;
  Notifier := nil;
end;
//------------------------------------------------------------------------------
constructor TNotifier.Create;
begin
  FActive := false;
  FText := '';

  // create window //
  FHWnd := 0;
  try
    RegisterWindowItemClass;
    FHWnd := CreateWindowEx(WS_EX_LAYERED or WS_EX_TOOLWINDOW, NOTIFIER_WCLASS, nil, WS_POPUP, -100, -100, 10, 10, 0, 0, hInstance, nil);
    DWM.ExcludeFromPeek(FHWnd);
    if IsWindow(FHWnd) then SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUInt(self))
    else err('Notifier.Create.CreateWindowEx failed', nil);
  except
    on e: Exception do err('Notifier.Create.CreateWindow', e);
  end;
end;
//------------------------------------------------------------------------------
destructor TNotifier.Destroy;
begin
  DestroyWindow(FHWnd);
end;
//------------------------------------------------------------------------------
procedure TNotifier.RegisterWindowItemClass;
var
  wndClass: windows.TWndClass;
begin
  try
    wndClass.style          := CS_DBLCLKS;
    wndClass.lpfnWndProc    := @NotifierClassWindowProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := NOTIFIER_WCLASS;
    windows.RegisterClass(wndClass);
  except
    on e: Exception do err('Notifier.RegisterWindowClass', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Message(Text: WideString; monitorRect: windows.TRect; alert: boolean = false; silent: boolean = false);
begin
  if alert then AddLog('!!! ' + text) else AddLog(text);
  try
    self.FAlert := self.FAlert or alert;

    if not silent or alert then
    begin
      FTimeout := 8000;
      if length(Text) > 50 then FTimeout := 15000
      else if length(Text) > 30 then FTimeout := 11000;

      Text := Text + LineEnding + LineEnding + '~ ' + declu.PROGRAM_NAME + '#';
      if FText = '' then FText := Text
      else FText := FText + LineEnding + LineEnding + Text;
      Message_Internal(FText, monitorRect, false);
    end;
  except
    on e: Exception do err('Notifier.Message', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.MessageNoLog(Text: WideString; monitorRect: windows.TRect; replace: boolean = false);
begin
  try
    FTimeout := 8000;
    if length(Text) > 50 then FTimeout := 15000
    else if length(Text) > 30 then FTimeout := 11000;

    Text := Text + LineEnding + LineEnding + '~ ' + declu.PROGRAM_NAME + '#';
    if replace or (FText = '') then FText := Text
    else FText := FText + LineEnding + LineEnding + Text;
    Message_Internal(FText, monitorRect, false);
  except
    on e: Exception do err('Notifier.MessageNoLog', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TNotifier.Message_Internal(Text: WideString; monitorRect: windows.TRect; animate: boolean = True);
var
  hgdip, path, hbrush: Pointer;
  font, ffamily: Pointer;
  textRect: TRectF;
  messageMargin, wa: Windows.TRect;
  bmp: _SimpleBitmap;
  alpha: uint;
  acoeff: integer;
begin
  if FActivating then exit;
  FMonitorRect := monitorRect;

  FActivating := True;
  FW := 320;
  messageMargin.left := 16;
  messageMargin.right := 16;
  messageMargin.top := 16;
  messageMargin.bottom := 16;

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
    try
      GdipCreateFontFamilyFromName(PWideChar(WideString(NOTIFIER_FONT_NAME)), nil, ffamily);
    except
      GdipCreateFontFamilyFromName(PWideChar(WideString(NOTIFIER_FONT_NAME_ALT)), nil, ffamily);
    end;
    GdipCreateFont(ffamily, 12, 0, 2, font);
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
    textRect.x := 0;
    textRect.y := 0;
    textRect.Width := FW - messageMargin.left - messageMargin.right;
    textRect.Height := 0;
    GdipMeasureString(hgdip, PWideChar(Text), -1, font, @textRect, nil, @textRect, nil, nil);
    textRect.Height := textRect.Height + 1;

    textRect.x := messageMargin.left;
    textRect.y := messageMargin.top;

    if assigned(hgdip) then GdipDeleteGraphics(hgdip);
    if bmp.dc > 0 then DeleteDC(bmp.dc);

    FH := messageMargin.top + trunc(textRect.Height) + messageMargin.bottom;

    // calc position //
    wa := FMonitorRect;
    FXTarget := wa.right - FW;
    FX := wa.right - FW div 2;
    FYTarget := wa.bottom - FH;
    FY := FYTarget;
    if not animate then
    begin
      FX := FXTarget;
      FY := FYTarget;
    end;
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Measure', e);
      FActivating := False;
      exit;
    end;
  end;

  // prepare to paint //
  try
    bmp.topleft.x := FX;
    bmp.topleft.y := FY;
    bmp.Width := FW;
    bmp.Height := FH;
    if not gfx.CreateBitmap(bmp, FHWnd) then raise Exception.Create('CreateBitmap failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
    GdipSetTextRenderingHint(hgdip, TextRenderingHintClearTypeGridFit);
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
    AddPathRoundRect(path, 0, 0, FW, FH, 0);
    if dwm.IsCompositionEnabled then alpha := $80000000 else alpha := $ff101010;
    // fill
    GdipCreateSolidFill(alpha, hbrush);
    GdipFillPath(hgdip, hbrush, path);
    GdipDeleteBrush(hbrush);
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
    if FAlert then GdipCreateSolidFill($ffff5000, hbrush) else GdipCreateSolidFill($ffffffff, hbrush);
    GdipDrawString(hgdip, PWideChar(Text), -1, font, @textRect, nil, hbrush);
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
      acoeff := 255 - (abs(FX - FXTarget) * 510 div FW);
      if acoeff < 0 then acoeff := 0;
      if acoeff > 255 then acoeff := 255;
    end;
    gfx.UpdateLWindow(FHWnd, bmp, acoeff);
    ShowWindow(FHWnd, SW_SHOWNOACTIVATE);
    SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
    if DWM.IsCompositionEnabled then
      DWM.EnableBlurBehindWindow(FHWnd, 0)
    else
      DWM.DisableBlurBehindWindow(FHWnd);
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
    GdipDeleteFont(font);
    GdipDeleteFontFamily(ffamily);
    GdipDeleteGraphics(hgdip);
    gfx.DeleteBitmap(bmp);
  except
    on e: Exception do
    begin
      err('Notifier.Message_Internal.Cleanup', e);
      FActivating := False;
      exit;
    end;
  end;

  FShowTime := gettickcount64;
  if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
  FActive := True;
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
  if FActive then
  try
    acoeff := 255 - (abs(FX - FXTarget) * 510 div FW);
    if acoeff < 0 then acoeff := 0;
    if acoeff > 255 then acoeff := 255;

    set_pos := (FXTarget <> FX) or (FYTarget <> FY);
    if set_pos then
    begin
      delta := abs(FXTarget - FX) div 6;
      if delta < 1 then delta := 1;
      if FX > FXTarget then Dec(FX, delta);
      if FX < FXTarget then Inc(FX, delta);
      delta := abs(FYTarget - FY) div 6;
      if delta < 1 then delta := 1;
      if FY > FYTarget then Dec(FY, delta);
      if FY < FYTarget then Inc(FY, delta);
      UpdateLWindowPosAlpha(FHWnd, FX, FY, acoeff);
    end;

    if (FX <> FXTarget) or (FY <> FYTarget) then FShowTime := gettickcount64
    else
    if not FAlert then
    begin
      GetCursorPos(pt);
      if WindowFromPoint(pt) <> FHWnd then
        if gettickcount64 - FShowTime > FTimeout then Close;
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
    bmp.topleft.x := -1;
    bmp.topleft.y := -1;
    bmp.Width := 1;
    bmp.Height := 1;
    if gfx.CreateBitmap(bmp, FHWnd) then
    begin
      gfx.UpdateLWindow(FHWnd, bmp, 255);
      gfx.DeleteBitmap(bmp);
    end;

    DWM.DisableBlurBehindWindow(FHWnd);
    KillTimer(FHWnd, ID_TIMER);
    ShowWindow(FHWnd, 0);
    FActive := False;
    FAlert := False;
    FText := '';
  except
    on e: Exception do err('Notifier.Close', e);
  end;
end;
//------------------------------------------------------------------------------
function TNotifier.WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  result := 0;
  if (message = wm_lbuttondown) or (message = wm_rbuttonup) then
  begin
    Close;
    exit;
  end
  else if message = WM_TIMER then
  begin
    Timer;
    exit;
  end;

  result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure TNotifier.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + LineEnding + e.message);
    messagebox(FHWnd, PChar(where + LineEnding + e.message), declu.PROGRAM_NAME, MB_ICONERROR)
  end else begin
    AddLog(where);
    messagebox(FHWnd, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.

