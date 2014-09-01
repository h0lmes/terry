unit hintu;

interface

uses Windows, Messages, SysUtils, Forms, Math,
  declu, toolu, GDIPAPI, gdip_gfx, setsu, dockh;

type
  _Hint = class
  private
    hWnd: THandle;
    wndClass: TWndClass;
    WindowClassInstance: uint;
    FActivating: boolean;
    awidth: integer;
    aheight: integer;
    Visible: boolean;
    Caption: WideString;
    font: _FontData;
    wnd_owner: uint;
    alpha: integer;
    ax: integer; // real x
    ay: integer; // real y
    aw: integer; // real width
    need_x: integer;
    need_y: integer;
    need_w: integer;
    need_h: integer;
    procedure err(where: string; e: Exception);
  public
    constructor Create;
    destructor Destroy; override;
    function GetMonitorRect(monitor: integer): Windows.TRect;
    procedure ActivateHint(hwndOwner: uint; caption_: string; x, y, direction, monitor: integer);
    procedure DeactivateHint(hwnd_: uint);
    procedure Timer;
    procedure UnregisterWindowClass;
  end;

implementation
//------------------------------------------------------------------------------
function WndProc(wnd: hwnd; message: uint; wParam: integer; lParam: integer): integer; stdcall;
var
  p: _Hint;
begin
  if message = WM_NCHITTEST then
  begin
    result := HTTRANSPARENT;
    exit;
  end
  else if (message = WM_TIMER) and (wParam = ID_TIMER) then
  begin
    p := _Hint(GetWindowLong(wnd, GWL_USERDATA));
    if p is _Hint then p.Timer;
    exit;
  end;

  Result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure _Hint.err(where: string; e: Exception);
begin
  if assigned(e) then dockh.notify(0, pchar(where + #10#13 + e.message))
  else dockh.notify(0, pchar(where));
end;
//------------------------------------------------------------------------------
constructor _Hint.Create;
begin
  inherited;

  StrCopy(@font.Name[0], PChar(GetFont));
  font.size := GetfontSize;
  font.color := $ffffffff;
  alpha := 0;
  Visible := False;

  try
    wndClass.style          := 0;
    wndClass.lpfnWndProc    := @WndProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, IDC_ARROW);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := 'Terry::Hint';
    WindowClassInstance := Windows.RegisterClass(wndClass);
    if WindowClassInstance < 33 then err('Can not register hint window class', nil);
  except
    on e: Exception do
    begin
      err('Hint.Create.RegisterClass', e);
      exit;
    end;
  end;

  try
    hWnd := CreateWindowEx(ws_ex_layered or ws_ex_toolwindow, 'Terry::Hint', '', ws_popup, 0, 0, 0, 0, 0, 0, hInstance, nil);
    if IsWindow(hWnd) then SetWindowLong(hWnd, GWL_USERDATA, cardinal(self))
    else err('Hint.Create.CreateWindowEx failed', nil);
  except
    on e: Exception do err('Hint.Create.CreateWindowEx', e);
  end;
end;
//------------------------------------------------------------------------------
function _Hint.GetMonitorRect(monitor: integer): Windows.TRect;
begin
  result.Left := 0;
  result.Top := 0;
  result.Right := screen.Width;
  result.Bottom := screen.Height;
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
procedure _Hint.ActivateHint(hwndOwner: uint; caption_: string; x, y, direction, monitor: integer);
var
  hgdip, hfont, hfontfamily, hbrush, path: Pointer;
  rect: TRectF;
  wa: Windows.TRect;
  bmp: _SimpleBitmap;
  mi: MONITORINFO;
begin
  if IsWindow(hWnd) then
  try
    FActivating := True;
    try
      wnd_owner := hwndOwner;
      Caption := caption_;

      CopyFontData(sets.container.Font, font);

      bmp.dc := CreateCompatibleDC(0);
      if bmp.dc = 0 then
      begin
        err('Hint.ActivateHint. Device context is null', nil);
        exit;
      end;
      GdipCreateFromHDC(bmp.dc, hgdip);
      try
        GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@font.Name))), nil, hfontfamily);
      except
        on e: Exception do
        begin
          err('Hint.ActivateHint.CreateFontFamily', e);
          exit;
        end;
      end;
      GdipCreateFont(hfontfamily, font.size, integer(font.bold) + integer(font.italic) * 2, 2, hfont);
      rect.x := 0;
      rect.y := 0;
      rect.Width := 0;
      rect.Height := 0;
      try GdipMeasureString(hgdip, PWideChar(WideString(Caption)), -1, hfont, @rect, nil, @rect, nil, nil);
      except
        on e: Exception do
        begin
          err('Hint.ActivateHint.MeasureString', e);
          exit;
        end;
      end;
      GdipDeleteGraphics(hgdip);
      DeleteDC(bmp.dc);

      aheight := round(rect.Height) + 2;
      awidth := round(rect.Width) + aheight - 4;
      awidth := max(awidth, aheight * 3 div 2);

      if direction = 0 then dec(y, aheight div 2);
      if direction = 1 then dec(x, awidth div 2);
      if direction = 2 then
      begin
        dec(y, aheight div 2);
        dec(x, awidth);
      end;
      if direction = 3 then
      begin
        dec(y, aheight);
        dec(x, awidth div 2);
      end;

      mi.cbSize := sizeof(MONITORINFO);
      GetMonitorInfoA(monitor, @mi);
      wa := mi.rcWork;
      if x + awidth > wa.right then x := wa.right - awidth;
      if y + aheight > wa.Bottom then y := wa.Bottom - aheight;
      if x < wa.left then x := wa.left;
      if y < wa.top then y := wa.top;

      need_x := x;
      need_y := y;
      if not sets.container.HintEffects then alpha := 255;
      if not Visible and (alpha = 0) or not sets.container.HintEffects then
      begin
        ax := x;
        ay := y;
      end;
    except
      on e: Exception do
      begin
        err('Hint.ActivateHint.Precalc', e);
        FActivating := False;
        exit;
      end;
    end;

    // background //

    try
      bmp.topleft.x := ax;
      bmp.topleft.y := ay;
      bmp.Width := awidth;
      bmp.Height := aheight;
      gdip_gfx.CreateBitmap(bmp);
      GdipCreateFromHDC(bmp.dc, hgdip);
      GdipGraphicsClear(hgdip, 0);
      GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
      GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);

      GdipCreatePath(FillModeWinding, path);
      GdipAddPathRectangle(path, aheight div 2, 0, awidth - aheight - 1, aheight);
      GdipAddPathEllipse(path, 0, 0, aheight - 1, aheight - 1);
      GdipAddPathEllipse(path, awidth - aheight - 1, 0, aheight - 1, aheight - 1);
      GdipCreateSolidFill($90000000 + font.color_outline and $ffffff, hbrush);
      GdipFillPath(hgdip, hbrush, path);
      GdipDeleteBrush(hbrush);
      GdipDeletePath(path);
    except
      on e: Exception do
      begin
        err('Hint.ActivateHint.backgroud', e);
        FActivating := False;
        exit;
      end;
    end;

    // text //

    try
      OffsetRectF(rect, font.size div 3 - 1, 1);
      OffsetRectF(rect, aheight div 4, 0);

      GdipCreateSolidFill(font.color, hbrush);
      GdipDrawString(hgdip, PWideChar(WideString(Caption)), -1, hfont, @rect, nil, hbrush);

      UpdateLWindow(hWnd, bmp, alpha);
      SetWindowPos(hWnd, hwnd_topmost, 0, 0, 0, 0, swp_noactivate + swp_nomove + swp_nosize + swp_showwindow);

      if not Visible and sets.container.HintEffects then SetTimer(hWnd, ID_TIMER, 10, nil);
      Visible := True;

      GdipDeleteBrush(hbrush);
      GdipDeleteFont(hfont);
      GdipDeleteFontFamily(hfontfamily);
      GdipDeleteGraphics(hgdip);
      DeleteBitmap(bmp);
    except
      on e: Exception do
      begin
        err('Hint.ActivateHint.Fin', e);
        FActivating := False;
        exit;
      end;
    end;
  finally
    FActivating := False;
  end;
end;
//------------------------------------------------------------------------------
procedure _Hint.Timer;
const
  STEP = 1;
  ITER = 4;
  ASTEP = 5;
  AITER = 3;
var
  delta: integer;
begin
  try
    if (not Visible and (alpha > 0)) or (Visible and (alpha < 255)) or (need_x <> ax) or (need_y <> ay) then
    begin
      delta := abs(255 - alpha) div AITER;
      if Visible then alpha := alpha + ifthen(delta < ASTEP, ASTEP, delta)
      else alpha := alpha - ifthen(delta < ASTEP, ASTEP, delta);
      if alpha < 0 then alpha := 0;
      if alpha > 255 then alpha := 255;

      delta := abs(need_x - ax) div ITER;
      if abs(need_x - ax) < STEP then ax := need_x
      else if need_x > ax then ax := ax + ifthen(delta < STEP, STEP, delta)
      else if need_x < ax then ax := ax - ifthen(delta < STEP, STEP, delta);

      delta := abs(need_y - ay) div ITER;
      if abs(need_y - ay) < STEP then ay := need_y
      else if need_y > ay then ay := ay + ifthen(delta < STEP, STEP, delta)
      else if need_y < ay then ay := ay - ifthen(delta < STEP, STEP, delta);

      UpdateLWindowPosAlpha(hWnd, ax, ay, alpha);

      if not Visible and (alpha = 0) then
      begin
        KillTimer(hWnd, ID_TIMER);
        ShowWindow(hWnd, 0);
      end;
    end;
  except
    on e: Exception do err('Hint.Timer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _Hint.DeactivateHint(hwnd_: uint);
begin
  if hwnd_ = wnd_owner then
  try
    Visible := false;
    if not sets.container.HintEffects then
    begin
      alpha := 0;
      KillTimer(hWnd, ID_TIMER);
      ShowWindow(hWnd, 0);
    end;
  except
    on e: Exception do err('Hint.DeactivateHint', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _Hint.UnregisterWindowClass;
begin
  try Windows.UnregisterClass('Terry::Hint', WindowClassInstance);
  except
    on e: Exception do err('Hint.UnregisterWindowClass', e);
  end;
end;
//------------------------------------------------------------------------------
destructor _Hint.Destroy;
begin
  DestroyWindow(hWnd);
  UnregisterWindowClass;
  inherited;
end;
//------------------------------------------------------------------------------
end.

