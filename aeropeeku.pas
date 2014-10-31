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
    FBorder, FShadow, ThumbW, ThumbH, ItemSplit: integer;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FWTarget: integer;
    FHTarget: integer;
    FTitleHeight: integer;
    FCloseButtonSize: integer;
    FCloseButtonDownIndex: integer;
    FActivating: boolean;
    FActive: boolean;
    FMonitor: integer;
    FSite: integer;
    FAnimate: boolean;
    FTopWindowIndex: integer;
    list: TFPList;
    listThumbnail: TFPList;
    FColor1, FColor2: cardinal;
    FCompositionEnabled: boolean;
    FFontFamily: string;
    FFontSize: integer;
    procedure DrawCloseButton(hgdip: pointer; rect: GDIPAPI.TRect; Pressed: boolean);
    function GetCloseButtonRect(index: integer): windows.TRect;
    function GetItemRect(index: integer): windows.TRect;
    function GetItemSelectionRect(index: integer): windows.TRect;
    function GetThumbRect(index: integer): windows.TRect;
    function GetTitleRect(index: integer): windows.TRect;
    procedure Paint;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    procedure RegisterThumbnails;
    procedure Timer;
    procedure UnRegisterThumbnails;
    procedure WindowProc(var msg: TMessage);
    procedure err(where: string; e: Exception);
  public
    property Handle: uint read FHWnd;
    property Active: boolean read FActive;

    class function Open(AppList: TFPList; AX, AY: integer; AMonitor: integer; Site: integer): boolean;
    class procedure SetPosition(AX, AY: integer; AMonitor: integer);
    class procedure Close(Timeout: cardinal = 0);
    class function IsActive: boolean;
    class procedure Cleanup;

    constructor Create;
    destructor Destroy; override;
    function OpenWindow(AppList: TFPList; AX, AY: integer; AMonitor: integer; Site: integer): boolean;
    procedure SetWindowPosition(AX, AY: integer; AMonitor: integer);
    procedure CloseWindow;
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
uses frmmainu;
//------------------------------------------------------------------------------
// open (show) AeroPeekWindow
class function TAeroPeekWindow.Open(AppList: TFPList; AX, AY: integer; AMonitor: integer; Site: integer): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.OpenWindow(AppList, AX, AY, AMonitor, Site);
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
  FFontFamily := toolu.GetFont;
  FFontSize := toolu.GetFontSize * 3 div 2;
  FCloseButtonDownIndex := -1;
  FTopWindowIndex := -1;
  list := TFPList.Create;
  listThumbnail := TFPList.Create;

  // create window //
  FHWnd := 0;
  try
    FHWnd := CreateWindowEx(WS_EX_LAYERED + WS_EX_TOOLWINDOW, WINITEM_CLASS, nil, WS_POPUP, -100, -100, 1, 1, 0, 0, hInstance, nil);
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
function TAeroPeekWindow.GetItemRect(index: integer): windows.TRect;
begin
  result.Left := FBorder + index * (ThumbW + ItemSplit);
  result.Top := FBorder;
  result.Right := result.Left + ThumbW;
  result.Bottom := result.Top + FTitleHeight + ThumbH;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.GetItemSelectionRect(index: integer): windows.TRect;
begin
  result := GetItemRect(index);
  result.Left -= 5;
  result.Top -= 5;
  result.Right += 5;
  result.Bottom += 5;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.GetThumbRect(index: integer): windows.TRect;
begin
  result.Left := FBorder + index * (ThumbW + ItemSplit);
  result.Top := FBorder + FTitleHeight;
  result.Right := result.Left + ThumbW;
  result.Bottom := result.Top + ThumbH;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.GetTitleRect(index: integer): windows.TRect;
begin
  result.Left := FBorder + index * (ThumbW + ItemSplit);
  result.Top := FBorder;
  result.Right := result.Left + ThumbW - FCloseButtonSize - 5;
  result.Bottom := result.Top + FTitleHeight;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.GetCloseButtonRect(index: integer): windows.TRect;
begin
  result := GetItemRect(index);
  result.Left := result.Right - FCloseButtonSize;
  result.Bottom := result.Top + FCloseButtonSize;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.RegisterThumbnails;
var
  idx: integer;
  rect: windows.TRect;
  ThumbnailId: THandle;
begin
  if FCompositionEnabled then
    for idx := 0 to list.Count - 1 do
    begin
      rect := GetThumbRect(idx);
      dwm.RegisterThumbnail(FHWnd, THandle(list.Items[idx]), rect, true, ThumbnailId);
      listThumbnail.Add(pointer(ThumbnailId));
    end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.UnRegisterThumbnails;
var
  idx: integer;
begin
  if listThumbnail.Count > 0 then
    for idx := 0 to listThumbnail.Count - 1 do dwm.UnregisterThumbnail(THandle(listThumbnail.Items[idx]));
  listThumbnail.Clear;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.DrawCloseButton(hgdip: pointer; rect: GDIPAPI.TRect; Pressed: boolean);
var
  brush, pen, path: Pointer;
  crossRect: GDIPAPI.TRect;
  color1, color2: cardinal;
begin
  if Pressed then
  begin
    color1 := $ffff3030;
    color2 := $ffff3030;
  end else begin
    color1 := $ffffa0a0;
    color2 := $ffff3030;
  end;
  // button
  GdipCreatePath(FillModeWinding, path);
  AddPathRoundRect(path, rect, 2);
  GdipCreateLineBrushFromRectI(@rect, color1, color2, LinearGradientModeVertical, WrapModeTileFlipY, brush);
  GdipFillPath(hgdip, brush, path);
  GdipDeleteBrush(brush);
  GdipCreatePen1($a0000000, 1, UnitPixel, pen);
  GdipDrawPath(hgdip, pen, path);
  GdipDeletePen(pen);
  //
  GdipResetPath(path);
  AddPathRoundRect(path, rect.x + 1, rect.y + 1, rect.width - 2, rect.height - 2, 2);
  GdipCreatePen1($60ffffff, 1, UnitPixel, pen);
  GdipDrawPath(hgdip, pen, path);
  GdipDeletePen(pen);
  GdipDeletePath(path);
  // cross
  crossRect.Width := rect.Width div 2;
  crossRect.Height := rect.Height div 2;
  crossRect.X := rect.X + (rect.Width - crossRect.Width) div 2;
  crossRect.Y := rect.Y + (rect.Height - crossRect.Height) div 2;
  GdipCreatePen1($a0000000, 4, UnitPixel, pen);
  GdipDrawLineI(hgdip, pen, crossRect.X, crossRect.Y, crossRect.X + crossRect.Width, crossRect.Y + crossRect.Height);
  GdipDrawLineI(hgdip, pen, crossRect.X, crossRect.Y + crossRect.Height, crossRect.X + crossRect.Width, crossRect.Y);
  GdipDeletePen(pen);
  GdipCreatePen1($c0ffffff, 2, UnitPixel, pen);
  GdipDrawLineI(hgdip, pen, crossRect.X + 1, crossRect.Y + 1, crossRect.X + crossRect.Width - 1, crossRect.Y + crossRect.Height - 1);
  GdipDrawLineI(hgdip, pen, crossRect.X + 1, crossRect.Y - 1 + crossRect.Height, crossRect.X + crossRect.Width - 1, crossRect.Y + 1);
  GdipDeletePen(pen);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Paint;
var
  bmp: _SimpleBitmap;
  hgdip, brush, pen, path, epath, family, font, format: Pointer;
  titleRect: GDIPAPI.TRectF;
  rect: GDIPAPI.TRect;
  pt: GDIPAPI.TPoint;
  shadowEndColor: array [0..0] of ARGB;
  rgn: HRGN;
  radius, count, idx: integer;
  title: string;
begin
  // prepare //
  radius := 6;
  bmp.topleft.x := Fx;
  bmp.topleft.y := Fy;
  bmp.Width := FWidth;
  bmp.Height := FHeight;
  if not gdip_gfx.CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
  hgdip := CreateGraphics(bmp.dc, 0);
  if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
  GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
  GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);

  // FShadow path
  GdipCreatePath(FillModeWinding, path);
  GdipCreatePath(FillModeWinding, epath);
  AddPathRoundRect(epath, 0, 0, FWidth, FHeight, trunc(radius * 2.5));
  AddPathRoundRect(path, FShadow, FShadow, FWidth - FShadow * 2, FHeight - FShadow * 2, radius);
  GdipSetClipPath(hgdip, path, CombineModeReplace);
  GdipSetClipPath(hgdip, epath, CombineModeComplement);
  // FShadow gradient
  GdipCreatePathGradientFromPath(epath, brush);
  GdipSetPathGradientCenterColor(brush, $ff000000);
  shadowEndColor[0] := 0;
  count := 1;
  GdipSetPathGradientSurroundColorsWithCount(brush, @shadowEndColor, count);
  pt := MakePoint(FWidth div 2 + 1, FHeight div 2 + 1);
  GdipSetPathGradientCenterPointI(brush, @pt);
  GdipSetPathGradientFocusScales(brush, 1 - 0.25 * FHeight / FWidth, 1 - 0.25);
  GdipFillPath(hgdip, brush, epath);
  GdipResetClip(hgdip);
  GdipDeleteBrush(brush);
  GdipDeletePath(epath);
  // background fill
  rect := GDIPAPI.MakeRect(FShadow, FShadow, FWidth - FShadow * 2, FHeight - FShadow * 2);
  GdipCreateLineBrushFromRectI(@rect, FColor1, FColor2, LinearGradientModeVertical, WrapModeTileFlipY, brush);
  GdipFillPath(hgdip, brush, path);
  GdipDeleteBrush(brush);
  // dark border
  GdipCreatePen1($a0000000, 1, UnitPixel, pen);
  GdipDrawPath(hgdip, pen, path);
  GdipDeletePen(pen);
  // light border
  GdipResetPath(path);
  AddPathRoundRect(path, FShadow + 1, FShadow + 1, FWidth - FShadow * 2 - 2, FHeight - FShadow * 2 - 2, radius);
  GdipCreatePen1($a0ffffff, 1, UnitPixel, pen);
  GdipDrawPath(hgdip, pen, path);
  GdipDeletePen(pen);
  GdipDeletePath(path);

  // selection
  if FTopWindowIndex > -1 then
  begin
    GdipCreatePath(FillModeWinding, path);
    // selection fill
    rect := WinRectToGDIPRect(GetItemSelectionRect(FTopWindowIndex));
    AddPathRoundRect(path, rect, radius div 2);
    GdipCreateSolidFill($30b0c0ff, brush);
    GdipFillPath(hgdip, brush, path);
    GdipDeleteBrush(brush);
    // selection border
    GdipCreatePen1($a0b0c0ff, 1, UnitPixel, pen);
    GdipDrawPath(hgdip, pen, path);
    GdipDeletePen(pen);
    GdipResetPath(path);
    GdipDeletePath(path);
  end;

  // titles and close buttons
  GdipCreateFontFamilyFromName(PWideChar(WideString(FFontFamily)), nil, family);
  GdipCreateFont(family, FFontSize, 0, 2, font);
  GdipCreateSolidFill($ffffffff, brush);
  GdipCreateStringFormat(0, 0, format);
  GdipSetStringFormatFlags(format, StringFormatFlagsNoWrap);
  for idx := 0 to list.Count - 1 do
  begin
    titleRect := WinRectToGDIPRectF(GetTitleRect(idx));
    title := ProcessHelper.GetWindowText(THandle(list.Items[idx]));
    GdipDrawString(hgdip, PWideChar(WideString(title)), -1, font, @titleRect, format, brush);

    rect := WinRectToGDIPRect(GetCloseButtonRect(idx));
    DrawCloseButton(hgdip, rect, FCloseButtonDownIndex = idx);
  end;
  GdipDeleteStringFormat(format);
  GdipDeleteBrush(brush);
  GdipDeleteFont(font);
  GdipDeleteFontFamily(family);

  // update window //
  UpdateLWindow(FHWnd, bmp, 255);
  GdipDeleteGraphics(hgdip);
  gdip_gfx.DeleteBitmap(bmp);
  if not FCompositionEnabled then SetWindowPos(FHWnd, $ffffffff, Fx, Fy, FWidth, FHeight, swp_noactivate + swp_showwindow);

  // enable blur behind
  if FCompositionEnabled then
  begin
    rgn := CreateRoundRectRgn(FShadow, FShadow, FWidth - FShadow, FHeight - FShadow, radius * 2, radius * 2);
    dwm.EnableBlurBehindWindow(FHWnd, rgn);
    DeleteObject(rgn);
  end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.OpenWindow(AppList: TFPList; AX, AY: integer; AMonitor: integer; Site: integer): boolean;
var
  idx: integer;
  wa: windows.TRect;
  opaque: bool;
  hwnd: THandle;
begin
  result := false;
  if not FActivating then
  try
    try
      FActivating := true;
      FTopWindowIndex := -1;
      FCompositionEnabled := dwm.CompositionEnabled;
      KillTimer(FHWnd, ID_TIMER_CLOSE);

      UnRegisterThumbnails;

      list.Clear;
      if AppList.Count = 0 then
      begin
        CloseWindow;
        exit;
      end;
      list.AddList(AppList);
      for idx := 0 to list.Count - 1 do
      begin
        hwnd := THandle(list.Items[idx]);
        if IsWindowVisible(hwnd) and not IsIconic(hwnd) then
          if ProcessHelper.WindowOnTop(hwnd) then FTopWindowIndex := idx;
      end;

      // size
      FMonitor := AMonitor;
      FSite := Site;
      wa := GetMonitorRect(FMonitor);

      FCloseButtonSize := 17;
      FBorder := 22;
      FShadow := 8;
      if FCompositionEnabled then
      begin
        FTitleHeight := 30;
        ItemSplit := 16;
        ThumbW := 200;
        ThumbH := round(ThumbW * (wa.Bottom - wa.Top) / (wa.Right - wa.Left));
      end else begin
        FTitleHeight := 30;
        ItemSplit := 16;
        ThumbW := 200;
        ThumbH := 0;
      end;
      FWTarget := FBorder * 2 + list.Count * (ThumbW + ItemSplit) - ItemSplit;
      FHTarget := FBorder * 2 + FTitleHeight + ThumbH;
      if not FActive then
      begin
        FWidth := FWTarget;
        FHeight := FHTarget;
      end;

      // position (default is bottom)
      FXTarget := AX - FWTarget div 2;
      FYTarget := AY - FHTarget;
      if FSite = 1 then // top
      begin
        FXTarget := AX - FWTarget div 2;
        FYTarget := AY;
      end else if FSite = 0 then // left
      begin
        FXTarget := AX;
        FYTarget := AY - FHTarget div 2;
      end else if FSite = 2 then // right
      begin
        FXTarget := AX - FWTarget;
        FYTarget := AY - FHTarget div 2;
      end;
      //
      if FAnimate then
      begin
        if not FActive then
        begin
          Fx := FXTarget;
          Fy := FYTarget + 20;
          if Site = 1 then // top
          begin
            Fx := FXTarget;
            Fy := FYTarget - 20;
          end else if Site = 0 then // left
          begin
            Fx := FXTarget - 20;
            Fy := FYTarget;
          end else if Site = 2 then // right
          begin
            Fx := FXTarget + 20;
            Fy := FYTarget;
          end;
        end;
      end
      else
      begin
        Fx := FXTarget;
        Fy := FYTarget;
      end;

      // update color info
      if not FActive then
      begin
        if FCompositionEnabled then
        begin
          FColor1 := $50000000;
          FColor2 := $10ffffff;
        end else begin
          FColor1 := $ff101010;
          FColor2 := $ff808080;
        end;
        dwm.GetColorizationColor(FColor1, opaque);
      end;

      // show the window
      Paint;
      SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_showwindow);
      SetActiveWindow(FHWnd);
      if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
      FActive := true;

      // register thumbnails
      RegisterThumbnails;
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
    if FSite = 1 then // top
    begin
      FXTarget := AX - FWTarget div 2;
      FYTarget := AY;
    end else if FSite = 0 then // left
    begin
      FXTarget := AX;
      FYTarget := AY - FHTarget div 2;
    end else if FSite = 2 then // right
    begin
      FXTarget := AX - FWTarget;
      FYTarget := AY - FHTarget div 2;
    end;
    Fx := FXTarget;
    Fy := FYTarget;

    if FCompositionEnabled then UpdateLWindowPosAlpha(FHWnd, Fx, Fy, 255)
    else SetWindowPos(FHWnd, $ffffffff, Fx, Fy, 0, 0, swp_nosize + swp_noactivate + swp_showwindow);
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

  // WM_LBUTTONDOWN
  if msg.msg = WM_LBUTTONDOWN then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for idx := 0 to list.Count - 1 do
    begin
      rect := GetItemRect(idx);
      if PtInRect(rect, pt) then
      begin
        rect := GetCloseButtonRect(idx);
        if PtInRect(rect, pt) then
        begin
          FCloseButtonDownIndex := idx;
          Paint;
        end;
      end;
    end;
    exit;
  end
  // WM_LBUTTONUP
  else if msg.msg = WM_LBUTTONUP then
  begin
    FCloseButtonDownIndex := -1;
    Paint;
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for idx := 0 to list.Count - 1 do
    begin
      rect := GetItemRect(idx);
      if PtInRect(rect, pt) then
      begin
        rect := GetCloseButtonRect(idx);
        if PtInRect(rect, pt) then ProcessHelper.CloseWindow(THandle(list.Items[idx]))
        else ProcessHelper.ActivateWindow(THandle(list.Items[idx]));
      end;
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

