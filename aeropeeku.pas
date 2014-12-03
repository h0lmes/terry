unit aeropeeku;

interface

uses Windows, Messages, Classes, SysUtils, Forms,
  declu, dwm_unit, GDIPAPI, gdip_gfx, toolu, processhlp;

type
  TAPWLayout = (apwlHorizontal, apwlVertical);

  TAeroPeekWindowItem = packed record
    hwnd: THandle;            // target window handle
    ThumbnailId: THandle;     // live preview thumbnail handle
    image: pointer;           // window icon as GDIP bitmap
    iw: cardinal;             // icon width
    ih: cardinal;             // icon height
    rect: windows.TRect;      // item bounds rect
    rectSel: windows.TRect;   // item selection rect
    rectIcon: windows.TRect;  // window icon rect
    rectTitle: windows.TRect; // window title rect
    rectThumb: windows.TRect; // live thumbnail rect
    rectClose: windows.TRect; // close button rect
  end;

  { TAeroPeekWindow }

  TAeroPeekWindow = class
  private
    FHWnd: uint;
    WindowClassInstance: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FWTarget: integer;
    FHTarget: integer;
    FIconSize, FBorderX, FBorderY, FShadow, ThumbW, ThumbH, ItemSplit: integer;
    FTitleHeight, FTitleSplit: integer;
    FRadius, FSelectionRadius: integer;
    FCloseButtonSize: integer;
    FActivating: boolean;
    FActive: boolean;
    FHostWnd: THandle;
    FSite: integer;
    FWorkArea: windows.TRect;
    FAnimate: boolean;
    FCloseButtonDownIndex: integer;
    FForegroundWindowIndex: integer;
    FColor1, FColor2, FTextColor: cardinal;
    FCompositionEnabled: boolean;
    FFontFamily: string;
    FFontSize: integer;
    FLayout: TAPWLayout;
    FWindowCount, FProcessCount, FItemCount: integer;
    FHoverIndex: integer;
    items: array of TAeroPeekWindowItem;
    procedure ClearImages;
    procedure DrawCloseButton(hgdip: pointer; rect: GDIPAPI.TRect; Pressed: boolean);
    procedure Paint;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    procedure RegisterThumbnails;
    procedure SetItems(AppList: TFPList);
    procedure Timer;
    procedure UnRegisterThumbnails;
    procedure WindowProc(var msg: TMessage);
    procedure err(where: string; e: Exception);
  public
    property Handle: uint read FHWnd;
    property HostHandle: uint read FHostWnd;
    property Active: boolean read FActive;

    class function Open(HostWnd: THandle; AppList: TFPList; AX, AY, Site: integer; LivePreviews: boolean): boolean;
    class procedure SetPosition(AX, AY: integer);
    class procedure Close(Timeout: cardinal = 0);
    class function IsActive: boolean;
    class function ActivatedBy(HostWnd: THandle): boolean;
    class procedure Cleanup;

    constructor Create;
    destructor Destroy; override;
    function OpenWindow(HostWnd: THandle; AppList: TFPList; AX, AY, Site: integer; LivePreviews: boolean): boolean;
    procedure SetWindowPosition(AX, AY: integer);
    procedure CloseWindow;
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
//------------------------------------------------------------------------------
// open (show) AeroPeekWindow
class function TAeroPeekWindow.Open(HostWnd: THandle; AppList: TFPList; AX, AY, Site: integer; LivePreviews: boolean): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then
  begin
    KillTimer(AeroPeekWindow.Handle, ID_TIMER_CLOSE);
    result := AeroPeekWindow.OpenWindow(HostWnd, AppList, AX, AY, Site, LivePreviews);
  end;
end;
//------------------------------------------------------------------------------
// set new position
class procedure TAeroPeekWindow.SetPosition(AX, AY: integer);
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.SetWindowPosition(AX, AY);
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
// check if AeroPeekWindow is visible and was activated by a particular host
class function TAeroPeekWindow.ActivatedBy(HostWnd: THandle): boolean;
begin
  result := false;
  if assigned(AeroPeekWindow) then result := AeroPeekWindow.Active and (AeroPeekWindow.HostHandle = HostWnd);
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
  FFontSize := round(toolu.GetFontSize * 1.45);
  FCloseButtonDownIndex := -1;
  FItemCount := 0;
  FHoverIndex := -1;

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
procedure TAeroPeekWindow.RegisterThumbnails;
var
  index: integer;
  ThumbnailId: THandle;
begin
  if FCompositionEnabled then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
        dwm.RegisterThumbnail(FHWnd, items[index].hwnd, items[index].rectThumb, true, items[index].ThumbnailId);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.UnRegisterThumbnails;
var
  index: integer;
begin
  if FItemCount > 0 then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
        dwm.UnregisterThumbnail(items[index].ThumbnailId);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.ClearImages;
var
  index: integer;
begin
  if FItemCount > 0 then
    for index := 0 to FItemCount - 1 do
      if assigned(items[index].image) then
      begin
        GdipDisposeImage(items[index].image);
        items[index].image := nil;
        items[index].iw := 0;
        items[index].ih := 0;
      end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.OpenWindow(HostWnd: THandle; AppList: TFPList; AX, AY, Site: integer; LivePreviews: boolean): boolean;
var
  idx: integer;
  wa: windows.TRect;
  opaque: bool;
  hwnd, mon: THandle;
  pt: windows.TPoint;
  mi: MONITORINFO;
begin
  result := false;
  if not FActivating then
  try
    try
      FActivating := true;
      FCompositionEnabled := dwm.CompositionEnabled and LivePreviews;
      FAnimate := FCompositionEnabled;

      UnRegisterThumbnails;
      ClearImages;

      // read window list
      if AppList.Count = 0 then
      begin
        CloseWindow;
        exit;
      end;

      // size
      FHostWnd := HostWnd;
      FSite := Site;
      FLayout := apwlHorizontal;
      if not FCompositionEnabled or (FSite = 0) or (FSite = 2) then FLayout := apwlVertical;
      // get monitor work area
      pt.x := AX;
      pt.y := AY;
      mon := MonitorFromPoint(pt, MONITOR_DEFAULTTONEAREST);
      FillChar(mi, sizeof(mi), 0);
      mi.cbSize := sizeof(mi);
      GetMonitorInfoA(mon, @mi);
      FWorkArea := mi.rcWork;
      // set items' positions, calulate window size, update workarea
      SetItems(AppList);
      if not FActive then
      begin
        FWidth := FWTarget;
        FHeight := FHTarget;
      end;

      // calculate position
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
      // position window inside workarea
      if FXTarget + FWTarget > FWorkArea.Right then FXTarget := FWorkArea.Right - FWTarget;
      if FYTarget + FHTarget > FWorkArea.Bottom then FYTarget := FWorkArea.Bottom - FHTarget;
      if FXTarget < FWorkArea.Left then FXTarget := FWorkArea.Left;
      if FYTarget < FWorkArea.Top then FYTarget := FWorkArea.Top;

      // set starting position
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

      // assign colors
      if not FActive then
      begin
        dwm.GetColorizationColor(FColor1, opaque);
        FColor1 := FColor1 and $ffffff or $a0000000;
        FColor2 := $10ffffff;
        if not FCompositionEnabled then
        begin
          FColor1 := $ff6083a7;
          FColor2 := $ff6083a7;
        end;
        if opaque then
        begin
          FColor1 := FColor1 or $ff000000;
          FColor2 := FColor2 or $ff000000;
        end;
        FTextColor := $ffffffff;
        if (FColor1 shr 16 and $ff + FColor1 shr 8 and $ff + FColor1 and $ff) div 3 > $90 then FTextColor := $ff000000;
      end;

      // show the window
      Paint;
      // set foreground
      SetWindowPos(FHWnd, $ffffffff, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_showwindow);
      if not FActive then SetTimer(FHWnd, ID_TIMER, 10, nil);
      FActive := true;

      // register thumbnails
      RegisterThumbnails;
    finally
      FActivating := false;
    end;
  except
    on e: Exception do err('AeroPeekWindow.OpenWindow', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.SetItems(AppList: TFPList);
var
  index, iitem: integer;
  maxw, maxh, position: integer;
  pid, prevpid: dword;
  separators: boolean;
  //
  title: array [0..255] of WideChar;
  dc: HDC;
  hgdip, family, font: pointer;
  rect: GDIPAPI.TRectF;
begin
  // set primary params
  if FCompositionEnabled then
  begin
    FBorderX := 24;
    FBorderY := 22;
    FShadow := 8;
    FIconSize := 16;
    FTitleHeight := 20;
    FTitleSplit := 9;
    ItemSplit := 16;
    FCloseButtonSize := 17;
    FRadius := 6;
    FSelectionRadius := 2;
  end else begin
    FBorderX := 18;
    FBorderY := 14;
    FShadow := 0;
    FIconSize := 16;
    FTitleHeight := 22;
    FTitleSplit := 0;
    ItemSplit := 10;
    FCloseButtonSize := 17;
    FRadius := 0;
    FSelectionRadius := 2;
  end;

  // count processes
  FWindowCount := AppList.Count;
  if FWindowCount > 0 then FProcessCount := 1;
  index := 0;
  while index < FWindowCount do
  begin
    GetWindowThreadProcessId(THandle(AppList.Items[index]), @pid);
    if (index > 0) and (pid <> prevpid) then inc(FProcessCount);
    prevpid := pid;
    inc(index);
  end;
  separators := (FProcessCount > 1) and (FProcessCount < FWindowCount);

  // store handles, load icons
  FItemCount := 0;
  index := 0;
  prevpid := 0;
  pid := 0;
  while index < FWindowCount do
  begin
    inc(FItemCount);
    SetLength(items, FItemCount);
    iitem := FItemCount - 1;
    items[iitem].hwnd := THandle(AppList.Items[index]);
    if separators then GetWindowThreadProcessId(items[iitem].hwnd, @pid);
    if (index > 0) and (pid <> prevpid) then
    begin
      items[iitem].hwnd := 0;
    end else begin
      LoadImageFromHWnd(items[iitem].hwnd, FIconSize, true, false, items[iitem].image, items[iitem].iw, items[iitem].ih, 500);
      inc(index);
    end;
    prevpid := pid;
  end;

  // calc thumbnail width and height
  if FCompositionEnabled then
  begin
    if FLayout = apwlHorizontal then
    begin
      ThumbW := min(200, (FWorkArea.Right - FWorkArea.Left - FBorderX * 2) div FItemCount - ItemSplit);
      ThumbH := round(ThumbW * (FWorkArea.Bottom - FWorkArea.Top) / (FWorkArea.Right - FWorkArea.Left));
    end else begin
      ThumbW := 200;
      ThumbH := round(ThumbW * (FWorkArea.Bottom - FWorkArea.Top) / (FWorkArea.Right - FWorkArea.Left));
      if ThumbH > (FWorkArea.Bottom - FWorkArea.Top - FBorderY * 2) div FItemCount - FTitleHeight - ItemSplit then
      begin
        ThumbH := (FWorkArea.Bottom - FWorkArea.Top - FBorderY * 2) div FItemCount - FTitleHeight - ItemSplit;
        ThumbW := round(ThumbH * (FWorkArea.Right - FWorkArea.Left) / (FWorkArea.Bottom - FWorkArea.Top));
      end;
    end;
  end else begin
    ThumbW := 200;
    ThumbH := 0;
  end;

  if FItemCount > 0 then
  begin
    // get max title width (if in "no live preview" mode)
    if not FCompositionEnabled then
    begin
      dc := CreateCompatibleDC(0);
      if dc <> 0 then
      begin
        GdipCreateFromHDC(dc, hgdip);
        GdipCreateFontFamilyFromName(PWideChar(WideString(FFontFamily)), nil, family);
        GdipCreateFont(family, FFontSize, 0, 2, font);
        for index := 0 to FItemCount - 1 do
        begin
          GetWindowTextW(items[index].hwnd, @title, 255);
          rect.x := 0;
          rect.y := 0;
          rect.Width := 0;
          rect.Height := 0;
          GdipMeasureString(hgdip, PWideChar(@title), -1, font, @rect, nil, @rect, nil, nil);
          maxw := round(rect.Width) + 3 + FIconSize + 3 + FCloseButtonSize + 3;
          if maxw > (FWorkArea.Right - FWorkArea.Left) div 2 then maxw := (FWorkArea.Right - FWorkArea.Left) div 2;
          if maxw > ThumbW then ThumbW := maxw;
        end;
        GdipDeleteGraphics(hgdip);
        GdipDeleteFont(font);
        GdipDeleteFontFamily(family);
        DeleteDC(dc);
      end;
    end;

    // set item props
    maxw := 0;
    maxh := 0;
    FForegroundWindowIndex := -1;
    if FLayout = apwlHorizontal then position := FBorderX else position := FBorderY;
    for index := 0 to FItemCount - 1 do
    begin
      if FLayout = apwlHorizontal then
      begin
        items[index].rect.Left := position;
        items[index].rect.Top := FBorderY;
      end else begin
        items[index].rect.Left := FBorderX;
        items[index].rect.Top := position;
      end;
      if items[index].hwnd <> 0 then
      begin
        items[index].rect.Right := items[index].rect.Left + ThumbW;
        items[index].rect.Bottom := items[index].rect.Top + FTitleHeight + FTitleSplit + ThumbH;
      end else begin
        if FLayout = apwlHorizontal then
        begin
          items[index].rect.Right := items[index].rect.Left + 10;
          items[index].rect.Bottom := items[index].rect.Top + FTitleHeight + FTitleSplit + ThumbH;
        end else begin
          items[index].rect.Right := items[index].rect.Left + ThumbW;
          items[index].rect.Bottom := items[index].rect.Top + 2;
        end;
      end;
      if items[index].rect.Right - items[index].rect.Left > maxw then maxw := items[index].rect.Right - items[index].rect.Left;
      if items[index].rect.Bottom - items[index].rect.Top > maxh then maxh := items[index].rect.Bottom - items[index].rect.Top;

      if items[index].hwnd <> 0 then
      begin
        items[index].rectSel := items[index].rect;
        items[index].rectSel.Left -= 5;
        items[index].rectSel.Top -= 5;
        items[index].rectSel.Right += 5;
        items[index].rectSel.Bottom += 5;

        items[index].rectThumb := items[index].rect;
        items[index].rectThumb.Top += FTitleHeight;
        items[index].rectThumb.Top += FTitleSplit;

        items[index].rectTitle := items[index].rect;
        if assigned(items[index].image) then
          items[index].rectTitle.Left += items[index].iw + 3;
        items[index].rectTitle.Right -= FCloseButtonSize + 1;
        items[index].rectTitle.Bottom := items[index].rectTitle.Top + FTitleHeight;

        items[index].rectIcon := items[index].rect;
        items[index].rectIcon.Top += round((FTitleHeight - FIconSize) / 2);
        items[index].rectIcon.Right := items[index].rectIcon.Left + items[index].iw;
        items[index].rectIcon.Bottom := items[index].rectIcon.Top + items[index].ih;

        items[index].rectClose := items[index].rect;
        items[index].rectClose.Top += round((FTitleHeight - FCloseButtonSize) / 2);
        items[index].rectClose.Left := items[index].rectClose.Right - FCloseButtonSize;
        items[index].rectClose.Bottom := items[index].rectClose.Top + FCloseButtonSize;

        if IsWindowVisible(items[index].hwnd) and not IsIconic(items[index].hwnd) then
           if ProcessHelper.WindowOnTop(items[index].hwnd) then FForegroundWindowIndex := index;
      end;

      if FLayout = apwlHorizontal then
      begin
        if items[index].hwnd <> 0 then position += ThumbW else position += 10;
      end else begin
        if items[index].hwnd <> 0 then position += FTitleHeight + FTitleSplit + ThumbH else position += 2;
      end;
      if index < FItemCount - 1 then position += ItemSplit;
    end;

    // calc width and height
    if FLayout = apwlHorizontal then
    begin
      position += FBorderX;
      FWTarget := position;
      FHTarget := FBorderY * 2 + maxh;
    end else begin
      FWTarget := FBorderX * 2 + maxw;
      position += FBorderY;
      FHTarget := position;
    end;
  end;
  if not FAnimate then
  begin
    FWidth := FWTarget;
    FHeight := FHTarget;
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Paint;
var
  bmp: _SimpleBitmap;
  hgdip, brush, pen, path, shadow_path, family, font, format: Pointer;
  titleRect: GDIPAPI.TRectF;
  rect: GDIPAPI.TRect;
  pt: GDIPAPI.TPoint;
  shadowEndColor: array [0..0] of ARGB;
  rgn: HRGN;
  count, index, tmp: integer;
  title: array [0..255] of WideChar;
begin
  try
    // prepare //
    bmp.topleft.x := Fx;
    bmp.topleft.y := Fy;
    bmp.Width := FWidth;
    bmp.Height := FHeight;
    if not gdip_gfx.CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);

    //
    GdipCreatePath(FillModeWinding, path);
    AddPathRoundRect(path, FShadow, FShadow, FWidth - FShadow * 2, FHeight - FShadow * 2, FRadius);

    // shadow
    if FShadow > 0 then
    begin
      // FShadow path
      GdipCreatePath(FillModeWinding, shadow_path);
      AddPathRoundRect(shadow_path, 0, 0, FWidth, FHeight, trunc(FRadius * 2.5));
      GdipSetClipPath(hgdip, path, CombineModeReplace);
      GdipSetClipPath(hgdip, shadow_path, CombineModeComplement);
      // FShadow gradient
      GdipCreatePathGradientFromPath(shadow_path, brush);
      GdipSetPathGradientCenterColor(brush, $ff000000);
      shadowEndColor[0] := 0;
      count := 1;
      GdipSetPathGradientSurroundColorsWithCount(brush, @shadowEndColor, count);
      pt := MakePoint(FWidth div 2 + 1, FHeight div 2 + 1);
      GdipSetPathGradientCenterPointI(brush, @pt);
      GdipSetPathGradientFocusScales(brush, 1 - FShadow / FWidth * 5, 1 - FShadow / FHeight * 5);
      GdipFillPath(hgdip, brush, shadow_path);
      GdipResetClip(hgdip);
      GdipDeleteBrush(brush);
      GdipDeletePath(shadow_path);
    end;

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
    AddPathRoundRect(path, FShadow + 1, FShadow + 1, FWidth - FShadow * 2 - 2, FHeight - FShadow * 2 - 2, FRadius);
    GdipCreatePen1($a0ffffff, 1, UnitPixel, pen);
    GdipDrawPath(hgdip, pen, path);
    GdipDeletePen(pen);
    GdipDeletePath(path);

    // item selection
    if (FItemCount > 0) and (FForegroundWindowIndex > -1) then
    begin
      GdipCreatePath(FillModeWinding, path);
      // selection fill
      rect := WinRectToGDIPRect(items[FForegroundWindowIndex].rectSel);
      AddPathRoundRect(path, rect, FSelectionRadius);
      GdipCreateSolidFill($40b0d0ff, brush);
      GdipFillPath(hgdip, brush, path);
      GdipDeleteBrush(brush);
      // selection border
      GdipCreatePen1($c0ffffff, 1, UnitPixel, pen);
      GdipDrawPath(hgdip, pen, path);
      GdipDeletePen(pen);
      GdipDeletePath(path);
    end;

    // item hover selection
    if (FItemCount > 0) and (FHoverIndex > -1) then
    begin
      GdipCreatePath(FillModeWinding, path);
      // selection fill
      rect := WinRectToGDIPRect(items[FHoverIndex].rectSel);
      AddPathRoundRect(path, rect, FSelectionRadius);
      GdipCreateSolidFill($30ffffff, brush);
      GdipFillPath(hgdip, brush, path);
      GdipDeleteBrush(brush);
      // selection border
      GdipCreatePen1($50ffffff, 1, UnitPixel, pen);
      GdipDrawPath(hgdip, pen, path);
      GdipDeletePen(pen);
      GdipDeletePath(path);
      // close button
      rect := WinRectToGDIPRect(items[FHoverIndex].rectClose);
      DrawCloseButton(hgdip, rect, FCloseButtonDownIndex = FHoverIndex);
    end;

    // icons and titles ... or separators
    GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAliasGridFit);
    GdipCreateFontFamilyFromName(PWideChar(WideString(FFontFamily)), nil, family);
    GdipCreateFont(family, FFontSize, 0, 2, font);
    GdipCreateSolidFill(FTextColor, brush);
    GdipCreateStringFormat(0, 0, format);
    GdipSetStringFormatFlags(format, StringFormatFlagsNoWrap or StringFormatFlagsNoFitBlackBox);
    GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd = 0 then // separator
      begin
        if FLayout = apwlHorizontal then
        begin
          // vertical lines
          tmp := items[index].rect.Left + (items[index].rect.Right - items[index].rect.Left) div 2;
          GdipCreatePen1($80000000, 1, UnitPixel, pen);
          GdipDrawLineI(hgdip, pen, tmp, items[index].rect.Top, tmp, items[index].rect.Bottom);
          GdipDeletePen(pen);
          inc(tmp);
          GdipCreatePen1($80ffffff, 1, UnitPixel, pen);
          GdipDrawLineI(hgdip, pen, tmp, items[index].rect.Top, tmp, items[index].rect.Bottom);
          GdipDeletePen(pen);
        end else begin
          // horizontal lines
          tmp := items[index].rect.Top + (items[index].rect.Bottom - items[index].rect.Top) div 2;
          GdipCreatePen1($80000000, 1, UnitPixel, pen);
          GdipDrawLineI(hgdip, pen, items[index].rect.Left, tmp, items[index].rect.Right, tmp);
          GdipDeletePen(pen);
          inc(tmp);
          GdipCreatePen1($80ffffff, 1, UnitPixel, pen);
          GdipDrawLineI(hgdip, pen, items[index].rect.Left, tmp, items[index].rect.Right, tmp);
          GdipDeletePen(pen);
        end;
      end else // regular item
      begin
        // icon
        if assigned(items[index].image) then
          GdipDrawImageRectRectI(hgdip, items[index].image,
            items[index].rectIcon.Left, items[index].rectIcon.Top, items[index].iw, items[index].ih,
            0, 0, items[index].iw, items[index].ih, UnitPixel, nil, nil, nil);
        // window title
        GetWindowTextW(items[index].hwnd, title, 255);
        titleRect := WinRectToGDIPRectF(items[index].rectTitle);
        if index <> FHoverIndex then titleRect.Width := items[index].rectClose.Right - items[index].rectTitle.Left;
        GdipDrawString(hgdip, PWideChar(@title), -1, font, @titleRect, format, brush);
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
      rgn := CreateRoundRectRgn(FShadow, FShadow, FWidth - FShadow, FHeight - FShadow, FRadius * 2, FRadius * 2);
      dwm.EnableBlurBehindWindow(FHWnd, rgn);
      DeleteObject(rgn);
    end;
  except
    on e: Exception do err('AeroPeekWindow.Paint', e);
  end;
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
procedure TAeroPeekWindow.SetWindowPosition(AX, AY: integer);
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
    // position window inside workarea
    if FXTarget + FWTarget > FWorkArea.Right then FXTarget := FWorkArea.Right - FWTarget;
    if FYTarget + FHTarget > FWorkArea.Bottom then FYTarget := FWorkArea.Bottom - FHTarget;
    if FXTarget < FWorkArea.Left then FXTarget := FWorkArea.Left;
    if FYTarget < FWorkArea.Top then FYTarget := FWorkArea.Top;

    Fx := FXTarget;
    Fy := FYTarget;

    if FCompositionEnabled then UpdateLWindowPosAlpha(FHWnd, Fx, Fy, 255)
    else SetWindowPos(FHWnd, $ffffffff, Fx, Fy, 0, 0, swp_nosize + swp_noactivate + swp_showwindow);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.CloseWindow;
begin
  try
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    KillTimer(FHWnd, ID_TIMER);
    UnRegisterThumbnails;
    ClearImages;
    ShowWindow(FHWnd, SW_HIDE);
    FActive := false;
    TAeroPeekWindow.Cleanup;
  except
    on e: Exception do err('AeroPeekWindow.CloseWindow', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Timer;
var
  delta: integer;
  rect: windows.TRect;
begin
  if FActive and not FActivating then
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
  index: integer;
  pt: windows.TPoint;
  rect: windows.TRect;
  hovered: boolean;
begin
  msg.Result := 0;

  // WM_LBUTTONDOWN
  if msg.msg = WM_LBUTTONDOWN then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    for index := 0 to FItemCount - 1 do
    begin
      if items[index].hwnd <> 0 then
        if PtInRect(items[index].rectSel, pt) then
          if PtInRect(items[index].rectClose, pt) then
          begin
            FCloseButtonDownIndex := index;
            Paint;
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
    for index := 0 to FItemCount - 1 do
    begin
      if items[index].hwnd <> 0 then
        if PtInRect(items[index].rectSel, pt) then
        begin
          if PtInRect(items[index].rectClose, pt) then
          begin
            ProcessHelper.CloseWindow(items[index].hwnd);
          end
          else ProcessHelper.ActivateWindow(items[index].hwnd);
        end;
    end;
    CloseWindow;
    exit;
  end
  // WM_MOUSEMOVE
  else if msg.msg = WM_MOUSEMOVE then
  begin
    pt.x := TSmallPoint(msg.lParam).x;
    pt.y := TSmallPoint(msg.lParam).y;
    hovered := false;
    for index := 0 to FItemCount - 1 do
    begin
      if items[index].hwnd <> 0 then
        if PtInRect(items[index].rectSel, pt) then
        begin
          hovered := true;
          if FHoverIndex <> index then
          begin
            FHoverIndex := index;
            Paint;
          end;
        end;
    end;
    if not hovered and (FHoverIndex > -1) then
    begin
      FHoverIndex := -1;
      Paint;
    end;
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

