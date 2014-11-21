unit taskitemu;

{$t+}

interface
uses Windows, jwaWindows, Messages, SysUtils, Controls, Classes,
  Math, GDIPAPI, gdip_gfx, declu, dockh, customitemu, toolu, processhlp,
  aeropeeku, dwm_unit, themeu;

type

  { TTaskItem }

  TTaskItem = class(TCustomItem)
  private
    FProcName: string;
    FAppList: TFPList;
    FIsOpen: boolean;
    FGrouping: boolean;
    FLivePreviews: boolean;
    procedure UpdateImage;
    procedure UpdateItemInternal;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure Exec;
    procedure ClosePeekWindow(Timeout: cardinal = 0);
    procedure ShowPeekWindow(Timeout: cardinal = 0);
    procedure UpdatePeekWindow;
  public
    function WindowInList(hwnd: THandle): boolean;
    function IsEmpty: boolean;
    procedure UpdateTaskItem(hwnd: THandle);
    function RemoveWindow(hwnd: THandle): boolean;
    procedure UpdateCaption;

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHover(AHover: boolean); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make(Grouping, LivePreviews: boolean): string;
  end;

implementation
//------------------------------------------------------------------------------
constructor TTaskItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FCanDrag := false;
  FDontSave := true;
  FGrouping := true;
  try FGrouping := boolean(strtoint(FetchValue(AData, 'gr="', '";')));
  except end;
  FLivePreviews := true;
  try FLivePreviews := boolean(strtoint(FetchValue(AData, 'lp="', '";')));
  except end;
  FProcName := '';
  FAppList := TFPList.Create;
  FIsOpen := false;
  SetTimer(FHWnd, ID_TIMER, 1000, nil);
end;
//------------------------------------------------------------------------------
destructor TTaskItem.Destroy;
begin
  FFreed := true;
  KillTimer(FHWnd, ID_TIMER);
  FAppList.free;
  try GdipDisposeImage(FImage);
  except end;
  inherited;
end;
//------------------------------------------------------------------------------
function TTaskItem.WindowInList(hwnd: THandle): boolean;
begin
  UpdateTaskItem(hwnd);
  result := FAppList.IndexOf(pointer(hwnd)) >= 0;
end;
//------------------------------------------------------------------------------
function TTaskItem.IsEmpty: boolean;
begin
  result := FAppList.Count = 0;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateTaskItem(hwnd: THandle);
var
  ProcName: string;
begin
  if FFreed or (not FGrouping and (FAppList.Count > 0)) then exit;

  if (FAppList.Count = 0) and (FProcName = '') then // if this is new item
  begin
    FAppList.Add(pointer(hwnd));
    FProcName := ProcessHelper.GetWindowProcessFullName(hwnd);
    UpdateItemInternal;
    exit;
  end;

  // try to get process executable path
  if FProcName = '' then
    FProcName := ProcessHelper.GetWindowProcessFullName(THandle(FAppList.Items[0]));

  // if the window is already in this group - do nothing
  if FAppList.IndexOf(pointer(hwnd)) >= 0 then exit;

  // if the window belong to the same process - add the window to the list
  ProcName := ProcessHelper.GetWindowProcessFullName(hwnd);
  if ProcName <> '' then
    if ProcName = FProcName then
    begin
      FAppList.Add(pointer(hwnd));
      UpdateItemInternal;
    end;
end;
//------------------------------------------------------------------------------
function TTaskItem.RemoveWindow(hwnd: THandle): boolean;
var
  index: integer;
begin
  result := false;
  if FFreed then exit;

  if FAppList.Count > 0 then
  begin
    index := FAppList.IndexOf(pointer(hwnd));
    if index >= 0 then
    begin
      FAppList.Delete(index);
      UpdateItemInternal;
      result := true;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateCaption;
begin
  if FAppList.Count = 1 then
    Caption := TProcessHelper.GetWindowText(THandle(FAppList.Items[0]));
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateItemInternal;
var
  hwnd: THandle;
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;
      // sort list
      if FAppList.Count > 1 then ProcessHelper.SortAppWindows(FAppList);
      // update image
      UpdateImage;
      // update caption
      if FAppList.Count = 1 then Caption := TProcessHelper.GetWindowText(hwnd) else Caption := '';
    finally
      FUpdating:= false;
    end;

    if FIsOpen then ShowPeekWindow; // update peek window
    Redraw;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateImage;
begin
  try
    if FAppList.Count > 0 then
    begin
      LoadAppImage(FProcName, THandle(FAppList.Items[0]), FBigItemSize, false, false, FImage, FIW, FIH, 500);
    end;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateImage'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TTaskItem.cmd(id: TGParam; param: integer): integer;
var
  temp: uint;
begin
  try
    result := inherited cmd(id, param);

    case id of
      // parameters //
      gpBigItemSize:
        begin
          if FBigItemSize <= 96 then temp := 96
          else if FBigItemSize <= 128 then temp := 128
          else if FBigItemSize <= 160 then temp := 160
          else if FBigItemSize <= 192 then temp := 192
          else if FBigItemSize <= 256 then temp := 256;
          if temp <> FIW then UpdateItemInternal;
        end;
      gpMonitor: ClosePeekWindow(0);
      gpSite: ClosePeekWindow(0);

      // commands //
      icIsItem: result := 0;
    end;

  except
    on e: Exception do raise Exception.Create('TaskItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TTaskItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst, brush, family, hfont, format, path: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  rect: GDIPAPI.TRectF;
  button: boolean;
begin
  try
    if FFreed or FUpdating or (FFloating and not AForce) then exit;

    // set position //
    try
      ItemRect := GetRectFromSize(ASize);
      Fx := Ax;
      Fy := Ay;
      FShowItem := AShowItem;
      if need_dock then
      begin
        Ax := FxDocking;
        Ay := FyDocking;
      end;
      xReal := Ax - ItemRect.Left;
      yReal := Ay - ItemRect.Top;

      if (FSize = ASize) and not AForce then
      begin
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem)
        else SetWindowPos(FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        UpdateHint(xReal, yReal);
        if FIsOpen then UpdatePeekWindow;
        exit;
      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW <> 0 then exit;
      UpdateHint(xReal, yReal);
      if FIsOpen then UpdatePeekWindow;
    except
      on e: Exception do raise Exception.Create('SetPosition'#10#13 + e.message);
    end;

    // init drawing //
    try
      bmp.topleft.x := xReal;
      bmp.topleft.y := yReal;
      bmp.width := FSize + ItemRect.Left * 2;
      bmp.height := FSize + ItemRect.Top * 2;
      if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
      GdipCreateFromHDC(bmp.dc, dst);
      if not assigned(dst) then raise Exception.Create('CreateGraphics failed');
      GdipSetCompositingMode(dst, CompositingModeSourceOver);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
      GdipDeleteBrush(brush);

      xBitmap := ItemRect.Left;
      yBitmap := ItemRect.Top;

      button := theme.DrawButton(dst, xBitmap, yBitmap, FSize);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
    except
      on e: Exception do raise Exception.Create('InitDraw'#10#13 + e.message);
    end;

    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, nil, nil, nil);

    // draw windows count indicator
    if assigned(FAppList) then
      if FAppList.Count > 1 then
      begin
        GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
        GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
        // background
        rect.Width := FItemSize / 2;
        rect.Height := FItemSize / 3;
        rect.X := ItemRect.Right - rect.Width + 1;
        rect.Y := ItemRect.Top - 1;
        GdipCreatePath(FillModeWinding, path);
        AddPathRoundRect(path, rect, rect.Height / 2);
        GdipCreateSolidFill($ffff0000, brush);
        GdipFillPath(dst, brush, path);
        GdipDeleteBrush(brush);
        GdipDeletePath(path);
        // number
        GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, family);
        GdipCreateFont(family, FItemSize div 4, 1, 2, hfont);
        GdipCreateSolidFill($ffffffff, brush);
        GdipCreateStringFormat(0, 0, format);
        GdipSetStringFormatAlign(format, StringAlignmentCenter);
        GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
        GdipDrawString(dst, PWideChar(WideString(inttostr(FAppList.Count))), -1, hfont, @rect, format, brush);
        GdipDeleteStringFormat(format);
        GdipDeleteBrush(brush);
        GdipDeleteFont(hfont);
        GdipDeleteFontFamily(family);
      end;

    if not button then
    begin
      if FReflection and (FReflectionSize > 0) and not FFloating and assigned(FImage) then
        BitmapReflection(bmp, xBitmap, yBitmap, FSize, FReflectionSize, FSite);
      theme.DrawIndicator(dst, xBitmap, yBitmap, FSize, FSite);
    end;
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));

    DeleteGraphics(dst);
    DeleteBitmap(bmp);

  except
    on e: Exception do raise Exception.Create('TaskItem.Draw(' + FCaption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TTaskItem.ToString: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
procedure TTaskItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
begin
  if button = mbLeft then Exec;

  if button = mbRight then
  begin
    ClosePeekWindow;
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;

  UpdateItemInternal; // update item icon and text //
end;
//------------------------------------------------------------------------------
function TTaskItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  result := true;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING + ifthen(FProcName = '', MF_DISABLED, 0), $f003, pchar(UTF8ToAnsi(XKillProcess)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
  if FAppList.Count = 1 then AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XCloseWindow)))
  else AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XCloseAllWindows)));
  AppendMenu(FHMenu, MF_STRING + ifthen(FProcName = '', MF_DISABLED, 0), $f002, pchar(UTF8ToAnsi(XPinToDock)));
  LME(true);

  msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
var
  item: cardinal;
  str: string;
  idx: integer;
begin
  result := 0;
  LME(false);
  DestroyMenu(FHMenu);
  FHMenu := 0;
  case wParam of // f001 to f020
    $f001:
      if FAppList.Count > 0 then
        for idx := FAppList.Count - 1 downto 0 do
          ProcessHelper.CloseWindow(THandle(FAppList.Items[idx]));
    $f002:
        if FProcName <> '' then dockh.DockAddProgram(pchar(FProcName));
    $f003:
        if FProcName <> '' then ProcessHelper.Kill(FProcName);
    $f004..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.MouseHover(AHover: boolean);
begin
  FHideHint := TAeroPeekWindow.IsActive;
  inherited;
  FHideHint := false;

  if not FFreed then
    if AHover then
    begin
      if TAeroPeekWindow.IsActive then
      begin
        if TAeroPeekWindow.ActivatedBy(FHWnd) then ShowPeekWindow else ShowPeekWindow(100);
      end
      else ShowPeekWindow(800);
    end else begin
      ClosePeekWindow(800);
    end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WndMessage(var msg: TMessage);
begin
  if FFreed then exit;

  // WM_ACTIVATEAPP
  if (msg.msg = WM_ACTIVATEAPP) and (msg.wParam = 0) then ClosePeekWindow;

  // WM_TIMER
  if msg.msg = WM_TIMER then
  begin
    // "OPEN" TIMER
    if msg.wParam = ID_TIMER_OPEN then ShowPeekWindow;
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Exec;
begin
  if FAppList.Count = 1 then
  begin
    KillTimer(FHWnd, ID_TIMER_OPEN);
    ProcessHelper.ActivateWindow(THandle(FAppList.Items[0]));
  end;
  if FAppList.Count > 1 then
    if TAeroPeekWindow.IsActive then ClosePeekWindow else ShowPeekWindow;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Save(szIni: pchar; szIniGroup: pchar);
begin
end;
//------------------------------------------------------------------------------
procedure TTaskItem.ShowPeekWindow(Timeout: cardinal = 0);
var
  pt: windows.TPoint;
begin
  if Timeout > 0 then
  begin
    SetTimer(FHWnd, ID_TIMER_OPEN, Timeout, nil);
    exit;
  end;

  KillTimer(FHWnd, ID_TIMER_OPEN);

  pt := GetScreenRect.TopLeft;
  if (FSite = 1) or (FSite = 3) then inc(pt.x, FSize div 2);
  if (FSite = 0) or (FSite = 2) then inc(pt.y, FSize div 2);
  if FSite = 0 then inc(pt.x, FSize);
  if FSite = 1 then inc(pt.y, FSize);
  case FSite of
    0: inc(pt.x, 10);
    1: inc(pt.y, 10);
    2: dec(pt.x, 10);
    3: dec(pt.y, 10);
  end;
  //LME(true);
  FHideHint := true;
  UpdateHint;
  TAeroPeekWindow.Open(FHWnd, FAppList, pt.x, pt.y, FSite, FLivePreviews);
  FIsOpen := true;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.ClosePeekWindow(Timeout: cardinal = 0);
begin
  KillTimer(FHWnd, ID_TIMER_OPEN);
  if FHideHint then
  begin
    FHideHint := false;
    UpdateHint;
  end;
  if FIsOpen then
  begin
    FIsOpen := false;
    //LME(false);
    TAeroPeekWindow.Close(Timeout);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdatePeekWindow;
var
  pt: windows.TPoint;
begin
  pt := GetScreenRect.TopLeft;
  if (FSite = 1) or (FSite = 3) then inc(pt.x, FSize div 2);
  if (FSite = 0) or (FSite = 2) then inc(pt.y, FSize div 2);
  if FSite = 0 then inc(pt.x, FSize);
  if FSite = 1 then inc(pt.y, FSize);
  case FSite of
    0: inc(pt.x, 10);
    1: inc(pt.y, 10);
    2: dec(pt.x, 10);
    3: dec(pt.y, 10);
  end;
  TAeroPeekWindow.SetPosition(pt.x, pt.y);
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
class function TTaskItem.Make(Grouping, LivePreviews: boolean): string;
begin
  result := 'class="task";' +
    'gr="' + inttostr(integer(Grouping)) + '";' +
    'lp="' + inttostr(integer(LivePreviews)) + '";';
end;
//------------------------------------------------------------------------------
end.

