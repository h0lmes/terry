unit taskitemu;

{$t+}

interface
uses Windows, jwaWindows, Messages, SysUtils, Controls, Classes,
  Math, GDIPAPI, gdip_gfx, declu, dockh, customitemu, toolu, processhlp, aeropeeku;

type

  { TTaskItem }

  TTaskItem = class(TCustomItem)
  private
    FProcName: string;
    FAppList: TFPList;
    procedure UpdateItemInternal;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure CheckAppList;
    procedure Exec;
    procedure CloseList;
    procedure OpenList;
  public
    function WindowInList(hwnd: THandle): boolean;
    procedure UpdateTaskItem(hwnd: THandle);

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
  end;

implementation
//------------------------------------------------------------------------------
constructor TTaskItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FCanDrag := false;
  FDontSave := true;
  FProcName := '';
  FAppList := TFPList.Create;
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
procedure TTaskItem.UpdateTaskItem(hwnd: THandle);
var
  ProcName: string;
begin
  if FFreed then exit;
  // check window process name
  ProcName := ProcessHelper.GetAppWindowProcessFullName(hwnd);
  if FProcName = '' then FProcName := ProcName;

  // if window belong to the same process
  if ProcName <> '' then
    if ProcName = FProcName then
    begin
      // add the window to the list
      if FAppList.IndexOf(pointer(hwnd)) < 0 then
      begin
        FAppList.Add(pointer(hwnd));
        UpdateItemInternal;
      end;
    end;
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
      FIW := 0;
      FIH := 0;
      if FAppList.Count <= 0 then
      begin
        FCaption := '';
      end else begin
        hwnd := THandle(FAppList.Items[0]);
        LoadImageFromHWnd(hwnd, FBigItemSize, false, false, FImage, FIW, FIH, 3000);
        Caption := TProcessHelper.GetWindowText(hwnd);
      end;
    finally
      FUpdating:= false;
    end;

    Redraw;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal'#10#13 + e.message);
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
  dst, brush, hff, hfont, hformat: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  rect: GDIPAPI.TRectF;
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
        exit;
      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW = SWP_HIDEWINDOW then exit;
      UpdateHint(xReal, yReal);
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

      xBitmap := 0;
      yBitmap := 0;
      inc(xBitmap, ItemRect.Left);
      inc(yBitmap, ItemRect.Top);
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
        //
        rect.Width := FSize * 5 div 12;
        rect.Height := FSize div 3;
        rect.X := ItemRect.Right - rect.Width;
        rect.Y := ItemRect.Top;
        GdipCreateSolidFill($ffff0000, brush);
        GdipFillRectangle(dst, brush, rect.X, rect.Y, rect.Width, rect.Height);
        GdipDeleteBrush(brush);
        //
        GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, hff);
        GdipCreateFont(hff, FSize div 4, 1, 2, hfont);
        GdipCreateSolidFill($ffffffff, brush);
        GdipCreateStringFormat(0, 0, hformat);
        GdipSetStringFormatAlign(hformat, StringAlignmentCenter);
        GdipSetStringFormatLineAlign(hformat, StringAlignmentCenter);
        GdipDrawString(dst, PWideChar(WideString(inttostr(FAppList.Count))), -1, hfont, @rect, hformat, brush);
        GdipDeleteStringFormat(hformat);
        GdipDeleteBrush(brush);
        GdipDeleteFont(hfont);
        GdipDeleteFontFamily(hff);
      end;

    if FReflection and (FReflectionSize > 0) and not FFloating and assigned(FImage) then
      BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
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
          postmessage(THandle(FAppList.Items[idx]), WM_SYSCOMMAND, SC_CLOSE, 0);
    $f002:
        if FProcName <> '' then dockh.DockAddProgram(pchar(FProcName));
    $f003..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WndMessage(var msg: TMessage);
begin
  if FFreed then exit;

  // WM_ACTIVATEAPP
  if (msg.msg = WM_ACTIVATEAPP) and (msg.wParam = 0) then CloseList;

  if (msg.msg = WM_TIMER) and (msg.wParam = ID_TIMER) then
  begin
    // validate windows
    CheckAppList;
    // update item caption
    if FAppList.Count > 0 then
      Caption := TProcessHelper.GetWindowText(THandle(FAppList.Items[0]));
    // delete self if no windows left
    if FAppList.Count = 0 then Delete;
  end;
end;
//------------------------------------------------------------------------------
// delete non-existing windows
procedure TTaskItem.CheckAppList;
var
  idx, old_count: integer;
  hwnd: THandle;
begin
  old_count := FAppList.Count;
  if FAppList.Count > 0 then
  begin
    for idx := FAppList.Count - 1 downto 0 do
    begin
      hwnd := THandle(FAppList.Items[idx]);
      if ProcessHelper.GetAppWindowIndex(hwnd) < 0 then FAppList.Delete(idx);
    end;
  end;
  if FAppList.Count <> old_count then Redraw;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Exec;
begin
  if FAppList.Count = 1 then OpenList;
    //ProcessHelper.ActivateWindow(THandle(FAppList.Items[0]));
  if FAppList.Count > 1 then OpenList;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Save(szIni: pchar; szIniGroup: pchar);
begin
end;
//------------------------------------------------------------------------------
procedure TTaskItem.OpenList;
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
  TAeroPeekWindow.Open(FAppList, pt.x, pt.y, 0);
end;
//------------------------------------------------------------------------------
procedure TTaskItem.CloseList;
begin
  TAeroPeekWindow.Close;
end;
//------------------------------------------------------------------------------
end.

