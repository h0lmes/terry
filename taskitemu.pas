unit taskitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes,
  Math, GDIPAPI, gfx, declu, dockh, customitemu, toolu, processhlp,
  aeropeeku, dwm_unit, themeu;

type

  { TTaskItem }

  TTaskItem = class(TCustomItem)
  private
    FProcName: string;
    FAppList: TFPList;
    FIsNew: boolean;
    FIsOpen: boolean;
    FTaskLivePreviews: boolean;
    FTaskThumbSize: integer;
    FTaskGrouping: boolean;
    FIsExecutable: boolean;
    FAttention: boolean;
    procedure BeforeUndock;
    procedure UpdateImage;
    procedure UpdateItemInternal;
    procedure DrawWindowsCount(dst: pointer; winList: TFPList; x, y, Size: integer);
    procedure Attention(value: boolean);
    procedure BeforeMouseHover(AHover: boolean);
    procedure MouseHover(AHover: boolean);
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure Exec(action: TExecuteAction);
    procedure ClosePeekWindow(Timeout: cardinal = 0);
    procedure ShowPeekWindow(Timeout: cardinal = 0);
    procedure UpdatePeekWindow;
  public
    function WindowInList(hwnd: THandle): boolean;
    function IsEmpty: boolean;
    procedure UpdateTaskItem(hwnd: THandle);
    procedure RemoveNonExisting;
    procedure UpdateItem;

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make: string;
  end;

implementation
uses frmmainu;
//------------------------------------------------------------------------------
constructor TTaskItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FTaskGrouping := AParams.TaskGrouping;
  FTaskLivePreviews := AParams.TaskLivePreviews;
  FTaskThumbSize := AParams.TaskThumbSize;
  FProcName := '';
  FAppList := TFPList.Create;
  FIsOpen := false;
  FIsNew := true;
  OnBeforeMouseHover := BeforeMouseHover;
  OnMouseHover := MouseHover;
  OnBeforeUndock := BeforeUndock;
  SetTimer(FHWnd, ID_TIMER, 1000, nil);
end;
//------------------------------------------------------------------------------
destructor TTaskItem.Destroy;
begin
  FFreed := true;
  if FIsOpen then TAeroPeekWindow.Close(0);
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
  if FFreed or (not FTaskGrouping and (FAppList.Count > 0)) then exit;

  if FIsNew then
  begin
    FAppList.Add(pointer(hwnd));
    Attention(true);
    FProcName := ProcessHelper.GetWindowProcessName(hwnd);
    FIsExecutable := SameText(ExtractFileExt(FProcName), '.exe');
    UpdateItemInternal;
    FIsNew := false;
    exit;
  end;

  // try to get process executable path
  if FProcName = '' then
  begin
    FProcName := ProcessHelper.GetWindowProcessName(THandle(FAppList.First));
    FIsExecutable := SameText(ExtractFileExt(FProcName), '.exe');
	end;

  // if the window is already in this group - do nothing
  if FAppList.IndexOf(pointer(hwnd)) >= 0 then exit;

  // if the window belong to the same process - add the window to the list
  ProcName := ProcessHelper.GetWindowProcessName(hwnd);
  if ProcName <> '' then
    if ProcName = FProcName then
    begin
      FAppList.Add(pointer(hwnd));
      Attention(true);
      UpdateItemInternal;
    end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.RemoveNonExisting;
var
  index, oldCount: integer;
begin
  if FFreed then exit;

  if FAppList.Count > 0 then
  begin
    oldCount := FAppList.Count;
    for index := FAppList.Count - 1 downto 0 do
    begin
      if not IsWindowVisible(THandle(FAppList.Items[index])) then FAppList.Delete(index);
    end;
    if (FAppList.Count <> oldCount) and (FAppList.Count > 0) then UpdateItemInternal;
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateItem;
begin
  try
    if FAppList.Count = 1 then
      Caption := TProcessHelper.GetWindowText(THandle(FAppList.First));
    if not assigned(FImage) then UpdateImage;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal'#10#13 + e.message);
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
  if FAppList.Count > 0 then
    LoadAppImage(FProcName, THandle(FAppList.Items[0]), FBigItemSize, false, false, FImage, FIW, FIH, 500);
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
      gpTaskLivePreviews: FTaskLivePreviews := boolean(param);
      gpTaskThumbSize: FTaskThumbSize := param;
      gpTaskGrouping: FTaskGrouping := boolean(param);

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
  dst, brush: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
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

      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
      GdipDeleteBrush(brush);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

      xBitmap := ItemRect.Left;
      yBitmap := ItemRect.Top;

      // draw the button
      button := theme.DrawButton(dst, xBitmap, yBitmap, FSize, FAttention);
      FNCHitText := button;
    except
      on e: Exception do raise Exception.Create('InitDraw'#10#13 + e.message);
    end;

    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, nil, nil, nil);

    // draw windows count indicator
    DrawWindowsCount(dst, FAppList, xBitmap, yBitmap, FSize);

    ////
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
procedure TTaskItem.DrawWindowsCount(dst: pointer; winList: TFPList; x, y, Size: integer);
var
  brush, family, hfont, format, path: Pointer;
  tmpItemSize: integer;
  rect: GDIPAPI.TRectF;
begin
  if assigned(winList) then
      if winList.Count > 1 then
      begin
        GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
        GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
        // background
        tmpItemSize := max(FItemSize, 40);
        if winList.Count > 99 then rect.Width := tmpItemSize * 9 / 12
        else if winList.Count > 9 then rect.Width := tmpItemSize * 7 / 12
        else rect.Width := tmpItemSize * 5 / 12;
        rect.Height := tmpItemSize * 5 / 12;
        rect.X := x + Size - rect.Width + 5;
        rect.Y := y - 5;
        GdipCreatePath(FillModeWinding, path);
        AddPathRoundRect(path, rect, rect.Height / 2);
        GdipCreateSolidFill($ffff0000, brush); // red indicator background
        GdipFillPath(dst, brush, path);
        GdipDeleteBrush(brush);
        GdipDeletePath(path);
        // number
        GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, family);
        GdipCreateFont(family, tmpItemSize * 5 div 16, 1, 2, hfont);
        GdipCreateSolidFill($ffffffff, brush);
        GdipCreateStringFormat(0, 0, format);
        GdipSetStringFormatAlign(format, StringAlignmentCenter);
        GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
        GdipDrawString(dst, PWideChar(WideString(inttostr(winList.Count))), -1, hfont, @rect, format, brush);
        GdipDeleteStringFormat(format);
        GdipDeleteBrush(brush);
        GdipDeleteFont(hfont);
        GdipDeleteFontFamily(family);
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
  if button = mbLeft then
  begin
      if ssAlt in shift then Exec(eaGroup)
      else
      if ssCtrl in shift then Exec(eaRun)
      else
        Exec(eaDefault);
	end;

  if button = mbRight then
  begin
    ClosePeekWindow;
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;

  UpdateItemInternal; // update item icon and text //
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Exec(action: TExecuteAction);
begin
  if (action = eaRun) and FIsExecutable then
  begin
    dockh.DockExecute(FHWnd, pchar(FProcName), nil, nil, SW_SHOWNORMAL);
	end
  else
	if FAppList.Count = 1 then
  begin
      KillTimer(FHWnd, ID_TIMER_OPEN);
      ProcessHelper.ActivateWindow(THandle(FAppList.First));
  end
  else
  if FAppList.Count > 1 then
  begin
      if action = eaGroup then
          ProcessHelper.ActivateWindowList(FAppList)
		  else
          if not TAeroPeekWindow.IsActive then ShowPeekWindow;
	end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.BeforeUndock;
begin
  ClosePeekWindow;
end;
//------------------------------------------------------------------------------
function TTaskItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
  mii: MENUITEMINFO;
begin
  result := true;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING, $f005, pchar(UTF8ToAnsi(XPlaceTasksHere)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
  AppendMenu(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f003, pchar(UTF8ToAnsi(XKillProcess)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
  if FAppList.Count < 2 then AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XCloseWindow)))
  else AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XCloseAllWindows)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
  AppendMenu(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f004, pchar(UTF8ToAnsi(XRun)));
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f004, false, @mii);
  AppendMenu(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f002, pchar(UTF8ToAnsi(XPinToDock)));
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
      begin
        for idx := FAppList.Count - 1 downto 0 do
          ProcessHelper.CloseWindow(THandle(FAppList.Items[idx]));
      end else begin
        Delete;
      end;
    $f002:
        if FIsExecutable then
        begin
          dockh.DockAddProgram(pchar(FProcName));
          Delete;
        end;
    $f003: if FIsExecutable then ProcessHelper.Kill(FProcName);
    $f004: if FIsExecutable then dockh.DockExecute(FHWnd, pchar(FProcName), nil, nil, SW_SHOWNORMAL);
    $f005: dockh.DockExecute(FHWnd, '/taskspot', nil, nil, 0);
    $f006..$f020: ;
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Attention(value: boolean);
begin
  FAttention := value;
  if FAttention then SetTimer(FHWnd, ID_TIMER_ATTENTION, 5000, nil)
  else
  begin
    KillTimer(FHWnd, ID_TIMER_ATTENTION);
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.BeforeMouseHover(AHover: boolean);
begin
  FHideHint := TAeroPeekWindow.IsActive;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.MouseHover(AHover: boolean);
begin
  if FAttention then Attention(false);

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
  if not FFreed then
    with msg do
    begin
        Result := 0;

        // WM_ACTIVATEAPP
        if (msg = WM_ACTIVATEAPP) and (wParam = 0) then ClosePeekWindow;

        // WM_TIMER
        if msg = WM_TIMER then
        begin
          // "OPEN" TIMER
          if wParam = ID_TIMER_OPEN then ShowPeekWindow;
          // cancel Attention timer
          if wParam = ID_TIMER_ATTENTION then Attention(false);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Save(szIni: pchar; szIniGroup: pchar);
begin
  // not saveable
end;
//------------------------------------------------------------------------------
procedure TTaskItem.ShowPeekWindow(Timeout: cardinal = 0);
var
  pt: windows.TPoint;
begin
  try
    if FAppList.Count < 1 then exit;

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
      0: inc(pt.x, 5);
      1: inc(pt.y, 5);
      2: dec(pt.x, 5);
      3: dec(pt.y, 5);
    end;
    //LME(true);
    FHideHint := true;
    UpdateHint;
    TAeroPeekWindow.Open(FHWnd, FAppList, pt.x, pt.y, FSite, FTaskThumbSize, FTaskLivePreviews);
    FIsOpen := true;
  except
    on e: Exception do raise Exception.Create('TaskItem.Cmd'#10#13 + e.message);
  end;
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
    0: inc(pt.x, 5);
    1: inc(pt.y, 5);
    2: dec(pt.x, 5);
    3: dec(pt.y, 5);
  end;
  TAeroPeekWindow.SetPosition(pt.x, pt.y);
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
class function TTaskItem.Make: string;
begin
  result := 'class="task";';
end;
//------------------------------------------------------------------------------
end.

