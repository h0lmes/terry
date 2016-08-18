unit taskitemu;

{$t+}

interface
uses jwaWindows, Windows, Messages, SysUtils, Controls, Classes, Math,
  GDIPAPI, gfx, declu, dockh, toolu, customdrawitemu, processhlp, aeropeeku;

type

  { TTaskItem }

  TTaskItem = class(TCustomDrawItem)
  private
    FIsExecutable: boolean;
    FExecutable: string;
    FAppList: TFPList;
    FIsNew: boolean;
    FIsOpen: boolean;
    FTaskLivePreviews: boolean;
    FTaskThumbSize: integer;
    FTaskGrouping: boolean;
    procedure BeforeUndock;
    procedure UpdateImage;
    procedure UpdateItemInternal;
    procedure AfterDraw;
    procedure DrawOverlay(dst: pointer; x, y, size: integer);
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

    constructor Create(AData: string; wndParent: HWND; AParams: TDItemCreateParams); override;
    destructor Destroy; override;
    function cmd(id: TDParam; param: PtrInt): PtrInt; override;
    procedure Timer; override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    procedure Save(ini, section: string); override;
    //
    class function Make: string;
  end;

implementation
//------------------------------------------------------------------------------
constructor TTaskItem.Create(AData: string; wndParent: HWND; AParams: TDItemCreateParams);
begin
  inherited;
  FTaskGrouping := AParams.TaskGrouping;
  FTaskLivePreviews := AParams.TaskLivePreviews;
  FTaskThumbSize := AParams.TaskThumbSize;
  FExecutable := '';
  FAppList := TFPList.Create;
  FIsOpen := false;
  FIsNew := true;
  FRunning := true;
  OnBeforeMouseHover := BeforeMouseHover;
  OnMouseHover := MouseHover;
  OnBeforeUndock := BeforeUndock;
  OnAfterDraw := AfterDraw;
  OnDrawOverlay := DrawOverlay;
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
    FExecutable := ProcessHelper.GetWindowProcessName(hwnd);
    FIsExecutable := SameText(ExtractFileExt(FExecutable), '.exe');
    UpdateItemInternal;
    FIsNew := false;
    exit;
  end;

  // try to get process executable path
  if FExecutable = '' then
  begin
    FExecutable := ProcessHelper.GetWindowProcessName(THandle(FAppList.First));
    FIsExecutable := SameText(ExtractFileExt(FExecutable), '.exe');
	end;

  // if the window is already in this group - do nothing
  if FAppList.IndexOf(pointer(hwnd)) >= 0 then exit;

  // if the window belong to the same process - add the window to the list
  ProcName := ProcessHelper.GetWindowProcessName(hwnd);
  if ProcName <> '' then
    if ProcName = FExecutable then
    begin
      FAppList.Add(pointer(hwnd));
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
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal ' + LineEnding + e.message);
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
      hwnd := THandle(FAppList.First);
      if FAppList.Count = 1 then Caption := TProcessHelper.GetWindowText(hwnd) else Caption := '';
    finally
      FUpdating:= false;
    end;

    if FIsOpen then ShowPeekWindow; // update peek window
    Redraw;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateImage;
begin
  if FAppList.Count > 0 then
    LoadAppImage(FExecutable, THandle(FAppList.Items[0]), FBigItemSize, false, false, FImage, FIW, FIH, 500);
end;
//------------------------------------------------------------------------------
function TTaskItem.cmd(id: TDParam; param: PtrInt): PtrInt;
var
  temp: uint;
  idx: integer;
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
          if temp <> FIW then UpdateImage;
        end;
      gpMonitor: ClosePeekWindow(0);
      gpSite: ClosePeekWindow(0);
      gpTaskLivePreviews: FTaskLivePreviews := boolean(param);
      gpTaskThumbSize: FTaskThumbSize := param;
      gpTaskGrouping: FTaskGrouping := boolean(param);
      tcDebugInfo:
        if FAppList.Count > 0 then // log every window info
        begin
          AddLog('---------- TaskItem.WindowsInfo');
          AddLog('Caption = ' + FCaption);
          AddLog('Process = ' + FExecutable);
          for idx := 0 to FAppList.Count - 1 do LogWindow(THandle(FAppList.Items[idx]));
        end;

      // commands //

      icFlashTaskWindow:
        if FAppList.IndexOf(pointer(param)) >= 0 then
        begin
          Animate;
          Attention(true);
        end;

      icIsItem: result := 0;
    end;

  except
    on e: Exception do raise Exception.Create('TaskItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
// Draw routines ---------------------------------------------------------------
procedure TTaskItem.AfterDraw;
begin
  if FIsOpen then UpdatePeekWindow;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.DrawOverlay(dst: pointer; x, y, size: integer);
begin
  if assigned(FAppList) then
    if FAppList.Count > 1 then DrawNumberOverlay(dst, x, y, size, FAppList.Count);
end;
// Draw routines ---------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TTaskItem.Timer;
begin
  try
    inherited;
    if FFreed or FUpdating then exit;

    // animation //
    if FAnimationProgress > 0 then
    begin
      inc(FAnimationProgress);
      if FAnimationProgress >= FAnimationEnd then FAnimationProgress := 0;
      Redraw;
    end;
  except
    on e: Exception do raise Exception.Create('TTaskItem.Timer ' + LineEnding + e.message);
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
    dockh.DockExecute(FHWnd, pchar(FExecutable), nil, nil, SW_SHOWNORMAL);
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
  AppendMenuW(FHMenu, MF_STRING, $f005, pwchar(UTF8Decode(XPlaceTasksHere)));
  AppendMenuW(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f002, pwchar(UTF8Decode(XPinToDock)));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f003, pwchar(UTF8Decode(XKillProcess)));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  if FAppList.Count < 2 then AppendMenuW(FHMenu, MF_STRING, $f007, pwchar(UTF8Decode(XCloseWindow)))
  else begin
    AppendMenuW(FHMenu, MF_STRING, $f007, pwchar(UTF8Decode(XCloseAllWindows)));
    AppendMenuW(FHMenu, MF_STRING, $f009, pwchar(UTF8Decode(XMinimizeRestoreAllWindows)));
  end;
  AppendMenuW(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f004, pwchar(UTF8Decode(XRun)));
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f004, false, @mii);
  LME(true);

  msg.WParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
var
  idx: integer;
begin
  result := 0;
  LME(false);
  DestroyMenu(FHMenu);
  FHMenu := 0;
  case wParam of // f001 to f020
    $f002:
        if FIsExecutable then
        begin
          dockh.DockAddProgram(pchar(FExecutable));
          Delete;
        end;
    $f003: if FIsExecutable then ProcessHelper.Kill(FExecutable);
    $f004: if FIsExecutable then dockh.DockExecute(FHWnd, pchar(FExecutable), nil, nil, SW_SHOWNORMAL);
    $f005: dockh.DockExecute(FHWnd, '/taskspot', nil, nil, 0);
    $f007:
      if FAppList.Count > 0 then
      begin
        for idx := FAppList.Count - 1 downto 0 do
          ProcessHelper.CloseWindow(THandle(FAppList.Items[idx]));
      end else begin
        Delete;
      end;
    $f009: ProcessHelper.ActivateWindowList(FAppList);
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
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Save(ini, section: string);
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
    FHideHint := true;
    UpdateHint;
    TAeroPeekWindow.Open(FHWnd, FAppList, pt.x, pt.y, FSite, FTaskThumbSize, FTaskLivePreviews);
    FIsOpen := true;
  except
    on e: Exception do raise Exception.Create('TaskItem.Cmd ' + LineEnding + e.message);
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

