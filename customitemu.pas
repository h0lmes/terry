unit customitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, FileUtil,
  declu, dockh, gfx, toolu, loggeru;

const
  anim_bounce: array [0..15] of single = (0, 0.1670, 0.3290, 0.4680, 0.5956, 0.6937, 0.7790, 0.8453, 0.8984, 0.9360, 0.9630, 0.9810, 0.9920, 0.9976, 0.9997, 1);
  MIN_BORDER = 20;
  TDITEM_WCLASS = 'TDockItemWClass';

type

  TOnMouseHover = procedure(param: boolean) of object;
  TOnBeforeMouseHover = procedure(param: boolean) of object;
  TOnBeforeUndock = procedure of object;

  { TCustomItem is an abstract class }
  TCustomItem = class
  private
    FHover: boolean;
  protected
    FFreed: boolean;
    FHWnd: HWND;
    FHWndParent: HWND;
    FHMenu: THandle;
    FCaption: WideString;
    FColorData: integer;
    FX: integer;
    FY: integer;
    FSize: integer;
    FBorder: integer;
    FxDockFrom: integer;
    FyDockFrom: integer;
    FXDocking: integer;
    FYDocking: integer;
    need_dock: boolean;
    FDockingProgress: single;
    FNCHitTestNC: boolean; // if true - HitTest returns true for non-client area

    FEnabled: boolean;
    FUpdating: boolean;
    FFloating: boolean;
    FSelected: boolean;
    FDropIndicator: integer;
    FReflection: boolean;
    FReflectionSize: integer;
    FShowHint: boolean; // global option
    FHideHint: boolean; // local option
    FHintVisible: boolean; // is hint currently visible?
    FMonitor: integer;
    FSite: integer;
    FLockDragging: boolean;
    FLockMouseEffect: boolean;
    FItemSize: integer;
    FBigItemSize: integer;
    FItemSpacing: integer;
    FLaunchInterval: integer;
    FActivateRunning: boolean;
    MouseDownPoint: windows.TPoint;
    FMouseDownButton: TMouseButton;
    FNeedMouseWheel: boolean;
    FAttention: boolean;

    FFont: TDFontData;
    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height
    FShowItem: uint;
    FItemAnimationType: integer; // animation type
    FAnimationEnd: integer;
    FAnimationProgress: integer; // animation progress 0..FAnimationEnd

    OnMouseHover: TOnMouseHover;
    OnBeforeMouseHover: TOnBeforeMouseHover;
    OnBeforeUndock: TOnBeforeUndock;

    procedure RegisterWindowItemClass;
    procedure Init; virtual;
    procedure Redraw(Force: boolean = true); // updates item appearance
    procedure Attention(value: boolean);
    procedure SetCaption(value: WideString);
    procedure MouseHover(AHover: boolean);
    procedure UpdateHint(Ax: integer = -32000; Ay: integer = -32000);
    function GetRectFromSize(ASize: integer): windows.TRect;
    function ExpandRect(r: windows.TRect; value: integer): windows.TRect;
    function GetClientRect: windows.TRect;
    function GetScreenRect: windows.TRect;
    procedure notify(message: string);
    procedure err(where: string; e: Exception);
  public
    property Freed: boolean read FFreed write FFreed;
    property Floating: boolean read FFloating;
    property Handle: HWND read FHWnd;
    property Caption: WideString read FCaption write SetCaption;
    property ColorData: integer read FColorData write FColorData;
    property X: integer read FX;
    property Y: integer read FY;
    property Size: integer read FSize;
    property Rect: windows.TRect read GetClientRect;
    property ScreenRect: windows.TRect read GetScreenRect;

    constructor Create(wndParent: HWND; var AParams: TDItemCreateParams); virtual;
    destructor Destroy; override;
    procedure SetFont(var Value: TDFontData); virtual;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi: HDWP; AShowItem: uint); virtual; abstract;
    function ToString: string; virtual; abstract;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    procedure MouseHeld(button: TMouseButton); virtual;
    function DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure WndMessage(var msg: TMessage); virtual; abstract;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); virtual; abstract;
    function cmd(id: TDParam; param: PtrInt): PtrInt; virtual;
    procedure Timer; virtual;
    procedure Configure; virtual;
    function CanOpenFolder: boolean; virtual;
    procedure OpenFolder; virtual;
    function RegisterProgram: string; virtual;
    function DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean; virtual;
    procedure Save(ini, section: string); virtual; abstract;

    function HitTest(Ax, Ay: integer): boolean;
    function ScreenHitTest(Ax, Ay: integer): boolean;
    procedure Animate;
    procedure LME(lock: boolean);
    procedure Delete;
    function WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
  end;

implementation
//------------------------------------------------------------------------------
function CustomItemClassWindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  inst: TCustomItem;
begin
  inst := TCustomItem(GetWindowLongPtr(wnd, GWL_USERDATA));
  if assigned(inst) then
    result := inst.WindowProc(wnd, message, wParam, lParam)
  else
    result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
constructor TCustomItem.Create(wndParent: HWND; var AParams: TDItemCreateParams);
begin
  inherited Create;
  Init;

  FHWndParent := wndParent;
  RegisterWindowItemClass;
  FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow, TDITEM_WCLASS, nil, ws_popup, FX, FY, FSize, FSize, FHWndParent, 0, hInstance, nil);
  if not IsWindow(FHWnd) then
  begin
    FFreed := true;
    exit;
  end;
  dockh.ExcludeFromPeek(FHWnd);
  SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUint(self));

  FItemSize := AParams.ItemSize;
  FSize := FItemSize;
  FBigItemSize := AParams.BigItemSize;
  FItemSpacing := AParams.ItemSpacing;
  FItemAnimationType := AParams.AnimationType;
  FLaunchInterval := AParams.LaunchInterval;
  FActivateRunning := AParams.ActivateRunning;
  FReflection := AParams.Reflection;
  FReflectionSize := AParams.ReflectionSize;
  FBorder := max(FReflectionSize, MIN_BORDER);
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
  CopyFontData(AParams.Font, FFont);
end;
//------------------------------------------------------------------------------
destructor TCustomItem.Destroy;
begin
  SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUint(0));
  if IsWindow(FHWnd) then DestroyWindow(FHWnd);
  FHWnd := 0;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.RegisterWindowItemClass;
var
  wndClass: windows.TWndClass;
begin
  try
    wndClass.style          := CS_DBLCLKS;
    wndClass.lpfnWndProc    := @CustomItemClassWindowProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := TDITEM_WCLASS;
    windows.RegisterClass(wndClass);
  except
    on e: Exception do err('CustomItem.RegisterWindowClass', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Init;
begin
  FFreed := false;
  FEnabled := true;
  FCaption := '';
  FX := -3000;
  FY := -3000;
  FSize := 32;
  FCaption := '';
  FUpdating := false;
  FFloating := false;
  FSelected := false;
  FColorData := DEF_COLOR_DATA;
  FDropIndicator := 0;
  FReflection := false;
  FReflectionSize := 16;
  FBorder := FReflectionSize;
  FShowHint := true;
  FHideHint := false;
  FHintVisible := false;
  FAttention := false;
  FSite := 3;
  FHover := false;
  FLockMouseEffect := false;
  FItemSize := 32;
  FBigItemSize := 32;
  FAnimationProgress := 0;
  FImage := nil;
  FIW := 32;
  FIH := 32;
  FShowItem := SWP_HIDEWINDOW;
  FXDocking := 0;
  FYDocking := 0;
  need_dock := false;
  FNCHitTestNC := false;
  FNeedMouseWheel := false;
end;
//------------------------------------------------------------------------------
function TCustomItem.cmd(id: TDParam; param: PtrInt): PtrInt;
var
  wRect: windows.TRect;
begin
  result:= 0;
  try
    case id of
      // parameters //
      gpItemSize:
        begin
          FItemSize := param;
          Redraw;
        end;
      gpBigItemSize: FBigItemSize := word(param);
      gpItemSpacing:
        begin
          FItemSpacing := word(param);
          Redraw;
        end;
      gpReflectionEnabled:
        begin
          FReflection := boolean(param);
          Redraw;
        end;
      gpReflectionSize:
        begin
          FReflectionSize := min(param, FItemSize);
          FBorder := max(FReflectionSize, MIN_BORDER);
          Redraw;
        end;
      gpMonitor: FMonitor := param;
      gpSite:
        if param <> FSite then
        begin
          FSite := param;
          Redraw;
        end;
      gpLockMouseEffect:
        begin
          FLockMouseEffect := param <> 0;
          UpdateHint;
        end;
      gpShowHint:
        begin
          FShowHint := boolean(param);
          UpdateHint;
        end;
      gpLockDragging: FLockDragging := param <> 0;
      gpLaunchInterval: FLaunchInterval := param;
      gpActivateRunning: FActivateRunning := boolean(param);
      gpItemAnimationType: FItemAnimationType := param;

      // commands //

      icSelect:
        if FSelected <> boolean(param) then
        begin
          FSelected := boolean(param);
          Redraw;
        end;

      icUndock:
        if FFloating <> boolean(param) then
        begin
          FFloating := boolean(param);
          if FFloating then
          begin
            FHover := false;
            FSelected := false;
          end;
          need_dock := not FFloating;
          if need_dock then
          begin
            wRect := ScreenRect;
            FxDockFrom := wRect.Left;
            FyDockFrom := wRect.Top;
            FXDocking := FxDockFrom;
            FYDocking := FyDockFrom;
            FDockingProgress := 0;
          end;
          Redraw;
        end;

      icDropIndicator:
        if FDropIndicator <> param then
        begin
          FDropIndicator := param;
          Redraw;
        end;

      icHover:
        begin
          if param = 0 then cmd(icSelect, 0);
          MouseHover(boolean(param));
        end;

      icFree: FFreed := param <> 0;
    end;

  except
    on e: Exception do raise Exception.Create('CustomItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.SetFont(var Value: TDFontData);
begin
  CopyFontData(Value, FFont);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Redraw(Force: boolean = true);
begin
  Draw(FX, FY, FSize, Force, 0, FShowItem);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Timer;
begin
  if FFreed or FUpdating then exit;
  // docking after item dropped onto dock //
  if need_dock then
  begin
    FDockingProgress += 0.05;
    FXDocking := FxDockFrom + round((FX - FxDockFrom) * FDockingProgress);
    FYDocking := FyDockFrom + round((FY - FyDockFrom) * FDockingProgress);
    Redraw(false);
    if FDockingProgress >= 1 then need_dock := false;
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Attention(value: boolean);
begin
  FAttention := value;
  if not FAttention then Redraw;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Configure;
begin
end;
//------------------------------------------------------------------------------
function TCustomItem.DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  result := true;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
  if not FFreed then
  begin
    FMouseDownButton := button;
    if button = mbLeft then SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 1000, nil)
    else SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 800, nil);
    cmd(icSelect, 1);
  end;
end;
//------------------------------------------------------------------------------
function TCustomItem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  result := not FFreed;
  KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
  if not FFreed and FSelected then
  begin
    cmd(icSelect, 0);
    MouseClick(button, shift, x, y);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseHeld(button: TMouseButton);
begin
  cmd(icSelect, 0);
  if button = mbLeft then
  begin
    if assigned(OnBeforeUndock) then OnBeforeUndock;
    cmd(icUndock, 1); // undock
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseHover(AHover: boolean);
begin
  if not FFreed and not (AHover = FHover) then
  begin
    if assigned(OnBeforeMouseHover) then OnBeforeMouseHover(AHover);
    FHover := AHover;
    if FAttention then Attention(false);
    if not FHover then KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
    UpdateHint;
    if assigned(OnMouseHover) then OnMouseHover(FHover);
  end;
end;
//------------------------------------------------------------------------------
function TCustomItem.DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
function TCustomItem.CanOpenFolder: boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.OpenFolder;
begin
end;
//------------------------------------------------------------------------------
function TCustomItem.RegisterProgram: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
procedure TCustomItem.SetCaption(value: WideString);
begin
  if not (FCaption = value) then
  begin
    FCaption := value;
    UpdateHint;
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.UpdateHint(Ax: integer = -32000; Ay: integer = -32000);
var
  hx, hy: integer;
  wrect, baserect: windows.TRect;
  do_show: boolean;
  hint_offset: integer;
begin
  if not FFreed then
  try
    do_show := FShowHint and FHover and not FHideHint and not FFloating and not FLockMouseEffect and (trim(FCaption) <> '');
    if not do_show then
    begin
      if FHintVisible then dockh.DeactivateHint(FHWnd);
      FHintVisible := false;
      exit;
    end;

    if (Ax <> -32000) and (Ay <> -32000) then
    begin
      wRect := Rect;
      hx := Ax + wRect.Left + FSize div 2;
      hy := Ay + wRect.Top + FSize div 2;
    end else begin
      wRect := ScreenRect;
      hx := wRect.left + FSize div 2;
      hy := wRect.top + FSize div 2;
    end;

    hint_offset := 10;
    baserect := dockh.DockGetRect;
    if FSite = 0 then hx := max(baserect.right,  hx + FSize div 2 + hint_offset)
    else
    if FSite = 1 then hy := max(baserect.bottom, hy + FSize div 2 + hint_offset)
    else
    if FSite = 2 then hx := min(baserect.left,   hx - FSize div 2 - hint_offset)
    else
                      hy := min(baserect.top,    hy - FSize div 2 - hint_offset);

    FHintVisible := true;
    dockh.ActivateHint(FHWnd, PWideChar(FCaption), hx, hy);
  except
    on e: Exception do raise Exception.Create('TCustomItem.UpdateHint ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TCustomItem.GetRectFromSize(ASize: integer): windows.TRect;
begin
  result := classes.rect(FBorder, FBorder, FBorder + ASize, FBorder + ASize);
end;
//------------------------------------------------------------------------------
// item rect in client coordinates
function TCustomItem.GetClientRect: windows.TRect;
begin
  result := GetRectFromSize(FSize);
end;
//------------------------------------------------------------------------------
// item rect in screen coordinates
function TCustomItem.GetScreenRect: windows.TRect;
var
  r: windows.TRect;
begin
  result := GetClientRect;
  GetWindowRect(FHWnd, @r);
  inc(result.Left, r.Left);
  inc(result.Right, r.Left);
  inc(result.Top, r.Top);
  inc(result.Bottom, r.Top);
end;
//------------------------------------------------------------------------------
// item rect in screen coordinates
function TCustomItem.ExpandRect(r: windows.TRect; value: integer): windows.TRect;
begin
  result := r;
  dec(result.Left, value);
  dec(result.Top, value);
  inc(result.Right, value);
  inc(result.Bottom, value);
end;
//------------------------------------------------------------------------------
function TCustomItem.HitTest(Ax, Ay: integer): boolean;
begin
  if FNCHitTestNC then
  begin
    result := true;
    exit;
  end;
  result := ptinrect(GetClientRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
function TCustomItem.ScreenHitTest(Ax, Ay: integer): boolean;
begin
  if FNCHitTestNC then
  begin
    result := true;
    exit;
  end;
  result := ptinrect(GetScreenRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Animate;
begin
  case FItemAnimationType of
    1: FAnimationEnd := 60; // rotate
    2: FAnimationEnd := 30; // bounce 1
    3: FAnimationEnd := 60; // bounce 2
    4: FAnimationEnd := 90; // bounce 3
    5: FAnimationEnd := 60; // quake
    6: FAnimationEnd := 56; // swing
    7: FAnimationEnd := 56; // vibrate
    8: FAnimationEnd := 56; // zoom
  end;
  FAnimationProgress := 1;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.LME(lock: boolean);
begin
  dockh.DockletLockMouseEffect(FHWnd, lock);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Delete;
begin
  FFreed := true;
  ShowWindow(FHWnd, SW_HIDE);
  dockh.DockDeleteItem(FHWnd);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.notify(message: string);
begin
  dockh.notify(FHWnd, pchar(message));
end;
//------------------------------------------------------------------------------
function TCustomItem.WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
const
  MK_ALT = $1000;
var
  idx: integer;
  ShiftState: classes.TShiftState;
  pos: windows.TSmallPoint;
  wpt: windows.TPoint;
  msg: TMessage;
  //
  filecount: integer;
  filename: array [0..MAX_PATH - 1] of char;
begin
  if not FFreed then
  try
    msg.msg := message;
    msg.wParam := wParam;
    msg.lParam := lParam;
    WndMessage(msg);
    result := msg.Result;
  except
    on e: Exception do err('CustomItem.WindowProc.WndMessage', e);
  end;

  try
    if not FFreed then
    begin
        result := 0;
        pos := TSmallPoint(dword(lParam));
        ShiftState := [];
        if HIBYTE(GetKeyState(VK_MENU)) and $80 <> 0 then Include(ShiftState, ssAlt);
        if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
        if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);

        if (message >= wm_keyfirst) and (message <= wm_keylast) then
        begin
          result := sendmessage(FHWndParent, message, wParam, lParam);
          exit;
        end;

        if message = wm_lbuttondown then
        begin
              MouseDownPoint.x:= pos.x;
              MouseDownPoint.y:= pos.y;
              if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, message, wParam, lParam);
        end
        else if message = wm_rbuttondown then
        begin
              MouseDownPoint.x:= pos.x;
              MouseDownPoint.y:= pos.y;
              if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, message, wParam, lParam);
        end
        else if message = wm_lbuttonup then
        begin
              cmd(icUndock, 0);
              if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, message, wParam, lParam);
        end
        else if message = wm_rbuttonup then
        begin
              if not FFreed then
              begin
                if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y)
                else sendmessage(FHWndParent, message, wParam, lParam);
              end;
        end
        else if message = wm_lbuttondblclk then
        begin
              if not HitTest(pos.x, pos.y) then sendmessage(FHWndParent, message, wParam, lParam)
              else
              if not DblClick(mbLeft, ShiftState, pos.x, pos.y) then sendmessage(FHWndParent, message, wParam, lParam);
        end
        else if message = wm_mousewheel then
        begin
              if not FNeedMouseWheel then sendmessage(FHWndParent, message, wParam, lParam);
        end
        else if message = wm_mousemove then
        begin
              // undock item (the only place to undock) //
              if (not FLockMouseEffect and not FLockDragging and (wParam and MK_LBUTTON <> 0)) or FFloating then
              begin
                if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then
                begin
                  cmd(icUndock, 1);
                  dockh.Undock(FHWnd);
                  SetWindowPos(FHWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
                  ReleaseCapture;
                  DefWindowProc(FHWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0);
                end;
              end;
              // just in case - dock item //
              if FFloating and (wParam and MK_LBUTTON = 0) then
              begin
                cmd(icUndock, 0);
                dockh.Dock(FHWnd);
              end;
        end
        else if message = wm_exitsizemove then
        begin
              // dock item (the only place to dock) //
              cmd(icUndock, 0);
              dockh.Dock(FHWnd);
        end
        else if message = wm_command then
        begin
              WMCommand(wParam, lParam, Result);
        end
        else if message = wm_timer then
        begin
              // mouse held //
              if wParam = ID_TIMER_MOUSEHELD then
              begin
                KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
                GetCursorPos(wpt);
                if WindowFromPoint(wpt) = FHWnd then MouseHeld(FMouseDownButton);
              end;
        end
        else if message = wm_dropfiles then
        begin
              filecount := DragQueryFile(wParam, $ffffffff, nil, 0);
              GetCursorPos(wpt);
              idx := 0;
              while idx < filecount do
              begin
                windows.dragQueryFile(wParam, idx, pchar(filename), MAX_PATH);
                if ScreenHitTest(wpt.x, wpt.y) then DropFile(FHWnd, wpt, pchar(filename));
                inc(idx);
              end;
        end
        else if (message = wm_close) or (message = wm_quit) then exit;

    end; // end if not FFreed
    if FHWnd <> 0 then
      result := DefWindowProc(FHWnd, message, wParam, lParam);
  except
    on e: Exception do err('CustomItem.WindowProc[ Msg=0x' + inttohex(message, 8) + ' ]', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + LineEnding + e.message);
    notify(where + LineEnding + e.message);
  end else begin
    AddLog(where);
    messagebox(FHWnd, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.
 
