unit customitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, GDIPAPI, declu, gdip_gfx;

const
  anim_bounce: array [0..15] of single = (0, 0.1670, 0.3290, 0.4680, 0.5956, 0.6937, 0.7790, 0.8453, 0.8984, 0.9360, 0.9630, 0.9810, 0.9920, 0.9976, 0.9997, 1);
  MIN_BORDER = 20;

type TCustomItem = class
  protected
    FFreed: boolean;
    FHWnd: uint;
    FHWndParent: uint;
    FHMenu: cardinal;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    FCaption: string;
    Fx: integer;
    Fy: integer;
    FSize: integer;
    FBorder: integer;
    FxDockFrom: integer;
    FyDockFrom: integer;
    FxDocking: integer;
    FyDocking: integer;
    need_dock: boolean;
    FDockingProgress: single;
    FCanDrag: boolean;
    FDontSave: boolean;

    FEnabled: boolean;
    FUpdating: boolean;
    FFloating: boolean;
    FSelected: boolean;
    FDropIndicator: integer;
    FReflection: boolean;
    FReflectionSize: integer;
    FShowHint: boolean;
    FSite: integer;
    FHover: boolean;
    FLockDragging: boolean;
    FLockMouseEffect: boolean;
    FItemSize: integer;
    FBigItemSize: integer;
    FLaunchInterval: integer;
    FActivateRunningDefault: boolean;
    MouseDownPoint: windows.TPoint;

    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height
    FShowItem: uint;
    FAnimationType: integer; // animation type
    FAnimationEnd: integer;
    FAnimationProgress: integer; // animation progress 0..FAnimationEnd

    procedure Init; virtual;
    procedure SetCaption(value: string);
    procedure UpdateHint(Ax: integer = -1000; Ay: integer = -1000);
    function GetRectFromSize(ASize: integer): windows.TRect;
    function GetClientRect: windows.TRect;
    function GetScreenRect: windows.TRect;
    procedure WindowProc(var message: TMessage);
  public
    property Freed: boolean read FFreed write FFreed;
    property Floating: boolean read FFloating;
    property HWnd: uint read FHWnd;
    property Caption: string read FCaption write SetCaption;
    property X: integer read Fx;
    property Y: integer read Fy;
    property Size: integer read FSize;
    property Rect: windows.TRect read GetClientRect;
    property ScreenRect: windows.TRect read GetScreenRect;
    property DontSave: boolean read FDontSave;

    function HitTest(Ax, Ay: integer): boolean;
    function ScreenHitTest(Ax, Ay: integer): boolean;
    procedure Animate(AAnimationType: integer);

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); virtual;
    destructor Destroy; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); virtual; abstract;
    function ToString: string; virtual; abstract;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    procedure MouseHeld(button: TMouseButton); virtual;
    function DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure MouseHover(AHover: boolean); virtual;
    procedure WndMessage(var msg: TMessage); virtual; abstract;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); virtual; abstract;
    function cmd(id: TGParam; param: integer): integer; virtual;
    procedure LME(lock: boolean);
    procedure Timer; virtual;
    function CanOpenFolder: boolean; virtual; abstract;
    procedure OpenFolder; virtual; abstract;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; virtual;
    procedure Save(szIni: pchar; szIniGroup: pchar); virtual; abstract;
    procedure Delete; virtual;
    //
    class procedure CreateColorAttributes(ColorData: cardinal; Selected: boolean; out attr: Pointer);
end;


implementation
uses dockh, frmterryu;
//------------------------------------------------------------------------------
constructor TCustomItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited Create;
  Init;

  FHWndParent := AHWndParent;
  FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow, WINITEM_CLASS, nil, ws_popup, -32, -32, 32, 32, FHWndParent, 0, hInstance, nil);
  if not IsWindow(FHWnd) then
  begin
    FFreed := true;
    exit;
  end;

  SetWindowLong(FHWnd, GWL_USERDATA, cardinal(self));
  // change window proc
  FWndInstance := MakeObjectInstance(WindowProc);
  FPrevWndProc := Pointer(GetWindowLongPtr(FHWnd, GWL_WNDPROC));
  SetWindowLongPtr(FHWnd, GWL_WNDPROC, LongInt(FWndInstance));

  FItemSize := AParams.ItemSize;
  FSize := FItemSize;
  FBigItemSize := AParams.BigItemSize;
  FLaunchInterval := AParams.LaunchInterval;
  FActivateRunningDefault := AParams.ActivateRunning;
  FReflection := AParams.Reflection;
  FReflectionSize := AParams.ReflectionSize;
  FBorder := max(AParams.ReflectionSize, MIN_BORDER);
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
end;
//------------------------------------------------------------------------------
destructor TCustomItem.Destroy;
begin
  // restore window proc
  if assigned(FPrevWndProc) then SetWindowLong(FHWnd, GWL_WNDPROC, LongInt(FPrevWndProc));
  if IsWindow(FHWnd) then DestroyWindow(FHWnd);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Init;
begin
  FPrevWndProc := nil;
  FFreed := false;
  FEnabled := true;
  FCanDrag := true;
  FCaption := '';
  Fx:= -1000;
  Fy:= -1000;
  FSize:= 32;
  FCaption := '';
  FUpdating:= false;
  FFloating:= false;
  FSelected:= false;
  FDropIndicator:= 0;
  FReflection:= false;
  FReflectionSize:= 16;
  FBorder := FReflectionSize;
  FShowHint := true;
  FSite:= 3;
  FHover:= false;
  FLockMouseEffect:= false;
  FItemSize := 32;
  FBigItemSize := 32;
  FAnimationProgress := 0;
  FImage := nil;
  FIW := 32;
  FIH := 32;
  FShowItem := SWP_HIDEWINDOW;
  FxDocking := 0;
  FyDocking := 0;
  need_dock := false;
  FDontSave := false;
end;
//------------------------------------------------------------------------------
function TCustomItem.cmd(id: TGParam; param: integer): integer;
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
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;
      gpBigItemSize:
          FBigItemSize := word(param);
      gpReflection:
        begin
          FReflection := boolean(param);
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;
      gpReflectionSize:
        begin
          FReflectionSize := param;
          FBorder := min(max(FReflectionSize, MIN_BORDER), FItemSize);
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;
      gpSite:
        if param <> FSite then
        begin
          FSite := param;
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
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
      gpActivateRunning: FActivateRunningDefault := boolean(param);

      // commands //

      icSelect:
        if FSelected <> boolean(param) then
        begin
          FSelected := boolean(param);
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;

      icFloat:
        if (FFloating <> boolean(param)) and FCanDrag then
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
            FxDocking := FxDockFrom;
            FyDocking := FyDockFrom;
            FDockingProgress := 0;
          end;
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;

      icDropIndicator:
        if FDropIndicator <> param then
        begin
          FDropIndicator := param;
          Draw(Fx, Fy, FSize, true, 0, FShowItem);
        end;

      icHover: MouseHover(boolean(param));

      icFree: FFreed := param <> 0;
    end;

  except
    on e: Exception do raise Exception.Create('CustomItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.LME(lock: boolean);
begin
  dockh.DockletLockMouseEffect(FHWnd, lock);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Timer;
begin
  if FFreed or FUpdating then exit;
  // docking after item dropped onto dock //
  if need_dock then
  begin
    FDockingProgress += 0.05;
    FxDocking := FxDockFrom + round((Fx - FxDockFrom) * FDockingProgress);
    FyDocking := FyDockFrom + round((Fy - FyDockFrom) * FDockingProgress);
    if FDockingProgress >= 1 then need_dock := false;
    draw(Fx, Fy, FSize, false, 0, FShowItem);
  end;
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
    if button = mbLeft then
      if not FLockMouseEffect then SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 1200, nil);
    cmd(icSelect, 1);
  end;
end;
//------------------------------------------------------------------------------
function TCustomItem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  result := false;
  KillTimer(FHWnd, ID_TIMER_MOUSEHELD);

  if not FFreed then
  begin
    result := true;
    if FSelected then MouseClick(button, shift, x, y);
    try cmd(icSelect, 0);
    except end;
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseHeld(button: TMouseButton);
var
  pt: windows.TPoint;
begin
  if (button = mbLeft) and not FLockMouseEffect then
  begin
    GetCursorPos(pt);
    if WindowFromPoint(pt) = FHWnd then cmd(icFloat, 1);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseHover(AHover: boolean);
begin
  FHover := AHover;
  if not FHover then KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
  UpdateHint;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Animate(AAnimationType: integer);
begin
  FAnimationType := AAnimationType;
  case FAnimationType of
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
procedure TCustomItem.SetCaption(value: string);
begin
  if not (FCaption = value) then
  begin
    FCaption := value;
    UpdateHint;
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.UpdateHint(Ax: integer = -1000; Ay: integer = -1000);
var
  hx, hy: integer;
  wrect, baserect: windows.TRect;
  do_show: boolean;
begin
  try
    do_show := FShowHint and FHover and not FFloating and not FLockMouseEffect and (FCaption <> '');
    if not do_show then
    begin
      frmterry.DeactivateHint(FHWnd);
      exit;
    end;

    if (Ax <> -1000) and (Ay <> -1000) then
    begin
      wRect := Rect;
      hx := Ax + wRect.Left + integer(round(FSize / 2) and $ffffffff);
      hy := Ay + wRect.Top + integer(round(FSize / 2) and $ffffffff);
    end else begin
      wRect := ScreenRect;
      hx := wRect.left + integer(round(FSize / 2) and $ffffffff);
      hy := wRect.top + integer(round(FSize / 2) and $ffffffff);
    end;

    baserect := dockh.DockGetRect;
    if FSite = 0 then hx := max(baserect.right + 10, hx + integer(round(FSize / 2) and $ffffffff) + 10)
    else
    if FSite = 1 then hy := max(baserect.bottom + 10, hy + integer(round(FSize / 2) and $ffffffff) + 10)
    else
    if FSite = 2 then hx := min(baserect.left - 10, hx - integer(round(FSize / 2) and $ffffffff) - 10)
    else
      hy := min(baserect.top - 10, hy - integer(round(FSize / 2) and $ffffffff) - 10);

    frmterry.ActivateHint(FHWnd, FCaption, hx, hy, FSite);
  except
    on e: Exception do raise Exception.Create('TCustomItem.UpdateHint'#10#13 + e.message);
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
  result := GetRectFromSize(FSize);
  GetWindowRect(FHWnd, @r);
  inc(result.Left, r.Left);
  inc(result.Right, r.Left);
  inc(result.Top, r.Top);
  inc(result.Bottom, r.Top);
end;
//------------------------------------------------------------------------------
function TCustomItem.HitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetClientRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
function TCustomItem.ScreenHitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetScreenRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
function TCustomItem.DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Delete;
begin
  FFreed := true;
  ShowWindow(FHWnd, SW_HIDE);
  dockh.DockDeleteItem(FHWnd);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.WindowProc(var message: TMessage);
var
  i: integer;
  ShiftState: classes.TShiftState;
  pos: windows.TSmallPoint;
  wpt: windows.TPoint;
  //
  filecount: integer;
  filename: array [0..MAX_PATH - 1] of char;
begin
  with message do
  begin
    try
      result := 0;
      pos := TSmallPoint(LParam);
      ShiftState := [];
      if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
      if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);
    except
      on e: Exception do raise Exception.Create('TCustomItem.WindowProc.Pre'#10#13 + e.message);
    end;

    try WndMessage(message);
    except
      on e: Exception do raise Exception.Create('TCustomItem.WindowProc.WndMessage'#10#13 + e.message);
    end;

    if (msg >= wm_keyfirst) and (msg <= wm_keylast) then
    begin
      sendmessage(FHWndParent, msg, wParam, lParam);
      exit;
    end;

    if msg = wm_lbuttondown then
    begin
        try
          MouseDownPoint.x:= pos.x;
          MouseDownPoint.y:= pos.y;
          if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y)
          else sendmessage(FHWndParent, msg, wParam, lParam);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_lbuttondown'#10#13 + e.message);
        end;
    end
    else if msg = wm_rbuttondown then
    begin
        try
          MouseDownPoint.x:= pos.x;
          MouseDownPoint.y:= pos.y;
          if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y)
          else sendmessage(FHWndParent, msg, wParam, lParam);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_rbuttondown'#10#13 + e.message);
        end;
    end
    else if msg = wm_lbuttonup then
    begin
        try
          cmd(icFloat, 0);
          if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y)
          else sendmessage(FHWndParent, msg, wParam, lParam);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_lbuttonup'#10#13 + e.message);
        end;
    end
    else if msg = wm_rbuttonup then
    begin
        try
          if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y)
          else sendmessage(FHWndParent, msg, wParam, lParam);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_rbuttonup'#10#13 + e.message);
        end;
    end
    else if msg = wm_lbuttondblclk then
    begin
        try
          if not HitTest(pos.x, pos.y) then sendmessage(FHWndParent, msg, wParam, lParam)
          else
          if not DblClick(mbLeft, ShiftState, pos.x, pos.y) then sendmessage(FHWndParent, msg, wParam, lParam);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_lbuttondblclk'#10#13 + e.message);
        end;
    end
    else if msg = wm_mousemove then
    begin
        try
          // undock item (the only place in whole code to undock) //
          if (FCanDrag and not FLockMouseEffect and not FLockDragging and (wParam and MK_LBUTTON <> 0)) or FFloating then
          begin
            if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then
            begin
              cmd(icFloat, 1);
              dockh.Undock(FHWnd);
            end;
          end;
          // dock item //
          if FFloating and (wParam and MK_LBUTTON = 0) then
          begin
            cmd(icFloat, 0);
            dockh.Dock(FHWnd);
          end;
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_mousemove'#10#13 + e.message);
        end;
    end
    else if msg = wm_exitsizemove then
    begin
        // dock item //
        try
          cmd(icFloat, 0);
          dockh.Dock(FHWnd);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_exitsizemove'#10#13 + e.message);
        end;
    end
    else if msg = wm_command then
    begin
        try
          WMCommand(message.wParam, message.lParam, message.Result);
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_command'#10#13 + e.message);
        end;
    end
    else if msg = wm_timer then
    begin
        // mouse held //
        try
          if wParam = ID_TIMER_MOUSEHELD then
          begin
            KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
            MouseHeld(mbLeft);
          end;
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_timer.dragdrop'#10#13 + e.message);
        end;
    end
    else if msg = wm_dropfiles then
    begin
        try
          filecount := DragQueryFile(wParam, $ffffffff, nil, 0);
          GetCursorPos(wpt);
          i := 0;
          while i < filecount do
          begin
            windows.dragQueryFile(wParam, i, pchar(filename), MAX_PATH);
            if ScreenHitTest(wpt.x, wpt.y) then DropFile(FHWnd, wpt, pchar(filename));
            inc(i);
          end;
        except
          on e: Exception do raise Exception.Create('TCustomItem.WindowProc.wm_dropfiles'#10#13 + e.message);
        end;
    end
    else if (msg = wm_close) or (msg = wm_quit) then exit;

    message.result := DefWindowProc(FHWnd, message.Msg, message.wParam, message.lParam);
  end;
end;
//------------------------------------------------------------------------------
class procedure TCustomItem.CreateColorAttributes(ColorData: cardinal; Selected: boolean; out attr: Pointer);
var
  lMatrix, aMatrix: ColorMatrix;
  brightness, tmpColorData: integer;
begin
  try
    attr := nil;
    tmpColorData := ColorData;
    if Selected then
    begin
      brightness := max(byte(ColorData shr 16) - $10, 0);
      tmpColorData := (ColorData and $ff00ffff) + brightness shl 16;
    end;
    if Selected or (ColorData <> DEFAULT_COLOR_DATA) then
    begin
      CreateColorMatrix(tmpColorData, lMatrix);
      GdipCreateImageAttributes(attr);
      GdipSetImageAttributesColorMatrix(attr, ColorAdjustTypeBitmap, true, @lMatrix, nil, ColorMatrixFlagsDefault);
    end;
  except
    on e: Exception do raise Exception.Create('CustomItem.CreateColorAttributes'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.
 
