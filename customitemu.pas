unit customitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math,
  declu, dockh, GDIPAPI, gdip_gfx;

const
  anim_bounce: array [0..15] of single = (0, 0.1670, 0.3290, 0.4680, 0.5956, 0.6937, 0.7790, 0.8453, 0.8984, 0.9360, 0.9630, 0.9810, 0.9920, 0.9976, 0.9997, 1);
  MIN_BORDER = 20;

type

  { TCustomItem }

  TCustomItem = class
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
    FShowHint: boolean; // global option
    FHideHint: boolean; // local option
    FMonitor: integer;
    FSite: integer;
    FHover: boolean;
    FLockDragging: boolean;
    FLockMouseEffect: boolean;
    FItemSize: integer;
    FBigItemSize: integer;
    FLaunchInterval: integer;
    FActivateRunning: boolean;
    MouseDownPoint: windows.TPoint;
    FMouseDownButton: TMouseButton;

    FFont: _FontData;
    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height
    FShowItem: uint;
    FAnimationType: integer; // animation type
    FAnimationEnd: integer;
    FAnimationProgress: integer; // animation progress 0..FAnimationEnd

    procedure Init; virtual;
    procedure Redraw(Force: boolean = true); // updates item appearance
    procedure SetCaption(value: string);
    procedure UpdateHint(Ax: integer = -32000; Ay: integer = -32000);
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

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); virtual;
    destructor Destroy; override;
    procedure SetFont(var Value: _FontData); virtual;
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
    procedure Timer; virtual;
    procedure Configure; virtual;
    function CanOpenFolder: boolean; virtual;
    procedure OpenFolder; virtual;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; virtual;
    procedure Save(szIni: pchar; szIniGroup: pchar); virtual; abstract;

    function HitTest(Ax, Ay: integer): boolean;
    function ScreenHitTest(Ax, Ay: integer): boolean;
    procedure Animate(AAnimationType: integer);
    procedure LME(lock: boolean);
    procedure Delete;

    class procedure CreateColorAttributes(ColorData: cardinal; Selected: boolean; out attr: Pointer);
  end;

procedure DrawDropIndicator(dst: Pointer; iType: integer; X, Y, Width, Height: integer);

implementation
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

  dockh.ExcludeFromPeek(FHWnd);
  SetWindowLong(FHWnd, GWL_USERDATA, cardinal(self));
  // change window proc
  FWndInstance := MakeObjectInstance(WindowProc);
  FPrevWndProc := Pointer(GetWindowLongPtr(FHWnd, GWL_WNDPROC));
  SetWindowLongPtr(FHWnd, GWL_WNDPROC, PtrInt(FWndInstance));

  FItemSize := AParams.ItemSize;
  FSize := FItemSize;
  FBigItemSize := AParams.BigItemSize;
  FLaunchInterval := AParams.LaunchInterval;
  FActivateRunning := AParams.ActivateRunning;
  FReflection := AParams.Reflection;
  FReflectionSize := AParams.ReflectionSize;
  FBorder := max(AParams.ReflectionSize, MIN_BORDER);
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
  CopyFontData(AParams.Font, FFont);
end;
//------------------------------------------------------------------------------
destructor TCustomItem.Destroy;
begin
  // restore window proc
  SetWindowLong(FHWnd, GWL_WNDPROC, PtrInt(FPrevWndProc));
  FreeObjectInstance(FWndInstance);
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
  Fx:= -32000;
  Fy:= -32000;
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
  FHideHint := false;
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
          Redraw;
        end;
      gpBigItemSize:
          FBigItemSize := word(param);
      gpReflection:
        begin
          FReflection := boolean(param);
          Redraw;
        end;
      gpReflectionSize:
        begin
          FReflectionSize := param;
          FBorder := min(max(FReflectionSize, MIN_BORDER), FItemSize);
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

      // commands //

      icSelect:
        if FSelected <> boolean(param) then
        begin
          FSelected := boolean(param);
          Redraw;
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
    on e: Exception do raise Exception.Create('CustomItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomItem.SetFont(var Value: _FontData);
begin
  CopyFontData(Value, FFont);
end;
//------------------------------------------------------------------------------
procedure TCustomItem.Redraw(Force: boolean = true);
begin
  Draw(Fx, Fy, FSize, Force, 0, FShowItem);
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
    Redraw(false);
    if FDockingProgress >= 1 then need_dock := false;
  end;
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
  if button = mbLeft then cmd(icFloat, 1); // undock
end;
//------------------------------------------------------------------------------
procedure TCustomItem.MouseHover(AHover: boolean);
begin
  FHover := AHover;
  if not FHover then KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
  UpdateHint;
end;
//------------------------------------------------------------------------------
function TCustomItem.DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean;
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
procedure TCustomItem.SetCaption(value: string);
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
      dockh.DeactivateHint(FHWnd);
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
    if FSite = 0 then hx := max(baserect.right, hx + FSize div 2 + hint_offset)
    else
    if FSite = 1 then hy := max(baserect.bottom, hy + FSize div 2 + hint_offset)
    else
    if FSite = 2 then hx := min(baserect.left, hx - FSize div 2 - hint_offset)
    else
      hy := min(baserect.top, hy - FSize div 2 - hint_offset);

    dockh.ActivateHint(FHWnd, PWideChar(WideString(FCaption)), hx, hy);
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
  result := GetClientRect;
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
procedure TCustomItem.WindowProc(var message: TMessage);
var
  idx: integer;
  ShiftState: classes.TShiftState;
  pos: windows.TSmallPoint;
  wpt: windows.TPoint;
  //
  filecount: integer;
  filename: array [0..MAX_PATH - 1] of char;
begin
  try
    WndMessage(message);
  except
    on e: Exception do raise Exception.Create('CustomItem.WindowProc.WndMessage'#10#13 + e.message);
  end;

  try
    with message do
    begin
        result := 0;
        pos := TSmallPoint(LParam);
        ShiftState := [];
        if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
        if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);

        if (msg >= wm_keyfirst) and (msg <= wm_keylast) then
        begin
          sendmessage(FHWndParent, msg, wParam, lParam);
          exit;
        end;

        if msg = wm_lbuttondown then
        begin
              MouseDownPoint.x:= pos.x;
              MouseDownPoint.y:= pos.y;
              if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, msg, wParam, lParam);
        end
        else if msg = wm_rbuttondown then
        begin
              MouseDownPoint.x:= pos.x;
              MouseDownPoint.y:= pos.y;
              if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, msg, wParam, lParam);
        end
        else if msg = wm_lbuttonup then
        begin
              cmd(icFloat, 0);
              if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y)
              else sendmessage(FHWndParent, msg, wParam, lParam);
        end
        else if msg = wm_rbuttonup then
        begin
              if not FFreed then
              begin
                if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y)
                else sendmessage(FHWndParent, msg, wParam, lParam);
              end;
        end
        else if msg = wm_lbuttondblclk then
        begin
              if not HitTest(pos.x, pos.y) then sendmessage(FHWndParent, msg, wParam, lParam)
              else
              if not DblClick(mbLeft, ShiftState, pos.x, pos.y) then sendmessage(FHWndParent, msg, wParam, lParam);
        end
        else if msg = wm_mousemove then
        begin
              // undock item (the only place to undock) //
              if (FCanDrag and not FLockMouseEffect and not FLockDragging and (wParam and MK_LBUTTON <> 0)) or FFloating then
              begin
                if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then
                begin
                  cmd(icFloat, 1);
                  dockh.Undock(FHWnd);
                end;
              end;
              // just in case - dock item //
              if FFloating and (wParam and MK_LBUTTON = 0) then
              begin
                cmd(icFloat, 0);
                dockh.Dock(FHWnd);
              end;
        end
        else if msg = wm_exitsizemove then
        begin
              // dock item (the only place to dock) //
              cmd(icFloat, 0);
              dockh.Dock(FHWnd);
        end
        else if msg = wm_command then
        begin
              WMCommand(message.wParam, message.lParam, message.Result);
        end
        else if msg = wm_timer then
        begin
              // mouse held //
              if wParam = ID_TIMER_MOUSEHELD then
              begin
                KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
                GetCursorPos(wpt);
                if WindowFromPoint(wpt) = FHWnd then MouseHeld(FMouseDownButton);
              end;
        end
        else if msg = wm_dropfiles then
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
        else if (msg = wm_close) or (msg = wm_quit) then exit;

        message.result := DefWindowProc(FHWnd, message.Msg, message.wParam, message.lParam);
    end;
  except
    on e: Exception do raise Exception.Create('CustomItem.WindowProc[ Msg=0x' + inttohex(message.msg, 8) + ' ]'#10#13 + e.message);
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
procedure DrawDropIndicator(dst: Pointer; iType: integer; X, Y, Width, Height: integer);
var
  brush, pen, path: pointer;
  rect: GDIPAPI.TRectF;
  cell, cell2, cell3, cell4: single;
  points: array [0..11] of GDIPAPI.TPointF;
begin
  if iType > 0 then
  try
    GdipCreateSolidFill($80ffffff, brush);
    GdipFillRectangle(dst, brush, X, Y, Width, Height);
    GdipDeleteBrush(brush);
    GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);

    if iType = 1 then // add
    begin
      GdipCreateSolidFill($ff303030, brush);
      GdipCreatePath(FillModeWinding, path);
      cell := Width * 0.2;
      cell2 := Width * 0.4;
      cell3 := Width * 0.6;
      cell4 := Width * 0.8;
      points[0].x := X + cell;
      points[0].y := Y + cell2;
      points[1].x := X + cell2;
      points[1].y := Y + cell2;
      points[2].x := X + cell2;
      points[2].y := Y + cell;
      points[3].x := X + cell3;
      points[3].y := Y + cell;
      points[4].x := X + cell3;
      points[4].y := Y + cell2;
      points[5].x := X + cell4;
      points[5].y := Y + cell2;
      points[6].x := X + cell4;
      points[6].y := Y + cell3;
      points[7].x := X + cell3;
      points[7].y := Y + cell3;
      points[8].x := X + cell3;
      points[8].y := Y + cell4;
      points[9].x := X + cell2;
      points[9].y := Y + cell4;
      points[10].x := X + cell2;
      points[10].y := Y + cell3;
      points[11].x := X + cell;
      points[11].y := Y + cell3;
      GdipAddPathClosedCurve2(path, points, 12, 0.1);
      GdipFillPath(dst, brush, path);
      GdipDeletePath(path);
      GdipDeleteBrush(brush);
    end
    else if iType = 2 then // run
    begin
      GdipCreateSolidFill($ff303030, brush);
      GdipCreatePath(FillModeWinding, path);
      points[0].x := X + Width * 0.2;
      points[0].y := Y + Height * 0.25;
      points[1].x := X + Width * 0.9;
      points[1].y := Y + Height * 0.25;
      points[2].x := X + Width * 0.8;
      points[2].y := Y + Height * 0.75;
      points[3].x := X + Width * 0.1;
      points[3].y := Y + Height * 0.75;
      GdipAddPathClosedCurve2(path, points, 4, 0.1);
      GdipFillPath(dst, brush, path);
      GdipDeletePath(path);
      GdipDeleteBrush(brush);

      GdipCreateSolidFill($ffe0e0e0, brush);
      GdipCreatePath(FillModeWinding, path);
      points[0].x := X + Width * (0.25 + 0.04);
      points[0].y := Y + Height * 0.35;
      points[1].x := X + Width * (0.72 + 0.04);
      points[1].y := Y + Height * 0.35;
      points[2].x := X + Width * 0.72;
      points[2].y := Y + Height * 0.55;
      points[3].x := X + Width * 0.25;
      points[3].y := Y + Height * 0.55;
      GdipAddPathClosedCurve2(path, points, 4, 0.1);
      GdipFillPath(dst, brush, path);
      GdipDeletePath(path);
      GdipDeleteBrush(brush);

      GdipCreatePen1($ff303030, 2, UnitPixel, pen);
      GdipDrawLine(dst, pen, X + Width * 0.33, Y + Height * 0.37, X + Width * 0.3, Y + Height * 0.53);
      GdipDeletePen(pen);
    end;

  except
    on e: Exception do raise Exception.Create('DrawDropIndicator'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.
 
