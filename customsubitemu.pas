unit customsubitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, Math, ComObj, ShlObj,
  GDIPAPI, gfx, PIDL, declu, dockh, toolu, customitemu, loggeru;

const
  MIN_BORDER = 8;
  BGMARGIN = 3;
  MAX_CAPTION_WIDTH = 150;
  TDSUBITEM_WCLASS = 'TDockSubItemWClass';

type

  { TCustomSubitem is an abstract class }

  TCustomSubitem = class
  protected
    FFreed: boolean;
    FHWnd: HWND;
    FHWndParent: HWND;
    FHMenu: THandle;
    FCaption: WideString;
    FCaptionWidth: integer;
    FCaptionHeight: integer;
    Fx: integer;
    Fy: integer;
    FSize: integer;
    FAngle: single;
    FAlpha: integer;
    FHintAlign: integer;
    FHintAlpha: integer;
    FBackground: boolean;
    FQueryDelete: boolean;
    FFont: TDFontData;
    FIsExecutable: boolean;
    FExecutable: WideString;

    FEnabled: boolean;
    FUpdating: boolean;
    FSelected: boolean; // when mouse button is down - icon becomes darken //
    FRunning: boolean;
    FProcessWindowsCount: integer;
    FShowHint: boolean;
    FSite: integer;
    FItemSize: integer;
    FLaunchInterval: integer;
    FActivateRunning: boolean;
    MouseDownPoint: windows.TPoint;
    FMouseDownButton: TMouseButton;
    FLockDragging: boolean;

    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height

    procedure RegisterWindowItemClass;
    procedure Init; virtual;
    procedure Redraw;
    function GetNCRect: windows.TRect;
    function GetClientRect: windows.TRect;
    function GetScreenRect: windows.TRect;
    procedure CloseStack;
    procedure UpdateCaptionExtent;
    procedure err(where: string; e: Exception);
  public
    property Freed: boolean read FFreed write FFreed;
    property Handle: HWND read FHWnd;
    property Caption: WideString read FCaption write FCaption;
    property X: integer read Fx;
    property Y: integer read Fy;
    property Size: integer read FSize;
    property Rect: windows.TRect read GetClientRect;
    property ScreenRect: windows.TRect read GetScreenRect;
    property Running: boolean read FRunning;
    property WindowCount: integer read FProcessWindowsCount;
    property QueryDelete: boolean read FQueryDelete;
    property Image: pointer read FImage;
    property ImageW: uint read FIW;
    property ImageH: uint read FIH;

    function HitTest(Ax, Ay: integer): boolean; virtual;
    function ScreenHitTest(Ax, Ay: integer): boolean;
    procedure SetFont(var Value: TDFontData);

    constructor Create(wndParent: HWND; AParams: TDItemCreateParams); virtual;
    destructor Destroy; override;
    procedure FromString(data: string); virtual; abstract;
    procedure Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; ABackground, AForce: boolean); virtual; abstract;
    function Measure(ASize: integer; AAngle: single; AHintAlign: integer): windows.TRect; virtual;
    procedure DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer); virtual; abstract;
    function ToString: string; virtual;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure MouseHeld(button: TMouseButton); virtual;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var result: LRESULT); virtual; abstract;
    procedure Configure; virtual; abstract;
    function cmd(id: TDParam; param: PtrInt): PtrInt; virtual;
    function CanOpenFolder: boolean; virtual; abstract;
    procedure OpenFolder; virtual; abstract;
    function DropFile(pt: windows.TPoint; filename: string): boolean; virtual; abstract;
    procedure Delete(AllowUndo: boolean = true); virtual;
    procedure BeginDrag; virtual;
    function WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
  end;


implementation
//------------------------------------------------------------------------------
function CustomSubitemClassWindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  inst: TCustomSubitem;
begin
  inst := TCustomSubitem(GetWindowLongPtr(wnd, GWL_USERDATA));
  if assigned(inst) then
    result := inst.WindowProc(wnd, message, wParam, lParam)
  else
    result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
constructor TCustomSubitem.Create(wndParent: HWND; AParams: TDItemCreateParams);
begin
  inherited Create;
  Init;

  FHWndParent := wndParent;
  RegisterWindowItemClass;
  FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow + ws_ex_acceptfiles, TDSUBITEM_WCLASS, nil, ws_popup, -1000, -1000, 32, 32, FHWndParent, 0, hInstance, nil);
  if not IsWindow(FHWnd) then
  begin
    FFreed := true;
    exit;
  end;
  SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUInt(self));

  FItemSize := AParams.ItemSize;
  FLaunchInterval := AParams.LaunchInterval;
  FActivateRunning := AParams.ActivateRunning;
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
  CopyFontData(AParams.Font, FFont);
end;
//------------------------------------------------------------------------------
destructor TCustomSubitem.Destroy;
begin
  SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUInt(0));
  if IsWindow(FHWnd) then DestroyWindow(FHWnd);
  FHWnd := 0;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.RegisterWindowItemClass;
var
  wndClass: windows.TWndClass;
begin
  try
    wndClass.style          := CS_DBLCLKS;
    wndClass.lpfnWndProc    := @CustomSubitemClassWindowProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := TDSUBITEM_WCLASS;
    windows.RegisterClass(wndClass);
  except
    on e: Exception do err('CustomSubitem.RegisterWindowClass', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.Init;
begin
  FFreed:= false;
  FQueryDelete := false;
  FEnabled:= true;
  FHWnd:= 0;
  FCaption:= '';
  Fx:= -1000;
  Fy:= -1000;
  FSize:= 32;
  FCaption := '';
  FUpdating:= false;
  FSelected:= false;
  FRunning:= false;
  FProcessWindowsCount := 0;
  FShowHint:= true;
  FSite:= 3;
  FItemSize := 32;
  FImage := nil;
  FIW := 32;
  FIH := 32;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.Redraw;
begin
  if IsWindowVisible(FHWnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, FBackground, true);
end;
//------------------------------------------------------------------------------
function TCustomSubitem.cmd(id: TDParam; param: PtrInt): PtrInt;
begin
  result := 0;
  try
    case id of
      // parameters //
      gpItemSize: FItemSize := param;
      gpSite: FSite := param;
      gpShowHint: FShowHint := boolean(param);
      gpLaunchInterval: FLaunchInterval := param;
      gpActivateRunning: FActivateRunning := boolean(param);
      gpLockDragging: FLockDragging := param <> 0;

      // commands //

      icSelect:
        if FSelected <> boolean(param) then
        begin
          FSelected := boolean(param);
          if IsWindowVisible(FHwnd) then Redraw;
        end;
    end;

  except
    on e: Exception do raise Exception.Create('TCustomSubitem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
// returns position of the item window in relative coordinates //
function TCustomSubitem.Measure(ASize: integer; AAngle: single; AHintAlign: integer): windows.TRect;
begin
  try
    if FFreed or FUpdating or FQueryDelete then exit;

    result.Left := -MIN_BORDER;
    result.Right := ASize + MIN_BORDER;
    result.Top := -MIN_BORDER;
    result.Bottom := ASize + MIN_BORDER;
  except
    on e: Exception do raise Exception.Create('TCustomSubitem.Measure(' + caption + ') ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.ToString: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
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
function TCustomSubitem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
  result := not FFreed;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.MouseHeld(button: TMouseButton);
begin
  FSelected := false;
  if button = mbLeft then BeginDrag;
end;
//------------------------------------------------------------------------------
// item window rect in client coordinates
function TCustomSubitem.GetNCRect: windows.TRect;
var
  Border: integer;
begin
  Border := FCaptionWidth + FCaptionHeight + MIN_BORDER;
  result.Left   := 0;
  result.Top    := 0;
  result.Right  := Border + FSize + Border;
  result.Bottom := Border + FSize + Border;
end;
//------------------------------------------------------------------------------
// item rect in client coordinates
function TCustomSubitem.GetClientRect: windows.TRect;
var
  Border: integer;
begin
  Border := FCaptionWidth + FCaptionHeight + MIN_BORDER;
  result.Left   := Border;
  result.Top    := Border;
  result.Right  := Border + FSize;
  result.Bottom := Border + FSize;
end;
//------------------------------------------------------------------------------
// item rect in screen coordinates
function TCustomSubitem.GetScreenRect: windows.TRect;
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
function TCustomSubitem.HitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetClientRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
function TCustomSubitem.ScreenHitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetScreenRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.CloseStack;
begin
  sendmessage(FHWndParent, WM_ACTIVATE, 0, 0);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.Delete(AllowUndo: boolean = true);
var
  wnd: THandle;
  Inst: TCustomItem;
  pt: windows.TPoint;
begin
  if AllowUndo then
  begin
    wnd := dockh.DockCreateItem(pchar(ToString));
    Inst := TCustomItem(GetWindowLongPtr(wnd, GWL_USERDATA));
    if Inst is TCustomItem then
    begin
      GetCursorPos(pt);
      Inst.Draw(pt.x - Inst.Size div 2, pt.y - Inst.Size div 2, FSize, true, 0, SWP_SHOWWINDOW);
      SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + SWP_SHOWWINDOW);
      Inst.Dock;
      Inst.Delete;
    end;
  end;
  FQueryDelete := true;
  ShowWindow(FHWnd, SW_HIDE);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.BeginDrag;
var
  wnd: THandle;
  Inst: TCustomItem;
  pt: windows.TPoint;
begin
  wnd := dockh.DockCreateItem(pchar(ToString));
  Inst := TCustomItem(GetWindowLongPtr(wnd, GWL_USERDATA));
  if Inst is TCustomItem then
  begin
    GetCursorPos(pt);
    Inst.Draw(pt.x - Inst.Size div 2, pt.y - Inst.Size div 2, FSize, true, 0, SWP_SHOWWINDOW);
    SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + SWP_SHOWWINDOW);
    Inst.Undock;
  end;
  Delete(false);
  dockh.Undock(wnd);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.SetFont(var Value: TDFontData);
begin
  CopyFontData(Value, FFont);
  UpdateCaptionExtent;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.UpdateCaptionExtent;
var
  hgdip, hfont, hfontfamily: Pointer;
  rect: TRectF;
  dc: HDC;
begin
  FCaptionWidth := 0;
  FCaptionHeight := 0;
  if FShowHint and (length(FCaption) > 0) then
  begin
    dc := CreateCompatibleDC(0);
    if dc = 0 then raise Exception.Create('CustomSubitem.UpdateCaptionExtent.CreateCompatibleDC failed');
    try
      GdipCreateFromHDC(dc, hgdip);
      try
        if Ok <> GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, hfontfamily) then
          raise Exception.Create('CustomSubitem.UpdateCaptionExtent.CreateFontFamily failed');
        try
          if Ok <> GdipCreateFont(hfontfamily, FFont.size2, integer(FFont.bold) + integer(FFont.italic) * 2, 2, hfont) then
            raise Exception.Create('CustomSubitem.UpdateCaptionExtent.CreateFont failed');
          try
            rect.x := 0;
            rect.y := 0;
            rect.Width := 0;
            rect.Height := 0;
            GdipMeasureString(hgdip, PWideChar(FCaption), -1, hfont, @rect, nil, @rect, nil, nil);
          finally
            GdipDeleteFont(hfont);
          end;
        finally
          GdipDeleteFontFamily(hfontfamily);
        end;
      finally
        GdipDeleteGraphics(hgdip);
      end;
    finally
      DeleteDC(dc);
    end;
    FCaptionWidth := min(ceil(rect.Width), MAX_CAPTION_WIDTH);
    FCaptionHeight := ceil(rect.Height);
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  pos: TSmallPoint;
  wpt: windows.TPoint;
  ShiftState: TShiftState;
begin
  try
      result := 0;
      pos := TSmallPoint(DWORD(lParam));
      ShiftState := [];
      if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
      if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);

      if message = wm_lbuttondown then
      begin
            SetActiveWindow(FHWndParent);
            MouseDownPoint.x:= pos.x;
            MouseDownPoint.y:= pos.y;
            if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y);
      end
      else if message = wm_rbuttondown then
      begin
            SetActiveWindow(FHWndParent);
            MouseDownPoint.x:= pos.x;
            MouseDownPoint.y:= pos.y;
            if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y);
      end
      else if message = wm_mbuttondown then
      begin
            SetActiveWindow(FHWndParent);
      end
      else if message = wm_lbuttonup then
      begin
            if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y);
      end
      else if message = wm_rbuttonup then
      begin
            if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y);
      end
      else if message = wm_mousemove then
      begin
            if not FLockDragging and (wParam and MK_LBUTTON <> 0) then
            begin
              if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then BeginDrag;
            end;
      end
      else if message = wm_command then
      begin
            WMCommand(wParam, lParam, result);
      end
      else if message = wm_timer then
      begin
            if wParam = ID_TIMER_MOUSEHELD then
            begin
              KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
              GetCursorPos(wpt);
              if WindowFromPoint(wpt) = FHWnd then MouseHeld(FMouseDownButton);
            end;
      end
      else if (message = wm_close) or (message = wm_quit) then exit;

  except
    on e: Exception do err('CustomSubitem.WindowProc[ Msg=0x' + inttohex(message, 8) + ' ]', e);
  end;

  result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + LineEnding + e.message);
    //notify(where + LineEnding + e.message);
  end else begin
    AddLog(where);
    //notify(where);
  end;
end;
//------------------------------------------------------------------------------
end.

