unit aeropeeku;

{$undef EXT_DEBUG}

interface

uses Windows, Messages, Classes, SysUtils, Forms, lazutf8, Syncobjs,
  declu, dwm_unit, GDIPAPI, gfx, toolu, processhlp, loggeru;

const
  TITLE_PART = 63;
  AEROPEEK_WCLASS = 'TDockAeroPeekWClass';

type
  TAPWLayout = (apwlHorizontal, apwlVertical);
  TAPWState = (apwsOpen, apwsClose);

  TAeroPeekWindowItem = packed record
    hwnd: THandle;            // target window handle
    ThumbnailId: THandle;     // live preview thumbnail handle
    srcW: integer;            // thumbnail source width
    srcH: integer;            // thumbnail source height
    image: pointer;           // window icon as GDIP bitmap
    iw: cardinal;             // icon width
    ih: cardinal;             // icon height
    rect: windows.TRect;      // item bounds rect
    rectSel: windows.TRect;   // item selection rect
    rectIcon: windows.TRect;  // window icon rect
    rectTitle: windows.TRect; // window title rect
    rectThumb: windows.TRect; // live thumbnail area rect
    rectClose: windows.TRect; // close button rect
    title: array [0..TITLE_PART] of WideChar;
  end;

  { TAeroPeekWindow }

  TAeroPeekWindow = class
  private
    FHWnd: THandle;
    FHostWnd: THandle;
    FPt: windows.TPoint;
    Fx: integer;
    Fy: integer;
    FXTarget: integer;
    FYTarget: integer;
    FWidth: integer;
    FHeight: integer;
    FWTarget: integer;
    FHTarget: integer;
    FAlpha: integer;
    FAlphaTarget: integer;
    FIconSize: integer;
    FBorderX: integer;
    FBorderY: integer;
    ThumbW: integer;
    ThumbH: integer;
    ItemSplit: integer;
    FTitleHeight: integer;
    FTitleSplit: integer;
    FSeparatorW: integer;
    FSeparatorH: integer;
    FRadius: integer;
    FSelectionRadius: integer;
    FCloseButtonSize: integer;
    FActivating: boolean;
    FActive: boolean;
    FSite: integer;
    FTaskThumbSize: integer;
    FWorkArea: windows.TRect;
    FAnimate: boolean;
    FCloseButtonDownIndex: integer;
    FForegroundWindowIndex: integer;
    FColor1: cardinal;
    FColor2: cardinal;
    FTextColor: cardinal;
    FCompositionEnabled: boolean;
    FFontFamily: string;
    FFontSize: integer;
    FLayout: TAPWLayout;
    FWindowCount: integer;
    FProcessCount: integer;
    FItemCount: integer;
    FSeparatorCount: integer;
    FHover: boolean;
    FHoverIndex: integer;
    FAeroPeekIndex: integer;
    FAeroPeekAllowedInt: boolean;
    FState: TAPWState;
    crs: TCriticalSection;
    items: array of TAeroPeekWindowItem;
    procedure AddItems(AppList: TFPList);
    procedure ClearImages;
    procedure DeleteItem(index: integer);
    procedure DoTransition;
    procedure DrawCloseButton(hgdip: pointer; rect: GDIPAPI.TRect; Pressed: boolean);
    function InTransition: boolean;
    procedure InvokeAeroPeek(index: integer);
    procedure MouseEnter;
    procedure Paint;
    function GetMonitorRect(AMonitor: integer): Windows.TRect;
    procedure PositionThumbnails;
    procedure RegisterThumbnails;
    procedure SetItems;
    procedure Timer;
    procedure TimerAeroPeek;
    procedure TimerClose;
    procedure TrackMouse;
    procedure UnRegisterThumbnails;
    procedure UpdateTitles;
    procedure err(where: string; e: Exception);
  public
    property Handle: THandle read FHWnd;
    property HostHandle: THandle read FHostWnd;
    property Active: boolean read FActive;

    class function Open(HostWnd: THandle; AppList: TFPList; AX, AY, Site, TaskThumbSize: integer; LivePreviews: boolean): boolean;
    class procedure SetPosition(AX, AY: integer);
    class procedure Close(Timeout: cardinal = 0);
    class function IsActive: boolean;
    class function ActivatedBy(HostWnd: THandle): boolean;
    class procedure CMouseLeave;
    class procedure Cleanup;

    constructor Create;
    destructor Destroy; override;
    procedure RegisterWindowClass;
    function OpenAPWindow(HostWnd: THandle; AppList: TFPList; AX, AY, Site, TaskThumbSize: integer; LivePreviews: boolean): boolean;
    procedure SetAPWindowPosition(AX, AY: integer);
    procedure MouseLeave;
    procedure CloseAPWindowInt;
    procedure CloseAPWindow(Timeout: cardinal = 0);
    function WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
    procedure LButtonDown(pt: windows.TPoint);
    procedure LButtonUp(pt: windows.TPoint);
    procedure MouseMove;
    procedure WMTimer(wParam: WPARAM);
  end;

var AeroPeekWindow: TAeroPeekWindow;

implementation
//------------------------------------------------------------------------------
// open (show) AeroPeekWindow
class function TAeroPeekWindow.Open(HostWnd: THandle; AppList: TFPList; AX, AY, Site, TaskThumbSize: integer; LivePreviews: boolean): boolean;
begin
  result := false;
  if not assigned(AeroPeekWindow) then AeroPeekWindow := TAeroPeekWindow.Create;
  if assigned(AeroPeekWindow) then
    result := AeroPeekWindow.OpenAPWindow(HostWnd, AppList, AX, AY, Site, TaskThumbSize, LivePreviews);
end;
//------------------------------------------------------------------------------
// set new position
class procedure TAeroPeekWindow.SetPosition(AX, AY: integer);
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.SetAPWindowPosition(AX, AY);
end;
//------------------------------------------------------------------------------
// close AeroPeekWindow
// if Timeout set then close after timeout has elapsed
class procedure TAeroPeekWindow.Close(Timeout: cardinal = 0);
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.CloseAPWindow(Timeout);
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
class procedure TAeroPeekWindow.CMouseLeave;
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.MouseLeave;
end;
//------------------------------------------------------------------------------
// destroy window
class procedure TAeroPeekWindow.Cleanup;
begin
  if assigned(AeroPeekWindow) then AeroPeekWindow.Free;
  AeroPeekWindow := nil;
end;
//------------------------------------------------------------------------------
function AeroPeekClassWindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  inst: TAeroPeekWindow;
begin
  inst := TAeroPeekWindow(GetWindowLongPtr(wnd, GWL_USERDATA));
  if assigned(inst) then
    result := inst.WindowProc(wnd, message, wParam, lParam)
  else
    result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
constructor TAeroPeekWindow.Create;
begin
  inherited;
  FState := apwsOpen;
  FActive := false;
  FAnimate := true;
  FFontFamily := toolu.GetFont;
  FFontSize := round(toolu.GetFontSize * 1.6);
  FCloseButtonDownIndex := -1;
  FItemCount := 0;
  FHover := false;
  FHoverIndex := -1;
  FAeroPeekIndex := -1;
  FAeroPeekAllowedInt := false;
  crs := TCriticalSection.Create;

  // create window //
  FHWnd := 0;
  try
    RegisterWindowClass;
    FHWnd := CreateWindowEx(WS_EX_LAYERED + WS_EX_TOOLWINDOW, AEROPEEK_WCLASS, nil, WS_POPUP, -100, -100, 1, 1, 0, 0, hInstance, nil);
    DWM.ExcludeFromPeek(FHWnd);
    if IsWindow(FHWnd) then SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUInt(self))
    else err('AeroPeekWindow.Create.CreateWindowEx failed', nil);
  except
    on e: Exception do err('AeroPeekWindow.Create.CreateWindow', e);
  end;
end;
//------------------------------------------------------------------------------
destructor TAeroPeekWindow.Destroy;
begin
  SetWindowLongPtr(FHWnd, GWL_USERDATA, PtrUInt(0));
  DestroyWindow(FHWnd);
  FHWnd := 0;
  crs.free;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.err(where: string; e: Exception);
begin
  if assigned(e) then
  begin
    AddLog(where + LineEnding + e.message);
    messagebox(0, PChar(where + LineEnding + e.message), declu.PROGRAM_NAME, MB_ICONERROR)
  end else begin
    AddLog(where);
    messagebox(0, PChar(where), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.RegisterWindowClass;
var
  wndClass: windows.TWndClass;
begin
  try
    wndClass.style          := CS_DBLCLKS;
    wndClass.lpfnWndProc    := @AeroPeekClassWindowProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := AEROPEEK_WCLASS;
    windows.RegisterClass(wndClass);
  except
    on e: Exception do err('AeroPeekWindow.RegisterWindowClass', e);
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
begin
  if FCompositionEnabled then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
      begin
        dwm.RegisterThumbnail(FHWnd, items[index].hwnd, items[index].ThumbnailId);
        dwm.GetThumbnailSize(items[index].ThumbnailId, items[index].srcW, items[index].srcH);
      end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.PositionThumbnails;
var
  index: integer;
begin
  if FCompositionEnabled then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
        dwm.SetThumbnailRect(items[index].ThumbnailId, items[index].rectThumb);
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
procedure TAeroPeekWindow.UpdateTitles;
var
  index: integer;
  title: array [0..TITLE_PART] of WideChar;
  needRepaint, needRearrange: boolean;
begin
  needRepaint := false;
  needRearrange := false;
  if FItemCount > 0 then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
      begin
        // if there is no icon for the window - update the icon
        if not assigned(items[index].image) then
        begin
          LoadImageFromHWnd(items[index].hwnd, FIconSize, true, false, items[index].image, items[index].iw, items[index].ih, 500);
          if assigned(items[index].image) then needRearrange := true;
        end;
        // update window title
        GetWindowTextW(items[index].hwnd, title, TITLE_PART);
        if UTF8CompareStr(UTF16ToUTF8(items[index].title), UTF16ToUTF8(title)) <> 0 then needRepaint := true;
      end;
  if needRearrange then SetItems;
  if needRepaint or needRearrange then Paint;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.OpenAPWindow(HostWnd: THandle; AppList: TFPList; AX, AY, Site, TaskThumbSize: integer; LivePreviews: boolean): boolean;
var
  opaque: bool;
  mon: THandle;
  mi: MONITORINFO;
begin
  result := false;
  if FActive then MouseLeave;
  if not FActivating then
  try
    try
      crs.Acquire;
      FActivating := true;
      FHostWnd := HostWnd;
      FSite := Site;
      FTaskThumbSize := TaskThumbSize;
      FState := apwsOpen;
      KillTimer(FHWnd, ID_TIMER_CLOSE);
      KillTimer(FHWnd, ID_TIMER_TRACKMOUSE);
      KillTimer(FHWnd, ID_TIMER_AEROPEEK);
      KillTimer(FHWnd, ID_TIMER);
      KillTimer(FHWnd, ID_TIMER_SLOW);
      UnRegisterThumbnails;
      ClearImages;
      if AppList.Count = 0 then
      begin
        CloseAPWindow;
        exit;
      end;

      // get monitor work area
      FPt.x := AX;
      FPt.y := AY;
      mon := MonitorFromPoint(FPt, MONITOR_DEFAULTTONEAREST);
      FillChar(mi, sizeof(mi), 0);
      mi.cbSize := sizeof(mi);
      GetMonitorInfoA(mon, @mi);
      FWorkArea := mi.rcWork;

      //
      FCompositionEnabled := dwm.IsCompositionEnabled and LivePreviews and (AppList.Count <= 14);
      FAnimate := FCompositionEnabled;
      FLayout := apwlHorizontal;
      if not FCompositionEnabled or (FSite = 0) or (FSite = 2) then FLayout := apwlVertical;

      // assign colors
      dwm.GetColorizationColor(FColor1, opaque);
      FColor1 := FColor1 and $ffffff or $a0000000;
      FColor2 := FColor1;
      //FColor2 := $40ffffff;
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

      // add items
      AddItems(AppList);
      // register thumbnails
      RegisterThumbnails;
      // set items' positions, calulate window size, update workarea
      SetItems;
      FAlphaTarget := 255;
      FAlpha := 255;
      FHover := false;
      FHoverIndex := -1;
      FAeroPeekIndex := -1;
      FAeroPeekAllowedInt := false;

      // set starting position
      if FAnimate then
      begin
        if not FActive then
        begin
          FAlpha := 25;
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

      // show the window
      Paint;
      // set it as foreground
      SetWindowPos(FHWnd, HWND_TOPMOST, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate);
      ShowWindow(FHWnd, SW_SHOW);
      if FAnimate then SetTimer(FHWnd, ID_TIMER, 10, nil);
      SetTimer(FHWnd, ID_TIMER_SLOW, 1000, nil);
      FActive := true;
    finally
      FActivating := false;
      crs.Leave;
    end;
  except
    on e: Exception do err('AeroPeekWindow.OpenWindow', e);
  end;

  // fix window icons bug
  UpdateTitles;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.CloseAPWindow(Timeout: cardinal = 0);
begin
  try
    KillTimer(FHWnd, ID_TIMER_AEROPEEK);
    KillTimer(FHWnd, ID_TIMER_TRACKMOUSE);
    KillTimer(FHWnd, ID_TIMER_CLOSE);

    if Timeout = 0 then
    begin
        // set desired position
        if FAnimate then
        begin
            FAlphaTarget := 25;
            if FSite = 3 then FYTarget := Fy + 20
            else if FSite = 1 then FYTarget := Fy - 20
            else if FSite = 0 then FXTarget := Fx - 20
            else if FSite = 2 then FXTarget := Fx + 20;
            FState := apwsClose;
        end else begin
            CloseAPWindowInt;
        end;
    end
    else
    begin
        SetTimer(Handle, ID_TIMER_CLOSE, Timeout, nil);
    end;
  except
    on e: Exception do err('AeroPeekWindow.CloseAPWindow', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.CloseAPWindowInt;
begin
  try
    InvokeAeroPeek(-1);
    KillTimer(FHWnd, ID_TIMER_AEROPEEK);
    KillTimer(FHWnd, ID_TIMER_TRACKMOUSE);
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    KillTimer(FHWnd, ID_TIMER_SLOW);
    KillTimer(FHWnd, ID_TIMER);
    UnRegisterThumbnails;
    ClearImages;
    FActive := false;
    ShowWindow(FHWnd, SW_HIDE);
    TAeroPeekWindow.Cleanup;
  except
    on e: Exception do err('AeroPeekWindow.CloseAPWindowInt', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.SetAPWindowPosition(AX, AY: integer);
begin
  KillTimer(FHWnd, ID_TIMER_CLOSE);
  KillTimer(FHWnd, ID_TIMER_TRACKMOUSE);
  KillTimer(FHWnd, ID_TIMER_AEROPEEK);

  if FActive and not (FState = apwsClose) then
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
procedure TAeroPeekWindow.TrackMouse;
var
  pt: windows.TPoint;
  wnd: THandle;
begin
  try
    GetCursorPos(pt);
    wnd := WindowFromPoint(pt);
    if wnd = FHWnd then
    begin
      if not FHover then MouseEnter;
    end else begin
      if FHover then MouseLeave;
    end;
  except
    on e: Exception do err('AeroPeekWindow.TrackMouse', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.MouseEnter;
begin
  if not FHover then
  try
    FHover := true;
    SetTimer(FHWnd, ID_TIMER_TRACKMOUSE, 150, nil);
    SetTimer(FHWnd, ID_TIMER_AEROPEEK, 1000, nil);
  except
    on e: Exception do err('AeroPeekWindow.MouseEnter', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.MouseLeave;
begin
  try
    if FHover then
    begin
      FHover := false;
      FHoverIndex := -1;
      KillTimer(FHWnd, ID_TIMER_TRACKMOUSE);
      KillTimer(FHWnd, ID_TIMER_AEROPEEK);
      InvokeAeroPeek(-1);
      CloseAPWindow(500);
    end;
  except
    //on e: Exception do err('AeroPeekWindow.MouseLeave', e);
  end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.WindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  pt: windows.TPoint;
begin
  Result := 0;
  try
    pt.x := TSmallPoint(DWORD(lParam)).x;
    pt.y := TSmallPoint(DWORD(lParam)).y;
    if message = WM_LBUTTONDOWN then LButtonDown(pt)
    else if message = WM_LBUTTONUP then LButtonUp(pt)
    else if message = WM_MOUSEMOVE then MouseMove
    else if message = WM_TIMER then WMTimer(wParam)
    else Result := DefWindowProc(wnd, message, wParam, lParam);
  except
    on e: Exception do err('AeroPeekWindow.WindowProc', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.LButtonDown(pt: windows.TPoint);
var
  index: integer;
begin
  try
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
  except
    on e: Exception do err('AeroPeekWindow.LButtonDown', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.LButtonUp(pt: windows.TPoint);
var
  index: integer;
begin
  try
    FCloseButtonDownIndex := -1;
    Paint;
    for index := 0 to FItemCount - 1 do
    begin
      if items[index].hwnd <> 0 then
        if PtInRect(items[index].rectSel, pt) then
        begin
          if PtInRect(items[index].rectClose, pt) then
          begin
            ProcessHelper.CloseWindow(items[index].hwnd);
            if FItemCount < 2 then CloseAPWindow
            else DeleteItem(index);
            exit;
          end
          else begin
            ProcessHelper.ActivateWindow(items[index].hwnd);
            CloseAPWindow;
            exit;
          end;
        end;
    end;
  except
    on e: Exception do err('AeroPeekWindow.LButtonUp', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.MouseMove;
var
  index: integer;
  pt: windows.TPoint;
begin
  if not InTransition then
  try
    if not FHover then MouseEnter;

    GetCursorPos(pt);
    dec(pt.x, Fx);
    dec(pt.y, Fy);
    for index := 0 to FItemCount - 1 do
    begin
      if items[index].hwnd <> 0 then
        if PtInRect(items[index].rectSel, pt) then
        begin
          if FHoverIndex <> index then
          begin
            FHoverIndex := index;
            Paint;
            InvokeAeroPeek(FHoverIndex);
          end;
        end;
    end;
  except
    on e: Exception do err('AeroPeekWindow.MouseMove', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.WMTimer(wParam: WPARAM);
begin
  try
    if wParam = ID_TIMER then Timer
    else
    if wParam = ID_TIMER_CLOSE then TimerClose
    else
    if wParam = ID_TIMER_SLOW then UpdateTitles
    else
    if wParam = ID_TIMER_AEROPEEK then TimerAeroPeek
    else
    if wParam = ID_TIMER_TRACKMOUSE then TrackMouse;
  except
    on e: Exception do err('AeroPeekWindow.WMTimer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.TimerClose;
var
  pt: windows.TPoint;
begin
  GetCursorPos(pt);
  if WindowFromPoint(pt) <> FHWnd then CloseAPWindow;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.TimerAeroPeek;
var
  pt: windows.TPoint;
begin
  KillTimer(FHWnd, ID_TIMER_AEROPEEK);
  GetCursorPos(pt);
  if WindowFromPoint(pt) = FHWnd then
  begin
    FAeroPeekAllowedInt := true;
    if FAeroPeekIndex <> FHoverIndex then InvokeAeroPeek(FHoverIndex);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Timer;
begin
  if FActive and not FActivating then
  try
    if InTransition then
    begin
      DoTransition;
      if (FState = apwsClose) and (Fx = FXTarget) and (Fy = FYTarget) then CloseAPWindowInt;
    end;
  except
    on e: Exception do err('AeroPeekWindow.Timer', e);
  end;
end;
//------------------------------------------------------------------------------
function TAeroPeekWindow.InTransition: boolean;
begin
  result := (FXTarget <> Fx) or (FYTarget <> Fy)
    or (FWTarget <> FWidth) or (FHTarget <> FHeight) or (FAlpha <> FAlphaTarget);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.DoTransition;
var
  delta: integer;
begin
  try
    if FXTarget <> Fx then
    begin
      delta := abs(FXTarget - Fx) div 4;
      if delta < 2 then delta := 2;
      if abs(Fx - FXTarget) <= delta then Fx := FXTarget;
      if Fx > FXTarget then Dec(Fx, delta);
      if Fx < FXTarget then Inc(Fx, delta);
    end;

    if FYTarget <> Fy then
    begin
      delta := abs(FYTarget - Fy) div 4;
      if delta < 2 then delta := 2;
      if abs(Fy - FYTarget) <= delta then Fy := FYTarget;
      if Fy > FYTarget then Dec(Fy, delta);
      if Fy < FYTarget then Inc(Fy, delta);
    end;

    if FWTarget <> FWidth then
    begin
      delta := abs(FWTarget - FWidth) div 4;
      if delta < 2 then delta := 2;
      if abs(FWidth - FWTarget) <= delta then FWidth := FWTarget;
      if FWidth > FWTarget then Dec(FWidth, delta);
      if FWidth < FWTarget then Inc(FWidth, delta);
    end;

    if FHTarget <> FHeight then
    begin
      delta := abs(FHTarget - FHeight) div 4;
      if delta < 2 then delta := 2;
      if abs(FHeight - FHTarget) <= delta then FHeight := FHTarget;
      if FHeight > FHTarget then Dec(FHeight, delta);
      if FHeight < FHTarget then Inc(FHeight, delta);
    end;

    if FAlphaTarget <> FAlpha then
    begin
      delta := abs(FAlphaTarget - FAlpha) div 4;
      if abs(FAlpha - FAlphaTarget) <= delta then FAlpha := FAlphaTarget;
      if delta < 1 then delta := 1;
      if FAlpha > FAlphaTarget then Dec(FAlpha, delta);
      if FAlpha < FAlphaTarget then Inc(FAlpha, delta);
    end;

    Paint;
  except
    on e: Exception do err('AeroPeekWindow.DoTransition', e);
  end;
end;
//------------------------------------------------------------------------------
// -1 to disable aero peek
procedure TAeroPeekWindow.InvokeAeroPeek(index: integer);
begin
  if (FAeroPeekIndex <> index) or (index = -1) then
  try
    crs.Acquire;
    if (index > -1) and FAeroPeekAllowedInt then
    begin
      FAeroPeekIndex := index;
      DWM.InvokeAeroPeek(1, items[index].hwnd, FHWnd);
    end;
    if index = -1 then
    begin
      FAeroPeekAllowedInt := false;
      FAeroPeekIndex := index;
      try DWM.InvokeAeroPeek(0, 0, 0);
      except end;
    end;
  finally
    crs.Leave;
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//
//
//
//
//
//
// items handling
//
//
//
//
//
//
//
//
//
//
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.AddItems(AppList: TFPList);
var
  index, itemIndex: integer;
  pid, prevpid: dword;
  needSeparators: boolean;
  wnd: THandle;
begin
  // count processes
  FWindowCount := AppList.Count;
  if FWindowCount > 0 then FProcessCount := 1;
  index := 0;
  prevpid := 0;
  while index < FWindowCount do
  begin
    wnd := THandle(AppList.Items[index]);
    if IsWindow(wnd) then
    begin
      GetWindowThreadProcessId(wnd, @pid);
      if (index > 0) and (pid <> prevpid) then inc(FProcessCount);
      prevpid := pid;
    end;
    inc(index);
  end;
  needSeparators := (FProcessCount > 1) and (FProcessCount < FWindowCount);

  // store handles
  FItemCount := 0;
  FSeparatorCount := 0;
  prevpid := 0;
  pid := 0;
  index := 0;
  while index < FWindowCount do
  begin
    wnd := THandle(AppList.Items[index]);
    if IsWindow(wnd) then
    begin
      inc(FItemCount);
      SetLength(items, FItemCount);
      itemIndex := FItemCount - 1;
      items[itemIndex].hwnd := wnd;
      if needSeparators then GetWindowThreadProcessId(wnd, @pid);
      if (itemIndex > 0) and (pid <> prevpid) then
      begin
        items[itemIndex].hwnd := 0;
        inc(FSeparatorCount);
      end else
        inc(index);
      prevpid := pid;
    end else begin
      inc(index);
    end;
  end;

  // load icons
  if FItemCount > 0 then
    for index := 0 to FItemCount - 1 do
      if items[index].hwnd <> 0 then
        LoadImageFromHWnd(items[index].hwnd, FIconSize, true, false, items[index].image, items[index].iw, items[index].ih, 500);
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.DeleteItem(index: integer);
var
  tmp: integer;
begin
  if (index >= 0) and (index < FItemCount) then
  begin
    // unregister appropriate thumbnail
    if items[index].hwnd <> 0 then dwm.UnregisterThumbnail(items[index].ThumbnailId)
    else dec(FSeparatorCount);

    // shift items in the array so they erase the one to delete
    tmp := index;
    while index < FItemCount - 1 do
    begin
      items[index].hwnd := items[index + 1].hwnd;
      items[index].ThumbnailId := items[index + 1].ThumbnailId;
      items[index].image := items[index + 1].image;
      items[index].iw := items[index + 1].iw;
      items[index].ih := items[index + 1].ih;
      inc(index);
    end;
    // resize items array
    dec(FItemCount);
    SetLength(items, FItemCount);

    // check for 2 separators next to each other
    index := tmp;
    if items[index].hwnd = 0 then
    begin
      if index = 0 then
      begin
        DeleteItem(index);
        exit;
      end else begin
        if items[index - 1].hwnd = 0 then
        begin
          DeleteItem(index);
          exit;
        end;
      end;
    end;

    if FItemCount < 1 then CloseAPWindow
    else
    begin
      SetItems;
      PositionThumbnails;
      Paint;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.SetItems;
var
  index: integer;
  maxw, maxh, position: integer;
  tmp: integer;
  maxRate: extended;
  //
  title: array [0..255] of WideChar;
  dc: HDC;
  hgdip, family, font: pointer;
  rect: GDIPAPI.TRectF;
begin
  try
    // set primary params
    if FCompositionEnabled then
    begin
      FBorderX := 16;
      FBorderY := 16;
      FIconSize := 16;
      FTitleHeight := 19;
      FTitleSplit := 14;
      ItemSplit := FBorderX * 2 - 1;
      FCloseButtonSize := FTitleHeight - 2;
      FRadius := 0;
      FSelectionRadius := 0;
    end else begin
      FBorderX := 18;
      FBorderY := 14;
      FIconSize := 16;
      FTitleHeight := 22;
      FTitleSplit := 0;
      ItemSplit := 10;
      FCloseButtonSize := 17;
      FRadius := 0;
      FSelectionRadius := 2;
    end;
    FSeparatorW := 10;
    FSeparatorH := 2;

    // calc thumbnail width and height
    if FCompositionEnabled then
    begin
      // get max rate = H / W
      maxRate := 0.1;
      if FItemCount > 0 then
        for index := 0 to FItemCount - 1 do
          if items[index].srcW <> 0 then
            if maxRate < items[index].srcH / items[index].srcW then maxRate := items[index].srcH / items[index].srcW;
      if maxRate > 0.8 then maxRate := 0.8;

      if FLayout = apwlHorizontal then
      begin
        ThumbW := min(FTaskThumbSize, (FWorkArea.Right - FWorkArea.Left - FBorderX * 2 -
          FSeparatorCount * (FSeparatorW + ItemSplit)) div (FItemCount - FSeparatorCount) - ItemSplit);
        ThumbH := round(ThumbW * maxRate);
      end else begin
        ThumbW := FTaskThumbSize;
        ThumbH := round(ThumbW * maxRate);
        tmp := (FWorkArea.Bottom - FWorkArea.Top - FBorderY * 2 - FSeparatorCount * (FSeparatorH + ItemSplit)) div
          (FItemCount - FSeparatorCount) - FTitleHeight - FTitleSplit - ItemSplit;
        if ThumbH > tmp then
        begin
          ThumbH := tmp;
          ThumbW := round(ThumbH / maxRate);
        end;
      end;

    end else begin
      ThumbW := FTaskThumbSize;
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
            FillChar(title, 512, #0);
            GetWindowTextW(items[index].hwnd, @title, 254);
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
            items[index].rect.Right := items[index].rect.Left + FSeparatorW;
            items[index].rect.Bottom := items[index].rect.Top + FTitleHeight + FTitleSplit + ThumbH;
          end else begin
            items[index].rect.Right := items[index].rect.Left + ThumbW;
            items[index].rect.Bottom := items[index].rect.Top + FSeparatorH;
          end;
        end;
        if items[index].rect.Right - items[index].rect.Left > maxw then maxw := items[index].rect.Right - items[index].rect.Left;
        if items[index].rect.Bottom - items[index].rect.Top > maxh then maxh := items[index].rect.Bottom - items[index].rect.Top;

        if items[index].hwnd <> 0 then
        begin
          items[index].rectSel := items[index].rect;
          if FCompositionEnabled then
          begin
            items[index].rectSel.Left -= FBorderX - FRadius;
            items[index].rectSel.Top -= FBorderX - FRadius;
            items[index].rectSel.Right += FBorderX - FRadius;
            items[index].rectSel.Bottom += FBorderX - FRadius;
          end else begin
            items[index].rectSel.Left -= 5;
            items[index].rectSel.Top -= 5;
            items[index].rectSel.Right += 5;
            items[index].rectSel.Bottom += 5;
          end;

          items[index].rectThumb := items[index].rect;
          items[index].rectThumb.Top += FTitleHeight;
          items[index].rectThumb.Top += FTitleSplit;
          if items[index].rectThumb.Bottom - items[index].rectThumb.Top > items[index].srcH then
          begin
            items[index].rectThumb.Top := items[index].rectThumb.Top + (items[index].rectThumb.Bottom - items[index].rectThumb.Top - items[index].srcH) div 2;
            items[index].rectThumb.Bottom := items[index].rectThumb.Top + items[index].srcH;
          end;
          if items[index].rectThumb.Right - items[index].rectThumb.Left > items[index].srcW then
          begin
            items[index].rectThumb.Left := items[index].rectThumb.Left + (items[index].rectThumb.Right - items[index].rectThumb.Left - items[index].srcW) div 2;
            items[index].rectThumb.Right := items[index].rectThumb.Left + items[index].srcW;
          end;

          items[index].rectTitle := items[index].rect;
          if assigned(items[index].image) then items[index].rectTitle.Left += items[index].iw + 3;
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
             //if items[index].hwnd = GetForegroundWindow then FForegroundWindowIndex := index;
        end;

        // update thumbnail rect
        if FCompositionEnabled then
          dwm.SetThumbnailRect(items[index].ThumbnailId, items[index].rectThumb);

        if FLayout = apwlHorizontal then
        begin
          if items[index].hwnd <> 0 then position += ThumbW else position += FSeparatorW;
        end else begin
          if items[index].hwnd <> 0 then position += FTitleHeight + FTitleSplit + ThumbH else position += FSeparatorH;
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

    if not FAnimate or not FActive then
    begin
      FWidth := FWTarget;
      FHeight := FHTarget;
    end;

    // calculate position
    FXTarget := FPt.x - FWTarget div 2;
    FYTarget := FPt.y - FHTarget;
    if FSite = 1 then // top
    begin
      FXTarget := FPt.x - FWTarget div 2;
      FYTarget := FPt.y;
    end else if FSite = 0 then // left
    begin
      FXTarget := FPt.x;
      FYTarget := FPt.y - FHTarget div 2;
    end else if FSite = 2 then // right
    begin
      FXTarget := FPt.x - FWTarget;
      FYTarget := FPt.y - FHTarget div 2;
    end;
    // position window inside workarea
    if FXTarget + FWTarget > FWorkArea.Right then FXTarget := FWorkArea.Right - FWTarget;
    if FYTarget + FHTarget > FWorkArea.Bottom then FYTarget := FWorkArea.Bottom - FHTarget;
    if FXTarget < FWorkArea.Left then FXTarget := FWorkArea.Left;
    if FYTarget < FWorkArea.Top then FYTarget := FWorkArea.Top;
  except
    on e: Exception do err('AeroPeekWindow.SetItems', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.Paint;
var
  bmp: _SimpleBitmap;
  hgdip, brush, pen, path, family, font, format: Pointer;
  titleRect: GDIPAPI.TRectF;
  rect: GDIPAPI.TRect;
  rgn: HRGN;
  index, tmp: integer;
  title: array [0..255] of WideChar;
begin
  try
    if FWidth < 1 then FWidth := 1;
    if FHeight < 1 then FHeight := 1;

    // prepare //
    bmp.topleft.x := Fx;
    bmp.topleft.y := Fy;
    bmp.Width := FWidth;
    bmp.Height := FHeight;
    if not gfx.CreateBitmap(bmp, FHWnd) then raise Exception.Create('CreateBitmap failed');
    hgdip := CreateGraphics(bmp.dc, 0);
    if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
    GdipCreatePath(FillModeWinding, path);
    GdipAddPathRectangle(path, 0, 0, FWidth, FHeight);

    // background
    rect := GDIPAPI.MakeRect(0, 0, FWidth, FHeight);
    GdipCreateLineBrushFromRectI(@rect, FColor1, FColor2, LinearGradientModeVertical, WrapModeTileFlipY, brush);
    GdipFillPath(hgdip, brush, path);
    GdipDeleteBrush(brush);
    GdipCreatePen1($a0000000, 1, UnitPixel, pen); // dark border
    GdipDrawPath(hgdip, pen, path);
    GdipDeletePen(pen);
    GdipResetPath(path); // light border
    GdipAddPathRectangle(path, 1, 1, FWidth - 2, FHeight - 2);
    GdipCreatePen1($10ffffff, 1, UnitPixel, pen);
    GdipDrawPath(hgdip, pen, path);
    GdipDeletePen(pen);
    GdipDeletePath(path);

    if FItemCount > 0 then
    begin
      // foreground item
      if FForegroundWindowIndex > -1 then
      begin
        GdipCreatePath(FillModeWinding, path);
        rect := WinRectToGDIPRect(items[FForegroundWindowIndex].rectSel);
        AddPathRoundRect(path, rect, FSelectionRadius);
        GdipCreateSolidFill($40b0d0ff, brush);
        GdipFillPath(hgdip, brush, path);
        GdipDeleteBrush(brush);
        GdipDeletePath(path);
      end;

      // hover item
      if FHoverIndex > -1 then
      begin
        GdipCreatePath(FillModeWinding, path);
        rect := WinRectToGDIPRect(items[FHoverIndex].rectSel);
        AddPathRoundRect(path, rect, FSelectionRadius);
        GdipCreateSolidFill($30ffffff, brush);
        GdipFillPath(hgdip, brush, path);
        GdipDeleteBrush(brush);
        GdipDeletePath(path);
        // close button
        DrawCloseButton(hgdip, WinRectToGDIPRect(items[FHoverIndex].rectClose), FCloseButtonDownIndex = FHoverIndex);
      end;
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
    begin
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
        GetWindowTextW(items[index].hwnd, items[index].title, TITLE_PART);
        titleRect := WinRectToGDIPRectF(items[index].rectTitle);
        if index <> FHoverIndex then titleRect.Width := items[index].rectClose.Right - items[index].rectTitle.Left;
        GdipDrawString(hgdip, PWideChar(@title), -1, font, @titleRect, format, brush);
      end;
    end;
    GdipDeleteStringFormat(format);
    GdipDeleteBrush(brush);
    GdipDeleteFont(font);
    GdipDeleteFontFamily(family);

    // update window //
    UpdateLWindow(FHWnd, bmp, FAlpha);
    GdipDeleteGraphics(hgdip);
    gfx.DeleteBitmap(bmp);
    if not FCompositionEnabled then SetWindowPos(FHWnd, HWND_TOPMOST, Fx, Fy, FWidth, FHeight, swp_noactivate + swp_showwindow);

    // enable blur behind
    if FCompositionEnabled then
    begin
      rgn := CreateRectRgn(0, 0, FWidth, FHeight);
      dwm.EnableBlurBehindWindow(FHWnd, rgn);
      DeleteObject(rgn);
    end else begin
      dwm.DisableBlurBehindWindow(FHWnd);
    end;
  except
    on e: Exception do err('AeroPeekWindow.Paint', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TAeroPeekWindow.DrawCloseButton(hgdip: pointer; rect: GDIPAPI.TRect; Pressed: boolean);
var
  pen: Pointer;
  crossRect: GDIPAPI.TRect;
  color: cardinal;
begin
  color := $ffffffff;
  if Pressed then color := $80ffffff;
  crossRect.Width := rect.Width - 4;
  crossRect.Height := rect.Height - 4;
  crossRect.X := rect.X + 2;
  crossRect.Y := rect.Y + 2;
  GdipCreatePen1(color, 3, UnitPixel, pen);
  GdipSetPenStartCap(pen, LineCapRound);
  GdipSetPenEndCap(pen, LineCapRound);
  GdipDrawLineI(hgdip, pen, crossRect.X, crossRect.Y, crossRect.X + crossRect.Width, crossRect.Y + crossRect.Height);
  GdipDrawLineI(hgdip, pen, crossRect.X, crossRect.Y + crossRect.Height, crossRect.X + crossRect.Width, crossRect.Y);
  GdipDeletePen(pen);
end;
//------------------------------------------------------------------------------
end.

