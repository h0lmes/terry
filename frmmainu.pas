unit frmmainu;

interface

uses
  jwaWindows, Windows, Messages, SysUtils, Classes, Controls, LCLType, Forms,
  Menus, Dialogs, ExtCtrls, ShellAPI, ComObj, ShlObj, Math, Syncobjs, MMSystem, LMessages,
  declu, GDIPAPI, gfx, dwm_unit, hintu, notifieru, itemmgru,
  DropTgtU, setsu, trayu, startmenu, aeropeeku, mixeru;

type
  PRunData = ^TRunData;
  TRunData = packed record
    Handle: THandle;
    exename: array [0..1023] of char;
    params: array [0..1023] of char;
    dir: array [0..1023] of char;
    showcmd: integer;
  end;

  { Tfrmmain }

  Tfrmmain = class(TForm)
    trayicon: TTrayIcon;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure trayiconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    inta: cardinal;
    closing: boolean;
    saving: boolean;
    AllowClose: boolean;
    PrevBlur: boolean;
    ZOrderWindow: uint;
    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;
    HiddenByFSA: boolean; // true if panel was hidden by HideOnFullScreenApp parameter //
    LockList: TList; // disables zoom and related. stores hadles of windows that have requested a lock //
    crsection: TCriticalSection;
    wndOffsetTarget: integer;
    HideKeysPressed: boolean;
    SavedWorkarea: windows.TRect;
    revertMonitor: integer;
    procedure CreateZOrderWindow;
    function  CloseQuery: integer;
    procedure SaveSets;
    procedure RegisterRawInput;
    procedure NativeWndProc(var message: TMessage);
    procedure AppException(Sender: TObject; e: Exception);
    procedure AppDeactivate(Sender: TObject);
    procedure WMCopyData(var Message: TMessage);
    procedure WMTimer(var msg: TMessage);
    procedure WMUser(var msg: TMessage);
    procedure WMCommand(var msg: TMessage);
    procedure WMDisplayChange(var Message: TMessage);
    procedure WMSettingChange(var Message: TMessage);
    procedure WMCompositionChanged(var Message: TMessage);
    procedure WMMouseWheel(var msg: TWMMouseWheel);
    procedure WHMouseMove(LParam: LParam);
    procedure WHButtonDown(button: integer);
    procedure OnMouseEnter;
    procedure OnMouseLeave;
    procedure DoMenu;
    procedure OnTimerMain;
    procedure OnTimerSlow;
    procedure OnTimerFSA;
    procedure OnTimerRoll;
    procedure OnTimerDragLeave;
    procedure UpdateRunning;
    function  IsHiddenDown: boolean;
    procedure DoRollDown;
    procedure DoRollUp;
    procedure OnTimerDoRollUpDown;
    procedure SetForeground;
    procedure SetNotForeground;
    procedure MaintainNotForeground;
    procedure BasePaint(flags: integer);
    procedure HideTaskbar(Hide: boolean);
    procedure SetWorkarea(rect: windows.TRect);
    procedure ReserveScreenEdge(Percent: integer; Edge: TBaseSite);
    procedure UnreserveScreenEdge(Edge: TBaseSite);
  public
    ItemMgr: TItemManager;
    DropMgr: TDropManager;
    AHint: THint;
    Tray: TTrayController;
    StartMenu: TStartMenuController;
    ///
    wndOffset: integer;
    OldBaseWindowRect: GDIPAPI.TRect;
    OldBaseImageRect: GDIPAPI.TRect;
    LastMouseHookPoint: Windows.TPoint;
    MouseOver: boolean;
    InitDone: boolean;
    hHook: THandle;
    hMenu: THandle;
    hMenuCreate: THandle;
    procedure Init(SetsFilename: string);
    procedure ExecAutorun;
    procedure CloseProgram;
    procedure ApplyParams;
    procedure err(where: string; e: Exception);
    procedure notify(message: string; silent: boolean = False);
    procedure alert(message: string);
    procedure ActivateHint(hwnd: uint; ACaption: WideString; x, y: integer);
    procedure DeactivateHint(hwnd: uint);
    procedure SetTheme(ATheme: string);
    function  BaseCmd(id: TGParam; param: integer): integer;
    procedure SetParam(id: TGParam; Value: integer);
    function  GetHMenu(ParentMenu: uint): uint;
    function  ContextMenu(pt: Windows.TPoint): boolean;
    procedure SetFont(var Value: _FontData);
    procedure LockMouseEffect(hWnd: HWND; lock: boolean);
    function  IsLockedMouseEffect: boolean;
    function  GetMonitorWorkareaRect(pMonitor: PInteger = nil): Windows.TRect;
    function  GetMonitorBoundsRect(pMonitor: PInteger = nil): Windows.TRect;
    procedure MoveDock(iDirection: integer);
    procedure OnDragEnter(list: TStrings; hWnd: uint);
    procedure OnDragOver;
    procedure OnDragLeave;
    procedure OnDrop(files: TStrings; hWnd: uint);
    procedure DropFiles(files: TStrings);
    procedure AddFile; overload;
    procedure AddFile(Filename: string); overload;
    procedure NewDock;
    procedure RemoveDock;
    procedure OpenWith(filename: string);
    function  FullScreenAppActive(HWnd: HWND): boolean;
    function  ListFullScreenApps: string;
    procedure mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
    procedure execute_cmdline(cmd: string; showcmd: integer = 1);
    procedure execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
    procedure Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
  end;

var frmmain: Tfrmmain;

implementation
uses themeu, toolu, scitemu, PIDL, dockh, frmsetsu, frmcmdu, frmitemoptu,
  frmStackPropu, frmAddCommandU, frmthemeeditoru, processhlp, frmhellou,
  frmtipu, multidocku;
{$R *.lfm}
{$R Resource\res.res}
//------------------------------------------------------------------------------
procedure Tfrmmain.Init(SetsFilename: string);
var
  load_err: boolean;
  //theFile: string;
begin
  try
    closing := false;
    saving := false;
    AllowClose := false;
    PrevBlur := false;
    InitDone := false;
    Application.OnException := AppException;
    Application.OnDeactivate := AppDeactivate;
    trayicon.Icon := application.Icon;
    LockList := TList.Create;
    crsection := TCriticalSection.Create;

    CreateZOrderWindow;

    // workaround for Windows message handling in LCL //
    FWndInstance := MakeObjectInstance(NativeWndProc);
    FPrevWndProc := Pointer(GetWindowLongPtr(Handle, GWL_WNDPROC));
    SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FWndInstance));

    dwm.ExcludeFromPeek(Handle);

    // hook //
    //theFile := UnzipPath('%pp%\hook.dll');
    //hHook := 0;
    //if FileExists(theFile) then hHook := LoadLibrary(pchar(theFile));

    // ProcessHelper (must be created before tray controller) //
    ProcessHelper := TProcessHelper.Create(toolu.bIsWindowsVista);

    sets := _Sets.Create(SetsFilename, UnzipPath('%pp%'), Handle);
    sets.Load;
    revertMonitor := sets.container.Monitor;

    theme := _Theme.Create(pchar(@sets.container.ThemeName), sets.container.Site);
    if not theme.Load then
    begin
      notify(UTF8ToAnsi(XErrorLoadTheme + ' ' + XErrorContactDeveloper));
      AddLog('Theme.Halt');
      halt;
    end;

    // create ItemManager (disabled, not visible) //
    AddLog('Init.ItemManager');
    ItemMgr := TItemManager.Create(false, false, Handle, BaseCmd,
      sets.container.ItemSize, sets.container.BigItemSize, sets.container.ZoomWidth,
      sets.container.ZoomTime, sets.container.ItemSpacing, sets.container.ZoomItems,
      sets.container.Reflection, sets.container.ReflectionSize, sets.container.LaunchInterval,
      sets.container.ItemAnimation, sets.container.SeparatorAlpha,
      sets.container.ActivateRunning, sets.container.UseShellContextMenus, sets.container.LockDragging,
      sets.container.StackOpenAnimation,
      sets.container.TaskLivePreviews, sets.container.TaskGrouping,
      sets.container.TaskThumbSize, sets.container.TaskSpot,
      sets.container.ShowHint, sets.container.Font);
    //ApplyParams;
    SetParam(gpStayOnTop, integer(sets.container.StayOnTop));
    SetParam(gpSite, integer(sets.container.site));
    SetParam(gpCenterOffsetPercent, sets.container.CenterOffsetPercent);
    SetParam(gpEdgeOffset, sets.container.EdgeOffset);
    SetParam(gpAutoHide, integer(sets.container.AutoHide));
    SetParam(gpAutoHidePixels, integer(sets.container.AutoHidePixels));
    SetParam(gpHideTaskBar, integer(sets.container.HideTaskBar));
    SetParam(gpReserveScreenEdge, sets.GetParam(gpReserveScreenEdge));
    SetParam(gpMonitor, sets.container.Monitor);
    SetParam(gpOccupyFullMonitor, integer(sets.container.OccupyFullMonitor));
    BaseCmd(tcThemeChanged, 0);

    // load items //
    load_err := false;
    try
      ItemMgr.Load(UnzipPath(sets.SetsPathFile));
      ItemMgr.Enable(true);
    except
      on e: Exception do
      begin
        AddLog('Init.LoadItems');
        AddLog(e.message);
        load_err := true;
      end;
    end;
    if load_err then
    begin
        if sets.Restore then
        begin
          AddLog('Init.Restore.Succeed');
          messagebox(handle, pchar(UTF8ToAnsi(XErrorSetsCorrupted + ' ' + XMsgSetsRestored + ' ' + XMsgRunAgain)), PROGRAM_NAME, MB_ICONEXCLAMATION);
          halt;
        end else begin
          AddLog('Init.Restore.Failed');
          messagebox(handle,
            pchar(UTF8ToAnsi(XErrorSetsCorrupted + ' ' + XErrorSetsRestoreFailed + ' ' + XErrorContactDeveloper)), PROGRAM_NAME, MB_ICONERROR);
          halt;
        end;
    end
    else
    if not sets.Backup then
    begin
        AddLog('Init.BackupFailed');
        messagebox(handle, pchar(UTF8ToAnsi(XErrorSetsBackupFailed)), PROGRAM_NAME, MB_ICONERROR);
    end;

    // Timers //
    SetTimer(handle, ID_TIMER, 10, nil);
    SetTimer(handle, ID_TIMER_SLOW, 1000, nil);
    SetTimer(handle, ID_TIMER_FSA, 2000, nil);

    if sets.Restored then notify(UTF8ToAnsi(XMsgSetsRestored));

    // Tray and StartMenu controllers //
    Tray := TTrayController.Create;
    StartMenu := TStartMenuController.Create;

    // show the dock //
    BaseCmd(tcSetVisible, 1);
    if sets.GetParam(gpStayOnTop) = 1 then SetParam(gpStayOnTop, 1);

    // RawInput (replacement for hook) //
    RegisterRawInput;

    // DropManager //
    DropMgr := TDropManager.Create(Handle);
    DropMgr.OnDrop := OnDrop;
    DropMgr.OnDragEnter := OnDragEnter;
    DropMgr.OnDragOver := OnDragOver;
    DropMgr.OnDragLeave := OnDragLeave;

    // apply the theme to do the full repaint //
    //ApplyParams;
    SetParam(gpStayOnTop, integer(sets.container.StayOnTop));
    SetParam(gpSite, integer(sets.container.site));
    SetParam(gpCenterOffsetPercent, sets.container.CenterOffsetPercent);
    SetParam(gpEdgeOffset, sets.container.EdgeOffset);
    SetParam(gpAutoHide, integer(sets.container.AutoHide));
    SetParam(gpAutoHidePixels, integer(sets.container.AutoHidePixels));
    SetParam(gpHideTaskBar, integer(sets.container.HideTaskBar));
    SetParam(gpReserveScreenEdge, sets.GetParam(gpReserveScreenEdge));
    SetParam(gpMonitor, sets.container.Monitor);
    SetParam(gpOccupyFullMonitor, integer(sets.container.OccupyFullMonitor));
    BaseCmd(tcThemeChanged, 0);
    //BaseCmd(tcThemeChanged, 0);

    // 'RollDown' on startup if set so //
    wndOffsetTarget := 0;
    wndOffset := 0;
    DoRollDown;

    if sets.container.Hello then TfrmHello.Open;
    InitDone := True;
  except
    on e: Exception do err('Base.Init', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.ExecAutorun;
var
  idx: integer;
begin
  try
      if assigned(sets.AutoRunList) then
      begin
          idx := 0;
          while idx < sets.AutoRunList.Count do
          begin
              if sets.AutoRunList.strings[idx] <> '' then
              begin
                  if copy(sets.AutoRunList.strings[idx], 1, 1) = '#' then
                    execute_cmdline(copy(sets.AutoRunList.strings[idx], 2, 1023), SW_MINIMIZE)
                  else
                    execute_cmdline(sets.AutoRunList.strings[idx]);
              end;
              application.processmessages;
              inc(idx);
          end;
      end;
  except
    on e: Exception do err('Base.ExecAutorun', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.CreateZOrderWindow;
begin
  try
    ZOrderWindow := CreateWindowEx(WS_EX_TOOLWINDOW, 'tooltips_class32',
      'ZOrder', WS_CHILD, 0, 0, 0, 0, handle, 0, hInstance, nil);
    if not IsWindow(ZOrderWindow) then notify('Base.CreateZOrderWindow. Window was not created');
  except
    on e: Exception do err('Base.CreateZOrderWindow', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.CloseProgram;
begin
  AllowClose := true;
  Close;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseQuery = 0 then CloseAction := caNone else CloseAction := caFree;
end;
//------------------------------------------------------------------------------
// check if program can close and, if yes, free all resources
function Tfrmmain.CloseQuery: integer;
begin
  result := 0;

  if AllowClose then
  try
    crsection.Acquire;
    closing := True;
    AddLog('CloseQuery begin');
    HideTaskbar(false);
    BaseCmd(tcSaveSets, 0);
    try
      KillTimer(handle, ID_TIMER);
      KillTimer(handle, ID_TIMER_SLOW);
      KillTimer(handle, ID_TIMER_FSA);
      TAeroPeekWindow.Cleanup;
      if assigned(DropMgr) then DropMgr.Destroy;
      DropMgr := nil;
      if assigned(ItemMgr) then ItemMgr.Free;
      ItemMgr := nil;
      if assigned(ahint) then ahint.Free;
      ahint := nil;
      if assigned(theme) then theme.Free;
      theme := nil;
      if assigned(sets) then sets.Free;
      sets := nil;
      TProcessHelper.Cleanup;
      TNotifier.Cleanup;
      LockList.free;
      // reset window proc
      SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FPrevWndProc));
      FreeObjectInstance(FWndInstance);
      // close other instances
      if not docks.RemoveDock then
      begin
        docks.Enum;
        docks.Close;
      end;
    except
      on e: Exception do messagebox(handle, PChar(e.message), 'Base.Close.Free', mb_iconexclamation);
    end;
    AddLog('CloseQuery done');
    result := 1;
  finally
    crsection.Leave;
    crsection.free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.ApplyParams;
begin
  try
    SetParam(gpStayOnTop, integer(sets.container.StayOnTop));
    SetParam(gpSite, integer(sets.container.site));
    SetParam(gpCenterOffsetPercent, sets.container.CenterOffsetPercent);
    SetParam(gpEdgeOffset, sets.container.EdgeOffset);
    SetParam(gpAutoHide, integer(sets.container.AutoHide));
    SetParam(gpAutoHidePixels, integer(sets.container.AutoHidePixels));
    SetParam(gpHideTaskBar, integer(sets.container.HideTaskBar));
    SetParam(gpReserveScreenEdge, sets.GetParam(gpReserveScreenEdge));
    SetParam(gpMonitor, sets.container.Monitor);
    SetParam(gpOccupyFullMonitor, integer(sets.container.OccupyFullMonitor));

    ItemMgr.SetParam(gpItemSize, sets.container.itemsize);
    ItemMgr.SetParam(gpBigItemSize, sets.container.BigItemSize);
    ItemMgr.SetParam(gpZoomItems, integer(sets.container.ZoomItems));
    ItemMgr.SetParam(gpZoomWidth, sets.container.ZoomWidth);
    ItemMgr.SetParam(gpZoomTime, sets.container.ZoomTime);
    ItemMgr.SetParam(gpItemSpacing, sets.container.ItemSpacing);
    ItemMgr.SetParam(gpReflection, integer(sets.container.Reflection));
    ItemMgr.SetParam(gpReflectionSize, sets.container.ReflectionSize);
    ItemMgr.SetParam(gpLaunchInterval, sets.container.LaunchInterval);
    ItemMgr.SetParam(gpActivateRunning, integer(sets.container.ActivateRunning));
    ItemMgr.SetParam(gpUseShellContextMenus, integer(sets.container.UseShellContextMenus));
    ItemMgr.SetParam(gpItemAnimation, sets.container.ItemAnimation);
    ItemMgr.SetParam(gpLockDragging, integer(sets.container.LockDragging));
    ItemMgr.SetParam(gpShowRunningIndicator, integer(sets.container.ShowRunningIndicator));
    ItemMgr.SetParam(gpStackOpenAnimation, integer(sets.container.StackOpenAnimation));
    ItemMgr.SetParam(gpTaskSameMonitor, integer(sets.container.TaskSameMonitor));
    ItemMgr.SetParam(gpTaskLivePreviews, integer(sets.container.TaskLivePreviews));
    ItemMgr.SetParam(gpTaskThumbSize, sets.container.TaskThumbSize);
    ItemMgr.SetParam(gpTaskGrouping, integer(sets.container.TaskGrouping));
    ItemMgr.SetParam(gpTaskSpot, sets.container.TaskSpot);
    ItemMgr.SetParam(gpSeparatorAlpha, sets.container.SeparatorAlpha);

    ItemMgr.SetFont(sets.container.Font);
    ItemMgr.SetParam(gpShowHint, integer(sets.container.ShowHint));

    BaseCmd(tcThemeChanged, 0);
  except
    on e: Exception do err('Base.ApplyParams', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.SaveSets;
begin
  if saving then
  begin
    AddLog('SaveSettings.exit_save');
    exit;
  end;
  if assigned(ItemMgr) then
  try
    crsection.Acquire;
    try
      saving := true;
      sets.Save;
      ItemMgr.Save(sets.SetsPathFile);
    finally
      saving := false;
      crsection.Leave;
    end;
    AddLog('SavedSettings');
  except
    on e: Exception do err('Base.BaseSaveSettings', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.SetTheme(ATheme: string);
begin
  StrCopy(sets.container.ThemeName, PChar(aTheme));
  theme.setTheme(ATheme);
  BaseCmd(tcThemeChanged, 0);
end;
//------------------------------------------------------------------------------
function Tfrmmain.BaseCmd(id: TGParam; param: integer): integer;
begin
  Result := 0;

  case id of
    tcRepaintBase: BasePaint(param);
    tcMenu: DoMenu;
    tcSaveSets: SaveSets;
    tcZOrder: SetForeground;
    // a big command to rearrange everything
    tcThemeChanged:
      if assigned(ItemMgr) then
      begin
        ItemMgr.ItemsArea := theme.CorrectMargins(theme.ItemsArea);
        ItemMgr.ItemsArea2 := theme.CorrectMargins(theme.ItemsArea2);
        ItemMgr.Margin := theme.Margin;
        ItemMgr.Margin2 := theme.Margin2;
        ItemMgr.FMonitorRect := GetMonitorBoundsRect;
        case sets.container.Site of
          bsLeft, bsRight:
            begin
              ItemMgr.FMonitorRect.Top += sets.container.StartOffset;
              ItemMgr.FMonitorRect.Bottom -= sets.container.EndOffset;
            end;
          bsTop, bsBottom:
            begin
              ItemMgr.FMonitorRect.Left += sets.container.StartOffset;
              ItemMgr.FMonitorRect.Right -= sets.container.EndOffset;
            end;
        end;
        ItemMgr.SetTheme;
      end;
    // set dock visibility
    tcSetVisible:
      begin
        if (param = 0) and assigned(ItemMgr) then ItemMgr.Visible := false;
        Visible := boolean(param);
        if boolean(param) then SetForeground;
        if (param <> 0) and assigned(ItemMgr) then ItemMgr.Visible := true;
      end;
    tcToggleVisible: BaseCmd(tcSetVisible, integer(not Visible));
    tcToggleTaskbar: frmmain.SetParam(gpHideTaskBar, ifthen(sets.GetParam(gpHideTaskBar) = 0, 1, 0));
    tcGetVisible: Result := integer(ItemMgr.Visible);
  end;
end;
//------------------------------------------------------------------------------
// stores given parameter value and propagates it to subsequent objects       //
// e.g. ItemManager and all items                                             //
procedure Tfrmmain.SetParam(id: TGParam; value: integer);
begin
  // take some actions prior to changing anything //
  case id of
    gpMonitor, gpSite: UnreserveScreenEdge(sets.container.Site);
  end;

  value := sets.StoreParam(id, value);

  // take some actions prior to notifying ItemMgr //
  case id of
    gpMonitor:
      begin
        BaseCmd(tcThemeChanged, 0);
        revertMonitor := sets.container.Monitor;
      end;
    gpSite:
      begin
        if assigned(theme) then theme.Site := sets.container.Site;
        BaseCmd(tcThemeChanged, 0);
      end;
    gpAutoHide:               if value = 0 then DoRollUp;
    gpHideTaskBar:            HideTaskbar(value <> 0);
    gpReserveScreenEdge:
      begin
        if value = 0 then UnreserveScreenEdge(sets.container.Site)
        else ReserveScreenEdge(sets.container.ReserveScreenEdgePercent, sets.container.Site);
      end;
    gpStayOnTop:              if value <> 0 then SetForeground else SetNotForeground;
    gpBaseAlpha:              BasePaint(1);
    gpBlur:                   BasePaint(1);
    gpShowRunningIndicator:   if value <> 0 then UpdateRunning;
  end;

  if assigned(ItemMgr) then ItemMgr.SetParam(id, value);

  // take some actions after notifying ItemMgr //
  case id of
    gpLockMouseEffect: WHMouseMove(0);
    gpOccupyFullMonitor, gpStartOffset, gpEndOffset: BaseCmd(tcThemeChanged, 0);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.RegisterRawInput;
var
  rid: RAWINPUTDEVICE;
begin
  Rid.usUsagePage := 1;
  Rid.usUsage := 2;
  Rid.dwFlags := RIDEV_INPUTSINK;
  Rid.hwndTarget := Handle;
  if not RegisterRawInputDevices(@Rid, 1, sizeof(Rid)) then notify('RegisterRawInput failed!');
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.NativeWndProc(var message: TMessage);
var
  dwSize: uint;
  ri: RAWINPUT;
begin
  message.result := 0;
  case message.msg of
    WM_INPUT:
      if not closing then
      begin
        dwSize := 0;
        GetRawInputData(message.lParam, RID_INPUT, nil, dwSize, sizeof(RAWINPUTHEADER));
        if GetRawInputData(message.lParam, RID_INPUT, @ri, dwSize, sizeof(RAWINPUTHEADER)) <> dwSize then
          raise Exception.Create('Base.NativeWndProc. Invalid size of RawInputData');
        if ri.header.dwType = RIM_TYPEMOUSE then
        begin
          if ri.mouse.usButtonFlags and RI_MOUSE_LEFT_BUTTON_DOWN <> 0 then WHButtonDown(1);
          if ri.mouse.usButtonFlags and RI_MOUSE_RIGHT_BUTTON_DOWN <> 0 then WHButtonDown(2);
          WHMouseMove(0);
        end;
        exit;
      end;
    WM_TIMER : WMTimer(message);
    WM_USER : WMUser(message);
    WM_COMMAND : WMCommand(message);
    WM_QUERYENDSESSION :
      begin
        AllowClose := true;
        message.Result := CloseQuery;
      end;
    WM_MOUSEWHEEL : WMMouseWheel(TWMMouseWheel(message));
    WM_COPYDATA : WMCopyData(message);
    WM_DISPLAYCHANGE : WMDisplayChange(message);
    WM_SETTINGCHANGE : WMSettingChange(message);
    WM_DWMCOMPOSITIONCHANGED : WMCompositionChanged(message);
    WM_APP_RUN_THREAD_END: CloseHandle(message.lParam);
  end;
  with message do result := CallWindowProc(FPrevWndProc, Handle, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHButtonDown(button: integer);
begin
  if not IsLockedMouseEffect and not MouseOver and not sets.container.StayOnTop then SetNotForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHMouseMove(LParam: LParam);
var
  pt: Windows.Tpoint;
  monitorRect: Windows.TRect;
  OldMouseOver: boolean;
begin
  if not IsLockedMouseEffect and assigned(ItemMgr) and IsWindowVisible(Handle) and not closing then
  begin
    Windows.GetCursorPos(pt);
    if (pt.x <> LastMouseHookPoint.x) or (pt.y <> LastMouseHookPoint.y) or (LParam = $fffffff) then
    begin
      LastMouseHookPoint.x := pt.x;
      LastMouseHookPoint.y := pt.y;
      ItemMgr.WHMouseMove(pt, (LParam <> $fffffff) and not IsHiddenDown);

      // detect mouse enter/leave //
      OldMouseOver := MouseOver;
      monitorRect := ItemMgr.FMonitorRect;
      if sets.container.site = bsBottom then
        MouseOver := (pt.y >= monitorRect.Bottom - 1) and
          (pt.x >= ItemMgr.FBaseWindowRect.X + ItemMgr.FBaseImageRect.X) and (pt.x <= ItemMgr.FBaseWindowRect.X + ItemMgr.FBaseImageRect.X + ItemMgr.FBaseImageRect.Width)
      else if sets.container.site = bsTop then
        MouseOver := (pt.y <= monitorRect.Top) and
          (pt.x >= ItemMgr.FBaseWindowRect.X + ItemMgr.FBaseImageRect.X) and (pt.x <= ItemMgr.FBaseWindowRect.X + ItemMgr.FBaseImageRect.X + ItemMgr.FBaseImageRect.Width)
      else if sets.container.site = bsLeft then
        MouseOver := (pt.x <= monitorRect.Left) and
          (pt.y >= ItemMgr.FBaseWindowRect.Y + ItemMgr.FBaseImageRect.Y) and (pt.y <= ItemMgr.FBaseWindowRect.Y + ItemMgr.FBaseImageRect.Y + ItemMgr.FBaseImageRect.Height)
      else if sets.container.site = bsRight then
        MouseOver := (pt.x >= monitorRect.Right - 1) and
          (pt.y >= ItemMgr.FBaseWindowRect.Y + ItemMgr.FBaseImageRect.Y) and (pt.y <= ItemMgr.FBaseWindowRect.Y + ItemMgr.FBaseImageRect.Y + ItemMgr.FBaseImageRect.Height);
      MouseOver := MouseOver or ItemMgr.CheckMouseOn or ItemMgr.FDraggingItem;

      if MouseOver and not OldMouseOver then OnMouseEnter;
      if not MouseOver and OldMouseOver then OnMouseLeave;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnMouseEnter;
begin
  SetTimer(Handle, ID_TIMER_ROLL, sets.container.AutoShowTime, nil);
  // set foreground if 'activate' option selected //
  if IsWindowVisible(handle) and sets.container.ActivateOnMouse then SetForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnMouseLeave;
begin
  SetTimer(Handle, ID_TIMER_ROLL, sets.container.AutoHideTime, nil);
  // just to be sure
  ItemMgr.DragLeave;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if button = mbRight then DoMenu;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoMenu;
var
  pt: Windows.TPoint;
begin
  Windows.GetCursorPos(pt);
  ContextMenu(pt);
end;
//------------------------------------------------------------------------------
function Tfrmmain.GetHMenu(ParentMenu: uint): uint;
  function IsValidItemString(str: string): boolean;
  var
    classname: string;
  begin
    classname := FetchValue(str, 'class="', '";');
    result := (classname = 'shortcut') or (classname = 'stack');
  end;

var
  idx: integer;
begin
  if IsMenu(hMenu) then DestroyMenu(hMenu);
  if ParentMenu = 0 then hMenu := CreatePopupMenu else hMenu := ParentMenu;
  hMenuCreate := CreatePopupMenu;

  if ParentMenu = 0 then
    if IsValidItemString(GetClipboard) then
      AppendMenu(hMenu, MF_STRING, IDM_PASTE, pchar(UTF8ToAnsi(XPaste)));

  // create submenu 'Add...' //

  AppendMenu(hMenuCreate, MF_STRING + ifthen(ItemMgr._itemsDeleted.Count > 0, 0, MF_DISABLED), $f026, pchar(UTF8ToAnsi(XUndeleteIcon)));
  AppendMenu(hMenuCreate, MF_STRING, $f023, pchar(UTF8ToAnsi(XSpecificIcons)));
  AppendMenu(hMenuCreate, MF_STRING, $f021, pchar(UTF8ToAnsi(XEmptyIcon)));
  AppendMenu(hMenuCreate, MF_STRING, $f022, pchar(UTF8ToAnsi(XFile)));
  //AppendMenu(hMenuCreate, MF_STRING, $f026, pchar(UTF8ToAnsi(XInstalledApplication)));
  AppendMenu(hMenuCreate, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenuCreate, MF_STRING, $f024, pchar(UTF8ToAnsi(XSeparator)));
  AppendMenu(hMenuCreate, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenuCreate, MF_STRING, $f025, pchar(UTF8ToAnsi(XDock)));
  if sets.GetPluginCount = -1 then sets.ScanPlugins;
  if sets.GetPluginCount > 0 then
  begin
    AppendMenu(hMenuCreate, MF_SEPARATOR, 0, '-');
    idx := 0;
    while idx < sets.GetPluginCount do
    begin
      AppendMenu(hMenuCreate, MF_STRING, $f041 + idx, PChar(sets.GetPluginName(idx)));
      Inc(idx);
    end;
  end;

  // insert all menu items //
  if ParentMenu <> 0 then AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING + MF_POPUP, hMenuCreate, pchar(UTF8ToAnsi(XAddIcon)));
  AppendMenu(hMenu, MF_STRING + ifthen(sets.container.LockDragging, MF_CHECKED, 0), IDM_LOCKICONS, pchar(UTF8ToAnsi(XLockIcons)));
  AppendMenu(hMenu, MF_STRING, IDM_COLLECTION, pchar(UTF8ToAnsi(XIconCollection)));
  AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING, IDM_TASKMGR, pchar(UTF8ToAnsi(XTaskManager)));
  AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING, IDM_SETS, pchar(UTF8ToAnsi(XProgramSettings)));
  AppendMenu(hMenu, MF_STRING, IDM_QUIT, pchar(UTF8ToAnsi(XExit)));

  Result := hMenu;
end;
//------------------------------------------------------------------------------
function Tfrmmain.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  Result := False;
  GetHMenu(0);
  LockMouseEffect(Handle, true);
  SetForegroundWindow(handle);
  SetForeground;
  msg.WParam := uint(TrackPopupMenuEx(hMenu, TPM_RETURNCMD, pt.x, pt.y, handle, nil));
  WMCommand(msg);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMCommand(var msg: TMessage);
var
  cmd: string;
begin
  try
    LockMouseEffect(Handle, false);
    msg.Result := 0;
    if IsMenu(hMenu) then DestroyMenu(hMenu);

    if (msg.wparam >= $f021) and (msg.wparam < $f040) then
    begin
      case msg.wparam of
        $f021: cmd := '/itemmgr.shortcut';
        $f022: cmd := '/program';
        $f023: cmd := '/command';
        $f024: cmd := '/itemmgr.separator';
        $f025: cmd := '/newdock';
        $f026: cmd := '/undelete';

        IDM_PASTE: cmd := '/paste';
        IDM_LOCKICONS: cmd := '/lockdragging';
        IDM_COLLECTION: cmd := '/collection';
        IDM_TASKMGR: cmd := '/taskmgr';
        IDM_SETS: cmd := '/sets';
        IDM_QUIT: cmd := '/quit';
      end;
      execute_cmdline(cmd);
    end
    else
    if (msg.wparam >= $f041) and (msg.wparam <= $f0ff) then
    begin
      ItemMgr.command('plugin', sets.GetPluginFileName(msg.wparam - $f041));
    end;

  except
    on e: Exception do err('Base.WMCommand', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if shift = [] then
  begin
    if key = 112 {F1} then execute_cmdline('/help');
    if key = 192 {tilda} then execute_cmdline('/cmd');
  end
  else if shift = [ssAlt] then
  begin
    if key = 37 then MoveDock(0);
    if key = 38 then MoveDock(1);
    if key = 39 then MoveDock(2);
    if key = 40 then MoveDock(3);
  end
  else if shift = [ssCtrl] then
  begin
    if key = 90 {Ctrl+Z} then execute_cmdline('/undelete');
  end;
end;
//------------------------------------------------------------------------------
// moves dock to another edge and/or monitor
procedure Tfrmmain.MoveDock(iDirection: integer);
begin
  if iDirection = 0 then
  begin
    if sets.GetMonitorCount = 1 then SetParam(gpSite, 0)
    else
      if (sets.GetParam(gpMonitor) > 0) and (sets.GetParam(gpSite) = 0) then
      begin
        dec(sets.container.Monitor);
        SetParam(gpSite, 2);
        SetParam(gpMonitor, sets.container.Monitor);
      end
      else
        SetParam(gpSite, 0);
  end;

  if iDirection = 2 then
  begin
    if sets.GetMonitorCount = 1 then SetParam(gpSite, 2)
    else
      if (sets.GetParam(gpMonitor) < sets.GetMonitorCount - 1) and (sets.GetParam(gpSite) = 2) then
      begin
        inc(sets.container.Monitor);
        SetParam(gpSite, 0);
        SetParam(gpMonitor, sets.container.Monitor);
      end
      else
        SetParam(gpSite, 2);
  end;

  if iDirection = 1 then SetParam(gpSite, 1);
  if iDirection = 3 then SetParam(gpSite, 3);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMUser(var msg: TMessage);
begin
  if (msg.wParam = wm_activate) and (msg.lParam = 0) then
  begin
    BaseCmd(tcSetVisible, 1);
    DoRollUp;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMTimer(var msg: TMessage);
begin
  try
    if msg.WParam = ID_TIMER then OnTimerMain
    else if msg.WParam = ID_TIMER_SLOW then OnTimerSlow
    else if msg.WParam = ID_TIMER_FSA then OnTimerFSA
    else if msg.WParam = ID_TIMER_ROLL then OnTimerRoll
    else if msg.WParam = ID_TIMER_DRAGLEAVE then OnTimerDragLeave;
  except
    on e: Exception do err('Base.WMTimer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerMain;
begin
  if IsWindowVisible(Handle) then
  begin
    if assigned(ItemMgr) then ItemMgr.Timer;
    if assigned(Tray) then Tray.Timer;
    if assigned(StartMenu) then StartMenu.Timer;
  end;
  OnTimerDoRollUpDown;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerSlow;
begin
  if assigned(ItemMgr) and assigned(sets) then
  try
    if IsWindowVisible(Handle) then
    begin
      WHMouseMove($fffffff);
      UpdateRunning;
      MaintainNotForeground;
      ItemMgr.CheckDeleted;
    end;

    // maintain visibility
    if ItemMgr.visible and not IsWindowVisible(handle) then BaseCmd(tcSetVisible, 1);
    // maintain taskbar visibility
    if sets.container.HideTaskBar then HideTaskbar(true);
  except
    on e: Exception do raise Exception.Create('Base.OnTimerSlow'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerFSA;
var
  fsa: boolean;
begin
  try
    // keep the edge of the screen reserved if set so
    if sets.container.ReserveScreenEdge then ReserveScreenEdge(sets.container.ReserveScreenEdgePercent, sets.container.Site);

    // hide/show the dock by fullscreen apps
    if HiddenByFSA and Visible then HiddenByFSA := false;
    fsa := FullScreenAppActive(0);
    if sets.container.AutoHideOnFullScreenApp then
    begin
      if fsa and Visible then
      begin
        BaseCmd(tcSetVisible, 0);
        HiddenByFSA := true;
      end;
      if not fsa and not Visible and HiddenByFSA then BaseCmd(tcSetVisible, 1);
    end;
  except
    on e: Exception do raise Exception.Create('Base.OnTimerFSA'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerRoll;
var
  sets_visible: boolean;
begin
  if MouseOver then
  begin
    KillTimer(Handle, ID_TIMER_ROLL);
    DoRollUp;
  end
  else
  begin
    sets_visible := false;
    if assigned(frmsets) then sets_visible := frmsets.visible;
    if not sets_visible then
    begin
      KillTimer(Handle, ID_TIMER_ROLL);
      DoRollDown;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.UpdateRunning;
var
  parent: THandle;
begin
  if not IsLockedMouseEffect then
  try
    if sets.container.TaskSameMonitor then parent := Handle else parent := 0;
    if sets.container.ShowRunningIndicator or sets.container.Taskbar then
    begin
      ProcessHelper.EnumAppWindows(parent);
      if ProcessHelper.WindowsCountChanged then ProcessHelper.EnumProc;
      if sets.container.ShowRunningIndicator then ItemMgr.SetParam(icUpdateRunning, 0);
		  if sets.container.Taskbar then ItemMgr.Taskbar;
		end;
	except
    on e: Exception do raise Exception.Create('Base.UpdateRunning'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.IsHiddenDown: boolean;
begin
  result := wndOffsetTarget > 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoRollDown;
begin
  if ItemMgr.FDraggingItem or ItemMgr.FDraggingFile then exit;
  if sets.container.AutoHide and not IsHiddenDown then
  begin
    if sets.getBaseOrientation = boVertical then wndOffsetTarget := ItemMgr.FBaseWindowRect.Width - sets.container.AutoHidePixels
    else wndOffsetTarget := ItemMgr.FBaseWindowRect.Height - sets.container.AutoHidePixels;
    if wndOffsetTarget < 0 then wndOffsetTarget := 0;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoRollUp;
begin
  wndOffsetTarget := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerDoRollUpDown;
var
  KeyPressed: boolean;
  key, KeySet: integer;
begin
  if sets.container.AutoHide then
    if wndoffset <> wndOffsetTarget then
    begin
      if abs(wndOffsetTarget - WndOffset) > RollStep then
      begin
        if wndOffsetTarget > WndOffset then inc(wndOffset, RollStep) else dec(wndOffset, RollStep);
      end
      else wndOffset := wndOffsetTarget;
      if assigned(ItemMgr) then
      begin
        ItemMgr.WndOffset := wndOffset;
        ItemMgr.ItemsChanged;
      end;
    end;

  // hide/show
  KeyPressed := false;
  Key := sets.container.HideKeys and not (scShift + scCtrl + scAlt);
  if Key > 0 then KeyPressed := getasynckeystate(Key) < 0;
  KeySet := scNone;
  if getasynckeystate(16) < 0 then inc(KeySet, scShift);
  if getasynckeystate(17) < 0 then inc(KeySet, scCtrl);
  if getasynckeystate(18) < 0 then inc(KeySet, scAlt);
  if sets.container.HideKeys and (scShift + scCtrl + scAlt) <> KeySet then KeyPressed := false;
  if KeyPressed then
  begin
    if not HideKeysPressed then
    begin
      BaseCmd(tcToggleVisible, 0);
      HideKeysPressed := true;
    end;
  end
  else HideKeysPressed := false;
end;
//------------------------------------------------------------------------------
// bring the dock along with all items to foreground
procedure Tfrmmain.SetForeground;
  procedure setfore(h: THandle);
  begin
    SetWindowPos(h, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    SetWindowPos(h, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  end;
begin
  if closing then exit;
  // set all items topmost and place the dock window right underneath
  SetWindowPos(handle, ItemMgr.ZOrder(HWND_TOPMOST), 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  // set dock window non topmost
  SetWindowPos(handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  // set all items non topmost
  ItemMgr.ZOrder(HWND_NOTOPMOST);
  // place all the items right underneath ZOrderWindow
  ItemMgr.ZOrder(ZOrderWindow);

  // bring to the foreground other dock windows if any visible
  if assigned(frmItemProp) then setfore(frmItemProp.handle);
  if assigned(frmStackProp) then setfore(frmStackProp.handle);
  if assigned(frmSets) then setfore(frmSets.handle);
  if assigned(frmThemeEditor) then setfore(frmThemeEditor.handle);
end;
//------------------------------------------------------------------------------
// its complicated. describe later ...
procedure Tfrmmain.SetNotForeground;

function IsDockWnd(wnd: uint): boolean;
begin
  result := (wnd = handle) or not (ItemMgr.IsItem(wnd) = 0);
end;

function ZOrderIndex(hWnd: uint): integer;
var
  index: integer;
  h: THandle;
begin
  result := 0;
  index := 0;
  h := FindWindow('Progman', nil);
  while (h <> 0) and (h <> hWnd) do
  begin
    inc(index);
    h := GetWindow(h, GW_HWNDPREV);
  end;
  result := index;
end;

function DockAboveWnd(wnd: uint): boolean;
var
  rect, dockrect: windows.TRect;
  buf: array [0..MAX_PATH - 1] of char;
begin
  result := false;
  if IsWindowVisible(wnd) and not IsIconic(wnd) then
  begin
    GetWindowRect(wnd, @rect);
    dockrect := dockh.DockGetRect;
    if IntersectRect(rect, dockrect, rect) then
    begin
      GetClassName(wnd, buf, MAX_PATH);
      if (strpas(buf) <> 'Progman')
         and (strpas(buf) <> 'WorkerW')
         and (strpas(buf) <> 'Shell_TrayWnd')
         and (ZOrderIndex(wnd) < ZOrderIndex(handle)) then result := true;
    end;
  end;
end;

var
  pt: windows.TPoint;
  wnd, awnd: THandle;
begin
  if closing then exit;
  GetCursorPos(pt);
  wnd := WindowFromPoint(pt);
  if IsDockWnd(wnd) then exit;

  awnd := GetAncestor(wnd, GA_ROOTOWNER);
  if IsWindow(awnd) then wnd := awnd;
  if DockAboveWnd(wnd) then
  begin
    SetWindowPos(handle, wnd, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    ItemMgr.ZOrder(wnd);
    ItemMgr.ZOrder(ZOrderWindow);
    ItemMgr.UnZoom();
  end;
end;
//------------------------------------------------------------------------------
// keep all items on top of the dock window
procedure Tfrmmain.MaintainNotForeground;
var
  h: THandle;
begin
	h := FindWindow('Progman', nil);
  h := GetWindow(h, GW_HWNDPREV);
	while h <> 0 do
	begin
    if h = handle then exit; // main dock window found - ok and exit
    if ItemMgr.IsItem(h) <> 0 then // one of the items found (but not the dock yet) - adjust dock position
    begin
      SetWindowPos(handle, h, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
      exit;
    end;
    h := GetWindow(h, GW_HWNDPREV);
	end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.BasePaint(flags: integer);
var
  hgdip, hbrush: Pointer;
  bmp: gfx._SimpleBitmap;
  RepaintBase: boolean;
  rgn: HRGN;
begin
  if assigned(ItemMgr) and assigned(theme) and Visible and not closing then
  try
    hgdip := nil;
    bmp.dc := 0;
    RepaintBase := flags and 1 = 1;

    if (ItemMgr.FBaseImageRect.X <> OldBaseImageRect.X) or
      (ItemMgr.FBaseImageRect.Y <> OldBaseImageRect.Y) or
      (ItemMgr.FBaseImageRect.Width <> OldBaseImageRect.Width) or
      (ItemMgr.FBaseImageRect.Height <> OldBaseImageRect.Height) then
    begin
      RepaintBase := True;
      OldBaseImageRect := ItemMgr.FBaseImageRect;
    end;
    if (ItemMgr.FBaseWindowRect.X <> OldBaseWindowRect.X) or
      (ItemMgr.FBaseWindowRect.Y <> OldBaseWindowRect.Y) or
      (ItemMgr.FBaseWindowRect.Width <> OldBaseWindowRect.Width) or
      (ItemMgr.FBaseWindowRect.Height <> OldBaseWindowRect.Height) then
    begin
      RepaintBase := True;
      OldBaseWindowRect := ItemMgr.FBaseWindowRect;
    end;

    if RepaintBase then
    try
      // prepare a bitmap //
      bmp.topleft.x := ItemMgr.FBaseWindowRect.x;
      bmp.topleft.y := ItemMgr.FBaseWindowRect.y;
      bmp.Width := ItemMgr.FBaseWindowRect.Width;
      bmp.Height := ItemMgr.FBaseWindowRect.Height;
      if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
      hgdip := gfx.CreateGraphics(bmp.dc);
      if not assigned(hgdip) then raise Exception.Create('CreateGraphics failed');
      GdipSetCompositingMode(hgdip, CompositingModeSourceOver);
      GdipSetCompositingQuality(hgdip, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(hgdip, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(hgdip, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);
      // workaround to eliminate rumble on certain backgrounds while dragging a file //
      if ItemMgr.FDraggingFile then GdipGraphicsClear(hgdip, ITEM_BACKGROUND);
      // draw dock background image //
      Theme.DrawBackground(hgdip, ItemMgr.FBaseImageRect, sets.container.BaseAlpha);
      // update dock window //
      UpdateLWindow(handle, bmp, 255);

      // deal with blur //
      if dwm.CompositionEnabled and sets.container.Blur and Theme.BlurEnabled then
      begin
        PrevBlur := true;
        rgn := Theme.GetBackgroundRgn(ItemMgr.FBaseImageRect);
        if rgn <> 0 then DWM.EnableBlurBehindWindow(handle, rgn);
        DeleteObject(rgn);
      end
      else if PrevBlur then
      begin
        PrevBlur := false;
        DWM.DisableBlurBehindWindow(handle);
      end;

    finally
      gfx.DeleteGraphics(hgdip);
      gfx.DeleteBitmap(bmp);
    end;
  except
    on e: Exception do raise Exception.Create('Base.BaseDraw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.trayiconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMenu;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.err(where: string; e: Exception);
begin
  where := UTF8ToAnsi(XErrorIn) + ' ' + where;
  if assigned(e) then where := where + #10#13 + e.message;
  notify(where);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.notify(message: string; silent: boolean = False);
begin
  if assigned(Notifier) then Notifier.Message(message, sets.GetParam(gpMonitor), False, silent)
  else if not silent then messagebox(handle, pchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.alert(message: string);
begin
  if assigned(notifier) then notifier.message(message, sets.GetParam(gpMonitor), True, False)
  else messagebox(handle, pchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.ActivateHint(hwnd: uint; ACaption: WideString; x, y: integer);
var
  monitor: cardinal;
begin
  if not closing and not IsHiddenDown
     and not ItemMgr.FDraggingFile and not ItemMgr.FDraggingItem then
  begin
    if InitDone and not assigned(AHint) then AHint := THint.Create;
    if hwnd = 0 then monitor := MonitorFromWindow(Handle, 0) else monitor := MonitorFromWindow(hwnd, 0);
    if assigned(AHint) then AHint.ActivateHint(hwnd, ACaption, x, y, monitor, sets.container.Site);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DeactivateHint(hwnd: uint);
begin
  if not closing and assigned(AHint) then AHint.DeactivateHint(hwnd);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.LockMouseEffect(hWnd: HWND; lock: boolean);
var
  index: integer;
begin
  crsection.Acquire;
  try
    if lock then
    begin
      LockList.Add(pointer(hWnd));
    end else begin
      index := LockList.IndexOf(pointer(hWnd));
      if index >= 0 then LockList.Delete(index);
    end;
    if IsLockedMouseEffect then AHint.DeactivateImmediate;
    SetParam(gpLockMouseEffect, integer(IsLockedMouseEffect));
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.IsLockedMouseEffect: boolean;
begin
  result := LockList.Count > 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.SetFont(var Value: _FontData);
begin
  CopyFontData(Value, sets.container.Font);
  if assigned(ItemMgr) then ItemMgr.SetFont(Value);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.AppException(Sender: TObject; e: Exception);
begin
  notify('[AppException]'#13#10 + Sender.ClassName + #13#10 + e.message);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.AppDeactivate(Sender: TObject);
begin
  ItemMgr.WMDeactivateApp;
end;
//------------------------------------------------------------------------------
// returns DesktopRect for monitor indexes < 0
function Tfrmmain.GetMonitorWorkareaRect(pMonitor: PInteger = nil): Windows.TRect;
var
  monitor: integer;
begin
  result := screen.DesktopRect;
  if assigned(pMonitor) then monitor := pMonitor^ else monitor := sets.GetParam(gpMonitor);
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
// returns DesktopRect for monitor indexes < 0
function Tfrmmain.GetMonitorBoundsRect(pMonitor: PInteger = nil): Windows.TRect;
var
  monitor: integer;
begin
  result := screen.DesktopRect;
  if assigned(pMonitor) then monitor := pMonitor^ else monitor := sets.GetParam(gpMonitor);
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].BoundsRect;
end;
//------------------------------------------------------------------------------
// hide/show taskbar and start button
procedure Tfrmmain.HideTaskbar(hide: boolean);
var
  hwndTaskbar, hwndButton: uint;
  buttonVisible, taskbarVisible, updateWorkarea: boolean;
  taskbarRect, taskbarMonitorWorkarea, taskbarMonitorBounds: Windows.TRect;
  ptMon, ptTaskbar: windows.TPoint;
  taskbarMonitorIndex: integer; // a monitor on which the taskbar is
  taskbarSite: TBaseSite;
begin
  try
    hwndTaskbar := FindWindow('Shell_TrayWnd', nil);
    hwndButton := FindWindow('Button', pchar(UTF8ToAnsi(XStartButtonText)));
    taskbarVisible := IsWindowVisible(hwndTaskbar);
    buttonVisible := IsWindowVisible(hwndButton);
    updateWorkarea := false;

    // if workarea meant to be changed
    if hide = taskbarVisible then
    begin
      // get taskbar monitor and site
      taskbarMonitorIndex := 0;
      if GetWindowRect(hwndTaskbar, taskbarRect) then
      begin
        // monitor index
        ptTaskbar.x := (taskbarRect.Left + taskbarRect.Right) div 2;
        ptTaskbar.y := (taskbarRect.Top + taskbarRect.Bottom) div 2;
        taskbarMonitorIndex := screen.MonitorFromPoint(ptTaskbar).MonitorNum;
        // monitor center point
        taskbarMonitorBounds := GetMonitorBoundsRect(@taskbarMonitorIndex);
        taskbarMonitorWorkarea := GetMonitorWorkareaRect(@taskbarMonitorIndex);
        ptMon.x := (taskbarMonitorBounds.Right + taskbarMonitorBounds.Left) div 2;
        ptMon.y := (taskbarMonitorBounds.Bottom + taskbarMonitorBounds.Top) div 2;
        // taskbar site
        if taskbarRect.Bottom - taskbarRect.Top < taskbarRect.Right - taskbarRect.Left then
        begin
          if ptTaskbar.y > ptMon.y then taskbarSite := bsBottom else taskbarSite := bsTop;
        end else begin
          if ptTaskbar.x > ptMon.x then taskbarSite := bsRight else taskbarSite := bsLeft;
        end;
      end;

      // do not allow to change WA if the taskbar and the dock occupy the same site
      updateWorkarea := (taskbarMonitorIndex <> sets.container.Monitor) or (taskbarSite <> sets.container.Site);

      // calculate new WA
      if updateWorkarea and hide then
      begin
        case taskbarSite of
          bsBottom: taskbarMonitorWorkarea.Bottom := taskbarMonitorBounds.Bottom;
          bsTop: taskbarMonitorWorkarea.Top := taskbarMonitorBounds.Top;
          bsLeft: taskbarMonitorWorkarea.Left := taskbarMonitorBounds.Left;
          bsRight: taskbarMonitorWorkarea.Right := taskbarMonitorBounds.Right;
        end;
      end;
      if updateWorkarea and not hide then
      begin
        case taskbarSite of
          bsBottom: taskbarMonitorWorkarea.Bottom := taskbarMonitorBounds.Bottom - taskbarRect.Bottom + taskbarRect.Top;
          bsTop: taskbarMonitorWorkarea.Top := taskbarMonitorBounds.Top + taskbarRect.Bottom - taskbarRect.Top;
          bsLeft: taskbarMonitorWorkarea.Left := taskbarMonitorBounds.Left + taskbarRect.Right - taskbarRect.Left;
          bsRight: taskbarMonitorWorkarea.Right := taskbarMonitorBounds.Right - taskbarRect.Right + taskbarRect.Left;
        end;
      end;
    end;

    // hide or show Start Button
    if hide and buttonVisible then         showwindow(hwndButton, SW_HIDE);
    if not hide and not buttonVisible then showwindow(hwndButton, SW_SHOWNORMAL);

    // hide or show Taskbar
    if hide and taskbarVisible then         showwindow(hwndTaskbar, SW_HIDE);
    if not hide and not taskbarVisible then showwindow(hwndTaskbar, SW_SHOWNORMAL);

    // update workarea
    if updateWorkarea then SetWorkarea(taskbarMonitorWorkarea);
  except
    on e: Exception do raise Exception.Create('Base.HideTaskbar'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.SetWorkarea(rect: windows.TRect);
var
  h: HANDLE;
  wp: WINDOWPLACEMENT;
  monitor: integer; // a monitor of given workarea
  bounds: windows.TRect;
begin
  SystemParametersInfo(SPI_SETWORKAREA, 0, @rect, SPIF_SENDCHANGE);
  monitor := screen.MonitorFromPoint(rect.TopLeft).MonitorNum;
  bounds := GetMonitorBoundsRect(@monitor);

  // re-maximize maximized windows to make them match new workarea
  wp.length := sizeof(wp);
  h := FindWindow('Progman', nil);
  h := GetWindow(h, GW_HWNDPREV);
	while h <> 0 do
	begin
    // exclude hidden and tool windows
    if IsWindowVisible(h) and (GetWindowLong(h, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
    begin
        // update only maximized windows
        GetWindowPlacement(h, wp);
        if wp.showCmd = SW_SHOWMAXIMIZED then
        begin
          if PtInRect(bounds, wp.ptMaxPosition) then
          begin
            ShowWindow(h, SW_SHOWNORMAL);
            ShowWindow(h, SW_SHOWMAXIMIZED);
          end;
        end;
    end;
    h := GetWindow(h, GW_HWNDPREV);
	end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.ReserveScreenEdge(Percent: integer; Edge: TBaseSite);
var
  Changed: boolean;
  Position: integer;
  WorkArea, Bounds: Windows.TRect;
begin
  if assigned(ItemMgr) then
  try
    Changed := false;
    WorkArea := GetMonitorWorkareaRect;
    Bounds := GetMonitorBoundsRect;

    if Edge = bsLeft then
    begin
      if sets.container.AutoHide then Position := 0
      else Position := ItemMgr.FBaseWindowRect.Width * Percent div 100;
      if WorkArea.Left <> Position then
      begin
        WorkArea.Left := Position;
        Changed := true;
      end;
    end
    else
    if Edge = bsTop then
    begin
      if sets.container.AutoHide then Position := 0
      else Position := ItemMgr.FBaseWindowRect.Height * Percent div 100;
      if WorkArea.Top <> Position then
      begin
        WorkArea.Top := Position;
        Changed := true;
      end;
    end
    else
    if Edge = bsRight then
    begin
      if sets.container.AutoHide then Position := Bounds.Right
      else Position := Bounds.Right - ItemMgr.FBaseWindowRect.Width * Percent div 100;
      if WorkArea.Right <> Position then
      begin
        WorkArea.Right := Position;
        Changed := true;
      end;
    end
    else
    if Edge = bsBottom then
    begin
      if sets.container.AutoHide then Position := Bounds.Bottom
      else Position := Bounds.Bottom - ItemMgr.FBaseWindowRect.Height * Percent div 100;
      if WorkArea.Bottom <> Position then
      begin
        WorkArea.Bottom := Position;
        Changed := true;
      end;
    end;

    if Changed then SetWorkarea(WorkArea);
  except
    on e: Exception do raise Exception.Create('Base.ReserveScreenEdge'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.UnreserveScreenEdge(Edge: TBaseSite);
var
  Changed: boolean;
  WorkArea, Bounds: Windows.TRect;
begin
  try
    Changed := false;
    WorkArea := GetMonitorWorkareaRect;
    Bounds := GetMonitorBoundsRect;

    if Edge = bsLeft then
    begin
      if WorkArea.Left <> 0 then
      begin
        WorkArea.Left := 0;
        Changed := true;
      end;
    end
    else
    if Edge = bsTop then
    begin
      if WorkArea.Top <> 0 then
      begin
        WorkArea.Top := 0;
        Changed := true;
      end;
    end
    else
    if Edge = bsRight then
    begin
      if WorkArea.Right <> Bounds.Right then
      begin
        WorkArea.Right := Bounds.Right;
        Changed := true;
      end;
    end
    else
    if Edge = bsBottom then
    begin
      if WorkArea.Bottom <> Bounds.Bottom then
      begin
        WorkArea.Bottom := Bounds.Bottom;
        Changed := true;
      end;
    end;

    if Changed then SetWorkarea(WorkArea);
  except
    on e: Exception do raise Exception.Create('Base.UnreserveScreenEdge'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDragEnter(list: TStrings; hWnd: uint);
begin
  KillTimer(Handle, ID_TIMER_DRAGLEAVE); // stop tracing mouse pointer
  ItemMgr.DragEnter;
  BasePaint(1);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDragOver;
begin
  ItemMgr.DragOver;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDragLeave;
begin
  // do not actually take OnDragLeave actions
  // but start to trace mouse pointer
  SetTimer(Handle, ID_TIMER_DRAGLEAVE, 200, nil);
end;
//------------------------------------------------------------------------------
// trace mouse pointer until it actually leaves the dock area
procedure Tfrmmain.OnTimerDragLeave;
var
  pt: windows.TPoint;
begin
  GetCursorPos(pt);
  if not PtInRect(ItemMgr.GetRect, pt) then
  begin
    KillTimer(Handle, ID_TIMER_DRAGLEAVE);
    ItemMgr.DragLeave;
    BasePaint(1);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDrop(files: TStrings; hWnd: uint);
var
  pt: Windows.TPoint;
begin
  try
    KillTimer(Handle, ID_TIMER_DRAGLEAVE); // stop tracing mouse pointer
    Windows.GetCursorPos(pt);
    if files.Count > 0 then
      if not ItemMgr.ItemDropFiles(hWnd, pt, files) then DropFiles(files);
    ItemMgr.DragLeave;
    ItemMgr.WHMouseMove(pt);
    SetActiveWindow(handle);
    SetForegroundWindow(handle); // set foreground to finalize dragdrop operation
  except
    on e: Exception do err('Base.OnDrop', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DropFiles(files: TStrings);
var
  idx: integer;
begin
  for idx := 0 to files.Count - 1 do files.strings[idx] := TShortcutItem.FromFile(files.strings[idx]);
  ItemMgr.InsertItems(files);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMCopyData(var Message: TMessage);
var
  pcds: PCOPYDATASTRUCT;
  ppd: PProgramData;
begin
  message.Result := 1;
  pcds := PCOPYDATASTRUCT(message.lParam);

  if pcds^.dwData = DATA_PROGRAM then
  begin
    if pcds^.cbData <> sizeof(TProgramData) then
    begin
      message.Result := 0;
      notify(UTF8ToAnsi(XErrorInvalidProgramDataStructureSize));
      exit;
    end;
    ppd := PProgramData(pcds^.lpData);
    ItemMgr.InsertItem(TShortcutItem.Make(0, pchar(ppd^.Name), ZipPath(pchar(ppd^.Filename)), '', '', '', SW_SHOWNORMAL));
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.AddFile;
begin
  with TOpenDialog.Create(self) do
  begin
    if Execute then AddFile(Filename);
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.AddFile(Filename: string);
begin
  if assigned(ItemMgr) then
    ItemMgr.InsertItem(TShortcutItem.Make(0, ChangeFileExt(ExtractFilename(Filename), ''),
      toolu.ZipPath(Filename), '', toolu.ZipPath(ExtractFilePath(Filename)), '', 1));
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.NewDock;
begin
  if assigned(docks) then
  begin
    docks.Enum;
    if docks.Count < 8 then
      if docks.HaveFreeSite then docks.NewDock;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.RemoveDock;
begin
  if assigned(docks) then
  begin
    docks.RequestRemoveDock(sets.SetsPathFile);
    execute_cmdline('/quit');
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OpenWith(filename: string);
var
  _file: string;
begin
  _file := toolu.UnzipPath(filename);
  if not fileexists(_file) then _file := toolu.FindFile(_file);
  ShellExecute(application.mainform.handle, nil, 'rundll32.exe', pchar('shell32.dll,OpenAs_RunDLL ' + _file), nil, 1);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMMouseWheel(var msg: TWMMouseWheel);
begin
  if msg.WheelDelta < 0 then TMixer.CInc(-1) else if msg.WheelDelta > 0 then TMixer.CInc(1);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMDisplayChange(var Message: TMessage);
begin
  screen.UpdateMonitors;
  sets.StoreParam(gpMonitor, max(sets.GetParam(gpMonitor), revertMonitor));
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMSettingChange(var Message: TMessage);
begin
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMCompositionChanged(var Message: TMessage);
begin
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
function Tfrmmain.FullScreenAppActive(HWnd: HWND): boolean;
const
  clsPM = 'Progman';
  clsWW = 'WorkerW';
  clsAVP = 'AVP.SandboxBorderWindow';
var
  wnd: hWnd;
  rc, rMonitor: windows.TRect;
  cls: array [0..10] of char;
begin
  result := false;
  rMonitor := ItemMgr.FMonitorRect;
  wnd := GetWindow(Handle, GW_HWNDFIRST);
  while wnd <> 0 do
  begin
    if not IsWindow(wnd) then exit
    else
    if IsWindowVisible(Wnd) then
    begin
        GetWindowRect(wnd, rc);
        if (GetWindowLong(wnd, GWL_STYLE) and WS_CAPTION = 0) and
          (rc.Left <= rMonitor.Left) and (rc.Top <= rMonitor.Top) and (rc.Right >= rMonitor.Right) and (rc.Bottom >= rMonitor.Bottom) then
        begin
          GetClassName(wnd, cls, 20);
          result := (strlcomp(@cls, clsPM, 7) <> 0) and (strlcomp(@cls, clsWW, 7) <> 0) and (strlcomp(@cls, clsAVP, 7) <> 0);
          exit;
        end;
    end;
    wnd := GetWindow(wnd, GW_HWNDNEXT);
  end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.ListFullScreenApps: string;
var
  wnd: hWnd;
  rc, rMonitor: windows.TRect;
  cls: array [0..MAX_PATH - 1] of char;
begin
  result := '';
  rMonitor := ItemMgr.FMonitorRect;
  wnd := GetWindow(Handle, GW_HWNDFIRST);
  while wnd <> 0 do
  begin
    if IsWindowVisible(Wnd) then
    begin
        GetWindowRect(wnd, rc);
        if (GetWindowLong(wnd, GWL_STYLE) and WS_CAPTION = 0) and
          (rc.Left <= rMonitor.Left) and (rc.Top <= rMonitor.Top) and (rc.Right >= rMonitor.Right) and (rc.Bottom >= rMonitor.Bottom) then
        begin
          result := result + 'HWnd = ' + inttostr(wnd) + #13#10;
          FillChar(cls, MAX_PATH, #0);
          GetClassName(wnd, cls, MAX_PATH);
          result := result + 'Class = ' + strpas(@cls) + #13#10;
          FillChar(cls, MAX_PATH, #0);
          GetWindowText(wnd, cls, MAX_PATH);
          result := result + 'Text = ' + strpas(@cls) + #13#10;
          result := result + 'Rect = ' + inttostr(rc.Left) + ', ' + inttostr(rc.Top) + ', ' + inttostr(rc.Right) + ', ' + inttostr(rc.Bottom) + #13#10;
          FillChar(cls, MAX_PATH, #0);
          GetWindowModuleFileName(wnd, cls, MAX_PATH);
          result := result + 'Module = ' + strpas(@cls) + #13#10#13#10;
        end;
    end;
    wnd := GetWindow(wnd, GW_HWNDNEXT);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
var
  acmd, aparams, adir: string;
begin
  try
    while cmd <> '' do
    begin
      acmd := trim(fetch(cmd, ';', true));
      aparams := trim(fetch(params, ';', true));
      adir := trim(fetch(dir, ';', true));
      execute(acmd, aparams, adir, showcmd, hwnd);
    end;
  except
    on e: Exception do raise Exception.Create(
        'Command parse/execute error.'#10#13 +
        'Command=' + acmd + #10#13 + 'Params=' + aparams + #10#13 + 'SYSMSG=' + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.execute_cmdline(cmd: string; showcmd: integer = 1);
var
  params: string;
begin
  if cmd[1] = '/' then
  begin
    split(cmd, '(', cmd, params);
    params := cuttolast(params, ')');
  end
  else
  begin
    split_cmd(cmd, cmd, params);
  end;
  execute(cmd, params, '', showcmd);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
var
  cmd2: string;
  i, i1, i2, i3, i4: integer;
  str1, str2: string;
  lpsz1, lpsz2: pchar;
  pt: windows.TPoint;
  mii: TMenuItemInfo;
  mname: array [0..MAX_PATH - 1] of char;
  list: TStrings;
begin
  if cmd = '' then exit;

  if cmd[1] <> '/' then
  begin
    Run(cmd, params, dir, showcmd);
    exit;
  end;

  system.Delete(cmd, 1, 1);

  params := toolu.UnzipPath(params);
  cmd2 := cutafter(cmd, '.');

  if cut(cmd, '.') = 'itemmgr' then frmmain.ItemMgr.command(cmd2, params)
  else if cmd = 'quit' then frmmain.CloseProgram
  else if cmd = 'hide' then frmmain.BaseCmd(tcSetVisible, 0)
  else if cmd = 'say' then frmmain.notify(toolu.UnzipPath(params))
  else if cmd = 'alert' then frmmain.alert(toolu.UnzipPath(params))
  else if cmd = 'togglevisible' then frmmain.BaseCmd(tcToggleVisible, 0)
  else if cmd = 'togglesystaskbar' then frmmain.BaseCmd(tcToggleTaskbar, 0)
  else if cmd = 'sets' then
  begin
    if not trystrtoint(params, i) then i := 0;
    Tfrmsets.Open(i);
  end
  else if cmd = 'cmd' then Tfrmcmd.Open
  else if cmd = 'collection' then Run('%pp%\collection.exe')
  else if cmd = 'apps' then Run('%pp%\apps.exe')
  else if cmd = 'taskmgr' then Run('%sysdir%\taskmgr.exe')
  else if cmd = 'undelete' then
  begin
    if assigned(ItemMgr) then ItemMgr.UnDelete;
  end
  else if cmd = 'program' then AddFile
  else if cmd = 'newdock' then NewDock
  else if cmd = 'removedock' then RemoveDock
  else if cmd = 'command' then TfrmAddCommand.Open
  else if cmd = 'hello' then TfrmHello.Open
  else if cmd = 'help' then TfrmTip.Open
  else if cmd = 'backup' then sets.Backup
  else if cmd = 'restore' then sets.Restore
  else if cmd = 'paste' then ItemMgr.InsertItem(GetClipboard)
  else if cmd = 'tray' then Tray.ShowTrayOverflow(sets.container.Site, hwnd, ItemMgr.GetRect, GetMonitorWorkareaRect)
  else if cmd = 'volume' then Tray.ShowVolumeControl(sets.container.Site, hwnd, ItemMgr.GetRect, GetMonitorWorkareaRect)
  else if cmd = 'networks' then Tray.ShowNetworks(sets.container.Site, hwnd, ItemMgr.GetRect, GetMonitorWorkareaRect)
  else if cmd = 'battery' then Tray.ShowBattery(sets.container.Site, hwnd, ItemMgr.GetRect, GetMonitorWorkareaRect)
  else if cmd = 'startmenu' then StartMenu.Show(sets.container.Site, hwnd, ItemMgr.GetRect, GetMonitorWorkareaRect)
  else if cmd = 'theme' then // themes popup menu
  begin
    GetCursorPos(pt);
    i := CreatePopupMenu;
    AppendMenu(i, MF_STRING, $f000, pchar(UTF8ToAnsi(XOpenThemesFolder)));
    AppendMenu(i, MF_SEPARATOR, 0, pchar('-'));
    theme.ThemesMenu(pchar(sets.container.ThemeName), i);
    LockMouseEffect(Handle, true);
    SetForegroundWindow(handle);
    SetForeground;
    i1 := integer(TrackPopupMenuEx(i, TPM_RETURNCMD, pt.x, pt.y, handle, nil));
    LockMouseEffect(Handle, false);
    if i1 = $f000 then execute_cmdline(theme.ThemesFolder)
    else
    if i1 <> 0 then
    begin
      FillChar(mii, sizeof(mii), #0);
      mii.cbSize := sizeof(mii);
      mii.dwTypeData := @mname;
      mii.cch := MAX_PATH;
      mii.fMask := MIIM_STRING;
      GetMenuItemInfo(i, i1, false, @mii);
      str1 := pchar(@mname);
      if str1 <> '' then setTheme(str1);
    end;
  end
  else if cmd = 'taskspot' then
  begin
    if assigned(ItemMgr) then
    begin
      i := ItemMgr.ItemIndex(hwnd);
      if i = ItemMgr.FItemCount - 1 then i := -1;
      SetParam(gpTaskSpot, i);
    end;
  end
  else if cmd = 'autotray' then Tray.SwitchAutoTray
  else if cmd = 'themeeditor' then TfrmThemeEditor.Open
  else if cmd = 'lockdragging' then SetParam(gpLockDragging, ifthen(sets.GetParam(gpLockDragging) = 0, 1, 0))
  else if cmd = 'site' then SetParam(gpSite, integer(StringToSite(params)))
  else if cmd = 'logoff' then    ProcessHelper.Shutdown(ifthen(params = 'force', 4, 0))
  else if cmd = 'shutdown' then  ProcessHelper.Shutdown(ifthen(params = 'force', 5, 1))
  else if cmd = 'reboot' then    ProcessHelper.Shutdown(ifthen(params = 'force', 6, 2))
  else if cmd = 'suspend' then   ProcessHelper.SetSuspendState(false)
  else if cmd = 'hibernate' then ProcessHelper.SetSuspendState(true)
  else if cmd = 'kill' then      ProcessHelper.Kill(params)
  else if cmd = 'displayoff' then sendmessage(handle, WM_SYSCOMMAND, SC_MONITORPOWER, 2)
  else if cmd = 'emptybin' then
  begin
    SHEmptyRecycleBin(Handle, nil, 0);
  end
  else if cmd = 'regp' then
  begin
    for i := 0 to ItemMgr._registeredPrograms.Count - 1 do notify(ItemMgr._registeredPrograms.Strings[i]);
  end
  else if cmd = 'setdisplaymode' then
  begin
    if not trystrtoint(Trim(fetch(params, ',', true)), i1) then i1 := 1024;
    if not trystrtoint(Trim(fetch(params, ',', true)), i2) then i2 := 768;
    if not trystrtoint(Trim(fetch(params, ',', true)), i3) then i3 := 32;
    if not trystrtoint(Trim(fetch(params, ',', true)), i4) then i4 := 60;
    setdisplaymode(i1, i2, i3, i4);
  end
  else if cmd = 'bsm' then
  begin
    if not trystrtoint(Trim(fetch(params, ',', true)), i1) then i1 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i2) then i2 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i3) then i3 := 0;
    bsm(i1, i2, i3);
  end
  else if cmd = 'wfp' then
  begin
    GetCursorPos(pt);
    inta := WindowFromPoint(pt);
    SetClipboard(inttohex(inta, 8));
  end
  else if cmd = 'findwindow' then
  begin
    str1 := Trim(fetch(params, ',', true));
    str2 := Trim(fetch(params, ',', true));
    if str1 = '' then lpsz1 := nil else lpsz1 := pchar(str1);
    if str2 = '' then lpsz2 := nil else lpsz2 := pchar(str2);
    inta := findwindow(lpsz1, lpsz2);
    SetClipboard(inttohex(inta, 8));
  end
  else if cmd = 'findwindowex' then
  begin
    str1 := Trim(fetch(params, ',', true));
    str2 := Trim(fetch(params, ',', true));
    if str1 = '' then lpsz1 := nil else lpsz1 := pchar(str1);
    if str2 = '' then lpsz2 := nil else lpsz2 := pchar(str2);
    inta := findwindowex(inta, 0, lpsz1, lpsz2);
    SetClipboard(inttohex(inta, 8));
  end
  else if cmd = 'showwindow' then showwindow(uint(inta), strtoint(params))
  else if cmd = 'sendmessage' then
  begin
    if not trystrtoint(Trim(fetch(params, ',', true)), i2) then i2 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i3) then i3 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i4) then i4 := 0;
    sendmessage(uint(inta), uint(i2), i3, i4);
  end
  else if cmd = 'winamp' then
  begin
    if SameText(params, 'play') then
      if not boolean(FindWinamp) then LaunchWinamp(showcmd) else toolu.wacmd(40045);
    if SameText(params, 'pause') then toolu.wacmd(40046);
    if SameText(params, 'stop') then toolu.wacmd(40047);
    if SameText(params, 'previous') then toolu.wacmd(40044);
    if SameText(params, 'next') then toolu.wacmd(40048);

    if SameText(params, 'close') then toolu.wacmd(40001);
    if SameText(params, 'preferences') then toolu.wacmd(40012);
    if SameText(params, 'open_file') then toolu.wacmd(40029);
    if SameText(params, 'stop_after_current') then toolu.wacmd(40157);
    if SameText(params, 'visualization') then toolu.wacmd(40192);
    if SameText(params, 'start_of_playlist') then toolu.wacmd(40154);
    if SameText(params, 'end_of_playlist') then toolu.wacmd(40158);
  end
  else if cmd = 'play' then sndPlaySound(pchar(UnzipPath(params)), SND_ASYNC or SND_FILENAME)
  else if cmd = 'guid' then SetClipboard(CreateClassId);
end;
//------------------------------------------------------------------------------
// runner thread function
function RunThread(p: pointer): PtrInt;
var
  Data: PRunData absolute p;
  params, dir: pchar;
  hostHandle: THandle;
begin
  hostHandle := Data.handle;
  params := nil;
  dir := nil;
  if pchar(@Data.params) <> '' then params := PChar(@Data.params);
  if pchar(@Data.dir) <> '' then dir := PChar(@Data.dir);
  shellexecute(hostHandle, nil, pchar(@Data.exename), params, dir, Data.showcmd);
  Dispose(Data);
  // request main form to close thread handle
  postmessage(hostHandle, WM_APP_RUN_THREAD_END, 0, LPARAM(GetCurrentThread));
end;
//------------------------------------------------------------------------------
// creates a new thread to run a program
procedure Tfrmmain.Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
var
  Data: PRunData;
  pparams, pdir: pchar;
  shell: string;
  sei: TShellExecuteInfoA;
  aPIDL: PItemIDList;
begin
  try
    exename := toolu.UnzipPath(exename);
    if params <> '' then params := toolu.UnzipPath(params);
    if dir <> '' then dir := toolu.UnzipPath(dir);

    if directoryexists(exename) then
    begin
      shell := toolu.UnzipPath(sets.container.Shell);
      if sets.container.useShell and fileexists(shell) then
      begin
        params := '"' + exename + '"';
        exename := shell;
        dir := '';
      end;
    end;

    aPIDL := nil;
    if IsGUID(exename) then aPIDL := PIDL_GetFromPath(pchar(exename));
    if IsPIDLString(exename) then aPIDL := PIDL_FromString(exename);
    if assigned(aPIDL) then
    begin
	    sei.cbSize := sizeof(sei);
	    sei.lpIDList := aPIDL;
	    sei.Wnd := Handle;
	    sei.nShow := 1;
	    sei.lpVerb := 'open';
	    sei.lpFile := nil;
	    sei.lpParameters := nil;
	    sei.lpDirectory := nil;
	    sei.fMask := SEE_MASK_IDLIST;
	    ShellExecuteExA(@sei);
      PIDL_Free(aPIDL);
      exit;
		end;

    if sets.container.RunInThread then
    begin
	    New(Data);
	    Data.handle := Handle;
	    strcopy(pchar(@Data.exename), pchar(exename));
	    strcopy(pchar(@Data.params), pchar(params));
	    strcopy(pchar(@Data.dir), pchar(dir));
	    Data.showcmd := showcmd;
	    if BeginThread(RunThread, Data) = 0 then notify('Run.BeginThread failed');
    end else begin
      pparams := nil;
      pdir := nil;
      if params <> '' then pparams := PChar(params);
      if dir <> '' then pdir := PChar(dir);
      shellexecute(Handle, nil, pchar(exename), pparams, pdir, showcmd);
    end;
  except
    on e: Exception do err('Base.Run', e);
  end;
end;
//------------------------------------------------------------------------------
end.

