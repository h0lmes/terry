unit frmterryu;

interface

uses
  jwaWindows, Windows, Messages, SysUtils, Classes, Controls, LCLType, Forms,
  Menus, Dialogs, ExtCtrls, ShellAPI, ComObj, Math, Syncobjs, MMSystem, LMessages,
  declu, GDIPAPI, gdip_gfx, dwm_unit, hintu,
  itemmgru, DropTgtU, notifieru, setsu, traycontrolleru;

type
  { Tfrmterry }

  Tfrmterry = class(TForm)
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
    function CloseQuery: integer;
    procedure MaintainNotForeground;
    procedure RegisterRawInput;
    procedure NativeWndProc(var message: TMessage);
    procedure CreateZOrderWindow;
    procedure AppException(Sender: TObject; e: Exception);
    procedure AppDeactivate(Sender: TObject);
    procedure WMCopyData(var Message: TMessage);
    procedure WMTimer(var msg: TMessage);
    procedure WMUser(var msg: TMessage);
    procedure WMCommand(var msg: TMessage);
    procedure WMDisplayChange(var Message: TMessage);
    procedure WMSettingChange(var Message: TMessage);
    procedure WMMouseWheel(var msg: TWMMouseWheel);
    procedure WMCompositionChanged(var Message: TMessage);
    procedure WHMouseMove(LParam: LParam);
    procedure WHButtonDown(button: integer);
    procedure MouseEnter;
    procedure MouseLeave;
    procedure UpdateRunning;
    procedure UpdateRunningI;
    procedure HideTaskbar(Hide: boolean);
    procedure ReserveScreenEdge(Reserve: boolean; Percent: integer; Edge: TBaseSite);
    procedure UnreserveScreenEdge(Edge: TBaseSite);

    procedure BaseSaveSettings;
    procedure DoMenu(mit: integer);
    procedure BaseDraw(flags: integer);
  public
    ItemMgr: _ItemManager;
    DropMgr: _DropManager;
    AHint: _Hint;
    Tray: _TrayController;

    OldBaseWindowRect: GDIPAPI.TRect;
    OldBaseImageRect: GDIPAPI.TRect;

    MonitorRect: Windows.TRect;

    LastMouseHookPoint: Windows.TPoint;
    MouseOver: boolean;
    InitDone: boolean;

    hHook: THandle;
    hMenu, hMenuCreate: uint;

    procedure Init;
    procedure ExecAutorun;
    procedure ApplyParams;
    procedure Help;
    procedure SetForeground;
    procedure SetNotForeground;
    procedure err(where: string; e: Exception);
    procedure notify(message: string; silent: boolean = False);
    procedure alert(message: string);
    procedure ActivateHint(hwnd: uint; caption_: string; x, y, direction: integer);
    procedure DeactivateHint(hwnd: uint);
    function BaseCmd(id: TGParam; param: integer): integer;
    procedure SetParam(id: TGParam; Value: integer);
    function GetHMenu(ParentMenu: uint): uint;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure SetFont(var Value: _FontData);
    procedure SetStackFont(var Value: _FontData);
    procedure LockMouseEffect(hWnd: HWND; lock: boolean);
    function IsLockedMouseEffect: boolean;
    function GetMonitorWorkareaRect: Windows.TRect;
    function GetMonitorBoundsRect: Windows.TRect;
    procedure MoveDock(iDirection: integer);
    procedure TimerMain;
    procedure TimerSlow;
    procedure TimerFSA;
    procedure OnDragEnter(list: TStrings; hWnd: uint);
    procedure OnDragOver;
    procedure OnDragLeave;
    procedure OnDrop(files: TStrings; hWnd: uint);
    procedure DropFiles(files: TStrings);
    procedure AddFile; overload;
    procedure AddFile(Filename: string); overload;
    procedure OpenWith(filename: string);
    function FullScreenAppActive(HWnd: HWND): boolean;
    function ListFullScreenApps: string;
    procedure mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
    procedure execute_cmdline(cmd: string; showcmd: integer = 1);
    procedure execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
    procedure Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
  end;

var frmterry: Tfrmterry;

implementation
uses themeu, toolu, scitemu, PIDL, dockh, frmsetsu, frmcmdu, frmitemoptu,
  frmAddCommandU, frmthemeeditoru, dropindicatoru, processhlp;
{$R *.lfm}
{$R Resource\res.res}
//------------------------------------------------------------------------------
procedure Tfrmterry.Init;
var
  i: integer;
  load_err: boolean;
  theFile: string;
begin
  try
    closing := False;
    saving := False;
    AllowClose := false;
    PrevBlur := False;
    InitDone := False;
    Application.OnException := AppException;
    Application.OnDeactivate := AppDeactivate;
    trayicon.Icon := application.Icon;
    LockList := TList.Create;
    crsection := TCriticalSection.Create;

    AddLog('Init.CreateZOrderWindow');
    CreateZOrderWindow;

    // workaround for Windows message handling in LCL //
    AddLog('Init.NativeWndProc');
    FWndInstance := MakeObjectInstance(NativeWndProc);
    FPrevWndProc := Pointer(GetWindowLongPtr(Handle, GWL_WNDPROC));
    SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FWndInstance));

    // hook //
    //theFile := UnzipPath('%pp%\hook.dll');
    //hHook := 0;
    //if FileExists(theFile) then hHook := LoadLibrary(pchar(theFile));

    // Sets //
    AddLog('Init.Sets');
    theFile := UnzipPath('%pp%\sets.ini');
    i := 1;
    while i <= ParamCount do
    begin
      if strlicomp(pchar(ParamStr(i)), '-s', 2) = 0 then theFile := UnzipPath(copy(ParamStr(i), 3, MAX_PATH));
      inc(i);
    end;
    AddLog('Init.Sets.Create');
    sets := _Sets.Create(theFile, UnzipPath('%pp%'), Handle, BaseCmd);
    AddLog('Init.Sets.Load');
    sets.Load;

    // Theme //
    AddLog('Init.Theme');
    theme := _Theme.Create(BaseCmd);
    if not theme.Load then
    begin
      notify(UTF8ToAnsi(XErrorLoadTheme + ' ' + XErrorContactDeveloper));
      AddLog('Halt');
      halt;
    end;

    // drop indicator //
    //AddLog('Init.CreateDropIndicator');
    //TDropIndicator.CreateIndicator;

    // create ItemManager (disabled, not visible) //
    AddLog('Init.ItemManager');
    ItemMgr := _ItemManager.Create(false, false, Handle, BaseCmd);

    // load items //
    AddLog('Init.ApplyParams');
    ApplyParams;
    AddLog('Init.LoadItems');
    load_err := false;
    try ItemMgr.Load(sets.SetsPathFile);
    except load_err := true;
    end;
    if load_err then
    begin
      AddLog('Init.Restore');
      if sets.Restore then
      begin
        messagebox(handle, pchar(UTF8ToAnsi(XErrorSetsCorrupted + ' ' + XMsgSetsRestored)), 'Terry', MB_ICONEXCLAMATION);
        AddLog('Halt');
        halt;
        AddLog('Init.Restore.SetsLoad');
        sets.Load;
        AddLog('Init.Restore.ApplyParams');
        ApplyParams;
        AddLog('Init.Restore.LoadItems');
        ItemMgr.Clear;
        ItemMgr.Load(sets.SetsPathFile);
      end else begin
        AddLog('Init.RestoreFailed');
        messagebox(handle,
          pchar(UTF8ToAnsi(XErrorSetsCorrupted + ' ' + XErrorSetsRestoreFailed + ' ' + XErrorContactDeveloper)),
          'Terry', MB_ICONERROR);
      end;
    end else begin
      if not sets.Backup then
      begin
        AddLog('Init.BackupFailed');
        messagebox(handle, pchar(UTF8ToAnsi(XErrorSetsBackupFailed)), 'Terry', MB_ICONERROR);
      end;
    end;
    // loaded, so enable ItemMgr //
    ItemMgr.Enable(true);

    // timers //
    AddLog('Init.Timers');
    SetTimer(handle, ID_TIMER, 10, nil);
    SetTimer(handle, ID_TIMER_SLOW, 1000, nil);
    SetTimer(handle, ID_TIMER_FSA, 2000, nil);

    // tray controller //
    AddLog('Init.TrayController');
    Tray := _TrayController.Create;

    // if needed make it 'RolledDown' on startup
    sets.RollDown;
    sets.wndOffset := sets.wndOffsetTarget;

    // show the panel and items //
    AddLog('Init.ItemMgr.Visible');
    BaseCmd(tcSetVisible, 1);
    if sets.GetParam(gpStayOnTop) <> 0 then SetParam(gpStayOnTop, 1);

    // RawInput (replacement for hook) //
    AddLog('Init.RegisterRawInput');
    RegisterRawInput;

    // create DropManager //
    AddLog('Init.DropManager');
    DropMgr := _DropManager.Create(Handle);
    DropMgr.OnDrop := OnDrop;
    DropMgr.OnDragEnter := OnDragEnter;
    DropMgr.OnDragOver := OnDragOver;
    DropMgr.OnDragLeave := OnDragLeave;

    // first time draw items by applying the theme //
    BaseCmd(tcThemeChanged, 0);

    // propose to add installed programs onto the dock //
    if ItemMgr.FirstRun then
      if idYes = MessageBox(Handle, pchar(UTF8ToAnsi(XMsgFirstRun + ' ' + XMsgAddMorePrograms)), 'Terry', mb_yesno) then
      begin
        execute_cmdline('/itemmgr.separator');
        execute_cmdline('/apps');
      end;

    InitDone := True;
  except
    on e: Exception do err('Base.Init', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.ExecAutorun;
var
  i: integer;
begin
  try
      if assigned(sets.AutoRunList) then
      begin
          i := 0;
          while i < sets.AutoRunList.Count do
          begin
              if sets.AutoRunList.strings[i] <> '' then
              begin
                  if copy(sets.AutoRunList.strings[i], 1, 1) = '#' then
                    execute_cmdline(copy(sets.AutoRunList.strings[i], 2, length(sets.AutoRunList.strings[i])), SW_MINIMIZE)
                  else
                    execute_cmdline(sets.AutoRunList.strings[i]);
              end;
              application.processmessages;
              inc(i);
          end;
      end;
  except
    on e: Exception do err('Base.ExecAutorun', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.CreateZOrderWindow;
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
function Tfrmterry.CloseQuery: integer;
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
      if assigned(DropMgr) then DropMgr.Destroy;
      if assigned(ItemMgr) then ItemMgr.Free;
      if assigned(ahint) then ahint.Free;
      if assigned(theme) then theme.Free;
      if assigned(sets) then sets.Free;
      if IsWindow(ZOrderWindow) then DestroyWindow(ZOrderWindow);
      LockList.free;
      SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FPrevWndProc));
      FreeObjectInstance(FWndInstance);
      //if hHook <> 0 then FreeLibrary(hHook);
      //TDropIndicator.DestroyIndicator;
    except
      on e: Exception do messagebox(handle, PChar(e.message), 'Terry.Base.Close.Free', mb_iconexclamation);
    end;
    AddLog('CloseQuery done');
    result := 1;
  finally
    crsection.Leave;
    crsection.free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseQuery = 0 then CloseAction := caNone else CloseAction := caFree;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.ApplyParams;
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

    ItemMgr.SetParam(gpItemSize, sets.container.itemsize);
    ItemMgr.SetParam(gpBigItemSize, sets.container.BigItemSize);
    ItemMgr.SetParam(gpZoomItems, integer(sets.container.ZoomItems));
    ItemMgr.SetParam(gpZoomWidth, sets.container.ZoomWidth);
    ItemMgr.SetParam(gpItemSpacing, sets.container.ItemSpacing);
    ItemMgr.SetParam(gpReflection, integer(sets.container.Reflection));
    ItemMgr.SetParam(gpReflectionSize, theme.ReflectionSize);
    ItemMgr.SetParam(gpLaunchInterval, sets.container.LaunchInterval);
    ItemMgr.SetParam(gpActivateRunning, integer(sets.container.ActivateRunning));
    ItemMgr.SetParam(gpItemAnimation, sets.container.ItemAnimation);
    ItemMgr.SetParam(gpLockDragging, integer(sets.container.LockDragging));
    ItemMgr.SetParam(gpShowHint, integer(sets.container.ShowHint));
    ItemMgr.SetParam(gpShowRunningIndicator, integer(sets.container.ShowRunningIndicator));

    ItemMgr.SetTheme;
  except
    on e: Exception do err('Base.ApplyParams', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.BaseSaveSettings;
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
      sets.SaveEx;
      ItemMgr.Save(sets.SetsPathFile);
      sets.SaveEx2;
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
function Tfrmterry.BaseCmd(id: TGParam; param: integer): integer;
begin
  Result := 0;

  case id of
    tcRepaintBase: BaseDraw(param);
    tcMenu: DoMenu(param);
    tcSaveSets: BaseSaveSettings;
    tcZOrder: SetForeground;
    tcThemeChanged:
      begin
        ItemMgr.SetParam(gpReflectionSize, theme.ReflectionSize);
        if assigned(ItemMgr) then ItemMgr.SetTheme;
      end;
    tcSetVisible:
      begin
        if (param = 0) and assigned(ItemMgr) then ItemMgr.Visible := false;
        sets.Visible := boolean(param);
        Visible := boolean(param);
        { возможно поможет убрать глюк с проваливанием иногда некоторых значков под Терри }
        if boolean(param) then SetForeground;
        if (param <> 0) and assigned(ItemMgr) then ItemMgr.Visible := true;
      end;
    tcToggleVisible: BaseCmd(tcSetVisible, integer(not Visible));
    tcToggleTaskbar: frmterry.SetParam(gpHideTaskBar, ifthen(sets.GetParam(gpHideTaskBar) = 0, 1, 0));
    tcGetVisible: Result := integer(sets.Visible);
    tcGetDragging: if assigned(ItemMgr) then Result := integer(ItemMgr.Dragging);
    tcApplyParams: ApplyParams;
    tcQuit:
      begin
        AllowClose := true;
        Close;
      end;
  end;
end;
//------------------------------------------------------------------------------
// stores given parameter value and propagates it to subsequent objects       //
// e.g. ItemManager and all items                                             //
procedure Tfrmterry.SetParam(id: TGParam; value: integer);
begin
  value := sets.StoreParam(id, value);

  case id of
    gpSite:                   if assigned(theme) then theme.ReloadGraphics;
    gpCenterOffsetPercent:    ItemMgr.ItemsChanged;
    gpEdgeOffset:             ItemMgr.ItemsChanged;
    gpAutoHide:               if value = 0 then sets.Rollup;
    gpHideTaskBar:            HideTaskbar(value <> 0);
    gpReserveScreenEdge:
      begin
        if value = 0 then UnreserveScreenEdge(sets.container.Site)
        else ReserveScreenEdge(true, sets.container.ReserveScreenEdgePercent, sets.container.Site);
      end;
    gpStayOnTop:              if value <> 0 then SetForeground else SetNotForeground;
    gpBaseAlpha:              BaseDraw(1);
    gpBlur:                   BaseDraw(1);
    gpTaskbar:                if value = 0 then ItemMgr.ClearTaskbar;
    gpShowRunningIndicator:   if value <> 0 then UpdateRunningI;
  end;

  if assigned(ItemMgr) then ItemMgr.SetParam(id, value);

  // placed after ItemMgr because WHMouseMove locked while mouse is locked //
  if id = gpLockMouseEffect then WHMouseMove(0);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.RegisterRawInput;
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
procedure Tfrmterry.NativeWndProc(var message: TMessage);
var
  dwSize: uint;
  ri: RAWINPUT;
begin
  case message.msg of
    WM_INPUT:
    begin
      dwSize := 0;
      GetRawInputData(message.lParam, RID_INPUT, nil, dwSize, sizeof(RAWINPUTHEADER));
      if GetRawInputData(message.lParam, RID_INPUT, @ri, dwSize, sizeof(RAWINPUTHEADER)) <> dwSize then
        raise Exception.Create('Base.NativeWndProc. Invalid size of RawInputData');
      if (ri.header.dwType = RIM_TYPEMOUSE) then
      begin
        if ri.mouse.usButtonData and RI_MOUSE_LEFT_BUTTON_DOWN <> 0 then WHButtonDown(1);
        if ri.mouse.usButtonData and RI_MOUSE_RIGHT_BUTTON_DOWN <> 0 then WHButtonDown(2);
        WHMouseMove(0);
      end;
    end;
    WM_TIMER : WMTimer(message);
    WM_USER : WMUser(message);
    WM_COMMAND : WMCommand(message);
    WM_QUERYENDSESSION : message.Result := CloseQuery;
    WM_MOUSEWHEEL : WMMouseWheel(TWMMouseWheel(message));
    WM_COPYDATA : WMCopyData(message);
    WM_DISPLAYCHANGE : WMDisplayChange(message);
    WM_SETTINGCHANGE : WMSettingChange(message);
    WM_DWMCOMPOSITIONCHANGED : WMCompositionChanged(message);
  end;
  with message do result := CallWindowProc(FPrevWndProc, Handle, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WHButtonDown(button: integer);
begin
  if not IsLockedMouseEffect and not MouseOver and not sets.container.StayOnTop then SetNotForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WHMouseMove(LParam: LParam);
var
  pt: Windows.Tpoint;
  mon_rect: Windows.TRect;
  OldMouseOver: boolean;
begin
  crsection.Acquire;
  try
    if IsLockedMouseEffect or not assigned(ItemMgr) or not IsWindowVisible(Handle) or closing then exit;

    Windows.GetCursorPos(pt);
    if (pt.x <> LastMouseHookPoint.x) or (pt.y <> LastMouseHookPoint.y) or (LParam = $fffffff) then
    begin
      LastMouseHookPoint.x := pt.x;
      LastMouseHookPoint.y := pt.y;
      ItemMgr.WHMouseMove(pt, (LParam <> $fffffff) and not sets.IsHiddenDown);

      // detect mouse enter/leave //
      OldMouseOver := MouseOver;
      mon_rect := ItemMgr.MonitorRect;
      if sets.container.site = bsBottom then
        MouseOver := (pt.y >= mon_rect.Bottom - 1) and
          (pt.x >= ItemMgr.BaseWindowRect.X + ItemMgr.BaseImageRect.X) and (pt.x <= ItemMgr.BaseWindowRect.X + ItemMgr.BaseImageRect.X + ItemMgr.BaseImageRect.Width)
      else if sets.container.site = bsTop then
        MouseOver := (pt.y <= mon_rect.Top) and
          (pt.x >= ItemMgr.BaseWindowRect.X + ItemMgr.BaseImageRect.X) and (pt.x <= ItemMgr.BaseWindowRect.X + ItemMgr.BaseImageRect.X + ItemMgr.BaseImageRect.Width)
      else if sets.container.site = bsLeft then
        MouseOver := (pt.x <= mon_rect.Left) and
          (pt.y >= ItemMgr.BaseWindowRect.Y + ItemMgr.BaseImageRect.Y) and (pt.y <= ItemMgr.BaseWindowRect.Y + ItemMgr.BaseImageRect.Y + ItemMgr.BaseImageRect.Height)
      else if sets.container.site = bsRight then
        MouseOver := (pt.x >= mon_rect.Right - 1) and
          (pt.y >= ItemMgr.BaseWindowRect.Y + ItemMgr.BaseImageRect.Y) and (pt.y <= ItemMgr.BaseWindowRect.Y + ItemMgr.BaseImageRect.Y + ItemMgr.BaseImageRect.Height);
      MouseOver := MouseOver or ItemMgr.CheckMouseOn or ItemMgr.Dragging;

      if MouseOver and not OldMouseOver then MouseEnter;
      if not MouseOver and OldMouseOver then MouseLeave;
    end;
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.MouseEnter;
begin
  sets.MouseOver := True;
  sets.LastMouseEnterTime := GetTickCount;

  // set foreground if 'activate' option selected //
  if IsWindowVisible(handle) and sets.container.ActivateOnMouse then SetForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.MouseLeave;
begin
  sets.MouseOver := False;
  sets.LastMouseLeaveTime := GetTickCount;
  ItemMgr.DragLeave;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if button = mbRight then DoMenu(0);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.DoMenu;
var
  pt: Windows.TPoint;
begin
  Windows.GetCursorPos(pt);
  ContextMenu(pt);
end;
//------------------------------------------------------------------------------
function Tfrmterry.GetHMenu(ParentMenu: uint): uint;
var
  i: integer;
begin
  if IsMenu(hMenu) then DestroyMenu(hMenu);
  if ParentMenu = 0 then hMenu := CreatePopupMenu else hMenu := ParentMenu;
  hMenuCreate := CreatePopupMenu;

  if ParentMenu = 0 then
  if IsValidItemString(GetClipboard) then
  begin
    //AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
    AppendMenu(hMenu, MF_STRING, $f030, pchar(UTF8ToAnsi(XPaste)));
  end;

  // create submenu 'Add...' //

  AppendMenu(hMenuCreate, MF_STRING, $f023, pchar(UTF8ToAnsi(XSpecificIcons)));
  AppendMenu(hMenuCreate, MF_STRING, $f021, pchar(UTF8ToAnsi(XEmptyIcon)));
  AppendMenu(hMenuCreate, MF_STRING, $f022, pchar(UTF8ToAnsi(XFile)));
  AppendMenu(hMenuCreate, MF_STRING, $f026, pchar(UTF8ToAnsi(XInstalledApplication)));
  AppendMenu(hMenuCreate, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenuCreate, MF_STRING, $f024, pchar(UTF8ToAnsi(XSeparator)));
  if sets.GetPluginCount = -1 then sets.ScanPlugins;
  if sets.GetPluginCount > 0 then
  begin
    AppendMenu(hMenuCreate, MF_SEPARATOR, 0, '-');
    i := 0;
    while i < sets.GetPluginCount do
    begin
      AppendMenu(hMenuCreate, MF_STRING, $f041 + i, PChar(sets.GetPluginName(i)));
      Inc(i);
    end;
  end;

  // insert all menu items //
  if ParentMenu <> 0 then AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING + MF_POPUP, hMenuCreate, pchar(UTF8ToAnsi(XAddIcon)));
  AppendMenu(hMenu, MF_STRING + ifthen(sets.container.LockDragging, MF_CHECKED, 0), $f031, pchar(UTF8ToAnsi(XLockIcons)));
  AppendMenu(hMenu, MF_STRING, $f032, pchar(UTF8ToAnsi(XIconCollection)));
  AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING, $f033, pchar(UTF8ToAnsi(XTaskManager)));
  AppendMenu(hMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(hMenu, MF_STRING, $f034, pchar(UTF8ToAnsi(XProgramSettings)));
  AppendMenu(hMenu, MF_STRING, $f035, pchar(UTF8ToAnsi(XExit)));

  Result := hMenu;
end;
//------------------------------------------------------------------------------
function Tfrmterry.ContextMenu(pt: Windows.TPoint): boolean;
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
procedure Tfrmterry.WMCommand(var msg: TMessage);
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
        $f026: cmd := '/apps';

        $f030: cmd := '/itemmgr.paste';
        $f031: cmd := '/lockdragging';
        $f032: cmd := '/collection';
        $f033: cmd := '/taskmgr';
        $f034: cmd := '/sets';
        $f035: cmd := '/quit';
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
procedure Tfrmterry.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if shift = [] then
  begin
    if key = 112 {F1} then Help;
    if key = 192 {tilda} then Tfrmcmd.StartForm;
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
    if key = 90 {Ctrl+Z} then
      if assigned(ItemMgr) then ItemMgr.UnDelete;
  end;
end;
//------------------------------------------------------------------------------
// moves dock to another edge and/or monitor
procedure Tfrmterry.MoveDock(iDirection: integer);
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
procedure Tfrmterry.WMUser(var msg: TMessage);
begin
  if (msg.wParam = wm_activate) and (msg.lParam = 0) then
  begin
    BaseCmd(tcSetVisible, 1);
    sets.RollUp;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMTimer(var msg: TMessage);
begin
  try
    if msg.WParam = ID_TIMER then TimerMain
    else if msg.WParam = ID_TIMER_SLOW then TimerSlow
    else if msg.WParam = ID_TIMER_FSA then TimerFSA;
  except
    on e: Exception do err('Base.WMTimer', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.TimerMain;
begin
  if assigned(sets) then sets.Timer;
  if IsWindowVisible(Handle) then
  begin
    if assigned(ItemMgr) then ItemMgr.Timer;
    if assigned(Tray) then Tray.Timer;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.TimerSlow;
begin
  if assigned(ItemMgr) and assigned(sets) then
  try
    if IsWindowVisible(Handle) then
    begin
      WHMouseMove($fffffff);
      UpdateRunning;
      if not sets.container.StayOnTop then MaintainNotForeground;
    end;

    if sets.visible and not IsWindowVisible(handle) then BaseCmd(tcSetVisible, 1);
    if sets.container.HideTaskBar then HideTaskbar(true);
  except
    on e: Exception do raise Exception.Create('Base.OnSlowTimer'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.TimerFSA;
var
  fsa: boolean;
begin
  try
    ReserveScreenEdge(sets.container.ReserveScreenEdge, sets.container.ReserveScreenEdgePercent, sets.container.Site);

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
    on e: Exception do raise Exception.Create('Base.OnFSATimer'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.UpdateRunning;
begin
  try
    if sets.container.ShowRunningIndicator or sets.container.Taskbar then ProcessHelper.EnumAppWindows;
    if sets.container.Taskbar then ItemMgr.Taskbar;
    if sets.container.ShowRunningIndicator and ProcessHelper.WindowsCountChanged then UpdateRunningI;
  except
    on e: Exception do raise Exception.Create('Base.UpdateRunning'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.UpdateRunningI;
begin
  ProcessHelper.EnumProc;
  ItemMgr.SetParam(icUpdateRunning, 0);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.SetForeground;
begin
  if closing then exit;
  SetWindowPos(handle, ItemMgr.ZOrder(HWND_TOPMOST), 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  SetWindowPos(handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  ItemMgr.ZOrder(HWND_NOTOPMOST);
  ItemMgr.ZOrder(ZOrderWindow);

  if assigned(frmItemProp) then
  try
    SetWindowPos(frmItemProp.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    SetWindowPos(frmItemProp.handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  except
  end;
  if assigned(frmSets) then
  try
    SetWindowPos(frmSets.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    SetWindowPos(frmSets.handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  except
  end;
  if assigned(frmThemeEditor) then
  try
    SetWindowPos(frmThemeEditor.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    SetWindowPos(frmThemeEditor.handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
  except
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.SetNotForeground;

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
procedure Tfrmterry.MaintainNotForeground;
var
  h: THandle;
begin
	h := FindWindow('Progman', nil);
  h := GetWindow(h, GW_HWNDPREV);
	while h <> 0 do
	begin
    if h = handle then exit; // main dock window found first - nothing to do
    if ItemMgr.IsItem(h) <> 0 then // one of the items is under a dock - adjust dock position
    begin
      SetWindowPos(handle, h, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
      AddLog('Base.MaintainNotForeground');
      exit;
    end;
    h := GetWindow(h, GW_HWNDPREV);
	end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.BaseDraw(flags: integer);
var
  hgdip, hbrush: Pointer;
  bmp: gdip_gfx._SimpleBitmap;
  RepaintBase: boolean;
  rgn: HRGN;
begin
  if assigned(ItemMgr) and assigned(theme) and Visible and not closing then
  try
    hgdip := nil;
    bmp.dc := 0;
    RepaintBase := flags and 1 = 1;

    if (ItemMgr.BaseImageRect.X <> OldBaseImageRect.X) or
      (ItemMgr.BaseImageRect.Y <> OldBaseImageRect.Y) or
      (ItemMgr.BaseImageRect.Width <> OldBaseImageRect.Width) or
      (ItemMgr.BaseImageRect.Height <> OldBaseImageRect.Height) then
    begin
      RepaintBase := True;
      OldBaseImageRect := ItemMgr.BaseImageRect;
    end;
    if (ItemMgr.BaseWindowRect.X <> OldBaseWindowRect.X) or
      (ItemMgr.BaseWindowRect.Y <> OldBaseWindowRect.Y) or
      (ItemMgr.BaseWindowRect.Width <> OldBaseWindowRect.Width) or
      (ItemMgr.BaseWindowRect.Height <> OldBaseWindowRect.Height) then
    begin
      RepaintBase := True;
      OldBaseWindowRect := ItemMgr.BaseWindowRect;
    end;

    if not RepaintBase then exit;

    try
      bmp.topleft.x := ItemMgr.BaseWindowRect.x;
      bmp.topleft.y := ItemMgr.BaseWindowRect.y;
      bmp.Width := ItemMgr.BaseWindowRect.Width;
      bmp.Height := ItemMgr.BaseWindowRect.Height;
      gdip_gfx.CreateBitmap(bmp);
      hgdip := gdip_gfx.CreateGraphics(bmp.dc);

      GdipSetCompositingMode(hgdip, CompositingModeSourceOver);
      GdipSetCompositingQuality(hgdip, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(hgdip, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(hgdip, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);

      // avoid flickering when dragging a file //
      if ItemMgr.DraggingFile then
      begin
        GdipCreateSolidFill(ITEM_BACKGROUND, hbrush);
        GdipFillRectangle(hgdip, hbrush, 0, 0, ItemMgr.BaseWindowRect.Width, ItemMgr.BaseWindowRect.Height);
        GdipDeleteBrush(hbrush);
      end;

      // draw background //
      Theme.DrawBackground(hgdip, ItemMgr.BaseImageRect);
      // update window //
      UpdateLWindow(handle, bmp, sets.container.BaseAlpha);

      // blur //
      if dwm.CompositingEnabled and sets.container.Blur and Theme.BlurEnabled then
      begin
        PrevBlur := true;
        rgn := Theme.GetBackgroundRgn(ItemMgr.BaseImageRect);
        if rgn <> 0 then DWM.EnableBlurBehindWindow(handle, rgn);
        DeleteObject(rgn);
      end
      else if PrevBlur then
      begin
        PrevBlur := false;
        DWM.DisableBlurBehindWindow(handle);
      end;

    finally
      gdip_gfx.DeleteGraphics(hgdip);
      gdip_gfx.DeleteBitmap(bmp);
    end;
  except
    on e: Exception do raise Exception.Create('Base.BaseDraw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.trayiconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMenu(0);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.Help;
begin
  if not FileExists(UnzipPath('Help\Help.html')) then notify(UTF8ToAnsi(XErrorHelpNotFound)) else Run('Help\Help.html', '', '', 1);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.err(where: string; e: Exception);
begin
  where := UTF8ToAnsi(XErrorIn) + ' ' + where;
  if assigned(e) then where := where + #10#13 + e.message;
  notify(where);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.notify(message: string; silent: boolean = False);
begin
  if assigned(Notifier) then Notifier.Message(message, sets.GetParam(gpMonitor), False, silent)
  else if not silent then messagebox(handle, pchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.alert(message: string);
begin
  if assigned(notifier) then notifier.message(message, sets.GetParam(gpMonitor), True, False)
  else messagebox(handle, pchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.ActivateHint(hwnd: uint; caption_: string; x, y, direction: integer);
var
  monitor: cardinal;
begin
  if not closing and not sets.IsHiddenDown and not ItemMgr.DraggingFile and not ItemMgr.Dragging then
  begin
    if InitDone and not assigned(AHint) then AHint := _Hint.Create;
    if hwnd = 0 then monitor := MonitorFromWindow(self.Handle, 0) else monitor := MonitorFromWindow(hwnd, 0);
    if assigned(AHint) then AHint.ActivateHint(hwnd, caption_, x, y, direction, monitor);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.DeactivateHint(hwnd: uint);
begin
  if not closing and assigned(AHint) then AHint.DeactivateHint(hwnd);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.AppException(Sender: TObject; e: Exception);
begin
  notify('[AppException]'#13#10 + Sender.ClassName + #13#10 + e.message);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.AppDeactivate(Sender: TObject);
begin
  ItemMgr.WMDeactivateApp;
end;
//------------------------------------------------------------------------------
function Tfrmterry.GetMonitorWorkareaRect: Windows.TRect;
var
  monitor: integer;
begin
  result := screen.DesktopRect;
  monitor := sets.GetParam(gpMonitor);
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].WorkareaRect;
end;
//------------------------------------------------------------------------------
function Tfrmterry.GetMonitorBoundsRect: Windows.TRect;
var
  monitor: integer;
begin
  result := screen.DesktopRect;
  monitor := sets.GetParam(gpMonitor);
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].BoundsRect;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.HideTaskbar(hide: boolean);
var
  hwnd: uint;
  r: Windows.TRect;
begin
  try
    hwnd := FindWindow('Shell_TrayWnd', nil);
    if hide and IsWindowVisible(hwnd) then
    begin
      showwindow(hwnd, 0);
      showwindow(FindWindow('Button', pchar(UTF8ToAnsi(XStartButtonText))), 0);
      r := ItemMgr.MonitorRect;
      if not EqualRect(r, GetMonitorWorkareaRect) then SystemParametersInfo(SPI_SETWORKAREA, 0, @r, SPIF_SENDCHANGE);
    end
    else
    if not hide and not IsWindowVisible(hwnd) then
    begin
      showwindow(hwnd, SW_SHOW);
      showwindow(FindWindow('Button', pchar(UTF8ToAnsi(XStartButtonText))), SW_SHOW);
      r := ItemMgr.MonitorRect;
      if not EqualRect(r, GetMonitorWorkareaRect) then SystemParametersInfo(SPI_SETWORKAREA, 0, @r, SPIF_SENDCHANGE);
    end;
  except
    on e: Exception do raise Exception.Create('Base.HideTaskbar'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.ReserveScreenEdge(Reserve: boolean; Percent: integer; Edge: TBaseSite);
var
  Changed: boolean;
  Position: integer;
  WorkArea, Bounds: Windows.TRect;
begin
  if Reserve then
  try
    Changed := false;
    WorkArea := GetMonitorWorkareaRect;
    Bounds := ItemMgr.MonitorRect;

    Position := ifthen(Edge = bsLeft, round(ItemMgr.BaseWindowRect.Width * Percent / 100), 0);
    if WorkArea.Left <> Position then
    begin
      WorkArea.Left := Position;
      Changed := true;
    end;

    Position := ifthen(Edge = bsTop, round(ItemMgr.BaseWindowRect.Height * Percent / 100), 0);
    if WorkArea.Top <> Position then
    begin
      WorkArea.Top := Position;
      Changed := true;
    end;

    Position := ifthen(Edge = bsRight, Bounds.Right - round(ItemMgr.BaseWindowRect.Width * Percent / 100), Bounds.Right);
    if WorkArea.Right <> Position then
    begin
      WorkArea.Right := Position;
      Changed := true;
    end;

    Position := ifthen(Edge = bsBottom, Bounds.Bottom - round(ItemMgr.BaseWindowRect.Height * Percent / 100), Bounds.Bottom);
    if WorkArea.Bottom <> Position then
    begin
      WorkArea.Bottom := Position;
      Changed := true;
    end;

    if Changed then SystemParametersInfo(SPI_SETWORKAREA, 0, @WorkArea, SPIF_SENDCHANGE);
  except
    on e: Exception do raise Exception.Create('Base.ReserveScreenEdge'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// 0 - left, 1 - top, 2 - right, 3 - bottom
procedure Tfrmterry.UnreserveScreenEdge(Edge: TBaseSite);
var
  Changed: boolean;
  WorkArea, Bounds: Windows.TRect;
begin
  try
    Changed := false;
    WorkArea := GetMonitorWorkareaRect;
    Bounds := ItemMgr.MonitorRect;

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

    if Changed then SystemParametersInfo(SPI_SETWORKAREA, 0, @WorkArea, SPIF_SENDCHANGE);
  except
    on e: Exception do raise Exception.Create('Base.UnreserveScreenEdge'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.OnDragEnter(list: TStrings; hWnd: uint);
begin
  ItemMgr.DragEnter;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.OnDragOver;
begin
  ItemMgr.DragOver;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.OnDragLeave;
begin
  ItemMgr.DragLeave;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.OnDrop(files: TStrings; hWnd: uint);
var
  pt: Windows.TPoint;
begin
  try
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
procedure Tfrmterry.DropFiles(files: TStrings);
var
  i: integer;
begin
  for i := 0 to files.Count - 1 do files.strings[i] := TShortcutItem.FromFile(files.strings[i]);
  ItemMgr.InsertItems(files);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMCopyData(var Message: TMessage);
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
procedure Tfrmterry.AddFile;
begin
  with TOpenDialog.Create(self) do
  begin
    if Execute then AddFile(Filename);
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.AddFile(Filename: string);
begin
  if assigned(ItemMgr) then
    ItemMgr.InsertItem(TShortcutItem.Make(0, ChangeFileExt(ExtractFilename(Filename), ''),
      toolu.ZipPath(Filename), '', toolu.ZipPath(ExtractFilePath(Filename)), '', 1));
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.LockMouseEffect(hWnd: HWND; lock: boolean);
var
  index: integer;
begin
  crsection.Acquire;
  try
    index := LockList.IndexOf(pointer(hWnd));
    if lock then
    begin
      if index < 0 then LockList.Add(pointer(hWnd));
    end else begin
      if index >= 0 then LockList.Delete(index);
    end;
    SetParam(gpLockMouseEffect, integer(LockList.Count > 0));
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
function Tfrmterry.IsLockedMouseEffect: boolean;
begin
  result := LockList.Count > 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.SetFont(var Value: _FontData);
begin
  CopyFontData(Value, sets.container.Font);
  SetParam(gpShowHint, integer(sets.container.ShowHint));
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.SetStackFont(var Value: _FontData);
begin
  CopyFontData(Value, sets.container.StackFont);
  SetParam(gpShowHint, integer(sets.container.ShowHint));
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.OpenWith(filename: string);
var
  _file: string;
begin
  _file := toolu.UnzipPath(filename);
  if not fileexists(_file) then _file := toolu.FindFile(_file);
  ShellExecute(application.mainform.handle, nil, 'rundll32.exe', pchar('shell32.dll,OpenAs_RunDLL ' + _file), nil, 1);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMMouseWheel(var msg: TWMMouseWheel);
begin
  if msg.WheelDelta < 0 then wacmd(40059) else if msg.WheelDelta > 0 then wacmd(40058);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMDisplayChange(var Message: TMessage);
begin
  screen.UpdateMonitors;
  sets.StoreParam(gpMonitor, sets.GetParam(gpMonitor));
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMSettingChange(var Message: TMessage);
begin
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.WMCompositionChanged(var Message: TMessage);
begin
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
function Tfrmterry.FullScreenAppActive(HWnd: HWND): boolean;
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
  rMonitor := ItemMgr.MonitorRect;
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
function Tfrmterry.ListFullScreenApps: string;
var
  wnd: hWnd;
  rc, rMonitor: windows.TRect;
  cls: array [0..MAX_PATH - 1] of char;
begin
  result := '';
  rMonitor := ItemMgr.MonitorRect;
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
procedure Tfrmterry.mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
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
procedure Tfrmterry.execute_cmdline(cmd: string; showcmd: integer = 1);
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
procedure Tfrmterry.execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = 1; hwnd: cardinal = 0);
var
  cmd2: string;
  i, i1, i2, i3, i4: integer;
  str1, str2: string;
  lpsz1, lpsz2: pchar;
  pt: windows.TPoint;
begin
  if cmd = '' then exit;

  if cmd[1] <> '/' then
  begin
    frmterry.Run(cmd, params, dir, showcmd);
    exit;
  end;

  system.Delete(cmd, 1, 1);

  params := toolu.UnzipPath(params);
  cmd2 := cutafter(cmd, '.');

  if cut(cmd, '.') = 'itemmgr' then frmterry.ItemMgr.command(cmd2, params)
  else if cmd = 'quit' then frmterry.BaseCmd(tcQuit, 0)
  else if cmd = 'hide' then frmterry.BaseCmd(tcSetVisible, 0)
  else if cmd = 'say' then frmterry.notify(toolu.UnzipPath(params))
  else if cmd = 'alert' then frmterry.alert(toolu.UnzipPath(params))
  else if cmd = 'togglevisible' then frmterry.BaseCmd(tcToggleVisible, 0)
  else if cmd = 'togglesystaskbar' then frmterry.BaseCmd(tcToggleTaskbar, 0)
  else if cmd = 'sets' then
  begin
    if not trystrtoint(params, i) then i := 0;
    Tfrmsets.StartForm(i);
  end
  else if cmd = 'cmd' then Tfrmcmd.StartForm
  else if cmd = 'collection' then frmterry.Run('%pp%\collection.exe')
  else if cmd = 'apps' then frmterry.Run('%pp%\apps.exe')
  else if cmd = 'taskmgr' then frmterry.Run('%sysdir%\taskmgr.exe')
  else if cmd = 'program' then frmterry.AddFile
  else if cmd = 'command' then TfrmAddCommand.Open
  else if cmd = 'backup' then sets.Backup
  else if cmd = 'restore' then sets.Restore
  else if cmd = 'tray' then frmterry.Tray.Show(sets.container.Site, hwnd)
  else if cmd = 'autotray' then frmterry.Tray.SwitchAutoTray
  else if cmd = 'themeeditor' then TfrmThemeEditor.Open
  else if cmd = 'lockdragging' then frmterry.SetParam(gpLockDragging, ifthen(sets.GetParam(gpLockDragging) = 0, 1, 0))
  else if cmd = 'site' then frmterry.SetParam(gpSite, integer(sets.StringToSite(params)))
  else if cmd = 'logoff' then toolu.ShutDown(ifthen(params = 'force', 4, 0))
  else if cmd = 'shutdown' then toolu.ShutDown(ifthen(params = 'force', 5, 1))
  else if cmd = 'reboot' then toolu.ShutDown(ifthen(params = 'force', 6, 2))
  else if cmd = 'suspend' then toolu.SetSuspendState(false, false, false)
  else if cmd = 'hibernate' then toolu.SetSuspendState(true, false, false)
  else if cmd = 'kill' then ProcessHelper.Kill(params)
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
  else if cmd = 'windowfrompoint' then
  begin
    windows.GetCursorPos(pt);
    uint(inta) := windows.WindowFromPoint(pt);
    SetClipboard(inttostr(inta));
  end
  else if cmd = 'findwindow' then
  begin
    str1 := Trim(fetch(params, ',', true));
    str2 := Trim(fetch(params, ',', true));
    if str1 = '' then lpsz1 := nil else lpsz1 := pchar(str1);
    if str2 = '' then lpsz2 := nil else lpsz2 := pchar(str2);
    uint(inta) := findwindow(lpsz1, lpsz2);
    SetClipboard(inttostr(inta));
  end
  else if cmd = 'findwindowex' then
  begin
    str1 := Trim(fetch(params, ',', true));
    str2 := Trim(fetch(params, ',', true));
    if str1 = '' then lpsz1 := nil else lpsz1 := pchar(str1);
    if str2 = '' then lpsz2 := nil else lpsz2 := pchar(str2);
    uint(inta) := findwindowex(inta, 0, lpsz1, lpsz2);
    SetClipboard(inttostr(inta));
  end
  else if cmd = 'showwindow' then showwindow(uint(inta), strtoint(params))
  else if cmd = 'setwindowtext' then setwindowtext(uint(inta), pchar(params))
  else if cmd = 'setwindowpos' then
  begin
    if not trystrtoint(Trim(fetch(params, ',', true)), i2) then i2 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i3) then i3 := 0;
    setwindowpos(uint(inta), 0, i2, i3, 0, 0, swp_nomove + swp_noactivate + swp_nozorder + swp_noreposition);
  end
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
  else if cmd = 'play' then sndPlaySound(pchar(params), SND_ASYNC or SND_FILENAME)
  else if cmd = 'guid' then SetClipboard(CreateClassId);
end;
//------------------------------------------------------------------------------
procedure Tfrmterry.Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
var
  shell: string;
begin
  try
    exename := toolu.UnzipPath(exename);
    params := toolu.UnzipPath(params);
    dir := toolu.UnzipPath(dir);
    shell := toolu.UnzipPath(sets.container.Shell);

    if directoryexists(exename) then
      if sets.container.useShell and fileexists(shell) then
      begin
        params := '"' + exename + '"';
        exename := shell;
        dir := '';
      end;

    ProcessHelper.Run(exename, params, dir, showcmd);
  except
    on e: Exception do err('Base.Run', e);
  end;
end;
//------------------------------------------------------------------------------
end.

