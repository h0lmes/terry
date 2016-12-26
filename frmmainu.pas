unit frmmainu;

{$undef EXT_DEBUG}

interface
uses
  jwaWindows, Windows, Messages, SysUtils, Classes, Controls, LCLType, Forms,
  Menus, Dialogs, ExtCtrls, ShellAPI, ComObj, Math, Syncobjs, MMSystem, LMessages,
  declu, GDIPAPI, gfx, dwm_unit, hintu, notifieru, itemmgru,
  DropTgtU, setsu, trayu, shelltraywndu, aeropeeku, loggeru;

type
  PRunData = ^TRunData;
  TRunData = packed record
    Handle: THandle;
    exename: array [0..1023] of wchar;
    params: array [0..1023] of wchar;
    dir: array [0..1023] of wchar;
    showcmd: integer;
  end;

  { Tfrmmain }

  Tfrmmain = class(TForm)
    trayicon: TTrayIcon;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure trayiconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    inta: THandle;
    FProgramIsClosing: boolean;
    FSavingSettings: boolean;
    FAllowCloseProgram: boolean;
    FBlurActive: boolean;
    FHiddenByFSA: boolean; // true if panel was hidden by HideOnFullScreenApp parameter //
    LockList: TList; // list of window handles that have requested a lock (disable zoom and related) //
    crsection: TCriticalSection;
    FWndOffset: integer;
    FWndOffsetTarget: integer;
    FHideKeysPressed: boolean;
    FConsoleKeysPressed: boolean;
    FMonitor: integer;
    FOldBaseWindowRect: GDIPAPI.TRect;
    FOldBaseImageRect: GDIPAPI.TRect;
    FLastMouseHookPoint: Windows.TPoint;
    FMouseOver: boolean;
    FMouseOverEdge: boolean;
    FInitDone: boolean;
    //FHook: THandle;
    FPrevWndProc: Pointer;
    FMenu: THandle;
    FMenuCreate: THandle;
    WM_SHELLHOOK: integer;
    FBlurWindow: THandle;
    function  CloseQueryI: integer;
    procedure CreateBlurWindow;
    procedure DestroyBlurWindow;
    procedure DoGlobalHotkeys;
    procedure FlashTaskWindow(hwnd: HWND);
    function IsHotkeyPressed(hotkey: integer): boolean;
    procedure SaveSets;
    procedure RegisterRawInput;
    procedure NativeWndProc(var message: TMessage);
    procedure AppException(Sender: TObject; e: Exception);
    procedure AppDeactivate(Sender: TObject);
    procedure UpdateBlurWindow;
    procedure WMCopyData(var Message: TMessage);
    procedure WMTimer(var msg: TMessage);
    procedure WMUser(var msg: TMessage);
    procedure WMCommand(var msg: TMessage);
    procedure WMDisplayChange(var Message: TMessage);
    procedure WMSettingChange(var Message: TMessage);
    procedure WMCompositionChanged(var Message: TMessage);
    procedure WMDPIChanged(var Message: TMessage);
    procedure WHRawMouse(mouse: RAWMOUSE);
    procedure WHRawKB(kb: RAWKEYBOARD);
    procedure WHButtonDown(button: integer);
    procedure WHMouseMove(LParam: LParam);
    procedure OnMouseEnter;
    procedure OnMouseLeave;
    procedure DoMenu;
    procedure OnTimerMain;
    procedure OnTimerSlow;
    procedure OnTimerFSA;
    procedure OnTimerRoll;
    procedure OnTimerForeground;
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
  public
    ItemMgr: TItemManager;
    DropMgr: TDropManager;
    AHint: THint;
    Tray: TTrayController;
    procedure Init(SetsFilename: string);
    procedure ExecAutorun;
    procedure CloseProgram;
    procedure ApplyParams;
    procedure Restore(backupFile: string);
    procedure err(where: WideString; e: Exception);
    procedure notify(message: WideString; silent: boolean = False);
    procedure alert(message: WideString);
    procedure ActivateHint(hwnd: THandle; ACaption: WideString; x, y: integer);
    procedure DeactivateHint(hwnd: THandle);
    procedure SetTheme(ATheme: string);
    function  BaseCmd(id: TDParam; param: PtrInt): PtrInt;
    procedure SetParam(id: TDParam; Value: integer);
    function  GetHMenu(ParentMenu: THandle): THandle;
    function  ContextMenu(pt: Windows.TPoint): boolean;
    procedure SetFont(var Value: TDFontData);
    procedure LockMouseEffect(hWnd: HWND; lock: boolean);
    function  IsMouseEffectLocked: boolean;
    function  GetMonitorWorkareaRect(pMonitor: PInteger = nil): Windows.TRect;
    function  GetMonitorBoundsRect(pMonitor: PInteger = nil): Windows.TRect;
    procedure MoveDock(iDirection: integer);
    procedure OnDragEnter(list: TStrings; hWnd: THandle);
    procedure OnDragOver;
    procedure OnDragLeave;
    procedure OnDrop(files: TStrings; hWnd: THandle);
    procedure DropFiles(files: TStrings);
    procedure AddFile; overload;
    procedure AddFile(Filename: string); overload;
    procedure NewDock;
    procedure RemoveDock;
    procedure OpenWith(filename: string);
    function  FullScreenAppActive: boolean;
    function  ListFullScreenApps: string;
    procedure ThemesMenu;
    procedure SetTaskSpot(wnd: THandle);
    procedure ListTasksAndModules;
    procedure WinampCmd(params: string; showcmd: integer = sw_shownormal);
    procedure mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal; hwnd: cardinal = 0);
    procedure execute_cmdline(cmd: string; showcmd: integer = sw_shownormal);
    procedure execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal; hwndCaller: cardinal = 0);
    procedure Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
  end;

var frmmain: Tfrmmain;

implementation
uses themeu, toolu, scitemu, PIDL, frmsetsu, frmcmdu, frmitemoptu,
  frmStackPropu, frmAddCommandU, frmthemeeditoru, processhlp, frmhellou,
  frmtipu, multidocku, frmrestoreu;
{$R *.lfm}
{$R Resource\res.res}
//------------------------------------------------------------------------------
function MainWindowProc(wnd: HWND; message: uint; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  inst: Tfrmmain;
  msg: TMessage;
begin
  inst := Tfrmmain(GetWindowLongPtr(wnd, GWL_USERDATA));
  if assigned(inst) then
  begin
    msg.msg := message;
    msg.wParam := wParam;
    msg.lParam := lParam;
    inst.NativeWndProc(msg);
    result := msg.Result;
  end
  else
    result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.Init(SetsFilename: string);
begin
  try
    FProgramIsClosing := false;
    FSavingSettings := false;
    FAllowCloseProgram := false;
    FBlurActive := false;
    FInitDone := false;
    Application.OnException := AppException;
    Application.OnDeactivate := AppDeactivate;
    trayicon.Icon := application.Icon;
    LockList := TList.Create;
    crsection := TCriticalSection.Create;
    FBlurWindow := 0;
    FMouseOver := false;
    FMouseOverEdge := false;

    // workaround for Windows message handling in LCL //
    SetWindowLongPtr(Handle, GWL_USERDATA, PtrUInt(self));
    FPrevWndProc := Pointer(GetWindowLongPtr(Handle, GWL_WNDPROC));
    SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(@MainWindowProc));

    dwm.ExcludeFromPeek(Handle);

    WM_SHELLHOOK := RegisterWindowMessage('SHELLHOOK');
    RegisterShellHookWindow(Handle);

    // hook //
    //theFile := UnzipPath('%pp%\hook.dll');
    //FHook := 0;
    //if FileExists(theFile) then FHook := LoadLibrary(pchar(theFile));

    sets := TDSets.Create(SetsFilename, UnzipPath('%pp%'), Handle);
    sets.Load;
    FMonitor := sets.container.Monitor;

    if sets.container.BlurEnabled then CreateBlurWindow;

    theme := TDTheme.Create(pchar(@sets.container.ThemeName), sets.container.Site);
    {$ifdef EXT_DEBUG} AddLog('TDTheme.Create'); {$endif}
    if not theme.Load then
    begin
      notify(UTF8Decode(XErrorLoadTheme + ' ' + XErrorContactDeveloper));
      AddLog('Theme.Halt');
      halt;
    end;
    {$ifdef EXT_DEBUG} AddLog('Theme.Load'); {$endif}

    // create ItemManager (disabled, invisible, ...) //
    try
      ItemMgr := TItemManager.Create(false, false, Handle, BaseCmd,
          sets.container.ItemSize, sets.container.BigItemSize, sets.container.ZoomWidth,
          sets.container.ZoomTime, sets.container.ItemSpacing, sets.container.ZoomEnabled,
          sets.container.ReflectionEnabled, sets.container.ReflectionSize, sets.container.LaunchInterval,
          sets.container.ItemAnimationType, sets.container.SeparatorAlpha,
          sets.container.ActivateRunningApps, sets.container.UseShellContextMenus, sets.container.LockDragging,
          sets.container.StackAnimationEnabled,
          sets.container.AeroPeekEnabled, sets.container.TaskLivePreviews,
          sets.container.TaskGrouping, sets.container.TaskSystemMenus,
          sets.container.TaskThumbSize, sets.container.TaskSpot,
          sets.container.ShowHint, sets.container.Font);
      {$ifdef EXT_DEBUG} AddLog('TItemManager.Create'); {$endif}
    except
      notify(UTF8Decode(XErrorCritical + ' ' + XErrorContactDeveloper));
      AddLog('Exception in TItemManager.Create');
      halt;
    end;
    SetParam(gpSite, integer(sets.container.site));
    SetParam(gpCenterOffsetPercent, sets.container.CenterOffsetPercent);
    SetParam(gpEdgeOffset, sets.container.EdgeOffset);
    SetParam(gpAutoHide, integer(sets.container.AutoHide));
    SetParam(gpAutoHidePixels, integer(sets.container.AutoHidePixels));
    SetParam(gpHideSystemTaskbar, integer(sets.container.HideSystemTaskbar));
    SetParam(gpReserveScreenEdge, sets.GetParam(gpReserveScreenEdge));
    SetParam(gpOccupyFullMonitor, integer(sets.container.OccupyFullMonitor));
    SetParam(gpBlurEnabled, sets.GetParam(gpBlurEnabled));
    BaseCmd(tcThemeChanged, 0);
    {$ifdef EXT_DEBUG} AddLog('BaseCmd(tcThemeChanged)'); {$endif}

    // load items //
    try
      ItemMgr.Load(UnzipPath(sets.SetsPathFile));
      {$ifdef EXT_DEBUG} AddLog('ItemMgr.Load'); {$endif}
      ItemMgr.Enable(true);
      {$ifdef EXT_DEBUG} AddLog('ItemMgr.Enable'); {$endif}
      if not sets.Backup then
      begin
        AddLog('Init.BackupFailed');
        messageboxw(handle, pwchar(UTF8Decode(XErrorSetsBackupFailed)), PROGRAM_NAME, MB_ICONERROR);
      end;
      {$ifdef EXT_DEBUG} AddLog('sets.Backup'); {$endif}
    except
      on e: Exception do
      begin
        SaveSets;
        AddLog('Init.LoadItems failed' + LineEnding + e.message);
        messageboxw(handle, pwchar(UTF8Decode(XErrorSetsCorrupted + ' ' + XMsgRunRestore)), PROGRAM_NAME, MB_ICONEXCLAMATION);
        TfrmRestore.Open;
      end;
    end;

    // Timers //
    SetTimer(handle, ID_TIMER, 10, nil);
    SetTimer(handle, ID_TIMER_SLOW, 1000, nil);
    SetTimer(handle, ID_TIMER_FSA, 2000, nil);
    {$ifdef EXT_DEBUG} AddLog('Timers'); {$endif}

    // Tray and StartMenu controllers //
    Tray := TTrayController.Create;
    {$ifdef EXT_DEBUG} AddLog('Tray'); {$endif}

    // show the dock //
    BaseCmd(tcSetVisible, 1);
    if sets.GetParam(gpStayOnTop) = 1 then SetParam(gpStayOnTop, 1); // apply StayOnTop
    {$ifdef EXT_DEBUG} AddLog('show dock'); {$endif}

    // RawInput (replacement for hook) //
    RegisterRawInput;
    {$ifdef EXT_DEBUG} AddLog('RegisterRawInput'); {$endif}

    // DropManager //
    DropMgr := TDropManager.Create(Handle);
    DropMgr.OnDrop := OnDrop;
    DropMgr.OnDragEnter := OnDragEnter;
    DropMgr.OnDragOver := OnDragOver;
    DropMgr.OnDragLeave := OnDragLeave;
    {$ifdef EXT_DEBUG} AddLog('TDropManager.Create'); {$endif}

    // apply theme. trigger full repaint //
    BaseCmd(tcThemeChanged, 0);
    {$ifdef EXT_DEBUG} AddLog('apply theme. trigger full repaint'); {$endif}

    // 'RollDown' on startup if set so //
    FWndOffsetTarget := 0;
    FWndOffset := 0;
    DoRollDown;

    UpdateRunning;
    if sets.container.Hello then TfrmHello.Open;
    FInitDone := True;
    AddLog('InitDone');
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
procedure Tfrmmain.CreateBlurWindow;
begin
  if FBlurWindow = 0 then
  try
    FBlurWindow := CreateWindowEx(WS_EX_LAYERED + WS_EX_TOOLWINDOW, TDWCLASS,
      'BlurWindow', WS_POPUP, -100, -100, 10, 10, 0, 0, hInstance, nil);
    dwm.ExcludeFromPeek(FBlurWindow);
    SetWindowLongPtr(Handle, GWL_HWNDPARENT, FBlurWindow); // attach main window
    SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS);
  except
    on e: Exception do raise Exception.Create('Base.CreateBlurWindow' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DestroyBlurWindow;
begin
  try
    SetWindowLongPtr(Handle, GWL_HWNDPARENT, 0); // detach main window
    if IsWindow(FBlurWindow) then DestroyWindow(FBlurWindow);
    FBlurWindow := 0;
  except
    on e: Exception do raise Exception.Create('Base.DestroyBlurWindow' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.CloseProgram;
begin
  FAllowCloseProgram := true;
  Close;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if CloseQueryI = 0 then CloseAction := caNone else CloseAction := caFree;
end;
//------------------------------------------------------------------------------
// check if program can close and, if yes, free all resources
function Tfrmmain.CloseQueryI: integer;
begin
  result := 0;

  if FAllowCloseProgram then
  try
    crsection.Acquire;
    FProgramIsClosing := true;
    AddLog('CloseQueryI.begin');
    try
      KillTimer(handle, ID_TIMER);
      KillTimer(handle, ID_TIMER_SLOW);
      KillTimer(handle, ID_TIMER_FSA);

      DeregisterShellHookWindow(Handle);
      SetWindowLongPtr(Handle, GWL_WNDPROC, PtrInt(FPrevWndProc)); // reset window proc

      BaseCmd(tcSaveSets, 0);

      ShellTrayWndController.UnreserveScreenEdge;
      ShellTrayWndController.HideTaskbar(false);

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
      if assigned(Tray) then Tray.Free;
      Tray := nil;

      TAeroPeekWindow.Cleanup;
      LockList.free;
      DestroyBlurWindow;
      // close other instances
      if not docks.ThisDockRemovalScheduled then docks.CloseOtherDocks;
    except
      on e: Exception do messagebox(handle, PChar(e.message), 'Base.Close.Free', mb_iconexclamation);
    end;
    AddLog('CloseQueryI.done');
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
    SetParam(gpHideSystemTaskbar, integer(sets.container.HideSystemTaskbar));
    SetParam(gpReserveScreenEdge, sets.GetParam(gpReserveScreenEdge));
    SetParam(gpOccupyFullMonitor, integer(sets.container.OccupyFullMonitor));

    ItemMgr.SetParam(gpItemSize, sets.container.itemsize);
    ItemMgr.SetParam(gpBigItemSize, sets.container.BigItemSize);
    ItemMgr.SetParam(gpZoomEnabled, integer(sets.container.ZoomEnabled));
    ItemMgr.SetParam(gpZoomWidth, sets.container.ZoomWidth);
    ItemMgr.SetParam(gpZoomTime, sets.container.ZoomTime);
    ItemMgr.SetParam(gpItemSpacing, sets.container.ItemSpacing);
    ItemMgr.SetParam(gpReflectionEnabled, integer(sets.container.ReflectionEnabled));
    ItemMgr.SetParam(gpReflectionSize, sets.container.ReflectionSize);
    ItemMgr.SetParam(gpLaunchInterval, sets.container.LaunchInterval);
    ItemMgr.SetParam(gpActivateRunning, integer(sets.container.ActivateRunningApps));
    ItemMgr.SetParam(gpUseShellContextMenus, integer(sets.container.UseShellContextMenus));
    ItemMgr.SetParam(gpItemAnimationType, sets.container.ItemAnimationType);
    ItemMgr.SetParam(gpLockDragging, integer(sets.container.LockDragging));
    ItemMgr.SetParam(gpShowRunningIndicator, integer(sets.container.ShowRunningIndicator));
    ItemMgr.SetParam(gpStackAnimationEnabled, integer(sets.container.StackAnimationEnabled));
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
  if FSavingSettings then
  begin
    AddLog('SaveSets.exit');
    exit;
  end;
  if assigned(ItemMgr) then
  try
    crsection.Acquire;
    try
      FSavingSettings := true;
      sets.Save;
      ItemMgr.Save(sets.SetsPathFile);
    finally
      FSavingSettings := false;
      crsection.Leave;
    end;
    AddLog('SaveSets.done');
  except
    on e: Exception do err('Base.BaseSaveSettings', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.Restore(backupFile: string);
begin
  notify('Restore: ' + backupFile);
  if sets.Restore(backupFile) then
  begin
    AddLog('Restore.Succeeded');
    messageboxw(handle, pwchar(UTF8Decode(XMsgSetsRestored + ' ' + XMsgRunAgain)), PROGRAM_NAME, MB_ICONEXCLAMATION);
    halt;
  end else begin
    AddLog('Restore.Failed');
    messageboxw(handle, pwchar(UTF8Decode(XErrorSetsRestoreFailed + ' ' + XErrorContactDeveloper)), PROGRAM_NAME, MB_ICONERROR);
    halt;
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
function Tfrmmain.BaseCmd(id: TDParam; param: PtrInt): PtrInt;
begin
  Result := 0;

  case id of
    tcActivate:
      begin
        SetForegroundWindow(Handle);
        SetActiveWindow(Handle);
        if sets.container.StayOnTop then SetForeground;
      end;
    tcRepaintBase: BasePaint(param);
    tcMenu: DoMenu;
    tcSaveSets: SaveSets;
    // a big command to rearrange everything
    tcThemeChanged:
      if assigned(ItemMgr) then
      begin
        FMonitor := sets.GetParam(gpMonitor);
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
        UpdateBlurWindow;
        if (param <> 0) and assigned(ItemMgr) then ItemMgr.Visible := true;
        if param <> 0 then UpdateRunning;
      end;
    tcToggleVisible: BaseCmd(tcSetVisible, PtrInt(not Visible));
    tcToggleTaskbar: frmmain.SetParam(gpHideSystemTaskbar, ifthen(sets.GetParam(gpHideSystemTaskbar) = 0, 1, 0));
    tcGetVisible: Result := PtrInt(ItemMgr.Visible);
    tcDebugInfo: if assigned(ItemMgr) then ItemMgr.AllItemCmd(tcDebugInfo, 0);
  end;
end;
//------------------------------------------------------------------------------
// stores given parameter value and propagates it to subsequent objects       //
// e.g. ItemManager and all items                                             //
procedure Tfrmmain.SetParam(id: TDParam; value: integer);
begin
  // take some actions prior to changing anything //
  case id of
    gpMonitor: if value <> sets.GetParam(id) then ShellTrayWndController.UnreserveScreenEdge;
    gpSite: if value <> sets.GetParam(id) then ShellTrayWndController.UnreserveScreenEdge;
  end;

  // here is the only place to "store" parameter
  // all other pieces of code should use frmmain.SetParam instead
  if (id > gpMin) and (id < gpMax) then value := sets.StoreParam(id, value);

  // take some actions prior to notify ItemMgr //
  case id of
    gpMonitor:                BaseCmd(tcThemeChanged, 0);
    gpSite:
      begin
        if assigned(theme) then theme.Site := sets.container.Site;
        BaseCmd(tcThemeChanged, 0);
      end;
    gpAutoHide:               if value = 0 then DoRollUp;
    gpHideSystemTaskbar:      ShellTrayWndController.HideTaskbar(value <> 0);
    gpReserveScreenEdge:
      begin
        if value = 0 then ShellTrayWndController.UnreserveScreenEdge
        else ShellTrayWndController.ReserveScreenEdge(
          sets.container.Monitor, sets.container.Site, sets.container.ReserveScreenEdgePercent, ItemMgr.FBaseWindowRect, sets.container.AutoHide);
      end;
    gpStayOnTop:              if value <> 0 then SetForeground else SetNotForeground;
    gpBaseAlpha:              BasePaint(1);
    gpBlurEnabled:
      begin
        if value = 0 then DestroyBlurWindow else CreateBlurWindow;
        BasePaint(1);
      end;
    gpShowRunningIndicator:   if value <> 0 then UpdateRunning;
  end;

  if assigned(ItemMgr) then ItemMgr.SetParam(id, value);

  // take some actions after notifying ItemMgr //
  case id of
    gpLockMouseEffect:       WHMouseMove(0);
    gpOccupyFullMonitor, gpStartOffset, gpEndOffset: BaseCmd(tcThemeChanged, 0);
    gpTaskbar:               UpdateRunning;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.RegisterRawInput;
var
  rid: array [0..0] of RAWINPUTDEVICE;
begin
  rid[0].usUsagePage := 1;
  rid[0].usUsage := 2; // mouse
  rid[0].dwFlags := RIDEV_INPUTSINK;
  rid[0].hwndTarget := Handle;
  //rid[1].usUsagePage := 1;
  //rid[1].usUsage := 6; // kb
  //rid[1].dwFlags := RIDEV_INPUTSINK;
  //rid[1].hwndTarget := Handle;
  if not RegisterRawInputDevices(@rid, 1, sizeof(RAWINPUTDEVICE)) then notify('RegisterRawInput failed!');
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.NativeWndProc(var message: TMessage);
var
  dwSize: DWORD;
  ri: RAWINPUT;
begin
  message.result := 0;

  if message.msg = WM_INPUT then
  begin
     if assigned(ItemMgr) then
       if not FProgramIsClosing and ItemMgr.Visible then
       begin
         dwSize := 0;
         GetRawInputData(message.lParam, RID_INPUT, nil, dwSize, sizeof(RAWINPUTHEADER));
         if GetRawInputData(message.lParam, RID_INPUT, @ri, dwSize, sizeof(RAWINPUTHEADER)) = dwSize then
         begin
           if ri.header.dwType = RIM_TYPEMOUSE then WHRawMouse(ri.mouse);
           //else if ri.header.dwType = RIM_TYPEKEYBOARD then WHRawKB(ri.keyboard);
         end;
       end;
     exit;
  end;

  if message.msg = WM_SHELLHOOK then
  begin
    if      message.wParam = HSHELL_FLASH           then FlashTaskWindow(message.lParam)
    else if message.wParam = HSHELL_WINDOWCREATED   then SetTimer(handle, ID_TIMER_WINDOWCREATED, 200, nil) // workaroud
    else if message.wParam = HSHELL_WINDOWDESTROYED then UpdateRunning
    else if message.wParam = HSHELL_MONITORCHANGED  then UpdateRunning
    else if message.wParam = HSHELL_WINDOWACTIVATED then ProcessHelper.ForegroundWindowHandle := message.lParam;
    exit;
  end;

  case message.msg of
    WM_TIMER :                 WMTimer(message);
    WM_USER :                  WMUser(message);
    WM_COMMAND :               WMCommand(message);
    WM_QUERYENDSESSION :
      begin
        FAllowCloseProgram := true;
        message.Result := CloseQueryI;
      end;
    WM_COPYDATA :              WMCopyData(message);
    WM_DISPLAYCHANGE :         WMDisplayChange(message);
    WM_SETTINGCHANGE :         WMSettingChange(message);
    WM_DWMCOMPOSITIONCHANGED : WMCompositionChanged(message);
    WM_DPICHANGED :            WMDPIChanged(message);
    WM_APP_RUN_THREAD_END :    CloseHandle(message.lParam);
    else
      message.result := CallWindowProc(FPrevWndProc, Handle, message.Msg, message.wParam, message.lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHRawKB(kb: RAWKEYBOARD);
var
  key: char;
  fw: THandle;
begin
  if assigned(frmcmd) then
  begin
    fw := GetForegroundWindow;
    if (fw <> handle) and (fw <> frmcmd.handle) and IsWindowVisible(frmcmd.Handle) then
      if kb.Message = WM_KEYDOWN then
      begin
        //notify('VKey = ' + inttostr(kb.VKey) + LineEnding + 'Message = ' + inttostr(kb.Message) + LineEnding +
          //'Flags = ' + inttostr(kb.Flags) + LineEnding + 'Ext = ' + inttostr(kb.ExtraInformation) + LineEnding + 'MakeCode = ' + inttostr(kb.MakeCode));
        if kb.vkey = vk_return then frmcmd.exec
        else
        if kb.vkey = vk_escape then frmcmd.close
        else
        begin
          key := chr(kb.vkey);
          if GetKeyState(VK_SHIFT) and $80 = 0 then key := LowerCase(key);
          //SendKey(frmcmd.edcmd.handle, char(key), kb.Flags <> 0);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHRawMouse(mouse: RAWMOUSE);
begin
  if mouse.usButtonData and RI_MOUSE_LEFT_BUTTON_DOWN <> 0 then WHButtonDown(1);
  if mouse.usButtonData and RI_MOUSE_RIGHT_BUTTON_DOWN <> 0 then WHButtonDown(2);
  WHMouseMove(0);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHButtonDown(button: integer);
begin
  if not IsMouseEffectLocked and not FMouseOver and not sets.container.StayOnTop then SetNotForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WHMouseMove(LParam: LParam);
var
  pt: Windows.Tpoint;
  monitorRect, itemMgrRect: Windows.TRect;
  oldMouseOver, oldMouseOverEdge: boolean;
begin
  if not IsMouseEffectLocked and assigned(ItemMgr) and not FProgramIsClosing then
    if ItemMgr.Visible then
    begin
      Windows.GetCursorPos(pt);
      if (pt.x <> FLastMouseHookPoint.x) or (pt.y <> FLastMouseHookPoint.y) or (LParam = $fffffff) then
      begin
        FLastMouseHookPoint.x := pt.x;
        FLastMouseHookPoint.y := pt.y;
        ItemMgr.WHMouseMove(pt, (LParam <> $fffffff) and not IsHiddenDown);

        // detect mouse enter/leave //
        oldMouseOver := FMouseOver;
        oldMouseOverEdge := FMouseOverEdge;
        monitorRect := ItemMgr.FMonitorRect;
        itemMgrRect := ItemMgr.Rect;
        if sets.container.site = bsBottom then
          FMouseOver := (pt.y >= monitorRect.Bottom - 1) and (pt.x >= itemMgrRect.Left) and (pt.x <= itemMgrRect.Right)
        else if sets.container.site = bsTop then
          FMouseOver := (pt.y <= monitorRect.Top) and (pt.x >= itemMgrRect.Left) and (pt.x <= itemMgrRect.Right)
        else if sets.container.site = bsLeft then
          FMouseOver := (pt.x <= monitorRect.Left) and (pt.y >= itemMgrRect.Top) and (pt.y <= itemMgrRect.Bottom)
        else if sets.container.site = bsRight then
          FMouseOver := (pt.x >= monitorRect.Right - 1) and (pt.y >= itemMgrRect.Top) and (pt.y <= itemMgrRect.Bottom);
        FMouseOverEdge := FMouseOver;
        FMouseOver := FMouseOver or ItemMgr.CheckMouseOn or ItemMgr.FDraggingItem;
        if (FMouseOver and not oldMouseOver) or (FMouseOverEdge and not oldMouseOverEdge) then OnMouseEnter;
        if not FMouseOver and oldMouseOver then OnMouseLeave;
      end;
    end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnMouseEnter;
begin
  SetTimer(Handle, ID_TIMER_ROLL, sets.container.AutoShowTime, nil);
  // set foreground if 'activate' option selected //
  if IsWindowVisible(handle) and sets.container.ActivateOnMouse then
  begin
    if sets.container.ActivateOnMouseInterval = 0 then SetForeground
    else SetTimer(Handle, ID_TIMER_FOREGROUND, sets.container.ActivateOnMouseInterval, nil);
	end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnMouseLeave;
begin
  SetTimer(Handle, ID_TIMER_ROLL, sets.container.AutoHideTime, nil);
  KillTimer(Handle, ID_TIMER_FOREGROUND);
  // just to be sure
  ItemMgr.DragLeave;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FlashTaskWindow(hwnd: HWND);
begin
  if assigned(ItemMgr) then ItemMgr.AllItemCmd(icFlashTaskWindow, hwnd);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // put blur exactly behind the dock
  SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if button = mbRight then DoMenu;
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
      end else
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
      end else
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
    else if msg.WParam = ID_TIMER_DRAGLEAVE then OnTimerDragLeave
    else if msg.WParam = ID_TIMER_WINDOWCREATED then
    begin
      KillTimer(handle, ID_TIMER_WINDOWCREATED);
      UpdateRunning;
    end
    else if msg.WParam = ID_TIMER_FOREGROUND then OnTimerForeground;
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
    if assigned(ShellTrayWndController) then ShellTrayWndController.Timer;
    OnTimerDoRollUpDown;
  end;
  if not Tfrmsets.IsVisible then DoGlobalHotkeys;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerSlow;
begin
  if assigned(ItemMgr) and assigned(sets) then
  try
    if IsWindowVisible(Handle) then
    begin
      WHMouseMove($fffffff);
      MaintainNotForeground;
      ItemMgr.CheckDeleted;
    end;

    // maintain visibility
    if ItemMgr.visible and not IsWindowVisible(handle) then BaseCmd(tcSetVisible, 1);
    // maintain taskbar visibility
    if sets.container.HideSystemTaskbar then ShellTrayWndController.HideTaskbar(true);
  except
    on e: Exception do raise Exception.Create('Base.OnTimerSlow' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerFSA;
var
  fsa: boolean;
begin
  try
    // keep the edge of the screen reserved if set so
    if sets.container.ReserveScreenEdge then
      ShellTrayWndController.ReserveScreenEdge(
        sets.container.Monitor, sets.container.Site, sets.container.ReserveScreenEdgePercent, ItemMgr.FBaseWindowRect, sets.container.AutoHide);

    // hide/show the dock if fullscreen apps active
    if FHiddenByFSA and Visible then FHiddenByFSA := false;
    fsa := FullScreenAppActive;
    if sets.container.AutoHideOnFullScreenApp then
    begin
      if fsa and Visible then
      begin
        BaseCmd(tcSetVisible, 0);
        FHiddenByFSA := true;
      end;
      if not fsa and not Visible and FHiddenByFSA then BaseCmd(tcSetVisible, 1);
    end;
  except
    on e: Exception do raise Exception.Create('Base.OnTimerFSA' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerRoll;
var
  sets_visible: boolean;
begin
  if FMouseOver then
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
procedure Tfrmmain.OnTimerForeground;
begin
  KillTimer(Handle, ID_TIMER_FOREGROUND);
  if FMouseOver then SetForeground;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.UpdateRunning;
var
  parent: THandle = 0;
begin
  try
    if sets.container.ShowRunningIndicator or sets.container.Taskbar then
    begin
      if sets.container.TaskSameMonitor then parent := Handle;
      ProcessHelper.EnumAppWindows(parent);
      if ProcessHelper.WindowsCountChanged then ProcessHelper.EnumProc;
      if sets.container.ShowRunningIndicator then ItemMgr.SetParam(icUpdateRunning, 0);
		  if sets.container.Taskbar then ItemMgr.Taskbar;
		end;
	except
    on e: Exception do raise Exception.Create('Base.UpdateRunning' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.IsHiddenDown: boolean;
begin
  result := FWndOffsetTarget > 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoRollDown;
begin
  if ItemMgr.FDraggingItem or ItemMgr.FDraggingFile then exit;
  if sets.container.AutoHide and not IsHiddenDown then
  begin
    if sets.getBaseOrientation = boVertical then FWndOffsetTarget := ItemMgr.FBaseWindowRect.Width - sets.container.AutoHidePixels
    else FWndOffsetTarget := ItemMgr.FBaseWindowRect.Height - sets.container.AutoHidePixels;
    if FWndOffsetTarget < 0 then FWndOffsetTarget := 0;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoRollUp;
begin
  FWndOffsetTarget := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnTimerDoRollUpDown;
begin
  if sets.container.AutoHide then
    if FWndOffset <> FWndOffsetTarget then
    begin
      if abs(FWndOffsetTarget - FWndOffset) > RollStep then
      begin
        if FWndOffsetTarget > FWndOffset then inc(FWndOffset, RollStep) else dec(FWndOffset, RollStep);
      end
      else FWndOffset := FWndOffsetTarget;
      if assigned(ItemMgr) then ItemMgr.WndOffset := FWndOffset;
      {$ifdef EXT_DEBUG} AddLog('OnTimerDoRollUpDown. FWndOffset = ' + inttostr(FWndOffset)); {$endif}
    end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.IsHotkeyPressed(hotkey: integer): boolean;
var
  key, shift, pressedShift: integer;
begin
  result := false;
  key := hotkey and not (scShift + scCtrl + scAlt);
  shift := hotkey and (scShift + scCtrl + scAlt);
  if key <= 0 then exit;
  result := getasynckeystate(key) < 0;
  pressedShift := scNone;
  if getasynckeystate(16) < 0 then inc(pressedShift, scShift);
  if getasynckeystate(17) < 0 then inc(pressedShift, scCtrl);
  if getasynckeystate(18) < 0 then inc(pressedShift, scAlt);
  if shift <> pressedShift then result := false;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DoGlobalHotkeys;
var
  KeyPressed: boolean;
begin
  // hide/show
  if sets.container.GlobalHotkeyFlag_Hide then
  begin
	    KeyPressed := IsHotkeyPressed(sets.container.GlobalHotkeyValue_Hide);
	    if not KeyPressed then FHideKeysPressed := false
	    else begin
	      if not FHideKeysPressed then BaseCmd(tcToggleVisible, 0);
	      FHideKeysPressed := true;
	    end;
	end;
	// command window (console)
  if sets.container.GlobalHotkeyFlag_Console then
  begin
	    KeyPressed := IsHotkeyPressed(sets.container.GlobalHotkeyValue_Console);
	    if not KeyPressed then FConsoleKeysPressed := false
	    else begin
	      if not FConsoleKeysPressed then execute_cmdline('/cmdna');
	      FConsoleKeysPressed := true;
	    end;
	end;
end;
//------------------------------------------------------------------------------
// bring the dock along with all items to foreground
procedure Tfrmmain.SetForeground;
  procedure setfore(h: THandle);
  begin
    SetWindowPos(h, HWND_TOPMOST, 0, 0, 0, 0, SWP_NO_FLAGS);
    SetWindowPos(h, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NO_FLAGS);
  end;
begin
  if assigned(ItemMgr) then
  begin
	    // set all items topmost and place the dock window right underneath
	    SetWindowPos(handle, ItemMgr.ZOrder(HWND_TOPMOST), 0, 0, 0, 0, SWP_NO_FLAGS);
      // set dock window non topmost
	    SetWindowPos(handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NO_FLAGS);
	    // set blur window underneath the dock
	    SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS);
	    // set all items non topmost
      SetWindowPos(handle, ItemMgr.ZOrder(HWND_NOTOPMOST), 0, 0, 0, 0, SWP_NO_FLAGS);
	end;

  // bring to the foreground other program windows if any visible
  if assigned(frmItemProp) then setfore(frmItemProp.handle);
  if assigned(frmStackProp) then setfore(frmStackProp.handle);
  if assigned(frmSets) then setfore(frmSets.handle);
  if assigned(frmThemeEditor) then setfore(frmThemeEditor.handle);
end;
//------------------------------------------------------------------------------
// it is complicated. describe later ...
procedure Tfrmmain.SetNotForeground;

function IsDockWnd(wnd: THandle): boolean;
begin
  result := (wnd = handle) or (wnd = FBlurWindow) or (ItemMgr.IsItem(wnd) <> 0);
end;

function ZOrderIndex(hWnd: THandle): integer;
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

function DockAboveWnd(wnd: THandle): boolean;
var
  rect, dockrect: windows.TRect;
  buf: array [0..MAX_PATH - 1] of char;
begin
  result := false;
  ZeroMemory(@buf, MAX_PATH);
  ZeroMemory(@dockrect, sizeof(dockrect));
  if IsWindowVisible(wnd) and not IsIconic(wnd) then
  begin
    GetWindowRect(wnd, @rect);
    if assigned(ItemMgr) then dockrect := ItemMgr.Rect;
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
  if FProgramIsClosing then exit;
  GetCursorPos(pt);
  wnd := WindowFromPoint(pt);
  if IsDockWnd(wnd) then exit;
  awnd := GetAncestor(wnd, GA_ROOTOWNER);
  if IsWindow(awnd) then wnd := awnd;
  if assigned(ItemMgr) and DockAboveWnd(wnd) then
  begin
    SetWindowPos(FBlurWindow, wnd, 0, 0, 0, 0, SWP_NO_FLAGS);
    SetWindowPos(handle, wnd, 0, 0, 0, 0, SWP_NO_FLAGS);
    SetWindowPos(handle, ItemMgr.ZOrder(wnd), 0, 0, 0, 0, SWP_NO_FLAGS);
  end;
end;
//------------------------------------------------------------------------------
// keep all items on top of the dock window
procedure Tfrmmain.MaintainNotForeground;
var
  h: THandle;
  blurFound: boolean = false;
begin
  // scan all windows up from Progman
  h := FindWindow('Progman', nil);
  h := GetWindow(h, GW_HWNDPREV);
	while h <> 0 do
	begin
    if h = FBlurWindow then
      blurFound := true
    else
    if h = Handle then
    begin
      // if BlurWindow above the dock - put blur exactly behind the dock
      if not blurFound and IsWindow(FBlurWindow) then SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS);
      // main dock window found - exit
      exit;
    end
    else
    if ItemMgr.IsItem(h) <> 0 then // one of the items found (but not the dock yet) - adjust dock position
    begin
      if IsWindow(FBlurWindow) then SetWindowPos(FBlurWindow, h, 0, 0, 0, 0, SWP_NO_FLAGS);
      SetWindowPos(handle, h, 0, 0, 0, 0, SWP_NO_FLAGS);
      if IsWindow(FBlurWindow) then SetWindowPos(FBlurWindow, handle, 0, 0, 0, 0, SWP_NO_FLAGS);
      exit;
    end
    else
    if blurFound then // if blur found, but then found some other window before - put blur exactly behind the dock
      SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS);

    h := GetWindow(h, GW_HWNDPREV);
	end;
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
function Tfrmmain.GetHMenu(ParentMenu: THandle): THandle;
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
  if IsMenu(FMenu) then DestroyMenu(FMenu);
  if ParentMenu = 0 then FMenu := CreatePopupMenu else FMenu := ParentMenu;

  if ParentMenu = 0 then
    if IsValidItemString(GetClipboard) then
      AppendMenuW(FMenu, MF_STRING, IDM_PASTE, pwchar(UTF8Decode(XPaste)));

  // create submenu 'Add...' //

  FMenuCreate := CreatePopupMenu;
  AppendMenuW(FMenuCreate, MF_STRING + ifthen(ItemMgr._itemsDeleted.Count > 0, 0, MF_DISABLED), $f026, pwchar(UTF8Decode(XUndeleteIcon)));
  AppendMenuW(FMenuCreate, MF_STRING, $f023, pwchar(UTF8Decode(XSpecificIcons)));
  AppendMenuW(FMenuCreate, MF_STRING, $f021, pwchar(UTF8Decode(XEmptyIcon)));
  AppendMenuW(FMenuCreate, MF_STRING, $f022, pwchar(UTF8Decode(XFile)));
  AppendMenuW(FMenuCreate, MF_STRING, $f024, pwchar(UTF8Decode(XSeparator)));
  AppendMenuW(FMenuCreate, MF_STRING, $f025, pwchar(UTF8Decode(XDock)));
  if sets.GetPluginCount = -1 then sets.ScanPlugins;
  if sets.GetPluginCount > 0 then
  begin
    AppendMenuW(FMenuCreate, MF_SEPARATOR, 0, '-');
    idx := 0;
    while idx < sets.GetPluginCount do
    begin
      AppendMenuW(FMenuCreate, MF_STRING, $f041 + idx, pwchar(WideString(sets.GetPluginName(idx))));
      inc(idx);
    end;
  end;

  // insert all menu items //
  if ParentMenu <> 0 then AppendMenuW(FMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FMenu, MF_STRING + MF_POPUP, FMenuCreate, pwchar(UTF8Decode(XAddIcon)));
  AppendMenuW(FMenu, MF_STRING + ifthen(sets.container.LockDragging, MF_CHECKED, 0), IDM_LOCKICONS, pwchar(UTF8Decode(XLockIcons)));
  AppendMenuW(FMenu, MF_STRING, IDM_COLLECTION, pwchar(UTF8Decode(XIconCollection)));
  AppendMenuW(FMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FMenu, MF_STRING, IDM_TASKMGR, pwchar(UTF8Decode(XTaskManager)));
  AppendMenuW(FMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FMenu, MF_STRING, IDM_SETS, pwchar(UTF8Decode(XProgramSettings)));
  AppendMenuW(FMenu, MF_STRING, IDM_QUIT, pwchar(UTF8Decode(XExit)));

  Result := FMenu;
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
  msg.WParam := WPARAM(TrackPopupMenuEx(FMenu, TPM_RETURNCMD, pt.x, pt.y, handle, nil));
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
    if IsMenu(FMenu) then DestroyMenu(FMenu);

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
procedure Tfrmmain.BasePaint(flags: integer);
var
  dst: Pointer;
  bmp: gfx._SimpleBitmap;
  needRepaint: boolean;
begin
  if assigned(ItemMgr) and assigned(theme) and ItemMgr.Visible and not FProgramIsClosing then
  try
    dst := nil;
    needRepaint := flags and 1 = 1;

    if (ItemMgr.FBaseImageRect.X <> FOldBaseImageRect.X) or
       (ItemMgr.FBaseImageRect.Y <> FOldBaseImageRect.Y) or
       (ItemMgr.FBaseImageRect.Width <> FOldBaseImageRect.Width) or
       (ItemMgr.FBaseImageRect.Height <> FOldBaseImageRect.Height) then
    begin
      needRepaint := True;
      FOldBaseImageRect := ItemMgr.FBaseImageRect;
    end;
    if (ItemMgr.FBaseWindowRect.X <> FOldBaseWindowRect.X) or
       (ItemMgr.FBaseWindowRect.Y <> FOldBaseWindowRect.Y) or
       (ItemMgr.FBaseWindowRect.Width <> FOldBaseWindowRect.Width) or
       (ItemMgr.FBaseWindowRect.Height <> FOldBaseWindowRect.Height) then
    begin
      needRepaint := True;
      FOldBaseWindowRect := ItemMgr.FBaseWindowRect;
    end;

    if needRepaint then
    try
      // prepare a bitmap //
      bmp.topleft.x := ItemMgr.FBaseWindowRect.x;
      bmp.topleft.y := ItemMgr.FBaseWindowRect.y;
      bmp.Width     := ItemMgr.FBaseWindowRect.Width;
      bmp.Height    := ItemMgr.FBaseWindowRect.Height;
      if not CreateBitmap(bmp, Handle) then exit;
      dst := gfx.CreateGraphics(bmp.dc);
      if not assigned(dst) then
      begin
        DeleteBitmap(bmp);
        exit;
      end;
      GdipSetCompositingMode(dst, CompositingModeSourceOver);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);
      // workaround to eliminate twitch with certain backgrounds while dragging a file //
      if ItemMgr.FDraggingFile then GdipGraphicsClear(dst, ITEM_BACKGROUND);
      // draw dock background image //
      Theme.DrawBackground(dst, ItemMgr.FBaseImageRect, sets.container.BaseAlpha);
      // update dock window //
      UpdateLWindow(Handle, bmp, 255);
      UpdateBlurWindow;
    finally
      gfx.DeleteGraphics(dst);
      gfx.DeleteBitmap(bmp);
    end;
  except
    on e: Exception do raise Exception.Create('Base.BaseDraw' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.UpdateBlurWindow;
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  rect: GDIPAPI.TRect;
begin
  try
	  if sets.container.BlurEnabled and Theme.BlurEnabled and IsWindow(FBlurWindow) and IsWindowVisible(Handle) then
	  begin
	    FBlurActive   := true;
	    rect          := Theme.GetBlurRect(ItemMgr.FBaseImageRect);
	    bmp.topleft.x := rect.X + ItemMgr.FBaseWindowRect.X;
	    bmp.topleft.y := rect.Y + ItemMgr.FBaseWindowRect.Y;
	    bmp.width     := rect.Width;
	    bmp.height    := rect.Height;
	    if not CreateBitmap(bmp, FBlurWindow) then exit; //raise Exception.Create('CreateBitmap failed');
	    GdipCreateFromHDC(bmp.dc, dst);
	    if not assigned(dst) then
	    begin
	      DeleteBitmap(bmp);
	      exit; //raise Exception.Create('CreateGraphics failed');
	    end;
	    UpdateLWindow(FBlurWindow, bmp, 255);
	    SetWindowPos(FBlurWindow, 0, 0, 0, 0, 0, SWP_NO_FLAGS + SWP_NOZORDER + SWP_SHOWWINDOW);
	    DWM.EnableBlurBehindWindow(FBlurWindow, 0);
	    DeleteGraphics(dst);
	    DeleteBitmap(bmp);
	  end
	  else if FBlurActive then
	  begin
	    FBlurActive := false;
	    DWM.DisableBlurBehindWindow(FBlurWindow);
	    SetWindowPos(FBlurWindow, Handle, 0, 0, 0, 0, SWP_NO_FLAGS + SWP_HIDEWINDOW);
	  end;
  except
    on e: Exception do raise Exception.Create('Base.UpdateBlurWindow' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.trayiconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoMenu;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.err(where: WideString; e: Exception);
begin
  where := UTF8Decode(XErrorIn) + ' ' + where;
  if assigned(e) then where := where + LineEnding + WideString(e.message);
  notify(where);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.notify(message: WideString; silent: boolean = False);
begin
  if assigned(Notifier) then Notifier.Message(message, GetMonitorWorkareaRect(@FMonitor), False, silent)
  else if not silent then messageboxw(handle, pwchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.alert(message: WideString);
begin
  if assigned(Notifier) then Notifier.Message(message, GetMonitorWorkareaRect(@FMonitor), True, False)
  else messageboxw(handle, pwchar(message), nil, mb_iconerror);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.ActivateHint(hwnd: THandle; ACaption: WideString; x, y: integer);
var
  monitor: THandle;
begin
  try
    if not FProgramIsClosing and not IsHiddenDown and not IsMouseEffectLocked
       and not ItemMgr.FDraggingFile and not ItemMgr.FDraggingItem then
    begin
      if FInitDone and not assigned(AHint) then AHint := THint.Create;
      if hwnd = 0 then monitor := MonitorFromWindow(Handle, 0) else monitor := MonitorFromWindow(hwnd, 0);
      if assigned(AHint) then AHint.ActivateHint(hwnd, ACaption, x, y, monitor, sets.container.Site);
    end;
  except
    on e: Exception do AddLog('frmmain.ActivateHint' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.DeactivateHint(hwnd: THandle);
begin
  if not FProgramIsClosing and assigned(AHint) then AHint.DeactivateHint(hwnd);
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
    if IsMouseEffectLocked and assigned(AHint) then AHint.DeactivateImmediate;
    SetParam(gpLockMouseEffect, integer(IsMouseEffectLocked));
  finally
    crsection.Leave;
  end;
end;
//------------------------------------------------------------------------------
function Tfrmmain.IsMouseEffectLocked: boolean;
begin
  result := LockList.Count > 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.SetFont(var Value: TDFontData);
begin
  CopyFontData(Value, sets.container.Font);
  if assigned(ItemMgr) then ItemMgr.SetFont(Value);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.AppException(Sender: TObject; e: Exception);
begin
  notify('[AppException] ' + LineEnding + Sender.ClassName + LineEnding + e.message);
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
  if assigned(pMonitor) then monitor := pMonitor^ else monitor := FMonitor;
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
  if assigned(pMonitor) then monitor := pMonitor^ else monitor := FMonitor;
  if monitor >= screen.MonitorCount then monitor := screen.MonitorCount - 1;
  if monitor >= 0 then Result := screen.Monitors[monitor].BoundsRect;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDragEnter(list: TStrings; hWnd: THandle);
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
  if not PtInRect(ItemMgr.Rect, pt) then
  begin
    KillTimer(Handle, ID_TIMER_DRAGLEAVE);
    ItemMgr.DragLeave;
    BasePaint(1);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.OnDrop(files: TStrings; hWnd: THandle);
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
begin
  ItemMgr.InsertFiles(files);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WMCopyData(var Message: TMessage);
var
  pcds: PCOPYDATASTRUCT;
  ppd: PTDProgramData;
begin
  message.Result := 1;
  pcds := PCOPYDATASTRUCT(message.lParam);

  if pcds^.dwData = DATA_PROGRAM then
  begin
    if pcds^.cbData <> sizeof(TDProgramData) then
    begin
      message.Result := 0;
      notify(UTF8Decode(XErrorInvalidProgramDataStructureSize));
      exit;
    end;
    ppd := PTDProgramData(pcds^.lpData);
    ItemMgr.InsertItem(TShortcutItem.Make(pchar(ppd^.Name), ZipPath(pchar(ppd^.Filename)), '', '', '', SW_SHOWNORMAL));
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
    ItemMgr.InsertItem(Filename);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.NewDock;
begin
  if assigned(docks) then
    if docks.HaveFreeSite then docks.NewDock;
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
procedure Tfrmmain.WMDisplayChange(var Message: TMessage);
begin
  screen.UpdateMonitors;
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
procedure Tfrmmain.WMDPIChanged(var Message: TMessage);
begin
  //GetDpiForMonitor
  BaseCmd(tcThemeChanged, 0);
  message.Result := 0;
end;
//------------------------------------------------------------------------------
function Tfrmmain.FullScreenAppActive: boolean;
const
  clsPM = 'Progman';
  clsWW = 'WorkerW';
  clsAVP = 'AVP.SandboxBorderWindow'; // Kaspersky
  MAX_CLS_LEN = 24;
var
  wnd: hWnd;
  rc, rMonitor: windows.TRect;
  cls: array [0..MAX_CLS_LEN - 1] of char;
begin
  result := false;
  ZeroMemory(@cls, MAX_CLS_LEN);
  rMonitor := ItemMgr.FMonitorRect;
  wnd := GetWindow(Handle, GW_HWNDFIRST);
  while wnd <> 0 do
  begin
    if wnd = Handle then exit; // exit if we reached main dock window
    if IsWindow(wnd) then
    begin
      if IsWindowVisible(wnd) and not DWM.IsWindowCloaked(wnd) then
      begin
        if GetWindowLongPtr(wnd, GWL_STYLE) and WS_CAPTION = 0 then
        begin
          GetWindowRect(wnd, rc);
          if (rc.Left <= rMonitor.Left) and (rc.Top <= rMonitor.Top) and (rc.Right >= rMonitor.Right) and (rc.Bottom >= rMonitor.Bottom) then
          begin
            GetClassName(wnd, cls, MAX_CLS_LEN - 1);
		        if (strlcomp(@cls, clsPM, 7) <> 0) and (strlcomp(@cls, clsWW, 7) <> 0) and (strlcomp(@cls, clsAVP, 23) <> 0) then
            begin
              result := true;
              exit;
						end;
					end;
				end;
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
    if wnd = Handle then
    begin
      result := result + 'HWnd = ' + inttostr(wnd) + LineEnding;
      FillChar(cls, MAX_PATH, #0);
      GetClassName(wnd, cls, MAX_PATH);
      result := result + 'Class = ' + strpas(@cls) + LineEnding;
      FillChar(cls, MAX_PATH, #0);
      GetWindowText(wnd, cls, MAX_PATH);
      result := result + 'Text = ' + strpas(@cls) + LineEnding;
      GetWindowRect(wnd, rc);
      result := result + 'Rect = ' + inttostr(rc.Left) + ', ' + inttostr(rc.Top) + ', ' + inttostr(rc.Right) + ', ' + inttostr(rc.Bottom) + LineEnding;
      FillChar(cls, MAX_PATH, #0);
      GetWindowModuleFileName(wnd, cls, MAX_PATH);
      result := result + 'Module = ' + strpas(@cls) + LineEnding + LineEnding;
		end
    else
		if IsWindowVisible(Wnd) then
    begin
        GetWindowRect(wnd, rc);
        if (GetWindowLongPtr(wnd, GWL_STYLE) and WS_CAPTION = 0) and
          (rc.Left <= rMonitor.Left) and (rc.Top <= rMonitor.Top) and (rc.Right >= rMonitor.Right) and (rc.Bottom >= rMonitor.Bottom) then
        begin
          result := result + 'HWnd = ' + inttostr(wnd) + LineEnding;
          FillChar(cls, MAX_PATH, #0);
          GetClassName(wnd, cls, MAX_PATH);
          result := result + 'Class = ' + strpas(@cls) + LineEnding;
          FillChar(cls, MAX_PATH, #0);
          GetWindowText(wnd, cls, MAX_PATH);
          result := result + 'Text = ' + strpas(@cls) + LineEnding;
          result := result + 'Rect = ' + inttostr(rc.Left) + ', ' + inttostr(rc.Top) + ', ' + inttostr(rc.Right) + ', ' + inttostr(rc.Bottom) + LineEnding;
          FillChar(cls, MAX_PATH, #0);
          GetWindowModuleFileName(wnd, cls, MAX_PATH);
          result := result + 'Module = ' + strpas(@cls) + LineEnding + LineEnding;
        end;
    end;
    wnd := GetWindow(wnd, GW_HWNDNEXT);
  end;
end;
//------------------------------------------------------------------------------
// show popup menu to select a theme
procedure Tfrmmain.ThemesMenu;
var
  menu: HMENU;
  pt: windows.TPoint;
  ret: integer;
  mii: TMenuItemInfo;
  mname: array [0..MAX_PATH - 1] of char;
begin
  GetCursorPos(pt);
  menu := CreatePopupMenu;
  AppendMenuW(menu, MF_STRING, $f000, pwchar(UTF8Decode(XOpenThemesFolder)));
  AppendMenuW(menu, MF_SEPARATOR, 0, '-');
  theme.ThemesMenu(pchar(sets.container.ThemeName), menu);
  LockMouseEffect(Handle, true);
  SetForegroundWindow(handle);
  SetForeground;
  ret := integer(TrackPopupMenuEx(menu, TPM_RETURNCMD, pt.x, pt.y, handle, nil));
  LockMouseEffect(Handle, false);
  if ret = $f000 then execute_cmdline(theme.ThemesFolder)
  else
  if ret <> 0 then
  begin
    FillChar(mii, sizeof(mii), #0);
    mii.cbSize := sizeof(mii);
    mii.dwTypeData := @mname;
    mii.cch := MAX_PATH;
    mii.fMask := MIIM_STRING;
    GetMenuItemInfo(menu, ret, false, @mii);
    if strlen(pchar(@mname)) > 0 then setTheme(pchar(@mname));
  end;
end;
//------------------------------------------------------------------------------
// set a position for taskbar icons to be placed on the dock
procedure Tfrmmain.SetTaskSpot(wnd: THandle);
var
  spot: integer;
begin
  spot := ItemMgr.ItemIndex(wnd);
  if spot = ItemMgr.FItemCount - 1 then spot := -1;
  SetParam(gpTaskSpot, spot);
end;
//------------------------------------------------------------------------------
// show the list of running tasks (processes) and modules
procedure Tfrmmain.ListTasksAndModules;
begin
  notify('----- tasks -----');
  notify(WideString(ProcessHelper.Processes));
  notify('----- modules -----');
  notify(WideString(ProcessHelper.ProcessesFullName));
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.WinampCmd(params: string; showcmd: integer = sw_shownormal);
begin
  if SameText(params, 'play') then
  begin
    if not boolean(FindWinamp) then LaunchWinamp(showcmd) else wacmd(40045);
  end
  else if SameText(params, 'pause') then              wacmd(40046)
  else if SameText(params, 'stop') then               wacmd(40047)
  else if SameText(params, 'previous') then           wacmd(40044)
  else if SameText(params, 'next') then               wacmd(40048)
  else if SameText(params, 'close') then              wacmd(40001)
  else if SameText(params, 'preferences') then        wacmd(40012)
  else if SameText(params, 'open_file') then          wacmd(40029)
  else if SameText(params, 'stop_after_current') then wacmd(40157)
  else if SameText(params, 'visualization') then      wacmd(40192)
  else if SameText(params, 'start_of_playlist') then  wacmd(40154)
  else if SameText(params, 'end_of_playlist') then    wacmd(40158);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.mexecute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = SW_SHOWNORMAL; hwnd: cardinal = 0);
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
        'Command parse/execute error.' + LineEnding +
        'Command=' + acmd + LineEnding + 'Params=' + aparams + LineEnding + 'SYSMSG=' + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.execute_cmdline(cmd: string; showcmd: integer = sw_shownormal);
var
  params: string;
begin
  if cmd[1] = '/' then
  begin
    split(cmd, ' ', cmd, params);
  end
  else
  begin
    split_cmd(cmd, cmd, params);
  end;
  execute(cmd, params, '', showcmd);
end;
//------------------------------------------------------------------------------
procedure Tfrmmain.execute(cmd: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal; hwndCaller: cardinal = 0);
var
  i, i1, i2, i3, i4: integer;
  str1, str2: string;
  lpsz1, lpsz2: pchar;
  pt: windows.TPoint;
begin
  if cmd = '' then exit;

  if cmd[1] <> '/' then
  begin
    Run(cmd, params, dir, showcmd);
    exit;
  end;

  system.Delete(cmd, 1, 1);

  if cut(cmd, '.') = 'itemmgr' then frmmain.ItemMgr.command(cutafter(cmd, '.'), toolu.UnzipPath(params))
  else if cmd = 'quit' then         frmmain.CloseProgram
  else if cmd = 'hide' then         frmmain.BaseCmd(tcSetVisible, 0)
  else if cmd = 'say' then          frmmain.notify(toolu.UnzipPath(params))
  else if cmd = 'alert' then        frmmain.alert(toolu.UnzipPath(params))
  else if cmd = 'visible' then      frmmain.BaseCmd(tcToggleVisible, 0)
  else if cmd = 'systaskbar' then   frmmain.BaseCmd(tcToggleTaskbar, 0)
  else if cmd = 'sets' then         Tfrmsets.Open
  else if cmd = 'cmd' then          Tfrmcmd.Open
	else if cmd = 'cmdna' then        Tfrmcmd.Open(true)
	else if cmd = 'collection' then   Run('%pp%\images')
  else if cmd = 'taskmgr' then      Run('%sysdir%\taskmgr.exe')
  else if cmd = 'undelete' then     ItemMgr.UnDelete
  else if cmd = 'program' then      AddFile
  else if cmd = 'newdock' then      NewDock
  else if cmd = 'removedock' then   RemoveDock
  else if cmd = 'command' then      TfrmAddCommand.Open
  else if cmd = 'hello' then        TfrmHello.Open
  else if cmd = 'help' then         TfrmTip.Open
  else if cmd = 'backup' then       sets.Backup
  else if cmd = 'restore' then      TfrmRestore.Open
  else if cmd = 'paste' then        ItemMgr.InsertItem(GetClipboard)
  else if cmd = 'autotray' then     Tray.SwitchAutoTray
  else if cmd = 'tray' then         Tray.ShowTrayOverflow(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'volume' then       Tray.ShowVolumeControl(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'networks' then     Tray.ShowNetworks(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'battery' then      Tray.ShowBattery(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'actioncenter' then Tray.ShowActionCenter(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'startmenu' then    ShellTrayWndController.ShowStartMenu(sets.container.Site, hwndCaller, ItemMgr.Rect, GetMonitorWorkareaRect)
  else if cmd = 'theme' then        ThemesMenu
  else if cmd = 'taskspot' then     SetTaskSpot(hwndCaller)
  else if cmd = 'themeeditor' then  TfrmThemeEditor.Open
  else if cmd = 'lockdragging' then SetParam(gpLockDragging, ifthen(sets.GetParam(gpLockDragging) = 0, 1, 0))
  else if cmd = 'site' then         SetParam(gpSite, integer(StringToSite(params)))
  else if cmd = 'logoff' then       ProcessHelper.Shutdown(ifthen(params = 'force', 4, 0))
  else if cmd = 'shutdown' then     ProcessHelper.Shutdown(ifthen(params = 'force', 5, 1))
  else if cmd = 'reboot' then       ProcessHelper.Shutdown(ifthen(params = 'force', 6, 2))
  else if cmd = 'suspend' then      ProcessHelper.SetSuspendState(false)
  else if cmd = 'hibernate' then    ProcessHelper.SetSuspendState(true)
  else if cmd = 'kill' then         ProcessHelper.Kill(params)
  else if cmd = 'displayoff' then   sendmessage(handle, WM_SYSCOMMAND, SC_MONITORPOWER, 2)
  else if cmd = 'emptybin' then     SHEmptyRecycleBin(Handle, nil, 0)
  else if cmd = 'winamp' then       WinampCmd(params, showcmd)
  else if cmd = 'play' then         sndPlaySound(pchar(UnzipPath(params)), SND_ASYNC or SND_FILENAME)
  else if cmd = 'guid' then         SetClipboard(CreateClassId)
  else if cmd = 'debug' then        frmmain.BaseCmd(tcDebugInfo, 0)
  else if cmd = 'tasks' then        ListTasksAndModules
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
  else if cmd = 'showwindow' then showwindow(THandle(inta), strtoint(params))
  else if cmd = 'sendmessage' then
  begin
    if not trystrtoint(Trim(fetch(params, ',', true)), i2) then i2 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i3) then i3 := 0;
    if not trystrtoint(Trim(fetch(params, ',', true)), i4) then i4 := 0;
    sendmessage(THandle(inta), uint(i2), i3, i4);
  end;
end;
//------------------------------------------------------------------------------
// thread function
function RunThread(p: pointer): PtrInt;
var
  Data: PRunData absolute p;
  hostHandle: THandle;
begin
  result := 0;
  hostHandle := Data.handle;
  shellexecute(hostHandle, nil, pwchar(@Data.exename), pwchar(@Data.params), pwchar(@Data.dir), Data.showcmd);
  Dispose(Data);
  // request main form to close thread handle
  postmessage(hostHandle, WM_APP_RUN_THREAD_END, 0, LPARAM(GetCurrentThread));
end;
//------------------------------------------------------------------------------
// create new thread, if needed, and run a program
procedure Tfrmmain.Run(exename: string; params: string = ''; dir: string = ''; showcmd: integer = sw_shownormal);
var
  Data: PRunData;
  wexename, wparams, wdir: WideString;
  shell: string;
  sei: TShellExecuteInfoW;
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
      ZeroMemory(@sei, sizeof(sei));
      sei.cbSize := sizeof(sei);
	    sei.lpIDList := aPIDL;
	    sei.fMask := SEE_MASK_IDLIST;
	    sei.Wnd := Handle;
	    sei.nShow := 1;
	    sei.lpVerb := 'open';
	    ShellExecuteExW(@sei);
      PIDL_Free(aPIDL);
      exit;
		end;

    wexename := exename;
    wparams := params;
    wdir := dir;

    if sets.container.RunInThread then
    begin
	    New(Data);
	    Data.handle := Handle;
	    strcopy(pwchar(@Data.exename), pwchar(wexename));
	    strcopy(pwchar(@Data.params), pwchar(wparams));
	    strcopy(pwchar(@Data.dir), pwchar(wdir));
	    Data.showcmd := showcmd;
	    if BeginThread(RunThread, Data) = 0 then
        notify(WideString('Run.BeginThread failed' + LineEnding +
          'cmd=' + exename + LineEnding +
          'params=' + params + LineEnding + 'dir=' + dir));
    end else
    begin
      shellexecutew(Handle, nil, PWChar(wexename), PWChar(wparams), PWChar(wdir), showcmd);
    end;
  except
    on e: Exception do err('Base.Run', e);
  end;
end;
//------------------------------------------------------------------------------
end.

