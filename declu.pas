unit declu;

interface
uses Windows, SysUtils, DefaultTranslator;

type
  TBaseSite = (bsLeft = 0, bsTop, bsRight, bsBottom);
  TBaseOrientation = (boHorizontal, boVertical);
  TExecuteAction = (eaDefault, eaRun, eaGroup);

  TGParam = (
    // general parameters //
    gpMin = 0,
    gpMonitor,
    gpSite,
    gpCenterOffsetPercent,
    gpEdgeOffset,
    gpOccupyFullMonitor,
    gpStartOffset,
    gpEndOffset,
    gpItemSize,
    gpBigItemSize,
    gpZoomWidth,
    gpItemSpacing,
    gpZoomEnabled,
    gpZoomTime,
    gpReserveScreenEdge,
    gpReserveScreenEdgePercent,
    gpTaskbar, // show taskbar items on the dock
    gpTaskLivePreviews,
    gpTaskThumbSize,
    gpTaskGrouping,
    gpTaskSameMonitor,
    gpAutoHide,
    gpAutoHideTime,
    gpAutoHidePixels,
    gpAutoShowTime,
    gpGlobalHotkeyFlag_Hide,
    gpGlobalHotkeyValue_Hide,
    gpGlobalHotkeyFlag_Console,
    gpGlobalHotkeyValue_Console,
    gpDropDistance,
    gpLaunchInterval,
    gpUseShell,
    gpRunInThread,
    gpLaunchInThread,
    gpActivateOnMouse,
    gpActivateOnMouseInterval,
    gpCloseCmdWindow,
    gpHideSystemTaskbar,
    gpStayOnTop,
    gpShowHint,
    gpHintEffects,
    gpLockDragging,
    gpAutoHideOnFullScreenApp,
    gpUseShellContextMenus,
    gpBaseAlpha,
    gpSeparatorAlpha,
    gpActivateRunningApps,
    gpActivateRunning,
    gpShowRunningIndicator,
    gpItemAnimationType,
    gpStackAnimationEnabled,
    gpLockMouseEffect,
    gpReflectionEnabled,
    gpReflectionSize,
    gpBlurEnabled,
    gpTaskSpot, // index where to place task items
    gpMax,
    // terry commands //
    tcMin = $1000,
    tcActivate,
    tcRepaintBase,
    tcMenu,
    tcSaveSets,
    tcThemeChanged,
    tcSetVisible,
    tcGetVisible,
    tcToggleVisible,
    tcToggleTaskbar,
    tcDebugInfo,
    tcMax,
    // item commands //
    icMin = $2000,
    icSelect, // informs item that user pressed mouse button on it //
    icUndock, // informs item that user draging this item //
    icFree, // marks item as freed //
    icHover, // mouse over //
    icUpdateRunning, // forces item to update it's running indicator //
    icDragEnter,
    icDragOver,
    icDragLeave,
    icIsItem,
    icDropIndicator,
    icVisible,
    icFlashTaskWindow,
    icMax);

  PFontData = ^_FontData;
  _FontData = packed record
    name: array [0..255] of char;
    size: integer;
    size2: integer;
    color: cardinal;
    backcolor: cardinal;
    bold: boolean;
    italic: boolean;
  end;

  TDItemCreateParams = packed record
    ItemSize: integer;
    BigItemSize: integer;
    ItemSpacing: integer;
    LaunchInterval: integer;
    ActivateRunning: boolean;
    UseShellContextMenus: boolean;
    Site: integer;
    Reflection: boolean;
    ReflectionSize: integer;
    ShowHint: boolean;
    AnimationType: integer;
    LockDragging: boolean;
    StackAnimationEnabled: boolean;
    SeparatorAlpha: integer;
    TaskLivePreviews: boolean;
    TaskThumbSize: integer;
    TaskGrouping: boolean;
    Font: _FontData;
  end;

  TBaseCmd = function(id: TGParam; param: integer): integer of object;

  RAWMOUSE = packed record
    usFlags: USHORT;
    usButtonFlags: USHORT;
    usButtonData: USHORT;
    ulRawButtons: ULONG;
    lLastX: LONG;
    lLastY: LONG;
    ulExtraInformation: ULONG;
  end;

  RAWKEYBOARD = record
    MakeCode: USHORT;
    Flags: USHORT;
    Reserved: USHORT;
    VKey: USHORT;
    Message: UINT;
    ExtraInformation: ULONG;
  end;

  RAWHID = record
    dwSizeHid: DWORD;
    dwCount: DWORD;
    bRawData: array [0..0] of BYTE;
  end;

  RAWINPUTHEADER = packed record
    dwType: DWORD;
    dwSize: DWORD;
    hDevice: DWORD;
    wParam: WPARAM;
  end;

  RAWINPUT = packed record
    header: RAWINPUTHEADER;
    case Integer of
      0: (mouse: RAWMOUSE);
      1: (keyboard: RAWKEYBOARD);
      2: (hid: RAWHID);
  end;
  PRAWINPUT = ^RAWINPUT;

  // for copying data from AppSearch //
  PProgramData = ^TProgramData;
  TProgramData = record
    Name: array [0..1023] of char;
    Filename: array [0..1023] of char;
  end;

  MONITORINFO = record
    cbSize: dword;
    rcMonitor: TRect;
    rcWork: TRect;
    dwFlags: dword;
  end;

  function MonitorFromWindow(HWND: hwnd; dwFlags: DWORD): THandle; stdcall; external 'user32.dll';
  function MonitorFromPoint(pt: windows.TPoint; dwFlags: DWORD): THandle; stdcall; external 'user32.dll';
  function GetMonitorInfoA(hMonitor: THandle; lpmi: pointer): bool; stdcall; external 'user32.dll';
  function SHGetNameFromIDList(_para1:LPCITEMIDLIST; sigdnName: longint; _para2:LPTSTR):WINBOOL; external 'shell32.dll' name 'SHGetNameFromIDList';

const
  PROGRAM_NAME = 'TDock';
  PROGRAM_TITLE = 'TDockApp';
  PROGRAM_REGKEY = 'tdock';
  PROGRAM_GUID = '{CF102D02-5C0B-4383-8902-2500AF8859B7}';
  WINITEM_CLASS = 'TDockWClass';
  ITEM_BACKGROUND = $2808080;
  RollStep = 4;
  SWP_NO_FLAGS = SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOOWNERZORDER + SWP_NOSENDCHANGING + SWP_ASYNCWINDOWPOS;
  NOT_AN_ITEM = $ffff; // result const in case when item (items[]) not found
  MONITOR_DEFAULTTONEAREST = 2;

  // icon Hint align
  HORIZONTAL_BOTTOM = 0;
  HORIZONTAL_LEFT = 4;
  VERTICAL_TOP = 5;
  HORIZONTAL_RIGHT = 6;
  VERTICAL_BOTTOM = 7;

  // common window messages
  WM_DPICHANGED = $02E0;

  // private WM_APP messages //
  WM_APP_UPDATE_PREVIEW = WM_APP + 1;
  WM_APP_RUN_THREAD_END = WM_APP + 2;

  // private popup menu command IDs //
  IDM_PASTE = $f030;
  IDM_LOCKICONS = $f031;
  IDM_COLLECTION = $f032;
  IDM_TASKMGR = $f033;
  IDM_SETS = $f034;
  IDM_QUIT = $f035;

  // WM_COPYDATA CDS.dwType
  DATA_PROGRAM = $f001; // TProgramData struct

  // private timer event IDs //
  ID_TIMER                  = 1;
  ID_TIMER_SLOW             = 2;
  ID_TIMER_FSA              = 3;
  ID_TIMER_OPEN             = 4;
  ID_TIMER_CLOSE            = 5;
  ID_TIMER_MOUSEHELD        = 6;
  ID_TIMER_UPDATE_SHORTCUT  = 7;
  ID_TIMER_ROLL             = 8;
  ID_TIMER_DRAGLEAVE        = 9;
  ID_TIMER_FOREGROUND       = 10;

  ___ = 'What is mind? No matter. What is matter? Never mind.';

resourcestring
  // common //
  XErrorContactDeveloper = 'Contact developer if error is permanent.';
  XErrorCritical = 'Critical error occured. Try to run the program again.';
  XErrorLoadTheme = 'Theme load error. Try to run the program again.';
  XErrorThemeObjectNotFound = 'Theme object not found';
  XMsgThemeSaved = 'Theme saved';
  XErrorSetsCorrupted = 'Settings file corrupted.';
  XErrorSetsRestoreFailed = 'Restore operation failed.';
  XMsgSetsRestored = 'The settings are restored from backup.';
  XMsgRunAgain = 'Run the program again.';
  XMsgRunRestore = 'Run restore operation.';
  XErrorSetsBackupFailed = 'Failed to backup settings.';
  XErrorHelpNotFound = 'Help file not found! Try reinstalling the program.';
  XErrorIn = 'Error in';
  XStartButtonText = 'Start';
  // frmterryu //
  XEmptyIcon = 'Empty icon';
  XSeparator = 'Separator (space)';
  XDock = 'New dock';
  XInstalledApplication = 'Installed application';
  XAddIcon = 'Add ...';
  XSpecificIcons = 'Specific icons';
  XLockIcons = 'Lock icons';
  XIconCollection = 'Open icon collection';
  XTaskManager = 'Task manager';
  XProgramSettings = 'Program settings';
  XExit = 'Exit';
  XCloseWindow = 'Close window';
  XCloseAllWindows = 'Close all windows';
  XMinimizeRestoreAllWindows = 'Minimize/restore all windows';
  XEmptyBin = 'Empty bin';
  XOpenThemesFolder = 'Open themes folder';
  XUndeleteIcon = 'Undelete icon  (Ctrl+Z)';
  XMsgPlaceTargetFileInsteadOfShortcut = 'Place target file onto dock instead of shortcut?';
  XShortcut = 'Shortcut';
  XFile = 'File';
  XErrorInvalidProgramDataStructureSize = 'Invalid program data structure size';
  // items //
  XRun = 'Run  (Ctrl + Click)';
  XConfigureIcon = 'Configure icon';
  XOpenFolderOf = 'Open folder of';
  XCopy = 'Copy';
  XPaste = 'Paste';
  XDeleteIcon = 'Delete icon';
  XDeleteSeparator = 'Delete separator';
  XPinToDock = 'Pin to dock';
  XKillProcess = 'Kill process';
  XPlaceTasksHere = 'Place tasks here';
  // frmsetsu //
  XLabelCenterOffset = 'Offset from center: %d %%';
  XLabelEdgeOffset = 'Offset from edge: %d pt';
  XLabelIconSize = 'Icon size: %d pt';
  XLabelZoomedIconSize = 'Icon size max: %d pt';
  XLabelIconSpacing = 'Icon spacing: %d pt';
  XLabelZoomWidth = 'Zoom width: %d';
  XLabelZoomTime = 'Zoom time: %d ms';
  XLabelReserveScreenEdgePercent = 'Reserve screen edge: %d%%';
  XPageGeneral = 'General';
  XPagePosition = 'Position';
  XPageStyle = 'Style';
  XPageIcons = 'Icons';
  XPageMisc = 'Misc';
  XPageAutorun = 'Autorun';
  XPageAbout = 'About';
  XSiteLeft = 'Left';
  XSiteTop = 'Top';
  XSiteRight = 'Right';
  XSiteBottom = 'Bottom';
  XAnimationNoAnimation = 'No animation';
  XAnimationRotate = 'Rotate';
  XAnimationBounceOnce = 'Bounce once';
  XAnimationBounceTwice = 'Bounce twice';
  XAnimationBounce3Times = 'Bounce 3 times';
  XAnimationQuake = 'Quake';
  XAnimationSwing = 'Swing';
  XAnimationVibrate = 'Vibrate';
  XAnimationZoom = 'Zoom';
  XAll = 'All';
  XMonitor = 'Monitor';
  // frmitemoptu / frmstackpropu //
  XShowCmdNormal = 'Normal size';
  XShowCmdMinimized = 'Minimized to taskbar';
  XShowCmdMaximized = 'Maximized to fullscreen';
  XSelectWorkingDirectory = 'Select working directory';
  XMsgUnsavedIconParams = 'Icon parameters changed. If you choose to proceed all changes will be lost. Proceed?';
  XOffsetOfIcons = 'Offset of icons: %d pt';
  XDistort = 'Spacing/Distortion: %d';
  XAlphaChannel = 'Alpha channel: %d';
  // frmstackpropu //
  XStackPreviewNone = 'none';
  XStackPreviewFour = 'four';
  XStackPreviewNine = 'nine';
  // frmAddCommand //
  XErrorCommandListNotFound = 'Command list file not found. Try reinstalling the program.';
  // trayController //
  XMsgNotificationAreaIcons = 'After you close this message you will see "Notification area icons" window. To use this function uncheck "Always show all icons" at the bottom of the window and set all behaviors to "Only show notifications".';


  function SiteToString(site: TBaseSite): string;
  function StringToSite(str: string): TBaseSite;
  procedure CopyFontData(var fFrom: _FontData; var fTo: _FontData);

implementation
//------------------------------------------------------------------------------
function SiteToString(site: TBaseSite): string;
begin
  result := 'left';
  if site = bsTop then result := 'top'
  else if site = bsRight then result := 'right'
  else if site = bsBottom then result := 'bottom';
end;
//------------------------------------------------------------------------------
function StringToSite(str: string): TBaseSite;
begin
  result := bsLeft;
  if str = 'top' then result := bsTop
  else if str = 'right' then result := bsRight
  else if str = 'bottom' then result := bsBottom;
end;
//--------------------------------------------------------------------------------------------------
procedure CopyFontData(var fFrom: _FontData; var fTo: _FontData);
begin
  strcopy(@fTo.name, @fFrom.name);
  fTo.size      := fFrom.size;
  fTo.size2     := fFrom.size2;
  fTo.color     := fFrom.color;
  fTo.backcolor := fFrom.backcolor;
  fTo.bold      := fFrom.bold;
  fTo.italic    := fFrom.italic;
end;
//------------------------------------------------------------------------------
end.
