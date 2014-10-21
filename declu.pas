unit declu;

interface
uses Windows, DefaultTranslator;

type
  TBaseSite = (bsLeft = 0, bsTop, bsRight, bsBottom);
  TBaseOrientation = (boHorizontal, boVertical);

  TGParam = (
    // general parameters //
    gpMin = 0,
    gpItemSize,
    gpBigItemSize,
    gpZoomWidth,
    gpItemSpacing,
    gpZoomItems,
    gpZoomTime,
    gpReserveScreenEdge,
    gpReserveScreenEdgePercent,
    gpTaskbar,
    gpSite,
    gpAutoHide,
    gpAutoHideTime,
    gpAutoHidePixels,
    gpCenterOffsetPercent,
    gpHideKeys,
    gpDropDistance,
    gpLaunchInterval,
    gpUseShell,
    gpLaunchInThread,
    gpActivateOnMouse,
    gpCloseCmdWindow,
    gpHideTaskBar,
    gpStayOnTop,
    gpShowHint,
    gpHintEffects,
    gpLockDragging,
    gpAutoHideOnFullScreenApp,
    gpUseShellContextMenus,
    gpBaseAlpha,
    gpActivateInvokedApps,
    gpMonitor,
    gpAutoShowTime,
    gpEdgeOffset,
    gpActivateRunning,
    gpShowRunningIndicator,
    gpItemAnimation,
    gpStackOpenAnimation,
    gpLockMouseEffect,
    gpReflection,
    gpReflectionSize,
    gpBlur,
    gpMax,
    // terry commands //
    tcMin = $1000,
    tcRepaintBase,
    tcMenu,
    tcSaveSets,
    tcThemeChanged,
    tcSetVisible,
    tcGetVisible,
    tcZOrder,
    tcToggleVisible,
    tcToggleTaskbar,
    tcMax,
    // item commands //
    icMin = $2000,
    icSelect, // informs item that user pressed mouse button on it //
    icFloat, // informs item that user draging this item //
    icFree, // marks item as freed //
    icHover, // mouse over //
    icUpdateRunning, // forces item to update it's running indicator //
    icDragEnter,
    icDragOver,
    icDragLeave,
    icIsItem,
    icDropIndicator,
    icMax);

  _ItemCreateParams = record
    ItemSize: integer;
    BigItemSize: integer;
    LaunchInterval: integer;
    ActivateRunning: boolean;
    UseShellContextMenus: boolean;
    Site: integer;
    Reflection: boolean;
    ReflectionSize: integer;
    ShowHint: boolean;
    Animation: integer;
    LockDragging: boolean;
    StackOpenAnimation: boolean;
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

  RAWINPUTHEADER = packed record
    dwType: DWORD;
    dwSize: DWORD;
    hDevice: DWORD;
    wParam: WPARAM;
  end;

  RAWINPUT = packed record
    header: RAWINPUTHEADER;
    mouse: RAWMOUSE;
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
  function GetMonitorInfoA(hMonitor: THandle; lpmi: pointer): bool; stdcall; external 'user32.dll';
  function SHGetNameFromIDList(_para1:LPCITEMIDLIST; sigdnName: longint; _para2:LPTSTR):WINBOOL; external 'shell32.dll' name 'SHGetNameFromIDList';


const
  PROGRAM_NAME = 'TDock';
  PROGRAM_REGKEY = 'tdock';
  PROGRAM_GUID = '{CF102D02-5C0B-4383-8902-2500AF8859B7}';
  WINITEM_CLASS = 'TDockItemWClass';
  ITEM_BACKGROUND = $2808080;
  RollStep = 4;
  NoAll = swp_nosize + swp_nomove + swp_nozorder + swp_noreposition;
  NOT_AN_ITEM = $ffff; // result const in case when item (items[]) not found

  // private Terry WM_APP messages //
  WM_APP_UPDATE_PREVIEW = WM_APP + 1;
  WM_APP_RUN_THREAD_END = WM_APP + 2;

  // popup menu command IDs //
  IDM_PASTE = $f030;
  IDM_LOCKICONS = $f031;
  IDM_COLLECTION = $f032;
  IDM_TASKMGR = $f033;
  IDM_SETS = $f034;
  IDM_QUIT = $f035;

  // WM_COPYDATA CDS.dwType
  DATA_PROGRAM = $f001; // TProgramData struct

  // system timer event ID's //
  ID_TIMER                  = 1;
  ID_TIMER_SLOW             = 2;
  ID_TIMER_FSA              = 3;
  ID_TIMER_OPEN             = 4;
  ID_TIMER_CLOSE            = 5;
  ID_TIMER_MOUSEHELD        = 6;
  ID_TIMER_UPDATE_SHORTCUT  = 7;
  ID_TIMER_ROLL             = 8;

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
  XErrorSetsBackupFailed = 'Failed to backup settings.';
  XErrorHelpNotFound = 'Help file not found! Try reinstalling the program.';
  XErrorIn = 'Error in';
  XStartButtonText = 'Start';

  // frmterryu //
  XConfigureIcon = 'Configure icon';
  XOpenFolderOf = 'Open folder of';
  XCopy = 'Copy';
  XPaste = 'Paste';
  XDeleteIcon = 'Delete icon';
  XDeleteSeparator = 'Delete separator';
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

  XMsgPlaceTargetFileInsteadOfShortcut = 'Place target file onto dock instead of shortcut?';
  XShortcut = 'Shortcut';
  XFile = 'File';
  XErrorInvalidProgramDataStructureSize = 'Invalid program data structure size';
  XAddProgramToDock = 'Add program to dock';

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

  // frmitemoptu //
  XShowCmdNormal = 'Normal size';
  XShowCmdMinimized = 'Minimized to taskbar';
  XShowCmdMaximized = 'Maximized to fullscreen';
  XSelectWorkingDirectory = 'Select working directory';
  XMsgUnsavedIconParams = 'Icon parameters changed. If you choose to proceed all changes will be lost. Proceed?';
  XOffsetOfIcons = 'Offset of icons: %d pt';
  XDistort = 'Distort: %d';

  // frmAddCommand //
  XErrorCommandListNotFound = 'Command list file not found. Try reinstalling the program.';

  // trayController //
  XMsgNotificationAreaIcons = 'After you close this message you will see "Notification area icons" window. To use this function uncheck "Always show all icons" at the bottom of the window and set all behaviors to "Only show notifications".';


  function SiteToString(site: TBaseSite): string;
  function StringToSite(str: string): TBaseSite;

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
//------------------------------------------------------------------------------
end.
