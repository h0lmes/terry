unit declu;

interface
uses Windows, DefaultTranslator;

type
  TBaseSite = (bsLeft, bsTop, bsRight, bsBottom);
  TBaseOrientation = (boHorizontal, boVertical);

  TGParam = (
    // general parameters //
    gpMin,
    gpItemSize,
    gpBigItemSize,
    gpZoomWidth,
    gpItemSpacing,
    gpZoomItems,
    gpZoomSmoothingLevel,
    gpReserveScreenEdge,
    gpReserveScreenEdgePercent,
    gpTaskbar,
    gpSite,
    gpAutoHideTime,
    gpRolledVisiblePixels,
    gpCenterOffsetPercent,
    gpHideKeys,
    gpDropDistance,
    gpLaunchInterval,
    gpUseShell,
    gpAutoHide,
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
    gpZoomSpeed,
    gpMoveSpeed,
    gpActivateInvokedApps,
    gpMonitor,
    gpAutoShowTime,
    gpEdgeOffset,
    gpActivateRunning,
    gpShowRunningIndicator,
    gpItemAnimation,
    gpLockMouseEffect,
    gpReflection,
    gpReflectionSize,
    gpBlur,
    gpMax,
    // terry commands //
    tcMin,
    tcRepaintBase,
    tcRecalcBase,
    tcMenu,
    tcSaveSets,
    tcThemeChanged,
    tcSetVisible,
    tcGetVisible,
    tcGetDragging,
    tcZOrder,
    tcToggleVisible,
    tcApplyParams,
    tcQuit,
    tcMax,
    // item commands //
    icMin,
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
    Params: array [0..1023] of char;
    Dir: array [0..1023] of char;
    Icon: array [0..1023] of char;
    ShowCmd: integer;
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
  WINITEM_CLASS = 'Terry::WinItem';
  ITEM_BACKGROUND = $2808080;
  GUID = '{CF102D02-5C0B-4383-8902-2500AF8859B7}';
  RollStep = 4;
  NoAll = swp_nosize + swp_nomove + swp_nozorder + swp_noreposition;
  NOT_AN_ITEM = $ffff; // result const in case when item (items[]) not found

  ICON_DRIVE = 'Images\devices\drive-harddisk-8.png';
  ICON_FOLDER = 'Images\places\crystal-style\folder.png';
  ICON_COMPUTER = 'Images\places\crystal-style\my_computer.png';
  ICON_FOLDER_DOCUMENTS = 'Images\places\crystal-style\folder-documents.png';
  ICON_CONTROL_PANEL = 'Images\categories\preferences-desktop-5.png';
  ICON_TRASH_BIN = 'Images\places\crystal-style\user-trash.png';
  ICON_SETTINGS = 'Images\categories\applications-system-3.png';

  // private Terry WM_APP messages //
  WM_APP_UPDATE_PREVIEW = WM_APP + 1;

  // WM_COPYDATA CDS.dwType
  DATA_PROGRAM = $f001; // TProgramData struct

  // system timer event ID's //
  ID_TIMER                  = 1;
  ID_SLOWTIMER              = 2;
  ID_FSATIMER               = 3;
  ID_TIMER_OPEN             = 4;
  ID_TIMER_CLOSE            = 5;
  ID_TIMER_MOUSEHELD        = 6;

  // menu item type IDs //
  mitProperties     = $1;
  mitFolder         = $2;
  mitDelete         = $4;
  mitTab            = $8;


resourcestring

  // common //
  XErrorContactDeveloper = 'Contact developer if error is permanent.';
  XErrorCritical = 'Critical error occured. Try to run program again.';
  XErrorLoadTheme = 'Theme load error. Try to run program again.';
  XErrorThemeObjectNotFound = 'Theme object not found';
  XMsgThemeSaved = 'Theme saved';
  XErrorSetsCorrupted = 'Settings file corrupted.';
  XErrorSetsRestoreFailed = 'Settings restore failed.';
  XMsgSetsRestored = 'Settings restored from backup. Run program again';
  XErrorSetsBackupFailed = 'Failed to backup settings file.';
  XErrorHelpNotFound = 'Help file not found! Try reinstalling the program.';
  XErrorIn = 'Error in';
  XStartButtonText = 'Start';
  XMsgFirstRun = 'Hello. This is the first time to run Terry.';
  XMsgAddMorePrograms = 'Would you like to add shortcuts for installed applications now?';

  // frmterryu //
  XConfigureIcon = 'Configure icon';
  XOpenFolderOf = 'Open folder of';
  XRunAsUser = 'Run as a different user';
  XInputUserName = 'Input user name (format domain\user)';
  XCopy = 'Copy';
  XPaste = 'Paste';
  XDeleteIcon = 'Delete icon';
  XDeleteSeparator = 'Delete separator';
  XEmptyIcon = 'Empty icon';
  XProgram = 'Program';
  XCommand = 'Command';
  XSeparator = 'Separator (space)';
  XPlugins = 'Plugins';
  XStack = 'Stack';
  XStackControls = 'Stack (Control panel)';
  XStackDrives = 'Stack (Drives)';
  XStackDesktop = 'Stack (Desktop)';
  XStackDocuments = 'Stack (Documents)';
  XTray = 'Tray';
  XInstalledApplication = 'Installed application';
  XAddIcon = 'Add ...';
  XLockDragging = 'Lock dragging';
  XIconsEditor = 'Icons editor';
  XIconCollection = 'Open icon collection';
  XTaskManager = 'Task manager';
  XProgramSettings = 'Program settings';
  XExit = 'Exit';
  XCloseWindow = 'Close window';

  XMsgPlaceTargetFileInsteadOfShortcut = 'Place target file onto dock instead of shortcut?';
  XShortcut = 'Shortcut';
  XFile = 'File';
  XErrorInvalidProgramDataStructureSize = 'Invalid program data structure size';

  // frmsetsu //
  XLabelCenterOffset = 'Offset from center: %d %%';
  XLabelEdgeOffset = 'Offset from edge: %d px';
  XLabelIconSize = 'Icon size: %d px';
  XLabelZoomedIconSize = 'Zoomed icon size: %d px';
  XLabelIconSpacing = 'Icon spacing: %d px';
  XLabelZoomWidth = 'Zoom width: %d';
  XLabelZoomSmoothingLevel = 'Zoom smoothing level: %d';
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
  XShowCmdNormal = 'Normal size';
  XShowCmdMinimized = 'Minimized to taskbar';
  XShowCmdMaximized = 'Maximized to fullscreen';

  // frmitemoptu //
  XSelectWorkingDirectory = 'Select working directory';
  XProgramActivationDefault = 'Default (see Program settings)';
  XProgramActivationActivate = 'Activate program if already running';
  XProgramActivationRun = 'Run program';
  XMsgUnsavedIconParams = 'Icon parameters changed. If you will proceed all changes will be lost. Proceed?';

  // frmAddCommand //
  XErrorCommandListNotFound = 'Command list file not found. Try reinstalling the program.';

  XStretchStyleNone = 'None';
  XStretchStyleStretch = 'Stretch';
  XStretchStyleTile = 'Tile';

  // trayController //
  XMsgNotificationAreaIcons = 'After you close this message you will see "Notification area icons" window. To use this function uncheck "Always show all icons" at the bottom of the window and set all behaviors to "Only show notifications".';


var
  CF_SHELLIDLIST: uint;

implementation
initialization
  CF_SHELLIDLIST := RegisterClipboardFormat('Shell IDList Array');
end.
