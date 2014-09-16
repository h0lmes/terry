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
    tcMenu,
    tcSaveSets,
    tcThemeChanged,
    tcSetVisible,
    tcGetVisible,
    tcGetDragging,
    tcZOrder,
    tcToggleVisible,
    tcToggleTaskbar,
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
  ID_TIMER_SLOW             = 2;
  ID_TIMER_FSA              = 3;
  ID_TIMER_OPEN             = 4;
  ID_TIMER_CLOSE            = 5;
  ID_TIMER_MOUSEHELD        = 6;
  ID_TIMER_UPDATE_SHORTCUT  = 7;


resourcestring

  // common //
  XErrorContactDeveloper = 'Contact developer if error is permanent.';
  XErrorCritical = 'Critical error occured. Try to run the program again.';
  XErrorLoadTheme = 'Theme load error. Try to run the program again.';
  XErrorThemeObjectNotFound = 'Theme object not found';
  XMsgThemeSaved = 'Theme saved';
  XErrorSetsCorrupted = 'Settings file corrupted.';
  XErrorSetsRestoreFailed = 'Restore operation failed.';
  XMsgSetsRestored = 'The settings are restored from backup. Run the program again';
  XErrorSetsBackupFailed = 'Failed to backup settings.';
  XErrorHelpNotFound = 'Help file not found! Try reinstalling the program.';
  XErrorIn = 'Error in';
  XStartButtonText = 'Start';
  XMsgFirstRun = 'Hello. This is the first time to run Terry.';
  XMsgAddMorePrograms = 'Would you like to add shortcuts for installed applications now?';

  // frmterryu //
  XConfigureIcon = 'Configure icon';
  XOpenFolderOf = 'Open folder of';
  XRunAsUser = 'Run as a different user';
  XCopy = 'Copy';
  XPaste = 'Paste';
  XDeleteIcon = 'Delete icon';
  XDeleteSeparator = 'Delete separator';
  XEmptyIcon = 'Empty icon';
  XSeparator = 'Separator (space)';
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

  // frmAddCommand //
  XErrorCommandListNotFound = 'Command list file not found. Try reinstalling the program.';

  // trayController //
  XMsgNotificationAreaIcons = 'After you close this message you will see "Notification area icons" window. To use this function uncheck "Always show all icons" at the bottom of the window and set all behaviors to "Only show notifications".';


var
  CF_SHELLIDLIST: uint;

implementation
initialization
  CF_SHELLIDLIST := RegisterClipboardFormat('Shell IDList Array');
end.
