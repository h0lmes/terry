unit setsu;

interface
uses Windows, Controls, Forms, Classes, SysUtils, Dialogs,
      Menus, StdCtrls, IniFiles, gdip_gfx, declu;

type
  _SetsContainer = record
    Monitor: integer;
    Site: TBaseSite;
    CenterOffsetPercent: integer;
    EdgeOffset: integer;

    AutoHide: boolean;
    AutoHideTime: integer;
    AutoShowTime: integer;
    AutoHidePixels: integer;

    ItemSize: integer;
    BigItemSize: integer;
    ZoomWidth: integer;
    ItemSpacing: integer;
    ZoomItems: boolean;

    HideKeys: integer;
    DropDistance: integer;
    LaunchInterval: integer;
    ActivateRunning: boolean;
    ShowRunningIndicator: boolean;
    ItemAnimation: integer;
    LaunchInThread: boolean;
    ActivateOnMouse: boolean;
    CloseCmdWindow: boolean;
    HideTaskBar: boolean;
    ReserveScreenEdge: boolean;
    ReserveScreenEdgePercent: integer;
    Taskbar: boolean;
    StayOnTop: boolean;
    ShowHint: boolean;
    HintEffects: boolean;
    LockDragging: boolean;
    UseShellContextMenus: boolean;

    AutoHideOnFullScreenApp: boolean;

    Reflection: boolean;
    BaseAlpha: integer;
    Blur: boolean;

    UseShell: boolean;

    Font: _FontData;
    StackFont: _FontData;

    Shell: array [0..MAX_PATH] of char;
    ThemeName: array [0..MAX_PATH] of char;
  end;

  _Sets = class
  private
    PluginsPath: string;
    PluginsList: TStrings;
    PluginFilesList: TStrings;
  public
    container: _SetsContainer;
    cancel_container: _SetsContainer;

    ParentHWnd: THandle;
    BaseCmd: TBaseCmd;
    width: integer;
    height: integer;
    visible: boolean;
    wndOffset: integer;
    wndOffsetTarget: integer;
    LastMouseEnterTime: integer;
    LastMouseLeaveTime: integer;
    MouseOnEdge: boolean;
    MouseOver: boolean;
    HideKeysPressed: boolean;
    WasOnEdge: boolean;
    progpath: string;
    SetsPathFile: string;
    ThemesPath: string;

    AutoRunList: TStrings;

    constructor Create(sets_file, _prog_path: string; Handle: THandle; ABaseCmd: TBaseCmd);
    destructor Destroy; override;
    procedure DoSetBaseVisible(param: boolean);
    function DoGetDragging: boolean;
    //
    procedure Load; overload;
    procedure Load(sets_file: string); overload;
    procedure Save; overload;
    procedure Save(sets_file: string); overload;
    procedure SaveEx;
    procedure SaveEx2;
    function Backup: boolean;
    function Restore: boolean;
    function StoreParam(id: TGParam; value: integer): integer;
    function GetParam(id: TGParam): integer;
    procedure StoreSetsContainer;
    procedure RestoreSetsContainer;
    procedure CopySetsContainer(var dst: _SetsContainer; var src: _SetsContainer);
    function SiteToString: string;
    function StringToSite(str: string): TBaseSite;
    function getBaseOrientation: TBaseOrientation;
    procedure RollDown;
    procedure RollUp;
    function IsHiddenDown: boolean;
    procedure Timer;
    function GetMonitorCount: integer;
    function GetMonitorName(index: integer): string;
    //
    function GetPluginCount: integer;
    function GetPluginName(index: integer): string;
    function GetPluginFileName(index: integer): string;
    procedure ScanPlugins;
    procedure GetPluginInfo(index: integer; mem: TMemo);
end;

var
  sets: _Sets;

implementation
uses frmterryu, toolu, frmsetsu, dockh, dwm_unit;
//------------------------------------------------------------------------------
constructor _Sets.Create(sets_file, _prog_path: string; Handle: THandle; ABaseCmd: TBaseCmd);
begin
  inherited Create;
  ParentHWnd := Handle;
  BaseCmd := ABaseCmd;
  StrCopy(container.ThemeName, 'Aero');
  visible := true;
  wndOffset := 0;
  MouseOnEdge := false;
  MouseOver := false;
  wndOffsetTarget := 0;
  container.AutoHidePixels := 0;
  container.CenterOffsetPercent := 50;
  container.EdgeOffset := 0;
  container.ActivateOnMouse := true;
  container.site := bsBottom;
  container.CloseCmdWindow := true;
  container.BaseAlpha := 255;
  SetsPathFile := sets_file;
  progpath := _prog_path;
  PluginsPath := progpath + '\Plugins';
  ThemesPath := IncludeTrailingPathDelimiter(progpath) + 'Themes\';
end;
//------------------------------------------------------------------------------
destructor _Sets.Destroy;
begin
  if assigned(PluginsList) then PluginsList.free;
  if assigned(PluginFilesList) then PluginFilesList.free;
  if assigned(AutoRunList) then AutoRunList.free;
end;
//------------------------------------------------------------------------------
procedure _Sets.DoSetBaseVisible(param: boolean);
begin
  if assigned(BaseCmd) then BaseCmd(tcSetVisible, integer(param));
end;
//------------------------------------------------------------------------------
function _Sets.DoGetDragging: boolean;
begin
  result:= false;
  if assigned(BaseCmd) then result:= boolean(BaseCmd(tcGetDragging, 0));
end;
//------------------------------------------------------------------------------
procedure _Sets.Load(sets_file: string);
begin
  SetsPathFile := sets_file;
  Load;
end;
//------------------------------------------------------------------------------
procedure _Sets.Load;
var
  i: integer;
  ini: TIniFile;
  tmplist: TStrings;
begin
  // restore from backup if sets file lost //
  if not FileExists(SetsPathFile) and FileExists(ChangeFileExt(SetsPathFile, '.bak')) then
  begin
    windows.CopyFile(pchar(ChangeFileExt(SetsPathFile, '.bak')), pchar(SetsPathFile), true);
    frmterry.notify('Settings file not found. Restored from backup.');
  end;

  // load sets //
  ini:= TIniFile.Create(SetsPathFile);
  // theme //
  StrCopy(container.ThemeName, pchar(ini.ReadString('theme', 'name', 'Aero')));
  // base //
  container.site := StringToSite(ini.ReadString('base', 'site', 'top'));
  StrCopy(container.Shell, pchar(ini.ReadString('base', 'Shell', 'explorer.exe')));
  container.autohidetime := SetRange(ini.ReadInteger('base', 'AutoHideTime', 800), 0, 9999);
  container.autoshowtime := SetRange(ini.ReadInteger('base', 'AutoShowTime', 400), 0, 9999);
  container.LaunchInterval := SetRange(ini.ReadInteger('base', 'LaunchInterval', 500), 0, 9999);
  container.ActivateRunning := ini.ReadBool('base', 'ActivateRunning', true);
  container.ShowRunningIndicator := ini.ReadBool('base', 'ShowRunningIndicator', true);
  container.ItemAnimation := SetRange(ini.ReadInteger('base', 'ItemAnimation', 4), 0, 8);
  container.ItemSize := SetRange(ini.ReadInteger('base', 'ItemSize', 48), 16, 128);
  container.BigItemSize := SetRange(ini.ReadInteger('base', 'BigItemSize', 96), container.ItemSize, 256);
  container.ItemSpacing := SetRange(ini.ReadInteger('base', 'ItemSpacing', 0), 0, 20);
  container.ZoomWidth := SetRange(ini.ReadInteger('base', 'ZoomWidth', 6), 4, 10);
  container.AutoHidePixels := ini.ReadInteger('base', 'AutoHidePixels', 15);
  container.CenterOffsetPercent := SetRange(ini.ReadInteger('base', 'CenterOffsetPercent', 50), 0, 100);
  container.EdgeOffset := SetRange(ini.ReadInteger('base', 'EdgeOffset', 0), -100, 100);
  container.DropDistance := container.ItemSize;
  container.HideKeys := ini.ReadInteger('base', 'HideKeys', 16490);
  container.Monitor := ini.ReadInteger('base', 'Monitor', 0);
  container.useShell := ini.ReadBool('base', 'UseShell', false);
  container.autohide := ini.ReadBool('base', 'AutoHide', false);
  container.LaunchInThread := ini.ReadBool('base', 'LaunchInThread', true);
  container.ZoomItems := ini.ReadBool('base', 'ZoomItems', true);
  container.ActivateOnMouse := ini.ReadBool('base', 'ActivateOnMouse', true);
  container.CloseCmdWindow := ini.ReadBool('base', 'CloseCmdWindow', true);
  container.HideTaskBar := ini.ReadBool('base', 'HideTaskBar', false);
  container.ReserveScreenEdge := ini.ReadBool('base', 'ReserveScreenEdge', false);
  container.ReserveScreenEdgePercent := SetRange(ini.ReadInteger('base', 'ReserveScreenEdgePercent', 100), 0, 200);
  container.Taskbar := ini.ReadBool('base', 'Taskbar', false);
  container.StayOnTop := ini.ReadBool('base', 'StayOnTop', false);
  container.LockDragging := ini.ReadBool('base', 'LockDragging', true);
  container.ShowHint := ini.ReadBool('base', 'ShowHint', true);
  container.HintEffects := ini.ReadBool('base', 'HintEffects', false);
  container.AutoHideOnFullScreenApp := ini.ReadBool('base', 'AutoHideOnFullScreenApp', true);
  container.UseShellContextMenus := ini.ReadBool('base', 'UseShellContextMenus', true);
  // font //
  StrCopy(container.Font.name, pchar(ini.ReadString('Font', 'name', toolu.GetFont)));
  container.Font.size:= SetRange(ini.ReadInteger('Font', 'size', 15), 6, 72);
  container.Font.color:= uint(ini.ReadInteger('Font', 'color', integer($ffffffff)));
  container.Font.color_outline:= uint(ini.ReadInteger('Font', 'color_o', integer($ff101010)));
  container.Font.bold:= ini.ReadBool('Font', 'bold', true);
  container.Font.italic:= ini.ReadBool('Font', 'italic', false);
  container.Font.outline:= ini.ReadBool('Font', 'outline', false);
  // stack font //
  StrCopy(container.StackFont.name, pchar(ini.ReadString('StackFont', 'name', toolu.GetFont)));
  container.StackFont.size:= SetRange(ini.ReadInteger('StackFont', 'size', 12), 6, 72);
  container.StackFont.color:= uint(ini.ReadInteger('StackFont', 'color', integer($ffffffff)));
  container.StackFont.color_outline:= uint(ini.ReadInteger('StackFont', 'color_o', integer($ff101010)));
  container.StackFont.bold:= ini.ReadBool('StackFont', 'bold', true);
  container.StackFont.italic:= ini.ReadBool('StackFont', 'italic', false);
  container.StackFont.outline:= ini.ReadBool('StackFont', 'outline', false);
  // gfx //
  container.BaseAlpha := SetRange(ini.ReadInteger('gfx', 'BaseAlpha', 255), 0, 255);
  container.Reflection := ini.ReadBool('gfx', 'Reflection', true);
  container.Blur := ini.ReadBool('gfx', 'Blur', true);

  // autoruns //
  tmpList := TStringList.Create;
  ini.ReadSection('autorun', tmpList);
  if not assigned(AutoRunList) then AutoRunList := TStringList.Create;
  AutoRunList.clear;
  i := 0;
  while i < tmpList.Count do
  begin
    AutoRunList.Add(ini.ReadString('autorun', tmplist.strings[i], ''));
    inc(i);
  end;
  tmpList.free;

  // finalization //
  ini.free;
end;
//------------------------------------------------------------------------------
procedure _Sets.Save(sets_file: string);
begin
  SetsPathFile := sets_file;
  Save;
end;
//------------------------------------------------------------------------------
procedure _Sets.Save;
var
  i: integer;
  ini: TIniFile;
begin
  windows.DeleteFile(PChar(SetsPathFile));

  ini:= TIniFile.Create(SetsPathFile);
  // theme //
  ini.WriteString('theme', 'Name', pchar(@container.ThemeName[0]));
  // base //
  ini.WriteString('base', 'Site', SiteToString);
  ini.WriteString('base', 'Shell', pchar(@container.Shell[0]));
  ini.WriteBool('base', 'AutoHide', container.autohide);
  ini.WriteInteger('base', 'AutoHideTime', container.autohidetime);
  ini.WriteInteger('base', 'AutoShowTime', container.autoshowtime);
  ini.WriteInteger('base', 'AutoHidePixels', container.AutoHidePixels);
  ini.WriteInteger('base', 'LaunchInterval', container.LaunchInterval);
  ini.WriteInteger('base', 'ActivateRunning', integer(container.ActivateRunning));
  ini.WriteInteger('base', 'ShowRunningIndicator', integer(container.ShowRunningIndicator));
  ini.WriteInteger('base', 'ItemAnimation', container.ItemAnimation);
  ini.WriteInteger('base', 'ItemSize', container.itemsize);
  ini.WriteInteger('base', 'BigItemSize', container.BigItemSize);
  ini.WriteInteger('base', 'ItemSpacing', container.ItemSpacing);
  ini.WriteInteger('base', 'ZoomWidth', container.ZoomWidth);
  ini.WriteInteger('base', 'CenterOffsetPercent', container.CenterOffsetPercent);
  ini.WriteInteger('base', 'EdgeOffset', container.EdgeOffset);
  ini.WriteInteger('base', 'HideKeys', container.HideKeys);
  ini.WriteInteger('base', 'Monitor', container.Monitor);
  ini.WriteBool('base', 'AutoHideOnFullScreenApp', container.AutoHideOnFullScreenApp);
  ini.WriteBool('base', 'UseShell', container.useShell);
  ini.WriteBool('base', 'ZoomItems', container.ZoomItems);
  ini.WriteBool('base', 'LaunchInThread', container.launchInThread);
  ini.WriteBool('base', 'ActivateOnMouse', container.ActivateOnMouse);
  ini.WriteBool('base', 'CloseCmdWindow', container.CloseCmdWindow);
  ini.WriteBool('base', 'HideTaskBar', container.HideTaskBar);
  ini.WriteBool('base', 'ReserveScreenEdge', container.ReserveScreenEdge);
  ini.WriteInteger('base', 'ReserveScreenEdgePercent', container.ReserveScreenEdgePercent);
  ini.WriteBool('base', 'Taskbar', container.Taskbar);
  ini.WriteBool('base', 'StayOnTop', container.StayOnTop);
  ini.WriteBool('base', 'LockDragging', container.LockDragging);
  ini.WriteBool('base', 'ShowHint', container.ShowHint);
  ini.WriteBool('base', 'HintEffects', container.HintEffects);
  ini.WriteBool('base', 'UseShellContextMenus', container.UseShellContextMenus);
  ini.WriteBool('base', 'Blur', container.Blur);
  // gfx //
  ini.WriteInteger('gfx', 'BaseAlpha', container.BaseAlpha);
  ini.WriteBool('gfx', 'Reflection', container.Reflection);
  // font //
  ini.WriteString('Font', 'name', pchar(@container.Font.name[0]));
  ini.WriteInteger('Font', 'size', container.Font.size);
  ini.WriteInteger('Font', 'color', container.Font.color);
  ini.WriteInteger('Font', 'color_o', container.Font.color_outline);
  ini.WriteBool('Font', 'bold', container.Font.bold);
  ini.WriteBool('Font', 'italic', container.Font.italic);
  ini.WriteBool('Font', 'outline', container.Font.outline);
  // stack font //
  ini.WriteString('StackFont', 'name', pchar(@container.StackFont.name[0]));
  ini.WriteInteger('StackFont', 'size', container.StackFont.size);
  ini.WriteInteger('StackFont', 'color', container.StackFont.color);
  ini.WriteInteger('StackFont', 'color_o', container.StackFont.color_outline);
  ini.WriteBool('StackFont', 'bold', container.StackFont.bold);
  ini.WriteBool('StackFont', 'italic', container.StackFont.italic);
  ini.WriteBool('StackFont', 'outline', container.StackFont.outline);

  // autoruns //
  i:= 0;
  if assigned(AutoRunList) then
    if AutoRunList.Count > 0 then
      while i < AutoRunList.count do
      begin
        if AutoRunList.strings[i] <> '' then
          ini.WriteString('autorun', 'command' + inttostr(i), '"' + AutoRunList.strings[i] + '"');
        inc(i);
      end;

  ini.UpdateFile;
  ini.free;
end;
//------------------------------------------------------------------------------
procedure _Sets.SaveEx;
begin
  try
    SetsPathFile := ChangeFileExt(SetsPathFile, '.tmpini');
    Save;
  except
    on e: Exception do raise Exception.Create('Terry.Sets.SaveEx'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _Sets.SaveEx2;
const
  MOVEFILE_WRITE_THROUGH = 8;
var
  tmpini: string;
begin
  try
    tmpini := SetsPathFile;
    SetsPathFile := ChangeFileExt(SetsPathFile, '.ini');
    windows.MoveFileEx(pchar(tmpini), pchar(SetsPathFile), MOVEFILE_REPLACE_EXISTING + MOVEFILE_WRITE_THROUGH);
  except
    on e: Exception do raise Exception.Create('Terry.Sets.SaveEx2'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _Sets.Backup: boolean;
begin
  result := true;
  try
    if FileExists(ChangeFileExt(SetsPathFile, '.bak')) then
      result := windows.DeleteFile(PChar(ChangeFileExt(SetsPathFile, '.bak')));
    if result then
      result := windows.CopyFile(pchar(SetsPathFile), pchar(ChangeFileExt(SetsPathFile, '.bak')), false);
  except
    on e: Exception do raise Exception.Create('Terry.Sets.Backup'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _Sets.Restore: boolean;
var
  bakfile: string;
begin
  result := false;
  try
    if FileExists(SetsPathFile) then
      if not windows.DeleteFile(PChar(SetsPathFile)) then raise Exception.Create('Sets.Restore.DeleteSetsFile failed');

    bakfile := ChangeFileExt(SetsPathFile, '.tmpini');
    if FileExists(bakfile) then
       if windows.MoveFile(pchar(bakfile), pchar(SetsPathFile)) then result := true;

    if not result then
    begin
      bakfile := ChangeFileExt(SetsPathFile, '.bak');
      if windows.CopyFile(pchar(bakfile), pchar(SetsPathFile), false) then result := true;
    end;
  except
    on e: Exception do raise Exception.Create('Terry.Sets.Restore'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _Sets.StoreParam(id: TGParam; value: integer): integer;
begin
  case id of
  gpItemSize: container.ItemSize := SetRange(value, 16, 128);
  gpBigItemSize: container.BigItemSize := SetRange(value, container.ItemSize, 256);
  gpItemSpacing: container.ItemSpacing := SetRange(value, 0, 20);
  gpZoomWidth: container.ZoomWidth := SetRange((value div 2) * 2, 4, 10);
  gpZoomItems: container.ZoomItems := boolean(value);
  gpMonitor: container.Monitor := value;
  gpSite: container.Site := TBaseSite(SetRange(value, 0, 3));
  gpCenterOffsetPercent: container.CenterOffsetPercent := SetRange(value, 0, 100);
  gpEdgeOffset: container.EdgeOffset := SetRange(value, -100, 100);
  gpAutoHideTime: container.AutoHideTime := value;
  gpAutoShowTime: container.AutoShowTime := value;
  gpAutoHidePixels: container.AutoHidePixels := SetRange(value, 0, 9999);
  gpHideKeys: container.HideKeys:= value;
  gpDropDistance: container.DropDistance := SetRange(value, 50, 500);
  gpLaunchInterval: container.LaunchInterval := SetRange(value, 0, 9999);
  gpActivateRunning: container.ActivateRunning := boolean(value);
  gpShowRunningIndicator: container.ShowRunningIndicator := boolean(value);
  gpItemAnimation: container.ItemAnimation := value;
  gpUseShell: container.UseShell := boolean(value);
  gpAutoHide: container.AutoHide := boolean(value);
  gpLaunchInThread: container.LaunchInThread := boolean(value);
  gpActivateOnMouse: container.ActivateOnMouse := boolean(value);
  gpCloseCmdWindow: container.CloseCmdWindow := boolean(value);
  gpHideTaskbar: container.HideTaskbar := boolean(value);
  gpReserveScreenEdge: container.ReserveScreenEdge := boolean(value);
  gpReserveScreenEdgePercent: container.ReserveScreenEdgePercent := SetRange(value, 0, 200);
  gpTaskBar: container.Taskbar := boolean(value);
  gpStayOnTop: container.StayOnTop := boolean(value);
  gpShowHint: container.ShowHint := boolean(value);
  gpHintEffects: container.HintEffects := boolean(value);
  gpLockDragging: container.LockDragging := boolean(value);
  gpReflection: container.Reflection := boolean(value);
  gpAutoHideOnFullScreenApp: container.AutoHideOnFullScreenApp := boolean(value);
  gpUseShellContextMenus: container.UseShellContextMenus := boolean(value);
  gpBaseAlpha: container.BaseAlpha := SetRange(value, 13, 255);
  gpBlur: container.Blur := boolean(value);
  end;

  result := value;
end;
//------------------------------------------------------------------------------
function _Sets.GetParam(id: TGParam): integer;
begin
  result:= 0;
  case id of
  gpItemSize: result := container.ItemSize;
  gpBigItemSize: result := container.BigItemSize;
  gpZoomWidth: result := container.ZoomWidth;
  gpItemSpacing: result := container.ItemSpacing;
  gpZoomItems: result := integer(container.ZoomItems);
  gpMonitor: result := container.Monitor;
  gpSite: result := integer(container.Site);
  gpCenterOffsetPercent: result := container.CenterOffsetPercent;
  gpEdgeOffset: result := container.EdgeOffset;
  gpAutoHideTime: result := container.AutoHideTime;
  gpAutoShowTime: result := container.AutoShowTime;
  gpAutoHidePixels: result := container.AutoHidePixels;
  gpHideKeys: result := container.HideKeys;
  gpDropDistance: result := container.DropDistance;
  gpLaunchInterval: result := container.LaunchInterval;
  gpActivateRunning: result := integer(container.ActivateRunning);
  gpShowRunningIndicator: result := integer(container.ShowRunningIndicator);
  gpItemAnimation: result := container.ItemAnimation;
  gpUseShell: result := integer(container.UseShell);
  gpAutoHide: result := integer(container.AutoHide);
  gpLaunchInThread: result := integer(container.LaunchInThread);
  gpActivateOnMouse: result := integer(container.ActivateOnMouse);
  gpCloseCmdWindow: result := integer(container.CloseCmdWindow);
  gpHideTaskbar: result := integer(container.HideTaskbar);
  gpReserveScreenEdge: result := integer(container.ReserveScreenEdge);
  gpReserveScreenEdgePercent: result := container.ReserveScreenEdgePercent;
  gpTaskbar: result := integer(container.Taskbar);
  gpStayOnTop: result := integer(container.StayOnTop);
  gpShowHint: result := integer(container.ShowHint);
  gpHintEffects: result := integer(container.HintEffects);
  gpLockDragging: result := integer(container.LockDragging);
  gpReflection: result := integer(container.Reflection);
  gpAutoHideOnFullScreenApp: result := integer(container.AutoHideOnFullScreenApp);
  gpUseShellContextMenus: result := integer(container.UseShellContextMenus);
  gpBaseAlpha: result := container.BaseAlpha;
  gpBlur: result := integer(container.Blur);
  end;
end;
//------------------------------------------------------------------------------
procedure _Sets.StoreSetsContainer;
begin
  CopySetsContainer(cancel_container, container);
end;
//------------------------------------------------------------------------------
procedure _Sets.RestoreSetsContainer;
begin
  CopySetsContainer(container, cancel_container);
end;
//------------------------------------------------------------------------------
procedure _Sets.CopySetsContainer(var dst: _SetsContainer; var src: _SetsContainer);
begin
  dst.Monitor := src.Monitor;
  dst.site := src.site;
  dst.CenterOffsetPercent := src.CenterOffsetPercent;
  dst.EdgeOffset := src.EdgeOffset;
  dst.autohide := src.autohide;
  dst.autohidetime := src.autohidetime;
  dst.autoshowtime := src.autoshowtime;
  dst.AutoHidePixels := src.AutoHidePixels;
  dst.ItemSize := src.ItemSize;
  dst.BigItemSize := src.BigItemSize;
  dst.ZoomWidth := src.ZoomWidth;
  dst.ItemSpacing := src.ItemSpacing;
  dst.ZoomItems := src.ZoomItems;
  dst.HideKeys := src.HideKeys;
  dst.DropDistance := src.DropDistance;
  dst.LaunchInterval := src.LaunchInterval;
  dst.ActivateRunning := src.ActivateRunning;
  dst.ShowRunningIndicator := src.ShowRunningIndicator;
  dst.ItemAnimation := src.ItemAnimation;
  dst.launchInThread := src.launchInThread;
  dst.ActivateOnMouse := src.ActivateOnMouse;
  dst.CloseCmdWindow := src.CloseCmdWindow;
  dst.HideTaskbar := src.HideTaskbar;
  dst.ReserveScreenEdge := src.ReserveScreenEdge;
  dst.ReserveScreenEdgePercent := src.ReserveScreenEdgePercent;
  dst.Taskbar := src.Taskbar;
  dst.StayOnTop := src.StayOnTop;
  dst.ShowHint := src.ShowHint;
  dst.HintEffects := src.HintEffects;
  dst.LockDragging := src.LockDragging;
  dst.UseShellContextMenus := src.UseShellContextMenus;
  dst.AutoHideOnFullScreenApp := src.AutoHideOnFullScreenApp;
  dst.Reflection := src.Reflection;
  dst.BaseAlpha := src.BaseAlpha;
  dst.Blur := src.Blur;
  dst.useShell := src.useShell;

  CopyFontData(src.Font, dst.Font);
  CopyFontData(src.StackFont, dst.StackFont);
  StrCopy(dst.Shell, pchar(@src.Shell));
  StrCopy(dst.ThemeName, pchar(@src.ThemeName));
end;
//------------------------------------------------------------------------------
function _Sets.SiteToString: string;
begin
  result := 'left';
  if container.site = bsTop then result := 'top'
  else if container.site = bsRight then result := 'right'
  else if container.site = bsBottom then result := 'bottom';
end;
//------------------------------------------------------------------------------
function _Sets.StringToSite(str: string): TBaseSite;
begin
  result := bsLeft;
  if str = 'top' then result := bsTop
  else if str = 'right' then result := bsRight
  else if str = 'bottom' then result := bsBottom;
end;
//------------------------------------------------------------------------------
function _Sets.getBaseOrientation: TBaseOrientation;
begin
  result := boHorizontal;
  if (container.site = bsLeft) or (container.site = bsRight) then result := boVertical;
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
function _Sets.IsHiddenDown: boolean;
begin
  result := wndOffsetTarget > 0;
end;
//------------------------------------------------------------------------------
procedure _Sets.RollDown;
begin
  if DoGetDragging then exit;
  if container.AutoHide and not IsHiddenDown then
  begin
    if getBaseOrientation = boVertical then wndOffsetTarget := width - container.AutoHidePixels
    else wndOffsetTarget := height - container.AutoHidePixels;
    if wndOffsetTarget < 0 then wndOffsetTarget := 0;
  end;
end;
//------------------------------------------------------------------------------
procedure _Sets.RollUp;
begin
  if IsWindowVisible(ParentHWnd) and IsHiddenDown then
  begin
    wndOffsetTarget := 0;
    frmterry.ItemMgr.ItemsChanged;
  end;
end;
//------------------------------------------------------------------------------
procedure _Sets.Timer;
var
  KeyPressed: boolean;
  key, KeySet: integer;
  sets_visible: boolean;
begin
  sets_visible := false;
  try if frmsets <> nil then sets_visible := frmsets.visible;
  except end;

  if not MouseOver and not sets_visible then
    if GetTickCount - LastMouseLeaveTime > container.AutoHideTime then RollDown;

  if MouseOver then
    if GetTickCount - LastMouseEnterTime > container.AutoShowTime then RollUp;

  if container.AutoHide then
    if wndoffset <> wndOffsetTarget then
    begin
      if abs(wndOffsetTarget - WndOffset) > RollStep then
      begin
        if wndOffsetTarget > WndOffset then inc(wndOffset, RollStep) else dec(wndOffset, RollStep);
      end
      else wndOffset := wndOffsetTarget;
      frmterry.ItemMgr.ItemsChanged;
    end;

  KeyPressed:= false;
  KeySet:= scNone;
  Key:= sets.container.HideKeys and not (scShift + scCtrl + scAlt);
  if key > 0 then
    if getasynckeystate(key) < 0 then KeyPressed:= true;
  if getasynckeystate(16) < 0 then inc(KeySet, scShift);
  if getasynckeystate(17) < 0 then inc(KeySet, scCtrl);
  if getasynckeystate(18) < 0 then inc(KeySet, scAlt);
  if sets.container.HideKeys and (scShift + scCtrl + scAlt) = KeySet then
    KeyPressed:= KeyPressed and true
  else
    KeyPressed:= false;

  if KeyPressed then
  begin
    if not HideKeysPressed then
    begin
      DoSetBaseVisible(not visible);
      HideKeysPressed:= true;
    end;
  end else HideKeysPressed:= false;
end;
//------------------------------------------------------------------------------
function _Sets.GetMonitorCount: integer;
begin
  result := screen.MonitorCount;
end;
//------------------------------------------------------------------------------
function _Sets.GetMonitorName(index: integer): string;
begin
  if index < 0 then result := XAll
  else result := XMonitor + ' ' + inttostr(screen.Monitors[index].MonitorNum + 1);
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
function _Sets.GetPluginCount: integer;
begin
  result := -1;
  if assigned(PluginsList) then result := PluginsList.Count;
end;
//------------------------------------------------------------------------------
function _Sets.GetPluginName(index: integer): string;
begin
  result := '';
  if assigned(PluginsList) then result := PluginsList.Strings[index];
end;
//------------------------------------------------------------------------------
function _Sets.GetPluginFileName(index: integer): string;
begin
  result := '';
  if assigned(PluginFilesList) then result := PluginFilesList.Strings[index];
end;
//------------------------------------------------------------------------------
procedure _Sets.ScanPlugins;
var
  i: integer;
  hLib: uint;
  OnGetInformation: _OnGetInformation;
  szName, szAuthor, szNotes: array [0..255] of char;
  lpiVersion: integer;
  cdir: string;
begin
  try
    cdir := GetCurrentDir;
    if not assigned(PluginsList) then PluginsList:= TStringList.Create;
    if not assigned(PluginFilesList) then PluginFilesList:= TStringList.Create;

    PluginsList.clear;
    PluginFilesList.clear;
    AddLog('Sets.ScanPlugins.SearchFilesRecurse');
    toolu.SearchFilesRecurse(PluginsPath, '*.dll', PluginFilesList);

    AddLog('Sets.ScanPlugins.CycleGetInfo');
    i := 0;
    while i < PluginFilesList.Count do
    begin
      SetCurrentDir(ExtractFilePath(PluginFilesList.strings[i]));
      hLib := LoadLibrary(pansichar(PluginFilesList.strings[i]));
      if hLib < 33 then PluginFilesList.Delete(i)
      else begin
        @OnGetInformation := GetProcAddress(hLib, 'OnGetInformation');
        if not assigned(OnGetInformation) then PluginFilesList.Delete(i)
        else
        begin
          try OnGetInformation(@szName, @szAuthor, @lpiVersion, @szNotes);
          except end;
          PluginsList.add(strpas(@szName));
          inc(i);
        end;
        FreeLibrary(hLib);
      end;
    end;
  finally
    if cdir <> '' then SetCurrentDir(cdir);
  end;
end;
//------------------------------------------------------------------------------
procedure _Sets.GetPluginInfo(index: integer; mem: TMemo);
var
  hLib: uint;
  OnGetInformation: _OnGetInformation;
  szName, szAuthor, szNotes: array [0..255] of char;
  lpiVersion: integer;
  ver: string;
  cdir: string;
begin
  try
    mem.clear;

    cdir := GetCurrentDir;
    SetCurrentDir(ExtractFilePath(PluginFilesList.strings[Index]));
    hLib := LoadLibrary(pansichar(PluginFilesList.strings[Index]));
    if hLib < 33 then
    begin
      mem.text:= 'Error loading plugin';
      exit;
    end;

    @OnGetInformation:= GetProcAddress(hLib, 'OnGetInformation');
    if assigned(OnGetInformation) then
    begin
      try OnGetInformation(@szName[0], @szAuthor[0], @lpiVersion, @szNotes[0]);
      except
        begin
          FreeLibrary(hLib);
          messagebox(application.mainform.handle, 'Error trying to get information', 'Terry', mb_iconerror);
          exit;
        end;
      end;
      mem.lines.add(strpas(@szNotes[0]));
      mem.lines.add('');
      mem.lines.add('Author: ' + strpas(@szAuthor[0]));
      mem.lines.add('');
      ver := 'v' + inttostr(lpiVersion div 100) + '.';
      if lpiVersion mod 100 < 10 then ver:= ver + '0';
      ver := ver + inttostr(lpiVersion mod 100);
      mem.lines.add(ver);
    end else
      mem.text := 'Unknown plugin format';
    FreeLibrary(hLib);
  finally
    if cdir <> '' then SetCurrentDir(cdir);
  end;
end;
//------------------------------------------------------------------------------
end.
