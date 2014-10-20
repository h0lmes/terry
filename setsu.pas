unit setsu;

interface
uses Windows, Controls, Forms, Classes, SysUtils, Dialogs,
      Menus, StdCtrls, IniFiles, gdip_gfx, declu, toolu, dockh;

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
    ZoomTime: integer;

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
    StackOpenAnimation: boolean;
    AutoHideOnFullScreenApp: boolean;

    Reflection: boolean;
    BaseAlpha: integer;
    Blur: boolean;

    UseShell: boolean;
    Hello: boolean;

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
    FRestored: boolean;
  public
    container: _SetsContainer;
    cancel_container: _SetsContainer;
    ParentHWnd: THandle;
    SetsPathFile: string;
    ThemesPath: string;
    AutoRunList: TStrings;

    property Restored: boolean read FRestored;

    constructor Create(ASetsFile, AProgPath: string; Handle: THandle);
    destructor Destroy; override;
    procedure Load; overload;
    procedure Load(ASetsFile: string); overload;
    procedure Save; overload;
    procedure Save(ASetsFile: string); overload;
    procedure SaveEx;
    procedure SaveEx2;
    function Backup: boolean;
    function Restore: boolean;
    function StoreParam(id: TGParam; value: integer): integer;
    function GetParam(id: TGParam): integer;
    procedure StoreSetsContainer;
    procedure RestoreSetsContainer;
    procedure CopySetsContainer(var dst: _SetsContainer; var src: _SetsContainer);
    function getBaseOrientation: TBaseOrientation;
    function GetMonitorCount: integer;
    function GetMonitorName(index: integer): string;
    function GetPluginCount: integer;
    function GetPluginName(index: integer): string;
    function GetPluginFileName(index: integer): string;
    procedure ScanPlugins;
    procedure GetPluginInfo(index: integer; mem: TMemo);
end;

var sets: _Sets;

implementation
//------------------------------------------------------------------------------
constructor _Sets.Create(ASetsFile, AProgPath: string; Handle: THandle);
begin
  inherited Create;
  ParentHWnd := Handle;
  StrCopy(container.ThemeName, 'Aero');
  container.AutoHidePixels := 0;
  container.CenterOffsetPercent := 50;
  container.EdgeOffset := 0;
  container.ActivateOnMouse := true;
  container.site := bsBottom;
  container.CloseCmdWindow := true;
  container.BaseAlpha := 255;
  container.Hello := true;
  SetsPathFile := ASetsFile;
  PluginsPath := AProgPath + '\Docklets';
  ThemesPath := IncludeTrailingPathDelimiter(AProgPath) + 'Themes\';
end;
//------------------------------------------------------------------------------
destructor _Sets.Destroy;
begin
  if assigned(PluginsList) then PluginsList.free;
  if assigned(PluginFilesList) then PluginFilesList.free;
  if assigned(AutoRunList) then AutoRunList.free;
end;
//------------------------------------------------------------------------------
procedure _Sets.Load(ASetsFile: string);
begin
  SetsPathFile := ASetsFile;
  Load;
end;
//------------------------------------------------------------------------------
procedure _Sets.Load;
var
  idx: integer;
  ini: TIniFile;
  tmplist: TStrings;
begin
  FRestored := false;
  // restore from backup if sets file lost //
  if not FileExists(SetsPathFile) and FileExists(ChangeFileExt(SetsPathFile, '.bak')) then
  begin
    windows.CopyFile(pchar(ChangeFileExt(SetsPathFile, '.bak')), pchar(SetsPathFile), true);
    FRestored := true;
  end;

  // load sets //
  ini:= TIniFile.Create(SetsPathFile);
  // base //
  StrCopy(container.ThemeName, pchar(ini.ReadString('base', 'Theme', 'Aero')));
  container.Monitor := ini.ReadInteger('base', 'Monitor', 0);
  container.Site := StringToSite(ini.ReadString('base', 'Site', 'top'));
  container.CenterOffsetPercent := SetRange(ini.ReadInteger('base', 'CenterOffsetPercent', 50), 0, 100);
  container.EdgeOffset := SetRange(ini.ReadInteger('base', 'EdgeOffset', 0), -100, 100);
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
  container.ZoomTime := SetRange(ini.ReadInteger('base', 'ZoomTime', 120), 0, 600);
  container.AutoHidePixels := ini.ReadInteger('base', 'AutoHidePixels', 15);
  container.DropDistance := container.ItemSize;
  container.HideKeys := ini.ReadInteger('base', 'HideKeys', 16490);
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
  container.StackOpenAnimation := ini.ReadBool('base', 'StackOpenAnimation', true);
  container.Hello := ini.ReadBool('base', 'Hello', true);
  container.useShell := ini.ReadBool('base', 'UseShell', false);
  StrCopy(container.Shell, pchar(ini.ReadString('base', 'Shell', '')));
  // font //
  StrCopy(container.Font.name, pchar(ini.ReadString('Font', 'name', toolu.GetFont)));
  container.Font.size:= SetRange(ini.ReadInteger('Font', 'size', 15), 6, 72);
  container.Font.color:= uint(ini.ReadInteger('Font', 'color', integer($ffffffff)));
  container.Font.color_outline:= uint(ini.ReadInteger('Font', 'backcolor', integer($ff202020)));
  container.Font.bold:= ini.ReadBool('Font', 'bold', true);
  container.Font.italic:= ini.ReadBool('Font', 'italic', false);
  // stack font //
  StrCopy(container.StackFont.name, pchar(ini.ReadString('StackFont', 'name', toolu.GetFont)));
  container.StackFont.size:= SetRange(ini.ReadInteger('StackFont', 'size', 14), 6, 72);
  container.StackFont.color:= uint(ini.ReadInteger('StackFont', 'color', integer($ffffffff)));
  container.StackFont.color_outline:= uint(ini.ReadInteger('StackFont', 'backcolor', integer($ff202020)));
  container.StackFont.bold:= ini.ReadBool('StackFont', 'bold', true);
  container.StackFont.italic:= ini.ReadBool('StackFont', 'italic', false);
  // gfx //
  container.BaseAlpha := SetRange(ini.ReadInteger('gfx', 'BaseAlpha', 255), 0, 255);
  container.Reflection := ini.ReadBool('gfx', 'Reflection', true);
  container.Blur := ini.ReadBool('gfx', 'Blur', true);

  // autoruns //
  tmpList := TStringList.Create;
  ini.ReadSection('autorun', tmpList);
  if not assigned(AutoRunList) then AutoRunList := TStringList.Create;
  AutoRunList.clear;
  idx := 0;
  while idx < tmpList.Count do
  begin
    AutoRunList.Add(ini.ReadString('autorun', tmplist.strings[idx], ''));
    inc(idx);
  end;
  tmpList.free;

  // finalization //
  ini.free;
end;
//------------------------------------------------------------------------------
procedure _Sets.Save(ASetsFile: string);
begin
  SetsPathFile := ASetsFile;
  Save;
end;
//------------------------------------------------------------------------------
procedure _Sets.Save;
var
  idx: integer;
  ini: TIniFile;
begin
  windows.DeleteFile(PChar(SetsPathFile));

  ini := TIniFile.Create(SetsPathFile);
  ini.CacheUpdates := true;
  // base //
  ini.WriteString ('base', 'Theme', pchar(@container.ThemeName[0]));
  ini.WriteInteger('base', 'Monitor', container.Monitor);
  ini.WriteString ('base', 'Site', SiteToString(container.Site));
  ini.WriteInteger('base', 'CenterOffsetPercent', container.CenterOffsetPercent);
  ini.WriteInteger('base', 'EdgeOffset', container.EdgeOffset);
  ini.WriteString ('base', 'Shell', pchar(@container.Shell[0]));
  ini.WriteBool   ('base', 'AutoHide', container.autohide);
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
  ini.WriteInteger('base', 'ZoomTime', container.ZoomTime);
  ini.WriteInteger('base', 'HideKeys', container.HideKeys);
  ini.WriteBool   ('base', 'AutoHideOnFullScreenApp', container.AutoHideOnFullScreenApp);
  ini.WriteBool   ('base', 'UseShell', container.useShell);
  ini.WriteBool   ('base', 'ZoomItems', container.ZoomItems);
  ini.WriteBool   ('base', 'LaunchInThread', container.launchInThread);
  ini.WriteBool   ('base', 'ActivateOnMouse', container.ActivateOnMouse);
  ini.WriteBool   ('base', 'CloseCmdWindow', container.CloseCmdWindow);
  ini.WriteBool   ('base', 'HideTaskBar', container.HideTaskBar);
  ini.WriteBool   ('base', 'ReserveScreenEdge', container.ReserveScreenEdge);
  ini.WriteInteger('base', 'ReserveScreenEdgePercent', container.ReserveScreenEdgePercent);
  ini.WriteBool   ('base', 'Taskbar', container.Taskbar);
  ini.WriteBool   ('base', 'StayOnTop', container.StayOnTop);
  ini.WriteBool   ('base', 'LockDragging', container.LockDragging);
  ini.WriteBool   ('base', 'ShowHint', container.ShowHint);
  ini.WriteBool   ('base', 'HintEffects', container.HintEffects);
  ini.WriteBool   ('base', 'UseShellContextMenus', container.UseShellContextMenus);
  ini.WriteBool   ('base', 'StackOpenAnimation', container.StackOpenAnimation);
  ini.WriteBool   ('base', 'Hello', false);
  // gfx //
  ini.WriteInteger('gfx', 'BaseAlpha', container.BaseAlpha);
  ini.WriteBool   ('gfx', 'Reflection', container.Reflection);
  ini.WriteBool   ('gfx', 'Blur', container.Blur);
  // font //
  ini.WriteString ('Font', 'name', pchar(@container.Font.name[0]));
  ini.WriteInteger('Font', 'size', container.Font.size);
  ini.WriteInteger('Font', 'color', container.Font.color);
  ini.WriteInteger('Font', 'backcolor', container.Font.color_outline);
  ini.WriteBool   ('Font', 'bold', container.Font.bold);
  ini.WriteBool   ('Font', 'italic', container.Font.italic);
  // stack font //
  ini.WriteString ('StackFont', 'name', pchar(@container.StackFont.name[0]));
  ini.WriteInteger('StackFont', 'size', container.StackFont.size);
  ini.WriteInteger('StackFont', 'color', container.StackFont.color);
  ini.WriteInteger('StackFont', 'backcolor', container.StackFont.color_outline);
  ini.WriteBool   ('StackFont', 'bold', container.StackFont.bold);
  ini.WriteBool   ('StackFont', 'italic', container.StackFont.italic);

  // autoruns //
  idx:= 0;
  if assigned(AutoRunList) then
    if AutoRunList.Count > 0 then
      while idx < AutoRunList.count do
      begin
        if AutoRunList.strings[idx] <> '' then
          ini.WriteString('autorun', 'command' + inttostr(idx), '"' + AutoRunList.strings[idx] + '"');
        inc(idx);
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
    on e: Exception do raise Exception.Create('Sets.SaveEx'#10#13 + e.message);
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
    on e: Exception do raise Exception.Create('Sets.SaveEx2'#10#13 + e.message);
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
    on e: Exception do raise Exception.Create('Sets.Backup'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _Sets.Restore: boolean;
var
  bakfile: string;
begin
  result := false;
  try
    AddLog('Sets.Restore');
    if FileExists(SetsPathFile) then
      if not windows.DeleteFile(PChar(SetsPathFile)) then raise Exception.Create('DeleteSetsFile failed');

    bakfile := ChangeFileExt(SetsPathFile, '.tmpini');
    if FileExists(bakfile) then
       if windows.MoveFile(pchar(bakfile), pchar(SetsPathFile)) then result := true;

    if not result then
    begin
      bakfile := ChangeFileExt(SetsPathFile, '.bak');
      if windows.CopyFile(pchar(bakfile), pchar(SetsPathFile), false) then result := true;
    end;
  except
    on e: Exception do raise Exception.Create('Sets.Restore'#10#13 + e.message);
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
  gpZoomTime: container.ZoomTime := SetRange(value, 0, 600);
  gpZoomItems: container.ZoomItems := boolean(value);
  gpMonitor: container.Monitor := SetRange(value, -1, screen.MonitorCount - 1);
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
  gpStackOpenAnimation: container.StackOpenAnimation := boolean(value);
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
  gpZoomTime: result := container.ZoomTime;
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
  gpStackOpenAnimation: result := integer(container.StackOpenAnimation);
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
  dst.ZoomTime := src.ZoomTime;
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
  dst.StackOpenAnimation := src.StackOpenAnimation;
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
  idx: integer;
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
    toolu.SearchFilesRecurse(PluginsPath, '*.dll', PluginFilesList);

    idx := 0;
    while idx < PluginFilesList.Count do
    begin
      SetCurrentDir(ExtractFilePath(PluginFilesList.strings[idx]));
      hLib := LoadLibrary(pchar(PluginFilesList.strings[idx]));
      if hLib < 33 then PluginFilesList.Delete(idx)
      else begin
        @OnGetInformation := GetProcAddress(hLib, 'OnGetInformation');
        if not assigned(OnGetInformation) then PluginFilesList.Delete(idx)
        else
        begin
          try OnGetInformation(@szName, @szAuthor, @lpiVersion, @szNotes);
          except end;
          PluginsList.add(strpas(@szName));
          inc(idx);
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
