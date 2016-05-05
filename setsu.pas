unit setsu;

interface
uses Windows, Controls, Forms, Classes, SysUtils, Dialogs,
      Menus, StdCtrls, IniFiles, gfx, declu, toolu, dockh;

type

  { TDSetsContainer }

  TDSetsContainer = record
    Monitor: integer;
    Site: TBaseSite;
    CenterOffsetPercent: integer;
    EdgeOffset: integer;
    OccupyFullMonitor: boolean; // draw the background over the entire width (or height) of the monitor
    StartOffset: integer; // for example: when Site=bsBottom this is at the left side of the dock
    EndOffset: integer; // and this is at the right side of the dock
    AutoHide: boolean;
    AutoHideTime: integer;
    AutoShowTime: integer;
    AutoHidePixels: integer;
    ItemSize: integer;
    BigItemSize: integer;
    ZoomWidth: integer;
    ItemSpacing: integer;
    ZoomEnabled: boolean;
    ZoomTime: integer;
    GlobalHotkeyFlag_Hide: boolean;
    GlobalHotkeyValue_Hide: integer;
    GlobalHotkeyFlag_Console: boolean;
    GlobalHotkeyValue_Console: integer;
    DropDistance: integer;
    LaunchInterval: integer;
    ActivateRunningApps: boolean;
    ShowRunningIndicator: boolean;
    ItemAnimationType: integer;
    LaunchInThread: boolean;
    ActivateOnMouse: boolean;
    ActivateOnMouseInterval: integer;
    CloseCmdWindow: boolean;
    HideSystemTaskbar: boolean;
    ReserveScreenEdge: boolean;
    ReserveScreenEdgePercent: integer;
    Taskbar: boolean;
    TaskLivePreviews: boolean;
    TaskThumbSize: integer;
    TaskGrouping: boolean;
    TaskSameMonitor: boolean;
    TaskSpot: integer;
    StayOnTop: boolean;
    ShowHint: boolean;
    HintEffects: boolean;
    LockDragging: boolean;
    UseShellContextMenus: boolean;
    StackAnimationEnabled: boolean;
    AutoHideOnFullScreenApp: boolean;
    RunInThread: boolean;
    UseShell: boolean;
    Hello: boolean;
    ReflectionEnabled: boolean;
    ReflectionSize: integer;
    BaseAlpha: integer;
    SeparatorAlpha: integer;
    BlurEnabled: boolean;
    Font: _FontData;
    Shell: array [0..MAX_PATH] of char;
    ThemeName: array [0..MAX_PATH] of char;
  end;

  { TDSets }

  TDSets = class
  private
    FSetsPathFile: string;
    FPluginsPath: string;
    PluginsList: TStrings;
    PluginFilesList: TStrings;
    FBackupPath: string;
    BackupsList: TStrings;
    FThemesPath: string;
  public
    container: TDSetsContainer;
    cancel_container: TDSetsContainer;
    ParentHWnd: THandle;
    AutoRunList: TStrings;

    property SetsPathFile: string read FSetsPathFile;
    property BackupsPath: string read FBackupPath;
    property PluginsPath: string read FPluginsPath;
    property ThemesPath: string read FThemesPath;

    constructor Create(ASetsPathFile, AProgramPath: string; Handle: THandle);
    destructor Destroy; override;
    procedure Load; overload;
    procedure Load(ASetsFile: string); overload;
    procedure Save; overload;
    procedure Save(ASetsFile: string); overload;
    function Backup: boolean;
    function Restore(backupFilename: string): boolean;
    function StoreParam(id: TGParam; value: integer): integer;
    function GetParam(id: TGParam): integer;
    procedure StoreSetsContainer;
    procedure RestoreSetsContainer;
    procedure CopySetsContainer(var dst: TDSetsContainer; var src: TDSetsContainer);
    function getBaseOrientation: TBaseOrientation;
    function GetMonitorCount: integer;
    function GetMonitorName(index: integer): string;
    function GetPluginCount: integer;
    function GetPluginName(index: integer): string;
    function GetPluginFileName(index: integer): string;
    procedure ScanPlugins;
    procedure GetPluginInfo(index: integer; mem: TMemo);
  end;

var sets: TDSets;

implementation
//------------------------------------------------------------------------------
constructor TDSets.Create(ASetsPathFile, AProgramPath: string; Handle: THandle);
begin
  inherited Create;
  ParentHWnd := Handle;
  StrCopy(container.ThemeName, 'Aero');
  container.AutoHidePixels := 0;
  container.CenterOffsetPercent := 50;
  container.EdgeOffset := 0;
  container.ActivateOnMouse := true;
  container.ActivateOnMouseInterval := 400;
  container.GlobalHotkeyFlag_Hide := false;
  container.GlobalHotkeyValue_Hide := 16506;
  container.GlobalHotkeyFlag_Console := false;
  container.GlobalHotkeyValue_Console := 0;
  container.site := bsBottom;
  container.CloseCmdWindow := true;
  container.BaseAlpha := 255;
  container.SeparatorAlpha := 255;
  container.Hello := true;
  FSetsPathFile := ASetsPathFile;
  FBackupPath := IncludeTrailingPathDelimiter(AProgramPath) + 'Backup';
  FPluginsPath := IncludeTrailingPathDelimiter(AProgramPath) + 'Docklets';
  FThemesPath := IncludeTrailingPathDelimiter(AProgramPath) + 'Themes\';
end;
//------------------------------------------------------------------------------
destructor TDSets.Destroy;
begin
  if assigned(BackupsList) then BackupsList.free;
  if assigned(PluginsList) then PluginsList.free;
  if assigned(PluginFilesList) then PluginFilesList.free;
  if assigned(AutoRunList) then AutoRunList.free;
end;
//------------------------------------------------------------------------------
procedure TDSets.Load(ASetsFile: string);
begin
  FSetsPathFile := ASetsFile;
  Load;
end;
//------------------------------------------------------------------------------
procedure TDSets.Load;
var
  idx: integer;
  ini: TIniFile;
  tmplist: TStrings;
begin
  // load sets //
  ini:= TIniFile.Create(SetsPathFile);
  // base //
  StrCopy(container.ThemeName, pchar(ini.ReadString('base', 'Theme', 'Aero')));
  container.Monitor := ini.ReadInteger('base', 'Monitor', 0);
  container.Site := StringToSite(ini.ReadString('base', 'Site', 'top'));
  container.CenterOffsetPercent := SetRange(ini.ReadInteger('base', 'CenterOffsetPercent', 50), 0, 100);
  container.EdgeOffset := SetRange(ini.ReadInteger('base', 'EdgeOffset', 0), -100, 100);
  container.OccupyFullMonitor := ini.ReadBool('base', 'OccupyFullMonitor', false);
  container.StartOffset := SetRange(ini.ReadInteger('base', 'StartOffset', 0), -10000, 10000);
  container.EndOffset := SetRange(ini.ReadInteger('base', 'EndOffset', 0), -10000, 10000);
  container.autohidetime := SetRange(ini.ReadInteger('base', 'AutoHideTime', 800), 0, 9999);
  container.autoshowtime := SetRange(ini.ReadInteger('base', 'AutoShowTime', 400), 0, 9999);
  container.LaunchInterval := SetRange(ini.ReadInteger('base', 'LaunchInterval', 500), 0, 9999);
  container.ActivateRunningApps := ini.ReadBool('base', 'ActivateRunning', true);
  container.ShowRunningIndicator := ini.ReadBool('base', 'ShowRunningIndicator', true);
  container.ItemAnimationType := SetRange(ini.ReadInteger('base', 'ItemAnimation', 4), 0, 8);
  container.ItemSize := SetRange(ini.ReadInteger('base', 'ItemSize', 48), 16, 128);
  container.BigItemSize := SetRange(ini.ReadInteger('base', 'BigItemSize', 96), container.ItemSize, 256);
  container.ItemSpacing := SetRange(ini.ReadInteger('base', 'ItemSpacing', 4), 0, 30);
  container.ZoomWidth := SetRange(ini.ReadInteger('base', 'ZoomWidth', 6), 4, 10);
  container.ZoomTime := SetRange(ini.ReadInteger('base', 'ZoomTime', 120), 0, 600);
  container.AutoHidePixels := ini.ReadInteger('base', 'AutoHidePixels', 15);
  container.DropDistance := container.ItemSize;
  container.GlobalHotkeyFlag_Hide := ini.ReadBool('base', 'GlobalHotkeyFlag_Hide', false);
  container.GlobalHotkeyValue_Hide := ini.ReadInteger('base', 'GlobalHotkeyValue_Hide', 16506);
  container.GlobalHotkeyFlag_Console := ini.ReadBool('base', 'GlobalHotkeyFlag_Console', false);
  container.GlobalHotkeyValue_Console := ini.ReadInteger('base', 'GlobalHotkeyValue_Console', 0);
  container.autohide := ini.ReadBool('base', 'AutoHide', false);
  container.LaunchInThread := ini.ReadBool('base', 'LaunchInThread', true);
  container.ZoomEnabled := ini.ReadBool('base', 'ZoomItems', true);
  container.ActivateOnMouse := ini.ReadBool('base', 'ActivateOnMouse', true);
  container.ActivateOnMouseInterval := SetRange(ini.ReadInteger('base', 'ActivateOnMouseInterval', 400), 0, 2000);
  container.CloseCmdWindow := ini.ReadBool('base', 'CloseCmdWindow', true);
  container.HideSystemTaskbar := ini.ReadBool('base', 'HideTaskBar', false);
  container.ReserveScreenEdge := ini.ReadBool('base', 'ReserveScreenEdge', false);
  container.ReserveScreenEdgePercent := SetRange(ini.ReadInteger('base', 'ReserveScreenEdgePercent', 100), 0, 200);
  container.Taskbar := ini.ReadBool('base', 'Taskbar', false);
  container.TaskLivePreviews := ini.ReadBool('base', 'TaskbarLivePreviews', true);
  container.TaskThumbSize := ini.ReadInteger('base', 'TaskThumbSize', 200);
  container.TaskGrouping := ini.ReadBool('base', 'TaskbarGrouping', true);
  container.TaskSameMonitor := ini.ReadBool('base', 'TaskbarSameMonitor', false);
  container.TaskSpot := ini.ReadInteger('base', 'TaskSpot', -1);
  container.StayOnTop := ini.ReadBool('base', 'StayOnTop', false);
  container.LockDragging := ini.ReadBool('base', 'LockDragging', true);
  container.ShowHint := ini.ReadBool('base', 'ShowHint', true);
  container.HintEffects := ini.ReadBool('base', 'HintEffects', false);
  container.AutoHideOnFullScreenApp := ini.ReadBool('base', 'AutoHideOnFullScreenApp', true);
  container.UseShellContextMenus := ini.ReadBool('base', 'UseShellContextMenus', true);
  container.StackAnimationEnabled := ini.ReadBool('base', 'StackOpenAnimation', true);
  container.Hello := ini.ReadBool('base', 'Hello', true);
  container.useShell := ini.ReadBool('base', 'UseShell', false);
  StrCopy(container.Shell, pchar(ini.ReadString('base', 'Shell', '')));
  container.RunInThread := ini.ReadBool('base', 'RunInThread', false);
  // font //
  StrCopy(container.Font.name, pchar(ini.ReadString('Font', 'name', toolu.GetFont)));
  container.Font.size:= SetRange(ini.ReadInteger('Font', 'size', 15), 6, 72);
  container.Font.size2:= SetRange(ini.ReadInteger('Font', 'size2', 14), 6, 72);
  container.Font.color:= uint(ini.ReadInteger('Font', 'color', integer($ffffffff)));
  container.Font.backcolor:= uint(ini.ReadInteger('Font', 'backcolor', integer($ff202020)));
  container.Font.bold:= ini.ReadBool('Font', 'bold', true);
  container.Font.italic:= ini.ReadBool('Font', 'italic', false);
  // gfx //
  container.BaseAlpha := SetRange(ini.ReadInteger('gfx', 'BaseAlpha', 255), 13, 255);
  container.SeparatorAlpha := SetRange(ini.ReadInteger('gfx', 'SeparatorAlpha', 255), 0, 255);
  container.ReflectionEnabled := ini.ReadBool('gfx', 'Reflection', true);
  container.ReflectionSize := ini.ReadInteger('gfx', 'ReflectionSize', 10);
  container.BlurEnabled := ini.ReadBool('gfx', 'Blur', true);

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
procedure TDSets.Save(ASetsFile: string);
begin
  FSetsPathFile := ASetsFile;
  Save;
end;
//------------------------------------------------------------------------------
procedure TDSets.Save;
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
  ini.WriteBool   ('base', 'OccupyFullMonitor', container.OccupyFullMonitor);
  ini.WriteInteger('base', 'StartOffset', container.StartOffset);
  ini.WriteInteger('base', 'EndOffset', container.EndOffset);
  ini.WriteString ('base', 'Shell', pchar(@container.Shell[0]));
  ini.WriteBool   ('base', 'AutoHide', container.autohide);
  ini.WriteInteger('base', 'AutoHideTime', container.autohidetime);
  ini.WriteInteger('base', 'AutoShowTime', container.autoshowtime);
  ini.WriteInteger('base', 'AutoHidePixels', container.AutoHidePixels);
  ini.WriteInteger('base', 'LaunchInterval', container.LaunchInterval);
  ini.WriteInteger('base', 'ActivateRunning', integer(container.ActivateRunningApps));
  ini.WriteInteger('base', 'ShowRunningIndicator', integer(container.ShowRunningIndicator));
  ini.WriteInteger('base', 'ItemAnimation', container.ItemAnimationType);
  ini.WriteInteger('base', 'ItemSize', container.itemsize);
  ini.WriteInteger('base', 'BigItemSize', container.BigItemSize);
  ini.WriteInteger('base', 'ItemSpacing', container.ItemSpacing);
  ini.WriteInteger('base', 'ZoomWidth', container.ZoomWidth);
  ini.WriteInteger('base', 'ZoomTime', container.ZoomTime);
  ini.WriteBool   ('base', 'GlobalHotkeyFlag_Hide', container.GlobalHotkeyFlag_Hide);
  ini.WriteInteger('base', 'GlobalHotkeyValue_Hide', container.GlobalHotkeyValue_Hide);
  ini.WriteBool   ('base', 'GlobalHotkeyFlag_Console', container.GlobalHotkeyFlag_Console);
  ini.WriteInteger('base', 'GlobalHotkeyValue_Console', container.GlobalHotkeyValue_Console);
  ini.WriteBool   ('base', 'AutoHideOnFullScreenApp', container.AutoHideOnFullScreenApp);
  ini.WriteBool   ('base', 'UseShell', container.useShell);
  ini.WriteBool   ('base', 'RunInThread', container.RunInThread);
  ini.WriteBool   ('base', 'ZoomItems', container.ZoomEnabled);
  ini.WriteBool   ('base', 'LaunchInThread', container.launchInThread);
  ini.WriteBool   ('base', 'ActivateOnMouse', container.ActivateOnMouse);
  ini.WriteInteger('base', 'ActivateOnMouseInterval', container.ActivateOnMouseInterval);
  ini.WriteBool   ('base', 'CloseCmdWindow', container.CloseCmdWindow);
  ini.WriteBool   ('base', 'HideTaskBar', container.HideSystemTaskbar);
  ini.WriteBool   ('base', 'ReserveScreenEdge', container.ReserveScreenEdge);
  ini.WriteInteger('base', 'ReserveScreenEdgePercent', container.ReserveScreenEdgePercent);
  ini.WriteBool   ('base', 'Taskbar', container.Taskbar);
  ini.WriteBool   ('base', 'TaskbarLivePreviews', container.TaskLivePreviews);
  ini.WriteInteger('base', 'TaskThumbSize', container.TaskThumbSize);
  ini.WriteBool   ('base', 'TaskbarGrouping', container.TaskGrouping);
  ini.WriteBool   ('base', 'TaskbarSameMonitor', container.TaskSameMonitor);
  ini.WriteInteger('base', 'TaskSpot', container.TaskSpot);
  ini.WriteBool   ('base', 'StayOnTop', container.StayOnTop);
  ini.WriteBool   ('base', 'LockDragging', container.LockDragging);
  ini.WriteBool   ('base', 'ShowHint', container.ShowHint);
  ini.WriteBool   ('base', 'HintEffects', container.HintEffects);
  ini.WriteBool   ('base', 'UseShellContextMenus', container.UseShellContextMenus);
  ini.WriteBool   ('base', 'StackOpenAnimation', container.StackAnimationEnabled);
  ini.WriteBool   ('base', 'Hello', false);
  // gfx //
  ini.WriteInteger('gfx', 'BaseAlpha', container.BaseAlpha);
  ini.WriteInteger('gfx', 'SeparatorAlpha', container.SeparatorAlpha);
  ini.WriteBool   ('gfx', 'Reflection', container.ReflectionEnabled);
  ini.WriteInteger('gfx', 'ReflectionSize', container.ReflectionSize);
  ini.WriteBool   ('gfx', 'Blur', container.BlurEnabled);
  // font //
  ini.WriteString ('Font', 'name', pchar(@container.Font.name[0]));
  ini.WriteInteger('Font', 'size', container.Font.size);
  ini.WriteInteger('Font', 'size2', container.Font.size2);
  ini.WriteInteger('Font', 'color', container.Font.color);
  ini.WriteInteger('Font', 'backcolor', container.Font.backcolor);
  ini.WriteBool   ('Font', 'bold', container.Font.bold);
  ini.WriteBool   ('Font', 'italic', container.Font.italic);

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
function TDSets.Backup: boolean;
var
  setsFilenameBase, backupFile, lastBackupFile: string;
  list: TStrings;
  lastBackupFS, setsFS: TFileStream;
  identical: boolean;
begin
  result := true;
  try
    if not FileExists(SetsPathFile) then exit;
    setsFilenameBase := ChangeFileExt(ExtractFileName(SetsPathFile), '');

    // list all backup files
    list := TStringList.Create;
    toolu.SearchFilesRecursive(BackupsPath, setsFilenameBase + '*.ini', list);
    toolu.qSortStrings(list);

    // compare the last one (if exists) with the settings file
    identical := false;
    if list.Count > 0 then
    begin
      lastBackupFile := list.Strings[list.Count - 1];
      lastBackupFS := TFileStream.Create(lastBackupFile, fmOpenRead);
      setsFS := TFileStream.Create(SetsPathFile, fmOpenRead);
      identical := IsIdenticalStreams(lastBackupFS, setsFS);
      lastBackupFS.free;
      setsFS.free;
		end;
    list.free;

    // if exists and they are identical - exit
    if identical then exit;

    if not DirectoryExists(BackupsPath) then windows.CreateDirectory(pchar(BackupsPath), nil);
    backupFile := BackupsPath + '\' + setsFilenameBase + '__' + FormatDateTime('yyyy-MM-dd__hh-nn-ss', Now) + '.ini';
    if not windows.CopyFile(pchar(SetsPathFile), pchar(backupFile), false) then result := false;
  except
    on e: Exception do raise Exception.Create('Sets.Backup '#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TDSets.Restore(backupFilename: string): boolean;
begin
  result := false;
  try
    AddLog('Sets.Restore');

    if not FileExists(backupFilename) then
      raise Exception.Create('Backup file not found!');

    if FileExists(SetsPathFile) then
      if not windows.DeleteFile(PChar(SetsPathFile)) then
        raise Exception.Create('Can not delete settings file!');

    if windows.CopyFile(pchar(backupFilename), pchar(SetsPathFile), false) then result := true;
  except
    on e: Exception do raise Exception.Create('Sets.Restore '#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TDSets.StoreParam(id: TGParam; value: integer): integer;
begin
  case id of
  gpItemSize: container.ItemSize := SetRange(value, 16, 128);
  gpBigItemSize: container.BigItemSize := SetRange(value, container.ItemSize, 256);
  gpItemSpacing: container.ItemSpacing := SetRange(value, 0, 30);
  gpZoomWidth: container.ZoomWidth := SetRange((value div 2) * 2, 4, 10);
  gpZoomTime: container.ZoomTime := SetRange(value, 0, 600);
  gpZoomEnabled: container.ZoomEnabled := boolean(value);
  gpMonitor: container.Monitor := SetRange(value, -1, screen.MonitorCount - 1);
  gpSite: container.Site := TBaseSite(SetRange(value, 0, 3));
  gpCenterOffsetPercent: container.CenterOffsetPercent := SetRange(value, 0, 100);
  gpEdgeOffset: container.EdgeOffset := SetRange(value, -100, 100);
  gpOccupyFullMonitor: container.OccupyFullMonitor := boolean(value);
  gpStartOffset: container.StartOffset := SetRange(value, -10000, 10000);
  gpEndOffset: container.EndOffset := SetRange(value, -10000, 10000);
  gpAutoHideTime: container.AutoHideTime := value;
  gpAutoShowTime: container.AutoShowTime := value;
  gpAutoHidePixels: container.AutoHidePixels := SetRange(value, 0, 9999);
  gpGlobalHotkeyFlag_Hide: container.GlobalHotkeyFlag_Hide:= boolean(value);
  gpGlobalHotkeyValue_Hide: container.GlobalHotkeyValue_Hide:= value;
  gpGlobalHotkeyFlag_Console: container.GlobalHotkeyFlag_Console:= boolean(value);
  gpGlobalHotkeyValue_Console: container.GlobalHotkeyValue_Console:= value;
  gpDropDistance: container.DropDistance := SetRange(value, 50, 500);
  gpLaunchInterval: container.LaunchInterval := SetRange(value, 0, 9999);
  gpActivateRunning: container.ActivateRunningApps := boolean(value);
  gpShowRunningIndicator: container.ShowRunningIndicator := boolean(value);
  gpItemAnimationType: container.ItemAnimationType := value;
  gpUseShell: container.UseShell := boolean(value);
  gpRunInThread: container.RunInThread := boolean(value);
  gpAutoHide: container.AutoHide := boolean(value);
  gpLaunchInThread: container.LaunchInThread := boolean(value);
  gpActivateOnMouse: container.ActivateOnMouse := boolean(value);
  gpActivateOnMouseInterval: container.ActivateOnMouseInterval := SetRange(value, 0, 2000);
  gpCloseCmdWindow: container.CloseCmdWindow := boolean(value);
  gpHideSystemTaskbar: container.HideSystemTaskbar := boolean(value);
  gpReserveScreenEdge: container.ReserveScreenEdge := boolean(value);
  gpReserveScreenEdgePercent: container.ReserveScreenEdgePercent := SetRange(value, 0, 200);
  gpTaskbar: container.Taskbar := boolean(value);
  gpTaskLivePreviews: container.TaskLivePreviews := boolean(value);
  gpTaskThumbSize: container.TaskThumbSize := value;
  gpTaskGrouping: container.TaskGrouping := boolean(value);
  gpTaskSameMonitor: container.TaskSameMonitor := boolean(value);
  gpStayOnTop: container.StayOnTop := boolean(value);
  gpShowHint: container.ShowHint := boolean(value);
  gpHintEffects: container.HintEffects := boolean(value);
  gpLockDragging: container.LockDragging := boolean(value);
  gpReflectionEnabled: container.ReflectionEnabled := boolean(value);
  gpReflectionSize: container.ReflectionSize := value;
  gpAutoHideOnFullScreenApp: container.AutoHideOnFullScreenApp := boolean(value);
  gpUseShellContextMenus: container.UseShellContextMenus := boolean(value);
  gpStackAnimationEnabled: container.StackAnimationEnabled := boolean(value);
  gpBaseAlpha: container.BaseAlpha := SetRange(value, 13, 255);
  gpSeparatorAlpha: container.SeparatorAlpha := SetRange(value, 0, 255);
  gpBlurEnabled: container.BlurEnabled := boolean(value);
  gpTaskSpot: container.TaskSpot := value;
  end;

  result := value;
end;
//------------------------------------------------------------------------------
function TDSets.GetParam(id: TGParam): integer;
begin
  result:= 0;
  case id of
  gpItemSize: result := container.ItemSize;
  gpBigItemSize: result := container.BigItemSize;
  gpZoomWidth: result := container.ZoomWidth;
  gpZoomTime: result := container.ZoomTime;
  gpItemSpacing: result := container.ItemSpacing;
  gpZoomEnabled: result := integer(container.ZoomEnabled);
  gpMonitor: result := container.Monitor;
  gpSite: result := integer(container.Site);
  gpCenterOffsetPercent: result := container.CenterOffsetPercent;
  gpEdgeOffset: result := container.EdgeOffset;
  gpOccupyFullMonitor: result := integer(container.OccupyFullMonitor);
  gpStartOffset: result := container.StartOffset;
  gpEndOffset: result := container.EndOffset;
  gpAutoHideTime: result := container.AutoHideTime;
  gpAutoShowTime: result := container.AutoShowTime;
  gpAutoHidePixels: result := container.AutoHidePixels;
  gpGlobalHotkeyFlag_Hide: result := integer(container.GlobalHotkeyFlag_Hide);
  gpGlobalHotkeyValue_Hide: result := container.GlobalHotkeyValue_Hide;
  gpGlobalHotkeyFlag_Console: result := integer(container.GlobalHotkeyFlag_Console);
  gpGlobalHotkeyValue_Console: result := container.GlobalHotkeyValue_Console;
  gpDropDistance: result := container.DropDistance;
  gpLaunchInterval: result := container.LaunchInterval;
  gpActivateRunning: result := integer(container.ActivateRunningApps);
  gpShowRunningIndicator: result := integer(container.ShowRunningIndicator);
  gpItemAnimationType: result := container.ItemAnimationType;
  gpUseShell: result := integer(container.UseShell);
  gpRunInThread: result := integer(container.RunInThread);
  gpAutoHide: result := integer(container.AutoHide);
  gpLaunchInThread: result := integer(container.LaunchInThread);
  gpActivateOnMouse: result := integer(container.ActivateOnMouse);
  gpActivateOnMouseInterval: result := container.ActivateOnMouseInterval;
  gpCloseCmdWindow: result := integer(container.CloseCmdWindow);
  gpHideSystemTaskbar: result := integer(container.HideSystemTaskbar);
  gpReserveScreenEdge: result := integer(container.ReserveScreenEdge);
  gpReserveScreenEdgePercent: result := container.ReserveScreenEdgePercent;
  gpTaskbar: result := integer(container.Taskbar);
  gpTaskLivePreviews: result := integer(container.TaskLivePreviews);
  gpTaskThumbSize: result := container.TaskThumbSize;
  gpTaskGrouping: result := integer(container.TaskGrouping);
  gpTaskSameMonitor: result := integer(container.TaskSameMonitor);
  gpStayOnTop: result := integer(container.StayOnTop);
  gpShowHint: result := integer(container.ShowHint);
  gpHintEffects: result := integer(container.HintEffects);
  gpLockDragging: result := integer(container.LockDragging);
  gpReflectionEnabled: result := integer(container.ReflectionEnabled);
  gpReflectionSize: result := container.ReflectionSize;
  gpAutoHideOnFullScreenApp: result := integer(container.AutoHideOnFullScreenApp);
  gpUseShellContextMenus: result := integer(container.UseShellContextMenus);
  gpStackAnimationEnabled: result := integer(container.StackAnimationEnabled);
  gpBaseAlpha: result := container.BaseAlpha;
  gpSeparatorAlpha: result := container.SeparatorAlpha;
  gpBlurEnabled: result := integer(container.BlurEnabled);
  gpTaskSpot: result := container.TaskSpot;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSets.StoreSetsContainer;
begin
  CopySetsContainer(cancel_container, container);
end;
//------------------------------------------------------------------------------
procedure TDSets.RestoreSetsContainer;
begin
  CopySetsContainer(container, cancel_container);
end;
//------------------------------------------------------------------------------
procedure TDSets.CopySetsContainer(var dst: TDSetsContainer; var src: TDSetsContainer);
begin
  dst.Monitor := src.Monitor;
  dst.site := src.site;
  dst.CenterOffsetPercent := src.CenterOffsetPercent;
  dst.EdgeOffset := src.EdgeOffset;
  dst.StartOffset := src.StartOffset;
  dst.EndOffset := src.EndOffset;
  dst.autohide := src.autohide;
  dst.autohidetime := src.autohidetime;
  dst.autoshowtime := src.autoshowtime;
  dst.AutoHidePixels := src.AutoHidePixels;
  dst.ItemSize := src.ItemSize;
  dst.BigItemSize := src.BigItemSize;
  dst.ZoomWidth := src.ZoomWidth;
  dst.ZoomTime := src.ZoomTime;
  dst.ItemSpacing := src.ItemSpacing;
  dst.ZoomEnabled := src.ZoomEnabled;
  dst.GlobalHotkeyFlag_Hide := src.GlobalHotkeyFlag_Hide;
  dst.GlobalHotkeyValue_Hide := src.GlobalHotkeyValue_Hide;
  dst.GlobalHotkeyFlag_Console := src.GlobalHotkeyFlag_Console;
  dst.GlobalHotkeyValue_Console := src.GlobalHotkeyValue_Console;
  dst.DropDistance := src.DropDistance;
  dst.LaunchInterval := src.LaunchInterval;
  dst.ActivateRunningApps := src.ActivateRunningApps;
  dst.ShowRunningIndicator := src.ShowRunningIndicator;
  dst.ItemAnimationType := src.ItemAnimationType;
  dst.launchInThread := src.launchInThread;
  dst.ActivateOnMouse := src.ActivateOnMouse;
  dst.ActivateOnMouseInterval := src.ActivateOnMouseInterval;
  dst.CloseCmdWindow := src.CloseCmdWindow;
  dst.HideSystemTaskbar := src.HideSystemTaskbar;
  dst.ReserveScreenEdge := src.ReserveScreenEdge;
  dst.ReserveScreenEdgePercent := src.ReserveScreenEdgePercent;
  dst.Taskbar := src.Taskbar;
  dst.TaskLivePreviews := src.TaskLivePreviews;
  dst.TaskThumbSize := src.TaskThumbSize;
  dst.TaskGrouping := src.TaskGrouping;
  dst.TaskSameMonitor := src.TaskSameMonitor;
  dst.StayOnTop := src.StayOnTop;
  dst.ShowHint := src.ShowHint;
  dst.HintEffects := src.HintEffects;
  dst.LockDragging := src.LockDragging;
  dst.UseShellContextMenus := src.UseShellContextMenus;
  dst.StackAnimationEnabled := src.StackAnimationEnabled;
  dst.AutoHideOnFullScreenApp := src.AutoHideOnFullScreenApp;
  dst.ReflectionEnabled := src.ReflectionEnabled;
  dst.ReflectionSize := src.ReflectionSize;
  dst.BaseAlpha := src.BaseAlpha;
  dst.SeparatorAlpha := src.SeparatorAlpha;
  dst.BlurEnabled := src.BlurEnabled;
  dst.OccupyFullMonitor := src.OccupyFullMonitor;
  dst.useShell := src.useShell;
  dst.RunInThread := src.RunInThread;
  dst.TaskSpot := src.TaskSpot;

  CopyFontData(src.Font, dst.Font);
  StrCopy(dst.Shell, pchar(@src.Shell));
  StrCopy(dst.ThemeName, pchar(@src.ThemeName));
end;
//------------------------------------------------------------------------------
function TDSets.getBaseOrientation: TBaseOrientation;
begin
  result := boHorizontal;
  if (container.site = bsLeft) or (container.site = bsRight) then result := boVertical;
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
function TDSets.GetMonitorCount: integer;
begin
  result := screen.MonitorCount;
end;
//------------------------------------------------------------------------------
function TDSets.GetMonitorName(index: integer): string;
begin
  if index < 0 then result := XAll
  else result := XMonitor + ' ' + inttostr(screen.Monitors[index].MonitorNum + 1);
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
function TDSets.GetPluginCount: integer;
begin
  result := -1;
  if assigned(PluginsList) then result := PluginsList.Count;
end;
//------------------------------------------------------------------------------
function TDSets.GetPluginName(index: integer): string;
begin
  result := '';
  if assigned(PluginsList) then result := PluginsList.Strings[index];
end;
//------------------------------------------------------------------------------
function TDSets.GetPluginFileName(index: integer): string;
begin
  result := '';
  if assigned(PluginFilesList) then result := PluginFilesList.Strings[index];
end;
//------------------------------------------------------------------------------
procedure TDSets.ScanPlugins;
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
    toolu.SearchFilesRecursive(PluginsPath, '*.dll', PluginFilesList);

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
procedure TDSets.GetPluginInfo(index: integer; mem: TMemo);
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
