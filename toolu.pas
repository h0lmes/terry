unit toolu;

interface

uses Windows, jwaWindows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ShellAPI, Registry, ComObj, ShlObj, ActiveX;

function IsWindowsVista: boolean;
function IsWin64: boolean;
function GetFont: string;
function GetContentFont: string;
function GetFontSize: integer;
function GetContentFontSize: integer;
function CreateAFont(Name: string; size: integer): HFont;
function cut(itext, ch: string): string;
function cutafter(itext, ch: string): string;
procedure split(itext, ch: string; var str1, str2: string);
procedure split_cmd(incmd: string; var cmd, params: string);
function ReplaceEx(strSrc, strWhat, strWith: string): string;
function fetch(var itext: string; delim: string; adelete: boolean = False): string;
function FetchValue(itext: string; Value, delim: string): string;
function PosEx(Value, atext: string; startpos: integer): integer;
function cuttolast(itext, ch: string): string;
function cutafterlast(itext, ch: string): string;
function IsValidItemString(str: string): boolean;
function IsValidShortcutString(str: string): boolean;
function StringToRect(str: string): Windows.Trect;
function RectToString(r: Windows.Trect): string;
function StringToSize(str: string): Windows.TSize;
function SizeToString(r: Windows.TSize): string;
function StringToPoint(str: string): Windows.Tpoint;
function SetRange(value, min, max: integer): integer;
function IsDriveIdent(ident: string): boolean;
procedure searchfiles(path, mask: string; list: TStrings);
procedure searchfolders(path: string; list: TStrings);
procedure searchfilesrecurse(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
function ReadIniString(IniFile, IniSection, KeyName, Default: string): string;
function ReadIniInteger(IniFile, IniSection, KeyName: string; Default: integer): integer;
function CheckAutoRun: boolean;
procedure SetAutoRun(enable: boolean);
function CheckRunAsAdmin(filename: string): boolean;
procedure SetRunAsAdmin(filename: string; enable: boolean);
function GetWinVersion: string;
procedure ShutDown(mode: integer);
function GetUser: string;
procedure GetFileVersion(filename: string; var maj, min, Release, build: integer);
procedure AllowSetForeground(hWnd: HWND);
function GetEnvVar(VarName: string): string;
function FindFile(filename: string): string;
function FindFilePF(filename: string): string;
function UnzipPath(path: string): string;
function ZipPath(path: string): string;
function GetSystemDir: string;
function GetWinDir: string;
function GetSystemPath(path: string): string;
procedure setdisplaymode(x: integer = 800; y: integer = 600; bits: integer = 16; freq: integer = 60);
procedure ResolveShortcut(wnd: HWND; var ShortcutPath: string; out params, dir, icon: string);
function BrowseFolder(hWnd: THandle; title, default: string): string;
procedure FreeAndNil(var Obj);
function LinkAPI(const module, functionname: string): pointer;
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: boolean): boolean;
function SetPrivilege(Name: string): boolean;
procedure SetClipboard(Text: string);
function GetClipboard: string;
function ColorToString(Color: uint): string;
function StringToColor(const str: string): uint;
function confirm(handle: cardinal; Text: string = ''): boolean;
function FindWinamp: cardinal;
function LaunchWinamp(sw: integer = sw_shownormal): boolean;
function wacmd(cmd: cardinal): boolean;
procedure AddLog(LogString: string);
procedure TruncLog(fs: TFileStream);
procedure bsm(msg: uint; wparam: WPARAM; lparam: LPARAM);

implementation

//------------------------------------------------------------------------------
function IsWindowsVista: boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
  GetVersionEx(@VerInfo);
  Result := VerInfo.dwMajorVersion >= 6;
end;
//------------------------------------------------------------------------------
function IsWin64: boolean;
var
  IsWow64Process: function(Handle: THandle; var Res: boolean): boolean; stdcall;
  res: boolean;
begin
  res := False;
  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
  if assigned(IsWow64Process) then IsWow64Process(GetCurrentProcess, res);
  Result := res;
end;
//------------------------------------------------------------------------------
function GetFont: string;
begin
  Result := 'tahoma';
  try
    if IsWindowsVista then Result := 'segoe ui';
  except
  end;
end;
//------------------------------------------------------------------------------
function GetContentFont: string;
begin
  Result := 'verdana';
  try
    if IsWindowsVista then Result := 'calibri';
  except
  end;
end;
//------------------------------------------------------------------------------
function GetFontSize: integer;
begin
  Result := 8;
  try
    if IsWindowsVista then Result := 9;
  except
  end;
end;
//------------------------------------------------------------------------------
function GetContentFontSize: integer;
begin
  Result := 8;
  try
    if IsWindowsVista then Result := 10;
  except
  end;
end;
//------------------------------------------------------------------------------
function CreateAFont(Name: string; size: integer): HFont;
begin
  Result := CreateFont(size, 0, 0, 0, 0, 0, 0, 0, DEFAULT_CHARSET, 0,
    0, PROOF_QUALITY, 0, PChar(Name));
end;
//------------------------------------------------------------------------------
function cut(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(ch), AnsiLowerCase(itext));
  if ipos > 0 then
    Result := copy(itext, 1, ipos - 1)
  else
    Result := itext;
end;
//------------------------------------------------------------------------------
function cutafter(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(ch), AnsiLowerCase(itext));
  if ipos > 0 then
    Result := copy(itext, ipos + length(ch), length(itext))
  else
    Result := '';
end;
//------------------------------------------------------------------------------
procedure split(itext, ch: string; var str1, str2: string);
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(ch), AnsiLowerCase(itext));
  if ipos > 0 then
  begin
    str1 := copy(itext, 1, ipos - 1);
    str2 := copy(itext, ipos + length(ch), length(itext));
  end
  else
  begin
    str1 := itext;
    str2 := '';
  end;
end;
//------------------------------------------------------------------------------
procedure split_cmd(incmd: string; var cmd, params: string);
var
  ipos: integer;
begin
  if cmd[1] = '"' then
  begin

    ipos := posex('"', incmd, 2);
    if ipos > 0 then
    begin
      cmd := copy(incmd, 2, ipos - 2);
      params := copy(incmd, ipos + 1, length(incmd));
    end
    else
    begin
      cmd := incmd;
      params := '';
    end;

  end
  else
  begin

    ipos := pos(' ', incmd);
    if ipos > 0 then
    begin
      cmd := copy(incmd, 1, ipos - 1);
      params := copy(incmd, ipos + 1, length(incmd));
    end
    else
    begin
      cmd := incmd;
      params := '';
    end;

  end;
end;
//------------------------------------------------------------------------------
function ReplaceEx(strSrc, strWhat, strWith: string): string;
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(strWhat), AnsiLowerCase(strSrc));
  while ipos > 0 do
  begin
    strSrc := copy(strSrc, 1, ipos - 1) + strWith + copy(strSrc, ipos + length(strWhat), length(strSrc));
    ipos := pos(AnsiLowerCase(strWhat), AnsiLowerCase(strSrc));
  end;
  Result := strSrc;
end;
//------------------------------------------------------------------------------
function fetch(var itext: string; delim: string; adelete: boolean = False): string;
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(delim), AnsiLowerCase(itext));
  if ipos > 0 then
  begin
    Result := system.copy(itext, 1, ipos - 1);
    if adelete then
      system.Delete(itext, 1, ipos - 1 + length(delim));
  end
  else
  begin
    Result := itext;
    itext := '';
  end;
end;
//------------------------------------------------------------------------------
function FetchValue(itext: string; Value, delim: string): string;
var
  ipos, ipos2: integer;
begin
  ipos := pos(AnsiLowerCase(Value), AnsiLowerCase(itext));
  if ipos > 0 then
  begin
    ipos2 := posex(delim, itext, ipos + length(Value));
    Result := system.copy(itext, ipos + length(Value), ipos2 - ipos - length(Value));
  end
  else
    Result := '';
end;
//------------------------------------------------------------------------------
function PosEx(Value, atext: string; startpos: integer): integer;
begin
  Result := startpos;
  if Value = '' then exit;

  while Result <= length(atext) do
  begin
    if AnsiLowerCase(atext[Result]) = AnsiLowerCase(Value[1]) then
      if AnsiLowerCase(copy(atext, Result, length(Value))) = AnsiLowerCase(Value) then
        exit;
    Inc(Result);
  end;
end;
//------------------------------------------------------------------------------
function cuttolast(itext, ch: string): string;
var
  i, len: integer;
begin
  Result := '';
  if itext = '' then
    exit;

  i := length(itext);
  len := length(ch);
  while i > 0 do
  begin
    if AnsiLowerCase(copy(itext, i, len)) = AnsiLowerCase(ch) then
    begin
      Result := copy(itext, 1, i - 1);
      exit;
    end;
    Dec(i);
  end;
  Result := itext;
end;
//------------------------------------------------------------------------------
function cutafterlast(itext, ch: string): string;
var
  i, ilen, len: integer;
begin
  Result := '';
  if itext = '' then
    exit;

  ilen := length(itext);
  i := ilen;
  len := length(ch);
  while i > 0 do
  begin
    if AnsiLowerCase(copy(itext, i, len)) = AnsiLowerCase(ch) then
    begin
      Result := copy(itext, i + len, ilen);
      exit;
    end;
    Dec(i);
  end;
  Result := itext;
end;
//------------------------------------------------------------------------------
function IsValidItemString(str: string): boolean;
var
  classname: string;
begin
  classname := FetchValue(str, 'class="', '";');
  result := (classname = 'shortcut') or (classname = 'separator') or (classname = 'plugin') or (classname = 'stack');
end;
//------------------------------------------------------------------------------
function IsValidShortcutString(str: string): boolean;
begin
  result := FetchValue(str, 'class="', '";') = 'shortcut';
end;
//------------------------------------------------------------------------------
function StringToRect(str: string): Windows.Trect;
begin
  Result := rect(0, 0, 0, 0);
  try
    Result.left := StrToInt(trim(fetch(str, ',', True)));
  except
  end;
  try
    Result.top := StrToInt(trim(fetch(str, ',', True)));
  except
  end;
  try
    Result.right := StrToInt(trim(fetch(str, ',', True)));
  except
  end;
  try
    Result.bottom := StrToInt(trim(fetch(str, ')')));
  except
  end;
end;
//------------------------------------------------------------------------------
function RectToString(r: Windows.Trect): string;
begin
  Result := IntToStr(r.left) + ',' + IntToStr(r.top) + ',' + IntToStr(r.right) +
    ',' + IntToStr(r.bottom);
end;
//------------------------------------------------------------------------------
function StringToSize(str: string): Windows.TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  try
    Result.cx := StrToInt(trim(cut(str, ',')));
    Result.cy := StrToInt(trim(cutafter(str, ',')));
  except
  end;
end;
//------------------------------------------------------------------------------
function SizeToString(r: Windows.TSize): string;
begin
  Result := IntToStr(r.cx) + ',' + IntToStr(r.cy);
end;
//------------------------------------------------------------------------------
function StringToPoint(str: string): Windows.Tpoint;
begin
  Result := point(0, 0);
  try
    Result.x := StrToInt(trim(cut(str, ',')));
    Result.y := StrToInt(trim(cutafter(str, ',')));
  except
  end;
end;
//------------------------------------------------------------------------------
function SetRange(value, min, max: integer): integer;
begin
  if value < min then value := min;
  if value > max then value := max;
  result := value;
end;
//------------------------------------------------------------------------------
function IsDriveIdent(ident: string): boolean;
begin
  Result := False;
  if (length(ident) < 2) or (length(ident) > 3) then exit;
  Result := (Ord(ident[1]) > 64) and (Ord(ident[1]) < 117);
  if length(ident) = 2 then Result := Result and (ident[2] = ':');
  if length(ident) = 3 then Result := Result and (ident[2] = ':') and (ident[3] = '\');
end;
//------------------------------------------------------------------------------
procedure searchfiles(path, mask: string; list: TStrings);
var
  fhandle: HANDLE;
  f: TWin32FindData;
begin
  list.Clear;
  path := IncludeTrailingPathDelimiter(path);
  fhandle := FindFirstFile(PChar(path + mask), f);
  if fhandle = INVALID_HANDLE_VALUE then exit;
  if (f.dwFileAttributes and $18) = 0 then list.addobject(f.cFileName, tobject(0));
  while FindNextFile(fhandle, f) do
    if (f.dwFileAttributes and $18) = 0 then list.addobject(f.cFileName, tobject(0));
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
procedure searchfolders(path: string; list: TStrings);
var
  fhandle: THandle;
  filename: string;
  f: TWin32FindData;
begin
  list.Clear;
  path := IncludeTrailingPathDelimiter(path);
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') then
      list.addobject(filename, tobject(0));
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') then
        list.addobject(filename, tobject(0));
    end;
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
procedure searchfilesrecurse(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
var
  fhandle: THandle;
  filename: string;
  f: TWin32FindData;
begin
  if level = 0 then list.Clear;
  path := IncludeTrailingPathDelimiter(path);

  // folders //
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') and (level < maxlevel) then
      searchfilesrecurse(path + filename, mask, list, level + 1);
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') and (level < maxlevel) then
        searchfilesrecurse(path + filename, mask, list, level + 1, maxlevel);
    end;
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);

  // files //
  fhandle := FindFirstFile(PChar(path + mask), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then list.addobject(path + f.cFileName, tobject(0));
    while FindNextFile(fhandle, f) do
      if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then list.addobject(path + f.cFileName, tobject(0));
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
function ReadIniString(IniFile, IniSection, KeyName, Default: string): string;
var
  buf: array [0..1023] of char;
begin
  GetPrivateProfileString(pchar(IniSection), pchar(KeyName), pchar(Default), pchar(@buf), 1024, pchar(IniFile));
  result:= strpas(pchar(@buf));
end;
//------------------------------------------------------------------------------
function ReadIniInteger(IniFile, IniSection, KeyName: string; Default: integer): integer;
var
  buf: array [0..15] of char;
begin
  result:= Default;
  GetPrivateProfileString(pchar(IniSection), pchar(KeyName), pchar(inttostr(Default)), pchar(@buf), 16, pchar(IniFile));
  try result:= strtoint(strpas(pchar(@buf)));
  except end;
end;
//------------------------------------------------------------------------------
function CheckRunAsAdmin(filename: string): boolean;
var
  reg: Treginifile;
begin
  reg := Treginifile.Create;
  reg.RootKey := HKEY_current_user;
  Result := AnsiUpperCase(reg.ReadString(
    'Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers', AnsiLowerCase(filename), '')) = 'RUNASADMIN';
  reg.Free;
end;
//----------------------------------------------------------------------
procedure SetRunAsAdmin(filename: string; enable: boolean);
var
  reg: Treginifile;
begin
  reg := Treginifile.Create;
  reg.RootKey := HKEY_current_user;
  reg.lazywrite := False;
  reg.DeleteKey('Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers', AnsiLowerCase(filename));
  if enable then reg.WriteString('Software\Microsoft\Windows NT\CurrentVersion\AppCompatFlags\Layers',
      AnsiLowerCase(filename), 'RUNASADMIN');
  reg.Free;
end;
//------------------------------------------------------------------------------
function CheckAutoRun: boolean;
var
  reg: Treginifile;
begin
  reg := Treginifile.Create;
  reg.RootKey := HKEY_current_user;
  result := (reg.ReadString('Software\Microsoft\Windows\CurrentVersion\Run', application.title, '') = ParamStr(0));
  reg.Free;
end;
//----------------------------------------------------------------------
procedure SetAutoRun(enable: boolean);
var
  reg: Treginifile;
begin
  reg := Treginifile.Create;
  reg.RootKey := HKEY_current_user;
  reg.lazywrite := False;
  if reg.ReadString('Software\Microsoft\Windows\CurrentVersion\Run', application.title, '') <> '' then
    reg.DeleteKey('Software\Microsoft\Windows\CurrentVersion\Run', application.title);
  if enable then reg.WriteString('Software\Microsoft\Windows\CurrentVersion\Run', application.title, ParamStr(0));
  reg.Free;
end;
//----------------------------------------------------------------------
function GetWinVersion: string;
var
  VersionInfo: Windows.TOSVersionInfo;
begin
  VersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if Windows.GetVersionEx(VersionInfo) then
  begin
    with VersionInfo do
    begin
      case dwPlatformId of
        VER_PLATFORM_WIN32s: Result := 'Win32s';
        VER_PLATFORM_WIN32_WINDOWS: Result := 'Windows 95';
        VER_PLATFORM_WIN32_NT: Result := 'Windows NT';
      end;
      Result := Result + ' Version ' + IntToStr(dwMajorVersion) + '.' +
        IntToStr(dwMinorVersion) + ' (Build ' + IntToStr(dwBuildNumber) +
        ': ' + szCSDVersion + ')';
    end;
  end
  else
    Result := '';
end;
//----------------------------------------------------------------------
procedure ShutDown(mode: integer);
begin
  if SetPrivilege('SeShutdownPrivilege') then ExitWindowsEx(mode, 0);
end;
//----------------------------------------------------------------------
function SetPrivilege(Name: string): boolean;
var
  hToken: cardinal;
  tkp, tkpo: Windows.TTokenPrivileges;
  rl: dword;
begin
  Result := False;
  rl := 0;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then exit;
  if not Windows.LookupPrivilegeValue(nil, PChar(Name), tkp.Privileges[0].Luid) then exit;
  tkp.PrivilegeCount := 1;
  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
  Windows.AdjustTokenPrivileges(hToken, False, tkp, sizeof(TTokenPrivileges), tkpo, rl);
  Result := GetLastError() = 0;
end;
//----------------------------------------------------------------------
function getuser: string;
var
  len: uint;
  charr: array [0..255] of char;
begin
  len := 255;
  if GetUserName(@charr[0], len) then
    Result := strpas(@charr[0])
  else
    Result := '';
end;
//----------------------------------------------------------------------
procedure GetFileVersion(filename: string; var maj, min, Release, build: integer);
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  Tmp: DWORD;
begin
  maj := 0;
  min := 0;
  Release := 0;
  build := 0;

  filename := UnzipPath(filename);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Tmp);
  if InfoSize <> 0 then
  begin
    GetMem(Info, InfoSize);
    try
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      maj := FileInfo.dwFileVersionMS shr 16;
      min := FileInfo.dwFileVersionMS and $FFFF;
      Release := FileInfo.dwFileVersionLS shr 16;
      build := FileInfo.dwFileVersionLS and $FFFF;
    finally
      FreeMem(Info, FileInfoSize);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure AllowSetForeground(hWnd: HWND);
var
  AllowSetForegroundWindow: function(dwProcess: dword): bool; stdcall;
  dwProcess: dword;
begin
  AllowSetForegroundWindow := LinkAPI('USER32.DLL', 'AllowSetForegroundWindow');
  if assigned(AllowSetForegroundWindow) then
  begin
    dwProcess := 0;
    GetWindowThreadProcessId(hWnd, @dwProcess);
    AllowSetForegroundWindow(dwProcess);
  end;
end;
//------------------------------------------------------------------------------
function GetEnvVar(VarName: string): string;
var
  i: integer;
begin
  Result := '';
  try
    i := Windows.GetEnvironmentVariable(PChar(VarName), nil, 0);
    if i > 0 then
    begin
      SetLength(Result, i);
      Windows.GetEnvironmentVariable(PChar(VarName), PChar(Result), i);
    end;
  except
  end;
end;
//------------------------------------------------------------------------------
function FindFile(filename: string): string;
var
  PathVar, ExtVar, TempExtVar, Path, Ext: string;
  HaveExt: boolean;
begin
  Result := filename;
  if fileexists(filename) then
    exit;

  // search evironment vars //
  PathVar := GetEnvVar('path');
  HaveExt := ExtractFileExt(filename) <> '';
  if not HaveExt then
    ExtVar := AnsiLowerCase(GetEnvVar('pathext'));
  while PathVar <> '' do
  begin
    Path := IncludeTrailingPathDelimiter(fetch(PathVar, ';', True));
    if HaveExt then
    begin
      Result := Path + filename;
      if fileexists(Result) then
        exit;
    end
    else
    begin
      TempExtVar := ExtVar;
      while TempExtVar <> '' do
      begin
        Ext := fetch(TempExtVar, ';', True);
        Result := Path + filename + Ext;
        if fileexists(Result) then
          exit;
      end;
    end;
  end;
  Result := filename;
end;
//------------------------------------------------------------------------------
function FindFilePF(filename: string): string;
var
  list: TStrings;
begin
  Result := filename;
  if fileexists(filename) then exit;
  list := TStringList.Create;
  searchfilesrecurse(UnzipPath('%pf%'), filename + '.exe', list, 1, 3, 1);
  searchfilesrecurse(UnzipPath('%pf% (x86)'), filename + '.exe', list, 1, 3, 1);
  if list.Count > 0 then Result := list[0];
  list.Free;
end;
//------------------------------------------------------------------------------
function UnzipPath(path: string): string;
var
  pp: string;
begin
  Result := path;
  if trim(path) = '' then exit;

  if length(Result) > 3 then
    if (Result[2] = ':') and (Result[3] = '\') then
      if fileexists(Result) or directoryexists(Result) then exit;

  pp := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  if fileexists(pp + '\' + Result) then
  begin
    Result := pp + '\' + Result;
    exit;
  end;

  // path vars //
  Result := ReplaceEx(Result, '%pp%', pp);
  Result := ReplaceEx(Result, '%windir%', GetWinDir);
  Result := ReplaceEx(Result, '%systemroot%', getwindir);
  Result := ReplaceEx(Result, '%sysdir%', getsystemdir);
  Result := ReplaceEx(Result, '%doc%', getsystempath('personal'));
  Result := ReplaceEx(Result, '%desktop%', getsystempath('desktop'));
  Result := ReplaceEx(Result, '%startmenu%', getsystempath('start menu'));
  Result := ReplaceEx(Result, '%commonstartmenu%', getsystempath('common start menu'));
  Result := ReplaceEx(Result, '%pfx86%', getwindir[1] + ':\Program Files (x86)');
  Result := ReplaceEx(Result, '%pf%', getwindir[1] + ':\Program Files');
  Result := ReplaceEx(Result, '%programfiles%', getwindir[1] + ':\Program Files');

  // non-path vars //

  Result := ReplaceEx(Result, '%date%', formatdatetime('dddddd', now));
  Result := ReplaceEx(Result, '%time%', formatdatetime('tt', now));
  Result := ReplaceEx(Result, '%win_version%', GetWinVersion);
  Result := ReplaceEx(Result, '%user%', GetUser);
  Result := ReplaceEx(Result, '%crlf%', #10#13);
end;
//------------------------------------------------------------------------------
function ZipPath(path: string): string;
var
  windir: string;
begin
  windir := getwindir;
  path := ReplaceEx(path, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))), '');
  path := ReplaceEx(path, getsystemdir, '%sysdir%');
  path := ReplaceEx(path, windir, '%windir%');
  path := ReplaceEx(path, getsystempath('personal'), '%doc%');
  path := ReplaceEx(path, getsystempath('desktop'), '%desktop%');
  path := ReplaceEx(path, getsystempath('start menu'), '%startmenu%');
  path := ReplaceEx(path, getsystempath('common start menu'), '%commonstartmenu%');
  path := ReplaceEx(path, windir[1] + ':\program files (x86)', '%pfx86%');
  path := ReplaceEx(path, windir[1] + ':\program files', '%pf%');
  Result := path;
end;
//----------------------------------------------------------------------
function GetSystemDir: string;
var
  SysDir: array [0..MAX_PATH - 1] of char;
begin
  SetString(Result, SysDir, GetSystemDirectory(SysDir, MAX_PATH));
  Result := ExcludeTrailingPathDelimiter(Result);
end;
//----------------------------------------------------------------------
function GetWinDir: string;
var
  WinDir: array [0..MAX_PATH - 1] of char;
begin
  SetString(Result, WinDir, GetWindowsDirectory(WinDir, MAX_PATH));
  Result := ExcludeTrailingPathDelimiter(Result);
end;
//------------------------------------------------------------------------------
function GetSystemPath(path: string): string;
var
  reg: TRegIniFile;
begin
  reg := TRegIniFile.Create;
  if pos('common', path) > 0 then reg.RootKey := hkey_local_machine else reg.RootKey := hkey_current_user;
  Result := ExcludeTrailingPathDelimiter(reg.ReadString('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', path, ''));
  reg.Free;
end;
//------------------------------------------------------------------------------
procedure setdisplaymode(x: integer = 800; y: integer = 600; bits: integer = 16; freq: integer = 60);
var
  devmode: Windows.TDeviceMode;
begin
  devmode.dmSize := SizeOf(devmode);
  devmode.dmBitsPerPel := bits;
  devmode.dmPelsWidth := x;
  devmode.dmPelsHeight := y;
  devmode.dmDisplayFrequency := freq;
  devmode.dmFields := DM_BITSPERPEL + DM_PELSWIDTH + DM_PELSHEIGHT + DM_DISPLAYFREQUENCY;
  Windows.ChangeDisplaySettings(DevMode, CDS_UPDATEREGISTRY);
  SendMessage(HWND_BROADCAST, WM_DISPLAYCHANGE, SPI_SETNONCLIENTMETRICS, 0);
end;
//------------------------------------------------------------------------------
procedure ResolveShortcut(wnd: HWND; var ShortcutPath: string; out params, dir, icon: string);
var
  obj: IUnknown;
  isl: IShellLink;
  ipf: IPersistFile;
  fda: Windows.TWin32FindDataA;
  s: string;
  i: integer;
begin
  obj := CreateComObject(CLSID_ShellLink);
  isl := obj as IShellLink;
  ipf := obj as IPersistFile;
  if S_OK <> ipf.Load(PWChar(WideString(ShortcutPath)), STGM_READ) then exit;
  if S_OK = isl.Resolve(wnd, SLR_NO_UI + SLR_NOUPDATE) then
  begin
    SetLength(s, MAX_PATH);
    isl.GetPath(PChar(s), length(s), fda, SLGP_UNCPRIORITY);
    ShortcutPath := PChar(s);

    SetLength(s, MAX_PATH);
    isl.GetArguments(PChar(s), length(s));
    params := PChar(s);

    SetLength(s, MAX_PATH);
    isl.GetWorkingDirectory(PChar(s), length(s));
    dir := PChar(s);

    SetLength(s, MAX_PATH);
    isl.GetIconLocation(PChar(s), length(s), i);
    icon := PChar(s);
  end;
end;
//--------------------------------------------------------------------------------------------------
function BrowseFolder(hWnd: THandle; title, default: string): string;
var
  lpItemID: PItemIDList;
  BrowseInfo: Windows.TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  path: array [0..MAX_PATH] of char;
begin
  zeroMemory(@BrowseInfo, sizeof(TBrowseInfo));
  BrowseInfo.hwndOwner := hWnd;
  BrowseInfo.pszDisplayName := @DisplayName;
  BrowseInfo.lpszTitle := PChar(title);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
  lpItemID := SHBrowseForFolder(@BrowseInfo);
  if lpItemId <> nil then
  begin
    SHGetPathFromIDList(lpItemID, path);
    result := strpas(path);
    result := IncludeTrailingPathDelimiter(Result);
    GlobalFreePtr(lpItemID);
  end
  else
    Result := default;
end;
//------------------------------------------------------------------------------
procedure FreeAndNil(var Obj);
var
  p: TObject;
begin
  p := TObject(Obj);
  TObject(Obj) := nil;
  p.Free;
end;
//------------------------------------------------------------------------------
function LinkAPI(const module, functionname: string): pointer;
var
  hLib: cardinal;
begin
  hLib := GetModuleHandle(PChar(module));
  if hLib = 0 then hLib := LoadLibrary(PChar(module));
  if hLib <> 0 then Result := GetProcAddress(hLib, PChar(functionname))
  else Result := nil;
end;
//------------------------------------------------------------------------------
function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: boolean): boolean;
var
  dummy: function(Hibernate, ForceCritical, DisableWakeEvent: bool): bool;
begin
  if not SetPrivilege('SeShutdownPrivilege') then exit;
  @dummy := linkAPI('powrprof.dll', 'SetSuspendState');
  if Assigned(dummy) then Result := dummy(Hibernate, ForceCritical, DisableWakeEvent)
  else Result := False;
end;
//------------------------------------------------------------------------------
procedure SetClipboard(Text: string);
var
  Data: cardinal;
  dataPtr: pointer;
  pch: PChar;
begin
  if not OpenClipboard(application.mainform.handle) then
  begin
    ShowMessage('Cannot open clipboard');
    exit;
  end;
  EmptyClipboard;
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, length(Text) + 1);
  dataPtr := GlobalLock(Data);
  pch := PChar(Text);
  move(pch^, dataPtr^, length(Text) + 1);
  SetClipboardData(CF_TEXT, Data);
  GlobalUnlock(Data);
  CloseClipboard;
end;
//------------------------------------------------------------------------------
function GetClipboard: string;
var
  Data: cardinal;
  dataptr: pointer;
  pch: PChar;
begin
  Result := '';
  if not OpenClipboard(application.mainform.handle) then
  begin
    ShowMessage('Cannot open clipboard');
    exit;
  end;
  try
    Data := GetClipboardData(CF_TEXT);
    if Data > 32 then
    begin
      dataptr := GlobalLock(Data);
      if dataptr <> nil then
      begin
        GetMem(pch, GlobalSize(Data));
        move(dataPtr^, pch^, GlobalSize(Data));
        Result := strpas(pch);
        FreeMem(pch, GlobalSize(Data));
      end;
      GlobalUnlock(Data);
    end
    else
      Result := '';
  except
  end;
  CloseClipboard;
end;
//------------------------------------------------------------------------------
function ColorToString(Color: uint): string;
begin
  FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;
//------------------------------------------------------------------------------
function StringToColor(const str: string): uint;
begin
  Result := StrToInt(str);
end;
//------------------------------------------------------------------------------
function confirm(handle: cardinal; Text: string = ''): boolean;
begin
  if Text = '' then Text := 'Confirm action';
  Result := messagebox(handle, PChar(Text), 'Confirm', mb_yesno or mb_iconexclamation or mb_defbutton2) = idYes;
end;
//------------------------------------------------------------------------------
function FindWinamp: cardinal;
begin
  Result := findwindow('BaseWindow', nil);
  if not IsWindow(Result) then
  begin
    Result := findwindow('Winamp v1.x', nil);
    if not IsWindow(Result) then Result := 0;
  end;
end;
//------------------------------------------------------------------------------
function LaunchWinamp(sw: integer = sw_shownormal): boolean;
var
  reg: TRegistry;
  wdir: string;
begin
  Result := False;

  try
    wdir := IncludeTrailingPathDelimiter(ExtractFileDrive(GetWinDir)) + 'program files\winamp\';

    if fileexists(wdir + 'winamp.exe') then
    begin
      shellexecute(0, nil, PChar(wdir + 'winamp.exe'), nil, PChar(wdir), sw);
      Result := True;
      exit;
    end;

    reg := TRegistry.Create;
    reg.RootKey := hkey_current_user;
    wdir := IncludeTrailingPathDelimiter(reg.ReadString('Software\Winamp'));
    if fileexists(wdir + 'winamp.exe') then
    begin
      shellexecute(0, nil, PChar(wdir + 'winamp.exe'), nil, PChar(wdir), sw);
      Result := True;
    end;
    reg.Free;
    reg := nil;
  except
  end;
end;
//------------------------------------------------------------------------------
function wacmd(cmd: cardinal): boolean;
var
  wahwnd: cardinal;
begin
  Result := False;
  wahwnd := FindWinamp;
  if wahwnd > 0 then
  begin
    sendmessage(wahwnd, wm_command, cmd, 0);
    Result := True;
  end;
end;
//------------------------------------------------------------------------------
procedure AddLog(LogString: string);
var
  LogFileName: string;
  faccess: dword;
  PStr: PChar;
  LengthLogString: integer;
  fs: TFileStream;
begin
  try
    // prepare log string
    LogString := formatdatetime('yyMMdd-hhnnss', now) + '  ' + LogString + #13#10;
    LengthLogString := Length(LogString);
    PStr := StrAlloc(LengthLogString + 1);
    StrPCopy(PStr, LogString);

    // open log
    LogFileName := UnzipPath('%pp%\terry.log');
    if FileExists(LogFileName) then faccess := fmOpenReadWrite else faccess := fmCreate;
    fs := TFileStream.Create(LogFileName, faccess);
    fs.Position := fs.Size;

    // write string
    fs.Write(PStr^, LengthLogString);
    StrDispose(PStr);

    // truncate file if needed
    TruncLog(fs);

    fs.Free;
  except
  end;
end;
//------------------------------------------------------------------------------
procedure TruncLog(fs: TFileStream);
const
  LOG_SIZE_MAX = 1024 * 30; // 30 KB
var
  buf: char;
  TruncBy: integer;
  ms: TMemoryStream;
begin
  try
    // how many bytes to delete from the beginning of the stream
    TruncBy := fs.Size - LOG_SIZE_MAX;

    if TruncBy > 0 then
    begin
      // skip TruncBy bytes
      fs.Position := TruncBy;

      // skip bytes until end-of-line found
      fs.Read(buf, 1);
      inc(TruncBy);
      fs.Position := TruncBy;
      while (TruncBy < fs.Size) and (buf <> #10) and (buf <> #13) do
      begin
        fs.Read(buf, 1);
        inc(TruncBy);
        fs.Position := TruncBy;
      end;
      inc(TruncBy);
      fs.Position := TruncBy;
      TruncBy := fs.Size - TruncBy;

      // copy data to buffer stream
      ms := TMemoryStream.Create;
      ms.Size := TruncBy;
      ms.Position := 0;
      ms.CopyFrom(fs, TruncBy);
      ms.Position := 0;

      // copy buffer back to file
      fs.Size := TruncBy;
      fs.Position := 0;
      fs.CopyFrom(ms, TruncBy);

      ms.free;
    end;
  except
  end;
end;
//------------------------------------------------------------------------------
procedure bsm(msg: uint; wparam: WPARAM; lparam: LPARAM);
var
  i: integer;
begin
  i := BSM_APPLICATIONS;
  BroadcastSystemMessage(BSF_IGNORECURRENTTASK + BSF_FORCEIFHUNG + BSF_POSTMESSAGE, @i, msg, wparam, lparam);
end;
//------------------------------------------------------------------------------
end.

