unit toolu;

interface

uses Windows, jwaWindows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, ShellAPI, MMSystem, Registry, ComObj, ShlObj, ActiveX;

function IsWindowsVista: boolean;
function GetFont: string;
function GetContentFont: string;
function GetFontSize: integer;
function GetContentFontSize: integer;
function cut(itext, ch: string): string;
function cutafter(itext, ch: string): string;
procedure split(itext, ch: string; var str1, str2: string);
function ReplaceEx(strSrc, strWhat, strWith: string): string;
function fetch(var itext: string; delim: string; adelete: boolean = False): string;
function FetchValue(itext: string; Value, delim: string): string;
function PosEx(Value, atext: string; startpos: integer): integer;
function cuttolast(itext, ch: string): string;
function cutafterlast(itext, ch: string): string;
procedure searchfiles(path, mask: string; list: TStrings);
procedure searchfolders(path: string; list: TStrings);
procedure searchfilesrecurse(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
function GetWinVersion: string;
function IsDriveIdent(ident: string): boolean;
function GetEnvVar(VarName: string): string;
function FindFile(filename: string): string;
function FindFilePF(filename: string): string;
function UnzipPath(path: string): string;
function ZipPath(path: string): string;
function GetSystemDir: string;
function GetWinDir: string;
function GetSystemPath(path: string): string;
function BrowseFolder(hWnd: THandle; title, default: string): string;
procedure FreeAndNil(var Obj);
procedure SetClipboard(Text: string);
function GetClipboard: string;
function confirm(handle: cardinal; Text: string = ''): boolean;

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
  if assigned(IsWow64Process) then
    IsWow64Process(GetCurrentProcess, res);
  Result := res;
end;
//------------------------------------------------------------------------------
function GetFont: string;
begin
  Result := 'tahoma';
  try
    if IsWindowsVista then
      Result := 'segoe ui';
  except
  end;
end;
//------------------------------------------------------------------------------
function GetContentFont: string;
begin
  Result := 'verdana';
  try
    if IsWindowsVista then
      Result := 'calibri';
  except
  end;
end;
//------------------------------------------------------------------------------
function GetFontSize: integer;
begin
  Result := 8;
  try
    if IsWindowsVista then
      Result := 9;
  except
  end;
end;
//------------------------------------------------------------------------------
function GetContentFontSize: integer;
begin
  Result := 8;
  try
    if IsWindowsVista then
      Result := 10;
  except
  end;
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
function ReplaceEx(strSrc, strWhat, strWith: string): string;
var
  ipos: integer;
begin
  ipos := pos(AnsiLowerCase(strWhat), AnsiLowerCase(strSrc));
  while ipos > 0 do
  begin
    strSrc := copy(strSrc, 1, ipos - 1) + strWith + copy(strSrc,
      ipos + length(strWhat), length(strSrc));
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
  if Value = '' then
    exit;

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
function IsDriveIdent(ident: string): boolean;
begin
  Result := False;
  if (length(ident) < 2) or (length(ident) > 3) then
    exit;
  Result := (Ord(ident[1]) > 64) and (Ord(ident[1]) < 117);
  if length(ident) = 2 then
    Result := Result and (ident[2] = ':');
  if length(ident) = 3 then
    Result := Result and (ident[2] = ':') and (ident[3] = '\');
end;
//------------------------------------------------------------------------------
procedure searchfiles(path, mask: string; list: TStrings);
var
  fhandle: THandle;
  f: TWin32FindData;
begin
  list.Clear;
  path := IncludeTrailingPathDelimiter(path);
  fhandle := FindFirstFile(PChar(path + mask), f);
  if fhandle = DWORD(-1) then exit;
  if (f.dwFileAttributes and $18) = 0 then
    list.addobject(f.cFileName, tobject(0));
  while FindNextFile(fhandle, f) do
    if (f.dwFileAttributes and $18) = 0 then
      list.addobject(f.cFileName, tobject(0));
  if not (fhandle = DWORD(-1)) then
    Windows.FindClose(fhandle);
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
  if fhandle <> DWORD(-1) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
      (filename <> '..') then
      list.addobject(filename, tobject(0));
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
        (filename <> '..') then
        list.addobject(filename, tobject(0));
    end;
  end;
  if not (fhandle = DWORD(-1)) then
    Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
procedure searchfilesrecurse(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
var
  fhandle: THandle;
  filename: string;
  f: TWin32FindData;
begin
  if level = 0 then
    list.Clear;
  path := IncludeTrailingPathDelimiter(path);

  // folders //
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = DWORD(-1)) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
      (filename <> '..') and (level < maxlevel) then
      searchfilesrecurse(path + filename, mask, list, level + 1);
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
        (filename <> '..') and (level < maxlevel) then
        searchfilesrecurse(path + filename, mask, list, level + 1, maxlevel);
    end;
  end;
  if not (fhandle = DWORD(-1)) then
    Windows.FindClose(fhandle);

  // files //
  fhandle := FindFirstFile(PChar(path + mask), f);
  if not (fhandle = DWORD(-1)) then
  begin
    if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then
      list.addobject(path + f.cFileName, tobject(0));
    while FindNextFile(fhandle, f) do
      if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then
        list.addobject(path + f.cFileName, tobject(0));
  end;
  if not (fhandle = DWORD(-1)) then
    Windows.FindClose(fhandle);
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
  if fileexists(filename) then
    exit;
  list := TStringList.Create;
  searchfilesrecurse(UnzipPath('%pf%'), filename + '.exe', list, 0, 2, 1);
  if list.Count > 0 then
    Result := list[0];
  list.Free;
end;
//------------------------------------------------------------------------------
function UnzipPath(path: string): string;
var
  pp: string;
begin
  if trim(path) = '' then
    exit;
  Result := path;
  if length(Result) > 3 then
    if (Result[2] = ':') and (Result[3] = '\') then
      if fileexists(Result) or directoryexists(Result) then
        exit;
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
  Result := ReplaceEx(Result, '%pf%', getwindir[1] + ':\Program Files');
  Result := ReplaceEx(Result, '%programfiles%', getwindir[1] + ':\Program Files');
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
  if pos('common', path) > 0 then
    reg.RootKey := hkey_local_machine
  else
    reg.RootKey := hkey_current_user;
  Result := ExcludeTrailingPathDelimiter(
    reg.ReadString('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
    path, ''));
  reg.Free;
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
    Result := strpas(path);
    if Result[length(Result)] <> '\' then
      Result := Result + '\';
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
  try
    p := TObject(Obj);
    TObject(Obj) := nil;
    p.Free;
    p := nil;
  except
    p := nil;
  end;
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
    ShowMessage('Cannot Open Clipboard');
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
    ShowMessage('Cannot Open Clipboard');
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
function confirm(handle: cardinal; Text: string = ''): boolean;
begin
  if Text = '' then
    Text := 'Подтвердите операцию';
  Result := messagebox(handle, PChar(Text),
    'Подтверждение', mb_yesno or mb_iconexclamation or mb_defbutton2) = idYes;
end;
//------------------------------------------------------------------------------
end.

