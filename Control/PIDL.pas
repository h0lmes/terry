unit PIDL;

interface

uses Windows, ShellAPI, ShlObj, SysUtils, Forms, ActiveX;

type
  PPItemIDList = ^PItemIDList;

function PIDL_GetSizeReal(pidl: PITEMIDLIST): integer;
function PIDL_FromDrop(p: Pointer; var size: uint): PItemIDList;
function PIDL_ToString(p: Pointer): string; overload;
function PIDL_ToString(p: Pointer; size: uint): string; overload;
function PIDL_FromString(Data: string): PItemIDList;
function PIDL_GetSize(pidl: PITEMIDLIST): integer;
function PIDL_Create(size: uint): PItemIDList;
function PIDL_Copy(pidl: PItemIDList): PItemIDList;
function PIDL_Next(pidl: PItemIDList): PItemIDList;
function PIDL_GetDisplayName(folder: IShellFolder; pidl: PItemIDList; dwFlags: DWORD; pszName: PChar; cchMax: uint): boolean;
procedure PIDL_GetRelative(var pidlFQ, ppidlRoot, ppidlItem: PItemIDList);
function PIDL_GetAbsolute(var pidlRoot, pidlItem: PItemIDList): PItemIDList;
function PIDL_GetFromPath(pszFile: PChar): PItemIDList;
function PIDL_GetFileFolder(pidl: PItemIDList; var folder: IShellFolder): boolean;
procedure PIDL_Free(pidl: PItemIDList);

var
  ShellMalloc: IMalloc;

implementation
//------------------------------------------------------------------------------
procedure err(where: string; e: Exception);
begin
  if assigned(e) then messagebox(application.mainform.handle, PChar(e.Message), PChar(where), mb_iconexclamation)
  else messagebox(application.mainform.handle, 'Error', PChar(where), mb_iconexclamation);
end;
//------------------------------------------------------------------------------
function PIDL_GetSizeReal(pidl: PITEMIDLIST): integer;
begin
  Result := 0;
  if pidl <> nil then
  begin
    while pidl^.mkid.cb <> 0 do
    begin
      Inc(Result, pidl^.mkid.cb);
      {$IFDEF WIN64}
      Inc(int64(pidl), pidl^.mkid.cb);
      {$ELSE}
      Inc(longint(pidl), pidl^.mkid.cb);
      {$ENDIF}
    end;
  end;
  if Result > 0 then inc(Result, sizeof(Word));
end;
//------------------------------------------------------------------------------
// creates a PIDL from a pointer to memory data from hdrop
function PIDL_FromDrop(p: Pointer; var size: uint): PItemIDList;
var
  iPIDL, nPIDLs, PIDL_size, temp_size: uint;
  U, temp: array [0..1023] of byte;
begin
  Result := nil;
  if size > 1024 then
  begin
    err('Identifier length exceeded', nil);
    exit;
  end;
  // make local copy of memory //
  CopyMemory(pointer(@U), p, size);
  FillChar(temp, 1024, 0);
  // num of PIDLs = first CIDA.aoffset minus sizeof(CIDA.cidl) divide by sizeof(CIDA.aoffset)
  nPIDLs := (uint(U[4]) - 4) div 4;
  // copy data, except terminating double NULLs of each PIDL structure //
  temp_size := 0;
  iPIDL := 0;
  while iPIDL < nPIDLs do
  begin
    PIDL_size := PIDL_GetSizeReal(@U[U[4 + iPIDL * 4]]);
    if PIDL_size > 0 then
    begin
      CopyMemory(@temp[temp_size], @U[U[4 + iPIDL * 4]], PIDL_size);
      Inc(temp_size, PIDL_size);
    end;
    Inc(iPIDL);
  end;
  // add terminating double NULL //
  Inc(temp_size, 2);
  size := temp_size;
  // create PIDL //
  Result := ShellMalloc.Alloc(temp_size);
  if Result <> nil then CopyMemory(Result, @temp, temp_size);
end;
//------------------------------------------------------------------------------
function PIDL_ToString(p: Pointer): string;
begin
  Result := PIDL_ToString(p, PIDL_GetSize(p));
end;
//------------------------------------------------------------------------------
function PIDL_ToString(p: Pointer; size: uint): string;
var
  i: uint;
begin
  Result := '';
  if p <> nil then
  begin
    i := 0;
    while i < size do
    begin
      Result := Result + inttohex(byte(pbyte(PChar(p) + i)^), 2);
      Inc(i);
    end;
  end;
end;
//------------------------------------------------------------------------------
// converts string of type "14001F50E04FD020EA3A6910A2D808002B30309D0000" into a PIDL
// each 2 digits represent one byte of PIDL in hex
function PIDL_FromString(Data: string): PItemIDList;
var
  i, size: word;
begin
  size := length(Data) div 2;
  Result := ShellMalloc.Alloc(size);
  i := 0;
  while i < size do
  begin
    byte(pbyte(cardinal(Result) + i)^) := byte(StrToInt('$' + copy(Data, 1 + i * 2, 2)));
    Inc(i);
  end;
end;
//------------------------------------------------------------------------------
// returns the total number of bytes in an ITEMIDLIST
function PIDL_GetSize(pidl: PITEMIDLIST): integer;
var
  p: PChar;
begin
  Result := 0;
  p := PChar(pidl);
  while p <> nil do
  begin
    if PItemIDList(p + Result)^.mkid.cb = 0 then
    begin
      Inc( Result, sizeof(Word) ); // size of terminator;
      break;
    end;
    Inc( Result, PItemIDList(p + Result)^.mkid.cb );
  end;
end;
//------------------------------------------------------------------------------
function PIDL_Create(size: uint): PItemIDList;
begin
  Result := ShellMalloc.Alloc(size);
  if assigned(Result) then FillChar(Result^, size, #0);
end;
//------------------------------------------------------------------------------
function PIDL_Copy(pidl: PItemIDList): PItemIDList;
var
  size: Integer;
begin
  Result := nil;
  if not Assigned(pidl) then Exit;
  size := PIDL_GetSize(pidl);
  Result := ShellMalloc.Alloc(size);
  if Assigned(Result) then CopyMemory(Result, pidl, size);
end;
//------------------------------------------------------------------------------
function PIDL_Next(pidl: PItemIDList): PItemIDList;
begin
  if assigned(pidl) then Result := PItemIDList(uint(pidl) + pidl^.mkid.cb)
  else Result := nil;
end;
//------------------------------------------------------------------------------
function PIDL_GetDisplayName(folder: IShellFolder; pidl: PItemIDList;
  dwFlags: DWORD; pszName: PChar; cchMax: UINT): boolean;
var
  Str: TStrRet;
begin
  if (folder = nil) and Failed(SHGetDesktopFolder(folder)) then
  begin
    Result := False;
    exit;
  end;
  Result := True;
  if folder.GetDisplayNameOf(pidl, dwFlags, Str) = 0 then
  begin
    case Str.uType of
      STRRET_WSTR: WideCharToMultiByte(CP_ACP, 0, str.pOleStr, -1, pszName, cchMax, nil, nil);
      STRRET_OFFSET: lstrcpyn(pszName, PChar(pidl) + str.uOffset, cchMax);
      STRRET_CSTR: lstrcpyn(pszName, str.cStr, cchMax);
      else Result := False;
    end;
  end
  else Result := False;
end;
//------------------------------------------------------------------------------
//  takes a fully qualified pidl and returns the the relative pidl
//  and the root part of that pidl
//  pidlFQ   - Pointer to the fully qualified ITEMIDLIST that needs to be parsed.
//  pidlRoot - Points to the pidl that will contain the root after parsing.
//  pidlItem - Points to the item relative to pidlRoot after parsing.
procedure PIDL_GetRelative(var pidlFQ, ppidlRoot, ppidlItem: PItemIDList);
var
  pidlTemp, pidlNext: PItemIDList;
begin
  if pidlFQ = nil then
  begin
    ppidlRoot := nil;
    ppidlItem := nil;
    exit;
  end;
  ppidlRoot := PIDL_Copy(pidlFQ);
  pidlTemp := ppidlRoot;
  while pidlTemp^.mkid.cb > 0 do
  begin
    pidlNext := PIDL_Next(pidlTemp);
    if pidlNext^.mkid.cb = 0 then
    begin
      ppidlItem := PIDL_Copy(pidlTemp);
      pidlTemp^.mkid.cb := 0;
      pidlTemp^.mkid.abID[0] := 0;
    end;
    pidlTemp := pidlNext;
  end;
end;
//------------------------------------------------------------------------------
function PIDL_GetAbsolute(var pidlRoot, pidlItem: PItemIDList): PItemIDList;
var
  folder: IShellFolder;
  pszName: array [0..255] of char;
begin
  result := nil;
  if not PIDL_GetFileFolder(pidlRoot, folder) then exit;
  if PIDL_GetDisplayName(folder, pidlItem, SHGDN_FORPARSING, pszName, 255) then
  begin
    result := PIDL_GetFromPath(pszName);
  end;
  if result = nil then result := pidlRoot;
end;
//------------------------------------------------------------------------------
// converts filesystem path to PIDL //
function PIDL_GetFromPath(pszFile: PChar): PItemIDList;
var
  desk: IShellFolder;
  path: array [0..MAX_PATH - 1] of TOleChar;
  eaten, attribs: uint;
begin
  Result := nil;
  if not Failed(SHGetDesktopFolder(desk)) then
  begin
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, pszFile, -1, @path, MAX_PATH);
    desk._AddRef;
    if Failed(desk.ParseDisplayName(0, nil, @path, eaten, Result, attribs)) then Result := nil;
  end;
end;
//------------------------------------------------------------------------------
// makes IShellFolder for pidl (that is FQ PIDL folder) //
function PIDL_GetFileFolder(pidl: PItemIDList; var folder: IShellFolder): boolean;
var
  desk: IShellFolder;
begin
  Result := False;
  if Failed(SHGetDesktopFolder(desk)) then exit;
  if not assigned(folder) then
    if Failed(SHGetDesktopFolder(folder)) then exit;
  Result := not Failed(desk.BindToObject(pidl, nil, IID_IShellFolder, pointer(folder)));
end;
//------------------------------------------------------------------------------
procedure PIDL_Free(pidl: PItemIDList);
begin
  if assigned(pidl) then ShellMalloc.Free(pidl);
  pidl := nil;
end;
//------------------------------------------------------------------------------
initialization
  SHGetMalloc(ShellMalloc);
finalization
  ShellMalloc := nil;
end.

