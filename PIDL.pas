unit PIDL;

interface

uses Windows, ShellAPI, ShlObj, SysUtils, Forms, ActiveX, notifieru;

type
  PPItemIDList = ^PItemIDList;

function PIDL_CountFromCIDA(ida: PCIDA): longint;
function PIDL_FromCIDA(index: longint; ida: PCIDA; var size: longint): PItemIDList;
function PIDL_GetSize(pidl: PITEMIDLIST): integer;
function PIDL_Create(size: uint): PItemIDList;
function PIDL_Copy(pidl: PItemIDList): PItemIDList;
function PIDL_Next(pidl: PItemIDList): PItemIDList;
function PIDL_GetDisplayName(folder: IShellFolder; pidl: PItemIDList; dwFlags: DWORD; pszName: PChar; cchMax: uint): boolean;
function PIDL_GetDisplayName2(pidl: PItemIDList): string;
procedure PIDL_GetRelative(var pidlFQ, ppidlRoot, ppidlItem: PItemIDList);
function PIDL_GetAbsolute(var pidlRoot, pidlItem: PItemIDList): PItemIDList;
function PIDL_GetFromPath(pszFile: PChar): PItemIDList;
function PIDL_GetFileFolder(pidl: PItemIDList; var folder: IShellFolder): boolean;
procedure PIDL_Free(pidl: PItemIDList);
function IsGUID(str: string): boolean;
function CSIDL_ToInt(csidl: string): integer;

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
// get PIDL count from CIDA structure
function PIDL_CountFromCIDA(ida: PCIDA): longint;
begin
  result := ida.cidl;
end;
//------------------------------------------------------------------------------
// converts CIDA structure to a PIDL
function PIDL_FromCIDA(index: longint; ida: PCIDA; var size: longint): PItemIDList;
var
  buf: array [0..4095] of byte;
  pidl: PItemIDList;
  pidl_size, offset: PtrUInt;
begin
  result := nil;
  FillChar(buf, 4095, 0);
  offset := 0;

  // copy folder //
  pidl := Pointer( PtrUInt(ida) + ida.aoffset[0] );
  if ida.aoffset[0] >= size then exit;
  pidl_size := ILGetSize(pidl) - 2;
  if pidl_size > 0 then
  begin
    CopyMemory(@buf[offset], pidl, pidl_size);
    inc(offset, pidl_size);
  end;

  // copy item //
  pidl := Pointer( PtrUInt(ida) + ida.aoffset[index + 1] );
  if ida.aoffset[index + 1] >= size then exit;
  pidl_size := ILGetSize(pidl);
  if ida.aoffset[index + 1] + pidl_size > size then exit;
  if pidl_size > 0 then
  begin
    CopyMemory(@buf[offset], pidl, pidl_size);
    inc(offset, pidl_size);
  end;

  // create PIDL //
  if offset > 0 then
  begin
    size := offset + 2;
    result := ShellMalloc.Alloc(size);
    if result <> nil then CopyMemory(result, @buf, size);
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
function PIDL_GetDisplayName2(pidl: PItemIDList): string;
var
  pszName: array [0..MAX_PATH - 1] of char;
begin
  result := '';
  if PIDL_GetDisplayName(nil, pidl, SHGDN_FORPARSING, pszName, MAX_PATH) then result := strpas(pszName);
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
function IsGUID(str: string): boolean;
begin
  result := strlcomp(pchar(str), '::{', 3) = 0;
end;
//------------------------------------------------------------------------------
function CSIDL_ToInt(csidl: string): integer;
begin
  result := -1;
  if strlcomp(pchar(csidl), 'CSIDL_', 6) = 0 then
  begin
    if csidl = 'CSIDL_CONTROLS' then result := CSIDL_CONTROLS;
    if csidl = 'CSIDL_PRINTERS' then result := CSIDL_PRINTERS;
    if csidl = 'CSIDL_BITBUCKET' then result := CSIDL_BITBUCKET;
    if csidl = 'CSIDL_DESKTOPDIRECTORY' then result := CSIDL_DESKTOPDIRECTORY;
    if csidl = 'CSIDL_MYMUSIC' then result := CSIDL_MYMUSIC;
    if csidl = 'CSIDL_MYVIDEO' then result := CSIDL_MYVIDEO;
    if csidl = 'CSIDL_DRIVES' then result := CSIDL_DRIVES;
    if csidl = 'CSIDL_NETWORK' then result := CSIDL_NETWORK;
    // not working properly
    //if csidl = 'CSIDL_PERSONAL' then result := CSIDL_PERSONAL;
    //if csidl = 'CSIDL_MYDOCUMENTS' then result := CSIDL_MYDOCUMENTS;
  end;
end;
//------------------------------------------------------------------------------
initialization
  SHGetMalloc(ShellMalloc);
finalization
  ShellMalloc := nil;
end.

