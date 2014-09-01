Unit shcontextu;

interface
uses Windows, ShlObj, SysUtils, Classes, Messages, ShellAPI, PIDL;

type
  PPIDLArray = ^TPIDLArray;
  TPIDLArray = array [0..0] of PItemIDList;

function ShContextMenuI(Handle: THandle; P: TPoint; ShellFolder: IShellFolder; PIDLCount: Integer; var PIDL: PItemIDList; ParentPopupMenu: HMenu): boolean;
// call this one //
function ShContextMenu(Handle: THandle; P: TPoint; FileName: string; ParentPopupMenu: HMenu): boolean; overload;
// or this one //
function ShContextMenu(Handle: THandle; P: TPoint; DirPIDLFQ: PItemIDList; ParentPopupMenu: HMenu): boolean; overload;
function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList; ForParsing: Boolean): string;
procedure GetFileInfo(PIDL: PItemIDList; Large, Open: Boolean; var FileInfo: TSHFileInfo);

implementation
{$R-}
//------------------------------------------------------------------------------
function MenuCallbackProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LResult; stdcall; export;
var
  CM2: IContextMenu2;
begin
  case Msg of
    WM_CREATE:
      begin
        CM2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
        SetWindowLong(Wnd, GWL_USERDATA, LongInt(CM2));
        result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;

    WM_DRAWITEM,
    WM_MEASUREITEM,
    WM_INITMENUPOPUP:
      begin
        CM2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
        if CM2 = nil then messagebox(wnd, 'Null CM2 menu pointer', 'Terry', 0)
        else CM2.HandleMenuMsg(Msg, wParam, lParam);
        if msg = WM_INITMENUPOPUP then result := 0 else result := 1;
      end;
    else result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
function ShContextMenuI(Handle: THandle; P: TPoint; ShellFolder: IShellFolder; PIDLCount: Integer; var PIDL: PItemIDList; ParentPopupMenu: HMenu): boolean;
const
  CLSNAME = 'Terry::ContextMenuCallback';
var
  PopupMenu: HMenu;
  cmd: uint;
  ICM: TCMInvokeCommandInfo;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  AWndClass: TWndClass;
  CallbackWnd, MenuHandle: HWnd;
begin
  result := false;
  CallBackWnd := 0;
  PopupMenu := CreatePopupMenu;

  try
    if Succeeded(ShellFolder.GetUIObjectOf(Handle, PIDLCount, PIDL, IID_IContextMenu, nil, Pointer(ContextMenu))) then
    begin
      if Succeeded(ContextMenu.QueryInterface(IID_IContextMenu2, ContextMenu2)) then
      begin
        ContextMenu2.QueryContextMenu(PopupMenu, 0, 1, $7FFF, CMF_EXPLORE);
        FillChar(AWndClass, SizeOf(AWndClass), #0);
        AWndClass.lpszClassName:= CLSNAME;
        AWndClass.Style:= CS_PARENTDC;
        AWndClass.lpfnWndProc:= @MenuCallbackProc;
        AWndClass.hInstance:= HInstance;
        Windows.RegisterClass(AWndClass);
        CallbackWnd:= CreateWindow(CLSNAME, nil, WS_POPUPWINDOW, 0, 0, 0, 0, 0, 0, HInstance, Pointer(ContextMenu2));
      end else
        ContextMenu.QueryContextMenu(PopupMenu, 0, 1, $7FFF, CMF_EXPLORE);

      if CallbackWnd = 0 then MenuHandle := Handle else MenuHandle := CallbackWnd;

      if ParentPopupMenu <> 0 then
      begin
        InsertMenu(ParentPopupMenu, 0, MF_BYPOSITION + MF_POPUP, PopupMenu, 'Explorer menu');
        InsertMenu(ParentPopupMenu, 1, MF_BYPOSITION + MF_SEPARATOR, 0, '-');
        cmd := uint(TrackPopupMenuEx(ParentPopupMenu, TPM_RETURNCMD + TPM_NONOTIFY, p.x, p.y, MenuHandle, nil));
      end else
        cmd := uint(TrackPopupMenuEx(PopupMenu, TPM_RETURNCMD + TPM_NONOTIFY, p.x, p.y, MenuHandle, nil));

      result:= true;

      if cmd >= $f000 then
      begin
        postmessage(handle, wm_command, cmd, 0);
      end
      else if (cmd > 0) and (cmd < $8000) then
      begin
        sendmessage(handle, wm_command, 0, 0);
        FillChar(ICM, SizeOf(TCMInvokeCommandInfo), #0);
        ICM.cbSize := Sizeof(TCMInvokeCommandInfo);
        ICM.hwnd := Handle;
        ICM.lpVerb := PAnsiChar(Cmd - 1);
        ICM.nShow := SW_SHOWNORMAL;
        if Assigned(ContextMenu2) then ContextMenu2.InvokeCommand(ICM) else ContextMenu.InvokeCommand(ICM);
      end else begin
        sendmessage(handle, wm_command, 0, 0);
      end;
    end;

  finally
    DestroyMenu(PopupMenu);
    if CallbackWnd <> 0 then DestroyWindow(CallbackWnd);
  end;
end;
//------------------------------------------------------------------------------
function ShContextMenu(Handle: THandle; P: TPoint; FileName: string; ParentPopupMenu: HMenu): boolean; overload;
var
  ShellFolder: iShellFolder;
  DirPIDL, DirPIDLFQ, ParentPIDL: PItemIDList;
begin
  result:= false;
  DirPIDL := nil;
  DirPIDLFQ := nil;
  ParentPIDL := nil;

  DirPidlFQ := PIDL_GetFromPath(PChar(FileName));
  if Assigned(DirPIDLFQ) then
  begin
    PIDL_GetRelative(DirPIDLFQ, ParentPIDL, DirPIDL);
    try
      if PIDL_GetFileFolder(ParentPIDL, ShellFolder) then
        result := ShContextMenuI(Handle, P, ShellFolder, 1, DirPIDL, ParentPopupMenu);
    finally
      PIDL_Free(DirPIDL);
      PIDL_Free(DirPIDLFQ);
      PIDL_Free(ParentPIDL);
    end;
  end;
end;
//------------------------------------------------------------------------------
function ShContextMenu(Handle: THandle; P: TPoint; DirPIDLFQ: PItemIDList; ParentPopupMenu: HMenu): boolean; overload;
var
  ShellFolder: IShellFolder;
  DirPIDL, ParentPIDL: PItemIDList;
begin
  result := false;
  DirPIDL := nil;
  ParentPIDL := nil;

  if Assigned(DirPIDLFQ) then
  begin
    PIDL_GetRelative(DirPIDLFQ, ParentPIDL, DirPIDL);
    try
      if PIDL_GetFileFolder(ParentPIDL, ShellFolder) then
        result := ShContextMenuI(Handle, P, ShellFolder, 1, DirPIDL, ParentPopupMenu);
    finally
      PIDL_Free(DirPIDL);
      PIDL_Free(ParentPIDL);
    end;
  end;
end;
//------------------------------------------------------------------------------
function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList; ForParsing: Boolean): string;
var
  StrRet: TStrRet;
  p: PChar;
  Flags: Integer;
begin
  Result := '';
  if ForParsing then Flags:= SHGDN_FORPARSING else Flags:= SHGDN_NORMAL;

  ShellFolder.GetDisplayNameOf(PIDL, Flags, StrRet);
  case StrRet.uType of
    STRRET_CSTR: SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
    STRRET_OFFSET:
      begin
        p:= @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR: Result:= StrRet.pOleStr;
  end;
end;
//------------------------------------------------------------------------------
procedure GetFileInfo(PIDL: PItemIDList; Large, Open: Boolean; var FileInfo: TSHFileInfo);
var
  Flags: integer;
begin
  FillChar(FileInfo, SizeOf(FileInfo), 0);
  Flags:= SHGFI_PIDL + SHGFI_SYSICONINDEX + SHGFI_ICON + SHGFI_DISPLAYNAME;
  if Open then inc(Flags, SHGFI_OPENICON);
  if Large then inc(Flags, SHGFI_LARGEICON) else inc(Flags, SHGFI_SMALLICON);
  SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), Flags);
end;
//------------------------------------------------------------------------------
end.
