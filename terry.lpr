program terry;

{$MODE Delphi}

uses
  Windows,
  Messages,
  Classes,
  SysUtils,
  Interfaces,
  Forms,
  ShellAPI,
  Dialogs,
  declu,
  DockH,
  frmmainu {frmterry},
  frmsetsu in 'frmsetsu.pas' {frmsets},
  frmitemoptu in 'frmitemoptu.pas' {frmShortcutOptions},
  frmcmdu in 'frmcmdu.pas' {frmcmd},
  frmFontU in 'frmFontU.pas' {frmFont},
  frmthemeeditoru {frmLayersEditor},
  frmColorU in 'frmColorU.pas' {frmColor},
  frmDebugU in 'frmDebugU.pas' {frmDebug},
  themeu,
  setsu,
  GDIPAPI,
  gdip_gfx,
  toolu,
  itemmgru,
  customitemu,
  scitemu,
  sepitemu,
  plgitemu,
  stackitemu,
  stacksubitemu,
  hintu,
  droptgtu,
  shcontextu,
  notifieru,
  PIDL,
  dwm_unit,
  EColor,
  traycontrolleru,
  frmAddCommandU,
  frmstackpropu,
  stackmodeu,
  processhlp,
  dropindicatoru,
  taskitemu,
  frmhellou, frmtipu;

{$R *.res}

{$undef DEBUG_EXPORTS}

//------------------------------------------------------------------------------
{$ifdef DEBUG_EXPORTS}
procedure inf(where, data: string);
begin
  frmterry.notify(where + ':   ' + data);
end;
{$endif}
//------------------------------------------------------------------------------
function DockletIsVisible(id: HWND): bool; stdcall;
begin
  result := IsWindowVisible(frmmain.Handle);
  {$ifdef DEBUG_EXPORTS} inf('DockletIsVisible2', inttostr(id) + ', ' + inttostr(integer(result))); {$endif}
end;
//------------------------------------------------------------------------------
function DockletIsUndocked(id: HWND): bool; stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletIsUndocked', inttostr(id)); {$endif}
  result := false;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
        result:= frmmain.ItemMgr.IsPluginUndocked(id);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletIsUndocked', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletGetRect(id: HWND; r: windows.PRect): bool; stdcall;
var
  tmp: windows.TRect;
begin
  //{$ifdef DEBUG_EXPORTS} inf('DockletGetRect', inttostr(id)); {$endif}
  result := false;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
      begin
        result := frmmain.ItemMgr.GetPluginRect(id, tmp);
        r.Left := tmp.left;
        r.Top := tmp.top;
        r.Right := tmp.right;
        r.Bottom := tmp.bottom;
      end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletGetRect', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletGetLabel(id: HWND; szCaption: pchar): integer; stdcall;
var
  capt: string;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletGetLabel', inttostr(id)); {$endif}
  result := 0;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
      begin
        capt := frmmain.ItemMgr.GetPluginCaption(id);
        result := length(capt);
        if szCaption <> nil then StrLCopy(szCaption, pchar(capt), result);
        inc(result);
      end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletGetLabel', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletSetLabel(id: HWND; szCaption: pchar): integer; stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletSetLabel', inttostr(id) + ', ' + strpas(szCaption)); {$endif}
  result := 0;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
      begin
        frmmain.ItemMgr.SetPluginCaption(id, strpas(szCaption));
        result := length(frmmain.ItemMgr.GetPluginCaption(id)) + 1;
      end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletSetLabel', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletLoadGDIPlusImage(szImage: pchar): Pointer; stdcall;
var
  imagefile: string;
begin
  result := nil;
  try
    if szImage <> nil then
    begin
      imagefile := UnzipPath(strpas(szImage));
      {$ifdef DEBUG_EXPORTS} inf('DockletLoadGDIPlusImage.szImage', imagefile); {$endif}
      GdipCreateBitmapFromFile(PWideChar(WideString(imagefile)), result);
    end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletLoadGDIPlusImage', e);
  end;
  {$ifdef DEBUG_EXPORTS} inf('DockletLoadGDIPlusImage', '0x' + inttohex(dword(result), 8)); {$endif}
end;
//------------------------------------------------------------------------------
procedure DockletSetImage(id: HWND; image: Pointer; AutoDelete: bool); stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletSetImage', inttostr(id) + ', 0x' + inttohex(dword(image), 8)); {$endif}
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
        frmmain.ItemMgr.SetPluginImage(id, image, AutoDelete);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletSetImage', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletSetImageFile(id: HWND; szImage: pchar); stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletSetImageFile', inttostr(id) + ', ' + strpas(szImage)); {$endif}
  try
    if szImage <> nil then
      DockletSetImage(id, DockletLoadGDIPlusImage(szImage), true);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletSetImageFile', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletSetImageOverlay(id: uint; overlay: Pointer; AutoDelete: bool); stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletSetImageOverlay', inttostr(id) + ', 0x' + inttohex(dword(overlay), 8)); {$endif}
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
        frmmain.ItemMgr.SetPluginOverlay(id, overlay, AutoDelete);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletSetImageOverlay', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletBrowseForImage(id: HWND; szImage: pchar; szRoot: pchar): bool; stdcall;
var
  sTemp: string;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletBrowseForImage1', inttostr(id) + ', ' + strpas(szImage) + ', ' + strpas(szRoot)); {$endif}
  result:= false;
  try
    if szImage <> nil then
      with TOpenDialog.Create(nil) do
      begin
        if szRoot <> nil then sTemp:= strpas(szRoot)
        else sTemp:= IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)));
        InitialDir := sTemp;
        if Execute then
        begin
          sTemp := FileName;
          StrLCopy(szImage, pchar(sTemp), MAX_PATH);
          result := true;
        end;
        Free;
      end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletBrowseForImage', e);
  end;
  {$ifdef DEBUG_EXPORTS} if result then inf('DockletBrowseForImage2', inttostr(id) + ', ' + strpas(szImage) + ', ' + strpas(szRoot)); {$endif}
end;
//------------------------------------------------------------------------------
procedure DockletLockMouseEffect(id: HWND; lock: bool); stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletLockMouseEffect', inttostr(id) + ', ' + inttostr(integer(lock))); {$endif}
  try
    if assigned(frmmain) then frmmain.LockMouseEffect(id, lock);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletLockMouseEffect', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletDoAttensionAnimation(id: HWND); stdcall;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletDoAttensionAnimation', inttostr(id)); {$endif}
  try
    if assigned(frmmain) then if assigned(frmmain.ItemMgr) then frmmain.ItemMgr.PluginAnimate(id);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletDoAttensionAnimation', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletGetRelativeFolder(id: HWND; szFolder: pchar); stdcall;
var
  rel: string;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletGetRelativeFolder1', inttostr(id)); {$endif}
  try
    rel := frmmain.ItemMgr.GetPluginFile(id);
    rel := IncludeTrailingPathDelimiter(extractfilepath(rel));
    rel := cutafter(rel, UnzipPath('%pp%\'));
    if assigned(szFolder) then StrLCopy(szFolder, pchar(rel), length(rel));
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletGetRelativeFolder', e);
  end;
  {$ifdef DEBUG_EXPORTS} inf('DockletGetRelativeFolder2', inttostr(id) + ', ' + strpas(szFolder)); {$endif}
end;
//------------------------------------------------------------------------------
procedure DockletGetRootFolder(id: HWND; szFolder: pchar); stdcall;
var
  rel: string;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletGetRootFolder1', inttostr(id)); {$endif}
  try
    rel := UnzipPath('%pp%\');
    if assigned(szFolder) then StrLCopy(szFolder, pchar(rel), MAX_PATH);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletGetRootFolder', e);
  end;
  {$ifdef DEBUG_EXPORTS} inf('DockletGetRootFolder2', inttostr(id) + ', ' + strpas(szFolder)); {$endif}
end;
//------------------------------------------------------------------------------
function DockletQueryDockEdge(id: HWND): integer; stdcall;
begin
  result := 0;
  if assigned(sets) then
    case sets.container.site of
      bsLeft: result := 2;
      bsTop: result := 1;
      bsRight: result := 3;
      bsBottom: result := 0;
    end;
end;
//------------------------------------------------------------------------------
function DockletSetDockEdge(id: HWND; Edge: integer): integer; stdcall;
begin
  result := 0;
  if assigned(sets) and assigned(frmmain) then
  begin
    case Edge of
      0: sets.container.site := bsBottom;
      1: sets.container.site := bsTop;
      2: sets.container.site := bsLeft;
      3: sets.container.site := bsRight;
    end;
    frmmain.SetParam(gpSite, Edge);
    case sets.container.site of
      bsLeft: result := 2;
      bsTop: result := 1;
      bsRight: result := 3;
      bsBottom: result := 0;
    end;
  end;
end;
//------------------------------------------------------------------------------
function DockletQueryDockAlign(id: HWND): integer; stdcall;
begin
  result := 1;
  if assigned(sets) then result:= (sets.container.CenterOffsetPercent + 25) div 50;
end;
//------------------------------------------------------------------------------
function DockletSetDockAlign(id: HWND; Offset: integer): integer; stdcall;
begin
  result := 1;
  if assigned(sets) and assigned(frmmain) then
  begin
    frmmain.SetParam(gpCenterOffsetPercent, Offset * 50);
    result:= (sets.container.CenterOffsetPercent + 25) div 50;
  end;
end;
//------------------------------------------------------------------------------
function DockColorDialog(AColor: puint): bool; stdcall;
begin
  result := false;
  try
    with TColorDialog.Create(nil) do
    begin
      Color := AColor^ and $FFFFFF;
      if Execute then
      begin
        AColor^ := Color;
        result := true;
      end;
      free;
    end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockColorDialog', e);
  end;
end;
//------------------------------------------------------------------------------
function DockGetRect: windows.TRect; stdcall;
begin
  result.Left := 0;
  result.Top := 0;
  result.Right := 0;
  result.Bottom := 0;
  if assigned(frmmain) and assigned(sets) then
  if assigned(frmmain.ItemMgr) then
  begin
    result.Left := frmmain.ItemMgr.BaseWindowRect.X + frmmain.ItemMgr.X;
    result.Top := frmmain.ItemMgr.BaseWindowRect.Y + frmmain.ItemMgr.Y;
    result.Right := frmmain.ItemMgr.BaseWindowRect.X + frmmain.ItemMgr.X + frmmain.ItemMgr.width;
    result.Bottom := frmmain.ItemMgr.BaseWindowRect.Y + frmmain.ItemMgr.Y + frmmain.ItemMgr.height;
    case sets.GetParam(gpSite) of
      0: result.Right := max(result.Right, frmmain.ItemMgr.GetZoomEdge);
      1: result.Bottom := max(result.Bottom, frmmain.ItemMgr.GetZoomEdge);
      2: result.Left := min(result.Left, frmmain.ItemMgr.GetZoomEdge);
      3: result.Top := min(result.Top, frmmain.ItemMgr.GetZoomEdge);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure DockExecute(id: HWND; exename, params, dir: pchar; showcmd: integer); stdcall;
begin
  try
    if assigned(frmmain) then frmmain.mexecute(exename, params, dir, showcmd, id);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockExecute', e);
  end;
end;
//------------------------------------------------------------------------------
function DockAddMenu(hMenu: HWND): uint; stdcall;
begin
  result := 0;
  try
    if assigned(frmmain) then result := frmmain.GetHMenu(hMenu);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockAddMenu', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Undock(id: HWND); stdcall;
begin
  if assigned(frmmain) then if assigned(frmmain.ItemMgr) then frmmain.ItemMgr.Undock(id);
end;
//------------------------------------------------------------------------------
procedure Dock(id: HWND); stdcall;
begin
  if assigned(frmmain) then if assigned(frmmain.ItemMgr) then frmmain.ItemMgr.Dock(id);
end;
//------------------------------------------------------------------------------
function DockCreateItem(data: pchar): uint; stdcall;
begin
  result := 0;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
        result := frmmain.ItemMgr.CreateItem(data);
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockCreateItem', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockDeleteItem(id: HWND); stdcall;
begin
  if assigned(frmmain) then if assigned(frmmain.ItemMgr) then frmmain.ItemMgr.DeleteItem(id);
end;
//------------------------------------------------------------------------------
function DockAddProgram(data: pchar): uint; stdcall;
begin
  try
    result := 0;
    if assigned(frmmain) then frmmain.AddFile(strpas(data));
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockAddProgram', e);
  end;
end;
//------------------------------------------------------------------------------
function FullScreenAppActive(id: HWND): bool; stdcall;
begin
  result := false;
  if assigned(frmmain) then result := frmmain.FullScreenAppActive(id);
end;
//------------------------------------------------------------------------------
procedure Notify(id: HWND; Message: PAnsiChar); stdcall;
begin
  if assigned(frmmain) then frmmain.Notify(strpas(Message));
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
exports
  DockletIsVisible,
  DockletIsUndocked,
  DockletGetRect,
  DockletGetLabel,
  DockletSetLabel,
  DockletSetImage,
  DockletSetImageFile,
  DockletSetImageOverlay,
  DockletDoAttensionAnimation,
  DockletGetRootFolder,
  DockletGetRelativeFolder,
  DockletLoadGDIPlusImage,
  DockletBrowseForImage,
  DockletLockMouseEffect,
  DockletQueryDockEdge,
  DockletSetDockEdge,
  DockletQueryDockAlign,
  DockletSetDockAlign,
  DockGetRect,
  DockColorDialog,
  DockExecute,
  DockAddMenu,
  Undock,
  Dock,
  DockCreateItem,
  DockDeleteItem,
  DockAddProgram,
  FullScreenAppActive,
  Notify;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function AWindowItemProc(wnd: HWND; message: uint; wParam: integer; lParam: integer): integer; stdcall;
begin
  result := DefWindowProc(wnd, message, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure RegisterWindowItemClass;
var
  wndClass: windows.TWndClass;
begin
  try
    wndClass.style          := CS_DBLCLKS;
    wndClass.lpfnWndProc    := @AWindowItemProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := WINITEM_CLASS;
    if windows.RegisterClass(wndClass) = 0 then raise Exception.Create('Can not register window class');
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('RegisterWindowItemClass', e);
  end;
end;
//------------------------------------------------------------------------------
function encodePath(path: string): string;
begin
  result := toolu.ReplaceEx(path, '\', '_');
  if length(result) > MAX_PATH - 1 - length(PROGRAM_GUID) - 7 then
     result := copy(result, length(result) - (MAX_PATH - 1 - length(PROGRAM_GUID) - 7), MAX_PATH);
end;

var
  i: integer;
  setsFiles: TStrings;
  SetsFilename: string;
  multiDock: boolean;
  dockIndex: integer;
  hMutex: uint;
  WinHandle: THandle;
begin
  //if FileExists('heap.trc') then DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');

  // multi-dock support //
  SetsFilename := AnsiLowerCase(UnzipPath('%pp%\sets.ini'));
  setsFiles := TStringList.Create;
  searchfiles(UnzipPath('%pp%'), 'sets*.ini', setsFiles);
  setsFiles.Delete(setsFiles.IndexOf(SetsFilename));
  qSortStrings(setsFiles);
  multiDock := setsFiles.Count > 0;

  dockIndex := 0;
  if multiDock then
  begin
    // read dock index (if specified)
    i := 1;
    while i <= ParamCount do
    begin
      if strlicomp(pchar(ParamStr(i)), '-dock', 5) = 0 then
        if not TryStrToInt(copy(ParamStr(i), 6, 1), dockIndex) then dockIndex := 0;
      inc(i);
    end;
    // read settings file name
    if dockIndex = 0 then
    else
    if dockIndex <= setsFiles.Count then
       SetsFilename := setsFiles.strings[dockIndex - 1]
    else halt;
  end;
  setsFiles.free;

  // check running instances //

  hMutex := CreateMutex(nil, false, pchar('Global\' + PROGRAM_GUID + encodePath(SetsFilename)));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    WinHandle := FindWindow('Window', PROGRAM_NAME);
    if IsWindow(WinHandle) then
    begin
      sendmessage(WinHandle, wm_user, wm_activate, 0);
      SetForegroundWindow(WinHandle);
    end;
    halt;
  end;

  // run next dock //

  if multiDock then
  begin
    inc(dockIndex);
    ShellExecute(0, nil, pchar(Paramstr(0)), pchar(UTF8ToAnsi('-dock' + inttostr(dockIndex))), pchar(ExtractFilePath(Paramstr(0))), SW_SHOWNORMAL);
  end;

  AddLog('--------------------------------------');
  AddLog('AppInitialize');
  Application.Initialize;

  AddLog('AppWindowStyle');
  WinHandle := FindWindow('Window', 'tdock');
  if IsWindow(WinHandle) then
    SetWindowLong(WinHandle, GWL_EXSTYLE,
      GetWindowLong(WinHandle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);

  AddLog('MainWindowStyle');
  Application.ShowMainForm := false;
  Application.CreateForm(Tfrmmain, frmmain);
  SetWindowLong(frmmain.handle, GWL_EXSTYLE, GetWindowLong(frmmain.handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);
  frmmain.Caption := PROGRAM_NAME;

  AddLog('RegisterWindowItemClass');
  RegisterWindowItemClass;
  AddLog('Notifier');
  Notifier := _Notifier.Create;
  AddLog('Init');
  frmmain.Init(SetsFilename);
  Application.ShowMainForm := true;
  AddLog('ExecAutorun');
  frmmain.ExecAutorun;

  AddLog('AppRun');
  Application.Run;

  CloseHandle(hMutex);
  AddLog('EndProgram');
end.
