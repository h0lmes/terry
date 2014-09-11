program terry;

{$MODE Delphi}

uses
  Windows,
  Messages,
  SysUtils,
  Interfaces,
  Forms,
  ShellAPI,
  Dialogs,
  declu,
  DockH,
  frmterryu in 'frmterryu.pas' {frmterry},
  frmsetsu in 'frmsetsu.pas' {frmsets},
  frmitemoptu in 'frmitemoptu.pas' {frmShortcutOptions},
  frmcmdu in 'frmcmdu.pas' {frmcmd},
  frmFontU in 'frmFontU.pas' {frmFont},
  frmLayersEditorU in 'frmLayersEditorU.pas' {frmLayersEditor},
  frmColorU in 'frmColorU.pas' {frmColor},
  frmDebugU in 'frmDebugU.pas' {frmDebug},
  frmColorData_unit in 'frmColorData_unit.pas' {frmColorData},
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
  taskitemu;

{$R *.res}

const debugPlugins: boolean = false;

//------------------------------------------------------------------------------
procedure inf(where, data: string);
begin
  frmterry.notify(where + #10#13 + '  ' + #10#13 + data);
end;
//------------------------------------------------------------------------------
function DockletIsVisible(id: HWND): bool; stdcall;
begin
  if debugPlugins then inf('DockletIsVisible', inttostr(id));
  result := false;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
        result := frmterry.ItemMgr.Visible;
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletIsVisible', e);
  end;
  if debugPlugins then inf('DockletIsVisible2', inttostr(id) + ' ' + inttostr(integer(result)));
end;
//------------------------------------------------------------------------------
function DockletIsUndocked(id: HWND): bool; stdcall;
begin
  if debugPlugins then inf('DockletIsUndocked', inttostr(id));
  result := false;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
        result:= frmterry.ItemMgr.IsPluginUndocked(id);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletIsUndocked', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletGetRect(id: HWND; r: windows.PRect): bool; stdcall;
begin
  if debugPlugins then inf('DockletGetRect', inttostr(id));
  result := false;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
      begin
        r^ := frmterry.ItemMgr.GetPluginRect(id);
        result := frmterry.ItemMgr.Visible;
      end;
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletGetRect', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletGetLabel(id: HWND; szCaption: pchar): integer; stdcall;
var
  capt: string;
begin
  if debugPlugins then inf('DockletGetLabel', inttostr(id));
  result := 0;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
      begin
        capt := frmterry.ItemMgr.GetPluginCaption(id);
        result := length(capt);
        if szCaption <> nil then StrLCopy(szCaption, pchar(capt), result);
        inc(result);
      end;
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletGetLabel', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletSetLabel(id: HWND; szCaption: pchar): integer; stdcall;
begin
  if debugPlugins then inf('DockletSetLabel', inttostr(id) + #10#13 + strpas(szCaption));
  result := 0;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
      begin
        frmterry.ItemMgr.SetPluginCaption(id, strpas(szCaption));
        result := length(frmterry.ItemMgr.GetPluginCaption(id)) + 1;
      end;
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletSetLabel', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletLoadGDIPlusImage(szImage: pchar): Pointer; stdcall;
var
  w, h: uint;
begin
  if debugPlugins then inf('DockletLoadGDIPlusImage1', strpas(szImage));
  result := nil;
  try
    if szImage <> nil then LoadImage(strpas(szImage), 256, false, false, result, w, h);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletLoadGDIPlusImage', e);
  end;
  if debugPlugins then inf('DockletLoadGDIPlusImage2', inttohex(dword(result), 8));
end;
//------------------------------------------------------------------------------
procedure DockletSetImage(id: HWND; image: Pointer; AutoDelete: bool); stdcall;
begin
  if debugPlugins then inf('DockletSetImage', inttostr(id) + #10#13 + inttohex(dword(image), 8));
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
        frmterry.ItemMgr.SetPluginImage(id, image, AutoDelete);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletSetImage', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletSetImageFile(id: HWND; szImage: pchar); stdcall;
begin
  if debugPlugins then inf('DockletSetImageFile', inttostr(id) + #10#13 + strpas(szImage));
  try
    if szImage <> nil then
      DockletSetImage(id, DockletLoadGDIPlusImage(szImage), true);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletSetImageFile', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletSetImageOverlay(id: uint; overlay: Pointer; AutoDelete: bool); stdcall;
begin
  if debugPlugins then inf('DockletSetImageOverlay', inttostr(id) + #10#13 + inttohex(dword(overlay), 8));
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
        frmterry.ItemMgr.SetPluginOverlay(id, overlay, AutoDelete);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletSetImageOverlay', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletBrowseForImage(id: HWND; szImage: pchar; szRoot: pchar): bool; stdcall;
var
  sTemp: string;
begin
  if debugPlugins then inf('DockletBrowseForImage1', inttostr(id) + #10#13 + strpas(szImage) + #10#13 + strpas(szRoot));
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
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletBrowseForImage', e);
  end;
  if debugPlugins then if result then inf('DockletBrowseForImage2', inttostr(id) + #10#13 + strpas(szImage) + #10#13 + strpas(szRoot));
end;
//------------------------------------------------------------------------------
procedure DockletLockMouseEffect(id: HWND; lock: bool); stdcall;
begin
  if debugPlugins then inf('DockletLockMouseEffect', inttostr(id) + #10#13 + inttostr(integer(lock)));
  try
    if assigned(frmterry) then frmterry.LockMouseEffect(id, lock);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletLockMouseEffect', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletDoAttensionAnimation(id: HWND); stdcall;
begin
  if debugPlugins then inf('DockletDoAttensionAnimation', inttostr(id));
  try
    if assigned(frmterry) then if assigned(frmterry.ItemMgr) then frmterry.ItemMgr.PluginAnimate(id);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletDoAttensionAnimation', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockletGetRelativeFolder(id: HWND; szFolder: pchar); stdcall;
var
  rel: string;
begin
  if debugPlugins then inf('DockletGetRelativeFolder1', inttostr(id) + #10#13 + strpas(szFolder));
  try
    rel := frmterry.ItemMgr.GetPluginFile(id);
    rel := IncludeTrailingPathDelimiter(extractfilepath(rel));
    rel := cutafter(rel, UnzipPath('%pp%\'));
    if assigned(szFolder) then StrLCopy(szFolder, pchar(rel), length(rel));
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletGetRelativeFolder', e);
  end;
  if debugPlugins then inf('DockletGetRelativeFolder2', inttostr(id) + #10#13 + strpas(szFolder));
end;
//------------------------------------------------------------------------------
procedure DockletGetRootFolder(id: HWND; szFolder: pchar); stdcall;
var
  rel: string;
begin
  if debugPlugins then inf('DockletGetRootFolder1', inttostr(id) + #10#13 + strpas(szFolder));
  try
    rel := UnzipPath('%pp%\');
    if assigned(szFolder) then StrLCopy(szFolder, pchar(rel), MAX_PATH);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockletGetRootFolder', e);
  end;
  if debugPlugins then inf('DockletGetRootFolder2', inttostr(id) + #10#13 + strpas(szFolder));
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
  if assigned(sets) and assigned(frmterry) then
  begin
    case Edge of
      0: sets.container.site := bsBottom;
      1: sets.container.site := bsTop;
      2: sets.container.site := bsLeft;
      3: sets.container.site := bsRight;
    end;
    frmterry.SetParam(gpSite, Edge);
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
  if assigned(sets) and assigned(frmterry) then
  begin
    frmterry.SetParam(gpCenterOffsetPercent, Offset * 50);
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
    on e: Exception do if assigned(frmterry) then frmterry.err('DockColorDialog', e);
  end;
end;
//------------------------------------------------------------------------------
function DockGetRect: windows.TRect; stdcall;
begin
  result.Left := 0;
  result.Top := 0;
  result.Right := 0;
  result.Bottom := 0;
  if assigned(frmterry) and assigned(sets) then
  if assigned(frmterry.ItemMgr) then
  begin
    result.Left := frmterry.ItemMgr.BaseWindowRect.X + frmterry.ItemMgr.X;
    result.Top := frmterry.ItemMgr.BaseWindowRect.Y + frmterry.ItemMgr.Y;
    result.Right := frmterry.ItemMgr.BaseWindowRect.X + frmterry.ItemMgr.X + frmterry.ItemMgr.width;
    result.Bottom := frmterry.ItemMgr.BaseWindowRect.Y + frmterry.ItemMgr.Y + frmterry.ItemMgr.height;
    case sets.GetParam(gpSite) of
      0: result.Right := max(result.Right, frmterry.ItemMgr.GetZoomEdge);
      1: result.Bottom := max(result.Bottom, frmterry.ItemMgr.GetZoomEdge);
      2: result.Left := min(result.Left, frmterry.ItemMgr.GetZoomEdge);
      3: result.Top := min(result.Top, frmterry.ItemMgr.GetZoomEdge);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure DockExecute(id: HWND; exename, params, dir: pchar; showcmd: integer); stdcall;
begin
  try
    if assigned(frmterry) then frmterry.mexecute(exename, params, dir, showcmd, id);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockExecute', e);
  end;
end;
//------------------------------------------------------------------------------
function DockAddMenu(hMenu: HWND): uint; stdcall;
begin
  result := 0;
  try
    if assigned(frmterry) then result := frmterry.GetHMenu(hMenu);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockAddMenu', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Undock(id: HWND); stdcall;
begin
  if assigned(frmterry) then if assigned(frmterry.ItemMgr) then frmterry.ItemMgr.Undock(id);
end;
//------------------------------------------------------------------------------
procedure Dock(id: HWND); stdcall;
begin
  if assigned(frmterry) then if assigned(frmterry.ItemMgr) then frmterry.ItemMgr.Dock(id);
end;
//------------------------------------------------------------------------------
function DockCreateItem(data: pchar): uint; stdcall;
begin
  result := 0;
  try
    if assigned(frmterry) then
      if assigned(frmterry.ItemMgr) then
        result := frmterry.ItemMgr.CreateItem(data);
  except
    on e: Exception do if assigned(frmterry) then frmterry.err('DockCreateItem', e);
  end;
end;
//------------------------------------------------------------------------------
procedure DockDeleteItem(id: HWND); stdcall;
begin
  if assigned(frmterry) then if assigned(frmterry.ItemMgr) then frmterry.ItemMgr.DeleteItem(id);
end;
//------------------------------------------------------------------------------
function FullScreenAppActive(id: HWND): bool; stdcall;
begin
  result := false;
  if assigned(frmterry) then result := frmterry.FullScreenAppActive(id);
end;
//------------------------------------------------------------------------------
procedure Notify(id: HWND; Message: PAnsiChar); stdcall;
begin
  if assigned(frmterry) then frmterry.Notify(strpas(Message));
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
    on e: Exception do if assigned(frmterry) then frmterry.err('RegisterWindowItemClass', e);
  end;
end;
//------------------------------------------------------------------------------
var
  WinHandle: THandle;
  hMutex: uint;
begin
  //if FileExists('heap.trc') then DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');


  hMutex := CreateMutex(nil, false, 'Global\' + declu.GUID);
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    WinHandle := FindWindow('Window', 'TerryApp');
    if IsWindow(WinHandle) then
    begin
      sendmessage(WinHandle, wm_user, wm_activate, 0);
      SetForegroundWindow(WinHandle);
    end;
    halt;
  end;

  AddLog('--------------------------------------');
  AddLog('AppInitialize');
  Application.Initialize;

  AddLog('AppWindowStyle');
  WinHandle := FindWindow('Window', 'terry');
  if IsWindow(WinHandle) then
    SetWindowLong(WinHandle, GWL_EXSTYLE,
      GetWindowLong(WinHandle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);

  AddLog('MainWindowStyle');
  Application.ShowMainForm := false;
  Application.CreateForm(Tfrmterry, frmterry);
  SetWindowLong(frmterry.handle, GWL_EXSTYLE, GetWindowLong(frmterry.handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);
  frmterry.Caption := 'TerryApp';

  AddLog('RegisterWindowItemClass');
  RegisterWindowItemClass;
  AddLog('Notifier');
  Notifier := _Notifier.Create;
  AddLog('Init');
  frmterry.Init;
  Application.ShowMainForm := true;
  AddLog('ExecAutorun');
  frmterry.ExecAutorun;

  AddLog('AppRun');
  Application.Run;

  if assigned(Notifier) then Notifier.Free;
  CloseHandle(hMutex);
  AddLog('EndProgram');
end.
