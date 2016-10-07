program terry;

{$MODE Delphi}

uses
  jwaWindows,
  Windows,
  Messages,
  Classes,
  SysUtils,
  Interfaces,
  interfacebase,
  Forms,
  Dialogs,
  ShellAPI,
  declu,
  DockH,
  frmmainu {frmterry},
  frmsetsu {frmsets},
  frmitemoptu {frmShortcutOptions},
  frmcmdu {frmcmd},
  frmthemeeditoru {frmLayersEditor},
  frmColorU {frmColor},
  frmDebugU {frmDebug},
  themeu,
  setsu,
  GDIPAPI,
  gfx,
  toolu,
  itemmgru,
  customitemu, scitemu, sepitemu, plgitemu, stackitemu, customsubitemu, scsubitemu,
  hintu, droptgtu, shcontextu, notifieru,
  PIDL, dwm_unit, trayu, frmAddCommandU, frmstackpropu, stackmodeu, processhlp,
  taskitemu, frmhellou, frmtipu, multidocku, aeropeeku,
shelltraywndu, MMDevApi_tlb,
  mixeru, networksu, customdrawitemu, frmrestoreu, iniproc, loggeru;

{$R *.res}

{$undef DEBUG_EXPORTS}
{$undef EXT_DEBUG}

//------------------------------------------------------------------------------
{$ifdef DEBUG_EXPORTS}
procedure inf(where, data: string);
begin
  AddLog(where + ':   ' + data);
end;
{$endif}
//------------------------------------------------------------------------------
function DockletIsVisible(id: HWND): bool; stdcall;
begin
  result := IsWindowVisible(frmmain.Handle);
  {$ifdef DEBUG_EXPORTS} inf('DockletIsVisible', inttostr(id) + ', ' + inttostr(integer(result))); {$endif}
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
  {$ifdef DEBUG_EXPORTS} inf('DockletGetRect', inttostr(id)); {$endif}
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
var
  pw: array [0..255] of wchar;
begin
  {$ifdef DEBUG_EXPORTS} inf('DockletSetLabel', inttostr(id) + ', ' + strpas(szCaption)); {$endif}
  result := 0;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
      begin
        MultiByteToWideChar(CP_ACP, 0, pchar(szCaption), length(szCaption) + 1, pw, 255);
        result := frmmain.ItemMgr.SetPluginCaption(id, strpas(pwchar(@pw))) + 1;
      end;
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockletSetLabel', e);
  end;
end;
//------------------------------------------------------------------------------
function DockletLoadGDIPlusImage(szImage: pchar): Pointer; stdcall;
var
  imagefile: WideString;
begin
  result := nil;
  try
    if szImage <> nil then
    begin
      imagefile := UnzipPath(strpas(szImage));
      {$ifdef DEBUG_EXPORTS} inf('DockletLoadGDIPlusImage.szImage', strpas(szImage)); {$endif}
      GdipCreateBitmapFromFile(PWideChar(imagefile), result);
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
procedure DockletSetImageOverlay(id: HWND; overlay: Pointer; AutoDelete: bool); stdcall;
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
    {$ifdef DEBUG_EXPORTS} inf('GetPluginFile', inttostr(id) + ', ' + rel); {$endif}
    rel := IncludeTrailingPathDelimiter(extractfilepath(rel));
    {$ifdef DEBUG_EXPORTS} inf('extractfilepath', inttostr(id) + ', ' + rel); {$endif}
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
  if assigned(frmmain) then
    if assigned(frmmain.ItemMgr) then
      result := frmmain.ItemMgr.Rect;
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
function DockAddMenu(hMenu: THandle): THandle; stdcall;
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
  if assigned(frmmain) then
    if assigned(frmmain.ItemMgr) then frmmain.ItemMgr.Dock(id);
end;
//------------------------------------------------------------------------------
function DockCreateItem(data: pchar): HWND; stdcall;
begin
  result := 0;
  try
    if assigned(frmmain) then
      if assigned(frmmain.ItemMgr) then
        result := frmmain.ItemMgr.CreateItemFromString(data);
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
function DockAddProgram(data: pchar): THandle; stdcall;
begin
  try
    result := 0;
    if assigned(frmmain) then frmmain.AddFile(strpas(data));
  except
    on e: Exception do if assigned(frmmain) then frmmain.err('DockAddProgram', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Notify(id: HWND; Message: PAnsiChar); stdcall;
begin
  if assigned(frmmain) then frmmain.Notify(WideString(strpas(Message)));
end;
//------------------------------------------------------------------------------
procedure ActivateHint(id: HWND; Caption: PWideChar; x, y: integer); stdcall;
begin
  if assigned(frmmain) then frmmain.ActivateHint(id, Caption, x, y);
end;
//------------------------------------------------------------------------------
procedure DeactivateHint(id: HWND); stdcall;
begin
  if assigned(frmmain) then frmmain.DeactivateHint(id);
end;
//------------------------------------------------------------------------------
procedure ExcludeFromPeek(id: HWND); stdcall;
begin
  if assigned(dwm) then dwm.ExcludeFromPeek(id);
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
  Notify,
  ActivateHint,
  DeactivateHint,
  ExcludeFromPeek;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
function TDWindowProc(wnd: HWND; message: uint; wParam: integer; lParam: integer): integer; stdcall;
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
    wndClass.lpfnWndProc    := @TDWindowProc;
    wndClass.cbClsExtra     := 0;
    wndClass.cbWndExtra     := 0;
    wndClass.hInstance      := hInstance;
    wndClass.hIcon          := 0;
    wndClass.hCursor        := LoadCursor(0, idc_Arrow);
    wndClass.hbrBackground  := 0;
    wndClass.lpszMenuName   := nil;
    wndClass.lpszClassName  := TDWCLASS;
    windows.RegisterClass(wndClass);
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
//------------------------------------------------------------------------------
var
  VerInfo: TOSVersioninfo;
  {$ifndef CPU64}
  IsWow64Process: function(Handle: THandle; var Res: boolean): boolean; stdcall;
  {$endif}
  idx: integer;
  setsFiles: TStrings;
  SetsFilename, ProgramPath: string;
  hMutex: THandle;
  h: THandle;
begin
  loggeru.SetLogFileName(ChangeFileExt(ParamStr(0), '.log'));
  AddLog('>>> TDock app start');

  //if FileExists('heap.trc') then DeleteFile('heap.trc');
  //SetHeapTraceOutput('heap.trc');

  // os version
  VerInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
  GetVersionEx(@VerInfo);
  toolu.bIsWindowsVista := VerInfo.dwMajorVersion >= 6;
  gfx.bIsWindowsVista := toolu.bIsWindowsVista;

  toolu.ScalingFactor := 100;
  // running on win64
  toolu.bIsWow64 := false;
  {$ifndef CPU64}
  IsWow64Process := GetProcAddress(GetModuleHandle(Kernel32), 'IsWow64Process');
  if assigned(IsWow64Process) then IsWow64Process(GetCurrentProcess, toolu.bIsWow64);
  {$endif}
  {$ifdef EXT_DEBUG} AddLog('version'); {$endif}

  // multi-dock support //

  TMultiDock.CCreate();
  // read sets filename from params
  ProgramPath := IncludeTrailingPathDelimiter(ExtractFilePath(Paramstr(0)));
  SetsFilename := '';
  idx := 1;
  while idx <= ParamCount do
  begin
    if strlicomp(pchar(ParamStr(idx)), '-i', 2) = 0 then
      SetsFilename := ProgramPath + copy(ParamStr(idx), 3, MAX_PATH - 1);
    inc(idx);
  end;
  {$ifdef EXT_DEBUG} AddLog('MultiDock'); {$endif}

  // if it was not specified
  if SetsFilename = '' then
  begin
    // list all sets files in program folder
    setsFiles := TStringList.Create;
    searchfiles(ProgramPath, 'sets*.ini', setsFiles);
    qSortStrings(setsFiles);
    // take 1st one
    if setsFiles.Count > 0 then SetsFilename := ProgramPath + setsFiles.Strings[0];
    // or the default one
    if not FileExists(SetsFilename) then SetsFilename := ProgramPath + 'sets.ini';
    // run instances for all other files
    if setsFiles.Count > 1 then
    begin
      for idx := 1 to setsFiles.Count - 1 do docks.RunDock(setsFiles.Strings[idx]);
    end;
    setsFiles.free;
  end;
  {$ifdef EXT_DEBUG} AddLog('scan sets files'); {$endif}


  // check running instances //

  hMutex := CreateMutex(nil, false, pchar('Global\' + PROGRAM_GUID + encodePath(SetsFilename)));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    h := FindWindow('Window', PROGRAM_NAME);
    if IsWindow(h) then
    begin
      sendmessage(h, wm_user, wm_activate, 0);
      SetForegroundWindow(h);
    end;
    halt;
  end;
  {$ifdef EXT_DEBUG} AddLog('CreateMutex'); {$endif}

  // application //

  Application.Initialize;
  {$ifdef EXT_DEBUG} AddLog('Application.Initialize'); {$endif}
  Application.Title := PROGRAM_TITLE;
  h := WidgetSet.AppHandle;
  SetWindowLongPtr(h, GWL_EXSTYLE, GetWindowLongPtr(h, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
  {$ifdef EXT_DEBUG} AddLog('SetWindowLongPtr appHandle'); {$endif}

  Application.ShowMainForm := false;
  Application.CreateForm(Tfrmmain, frmmain);
  {$ifdef EXT_DEBUG} AddLog('frmmain.Create'); {$endif}
  SetWindowLongPtr(frmmain.handle, GWL_EXSTYLE, GetWindowLongPtr(frmmain.handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TOOLWINDOW);
  frmmain.Caption := PROGRAM_NAME;
  {$ifdef EXT_DEBUG} AddLog('SetWindowLongPtr frmmain'); {$endif}

  RegisterWindowItemClass;
  {$ifdef EXT_DEBUG} AddLog('RegisterWindowItemClass'); {$endif}
  Notifier := TNotifier.Create;
  {$ifdef EXT_DEBUG} AddLog('TNotifier.Create'); {$endif}
  mixer := TMixer.Create;
  {$ifdef EXT_DEBUG} AddLog('TMixer.Create'); {$endif}
  // ProcessHelper (must be created before tray controller). Depends on DWM //
  ProcessHelper := TProcessHelper.Create;
  {$ifdef EXT_DEBUG} AddLog('TProcessHelper.Create'); {$endif}
  ShellTrayWndController := TShellTrayWndController.Create;
  {$ifdef EXT_DEBUG} AddLog('TShellTrayWndController.Create'); {$endif}

  frmmain.Init(SetsFilename);
  {$ifdef EXT_DEBUG} AddLog('frmmain.Init'); {$endif}
  Application.ShowMainForm := true;
  frmmain.ExecAutorun;

  Application.Run;

  CloseHandle(hMutex);
  TShellTrayWndController.Cleanup;
  TProcessHelper.Cleanup;
  TMixer.Cleanup;
  TNotifier.Cleanup;
  TMultiDock.CDestroy;
  AddLog('>>> End');
end.
