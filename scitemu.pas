unit scitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, Math, ShellAPI, ComObj, ShlObj,
  IniFiles, GDIPAPI, gfx, PIDL, ShContextU, declu, dockh, customdrawitemu,
  toolu, processhlp, aeropeeku, mixeru, networksu, iniproc;

type

  { TShortcutItem }

  TShortcutItem = class(TCustomDrawItem)
  private
    FCommand: WideString;
    FParams: WideString;
    FDir: WideString;
    FImageFile: WideString;
    FImageFile2: WideString;
    FShowCmd: integer;
    FHide: boolean;
    FUseShellContextMenus: boolean;
    FTaskLivePreviews: boolean;
    FTaskThumbSize: integer;
    FTaskGrouping: boolean;
    FIsExecutable: boolean;
    FExecutable: string;
    FIsPIDL: boolean;
    FPIDL: PItemIDList;
    FLastMouseUp: PtrUInt;
    FAppList: TFPList;
    FIsOpen: boolean; // is PeekWindow open or not
    FDynObject: boolean;
    FDynObjectRecycleBin: boolean;
    FDynObjectState: integer;
    procedure BeforeUndock;
    procedure UpdateItemI;
    procedure UpdateItemRunningState;
    procedure LoadImageI;
    procedure LoadDynObjectImage(imagefile: string; MaxSize: integer; exact: boolean; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
    procedure CheckIfDynObject;
    procedure DynObjectUpdate;
    procedure AfterDraw;
    procedure DrawOverlay(dst: pointer; x, y, size: integer);
    procedure BeforeMouseHover(AHover: boolean);
    procedure MouseHover(AHover: boolean);
    procedure Exec(action: TExecuteAction);
    function ActivateProcessMainWindow(group: boolean): boolean;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure ClosePeekWindow(Timeout: cardinal = 0);
    procedure ShowPeekWindow(Timeout: cardinal = 0);
    procedure UpdatePeekWindow;
  public
    procedure UpdateItem(AData: string);
    //
    constructor Create(AData: string; AWndParent: HWND; AParams: TDItemCreateParams); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TDParam; param: PtrInt): PtrInt; override;
    procedure Timer; override;
    procedure Configure; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function RegisterProgram: string; override;
    function DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make(wnd: HWND; ACaption: WideString; ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

implementation
uses frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutItem.Create(AData: string; AWndParent: HWND; AParams: TDItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;
  FTaskGrouping := AParams.TaskGrouping;
  FTaskLivePreviews := AParams.TaskLivePreviews;
  FTaskThumbSize := AParams.TaskThumbSize;

  FLastMouseUp:= 0;
  FCommand:= '';
  FParams:= '';
  FDir:= '';
  FImageFile:= '';
  FShowCmd:= 0;
  FHide:= false;
  FRunning:= false;
  FAppList := TFPList.Create;
  FIsOpen := false;
  OnBeforeMouseHover := BeforeMouseHover;
  OnMouseHover := MouseHover;
  OnBeforeUndock := BeforeUndock;
  OnAfterDraw := AfterDraw;
  OnDrawOverlay := DrawOverlay;

  UpdateItem(AData);
end;
//------------------------------------------------------------------------------
destructor TShortcutItem.Destroy;
begin
  FFreed := true;
  if FIsOpen then TAeroPeekWindow.Close(0);
  KillTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT);
  try GdipDisposeImage(FImage);
  except end;
  try if FIsPIDL then PIDL_Free(FPIDL);
  except end;
  FAppList.free;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateItem(AData: string);
var
  IniFile, IniSection: string;
begin
  if FFreed then exit;

  try
    IniFile := FetchValue(AData, 'inifile="', '";');
    IniSection := FetchValue(AData, 'inisection="', '";');

    if (length(IniFile) > 0) and (length(IniSection) > 0) then
    begin
      Caption :=    GetIniStringW(IniFile, IniSection, 'caption', '');
      FCommand :=   GetIniStringW(IniFile, IniSection, 'command', '');
      FParams :=    GetIniStringW(IniFile, IniSection, 'params', '');
      FDir :=       GetIniStringW(IniFile, IniSection, 'dir', '');
      FImageFile := GetIniStringW(IniFile, IniSection, 'image', '');
      FImageFile2 := cutafter(FImageFile, ';');
      FImageFile := cut(FImageFile, ';');
      FHide :=      GetIniBoolW(IniFile, IniSection, 'hide', false);
      FColorData := toolu.StringToColor(GetIniStringW(IniFile, IniSection, 'color_data', toolu.ColorToString(DEFAULT_COLOR_DATA)));
      FShowCmd :=   GetIniIntW(IniFile, IniSection, 'showcmd', sw_shownormal);
    end
    else
    begin
      Caption := FetchValue(AData, 'caption="', '";');
      FCommand := FetchValue(AData, 'command="', '";');
      FParams := FetchValue(AData, 'params="', '";');
      FDir := FetchValue(AData, 'dir="', '";');
      FImageFile := FetchValue(AData, 'image="', '";');
      FImageFile2 := cutafter(FImageFile, ';');
      FImageFile := cut(FImageFile, ';');
      FHide := false;
      FColorData := DEFAULT_COLOR_DATA;
      FShowCmd := 1;
      try FHide := boolean(strtoint(FetchValue(AData, 'hide="', '";')));
      except end;
      try FColorData := strtoint(FetchValue(AData, 'color_data="', '";'));
      except end;
      try FShowCmd := strtoint(FetchValue(AData, 'showcmd="', '";'));
      except end;
    end;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateItem ' + LineEnding + e.message);
  end;

  UpdateItemI;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateItemI;
var
  sfi: TSHFileInfoW;
  pidFolder: PItemIDList;
  csidl: integer;
  pszName: array [0..255] of char;
  //
  ext, params, dir, icon: string;
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;

      // convert CSIDL to GUID or path //
      csidl := CSIDL_ToInt(FCommand);
      if csidl > -1 then
      begin
        OleCheck(SHGetSpecialFolderLocation(0, csidl or CSIDL_FLAG_NO_ALIAS, pidFolder));
        if PIDL_GetDisplayName(nil, pidFolder, SHGDN_FORPARSING, pszName, 255) then FCommand := strpas(pszName);
        PIDL_Free(pidFolder);
        if FileExists(FCommand) or DirectoryExists(FCommand) then FCommand := ZipPath(FCommand)
        else FCaption := '::::'; // assuming this is a PIDL
      end;

      // create PIDL from GUID //
      PIDL_Free(FPIDL);
      if IsPIDLString(FCommand) then FPIDL := PIDL_FromString(FCommand);
      if not assigned(FPIDL) then
        if not FileExists(toolu.UnzipPath(FCommand)) then
          FPIDL := PIDL_GetFromPath(pchar(FCommand));
      FIsPIDL := assigned(FPIDL);
      if FIsPIDL and (FCaption = '::::') then
      begin
        OleCheck(SHGetFileInfoW(pwchar(FPIDL), 0, sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME));
        FCaption := strpas(pwchar(sfi.szDisplayName));
      end;

      // check if this is the shortcut to an executable file
      FIsExecutable := false;
      if not FIsPIDL then
      begin
        FExecutable := toolu.UnzipPath(FCommand);
        ext := ExtractFileExt(FExecutable);
        if SameText(ext, '.appref-ms') then ResolveAppref(FHWnd, FExecutable);
        if SameText(ext, '.lnk') then ResolveLNK(FHWnd, FExecutable, params, dir, icon);
        ext := ExtractFileExt(FExecutable);
        FIsExecutable := SameText(ext, '.exe');
        if not FileExists(FExecutable) and not FIsExecutable then FExecutable := '';
      end;

      // check if this is a dynamic state object //
      CheckIfDynObject;

      // load appropriate image //
      LoadImageI;
    finally
      FUpdating:= false;
    end;

    Redraw;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateItemInternal ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.LoadImageI;
begin
  try if FImage <> nil then GdipDisposeImage(FImage);
  except end;
  FImage := nil;

  if FDynObject then // if a dynamic state object
  begin
    LoadDynObjectImage(FImageFile, FBigItemSize, false, true, FImage, FIW, FIH);
  end
  else
  if FImageFile <> '' then // if custom image file specified
  begin
    LoadImage(UnzipPath(FImageFile), FBigItemSize, false, true, FImage, FIW, FIH);
  end
  else // if no custom image set - load from object itself (PIDL or File)
  begin
    if FIsPIDL then LoadImageFromPIDL(FPIDL, FBigItemSize, false, true, FImage, FIW, FIH)
    else LoadImage(UnzipPath(FCommand), FBigItemSize, false, true, FImage, FIW, FIH);
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TShortcutItem.LoadDynObjectImage(imagefile: string; MaxSize: integer; exact: boolean; default: boolean; var image: pointer; var srcwidth, srcheight: uint);
begin
  try
    if pos('{LANGID}', imagefile) > 0 then
    begin
      imagefile := ReplaceEx(imagefile, '{LANGID}', GetLangIDString(FDynObjectState));
      LoadImage(UnzipPath(imagefile), MaxSize, exact, default, image, srcwidth, srcheight);
      FCaption := GetLangIDName(FDynObjectState);
    end;
    if pos('{VOLUME}', imagefile) > 0 then
    begin
      imagefile := ReplaceEx(imagefile, '{VOLUME}', Mixer.StateString);
      LoadImage(UnzipPath(imagefile), MaxSize, exact, default, image, srcwidth, srcheight);
      FCaption := Mixer.Description;
    end;
    if pos('{NETWORK}', imagefile) > 0 then
    begin
      imagefile := ReplaceEx(imagefile, '{NETWORK}', TNetworks.CStateString);
      LoadImage(UnzipPath(imagefile), MaxSize, exact, default, image, srcwidth, srcheight);
      FCaption := TNetworks.CDescription;
    end;
  except
    on e: Exception do raise Exception.Create('LoadDynObjectImage ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.CheckIfDynObject;
var
  pidFolder: PItemIDList;
begin
  FDynObjectState := 0;
  FDynObject := (pos('{LANGID}', FImageFile) > 0) or (pos('{VOLUME}', FImageFile) > 0) or (pos('{NETWORK}', FImageFile) > 0);
  FDynObjectRecycleBin := false;
  if FIsPIDL then
  begin
    OleCheck(SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET or CSIDL_FLAG_NO_ALIAS, pidFolder));
    FDynObjectRecycleBin := PIDL_GetDisplayName2(pidFolder) = FCommand;
    PIDL_Free(pidFolder);
  end;

  if FDynObject or FDynObjectRecycleBin then DynObjectUpdate;

  // setup update timer
  if FDynObject then SetTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT, 500, nil)
  else
  if FDynObjectRecycleBin then SetTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT, 1000, nil)
  else
    KillTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.DynObjectUpdate;
var
  tempState: integer = 0;
begin
  // if this is a dynamic object
  if FDynObject then
  begin
    if pos('{LANGID}', FImageFile) > 0 then tempState := GetLangID;
    if pos('{VOLUME}', FImageFile) > 0 then tempState := TMixer.CUpdate;
    if pos('{NETWORK}', FImageFile) > 0 then tempState := TNetworks.CUpdate;
  end
  // if this is a Recycle Bin
  else if FDynObjectRecycleBin then tempState := GetRecycleBinState;

  // if 'state' changed
  if FDynObjectState <> tempState then
  begin
    FDynObjectState := tempState;
    LoadImageI;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.cmd(id: TDParam; param: PtrInt): PtrInt;
var
  temp: uint;
  idx: integer;
begin
  try
    result := inherited cmd(id, param);

    case id of
      // parameters //
      gpBigItemSize:
        begin
          if FBigItemSize <= 96 then temp := 96
          else if FBigItemSize <= 128 then temp := 128
          else if FBigItemSize <= 160 then temp := 160
          else if FBigItemSize <= 192 then temp := 192
          else if FBigItemSize <= 256 then temp := 256;
          if temp <> FIW then LoadImageI;
        end;
      gpShowRunningIndicator:
        if FRunning and not boolean(param) then
        begin
          FRunning := false;
          Redraw;
        end;
      gpTaskLivePreviews: FTaskLivePreviews := boolean(param);
      gpTaskThumbSize: FTaskThumbSize := param;
      gpTaskGrouping: FTaskGrouping := boolean(param);
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpSite: if FRunning then Redraw;
      tcThemeChanged: if FRunning then Redraw;
      tcDebugInfo:
        if FAppList.Count > 0 then // log every window info
        begin
          AddLog('---------- ShortcutItem.WindowsInfo');
          AddLog('Caption = ' + AnsiString(FCaption));
          for idx := 0 to FAppList.Count - 1 do LogWindow(THandle(FAppList.Items[idx]));
        end;

      // commands //
      icUpdateRunning: UpdateItemRunningState;
      icDragEnter:
        begin
          FDropIndicator := DII_RUN;
          Redraw;
          result := FDropIndicator;
        end;
      icDragLeave:
        begin
          FDropIndicator := 0;
          Redraw;
        end;

      icFlashTaskWindow:
        if FAppList.IndexOf(pointer(param)) >= 0 then
        begin
          Animate;
          Attention(true);
        end;
    end;

  except
    on e: Exception do raise Exception.Create('ShortcutItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateItemRunningState;
var
  aRunning: boolean;
  appCount: integer;
begin
  if FIsExecutable then
  begin
    appCount := FAppList.Count;
    ProcessHelper.GetProcessWindows(FExecutable, FAppList);
    aRunning := FAppList.Count > 0;
    if (aRunning <> FRunning) or (appCount <> FAppList.Count) then
    begin
      FRunning := aRunning;
      Redraw;
    end;
  end;
end;
//------------------------------------------------------------------------------
// Draw routines ---------------------------------------------------------------
procedure TShortcutItem.AfterDraw;
begin
  if FIsOpen then UpdatePeekWindow;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.DrawOverlay(dst: pointer; x, y, size: integer);
begin
  if assigned(FAppList) then
    if FAppList.Count > 1 then DrawNumberOverlay(dst, x, y, size, FAppList.Count);
  DrawItemIndicator(dst, FDropIndicator, x, y, size, size);
end;
// Draw routines ---------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TShortcutItem.Timer;
begin
  try
    inherited;
    if FFreed or FUpdating then exit;

    // animation //
    if FAnimationProgress > 0 then
    begin
      inc(FAnimationProgress);
      if FAnimationProgress >= FAnimationEnd then FAnimationProgress := 0;
      Redraw;
    end;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.Timer ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Configure;
begin
  TfrmItemProp.Open(ToString, UpdateItem);
end;
//------------------------------------------------------------------------------
function TShortcutItem.ToString: string;
var
  img: string;
begin
  img := FImageFile;
  if FImageFile2 <> '' then img := img + ';' + FImageFile2;
  result:= Make(FHWnd, FCaption, FCommand, FParams, FDir, img, FShowCmd, FColorData, FHide);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
  tickCount: PtrUInt;
begin
  {$ifdef CPU64}
  tickCount := gettickcount64;
  {$else CPU64}
  tickCount := gettickcount;
  {$endif CPU64}

  if button = mbLeft then
  begin
    if FLastMouseUp > tickCount then FLastMouseUp := 0;
    if tickCount - FLastMouseUp > FLaunchInterval then
    begin
      if ssAlt in shift then Exec(eaGroup)
      else
      if ssCtrl in shift then Exec(eaRun)
      else
        Exec(eaDefault);
    end;
    FLastMouseUp := tickCount;
  end;

  if button = mbRight then
  begin
    ClosePeekWindow;
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Exec(action: TExecuteAction);
  procedure Run;
  begin
    if FHide then dockh.DockExecute(FHWnd, '/hide', '', '', 0) else DockletDoAttensionAnimation(FHWnd);
    DockExecuteW(FHWnd, pwchar(FCommand), pwchar(FParams), pwchar(FDir), FShowCmd);
  end;
begin
  if FIsPIDL then Run
  else
  begin
      if FActivateRunning and not (action = eaRun) then
      begin
          if FHide then DockExecute(FHWnd, '/hide', '', '', 0);
          if not ActivateProcessMainWindow(action = eaGroup) then Run;
      end
      else Run;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ActivateProcessMainWindow(group: boolean): boolean;
begin
  result := false;
  if not FIsExecutable then exit;

  ProcessHelper.EnumAppWindows;
  ProcessHelper.GetProcessWindows(FExecutable, FAppList);
  if FAppList.Count = 1 then
  begin
    result := true;
    KillTimer(FHWnd, ID_TIMER_OPEN);
    ProcessHelper.ActivateWindow(THandle(FAppList.First));
  end else
  if FAppList.Count > 1 then
  begin
    result := true;
    if group then
      ProcessHelper.ActivateWindowList(FAppList)
		else begin
      if not TAeroPeekWindow.IsActive then ShowPeekWindow;
    end;
	end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.MouseHeld(button: TMouseButton);
begin
  inherited;
  if button = mbRight then Configure;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.BeforeUndock;
begin
  ClosePeekWindow;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  filename: string;
  msg: TMessage;
  mii: MENUITEMINFO;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  if FDynObjectRecycleBin and (FDynObjectState > 0) then AppendMenuW(FHMenu, MF_STRING, $f005, pwchar(UTF8Decode(XEmptyBin)));
  AppendMenuW(FHMenu, MF_STRING, $f001, pwchar(UTF8Decode(XConfigureIcon)));
  AppendMenuW(FHMenu, MF_STRING, $f003, pwchar(UTF8Decode(XCopy)));
  if CanOpenFolder then AppendMenuW(FHMenu, MF_STRING, $f002, pwchar(UTF8Decode(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING, $f004, pwchar(UTF8Decode(XDeleteIcon)));

  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  if FRunning then
  begin
    AppendMenuW(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f008, pwchar(UTF8Decode(XKillProcess)));
    AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
    if FAppList.Count < 2 then AppendMenuW(FHMenu, MF_STRING, $f007, pwchar(UTF8Decode(XCloseWindow)))
    else begin
      AppendMenuW(FHMenu, MF_STRING, $f007, pwchar(UTF8Decode(XCloseAllWindows)));
      AppendMenuW(FHMenu, MF_STRING, $f009, pwchar(UTF8Decode(XMinimizeRestoreAllWindows)));
    end;
  end;
  AppendMenuW(FHMenu, MF_STRING, $f006, pwchar(UTF8Decode(XRun)));
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f006, false, @mii);
  LME(true);

  // if shell context menu is enabled //
  if FUseShellContextMenus and ((FCommand <> '') or FIsPIDL) then
  begin
    if FIsPIDL then result := shcontextu.ShContextMenu(FHWnd, pt, FPIDL, FHMenu)
    else
    begin
      filename := toolu.UnzipPath(FCommand);
      //if not fileexists(filename) and not directoryexists(filename) then filename := toolu.FindFile(filename);
      if fileexists(filename) or directoryexists(filename) then result := shcontextu.ShContextMenu(FHWnd, pt, filename, FHMenu);
    end;
  end;

  // else, if it is disabled //
  if not result then msg.WParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
var
  idx: integer;
begin
  result := 0;
  LME(false);
  DestroyMenu(FHMenu);
  FHMenu := 0;
  case wParam of // f001 to f020
    $f001: Configure;
    $f002: OpenFolder;
    $f003: toolu.SetClipboard(ToString);
    $f004: Delete;
    $f005: DockExecute(FHWnd, pchar('/emptybin'), nil, nil, 1);
    $f006: Exec(eaRun);
    $f007:
      if FAppList.Count > 0 then
      begin
        for idx := FAppList.Count - 1 downto 0 do
          ProcessHelper.CloseWindow(THandle(FAppList.Items[idx]));
      end;
    $f008: ProcessHelper.Kill(FExecutable);
    $f009: ProcessHelper.ActivateWindowList(FAppList);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.WndMessage(var msg: TMessage);
begin
  if not FFreed then
    with msg do
    begin
        Result := 0;

        // WM_ACTIVATEAPP
        if (msg = WM_ACTIVATEAPP) and (wParam = 0) then ClosePeekWindow;

        // WM_TIMER
        if msg = WM_TIMER then
        begin
          // "OPEN" TIMER
          if wParam = ID_TIMER_OPEN then ShowPeekWindow;
          // update bitbucket
          if wParam = ID_TIMER_UPDATE_SHORTCUT then
            if FDynObject or FDynObjectRecycleBin then DynObjectUpdate;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.BeforeMouseHover(AHover: boolean);
begin
  FHideHint := TAeroPeekWindow.IsActive and (FAppList.Count > 0);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.MouseHover(AHover: boolean);
begin
  if FAppList.Count > 0 then
    if AHover then
    begin
      if TAeroPeekWindow.IsActive then
      begin
        if TAeroPeekWindow.ActivatedBy(FHWnd) then ShowPeekWindow else ShowPeekWindow(100);
      end
      else ShowPeekWindow(800);
    end else begin
      ClosePeekWindow(800);
    end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.ShowPeekWindow(Timeout: cardinal = 0);
var
  pt: windows.TPoint;
begin
  if Timeout > 0 then
  begin
    SetTimer(FHWnd, ID_TIMER_OPEN, Timeout, nil);
    exit;
  end;

  KillTimer(FHWnd, ID_TIMER_OPEN);

  pt := GetScreenRect.TopLeft;
  if (FSite = 1) or (FSite = 3) then inc(pt.x, FSize div 2);
  if (FSite = 0) or (FSite = 2) then inc(pt.y, FSize div 2);
  if FSite = 0 then inc(pt.x, FSize);
  if FSite = 1 then inc(pt.y, FSize);
  case FSite of
    0: inc(pt.x, 5);
    1: inc(pt.y, 5);
    2: dec(pt.x, 5);
    3: dec(pt.y, 5);
  end;
  FHideHint := true;
  UpdateHint;
  TAeroPeekWindow.Open(FHWnd, FAppList, pt.x, pt.y, FSite, FTaskThumbSize, FTaskLivePreviews);
  FIsOpen := true;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.ClosePeekWindow(Timeout: cardinal = 0);
begin
  KillTimer(FHWnd, ID_TIMER_OPEN);
  if FHideHint then
  begin
    FHideHint := false;
    UpdateHint;
  end;
  if FIsOpen then
  begin
    FIsOpen := false;
    TAeroPeekWindow.Close(Timeout);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdatePeekWindow;
var
  pt: windows.TPoint;
begin
  pt := GetScreenRect.TopLeft;
  if (FSite = 1) or (FSite = 3) then inc(pt.x, FSize div 2);
  if (FSite = 0) or (FSite = 2) then inc(pt.y, FSize div 2);
  if FSite = 0 then inc(pt.x, FSize);
  if FSite = 1 then inc(pt.y, FSize);
  case FSite of
    0: inc(pt.x, 5);
    1: inc(pt.y, 5);
    2: dec(pt.x, 5);
    3: dec(pt.y, 5);
  end;
  TAeroPeekWindow.SetPosition(pt.x, pt.y);
end;
//------------------------------------------------------------------------------
function TShortcutItem.CanOpenFolder: boolean;
var
  strFile: string;
begin
  strFile := toolu.UnzipPath(FCommand);
  result := fileexists(strFile) or directoryexists(strFile);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.OpenFolder;
var
  strFile: string;
begin
  strFile := ExtractFilePath(toolu.UnzipPath(FCommand));
  DockExecute(FHWnd, pchar(strFile), nil, nil, sw_shownormal);
end;
//------------------------------------------------------------------------------
function TShortcutItem.RegisterProgram: string;
begin
  result := FExecutable;
end;
//------------------------------------------------------------------------------
function TShortcutItem.DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean;
var
  ext: string;
begin
  result := not FFreed;
  if result then
  begin
    ext := AnsiLowerCase(ExtractFileExt(filename));
    if (ext = '.png') or (ext = '.ico') then
    begin
      FImageFile := toolu.ZipPath(filename);
      FColorData := DEFAULT_COLOR_DATA;
      UpdateItemI;
    end
    else
    begin
      if not FIsPIDL then DockExecute(FHWnd, pchar(FCommand), pchar('"' + filename + '"'), nil, 1);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Save(szIni: pchar; szIniGroup: pchar);
var
  img: WideString;
  section, ini: WideString;
begin
  if FFreed or (szIni = nil) or (szIniGroup = nil) then exit;

  section := strpas(szIniGroup);
  ini := strpas(szIni);
  WritePrivateProfileString(szIniGroup, nil, nil, szIni);
  WritePrivateProfileString(szIniGroup, 'class', 'shortcut', szIni);
  if not FDynObject then
    if caption <> '' then WriteIniStringW(ini, section, 'caption', Caption);
  if FCommand <> '' then  WriteIniStringW(ini, section, 'command', FCommand);
  if FParams <> '' then   WriteIniStringW(ini, section, 'params', FParams);
  if FDir <> '' then      WriteIniStringW(ini, section, 'dir', FDir);
  if FImageFile <> '' then
  begin
    img := FImageFile;
    if FImageFile2 <> '' then img := img + ';' + FImageFile2;
    WriteIniStringW(ini, section, 'image', img);
  end;
  if FShowCmd <> sw_shownormal then        WriteIniStringW(ini, section, 'showcmd', inttostr(FShowCmd));
  if FColorData <> DEFAULT_COLOR_DATA then WriteIniStringW(ini, section, 'color_data', toolu.ColorToString(FColorData));
  if FHide then                            WriteIniStringW(ini, section, 'hide', '1');
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
class function TShortcutItem.Make(wnd: HWND; ACaption: WideString; ACommand, AParams, ADir, AImage: string;
  AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
begin
  result := 'class="shortcut";';
  result := result + 'hwnd="' + inttostr(wnd) + '";';
  if ACaption <> '' then result := result + 'caption="' + AnsiString(ACaption) + '";';
  if ACommand <> '' then result := result + 'command="' + ACommand + '";';
  if AParams <> '' then result := result + 'params="' + AParams + '";';
  if ADir <> '' then result := result + 'dir="' + ADir + '";';
  if AImage <> '' then result := result + 'image="' + AImage + '";';
  if AShowCmd <> 1 then result := result + 'showcmd="' + inttostr(AShowCmd) + '";';
  if AColorData <> DEFAULT_COLOR_DATA then result := result + 'color_data="' + toolu.ColorToString(AColorData) + '";';
  if AHide then result := result + 'hide="1";';
end;
//------------------------------------------------------------------------------
class function TShortcutItem.FromFile(filename: string): string;
var
  fcaption: WideString;
  fparams, fdir, ficon, ext: string;
begin
  result := '';
  if IsGUID(filename) or IsPIDLString(filename) then
  begin
    result := TShortcutItem.Make(0, '::::', filename, '', '', '', 1);
    exit
  end;

  fparams := '';
  fdir := '';
  ficon := '';
  ext := AnsiLowerCase(ExtractFileExt(filename));

  if DirectoryExists(filename) then fcaption := WideString(filename)
  else fcaption := WideString(ChangeFileExt(ExtractFilename(filename), ''));
  if ext = '.exe' then fdir := ExcludeTrailingPathDelimiter(ExtractFilePath(filename));

  result := TShortcutItem.Make(0, fcaption, ZipPath(filename), ZipPath(fparams), ZipPath(fdir), ZipPath(ficon), 1);
end;
//------------------------------------------------------------------------------
end.
 
