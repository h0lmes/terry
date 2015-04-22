unit scitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, Math, ShellAPI, ComObj, ShlObj,
  IniFiles, GDIPAPI, gfx, PIDL, ShContextU, declu, dockh, customdrawitemu,
  toolu, processhlp, aeropeeku, mixeru, networksu;

type

  { TShortcutItem }

  TShortcutItem = class(TCustomDrawItem)
  private
    FCommand: string;
    FParams: string;
    FDir: string;
    FImageFile: string;
    FImageFile2: string;
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
    FLastMouseUp: cardinal;
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
    procedure DrawWindowsCount(dst: pointer; winList: TFPList; x, y, Size: integer);
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
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Timer; override;
    procedure Configure; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function RegisterProgram: string; override;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

implementation
uses frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
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
  ini: TIniFile;
begin
  if FFreed then exit;

  try
    IniFile := FetchValue(AData, 'inifile="', '";');
    IniSection := FetchValue(AData, 'inisection="', '";');

    if (length(IniFile) > 0) and (length(IniSection) > 0) then
    begin
      ini := TIniFile.Create(IniFile);
      caption := ini.ReadString(IniSection, 'caption', '');
      FCommand := ini.ReadString(IniSection, 'command', '');
      FParams := ini.ReadString(IniSection, 'params', '');
      FDir := ini.ReadString(IniSection, 'dir', '');
      FImageFile := ini.ReadString(IniSection, 'image', '');
      FImageFile2 := cutafter(FImageFile, ';');
      FImageFile := cut(FImageFile, ';');
      FHide := boolean(ini.ReadInteger(IniSection, 'hide', 0));
      FColorData := toolu.StringToColor(ini.ReadString(IniSection, 'color_data', toolu.ColorToString(DEFAULT_COLOR_DATA)));
      FShowCmd := ini.ReadInteger(IniSection, 'showcmd', sw_shownormal);
      ini.free;
    end
    else
    begin
      caption := FetchValue(AData, 'caption="', '";');
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
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateItem'#10#13 + e.message);
  end;

  UpdateItemI;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateItemI;
var
  sfi: TSHFileInfoA;
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
        PIDL_GetDisplayName(nil, pidFolder, SHGDN_FORPARSING, pszName, 255);
        PIDL_Free(pidFolder);
        FCommand := strpas(pszName);
        if FileExists(FCommand) or DirectoryExists(FCommand) then FCommand := ZipPath(FCommand)
        else FCaption := '::::'; // assuming this is a PIDL
      end;

      // create PIDL from GUID //
      PIDL_Free(FPIDL);
      if IsGUID(FCommand) then FPIDL := PIDL_GetFromPath(pchar(FCommand));
      if IsPIDLString(FCommand) then FPIDL := PIDL_FromString(FCommand);
      FIsPIDL := assigned(FPIDL);
      if FIsPIDL and (FCaption = '::::') then
      begin
        SHGetFileInfoA(pchar(FPIDL), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
        FCaption := sfi.szDisplayName;
      end;

      // check if this is the shortcut to an executable file
      FIsExecutable := false;
      if not FIsPIDL then
      begin
        FExecutable := toolu.UnzipPath(FCommand);
        if not FileExists(FExecutable) then FExecutable := ''
        else
        begin
          ext := ExtractFileExt(FExecutable);
          if SameText(ext, '.appref-ms') then ResolveAppref(FHWnd, FExecutable);
          if SameText(ext, '.lnk') then ResolveLNK(FHWnd, FExecutable, params, dir, icon);
        end;
        FIsExecutable := SameText(ExtractFileExt(FExecutable), '.exe');
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
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateItemInternal'#10#13 + e.message);
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
  else // if no custom image set
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
      imagefile := ReplaceEx(imagefile, '{NETWORK}', Networks.StateString);
      LoadImage(UnzipPath(imagefile), MaxSize, exact, default, image, srcwidth, srcheight);
      FCaption := Networks.Description;
    end;
  except
    on e: Exception do raise Exception.Create('LoadDynObjectImage'#10#13 + e.message);
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
  tempState: integer;
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
function TShortcutItem.cmd(id: TGParam; param: integer): integer;
var
  b: boolean;
  temp: uint;
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
          if temp <> FIW then UpdateItemI;
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
    end;

  except
    on e: Exception do raise Exception.Create('ShortcutItem.Cmd'#10#13 + e.message);
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
      if appCount < FAppList.Count then Attention(FRunning);
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
  DrawWindowsCount(dst, FAppList, x, y, size);
  DrawItemIndicator(dst, FDropIndicator, x, y, size, size);
end;
// Draw routines ---------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TShortcutItem.DrawWindowsCount(dst: pointer; winList: TFPList; x, y, Size: integer);
var
  brush, family, hfont, format, path: Pointer;
  tmpItemSize: integer;
  rect: GDIPAPI.TRectF;
begin
  if assigned(winList) then
      if winList.Count > 1 then
      begin
        GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
        GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
        // background
        tmpItemSize := max(FItemSize, 40);
        if winList.Count > 99 then rect.Width := round(tmpItemSize * 9 / 12)
        else if winList.Count > 9 then rect.Width := round(tmpItemSize * 7 / 12)
        else rect.Width := round(tmpItemSize * 5 / 12);
        rect.Height := round(tmpItemSize * 5 / 12);
        rect.X := x + Size - rect.Width + 5;
        rect.Y := y - 5;
        GdipCreatePath(FillModeWinding, path);
        AddPathRoundRect(path, rect, rect.Height / 2);
        GdipCreateSolidFill($ffff0000, brush); // red indicator background
        GdipFillPath(dst, brush, path);
        GdipDeleteBrush(brush);
        GdipDeletePath(path);
        // number
        GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, family);
        GdipCreateFont(family, tmpItemSize * 5 div 16, 1, 2, hfont);
        GdipCreateSolidFill($ffffffff, brush);
        GdipCreateStringFormat(0, 0, format);
        GdipSetStringFormatAlign(format, StringAlignmentCenter);
        GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
        GdipDrawString(dst, PWideChar(WideString(inttostr(winList.Count))), -1, hfont, @rect, format, brush);
        GdipDeleteStringFormat(format);
        GdipDeleteBrush(brush);
        GdipDeleteFont(hfont);
        GdipDeleteFontFamily(family);
      end;
end;
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
    on e: Exception do raise Exception.Create('ShortcutItem.Timer'#10#13 + e.message);
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
begin
  if button = mbLeft then
  begin
    if (abs(gettickcount - FLastMouseUp) > FLaunchInterval) then
    begin
      if ssAlt in shift then Exec(eaGroup)
      else
      if ssCtrl in shift then Exec(eaRun)
      else
        Exec(eaDefault);
    end;
    FLastMouseUp := gettickcount;
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
    DockExecute(FHWnd, pchar(FCommand), pchar(FParams), pchar(FDir), FShowCmd);
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
var
  pt: windows.TPoint;
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
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f006, false, @mii);
  if FDynObjectRecycleBin and (FDynObjectState > 0) then AppendMenu(FHMenu, MF_STRING, $f005, pchar(UTF8ToAnsi(XEmptyBin)));
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  if CanOpenFolder then AppendMenu(FHMenu, MF_STRING, $f002, PChar(UTF8ToAnsi(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));

  AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
  if FRunning then
  begin
    AppendMenu(FHMenu, MF_STRING + ifthen(FIsExecutable, 0, MF_DISABLED), $f008, pchar(UTF8ToAnsi(XKillProcess)));
    AppendMenu(FHMenu, MF_SEPARATOR, 0, pchar('-'));
    if FAppList.Count < 2 then AppendMenu(FHMenu, MF_STRING, $f007, pchar(UTF8ToAnsi(XCloseWindow)))
    else AppendMenu(FHMenu, MF_STRING, $f007, pchar(UTF8ToAnsi(XCloseAllWindows)));
  end;
  AppendMenu(FHMenu, MF_STRING, $f006, pchar(UTF8ToAnsi(XRun)));
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
  if not result then msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
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
    //$f009..$f020: ;
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
  if FAttention then Attention(false);

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
function TShortcutItem.DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean;
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
  img: string;
begin
  if FFreed or (szIni = nil) or (szIniGroup = nil) then exit;

  WritePrivateProfileString(szIniGroup, nil, nil, szIni);
  WritePrivateProfileString(szIniGroup, 'class', 'shortcut', szIni);
  if not FDynObject then
    if caption <> '' then WritePrivateProfileString(szIniGroup, 'caption', pchar(caption), szIni);
  if FCommand <> '' then WritePrivateProfileString(szIniGroup, 'command', pchar(FCommand), szIni);
  if FParams <> '' then WritePrivateProfileString(szIniGroup, 'params', pchar(FParams), szIni);
  if FDir <> '' then WritePrivateProfileString(szIniGroup, 'dir', pchar(FDir), szIni);
  if FImageFile <> '' then
  begin
    img := FImageFile;
    if FImageFile2 <> '' then img := img + ';' + FImageFile2;
    WritePrivateProfileString(szIniGroup, 'image', pchar(img), szIni);
  end;
  if FShowCmd <> sw_shownormal then WritePrivateProfileString(szIniGroup, 'showcmd', pchar(inttostr(FShowCmd)), szIni);
  if FColorData <> DEFAULT_COLOR_DATA then WritePrivateProfileString(szIniGroup, 'color_data', pchar(toolu.ColorToString(FColorData)), szIni);
  if FHide then WritePrivateProfileString(szIniGroup, 'hide', '1', szIni);
end;
//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
class function TShortcutItem.Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
  AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
begin
  result := 'class="shortcut";';
  result := result + 'hwnd="' + inttostr(AHWnd) + '";';
  if ACaption <> '' then result := result + 'caption="' + ACaption + '";';
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
  fcaption, fparams, fdir, ficon, ext: string;
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

  if DirectoryExists(filename) then fcaption := filename
  else fcaption := ChangeFileExt(ExtractFilename(filename), '');
  if ext = '.exe' then fdir := ExcludeTrailingPathDelimiter(ExtractFilePath(filename));

  result := TShortcutItem.Make(0, fcaption, ZipPath(filename), ZipPath(fparams), ZipPath(fdir), ZipPath(ficon), 1);
end;
//------------------------------------------------------------------------------
end.
 
