unit scitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, ComObj, ShlObj,
  IniFiles, GDIPAPI, gdip_gfx, PIDL, ShContextU, declu, dockh, customitemu, toolu, processhlp;

type

  { TShortcutItem }

  TShortcutItem = class(TCustomItem)
  private
    FCommand: string;
    FParams: string;
    FDir: string;
    FImageFile: string;
    FImageFile2: string;
    FShowCmd: integer;
    FHide: boolean;
    FColorData: integer;
    FUseShellContextMenus: boolean;
    FRunning: boolean;
    FIndicator: Pointer;
    FIndicatorW: integer;
    FIndicatorH: integer;
    is_pidl: boolean;
    apidl: PItemIDList;
    LastMouseUp: cardinal;
    FBitBucket: boolean;
    FBitBucketFiles: integer;
    procedure UpdateItemI;
    procedure UpdateItemRunningState;
    procedure LoadImageI;
    procedure BitBucketCheck;
    procedure BitBucketUpdate;
    procedure UpdateIndicator;
    procedure DrawIndicator(dst: Pointer);
    procedure Exec;
    function ActivateProcessMainWindow: boolean;
    function ContextMenu(pt: Windows.TPoint): boolean;
  public
    procedure UpdateItem(AData: string);
    //
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
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
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

implementation
uses themeu, frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;

  LastMouseUp:= 0;
  FCommand:= '';
  FParams:= '';
  FDir:= '';
  FImageFile:= '';
  FColorData:= DEFAULT_COLOR_DATA;
  FShowCmd:= 0;
  FHide:= false;
  FRunning:= false;

  UpdateItem(AData);
  UpdateIndicator;
end;
//------------------------------------------------------------------------------
destructor TShortcutItem.Destroy;
begin
  FFreed := true;
  KillTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT);
  try GdipDisposeImage(FImage);
  except end;
  try if is_pidl then PIDL_Free(apidl);
  except end;
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
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;

      // convert CSIDL to path //
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
      PIDL_Free(apidl);
      if IsGUID(FCommand) then apidl := PIDL_GetFromPath(pchar(FCommand));
      if IsPIDLString(FCommand) then apidl := PIDL_FromString(FCommand);
      is_pidl := assigned(apidl);
      if is_pidl and (FCaption = '::::') then
      begin
        SHGetFileInfoA(pchar(apidl), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
        FCaption := sfi.szDisplayName;
      end;

      // check if it is BITBUCKET //
      BitBucketCheck;

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

  if FImageFile <> '' then // if custom image set
  begin
    if (FImageFile2 <> '') and (FBitBucketFiles > 0) then // if bitbucket is full and appropriate image exists
       LoadImage(FImageFile2, FBigItemSize, false, true, FImage, FIW, FIH)
    else // if not a bitbucket or it is empty or no image for full bitbucket
      LoadImage(FImageFile, FBigItemSize, false, true, FImage, FIW, FIH);
  end
  else // if no custom image
  begin
    if is_pidl then LoadImageFromPIDL(apidl, FBigItemSize, false, true, FImage, FIW, FIH)
    else LoadImage(FCommand, FBigItemSize, false, true, FImage, FIW, FIH);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.BitBucketCheck;
var
  psfDesktop: IShellFolder;
  psfFolder: IShellFolder;
  pidFolder, pidChild: PItemIDList;
  pEnumList: IEnumIDList;
  celtFetched: ULONG;
  ext: string;
begin
  FBitBucket := false;
  if is_pidl then
  begin
    OleCheck(SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET or CSIDL_FLAG_NO_ALIAS, pidFolder));
    FBitBucket := PIDL_GetDisplayName2(pidFolder) = FCommand;
    if FBitBucket then
    begin
      OleCheck(SHGetDesktopFolder(psfDesktop));
      OleCheck(psfDesktop.BindToObject(pidFolder, nil, IID_IShellFolder, psfFolder));
      OleCheck(psfFolder.EnumObjects(0, SHCONTF_NONFOLDERS or SHCONTF_FOLDERS, pEnumList));
      FBitBucketFiles := 0;
      if pEnumList.Next(1, pidChild, celtFetched) = NOERROR then
      begin
        inc(FBitBucketFiles);
        PIDL_Free(pidChild);
      end;
    end;
    PIDL_Free(pidFolder);
  end;

  FImageFile2 := '';
  if FBitBucket then // if this shortcut is a bitbucket
  begin
    SetTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT, 5000, nil); // set timer for update

    if FImageFile <> '' then // search an image for full bitbucket
    begin
      ext := ExtractFileExt(FImageFile);
      FImageFile2 := ChangeFileExt(FImageFile, ' full' + ext);
      if not FileExists(FImageFile2) then FImageFile2 := ChangeFileExt(FImageFile, '_full' + ext);
      if not FileExists(FImageFile2) then FImageFile2 := ChangeFileExt(FImageFile, '-full' + ext);
      if not FileExists(FImageFile2) then FImageFile2 := '';
    end;
  end
  else // if this shortcut is not a bitbucket
    KillTimer(FHWnd, ID_TIMER_UPDATE_SHORTCUT); // remove timer
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.BitBucketUpdate;
var
  psfDesktop: IShellFolder;
  psfFolder: IShellFolder;
  pidFolder, pidChild: PItemIDList;
  pEnumList: IEnumIDList;
  celtFetched: ULONG;
  qty: integer;
  temp: string;
begin
  OleCheck(SHGetDesktopFolder(psfDesktop));
  OleCheck(SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET or CSIDL_FLAG_NO_ALIAS, pidFolder));
  OleCheck(psfDesktop.BindToObject(pidFolder, nil, IID_IShellFolder, psfFolder));
  OleCheck(psfFolder.EnumObjects(0, SHCONTF_NONFOLDERS or SHCONTF_FOLDERS, pEnumList));
  qty := 0;
  if pEnumList.Next(1, pidChild, celtFetched) = NOERROR then
  begin
    inc(qty);
    PIDL_Free(pidChild);
  end;
  PIDL_Free(pidFolder);

  // if quantity changed
  if FBitBucketFiles <> qty then
  begin
    FBitBucketFiles := qty;
    LoadImageI;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateIndicator;
begin
  // make a local copy //
  // gdiplus does not like invalid pointers //
  if not FFreed then
  try
    if FIndicator <> nil then GdipDisposeImage(FIndicator);
    FIndicatorW := theme.Indicator.W and $ffff;
    FIndicatorH := theme.Indicator.H and $ffff;
    GdipCloneBitmapAreaI(0, 0, FIndicatorW, FIndicatorH, PixelFormat32bppPARGB, theme.Indicator.Image, FIndicator);
    if FRunning then Redraw;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateIndicator'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.UpdateItemRunningState;
var
  b: boolean;
begin
  if length(FCommand) > 0 then
  begin
    b := ProcessHelper.FullNameExists(UnzipPath(FCommand));
    if b and (FIndicator = nil) then UpdateIndicator;
    if b <> FRunning then
    begin
      FRunning:= b;
      Redraw;
    end;
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
        begin
          if FRunning and not boolean(param) then
          begin
            FRunning:= false;
            Redraw;
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpSite: if FIndicator <> nil then UpdateIndicator;
      tcThemeChanged: if FIndicator <> nil then UpdateIndicator;

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
// set position, size, repaint window //
procedure TShortcutItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  hattr, brush: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  animation_offset_x, animation_offset_y, animation_size: integer;
begin
  try
    if FFreed or FUpdating or (FFloating and not AForce)
       or ((Fx = Ax) and (Fy = Ay) and (FSize = ASize) and (FShowItem = AShowItem) and not AForce and not need_dock) then exit;
    animation_offset_x := 0;
    animation_offset_y := 0;
    animation_size := 0;

    // set position //
    try
      ItemRect := GetRectFromSize(ASize);
      Fx := Ax;
      Fy := Ay;
      FShowItem := AShowItem;
      if need_dock then
      begin
        Ax := FxDocking;
        Ay := FyDocking;
      end;
      xReal := Ax - ItemRect.Left;
      yReal := Ay - ItemRect.Top;

      // bounce animation //
      if FAnimationProgress > 0 then
        if (FAnimationType >= 2) and (FAnimationType <= 4) then
        begin
          animation_offset_x := FAnimationProgress mod 30;
          if animation_offset_x > 15 then animation_offset_x := 30 - animation_offset_x;
          animation_offset_x := round(anim_bounce[animation_offset_x] * min(FBorder, 40));
          if (FSite = 0) or (FSite = 2) then dec(yReal, animation_offset_x);
        end;

      if (FSize = ASize) and not AForce then
      begin
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem)
        else SetWindowPos(FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        UpdateHint(xReal, yReal);
        exit;
      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW <> 0 then exit;
      UpdateHint(xReal, yReal);
    except
      on e: Exception do raise Exception.Create('SetPosition'#10#13 + e.message);
    end;

    // init bitmap //
    try
      bmp.topleft.x := xReal;
      bmp.topleft.y := yReal;
      bmp.width := FSize + ItemRect.Left * 2;
      bmp.height := FSize + ItemRect.Top * 2;
      if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
      GdipCreateFromHDC(bmp.dc, dst);
      if not assigned(dst) then raise Exception.Create('CreateGraphics failed');
      GdipSetCompositingMode(dst, CompositingModeSourceOver);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
      GdipDeleteBrush(brush);

      xBitmap := 0;
      yBitmap := 0;

      if FAnimationProgress > 0 then
      begin
        // rotate //
        if FAnimationType = 1 then
        begin
          dec(xBitmap, ItemRect.Left);
          dec(yBitmap, ItemRect.Top);
          dec(xBitmap, FSize div 2);
          dec(yBitmap, FSize div 2);
          GdipTranslateWorldTransform(dst, -xBitmap, -yBitmap, MatrixOrderPrepend);
          GdipRotateWorldTransform(dst, FAnimationProgress * 6, MatrixOrderPrepend);
        end;
        // bounce //
        if (FAnimationType >= 2) and (FAnimationType <= 4) then
        begin
          animation_offset_x := FAnimationProgress mod 30;
          if animation_offset_x > 15 then animation_offset_x := 30 - animation_offset_x;
          animation_offset_x := round(anim_bounce[animation_offset_x] * min(FBorder, 40));
          if FSite = 1 then inc(yBitmap, animation_offset_x)
          else if FSite = 3 then dec(yBitmap, animation_offset_x);
        end;
        // quake //
        if FAnimationType = 5 then
        begin
          if FAnimationProgress mod 2 = 1 then exit;
          animation_offset_x := random(FItemSize div 3) - FItemSize div 6;
          animation_offset_y := random(FItemSize div 3) - FItemSize div 6;
          inc(xBitmap, animation_offset_x);
          inc(yBitmap, animation_offset_y);
        end;
        // swing //
        if FAnimationType = 6 then
        begin
          dec(xBitmap, ItemRect.Left);
          dec(yBitmap, ItemRect.Top);
          dec(xBitmap, FSize div 2);
          dec(yBitmap, FSize div 2);
          GdipTranslateWorldTransform(dst, -xBitmap, -yBitmap, MatrixOrderPrepend);
          GdipRotateWorldTransform(dst, sin(FAnimationProgress * 6) * 30, MatrixOrderPrepend);
        end;
        // vibrate //
        if FAnimationType = 7 then
        begin
          animation_size := round(sin(FAnimationProgress * 3) * 6);
          dec(xBitmap, animation_size div 2);
          dec(yBitmap, animation_size div 2);
        end;
        // zoom //
        if FAnimationType = 8 then
        begin
          animation_size := round(sin(FAnimationProgress * 6) * 10);
          dec(xBitmap, animation_size div 2);
          dec(yBitmap, animation_size div 2);
        end;
      end;

      inc(xBitmap, ItemRect.Left);
      inc(yBitmap, ItemRect.Top);
    except
      on e: Exception do raise Exception.Create('InitBitmap'#10#13 + e.message);
    end;

    // draw icon //
    CreateColorAttributes(FColorData, FSelected, hattr);
    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize + animation_size, FSize + animation_size, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if hattr <> nil then GdipDisposeImageAttributes(hattr);

    // drop indicator
    DrawItemIndicator(dst, FDropIndicator, xBitmap, yBitmap, FSize, FSize);

    ////
    if FAnimationProgress > 0 then GdipResetWorldTransform(dst);
    if FReflection and (FReflectionSize > 0) and not FFloating then
      BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
    if FRunning then DrawIndicator(dst);
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));

    // cleanup //
    DeleteGraphics(dst);
    DeleteBitmap(bmp);

  except
    on e: Exception do raise Exception.Create('ShortcutItem.Draw(' + FCaption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.DrawIndicator(dst: Pointer);
var
  xBitmap, yBitmap: integer;
  ItemRect: windows.TRect;
begin
  try
    if FIndicator = nil then exit;
    ItemRect := Rect;

    xBitmap := ItemRect.Left + (FSize - FIndicatorW) div 2;
    yBitmap := ItemRect.Bottom - FIndicatorH div 2;
    if FSite = 0 then
    begin
      xBitmap := ItemRect.Left - FIndicatorW div 2;
      yBitmap := ItemRect.Top + (FSize - FIndicatorH) div 2;
    end
    else
    if FSite = 1 then
    begin
      xBitmap := ItemRect.Left + (FSize - FIndicatorW) div 2;
      yBitmap := ItemRect.Top - FIndicatorH div 2;
    end
    else
    if FSite = 2 then
    begin
      xBitmap := ItemRect.Right - FIndicatorW div 2;
      yBitmap := ItemRect.Top + (FSize - FIndicatorH) div 2;
    end;

    GdipDrawImageRectRectI(dst, FIndicator, xBitmap, yBitmap, FIndicatorW, FIndicatorH, 0, 0, FIndicatorW, FIndicatorH, UnitPixel, nil, nil, nil);
  except
    on e: Exception do raise Exception.Create('DrawIndicator'#10#13 + e.message);
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
begin
  result:= Make(FHWnd, FCaption, FCommand, FParams, FDir, FImageFile, FShowCmd, FColorData, FHide);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
begin
  if button = mbLeft then
  begin
    if (abs(gettickcount - LastMouseUp) > FLaunchInterval) then Exec;
    LastMouseUp := gettickcount;
  end;

  if button = mbRight then
  begin
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.MouseHeld(button: TMouseButton);
begin
  inherited;
  if button = mbRight then Configure;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  filename: string;
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  if FBitBucketFiles > 0 then AppendMenu(FHMenu, MF_STRING, $f005, pchar(UTF8ToAnsi('Empty bin')));
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  if CanOpenFolder then AppendMenu(FHMenu, MF_STRING, $f002, PChar(UTF8ToAnsi(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  // if shell context menu is enabled //
  if FUseShellContextMenus and ((FCommand <> '') or is_pidl) then
  begin
    if is_pidl then result := shcontextu.ShContextMenu(FHWnd, pt, apidl, FHMenu)
    else
    begin
      filename := toolu.UnzipPath(FCommand);
      if not fileexists(filename) and not directoryexists(filename) then filename := toolu.FindFile(filename);
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
    $f006..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.WndMessage(var msg: TMessage);
begin
  with msg do
  begin
      Result := 0;

      if (Msg = WM_TIMER) and (wParam = ID_TIMER_UPDATE_SHORTCUT) then
      begin
        if FBitBucket then BitBucketUpdate;
      end;
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Exec;
var
  sei: TShellExecuteInfo;
begin
  if is_pidl then
  begin
    if FHide then dockh.DockExecute(FHWnd, '/hide', '', '', 0)
    else DockletDoAttensionAnimation(FHWnd);
    sei.cbSize := sizeof(sei);
    sei.lpIDList := apidl;
    sei.Wnd := FHWnd;
    sei.nShow := 1;
    sei.lpVerb := 'open';
    sei.lpFile := nil;
    sei.lpParameters := nil;
    sei.lpDirectory := nil;
    sei.fMask := SEE_MASK_IDLIST;
    ShellExecuteEx(@sei);
  end else
  begin
    if FActivateRunning and (GetAsyncKeystate(17) >= 0) then
    begin
      if FHide then DockExecute(FHWnd, '/hide', '', '', 0);
      if not ActivateProcessMainWindow then
      begin
        if not FHide then DockletDoAttensionAnimation(FHWnd);
        DockExecute(FHWnd, pchar(FCommand), pchar(FParams), pchar(FDir), FShowCmd);
      end;
    end else begin
      if FHide then DockExecute(FHWnd, '/hide', '', '', 0) else DockletDoAttensionAnimation(FHWnd);
      DockExecute(FHWnd, pchar(FCommand), pchar(FParams), pchar(FDir), FShowCmd);
    end;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ActivateProcessMainWindow: boolean;
begin
  try
    result := false;
    LME(true);
    result := ProcessHelper.ActivateProcessMainWindow(UnzipPath(FCommand), FHWnd, ScreenRect, FSite);
  finally
    LME(false);
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.CanOpenFolder: boolean;
var
  _file: string;
begin
  _file := toolu.UnzipPath(FCommand);
  if not fileexists(_file) or not directoryexists(_file) then _file := ExtractFilePath(toolu.FindFile(_file));
  result := fileexists(_file) or directoryexists(_file);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.OpenFolder;
var
  _file: string;
begin
  _file := toolu.UnzipPath(FCommand);
  if not fileexists(_file) or not directoryexists(_file)
  then _file := ExtractFilePath(toolu.FindFile(_file))
  else _file := ExtractFilePath(_file);
  DockExecute(FHWnd, pchar(_file), nil, nil, sw_shownormal);
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
      if not is_pidl then DockExecute(FHWnd, pchar(FCommand), pchar('"' + filename + '"'), nil, 1);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Save(szIni: pchar; szIniGroup: pchar);
begin
  if FFreed or (szIni = nil) or (szIniGroup = nil) then exit;

  WritePrivateProfileString(szIniGroup, nil, nil, szIni);
  WritePrivateProfileString(szIniGroup, 'class', 'shortcut', szIni);
  if caption <> '' then WritePrivateProfileString(szIniGroup, 'caption', pchar(caption), szIni);
  if FCommand <> '' then WritePrivateProfileString(szIniGroup, 'command', pchar(FCommand), szIni);
  if FParams <> '' then WritePrivateProfileString(szIniGroup, 'params', pchar(FParams), szIni);
  if FDir <> '' then WritePrivateProfileString(szIniGroup, 'dir', pchar(FDir), szIni);
  if FImageFile <> '' then WritePrivateProfileString(szIniGroup, 'image', pchar(FImageFile), szIni);
  if FShowCmd <> sw_shownormal then WritePrivateProfileString(szIniGroup, 'showcmd', pchar(inttostr(FShowCmd)), szIni);
  if FColorData <> DEFAULT_COLOR_DATA then WritePrivateProfileString(szIniGroup, 'color_data', pchar(toolu.ColorToString(FColorData)), szIni);
  if FHide then WritePrivateProfileString(szIniGroup, 'hide', '1', szIni);
end;
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
 
