unit scitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, ComObj, ShlObj,
  IniFiles, GDIPAPI, gdip_gfx, PIDL, ShContextU, declu, customitemu, processhlp;

type

  { TShortcutItem }

  TShortcutItem = class(TCustomItem)
  private
    command: string;
    params: string;
    dir: string;
    FImageFile: string;
    FImageFile2: string;
    showcmd: integer;
    hide: boolean;
    FUseShellContextMenus: boolean;
    FRunning: boolean;
    FIndicator: Pointer;
    FIndicatorW: integer;
    FIndicatorH: integer;
    is_pidl: boolean;
    apidl: PItemIDList;
    color_data: integer;
    LastMouseUp: cardinal;
    FBitBucket: boolean;
    FBitBucketFiles: integer;
    procedure UpdateItemI;
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
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Timer; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    //
    class function Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; color_data: integer = DEFAULT_COLOR_DATA; hide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

var window_list: TFPList;

implementation
uses dockh, themeu, toolu, frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;

  LastMouseUp:= 0;
  command:= '';
  params:= '';
  dir:= '';
  FImageFile:= '';
  color_data:= DEFAULT_COLOR_DATA;
  showcmd:= 0;
  hide:= false;
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
      command := ini.ReadString(IniSection, 'command', '');
      params := ini.ReadString(IniSection, 'params', '');
      dir := ini.ReadString(IniSection, 'dir', '');
      FImageFile := ini.ReadString(IniSection, 'image', '');
      hide := boolean(ini.ReadInteger(IniSection, 'hide', 0));
      color_data := toolu.StringToColor(ini.ReadString(IniSection, 'color_data', toolu.ColorToString(DEFAULT_COLOR_DATA)));
      showcmd := ini.ReadInteger(IniSection, 'showcmd', sw_shownormal);
      ini.free;
    end
    else
    begin
      caption := FetchValue(AData, 'caption="', '";');
      command := FetchValue(AData, 'command="', '";');
      params := FetchValue(AData, 'params="', '";');
      dir := FetchValue(AData, 'dir="', '";');
      FImageFile := FetchValue(AData, 'image="', '";');
      hide := false;
      color_data := DEFAULT_COLOR_DATA;
      showcmd := 1;
      try hide := boolean(strtoint(FetchValue(AData, 'hide="', '";')));
      except end;
      try color_data := strtoint(FetchValue(AData, 'color_data="', '";'));
      except end;
      try showcmd := strtoint(FetchValue(AData, 'showcmd="', '";'));
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
      csidl := CSIDL_ToInt(command);
      if csidl > -1 then
      begin
        OleCheck(SHGetSpecialFolderLocation(0, csidl or CSIDL_FLAG_NO_ALIAS, pidFolder));
        PIDL_GetDisplayName(nil, pidFolder, SHGDN_FORPARSING, pszName, 255);
        PIDL_Free(pidFolder);
        command := strpas(pszName);
        if FileExists(command) or DirectoryExists(command) then command := ZipPath(command)
        else FCaption := '::::'; // assuming it is a PIDL
      end;

      // create PIDL from GUID //
      PIDL_Free(apidl);
      if IsGUID(command) then apidl := PIDL_GetFromPath(pchar(command));
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
  except
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateItemInternal'#10#13 + e.message);
  end;

  Draw(Fx, Fy, FSize, true, 0, FShowItem);
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
    else // if no image for full bitbucket
      LoadImage(FImageFile, FBigItemSize, false, true, FImage, FIW, FIH);
  end
  else // if no custom image
  begin
    if is_pidl then LoadImageFromPIDL(apidl, FBigItemSize, false, true, FImage, FIW, FIH)
    else LoadImage(command, FBigItemSize, false, true, FImage, FIW, FIH);
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
    FBitBucket := PIDL_GetDisplayName2(pidFolder) = command;
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
  if FBitBucket then // if this shortcut is bitbucket
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
  else // if it is not
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
    Draw(Fx, Fy, FSize, true, 0, FShowItem);
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
    if FRunning then Draw(Fx, Fy, FSize, true, 0, FShowItem);
  except
    on e: Exception do raise Exception.Create('ShortcutItem.UpdateIndicator'#10#13 + e.message);
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
          if FBigItemSize <= 128 then temp:= 128
          else if FBigItemSize <= 160 then temp:= 160
          else if FBigItemSize <= 192 then temp:= 192;
          if temp <> FIW then UpdateItemI;
        end;
      gpShowRunningIndicator:
        begin
          if FRunning and not boolean(param) then
          begin
            FRunning:= false;
            Draw(Fx, Fy, FSize, true, 0, FShowItem);
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpSite: if FIndicator <> nil then UpdateIndicator;
      tcThemeChanged: if FIndicator <> nil then UpdateIndicator;

      // commands //

      icUpdateRunning:
        if length(command) > 0 then
        begin
          b := ProcessHelper.FullNameExists(UnzipPath(command));
          if b and (FIndicator = nil) then UpdateIndicator;
          if b <> FRunning then
          begin
            FRunning:= b;
            Draw(Fx, Fy, FSize, true, 0, FShowItem);
          end;
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
    if FFreed or FUpdating or (FFloating and not AForce) then exit;
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

        if wpi > 0 then
        begin
          DeferWindowPos(wpi, FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
          UpdateHint(xReal, yReal);
        end else
          SetWindowPos(FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        exit;

      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW = SWP_HIDEWINDOW then exit;

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
      if FFloating then dst := CreateGraphics(bmp.dc, ITEM_BACKGROUND) else dst := CreateGraphics(bmp.dc, 0);
      if not assigned(dst) then raise Exception.Create('CreateGraphics failed');
      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
      GdipDeleteBrush(brush);

      GdipSetCompositingMode(dst, CompositingModeSourceOver);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

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

    // draw icons //
    TCustomItem.CreateColorAttributes(color_data, FSelected, hattr);
    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize + animation_size, FSize + animation_size, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if hattr <> nil then GdipDisposeImageAttributes(hattr);

    if FDropIndicator = 1 then
      if assigned(theme.DropIndicatorAdd.Image) then
        GdipDrawImageRectRectI(dst, theme.DropIndicatorAdd.Image, xBitmap, yBitmap, FSize, FSize, 0, 0, theme.DropIndicatorAdd.W, theme.DropIndicatorAdd.H, UnitPixel, nil, nil, nil);
    if FDropIndicator = 2 then
      if assigned(theme.DropIndicatorRun.Image) then
        GdipDrawImageRectRectI(dst, theme.DropIndicatorRun.Image, xBitmap, yBitmap, FSize, FSize, 0, 0, theme.DropIndicatorRun.W, theme.DropIndicatorRun.H, UnitPixel, nil, nil, nil);

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
    on e: Exception do raise Exception.Create('ShortcutItem.Draw(' + caption + ')'#10#13 + e.message);
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
      draw(Fx, Fy, FSize, true, 0, FShowItem);
    end;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.Timer'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ToString: string;
begin
  result:= Make(FHWnd, FCaption, command, params, dir, FImageFile, showcmd, color_data, hide);
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
function TShortcutItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  filename: string;
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  if CanOpenFolder then AppendMenu(FHMenu, MF_STRING, $f002, PChar(UTF8ToAnsi(XOpenFolderOf) + ' "' + Caption + '"'));
  //AppendMenu(FHMenu, MF_STRING, $f005, PChar(UTF8ToAnsi(XRunAsUser)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  // if shell context menu is enabled //
  if FUseShellContextMenus and (command <> '') or is_pidl then
  begin
    if is_pidl then result := shcontextu.ShContextMenu(FHWnd, pt, apidl, FHMenu)
    else
    begin
      filename := toolu.UnzipPath(command);
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
procedure TShortcutItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
begin
  result := 0;
  DestroyMenu(FHMenu);
  LME(false);
  case wParam of // f001 to f020
    $f001: TfrmItemProp.Open(ToString, UpdateItem);
    $f002: OpenFolder;
    $f003: toolu.SetClipboard(ToString);
    $f004: Delete;
    $f005: ProcessHelper.RunAsUser(command, params, dir, showcmd);
    $f006..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.Exec;
var
  sei: TShellExecuteInfo;
begin
  if is_pidl then
  begin
    if hide then dockh.DockExecute(FHWnd, '/hide', '', '', 0)
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
    if FActivateRunningDefault and (GetAsyncKeystate(17) >= 0) then
    begin
      if hide then DockExecute(FHWnd, '/hide', '', '', 0);
      if not ActivateProcessMainWindow then
      begin
        if not hide then DockletDoAttensionAnimation(FHWnd);
        DockExecute(FHWnd, pchar(command), pchar(params), pchar(dir), showcmd);
      end;
    end else begin
      if hide then DockExecute(FHWnd, '/hide', '', '', 0) else DockletDoAttensionAnimation(FHWnd);
      DockExecute(FHWnd, pchar(command), pchar(params), pchar(dir), showcmd);
    end;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.ActivateProcessMainWindow: boolean;
begin
  try
    result := false;
    LME(true);
    result := ProcessHelper.ActivateProcessMainWindow(UnzipPath(command), FHWnd, ScreenRect, FSite);
  finally
    LME(false);
  end;
end;
//------------------------------------------------------------------------------
function TShortcutItem.CanOpenFolder: boolean;
var
  _file: string;
begin
  _file := toolu.UnzipPath(command);
  if not fileexists(_file) or not directoryexists(_file) then _file := ExtractFilePath(toolu.FindFile(_file));
  result := fileexists(_file) or directoryexists(_file);
end;
//------------------------------------------------------------------------------
procedure TShortcutItem.OpenFolder;
var
  _file: string;
begin
  _file := toolu.UnzipPath(command);
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
      color_data := DEFAULT_COLOR_DATA;
      UpdateItemI;
    end
    else
    begin
      if not is_pidl then DockExecute(FHWnd, pchar(command), pchar('"' + filename + '"'), nil, 1);
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
  if command <> '' then WritePrivateProfileString(szIniGroup, 'command', pchar(command), szIni);
  if params <> '' then WritePrivateProfileString(szIniGroup, 'params', pchar(params), szIni);
  if dir <> '' then WritePrivateProfileString(szIniGroup, 'dir', pchar(dir), szIni);
  if FImageFile <> '' then WritePrivateProfileString(szIniGroup, 'image', pchar(FImageFile), szIni);
  if showcmd <> sw_shownormal then WritePrivateProfileString(szIniGroup, 'showcmd', pchar(inttostr(showcmd)), szIni);
  if color_data <> DEFAULT_COLOR_DATA then WritePrivateProfileString(szIniGroup, 'color_data', pchar(toolu.ColorToString(color_data)), szIni);
  if hide then WritePrivateProfileString(szIniGroup, 'hide', '1', szIni);
end;
//------------------------------------------------------------------------------
class function TShortcutItem.Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
  AShowCmd: integer = 1; color_data: integer = DEFAULT_COLOR_DATA; hide: boolean = false): string;
begin
  result := 'class="shortcut";';
  result := result + 'hwnd="' + inttostr(AHWnd) + '";';
  if ACaption <> '' then result := result + 'caption="' + ACaption + '";';
  if ACommand <> '' then result := result + 'command="' + ACommand + '";';
  if AParams <> '' then result := result + 'params="' + AParams + '";';
  if ADir <> '' then result := result + 'dir="' + ADir + '";';
  if AImage <> '' then result := result + 'image="' + AImage + '";';
  if AShowCmd <> 1 then result := result + 'showcmd="' + inttostr(AShowCmd) + '";';
  if color_data <> DEFAULT_COLOR_DATA then result := result + 'color_data="' + toolu.ColorToString(color_data) + '";';
  if hide then result := result + 'hide="1";';
end;
//------------------------------------------------------------------------------
class function TShortcutItem.FromFile(filename: string): string;
var
  fcaption, fparams, fdir, ficon, ext: string;
begin
  result := '';
  if IsGUID(filename) then
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
 
