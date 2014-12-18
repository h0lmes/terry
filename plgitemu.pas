unit plgitemu;

interface
uses Windows, Messages, SysUtils, Controls, Classes, Dialogs,
    IniFiles, GDIPAPI, gfx, math, dynlibs, declu, DockH, customitemu, toolu;

type TPluginItem = class(TCustomItem)
  private
    lpData: Pointer;
    MouseDownPoint: windows.TPoint;
    AutoDeleteImage: boolean;
    AutoDeleteOverlay: boolean;
    FImage2: Pointer;
    FIW2: cardinal;
    FIH2: cardinal;
    hwnd2: uint; // to speed up gdiplus drawing //
    PluginFile: string;
    FIniFile: string;
    FIniSection: string;
    // plugin lib vars
    hLib: uint;
    OnCreate: _OnCreate;
    OnSave: _OnSave;
    OnDestroy: _OnDestroy;
    OnLeftButtonClick: _OnLeftButtonClick;
    OnLeftButtonHeld: _OnLeftButtonHeld;
    OnDoubleClick: _OnDoubleClick;
    OnRightButtonClick: _OnRightButtonClick;
    OnConfigure: _OnConfigure;
    OnWndMessage: _OnProcessMessage;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure CreatePlugin(AData: string);
  public
    procedure CallCreate;
    procedure UpdateImage(AImage: Pointer; AutoDelete: boolean);
    procedure UpdateOverlay(AOverlay: Pointer; AutoDelete: boolean);
    function GetFilename: string;
    //
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    function DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; override;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Timer; override;
    procedure Configure; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
  end;

implementation
//------------------------------------------------------------------------------
constructor TPluginItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  FFreed := true;
  inherited;
  FFreed := false;

  AutoDeleteImage := true;
  FImage := nil;
  AutoDeleteOverlay := true;
  FImage2 := nil;

  lpData := nil;
  CreatePlugin(AData);
end;
//------------------------------------------------------------------------------
procedure TPluginItem.CreatePlugin(AData: string);
var
  ini: TIniFile;
begin
  try
    // window to speed up drawing //
    hwnd2 := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow + ws_ex_noactivate, WINITEM_CLASS, nil, ws_popup, -100, -100, 32, 32, 0, 0, hInstance, nil);

    FFreed := true;
    // get library path //
    FIniFile := FetchValue(AData, 'inifile="', '";');
    FIniSection := FetchValue(AData, 'inisection="', '";');
    if (length(FIniFile) > 0) and (length(FIniSection) > 0) then
    begin
      ini := TIniFile.Create(FIniFile);
      PluginFile := toolu.UnzipPath(ini.ReadString(FIniSection, 'file', ''));
      ini.free;
    end else begin
      PluginFile := toolu.UnzipPath(FetchValue(AData, 'file="', '";'));
    end;

    // load library //
    SetCurrentDir(ExtractFilePath(PluginFile));
    hLib := LoadLibrary(PluginFile);
    if hLib = 0 then
    begin
      MessageBox(FHWnd, pchar('LoadLibrary(' + PluginFile + ') failed'), 'CreatePlugin', 0);
      exit;
    end;
    @OnCreate := GetProcAddress(hLib, 'OnCreate');
    @OnSave := GetProcAddress(hLib, 'OnSave');
    @OnDestroy := GetProcAddress(hLib, 'OnDestroy');
    @OnLeftButtonClick := GetProcAddress(hLib, 'OnLeftButtonClick');
    @OnDoubleClick := GetProcAddress(hLib, 'OnDoubleClick');
    @OnRightButtonClick := GetProcAddress(hLib, 'OnRightButtonClick');
    @OnConfigure := GetProcAddress(hLib, 'OnConfigure');
    @OnWndMessage := GetProcAddress(hLib, 'OnProcessMessage');
    if not assigned(OnCreate) then
    begin
      MessageBox(FHWnd, pchar('OnCreate(' + PluginFile + ') is NULL'), 'CreatePlugin', 0);
      exit;
    end;
    FFreed := false;
  except
    on e: Exception do raise Exception.Create('PluginItem.CreatePlugin'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.CallCreate;
var
  szIni, szIniGroup: array [0..MAX_PATH - 1] of char;
begin
  lpData := nil;
  if (FIniFile <> '') and (FIniSection <> '') then
  begin
    try
      strlcopy(szIni, pchar(FIniFile), length(FIniFile));
      strlcopy(szIniGroup, pchar(FIniSection), length(FIniSection));
      lpData := OnCreate(FHWnd, hLib, @szIni, @szIniGroup);
    except
      on e: Exception do raise Exception.Create('PluginItem.CallCreate'#10#13 + e.message);
    end;
  end
  else
  begin
    try
      lpData := OnCreate(FHWnd, hLib, nil, nil);
    except
      on e: Exception do raise Exception.Create('PluginItem.CallCreate_NoIni'#10#13 + e.message);
    end;
  end;
end;
//------------------------------------------------------------------------------
destructor TPluginItem.Destroy;
begin
  try
    if assigned(OnDestroy) then OnDestroy(lpData, FHWnd);
  except
    on e: Exception do raise Exception.Create('PluginItem.OnDestroy'#10#13 + e.message);
  end;
  try if AutoDeleteImage then GdipDisposeImage(FImage);
  except end;
  try if AutoDeleteOverlay then GdipDisposeImage(FImage2);
  except end;
  try windows.DestroyWindow(HWnd2);
  except end;

  inherited;
end;
//------------------------------------------------------------------------------
function TPluginItem.cmd(id: TGParam; param: integer): integer;
begin
  try
    result := inherited cmd(id, param);
  except
    on e: Exception do raise Exception.Create('PluginItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.UpdateImage(AImage: Pointer; AutoDelete: boolean);
begin
  if not FFreed then
  begin
    try
      if AutoDeleteImage then
        if assigned(FImage) then GdipDisposeImage(FImage);
    except end;
    FImage := AImage;
    //GdipGetImageWidth(FImage, FIW);
    //GdipGetImageHeight(FImage, FIH);
    //AutoDeleteImage := AutoDelete;
    AutoDeleteImage := DownscaleImage(FImage, FBigItemSize, false, FIW, FIH, AutoDelete) or AutoDelete;
    if not FFloating then Redraw;
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.UpdateOverlay(AOverlay: Pointer; AutoDelete: boolean);
begin
  if not FFreed then
  begin
    try
      if AutoDeleteOverlay then
        if assigned(FImage2) then GdipDisposeImage(FImage2);
    except end;
    FImage2 := AOverlay;
    //GdipGetImageWidth(FImage2, FIW2);
    //GdipGetImageHeight(FImage2, FIH2);
    //AutoDeleteOverlay := AutoDelete;
    AutoDeleteOverlay := DownscaleImage(FImage2, FBigItemSize, false, FIW2, FIH2, AutoDelete) or AutoDelete;
    if not FFloating then Redraw;
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  brush: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  animation_offset_x, animation_offset_y, animation_size: integer;
begin
  try
    if FFreed or not FEnabled or (FFloating and not AForce) then exit;
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

    // init draw //
    try
      bmp.topleft.x := xReal;
      bmp.topleft.y := yReal;
      bmp.width := FSize + ItemRect.Left * 2;
      bmp.height := FSize + ItemRect.Top * 2;
      if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
      GdipCreateFromHDC(bmp.dc, dst);
      if not assigned(dst) then raise Exception.Create('CreateGraphics failed');

      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 2, ItemRect.Bottom - ItemRect.Top + 2);
      GdipDeleteBrush(brush);
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
      on e: Exception do raise Exception.Create('InitDraw'#10#13 + e.message);
    end;

    // draw icons //
    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize + animation_size, FSize + animation_size, 0, 0, FIW, FIH, UnitPixel, nil, nil, nil);
    if assigned(FImage2) then
      GdipDrawImageRectRectI(dst, FImage2, xBitmap, yBitmap, FSize + animation_size, FSize + animation_size, 0, 0, FIW2, FIH2, UnitPixel, nil, nil, nil);

    if FAnimationProgress > 0 then GdipResetWorldTransform(dst);
    if FReflection and (FReflectionSize > 0) and not FFloating then
      BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));

    DeleteGraphics(dst);
    DeleteBitmap(bmp);
  except
    on e: Exception do raise Exception.Create('PluginItem.Draw(' + FCaption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Timer;
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
    on e: Exception do raise Exception.Create('PluginItem.Timer'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Configure;
begin
  if assigned(OnConfigure) then OnConfigure(lpData);
end;
//------------------------------------------------------------------------------
function TPluginItem.GetFilename: string;
begin
  result := PluginFile;
end;
//------------------------------------------------------------------------------
function TPluginItem.ToString: string;
begin
  result := '';
  if not FFreed then
  begin
    result := result + '';
    result := 'class="plugin";hwnd="' + inttostr(FHWnd) + '";file="' + toolu.ZipPath(PluginFile) + '";caption="' + FCaption + '";';
  end;
end;
//------------------------------------------------------------------------------
function TPluginItem.DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
var
  sz: windows.TSize;
begin
  result := inherited DblClick(button, shift, x, y);
  if not FFreed then
  begin
    sz.cx := FSize;
    sz.cy := FSize;
    MouseDownPoint := point(x - Rect.Left, y - Rect.Top);
    if assigned(OnDoubleClick) then OnDoubleClick(lpData, @MouseDownPoint, @sz);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
  inherited;
  if not FFreed then MouseDownPoint := point(x - Rect.Left, y - Rect.Top);
end;
//------------------------------------------------------------------------------
procedure TPluginItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
  sz: windows.TSize;
  result: boolean;
begin
  pt := point(x - Rect.Left, y - Rect.Top);
  sz.cx := FSize;
  sz.cy := FSize;

  if button = mbLeft then if assigned(OnLeftButtonClick) then OnLeftButtonClick(lpData, @pt, @sz);

  if button = mbRight then
  begin
    result := false;
    if assigned(OnRightButtonClick) then result := OnRightButtonClick(lpData, @pt, @sz);
    if not result then Configure;
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.MouseHeld(button: TMouseButton);
var
  pt: windows.TPoint;
  sz: windows.TSize;
  result: boolean;
begin
  inherited;
  if button = mbRight then
  begin
    result := false;
    if assigned(OnLeftButtonHeld) then
    begin
      pt := point(MouseDownPoint.x - Rect.Left, MouseDownPoint.y - Rect.Top);
      sz.cx := FSize;
      sz.cy := FSize;
      result := OnLeftButtonHeld(lpData, @pt, @sz);
    end;
    if not result then Configure;
  end;
end;
//------------------------------------------------------------------------------
function TPluginItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  if CanOpenFolder then AppendMenu(FHMenu, MF_STRING, $f002, PChar(UTF8ToAnsi(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  // else, if it is disabled //
  msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
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
    $f005..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.WndMessage(var msg: TMessage);
begin
  msg.Result := 0;
  if not FFreed and assigned(OnWndMessage) then
  with msg do
  begin
      {if (msg >= wm_mousefirst) and (msg <= wm_mouselast) then
      begin
        TSmallPoint(lParam).x := TSmallPoint(lParam).x - Rect.Left;
        TSmallPoint(lParam).y := TSmallPoint(lParam).y - Rect.Top;
      end;}
      OnWndMessage(lpData, FHWnd, Msg, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Save(szIni: pchar; szIniGroup: pchar);
begin
  if FFreed or (szIni = nil) or (szIniGroup = nil) then exit;
  try
    WritePrivateProfileString(szIniGroup, nil, nil, szIni);
    WritePrivateProfileString(szIniGroup, 'class', 'plugin', szIni);
    WritePrivateProfileString(szIniGroup, 'file', pchar(toolu.ZipPath(PluginFile)), szIni);
    if assigned(OnSave) then OnSave(lpData, szIni, szIniGroup, false);
  except
    on E: Exception do raise Exception.Create('PluginItem.Save'#10#13'Plugin DLL: ' + PluginFile + #10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.
