unit scsubitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, ComObj, ShlObj,
  GDIPAPI, gfx, PIDL, ShContextU, declu, dockh, toolu, customsubitemu, processhlp, loggeru;

type

  { TShortcutSubitem }

  TShortcutSubitem = class(TCustomSubitem)
  private
    FCommand: string;
    FParams: string;
    FDir: string;
    FImageFile: string;
    FShowCmd: integer;
    FHide: boolean;
    FColorData: integer;
    //
    FUseShellContextMenus: boolean;
    FIsPIDL: boolean;
    FPIDL: PITEMIDLIST;
    FLastMouseUp: PtrUInt;
    procedure LoadImageI;
    procedure UpdateRunning;
    procedure Exec(action: TExecuteAction);
    function ActivateProcessMainWindow: boolean;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure DrawNumberOverlay(dst: pointer; x, y, size, number: integer);
  public
    property Command: string read FCommand write FCommand;
    property Params: string read FParams write FParams;
    property Dir: string read FDir write FDir;
    property ImageFile: string read FImageFile write FImageFile;
    property ShowCmd: integer read FShowCmd write FShowCmd;
    property ColorData: integer read FColorData write FColorData;
    property Hide: boolean read FHide write FHide;

    constructor Create(wndParent: HWND; AParams: TDItemCreateParams); overload; override;
    destructor Destroy; override;
    procedure FromString(data: string); override;
    procedure Update;
    procedure HideItem; override;
    procedure Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; ABackground, AForce: boolean); override;
    procedure DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer); override;
    function ToString: string; override;
    function HitTest(Ax, Ay: integer): boolean; override;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var result: LRESULT); override;
    procedure Configure; override;
    function cmd(id: TDParam; param: PtrInt): PtrInt; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(pt: windows.TPoint; filename: string): boolean; override;

    class function Make(ACaption: WideString = '';
      ACommand: string = ''; AParams: string = ''; ADir: string = ''; AImage: string = '';
      AShowCmd: integer = 1; AColorData: integer = DEF_COLOR_DATA; AHide: boolean = false): string;
    class function MakeFromFilename(value: string): string;
  end;

implementation
uses frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutSubitem.Create(wndParent: HWND; AParams: TDItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;
  FCommand     := '';
  FParams      := '';
  FDir         := '';
  FImageFile   := '';
  FColorData   := DEF_COLOR_DATA;
  FShowCmd     := 0;
  FHide        := false;
  FLastMouseUp := 0;
end;
//------------------------------------------------------------------------------
destructor TShortcutSubitem.Destroy;
begin
  FFreed := true;
  try GdipDisposeImage(FImage);
  except on e: Exception do raise Exception.Create('TShortcutSubitem.Destroy.GdipDisposeImage ' + LineEnding + e.message);
  end;
  try if FIsPIDL then PIDL_Free(FPIDL);
  except on e: Exception do raise Exception.Create('TShortcutSubitem.Destroy.PIDL_Free ' + LineEnding + e.message);
  end;
  inherited;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.cmd(id: TDParam; param: PtrInt): PtrInt;
begin
  if not FFreed then
  try
    result := inherited cmd(id, param);

    case id of
      // parameters //
      gpShowRunningIndicator:
        begin
          if FRunning and not boolean(param) then
          begin
            FRunning := false;
            Redraw;
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpShowHint: UpdateCaptionExtent; //Update;
      gpSite: if FRunning then Redraw;
      tcThemeChanged: if FRunning then Redraw;

      // commands //
      icUpdateRunning: UpdateRunning;
    end;

  except
    on e: Exception do raise Exception.Create('ShortcutItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.FromString(data: string);
begin
  try
    if data <> '' then
    begin
      FCaption       := FetchValue(data, 'caption="', '";');
      FCommand       := FetchValue(data, 'command="', '";');
      FParams        := FetchValue(data, 'params="', '";');
      FDir           := FetchValue(data, 'dir="', '";');
      FImageFile     := FetchValue(data, 'image="', '";');
      FHide          := false;
      FColorData     := DEF_COLOR_DATA;
      FShowCmd       := 1;
      try FHide      := boolean(strtoint(FetchValue(data, 'hide="', '";')));
      except end;
      try FColorData := strtoint(FetchValue(data, 'color_data="', '";'));
      except end;
      try FShowCmd   := strtoint(FetchValue(data, 'showcmd="', '";'));
      except end;
      Update;
    end;
  except
    on e: Exception do raise Exception.Create('ShortcutItem.FromString ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.Update;
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

      // if FCommand is a CSIDL convert it to GUID or path //
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
          if IsGUID(FCommand) then FPIDL := PIDL_GetFromPath(pchar(FCommand));
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

      LoadImageI;

      // measure caption and adjust border size //
      UpdateCaptionExtent;
    finally
      FUpdating:= false;
    end;

    Redraw;
    sendmessage(FHWndParent, WM_APP_UPDATE_PREVIEW, 0, 0); // notify parent stack item
  except
    on e: Exception do raise Exception.Create('StackSubitem.Update ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.LoadImageI;
begin
  try if assigned(FImage) then GdipDisposeImage(FImage);
  except end;
  FImage := nil;

  if FImageFile <> '' then // if custom image file specified
  begin
    LoadImage(UnzipPath(FImageFile), FItemSize, true, true, FImage, FIW, FIH)
  end
  else // if no custom image set - load from object itself (PIDL or File)
  begin
    if FIsPIDL then LoadImageFromPIDL(FPIDL, FItemSize, true, true, FImage, FIW, FIH)
    else LoadImage(UnzipPath(FCommand), FItemSize, true, true, FImage, FIW, FIH);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateRunning;
begin
  if FExecutable <> '' then FProcessWindowsCount := ProcessHelper.GetProcessWindowsCount(FExecutable);
  if (FProcessWindowsCount > 0) <> FRunning then
  begin
    FRunning := FProcessWindowsCount > 0;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.HideItem;
var
  bmp: _SimpleBitmap;
  ClientRect: windows.TRect;
begin
  ClientRect := GetClientRect;
  bmp.topleft.x := Fx - FSize div 2 - ClientRect.Left;
  bmp.topleft.y := Fy - FSize div 2 - ClientRect.Top;
  bmp.width := 1;
  bmp.height := 1;
  if not CreateBitmap(bmp, FHWnd) then exit;
  UpdateLWindow(FHWnd, bmp, FAlpha);
  DeleteBitmap(bmp);
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
// Ax, Ay - center of icon
procedure TShortcutSubitem.Draw(Ax, Ay, ASize, AAlpha: integer; AAngle: single; AHintAlign, AHintAlpha: integer; ABackground, AForce: boolean);
const BGMARGIN = 3;
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  attr, brush, path, font, family: Pointer;
  IconX, IconY: integer; // relative icon coordinates
  WindowX, WindowY: integer;
  ClientRect, NCRect, BackgroundRect: windows.TRect;
  CaptionRect: TRectF;
  ptsBackground: array [0..3] of GDIPAPI.TPoint;
begin
  try
    if FFreed or FUpdating or FQueryDelete then exit;
    if not AForce and (Fx = Ax) and (Fy = Ay) and (FSize = ASize) and (FAlpha = AAlpha) and (FAngle = AAngle) and (FHintAlign =  AHintAlign) and (FHintAlpha = AHintAlpha) then exit;

    // ensure that item is not "selected" before making it visible
    if not IsWindowVisible(FHWnd) then FSelected := false;

    // set position //
    try
      Fx := Ax;
      Fy := Ay;
      FSize := ASize;
      FAngle := AAngle;
      FAlpha := AAlpha;
      FHintAlign :=  AHintAlign;
      if AHintAlpha < 0 then AHintAlpha := 0;
      if AHintAlpha > 255 then AHintAlpha := 255;
      FHintAlpha := AHintAlpha;
      FBackground := ABackground;
      ClientRect := GetClientRect;
      NCRect := GetNCRect;
      WindowX := Ax - FSize div 2 - ClientRect.Left - NCRect.Left;
      WindowY := Ay - FSize div 2 - ClientRect.Top - NCRect.Top;
    except
      on e: Exception do raise Exception.Create('SetPosition ' + LineEnding + e.message);
    end;

    // init //
    try
      bmp.topleft.x := WindowX + NCRect.Left;
      bmp.topleft.y := WindowY + NCRect.Top;
      bmp.width := NCRect.Right - NCRect.Left;
      bmp.height := NCRect.Bottom - NCRect.Top;
      if not CreateBitmap(bmp, FHWnd) then exit;
      dst := CreateGraphics(bmp.dc, 0);
      if not assigned(dst) then
      begin
        DeleteBitmap(bmp);
        exit;
      end;
      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ClientRect.Left - 1, ClientRect.Top - 1, ClientRect.Right - ClientRect.Left + 2, ClientRect.Bottom - ClientRect.Top + 2);
      GdipDeleteBrush(brush);
      GdipSetInterpolationMode(dst, InterpolationModeBilinear);
      IconX := ClientRect.Left;
      IconY := ClientRect.Top;
      if AAngle > 0 then
      begin
        IconX := -FSize div 2;
        IconY := -FSize div 2;
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
      end;
    except
      on e: Exception do raise Exception.Create('InitDraw ' + LineEnding + e.message);
    end;

    // background //
    if FBackground then
    begin
      BackgroundRect := classes.rect(-BGMARGIN, -BGMARGIN, FSize + BGMARGIN, FSize + BGMARGIN);
      if FShowHint and (length(FCaption) > 0) and ((AHintAlign >= 0) and (AHintAlign <= 7)) and (AHintAlpha > 25) then
      begin
        if AHintAlign = HA_HORIZONTAL_LEFT then
        begin
          BackgroundRect.Left -= FCaptionWidth + FCaptionHeight div 3 + 5;
        end else
        if AHintAlign = HA_HORIZONTAL_RIGHT then
        begin
          BackgroundRect.Right += FCaptionWidth + FCaptionHeight div 3 + 5;
        end else
        if AHintAlign = HA_VERTICAL_TOP then
        begin
          BackgroundRect.Top -= FCaptionWidth + FCaptionHeight div 3 + 5;
        end else
        if AHintAlign = HA_VERTICAL_BOTTOM then
        begin
          BackgroundRect.Bottom += FCaptionWidth + FCaptionHeight div 3 + 5;
        end else
        if AHintAlign = HA_HORIZONTAL_BOTTOM then
        begin
          BackgroundRect.Left := -FCaptionWidth div 2 + FSize div 2 - BGMARGIN;
          BackgroundRect.Right := FCaptionWidth div 2 + FSize div 2 + BGMARGIN;
          BackgroundRect.Bottom += FCaptionHeight;
        end;
      end;
      ptsBackground[0].x := IconX + BackgroundRect.Left;
      ptsBackground[0].y := IconY + BackgroundRect.Top;
      ptsBackground[1].x := IconX + BackgroundRect.Right;
      ptsBackground[1].y := IconY + BackgroundRect.Top;
      ptsBackground[2].x := IconX + BackgroundRect.Right;
      ptsBackground[2].y := IconY + BackgroundRect.Bottom;
      ptsBackground[3].x := IconX + BackgroundRect.Left;
      ptsBackground[3].y := IconY + BackgroundRect.Bottom;
      GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
	    GdipCreatePath(FillModeWinding, path);
      GdipAddPathClosedCurve2I(path, ptsBackground, 4, 0.1);
	    GdipCreateSolidFill(AHintAlpha shl 24 + FFont.backcolor and $ffffff, brush);
      GdipFillPath(dst, brush, path);
	    GdipDeleteBrush(brush);
	    GdipDeletePath(path);
    end;

    // icon and number //
    CreateColorAttributes(FColorData, FSelected, attr);
    if assigned(FImage) then GdipDrawImageRectRectI(dst, FImage, IconX, IconY, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, attr, nil, nil);
    if attr <> nil then GdipDisposeImageAttributes(attr);
    GdipSetCompositingMode(dst, CompositingModeSourceOver);
    if FProcessWindowsCount > 0 then DrawNumberOverlay(dst, IconX, IconY, FSize, FProcessWindowsCount);
    GdipResetWorldTransform(dst);

    // caption //
    if FShowHint and (length(FCaption) > 0) and ((AHintAlign >= 0) and (AHintAlign <= 7)) and (AHintAlpha > 25) then
    begin
      if AHintAlign = HA_HORIZONTAL_LEFT then
      begin
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        IconX := -FSize div 2 - FCaptionHeight div 3 - FCaptionWidth - 5;
        IconY := -FCaptionHeight div 2;
      end else
      if AHintAlign = HA_HORIZONTAL_RIGHT then
      begin
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        IconX := FSize div 2 + FCaptionHeight div 3 + 5;
        IconY := -FCaptionHeight div 2;
      end else
      if AHintAlign = HA_VERTICAL_TOP then
      begin
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle - 90, MatrixOrderPrepend);
        IconX := FSize div 2 + FCaptionHeight div 3 + 5;
        IconY := -FCaptionHeight div 2;
      end else
      if AHintAlign = HA_VERTICAL_BOTTOM then
      begin
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle + 90, MatrixOrderPrepend);
        IconX := FSize div 2 + FCaptionHeight div 3 + 5;
        IconY := -FCaptionHeight div 2;
      end else
      if AHintAlign = HA_HORIZONTAL_BOTTOM then
      begin
        GdipTranslateWorldTransform(dst, ClientRect.Left + FSize div 2, ClientRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        IconX := -FCaptionWidth div 2;
        IconY := FSize div 2 + 3;
      end;
      // caption background //
      if not FBackground then
      begin
	      GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
	      ptsBackground[0].x := IconX - FCaptionHeight div 10;
	      ptsBackground[0].y := IconY - 1;
	      ptsBackground[1].x := ptsBackground[0].x + FCaptionWidth - 1 + FCaptionHeight div 5;
	      ptsBackground[1].y := ptsBackground[0].y;
	      ptsBackground[2].x := ptsBackground[1].x;
	      ptsBackground[2].y := ptsBackground[0].y + FCaptionHeight + 1;
	      ptsBackground[3].x := ptsBackground[0].x;
	      ptsBackground[3].y := ptsBackground[2].y;
	      GdipCreatePath(FillModeWinding, path);
        GdipAddPathClosedCurve2I(path, ptsBackground, 4, 15 / FCaptionWidth);
	      GdipCreateSolidFill(AHintAlpha shl 24 + FFont.backcolor and $ffffff, brush);
	      GdipFillPath(dst, brush, path);
	      GdipDeleteBrush(brush);
	      GdipDeletePath(path);
      end;
      //
      CaptionRect.X := IconX;
      CaptionRect.Y := IconY;
      CaptionRect.Width := FCaptionWidth;
      CaptionRect.Height := FCaptionHeight;
      GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, family);
      GdipCreateFont(family, FFont.size2, integer(FFont.bold) + integer(FFont.italic) * 2, 2, font);
      GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
      GdipCreateSolidFill(AHintAlpha shl 24 + FFont.color and $ffffff, brush);
      GdipDrawString(dst, PWideChar(FCaption), -1, font, @CaptionRect, nil, brush);
      GdipDeleteBrush(brush);
      GdipDeleteFont(font);
      GdipDeleteFontFamily(family);
      //
      GdipResetWorldTransform(dst);
    end;

    // update window content //
    UpdateLWindow(FHWnd, bmp, AAlpha);

    // cleanup //
    DeleteGraphics(dst);
    DeleteBitmap(bmp);
  except
    on e: Exception do raise Exception.Create('StackSubitem.Draw(' + caption + ') ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.DrawNumberOverlay(dst: pointer; x, y, size, number: integer);
var
  brush, family, hfont, format, path: Pointer;
  tmpItemSize: integer;
  rect: GDIPAPI.TRectF;
begin
  GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
  GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
  // background
  tmpItemSize := max(FItemSize, 40);
  if number > 99 then rect.Width := round(tmpItemSize * 9 / 12)
  else if number > 9 then rect.Width := round(tmpItemSize * 7 / 12)
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
  GdipDrawString(dst, PWideChar(WideString(inttostr(number))), -1, hfont, @rect, format, brush);
  GdipDeleteStringFormat(format);
  GdipDeleteBrush(brush);
  GdipDeleteFont(hfont);
  GdipDeleteFontFamily(family);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer);
var
  matrix: ColorMatrix;
  hattr: Pointer;
begin
  try
    hattr := nil;
    if FColorData <> DEF_COLOR_DATA then
    begin
      CreateColorMatrix(FColorData, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);
    end;
    GdipDrawImageRectRectI(graphics, FImage, Ax, Ay, ASize, ASize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if FColorData <> DEF_COLOR_DATA then GdipDisposeImageAttributes(hattr);
  except
    on e: Exception do raise Exception.Create('StackSubitem.DrawPreview ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ToString: string;
begin
  result := Make(FCaption, FCommand, FParams, FDir, FImageFile, FShowCmd, FColorData, FHide);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.HitTest(Ax, Ay: integer): boolean;
begin
  result := true;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
  inherited;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
var
  pt: windows.TPoint;
  tickCount, elapsed: QWord;
begin
  inherited;
  result := false;

  if not FFreed and FSelected then
  begin
    cmd(icSelect, 0);

    tickCount := gettickcount64;
    elapsed := tickCount - FLastMouseUp;

    if button = mbLeft then
    begin
      if elapsed > FLaunchInterval then
      begin
        if ssAlt in shift then Exec(eaGroup)
        else
        if ssCtrl in shift then Exec(eaRun)
        else
          Exec(eaDefault);
        CloseStack;
      end;
      FLastMouseUp := tickCount;
      result := true;
    end;

    if button = mbRight then
    begin
      windows.GetCursorPos(pt);
      result := ContextMenu(pt);
    end;

    SetActiveWindow(FHWndParent);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.MouseHeld(button: TMouseButton);
begin
  inherited;
  if button = mbRight then Configure;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ContextMenu(pt: Windows.TPoint): boolean;
var
  filename: string;
  msg: TMessage;
  mii: MENUITEMINFO;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenuW(FHMenu, MF_STRING, $f001, pwchar(UTF8Decode(XConfigureIcon)));
  AppendMenuW(FHMenu, MF_STRING, $f003, pwchar(UTF8Decode(XCopy)));
  if CanOpenFolder then AppendMenuW(FHMenu, MF_STRING, $f002, pwchar(UTF8Decode(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING, $f004, pwchar(UTF8Decode(XDeleteIcon)));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING, $f006, pwchar(UTF8Decode(XRun)));
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f006, false, @mii);

  // if shell context menu is enabled //
  if FUseShellContextMenus and (FCommand <> '') or FIsPIDL then
  begin
    if FIsPIDL then result := shcontextu.ShContextMenu(FHWnd, pt, FPIDL, FHMenu)
    else
    begin
      filename := toolu.UnzipPath(FCommand);
      if not fileexists(filename) and not directoryexists(filename) then filename := toolu.FindFile(filename);
      if fileexists(filename) or directoryexists(filename) then result := shcontextu.ShContextMenu(FHWnd, pt, filename, FHMenu);
    end;
  end;

  // else, if it is disabled //
  if not result then msg.WParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.WMCommand(wParam: WPARAM; lParam: LPARAM; var result: LRESULT);
begin
  try
    DestroyMenu(FHMenu);
    FHMenu := 0;
    CloseStack;
    result := 0;
    case wparam of // f001 to f020
      $f001: Configure;
      $f002: OpenFolder;
      $f003: toolu.SetClipboard(ToString);
      $f004: Delete;
      $f006: Exec(eaRun);
    end;
  except
    on e: Exception do raise Exception.Create('TStackSubitem.WMCommand ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.Configure;
begin
  TfrmItemProp.Open(Handle);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.Exec(action: TExecuteAction);
  procedure Run;
  begin
    if FHide then DockExecute(FHWnd, '/hide', '', '', 0) else DockletDoAttensionAnimation(FHWnd);
    DockExecute(FHWnd, pchar(FCommand), pchar(FParams), pchar(FDir), FShowCmd);
  end;
begin
  if FIsPIDL then Run
  else
  begin
    if FActivateRunning and not (action = eaRun) then
    begin
      if FHide then DockExecute(FHWnd, '/hide', '', '', 0);
      if not ActivateProcessMainWindow then Run;
    end
    else Run;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ActivateProcessMainWindow: boolean;
begin
  result := ProcessHelper.ActivateProcessMainWindow(UnzipPath(FCommand), FHWnd, ScreenRect, FSite);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.CanOpenFolder: boolean;
var
  strFile: string;
begin
  strFile := toolu.UnzipPath(FCommand);
  result := fileexists(strFile) or directoryexists(strFile);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.OpenFolder;
var
  strFile: string;
begin
  strFile := ExtractFilePath(toolu.UnzipPath(FCommand));
  DockExecute(FHWnd, pchar(strFile), nil, nil, sw_shownormal);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.DropFile(pt: windows.TPoint; filename: string): boolean;
var
  ext: string;
begin
  result := not FFreed;
  if result then
  begin
    ext := LowerCase(ExtractFileExt(filename));
    if (ext = '.png') or (ext = '.ico') then
    begin
      FImageFile := toolu.ZipPath(filename);
      FColorData := DEF_COLOR_DATA;
      Update;
    end
    else
    begin
      if not FIsPIDL then DockExecute(FHWnd, pchar(FCommand), pchar('"' + filename + '"'), nil, 1);
    end;
  end;
end;
//------------------------------------------------------------------------------
class function TShortcutSubitem.Make(ACaption: WideString = '';
  ACommand: string = ''; AParams: string = ''; ADir: string = ''; AImage: string = '';
  AShowCmd: integer = 1; AColorData: integer = DEF_COLOR_DATA; AHide: boolean = false): string;
begin
  result := 'class="shortcut";';
  if ACaption <> '' then result := result + 'caption="' + ACaption + '";';
  if ACommand <> '' then result := result + 'command="' + ACommand + '";';
  if AParams <> '' then result := result + 'params="' + AParams + '";';
  if ADir <> '' then result := result + 'dir="' + ADir + '";';
  if AImage <> '' then result := result + 'image="' + AImage + '";';
  if AShowCmd <> 1 then result := result + 'showcmd="' + inttostr(AShowCmd) + '";';
  if AColorData <> DEF_COLOR_DATA then result := result + 'color_data="' + toolu.ColorToString(AColorData) + '";';
  if AHide then result := result + 'hide="1";';
end;
//------------------------------------------------------------------------------
class function TShortcutSubitem.MakeFromFilename(value: string): string;
var
  Caption: WideString;
  Dir: string = '';
begin
  if IsGUID(value) or IsPIDLString(value) then
  begin
    result := Make('::::', value, '', '', '', 1);
  end
  else
  begin
    if DirectoryExists(value) then
      Caption  := value
    else
      Caption  := ChangeFileExt(ExtractFilename(value), '');
    if LowerCase(ExtractFileExt(value)) = '.exe' then Dir := ZipPath(ExcludeTrailingPathDelimiter(ExtractFilePath(value)));
    result := Make(Caption, ZipPath(value), '', Dir, '', 1);
  end;
end;
//------------------------------------------------------------------------------
end.
 
