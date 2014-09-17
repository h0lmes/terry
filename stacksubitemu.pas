unit stacksubitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, ComObj, ShlObj,
  GDIPAPI, gdip_gfx, PIDL, ShContextU, declu, customitemu, processhlp;

type
  TCustomSubitem = class
  protected
    FFreed: boolean;
    FHWnd: cardinal;
    FHWndParent: cardinal;
    FHMenu: cardinal;
    FCaption: string;
    FCaptionWidth: integer;
    FCaptionHeight: integer;
    Fx: integer;
    Fy: integer;
    FSize: integer;
    FBorder: integer;
    FAngle: single;
    FAlpha: integer;
    FHintAlign: integer;
    FHintAlpha: integer;
    FQueryDelete: boolean;
    FFont: _FontData;

    FEnabled: boolean;
    FConfigurable: boolean;
    FUpdating: boolean;
    FSelected: boolean; // when mouse button is down - icon becomes darken //
    FRunning: boolean;
    FShowHint: boolean;
    FSite: integer;
    FItemSize: integer;
    FLaunchInterval: integer;
    FActivateRunningDefault: boolean;
    MouseDownPoint: windows.TPoint;
    FLockDragging: boolean;

    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height

    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;

    procedure Init; virtual;
    function GetRectFromSize(ASize: integer): windows.TRect;
    function GetClientRect: windows.TRect;
    function GetScreenRect: windows.TRect;
    procedure CloseStack;
    procedure NativeWndProc(var message: TMessage);
  public
    property Freed: boolean read FFreed write FFreed;
    property HWnd: uint read FHWnd;
    property Caption: string read FCaption;
    property X: integer read Fx;
    property Y: integer read Fy;
    property Size: integer read FSize;
    property Rect: windows.TRect read GetClientRect;
    property ScreenRect: windows.TRect read GetScreenRect;
    property Running: boolean read FRunning;
    property QueryDelete: boolean read FQueryDelete;
    property Image: pointer read FImage;
    property ImageW: uint read FIW;
    property ImageH: uint read FIH;

    function HitTest(Ax, Ay: integer): boolean;
    function ScreenHitTest(Ax, Ay: integer): boolean;

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); virtual;
    destructor Destroy; override;
    procedure UpdateItem(AData: string); virtual; abstract;
    procedure Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; AForce: boolean); virtual; abstract;
    procedure DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer); virtual; abstract;
    function ToString: string; virtual;
    function SaveToString: string; virtual; abstract;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); virtual;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; virtual;
    procedure MouseHeld(button: TMouseButton); virtual;
    procedure WMCommand(var msg: TMessage); virtual; abstract;
    procedure Configure; virtual; abstract;
    function cmd(id: TGParam; param: integer): integer; virtual;
    function CanOpenFolder: boolean; virtual; abstract;
    procedure OpenFolder; virtual; abstract;
    function DropFile(pt: windows.TPoint; filename: string): boolean; virtual; abstract;
    procedure Delete(AllowUndo: boolean = true); virtual;
    procedure BeginDrag; virtual;
  end;

  TShortcutSubitem = class(TCustomSubitem)
  private
    command: string;
    params: string;
    dir: string;
    showcmd: integer;
    hide: boolean;
    FUseShellContextMenus: boolean;
    FIndicator: Pointer;
    FIndicatorW: integer;
    FIndicatorH: integer;
    is_pidl: boolean;
    apidl: PITEMIDLIST;
    imagefile: string;
    color_data: integer;
    LastMouseUp: cardinal;
    procedure UpdateItemInternal;
    procedure UpdateIndicator;
    procedure DrawIndicator(dst: Pointer; xBitmap: integer; yBitmap: integer);
    procedure Exec;
    function ActivateProcessMainWindow: boolean;
    function ContextMenu(pt: Windows.TPoint): boolean;
  public
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); overload; override;
    destructor Destroy; override;
    procedure UpdateItem(AData: string); overload; override;
    procedure Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; AForce: boolean); override;
    procedure DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer); override;
    function ToString: string; override;
    function SaveToString: string; override;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    function MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; override;
    procedure WMCommand(var msg: TMessage); override;
    procedure Configure; override;
    function cmd(id: TGParam; param: integer): integer; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(pt: windows.TPoint; filename: string): boolean; override;
    class function Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; color_data: integer = DEFAULT_COLOR_DATA; hide: boolean = false): string;
    class function SaveMake(ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; color_data: integer = DEFAULT_COLOR_DATA; hide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

implementation
uses dockh, themeu, setsu, toolu, frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutSubitem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;

  LastMouseUp:= 0;
  command:= '';
  params:= '';
  dir:= '';
  imagefile:= '';
  color_data:= DEFAULT_COLOR_DATA;
  showcmd:= 0;
  hide:= false;

  UpdateItem(AData);
  UpdateIndicator;
end;
//------------------------------------------------------------------------------
destructor TShortcutSubitem.Destroy;
begin
  FFreed := true;
  try GdipDisposeImage(FImage);
  except on e: Exception do raise Exception.Create('StackSubitem.Destroy.GdipDisposeImage'#10#13 + e.message);
  end;
  try if is_pidl then PIDL_Free(apidl);
  except on e: Exception do raise Exception.Create('StackSubitem.Destroy.PIDL_Free'#10#13 + e.message);
  end;

  inherited;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.cmd(id: TGParam; param: integer): integer;
var
  b: boolean;
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
            if IsWindowVisible(FHWnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpSite: if FIndicator <> nil then UpdateIndicator;
      gpShowHint: UpdateItemInternal;
      tcThemeChanged: if FIndicator <> nil then UpdateIndicator;

      // commands //

      icUpdateRunning:
        begin
          b := ProcessHelper.FullNameExists(UnzipPath(command));
          if b and (FIndicator = nil) then UpdateIndicator;
          if b <> FRunning then
          begin
            FRunning := b;
            if IsWindowVisible(FHWnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);
          end;
        end;
    end;

  except
    on e: Exception do raise Exception.Create('ShortcutItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateItem(AData: string);
begin
  if not FFreed then
  try
    FCaption := FetchValue(AData, 'caption="', '";');
    command := FetchValue(AData, 'command="', '";');
    params := FetchValue(AData, 'params="', '";');
    dir := FetchValue(AData, 'dir="', '";');
    imagefile := FetchValue(AData, 'image="', '";');
    hide := false;
    color_data := DEFAULT_COLOR_DATA;
    showcmd := 1;
    try hide := boolean(strtoint(FetchValue(AData, 'hide="', '";')));
    except end;
    try color_data := strtoint(FetchValue(AData, 'color_data="', '";'));
    except end;
    try showcmd := strtoint(FetchValue(AData, 'showcmd="', '";'));
    except end;
  except
    on e: Exception do raise Exception.Create('StackSubitem.UpdateItem.Data'#10#13 + e.message);
  end;

  UpdateItemInternal;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateItemInternal;
var
  sfi: TSHFileInfoA;
  path: array [0..MAX_PATH] of char;
  temp: string;
  pidFolder: PItemIDList;
  csidl: integer;
  pszName: array [0..255] of char;
  // caption extent measurement vars //
  hgdip, hfont, hfontfamily: Pointer;
  rect: TRectF;
  dc: HDC;
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
        if FileExists(command) or DirectoryExists(command) then
          command := ZipPath(command)
        else
          FCaption := '::::';
      end;

      // create PIDL from GUID //
      PIDL_Free(apidl);
      if IsGUID(command) then apidl := PIDL_GetFromPath(pchar(command));
      is_pidl := assigned(apidl);

      // parse PIDL //
      if is_pidl and (FCaption = '::::') then
      begin
        SHGetFileInfoA(pchar(apidl), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
        FCaption := sfi.szDisplayName;
        // try converting PIDL to file system path //
        if SHGetPathFromIDList(apidl, pchar(@path)) then
        begin
          temp := strpas(pchar(@path));
          if FileExists(temp) or DirectoryExists(temp) then
          begin
            command := ZipPath(temp);
            PIDL_Free(apidl);
            is_pidl := false;
          end;
        end;
      end;

      // load images //
      try if FImage <> nil then GdipDisposeImage(FImage);
      except end;
      FImage := nil;
      if imagefile <> '' then LoadImage(imagefile, FItemSize, true, true, FImage, FIW, FIH)
      else
      begin
        if is_pidl then LoadImageFromPIDL(apidl, FItemSize, true, true, FImage, FIW, FIH)
        else LoadImage(command, FItemSize, true, true, FImage, FIW, FIH);
      end;

      // measure caption and adjust border size //
      FBorder := 8;
      FCaptionWidth := 0;
      FCaptionHeight := 0;
      if FShowHint and (length(FCaption) > 0) then
      begin
        CopyFontData(sets.container.StackFont, FFont);
        dc := CreateCompatibleDC(0);
        if dc = 0 then raise Exception.Create('StackSubitem.UpdateItemInternal.Measure. Device context is null');
        GdipCreateFromHDC(dc, hgdip);
        try
          GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, hfontfamily);
        except
          on e: Exception do raise Exception.Create('StackSubitem.UpdateItemInternal.Measure.CreateFontFamily'#10#13 + e.message);
        end;
        GdipCreateFont(hfontfamily, FFont.size, integer(FFont.bold) + integer(FFont.italic) * 2, 2, hfont);
        rect.x := 0;
        rect.y := 0;
        rect.Width := 0;
        rect.Height := 0;
        try GdipMeasureString(hgdip, PWideChar(WideString(FCaption)), -1, hfont, @rect, nil, @rect, nil, nil);
        except
          on e: Exception do raise Exception.Create('StackSubitem.UpdateItemInternal.Measure.MeasureString'#10#13 + e.message);
        end;
        GdipDeleteGraphics(hgdip);
        DeleteDC(dc);
        FCaptionWidth := min(ceil(rect.Width), 150);
        FCaptionHeight := ceil(rect.Height);
        FBorder := FCaptionWidth + FCaptionHeight + 8;
      end;
    finally
      FUpdating:= false;
    end;
  except
    on e: Exception do raise Exception.Create('StackSubitem.UpdateItemInternal'#10#13 + e.message);
  end;

  Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);

  sendmessage(FHWndParent, WM_APP_UPDATE_PREVIEW, 0, 0);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateIndicator;
begin
  // make a local copy //
  // gdiplus does not like invalid pointers //
  try
    if FIndicator <> nil then GdipDisposeImage(FIndicator);
    FIndicatorW := theme.Indicator.W and $ffff;
    FIndicatorH := theme.Indicator.H and $ffff;
    GdipCloneBitmapAreaI(0, 0, FIndicatorW, FIndicatorH, PixelFormat32bppPARGB, theme.Indicator.Image, FIndicator);
    if FRunning and IsWindowVisible(FHWnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);
  except
    on e: Exception do raise Exception.Create('StackSubitem.UpdateIndicator'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TShortcutSubitem.Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; AForce: boolean);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  hattr, brush, path, hfont, hfontfamily: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  rect: TRectF;
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
      ItemRect := GetRectFromSize(FSize);
      xReal := Ax - ItemRect.Left - FSize div 2;
      yReal := Ay - ItemRect.Top - FSize div 2;
    except
      on e: Exception do raise Exception.Create('SetPosition'#10#13 + e.message);
    end;

    // init drawing //
    try
      bmp.topleft.x := xReal;
      bmp.topleft.y := yReal;
      bmp.width := FSize + ItemRect.Left * 2;
      bmp.height := FSize + ItemRect.Top * 2;
      if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
      dst := CreateGraphics(bmp.dc, 0);
      if not assigned(dst) then raise Exception.Create('CreateGraphics failed');
      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 2, ItemRect.Bottom - ItemRect.Top + 2);
      GdipDeleteBrush(brush);

      GdipSetCompositingMode(dst, CompositingModeSourceOver);
      GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
      GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
      GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

      xBitmap := ItemRect.Left;
      yBitmap := ItemRect.Top;

      if AAngle > 0 then
      begin
        xBitmap := -FSize div 2;
        yBitmap := -FSize div 2;
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
      end;

    except
      on e: Exception do raise Exception.Create('InitDraw'#10#13 + e.message);
    end;

    // draw icon //
    TCustomItem.CreateColorAttributes(color_data, FSelected, hattr);
    if assigned(FImage) then GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if hattr <> nil then GdipDisposeImageAttributes(hattr);

    if FRunning and (AAlpha > 10) then DrawIndicator(dst, xBitmap, yBitmap);
    if AAngle > 0 then GdipResetWorldTransform(dst);

    // hint (caption) //
    if FShowHint and (length(FCaption) > 0) and ((AHintAlign >= 0) and (AHintAlign <= 7)) and (AHintAlpha > 24) then
    begin
      if AHintAlign = 4 then
      begin
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        xBitmap := -FSize div 2 - FCaptionHeight div 2 - FCaptionWidth - 5;
        yBitmap := -FCaptionHeight div 2;
      end else
      if AHintAlign = 5 then
      begin
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle - 90, MatrixOrderPrepend);
        xBitmap := FSize div 2 + FCaptionHeight div 2 + 5;
        yBitmap := -FCaptionHeight div 2;
      end else
      if AHintAlign = 6 then
      begin
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        xBitmap := FSize div 2 + FCaptionHeight div 2 + 5;
        yBitmap := -FCaptionHeight div 2;
      end else
      if AHintAlign = 7 then
      begin
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle + 90, MatrixOrderPrepend);
        xBitmap := FSize div 2 + FCaptionHeight div 2 + 5;
        yBitmap := -FCaptionHeight div 2;
      end else
      if AHintAlign = 0 then
      begin
        GdipTranslateWorldTransform(dst, ItemRect.Left + FSize div 2, ItemRect.Top + FSize div 2, MatrixOrderPrepend);
        GdipRotateWorldTransform(dst, AAngle, MatrixOrderPrepend);
        xBitmap := -FCaptionWidth div 2;
        yBitmap := FSize div 2 + 3;
      end;
      //
      try GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, hfontfamily);
      except on e: Exception do raise Exception.Create('Caption.CreateFontFamily'#10#13 + e.message);
      end;
      GdipCreateFont(hfontfamily, FFont.size, integer(FFont.bold) + integer(FFont.italic) * 2, 2, hfont);
      GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
      GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
      //
      GdipCreatePath(FillModeWinding, path);
      GdipAddPathRectangle(path, xBitmap, yBitmap - 1, FCaptionWidth - 3, FCaptionHeight + 1);
      GdipAddPathEllipse(path, xBitmap - FCaptionHeight div 2, yBitmap - 1, FCaptionHeight + 2, FCaptionHeight + 1);
      GdipAddPathEllipse(path, xBitmap + FCaptionWidth - FCaptionHeight div 2 - 3, yBitmap - 1, FCaptionHeight + 2, FCaptionHeight + 1);
      GdipCreateSolidFill(AHintAlpha * 12 div 16 * $1000000 + FFont.color_outline and $ffffff, brush);
      GdipFillPath(dst, brush, path);
      GdipDeleteBrush(brush);
      GdipDeletePath(path);
      //
      rect.X := xBitmap;
      rect.Y := yBitmap;
      rect.Width := FCaptionWidth;
      rect.Height := FCaptionHeight;
      GdipCreateSolidFill(AHintAlpha * $1000000 + FFont.color and $ffffff, brush);
      GdipDrawString(dst, PWideChar(WideString(FCaption)), -1, hfont, @rect, nil, brush);
      GdipDeleteBrush(brush);
      GdipDeleteFont(hfont);
      GdipDeleteFontFamily(hfontfamily);
      //
      GdipResetWorldTransform(dst);
    end;

    // update window content //
    UpdateLWindow(FHWnd, bmp, AAlpha);

    // cleanup //
    DeleteGraphics(dst);
    DeleteBitmap(bmp);

  except
    on e: Exception do raise Exception.Create('StackSubitem.Draw(' + caption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.DrawPreview(graphics: Pointer; Ax, Ay, ASize: integer);
var
  matrix: ColorMatrix;
  hattr: Pointer;
begin
  try
    hattr := nil;
    if color_data <> DEFAULT_COLOR_DATA then
    begin
      CreateColorMatrix(color_data, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);
    end;
    GdipDrawImageRectRectI(graphics, FImage, Ax, Ay, ASize, ASize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if color_data <> DEFAULT_COLOR_DATA then GdipDisposeImageAttributes(hattr);
  except
    on e: Exception do raise Exception.Create('StackSubitem.DrawPreview'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.DrawIndicator(dst: Pointer; xBitmap: integer; yBitmap: integer);
begin
  if FIndicator = nil then exit;
  if FSite = 0 then
  begin
    xBitmap -= FIndicatorW div 2;
    yBitmap += (FSize - FIndicatorH) div 2;
  end
  else
  if FSite = 1 then
  begin
    xBitmap += (FSize - FIndicatorW) div 2;
    yBitmap -= FIndicatorH div 2;
  end
  else
  if FSite = 2 then
  begin
    xBitmap += FSize - FIndicatorW div 2;
    yBitmap += (FSize - FIndicatorH) div 2;
  end
  else
  if FSite = 3 then
  begin
    xBitmap += (FSize - FIndicatorW) div 2;
    yBitmap += FSize - FIndicatorH div 2;
  end;
  GdipDrawImageRectRectI(dst, FIndicator, xBitmap, yBitmap, FIndicatorW, FIndicatorH,
    0, 0, FIndicatorW, FIndicatorH, UnitPixel, nil, nil, nil);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ToString: string;
begin
  result := Make(FHWnd, FCaption, command, params, dir, imagefile, showcmd, color_data, hide);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.SaveToString: string;
begin
  result := SaveMake(FCaption, command, params, dir, imagefile, showcmd, color_data, hide);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
  inherited;
  cmd(icSelect, 1);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
var
  pt: windows.TPoint;
begin
  inherited;
  result := false;
  if FFreed then exit;

  if button = mbLeft then
  begin
    if FSelected and (abs(gettickcount - LastMouseUp) > FLaunchInterval) then
    begin
      Exec;
      CloseStack;
    end;
    LastMouseUp := gettickcount;
    result := true;
  end;

  if button = mbRight then
  begin
    windows.GetCursorPos(pt);
    result := ContextMenu(pt);
  end;

  // try..except if item was deleted //
  try cmd(icSelect, 0);
  except end;
  SetActiveWindow(FHWndParent);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ContextMenu(pt: Windows.TPoint): boolean;
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
  WMCommand(msg);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.WMCommand(var msg: TMessage);
begin
  try
    DestroyMenu(FHMenu);
    CloseStack;
    msg.Result := 0;
    case msg.wparam of // f001 to f020
      $f001: Configure;
      $f002: OpenFolder;
      $f003: toolu.SetClipboard(ToString);
      $f004: Delete;
      $f005: ProcessHelper.RunAsUser(command, params, dir, showcmd);
      $f006..$f020: ;
      else sendmessage(FHWndParent, WM_COMMAND, msg.wParam, msg.lParam);
    end;
  except
    on e: Exception do raise Exception.Create('TStackSubitem.WMCommand'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.Configure;
begin
  TfrmItemProp.Open(ToString, UpdateItem);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.Exec;
var
  sei: TShellExecuteInfo;
begin
  if is_pidl then
  begin
    if hide then dockh.DockExecute(FHWnd, '/hide', '', '', 0);
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
      if hide then DockExecute(FHWnd, '/hide', '', '', 0);
      DockExecute(FHWnd, pchar(command), pchar(params), pchar(dir), showcmd);
    end;
  end;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ActivateProcessMainWindow: boolean;
begin
  result := ProcessHelper.ActivateProcessMainWindow(UnzipPath(command), FHWnd, ScreenRect, FSite);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.CanOpenFolder: boolean;
var
  _file: string;
begin
  _file := toolu.UnzipPath(command);
  if not fileexists(_file) or not directoryexists(_file) then _file := ExtractFilePath(toolu.FindFile(_file));
  result := fileexists(_file) or directoryexists(_file);
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.OpenFolder;
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
function TShortcutSubitem.DropFile(pt: windows.TPoint; filename: string): boolean;
var
  ext: string;
begin
  result := not FFreed;
  if result then
  begin
    ext := AnsiLowerCase(ExtractFileExt(filename));
    if (ext = '.png') or (ext = '.ico') then
    begin
      imagefile := toolu.ZipPath(filename);
      color_data := DEFAULT_COLOR_DATA;
      UpdateItemInternal;
    end
    else
    begin
      if not is_pidl then DockExecute(FHWnd, pchar(command), pchar('"' + filename + '"'), nil, 1);
    end;
  end;
end;
//------------------------------------------------------------------------------
class function TShortcutSubitem.Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
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
class function TShortcutSubitem.SaveMake(ACaption, ACommand, AParams, ADir, AImage: string;
  AShowCmd: integer = 1; color_data: integer = DEFAULT_COLOR_DATA; hide: boolean = false): string;
begin
  result := 'class="shortcut";';
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
class function TShortcutSubitem.FromFile(filename: string): string;
var
  fcaption, fparams, fdir, ficon, ext: string;
begin
  result := '';
  if IsGUID(filename) then
  begin
    result := TShortcutSubitem.Make(0, '::::', filename, '', '', '', 1);
    exit
  end;

  fparams := '';
  fdir := '';
  ficon := '';
  ext := AnsiLowerCase(ExtractFileExt(filename));

  if DirectoryExists(filename) then fcaption := filename
  else fcaption := ChangeFileExt(ExtractFilename(filename), '');
  if ext = '.exe' then fdir := ExcludeTrailingPathDelimiter(ExtractFilePath(filename));

  result := TShortcutSubitem.Make(0, fcaption, ZipPath(filename), ZipPath(fparams), ZipPath(fdir), ZipPath(ficon));
end;
//------------------------------------------------------------------------------
//
//
//
//
//   TCustomSubitem
//
//
//
//
//------------------------------------------------------------------------------
constructor TCustomSubitem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited Create;
  Init;

  FHWndParent := AHWndParent;
  FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow + ws_ex_acceptfiles, WINITEM_CLASS, nil, ws_popup, -1000, -1000, 32, 32, FHWndParent, 0, hInstance, nil);
  if not IsWindow(FHWnd) then
  begin
    FFreed := true;
    exit;
  end;

  SetWindowLong(FHWnd, GWL_USERDATA, cardinal(self));
  // change window proc
  FWndInstance := MakeObjectInstance(self.NativeWndProc);
  FPrevWndProc := Pointer(GetWindowLongPtr(FHWnd, GWL_WNDPROC));
  SetWindowLongPtr(FHWnd, GWL_WNDPROC, LongInt(FWndInstance));

  FItemSize := AParams.ItemSize;
  FLaunchInterval := AParams.LaunchInterval;
  FActivateRunningDefault := AParams.ActivateRunning;
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.Init;
begin
  FPrevWndProc := nil;
  FFreed:= false;
  FQueryDelete := false;
  FEnabled:= true;
  FHWnd:= 0;
  FCaption:= '';
  Fx:= -1000;
  Fy:= -1000;
  FSize:= 32;
  FBorder := 8;
  FCaption := '';
  FConfigurable:= true;
  FUpdating:= false;
  FSelected:= false;
  FRunning:= false;
  FShowHint:= true;
  FSite:= 3;
  FItemSize := 32;
  FImage := nil;
  FIW := 32;
  FIH := 32;
end;
//------------------------------------------------------------------------------
destructor TCustomSubitem.Destroy;
begin
  // restore window proc
  if assigned(FPrevWndProc) then SetWindowLong(FHWnd, GWL_WNDPROC, LongInt(FPrevWndProc));
  if IsWindow(FHWnd) then DestroyWindow(FHWnd);
  inherited;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.cmd(id: TGParam; param: integer): integer;
begin
  result:= 0;
  try
    case id of
      // parameters //
      gpItemSize: FItemSize := param;
      gpSite: FSite := param;
      gpShowHint: FShowHint := boolean(param);
      gpLaunchInterval: FLaunchInterval := param;
      gpActivateRunning: FActivateRunningDefault := boolean(param);
      gpLockDragging: FLockDragging := param <> 0;

      // commands //

      icSelect:
        if FSelected <> boolean(param) then
        begin
          FSelected := boolean(param);
          if IsWindowVisible(FHwnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);
        end;
    end;

  except
    on e: Exception do raise Exception.Create('TCustomSubitem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.ToString: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer);
begin
  if FFreed then exit;
  if button = mbLeft then
  begin
    SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 1200, nil);
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  result := false;
  if FFreed then exit;
  result := true;
  KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.MouseHeld(button: TMouseButton);
var
  pt: windows.TPoint;
begin
  if button = mbLeft then
  begin
    GetCursorPos(pt);
    if WindowFromPoint(pt) = FHWnd then BeginDrag;
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.GetRectFromSize(ASize: integer): windows.TRect;
begin
  result := classes.rect(FBorder, FBorder, FBorder + ASize, FBorder + ASize);
end;
//------------------------------------------------------------------------------
// item rect in client coordinates
function TCustomSubitem.GetClientRect: windows.TRect;
begin
  result := GetRectFromSize(FSize);
end;
//------------------------------------------------------------------------------
// item rect in screen coordinates
function TCustomSubitem.GetScreenRect: windows.TRect;
var
  r: windows.TRect;
begin
  result := GetRectFromSize(FSize);
  GetWindowRect(FHWnd, @r);
  inc(result.Left, r.Left);
  inc(result.Right, r.Left);
  inc(result.Top, r.Top);
  inc(result.Bottom, r.Top);
end;
//------------------------------------------------------------------------------
function TCustomSubitem.HitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetClientRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
function TCustomSubitem.ScreenHitTest(Ax, Ay: integer): boolean;
begin
  result := ptinrect(GetScreenRect, classes.Point(Ax, Ay));
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.CloseStack;
begin
  sendmessage(FHWndParent, WM_ACTIVATE, 0, 0);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.Delete(AllowUndo: boolean = true);
var
  wnd: THandle;
  Inst: TCustomItem;
  pt: windows.TPoint;
begin
  if AllowUndo then
  begin
    wnd := dockh.DockCreateItem(pchar(ToString));
    Inst := TCustomItem(GetWindowLong(wnd, GWL_USERDATA));
    if Inst is TCustomItem then
    begin
      GetCursorPos(pt);
      Inst.Draw(pt.x - Inst.Size div 2, pt.y - Inst.Size div 2, FSize, true, 0, SWP_SHOWWINDOW);
      SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + SWP_SHOWWINDOW);
      Inst.cmd(icFloat, 0);
      Inst.Delete;
    end;
  end;
  FQueryDelete := true;
  ShowWindow(FHWnd, SW_HIDE);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.BeginDrag;
var
  wnd: THandle;
  Inst: TCustomItem;
  pt: windows.TPoint;
begin
  wnd := dockh.DockCreateItem(pchar(ToString));
  Inst := TCustomItem(GetWindowLong(wnd, GWL_USERDATA));
  if Inst is TCustomItem then
  begin
    GetCursorPos(pt);
    Inst.Draw(pt.x - Inst.Size div 2, pt.y - Inst.Size div 2, FSize, true, 0, SWP_SHOWWINDOW);
    SetWindowPos(wnd, HWND_TOPMOST, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + SWP_SHOWWINDOW);
    Inst.cmd(icFloat, 1);
  end;
  Delete(false);
  dockh.Undock(wnd);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.NativeWndProc(var message: TMessage);
var
  pos: TSmallPoint;
  ShiftState: TShiftState;
begin
  with message do
  begin
    pos := TSmallPoint(lParam);
    ShiftState := [];
    if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
    if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);
  end;

  if message.msg = wm_lbuttondown then
  begin
      try
        SetActiveWindow(FHWndParent);
        MouseDownPoint.x:= pos.x;
        MouseDownPoint.y:= pos.y;
        if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y);
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.NativeWndProc.wm_lbuttondown'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_rbuttondown then
  begin
      try
        SetActiveWindow(FHWndParent);
        MouseDownPoint.x:= pos.x;
        MouseDownPoint.y:= pos.y;
        if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y);
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.NativeWndProc.wm_rbuttondown'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_mbuttondown then
  begin
      SetActiveWindow(FHWndParent);
  end
  else if message.msg = wm_lbuttonup then
  begin
      try
        if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y);
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.NativeWndProc.wm_lbuttonup'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_rbuttonup then
  begin
      try
        if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y);
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.NativeWndProc.wm_rbuttonup'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_mousemove then
  begin
      try
        if not FLockDragging and (message.wParam and MK_LBUTTON <> 0) then
        begin
          if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then BeginDrag;
        end;
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.WindowProc.wm_mousemove'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_command then
  begin
      try
        WMCommand(message);
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.NativeWndProc.wm_command'#10#13 + e.message);
      end;
  end
  else if message.msg = wm_timer then
  begin
      // mouse held //
      try
        if message.wParam = ID_TIMER_MOUSEHELD then
        begin
          KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
          MouseHeld(mbLeft);
        end;
      except
        on e: Exception do raise Exception.Create('TCustomSubitem.WindowProc.wm_timer.dragdrop'#10#13 + e.message);
      end;
  end
  else if (message.msg = wm_close) or (message.msg = wm_quit) then exit;

  with message do result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------
end.
 
