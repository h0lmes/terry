unit stacksubitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math, ComObj, ShlObj,
  GDIPAPI, gfx, PIDL, ShContextU, declu, dockh, toolu, customitemu, processhlp;

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
    FIsExecutable: boolean;
    FExecutable: string;

    FEnabled: boolean;
    FUpdating: boolean;
    FSelected: boolean; // when mouse button is down - icon becomes darken //
    FRunning: boolean;
    FShowHint: boolean;
    FSite: integer;
    FItemSize: integer;
    FLaunchInterval: integer;
    FActivateRunning: boolean;
    MouseDownPoint: windows.TPoint;
    FMouseDownButton: TMouseButton;
    FLockDragging: boolean;

    FImage: Pointer;
    FIW: uint; // image width
    FIH: uint; // image height

    FWndInstance: TFarProc;
    FPrevWndProc: TFarProc;

    procedure Init; virtual;
    procedure Redraw;
    function GetRectFromSize(ASize: integer): windows.TRect;
    function GetClientRect: windows.TRect;
    function GetScreenRect: windows.TRect;
    procedure CloseStack;
    procedure UpdateItemMeasureCaption;
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
    procedure SetFont(var Value: _FontData);

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
    FUseShellContextMenus: boolean;
    FIsPIDL: boolean;
    FPIDL: PITEMIDLIST;
    FLastMouseUp: cardinal;
    procedure UpdateItemI;
    procedure UpdateItemRunningState;
    procedure Exec(action: TExecuteAction);
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
    procedure MouseHeld(button: TMouseButton); override;
    procedure WMCommand(var msg: TMessage); override;
    procedure Configure; override;
    function cmd(id: TGParam; param: integer): integer; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(pt: windows.TPoint; filename: string): boolean; override;

    class function Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
    class function SaveMake(ACaption, ACommand, AParams, ADir, AImage: string;
      AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
    class function FromFile(filename: string): string;
  end;

implementation
uses themeu, frmitemoptu;
//------------------------------------------------------------------------------
constructor TShortcutSubitem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;

  FLastMouseUp:= 0;
  FCommand:= '';
  FParams:= '';
  FDir:= '';
  FImageFile:= '';
  FColorData:= DEFAULT_COLOR_DATA;
  FShowCmd:= 0;
  FHide:= false;

  UpdateItem(AData);
end;
//------------------------------------------------------------------------------
destructor TShortcutSubitem.Destroy;
begin
  FFreed := true;
  try GdipDisposeImage(FImage);
  except on e: Exception do raise Exception.Create('StackSubitem.Destroy.GdipDisposeImage'#10#13 + e.message);
  end;
  try if FIsPIDL then PIDL_Free(FPIDL);
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
            Redraw;
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpShowHint: UpdateItemI;
      gpSite: if FRunning then Redraw;
      tcThemeChanged: if FRunning then Redraw;

      // commands //
      icUpdateRunning: UpdateItemRunningState;
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
  except
    on e: Exception do raise Exception.Create('StackSubitem.UpdateItem.Data'#10#13 + e.message);
  end;

  UpdateItemI;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateItemI;
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

      // convert CSIDL to path //
      csidl := CSIDL_ToInt(FCommand);
      if csidl > -1 then
      begin
        OleCheck(SHGetSpecialFolderLocation(0, csidl or CSIDL_FLAG_NO_ALIAS, pidFolder));
        PIDL_GetDisplayName(nil, pidFolder, SHGDN_FORPARSING, pszName, 255);
        PIDL_Free(pidFolder);
        FCommand := strpas(pszName);
        if FileExists(FCommand) or DirectoryExists(FCommand) then FCommand := ZipPath(FCommand)
        else FCaption := '::::';  // assuming it is a PIDL
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

      // load images //
      try if FImage <> nil then GdipDisposeImage(FImage);
      except end;
      FImage := nil;
      if FImageFile <> '' then LoadImage(UnzipPath(FImageFile), FItemSize, true, true, FImage, FIW, FIH)
      else
      begin
        if FIsPIDL then LoadImageFromPIDL(FPIDL, FItemSize, true, true, FImage, FIW, FIH)
        else LoadImage(UnzipPath(FCommand), FItemSize, true, true, FImage, FIW, FIH);
      end;

      // measure caption and adjust border size //
      UpdateItemMeasureCaption;
    finally
      FUpdating:= false;
    end;

    Redraw;
    sendmessage(FHWndParent, WM_APP_UPDATE_PREVIEW, 0, 0); // notify parent stack item
  except
    on e: Exception do raise Exception.Create('StackSubitem.UpdateItemInternal'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.UpdateItemRunningState;
var
  b: boolean;
begin
  if length(FExecutable) > 0 then
  begin
    b := ProcessHelper.ProcessExists(FExecutable);
    if b <> FRunning then
    begin
      FRunning := b;
      Redraw;
    end;
  end;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TShortcutSubitem.Draw(Ax, Ay, ASize: integer; AAlpha: integer; AAngle: single; AHintAlign: integer; AHintAlpha: integer; AForce: boolean);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  hattr, brush, path, font, family: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
  rect: TRectF;
  points: array [0..3] of GDIPAPI.TPoint;
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
      GdipSetInterpolationMode(dst, InterpolationModeBilinear);

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
    CreateColorAttributes(FColorData, FSelected, hattr);
    if assigned(FImage) then GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if hattr <> nil then GdipDisposeImageAttributes(hattr);

    GdipSetCompositingMode(dst, CompositingModeSourceOver);
    if FRunning and (AAlpha > 127) then theme.DrawIndicator(dst, xBitmap, yBitmap, FSize, FSite);
    GdipResetWorldTransform(dst);

    // hint (caption) //
    if FShowHint and (length(FCaption) > 0) and ((AHintAlign >= 0) and (AHintAlign <= 7)) and (AHintAlpha > 25) then
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
      // hint background
      GdipCreatePath(FillModeWinding, path);
      points[0].x := xBitmap - FCaptionHeight div 4;
      points[0].y := yBitmap - 1;
      points[1].x := points[0].x + FCaptionWidth - 3 + FCaptionHeight div 2;
      points[1].y := points[0].y;
      points[2].x := points[1].x;
      points[2].y := points[0].y + FCaptionHeight + 1;
      points[3].x := points[0].x;
      points[3].y := points[2].y;
      GdipAddPathClosedCurve2I(path, points, 4, 15 / FCaptionWidth);
      GdipCreateSolidFill(AHintAlpha shl 24 + FFont.backcolor and $ffffff, brush);
      GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
      GdipFillPath(dst, brush, path);
      GdipDeleteBrush(brush);
      GdipDeletePath(path);
      //
      rect.X := xBitmap;
      rect.Y := yBitmap;
      rect.Width := FCaptionWidth;
      rect.Height := FCaptionHeight;
      GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, family);
      GdipCreateFont(family, FFont.size2, integer(FFont.bold) + integer(FFont.italic) * 2, 2, font);
      GdipCreateSolidFill(AHintAlpha shl 24 + FFont.color and $ffffff, brush);
      GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
      GdipDrawString(dst, PWideChar(WideString(FCaption)), -1, font, @rect, nil, brush);
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
    if FColorData <> DEFAULT_COLOR_DATA then
    begin
      CreateColorMatrix(FColorData, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);
    end;
    GdipDrawImageRectRectI(graphics, FImage, Ax, Ay, ASize, ASize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if FColorData <> DEFAULT_COLOR_DATA then GdipDisposeImageAttributes(hattr);
  except
    on e: Exception do raise Exception.Create('StackSubitem.DrawPreview'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.ToString: string;
begin
  result := Make(FHWnd, FCaption, FCommand, FParams, FDir, FImageFile, FShowCmd, FColorData, FHide);
end;
//------------------------------------------------------------------------------
function TShortcutSubitem.SaveToString: string;
begin
  result := SaveMake(FCaption, FCommand, FParams, FDir, FImageFile, FShowCmd, FColorData, FHide);
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
begin
  inherited;
  result := false;

  if not FFreed and FSelected then
  begin
    cmd(icSelect, 0);

    if button = mbLeft then
    begin
      if abs(gettickcount - FLastMouseUp) > FLaunchInterval then
      begin
        if ssAlt in shift then Exec(eaGroup)
        else
        if ssCtrl in shift then Exec(eaRun)
        else
          Exec(eaDefault);
        CloseStack;
      end;
      FLastMouseUp := gettickcount;
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
  if FRunning then AppendMenu(FHMenu, MF_STRING, $f006, pchar(UTF8ToAnsi(XRun)));
  mii.cbSize := sizeof(MENUITEMINFO);
  mii.fMask := MIIM_STATE;
  mii.fState := MFS_DEFAULT;
  SetMenuItemInfo(FHMenu, $f006, false, @mii);
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  if CanOpenFolder then AppendMenu(FHMenu, MF_STRING, $f002, PChar(UTF8ToAnsi(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));

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
  if not result then msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TShortcutSubitem.WMCommand(var msg: TMessage);
begin
  try
    DestroyMenu(FHMenu);
    FHMenu := 0;
    CloseStack;
    msg.Result := 0;
    case msg.wparam of // f001 to f020
      $f001: Configure;
      $f002: OpenFolder;
      $f003: toolu.SetClipboard(ToString);
      $f004: Delete;
      $f006: Exec(eaRun);
      //$f007..$f020: ;
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
class function TShortcutSubitem.Make(AHWnd: uint; ACaption, ACommand, AParams, ADir, AImage: string;
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
class function TShortcutSubitem.SaveMake(ACaption, ACommand, AParams, ADir, AImage: string;
  AShowCmd: integer = 1; AColorData: integer = DEFAULT_COLOR_DATA; AHide: boolean = false): string;
begin
  result := 'class="shortcut";';
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
class function TShortcutSubitem.FromFile(filename: string): string;
var
  fcaption, fparams, fdir, ficon, ext: string;
begin
  result := '';
  if IsGUID(filename) or IsPIDLString(filename) then
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
  FActivateRunning := AParams.ActivateRunning;
  FSite := AParams.Site;
  FShowHint := AParams.ShowHint;
  FLockDragging := AParams.LockDragging;
  CopyFontData(AParams.Font, FFont);
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
procedure TCustomSubitem.Redraw;
begin
  if IsWindowVisible(FHWnd) then Draw(Fx, Fy, FSize, 255, FAngle, FHintAlign, FHintAlpha, true);
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
      gpActivateRunning: FActivateRunning := boolean(param);
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
  if not FFreed then
  begin
    FMouseDownButton := button;
    if button = mbLeft then SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 1000, nil)
    else SetTimer(FHWnd, ID_TIMER_MOUSEHELD, 800, nil);
    cmd(icSelect, 1);
  end;
end;
//------------------------------------------------------------------------------
function TCustomSubitem.MouseUp(button: TMouseButton; shift: TShiftState; x, y: integer): boolean;
begin
  KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
  result := not FFreed;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.MouseHeld(button: TMouseButton);
begin
  FSelected := false;
  if button = mbLeft then BeginDrag;
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
      Inst.cmd(icUndock, 0);
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
    Inst.cmd(icUndock, 1);
  end;
  Delete(false);
  dockh.Undock(wnd);
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.SetFont(var Value: _FontData);
begin
  CopyFontData(Value, FFont);
  UpdateItemMeasureCaption;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.UpdateItemMeasureCaption;
var
  hgdip, hfont, hfontfamily: Pointer;
  rect: TRectF;
  dc: HDC;
begin
  FBorder := 8;
  FCaptionWidth := 0;
  FCaptionHeight := 0;
  if FShowHint and (length(FCaption) > 0) then
  begin
    dc := CreateCompatibleDC(0);
    if dc = 0 then raise Exception.Create('CustomSubitem.UpdateItemMeasureCaption.CreateCompatibleDC failed');
    GdipCreateFromHDC(dc, hgdip);
    if Ok <> GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, hfontfamily) then
      raise Exception.Create('CustomSubitem.UpdateItemMeasureCaption.CreateFontFamily failed');
    GdipCreateFont(hfontfamily, FFont.size2, integer(FFont.bold) + integer(FFont.italic) * 2, 2, hfont);
    rect.x := 0;
    rect.y := 0;
    rect.Width := 0;
    rect.Height := 0;
    GdipMeasureString(hgdip, PWideChar(WideString(FCaption)), -1, hfont, @rect, nil, @rect, nil, nil);
    GdipDeleteGraphics(hgdip);
    DeleteDC(dc);
    FCaptionWidth := min(ceil(rect.Width), 150);
    FCaptionHeight := ceil(rect.Height);
    FBorder := FCaptionWidth + FCaptionHeight + 8;
  end;
end;
//------------------------------------------------------------------------------
procedure TCustomSubitem.NativeWndProc(var message: TMessage);
var
  pos: TSmallPoint;
  wpt: windows.TPoint;
  ShiftState: TShiftState;
begin
  try
      with message do
      begin
        result := 0;
        pos := TSmallPoint(lParam);
        ShiftState := [];
        if wParam and MK_SHIFT <> 0 then Include(ShiftState, ssShift);
        if wParam and MK_CONTROL <> 0 then Include(ShiftState, ssCtrl);
      end;

      if message.msg = wm_lbuttondown then
      begin
            SetActiveWindow(FHWndParent);
            MouseDownPoint.x:= pos.x;
            MouseDownPoint.y:= pos.y;
            if HitTest(pos.x, pos.y) then MouseDown(mbLeft, ShiftState, pos.x, pos.y);
      end
      else if message.msg = wm_rbuttondown then
      begin
            SetActiveWindow(FHWndParent);
            MouseDownPoint.x:= pos.x;
            MouseDownPoint.y:= pos.y;
            if HitTest(pos.x, pos.y) then MouseDown(mbRight, ShiftState, pos.x, pos.y);
      end
      else if message.msg = wm_mbuttondown then
      begin
          SetActiveWindow(FHWndParent);
      end
      else if message.msg = wm_lbuttonup then
      begin
            if HitTest(pos.x, pos.y) then MouseUp(mbLeft, ShiftState, pos.x, pos.y);
      end
      else if message.msg = wm_rbuttonup then
      begin
            if HitTest(pos.x, pos.y) then MouseUp(mbRight, ShiftState, pos.x, pos.y);
      end
      else if message.msg = wm_mousemove then
      begin
            if not FLockDragging and (message.wParam and MK_LBUTTON <> 0) then
            begin
              if (abs(pos.x - MouseDownPoint.x) >= 4) or (abs(pos.y - MouseDownPoint.y) >= 4) then BeginDrag;
            end;
      end
      else if message.msg = wm_command then
      begin
            WMCommand(message);
      end
      else if message.msg = wm_timer then
      begin
            if message.wParam = ID_TIMER_MOUSEHELD then
            begin
              KillTimer(FHWnd, ID_TIMER_MOUSEHELD);
              GetCursorPos(wpt);
              if WindowFromPoint(wpt) = FHWnd then MouseHeld(FMouseDownButton);
            end;
      end
      else if (message.msg = wm_close) or (message.msg = wm_quit) then exit;

  except
    on e: Exception do raise Exception.Create('CustomSubitem.WindowProc[ Msg=0x' + inttohex(message.msg, 8) + ' ]'#10#13 + e.message);
  end;

  with message do result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------
end.
 
