unit stackitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, ComObj, ShlObj,
  Math, IniFiles,
  GDIPAPI, PIDL, gdip_gfx, declu, customitemu, stacksubitemu, stackmodeu;

const
  MAX_SUBITEMS = 64;
  STATE_PROGRESS_MIN = 0.0;
  STATE_PROGRESS_MAX = 1.0;
  DEFAULT_ANIM_SPEED = 5;
  DEFAULT_DISTORT = 1;

type
  TStackState = (stsClosed, stsOpening, stsOpen, stsClosing);

  PCSIBucket = ^TCSIBucket;
  TCSIBucket = record
    x: integer;
    y: integer;
    s: integer;
    alpha: integer;
    angle: single;
    hint_align: integer;
    hint_alpha: integer;
    draw: boolean;
    hWnd: HANDLE;
    item: TCustomSubitem;
  end;

  TStackItem = class(TCustomItem)
  private
    FRunning: boolean;
    FIndicator: Pointer;
    FIndicatorW: integer;
    FIndicatorH: integer;
    FUseShellContextMenus: boolean;
    imagefile: string;
    color_data: integer;
    items: array of TCSIBucket; // using a dynamic array. static causes obscure error while deleting stackitem
    FItemCount: integer;
    FState: TStackState;
    FStateProgress: extended;
    FMode: integer;
    FOffset: integer;
    FAnimationSpeed: integer;
    FDistort: integer;
    FDragOver: boolean;
    FSpecialFolder: integer;
    FPreview: boolean;
    FPreviewImage: pointer;
    FPreviewImageW: uint;
    FPreviewImageH: uint;
    procedure UpdateItemInternal;
    procedure UpdateIndicator;
    procedure DrawIndicator(dst: Pointer);
    procedure Exec;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure Configure;
    procedure OnDragEnter;
    procedure OnDragOver;
    procedure OnDragLeave;
    procedure DropFileI(filename: string);
    // items handling //
    procedure UpdateSpecialFolder;
    procedure AddSpecialFolder(csidl: integer);
    procedure UpdatePreview;
    function MakeICP: _ItemCreateParams;
    procedure CheckDeleteSubitems;
    procedure DeleteSubitems;
    procedure CopyCSIBucket(pFrom, pTo: PCSIBucket);
    function ItemIndex(HWnd: HANDLE): integer;
    procedure AllSubitemsCmd(id: TGParam; param: integer);
    procedure OpenStack;
    procedure CloseStack;
    procedure DoStateProgress;
    procedure ShowStackState;
  public
    property ItemCount: integer read FItemCount;

    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    procedure Init; override;
    procedure UpdateItem(AData: string); override;
    procedure UpdateImage(AImage: Pointer; AutoDelete: boolean); override;
    procedure UpdateOverlay(AOverlay: Pointer; AutoDelete: boolean); override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Timer; override;
    function GetItemFilename: string; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
    class function Make(AHWnd: uint; ACaption, AImage: string;
      color_data: integer = DEFAULT_COLOR_DATA; AMode: integer = 0;
      AOffset: integer = 0; AAnimationSpeed: integer = DEFAULT_ANIM_SPEED;
      ADistort: integer = DEFAULT_DISTORT; ASpecialFolder: integer = 0; APreview: boolean = true): string;

    procedure AddSubitemDefault;
    procedure AddSubitem(data: string);
    function GetSubitemCaption(index: integer): string;
    function SubitemToString(index: integer): string;
    procedure DeleteSubitem(index: integer);
    procedure SubitemMoveUp(index: integer);
    procedure SubitemMoveDown(index: integer);
    procedure SubitemConfigure(index: integer);
  end;

implementation
uses dockh, themeu, toolu, frmstackpropu;
//------------------------------------------------------------------------------
constructor TStackItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;
  UpdateItem(AData);
  UpdateIndicator;
end;
//------------------------------------------------------------------------------
procedure TStackItem.Init;
begin
  inherited;

  imagefile := '';
  color_data := DEFAULT_COLOR_DATA;
  FRunning := false;
  FItemCount := 0;
  SetLength(items, MAX_SUBITEMS);
  FState := stsClosed;
  FStateProgress := STATE_PROGRESS_MIN;
  FMode := 0;
  FOffset := 0;
  FAnimationSpeed := DEFAULT_ANIM_SPEED;
  FDistort := DEFAULT_DISTORT;
  FDragOver := false;
  FSpecialFolder := 0;
  FPreview := true;
  FPreviewImage := nil;
end;
//------------------------------------------------------------------------------
destructor TStackItem.Destroy;
begin
  FFreed := true;

  try GdipDisposeImage(FImage);
  except end;
  try if FPreviewImage <> nil then GdipDisposeImage(FPreviewImage);
  except end;

  DeleteSubitems;

  inherited;
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdateItem(AData: string);
var
  IniFile, IniSection: string;
  ini: TIniFile;
  i: integer;
  data: string;
begin
  if FFreed then exit;

  try
    try
      FUpdating := true;

      IniFile := FetchValue(AData, 'inifile="', '";');
      IniSection := FetchValue(AData, 'inisection="', '";');

      if (length(IniFile) > 0) and (length(IniSection) > 0) then
      begin
        ini := TIniFile.Create(IniFile);
        caption := ini.ReadString(IniSection, 'caption', '');
        imagefile := ini.ReadString(IniSection, 'image', '');
        color_data := toolu.StringToColor(ini.ReadString(IniSection, 'color_data', toolu.ColorToString(DEFAULT_COLOR_DATA)));
        try FMode := SetRange(strtoint(ini.ReadString(IniSection, 'mode', '0')), 0, 1000);
        except end;
        try FOffset := SetRange(strtoint(ini.ReadString(IniSection, 'offset', '0')), 0, 200);
        except end;
        try FAnimationSpeed := SetRange(strtoint(ini.ReadString(IniSection, 'animation_speed', inttostr(DEFAULT_ANIM_SPEED))), 1, 10);
        except end;
        try FDistort := SetRange(strtoint(ini.ReadString(IniSection, 'distort', inttostr(DEFAULT_DISTORT))), 1, 10);
        except end;
        try FSpecialFolder := SetRange(strtoint(ini.ReadString(IniSection, 'special_folder', '')), 0, $3f);
        except end;
        try FPreview := not (ini.ReadString(IniSection, 'preview', '') = '0');
        except end;
        UpdateSpecialFolder;

        i := 1;
        repeat
          data := ini.ReadString(IniSection, 'subitem' + inttostr(i), '');
          if data <> '' then AddSubitem(data);
          inc(i);
        until (data = '') or (i > MAX_SUBITEMS);
        ini.free;
      end
      else
      begin
        caption := FetchValue(AData, 'caption="', '";');
        imagefile := FetchValue(AData, 'image="', '";');
        color_data := DEFAULT_COLOR_DATA;
        FMode := 0;
        FOffset := 0;
        FAnimationSpeed := DEFAULT_ANIM_SPEED;
        FDistort := DEFAULT_DISTORT;
        FSpecialFolder := 0;
        try color_data := strtoint(FetchValue(AData, 'color_data="', '";'));
        except end;
        try FMode := strtoint(FetchValue(AData, 'mode="', '";'));
        except end;
        try FOffset := strtoint(FetchValue(AData, 'offset="', '";'));
        except end;
        try FAnimationSpeed := strtoint(FetchValue(AData, 'animation_speed="', '";'));
        except end;
        try FDistort := strtoint(FetchValue(AData, 'distort="', '";'));
        except end;
        try FSpecialFolder := strtoint(FetchValue(AData, 'special_folder="', '";'));
        except end;
        try FPreview := not (FetchValue(AData, 'preview="', '";') = '0');
        except end;
        UpdateSpecialFolder;
      end;

    finally
      FUpdating:= false;
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.UpdateItem'#10#13 + e.message);
  end;

  UpdateItemInternal;
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdateImage(AImage: Pointer; AutoDelete: boolean);
begin
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdateOverlay(AOverlay: Pointer; AutoDelete: boolean);
begin
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdateItemInternal;
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;
      // load images from files //
      LoadImage(imagefile, FBigItemSize, false, false, FImage, FIW, FIH);
      // default stack image //
      if FImage = nil then
      begin
        GdipCloneImage(theme.Stack.Image, FImage);
        FIW := theme.Stack.W;
        FIH := theme.Stack.H;
      end;
    finally
      FUpdating:= false;
    end;

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.UpdateItemInternal'#10#13 + e.message);
  end;

  Draw(Fx, Fy, FSize, true, 0, FShowItem);
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdateIndicator;
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
    on e: Exception do raise Exception.Create('StackItem.UpdateIndicator'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.cmd(id: TGParam; param: integer): integer;
var
  b: boolean;
  temp: uint;
  i: integer;
begin
  try
    result := inherited cmd(id, param);

    case id of
      // parameters //
      gpBigItemSize:
        begin
          if FBigItemSize <= 128 then temp := 128
          else if FBigItemSize <= 160 then temp := 160
          else if FBigItemSize <= 192 then temp := 192;
          if temp <> FIW then UpdateItemInternal;
        end;
      gpShowRunningIndicator:
        begin
          if FRunning and not boolean(param) then
          begin
            FRunning := false;
            Draw(Fx, Fy, FSize, true, 0, FShowItem);
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpSite:
        begin
          if FIndicator <> nil then UpdateIndicator;
          CloseStack;
        end;
      tcThemeChanged:
        begin
          UpdateItemInternal; // in order to update default stack image //
          if FIndicator <> nil then UpdateIndicator;
        end;

      // commands //

      icUpdateRunning:
        begin
          b := false;
          i := 0;
          while i < FItemCount do
          begin
            items[i].item.cmd(icUpdateRunning, 0);
            if items[i].item.Running then b := true;
            inc(i);
          end;
          if b and (FIndicator = nil) then UpdateIndicator;
          if b <> FRunning then
          begin
            FRunning := b;
            Draw(Fx, Fy, FSize, true, 0, FShowItem);
          end;
        end;
      icDragEnter: OnDragEnter;
      icDragOver: OnDragOver;
      icDragLeave: OnDragLeave;
      icIsItem:
        begin
          result := 0;
          if (FItemCount > 0) and (FState = stsOpen) then
          begin
            for i := 0 to FItemCount - 1 do
              if items[i].hWnd = HANDLE(param) then result := FHWnd;
          end;
        end;
    end;

    b := (id = gpItemSize) or (id = gpUseShellContextMenus) or (id = gpSite) or (id = gpShowHint) or
      (id = gpLaunchInterval) or (id = gpActivateRunning) or (id = gpLockDragging) or (id = tcThemeChanged);
    if (FItemCount > 0) and b then
    begin
      for i := 0 to FItemCount - 1 do items[i].item.cmd(id, param);
    end;

  except
    on e: Exception do raise Exception.Create('StackItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TStackItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  hattr, brush: Pointer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
begin
  try
    if FFreed or FUpdating or (FFloating and not AForce) then exit;

    // update subitems positions if stack is open //
    if FState = stsOpen then DoStateProgress;

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
      on e: Exception do raise Exception.Create('SetPosition(' + caption + ')'#10#13 + e.message);
    end;

    // init draw //
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
      inc(xBitmap, ItemRect.Left);
      inc(yBitmap, ItemRect.Top);
    except
      on e: Exception do raise Exception.Create('InitDraw'#10#13 + e.message);
    end;

    // draw icons //
    TCustomItem.CreateColorAttributes(color_data, FSelected, hattr);
    if assigned(FImage) then
      GdipDrawImageRectRectI(dst, FImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if FSelected or (color_data <> DEFAULT_COLOR_DATA) then GdipDisposeImageAttributes(hattr);

    if assigned(FPreviewImage) then
      GdipDrawImageRectRectI(dst, FPreviewImage, xBitmap, yBitmap, FSize, FSize, 0, 0, FPreviewImageW, FPreviewImageH, UnitPixel, nil, nil, nil);
    if FDropIndicator = 1 then
      if assigned(theme.DropIndicatorAdd.Image) then
        GdipDrawImageRectRectI(dst, theme.DropIndicatorAdd.Image, xBitmap, yBitmap, FSize, FSize, 0, 0, theme.DropIndicatorAdd.W, theme.DropIndicatorAdd.H, UnitPixel, nil, nil, nil);
    if FDropIndicator = 2 then
      if assigned(theme.DropIndicatorRun.Image) then
        GdipDrawImageRectRectI(dst, theme.DropIndicatorRun.Image, xBitmap, yBitmap, FSize, FSize, 0, 0, theme.DropIndicatorRun.W, theme.DropIndicatorRun.H, UnitPixel, nil, nil, nil);

    ////
    if FReflection and not FFloating then BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
    if FRunning then DrawIndicator(dst);
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));

    // cleanup //
    DeleteGraphics(dst);
    DeleteBitmap(bmp);
  except
    on e: Exception do raise Exception.Create('StackItem.Draw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DrawIndicator(dst: Pointer);
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
procedure TStackItem.Timer;
begin
  inherited;
  if not FFreed and not FUpdating then
    if (FState = stsOpening) or (FState = stsClosing) then DoStateProgress;
end;
//------------------------------------------------------------------------------
function TStackItem.GetItemFilename: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
function TStackItem.ToString: string;
begin
  result:= Make(FHWnd, FCaption, imagefile, color_data, FMode,
    FOffset, FAnimationSpeed, FDistort, FSpecialFolder, FPreview);
end;
//------------------------------------------------------------------------------
procedure TStackItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
begin
  if button = mbLeft then Exec;

  if button = mbRight then
  begin
    CloseStack;
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.MouseHeld(button: TMouseButton);
begin
  CloseStack;
  inherited;
end;
//------------------------------------------------------------------------------
function TStackItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XConfigureIcon)));
  AppendMenu(FHMenu, MF_STRING, $f003, pchar(UTF8ToAnsi(XCopy)));
  if IsValidShortcutString(GetClipboard) then AppendMenu(FHMenu, MF_STRING, $f005, pchar(UTF8ToAnsi(XPaste)));
  AppendMenu(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenu(FHMenu, MF_STRING, $f004, pchar(UTF8ToAnsi(XDeleteIcon)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TStackItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
begin
  result := 0;
  DestroyMenu(FHMenu);
  LME(false);
  case wParam of // f001 to f020
    $f001: Configure;
    $f002: ; // open folder
    $f003: toolu.SetClipboard(ToString);
    $f004: Delete;
    $f005: AddSubitem(GetClipboard);
    $f006..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.Configure;
begin
  TfrmStackProp.Open(ToString, UpdateItem, self);
end;
//------------------------------------------------------------------------------
procedure TStackItem.WndMessage(var msg: TMessage);
var
  i: integer;
  found: boolean;
begin
  if FFreed then exit;

  with msg do
  begin
      Result := 0;

      // WM_ACTIVATEAPP
      if (Msg = WM_ACTIVATEAPP) and (wParam = 0) then CloseStack;

      // WM_ACTIVATE
      if (Msg = WM_ACTIVATE) and (wParam and $f = 0) then
      begin
        found := false;
        if FItemCount > 0 then
          for i := 0 to FItemCount - 1 do
            if items[i].hWnd = HANDLE(lParam) then found := true;
        if not found then CloseStack;
      end;

      // WM_TIMER (OPEN)
      if (Msg = WM_TIMER) and (wParam = ID_TIMER_OPEN) then
      begin
        KillTimer(FHWnd, ID_TIMER_OPEN);
        OpenStack;
      end;

      // WM_TIMER (CLOSE)
      if (Msg = WM_TIMER) and (wParam = ID_TIMER_CLOSE) then
      begin
        KillTimer(FHWnd, ID_TIMER_CLOSE);
        CloseStack;
      end;

      // WM_APP_UPDATE_PREVIEW private message //
      if (Msg = WM_APP_UPDATE_PREVIEW) and not FUpdating then UpdatePreview;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.Exec;
begin
  if FState = stsClosed then OpenStack
  else
  if FState = stsOpen then CloseStack;
end;
//------------------------------------------------------------------------------
function TStackItem.CanOpenFolder: boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TStackItem.OpenFolder;
begin
end;
//------------------------------------------------------------------------------
procedure TStackItem.OnDragEnter;
begin
  if not FFreed then
  try
    FDragOver := true;
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    SetTimer(FHWnd, ID_TIMER_OPEN, 1000, nil);
  except
    on e: Exception do raise Exception.Create('StackItem.OnDragEnter'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.OnDragOver;
begin
end;
//------------------------------------------------------------------------------
procedure TStackItem.OnDragLeave;
begin
  if not FFreed then
  try
    cmd(icSelect, 0);
    FDragOver := false;
    KillTimer(FHWnd, ID_TIMER_OPEN);
    SetTimer(FHWnd, ID_TIMER_CLOSE, 1000, nil);
  except
    on e: Exception do raise Exception.Create('StackItem.OnDragLeave'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean;
var
  index: integer;
begin
  result := not FFreed;
  if result then
  begin
    if hWnd = FHWnd then DropFileI(filename)
    else
    begin
      index := ItemIndex(hWnd);
      if index <> NOT_AN_ITEM then items[index].item.DropFile(pt, filename);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DropFileI(filename: string);
var
  fcaption, fname, fparams, fdir, ficon, ext: string;
begin
  // update icon //
  ext := AnsiLowerCase(ExtractFileExt(filename));
  if (ext = '.png') or (ext = '.ico') then
  begin
    imagefile := toolu.ZipPath(filename);
    color_data := DEFAULT_COLOR_DATA;
    UpdateItemInternal;
    exit;
  end;

  if FSpecialFolder > 0 then
  begin
    // TODO: IFileOperation. No definition in ShlObj for IFileOperation
    exit;
  end;

  // PIDL //
  if copy(filename, 1, 4) = '::::' then
  begin
    AddSubitem(TStackSubitem.Make(0, '::::', filename, '', '', '', 1, DEFAULT_COLOR_DATA, false));
    exit;
  end;

  // any other file //
  fname := filename;
  fdir := '';
  ficon := '';
  ext := AnsiLowerCase(ExtractFileExt(filename));

  if ext = '.lnk' then
  begin
    resolveShortcut(FHWnd, fname, fparams, fdir, ficon);
    fcaption := ChangeFileExt(ExtractFilename(filename), '');
  end
  else
  if ext = '.exe' then
  begin
    fcaption := ChangeFileExt(ExtractFilename(fname), '');
    fdir := ExcludeTrailingPathDelimiter(ExtractFilePath(fname));
  end
  else
  begin
    if DirectoryExists(filename) then fcaption := filename
    else fcaption := ChangeFileExt(ExtractFilename(fname), '');
  end;

  if DirectoryExists(filename) then
  begin
    fcaption := filename;
    if ficon = '' then
      if toolu.IsDriveIdent(filename) then ficon := '%sysdir%\shell32.dll,8' else ficon := '%sysdir%\shell32.dll,3';
  end;

  AddSubitem(TStackSubitem.Make(0, fcaption, fname, fparams, fdir, ficon, 1, DEFAULT_COLOR_DATA, false));
end;
//------------------------------------------------------------------------------
procedure TStackItem.Save(szIni: pchar; szIniGroup: pchar);
var
  i: integer;
begin
  if FFreed or (szIni = nil) or (szIniGroup = nil) then exit;

  WritePrivateProfileString(szIniGroup, nil, nil, szIni);
  WritePrivateProfileString(szIniGroup, 'class', 'stack', szIni);
  if caption <> '' then WritePrivateProfileString(szIniGroup, 'caption', pchar(caption), szIni);
  if imagefile <> '' then WritePrivateProfileString(szIniGroup, 'image', pchar(imagefile), szIni);
  if color_data <> DEFAULT_COLOR_DATA then WritePrivateProfileString(szIniGroup, 'color_data', pchar(toolu.ColorToString(color_data)), szIni);
  if FMode <> 0 then WritePrivateProfileString(szIniGroup, 'mode', pchar(inttostr(FMode)), szIni);
  if FOffset <> 0 then WritePrivateProfileString(szIniGroup, 'offset', pchar(inttostr(FOffset)), szIni);
  WritePrivateProfileString(szIniGroup, 'animation_speed', pchar(inttostr(FAnimationSpeed)), szIni);
  WritePrivateProfileString(szIniGroup, 'distort', pchar(inttostr(FDistort)), szIni);
  WritePrivateProfileString(szIniGroup, 'special_folder', pchar(inttostr(FSpecialFolder)), szIni);
  if not FPreview then WritePrivateProfileString(szIniGroup, 'preview', '0', szIni);
  if (FItemCount > 0) and (FSpecialFolder = 0) then
  begin
    for i := 0 to FItemCount - 1 do
      WritePrivateProfileString(szIniGroup, pchar('subitem' + inttostr(i + 1)), pchar(items[i].item.SaveToString), szIni);
  end;
end;
//------------------------------------------------------------------------------
class function TStackItem.Make(AHWnd: uint; ACaption, AImage: string;
  color_data: integer = DEFAULT_COLOR_DATA; AMode: integer = 0;
  AOffset: integer = 0; AAnimationSpeed: integer = DEFAULT_ANIM_SPEED;
  ADistort: integer = DEFAULT_DISTORT; ASpecialFolder: integer = 0; APreview: boolean = true): string;
begin
  result := 'class="stack";';
  result := result + 'hwnd="' + inttostr(AHWnd) + '";';
  if ACaption <> '' then result := result + 'caption="' + ACaption + '";';
  if AImage <> '' then result := result + 'image="' + AImage + '";';
  if color_data <> DEFAULT_COLOR_DATA then result := result + 'color_data="' + toolu.ColorToString(color_data) + '";';
  if AMode <> 0 then result := result + 'mode="' + inttostr(AMode) + '";';
  if AOffset <> 0 then result := result + 'offset="' + inttostr(AOffset) + '";';
  result := result + 'animation_speed="' + inttostr(AAnimationSpeed) + '";';
  result := result + 'distort="' + inttostr(ADistort) + '";';
  if ASpecialFolder <> 0 then result := result + 'special_folder="' + inttostr(ASpecialFolder) + '";';
  if not APreview then result := result + 'preview="0";';
end;
//------------------------------------------------------------------------------
//
//
//
//   subitems handling
//
//
//
//------------------------------------------------------------------------------
procedure TStackItem.UpdateSpecialFolder;
begin
  if FSpecialFolder > 0 then
  begin
    DeleteSubitems;
    FCaption := '::::';
    AddSpecialFolder(FSpecialFolder);
    if FSpecialFolder = CSIDL_DESKTOPDIRECTORY then AddSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY);
    if FSpecialFolder = CSIDL_PERSONAL then AddSpecialFolder(CSIDL_COMMON_DOCUMENTS);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AddSpecialFolder(csidl: integer);
var
  sfi: TSHFileInfoA;
  psfDesktop: IShellFolder;
  psfFolder: IShellFolder;
  pidFolder: PITEMIDLIST;
  pidChild: PITEMIDLIST;
  pidAbsolute: PItemIdList;
  pEnumList: IEnumIDList;
  celtFetched: ULONG;
begin
  if FFreed then exit;

  try
    OleCheck(SHGetDesktopFolder(psfDesktop));
    OleCheck(SHGetSpecialFolderLocation(0, csidl or CSIDL_FLAG_NO_ALIAS, pidFolder));
    OleCheck(psfDesktop.BindToObject(pidFolder, nil, IID_IShellFolder, psfFolder));
    OleCheck(psfFolder.EnumObjects(0, SHCONTF_NONFOLDERS or SHCONTF_FOLDERS, pEnumList));
    if FCaption = '::::' then
    begin
      SHGetFileInfoA(pchar(pidFolder), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
      FCaption := sfi.szDisplayName;
    end;

    while pEnumList.Next(1, pidChild, celtFetched) = NOERROR do
    begin
      pidAbsolute := PIDL_GetAbsolute(pidFolder, pidChild);
      SHGetFileInfoA(pchar(pidAbsolute), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
      AddSubitem(TStackSubitem.Make(0, sfi.szDisplayName, PIDL_ToString(pidAbsolute), '', '', ''));
      PIDL_Free(pidChild);
    end;

    PIDL_Free(pidFolder);
  except
    on e: Exception do raise Exception.Create('StackItem.AddControlPanel'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdatePreview;
var
  i, border, itemSize: integer;
  g: Pointer;
begin
  if FFreed or FUpdating then exit;

  try
    try if FPreviewImage <> nil then GdipDisposeImage(FPreviewImage);
    except end;
    FPreviewImage := nil;

    if (FItemCount > 0) and FPreview then
    begin
      border := round(FBigItemSize / 8);
      itemSize := round((FBigItemSize - border * 2) / 3);
      FPreviewImageW := FBigItemSize and $fff;
      FPreviewImageH := FBigItemSize and $fff;
      GdipCreateBitmapFromScan0(FPreviewImageW, FPreviewImageH, 0, PixelFormat32bppPARGB, nil, FPreviewImage);
      GdipGetImageGraphicsContext(FPreviewImage, g);
      GdipSetInterpolationMode(g, InterpolationModeHighQualityBicubic);
      GdipSetPixelOffsetMode(g, PixelOffsetModeHighQuality);
      for i := 0 to Math.Min(FItemCount, 9) - 1 do
        items[i].item.DrawPreview(g, border + (itemSize + 1) * (i mod 3), border + (itemSize + 1) * (i div 3), itemSize - 2);
      GdipDeleteGraphics(g);
    end;

    Draw(Fx, Fy, FSize, true, 0, FShowItem);
  except
    on e: Exception do raise Exception.Create('StackItem.AddControlPanel'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.MakeICP: _ItemCreateParams;
begin
  result.ItemSize := FItemSize;
  result.BigItemSize := FItemSize;
  result.LaunchInterval := FLaunchInterval;
  result.ActivateRunning := FActivateRunningDefault;
  result.UseShellContextMenus := FUseShellContextMenus;
  result.Site := FSite;
  result.Reflection := false;
  result.ReflectionSize := 0;
  result.ShowHint := FShowHint;
  result.Animation := 0;
  result.LockDragging := FLockDragging;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AddSubitem(data: string);
var
  upd: boolean;
begin
  if FFreed then exit;

  try
    if FItemCount >= MAX_SUBITEMS then exit;
    inc(FItemCount);
    upd := FUpdating;
    FUpdating := true;
    items[FItemCount - 1].item := TStackSubitem.Create(data, FHWnd, MakeICP);
    if items[FItemCount - 1].item.Freed then DeleteSubitem(FItemCount - 1)
    else items[FItemCount - 1].hWnd := items[FItemCount - 1].item.HWnd;
    FUpdating := upd;

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.AddSubitem'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.CheckDeleteSubitems;
var
  i: integer;
begin
  if FFreed then exit;

  if FItemCount > 0 then
  try
    i := 0;
    while i < FItemCount do
    begin
      if items[i].item.QueryDelete then DeleteSubitem(i) else inc(i);
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.CheckDeleteSubitems'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DeleteSubitem(index: integer);
begin
  if (index >= 0) and (index < FItemCount) then
  try
    items[index].item.Free;
    items[index].item := nil;
    items[index].hWnd := 0;
    while index < FItemCount - 1 do
    begin
      CopyCSIBucket(@items[index + 1], @items[index]);
      inc(index);
    end;
    dec(FItemCount);

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.DeleteSubitem'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DeleteSubitems;
var
  i: integer;
begin
  if FFreed then exit;

  if FItemCount > 0 then
  try
    for i := 0 to FItemCount - 1 do
    begin
      items[i].item.Free;
      items[i].item := nil;
      items[i].hWnd := 0;
    end;
    FItemCount := 0;

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.DeleteSubitems'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AddSubitemDefault;
begin
  AddSubitem(TStackSubitem.Make(0, 'New item', '', '', '', ''));
end;
//------------------------------------------------------------------------------
function TStackItem.GetSubitemCaption(index: integer): string;
begin
  result := '';
  if (index >= 0) and (index < FItemCount) then
  try
    result := items[index].item.Caption;
  except
    on e: Exception do raise Exception.Create('StackItem.GetSubitemCaption'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.SubitemToString(index: integer): string;
begin
  result := '';
  if (index >= 0) and (index < FItemCount) then
  try
    result := items[index].item.ToString;
  except
    on e: Exception do raise Exception.Create('StackItem.SubitemToString'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.SubitemMoveUp(index: integer);
var
  csib: TCSIBucket;
begin
  if (index > 0) and (index < FItemCount) then
  try
    CopyCSIBucket(@items[index - 1], @csib);
    CopyCSIBucket(@items[index], @items[index - 1]);
    CopyCSIBucket(@csib, @items[index]);
  except
    on e: Exception do raise Exception.Create('StackItem.SubitemMoveUp'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.SubitemMoveDown(index: integer);
var
  csib: TCSIBucket;
begin
  if (index >= 0) and (index < FItemCount - 1) then
  try
    CopyCSIBucket(@items[index + 1], @csib);
    CopyCSIBucket(@items[index], @items[index + 1]);
    CopyCSIBucket(@csib, @items[index]);
  except
    on e: Exception do raise Exception.Create('StackItem.SubitemMoveDown'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.SubitemConfigure(index: integer);
begin
  if (index >= 0) and (index < FItemCount) then
  try
    items[index].item.Configure;
  except
    on e: Exception do raise Exception.Create('StackItem.SubitemConfigure'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.CopyCSIBucket(pFrom, pTo: PCSIBucket);
begin
  pTo^.hWnd := pFrom^.hWnd;
  pTo^.item := pFrom^.item;
end;
//------------------------------------------------------------------------------
function TStackItem.ItemIndex(HWnd: HANDLE): integer;
var
  i: integer;
begin
  try
    result := NOT_AN_ITEM;
    if (HWnd = 0) or (FItemCount <= 0) then exit;

    for i := 0 to FItemCount - 1 do
    begin
      if items[i].hWnd = hWnd then
      begin
        result := i;
        break;
      end;
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.ItemIndex'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AllSubitemsCmd(id: TGParam; param: integer);
var
  i: integer;
begin
  if FFreed then exit;

  if FItemCount > 0 then
  try
    for i := 0 to FItemCount - 1 do items[i].item.cmd(id, param);
  except
    on e: Exception do raise Exception.Create('StackItem.CheckDeleteSubitems'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.OpenStack;
begin
  if FFreed then exit;

  try
    FUpdating := true;
    UpdateSpecialFolder;
  finally
    FUpdating := false;
  end;
  UpdatePreview;

  if FItemCount = 0 then exit;

  CheckDeleteSubitems;
  if (FState = stsOpen) or (FState = stsOpening) then exit;

  FState := stsOpening; // further progress is being done by timer //
  LME(true);
end;
//------------------------------------------------------------------------------
procedure TStackItem.CloseStack;
begin
  if FFreed or (FItemCount = 0) then exit;

  cmd(icSelect, 0);
  if FState = stsClosed then CheckDeleteSubitems;
  if (FState = stsClosed) or (FState = stsClosing) then exit;

  FState := stsClosing;  // further progress is being done by timer //
  if FStateProgress <= 0 then LME(false);
end;
//------------------------------------------------------------------------------
procedure TStackItem.DoStateProgress;
var
  step: extended;
  i: integer;
  wpi: uint;
begin
  step := mc.GetStep(FMode, FItemCount);
  if FAnimationSpeed > DEFAULT_ANIM_SPEED then step := step * (1 + (FAnimationSpeed - DEFAULT_ANIM_SPEED) * 0.5);
  if FAnimationSpeed < DEFAULT_ANIM_SPEED then step := step * (1 - (DEFAULT_ANIM_SPEED - FAnimationSpeed) * 0.15);

  if FState = stsOpen then
  begin
      FStateProgress := 1;
      ShowStackState;
  end
  else
  if (FState = stsOpening) and (FStateProgress < 1) then
  begin
      if FStateProgress = 0 then
      begin
        wpi := BeginDeferWindowPos(FItemCount);
        for i := 0 to FItemCount - 1 do DeferWindowPos(wpi, items[i].hWnd, FHWnd, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + swp_showwindow);
        EndDeferWindowPos(wpi);
      end;
      FStateProgress += step;
      if FStateProgress >= 1 then
      begin
        FStateProgress := 1;
        FState := stsOpen;
      end;
      ShowStackState;
  end
  else
  if (FState = stsClosing) and (FStateProgress > 0) then
  begin
      FStateProgress -= step;
      if FStateProgress <= 0 then
      begin
        FStateProgress := 0;
        FState := stsClosed;
        LME(false);
        CheckDeleteSubitems;
        AllSubitemsCmd(icSelect, 0);
      end;
      ShowStackState;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.ShowStackState;
var
  i: integer;
  wr: windows.TRect;
  xyaa: TStackItemData;
begin
  if FItemCount = 0 then
  begin
    FState := stsClosed;
    exit;
  end;

  // calc base point //
  wr := ScreenRect;
  wr.left += FSize div 2;
  wr.top += FSize div 2;

  // get item params //
  for i := 0 to FItemCount - 1 do
  begin
    xyaa := mc.GetItemData(FMode, (FState = stsOpening) or (FState = stsOpen), i, FStateProgress,
      FItemCount, FSite, FItemSize, FOffset + (FSize - FItemSize) div 2, FDistort);
    //items[i].draw := (items[i].alpha > 0) or (xyaa.alpha > 0);
    items[i].draw := true;
    items[i].x := wr.left + xyaa.x;
    items[i].y := wr.top + xyaa.y;
    items[i].s := xyaa.s;
    items[i].alpha := xyaa.alpha;
    items[i].angle := xyaa.angle;
    items[i].hint_align := xyaa.hint_align;
    items[i].hint_alpha := xyaa.hint_alpha;
  end;

  // draw items //
  for i := 0 to FItemCount - 1 do
  begin
    if items[i].draw then items[i].item.Draw(items[i].x, items[i].y, items[i].s, items[i].alpha, items[i].angle, items[i].hint_align, items[i].hint_alpha, false);
  end;

  if FState = stsClosed then
  begin
    for i := 0 to FItemCount - 1 do
      SetWindowPos(items[i].hWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + swp_hidewindow);
  end;
end;
//------------------------------------------------------------------------------
end.
 
