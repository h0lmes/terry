unit stackitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, ComObj, ShlObj,
  Math, GDIPAPI, PIDL, gfx, declu, toolu, customdrawitemu, stacksubitemu,
  stackmodeu, dwm_unit, iniproc;

const
  MAX_SUBITEMS = 64;
  STATE_PROGRESS_MIN = 0.0;
  STATE_PROGRESS_MAX = 1.0;
  DEF_ANIM_SPEED = 8;
  MID_ANIM_SPEED = 4;
  DEF_DISTORT = 1;
  DEF_STACK_PREVIEW = 1;

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
    wnd: HWND;
    item: TCustomSubitem;
  end;

  { TStackItem }

  TStackItem = class(TCustomDrawItem)
  private
    FImageFile: string;
    FSpecialFolder: string;
    FMode: integer;
    FOffset: integer;
    FAnimationSpeed: integer;
    FDistort: integer;
    FPreview: integer; // 0 - none, 1 - four, 2 - nine
    FShowBackground: boolean;
    //
    FUseShellContextMenus: boolean;
    items: array of TCSIBucket; // using a dynamic array. static causes obscure error while deleting stackitem
    FItemCount: integer;
    FState: TStackState;
    FStateProgress: extended;
    FOpenAnimation: boolean;
    FDragOver: boolean;
    FPreviewImage: pointer;
    FPreviewImageW: uint;
    FPreviewImageH: uint;
    FBackgroundWindow: uint;
    FWindowCount: integer;
    procedure BeforeDraw;
    procedure DrawOverlay(dst: pointer; x, y, size: integer);
    procedure Exec;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure OnDragEnter;
    procedure OnDragOver;
    procedure OnDragLeave;
    procedure DropFileI(filename: string);
    // items handling //
    procedure UpdateSpecialFolder;
    procedure AddSpecialFolder(csidl: integer);
    procedure UpdatePreview;
    function MakeICP: TDItemCreateParams;
    procedure CheckDeleteSubitems;
    procedure DeleteSubitems;
    procedure CopyCSIBucket(pFrom, pTo: PCSIBucket);
    function ItemIndex(wnd: HWND): integer;
    procedure AllSubitemsCmd(id: TDParam; param: PtrInt);
    procedure OpenStack;
    procedure CloseStack(immediate: boolean = false);
    procedure DoStateProgress;
    procedure ShowStackState;
  public
    property ItemCount: integer read FItemCount;
    property ImageFile: string read FImageFile write FImageFile;
    property SpecialFolder: string read FSpecialFolder write FSpecialFolder;
    property Mode: integer read FMode write FMode;
    property Offset: integer read FOffset write FOffset;
    property AnimationSpeed: integer read FAnimationSpeed write FAnimationSpeed;
    property Distort: integer read FDistort write FDistort;
    property Preview: integer read FPreview write FPreview;
    property ShowBackground: boolean read FShowBackground write FShowBackground;
    //
    function ToStringFullCopy: string;
    //
    constructor Create(wndParent: HWND; var AParams: TDItemCreateParams); override;
    destructor Destroy; override;
    procedure Init; override;
    procedure FromIni(IniFile, IniSection: string);
    procedure FromString(value: string);
    procedure Update;
    procedure SetFont(var Value: TDFontData); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TDParam; param: PtrInt): PtrInt; override;
    procedure Timer; override;
    procedure Configure; override;
    function DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(ini, section: string); override;
    //
    procedure AddSubitemDefault;
    procedure AddSubitem(data: string);
    function GetSubitemCaption(index: integer): WideString;
    function SubitemToString(index: integer): string;
    procedure DeleteSubitem(index: integer);
    procedure SubitemMoveUp(index: integer);
    procedure SubitemMoveDown(index: integer);
    procedure SubitemConfigure(index: integer);
    //
    class function Make(ACaption: WideString = ''; AImage: string = ''; ASpecialFolder: string = '';
      color_data: integer = DEF_COLOR_DATA; AMode: integer = 0;
      AOffset: integer = 0; AAnimationSpeed: integer = DEF_ANIM_SPEED;
      ADistort: integer = DEF_DISTORT; APreview: integer = DEF_STACK_PREVIEW;
      AShowBackground: boolean = false): string;
  end;

implementation
uses themeu, frmstackpropu;
//------------------------------------------------------------------------------
constructor TStackItem.Create(wndParent: HWND; var AParams: TDItemCreateParams);
begin
  inherited;
  FUseShellContextMenus := AParams.UseShellContextMenus;
  FOpenAnimation := AParams.StackAnimationEnabled;
  OnBeforeDraw := BeforeDraw;
  OnDrawOverlay := DrawOverlay;

  if AParams.IniFile <> '' then FromIni(AParams.IniFile, AParams.IniSection)
  else FromString(Aparams.Parameter);
end;
//------------------------------------------------------------------------------
procedure TStackItem.Init;
begin
  inherited;
  FImageFile := '';
  FItemCount := 0;
  SetLength(items, MAX_SUBITEMS);
  FState := stsClosed;
  FStateProgress := STATE_PROGRESS_MIN;
  FMode := 0;
  FOffset := 0;
  FAnimationSpeed := DEF_ANIM_SPEED;
  FOpenAnimation := true;
  FDistort := DEF_DISTORT;
  FDragOver := false;
  FSpecialFolder := '';
  FPreview := DEF_STACK_PREVIEW;
  FPreviewImage := nil;
  FBackgroundWindow := 0;
  FShowBackground := false;
  FWindowCount := 0;
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
  if IsWindow(FBackgroundWindow) then DestroyWindow(FBackgroundWindow);
  inherited;
end;
//------------------------------------------------------------------------------
procedure TStackItem.FromIni(IniFile, IniSection: string);
var
  idx: integer;
  data: string;
begin
  if not FFreed then
  try
    try
      FUpdating := true;

      if (length(IniFile) > 0) and (length(IniSection) > 0) then
      begin
        Caption          := ReadIniStringW(IniFile, IniSection, 'caption', '');
        FImageFile       := ReadIniStringW(IniFile, IniSection, 'image', '');
        FColorData       := toolu.StringToColor(ReadIniStringW(IniFile, IniSection, 'color_data', toolu.ColorToString(DEF_COLOR_DATA)));
        FMode            := ReadIniIntW(IniFile, IniSection, 'mode', 0, 0, 1000);
        FOffset          := ReadIniIntW(IniFile, IniSection, 'offset', 0, -100, 100);
        FAnimationSpeed  := ReadIniIntW(IniFile, IniSection, 'animation_speed', DEF_ANIM_SPEED, 1, 10);
        FDistort         := ReadIniIntW(IniFile, IniSection, 'distort', DEF_DISTORT, -10, 10);
        FPreview         := ReadIniIntW(IniFile, IniSection, 'preview', DEF_STACK_PREVIEW, 0, 2);
        FSpecialFolder   := ReadIniStringW(IniFile, IniSection, 'special_folder', '');
        FShowBackground  := ReadIniBoolW(IniFile, IniSection, 'background', false);

        idx := 1;
        repeat
          data := ReadIniStringW(IniFile, IniSection, WideString('subitem' + inttostr(idx)), '');
          if data <> '' then AddSubitem(data);
          inc(idx);
        until (data = '') or (idx > MAX_SUBITEMS);

        FUpdating:= false;
        Update;
      end;

    finally
      FUpdating:= false;
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.FromIni ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.FromString(value: string);
var
  idx: integer;
  list: TStrings;
begin
  if not FFreed then
  try
    try
      FUpdating := true;

      list := TStringList.Create;
      list.AddText(value);
      if list.count > 1 then value := list.strings[0];

      Caption              := FetchValue(value, 'caption="', '";');
      FImageFile           := FetchValue(value, 'image="', '";');
      FColorData           := DEF_COLOR_DATA;
      FMode                := 0;
      FOffset              := 0;
      FAnimationSpeed      := DEF_ANIM_SPEED;
      FDistort             := DEF_DISTORT;
      FSpecialFolder       := '';
      FShowBackground      := false;
      try FColorData       := strtoint(FetchValue(value, 'color_data="', '";'));
      except end;
      try FMode            := strtoint(FetchValue(value, 'mode="', '";'));
      except end;
      try FOffset          := strtoint(FetchValue(value, 'offset="', '";'));
      except end;
      try FAnimationSpeed  := strtoint(FetchValue(value, 'animation_speed="', '";'));
      except end;
      try FDistort         := strtoint(FetchValue(value, 'distort="', '";'));
      except end;
      FPreview             := DEF_STACK_PREVIEW;
      try FPreview         := strtoint(FetchValue(value, 'preview="', '";'));
      except end;
      FSpecialFolder       := FetchValue(value, 'special_folder="', '";');
      try FShowBackground  := boolean(strtoint(FetchValue(value, 'background="', '";')));
      except end;

      if list.count > 1 then
      begin
        idx := 1;
        while idx < list.Count do
        begin
          if list.strings[idx] <> '' then AddSubitem(list.strings[idx]);
          inc(idx);
        end;
      end;
      list.free;

    finally
      FUpdating:= false;
    end;

    Update;
  except
    on e: Exception do raise Exception.Create('StackItem.FromString ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.Update;
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;
      // load images from files //
      LoadImage(UnzipPath(FImageFile), FBigItemSize, false, false, FImage, FIW, FIH);
      // default stack image //
      if FImage = nil then
      begin
        FImage := theme.Stack.Image;
        DownscaleImage(Fimage, FBigItemSize, false, FIW, FIH, false);
      end;
    finally
      FUpdating:= false;
    end;

    UpdateSpecialFolder;
    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.UpdateItemInternal ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.cmd(id: TDParam; param: PtrInt): PtrInt;
var
  b: boolean;
  temp: uint;
  idx, windowCount: integer;
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
          if temp <> FIW then Update;
        end;
      gpShowRunningIndicator:
        begin
          if FRunning and not boolean(param) then
          begin
            FRunning := false;
            Redraw;
          end;
        end;
      gpUseShellContextMenus: FUseShellContextMenus := boolean(param);
      gpStackAnimationEnabled: FOpenAnimation := param <> 0;
      gpSite:
        begin
          if FRunning then Redraw;
          CloseStack;
        end;
      tcThemeChanged:
        begin
          Update; // in order to update default stack image //
          if FRunning then Redraw;
        end;

      // commands //

      icUpdateRunning:
        begin
          windowCount := 0;
          idx := 0;
          while idx < FItemCount do
          begin
            items[idx].item.cmd(icUpdateRunning, 0);
            inc(windowCount, items[idx].item.WindowCount);
            inc(idx);
          end;
          if windowCount <> FWindowCount then
          begin
            FWindowCount := windowCount;
            FRunning := windowCount > 0;
            Redraw;
          end;
        end;
      icDragEnter:
        begin
          OnDragEnter;
          result := FDropIndicator;
        end;
      icDragOver: OnDragOver;
      icDragLeave: OnDragLeave;
      icVisible: if not boolean(param) then CloseStack(true);
      icIsItem:
        begin
          result := 0;
          if (FItemCount > 0) and (FState = stsOpen) then
          begin
            for idx := 0 to FItemCount - 1 do
              if items[idx].wnd = THandle(param) then result := FHWnd;
          end;
        end;
    end;

    b := (id = gpItemSize) or (id = gpUseShellContextMenus) or (id = gpSite) or (id = gpShowHint) or
      (id = gpLaunchInterval) or (id = gpActivateRunning) or (id = gpLockDragging) or (id = tcThemeChanged);
    if (FItemCount > 0) and b then
    begin
      for idx := 0 to FItemCount - 1 do items[idx].item.cmd(id, param);
    end;

  except
    on e: Exception do raise Exception.Create('StackItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.SetFont(var Value: TDFontData);
var
  idx: integer;
begin
  inherited;

  if FItemCount > 0 then
  begin
    for idx := 0 to FItemCount - 1 do items[idx].item.SetFont(FFont);
  end;
end;
//------------------------------------------------------------------------------
// Draw routines ---------------------------------------------------------------
procedure TStackItem.BeforeDraw;
begin
  if FState = stsOpen then DoStateProgress;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DrawOverlay(dst: pointer; x, y, size: integer);
begin
  if assigned(FPreviewImage) and (FState = stsClosed) then
    GdipDrawImageRectRectI(dst, FPreviewImage, x, y, size, size, 0, 0, FPreviewImageW, FPreviewImageH, UnitPixel, nil, nil, nil);
  if FWindowCount > 0 then DrawNumberOverlay(dst, x, y, size, FWindowCount);
  DrawItemIndicator(dst, FDropIndicator, x, y, size, size);
end;
// Draw routines ---------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TStackItem.Timer;
begin
  inherited;
  if not FFreed and not FUpdating then
    if (FState = stsOpening) or (FState = stsClosing) then DoStateProgress;
end;
//------------------------------------------------------------------------------
procedure TStackItem.Configure;
begin
  TfrmStackProp.Open(Handle);
end;
//------------------------------------------------------------------------------
function TStackItem.ToString: string;
begin
  result := Make(FCaption, FImageFile, FSpecialFolder,
    FColorData, FMode, FOffset, FAnimationSpeed, FDistort, FPreview, FShowBackground);
end;
//------------------------------------------------------------------------------
function TStackItem.ToStringFullCopy: string;
var
  idx: integer;
begin
  result := ToString;
  if (FItemCount > 0) and (FSpecialFolder = '') then
  begin
    for idx := 0 to FItemCount - 1 do
      result := result + #10 + items[idx].item.ToString;
  end;
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
  if button = mbRight then Configure;
end;
//------------------------------------------------------------------------------
function TStackItem.ContextMenu(pt: Windows.TPoint): boolean;
  function IsValidShortcutString(str: string): boolean;
  begin
    result := FetchValue(str, 'class="', '";') = 'shortcut';
  end;

var
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenuW(FHMenu, MF_STRING, $f001, pwchar(UTF8Decode(XConfigureIcon)));
  AppendMenuW(FHMenu, MF_STRING, $f003, pwchar(UTF8Decode(XCopy)));
  if IsValidShortcutString(GetClipboard) then AppendMenuW(FHMenu, MF_STRING, $f005, pwchar(UTF8Decode(XPaste)));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING, $f004, pwchar(UTF8Decode(XDeleteIcon)));

  LME(true);
  msg.WParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TStackItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
begin
  result := 0;
  LME(false);
  DestroyMenu(FHMenu);
  FHMenu := 0;
  case wParam of // f001 to f020
    $f001: Configure;
    $f002: ; // open folder
    $f003: toolu.SetClipboard(ToStringFullCopy);
    $f004: Delete;
    $f005: AddSubitem(GetClipboard);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.WndMessage(var msg: TMessage);
var
  idx: integer;
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
          for idx := 0 to FItemCount - 1 do
            if items[idx].wnd = THandle(lParam) then found := true;
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
  if (FState = stsClosed) or (FState = stsClosing) then OpenStack else CloseStack;
end;
//------------------------------------------------------------------------------
procedure TStackItem.OnDragEnter;
begin
  if not FFreed then
  try
    FDropIndicator := DII_ADD;
    FDragOver := true;
    KillTimer(FHWnd, ID_TIMER_CLOSE);
    SetTimer(FHWnd, ID_TIMER_OPEN, 1000, nil);
    Redraw;
  except
    on e: Exception do raise Exception.Create('StackItem.OnDragEnter ' + LineEnding + e.message);
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
    FDropIndicator := 0;
    FDragOver := false;
    KillTimer(FHWnd, ID_TIMER_OPEN);
    SetTimer(FHWnd, ID_TIMER_CLOSE, 1000, nil);
    Redraw;
  except
    on e: Exception do raise Exception.Create('StackItem.OnDragLeave ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.DropFile(wnd: HWND; pt: windows.TPoint; filename: string): boolean;
var
  index: integer;
begin
  result := not FFreed;
  if result then
  begin
    if wnd = FHWnd then DropFileI(filename)
    else
    begin
      index := ItemIndex(wnd);
      if index <> NOT_AN_ITEM then items[index].item.DropFile(pt, filename);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DropFileI(filename: string);
var
  ext: string;
begin
  // update icon //
  ext := LowerCase(ExtractFileExt(filename));
  if (ext = '.png') or (ext = '.ico') then
  begin
    FImageFile := toolu.ZipPath(filename);
    FColorData := DEF_COLOR_DATA;
    Update;
    exit;
  end;

  if FSpecialFolder <> '' then
  begin
    // TODO: IFileOperation. No definition in ShlObj for IFileOperation
    exit;
  end;

  AddSubitem(TShortcutSubitem.MakeFromFilename(filename));
end;
//------------------------------------------------------------------------------
procedure TStackItem.Save(ini, section: string);
var
  idx: integer;
begin
  if (ini = '') or (section = '') then exit;
  WritePrivateProfileString(pchar(section), nil, nil, pchar(ini));
  WritePrivateProfileString(pchar(section), 'class', 'stack', pchar(ini));
  if Caption <> '' then                         WriteIniStringW(ini, section, 'caption', Caption);
  if FImageFile <> '' then                      WriteIniStringW(ini, section, 'image', FImageFile);
  if FColorData <> DEF_COLOR_DATA then          WriteIniStringW(ini, section, 'color_data', toolu.ColorToString(FColorData));
  if FMode <> 0 then                            WriteIniStringW(ini, section, 'mode', inttostr(FMode));
  if FOffset <> 0 then                          WriteIniStringW(ini, section, 'offset', inttostr(FOffset));
  if FAnimationSpeed <> DEF_ANIM_SPEED then     WriteIniStringW(ini, section, 'animation_speed', inttostr(FAnimationSpeed));
  if FDistort <> DEF_DISTORT then               WriteIniStringW(ini, section, 'distort', inttostr(FDistort));
  if FSpecialFolder <> '' then                  WriteIniStringW(ini, section, 'special_folder', FSpecialFolder);
  if FPreview <> DEF_STACK_PREVIEW then         WriteIniStringW(ini, section, 'preview', inttostr(FPreview));
  if FShowBackground then                       WriteIniStringW(ini, section, 'background', '1');
  if (FItemCount > 0) and (FSpecialFolder = '') then
  begin
    for idx := 0 to FItemCount - 1 do   WriteIniStringW(ini, section, 'subitem' + inttostr(idx + 1), items[idx].item.ToString);
  end;
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
var
  csidl: integer;
begin
  if FSpecialFolder <> '' then
  begin
      csidl := CSIDL_ToInt(FSpecialFolder);
      if csidl > -1 then
      begin
        DeleteSubitems;
        FCaption := '::::';
        AddSpecialFolder(csidl);
        if csidl = CSIDL_DESKTOPDIRECTORY then AddSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY);
        if csidl = CSIDL_PERSONAL then AddSpecialFolder(CSIDL_COMMON_DOCUMENTS);
        UpdatePreview;
      end else begin
        FSpecialFolder := '';
      end;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AddSpecialFolder(csidl: integer);
var
  sfi: TSHFileInfoW;
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
      FillChar(sfi, sizeof(sfi), 0);
      SHGetFileInfoW(pwchar(pidFolder), 0, sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
      FCaption := strpas(pwchar(sfi.szDisplayName));
    end;

    while pEnumList.Next(1, pidChild, celtFetched) = NOERROR do
    begin
      pidAbsolute := PIDL_GetAbsolute(pidFolder, pidChild);
      AddSubitem(TShortcutSubitem.MakeFromFilename(PIDL_GetDisplayName2(pidAbsolute)));
      PIDL_Free(pidChild);
    end;

    PIDL_Free(pidFolder);
  except
    on e: Exception do raise Exception.Create('StackItem.AddSpecialFolder ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.UpdatePreview;
var
  i, border, itemSize, viewItemCount: integer;
  g: Pointer;
begin
  if FFreed or FUpdating then exit;

  try
    try if FPreviewImage <> nil then GdipDisposeImage(FPreviewImage);
    except end;
    FPreviewImage := nil;

    if (FPreview > 0) and (FItemCount > 0) then
    begin
      viewItemCount := Math.Min(FItemCount, 4);
      if FPreview = 2 then viewItemCount := Math.Min(FItemCount, 9);

	    border := round(FBigItemSize / 8);
	    itemSize := (FBigItemSize - border * 2);
	    if viewItemCount > 4 then itemSize := round(itemSize / 3)
	    else
	    if viewItemCount > 1 then itemSize := round(itemSize / 2);

      FPreviewImageW := FBigItemSize and $fff;
      FPreviewImageH := FBigItemSize and $fff;
      GdipCreateBitmapFromScan0(FPreviewImageW, FPreviewImageH, 0, PixelFormat32bppPARGB, nil, FPreviewImage);
      GdipGetImageGraphicsContext(FPreviewImage, g);
      GdipSetInterpolationMode(g, InterpolationModeHighQualityBicubic);
      GdipSetPixelOffsetMode(g, PixelOffsetModeHighQuality);
      if viewItemCount = 1 then
         items[0].item.DrawPreview(g, border, border, itemSize - 2)
      else
      if (viewItemCount >= 2) and (viewItemCount <= 4) then
      begin
        for i := 0 to viewItemCount - 1 do
          items[i].item.DrawPreview(g, border + (itemSize + 1) * (i mod 2), border + (itemSize + 1) * (i div 2), itemSize - 2);
      end
      else
      if viewItemCount >= 5 then
      begin
        for i := 0 to viewItemCount - 1 do
          items[i].item.DrawPreview(g, border + (itemSize + 1) * (i mod 3), border + (itemSize + 1) * (i div 3), itemSize - 2);
      end;
      GdipDeleteGraphics(g);
    end;

    Redraw;
  except
    on e: Exception do raise Exception.Create('StackItem.UpdatePreview ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TStackItem.MakeICP: TDItemCreateParams;
begin
  result.ItemSize := FItemSize;
  result.BigItemSize := FItemSize;
  result.LaunchInterval := FLaunchInterval;
  result.ActivateRunning := FActivateRunning;
  result.UseShellContextMenus := FUseShellContextMenus;
  result.Site := FSite;
  result.Reflection := false;
  result.ReflectionSize := 0;
  result.ShowHint := FShowHint;
  result.AnimationType := 0;
  result.LockDragging := FLockDragging;
  CopyFontData(FFont, result.Font);
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
    try
      FUpdating := true;
      items[FItemCount - 1].item := TShortcutSubitem.Create(FHWnd, MakeICP);
      if items[FItemCount - 1].item.Freed then
      begin
        DeleteSubitem(FItemCount - 1);
        dec(FItemCount);
      end
      else begin
        items[FItemCount - 1].wnd := items[FItemCount - 1].item.Handle;
        items[FItemCount - 1].item.FromString(data);
      end;
    finally
      FUpdating := upd;
    end;

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.AddSubitem ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.CheckDeleteSubitems;
var
  idx: integer;
begin
  if FFreed then exit;

  if FItemCount > 0 then
  try
    idx := 0;
    while idx < FItemCount do
    begin
      if items[idx].item.QueryDelete then DeleteSubitem(idx) else inc(idx);
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.CheckDeleteSubitems ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DeleteSubitem(index: integer);
begin
  if (index >= 0) and (index < FItemCount) then
  try
    items[index].item.Free;
    items[index].item := nil;
    items[index].wnd := 0;
    while index < FItemCount - 1 do
    begin
      CopyCSIBucket(@items[index + 1], @items[index]);
      inc(index);
    end;
    dec(FItemCount);

    UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.DeleteSubitem ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DeleteSubitems;
var
  idx: integer;
begin
  if FItemCount > 0 then
  try
    for idx := 0 to FItemCount - 1 do
    begin
      items[idx].item.Free;
      items[idx].item := nil;
      items[idx].wnd := 0;
    end;
    FItemCount := 0;

    if not FFreed then UpdatePreview;
  except
    on e: Exception do raise Exception.Create('StackItem.DeleteSubitems ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AddSubitemDefault;
begin
  AddSubitem(TShortcutSubitem.Make('New item'));
end;
//------------------------------------------------------------------------------
function TStackItem.GetSubitemCaption(index: integer): WideString;
begin
  result := '';
  if (index >= 0) and (index < FItemCount) then
  try
    result := items[index].item.Caption;
  except
    on e: Exception do raise Exception.Create('StackItem.GetSubitemCaption ' + LineEnding + e.message);
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
    on e: Exception do raise Exception.Create('StackItem.SubitemToString ' + LineEnding + e.message);
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
    on e: Exception do raise Exception.Create('StackItem.SubitemMoveUp ' + LineEnding + e.message);
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
    on e: Exception do raise Exception.Create('StackItem.SubitemMoveDown ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.SubitemConfigure(index: integer);
begin
  if (index >= 0) and (index < FItemCount) then
  try
    items[index].item.Configure;
  except
    on e: Exception do raise Exception.Create('StackItem.SubitemConfigure ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.CopyCSIBucket(pFrom, pTo: PCSIBucket);
begin
  pTo^.wnd := pFrom^.wnd;
  pTo^.item := pFrom^.item;
end;
//------------------------------------------------------------------------------
function TStackItem.ItemIndex(wnd: HWND): integer;
var
  idx: integer;
begin
  try
    result := NOT_AN_ITEM;
    if (wnd = 0) or (FItemCount <= 0) then exit;

    for idx := 0 to FItemCount - 1 do
    begin
      if items[idx].wnd = wnd then
      begin
        result := idx;
        break;
      end;
    end;
  except
    on e: Exception do raise Exception.Create('StackItem.ItemIndex ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.AllSubitemsCmd(id: TDParam; param: PtrInt);
var
  idx: integer;
begin
  if FFreed then exit;

  if FItemCount > 0 then
  try
    for idx := 0 to FItemCount - 1 do items[idx].item.cmd(id, param);
  except
    on e: Exception do raise Exception.Create('StackItem.CheckDeleteSubitems ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.OpenStack;
begin
  if not FFreed and (FItemCount > 0) and ((FState = stsClosed) or (FState = stsClosing)) then
  begin
    {try
      FUpdating := true;
      UpdateSpecialFolder;
    finally
      FUpdating := false;
    end;}
    FStateProgress := 0;
    FState := stsOpening; // further progress is being done by timer //
    FHideHint := true;
    Redraw;
    LME(true);
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.CloseStack(immediate: boolean = false);
begin
  if not FFreed and (FItemCount > 0) and ((FState = stsOpen) or (FState = stsOpening)) then
  begin
    cmd(icSelect, 0);
    FStateProgress := 1;
    FState := stsClosing;  // further progress is being done by timer //
    if immediate then
    begin
      FStateProgress := 0;
      DoStateProgress;
		end;
	end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.DoStateProgress;
var
  step: extended;
  idx: integer;
  wpi: uint;
  showItems: boolean;
begin
  step := mc.GetStep(FMode, FItemCount);
  if FAnimationSpeed > MID_ANIM_SPEED then step := step * (1 + (FAnimationSpeed - MID_ANIM_SPEED) * 0.5);
  if FAnimationSpeed < MID_ANIM_SPEED then step := step * (1 - (MID_ANIM_SPEED - FAnimationSpeed) * 0.15);

  if FState = stsOpen then
  begin
      FStateProgress := 1;
      ShowStackState;
  end
  else
  if (FState = stsOpening) and (FStateProgress < 1) then
  begin
      showItems := FStateProgress = 0;

      FStateProgress += step;
      if not FOpenAnimation then FStateProgress := 1;
      if FStateProgress >= 1 then
      begin
        FStateProgress := 1;
        FState := stsOpen;
      end;
      ShowStackState;

      if showItems then
      begin
        wpi := BeginDeferWindowPos(FItemCount);
        for idx := 0 to FItemCount - 1 do DeferWindowPos(wpi, items[idx].wnd, FHWnd, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + swp_showwindow);
        EndDeferWindowPos(wpi);
      end;
  end
  else
  if FState = stsClosing then
  begin
      FStateProgress -= step;
      if not FOpenAnimation then FStateProgress := 0;

      if FStateProgress <= 0 then
      begin
        FStateProgress := 0;
        FState := stsClosed;
        LME(false);
        FHideHint := false;
        CheckDeleteSubitems;
        AllSubitemsCmd(icSelect, 0);
        Redraw;
      end;

      if FState = stsClosed then
      begin
        if FShowBackground then ShowWindow(FBackgroundWindow, SW_HIDE);
        wpi := BeginDeferWindowPos(FItemCount);
        for idx := 0 to FItemCount - 1 do DeferWindowPos(wpi, items[idx].wnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + swp_hidewindow);
        EndDeferWindowPos(wpi);
      end else begin
        ShowStackState;
      end;
  end;
end;
//------------------------------------------------------------------------------
procedure TStackItem.ShowStackState;
var
  idx: integer;
  wr: windows.TRect;
  xyaa: TStackItemData;
begin
  if FItemCount = 0 then
  begin
    FState := stsClosed;
    exit;
  end;

  // calc base point (center of icon) //
  wr := ScreenRect;
  wr.left += FSize div 2;
  wr.top += FSize div 2;

  // get item params //
  for idx := 0 to FItemCount - 1 do
  begin
    xyaa := mc.GetItemData(FMode, (FState = stsOpening) or (FState = stsOpen), FShowHint, FStateProgress,
      idx, FItemCount, FSite, FItemSize, FOffset + (FSize - FItemSize) div 2, FDistort);
    items[idx].draw := true;
    items[idx].x := wr.left + xyaa.x;
    items[idx].y := wr.top + xyaa.y;
    items[idx].s := xyaa.s;
    items[idx].alpha := xyaa.alpha;
    items[idx].angle := xyaa.angle;
    items[idx].hint_align := xyaa.hint_align;
    items[idx].hint_alpha := xyaa.hint_alpha;
  end;

  // draw items //
  for idx := 0 to FItemCount - 1 do
  begin
    if items[idx].draw then items[idx].item.Draw(items[idx].x, items[idx].y, items[idx].s,
      items[idx].alpha, items[idx].angle, items[idx].hint_align, items[idx].hint_alpha, FShowBackground, false);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//
//
//
//
//------------------------------------------------------------------------------
class function TStackItem.Make(ACaption: WideString = ''; AImage: string = ''; ASpecialFolder: string = '';
      color_data: integer = DEF_COLOR_DATA; AMode: integer = 0;
      AOffset: integer = 0; AAnimationSpeed: integer = DEF_ANIM_SPEED;
      ADistort: integer = DEF_DISTORT; APreview: integer = DEF_STACK_PREVIEW;
      AShowBackground: boolean = false): string;
begin
  result := 'class="stack";';
  if ACaption <> '' then result := result + 'caption="' + ACaption + '";';
  if AImage <> '' then result := result + 'image="' + AImage + '";';
  if ASpecialFolder <> '' then result := result + 'special_folder="' + ASpecialFolder + '";';
  if color_data <> DEF_COLOR_DATA then result := result + 'color_data="' + toolu.ColorToString(color_data) + '";';
  if AMode <> 0 then result := result + 'mode="' + inttostr(AMode) + '";';
  if AOffset <> 0 then result := result + 'offset="' + inttostr(AOffset) + '";';
  result := result + 'animation_speed="' + inttostr(AAnimationSpeed) + '";';
  result := result + 'distort="' + inttostr(ADistort) + '";';
  if APreview <> DEF_STACK_PREVIEW then result := result + 'preview="' + inttostr(APreview) + '";';
  if AShowBackground then result := result + 'background="0";';
end;
//------------------------------------------------------------------------------
end.
 
