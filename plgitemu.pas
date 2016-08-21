unit plgitemu;

interface
uses Windows, Messages, SysUtils, Controls, Classes, Dialogs, GDIPAPI,
  gfx, math, dynlibs, declu, DockH, customitemu, customdrawitemu, toolu, iniproc;

type

  { TPluginItem }

  TPluginItem = class(TCustomDrawItem)
  private
    lpData: Pointer;
    MouseDownPoint: windows.TPoint;
    AutoDeleteImage: boolean;
    AutoDeleteOverlay: boolean;
    FImage2: Pointer;
    FIW2: cardinal;
    FIH2: cardinal;
    hwnd2: HWND; // to speed up gdiplus drawing //
    FPluginFile: string;
    FIniFile: string;
    FIniSection: string;
    // plugin lib vars
    hLib: THandle;
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
    procedure LoadPlugin;
    procedure DrawOverlay(dst: pointer; x, y, size: integer);
  public
    property Filename: string read FPluginFile;
    constructor Create(wndParent: HWND; var AParams: TDItemCreateParams); override;
    destructor Destroy; override;
    procedure FromIni(IniFile, IniSection: string);
    procedure FromString(value: string);
    procedure UpdateImage(AImage: Pointer; AutoDelete: boolean);
    procedure UpdateOverlay(AOverlay: Pointer; AutoDelete: boolean);
    procedure CallCreate;
    function ToString: string; override;
    function DblClick(button: TMouseButton; shift: TShiftState; x, y: integer): boolean; override;
    procedure MouseDown(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    procedure Timer; override;
    procedure Configure; override;
    procedure Save(ini, section: string); override;
    //
    class function Make(AFile: string): string;
  end;

implementation
//------------------------------------------------------------------------------
constructor TPluginItem.Create(wndParent: HWND; var AParams: TDItemCreateParams);
begin
  FFreed := true;
  inherited;
  FNeedMouseWheel := true;
  OnDrawOverlay := DrawOverlay;

  AutoDeleteImage := true;
  FImage := nil;
  AutoDeleteOverlay := true;
  FImage2 := nil;

  lpData := nil;

  // window to speed up drawing //
  hwnd2 := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow + ws_ex_noactivate, TDITEM_WCLASS, nil, ws_popup, -100, -100, 32, 32, 0, 0, hInstance, nil);

  if AParams.IniFile <> '' then FromIni(AParams.IniFile, AParams.IniSection)
  else FromString(Aparams.Parameter);
end;
//------------------------------------------------------------------------------
procedure TPluginItem.FromIni(IniFile, IniSection: string);
begin
  FIniFile := IniFile;
  FIniSection := IniSection;
  if (length(IniFile) > 0) and (length(IniSection) > 0) then
  begin
    FPluginFile := toolu.UnzipPath(GetIniStringW(FIniFile, FIniSection, 'file', ''));
    LoadPlugin;
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.FromString(value: string);
begin
  FIniFile := '';
  FIniSection := '';
  FPluginFile := toolu.UnzipPath(FetchValue(value, 'file="', '";'));
  LoadPlugin;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.LoadPlugin;
begin
  try
    FFreed := true;
    SetCurrentDir(ExtractFilePath(FPluginFile));
    hLib := LoadLibrary(FPluginFile);
    if hLib = 0 then
    begin
      MessageBox(FHWnd, pchar('LoadLibrary(' + FPluginFile + ') failed'), 'CreatePlugin', 0);
      exit;
    end;
    @OnCreate           := GetProcAddress(hLib, 'OnCreate');
    @OnSave             := GetProcAddress(hLib, 'OnSave');
    @OnDestroy          := GetProcAddress(hLib, 'OnDestroy');
    @OnLeftButtonClick  := GetProcAddress(hLib, 'OnLeftButtonClick');
    @OnDoubleClick      := GetProcAddress(hLib, 'OnDoubleClick');
    @OnRightButtonClick := GetProcAddress(hLib, 'OnRightButtonClick');
    @OnConfigure        := GetProcAddress(hLib, 'OnConfigure');
    @OnWndMessage       := GetProcAddress(hLib, 'OnProcessMessage');
    if not assigned(OnCreate) then
    begin
      MessageBox(FHWnd, pchar('OnCreate(' + FPluginFile + ') is NULL'), 'CreatePlugin', 0);
      exit;
    end;
    FFreed := false;
  except
    on e: Exception do raise Exception.Create('PluginItem.LoadPlugin ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.CallCreate;
var
  szIni, szIniGroup: array [0..MAX_PATH - 1] of char;
begin
  if assigned(lpData) then exit;

  if (FIniFile <> '') and (FIniSection <> '') then
  begin
    try
      strlcopy(szIni, pchar(FIniFile), length(FIniFile));
      strlcopy(szIniGroup, pchar(FIniSection), length(FIniSection));
      lpData := OnCreate(FHWnd, hLib, @szIni, @szIniGroup);
    except
      on e: Exception do raise Exception.Create('PluginItem.CallCreate ' + LineEnding + e.message);
    end;
  end
  else
  begin
    try
      lpData := OnCreate(FHWnd, hLib, nil, nil);
    except
      on e: Exception do raise Exception.Create('PluginItem.CallCreate_NoIni ' + LineEnding + e.message);
    end;
  end;
end;
//------------------------------------------------------------------------------
destructor TPluginItem.Destroy;
begin
  try
    if assigned(OnDestroy) then OnDestroy(lpData, FHWnd);
  except
    on e: Exception do raise Exception.Create('PluginItem.OnDestroy ' + LineEnding + e.message);
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
procedure TPluginItem.UpdateImage(AImage: Pointer; AutoDelete: boolean);
begin
  if not FFreed then
  begin
    try
      if AutoDeleteImage then
        if assigned(FImage) then GdipDisposeImage(FImage);
    except end;
    FImage := AImage;
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
    AutoDeleteOverlay := DownscaleImage(FImage2, FBigItemSize, false, FIW2, FIH2, AutoDelete) or AutoDelete;
    if not FFloating then Redraw;
  end;
end;
//------------------------------------------------------------------------------
// Draw routines ---------------------------------------------------------------
procedure TPluginItem.DrawOverlay(dst: pointer; x, y, size: integer);
begin
  if assigned(FImage2) then GdipDrawImageRectRectI(dst, FImage2, x, y, size, size, 0, 0, FIW2, FIH2, UnitPixel, nil, nil, nil);
end;
// Draw routines ---------------------------------------------------------------
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
    on e: Exception do raise Exception.Create('PluginItem.Timer ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Configure;
begin
  if assigned(OnConfigure) then OnConfigure(lpData);
end;
//------------------------------------------------------------------------------
function TPluginItem.ToString: string;
begin
  result := '';
  if not FFreed then
  begin
    result := result + '';
    result := 'class="plugin";hwnd="' + inttostr(FHWnd) + '";file="' + toolu.ZipPath(FPluginFile) + '";caption="' + FCaption + '";';
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

  if button = mbLeft then
    if assigned(OnLeftButtonClick) then OnLeftButtonClick(lpData, @pt, @sz);

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
  AppendMenuW(FHMenu, MF_STRING, $f001, pwchar(UTF8Decode(XConfigureIcon)));
  if CanOpenFolder then AppendMenuW(FHMenu, MF_STRING, $f002, pwchar(UTF8Decode(XOpenFolderOf) + ' "' + Caption + '"'));
  AppendMenuW(FHMenu, MF_STRING, $f003, pwchar(UTF8Decode(XCopy)));
  AppendMenuW(FHMenu, MF_SEPARATOR, 0, '-');
  AppendMenuW(FHMenu, MF_STRING, $f004, pwchar(UTF8Decode(XDeleteIcon)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  // else, if it is disabled //
  msg.WParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
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
  case wParam of
    $f001: Configure;
    $f002: OpenFolder;
    $f003: toolu.SetClipboard(ToString);
    $f004: Delete;
  end;
end;
//------------------------------------------------------------------------------
procedure TPluginItem.WndMessage(var msg: TMessage);
begin
  msg.Result := 0;
  if not FFreed and assigned(OnWndMessage) then
    with msg do
      OnWndMessage(lpData, FHWnd, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------
procedure TPluginItem.Save(ini, section: string);
begin
  try
    if FFreed or (ini = '') or (section = '') then exit;
    WritePrivateProfileString(pchar(section), nil, nil, pchar(ini));
    WritePrivateProfileString(pchar(section), 'class', 'plugin', pchar(ini));
    WriteIniStringW(ini, section, 'file', toolu.ZipPath(FPluginFile));
    if assigned(OnSave) then OnSave(lpData, pchar(ini), pchar(section), false);
  except
    on E: Exception do raise Exception.Create('PluginItem.Save ' + LineEnding + 'Plugin DLL: ' + FPluginFile + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//
//
//------------------------------------------------------------------------------
class function TPluginItem.Make(AFile: string): string;
begin
  result := 'class="plugin";';
  if AFile <> '' then result := result + 'file="' + AFile + '";';
end;
//------------------------------------------------------------------------------
end.
