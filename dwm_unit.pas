unit dwm_unit;

interface
uses windows;

const
  WM_DWMCOMPOSITIONCHANGED = $031E;
  DWMWA_EXCLUDED_FROM_PEEK = 12;

type
  _DWM_BLURBEHIND = record
    dwFlags: dword;
    fEnable: bool;
    hRgnBlur: HRGN;
    fTransitionOnMaximized: bool;
  end;
  P_DWM_BLURBEHIND = ^_DWM_BLURBEHIND;

  { TDWMHelper }

  TDWMHelper = class
    private
      IsVista: boolean;
      hDwmLib: uint;
      DwmIsCompositionEnabled: function(pfEnabled: PBoolean): HRESULT; stdcall;
      DwmEnableBlurBehindWindow: function(Wnd: HWND; bb: P_DWM_BLURBEHIND): HRESULT; stdcall;
      DwmSetWindowAttribute: function(Wnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cb: DWORD): HRESULT; stdcall;
      DwmExtendFrameIntoClientArea: function(Wnd: HWND; var margins: windows.TRect): HRESULT; stdcall;
    public
      constructor Create;
      destructor Destroy; override;
      function CompositingEnabled: boolean;
      procedure EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
      procedure DisableBlurBehindWindow(const AHandle: THandle);
      procedure ExcludeFromPeek(const AHandle: THandle);
      procedure ExtendFrameIntoClientArea(const AHandle: THandle; margins: windows.TRect);
  end;

var DWM: TDWMHelper;

implementation
//------------------------------------------------------------------------------
constructor TDWMHelper.Create;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize:= sizeof(TOSVersionInfo);
  GetVersionEx(VerInfo);
  IsVista:= VerInfo.dwMajorVersion >= 6;

  hDwmLib:= LoadLibrary('dwmapi.dll');
  if hDwmLib <> 0 then
  begin
    @DwmIsCompositionEnabled:= GetProcAddress(hDwmLib, 'DwmIsCompositionEnabled');
    @DwmEnableBlurBehindWindow:= GetProcAddress(hDwmLib, 'DwmEnableBlurBehindWindow');
    @DwmSetWindowAttribute:= GetProcAddress(hDwmLib, 'DwmSetWindowAttribute');
    @DwmExtendFrameIntoClientArea:= GetProcAddress(hDwmLib, 'DwmExtendFrameIntoClientArea');
  end;
end;
//------------------------------------------------------------------------------
destructor TDWMHelper.Destroy;
begin
  FreeLibrary(hDwmLib);
  inherited;
end;
//------------------------------------------------------------------------------
function TDWMHelper.CompositingEnabled: boolean;
var
  enabled: Boolean;
begin
  enabled:= false;
  if @DwmIsCompositionEnabled <> nil then DwmIsCompositionEnabled(@enabled);
  result:= enabled;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
var
  bb: _DWM_BLURBEHIND;
begin
  if CompositingEnabled and (@DwmEnableBlurBehindWindow <> nil) then
  begin
    ZeroMemory(@bb, SizeOf(bb));
    bb.dwFlags:= 3;
    bb.fEnable:= true;
    bb.hRgnBlur:= rgn;
    DwmEnableBlurBehindWindow(AHandle, @bb);
  end else
    DisableBlurBehindWindow(AHandle);
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.DisableBlurBehindWindow(const AHandle: THandle);
var
  bb: _DWM_BLURBEHIND;
begin
  if @DwmEnableBlurBehindWindow <> nil then
  begin
    ZeroMemory(@bb, SizeOf(bb));
    bb.dwFlags:= 1;
    bb.fEnable:= false;
    DwmEnableBlurBehindWindow(AHandle, @bb);
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.ExcludeFromPeek(const AHandle: THandle);
var
  exclude: integer;
begin
  if @DwmSetWindowAttribute <> nil then
  begin
    exclude := -1;
    DwmSetWindowAttribute(AHandle, DWMWA_EXCLUDED_FROM_PEEK, @exclude, sizeof(exclude));
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.ExtendFrameIntoClientArea(const AHandle: THandle; margins: windows.TRect);
begin
  if @DwmExtendFrameIntoClientArea <> nil then DwmExtendFrameIntoClientArea(AHandle, margins);
end;
//------------------------------------------------------------------------------
initialization
  DWM:= TDWMHelper.Create;
finalization
  DWM.free;
end.
