unit dwm_unit;

interface
uses windows;

const
  WM_DWMCOMPOSITIONCHANGED = $031E;

type
  _DWM_BLURBEHIND = record
    dwFlags: dword;
    fEnable: bool;
    hRgnBlur: HRGN;
    fTransitionOnMaximized: bool;
  end;
  P_DWM_BLURBEHIND = ^_DWM_BLURBEHIND;

  _DWM = class
    private
      IsVista: boolean;
      hDwmLib: uint;
      DwmIsCompositionEnabled: function(pfEnabled: PBoolean): HRESULT; stdcall;
      DwmEnableBlurBehindWindow: function(destWnd: HWND; bb: P_DWM_BLURBEHIND): HRESULT; stdcall;
    public
      constructor Create;
      destructor Destroy; override;
      function CompositingEnabled: boolean;
      procedure EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
      procedure DisableBlurBehindWindow(const AHandle: THandle);
  end;

var DWM: _DWM;

implementation
//------------------------------------------------------------------------------
constructor _DWM.Create;
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
  end;
end;
//------------------------------------------------------------------------------
destructor _DWM.Destroy;
begin
  FreeLibrary(hDwmLib);
  inherited;
end;
//------------------------------------------------------------------------------
function _DWM.CompositingEnabled: boolean;
var
  enabled: Boolean;
begin
  enabled:= false;
  if @DwmIsCompositionEnabled <> nil then DwmIsCompositionEnabled(@enabled);
  result:= enabled;
end;
//------------------------------------------------------------------------------
procedure _DWM.EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
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
procedure _DWM.DisableBlurBehindWindow(const AHandle: THandle);
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
initialization
  DWM:= _DWM.Create;
finalization
  DWM.free;
end.
