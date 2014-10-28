unit dwm_unit;

interface
uses windows, math;

const
  WM_DWMCOMPOSITIONCHANGED = $031E;
  DWMWA_EXCLUDED_FROM_PEEK = 12;

  // _DWM_THUMBNAIL_PROPERTIES.dwFlags
  DWM_TNP_RECTDESTINATION      = $1; // A value for the rcDestination member has been specified.
  DWM_TNP_RECTSOURCE           = $2; // A value for the rcSource member has been specified.
  DWM_TNP_OPACITY              = $4; // A value for the opacity member has been specified.
  DWM_TNP_VISIBLE              = $8; // A value for the fVisible member has been specified.
  DWM_TNP_SOURCECLIENTAREAONLY = $10; // A value for the fSourceClientAreaOnly member has been specified.

type
  _DWM_BLURBEHIND = record
    dwFlags: dword;
    fEnable: bool;
    hRgnBlur: HRGN;
    fTransitionOnMaximized: bool;
  end;
  P_DWM_BLURBEHIND = ^_DWM_BLURBEHIND;

  _DWM_THUMBNAIL_PROPERTIES = record
    dwFlags: DWORD;
    rcDestination: TRect;
    rcSource: TRect;
    opacity: byte;
    fVisible: bool;
    fSourceClientAreaOnly: bool;
  end;
  P_DWM_THUMBNAIL_PROPERTIES = ^_DWM_THUMBNAIL_PROPERTIES;

  { TDWMHelper }

  TDWMHelper = class
    private
      IsVista: boolean;
      hDwmLib: uint;
      DwmIsCompositionEnabled: function(pfEnabled: PBoolean): HRESULT; stdcall;
      DwmEnableBlurBehindWindow: function(Wnd: HWND; bb: P_DWM_BLURBEHIND): HRESULT; stdcall;
      DwmSetWindowAttribute: function(Wnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cb: DWORD): HRESULT; stdcall;
      DwmExtendFrameIntoClientArea: function(Wnd: HWND; var margins: windows.TRect): HRESULT; stdcall;
      //
      DwmRegisterThumbnail: function(hwndDestination, hwndSource: HWND; phThumbnailId: PHandle): HRESULT; stdcall;
      DwmUnregisterThumbnail: function(hThumbnailId: THandle): HRESULT; stdcall;
      DwmUpdateThumbnailProperties: function(hThumbnailId: THandle; ptnProperties: P_DWM_THUMBNAIL_PROPERTIES): HRESULT; stdcall;
      DwmQueryThumbnailSourceSize: function(hThumbnailId: THandle; pSize: PSize): HRESULT; stdcall;
    public
      constructor Create;
      destructor Destroy; override;
      function CompositionEnabled: boolean;
      procedure EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
      procedure DisableBlurBehindWindow(const AHandle: THandle);
      procedure ExcludeFromPeek(const AHandle: THandle);
      procedure ExtendFrameIntoClientArea(const AHandle: THandle; margins: windows.TRect);
      //
      function RegisterThumbnail(hwndDestination, hwndSource: HWND; destRect: TRect; var hThumbnailId: THandle): boolean;
      procedure UnregisterThumbnail(hThumbnailId: THandle);
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
    //
    @DwmRegisterThumbnail := GetProcAddress(hDwmLib, 'DwmRegisterThumbnail');
    @DwmUnregisterThumbnail := GetProcAddress(hDwmLib, 'DwmUnregisterThumbnail');
    @DwmUpdateThumbnailProperties := GetProcAddress(hDwmLib, 'DwmUpdateThumbnailProperties');
    @DwmQueryThumbnailSourceSize := GetProcAddress(hDwmLib, 'DwmQueryThumbnailSourceSize');
  end;
end;
//------------------------------------------------------------------------------
destructor TDWMHelper.Destroy;
begin
  FreeLibrary(hDwmLib);
  inherited;
end;
//------------------------------------------------------------------------------
function TDWMHelper.CompositionEnabled: boolean;
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
  if CompositionEnabled and (@DwmEnableBlurBehindWindow <> nil) then
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
  if CompositionEnabled then
    if @DwmExtendFrameIntoClientArea <> nil then
      DwmExtendFrameIntoClientArea(AHandle, margins);
end;
//------------------------------------------------------------------------------
function TDWMHelper.RegisterThumbnail(hwndDestination, hwndSource: HWND; destRect: TRect; var hThumbnailId: THandle): boolean;
var
  hr: HRESULT;
  Size: windows.TSize;
  ThumbW, ThumbH: integer;
  dskThumbProps: _DWM_THUMBNAIL_PROPERTIES;
  dstRatio, srcRatio: single;
begin
  result := false;
  if CompositionEnabled then
  begin
    hr := DwmRegisterThumbnail(hwndDestination, hwndSource, @hThumbnailId);
	  if SUCCEEDED(hr) then
	  begin
      DwmQueryThumbnailSourceSize(hThumbnailId, @Size);
      dstRatio := (destRect.Right - destRect.Left) / (destRect.Bottom - destRect.Top);
      srcRatio := Size.cx / Size.cy;
      if srcRatio < dstRatio then
      begin
        ThumbW := round((destRect.Bottom - destRect.Top) * srcRatio);
        destRect.Left := destRect.Left + (destRect.Right - destRect.Left - ThumbW) div 2;
        destRect.Right := destRect.Left + ThumbW;
      end;
      if srcRatio > dstRatio then
      begin
        ThumbH := round((destRect.Right - destRect.Left) / srcRatio);
        destRect.Top := destRect.Top + (destRect.Bottom - destRect.Top - ThumbH) div 2;
        destRect.Bottom := destRect.Top + ThumbH;
      end;

      FillChar(dskThumbProps, sizeof(_DWM_THUMBNAIL_PROPERTIES), #0);
      dskThumbProps.dwFlags := DWM_TNP_RECTDESTINATION or DWM_TNP_VISIBLE or DWM_TNP_SOURCECLIENTAREAONLY;
      dskThumbProps.fSourceClientAreaOnly := false;
		  dskThumbProps.fVisible := true;
		  dskThumbProps.opacity := 255;
		  dskThumbProps.rcDestination := destRect;
		  hr := DwmUpdateThumbnailProperties(hThumbnailId, @dskThumbProps);
      result := SUCCEEDED(hr);
      if not result then UnregisterThumbnail(hThumbnailId);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.UnregisterThumbnail(hThumbnailId: THandle);
begin
  if @DwmUnregisterThumbnail <> nil then
     DwmUnregisterThumbnail(hThumbnailId);
end;
//------------------------------------------------------------------------------
initialization
  DWM:= TDWMHelper.Create;
finalization
  DWM.free;
end.
