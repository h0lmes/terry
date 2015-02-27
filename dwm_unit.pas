unit dwm_unit;

interface
uses windows, math;

const
  WM_DWMCOMPOSITIONCHANGED       = $031E;
  WM_DWMNCRENDERINGCHANGED       = $031F;
  WM_DWMCOLORIZATIONCOLORCHANGED = $0320;
  WM_DWMWINDOWMAXIMIZEDCHANGE    = $0321;

  // _DWM_THUMBNAIL_PROPERTIES.dwFlags
  DWM_TNP_RECTDESTINATION      = $1; // A value for the rcDestination member has been specified.
  DWM_TNP_RECTSOURCE           = $2; // A value for the rcSource member has been specified.
  DWM_TNP_OPACITY              = $4; // A value for the opacity member has been specified.
  DWM_TNP_VISIBLE              = $8; // A value for the fVisible member has been specified.
  DWM_TNP_SOURCECLIENTAREAONLY = $10; // A value for the fSourceClientAreaOnly member has been specified.

  PEEK_TYPE_DESKTOP = 1;
  PEEK_TYPE_WINDOW = 3;

type
  _DWMWINDOWATTRIBUTE = (
      DWMWA_NCRENDERING_ENABLED = 1, // Discovers whether non-client rendering is enabled
      DWMWA_NCRENDERING_POLICY, // Sets the non-client rendering policy
      DWMWA_TRANSITIONS_FORCEDISABLED, // Enables or forcibly disables DWM transitions
      DWMWA_ALLOW_NCPAINT, // Enables content rendered in the non-client area to be visible on the frame drawn by DWM
      DWMWA_CAPTION_BUTTON_BOUNDS, // Retrieves the bounds of the caption button area in the window-relative space
      DWMWA_NONCLIENT_RTL_LAYOUT, // Specifies whether non-client content is right-to-left (RTL) mirrored
      DWMWA_FORCE_ICONIC_REPRESENTATION, // Forces the window to display an iconic thumbnail or peek representation (a static bitmap)
      DWMWA_FLIP3D_POLICY, // Sets how Flip3D treats the window
      DWMWA_EXTENDED_FRAME_BOUNDS, // Retrieves the extended frame bounds rectangle in screen space
      // since WIN 7
      DWMWA_HAS_ICONIC_BITMAP, // The window will provide a bitmap for use by DWM as an iconic thumbnail or peek representation (a static bitmap) for the window
      DWMWA_DISALLOW_PEEK, // Do not show peek preview for the window
      DWMWA_EXCLUDED_FROM_PEEK, // Prevents a window from fading to a glass sheet when peek is invoked
      // since WIN 8
      DWMWA_CLOAK, // Cloaks the window such that it is not visible to the user
      DWMWA_CLOAKED, // If the window is cloaked, provides one of the following values explaining why: DWM_CLOAKED_APP = 1, DWM_CLOAKED_SHELL = 2, DWM_CLOAKED_INHERITED = 4
      DWMWA_FREEZE_REPRESENTATION // Freeze the window's thumbnail image with its current visuals
  );

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
      DwmSetWindowAttribute: function(Wnd: HWND; dwAttribute: _DWMWINDOWATTRIBUTE; pvAttribute: Pointer; cb: DWORD): HRESULT; stdcall;
      DwmGetWindowAttribute: function(Wnd: HWND; dwAttribute: _DWMWINDOWATTRIBUTE; pvAttribute: Pointer; cb: DWORD): HRESULT; stdcall;
      DwmExtendFrameIntoClientArea: function(Wnd: HWND; var margins: windows.TRect): HRESULT; stdcall;
      DwmGetColorizationColor: function(var pcrColorization: DWORD; var pfOpaqueBlend: bool): HRESULT; stdcall;
      //
      DwmRegisterThumbnail: function(hwndDestination, hwndSource: HWND; phThumbnailId: PHandle): HRESULT; stdcall;
      DwmUnregisterThumbnail: function(hThumbnailId: THandle): HRESULT; stdcall;
      DwmUpdateThumbnailProperties: function(hThumbnailId: THandle; ptnProperties: P_DWM_THUMBNAIL_PROPERTIES): HRESULT; stdcall;
      DwmQueryThumbnailSourceSize: function(hThumbnailId: THandle; pSize: PSize): HRESULT; stdcall;
      //
      //DwmInvokeAeroPeek: function(Enable: integer; Target, Caller: THandle; PeekType: integer; p: pointer; x3244: IntPtr): HRESULT; stdcall;
      DwmInvokeAeroPeek: function(Enable: integer; Target, Caller: THandle; PeekType: integer): HRESULT; stdcall;
    public
      constructor Create;
      destructor Destroy; override;
      function CompositionEnabled: boolean;
      procedure EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
      procedure DisableBlurBehindWindow(const AHandle: THandle);
      procedure ExcludeFromPeek(const AHandle: THandle);
      function IsWindowCloaked(const AHandle: THandle): boolean;
      procedure GetColorizationColor(var color: cardinal; var opaque: bool);
      procedure EnableNCRendering(const AHandle: THandle);
      procedure ExtendFrameIntoClientArea(const AHandle: THandle; margins: windows.TRect);
      //
      function RegisterThumbnail(hwndDestination, hwndSource: HWND; var hThumbnailId: THandle): boolean;
      function GetThumbnailSize(hThumbnailId: THandle; var w, h: integer): boolean;
      function SetThumbnailRect(hThumbnailId: THandle; destRect: TRect): boolean;
      function SetThumbnailVisible(hThumbnailId: THandle; visible: boolean): boolean;
      procedure UnregisterThumbnail(hThumbnailId: THandle);
      //
      procedure InvokeAeroPeek(Enable: integer; Target, Caller: THandle);
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
    @DwmGetWindowAttribute:= GetProcAddress(hDwmLib, 'DwmGetWindowAttribute');
    @DwmExtendFrameIntoClientArea:= GetProcAddress(hDwmLib, 'DwmExtendFrameIntoClientArea');
    @DwmGetColorizationColor:= GetProcAddress(hDwmLib, 'DwmGetColorizationColor');
    //
    @DwmRegisterThumbnail := GetProcAddress(hDwmLib, 'DwmRegisterThumbnail');
    @DwmUnregisterThumbnail := GetProcAddress(hDwmLib, 'DwmUnregisterThumbnail');
    @DwmUpdateThumbnailProperties := GetProcAddress(hDwmLib, 'DwmUpdateThumbnailProperties');
    @DwmQueryThumbnailSourceSize := GetProcAddress(hDwmLib, 'DwmQueryThumbnailSourceSize');
    //
    @DwmInvokeAeroPeek := GetProcAddress(hDwmLib, pchar(113));
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
  value: integer;
begin
  if @DwmSetWindowAttribute <> nil then
  begin
    value := -1;
    DwmSetWindowAttribute(AHandle, DWMWA_EXCLUDED_FROM_PEEK, @value, sizeof(value));
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.GetColorizationColor(var color: cardinal; var opaque: bool);
begin
  if @DwmGetColorizationColor <> nil then DwmGetColorizationColor(color, opaque);
end;
//------------------------------------------------------------------------------
function TDWMHelper.IsWindowCloaked(const AHandle: THandle): boolean;
var
  value: integer;
begin
  result := false;
  if @DwmGetWindowAttribute <> nil then
  begin
    value := 0;
    if SUCCEEDED(DwmGetWindowAttribute(AHandle, DWMWA_CLOAKED, @value, sizeof(value))) then result := value <> 0;
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.EnableNCRendering(const AHandle: THandle);
var
  value: integer;
begin
  if @DwmSetWindowAttribute <> nil then
  begin
    value := 3;
    DwmSetWindowAttribute(AHandle, DWMWA_NCRENDERING_POLICY, @value, sizeof(value));
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
function TDWMHelper.RegisterThumbnail(hwndDestination, hwndSource: HWND; var hThumbnailId: THandle): boolean;
var
  dskThumbProps: _DWM_THUMBNAIL_PROPERTIES;
begin
  result := false;
  if CompositionEnabled then
  begin
    result := Succeeded(DwmRegisterThumbnail(hwndDestination, hwndSource, @hThumbnailId));

    FillChar(dskThumbProps, sizeof(_DWM_THUMBNAIL_PROPERTIES), #0);
    dskThumbProps.dwFlags := DWM_TNP_SOURCECLIENTAREAONLY;
    dskThumbProps.fSourceClientAreaOnly := true;
		DwmUpdateThumbnailProperties(hThumbnailId, @dskThumbProps);
  end;
end;
//------------------------------------------------------------------------------
function TDWMHelper.GetThumbnailSize(hThumbnailId: THandle; var w, h: integer): boolean;
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
    result := Succeeded(DwmQueryThumbnailSourceSize(hThumbnailId, @Size));
    w := Size.cx;
    h := Size.cy;
  end;
end;
//------------------------------------------------------------------------------
function TDWMHelper.SetThumbnailRect(hThumbnailId: THandle; destRect: TRect): boolean;
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
      dskThumbProps.dwFlags := DWM_TNP_RECTDESTINATION or DWM_TNP_VISIBLE or DWM_TNP_SOURCECLIENTAREAONLY or DWM_TNP_OPACITY;
      dskThumbProps.fSourceClientAreaOnly := true;
		  dskThumbProps.fVisible := true;
		  dskThumbProps.opacity := 255;
		  dskThumbProps.rcDestination := destRect;
		  hr := DwmUpdateThumbnailProperties(hThumbnailId, @dskThumbProps);
      result := SUCCEEDED(hr);
  end;
end;
//------------------------------------------------------------------------------
function TDWMHelper.SetThumbnailVisible(hThumbnailId: THandle; visible: boolean): boolean;
var
  hr: HRESULT;
  dskThumbProps: _DWM_THUMBNAIL_PROPERTIES;
begin
  result := false;
  if CompositionEnabled then
  begin
      FillChar(dskThumbProps, sizeof(_DWM_THUMBNAIL_PROPERTIES), #0);
      dskThumbProps.dwFlags := DWM_TNP_VISIBLE;
      dskThumbProps.fVisible := visible;
		  hr := DwmUpdateThumbnailProperties(hThumbnailId, @dskThumbProps);
      result := SUCCEEDED(hr);
  end;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.UnregisterThumbnail(hThumbnailId: THandle);
begin
  if @DwmUnregisterThumbnail <> nil then DwmUnregisterThumbnail(hThumbnailId);
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.InvokeAeroPeek(Enable: integer; Target, Caller: THandle);
var
  ip: pinteger;
begin
  //ip := pinteger(32);
  //if assigned(DwmInvokeAeroPeek) then DwmInvokeAeroPeek(Enable, Target, Caller, 1, ip, $3244);
  if assigned(DwmInvokeAeroPeek) then DwmInvokeAeroPeek(Enable, Target, Caller, 1);
end;
//------------------------------------------------------------------------------
initialization
  DWM:= TDWMHelper.Create;
finalization
  DWM.free;
end.
