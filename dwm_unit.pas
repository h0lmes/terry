//------------------------------------------------------------------------------
//
//
//
// This unit incorporates functionality to interact with Desktop Windows Manager
//
//
//
//------------------------------------------------------------------------------

unit dwm_unit;

interface
uses windows, sysutils, math;

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

  _DWMFLIP3DWINDOWPOLICY = (
      DWMFLIP3D_DEFAULT,
      DWMFLIP3D_EXCLUDEBELOW,
      DWMFLIP3D_EXCLUDEABOVE,
      DWMFLIP3D_LAST
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

  _WINDOWCOMPOSITIONATTRIBUTE = (
      WCA_CLIENTRENDERING_POLICY = 16,
      WCA_ACCENT_POLICY = 19
  );

  _ACCENTSTATE = (
		  ACCENT_DISABLED = 0,
		  ACCENT_ENABLE_GRADIENT = 1,
		  ACCENT_ENABLE_TRANSPARENTGRADIENT = 2,
		  ACCENT_ENABLE_BLURBEHIND = 3,
		  ACCENT_INVALID_STATE = 4
  );

  TWindowCompositionAttributeData = packed record
    attribute: THandle; //_WINDOWCOMPOSITIONATTRIBUTE;
    data: Pointer;
    size: dword;
  end;

  TAccentPolicy = packed record
    AccentState: integer; //_ACCENTSTATE;
    AccentFlags: integer;
    GradientColor: integer;
    AnimationId: integer;
  end;

  { TDWMHelper }

  TDWMHelper = class
    private
      IsVista: boolean;
      IsWin7: boolean;
      IsWin10: boolean;
      hDwmLib: HANDLE;
      DwmDefWindowProc: function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM; Result: PInteger): BOOL; stdcall;
      SetWindowCompositionAttribute: function(Wnd: HWND; const AttrData: TWindowCompositionAttributeData): BOOL; stdcall;
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
      function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): integer;
      function IsCompositionEnabled: boolean;
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
  IsVista := VerInfo.dwMajorVersion >= 6;
  IsWin7 := (VerInfo.dwMajorVersion > 6) or ((VerInfo.dwMajorVersion = 6) and (VerInfo.dwMinorVersion >= 2));
  IsWin10 := VerInfo.dwMajorVersion >= 10;

  SetWindowCompositionAttribute := GetProcAddress(GetModuleHandle(user32), 'SetWindowCompositionAttribute');

  hDwmLib:= LoadLibrary('dwmapi.dll');
  if hDwmLib <> 0 then
  begin
    @DwmDefWindowProc := GetProcAddress(hDwmLib, 'DwmDefWindowProc');
    @DwmIsCompositionEnabled := GetProcAddress(hDwmLib, 'DwmIsCompositionEnabled');
    @DwmEnableBlurBehindWindow := GetProcAddress(hDwmLib, 'DwmEnableBlurBehindWindow');
    @DwmSetWindowAttribute := GetProcAddress(hDwmLib, 'DwmSetWindowAttribute');
    @DwmGetWindowAttribute := GetProcAddress(hDwmLib, 'DwmGetWindowAttribute');
    @DwmExtendFrameIntoClientArea := GetProcAddress(hDwmLib, 'DwmExtendFrameIntoClientArea');
    @DwmGetColorizationColor := GetProcAddress(hDwmLib, 'DwmGetColorizationColor');
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
function TDWMHelper.DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): integer;
var
  ret: integer;
  done: boolean = false;
begin
  if @DwmDefWindowProc <> nil then done := DwmDefWindowProc(hWnd, Msg, wParam, lParam, @ret);
  if not done then ret := SendMessage(hWnd, Msg, wParam, lParam);
  result := ret;
end;
//------------------------------------------------------------------------------
function TDWMHelper.IsCompositionEnabled: boolean;
var
  enabled: Boolean;
begin
  enabled := false;
  if @DwmIsCompositionEnabled <> nil then DwmIsCompositionEnabled(@enabled);
  result:= enabled;
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.EnableBlurBehindWindow(const AHandle: THandle; rgn: HRGN);
var
  bb: _DWM_BLURBEHIND;
  accent: TAccentPolicy;
  flag: bool;
  data: TWindowCompositionAttributeData;
  margins: windows.TRect;
begin
  if IsWin10 then
  begin
	  ZeroMemory(@accent, sizeof(TAccentPolicy));
	  ZeroMemory(@data, sizeof(TWindowCompositionAttributeData));
	  accent.AccentState := integer(_ACCENTSTATE.ACCENT_ENABLE_BLURBEHIND);
	  data.attribute := THandle(_WINDOWCOMPOSITIONATTRIBUTE.WCA_ACCENT_POLICY);
	  data.size := sizeof(TAccentPolicy);
	  data.data := @accent;
	  SetWindowCompositionAttribute(AHandle, data);

    flag := true;
	  data.attribute := THandle(_WINDOWCOMPOSITIONATTRIBUTE.WCA_CLIENTRENDERING_POLICY);
	  data.size := sizeof(flag);
	  data.data := @flag;
	  SetWindowCompositionAttribute(AHandle, data);
  end
  else
  begin
	    if IsCompositionEnabled and (@DwmEnableBlurBehindWindow <> nil) then
	    begin
	      ZeroMemory(@bb, SizeOf(bb));
	      bb.dwFlags := 3;
	      bb.fEnable := true;
	      bb.hRgnBlur := rgn;
	      DwmEnableBlurBehindWindow(AHandle, @bb);
	    end else
	      DisableBlurBehindWindow(AHandle);
	end;

  if rgn <> 0 then DeleteObject(rgn);
end;
//------------------------------------------------------------------------------
procedure TDWMHelper.DisableBlurBehindWindow(const AHandle: THandle);
var
  bb: _DWM_BLURBEHIND;
  accent: TAccentPolicy;
  flag: bool;
  data: TWindowCompositionAttributeData;
begin
  if IsWin10 then
  begin
    SetWindowRgn(AHandle, 0, true);
    ZeroMemory(@accent, sizeof(TAccentPolicy));
    ZeroMemory(@data, sizeof(TWindowCompositionAttributeData));
    accent.AccentState := integer(_ACCENTSTATE.ACCENT_DISABLED);
    data.attribute := THandle(_WINDOWCOMPOSITIONATTRIBUTE.WCA_ACCENT_POLICY);
    data.size := sizeof(TAccentPolicy);
    data.data := @accent;
    SetWindowCompositionAttribute(AHandle, data);

    flag := false;
    data.attribute := THandle(_WINDOWCOMPOSITIONATTRIBUTE.WCA_CLIENTRENDERING_POLICY);
    data.size := sizeof(flag);
    data.data := @flag;
    SetWindowCompositionAttribute(AHandle, data);
  end
  else
  begin
       if @DwmEnableBlurBehindWindow <> nil then
       begin
	      ZeroMemory(@bb, SizeOf(bb));
	      bb.dwFlags := 1;
	      bb.fEnable := false;
	      DwmEnableBlurBehindWindow(AHandle, @bb);
       end;
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
    value := integer(_DWMFLIP3DWINDOWPOLICY.DWMFLIP3D_EXCLUDEBELOW);
    DwmSetWindowAttribute(AHandle, DWMWA_FLIP3D_POLICY, @value, sizeof(value));
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
  if IsCompositionEnabled then
    if @DwmExtendFrameIntoClientArea <> nil then
      DwmExtendFrameIntoClientArea(AHandle, margins);
end;
//------------------------------------------------------------------------------
function TDWMHelper.RegisterThumbnail(hwndDestination, hwndSource: HWND; var hThumbnailId: THandle): boolean;
var
  dskThumbProps: _DWM_THUMBNAIL_PROPERTIES;
begin
  result := false;
  if IsCompositionEnabled then
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
  if IsCompositionEnabled then
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
  try
  if IsCompositionEnabled then
  begin
      DwmQueryThumbnailSourceSize(hThumbnailId, @Size);
      dstRatio := 1;
      if destRect.Bottom > destRect.Top then
        dstRatio := (destRect.Right - destRect.Left) / (destRect.Bottom - destRect.Top);
      srcRatio := 1;
      if Size.cy > 0 then srcRatio := Size.cx / Size.cy;
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
  except
    on e: Exception do raise Exception.Create('DWMHelper.SetThumbnailRect'#13#10 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TDWMHelper.SetThumbnailVisible(hThumbnailId: THandle; visible: boolean): boolean;
var
  hr: HRESULT;
  dskThumbProps: _DWM_THUMBNAIL_PROPERTIES;
begin
  result := false;
  if IsCompositionEnabled then
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
  DWM := TDWMHelper.Create;
finalization
  DWM.free;
end.
