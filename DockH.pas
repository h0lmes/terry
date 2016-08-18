unit DockH;

// modified aug 17, 2016 //

interface
uses Windows;

type
  _PluginData = Pointer;
  _OnGetInformation =   procedure(szName, szAuthor: pchar; lpiVersion: PInteger; szNotes: pchar); stdcall;
  _OnCreate =           function(id: HWND; hInstance: THandle; szIni, szIniGroup: pchar): _PluginData; stdcall;
  _OnSave =             procedure(lpData: _PluginData; szIni, szIniGroup: pchar; isForExport: boolean); stdcall;
  _OnDestroy =          procedure(lpData: _PluginData; id: HWND); stdcall;
  _OnLeftButtonClick =  function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnLeftButtonHeld =   function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnDoubleClick =      function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnRightButtonClick = function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnConfigure =        procedure(lpData: _PluginData); stdcall;
  _OnAcceptDropFiles =  function(lpData: _PluginData): boolean; stdcall;
  _OnDropFiles =        procedure(lpData: _PluginData; hDrop: THandle); stdcall;
  _OnProcessMessage =   procedure(lpData: _PluginData; hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM); stdcall;

function DockletIsVisible(id: HWND): bool;
function DockletIsUndocked(id: HWND): bool;
function DockletGetRect(id: HWND; r: PRect): bool;
function DockletGetLabel(id: HWND; szCaption: pchar): integer;
function DockletSetLabel(id: HWND; szCaption: pchar): integer;
procedure DockletSetImage(id: HWND; image: pointer; AutoDelete: bool);
procedure DockletSetImageFile(id: HWND; szImage: pchar);
procedure DockletSetImageOverlay(id: HWND; overlay: pointer; AutoDelete: bool);
procedure DockletDoAttensionAnimation(id: HWND);
procedure DockletGetRootFolder(id: HWND; szFolder: pchar);
procedure DockletGetRelativeFolder(id: HWND; szFolder: pchar);
function DockletBrowseForImage(id: HWND; szImage: pchar; szRoot: pchar): bool;
function DockletLoadGDIPlusImage(szImage: pchar): pointer;
procedure DockletLockMouseEffect(id: HWND; lock: bool);
function DockletQueryDockEdge(id: HWND): integer;
function DockletSetDockEdge(id: HWND; Edge: integer): integer;
function DockletQueryDockAlign(id: HWND): integer;
function DockletSetDockAlign(id: HWND; Offset: integer): integer;
// additional API, Terry specific //
function DockColorDialog(color: puint): bool;
function DockGetRect: TRect;
procedure DockExecute(id: HWND; exename, params, dir: pchar; showcmd: integer);
function DockAddMenu(hMenu: THandle): THandle;
function EdgeFromSDK(edge: integer): integer;
function EdgeToSDK(edge: integer): integer;
procedure Undock(id: HWND);
procedure Dock(id: HWND);
function DockCreateItem(data: pchar): HWND;
procedure DockDeleteItem(id: HWND);
function DockAddProgram(data: pchar): HWND;
procedure Notify(id: HWND; Message: PAnsiChar);
procedure ActivateHint(id: HWND; ACaption: PWideChar; x, y: integer);
procedure DeactivateHint(id: HWND);
procedure ExcludeFromPeek(id: HWND);

implementation
//------------------------------------------------------------------------------
function DockletIsVisible(id: HWND): bool;
type dtype = function(id: HWND): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletIsVisible');
	if not assigned(proc) then result:= false else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletIsUndocked(id: HWND): bool;
type dtype = function(id: HWND): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletIsUndocked');
	if not assigned(proc) then result:= false else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletGetRect(id: HWND; r: PRect): bool;
type dtype = function(id: HWND; r: PRect): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetRect');
	if not assigned(proc) then result:= false else result:= proc(id, r);
end;
//------------------------------------------------------------------------------
function DockletGetLabel(id: HWND; szCaption: pchar): integer;
type dtype = function(id: HWND; szCaption: pchar): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetLabel');
	if not assigned(proc) then result:= 0 else result:= proc(id, szCaption);
end;
//------------------------------------------------------------------------------
function DockletSetLabel(id: HWND; szCaption: pchar): integer;
type dtype = function(id: HWND; szCaption: pchar): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetLabel');
	if not assigned(proc) then result:= 0 else result:= proc(id, szCaption);
end;
//------------------------------------------------------------------------------
procedure DockletSetImage(id: HWND; image: pointer; AutoDelete: bool);
type dtype = procedure(id: HWND; image: pointer; AutoDelete: bool); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImage');
	if assigned(proc) then proc(id, image, AutoDelete);
end;
//------------------------------------------------------------------------------
procedure DockletSetImageFile(id: HWND; szImage: pchar);
type dtype = procedure(id: HWND; szImage: pchar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImageFile');
	if assigned(proc) then proc(id, szImage);
end;
//------------------------------------------------------------------------------
procedure DockletSetImageOverlay(id: HWND; overlay: pointer; AutoDelete: bool);
type dtype = procedure(id: HWND; overlay: pointer; AutoDelete: bool); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImageOverlay');
	if assigned(proc) then proc(id, overlay, AutoDelete);
end;
//------------------------------------------------------------------------------
procedure DockletDoAttensionAnimation(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletDoAttensionAnimation');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
procedure DockletGetRootFolder(id: HWND; szFolder: pchar);
type dtype = procedure(id: HWND; szFolder: pchar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetRootFolder');
	if assigned(proc) then proc(id, szFolder);
end;
//------------------------------------------------------------------------------
procedure DockletGetRelativeFolder(id: HWND; szFolder: pchar);
type dtype = procedure(id: HWND; szFolder: pchar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetRelativeFolder');
	if assigned(proc) then proc(id, szFolder);
end;
//------------------------------------------------------------------------------
function DockletLoadGDIPlusImage(szImage: pchar): pointer;
type dtype = function(szImage: pchar): pointer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletLoadGDIPlusImage');
	if not assigned(proc) then result:= nil else result:= proc(szImage);
end;
//------------------------------------------------------------------------------
function DockletBrowseForImage(id: HWND; szImage: pchar; szRoot: pchar): bool;
type dtype = function(id: HWND; szImage: pchar; szRoot: pchar): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletBrowseForImage');
	if not assigned(proc) then result:= false else result:= proc(id, szImage, szRoot);
end;
//------------------------------------------------------------------------------
function DockletQueryDockEdge(id: HWND): integer;
type dtype = function(id: HWND): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletQueryDockEdge');
	if not assigned(proc) then result:= 0 else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletSetDockEdge(id: HWND; Edge: integer): integer;
type dtype = function(id: HWND; Edge: integer): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetDockEdge');
	if not assigned(proc) then result:= 0 else result:= proc(id, Edge);
end;
//------------------------------------------------------------------------------
function DockletQueryDockAlign(id: HWND): integer;
type dtype = function(id: HWND): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletQueryDockAlign');
	if not assigned(proc) then result:= 0 else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletSetDockAlign(id: HWND; Offset: integer): integer;
type dtype = function(id: HWND; Offset: integer): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetDockAlign');
	if not assigned(proc) then result:= 0 else result:= proc(id, Offset);
end;
//------------------------------------------------------------------------------
procedure DockletLockMouseEffect(id: HWND; lock: bool);
type dtype = procedure(id: HWND; lock: bool); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletLockMouseEffect');
	if assigned(proc) then proc(id, lock);
end;
//------------------------------------------------------------------------------
//
//
//  ADDITIONAL API
//
//
//------------------------------------------------------------------------------
function DockColorDialog(color: puint): bool;
type dtype = function(color: puint): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockColorDialog');
	if not assigned(proc) then result:= false else result:= proc(color);
end;
//------------------------------------------------------------------------------
function DockGetRect: TRect;
type dtype = function: TRect; stdcall;
var
  proc: dtype;
begin
  result.left := 0;
  result.top := 0;
  result.right := 0;
  result.bottom := 0;
	@proc := GetProcAddress(GetModuleHandle(nil), 'DockGetRect');
	if assigned(proc) then result := proc;
end;
//------------------------------------------------------------------------------
procedure DockExecute(id: HWND; exename, params, dir: pchar; showcmd: integer);
type dtype = procedure(id: HWND; exename, params, dir: pchar; showcmd: integer); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockExecute');
	if assigned(proc) then proc(id, exename, params, dir, showcmd);
end;
//------------------------------------------------------------------------------
function DockAddMenu(hMenu: THandle): THandle;
type dtype = function(hMenu: THandle): THandle; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockAddMenu');
	if not assigned(proc) then result := 0 else result := proc(hMenu);
end;
//------------------------------------------------------------------------------
function EdgeFromSDK(edge: integer): integer;
begin
  case edge of
    0: result := 3;//bsBottom;
    1: result := 1;//bsTop;
    2: result := 0;//bsLeft;
    3: result := 2;//bsRight;
  end;
end;
//------------------------------------------------------------------------------
function EdgeToSDK(edge: integer): integer;
begin
  case edge of
    0: result := 2;
    1: result := 1;
    2: result := 3;
    3: result := 0;
  end;
end;
//------------------------------------------------------------------------------
procedure Undock(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Undock');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
procedure Dock(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Dock');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
function DockCreateItem(data: pchar): HWND;
type dtype = function(data: pchar): HWND; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockCreateItem');
	if not assigned(proc) then result := 0 else result := proc(data);
end;
//------------------------------------------------------------------------------
procedure DockDeleteItem(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockDeleteItem');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
function DockAddProgram(data: pchar): HWND;
type dtype = function(data: pchar): HWND; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockAddProgram');
	if not assigned(proc) then result := 0 else result := proc(data);
end;
//------------------------------------------------------------------------------
procedure Notify(id: HWND; Message: PAnsiChar);
type dtype = procedure(id: HWND; Message: PAnsiChar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Notify');
	if assigned(proc) then proc(id, Message);
end;
//------------------------------------------------------------------------------
procedure ActivateHint(id: HWND; ACaption: PWideChar; x, y: integer);
type dtype = procedure(id: HWND; ACaption: PWideChar; x, y: integer); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'ActivateHint');
	if assigned(proc) then proc(id, ACaption, x, y);
end;
//------------------------------------------------------------------------------
procedure DeactivateHint(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DeactivateHint');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
procedure ExcludeFromPeek(id: HWND);
type dtype = procedure(id: HWND); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'ExcludeFromPeek');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
end.
