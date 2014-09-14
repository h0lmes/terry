unit DockH;

// modified july 11, 2012 //

interface
uses Windows;

type
  _PluginData = Pointer;
  _OnGetInformation = procedure(szName, szAuthor: pchar; lpiVersion: PInteger; szNotes: pchar); stdcall;
  _OnCreate = function(id, hInstance: uint; szIni, szIniGroup: pchar): _PluginData; stdcall;
  _OnSave = procedure(lpData: _PluginData; szIni, szIniGroup: pchar; isForExport: boolean); stdcall;
  _OnDestroy = procedure(lpData: _PluginData; id: uint); stdcall;
  _OnLeftButtonClick = function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnDoubleClick = function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnRightButtonClick = function(lpData: _PluginData; ptCursor: PPoint; lpSize: PSize): boolean; stdcall;
  _OnConfigure = procedure(lpData: _PluginData); stdcall;
  _OnAcceptDropFiles = function(lpData: _PluginData): boolean; stdcall;
  _OnDropFiles = procedure(lpData: _PluginData; hDrop: uint); stdcall;
  _OnProcessMessage = procedure(lpData: _PluginData; hwnd, uMsg: uint; wParam: WPARAM; lParam: LPARAM); stdcall;

function DockletIsVisible(id: uint): bool;
function DockletIsUndocked(id: uint): bool;
function DockletGetRect(id: uint; r: PRect): bool;
function DockletGetLabel(id: uint; szCaption: pchar): integer;
function DockletSetLabel(id: uint; szCaption: pchar): integer;
procedure DockletSetImage(id: uint; image: pointer; AutoDelete: bool);
procedure DockletSetImageFile(id: uint; szImage: pchar);
procedure DockletSetImageOverlay(id: uint; overlay: pointer; AutoDelete: bool);
procedure DockletDoAttensionAnimation(id: uint);
procedure DockletGetRootFolder(id: uint; szFolder: pchar);
procedure DockletGetRelativeFolder(id: uint; szFolder: pchar);
function DockletBrowseForImage(id: uint; szImage: pchar; szRoot: pchar): bool;
function DockletLoadGDIPlusImage(szImage: pchar): pointer;
procedure DockletLockMouseEffect(id: uint; lock: bool);
function DockletQueryDockEdge(id: uint): integer;
function DockletSetDockEdge(id: uint; Edge: integer): integer;
function DockletQueryDockAlign(id: uint): integer;
function DockletSetDockAlign(id: uint; Offset: integer): integer;
// additional API, Terry specific //
function DockColorDialog(color: puint): bool;
function DockGetRect: TRect;
procedure DockExecute(id: HWND; exename, params, dir: pchar; showcmd: integer);
function DockAddMenu(hMenu: HWND): uint;
function EdgeFromSDK(edge: integer): integer;
function EdgeToSDK(edge: integer): integer;
procedure Undock(id: uint);
procedure Dock(id: uint);
function DockCreateItem(data: pchar): uint;
procedure DockDeleteItem(id: uint);
function DockAddProgram(data: pchar): uint;
function FullScreenAppActive(id: uint): bool;
procedure Notify(id: uint; Message: PAnsiChar);

implementation
//------------------------------------------------------------------------------
function DockletIsVisible(id: uint): bool;
type dtype = function(id: uint): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletIsVisible');
	if not assigned(proc) then result:= false else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletIsUndocked(id: uint): bool;
type dtype = function(id: uint): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletIsUndocked');
	if not assigned(proc) then result:= false else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletGetRect(id: uint; r: PRect): bool;
type dtype = function(id: uint; r: PRect): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetRect');
	if not assigned(proc) then result:= false else result:= proc(id, r);
end;
//------------------------------------------------------------------------------
function DockletGetLabel(id: uint; szCaption: pchar): integer;
type dtype = function(id: uint; szCaption: pchar): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetLabel');
	if not assigned(proc) then result:= 0 else result:= proc(id, szCaption);
end;
//------------------------------------------------------------------------------
function DockletSetLabel(id: uint; szCaption: pchar): integer;
type dtype = function(id: uint; szCaption: pchar): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetLabel');
	if not assigned(proc) then result:= 0 else result:= proc(id, szCaption);
end;
//------------------------------------------------------------------------------
procedure DockletSetImage(id: uint; image: pointer; AutoDelete: bool);
type dtype = procedure(id: uint; image: pointer; AutoDelete: bool); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImage');
	if assigned(proc) then proc(id, image, AutoDelete);
end;
//------------------------------------------------------------------------------
procedure DockletSetImageFile(id: uint; szImage: pchar);
type dtype = procedure(id: uint; szImage: pchar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImageFile');
	if assigned(proc) then proc(id, szImage);
end;
//------------------------------------------------------------------------------
procedure DockletSetImageOverlay(id: uint; overlay: pointer; AutoDelete: bool);
type dtype = procedure(id: uint; overlay: pointer; AutoDelete: bool); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetImageOverlay');
	if assigned(proc) then proc(id, overlay, AutoDelete);
end;
//------------------------------------------------------------------------------
procedure DockletDoAttensionAnimation(id: uint);
type dtype = procedure(id: uint); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletDoAttensionAnimation');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
procedure DockletGetRootFolder(id: uint; szFolder: pchar);
type dtype = procedure(id: uint; szFolder: pchar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletGetRootFolder');
	if assigned(proc) then proc(id, szFolder);
end;
//------------------------------------------------------------------------------
procedure DockletGetRelativeFolder(id: uint; szFolder: pchar);
type dtype = procedure(id: uint; szFolder: pchar); stdcall;
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
function DockletBrowseForImage(id: uint; szImage: pchar; szRoot: pchar): bool;
type dtype = function(id: uint; szImage: pchar; szRoot: pchar): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletBrowseForImage');
	if not assigned(proc) then result:= false else result:= proc(id, szImage, szRoot);
end;
//------------------------------------------------------------------------------
function DockletQueryDockEdge(id: uint): integer;
type dtype = function(id: uint): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletQueryDockEdge');
	if not assigned(proc) then result:= 0 else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletSetDockEdge(id: uint; Edge: integer): integer;
type dtype = function(id: uint; Edge: integer): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetDockEdge');
	if not assigned(proc) then result:= 0 else result:= proc(id, Edge);
end;
//------------------------------------------------------------------------------
function DockletQueryDockAlign(id: uint): integer;
type dtype = function(id: uint): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletQueryDockAlign');
	if not assigned(proc) then result:= 0 else result:= proc(id);
end;
//------------------------------------------------------------------------------
function DockletSetDockAlign(id: uint; Offset: integer): integer;
type dtype = function(id: uint; Offset: integer): integer; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockletSetDockAlign');
	if not assigned(proc) then result:= 0 else result:= proc(id, Offset);
end;
//------------------------------------------------------------------------------
procedure DockletLockMouseEffect(id: uint; lock: bool);
type dtype = procedure(id: uint; lock: bool); stdcall;
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
function DockAddMenu(hMenu: HWND): uint;
type dtype = function(hMenu: HWND): uint; stdcall;
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
procedure Undock(id: uint);
type dtype = procedure(id: uint); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Undock');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
procedure Dock(id: uint);
type dtype = procedure(id: uint); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Dock');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
function DockCreateItem(data: pchar): uint;
type dtype = function(data: pchar): uint; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockCreateItem');
	if not assigned(proc) then result := 0 else result := proc(data);
end;
//------------------------------------------------------------------------------
procedure DockDeleteItem(id: uint);
type dtype = procedure(id: uint); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockDeleteItem');
	if assigned(proc) then proc(id);
end;
//------------------------------------------------------------------------------
function DockAddProgram(data: pchar): uint;
type dtype = function(data: pchar): uint; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'DockAddProgram');
	if not assigned(proc) then result := 0 else result := proc(data);
end;
//------------------------------------------------------------------------------
function FullScreenAppActive(id: uint): bool;
type dtype = function(id: HWND): bool; stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'FullScreenAppActive');
	if not assigned(proc) then result := false else result := proc(id);
end;
//------------------------------------------------------------------------------
procedure Notify(id: uint; Message: PAnsiChar);
type dtype = procedure(id: uint; Message: PAnsiChar); stdcall;
var
  proc: dtype;
begin
	@proc:= GetProcAddress(GetModuleHandle(nil), 'Notify');
	if assigned(proc) then proc(id, Message);
end;
//------------------------------------------------------------------------------
end.
