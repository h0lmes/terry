unit DropTgtU;

interface

uses Windows, Classes, SysUtils, ActiveX, ShlObj, ShellAPI, PIDL, declu, notifieru;

type
  _DropTarget = class;

  TProc = procedure of object;
  TDropProc = procedure(list: TStrings; hWnd: uint) of object;

  // Life states //
  Tgwdt_ls = (gwdt_ls_start, gwdt_ls_exists, gwdt_ls_locked, gwdt_ls_regd);

  _DropManager = class
  protected
    FDropTarget: _DropTarget;
    FList: TStrings;
    procedure  DropTarget_Forget;  // used by DropTarget.Destroy;
  public
    OnDragOver: TProc;
    OnDragLeave: TProc;
    OnDrop: TDropProc;
    OnDragEnter: TDropProc;
    property  DropTarget: _DropTarget read FDropTarget;
    // Create in  OwnerForm.OnCreate, Destroy in OwnerForm.OnDestroy
    constructor Create(ADropHWnd: HWND); virtual;
    destructor  Destroy; override;
    function DropTarget_Create(ADropHWnd: HWND): HResult;
    function DropTarget_Exists: Boolean;
    function DropTarget_LifeState: Tgwdt_ls;
    procedure AddToListHDrop(h: HDROP);
    procedure AddToListHGlobalPIDL(h: HGLOBAL);
    procedure AddToListIStreamPIDL(h: Pointer);
    procedure AddToListIStreamFileName(h: Pointer);
    procedure MakeList(const dataObj: IDataObject);
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
    function DragLeave: HResult; virtual;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
  end;

  _DropTarget = class(TObject, IUnknown, IDropTarget)
  private
    FDropHWND: HWND;
    FDropInterface: _DropManager;
    FLifeState: Tgwdt_ls;
    FEnable: boolean;
    FRefCount: Integer;
    procedure SetLifeState(Value: Tgwdt_ls);
  protected
    function QueryInterface(constref IID: TGUID; out Obj): longint; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    property DropHWND: HWND read FDropHWnd;
    property LifeState: Tgwdt_ls Read FLifeState Write SetLifeState;
    property Enable: Boolean Read FEnable     Write FEnable;
    property RefCount: Integer read FRefCount;

    constructor Create(ADropHWnd: HWND; ADropManager: _DropManager); virtual;
    destructor  Destroy; override;
    function ToState_Exists : HResult;
    function ToState_Locked : HResult;
    function ToState_Regd   : HResult;

    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
  end;

implementation
//------------------------------------------------------------------------------
//
//
//
//    _DropManager
//
//
//
//------------------------------------------------------------------------------
constructor _DropManager.Create(ADropHWnd: HWND);
begin
  inherited Create;
  FList := TStringList.Create;
  FDropTarget := nil;
  DropTarget_Create(ADropHWnd);
end;
//------------------------------------------------------------------------------
destructor _DropManager.Destroy;
begin
  FList.free;
  if Assigned(FDropTarget) then FDropTarget.Destroy;
  inherited Destroy;
end;
//------------------------------------------------------------------------------
function _DropManager.DropTarget_Create(ADropHWnd: HWND): HResult;
begin
  result := E_UNEXPECTED;
  FDropTarget := _DropTarget.Create(ADropHWnd, Self);
  if Assigned(FDropTarget) then result := DropTarget.ToState_Regd;
end;
//------------------------------------------------------------------------------
function _DropManager.DropTarget_Exists: Boolean;
begin
  result := Assigned(FDropTarget);
end;
//------------------------------------------------------------------------------
procedure _DropManager.DropTarget_Forget;
begin
  FDropTarget := nil;
end;
//------------------------------------------------------------------------------
function _DropManager.DropTarget_LifeState: Tgwdt_ls;
begin
  if DropTarget_Exists then result := DropTarget.LifeState
  else result := gwdt_ls_Start;
end;
//------------------------------------------------------------------------------
procedure _DropManager.AddToListHDrop(h: HDROP);
var
  i, size: uint;
  filename: array [0..MAX_PATH] of char;
begin
  try
    size := DragQueryFile(h, $ffffffff, nil, 0);
    i := 0;
    while i < size do
    begin
      dragQueryFile(h, i, filename, sizeof(filename));
      FList.Add(filename);
      inc(i);
    end;
    DragFinish(h);
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListHDrop'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _DropManager.AddToListIStreamFileName(h: Pointer);
var
  ist: IStream;
  stat: STATSTG;
  size: longint;
  data: array [0..1024] of char;
  cbRead: dword;
begin
  try
    ist := IStream(h);
    ist.Stat(stat, 0);
    size := longint(stat.cbSize);
    ist.Read(@data, size, @cbRead);
    FList.Add(strpas(pchar(@data)));
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamFileName'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _DropManager.AddToListHGlobalPIDL(h: HGLOBAL);
var
  p: Pointer;
  i, gsize, size, qty: longint;
  pidl: PItemIDList;
  data: array [0..1024] of char;
  idpath: string;
begin
  try
    p := GlobalLock(h);
    gsize := GlobalSize(h);

    qty := PIDL_CountFromCIDA(p);
    for i := 0 to qty - 1 do
    begin
      size := gsize;
      pidl := PIDL_FromCIDA(i, p, size);
      if assigned(pidl) then
      begin
        idpath := PIDL_ToString(pidl, size);
        PIDL_Free(pidl);
        FList.Add(idpath);
      end;
    end;
    GlobalUnlock(h);
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListHGlobalPIDL'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// TODO: check if CIDA is valid
procedure _DropManager.AddToListIStreamPIDL(h: Pointer);
var
  ist: IStream;
  stat: STATSTG;
  i, size, qty: longint;
  data: array [0..1024] of char;
  cbRead: dword;
  pidl: PItemIDList;
  idpath: string;
begin
  try
    ist := IStream(h);
    ist.Stat(stat, 0);
    size := longint(stat.cbSize);
    ist.Read(@data, size, @cbRead);

    {i:= 0;
    while i < cbRead do
    begin
      idpath := idpath + inttohex(byte(data[i]), 2);
      inc(i);
    end;
    notifier.message(idpath);}

    qty := PIDL_CountFromCIDA(@data);
    for i := 0 to qty - 1 do
    begin
      size := cbRead;
      pidl := PIDL_FromCIDA(i, @data, size);
      if assigned(pidl) then
      begin
        idpath := PIDL_ToString(pidl, size);
        PIDL_Free(pidl);
        FList.Add(idpath);
      end;
    end;
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamPIDL'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _DropManager.MakeList(const dataObj: IDataObject);
var
  Rslt: HResult;
  EnumFormatEtc: IEnumFormatEtc;
  FetchedCount: longint;
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  ch: array [0..50] of char;
begin
  if S_OK <> dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc) then exit;

  // handle HDROP //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    if FormatEtc.cfFormat = CF_HDROP then
      if FormatEtc.tymed and TYMED_HGLOBAL <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL then AddToListHDrop(StgMedium.hGlobal);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  if FList.Count > 0 then exit;

  // handle SHELLIDLIST //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while (Rslt = S_OK) and (FList.Count < 1) do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    if stricomp(pchar(@ch), 'SHELL IDLIST ARRAY') = 0 then
      if FormatEtc.tymed and TYMED_HGLOBAL <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL then AddToListHGlobalPIDL(StgMedium.hGlobal)
          else if StgMedium.tymed and TYMED_ISTREAM = TYMED_ISTREAM then AddToListIStreamPIDL(StgMedium.pstm);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  if FList.Count > 0 then exit;

  // handle FileName //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    if stricomp(pchar(@ch), 'FILENAME') = 0 then
      if FormatEtc.tymed and TYMED_HGLOBAL <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_FILE = TYMED_FILE then FList.Add(pchar(StgMedium.lpszFileName))
          else if StgMedium.tymed and TYMED_ISTREAM = TYMED_ISTREAM then AddToListIStreamFileName(StgMedium.pstm);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
end;
//------------------------------------------------------------------------------
function _DropManager.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  result := S_OK;
  if not assigned(OnDragEnter) then exit;
  OnDragEnter(nil, WindowFromPoint(pt));
end;
//------------------------------------------------------------------------------
function _DropManager.DragOver(grfKeyState : DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if assigned(OnDragOver) then OnDragOver;
  result := S_OK;
end;
//------------------------------------------------------------------------------
function _DropManager.DragLeave: HResult;
begin
  if assigned(OnDragLeave) then OnDragLeave;
  result := S_OK;
end;
//------------------------------------------------------------------------------
function _DropManager.Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  result := S_OK;
  if not assigned(OnDrop) then exit;
  FList.Clear;
  MakeList(dataObj);
  OnDrop(FList, WindowFromPoint(pt));
  FList.Clear;
end;
//------------------------------------------------------------------------------
//
//
//
//   DropTarget
//
//
//
//------------------------------------------------------------------------------
const
  di_DragEnter = 3;
  di_DragOver = 4;
  di_DragLeave = 5;
  di_Drop = 6;
//------------------------------------------------------------------------------
function _DropTarget.QueryInterface(constref IID: TGUID; out Obj): longint; stdcall;
const E_NOINTERFACE = $80004002;
begin
  if GetInterface(IID, Obj) then result := 0 else result := E_NOINTERFACE;
end;
//------------------------------------------------------------------------------
function _DropTarget._AddRef: Integer;
begin
  inc(FRefCount);
  result:= FRefCount;
end;
//------------------------------------------------------------------------------
function _DropTarget._Release: Integer;
begin
  dec(FRefCount);
  if FRefCount = 0 then
  begin
    Destroy;
    result:= 0;
  end else result:= FRefCount;
end;
//------------------------------------------------------------------------------
constructor _DropTarget.Create(ADropHWnd: HWND; ADropManager: _DropManager);
begin
  inherited Create;
  FDropHWND      := ADropHWnd;
  FDropInterface := ADropManager;
  FLifeState     := gwdt_ls_exists;
  FEnable        := true;
end;
//------------------------------------------------------------------------------
destructor _DropTarget.Destroy;
begin
  if Assigned(FDropInterface) then FDropInterface.DropTarget_Forget;
  if FLifeState > gwdt_ls_Locked then
  begin
    while RefCount < 2 do _AddRef;   // avoid retriggering destroy
    ActiveX.RevokeDragDrop(FDropHWND);
    FLifeState := gwdt_ls_Locked;
  end;

  if FLifeState > gwdt_ls_Exists then
  begin
    while RefCount < 2 do _AddRef;   // avoid retriggering destroy
    ActiveX.CoLockObjectExternal(IDropTarget(self), false, false);
    FLifeState := gwdt_ls_Exists;
  end;

  inherited Destroy;
end;
//------------------------------------------------------------------------------
function _DropTarget.ToState_Exists : HResult;
begin
  result := S_OK;
  if LifeState = gwdt_ls_Regd then result := ToState_Locked;

  if LifeState = gwdt_ls_Locked then
  begin
    LifeState := gwdt_ls_Exists;
    result := ActiveX.CoLockObjectExternal(IDropTarget(self), false, true);
  end;
end;
//------------------------------------------------------------------------------
function _DropTarget.ToState_Locked : HResult;
begin
  result := S_OK;

  if LifeState = gwdt_ls_Exists then
  begin
    result := ActiveX.CoLockObjectExternal(IDropTarget(self), true, false);
    if result = S_OK then LifeState := gwdt_ls_Locked;
  end;

  if LifeState = gwdt_ls_Regd then
  begin
    while RefCount < 2 do _AddRef;
    result := ActiveX.RevokeDragDrop(FDropHWND);
    if Result = S_OK then LifeState := gwdt_ls_Locked;
  end;
end;
//------------------------------------------------------------------------------
function _DropTarget.ToState_Regd: HResult;
begin
  result := S_OK;
  if LifeState = gwdt_ls_Exists then result := ToState_Locked;
  if LifeState = gwdt_ls_Locked then
  begin
    result := ActiveX.RegisterDragDrop(FDropHWND, IDropTarget(self));
    if result = S_OK then LifeState := gwdt_ls_Regd;
  end;
end;
//------------------------------------------------------------------------------
procedure _DropTarget.SetLifeState(Value: Tgwdt_ls);
begin
  FLifeState := Value;
end;
//------------------------------------------------------------------------------
function _DropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if enable then Result := FDropInterface.DragEnter(dataObj, grfKeyState, pt, dwEffect)
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
function _DropTarget.DragOver(grfKeyState : DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if Enable then result := FDropInterface.DragOver(grfKeyState, pt, dwEffect)
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
function _DropTarget.DragLeave: HResult;
begin
  result := FDropInterface.DragLeave;
end;
//------------------------------------------------------------------------------
function _DropTarget.Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if Enable then result := FDropInterface.Drop(dataObj, grfKeyState, pt, dwEffect)
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
initialization
  OleInitialize(nil);
//------------------------------------------------------------------------------
finalization
  OleUninitialize;
//------------------------------------------------------------------------------
end.

