unit DropTgtU;

interface

uses Windows, Classes, SysUtils, ActiveX, ShlObj, ShellAPI, PIDL, declu, loggeru;

type
  TDropTarget = class;

  TProc = procedure of object;
  TDropProc = procedure(list: TStrings; hWnd: THandle) of object;

  // Life states //
  Tgwdt_ls = (gwdt_ls_start, gwdt_ls_exists, gwdt_ls_locked, gwdt_ls_regd);

  TDropManager = class
  protected
    FDropTarget: TDropTarget;
    procedure  DropTarget_Forget;  // used by DropTarget.Destroy;
    function tymedToString(tymed: DWORD): string;
  public
    OnDragOver: TProc;
    OnDragLeave: TProc;
    OnDrop: TDropProc;
    OnDragEnter: TDropProc;
    property  DropTarget: TDropTarget read FDropTarget;
    // Create in  OwnerForm.OnCreate, Destroy in OwnerForm.OnDestroy
    constructor Create(ADropHWnd: HWND); virtual;
    destructor  Destroy; override;
    function DropTarget_Create(ADropHWnd: HWND): HResult;
    function DropTarget_Exists: Boolean;
    function DropTarget_LifeState: Tgwdt_ls;
    procedure AddToListHDrop(h: HDROP; var List: TStrings);
    procedure AddToListHGlobalPIDL(h: HGLOBAL; var List: TStrings);
    procedure AddToListIStreamPIDL(h: Pointer; var List: TStrings);
    procedure AddToListFileFile(lpsz: POLESTR; var List: TStrings);
    procedure AddToListIStreamFileName(h: Pointer; var List: TStrings);
    procedure AddToListHGlobalFileGroupDescriptorW(h: HGLOBAL; var List: TStrings);
    procedure MakeList(const dataObj: IDataObject; var List: TStrings);
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
    function DragLeave: HResult; virtual;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; virtual;
  end;

  TDropTarget = class(TObject, IUnknown, IDropTarget)
  private
    FDropHWND: HWND;
    FDropInterface: TDropManager;
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

    constructor Create(ADropHWnd: HWND; ADropManager: TDropManager); virtual;
    destructor  Destroy; override;
    function ToState_Exists : HResult;
    function ToState_Locked : HResult;
    function ToState_Regd   : HResult;

    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult; stdcall;
  end;

{$undef DEBUG_DROPTGT}
implementation
//------------------------------------------------------------------------------
//
//
//
//    TDropManager
//
//
//
//------------------------------------------------------------------------------
constructor TDropManager.Create(ADropHWnd: HWND);
begin
  inherited Create;
  FDropTarget := nil;
  DropTarget_Create(ADropHWnd);
end;
//------------------------------------------------------------------------------
destructor TDropManager.Destroy;
begin
  if Assigned(FDropTarget) then FDropTarget.Destroy;
  inherited Destroy;
end;
//------------------------------------------------------------------------------
function TDropManager.DropTarget_Create(ADropHWnd: HWND): HResult;
begin
  result := E_UNEXPECTED;
  FDropTarget := TDropTarget.Create(ADropHWnd, Self);
  if Assigned(FDropTarget) then result := DropTarget.ToState_Regd;
end;
//------------------------------------------------------------------------------
function TDropManager.DropTarget_Exists: Boolean;
begin
  result := Assigned(FDropTarget);
end;
//------------------------------------------------------------------------------
procedure TDropManager.DropTarget_Forget;
begin
  FDropTarget := nil;
end;
//------------------------------------------------------------------------------
function TDropManager.DropTarget_LifeState: Tgwdt_ls;
begin
  if DropTarget_Exists then result := DropTarget.LifeState
  else result := gwdt_ls_Start;
end;
//------------------------------------------------------------------------------
procedure TDropManager.AddToListHDrop(h: HDROP; var List: TStrings);
var
  i, size: uint;
  filenameW: array [0..MAX_PATH - 1] of wchar;
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListHDrop'); {$endif}

    size := DragQueryFileW(h, $ffffffff, nil, 0);
    i := 0;
    while i < size do
    begin
      DragQueryFileW(h, i, @filenameW, MAX_PATH);
      List.Add(strpas(pwchar(@filenameW)));
      {$ifdef DEBUG_DROPTGT} AddLog(filenameW); {$endif}
      inc(i);
    end;
    DragFinish(h);
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListHDrop ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TDropManager.AddToListIStreamFileName(h: Pointer; var List: TStrings);
var
  ist: IStream;
  stat: STATSTG;
  size: longint;
  data: array [0..MAX_PATH - 1] of char;
  cbRead: dword;
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListIStreamFileName'); {$endif}

    FillChar(data, MAX_PATH, 0);
    ist := IStream(h);
    ist.Stat(stat, 0);
    size := longint(stat.cbSize);
    if size > MAX_PATH then size := MAX_PATH;
    ist.Read(@data, size, @cbRead);
    List.Add(strpas(pchar(@data)));
    {$ifdef DEBUG_DROPTGT} AddLog(strpas(pchar(@data))); {$endif}
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamFileName ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TDropManager.AddToListHGlobalPIDL(h: HGLOBAL; var List: TStrings);
var
  p: Pointer;
  i, gsize, size, qty: longint;
  pidl: PItemIDList;
  data: array [0..4095] of char;
  idpath: string;
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListHGlobalPIDL'); {$endif}

    FillChar(data, 4096, 0);
    p := GlobalLock(h);
    gsize := GlobalSize(h);

    {$ifdef DEBUG_DROPTGT}
    AddLog('Raw data size = ' + inttostr(gsize));
    i:= 0;
    while i < gsize do
    begin
      idpath := idpath + inttohex(pbyte(PtrUInt(p) + i)^, 2);
      inc(i);
    end;
    AddLog('Raw data = ' + idpath);
    {$endif}

    qty := PIDL_CountFromCIDA(p);
    {$ifdef DEBUG_DROPTGT} AddLog('PIDL count = ' + inttostr(qty)); {$endif}

    for i := 0 to qty - 1 do
    begin
      size := gsize;
      pidl := PIDL_FromCIDA(i, p, size);
      if assigned(pidl) then
      begin
        idpath := PIDL_GetDisplayName2(pidl);
        PIDL_Free(pidl);
        List.Add(idpath);
        {$ifdef DEBUG_DROPTGT} AddLog('PIDL ToString = ' + idpath); {$endif}
      end;
    end;
    GlobalUnlock(h);
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListHGlobalPIDL ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
// TODO: check if CIDA is valid
procedure TDropManager.AddToListIStreamPIDL(h: Pointer; var List: TStrings);
var
  ist: IStream;
  stat: STATSTG;
  i, size, qty: longint;
  data: array [0..4095] of char;
  cbRead: dword;
  pidl: PItemIDList;
  idpath: string;
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListIStreamPIDL'); {$endif}

    FillChar(data, 4096, 0);
    ist := IStream(h);
    ist.Stat(stat, 0);
    size := longint(stat.cbSize);
    ist.Read(@data, size, @cbRead);

    {$ifdef DEBUG_DROPTGT}
    AddLog('Raw data size = ' + inttostr(cbRead));
    i:= 0;
    while i < cbRead do
    begin
      idpath := idpath + inttohex(byte(data[i]), 2);
      inc(i);
    end;
    AddLog('Raw data = ' + idpath);
    {$endif}

    qty := PIDL_CountFromCIDA(@data);
    {$ifdef DEBUG_DROPTGT} AddLog('PIDL count = ' + inttostr(qty)); {$endif}

    for i := 0 to qty - 1 do
    begin
      size := cbRead;
      pidl := PIDL_FromCIDA(i, @data, size);
      if assigned(pidl) then
      begin
        idpath := PIDL_GetDisplayName2(pidl);
        PIDL_Free(pidl);
        List.Add(idpath);
        {$ifdef DEBUG_DROPTGT} AddLog('PIDL ToString = ' + idpath); {$endif}
      end;
    end;
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamPIDL ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TDropManager.AddToListFileFile(lpsz: POLESTR; var List: TStrings);
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListFileFile'); {$endif}
    List.Add(strpas(pwchar(lpsz)));
    {$ifdef DEBUG_DROPTGT} AddLog(pchar(lpsz)); {$endif}
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamPIDL ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TDropManager.AddToListHGlobalFileGroupDescriptorW(h: HGLOBAL; var List: TStrings);
var
  i: longint;
  pgroup: PFILEGROUPDESCRIPTORW;
  pfgd: PFILEDESCRIPTORW;
begin
  try
    {$ifdef DEBUG_DROPTGT} AddLog('DropManager.AddToListHGlobalFileGroupDescriptorW'); {$endif}

    pgroup := GlobalLock(h);
    i := 0;
    while i < pgroup.cItems do
    begin
      pfgd := PFILEDESCRIPTORW(PtrUInt(@pgroup.fgd) + i * sizeof(PFILEDESCRIPTORW));
      List.Add(pfgd.cFileName);
      {$ifdef DEBUG_DROPTGT} AddLog(pfgd.cFileName); {$endif}
      inc(i);
    end;
    GlobalUnlock(h);
  except
    on e: Exception do raise Exception.Create('DropManager.AddToListIStreamPIDL ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TDropManager.tymedToString(tymed: DWORD): string;
begin
  result := '';
  if tymed and TYMED_HGLOBAL  = TYMED_HGLOBAL then  result += 'TYMED_HGLOBAL ';
  if tymed and TYMED_FILE     = TYMED_FILE then     result += 'TYMED_FILE ';
  if tymed and TYMED_ISTREAM  = TYMED_ISTREAM then  result += 'TYMED_ISTREAM ';
  if tymed and TYMED_ISTORAGE = TYMED_ISTORAGE then result += 'TYMED_ISTORAGE ';
  if tymed and TYMED_GDI      = TYMED_GDI then      result += 'TYMED_GDI ';
  if tymed and TYMED_MFPICT   = TYMED_MFPICT then   result += 'TYMED_MFPICT ';
  if tymed and TYMED_ENHMF    = TYMED_ENHMF then    result += 'TYMED_ENHMF ';
end;
//------------------------------------------------------------------------------
procedure TDropManager.MakeList(const dataObj: IDataObject; var List: TStrings);
var
  Rslt: HResult;
  EnumFormatEtc: IEnumFormatEtc;
  FetchedCount: longint;
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  ch: array [0..50] of char;
begin
  {$ifdef DEBUG_DROPTGT}
  AddLog('DropManager.MakeList');
  {$endif}

  if S_OK <> dataObj.EnumFormatEtc(DATADIR_GET, EnumFormatEtc) then exit;

  {$ifdef DEBUG_DROPTGT}
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    AddLog('cfFormat = ' + strpas(pchar(@ch)) + '. tymed = ' + tymedToString(FormatEtc.tymed));
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  {$endif}

  // handle FileGroupDescriptorW //
  {EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    if stricomp(pchar(@ch), 'FILEGROUPDESCRIPTORW') = 0 then
      if FormatEtc.tymed and TYMED_HGLOBAL <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL then AddToListHGlobalFileGroupDescriptorW(StgMedium.hGlobal, List);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  if List.Count > 0 then exit;}

  // handle HDROP //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    if FormatEtc.cfFormat = CF_HDROP then
      if FormatEtc.tymed and TYMED_HGLOBAL <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL then AddToListHDrop(StgMedium.hGlobal, List);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  if List.Count > 0 then exit;

  // handle SHELLIDLIST //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while (Rslt = S_OK) and (List.Count < 1) do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    if stricomp(pchar(@ch), 'SHELL IDLIST ARRAY') = 0 then
      if FormatEtc.tymed and (TYMED_HGLOBAL or TYMED_ISTREAM) <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_HGLOBAL = TYMED_HGLOBAL then AddToListHGlobalPIDL(StgMedium.hGlobal, List)
          else
          if StgMedium.tymed and TYMED_ISTREAM = TYMED_ISTREAM then AddToListIStreamPIDL(StgMedium.pstm, List);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
  if List.Count > 0 then exit;

  // handle FileName //
  EnumFormatEtc.Reset;
  Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  while Rslt = S_OK do
  begin
    GetClipboardFormatName(FormatEtc.cfFormat, @ch, 50);
    if stricomp(pchar(@ch), 'FILENAME') = 0 then
      if FormatEtc.tymed and (TYMED_FILE or TYMED_ISTREAM) <> 0 then
        if dataObj.GetData(FormatEtc, StgMedium) = S_OK then
        begin
          if StgMedium.tymed and TYMED_FILE = TYMED_FILE then AddToListFileFile(StgMedium.lpszFileName, List)
          else
          if StgMedium.tymed and TYMED_ISTREAM = TYMED_ISTREAM then AddToListIStreamFileName(StgMedium.pstm, List);
          try if StgMedium.tymed <> TYMED_NULL then ReleaseStgMedium(StgMedium);
          except end;
        end;
    Rslt := EnumFormatEtc.Next(1, FormatEtc, @FetchedCount);
  end;
end;
//------------------------------------------------------------------------------
function TDropManager.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  dwEffect := DROPEFFECT_COPY;
  result := S_OK;
  if assigned(OnDragEnter) then OnDragEnter(nil, WindowFromPoint(pt));
end;
//------------------------------------------------------------------------------
function TDropManager.DragOver(grfKeyState : DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if assigned(OnDragOver) then OnDragOver;
  result := S_OK;
end;
//------------------------------------------------------------------------------
function TDropManager.DragLeave: HResult;
begin
  if assigned(OnDragLeave) then OnDragLeave;
  result := S_OK;
end;
//------------------------------------------------------------------------------
function TDropManager.Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
var
  FList: TStrings;
begin
  dwEffect := DROPEFFECT_COPY;
  result := S_OK;
  {$ifdef DEBUG_DROPTGT}
  AddLog('DropManager.Drop');
  {$endif}
  if assigned(OnDrop) then
  begin
    FList := TStringList.Create;
    MakeList(dataObj, FList);
    OnDrop(FList, WindowFromPoint(pt));
    FList.Free;
  end;
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
function TDropTarget.QueryInterface(constref IID: TGUID; out Obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then result := 0 else result := longint(E_NOINTERFACE);
end;
//------------------------------------------------------------------------------
function TDropTarget._AddRef: Integer;
begin
  inc(FRefCount);
  result:= FRefCount;
end;
//------------------------------------------------------------------------------
function TDropTarget._Release: Integer;
begin
  dec(FRefCount);
  if FRefCount = 0 then
  begin
    Destroy;
    result:= 0;
  end else result:= FRefCount;
end;
//------------------------------------------------------------------------------
constructor TDropTarget.Create(ADropHWnd: HWND; ADropManager: TDropManager);
begin
  inherited Create;
  FDropHWND      := ADropHWnd;
  FDropInterface := ADropManager;
  FLifeState     := gwdt_ls_exists;
  FEnable        := true;
end;
//------------------------------------------------------------------------------
destructor TDropTarget.Destroy;
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
function TDropTarget.ToState_Exists : HResult;
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
function TDropTarget.ToState_Locked : HResult;
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
function TDropTarget.ToState_Regd: HResult;
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
procedure TDropTarget.SetLifeState(Value: Tgwdt_ls);
begin
  FLifeState := Value;
end;
//------------------------------------------------------------------------------
function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if enable then Result := FDropInterface.DragEnter(dataObj, grfKeyState, pt, dwEffect)
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
function TDropTarget.DragOver(grfKeyState : DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
begin
  if Enable then result := FDropInterface.DragOver(grfKeyState, pt, dwEffect)
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
function TDropTarget.DragLeave: HResult;
begin
  result := FDropInterface.DragLeave;
end;
//------------------------------------------------------------------------------
function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HResult;
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

