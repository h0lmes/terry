unit dropsrcu;

{$mode delphi}

interface

uses Windows, ActiveX, ShlObj, Classes, SysUtils, ShellAPI;

type
  TDragDropInfo = class(TObject)
  private
    FInClientArea : Boolean;
    FDropPoint : TPoint;
    FFileList : TStringList;
  public
    constructor Create(ADropPoint: TPoint; AInClient: Boolean);
    destructor Destroy; override;
    procedure Add(const s: string);
    function CreateHDrop: HGlobal;
    property InClientArea: Boolean read FInClientArea;
    property DropPoint: TPoint read FDropPoint;
    property Files: TStringList read FFileList;
  end;

  TFileDropSource = class(TInterfacedObject, IDropSource)
    constructor Create;
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult; stdcall;
    function GiveFeedback(dwEffect: DWORD): HResult; stdcall;
  end;

  THDropDataObject = class(TInterfacedObject, IDataObject)
  private
    FDropInfo : TDragDropInfo;
  public
    constructor Create(ADropPoint: TPoint; AInClient : Boolean);
    destructor Destroy; override;
    procedure Add(const s : String);
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: tagFormatEtc;
      const medium: tagStgMedium; fRelease: longbool): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longword;
      out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: tagFormatEtc; advf: Longword;
      const advSink: IAdviseSink; out dwConnection: Longword): HResult; stdcall;
    function DUnadvise(dwConnection: Longword): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

implementation

uses EnumFmt;
//------------------------------------------------------------------------------
// TDragDropInfo //
//------------------------------------------------------------------------------
constructor TDragDropInfo.Create(ADropPoint : TPoint; AInClient : Boolean);
begin
  inherited Create;
  FFileList := TStringList.Create;
  FDropPoint := ADropPoint;
  FInClientArea := AInClient;
end;
//------------------------------------------------------------------------------
destructor TDragDropInfo.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure TDragDropInfo.Add(const s: string);
begin
  Files.Add(s);
end;
//------------------------------------------------------------------------------
function TDragDropInfo.CreateHDrop: HGlobal;
var
  RequiredSize : Integer;
  i : Integer;
  hGlobalDropInfo : HGlobal;
  DropFiles: PDropFiles;
  c: PChar;
begin
  RequiredSize := sizeof (TDropFiles);
  for i := 0 to Files.Count - 1 do
    RequiredSize := RequiredSize + Length(Files[i]) + 1;
  inc (RequiredSize);

  hGlobalDropInfo := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT, RequiredSize);
  if hGlobalDropInfo <> 0 then
  begin
    DropFiles := GlobalLock(hGlobalDropInfo);
    DropFiles.pFiles := sizeof(TDropFiles);
    DropFiles.pt := Self.FDropPoint;
    DropFiles.fNC := Self.InClientArea;
    DropFiles.fWide := false;

    c := pchar(DropFiles);
    c := c + DropFiles.pFiles;
    for i := 0 to Files.Count - 1 do
    begin
      StrCopy(c, PChar(Files[i]));
      c := c + Length(Files[i]);
    end;

    GlobalUnlock(hGlobalDropInfo);
  end;
  Result := hGlobalDropInfo;
end;
//------------------------------------------------------------------------------
//
// TFileDropSource //
//
//------------------------------------------------------------------------------
constructor TFileDropSource.Create;
begin
  inherited Create;
  _AddRef;
end;
//------------------------------------------------------------------------------
// QueryContinueDrag определяет необходимые действия. Функция предполагает,
// что для перетаскивания используется только левая кнопка мыши //
function TFileDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HResult;
begin
  if fEscapePressed then
  begin
    Result := DRAGDROP_S_CANCEL;
  end
  else
  if grfKeyState and MK_LBUTTON = 0 then
  begin
    Result := DRAGDROP_S_DROP;
  end
  else
  begin
    Result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
function TFileDropSource.GiveFeedback(dwEffect: DWORD): HResult;
begin
  case dwEffect of
    DROPEFFECT_NONE,
    DROPEFFECT_COPY,
    DROPEFFECT_LINK,
    DROPEFFECT_SCROLL: Result := DRAGDROP_S_USEDEFAULTCURSORS;
    else
      Result := S_OK;
  end;
end;
//------------------------------------------------------------------------------
// THDropDataObject //
//------------------------------------------------------------------------------
constructor THDropDataObject.Create(ADropPoint : TPoint; AInClient : Boolean);
begin
  inherited Create;
  _AddRef;
  FDropInfo := TDragDropInfo.Create(ADropPoint, AInClient);
end;
//------------------------------------------------------------------------------
destructor THDropDataObject.Destroy;
begin
  if FDropInfo <> nil then FDropInfo.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------
procedure THDropDataObject.Add(const s: string);
begin
  FDropInfo.Add(s);
end;
//------------------------------------------------------------------------------
function THDropDataObject.GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
  medium.tymed := 0;
  medium.hGlobal := 0;
  medium.PUnkForRelease := nil;

  // if supported format //
  if QueryGetData(formatetcIn) = S_OK then
  begin
    if FDropInfo <> nil then
    begin
      medium.tymed := TYMED_HGLOBAL;
      medium.hGlobal := FDropInfo.CreateHDrop;
      Result := S_OK;
    end;
  end;
end;
//------------------------------------------------------------------------------
function THDropDataObject.GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;  // не поддерживается //
end;
//------------------------------------------------------------------------------
function THDropDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  result := DV_E_FORMATETC;
  with formatetc do
    if dwAspect = DVASPECT_CONTENT then
      if (cfFormat = CF_HDROP) and (tymed = TYMED_HGLOBAL) then
        Result := S_OK;
end;
//------------------------------------------------------------------------------
function THDropDataObject.GetCanonicalFormatEtc(
  const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HResult;
begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;
//------------------------------------------------------------------------------
function THDropDataObject.SetData(const formatetc: tagFormatEtc;
  const medium: tagStgMedium; fRelease: Longbool): HResult;
begin
  Result := E_NOTIMPL;
end;
//------------------------------------------------------------------------------
// EnumFormatEtc возвращает список поддерживаемых форматов //
function THDropDataObject.EnumFormatEtc(dwDirection: Longword;
  out enumFormatEtc: IEnumFormatEtc): HResult;
const
  DataFormats: array [0..0] of TFormatEtc =
  (
    (
      cfFormat : CF_HDROP;
      ptd      : nil;
      dwAspect : DVASPECT_CONTENT;
      lindex   : -1;
      tymed    : TYMED_HGLOBAL;
    )
  );
  DataFormatCount = 1;
begin
  // Поддерживается только Get. Задать содержимое данных нельзя //
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := TEnumFormatEtc.Create(@DataFormats, DataFormatCount, 0);
    result := S_OK;
  end
  else
  begin
    enumFormatEtc := nil;
    result := E_NOTIMPL;
  end;
end;
//------------------------------------------------------------------------------
// Функции Advise не поддерживаются //
function THDropDataObject.DAdvise(const formatetc: tagFormatEtc; advf: Longword;
  const advSink: IAdviseSink; out dwConnection: Longword): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
//------------------------------------------------------------------------------
function THDropDataObject.DUnadvise(dwConnection: Longword): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
//------------------------------------------------------------------------------
function THDropDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
//------------------------------------------------------------------------------
initialization
OleInitialize(nil);
//------------------------------------------------------------------------------
finalization
OleUninitialize;
//------------------------------------------------------------------------------
end.
