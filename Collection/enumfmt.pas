unit EnumFmt;

{$mode delphi}

interface

uses Windows, ActiveX, SysUtils;

type
  PFormatList = ^TFormatList;
  TFormatList = array [0..1] of TFormatEtc;

  TEnumFormatEtc = class (TInterfacedObject, IEnumFormatEtc)
  private
    FFormatList: PFormatList;
    FFormatCount: Integer;
    FIndex: Integer;
  public
    constructor Create(FormatList: PFormatList; FormatCount, Index: Integer);
    function Next(celt: Longword; out elt: tagFormatEtc; pceltFetched: PULong): Longint; stdcall;
    function Skip(celt: Longword): Longint; stdcall;
    function Reset: Longint; stdcall;
    function Clone(out enum: IEnumFormatEtc): Longint; stdcall;
  end;

implementation
//------------------------------------------------------------------------------
constructor TEnumFormatEtc.Create(FormatList: PFormatList; FormatCount, Index : Integer);
begin
  inherited Create;
  FFormatList := FormatList;
  FFormatCount := FormatCount;
  FIndex := Index;
end;
//------------------------------------------------------------------------------
function TEnumFormatEtc.Next(celt: Longword; out elt: tagFormatEtc; pceltFetched: PULong): Longint;
var
  i: Integer;
  eltout: TFormatList absolute elt;
begin
  i := 0;

  while (i < celt) and (FIndex < FFormatCount) do
  begin
    eltout[i] := FFormatList[FIndex];
    inc(FIndex);
    inc(i);
  end;

  if pceltFetched <> nil then pceltFetched^ := i;

  if i = celt then Result := S_OK else Result := S_FALSE;
end;
//------------------------------------------------------------------------------
function TEnumFormatEtc.Skip(celt: Longword): Longint;
begin
  if celt <= FFormatCount - FIndex then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end
  else
  begin
    FIndex := FFormatCount;
    Result := S_FALSE;
  end;
end;
//------------------------------------------------------------------------------
function TEnumFormatEtc.Reset: Longint;
begin
  FIndex := 0;
  Result := S_OK;
end;
//------------------------------------------------------------------------------
function TEnumFormatEtc.Clone(out enum: IEnumFormatEtc): Longint;
begin
  enum := TEnumFormatEtc.Create(FFormatList, FFormatCount, FIndex);
  Result := S_OK;
end;
//------------------------------------------------------------------------------
end.
