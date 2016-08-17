unit iniproc;

{$mode delphi}

interface
uses Windows, Classes, SysUtils;

function GetIniStringW(ini, section, key, default: WideString): WideString;
function GetIniIntW(ini, section, key: WideString; default: integer): integer;
function GetIniBoolW(ini, section, key: WideString; default: boolean): boolean;
procedure WriteIniStringW(ini, section, key, value: WideString);

implementation
//------------------------------------------------------------------------------
function GetIniStringW(ini, section, key, default: WideString): WideString;
var
  buf: array [0..2048] of WCHAR;
begin
  FillChar(buf, sizeof(buf), #0);
  GetPrivateProfileStringW(pwchar(section), pwchar(key), pwchar(default), pwchar(@buf), 2048, pwchar(ini));
  result := strpas(pwchar(@buf));
end;
//------------------------------------------------------------------------------
function GetIniIntW(ini, section, key: WideString; default: integer): integer;
begin
  result := StrToInt(GetIniStringW(ini, section, key, inttostr(default)));
end;
//------------------------------------------------------------------------------
function GetIniBoolW(ini, section, key: WideString; default: boolean): boolean;
begin
  result := not (GetIniStringW(ini, section, key, inttostr(integer(default))) = '0');
end;
//------------------------------------------------------------------------------
procedure WriteIniStringW(ini, section, key, value: WideString);
begin
  WritePrivateProfileStringW(pwchar(section), pwchar(key), pwchar(value), pwchar(ini));
end;
//------------------------------------------------------------------------------
end.

