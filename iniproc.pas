unit iniproc;

{$mode delphi}

interface
uses Windows, Classes, SysUtils;

function  ReadIniStringW(ini, section, key, default: WideString): WideString;
function  ReadIniIntW(ini, section, key: WideString; default: integer): integer; overload;
function  ReadIniIntW(ini, section, key: WideString; default, min, max: integer): integer; overload;
function  ReadIniBoolW(ini, section, key: WideString; default: boolean): boolean;
procedure WriteIniStringW(ini, section, key, value: WideString);

implementation
//------------------------------------------------------------------------------
function ReadIniStringW(ini, section, key, default: WideString): WideString;
var
  buf: array [0..2048] of WCHAR;
begin
  FillChar(buf, sizeof(buf), #0);
  GetPrivateProfileStringW(pwchar(section), pwchar(key), pwchar(default), pwchar(@buf), 2048, pwchar(ini));
  result := strpas(pwchar(@buf));
end;
//------------------------------------------------------------------------------
function ReadIniIntW(ini, section, key: WideString; default: integer): integer; overload;
begin
  result := StrToInt(ReadIniStringW(ini, section, key, inttostr(default)));
end;
//------------------------------------------------------------------------------
function ReadIniIntW(ini, section, key: WideString; default, min, max: integer): integer; overload;
begin
  result := StrToInt(ReadIniStringW(ini, section, key, inttostr(default)));
  if result < min then result := min;
  if result > max then result := max;
end;
//------------------------------------------------------------------------------
function ReadIniBoolW(ini, section, key: WideString; default: boolean): boolean;
begin
  result := not (ReadIniStringW(ini, section, key, inttostr(integer(default))) = '0');
end;
//------------------------------------------------------------------------------
procedure WriteIniStringW(ini, section, key, value: WideString);
begin
  WritePrivateProfileStringW(pwchar(section), pwchar(key), pwchar(value), pwchar(ini));
end;
//------------------------------------------------------------------------------
end.

