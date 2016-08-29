unit loggeru;

interface

uses jwaWindows, Windows, SysUtils, Classes;

// call SetLogFileName at program start before call to any other function
// example: loggeru.SetLogFileName(ChangeFileExt(ParamStr(0), '.log'));
procedure SetLogFileName(LogFile: string);

function GetLogFileName: string;

// log a string
procedure AddLog(LogString: string);

// log a binary data as a sequence of hex bytes
procedure AddLogBlob(Data: Pointer; Length: integer);

// log a binary data as a sequence of Char
procedure AddLogClob(Data: Pointer; Length: integer);

// log a binary data as a sequence of WideChar
procedure AddLogWClob(Data: Pointer; Length: integer);

// log a particular window data, incuding: handle, class, title, rect, module and props
procedure LogWindow(handle: HWND);

// log size limit
const LOG_SIZE_MAX = 1024 * 30; // 30 KB

var LogFileName: string = '';

implementation
//------------------------------------------------------------------------------
procedure SetLogFileName(LogFile: string);
begin
  LogFileName := LogFile;
end;
//------------------------------------------------------------------------------
function GetLogFileName: string;
begin
  result := LogFileName;
end;
//------------------------------------------------------------------------------
procedure AddLogBlob(Data: Pointer; Length: integer);
var
  i: integer;
  letter: byte;
  str: string;
begin
  for i := 0 to Length do
  begin
      if i mod 16 = 0 then str := str + LineEnding;
      letter := pbyte(PtrUInt(Data) + i)^;
      str := str + inttohex(letter, 2) + ' ';
  end;
  Addlog(str);
end;
//------------------------------------------------------------------------------
procedure AddLogClob(Data: Pointer; Length: integer);
var
  i: integer;
  letter: byte;
  str: string;
begin
  for i := 0 to Length do
  begin
      if i mod 16 = 0 then str := str + LineEnding;
      letter := pbyte(PtrUInt(Data) + i)^;
      str := str + chr(letter);
  end;
  Addlog(str);
end;
//------------------------------------------------------------------------------
procedure AddLogWClob(Data: Pointer; Length: integer);
var
  i: integer;
  letter: word;
  str: string;
begin
  for i := 0 to (Length - 1) div 2 do
  begin
      if i mod 16 = 0 then str := str + LineEnding;
      letter := pword(PtrUInt(Data) + i * 2)^;
      str := str + chr(letter);
  end;
  Addlog(str);
end;
//------------------------------------------------------------------------------
procedure TruncLog(fs: TFileStream);
var
  buf: char;
  TruncBy: integer;
  ms: TMemoryStream;
begin
  try
    // how many bytes to delete from the beginning of the stream
    TruncBy := fs.Size - LOG_SIZE_MAX;

    if TruncBy > 0 then
    begin
      // skip TruncBy bytes
      fs.Position := TruncBy;

      // skip bytes until end-of-line found
      fs.Read(buf, 1);
      inc(TruncBy);
      fs.Position := TruncBy;
      while (TruncBy < fs.Size) and (buf <> #10) and (buf <> #13) do
      begin
        fs.Read(buf, 1);
        inc(TruncBy);
        fs.Position := TruncBy;
      end;
      inc(TruncBy);
      fs.Position := TruncBy;
      TruncBy := fs.Size - TruncBy;

      // copy data to buffer stream
      ms := TMemoryStream.Create;
      ms.Size := TruncBy;
      ms.Position := 0;
      ms.CopyFrom(fs, TruncBy);
      ms.Position := 0;

      // copy buffer back to file
      fs.Size := TruncBy;
      fs.Position := 0;
      fs.CopyFrom(ms, TruncBy);

      ms.free;
    end;
  except
  end;
end;
//------------------------------------------------------------------------------
procedure AddLog(LogString: string);
var
  faccess: dword;
  PStr: PChar;
  LengthLogString: integer;
  fs: TFileStream;
begin
  if LogFileName <> '' then
  try
    // prepare log string
    LogString := formatdatetime('yyMMdd-hhnnss.zzz', now) + '  ' + LogString + LineEnding;
    LengthLogString := Length(LogString);
    PStr := StrAlloc(LengthLogString + 1);
    StrPCopy(PStr, LogString);

    // open log
    if FileExists(LogFileName) then faccess := fmOpenReadWrite else faccess := fmCreate;
    fs := TFileStream.Create(LogFileName, faccess);
    fs.Position := fs.Size;

    // write string
    fs.Write(PStr^, LengthLogString);
    StrDispose(PStr);

    // truncate file if needed
    TruncLog(fs);

    fs.Free;
  except
  end;
end;
//------------------------------------------------------------------------------
function EnumPropProc(hwnd: HWND; lpszString: LPSTR; hData: THandle; dwData: ULONG_PTR): BOOL; stdcall;
begin
  result := false;
  if hData <> 0 then
  begin
    AddLog(lpszString + ' = ' + inttohex(hData, 8));
    result := true;
  end;
end;
//------------------------------------------------------------------------------
procedure LogWindow(handle: HWND);
var
  rc: windows.TRect;
  cls: array [0..MAX_PATH - 1] of char;
begin
  AddLog('-');
  AddLog('Handle = ' + inttohex(handle, 8));
  FillChar(cls, MAX_PATH, #0);
  GetClassName(handle, cls, MAX_PATH);
  AddLog('Class = ' + strpas(@cls));
  FillChar(cls, MAX_PATH, #0);
  GetWindowText(handle, cls, MAX_PATH);
  AddLog('Text = ' + strpas(@cls));
  GetWindowRect(handle, rc);
  AddLog('Rect = ' + inttostr(rc.Left) + ', ' + inttostr(rc.Top) + ', ' + inttostr(rc.Right) + ', ' + inttostr(rc.Bottom));
  FillChar(cls, MAX_PATH, #0);
  GetWindowModuleFileName(handle, cls, MAX_PATH);
  AddLog('Module = ' + strpas(@cls));
  AddLog('Props:');
  try jwaWindows.EnumPropsEx(handle, @EnumPropProc, 0);
  except on e: Exception do AddLog(e.message);
  end;
  AddLog('-');
end;
//------------------------------------------------------------------------------
end.

