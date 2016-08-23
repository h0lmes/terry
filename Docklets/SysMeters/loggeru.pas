unit loggeru;

interface

uses Windows, SysUtils, Classes;

procedure SetLogFileName(LogFile: string);
procedure AddLog(LogString: string);
procedure AddLogBlob(Data: Pointer; Length: integer);
procedure TruncLog(fs: TFileStream);

var
  LogFileName: string = '';

implementation
//------------------------------------------------------------------------------
procedure SetLogFileName(LogFile: string);
begin
  LogFileName := LogFile;
end;
//------------------------------------------------------------------------------
procedure AddLogBlob(Data: Pointer; Length: integer);
var
  i: integer;
  bt: byte;
  str: string;
begin
  for i := 0 to Length do
  begin
      if i mod 16 = 0 then str := str + LineEnding;
      bt := pbyte(PtrUInt(Data) + i)^;
      str := str + inttohex(bt, 2) + ' ';
  end;
  Addlog(str);
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
    LogString := formatdatetime('yyMMdd-hhnnss', now) + '  ' + LogString + LineEnding;
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
procedure TruncLog(fs: TFileStream);
const
  LOG_SIZE_MAX = 1024 * 30; // 30 KB
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
end.

