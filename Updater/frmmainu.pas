unit frmmainu;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Wininet;

type

  { Tfrmmain }

  Tfrmmain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
  public
    function DownloadURL(const aUrl: string; const aFile: string): boolean;
  end;

var
  frmmain: Tfrmmain;

const
  URL = 'https://drive.google.com/uc?export=download&id=';
  ID = '0Bxj8_7YelLhQUUJablJoeHJPSHc';

implementation
{$R *.lfm}
//------------------------------------------------------------------------------
procedure Tfrmmain.FormCreate(Sender: TObject);
begin
  if not DownloadURL(URL + ID, 'c:\terry\new_terry.exe') then
    messagebox(handle, 'Not downloaded', nil, 0);
end;
//------------------------------------------------------------------------------
function Tfrmmain.DownloadURL(const aUrl: string; const aFile: string): boolean;
const SIZE = 4096;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  buf: array [0..SIZE - 1] of char;
  dwBytesRead: dword;
  fs: TFileStream;
begin
  Result := false;
  hSession := InternetOpen('Updater', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if Assigned(hSession) then
    begin
      hService := InternetOpenUrl(hSession, pchar(aUrl), nil, 0, INTERNET_FLAG_RELOAD, 0);
      if Assigned(hService) then
      begin
        try
          fs := TFileStream.Create(aFile, fmCreate);
          while true do
          begin
            dwBytesRead := SIZE;
            InternetReadFile(hService, @buf, SIZE, dwBytesRead);
            if dwBytesRead = 0 then break;
            fs.Write(buf, dwBytesRead);
          end;
          fs.free;
          Result := true;
        finally
          InternetCloseHandle(hService);
        end;
      end;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;
//------------------------------------------------------------------------------
end.

