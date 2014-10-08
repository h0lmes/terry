unit multidocku;

{$mode delphi}
interface
uses Windows, Classes, SysUtils;

type
  TMultiDock = class
  private
    listWindows: TFPList;
    function GetWindowText(h: THandle): string;
  public
    class procedure CreateMD;
    class procedure DestroyMD;
    constructor Create;
    destructor Destroy; override;
    procedure Enum;
    procedure CloseAll;
  end;

var
  multidock: TMultiDock;

implementation
uses declu;
//------------------------------------------------------------------------------
class procedure TMultiDock.CreateMD;
begin
  if not assigned(multidock) then multidock := TMultiDock.Create;
end;
//------------------------------------------------------------------------------
class procedure TMultiDock.DestroyMD;
begin
  if assigned(multidock) then multidock.Free;
end;
//------------------------------------------------------------------------------
constructor TMultiDock.Create;
begin
  inherited Create;
  listWindows := TFPList.Create;
end;
//------------------------------------------------------------------------------
destructor TMultiDock.Destroy;
begin
  listWindows.free;
  inherited;
end;
//------------------------------------------------------------------------------
function TMultiDock.GetWindowText(h: THandle): string;
var
  win_name: array [0..255] of char;
begin
  windows.GetWindowText(h, @win_name[0], 255);
  result := strpas(pchar(@win_name[0]));
end;
//------------------------------------------------------------------------------
procedure TMultiDock.Enum;
var
  h: HANDLE;
begin
  listWindows.clear;
  h := FindWindow('Progman', nil);
  while h <> 0 do
  begin
    if GetWindowText(h) = PROGRAM_NAME then listWindows.add(pointer(h));
    h := GetWindow(h, GW_HWNDPREV);
  end;
end;
//------------------------------------------------------------------------------
procedure TMultiDock.CloseAll;
var
  i: integer;
begin
  i := 0;
  while i < listWindows.Count do
  begin
    postmessage(HANDLE(listWindows.items[i]), WM_COMMAND, IDM_QUIT, 0);
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
end.

