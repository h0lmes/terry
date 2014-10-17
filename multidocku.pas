unit multidocku;

{$mode delphi}
interface
uses Windows, Classes, SysUtils, Forms, declu;

type

  { TMultiDock }

  TMultiDock = class
  private
    listWindows: TFPList;
    listSites: TFPList;
    FCount: integer;
    FFreeMonitor: integer;
    FFreeSite: TBaseSite;
    FRemoveDock: boolean;
    FSetsFilename: string;
    ProgramPath: string;
    ProgramExe: string;
    function GetWindowText(h: THandle): string;
    procedure ReadSites;
    function GetNewSetsFilename: string;
  public
    property Count: integer read FCount;
    property FreeMonitor: integer read FFreeMonitor;
    property FreeSite: TBaseSite read FFreeSite;
    property RemoveDock: boolean read FRemoveDock;

    class procedure Create_;
    class procedure Destroy_;

    constructor Create;
    destructor Destroy; override;
    procedure Enum;
    procedure Close;
    function HaveFreeSite: boolean;
    procedure NewDock;
    procedure RunDock(ASetsFilename: string);
    procedure RequestRemoveDock(ASetsFilename: string);
  end;

var
  docks: TMultiDock;

implementation
uses toolu;
//------------------------------------------------------------------------------
class procedure TMultiDock.Create_;
begin
  if not assigned(docks) then docks := TMultiDock.Create;
end;
//------------------------------------------------------------------------------
class procedure TMultiDock.Destroy_;
begin
  if assigned(docks) then docks.Free;
end;
//------------------------------------------------------------------------------
constructor TMultiDock.Create;
begin
  inherited Create;
  ProgramExe := Paramstr(0);
  ProgramPath := IncludeTrailingPathDelimiter(ExtractFilePath(ProgramExe));
  listWindows := TFPList.Create;
  listSites := TFPList.Create;
  FCount := 0;
  FFreeMonitor := 0;
  FFreeSite := bsTop;
end;
//------------------------------------------------------------------------------
destructor TMultiDock.Destroy;
begin
  listWindows.free;
  listSites.free;

  if FRemoveDock then
  begin
    windows.DeleteFile(pchar(FSetsFilename));
    FSetsFilename := ChangeFileExt(FSetsFilename, '.bak');
    windows.DeleteFile(pchar(FSetsFilename));
  end;

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
  FCount := listWindows.Count;
end;
//------------------------------------------------------------------------------
procedure TMultiDock.ReadSites;
var
  i, m, s: integer;
  rDock, rMon: windows.TRect;
  ptDock, ptMon: windows.TPoint;
begin
  listSites.Clear;
  i := 0;
  while i < listWindows.Count do
  begin
    // dock center point
    GetWindowRect(HANDLE(listWindows.items[i]), rDock);
    ptDock.x := (rDock.Right + rDock.Left) div 2;
    ptDock.y := (rDock.Bottom + rDock.Top) div 2;
    // get monitor
    m := screen.MonitorFromPoint(ptDock).MonitorNum;
    // monitor center point
    rMon := screen.Monitors[m].BoundsRect;
    ptMon.x := (rMon.Right + rMon.Left) div 2;
    ptMon.y := (rMon.Bottom + rMon.Top) div 2;
    // get site
    if rDock.Bottom - rDock.Top < rDock.Right - rDock.Left then
    begin
      if ptDock.y > ptMon.y then s := integer(bsBottom) else s := integer(bsTop);
    end else begin
      if ptDock.x > ptMon.x then s := integer(bsRight) else s := integer(bsLeft);
    end;
    // add encoded position to list
    listSites.Add(pointer(s + m shl 8));
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
procedure TMultiDock.Close;
var
  idx: integer;
begin
  idx := 0;
  while idx < listWindows.Count do
  begin
    postmessage(HANDLE(listWindows.items[idx]), WM_COMMAND, IDM_QUIT, 0);
    inc(idx);
  end;
end;
//------------------------------------------------------------------------------
function TMultiDock.HaveFreeSite: boolean;
var
  m, s: integer;
begin
  result := false;
  ReadSites;

  for m := 0 to screen.MonitorCount - 1 do
    for s := 0 to 3 do
      if not result then
        if listSites.IndexOf(pointer(s + m shl 8)) < 0 then
        begin
          result := true;
          FFreeMonitor := m;
          FFreeSite := TBaseSite(s);
        end;
end;
//------------------------------------------------------------------------------
function TMultiDock.GetNewSetsFilename: string;
var
  idx: integer;
begin
  result := 'sets.ini';
  idx := 2;
  while FileExists(ProgramPath + result) and (idx <= 8) do
  begin
    result := 'sets' + inttostr(idx) + '.ini';
    inc(idx);
  end;
end;
//------------------------------------------------------------------------------
procedure TMultiDock.NewDock;
var
  newSets: string;
begin
  newSets := GetNewSetsFilename;
  WritePrivateProfileString('base', 'Monitor', pchar(inttostr(FFreeMonitor)), pchar(ProgramPath + newSets));
  WritePrivateProfileString('base', 'Site', pchar(SiteToString(FFreeSite)), pchar(ProgramPath + newSets));
  WritePrivateProfileString('base', 'Hello', '0', pchar(ProgramPath + newSets));
  RunDock(newSets);
end;
//------------------------------------------------------------------------------
procedure TMultiDock.RunDock(ASetsFilename: string);
begin
  ShellExecute(0, nil, pchar(ProgramExe), pchar('-i' + ASetsFilename), pchar(ProgramPath), SW_SHOWNORMAL);
end;
//------------------------------------------------------------------------------
procedure TMultiDock.RequestRemoveDock(ASetsFilename: string);
begin
  FRemoveDock := true;
  FSetsFilename := ASetsFilename;
end;
//------------------------------------------------------------------------------
end.

