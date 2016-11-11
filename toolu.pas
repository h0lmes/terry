unit toolu;

interface

uses Windows, jwaWindows, Messages, SysUtils, Variants, Classes,
  Controls, Forms, Dialogs, ShellAPI, Registry, ComObj, ShlObj, ActiveX,
  MMDevApi_tlb, declu;

//function AddClipboardFormatListener(hWnd: HWND): BOOL; stdcall; external 'user32.dll';
//function RemoveClipboardFormatListener(hWnd: HWND): BOOL; stdcall; external 'user32.dll';

type
  KNOWNFOLDERID = TGuid;
  TSHGetKnownFolderPath = function(const rfid: KNOWNFOLDERID; dwFlags: DWord; hToken: THandle; var ppSzPath: LPWSTR) : HResult; stdcall;

const
  FOLDERID_AddNewPrograms: KNOWNFOLDERID = '{DE61D971-5EBC-4F02-A3A9-6C82895E5C04}';
  FOLDERID_AdminTools: KNOWNFOLDERID = '{724EF170-A42D-4FEF-9F26-B60E846FBA4F}';
  FOLDERID_AppUpdates: KNOWNFOLDERID = '{A305CE99-F527-492B-8B1A-7E76FA98D6E4}';
  FOLDERID_CDBurning: KNOWNFOLDERID = '{9E52AB10-F80D-49DF-ACB8-4330F5687855}';
  FOLDERID_ChangeRemovePrograms: KNOWNFOLDERID = '{DF7266AC-9274-4867-8D55-3BD661DE872D}';
  FOLDERID_CommonAdminTools: KNOWNFOLDERID = '{D0384E7D-BAC3-4797-8F14-CBA229B392B5}';
  FOLDERID_CommonOEMLinks: KNOWNFOLDERID = '{C1BAE2D0-10DF-4334-BEDD-7AA20B227A9D}';
  FOLDERID_CommonPrograms: KNOWNFOLDERID = '{0139D44E-6AFE-49F2-8690-3DAFCAE6FFB8}';
  FOLDERID_CommonStartMenu: KNOWNFOLDERID = '{A4115719-D62E-491D-AA7C-E74B8BE3B067}';
  FOLDERID_CommonStartup: KNOWNFOLDERID = '{82A5EA35-D9CD-47C5-9629-E15D2F714E6E}';
  FOLDERID_CommonTemplates: KNOWNFOLDERID = '{B94237E7-57AC-4347-9151-B08C6C32D1F7}';
  FOLDERID_ComputerFolder: KNOWNFOLDERID = '{0AC0837C-BBF8-452A-850D-79D08E667CA7}';
  FOLDERID_ConflictFolder: KNOWNFOLDERID = '{4BFEFB45-347D-4006-A5BE-AC0CB0567192}';
  FOLDERID_ConnectionsFolder: KNOWNFOLDERID = '{6F0CD92B-2E97-45D1-88FF-B0D186B8DEDD}';
  FOLDERID_Contacts: KNOWNFOLDERID = '{56784854-C6CB-462B-8169-88E350ACB882}';
  FOLDERID_ControlPanelFolder: KNOWNFOLDERID = '{82A74AEB-AEB4-465C-A014-D097EE346D63}';
  FOLDERID_Cookies: KNOWNFOLDERID = '{2B0F765D-C0E9-4171-908E-08A611B84FF6}';
  FOLDERID_Desktop: KNOWNFOLDERID = '{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}';
  FOLDERID_DeviceMetadataStore: KNOWNFOLDERID = '{5CE4A5E9-E4EB-479D-B89F-130C02886155}';
  FOLDERID_Documents: KNOWNFOLDERID = '{FDD39AD0-238F-46AF-ADB4-6C85480369C7}';
  FOLDERID_DocumentsLibrary: KNOWNFOLDERID = '{7B0DB17D-9CD2-4A93-9733-46CC89022E7C}';
  FOLDERID_Downloads: KNOWNFOLDERID = '{374DE290-123F-4565-9164-39C4925E467B}';
  FOLDERID_Favorites: KNOWNFOLDERID = '{1777F761-68AD-4D8A-87BD-30B759FA33DD}';
  FOLDERID_Fonts : KNOWNFOLDERID = '{FD228CB7-AE11-4AE3-864C-16F3910AB8FE}';
  FOLDERID_Games : KNOWNFOLDERID = '{CAC52C1A-B53D-4EDC-92D7-6B2E8AC19434}';
  FOLDERID_GameTasks : KNOWNFOLDERID = '{054FAE61-4DD8-4787-80B6-090220C4B700}';
  FOLDERID_History : KNOWNFOLDERID = '{D9DC8A3B-B784-432E-A781-5A1130A75963}';
  FOLDERID_HomeGroup : KNOWNFOLDERID = '{52528A6B-B9E3-4ADD-B60D-588C2DBA842D}';
  FOLDERID_ImplicitAppShortcuts : KNOWNFOLDERID = '{BCB5256F-79F6-4CEE-B725-DC34E402FD46}';
  FOLDERID_InternetCache : KNOWNFOLDERID = '{352481E8-33BE-4251-BA85-6007CAEDCF9D}';
  FOLDERID_InternetFolder : KNOWNFOLDERID = '{4D9F7874-4E0C-4904-967B-40B0D20C3E4B}';
  FOLDERID_Libraries : KNOWNFOLDERID = '{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';
  FOLDERID_Links : KNOWNFOLDERID = '{BFB9D5E0-C6A9-404C-B2B2-AE6DB6AF4968}';
  FOLDERID_LocalAppData : KNOWNFOLDERID = '{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}';
  FOLDERID_LocalAppDataLow : KNOWNFOLDERID = '{A520A1A4-1780-4FF6-BD18-167343C5AF16}';
  FOLDERID_LocalizedResourcesDir : KNOWNFOLDERID = '{2A00375E-224C-49DE-B8D1-440DF7EF3DDC}';
  FOLDERID_Music : KNOWNFOLDERID = '{4BD8D571-6D19-48D3-BE97-422220080E43}';
  FOLDERID_MusicLibrary : KNOWNFOLDERID = '{2112AB0A-C86A-4FFE-A368-0DE96E47012E}';
  FOLDERID_NetHood : KNOWNFOLDERID = '{C5ABBF53-E17F-4121-8900-86626FC2C973}';
  FOLDERID_NetworkFolder : KNOWNFOLDERID = '{D20BEEC4-5CA8-4905-AE3B-BF251EA09B53}';
  FOLDERID_OriginalImages : KNOWNFOLDERID = '{2C36C0AA-5812-4b87-BFD0-4CD0DFB19B39}';
  FOLDERID_PhotoAlbums : KNOWNFOLDERID = '{69D2CF90-FC33-4FB7-9A0C-EBB0F0FCB43C}';
  FOLDERID_PicturesLibrary : KNOWNFOLDERID = '{A990AE9F-A03B-4E80-94BC-9912D7504104}';
  FOLDERID_Pictures : KNOWNFOLDERID = '{33E28130-4E1E-4676-835A-98395C3BC3BB}';
  FOLDERID_Playlists : KNOWNFOLDERID = '{DE92C1C7-837F-4F69-A3BB-86E631204A23}';
  FOLDERID_PrintersFolder : KNOWNFOLDERID = '{76FC4E2D-D6AD-4519-A663-37BD56068185}';
  FOLDERID_PrintHood : KNOWNFOLDERID = '{9274BD8D-CFD1-41C3-B35E-B13F55A758F4}';
  FOLDERID_Profile : KNOWNFOLDERID = '{5E6C858F-0E22-4760-9AFE-EA3317B67173}';
  FOLDERID_ProgramData : KNOWNFOLDERID = '{62AB5D82-FDC1-4DC3-A9DD-070D1D495D97}';
  FOLDERID_ProgramFiles: KNOWNFOLDERID = '{905E63B6-C1BF-494E-B29C-65B732D3D21A}';
  FOLDERID_ProgramFilesX64: KNOWNFOLDERID = '{6D809377-6AF0-444b-8957-A3773F02200E}';
  FOLDERID_ProgramFilesX86: KNOWNFOLDERID = '{7C5A40EF-A0FB-4BFC-874A-C0F2E0B9FA8E}';
  FOLDERID_ProgramFilesCommon: KNOWNFOLDERID = '{F7F1ED05-9F6D-47A2-AAAE-29D317C6F066}';
  FOLDERID_ProgramFilesCommonX64: KNOWNFOLDERID = '{6365D5A7-0F0D-45E5-87F6-0DA56B6A4F7D}';
  FOLDERID_ProgramFilesCommonX86: KNOWNFOLDERID = '{DE974D24-D9C6-4D3E-BF91-F4455120B917}';
  FOLDERID_Programs : KNOWNFOLDERID = '{A77F5D77-2E2B-44C3-A6A2-ABA601054A51}';
  FOLDERID_Public : KNOWNFOLDERID = '{DFDF76A2-C82A-4D63-906A-5644AC457385}';
  FOLDERID_PublicDesktop : KNOWNFOLDERID = '{C4AA340D-F20F-4863-AFEF-F87EF2E6BA25}';
  FOLDERID_PublicDocuments : KNOWNFOLDERID = '{ED4824AF-DCE4-45A8-81E2-FC7965083634}';
  FOLDERID_PublicDownloads : KNOWNFOLDERID = '{3D644C9B-1FB8-4f30-9B45-F670235F79C0}';
  FOLDERID_PublicGameTasks : KNOWNFOLDERID = '{DEBF2536-E1A8-4c59-B6A2-414586476AEA}';
  FOLDERID_PublicLibraries : KNOWNFOLDERID = '{48DAF80B-E6CF-4F4E-B800-0E69D84EE384}';
  FOLDERID_PublicMusic : KNOWNFOLDERID = '{3214FAB5-9757-4298-BB61-92A9DEAA44FF}';
  FOLDERID_PublicPictures : KNOWNFOLDERID = '{B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5}';
  FOLDERID_PublicRingtones : KNOWNFOLDERID = '{E555AB60-153B-4D17-9F04-A5FE99FC15EC}';
  FOLDERID_PublicVideos : KNOWNFOLDERID = '{2400183A-6185-49FB-A2D8-4A392A602BA3}';
  FOLDERID_QuickLaunch : KNOWNFOLDERID = '{52A4F021-7B75-48A9-9F6B-4B87A210BC8F}';
  FOLDERID_Recent : KNOWNFOLDERID = '{AE50C081-EBD2-438A-8655-8A092E34987A}';
  FOLDERID_RecordedTVLibrary : KNOWNFOLDERID = '{1A6FDBA2-F42D-4358-A798-B74D745926C5}';
  FOLDERID_RecycleBinFolder : KNOWNFOLDERID = '{B7534046-3ECB-4C18-BE4E-64CD4CB7D6AC}';
  FOLDERID_ResourceDir : KNOWNFOLDERID = '{8AD10C31-2ADB-4296-A8F7-E4701232C972}';
  FOLDERID_Ringtones : KNOWNFOLDERID = '{C870044B-F49E-4126-A9C3-B52A1FF411E8}';
  FOLDERID_RoamingAppData : KNOWNFOLDERID = '{3EB685DB-65F9-4CF6-A03A-E3EF65729F3D}';
  FOLDERID_SampleMusic : KNOWNFOLDERID = '{B250C668-F57D-4EE1-A63C-290EE7D1AA1F}';
  FOLDERID_SamplePictures : KNOWNFOLDERID = '{C4900540-2379-4C75-844B-64E6FAF8716B}';
  FOLDERID_SamplePlaylists : KNOWNFOLDERID = '{15CA69B3-30EE-49C1-ACE1-6B5EC372AFB5}';
  FOLDERID_SampleVideos : KNOWNFOLDERID = '{859EAD94-2E85-48AD-A71A-0969CB56A6CD}';
  FOLDERID_SavedGames : KNOWNFOLDERID = '{4C5C32FF-BB9D-43b0-B5B4-2D72E54EAAA4}';
  FOLDERID_SavedSearches : KNOWNFOLDERID = '{7D1D3A04-DEBB-4115-95CF-2F29DA2920DA}';
  FOLDERID_SEARCH_CSC : KNOWNFOLDERID = '{EE32E446-31CA-4ABA-814F-A5EBD2FD6D5E}';
  FOLDERID_SEARCH_MAPI : KNOWNFOLDERID = '{98EC0E18-2098-4D44-8644-66979315A281}';
  FOLDERID_SearchHome : KNOWNFOLDERID = '{190337D1-B8CA-4121-A639-6D472D16972A}';
  FOLDERID_SendTo : KNOWNFOLDERID = '{8983036C-27C0-404B-8F08-102D10DCFD74}';
  FOLDERID_SidebarDefaultParts : KNOWNFOLDERID = '{7B396E54-9EC5-4300-BE0A-2482EBAE1A26}';
  FOLDERID_SidebarParts : KNOWNFOLDERID = '{A75D362E-50FC-4FB7-AC2C-A8BEAA314493}';
  FOLDERID_StartMenu : KNOWNFOLDERID = '{625B53C3-AB48-4EC1-BA1F-A1EF4146FC19}';
  FOLDERID_Startup : KNOWNFOLDERID = '{B97D20BB-F46A-4C97-BA10-5E3608430854}';
  FOLDERID_SyncManagerFolder : KNOWNFOLDERID = '{43668BF8-C14E-49B2-97C9-747784D784B7}';
  FOLDERID_SyncResultsFolder : KNOWNFOLDERID = '{289A9A43-BE44-4057-A41B-587A76D7E7F9}';
  FOLDERID_SyncSetupFolder : KNOWNFOLDERID = '{0F214138-B1D3-4a90-BBA9-27CBC0C5389A}';
  FOLDERID_System : KNOWNFOLDERID = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}';
  FOLDERID_SystemX86 : KNOWNFOLDERID = '{D65231B0-B2F1-4857-A4CE-A8E7C6EA7D27}';
  FOLDERID_Templates : KNOWNFOLDERID = '{A63293E8-664E-48DB-A079-DF759E0509F7}';
  FOLDERID_UserPinned : KNOWNFOLDERID = '{9E3995AB-1F9C-4F13-B827-48B24B6C7174}';
  FOLDERID_UserProfiles : KNOWNFOLDERID = '{0762D272-C50A-4BB0-A382-697DCD729B80}';
  FOLDERID_UserProgramFiles : KNOWNFOLDERID = '{5CD7AEE2-2219-4A67-B85D-6C9CE15660CB}';
  FOLDERID_UserProgramFilesCommon : KNOWNFOLDERID = '{BCBD3057-CA5C-4622-B42D-BC56DB0AE516}';
  FOLDERID_UsersFiles : KNOWNFOLDERID = '{F3CE0F7C-4901-4ACC-8648-D5D44B04EF8F}';
  FOLDERID_UsersLibraries : KNOWNFOLDERID = '{A302545D-DEFF-464b-ABE8-61C8648D939B}';
  FOLDERID_Videos : KNOWNFOLDERID = '{18989B1D-99B5-455B-841C-AB7C74E4DDFC}';
  FOLDERID_VideosLibrary : KNOWNFOLDERID = '{491E922F-5643-4AF4-A7EB-4E7A138D8174}';
  FOLDERID_Windows : KNOWNFOLDERID = '{F38BF404-1D43-42F2-9305-67DE0B28FC23}';
  KF_FLAG_NO_ALIAS = $1000;
  KF_FLAG_NO_APPCONTAINER_REDIRECTION = $10000;

function GetFont: string;
function GetFontSize: integer;
function cut(itext, ch: string): string;
function cutafter(itext, ch: string): string;
procedure split(itext, ch: string; var str1, str2: string);
procedure split_cmd(incmd: string; var cmd, params: string);
function ReplaceEx(strSrc, strWhat, strWith: string): string;
function fetch(var itext: string; delim: string; adelete: boolean = False): string;
function FetchValue(itext: string; Value, delim: string): string;
function PosEx(Value, atext: string; startpos: integer): integer;
function cuttolast(itext, ch: string): string;
function cutafterlast(itext, ch: string): string;
procedure RotatePoint(cx, cy: integer; theta: single; var pt: windows.TPoint);
procedure RotateRect(cx, cy: integer; theta: single; var rect: windows.TRect);
procedure RotateAndEmbedRect(cx, cy: integer; theta: single;
  source: windows.TRect; var result: windows.TRect; var bounds: windows.TRect);
function StringToRect(str: string): Windows.Trect;
function RectToString(r: Windows.Trect): string;
function StringToSize(str: string): Windows.TSize;
function SizeToString(r: Windows.TSize): string;
function StringToPoint(str: string): Windows.Tpoint;
function SetRange(value, min, max: integer): integer;
function IsDriveIdent(ident: string): boolean;
procedure qSortStrings(var list: TStrings);
procedure searchfiles(path, mask: string; list: TStrings);
procedure searchfolders(path: string; list: TStrings);
procedure searchfilesrecursive(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
function CheckAutoRun: boolean;
procedure SetAutoRun(enable: boolean);
procedure GetFileVersion(filename: string; var maj, min, Release, build: integer);
function GetEnvVar(VarName: string): string;
function FindFile(filename: string): string;
function FindFilePF(filename: string): string;
function UnzipPath(path: string): string;
function ZipPath(path: string): string;
function GetSystemDir: string;
function GetWinDir: string;
function GetSystemPath(path: string): string;
function GetKnownPath(rfid: KNOWNFOLDERID): WideString;
procedure setdisplaymode(x: integer = 800; y: integer = 600; bits: integer = 16; freq: integer = 60);
function GetLangID: integer;
function GetLangIDString(id: integer): string;
function GetLangIDName(id: integer): WideString;
function GetRecycleBinState: integer;
function ResolveLNK(wnd: HWND; var Target: WideString; out params, dir, icon: string): boolean;
procedure ResolveAppref(wnd: HWND; var Target: WideString);
function BrowseFolder(hWnd: THandle; title, default: string): string;
procedure SetClipboard(Text: string);
function GetClipboard: string;
function ColorToString(Color: uint): string;
function StringToColor(const str: string): uint;
function confirm(handle: HWND; Text: WideString = ''): boolean;
function FindWinamp: HWND;
function LaunchWinamp(sw: integer = sw_shownormal): boolean;
function wacmd(cmd: HANDLE): boolean;
procedure bsm(msg: UINT; wparam: WPARAM; lparam: LPARAM);
function IsIdenticalStreams(Source, Destination: TStream): boolean;
procedure SendShift(hwnd: HWnd; Down: Boolean);
procedure SendCtrl(hwnd: HWnd; Down: Boolean);
procedure SendKey(hwnd: HWnd; Key: char; noChar: boolean);

var
  ShGetKnownFolderPath: TShGetKnownFolderPath;
  bIsWindowsVistaOrHigher: boolean;
  bIsWow64: boolean;
  ScalingFactor: integer;

implementation
//------------------------------------------------------------------------------
function GetFont: string;
begin
  Result := 'tahoma';
  try
    if bIsWindowsVistaOrHigher then Result := 'segoe ui';
  except
  end;
end;
//------------------------------------------------------------------------------
function GetFontSize: integer;
begin
  Result := 8;
  try
    if bIsWindowsVistaOrHigher then Result := 9;
  except
  end;
  if ScalingFactor > 100 then Result := Result * ScalingFactor div 100;
end;
//------------------------------------------------------------------------------
function cut(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos := pos(LowerCase(ch), LowerCase(itext));
  if ipos > 0 then
    Result := copy(itext, 1, ipos - 1)
  else
    Result := itext;
end;
//------------------------------------------------------------------------------
function cutafter(itext, ch: string): string;
var
  ipos: integer;
begin
  ipos := pos(LowerCase(ch), LowerCase(itext));
  if ipos > 0 then
    Result := copy(itext, ipos + length(ch), length(itext))
  else
    Result := '';
end;
//------------------------------------------------------------------------------
procedure split(itext, ch: string; var str1, str2: string);
var
  ipos: integer;
begin
  ipos := pos(LowerCase(ch), LowerCase(itext));
  if ipos > 0 then
  begin
    str1 := copy(itext, 1, ipos - 1);
    str2 := copy(itext, ipos + length(ch), length(itext));
  end
  else
  begin
    str1 := itext;
    str2 := '';
  end;
end;
//------------------------------------------------------------------------------
procedure split_cmd(incmd: string; var cmd, params: string);
var
  ipos: integer;
begin
  if cmd[1] = '"' then
  begin

    ipos := posex('"', incmd, 2);
    if ipos > 0 then
    begin
      cmd := copy(incmd, 2, ipos - 2);
      params := copy(incmd, ipos + 1, length(incmd));
    end
    else
    begin
      cmd := incmd;
      params := '';
    end;

  end
  else
  begin

    ipos := pos(' ', incmd);
    if ipos > 0 then
    begin
      cmd := copy(incmd, 1, ipos - 1);
      params := copy(incmd, ipos + 1, length(incmd));
    end
    else
    begin
      cmd := incmd;
      params := '';
    end;

  end;
end;
//------------------------------------------------------------------------------
function ReplaceEx(strSrc, strWhat, strWith: string): string;
var
  ipos: integer;
begin
  ipos := pos(LowerCase(strWhat), LowerCase(strSrc));
  while ipos > 0 do
  begin
    strSrc := copy(strSrc, 1, ipos - 1) + strWith + copy(strSrc, ipos + length(strWhat), length(strSrc));
    ipos := pos(LowerCase(strWhat), LowerCase(strSrc));
  end;
  Result := strSrc;
end;
//------------------------------------------------------------------------------
function fetch(var itext: string; delim: string; adelete: boolean = False): string;
var
  ipos: integer;
begin
  ipos := pos(LowerCase(delim), LowerCase(itext));
  if ipos > 0 then
  begin
    Result := system.copy(itext, 1, ipos - 1);
    if adelete then
      system.Delete(itext, 1, ipos - 1 + length(delim));
  end
  else
  begin
    Result := itext;
    itext := '';
  end;
end;
//------------------------------------------------------------------------------
function FetchValue(itext: string; Value, delim: string): string;
var
  ipos, ipos2: integer;
begin
  ipos := pos(LowerCase(Value), LowerCase(itext));
  if ipos > 0 then
  begin
    ipos2 := posex(delim, itext, ipos + length(Value));
    Result := system.copy(itext, ipos + length(Value), ipos2 - ipos - length(Value));
  end
  else
    Result := '';
end;
//------------------------------------------------------------------------------
function PosEx(Value, atext: string; startpos: integer): integer;
begin
  Result := startpos;
  if Value = '' then exit;

  while Result <= length(atext) do
  begin
    if LowerCase(atext[Result]) = LowerCase(Value[1]) then
      if LowerCase(copy(atext, Result, length(Value))) = LowerCase(Value) then
        exit;
    Inc(Result);
  end;
end;
//------------------------------------------------------------------------------
function cuttolast(itext, ch: string): string;
var
  i, len: integer;
begin
  Result := '';
  if itext = '' then
    exit;

  i := length(itext);
  len := length(ch);
  while i > 0 do
  begin
    if LowerCase(copy(itext, i, len)) = LowerCase(ch) then
    begin
      Result := copy(itext, 1, i - 1);
      exit;
    end;
    Dec(i);
  end;
  Result := itext;
end;
//------------------------------------------------------------------------------
function cutafterlast(itext, ch: string): string;
var
  i, ilen, len: integer;
begin
  Result := '';
  if itext = '' then
    exit;

  ilen := length(itext);
  i := ilen;
  len := length(ch);
  while i > 0 do
  begin
    if LowerCase(copy(itext, i, len)) = LowerCase(ch) then
    begin
      Result := copy(itext, i + len, ilen);
      exit;
    end;
    Dec(i);
  end;
  Result := itext;
end;
//------------------------------------------------------------------------------
// cx, cy - rotation center
// theta in radians
procedure RotatePoint(cx, cy: integer; theta: single; var pt: windows.TPoint);
var
  tempX, tempY: single;
begin
  tempX := pt.x - cx;
  tempY := pt.y - cy;
  pt.x := round(cx + tempX * cos(theta) - tempY * sin(theta));
  pt.y := round(cy + tempX * sin(theta) + tempY * cos(theta));
end;
//------------------------------------------------------------------------------
// cx, cy - rotation center
// theta in radians
procedure RotateRect(cx, cy: integer; theta: single; var rect: windows.TRect);
var
  topLeft, topRight, bottomLeft, bottomRight: windows.TPoint;
begin
  topLeft := rect.TopLeft;
  topRight.x := rect.Right;
  topRight.y := rect.Top;
  bottomRight := rect.BottomRight;
  bottomLeft.x := rect.Left;
  bottomLeft.y := rect.Bottom;
  RotatePoint(cx, cy, theta, topLeft);
  RotatePoint(cx, cy, theta, topRight);
  RotatePoint(cx, cy, theta, bottomRight);
  RotatePoint(cx, cy, theta, bottomLeft);
  rect.Left := min(topLeft.x, min(topRight.x, min(bottomRight.x, bottomLeft.x)));
  rect.Top := min(topLeft.y, min(topRight.y, min(bottomRight.y, bottomLeft.y)));
  rect.Right := max(topLeft.x, max(topRight.x, max(bottomRight.x, bottomLeft.x)));
  rect.Bottom := max(topLeft.y, max(topRight.y, max(bottomRight.y, bottomLeft.y)));
end;
//------------------------------------------------------------------------------
// cx, cy - rotation center
// theta in radians
procedure RotateAndEmbedRect(cx, cy: integer; theta: single;
  source: windows.TRect; var result: windows.TRect; var bounds: windows.TRect);
var
  topLeft, topRight, bottomLeft, bottomRight: windows.TPoint;
begin
  topLeft      := source.TopLeft;
  topRight.x   := source.Right;
  topRight.y   := source.Top;
  bottomRight  := source.BottomRight;
  bottomLeft.x := source.Left;
  bottomLeft.y := source.Bottom;

  RotatePoint(cx, cy, theta, topLeft);
  RotatePoint(cx, cy, theta, topRight);
  RotatePoint(cx, cy, theta, bottomRight);
  RotatePoint(cx, cy, theta, bottomLeft);

  bounds.Left   := min(topLeft.x, min(topRight.x, min(bottomRight.x, bottomLeft.x)));
  bounds.Top    := min(topLeft.y, min(topRight.y, min(bottomRight.y, bottomLeft.y)));
  bounds.Right  := max(topLeft.x, max(topRight.x, max(bottomRight.x, bottomLeft.x)));
  bounds.Bottom := max(topLeft.y, max(topRight.y, max(bottomRight.y, bottomLeft.y)));

  result := source;
  if bounds.Left < 0 then
  begin
    inc(result.Left, -bounds.Left);
    inc(result.Right, -bounds.Left);
    inc(bounds.Right, -bounds.Left);
    bounds.Left := 0;
  end;
  if bounds.Top < 0 then
  begin
    inc(result.Top, -bounds.Top);
    inc(result.Bottom, -bounds.Top);
    inc(bounds.Bottom, -bounds.Top);
    bounds.Top := 0;
  end;
end;
//------------------------------------------------------------------------------
function StringToRect(str: string): Windows.Trect;
begin
  Result := rect(0, 0, 0, 0);
  try Result.left := StrToInt(trim(fetch(str, ',', True)));
  except end;
  try Result.top := StrToInt(trim(fetch(str, ',', True)));
  except end;
  try Result.right := StrToInt(trim(fetch(str, ',', True)));
  except end;
  try Result.bottom := StrToInt(trim(fetch(str, ')')));
  except end;
end;
//------------------------------------------------------------------------------
function RectToString(r: Windows.Trect): string;
begin
  Result := IntToStr(r.left) + ',' + IntToStr(r.top) + ',' + IntToStr(r.right) + ',' + IntToStr(r.bottom);
end;
//------------------------------------------------------------------------------
function StringToSize(str: string): Windows.TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  try
    Result.cx := StrToInt(trim(cut(str, ',')));
    Result.cy := StrToInt(trim(cutafter(str, ',')));
  except end;
end;
//------------------------------------------------------------------------------
function SizeToString(r: Windows.TSize): string;
begin
  Result := IntToStr(r.cx) + ',' + IntToStr(r.cy);
end;
//------------------------------------------------------------------------------
function StringToPoint(str: string): Windows.Tpoint;
begin
  Result := point(0, 0);
  try
    Result.x := StrToInt(trim(cut(str, ',')));
    Result.y := StrToInt(trim(cutafter(str, ',')));
  except end;
end;
//------------------------------------------------------------------------------
function SetRange(value, min, max: integer): integer;
begin
  if value < min then value := min;
  if value > max then value := max;
  result := value;
end;
//------------------------------------------------------------------------------
function IsDriveIdent(ident: string): boolean;
begin
  Result := False;
  if (length(ident) < 2) or (length(ident) > 3) then exit;
  Result := (Ord(ident[1]) > 64) and (Ord(ident[1]) < 117);
  if length(ident) = 2 then Result := Result and (ident[2] = ':');
  if length(ident) = 3 then Result := Result and (ident[2] = ':') and (ident[3] = '\');
end;
//------------------------------------------------------------------------------
procedure qSortStrings(var list: TStrings);
  procedure sort(var list: TStrings; low, high: integer);
  var
    i, j: integer;
    median, temp: string;
  begin
    i := low;
    j := high;
    median := AnsiUpperCase(ExtractFileName(list.Strings[(i+j) div 2]));
    repeat
      while AnsiUpperCase(ExtractFileName(list.Strings[i])) < median do inc(i);
      while AnsiUpperCase(ExtractFileName(list.Strings[j])) > median do dec(j);
      if i <= j then
      begin
        temp := list.Strings[i];
        list.Strings[i] := list.Strings[j];
        list.Strings[j] := temp;
        inc(i);
        dec(j);
      end;
    until i > j;

    if low < j then sort(list, low, j);
    if i < high then sort(list, i, high);
  end;
begin
  if list.Count > 0 then sort(list, 0, list.Count - 1);
end;
//------------------------------------------------------------------------------
procedure searchfiles(path, mask: string; list: TStrings);
var
  fhandle: THandle;
  f: TWin32FindData;
begin
  list.Clear;
  path := IncludeTrailingPathDelimiter(path);
  fhandle := FindFirstFile(PChar(path + mask), f);
  if fhandle = INVALID_HANDLE_VALUE then exit;
  if (f.dwFileAttributes and $18) = 0 then list.addobject(f.cFileName, tobject(0));
  while FindNextFile(fhandle, f) do
    if (f.dwFileAttributes and $18) = 0 then list.addobject(f.cFileName, tobject(0));
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
procedure searchfolders(path: string; list: TStrings);
var
  fhandle: THandle;
  filename: string;
  f: TWin32FindData;
begin
  list.Clear;
  path := IncludeTrailingPathDelimiter(path);
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') then
      list.addobject(filename, tobject(0));
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') then
        list.addobject(filename, tobject(0));
    end;
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
procedure searchfilesrecursive(path, mask: string; list: TStrings;
  level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
var
  fhandle: THandle;
  filename: string;
  f: TWin32FindData;
begin
  if level = 0 then list.Clear;
  path := IncludeTrailingPathDelimiter(path);

  // folders //
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') and (level < maxlevel) then
      searchfilesrecursive(path + filename, mask, list, level + 1);
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and (filename <> '..') and (level < maxlevel) then
        searchfilesrecursive(path + filename, mask, list, level + 1, maxlevel);
    end;
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);

  // files //
  fhandle := FindFirstFile(PChar(path + mask), f);
  if not (fhandle = INVALID_HANDLE_VALUE) then
  begin
    if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then list.addobject(path + f.cFileName, tobject(0));
    while FindNextFile(fhandle, f) do
      if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then list.addobject(path + f.cFileName, tobject(0));
  end;
  if not (fhandle = INVALID_HANDLE_VALUE) then Windows.FindClose(fhandle);
end;
//------------------------------------------------------------------------------
function CheckAutorun: boolean;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('Software\\Microsoft\\Windows\\CurrentVersion\\Run', false) then
    begin
      if reg.ValueExists(UTF8ToAnsi(PROGRAM_REGKEY)) then
         result := reg.ReadString(UTF8ToAnsi(PROGRAM_REGKEY)) = ParamStr(0);
    end else begin
      raise Exception.Create('CheckAutorun.OpenKey failed');
    end;
  finally
    reg.free;
  end;
end;
//----------------------------------------------------------------------
procedure SetAutorun(enable: boolean);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_ALL_ACCESS);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    if reg.OpenKey('Software\\Microsoft\\Windows\\CurrentVersion\\Run', false) then
    begin
      if enable then reg.WriteString(UTF8ToAnsi(PROGRAM_REGKEY), ParamStr(0))
      else reg.WriteString(UTF8ToAnsi(PROGRAM_REGKEY), '');
    end else begin
      raise Exception.Create('SetAutorun.OpenKey failed');
    end;
  finally
    reg.free;
  end;
end;
//----------------------------------------------------------------------
procedure GetFileVersion(filename: string; var maj, min, Release, build: integer);
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  Tmp: DWORD;
begin
  maj := 0;
  min := 0;
  Release := 0;
  build := 0;

  filename := UnzipPath(filename);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Tmp);
  if InfoSize <> 0 then
  begin
    GetMem(Info, InfoSize);
    try
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      maj := FileInfo.dwFileVersionMS shr 16;
      min := FileInfo.dwFileVersionMS and $FFFF;
      Release := FileInfo.dwFileVersionLS shr 16;
      build := FileInfo.dwFileVersionLS and $FFFF;
    finally
      FreeMem(Info, FileInfoSize);
    end;
  end;
end;
//------------------------------------------------------------------------------
function GetEnvVar(VarName: string): string;
var
  idx: integer;
begin
  Result := '';
  try
    idx := Windows.GetEnvironmentVariable(PChar(VarName), nil, 0);
    if idx > 0 then
    begin
      SetLength(Result, idx);
      Windows.GetEnvironmentVariable(PChar(VarName), PChar(Result), idx);
    end;
  except
  end;
end;
//------------------------------------------------------------------------------
function FindFile(filename: string): string;
var
  PathVar, ExtVar, TempExtVar, Path, Ext: string;
  HaveExt: boolean;
begin
  Result := filename;
  if fileexists(filename) then exit;

  // search evironment vars //
  PathVar := GetEnvVar('path');
  HaveExt := ExtractFileExt(filename) <> '';
  if not HaveExt then ExtVar := LowerCase(GetEnvVar('pathext'));
  while PathVar <> '' do
  begin
    Path := IncludeTrailingPathDelimiter(fetch(PathVar, ';', True));
    if HaveExt then
    begin
      Result := Path + filename;
      if fileexists(Result) then exit;
    end
    else
    begin
      TempExtVar := ExtVar;
      while TempExtVar <> '' do
      begin
        Ext := fetch(TempExtVar, ';', True);
        Result := Path + filename + Ext;
        if fileexists(Result) then exit;
      end;
    end;
  end;
  Result := filename;
end;
//------------------------------------------------------------------------------
function FindFilePF(filename: string): string;
var
  list: TStrings;
begin
  Result := filename;
  if fileexists(filename) then exit;
  list := TStringList.Create;
  searchfilesrecursive(UnzipPath('%pf%'), filename + '.exe', list, 1, 3, 1);
  searchfilesrecursive(UnzipPath('%pfx86%'), filename + '.exe', list, 1, 3, 1);
  if list.Count > 0 then Result := list[0];
  list.Free;
end;
//------------------------------------------------------------------------------
function UnzipPath(path: string): string;
var
  pp, windir: string;
begin
  Result := path;
  if trim(path) = '' then exit;

  if length(Result) > 3 then
    if (Result[2] = ':') and (Result[3] = '\') then
      if fileexists(Result) or directoryexists(Result) then exit;

  pp := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  if fileexists(pp + '\' + Result) then
  begin
    Result := pp + '\' + Result;
    exit;
  end;

  // path vars //
  windir := GetWinDir;
  Result := ReplaceEx(Result, '%pp%', pp);
  Result := ReplaceEx(Result, '%windir%', windir);
  Result := ReplaceEx(Result, '%systemroot%', windir);
  if pos('%sysdir%', Result) > 0 then Result := ReplaceEx(Result, '%sysdir%', getsystemdir);
  if bIsWindowsVistaOrHigher then
  begin
    if pos('%doc%', Result) > 0 then Result := ReplaceEx(Result, '%doc%', getKnownPath(FOLDERID_Documents));
    if pos('%appdata%', Result) > 0 then Result := ReplaceEx(Result, '%appdata%', getKnownPath(FOLDERID_RoamingAppData));
    if pos('%desktop%', Result) > 0 then Result := ReplaceEx(Result, '%desktop%', getKnownPath(FOLDERID_Desktop));
    if pos('%userprofile%', Result) > 0 then Result := ReplaceEx(Result, '%userprofile%', getKnownPath(FOLDERID_Profile));
    if pos('%startmenu%', Result) > 0 then Result := ReplaceEx(Result, '%startmenu%', getKnownPath(FOLDERID_StartMenu));
    if pos('%commonstartmenu%', Result) > 0 then Result := ReplaceEx(Result, '%commonstartmenu%', getKnownPath(FOLDERID_CommonStartMenu));
  end else begin
    if pos('%doc%', Result) > 0 then Result := ReplaceEx(Result, '%doc%', GetSystemPath('personal'));
    if pos('%desktop%', Result) > 0 then Result := ReplaceEx(Result, '%desktop%', GetSystemPath('desktop'));
  end;
  Result := ReplaceEx(Result, '%pfx86%', windir[1] + ':\Program Files (x86)');
  Result := ReplaceEx(Result, '%pf%', windir[1] + ':\Program Files');
  Result := ReplaceEx(Result, '%programfiles%', windir[1] + ':\Program Files');

  // non-path vars //
  Result := ReplaceEx(Result, '%date%', formatdatetime('dddddd', now));
  Result := ReplaceEx(Result, '%time%', formatdatetime('tt', now));
end;
//------------------------------------------------------------------------------
function ZipPath(path: string): string;
var
  windir: string;
begin
  windir := getwindir;
  path := ReplaceEx(path, IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))), '');
  path := ReplaceEx(path, getsystemdir, '%sysdir%');
  path := ReplaceEx(path, windir, '%windir%');
  if bIsWindowsVistaOrHigher then
  begin
    path := ReplaceEx(path, getKnownPath(FOLDERID_Documents), '%doc%');
    path := ReplaceEx(path, getKnownPath(FOLDERID_Desktop), '%desktop%');
    path := ReplaceEx(path, getKnownPath(FOLDERID_Profile), '%userprofile%');
    path := ReplaceEx(path, getKnownPath(FOLDERID_RoamingAppData), '%appdata%');
  end else begin
    path := ReplaceEx(path, GetSystemPath('personal'), '%doc%');
    path := ReplaceEx(path, GetSystemPath('desktop'), '%desktop%');
  end;
  path := ReplaceEx(path, windir[1] + ':\program files (x86)', '%pfx86%');
  path := ReplaceEx(path, windir[1] + ':\program files', '%pf%');
  Result := path;
end;
//----------------------------------------------------------------------
function GetSystemDir: string;
var
  SysDir: array [0..MAX_PATH - 1] of char;
begin
  SetString(Result, SysDir, GetSystemDirectory(SysDir, MAX_PATH));
  Result := ExcludeTrailingPathDelimiter(Result);
end;
//----------------------------------------------------------------------
function GetWinDir: string;
var
  WinDir: array [0..MAX_PATH - 1] of char;
begin
  SetString(Result, WinDir, GetWindowsDirectory(WinDir, MAX_PATH));
  Result := ExcludeTrailingPathDelimiter(Result);
end;
//------------------------------------------------------------------------------
function GetSystemPath(path: string): string;
var
  reg: TRegIniFile;
begin
  reg := TRegIniFile.Create;
  if pos('common', path) > 0 then reg.RootKey := hkey_local_machine else reg.RootKey := hkey_current_user;
  Result := ExcludeTrailingPathDelimiter(reg.ReadString('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', path, ''));
  reg.Free;
end;
//------------------------------------------------------------------------------
function GetKnownPath(rfid: KNOWNFOLDERID): WideString;
var
  hShell32: HMODULE;
  buffer: LPWSTR;
begin
  result := '';
  if not assigned(ShGetKnownFolderPath) then
  begin
    hShell32 := GetModuleHandle('shell32.dll');
    if hShell32 <> 0 then
      @ShGetKnownFolderPath := GetProcAddress(hShell32, 'SHGetKnownFolderPath');
  end;
  if assigned(ShGetKnownFolderPath) then
  begin
    buffer := nil;
    if S_OK = ShGetKnownFolderPath(rfid, 0, 0, buffer) then
    begin
      try result := buffer;
      finally CoTaskMemFree(buffer);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure setdisplaymode(x: integer = 800; y: integer = 600; bits: integer = 16; freq: integer = 60);
var
  devmode: Windows.TDeviceMode;
begin
  devmode.dmSize := SizeOf(devmode);
  devmode.dmBitsPerPel := bits;
  devmode.dmPelsWidth := x;
  devmode.dmPelsHeight := y;
  devmode.dmDisplayFrequency := freq;
  devmode.dmFields := DM_BITSPERPEL + DM_PELSWIDTH + DM_PELSHEIGHT + DM_DISPLAYFREQUENCY;
  Windows.ChangeDisplaySettings(DevMode, CDS_UPDATEREGISTRY);
  SendMessage(HWND_BROADCAST, WM_DISPLAYCHANGE, SPI_SETNONCLIENTMETRICS, 0);
end;
//------------------------------------------------------------------------------
function GetLangID: integer;
var
  procID: dword;
begin
  result := 0;
  try result := $ffff and GetKeyboardLayout(GetWindowThreadProcessId(GetForegroundWindow(), @procID));
  except end;
end;
//------------------------------------------------------------------------------
function GetLangIDString(id: integer): string;
begin
  result := '--';
  case id of
    $0419: result := 'RU'; // ru-RU,  Russia
    $046d: result := 'RU'; // ba-RU,  Russia, Bashkir
    $0444: result := 'RU'; // tt-RU,  Russia, Tatar
    $0485: result := 'RU'; // sah-RU, Russia, Sakha
    $0423: result := 'BY'; // be-BY,  Belorussia
    $0422: result := 'UA'; // uk-UA,  Ukraine, Ukrainian
    $0409: result := 'EN'; // en-US,  USA, English
    $0809: result := 'GB'; //         United Kingdom
    $0C0C: result := 'CA'; //         Canada
    $1009: result := 'CA'; //         Canada
    $0C09: result := 'AU'; // en-AU,  Australia, English
    $0407: result := 'DE'; //         Germany
    $0406: result := 'DK'; // da-DK,  Denmark, Danish
    $0813: result := 'BE'; // nl-BE,  Belgium, Dutch
    $080c: result := 'BE'; // fr-BE,  Belgium, French
    $0410: result := 'IT'; // it-IT,  Italy
    $040c: result := 'FR'; //         France
    $047e: result := 'FR'; // br-FR
    $0483: result := 'FR'; // co-FR,  France, Corsican
    $0403: result := 'ES'; // ca-ES
    $0C0A: result := 'ES'; //         Spain, Modern Sort
    $040A: result := 'ES'; //         Spain, Traditional Sort
    $0405: result := 'CZ'; // cs-CZ,  Czech Republic
    $043b: result := 'NO'; // Norway
    $103b: result := 'NO'; // smj-NO, Norway, Sami
    $040B: result := 'FI'; // Finland
    $0425: result := 'EE'; // et-EE,  Estonian
    $0C07: result := 'AT'; // de-AT,  Austria, German
    $0807: result := 'CH'; //         Switzerland
    $0810: result := 'CH'; //         Switzerland, Italian Swiss
    $0417: result := 'CH'; // rm-CH,  Switzerland, Romansh
    $0408: result := 'GR'; // el-GR,  Greece
    $0418: result := 'RO'; // ro-RO,  Romania, Romanian
    $0426: result := 'LV'; // Latvia
    $0427: result := 'LT'; // Lithuania
    $0415: result := 'PL'; // pl-PL,  Poland
    $0416: result := 'BR'; // pt-BR,  Brazil, Portuguese
    $0816: result := 'PT'; //         Portugal
    $041B: result := 'SK'; // sk-SK,  Slovakia
    $0424: result := 'SI'; // sl-SI,  Slovenia
    $041F: result := 'TR'; // tr-TR,  Turkey, Turkish
    $042b: result := 'AM'; // hy-AM,  Armenia
    $044D: result := 'IN'; // as-IN,  India, Assamese
    $4009: result := 'IN'; //         India, English
    $0447: result := 'IN'; // gu-IN,  India, Gujarati
    $0439: result := 'IN'; // hi-IN,  India, Hindi
    $044B: result := 'IN'; // kn-IN,  India, Kannada
    $082C: result := 'AZ'; // az-AZ,  Azerbaijan, Cyrillic
    $042C: result := 'AZ'; // AZ,     Azerbaijan, Latin
    $201A: result := 'BA'; // bs-BA,  Bosnia and Herzegovina, Cyrillic
    $141A: result := 'BA'; // BA,     Bosnia and Herzegovina, Latin
    $101A: result := 'BA'; // hr-BA,  Bosnia and Herzegovina, Croatian
    $0C04: result := 'HK'; // zh-HK,  Hong Kong SAR, PRC
    $2801: result := 'SY'; //         Syria
    $1801: result := 'MA'; //         Morocco
    $0401: result := 'SA'; //         Saudi Arabia
    $3801: result := 'AE'; //         UAE
    $1004: result := 'SG'; //         Singapore
    $4809: result := 'SG'; //         Singapore
    $0411: result := 'JP'; // ja-JP,  Japan
    $0004: result := 'zh-Ha'; // zh-Hans, Chinese, Simplified
    $7C04: result := 'zh-Ha'; // zh-Hant, Chinese, Traditional
  end;
end;
//------------------------------------------------------------------------------
function GetLangIDName(id: integer): WideString;
const
  MAX_LANG_LEN = 85;
var
  buffer: array [0..MAX_LANG_LEN] of wchar;
begin
  result := '';
  if GetLocaleInfoW(MAKELCID(id, SORT_DEFAULT), LOCALE_SLANGUAGE, buffer, MAX_LANG_LEN) > 0 then
    result := pwchar(buffer);
end;
//------------------------------------------------------------------------------
// returns 1 for full bin, 0 for empty bin
function GetRecycleBinState: integer;
var
  psfDesktop: IShellFolder;
  psfFolder: IShellFolder;
  pidFolder, pidChild: PItemIDList;
  pEnumList: IEnumIDList;
  celtFetched: ULONG;
begin
  OleCheck(SHGetDesktopFolder(psfDesktop));
  OleCheck(SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET or CSIDL_FLAG_NO_ALIAS, pidFolder));
  OleCheck(psfDesktop.BindToObject(pidFolder, nil, IID_IShellFolder, psfFolder));
  OleCheck(psfFolder.EnumObjects(0, SHCONTF_NONFOLDERS or SHCONTF_FOLDERS, pEnumList));
  result := 0;
  if pEnumList.Next(1, pidChild, celtFetched) = NOERROR then
  begin
    inc(result);
    ILFree(pidChild);
  end;
  ILFree(pidFolder);
end;
//------------------------------------------------------------------------------
function ResolveLNK(wnd: HWND; var Target: WideString; out params, dir, icon: string): boolean;
var
  obj: IUnknown;
  isl: IShellLink;
  ipf: IPersistFile;
  fda: Windows.TWin32FindData;
  s: string;
  iIcon: integer;
begin
  result := false;
  obj := CreateComObject(CLSID_ShellLink);
  if assigned(obj) then
  begin
      isl := obj as IShellLink;
      ipf := obj as IPersistFile;
      if S_OK = ipf.Load(PWChar(Target), STGM_READ) then
      begin
          if S_OK = isl.Resolve(wnd, SLR_NO_UI + SLR_NOUPDATE) then
          begin
            SetLength(s, MAX_PATH);
            if S_OK = isl.GetPath(PChar(s), MAX_PATH, fda, SLGP_UNCPRIORITY) then Target := strpas(PChar(s));
            if S_OK = isl.GetArguments(PChar(s), MAX_PATH) then params := PChar(s);
            if S_OK = isl.GetWorkingDirectory(PChar(s), MAX_PATH) then dir := PChar(s);
            if S_OK = isl.GetIconLocation(PChar(s), MAX_PATH, iIcon) then icon := PChar(s);
            result := true;
          end;
      end;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure ResolveAppref(wnd: HWND; var Target: WideString);
//var
  //fs: TFileStream;
  //size: integer;
  //wch: array of WChar;
begin
  Target := ChangeFileExt(ExtractFileName(Target), '.exe');
  {fs := TFileStream.Create(Target, fmOpenRead);
  try
    size := integer(fs.Size) div 2;
    SetLength(wch, size + 1);
    fs.ReadBuffer(wch, fs.Size);
    Target := ChangeFileExt(cut(cutafter(strpas(pwchar(wch)), '#'), ','), '.exe');
  finally
    fs.free;
  end;}
end;
//--------------------------------------------------------------------------------------------------
function BrowseFolder(hWnd: THandle; title, default: string): string;
var
  lpItemID: PItemIDList;
  BrowseInfo: ShlObj.TBrowseInfo;
  DisplayName: array[0..MAX_PATH] of char;
  path: array [0..MAX_PATH] of char;
begin
  zeroMemory(@BrowseInfo, sizeof(TBrowseInfo));
  BrowseInfo.hwndOwner := hWnd;
  BrowseInfo.pszDisplayName := @DisplayName;
  BrowseInfo.lpszTitle := PChar(title);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
  lpItemID := SHBrowseForFolder(@BrowseInfo);
  if lpItemId <> nil then
  begin
    SHGetPathFromIDList(lpItemID, path);
    result := strpas(path);
    result := IncludeTrailingPathDelimiter(Result);
    GlobalFreePtr(lpItemID);
  end
  else
    Result := default;
end;
//------------------------------------------------------------------------------
procedure SetClipboard(Text: string);
var
  Data: HGLOBAL;
  dataPtr: pointer;
  pch: PChar;
begin
  if not OpenClipboard(application.mainform.handle) then
  begin
    ShowMessage('Cannot open clipboard');
    exit;
  end;
  EmptyClipboard;
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, length(Text) + 1);
  dataPtr := GlobalLock(Data);
  pch := PChar(Text);
  move(pch^, dataPtr^, length(Text) + 1);
  SetClipboardData(CF_TEXT, Data);
  GlobalUnlock(Data);
  CloseClipboard;
end;
//------------------------------------------------------------------------------
function GetClipboard: string;
var
  Data: HGLOBAL;
  dataptr: pointer;
  pch: PChar;
begin
  Result := '';
  if not OpenClipboard(application.mainform.handle) then
  begin
    ShowMessage('Cannot open clipboard');
    exit;
  end;
  try
    Data := GetClipboardData(CF_TEXT);
    if Data > 32 then
    begin
      dataptr := GlobalLock(Data);
      if dataptr <> nil then
      begin
        GetMem(pch, GlobalSize(Data));
        move(dataPtr^, pch^, GlobalSize(Data));
        Result := strpas(pch);
        FreeMem(pch, GlobalSize(Data));
      end;
      GlobalUnlock(Data);
    end
    else
      Result := '';
  except
  end;
  CloseClipboard;
end;
//------------------------------------------------------------------------------
function ColorToString(Color: uint): string;
begin
  FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;
//------------------------------------------------------------------------------
function StringToColor(const str: string): uint;
begin
  Result := StrToInt(str);
end;
//------------------------------------------------------------------------------
function confirm(handle: HWND; Text: WideString = ''): boolean;
begin
  if Text = '' then Text := 'Confirm action';
  Result := messageboxw(handle, PWChar(Text), 'Confirm', mb_yesno or mb_iconexclamation or mb_defbutton2) = idYes;
end;
//------------------------------------------------------------------------------
function FindWinamp: HWND;
begin
  Result := findwindow('BaseWindow', nil);
  if not IsWindow(Result) then
  begin
    Result := findwindow('Winamp v1.x', nil);
    if not IsWindow(Result) then Result := 0;
  end;
end;
//------------------------------------------------------------------------------
function LaunchWinamp(sw: integer = sw_shownormal): boolean;
var
  reg: TRegistry;
  wdir: string;
begin
  Result := False;

  try
    wdir := IncludeTrailingPathDelimiter(ExtractFileDrive(GetWinDir)) + 'program files\winamp\';

    if fileexists(wdir + 'winamp.exe') then
    begin
      shellexecute(0, nil, PChar(wdir + 'winamp.exe'), nil, PChar(wdir), sw);
      Result := True;
      exit;
    end;

    reg := TRegistry.Create;
    reg.RootKey := hkey_current_user;
    wdir := IncludeTrailingPathDelimiter(reg.ReadString('Software\Winamp'));
    if fileexists(wdir + 'winamp.exe') then
    begin
      shellexecute(0, nil, PChar(wdir + 'winamp.exe'), nil, PChar(wdir), sw);
      Result := True;
    end;
    reg.Free;
    reg := nil;
  except
  end;
end;
//------------------------------------------------------------------------------
function wacmd(cmd: HANDLE): boolean;
var
  wahwnd: THandle;
begin
  Result := False;
  wahwnd := FindWinamp;
  if wahwnd > 0 then
  begin
    sendmessage(wahwnd, wm_command, cmd, 0);
    Result := True;
  end;
end;
//------------------------------------------------------------------------------
procedure bsm(msg: UINT; wparam: WPARAM; lparam: LPARAM);
var
  recip: integer;
begin
  recip := BSM_APPLICATIONS;
  BroadcastSystemMessage(BSF_IGNORECURRENTTASK + BSF_FORCEIFHUNG + BSF_POSTMESSAGE, @recip, msg, wparam, lparam);
end;
//------------------------------------------------------------------------------
function IsIdenticalStreams(Source, Destination: TStream): boolean;
const
    Block_Size = 4096;
var
    Buffer_1: array[0..Block_Size-1] of byte;
    Buffer_2: array[0..Block_Size-1] of byte;
    Buffer_Length: integer;
begin
  Result := false;

  if Source.Size <> Destination.Size then Exit;

  FillChar(Buffer_1, Block_Size, #0);
  FillChar(Buffer_2, Block_Size, #0);
  Source.Seek(0, soBeginning);
  Destination.Seek(0, soBeginning);
  while Source.Position < Source.Size do
  begin
      Buffer_Length := Source.Read(Buffer_1, Block_Size);
      Destination.Read(Buffer_2, Block_Size);

      if not CompareMem(@Buffer_1, @Buffer_2, Buffer_Length) then Exit;
  end;

  Result := true;
end;
//------------------------------------------------------------------------------
procedure SendShift(hwnd: HWnd; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: longint;
begin
  vKey := $10;
  ScanCode := MapVirtualKey(vKey, 0);
  //wParam := vKey or ScanCode shl 8;
  lParam := longint(ScanCode) shl 16 or 1;
  if not Down then lParam := lParam or $C0000000;
  SendMessage(hwnd, WM_KEYDOWN, vKey, lParam);
end;
//------------------------------------------------------------------------------
procedure SendCtrl(hwnd: HWnd; Down: Boolean);
var
  vKey, ScanCode: Word;
  lParam: longint;
begin
  vKey := $11;
  ScanCode := MapVirtualKey(vKey, 0);
  //wParam := vKey or ScanCode shl 8;
  lParam := longint(ScanCode) shl 16 or 1;
  if not Down then lParam := lParam or $C0000000;
  SendMessage(hwnd, WM_KEYDOWN, vKey, lParam);
end;
//------------------------------------------------------------------------------
procedure SendKey(hwnd: HWnd; Key: char; noChar: boolean);
const MAPVK_VK_TO_CHAR = 2;
var
  kbLayout: HKL;
  vKey, ScanCode, wParam: Word;
  lParam, ExKey: longint;
  Shift, Ctrl: boolean;
  keyboardState: TKeyboardState;
  chars: string;
  asciiResult: longint;
begin
  kbLayout := GetKeyboardLayout(0);
  ExKey := VkKeyScanEx(Key, kbLayout);
  Shift := GetKeyState(VK_SHIFT) and $80 = 0; //(ExKey and $00020000) <> 0;
  Ctrl := (ExKey and $00040000) <> 0;
  ScanCode := ExKey and $000000FF or $FF00;
  if ord(key) < 128 then vKey := ord(Key)
  else vKey := MapVirtualKeyEx(ord(key), MAPVK_VK_TO_CHAR, kbLayout);
  wParam := vKey;
  lParam := longint(ScanCode) shl 16 or 1;
  if Shift then SendShift(hwnd, true);
  if Ctrl then SendCtrl(hwnd, true);
  SendMessage(hwnd, WM_KEYDOWN, vKey, lParam);
  if not noChar then SendMessage(hwnd, WM_CHAR, vKey, lParam);
  if not noChar then
  begin
    GetKeyboardState(keyboardState);
    SetLength(chars, 2);
    asciiResult := ToAsciiEx(vkey, ScanCode and $8FFF, keyboardState, @chars[1], 0, kbLayout);
    if asciiResult = 1 then SetLength(chars, 1)
    else if asciiResult <> 2 then chars := '';
    if length(chars) = 1 then SendMessage(hwnd, WM_CHAR, ord(chars[1]), lParam);
    if length(chars) = 2 then
    begin
      SendMessage(hwnd, WM_CHAR, ord(chars[1]), lParam);
      SendMessage(hwnd, WM_CHAR, ord(chars[2]), lParam);
    end;
  end;
  lParam := lParam or $C0000000;
  SendMessage(hwnd, WM_KEYUP, vKey, lParam);
  if Shift then SendShift(hwnd, false);
  if Ctrl then SendCtrl(hwnd, false);
end;
//------------------------------------------------------------------------------
end.

