unit frmAppsU;

{$mode Delphi}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, ComObj, ShlObj, ActiveX;

type

  { TfrmApps }

  TfrmApps = class(TForm)
    images: TImageList;
    listApps: TListView;
    procedure FormShow(Sender: TObject);
    procedure listAppsAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure listAppsDblClick(Sender: TObject);
    procedure listAppsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    files: TStrings;
    FNoWindows: boolean;
    procedure AddSelected;
    procedure UpdateStatus;
    procedure LoadApps;
    procedure ResolveShortcut(wnd: HWND; ShortcutPath: string; out description, filename, params, dir, icn: string; out showcmd: integer);
    procedure searchfilesrecurse(path, mask: string; var list: TStrings; level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
    function GetFileVersion(filename: string): string;
  public
  end;

  TProgramData = record
    Name: array [0..1023] of char;
    Filename: array [0..1023] of char;
    Params: array [0..1023] of char;
    Dir: array [0..1023] of char;
    Icon: array [0..1023] of char;
    ShowCmd: integer;
  end;

const
  ICON_SIZE = 32;
  DATA_PROGRAM = $f001;

var
  frmApps: TfrmApps;

implementation
{$R *.lfm}
uses gdip_gfx, GDIPAPI;
//------------------------------------------------------------------------------
procedure TfrmApps.FormShow(Sender: TObject);
begin
  listApps.ViewStyle := vsIcon;
  if listApps.ViewStyle = vsReport then
  begin
    listApps.Columns.Items[0].Width := images.Width + 5;
    listApps.Columns.Items[1].Width := 250;
    listApps.Columns.Items[2].Width := 400;
  end;

  FNoWindows := true;

  LoadApps;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.listAppsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateStatus;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.listAppsDblClick(Sender: TObject);
begin
  AddSelected;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.AddSelected;
var
  pdata: TProgramData;
  cds: COPYDATASTRUCT;
  WinHandle: uint;
begin
  if listApps.ItemIndex >= 0 then
  begin
    strcopy(pdata.Name, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[0])));
    strcopy(pdata.Filename, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[2])));
    strcopy(pdata.Params, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[3])));
    strcopy(pdata.Dir, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[4])));
    strcopy(pdata.Icon, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[5])));
    pdata.ShowCmd := strtoint(listApps.Items[listApps.ItemIndex].SubItems[6]);
    cds.cbData := sizeof(pdata);
    cds.dwData := DATA_PROGRAM;
    cds.lpData := @pdata;
    WinHandle := FindWindow('Window', 'TerryApp');
    SendMessage(WinHandle, WM_COPYDATA, Handle, LPARAM(@cds));
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.UpdateStatus;
begin
end;
//------------------------------------------------------------------------------
procedure TfrmApps.LoadApps;
var
  path: array [0..1023] of char;
  i, j: integer;
  item: TListItem;
  xfiles: TStrings;
  //
  description: string;
  filename: string;
  params: string;
  dir: string;
  icn: string;
  showcmd: integer;
  //
  img: Pointer;
  iw, ih: uint;
begin
  try
    listApps.BeginUpdate;

    // clear list //
    i := 0;
    while i < listApps.Items.Count do
    begin
      try GdipDisposeImage(listApps.Items[i].Data);
      except end;
      inc(i);
    end;
    listApps.Clear;

    try
      // scan Start menu for .LNK //
      files := TStringList.Create;
      xfiles := TStringList.Create;
      if SHGetSpecialFolderPath(Handle, path, CSIDL_COMMON_STARTMENU, false) then searchfilesrecurse(path, '*.lnk', files);
      if SHGetSpecialFolderPath(Handle, path, CSIDL_STARTMENU, false) then searchfilesrecurse(path, '*.lnk', files, 1);

      // sort list //
      for i := 0 to files.Count - 2 do
        for j := i + 1 to files.Count - 1 do
          if AnsiLowerCase(ExtractFileName(files.Strings[i])) > AnsiLowerCase(ExtractFileName(files.Strings[j])) then
            files.move(i, j);

      i := 0;
      while i < files.Count do
      begin
        // get shortcut data //
        ResolveShortcut(Handle, files.Strings[i], description, filename, params, dir, icn, showcmd);

        // check if we are interested in //
        if (AnsiLowerCase(ExtractFileExt(filename)) = '.exe') // only executables //
          and (pos('unins', AnsiLowerCase(filename)) < 1) // get rid of uninstalls //
          and (not FNoWindows or ((pos('windows', AnsiLowerCase(filename)) < 1) and (pos('microsoft', AnsiLowerCase(filename)) < 1))) // get rid of standard progs //
          and (xfiles.IndexOf(AnsiLowerCase(ExtractFileName(filename))) < 0) // get rid of duplicates //
        then
        begin
          xfiles.add(AnsiLowerCase(ExtractFileName(filename)));

          // load image as GDIP object //
          img := nil;
          if FileExists(icn) then gdip_gfx.LoadImage(icn, ICON_SIZE, false, img, iw, ih)
          else gdip_gfx.LoadImage(filename, ICON_SIZE, false, img, iw, ih);
          if img = nil then gdip_gfx.LoadImage(files.Strings[i], ICON_SIZE, false, img, iw, ih);

          // add list item //
          item := listApps.Items.Add;
          item.Caption := AnsiToUTF8(ChangeFileExt(ExtractFileName(files.Strings[i]), ''));
          item.Data := img;
          item.SubItems.Add(AnsiToUTF8(ChangeFileExt(ExtractFileName(files.Strings[i]), ''))); // name
          item.SubItems.Add(AnsiToUTF8(description)); // description
          item.SubItems.Add(AnsiToUTF8(filename)); // file
          item.SubItems.Add(AnsiToUTF8(params)); // parameters
          item.SubItems.Add(AnsiToUTF8(dir)); // working directory
          item.SubItems.Add(AnsiToUTF8(icn)); // icon file
          item.SubItems.Add(AnsiToUTF8(inttostr(showcmd))); // show cmd
          item.SubItems.Add(AnsiToUTF8(GetFileVersion(filename))); // version

          inc(i);
        end else begin
          files.Delete(i);
        end;
      end;

      UpdateStatus;
    finally
      listApps.EndUpdate;
      files.free;
      xfiles.free;
    end;
  except
    on e: Exception do messagebox(Handle, pchar(e.message), nil, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.ResolveShortcut(wnd: HWND; ShortcutPath: string;
  out description, filename, params, dir, icn: string; out showcmd: integer);
var
  obj: IUnknown;
  isl: IShellLink;
  ipf: IPersistFile;
  fda: Windows.TWin32FindDataA;
  s: string;
  i: integer;
begin
  obj := CreateComObject(CLSID_ShellLink);
  isl := obj as IShellLink;
  ipf := obj as IPersistFile;
  ipf.Load(PWChar(WideString(ShortcutPath)), STGM_READ);
  isl.Resolve(wnd, SLR_ANY_MATCH);

  SetLength(s, MAX_PATH);
  isl.GetDescription(PChar(s), length(s));
  description := PChar(s);

  SetLength(s, MAX_PATH);
  isl.GetPath(PChar(s), length(s), fda, SLGP_UNCPRIORITY);
  filename := PChar(s);

  SetLength(s, MAX_PATH);
  isl.GetArguments(PChar(s), length(s));
  params := PChar(s);

  SetLength(s, MAX_PATH);
  isl.GetWorkingDirectory(PChar(s), length(s));
  dir := PChar(s);

  SetLength(s, MAX_PATH);
  isl.GetIconLocation(PChar(s), length(s), i);
  icn := PChar(s);

  isl.GetShowCmd(showcmd);
end;
//------------------------------------------------------------------------------
procedure TfrmApps.searchfilesrecurse(path, mask: string; var list: TStrings; level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
var
  fhandle: HANDLE;
  filename: string;
  f: TWin32FindData;
begin
  if level = 0 then list.Clear;
  path := IncludeTrailingPathDelimiter(path);

  // folders //
  fhandle := FindFirstFile(PChar(path + '*.*'), f);
  if not (fhandle = THANDLE(-1)) then
  begin
    filename := strpas(f.cFileName);
    if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
      (filename <> '..') and (level < maxlevel) then
      searchfilesrecurse(path + filename, mask, list, level + 1);
    while FindNextFile(fhandle, f) do
    begin
      filename := strpas(f.cFileName);
      if ((f.dwFileAttributes and 16) = 16) and (filename <> '.') and
        (filename <> '..') and (level < maxlevel) then
        searchfilesrecurse(path + filename, mask, list, level + 1, maxlevel);
    end;
  end;
  if not (fhandle = THANDLE(-1)) then Windows.FindClose(fhandle);

  // files //
  fhandle := FindFirstFile(PChar(path + mask), f);
  if not (fhandle = THANDLE(-1)) then
  begin
    if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then
      list.addobject(path + f.cFileName, tobject(0));
    while FindNextFile(fhandle, f) do
      if ((f.dwFileAttributes and $18) = 0) and (list.Count < maxcount) then
        list.addobject(path + f.cFileName, tobject(0));
  end;
  if not (fhandle = THANDLE(-1)) then Windows.FindClose(fhandle);
end;
//----------------------------------------------------------------------
function TfrmApps.GetFileVersion(filename: string): string;
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  dw: DWORD;
begin
  result := 'n/a';
  InfoSize := GetFileVersionInfoSize(PChar(FileName), dw);
  if InfoSize <> 0 then
  begin
    GetMem(Info, InfoSize);
    try
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      result := inttostr(integer(FileInfo^.dwFileVersionMS shr 16)) + '.' +
        inttostr(FileInfo^.dwFileVersionMS and $FFFF) + '.' +
        inttostr(FileInfo^.dwFileVersionLS shr 16) + '.' +
        inttostr(FileInfo^.dwFileVersionLS and $FFFF);
    finally
      FreeMem(Info, FileInfoSize);
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.listAppsAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  rect: windows.TRect;
  h: Pointer;
  hbrush: Pointer;
  srcwidth, srcheight: cardinal;
begin
  DefaultDraw := true;
  if Stage = cdPostPaint then
  try
    DefaultDraw := false;
    if listApps.ViewStyle = vsReport then
    begin
      rect := Item.DisplayRect(drBounds);
      GdipCreateFromHDC(sender.Canvas.Handle, h);
      GdipCreateSolidFill($ffffffff, hbrush);
      GdipFillRectangleI(h, hbrush, rect.Left, rect.Top, listApps.Columns.Items[0].Width, images.Height + 1);
      GdipDeletebrush(hbrush);
      GdipGetImageWidth(Item.Data, srcwidth);
      GdipGetImageHeight(Item.Data, srcheight);
      GdipSetInterpolationMode(h, InterpolationModeHighQualityBicubic);
      GdipDrawImageRectRectI(h, Item.Data, rect.Left + 16, rect.Top + (images.Height - ICON_SIZE) div 2,
        ICON_SIZE, ICON_SIZE, 0, 0, srcwidth, srcheight, UnitPixel, nil, nil, nil);
    end
    else
    begin
      rect := Item.DisplayRect(drIcon);
      GdipCreateFromHDC(sender.Canvas.Handle, h);
      GdipCreateSolidFill($ffffffff, hbrush);
      GdipFillRectangleI(h, hbrush, rect.Left, rect.Top, rect.Right - rect.Left, ICON_SIZE + 1);
      GdipDeletebrush(hbrush);
      GdipGetImageWidth(Item.Data, srcwidth);
      GdipGetImageHeight(Item.Data, srcheight);
      GdipSetInterpolationMode(h, InterpolationModeHighQualityBicubic);
      GdipDrawImageRectRectI(h, Item.Data, rect.Left + (rect.Right - rect.Left - ICON_SIZE) div 2,
        rect.Top + (images.Height - ICON_SIZE) div 2, ICON_SIZE, ICON_SIZE, 0, 0, srcwidth, srcheight, UnitPixel, nil, nil, nil);
    end;
  finally
    GdipDeleteGraphics(h);
  end;
end;
//------------------------------------------------------------------------------
end.

