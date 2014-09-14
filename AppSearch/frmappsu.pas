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
    procedure AddSelected;
    procedure LoadApps;
    procedure qSortStrings(list: TStrings);
    procedure ResolveShortcut(wnd: HWND; ShortcutPath: string; out description, filename, params, dir, icn: string; out showcmd: integer);
    procedure searchfilesrecurse(path, mask: string; var list: TStrings; level: cardinal = 0; maxlevel: cardinal = 255; maxcount: integer = $7fffffff);
  public
  end;

  TProgramData = record
    Name: array [0..1023] of char;
    Filename: array [0..1023] of char;
  end;

const
  ICON_SIZE = 32;
  DATA_PROGRAM = $f001;

var
  frmApps: TfrmApps;

implementation
{$R *.lfm}
uses gdip_gfx, GDIPAPI, toolu;
//------------------------------------------------------------------------------
procedure TfrmApps.FormShow(Sender: TObject);
begin
  if listApps.ViewStyle = vsReport then
  begin
    listApps.Columns.Items[0].Width := images.Width + 5;
    listApps.Columns.Items[1].Width := 250;
    listApps.Columns.Items[2].Width := 400;
  end;
  LoadApps;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.listAppsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
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
    strcopy(pdata.Filename, pchar(UTF8ToAnsi(listApps.Items[listApps.ItemIndex].SubItems[1])));
    cds.cbData := sizeof(pdata);
    cds.dwData := DATA_PROGRAM;
    cds.lpData := @pdata;
    WinHandle := FindWindow('Window', 'TerryApp');
    SendMessage(WinHandle, WM_COPYDATA, Handle, LPARAM(@cds));
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmApps.LoadApps;
var
  path: array [0..1023] of char;
  i: integer;
  item: TListItem;
  files: TStrings;
  xfiles: TStrings;
  filename: string;
  filenameNoPath: string;
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
      // scan Start Menu for .LNK //
      files := TStringList.Create;
      xfiles := TStringList.Create;
      if SHGetSpecialFolderPath(Handle, path, CSIDL_COMMON_STARTMENU, false) then searchfilesrecurse(path, '*.lnk', files);
      if SHGetSpecialFolderPath(Handle, path, CSIDL_COMMON_STARTMENU, false) then searchfilesrecurse(path, '*.appref-ms', files, 1);
      if SHGetSpecialFolderPath(Handle, path, CSIDL_STARTMENU, false) then searchfilesrecurse(path, '*.lnk', files, 1);
      if SHGetSpecialFolderPath(Handle, path, CSIDL_STARTMENU, false) then searchfilesrecurse(path, '*.appref-ms', files, 1);
      // sort list //
      qSortStrings(files);

      i := 0;
      while i < files.Count do
      begin
        // get shortcut data //
        filename := files.Strings[i];
        filenameNoPath := AnsiLowerCase(ExtractFileName(filename));

        // check if we are interested in //
        if (pos('unins', filenameNoPath) < 1) and (pos('деинст', filenameNoPath) < 1) // get rid of uninstalls //
          and (xfiles.IndexOf(AnsiLowerCase(ExtractFileName(filename))) < 0) // get rid of duplicates //
        then
        begin
          xfiles.add(filenameNoPath);

          // load image as GDIP object //
          img := nil;
          LoadImage(filename, ICON_SIZE, true, false, img, iw, ih);

          // add list item //
          item := listApps.Items.Add;
          item.Caption := AnsiToUTF8(ChangeFileExt(ExtractFileName(filename), ''));
          item.Data := img;
          item.SubItems.Add(item.Caption); // name
          item.SubItems.Add(AnsiToUTF8(filename)); // file
        end;

        inc(i);
      end;
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
procedure TfrmApps.qSortStrings(list: TStrings);
  procedure sort(list: TStrings; low, high: integer);
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
  sort(list, 0, list.Count - 1);
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

