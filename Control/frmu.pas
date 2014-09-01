unit frmu;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ShlObj, ComObj, ShellApi,
  PIDL;

type
  Tfrm = class(TForm)
    listbox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure listboxDblClick(Sender: TObject);
  private
    list: TStrings;
  public
    procedure ReadSpecialFolder(csidl: integer);
  end;

var
  frm: Tfrm;

implementation
{$R *.lfm}
//------------------------------------------------------------------------------
procedure Tfrm.FormCreate(Sender: TObject);
begin
  list := TStringList.Create;
  ReadSpecialFolder(CSIDL_CONTROLS);
end;
//------------------------------------------------------------------------------
procedure Tfrm.listboxDblClick(Sender: TObject);
var
  pidl: PITEMIDLIST;
  sei: TShellExecuteInfoA;
begin
  pidl := PIDL_FromString(list.Strings[int64(listbox.Items.Objects[listbox.ItemIndex])]);
  sei.cbSize := sizeof(sei);
  sei.lpIDList := pidl;
  sei.Wnd := Handle;
  sei.nShow := 1;
  sei.lpVerb := 'open';
  sei.lpFile := nil;
  sei.lpParameters := nil;
  sei.lpDirectory := nil;
  sei.fMask := SEE_MASK_IDLIST;
  ShellExecuteExA(@sei);
end;
//------------------------------------------------------------------------------
procedure Tfrm.ReadSpecialFolder(csidl: integer);
var
  sfi: TSHFileInfoA;
  psfDesktop: IShellFolder;
  psfFolder: IShellFolder;
  pidFolder: PITEMIDLIST;
  pidChild: PITEMIDLIST;
  pidAbsolute: PItemIdList;
  pEnumList: IEnumIDList;
  celtFetched: ULONG;
begin
  try
    OleCheck(SHGetDesktopFolder(psfDesktop));
    OleCheck(SHGetSpecialFolderLocation(0, csidl or CSIDL_FLAG_NO_ALIAS, pidFolder));
    OleCheck(psfDesktop.BindToObject(pidFolder, nil, IID_IShellFolder, psfFolder));
    OleCheck(psfFolder.EnumObjects(0, SHCONTF_NONFOLDERS or SHCONTF_FOLDERS, pEnumList));
    SHGetFileInfoA(pchar(pidFolder), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
    Caption := AnsiToUTF8(sfi.szDisplayName);

    listbox.clear;
    list.clear;
    while pEnumList.Next(1, pidChild, celtFetched) = NOERROR do
    begin
      pidAbsolute := PIDL_GetAbsolute(pidFolder, pidChild);
      SHGetFileInfoA(pchar(pidAbsolute), 0, @sfi, sizeof(sfi), SHGFI_PIDL or SHGFI_DISPLAYNAME);
      listbox.items.addobject(AnsiToUTF8(sfi.szDisplayName), TObject(int64(list.count and $ffff)));
      list.add(PIDL_ToString(pidAbsolute));
      PIDL_Free(pidChild);
    end;

    PIDL_Free(pidFolder);
  except
    on e: Exception do raise Exception.Create('in ReadSpecialFolder'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.
