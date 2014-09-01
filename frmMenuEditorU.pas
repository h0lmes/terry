unit frmMenuEditorU;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type

  { TfrmMenuEditor }

  TfrmMenuEditor = class(TForm)
    bbAddSep: TBitBtn;
    bbIconDown: TBitBtn;
    bbAddIcon: TBitBtn;
    bbEditIcon: TBitBtn;
    bbDelIcon: TBitBtn;
    bbIconUp: TBitBtn;
    GroupBox2: TGroupBox;
    listItems: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnMenuItemPropertiesClick(Sender: TObject);
    procedure btnDelMenuItemClick(Sender: TObject);
    procedure btnCreateShortcutClick(Sender: TObject);
    procedure btnCreateSeparatorClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure listItemsDblClick(Sender: TObject);
    procedure btn_item_upClick(Sender: TObject);
    procedure btn_item_downClick(Sender: TObject);
  private
    function getid(list: TListBox): uint;
    procedure ReadItems;
  public
    class procedure StartForm;
  end;

var
  frmMenuEditor: TfrmMenuEditor;

implementation
uses frmterryu, declu, toolu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmMenuEditor.StartForm;
begin
  if not assigned(frmMenuEditor) then Application.CreateForm(self, frmMenuEditor);
  frmMenuEditor.show;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.FormShow(Sender: TObject);
begin
  try
    font.name:= GetFont;
    font.size:= GetFontSize;
    ReadItems;
  except
    on e: Exception do messageBox(handle, pchar('Menu Editor Show'#10#13 + e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
end;
//------------------------------------------------------------------------------
function TfrmMenuEditor.getid(list: TListBox): uint;
begin
  result := 0;
  try result := uint(list.items.objects[list.ItemIndex]);
  except end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btnMenuItemPropertiesClick(Sender: TObject);
var
  item: uint;
begin
  try
    item := getid(listItems);
    if IsWindow(item) then frmterry.ItemMgr.ItemCmd(item, icConfigure, 0);
  except
    on e: Exception do messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btnDelMenuItemClick(Sender: TObject);
var
  li: integer;
  item: uint;
begin
  try
    li := listItems.ItemIndex;
    if li >= 0 then
      if not confirm(Handle, UTF8ToAnsi(XDeleteIcon + ' ' + listItems.Items.Strings[li] + '?')) then exit;
    item := getid(listItems);
    if IsWindow(item) then
    begin
      frmterry.ItemMgr.DeleteItem(item);
      ReadItems;
      if li > listItems.count - 1 then listItems.ItemIndex := listItems.count - 1
      else listItems.ItemIndex := li;
    end;
  except
    on e: Exception do messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btnCreateShortcutClick(Sender: TObject);
begin
  try
    frmterry.ItemMgr.NewShortcut;
    ReadItems;
  except
    on e: Exception do messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btnCreateSeparatorClick(Sender: TObject);
begin
  try
    frmterry.ItemMgr.NewSeparator;
    ReadItems;
  except
    on e: Exception do
      messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btn_item_upClick(Sender: TObject);
var
  li: integer;
begin
  try
      li := listItems.ItemIndex;
      if listItems.ItemIndex > 0 then
      begin
        frmterry.ItemMgr.MoveItem(uint(listItems.Items.Objects[li]), li - 1);
        frmterry.ItemMgr.ItemsChanged;
        ReadItems;
        listItems.ItemIndex := li - 1;
      end;
  except
    on e: Exception do messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.btn_item_downClick(Sender: TObject);
var
  li: integer;
begin
  try
      li := listItems.ItemIndex;
      if listItems.ItemIndex < listItems.count - 1 then
      begin
        frmterry.ItemMgr.MoveItem(uint(listItems.Items.Objects[li]), li + 2);
        frmterry.ItemMgr.ItemsChanged;
        ReadItems;
        listItems.ItemIndex := li + 1;
      end;
  except
    on e: Exception do messageBox(handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.ReadItems;
var
  item: integer;
  sLabel: string;
  handle: uint;
begin
    listItems.clear;
    item := 0;
    while item < frmterry.ItemMgr.VItemCount do
    begin
      handle := frmterry.ItemMgr.VItemHWnd(item);
      if frmterry.ItemMgr.IsSeparator(handle) then sLabel := '---' else sLabel := frmterry.ItemMgr.GetPluginCaption(handle);
      if sLabel = '' then sLabel := '<No label>';
      listItems.Items.addobject(AnsiToUTF8(sLabel), tobject(handle));
      inc(item);
    end;
    listItems.ItemIndex := integer(listItems.count > 0) - 1;
end;
//------------------------------------------------------------------------------
procedure TfrmMenuEditor.listItemsDblClick(Sender: TObject);
begin
  btnMenuItemPropertiesClick(nil);
end;
//------------------------------------------------------------------------------
end.
