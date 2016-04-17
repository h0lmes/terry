unit frmAddCommandU;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  gettext, DefaultTranslator, ComCtrls, ExtCtrls, StdCtrls, IniFiles;

type
  TItem = record
    classname: array [0..31] of char;
    name: array [0..255] of char;
    command: array [0..255] of char;
    params: array [0..255] of char;
    icon: array [0..255] of char;
    description: array [0..1023] of char;
  end;
  PItem = ^TItem;

  { TfrmAddCommand }

  TfrmAddCommand = class(TForm)
    btnAdd: TButton;
    images: TImageList;
    memo: TMemo;
    tree: TTreeView;
    procedure btnAddClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure treeDblClick(Sender: TObject);
    procedure treeSelectionChanged(Sender: TObject);
  private
    function AddGroup(name: string): TTreeNode;
    procedure AddItem(node: TTreeNode; classname, name, command, params, icon, description: string);
  public
    Filename: string;
    class procedure Open;
  end; 

var
  frmAddCommand: TfrmAddCommand;

implementation
uses declu, toolu, frmmainu, scitemu, stackitemu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmAddCommand.Open;
var
  Lang, FallbackLang, tmpFilename: string;
begin
  GetLanguageIDs(Lang, FallbackLang);
  tmpFilename := UnzipPath('%pp%\locale\commandlist.' + FallbackLang + '.ini');
  if not FileExists(tmpFilename) then tmpFilename := UnzipPath('%pp%\locale\commandlist.ini');
  if not FileExists(tmpFilename) then
  begin
    frmmain.notify(UTF8ToAnsi(XErrorCommandListNotFound));
    exit;
  end;
  if not assigned(frmAddCommand) then application.CreateForm(self, frmAddCommand);
  frmAddCommand.Filename := tmpFilename;
  frmAddCommand.ShowModal;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.FormCreate(Sender: TObject);
begin
  Caption := XSpecificIcons;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  idx: integer;
  item: PItem;
begin
  idx := 0;
  while idx < tree.Items.Count do
  begin
    item := tree.Items.Item[idx].Data;
    if assigned(item) then Dispose(item);
    inc(idx);
  end;

  CloseAction := cafree;
  frmAddCommand := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.FormShow(Sender: TObject);
var
  ini: TIniFile;
  list: TStrings;
  groupIdx, itemIdx: integer;
  node: TTreeNode;
  group, groupName, classname, name, command, params, icon, description: string;
begin
  try
    font.name := GetFont;
    font.size := GetFontSize;

    constraints.MinHeight := ClientHeight;
    constraints.MinWidth := ClientWidth;

    tree.BeginUpdate;
    tree.Items.Clear;

    ini := TIniFile.Create(Filename);
    list := TStringList.Create;
    ini.ReadSections(list);

    groupIdx := 1;
    repeat
      group := 'group' + inttostr(groupIdx);
      groupName := ini.ReadString(group, 'groupname', '');
      if groupName <> '' then node := AddGroup(groupName);

      itemIdx := 1;
      repeat
        classname := ini.ReadString(group, 'class' + inttostr(itemIdx), 'shortcut');
        name := ini.ReadString(group, 'name' + inttostr(itemIdx), '');
        command := ini.ReadString(group, 'command' + inttostr(itemIdx), '');
        params := ini.ReadString(group, 'params' + inttostr(itemIdx), '');
        icon := ini.ReadString(group, 'icon' + inttostr(itemIdx), '');
        description := ini.ReadString(group, 'description' + inttostr(itemIdx), '');
        if name <> '' then AddItem(node, classname, name, command, params, icon, description);
        inc(itemIdx);
      until name = '';

      node.Expand(true);
      inc(groupIdx);
    until (groupName = '');


    ini.free;

    tree.EndUpdate;
  except
    on e: Exception do raise Exception.Create('frmAddCommand.FormShow.ReadItems'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TfrmAddCommand.AddGroup(name: string): TTreeNode;
begin
  result := tree.Items.Add(nil, AnsiToUTF8(name));
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.AddItem(node: TTreeNode; classname, name, command, params, icon, description: string);
var
  item: PItem;
begin
  if assigned(node) then
  try
    New(item);
    tree.Items.AddChildObject(node, AnsiToUTF8(name), item);
    strlcopy(item.classname, pchar(classname), 31);
    strlcopy(item.name, pchar(name), 255);
    strlcopy(item.command, pchar(command), 255);
    strlcopy(item.params, pchar(params), 255);
    strlcopy(item.icon, pchar(icon), 255);
    strlcopy(item.description, pchar(description), 1023);
  except
    on e: Exception do raise Exception.Create('frmAddCommand.AddItem'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.treeSelectionChanged(Sender: TObject);
var
  item: PItem;
begin
  item := nil;
  if assigned(tree.Selected) then
    if assigned(tree.Selected.Parent) then
      item := tree.Selected.Data;

  if assigned(item) then
  begin
    btnAdd.Show;
    memo.Text := AnsiToUTF8(pchar(item.description));
  end else begin
    btnAdd.Hide;
    memo.Clear;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.btnAddClick(Sender: TObject);
var
  item: PItem;
  name, cmd, params, icon, strItem: string;
begin
  if assigned(tree.Selected) then
    if assigned(tree.Selected.Parent) then
    begin
      item := tree.Selected.Data;
      name := pchar(item.name);
      cmd := pchar(item.command);
      params := pchar(item.params);
      icon := pchar(item.icon);

      if strlcomp(pchar(item.classname), 'stack', 5) = 0 then
        strItem := TStackItem.Make(0, name, icon, cmd)
      else
        strItem := TShortcutItem.Make(0, name, cmd, params, '', icon, 1);

      frmmain.ItemMgr.InsertItem(strItem);
    end;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.treeDblClick(Sender: TObject);
begin
  btnAdd.Click;
end;
//------------------------------------------------------------------------------
end.

