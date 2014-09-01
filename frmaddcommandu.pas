unit frmAddCommandU;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  gettext, DefaultTranslator, ComCtrls, ExtCtrls, StdCtrls;

type

  { TfrmAddCommand }

  TfrmAddCommand = class(TForm)
    btnAdd: TButton;
    btnClose: TButton;
    images: TImageList;
    lv: TListView;
    Panel1: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
  public
    Filename: string;
    class procedure Open;
  end; 

var
  frmAddCommand: TfrmAddCommand;

implementation
uses declu, toolu, frmterryu, scitemu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmAddCommand.Open;
var
  Lang, FallbackLang, tmpFilename: string;
begin
  GetLanguageIDs(Lang, FallbackLang);
  tmpFilename := UnzipPath('%pp%\locale\commandlist.' + FallbackLang + '.txt');
  if not FileExists(tmpFilename) then tmpFilename := UnzipPath('%pp%\locale\commandlist.txt');
  if not FileExists(tmpFilename) then
  begin
    frmterry.notify(UTF8ToAnsi(XErrorCommandListNotFound));
    exit;
  end;
  if not assigned(frmAddCommand) then application.CreateForm(self, frmAddCommand);
  frmAddCommand.Filename := tmpFilename;
  frmAddCommand.ShowModal;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.FormShow(Sender: TObject);
var
  list: TStrings;
  i: integer;
  item: TListItem;
begin
  try list := TStringlist.Create;
  except exit;
  end;

  try
    list.LoadFromFile(Filename);
    lv.BeginUpdate;
    lv.Items.Clear;
    i := 0;
    while i < list.Count do
    begin
      item := lv.Items.Add;
      item.Caption := AnsiToUTF8(list.strings[i]);
      if i + 1 < list.Count then item.SubItems.Add(AnsiToUTF8(list.strings[i + 1]));
      inc(i, 2);
    end;
    lv.EndUpdate;
  finally
    list.free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  lv.Items.Clear;
  CloseAction := cafree;
  frmAddCommand := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmAddCommand.btnAddClick(Sender: TObject);
var
  cmd, params: string;
begin
  if lv.ItemIndex > -1 then
  begin
    cmd := UTF8ToAnsi(lv.Items[lv.ItemIndex].Caption);
    split(cmd, '(', cmd, params);
    params := cuttolast(params, ')');
    frmterry.ItemMgr.InsertItem(TShortcutItem.Make(0, copy(cmd, 2, length(cmd)), cmd, params, '', '', 1));
  end;
end;
//------------------------------------------------------------------------------
end.

