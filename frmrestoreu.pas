unit frmrestoreu;

{$mode delphi}

interface

uses
    Windows, Classes, SysUtils, Forms, Controls, StdCtrls;

type

		{ TfrmRestore }

    TfrmRestore = class(TForm)
				btnRestore: TButton;
				list: TListBox;
				procedure btnRestoreClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
				procedure FormShow(Sender: TObject);
				procedure listDblClick(Sender: TObject);
    private
    public
      class procedure Open;
    end;

var
  frmRestore: TfrmRestore;

implementation
uses frmmainu, toolu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmRestore.Open;
begin
  if not assigned(frmRestore) then application.CreateForm(self, frmRestore);
  frmRestore.Show;
end;
//------------------------------------------------------------------------------
procedure TfrmRestore.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SetActiveWindow(frmmain.handle);
end;
//------------------------------------------------------------------------------
procedure TfrmRestore.FormShow(Sender: TObject);
begin
  font.name := GetFont;
  font.size := GetFontSize;

  list.clear;
  SearchFilesRecursive(UnzipPath('%pp%\Backup'), 'sets*.ini', list.Items);
  list.SetFocus;
end;
//------------------------------------------------------------------------------
procedure TfrmRestore.listDblClick(Sender: TObject);
begin
  btnRestore.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmRestore.btnRestoreClick(Sender: TObject);
begin
  close;
  if list.ItemIndex >= 0 then frmmain.Restore(list.Items.Strings[list.ItemIndex]);
end;
//------------------------------------------------------------------------------
end.

