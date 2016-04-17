unit frmtipu;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfrmTip }

  TfrmTip = class(TForm)
    btnEnough: TButton;
    btnNext: TButton;
    memo: TMemo;
    procedure btnEnoughClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure FormShow(Sender: TObject);
  private
    FIndex: integer;
    list: TStrings;
  public
    class procedure Open;
    procedure Load;
  end;

var
  frmTip: TfrmTip;

implementation
{$R *.lfm}
uses toolu, setsu;
//------------------------------------------------------------------------------
class procedure TfrmTip.Open;
begin
  if not FileExists(UnzipPath('%pp%\tips.txt')) then exit;
  Application.CreateForm(TfrmTip, frmTip);
  frmTip.Show;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormCreate(Sender: TObject);
begin
  list := TStringList.Create;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormShow(Sender: TObject);
begin
  font.name := GetFont;
  font.size := GetFontSize;
  memo.font.name := GetFont;
  memo.font.size := GetFontSize * 4 div 3;

  Load;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  list.free;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then btnEnough.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnEnoughClick(Sender: TObject);
begin
  close;
  frmTip := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.Load;
begin
  list.LoadFromFile(UnzipPath('%pp%\tips.txt'));
  FIndex := 0;
  btnNext.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnNextClick(Sender: TObject);
begin
  memo.Clear;
  if FIndex >= list.Count then FIndex := 0;
  while list.strings[FIndex] <> '' do
  begin
    memo.lines.add(AnsiToUTF8(list.strings[FIndex]));
    inc(FIndex);
    if FIndex >= list.Count then break;
  end;
  inc(FIndex);
end;
//------------------------------------------------------------------------------
end.

