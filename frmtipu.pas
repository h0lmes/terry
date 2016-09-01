unit frmtipu;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, gettext, DefaultTranslator, declu, LazUtf8;

type

  { TfrmTip }

  TfrmTip = class(TForm)
    btnClose: TButton;
    btnNext: TButton;
    memo: TMemo;
    procedure btnCloseClick(Sender: TObject);
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
uses toolu;
//------------------------------------------------------------------------------
class procedure TfrmTip.Open;
begin
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
procedure TfrmTip.Load;
var
  Lang, FallbackLang, tmpFilename, pp: string;
begin
  GetLanguageIDs(Lang, FallbackLang);
  pp := IncludeTrailingPathDelimiter(ExtractFilePath(Paramstr(0)));
  tmpFilename := pp + 'locale\tips.' + FallbackLang + '.txt';
  if not FileExists(tmpFilename) then tmpFilename := pp + 'locale\tips.txt';
  if not FileExists(tmpFilename) then
  begin
    list.Text := XErrorTipsNotFound;
    exit;
  end;
  list.LoadFromFile(tmpFilename);
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
    memo.lines.add(WinCPToUTF8(list.strings[FIndex]));
    inc(FIndex);
    if FIndex >= list.Count then break;
  end;
  inc(FIndex);
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then btnClose.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnCloseClick(Sender: TObject);
begin
  close;
  frmTip := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  list.free;
end;
//------------------------------------------------------------------------------
end.

