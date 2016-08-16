unit frmcmdu;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, LMessages;

type

  { Tfrmcmd }

  Tfrmcmd = class(TForm)
    btn_browse: TButton;
    edcmd: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btn_browseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    recent: TStrings;
    FOpenWithNoActivate: boolean;
    procedure ShowRecent(step: integer);
    procedure WMNCHitTest(var msg: TWMNCHitTest); message WM_NCHITTEST;
  public
    property OpenWithNoActivate: boolean read FOpenWithNoActivate;
    class procedure Open(noActivate: boolean = false);
    procedure exec;
  end;

var
  frmcmd: Tfrmcmd;

implementation
uses frmmainu, toolu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure Tfrmcmd.Open(noActivate: boolean = false);
begin
  if not assigned(frmcmd) then application.CreateForm(self, frmcmd);
  frmcmd.Show;
  if noActivate then
  begin
    frmcmd.FOpenWithNoActivate := true;
    showWindow(frmcmd.handle, SW_HIDE);
    showWindow(frmcmd.handle, SW_SHOWNOACTIVATE);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormCreate(Sender: TObject);
begin
  try
    recent := TStringList.Create;
    if FileExists(toolu.UnzipPath('%pp%\recent_commands.txt')) then
      recent.LoadFromFile(toolu.UnzipPath('%pp%\recent_commands.txt'));
  except
    on e: Exception do frmmain.err('frmCmd.LoadRecent', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  edcmd.Clear;
  SetActiveWindow(frmmain.handle);
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.WMNCHitTest(var msg: TWMNCHitTest);
begin
  msg.Result := HTCAPTION;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormDestroy(Sender: TObject);
begin
  recent.Free;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormShow(Sender: TObject);
begin
  font.Name := GetFont;
  font.size := GetFontSize;

  clientheight := edcmd.top * 2 + edcmd.Height;
  edcmd.SetFocus;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.exec;
var
  cmd: string;
  idx: integer;
begin
  cmd := Trim(UTF8ToAnsi(edcmd.Text));
  if cmd = '' then exit;

  idx := recent.IndexOf(cmd);
  if idx < 0 then recent.insert(0, cmd)
  else if idx > 0 then
  begin
    recent.Delete(idx);
    recent.insert(0, cmd);
  end;

  edcmd.Clear;
  frmmain.execute_cmdline(cmd);
  Close;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 13) and (shift = []) then exec;
  if (key = 13) and (shift = [ssCtrl]) then
  begin
    edcmd.Text := '"' + AnsiToUTF8(FindFilePF(UTF8ToAnsi(edcmd.Text))) + '"';
    exec;
  end;
  if (key = vk_escape) and (shift = []) then Close;
  if (key = 79) and (shift = [ssCtrl]) then btn_browseClick(nil);
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
begin
  if msg.CharCode = vk_up then
  begin
    ShowRecent(1);
    Handled := true;
  end;
  if msg.CharCode = vk_down then
  begin
    ShowRecent(-1);
    Handled := true;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.ShowRecent(step: integer);
var
  idx: integer;
begin
  if recent.Count > 0 then
  begin
    idx := recent.IndexOf(UTF8ToAnsi(edcmd.Text));
    Inc(idx, step);
    if idx < 0 then idx := recent.Count - 1;
    if idx >= recent.Count then idx := 0;
    edcmd.Text := AnsiToUTF8(recent.strings[idx]);
    edcmd.SelStart := length(edcmd.Text) + 1;
    edcmd.SelLength := 0;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmcmd.btn_browseClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if Execute then
    begin
      edcmd.Text := AnsiToUTF8(filename);
      edcmd.SelStart := length(edcmd.Text) + 1;
      edcmd.SelLength := 0;
    end;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
end.

