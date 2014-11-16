unit frmthemeeditoru;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, themeu, toolu, gdip_gfx, ExtCtrls, Buttons, Menus;

type

  { TfrmThemeEditor }

  TfrmThemeEditor = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    edBlurRegion: TEdit;
    edItemsArea: TEdit;
    edmargin: TEdit;
    edSeparatorMargins: TEdit;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label9: TLabel;
    procedure edItemsAreaChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure mcloseClick(Sender: TObject);
    procedure msaveClick(Sender: TObject);
  private
  public
    class procedure Open;
  end;

var
  frmThemeEditor: TfrmThemeEditor;

implementation
uses declu, frmmainu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmThemeEditor.Open;
begin
  if not assigned(theme) then
  begin
    raise Exception.Create(UTF8ToAnsi(XErrorThemeObjectNotFound));
    exit;
  end;
  if not assigned(frmThemeEditor) then Application.CreateForm(self, frmThemeEditor);
  frmThemeEditor.show;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.FormShow(Sender: TObject);
begin
  font.name:= GetFont;
  font.size:= GetFontSize;

  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;

  edItemsArea.OnChange:= nil;
  edSeparatorMargins.OnChange:= nil;
  edBlurRegion.OnChange:= nil;

  edItemsArea.text:= RectToString(theme.ItemsArea);
  edSeparatorMargins.text:= RectToString(theme.Separator.Margins);
  edBlurRegion.text:= theme.BlurRegion;

  edItemsArea.OnChange:= edItemsAreaChange;
  edSeparatorMargins.OnChange:= edItemsAreaChange;
  edBlurRegion.OnChange:= edItemsAreaChange;

  // background //
  edMargin.OnChange:= nil;
  edMargin.text:= RectToString(theme.Background.Margins);
  edMargin.OnChange:= edImageChange;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.edItemsAreaChange(Sender: TObject);
begin
  if assigned(theme) then
  begin
    theme.ItemsArea := StringToRect(edItemsArea.text);
    theme.Separator.Margins := StringToRect(edSeparatorMargins.text);
    theme.BlurRegion := edBlurRegion.Text;
    frmmain.BaseCmd(tcThemeChanged, 0);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.edImageChange(Sender: TObject);
begin
  if assigned(theme) then
  begin
    theme.Background.Margins:= StringToRect(edMargin.text);
    theme.ReloadGraphics;
    frmmain.BaseCmd(tcThemeChanged, 0);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := cafree;
  frmThemeEditor := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.mcloseClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmThemeEditor.msaveClick(Sender: TObject);
begin
  if assigned(theme) then
  try
    if theme.Save then frmmain.notify(UTF8ToAnsi(XMsgThemeSaved));
  except
    on e: Exception do messagebox(Handle, pchar(e.message), declu.PROGRAM_NAME, MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.
