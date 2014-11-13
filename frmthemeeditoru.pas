unit frmthemeeditoru;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, themeu, toolu, gdip_gfx, ExtCtrls, Buttons, Menus;

type

  { TfrmThemeEditor }

  TfrmThemeEditor = class(TForm)
    edBlurRegion: TEdit;
    edItemsArea: TEdit;
    edmargin: TEdit;
    edReflectionSize: TEdit;
    edSeparatorMargins: TEdit;
    edBaseOffset: TEdit;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label9: TLabel;
    mclose: TMenuItem;
    msave: TMenuItem;
    mmenu: TMainMenu;
    procedure edItemsAreaChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  edReflectionSize.OnChange:= nil;
  edBaseOffset.OnChange:= nil;
  edSeparatorMargins.OnChange:= nil;
  edBlurRegion.OnChange:= nil;

  edItemsArea.text:= RectToString(theme.ItemsArea);
  edReflectionSize.text:= inttostr(theme.ReflectionSize);
  edBaseOffset.text:= inttostr(theme.BaseOffset);
  edSeparatorMargins.text:= RectToString(theme.Separator.Margins);
  edBlurRegion.text:= theme.BlurRegion;

  edItemsArea.OnChange:= edItemsAreaChange;
  edReflectionSize.OnChange:= edItemsAreaChange;
  edBaseOffset.OnChange:= edItemsAreaChange;
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
    try theme.ReflectionSize := StrToInt(edReflectionSize.text);
    except theme.ReflectionSize := 0;
    end;
    try theme.BaseOffset := StrToInt(edBaseOffset.text);
    except theme.BaseOffset := 10;
    end;
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
    on e: Exception do messagebox(Handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.
