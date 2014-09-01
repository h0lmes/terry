unit frmLayersEditorU;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, themeu, toolu, gdip_gfx, ExtCtrls, Buttons, Menus;

type

  { TfrmLayersEditor }

  TfrmLayersEditor = class(TForm)
    edBlurRegion: TEdit;
    edItemsArea: TEdit;
    edmargin: TEdit;
    edReflectionSize: TEdit;
    edSeparatorMargins: TEdit;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label9: TLabel;
    mclose: TMenuItem;
    msave: TMenuItem;
    mmenu: TMainMenu;
    procedure edItemsAreaChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mcloseClick(Sender: TObject);
    procedure msaveClick(Sender: TObject);
  private
  public
    class procedure StartForm;
  end;

var
  frmLayersEditor: TfrmLayersEditor;

implementation
uses declu, frmterryu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmLayersEditor.StartForm;
begin
  if not assigned(theme) then
  begin
    raise Exception.Create(UTF8ToAnsi(XErrorThemeObjectNotFound));
    exit;
  end;
  if not assigned(frmLayersEditor) then Application.CreateForm(self, frmLayersEditor);
  frmLayersEditor.show;
end;
//------------------------------------------------------------------------------
procedure TfrmLayersEditor.FormShow(Sender: TObject);
begin
  font.name:= GetFont;
  font.size:= GetFontSize;

  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;

  edItemsArea.OnChange:= nil;
  edReflectionSize.OnChange:= nil;
  edSeparatorMargins.OnChange:= nil;
  edBlurRegion.OnChange:= nil;

  edItemsArea.text:= RectToString(theme.ItemsArea);
  edReflectionSize.text:= inttostr(theme.ReflectionSize);
  edSeparatorMargins.text:= RectToString(theme.Separator.Margins);
  edBlurRegion.text:= theme.BlurRegion;

  edItemsArea.OnChange:= edItemsAreaChange;
  edReflectionSize.OnChange:= edItemsAreaChange;
  edSeparatorMargins.OnChange:= edItemsAreaChange;
  edBlurRegion.OnChange:= edItemsAreaChange;

  // background //
  edMargin.OnChange:= nil;
  edMargin.text:= RectToString(theme.Background.Margins);
  edMargin.OnChange:= edImageChange;
end;
//------------------------------------------------------------------------------
procedure TfrmLayersEditor.edItemsAreaChange(Sender: TObject);
begin
  if assigned(theme) then
  begin
    theme.ItemsArea := StringToRect(edItemsArea.text);
    theme.Separator.Margins := StringToRect(edSeparatorMargins.text);
    theme.BlurRegion:= edBlurRegion.Text;
    try theme.ReflectionSize := StrToInt(edReflectionSize.text);
    except theme.ReflectionSize := 0;
    end;
    theme.DoThemeChanged;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmLayersEditor.edImageChange(Sender: TObject);
begin
  if assigned(theme) then
  begin
    theme.Background.Margins:= StringToRect(edMargin.text);
    theme.ReloadGraphics;
    theme.DoThemeChanged;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmLayersEditor.mcloseClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmLayersEditor.msaveClick(Sender: TObject);
begin
  if assigned(theme) then
  try
    if theme.Save then frmterry.notify(UTF8ToAnsi(XMsgThemeSaved));
  except
    on e: Exception do messagebox(Handle, pchar(e.message), 'Terry', MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
end.
