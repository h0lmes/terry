unit frmFontU;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, EColor, gdip_gfx;

type
  TUProc = procedure(afont: _FontData) of object;

  { TfrmFont }

  TfrmFont = class(TForm)
    btnok: TButton;
    btnCancel: TButton;
    rb_text: TRadioButton;
    rb_outline: TRadioButton;
    listFont: TListBox;
    gbAttributes: TGroupBox;
    sbtn_bold: TSpeedButton;
    sbtn_italic: TSpeedButton;
    sbtn_outline: TSpeedButton;
    edFontSize: TEdit;
    ud: TUpDown;
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ok(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnokClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure rb_textClick(Sender: TObject);
    procedure cbarChange(Sender: TObject);
  private
    cbar: TEColor;
    FFont: _FontData;
    FCancelFont: _FontData;
    ok_done: boolean;
    callback: TUProc;
  public
    class function StartForm(var AFont: _FontData; uproc: TUProc): boolean;
  end;

var
  frmFont: TfrmFont;

implementation

uses toolu, frmterryu;

{$R *.lfm}
//------------------------------------------------------------------------------
class function TfrmFont.StartForm(var AFont: _FontData; uproc: TUProc): boolean;
begin
  result := false;
  try
    if not assigned(frmFont) then Application.CreateForm(self, frmFont);

    with frmFont do
    begin
      callback:= uproc;

      font.Name := GetFont;
      font.size := GetFontSize;

      listFont.Items := screen.fonts;
      CopyFontData(AFont, FCancelFont);
      CopyFontData(AFont, FFont);

      listFont.OnClick := nil;
      edFontSize.OnChange := nil;
      listFont.ItemIndex := listFont.items.IndexOf(PChar(@AFont.Name));
      ud.position := AFont.size;
      listFont.OnClick := ok;
      edFontSize.OnChange := ok;

      sbtn_bold.down := AFont.bold;
      sbtn_Italic.down := AFont.Italic;
      sbtn_Outline.down := AFont.Outline;

      rb_textClick(nil);

      ok_done := False;

      result:= ShowModal = mrOk;

      if result then CopyFontData(FFont, AFont);
    end;
  except
    on e: Exception do frmterry.notify('frmFont.StartForm'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.FormCreate(Sender: TObject);
begin
  try
    cbar := TEColor.Create(self);
    with cbar do
    begin
      Left := 245;
      Top := 110;
      Width := 281;
      Height := 241;
      Hue := 0;
      Lightness := 0;
      Saturation := 0;
      BorderColor := clBlack;
      VSpace := 8;
      HSpace := 8;
      SplitWidth := 0;
      OnChange := cbarChange;
      Parent := frmFont;
    end;
  except
    on e: Exception do frmterry.notify('frmFont.FormCreate'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.FormPaint(Sender: TObject);
begin
  cbar.Paint;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.ok(Sender: TObject);
begin
  try
    ok_done:= true;
    if listFont.ItemIndex >= 0 then StrCopy(pchar(@FFont.Name), PChar(listFont.Items[listFont.ItemIndex]));
    FFont.size := StrToInt(edFontSize.Text);
    FFont.bold := sbtn_bold.down;
    FFont.italic := sbtn_Italic.down;
    FFont.outline := sbtn_Outline.down;
    if assigned(callback) then callback(FFont);
    Activate;
  except
    on e: Exception do frmterry.notify('frmFont.ok'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  callback := nil;
  listFont.Clear;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then Close;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.btnokClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.btnCancelClick(Sender: TObject);
begin
  if assigned(callback) and ok_done then callback(FCancelFont);
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.rb_textClick(Sender: TObject);
begin
  cbar.OnChange := nil;
  if rb_text.Checked then cbar.Color := gdip_gfx.SwapColor(FFont.color)
  else cbar.Color := gdip_gfx.SwapColor(FFont.color_outline);
  cbar.OnChange := cbarChange;
end;
//------------------------------------------------------------------------------
procedure TfrmFont.cbarChange(Sender: TObject);
begin
  if rb_text.Checked then FFont.color := $ff000000 or gdip_gfx.swapcolor(cbar.Color)
  else FFont.color_outline := $ff000000 or gdip_gfx.swapcolor(cbar.Color);
  ok(nil);
end;
//------------------------------------------------------------------------------
end.

