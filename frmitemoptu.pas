unit frmitemoptu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, GDIPAPI, gdip_gfx;

type
  _uproc = procedure(AData: string) of object;

  { TfrmItemProp }

  TfrmItemProp = class(TForm)
    btnDefaultColor: TButton;
    btnDir: TButton;
    btnFile: TButton;
    btnOK: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    btnBrowseImage1: TButton;
    btnParams: TButton;
    cbo_activate_running: TComboBox;
    cbo_showcmd: TComboBox;
    chb_hide: TCheckBox;
    ed_caption: TEdit;
    ed_cmd: TEdit;
    ed_dir: TEdit;
    edPic: TEdit;
    ed_params: TEdit;
    lblTip1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCaption: TLabel;
    lblCommand: TLabel;
    lblDir: TLabel;
    gbImage: TGroupBox;
    iPic: TPaintBox;
    btnClearImage: TButton;
    btnColorData: TButton;
    lblParams: TLabel;
    lblWorkingDirectory: TLabel;
    lblWindowSize: TLabel;
    lblRun: TLabel;
    lblImage: TLabel;
    pages: TPageControl;
    tsProperties: TTabSheet;
    tsColor: TTabSheet;
    tbBr: TTrackBar;
    tbCont: TTrackBar;
    tbHue: TTrackBar;
    tbSat: TTrackBar;
    procedure btnBrowseImage1Click(Sender: TObject);
    procedure btnDefaultColorClick(Sender: TObject);
    procedure cbo_activate_runningChange(Sender: TObject);
    procedure cbo_showcmdChange(Sender: TObject);
    procedure ed_captionChange(Sender: TObject);
    procedure ed_dirChange(Sender: TObject);
    procedure ed_paramsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnColorDataClick(Sender: TObject);
    procedure btn_colorClick(Sender: TObject);
    procedure btnDirClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnParamsClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure ed_cmdChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbHueChange(Sender: TObject);
  private
    cancel_data: string;
    UpdateItemProc: _uproc;
    color_: uint;
    image1: string;
    color_data: integer;
    ItemHWnd: uint;
    FChanged: boolean;
    procedure SetData(AData: string);
    procedure OpenColor;
    procedure iPicPaint(Sender: TObject);
    procedure Draw;
    procedure DrawFit(image: Pointer; p: TPaintBox; color_data: integer);
  public
    class procedure Open(AData: string; uproc: _uproc);
  end;

var
  frmItemProp: TfrmItemProp;

{$t+}
implementation
uses declu, scitemu, PIDL, toolu, setsu, frmterryu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmItemProp.Open(AData: string; uproc: _uproc);
begin
  if AData <> '' then
  try
    if not assigned(frmItemProp) then Application.CreateForm(self, frmItemProp);
    frmItemProp.UpdateItemProc := uproc;
    frmItemProp.SetData(AData);
    frmItemProp.Show;
  except
    on e: Exception do frmterry.notify('TfrmItemProp.Open'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormCreate(Sender: TObject);
begin
  FChanged := false;

  font.Name := GetFont;
  font.size := GetFontSize;
  clientheight := btnOK.top + btnOK.Height + 7;
  constraints.minheight := Height;
  constraints.maxheight := Height;
  constraints.minwidth := Width;

  cbo_showcmd.Items.Add(XShowCmdNormal);
  cbo_showcmd.Items.Add(XShowCmdMinimized);
  cbo_showcmd.Items.Add(XShowCmdMaximized);
  cbo_activate_running.Items.Add(XProgramActivationDefault);
  cbo_activate_running.Items.Add(XProgramActivationActivate);
  cbo_activate_running.Items.Add(XProgramActivationRun);
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.SetData(AData: string);
var
  i: integer;
begin
  if FChanged then
    if not confirm(Handle, UTF8ToAnsi(XMsgUnsavedIconParams)) then exit;

  cancel_data := AData;
  try ItemHWnd := strtoint(FetchValue(AData, 'hwnd="', '";'));
  except end;
  pages.ActivePageIndex := 0;

  // show parameters //

  ed_caption.Text := AnsiToUTF8(FetchValue(AData, 'caption="', '";'));
  ed_cmd.Text := AnsiToUTF8(FetchValue(AData, 'command="', '";'));
  ed_params.Text := AnsiToUTF8(FetchValue(AData, 'params="', '";'));
  ed_dir.Text := AnsiToUTF8(FetchValue(AData, 'dir="', '";'));
  image1 := FetchValue(AData, 'image="', '";');

  chb_hide.Checked := False;
  try chb_hide.Checked := boolean(StrToInt(FetchValue(AData, 'hide="', '";')));
  except
  end;

  color_data := DEFAULT_COLOR_DATA;
  try color_data := toolu.StringToColor(FetchValue(AData, 'color_data="', '";'));
  except
  end;
  tbHue.OnChange := nil;
  tbSat.OnChange := nil;
  tbBr.OnChange := nil;
  tbCont.OnChange := nil;
  tbHue.position := byte(color_data);
  tbSat.position := byte(color_data shr 8);
  tbBr.position := byte(color_data shr 16);
  tbCont.position := byte(color_data shr 24);
  tbHue.OnChange := tbHueChange;
  tbSat.OnChange := tbHueChange;
  tbBr.OnChange := tbHueChange;
  tbCont.OnChange := tbHueChange;

  i := 0;
  try i := StrToInt(FetchValue(AData, 'showcmd="', '";'));
  except
  end;
  cbo_showcmd.ItemIndex := 0;
  if i = sw_showminimized then cbo_showcmd.ItemIndex := 1
  else if i = sw_showmaximized then cbo_showcmd.ItemIndex := 2;

  i := 0;
  try i := StrToInt(FetchValue(AData, 'activate_running="', '";'));
  except
  end;
  cbo_activate_running.ItemIndex := i;

  Draw;

  iPic.OnPaint := iPicPaint;

  // reset 'changed' state //
  FChanged := false;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then btnCancel.Click;
  if (key = 13) and (shift = []) then btnOk.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnCancelClick(Sender: TObject);
begin
  FChanged := false;
  if assigned(UpdateItemProc) then UpdateItemProc(cancel_data);
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnOKClick(Sender: TObject);
begin
  btnApplyClick(nil);
  // save settings !!!
  frmterry.BaseCmd(tcSaveSets, 0);
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnApplyClick(Sender: TObject);
var
  showcmd: integer;
  str: string;
begin
  try
    FChanged := false;

    if cbo_showcmd.ItemIndex = 0 then showcmd := sw_shownormal
    else if cbo_showcmd.ItemIndex = 1 then showcmd := sw_showminimized
    else showcmd := sw_showmaximized;

    str := TShortcutItem.Make(ItemHWnd, UTF8ToAnsi(ed_caption.Text),
      UTF8ToAnsi(ed_cmd.Text), UTF8ToAnsi(ed_params.Text), UTF8ToAnsi(ed_dir.Text),
      image1, showcmd, color_data, chb_hide.Checked,
      cbo_activate_running.ItemIndex);

    if assigned(UpdateItemProc) then UpdateItemProc(str);
  except
    on e: Exception do frmterry.notify('TfrmItemProp.btnApplyClick'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetActiveWindow(frmterry.handle);
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btn_colorClick(Sender: TObject);
begin
  with TColorDialog.Create(self) do
  begin
    color := color_ and $ffffff;
    if Execute then
    begin
      FChanged := true;
      color_ := uint(Color) or $ff000000;
    end;
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if ed_cmd.Text = '' then
      InitialDir := AnsiToUTF8(sets.progpath)
    else
      InitialDir := ExtractFilePath(toolu.UnzipPath(ed_cmd.Text));
    if Execute then
    begin
      ed_cmd.Text := toolu.ZipPath(FileName);
      if fileexists(FileName) then
        ed_caption.Text := cut(ExtractFileName(FileName), '.')
      else
        ed_caption.Text := ed_cmd.Text;
    end;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnParamsClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    InitialDir := ExtractFilePath(toolu.UnzipPath(ed_params.Text));
    if Execute then
      ed_params.Text := toolu.ZipPath(FileName);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnDirClick(Sender: TObject);
begin
  ed_dir.Text := AnsiToUTF8(toolu.BrowseFolder(handle, XSelectWorkingDirectory, UTF8ToAnsi(ed_dir.Text)));
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnBrowseImage1Click(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if image1 = '' then InitialDir:= toolu.UnzipPath('%pp%\images')
    else InitialDir:= ExtractFilePath(toolu.UnzipPath(image1));
    if execute then
    begin
      FChanged := true;
      image1 := toolu.ZipPath(FileName);
      Draw;
    end;
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.cbo_activate_runningChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.cbo_showcmdChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.ed_captionChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.ed_dirChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.ed_paramsChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnClearImageClick(Sender: TObject);
begin
  FChanged := true;
  image1 := '';
  draw;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnColorDataClick(Sender: TObject);
begin
  OpenColor;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.OpenColor;
begin
  pages.ActivePageIndex := 1;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.tbHueChange(Sender: TObject);
begin
  FChanged := true;
  color_data := byte(tbHue.Position) +
    byte(tbSat.Position) shl 8 +
    byte(tbBr.Position) shl 16 +
    byte(tbCont.Position) shl 24;
  Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnDefaultColorClick(Sender: TObject);
begin
  color_data := DEFAULT_COLOR_DATA;

  tbHue.OnChange:= nil;
  tbSat.OnChange:= nil;
  tbBr.OnChange:= nil;
  tbCont.OnChange:= nil;
  tbHue.position:= byte(color_data);
  tbSat.position:= byte(color_data shr 8);
  tbBr.position:= byte(color_data shr 16);
  tbCont.position:= byte(color_data shr 24);
  tbHue.OnChange:= tbHueChange;
  tbSat.OnChange:= tbHueChange;
  tbBr.OnChange:= tbHueChange;
  tbCont.OnChange:= tbHueChange;

  tbHueChange(nil);
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.Draw;
var
  str: string;
  img: Pointer;
  w, h: uint;
begin
  try
    edPic.Text := image1;
    img := nil;
    if image1 = '' then
      str := UnzipPath(UTF8ToAnsi(ed_cmd.Text))
    else
      str := UnzipPath(image1);

    if fileexists(cut(str, ',')) then LoadImage(str, 128, false, true, img, w, h);
    DrawFit(img, iPic, color_data);
    if assigned(img) then GdipDisposeImage(img);
  except
    on e: Exception do frmterry.notify('TfrmItemProp.Draw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.iPicPaint(Sender: TObject);
begin
  Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.DrawFit(image: Pointer; p: TPaintBox; color_data: integer);
var
  hgdip, hbrush, hpen, hattr: Pointer;
  w, h: uint;
  w_coeff, h_coeff: extended;
  matrix: ColorMatrix;
begin
  try
    if assigned(image) then
    begin
      GdipGetImageWidth(image, w);
      GdipGetImageHeight(image, h);
    end else begin
      w := 128;
      h := 128;
    end;

    w_coeff := 1;
    h_coeff := 1;
    try
      if w / h > (p.Width - 2) / (p.Height - 2) then h_coeff := (p.Width - 2) * h / w / (p.Height - 2);
      if w / h < (p.Width - 2) / (p.Height - 2) then w_coeff := (p.Height - 2) * w / h / (p.Width - 2);
    except
    end;

    GdipCreateFromHDC(p.canvas.handle, hgdip);
    GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);

    GdipCreateSolidFill($ffe6e8ea, hbrush);
    GdipFillRectangleI(hgdip, hbrush, 0, 0, p.Width - 1, p.Height - 1);
    GdipDeleteBrush(hbrush);
    GdipCreatePen1($ff303030, 1, UnitPixel, hpen);
    GdipDrawRectangleI(hgdip, hpen, 0, 0, p.Width - 1, p.Height - 1);
    if not assigned(image) then
    begin
      GdipDrawLine(hgdip, hpen, 0, 0, p.Width - 1, p.Height - 1);
      GdipDrawLine(hgdip, hpen, p.Width - 1, 0, 0, p.Height - 1);
    end;
    GdipDeletePen(hpen);

    if assigned(image) then
    begin
      CreateColorMatrix(color_data, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap,
        True, @matrix, nil, ColorMatrixFlagsDefault);

      GdipDrawImageRectRectI(hgdip, image,
        (p.Width - trunc(p.Width * w_coeff)) div 2,
        (p.Height - trunc(p.Height * h_coeff)) div 2,
        trunc(p.Width * w_coeff), trunc(p.Height * h_coeff),
        0, 0, w, h, UnitPixel, hattr, nil, nil);

      GdipDisposeImageAttributes(hattr);
    end;

    GdipDeleteGraphics(hgdip);
  except
    on e: Exception do frmterry.notify('TfrmItemProp.DrawFit'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.ed_cmdChange(Sender: TObject);
begin
  FChanged := true;
  if image1 = '' then Draw;
end;
//------------------------------------------------------------------------------
end.

