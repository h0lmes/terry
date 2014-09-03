unit frmitemoptu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, GDIPAPI, gdip_gfx;

type
  _uproc = procedure(AData: string) of object;

  { TfrmItemProp }

  TfrmItemProp = class(TForm)
    btnBrowseImage1: TButton;
    btnClearImage: TButton;
    btnDefaultColor: TButton;
    btnDir: TButton;
    btnFile: TButton;
    btnOK: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    btnParams: TButton;
    btnSelectColor: TButton;
    cboRun: TComboBox;
    cboWindow: TComboBox;
    chbHide: TCheckBox;
    edCaption: TEdit;
    edCmd: TEdit;
    edDir: TEdit;
    edImage: TEdit;
    edParams: TEdit;
    iPic: TPaintBox;
    lblTip1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCaption: TLabel;
    lblCommand: TLabel;
    lblDir: TLabel;
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
    procedure cboRunChange(Sender: TObject);
    procedure cboWindowChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure edDirChange(Sender: TObject);
    procedure edParamsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnSelectColorClick(Sender: TObject);
    procedure btn_colorClick(Sender: TObject);
    procedure btnDirClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnParamsClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edCmdChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tbHueChange(Sender: TObject);
  private
    cancel_data: string;
    UpdateItemProc: _uproc;
    color_: uint;
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

  cboWindow.Items.Add(XShowCmdNormal);
  cboWindow.Items.Add(XShowCmdMinimized);
  cboWindow.Items.Add(XShowCmdMaximized);
  cboRun.Items.Add(XProgramActivationDefault);
  cboRun.Items.Add(XProgramActivationActivate);
  cboRun.Items.Add(XProgramActivationRun);
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

  edCaption.Text := AnsiToUTF8(FetchValue(AData, 'caption="', '";'));
  edCmd.Text := AnsiToUTF8(FetchValue(AData, 'command="', '";'));
  edParams.Text := AnsiToUTF8(FetchValue(AData, 'params="', '";'));
  edDir.Text := AnsiToUTF8(FetchValue(AData, 'dir="', '";'));
  edImage.text := FetchValue(AData, 'image="', '";');

  chbHide.Checked := False;
  try chbHide.Checked := boolean(StrToInt(FetchValue(AData, 'hide="', '";')));
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
  cboWindow.ItemIndex := 0;
  if i = sw_showminimized then cboWindow.ItemIndex := 1
  else if i = sw_showmaximized then cboWindow.ItemIndex := 2;

  i := 0;
  try i := StrToInt(FetchValue(AData, 'activate_running="', '";'));
  except
  end;
  cboRun.ItemIndex := i;

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
  if FChanged then
     if assigned(UpdateItemProc) then UpdateItemProc(cancel_data);
  FChanged := false;
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnOKClick(Sender: TObject);
begin
  btnApply.Click;
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

    if cboWindow.ItemIndex = 0 then showcmd := sw_shownormal
    else if cboWindow.ItemIndex = 1 then showcmd := sw_showminimized
    else showcmd := sw_showmaximized;

    str := TShortcutItem.Make(ItemHWnd, UTF8ToAnsi(edCaption.Text),
      UTF8ToAnsi(edCmd.Text), UTF8ToAnsi(edParams.Text), UTF8ToAnsi(edDir.Text),
      UTF8ToAnsi(edImage.Text), showcmd, color_data, chbHide.Checked,
      cboRun.ItemIndex);

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
    if edCmd.Text = '' then
      InitialDir := AnsiToUTF8(sets.progpath)
    else
      InitialDir := ExtractFilePath(toolu.UnzipPath(edCmd.Text));
    if Execute then
    begin
      edCmd.Text := toolu.ZipPath(FileName);
      if fileexists(FileName) then
        edCaption.Text := cut(ExtractFileName(FileName), '.')
      else
        edCaption.Text := edCmd.Text;
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
    InitialDir := ExtractFilePath(toolu.UnzipPath(edParams.Text));
    if Execute then
      edParams.Text := toolu.ZipPath(FileName);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnDirClick(Sender: TObject);
begin
  edDir.Text := AnsiToUTF8(toolu.BrowseFolder(handle, XSelectWorkingDirectory, UTF8ToAnsi(edDir.Text)));
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnBrowseImage1Click(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if edImage.text = '' then InitialDir:= toolu.UnzipPath('%pp%\images')
    else InitialDir:= ExtractFilePath(toolu.UnzipPath(edImage.text));
    if execute then edImage.text := toolu.ZipPath(FileName);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.cboRunChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.cboWindowChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edCaptionChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edCmdChange(Sender: TObject);
begin
  FChanged := true;
  if edImage.text = '' then Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edDirChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edParamsChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edImageChange(Sender: TObject);
begin
  FChanged := true;
  Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnClearImageClick(Sender: TObject);
begin
  edImage.Clear;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnSelectColorClick(Sender: TObject);
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
    img := nil;
    if edImage.text = '' then
      str := UnzipPath(UTF8ToAnsi(edCmd.Text))
    else
      str := UnzipPath(UTF8ToAnsi(edImage.text));

    if fileexists(cut(str, ',')) then LoadImage(str, 128, false, true, img, w, h);
    DrawFit(img, iPic, color_data);
    if assigned(img) then GdipDisposeImage(img);
  except
    on e: Exception do frmterry.notify('frmItemProp.Draw'#10#13 + e.message);
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
  hgdip, hbrush, hattr: Pointer;
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
    GdipFillRectangleI(hgdip, hbrush, 0, 0, p.Width, p.Height);
    GdipDeleteBrush(hbrush);

    if assigned(image) then
    begin
      CreateColorMatrix(color_data, matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);

      GdipDrawImageRectRectI(hgdip, image,
        (p.Width - trunc(p.Width * w_coeff)) div 2, (p.Height - trunc(p.Height * h_coeff)) div 2,
        trunc(p.Width * w_coeff), trunc(p.Height * h_coeff),
        0, 0, w, h, UnitPixel, hattr, nil, nil);

      GdipDisposeImageAttributes(hattr);
    end;

    GdipDeleteGraphics(hgdip);
  except
    on e: Exception do frmterry.notify('frmItemProp.DrawFit'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

