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
    btnProperties: TButton;
    btnConvertLink: TButton;
    cboWindow: TComboBox;
    chbHide: TCheckBox;
    edCaption: TEdit;
    edCmd: TEdit;
    edDir: TEdit;
    edImage: TEdit;
    edParams: TEdit;
    iPic: TPaintBox;
    lblTip: TLabel;
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
    lblImage: TLabel;
    pages: TPageControl;
    tsProperties: TTabSheet;
    tsColor: TTabSheet;
    tbBr: TTrackBar;
    tbCont: TTrackBar;
    tbHue: TTrackBar;
    tbSat: TTrackBar;
    procedure btnBrowseImage1Click(Sender: TObject);
    procedure btnConvertLinkClick(Sender: TObject);
    procedure btnDefaultColorClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure cboWindowChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure edDirChange(Sender: TObject);
    procedure edParamsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnSelectColorClick(Sender: TObject);
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
    FImage: Pointer;
    FIW: cardinal;
    FIH: cardinal;
    function SetData(AData: string): boolean;
    procedure OpenColor;
    procedure iPicPaint(Sender: TObject);
    procedure Draw;
    procedure DrawFit;
  public
    class procedure Open(AData: string; uproc: _uproc);
  end;

var
  frmItemProp: TfrmItemProp;

{$t+}
implementation
uses declu, scitemu, PIDL, toolu, setsu, frmmainu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmItemProp.Open(AData: string; uproc: _uproc);
begin
  if AData <> '' then
  try
    if not assigned(frmItemProp) then Application.CreateForm(self, frmItemProp);
    if frmItemProp.SetData(AData) then
    begin
      frmItemProp.UpdateItemProc := uproc;
      frmItemProp.Show;
      frmItemProp.edCaption.SetFocus;
    end;
  except
    on e: Exception do frmmain.notify('TfrmItemProp.Open'#10#13 + e.message);
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
end;
//------------------------------------------------------------------------------
function TfrmItemProp.SetData(AData: string): boolean;
var
  scmd: integer;
begin
  result := false;
  if FChanged then
    if not confirm(Handle, UTF8ToAnsi(XMsgUnsavedIconParams)) then exit;
  result := true;

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

  scmd := 0;
  try scmd := StrToInt(FetchValue(AData, 'showcmd="', '";'));
  except
  end;
  cboWindow.ItemIndex := 0;
  if scmd = sw_showminimized then cboWindow.ItemIndex := 1
  else if scmd = sw_showmaximized then cboWindow.ItemIndex := 2;

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
  frmmain.BaseCmd(tcSaveSets, 0);
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
      UTF8ToAnsi(edImage.Text), showcmd, color_data, chbHide.Checked);

    if assigned(UpdateItemProc) then UpdateItemProc(str);
  except
    on e: Exception do frmmain.notify('TfrmItemProp.btnApplyClick'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try if assigned(FImage) then GdipDisposeImage(FImage);
  except end;
  FImage := nil;
  action := cafree;
  frmItemProp := nil;
  //SetActiveWindow(frmmain.handle);
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnPropertiesClick(Sender: TObject);
begin
  pages.ActivePageIndex := 0;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnFileClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if edCmd.Text = '' then
      InitialDir := AnsiToUTF8(UnzipPath('%pp%'))
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
  edDir.Text := AnsiToUTF8(toolu.BrowseFolder(handle, UTF8ToAnsi(XSelectWorkingDirectory), UTF8ToAnsi(edDir.Text)));
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnBrowseImage1Click(Sender: TObject);
var
  img: string;
begin
  with TOpenDialog.Create(self) do
  try
    img := toolu.UnzipPath(cut(edImage.text, ';')); // take only first image
    if img = '' then InitialDir := toolu.UnzipPath('%pp%\images')
    else
    begin
      InitialDir := ExtractFilePath(img);
      Filename := ExtractFileName(img);
    end;
    Options := Options + [ofAllowMultiSelect];
    if execute then
    begin
      edImage.text := toolu.ZipPath(Filename);
      if Files.Count > 1 then
        edImage.text := edImage.text + ';' + toolu.ZipPath(Files.strings[1]);
    end;
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnConvertLinkClick(Sender: TObject);
var
  cmd, param, dir, icon: string;
begin
  if SameText(ExtractFileExt(edCmd.Text), '.lnk') then
  begin
    cmd := UnzipPath(edCmd.Text);
    ResolveLNK(handle, cmd, param, dir, icon);
    edCmd.Text := ZipPath(cmd);
    edParams.Text := ZipPath(param);
    edDir.Text := ZipPath(dir);
    edImage.text := ZipPath(icon);
  end;
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
  btnConvertLink.Visible := SameText(ExtractFileExt(edCmd.Text), '.lnk');
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
  DrawFit;
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
  apidl: PItemIDList;
begin
  try
    if edImage.text = '' then
      str := UnzipPath(UTF8ToAnsi(cut(edCmd.Text, ';'))) // take only first command
    else
      str := UnzipPath(UTF8ToAnsi(cut(edImage.text, ';'))); // take only first image

    try if assigned(FImage) then GdipDisposeImage(FImage);
    except end;
    FImage := nil;

    apidl := nil;
    if IsGUID(str) then apidl := PIDL_GetFromPath(pchar(str));
    if IsPIDLString(str) then apidl := PIDL_FromString(str);
    if assigned(apidl) then
    begin
      LoadImageFromPIDL(apidl, 128, true, true, FImage, FIW, FIH);
      PIDL_Free(apidl);
    end else begin
      LoadImage(str, 128, true, true, FImage, FIW, FIH);
    end;

    DrawFit;
  except
    on e: Exception do frmmain.notify('frmItemProp.Draw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.iPicPaint(Sender: TObject);
begin
  DrawFit;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.DrawFit;
var
  hgdip, hbrush, hattr: Pointer;
  w_coeff, h_coeff: extended;
  matrix: ColorMatrix;
  background: cardinal;
begin
  if assigned(FImage) then
  try
    w_coeff := 1;
    h_coeff := 1;
    try
      if FIW / FIH > (iPic.Width - 2) / (iPic.Height - 2) then h_coeff := (iPic.Width - 2) * FIH / FIW / (iPic.Height - 2);
      if FIW / FIH < (iPic.Width - 2) / (iPic.Height - 2) then w_coeff := (iPic.Height - 2) * FIW / FIH / (iPic.Width - 2);
    except
    end;

    GdipCreateFromHDC(iPic.canvas.handle, hgdip);
    GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);

    background := GetRGBColorResolvingParent;
    background := SwapColor(background) or $ff000000;
    GdipCreateSolidFill(background, hbrush);
    GdipFillRectangleI(hgdip, hbrush, 0, 0, iPic.Width, iPic.Height);
    GdipDeleteBrush(hbrush);

    CreateColorMatrix(color_data, matrix);
    GdipCreateImageAttributes(hattr);
    GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @matrix, nil, ColorMatrixFlagsDefault);

    GdipDrawImageRectRectI(hgdip, FImage,
      (iPic.Width - trunc(iPic.Width * w_coeff)) div 2, (iPic.Height - trunc(iPic.Height * h_coeff)) div 2,
      trunc(iPic.Width * w_coeff), trunc(iPic.Height * h_coeff),
      0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);

    GdipDisposeImageAttributes(hattr);
    GdipDeleteGraphics(hgdip);
  except
    on e: Exception do frmmain.notify('frmItemProp.DrawFit'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

