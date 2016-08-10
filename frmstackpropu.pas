unit frmstackpropu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons,
  GDIPAPI, gfx, PIDL, stackitemu, themeu, DividerBevel;

type
  _uproc = procedure(AData: string) of object;

  { TfrmStackProp }

  TfrmStackProp = class(TForm)
    bbAddIcon: TBitBtn;
    bbDelIcon: TBitBtn;
    bbEditIcon: TBitBtn;
    bbIconDown: TBitBtn;
    bbIconUp: TBitBtn;
    btnBrowseImage1: TButton;
    btnClearImage: TButton;
    btnDefaultColor: TButton;
    btnOK: TButton;
    btnApply: TButton;
    btnCancel: TButton;
    btnProperties: TButton;
    btnSelectColor: TButton;
    btnSelectBkColor: TButton;
    cboMode: TComboBox;
    cboPreview: TComboBox;
    chbBackgroundBlur: TCheckBox;
    chbBackground: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    edImage: TEdit;
    edCaption: TEdit;
    iPic: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblBackgroundAlpha: TLabel;
    lblImage: TLabel;
    lblDistort: TLabel;
    lblCaption: TLabel;
    lblAnimationSpeed: TLabel;
    lblStyle: TLabel;
    lblDir: TLabel;
    lblOffset: TLabel;
    list: TListBox;
    pages: TPageControl;
    tbAnimationSpeed: TTrackBar;
    tbDistort: TTrackBar;
    tbBackgroundAlpha: TTrackBar;
    tbOffset: TTrackBar;
    tsProperties: TTabSheet;
    tsColor: TTabSheet;
    tbBr: TTrackBar;
    tbCont: TTrackBar;
    tbHue: TTrackBar;
    tbSat: TTrackBar;
    procedure bbAddIconClick(Sender: TObject);
    procedure bbDelIconClick(Sender: TObject);
    procedure bbEditIconClick(Sender: TObject);
    procedure bbIconDownClick(Sender: TObject);
    procedure bbIconUpClick(Sender: TObject);
    procedure btnBrowseImage1Click(Sender: TObject);
    procedure btnDefaultColorClick(Sender: TObject);
    procedure btnPropertiesClick(Sender: TObject);
    procedure btnSelectBkColorClick(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure edImageChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearImageClick(Sender: TObject);
    procedure btnSelectColorClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
		procedure FormShow(Sender: TObject);
    procedure listDblClick(Sender: TObject);
    procedure tbBackgroundAlphaChange(Sender: TObject);
    procedure tbDistortChange(Sender: TObject);
    procedure tbHueChange(Sender: TObject);
    procedure tbOffsetChange(Sender: TObject);
  private
    cancel_data: string;
    UpdateItemProc: _uproc;
    color_: uint;
    color_data: uint;
    background_color: uint;
    SpecialFolder: string;
    ItemHWnd: uint;
    FChanged: boolean;
    item: TStackItem;
    FImage: Pointer;
    FIW: cardinal;
    FIH: cardinal;
    function SetData(AData: string): boolean;
    procedure ReadSubitems;
    procedure OpenColor;
    procedure iPicPaint(Sender: TObject);
    procedure Draw;
    procedure DrawFit;
  public
    class procedure Open(AData: string; uproc: _uproc; AItem: TStackItem);
  end;

var
  frmStackProp: TfrmStackProp;

{$t+}
implementation
uses declu, toolu, frmmainu, stackmodeu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmStackProp.Open(AData: string; uproc: _uproc; AItem: TStackItem);
begin
  if AData <> '' then
  try
    if not assigned(frmStackProp) then Application.CreateForm(self, frmStackProp);
    if frmStackProp.SetData(AData) then
    begin
      frmStackProp.UpdateItemProc := uproc;
      frmStackProp.item := AItem;
      frmStackProp.ReadSubitems;
      frmStackProp.Show;
      frmStackProp.edCaption.SetFocus;
    end;
  except
    on e: Exception do frmmain.notify('frmStackProp.Open'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.FormCreate(Sender: TObject);
var
  idx: integer;
begin
  FChanged := false;

  for idx := 0 to mc.GetModeCount - 1 do cboMode.Items.Add(mc.GetModeName(idx));
  cboPreview.Clear;
  cboPreview.Items.Add(XStackPreviewNone);
  cboPreview.Items.Add(XStackPreviewFour);
  cboPreview.Items.Add(XStackPreviewNine);
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.FormShow(Sender: TObject);
begin
  font.Name := GetFont;
  font.size := GetFontSize;
  clientheight := pages.top + pages.Height + 7;
  constraints.minheight := Height;
  constraints.maxheight := Height;
  constraints.minwidth := Width;
end;
//------------------------------------------------------------------------------
function TfrmStackProp.SetData(AData: string): boolean;
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
  edImage.Text := FetchValue(AData, 'image="', '";');

  try
    color_data := DEFAULT_COLOR_DATA;
    color_data := toolu.StringToColor(FetchValue(AData, 'color_data="', '";'));
  except end;
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


  cboMode.ItemIndex := 0;
  try cboMode.ItemIndex := strtoint(FetchValue(AData, 'mode="', '";'));
  except end;

  tbOffset.Position := -1;
  tbOffset.Position := 0;
  tbAnimationSpeed.Position := tbAnimationSpeed.Min;
  tbDistort.Position := -1;
  tbDistort.Position := 0;
  try tbOffset.Position := strtoint(FetchValue(AData, 'offset="', '";'));
  except end;
  try tbAnimationSpeed.Position := strtoint(FetchValue(AData, 'animation_speed="', '";'));
  except end;
  try tbDistort.Position := strtoint(FetchValue(AData, 'distort="', '";'));
  except end;
  cboPreview.ItemIndex := 1;
  try cboPreview.ItemIndex := strtoint(FetchValue(AData, 'preview="', '";'));
  except end;
  try chbBackground.checked := FetchValue(AData, 'background="', '";') = '1';
  except end;
  try chbBackgroundBlur.checked := FetchValue(AData, 'background_blur="', '";') = '1';
  except end;
  try
    background_color := DEFAULT_STACK_BGCOLOR;
    background_color := toolu.StringToColor(FetchValue(AData, 'background_color="', '";'));
  except end;
  tbBackgroundAlpha.Position := (background_color and $ff000000 shr 24) * 100 div 255;

  SpecialFolder := FetchValue(AData, 'special_folder="', '";');

  Draw;

  iPic.OnPaint := iPicPaint;

  // reset 'changed' state //
  FChanged := false;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.ReadSubitems;
var
  idx: integer;
begin
  list.Items.BeginUpdate;
  list.Clear;
  if item.ItemCount > 0 then
    for idx := 0 to item.ItemCount - 1 do
      list.Items.Add(AnsiToUTF8(item.GetSubitemCaption(idx)));
  list.Items.EndUpdate;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.bbIconUpClick(Sender: TObject);
var
  idx: integer;
begin
  idx := list.ItemIndex;
  item.SubitemMoveUp(idx);
  ReadSubitems;
  if idx > 0 then dec(idx);
  list.ItemIndex := idx;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.bbIconDownClick(Sender: TObject);
var
  idx: integer;
begin
  idx := list.ItemIndex;
  item.SubitemMoveDown(idx);
  ReadSubitems;
  if idx < list.Items.Count - 1 then inc(idx);
  list.ItemIndex := idx;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.bbAddIconClick(Sender: TObject);
begin
  item.AddSubitemDefault;
  ReadSubitems;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.bbDelIconClick(Sender: TObject);
begin
  item.DeleteSubitem(list.ItemIndex);
  ReadSubitems;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.bbEditIconClick(Sender: TObject);
begin
  item.SubitemConfigure(list.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.listDblClick(Sender: TObject);
begin
  bbEditIcon.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then btnCancel.Click;
  if (key = 13) and (shift = []) then btnOK.Click;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnCancelClick(Sender: TObject);
begin
  if FChanged then
    if assigned(UpdateItemProc) then UpdateItemProc(cancel_data);
  FChanged := false;
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnOKClick(Sender: TObject);
begin
  btnApply.Click;
  // save settings !!!
  frmmain.BaseCmd(tcSaveSets, 0);
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnApplyClick(Sender: TObject);
var
  str: string;
begin
  try
    FChanged := false;
    str := TStackItem.Make(ItemHWnd, UTF8ToAnsi(edCaption.Text), UTF8ToAnsi(edImage.Text), SpecialFolder,
      color_data, cboMode.ItemIndex, tbOffset.Position, tbAnimationSpeed.Position, tbDistort.Position, cboPreview.ItemIndex,
      chbBackground.Checked, chbBackgroundBlur.Checked, background_color);
    if assigned(UpdateItemProc) then UpdateItemProc(str);
  except
    on e: Exception do frmmain.notify('frmStackProp.btnApplyClick'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try if assigned(FImage) then GdipDisposeImage(FImage);
  except end;
  FImage := nil;
  action := caFree;
  frmStackProp := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  postmessage(handle, $a1, 2, 0);
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnBrowseImage1Click(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if edImage.Text = '' then InitialDir:= toolu.UnzipPath('%pp%\images')
    else InitialDir:= ExtractFilePath(toolu.UnzipPath(edImage.Text));
    if execute then edImage.Text := toolu.ZipPath(FileName);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.edCaptionChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.edImageChange(Sender: TObject);
begin
  FChanged := true;
  Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.tbOffsetChange(Sender: TObject);
begin
  lblOffset.Caption := Format(XOffsetOfIcons, [TTrackBar(Sender).Position]);
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.tbDistortChange(Sender: TObject);
begin
  lblDistort.Caption := Format(XDistort, [TTrackBar(Sender).Position]);
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnSelectBkColorClick(Sender: TObject);
begin
  with TColorDialog.Create(self) do
  begin
    Color := gfx.SwapColor(background_color and $ffffff);
    if Execute then background_color := (background_color and $ff000000) + gfx.SwapColor(Color and $ffffff);
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.tbBackgroundAlphaChange(Sender: TObject);
begin
  lblBackgroundAlpha.Caption := Format(XAlphaChannel, [TTrackBar(Sender).Position]);
  background_color := (background_color and $ffffff) + TTrackBar(Sender).Position * 255 div 100 shl 24;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnClearImageClick(Sender: TObject);
begin
  edImage.Clear;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnSelectColorClick(Sender: TObject);
begin
  OpenColor;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.OpenColor;
begin
  pages.ActivePageIndex := 1;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnPropertiesClick(Sender: TObject);
begin
  pages.ActivePageIndex := 0;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.tbHueChange(Sender: TObject);
begin
  FChanged := true;
  color_data := byte(tbHue.Position) +
    byte(tbSat.Position) shl 8 +
    byte(tbBr.Position) shl 16 +
    byte(tbCont.Position) shl 24;
  DrawFit;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.btnDefaultColorClick(Sender: TObject);
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
procedure TfrmStackProp.Draw;
var
  str: string;
  apidl: PItemIDList;
begin
  try
    try if assigned(FImage) then GdipDisposeImage(FImage);
    except end;
    FImage := nil;

    str := UnzipPath(UTF8ToAnsi(edImage.Text));
    LoadImage(str, 128, true, false, FImage, FIW, FIH);

    // default stack image //
    if not assigned(FImage) then
    begin
      FImage := theme.Stack.Image;
      DownscaleImage(Fimage, 128, true, FIW, FIH, false);
    end;

    DrawFit;
  except
    on e: Exception do frmmain.notify('frmStackProp.Draw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.iPicPaint(Sender: TObject);
begin
  DrawFit;
end;
//------------------------------------------------------------------------------
procedure TfrmStackProp.DrawFit;
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
    on e: Exception do frmmain.notify('frmStackProp.DrawFit'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

