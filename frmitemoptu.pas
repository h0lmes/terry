unit frmitemoptu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, GDIPAPI,
  gfx;

type
  { TfrmItemProp }

  TfrmItemProp = class(TForm)
    btnBrowseImage1: TButton;
    btnClearImage: TButton;
    btnDefaultColor: TButton;
    btnDir: TButton;
    btnImage: TButton;
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
    procedure chbHideChange(Sender: TObject);
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
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure FormShow(Sender: TObject);
    procedure tbHueChange(Sender: TObject);
  private
    savedCaption: WideString;
    savedColorData: integer;
    savedCommand: string;
    savedParams: string;
    savedDir: string;
    savedImageFile: string;
    savedImageFile2: string;
    savedShowCmd: integer;
    savedHide: boolean;
    //
    color_data: integer;
    ItemHWnd: THandle;
    FChanged: boolean;
    FImage: Pointer;
    FIW: cardinal;
    FIH: cardinal;
    function SetData(itemWnd: HWND): boolean;
    procedure OpenColor;
    procedure iPicPaint(Sender: TObject);
    procedure Draw;
    procedure DrawFit;
  public
    class procedure Open(itemWnd: HWND);
  end;

var
  frmItemProp: TfrmItemProp;

{$t+}
implementation
uses declu, scitemu, stacksubitemu, PIDL, toolu, frmmainu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmItemProp.Open(itemWnd: HWND);
begin
  try
    if not assigned(frmItemProp) then Application.CreateForm(self, frmItemProp);
    if frmItemProp.SetData(itemWnd) then
    begin
      frmItemProp.Show;
      frmItemProp.edCaption.SetFocus;
    end;
  except
    on e: Exception do frmmain.err('TfrmItemProp.Open', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormCreate(Sender: TObject);
begin
  FChanged := false;

  cboWindow.Items.Add(XShowCmdNormal);
  cboWindow.Items.Add(XShowCmdMinimized);
  cboWindow.Items.Add(XShowCmdMaximized);
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormShow(Sender: TObject);
begin
  font.Name := GetFont;
  font.size := GetFontSize;
  clientheight := btnOK.top + btnOK.Height + 7;
  constraints.minheight := Height;
  constraints.maxheight := Height;
  constraints.minwidth := Width;
end;
//------------------------------------------------------------------------------
function TfrmItemProp.SetData(itemWnd: HWND): boolean;
var
  Inst: TObject = nil;
  sci: TShortcutItem = nil;
  scs: TShortcutSubitem = nil;
begin
  // checks

  result := false;

  if FChanged then
    if not confirm(Handle, UTF8Decode(XMsgUnsavedIconParams)) then exit;

  Inst := TObject(GetWindowLongPtr(itemWnd, GWL_USERDATA));
  if not (Inst is TShortcutItem) and not (Inst is TShortcutSubitem) then exit;

  result := true;
  pages.ActivePageIndex := 0;
  if Inst is TShortcutItem then sci := TShortcutItem(Inst);
  if Inst is TShortcutSubitem then scs := TShortcutSubitem(Inst);

  // read item data

  ItemHWnd := itemWnd;
  if sci <> nil then
  begin
    savedCaption    := sci.Caption;
    savedCommand    := sci.Command;
    savedParams     := sci.Params;
    savedDir        := sci.Dir;
    savedImageFile  := sci.ImageFile;
    savedImageFile2 := sci.ImageFile2;
    savedShowCmd    := sci.ShowCmd;
    savedColorData  := sci.ColorData;
    savedHide       := sci.Hide;
  end
  else
  if scs <> nil then
  begin
    savedCaption    := scs.Caption;
    savedCommand    := scs.Command;
    savedParams     := scs.Params;
    savedDir        := scs.Dir;
    savedImageFile  := scs.ImageFile;
    savedImageFile2 := '';
    savedShowCmd    := scs.ShowCmd;
    savedColorData  := scs.ColorData;
    savedHide       := scs.Hide;
  end;

  // show parameters //

  edCaption.Text    := UTF8Encode(savedCaption);
  edCmd.Text        := AnsiToUTF8(savedCommand);
  edParams.Text     := AnsiToUTF8(savedParams);
  edDir.Text        := AnsiToUTF8(savedDir);
  edImage.text      := savedImageFile;
  if savedImageFile2 <> '' then
       edImage.text := edImage.text + ';' + savedImageFile2;
  chbHide.Checked   := savedHide;
  //
  color_data        := savedColorData;
  tbHue.OnChange    := nil;
  tbSat.OnChange    := nil;
  tbBr.OnChange     := nil;
  tbCont.OnChange   := nil;
  tbHue.position    := byte(color_data);
  tbSat.position    := byte(color_data shr 8);
  tbBr.position     := byte(color_data shr 16);
  tbCont.position   := byte(color_data shr 24);
  tbHue.OnChange    := tbHueChange;
  tbSat.OnChange    := tbHueChange;
  tbBr.OnChange     := tbHueChange;
  tbCont.OnChange   := tbHueChange;
  //
  cboWindow.ItemIndex := 0;
  if savedShowCmd = sw_showminimized then cboWindow.ItemIndex := 1
  else if savedShowCmd = sw_showmaximized then cboWindow.ItemIndex := 2;

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
procedure TfrmItemProp.btnOKClick(Sender: TObject);
begin
  if FChanged then btnApply.Click;
  // save settings !!!
  frmmain.BaseCmd(tcSaveSets, 0);
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnApplyClick(Sender: TObject);
var
  Inst: TObject = nil;
  sci: TShortcutItem = nil;
  scs: TShortcutSubitem = nil;
  ShowCmd: integer;
begin
  try
    Inst := TObject(GetWindowLongPtr(ItemHWnd, GWL_USERDATA));
    if Inst is TShortcutItem then sci := TShortcutItem(Inst);
    if Inst is TShortcutSubitem then scs := TShortcutSubitem(Inst);

    if cboWindow.ItemIndex = 0 then ShowCmd := sw_shownormal
    else if cboWindow.ItemIndex = 1 then ShowCmd := sw_showminimized
    else ShowCmd := sw_showmaximized;

    if sci <> nil then
    begin
      sci.Caption    := UTF8Decode(edCaption.Text);
      sci.Command    := UTF8ToAnsi(edCmd.Text);
      sci.Params     := UTF8ToAnsi(edParams.Text);
      sci.Dir        := UTF8ToAnsi(edDir.Text);
      sci.ImageFile  := cut(UTF8ToAnsi(edImage.Text), ';');
      sci.ImageFile2 := cutafter(UTF8ToAnsi(edImage.Text), ';');
      sci.ShowCmd    := ShowCmd;
      sci.ColorData  := color_data;
      sci.Hide       := chbHide.Checked;
      sci.Update;
    end
    else
    if scs <> nil then
    begin
      scs.Caption    := UTF8Decode(edCaption.Text);
      scs.Command    := UTF8ToAnsi(edCmd.Text);
      scs.Params     := UTF8ToAnsi(edParams.Text);
      scs.Dir        := UTF8ToAnsi(edDir.Text);
      scs.ImageFile  := UTF8ToAnsi(edImage.Text);
      scs.ShowCmd    := ShowCmd;
      scs.ColorData  := color_data;
      scs.Hide       := chbHide.Checked;
      scs.Update;
    end;
    FChanged := false;
  except
    on e: Exception do frmmain.err('TfrmItemProp.btnApplyClick', e);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.btnCancelClick(Sender: TObject);
var
  Inst: TObject = nil;
  sci: TShortcutItem = nil;
  scs: TShortcutSubitem = nil;
begin
  if FChanged then
  begin
    Inst := TObject(GetWindowLongPtr(ItemHWnd, GWL_USERDATA));
    if Inst is TShortcutItem then sci := TShortcutItem(Inst);
    if Inst is TShortcutSubitem then scs := TShortcutSubitem(Inst);
    if sci <> nil then
    begin
      sci.Caption    := savedCaption;
      sci.Command    := savedCommand;
      sci.Params     := savedParams;
      sci.Dir        := savedDir;
      sci.ImageFile  := savedImageFile;
      sci.ImageFile2 := savedImageFile2;
      sci.ShowCmd    := savedShowCmd;
      sci.ColorData  := savedColorData;
      sci.Hide       := savedHide;
      sci.Update;
    end
    else
    if scs <> nil then
    begin
      scs.Caption    := savedCaption;
      scs.Command    := savedCommand;
      scs.Params     := savedParams;
      scs.Dir        := savedDir;
      scs.ImageFile  := savedImageFile;
      scs.ShowCmd    := savedShowCmd;
      scs.ColorData  := savedColorData;
      scs.Hide       := savedHide;
      scs.Update;
    end;
  end;
  FChanged := false;
  Close;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try if assigned(FImage) then GdipDisposeImage(FImage);
  except end;
  FImage := nil;
  ItemHWnd := 0;
  action := caFree;
  frmItemProp := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  postmessage(handle, $a1, 2, 0);
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
procedure TfrmItemProp.edParamsChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.edDirChange(Sender: TObject);
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
procedure TfrmItemProp.cboWindowChange(Sender: TObject);
begin
  FChanged := true;
end;
//------------------------------------------------------------------------------
procedure TfrmItemProp.chbHideChange(Sender: TObject);
begin
  FChanged := true;
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
  color_data      := DEF_COLOR_DATA;
  tbHue.OnChange  := nil;
  tbSat.OnChange  := nil;
  tbBr.OnChange   := nil;
  tbCont.OnChange := nil;
  tbHue.position  := byte(color_data);
  tbSat.position  := byte(color_data shr 8);
  tbBr.position   := byte(color_data shr 16);
  tbCont.position := byte(color_data shr 24);
  tbHue.OnChange  := tbHueChange;
  tbSat.OnChange  := tbHueChange;
  tbBr.OnChange   := tbHueChange;
  tbCont.OnChange := tbHueChange;
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
    on e: Exception do frmmain.err('frmItemProp.Draw', e);
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
    on e: Exception do frmmain.err('frmItemProp.DrawFit', e);
  end;
end;
//------------------------------------------------------------------------------
end.

