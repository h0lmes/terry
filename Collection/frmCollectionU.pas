unit frmCollectionU;

interface

{$mode Delphi}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, ExtCtrls, StdCtrls, ActiveX, Math, Menus,
  GDIPAPI, gdip_gfx, toolu, types;

type
  TWindowProc = procedure(var Message: TMessage) of object;

  { TfrmCollection }

  TfrmCollection = class(TForm)
    edSearch: TEdit;
    list: TListBox;
    mBrowseFolder: TMenuItem;
    mhint: TMenuItem;
    mExit: TMenuItem;
    mmenu: TMainMenu;
    Panel1: TPanel;
    rbBlack: TRadioButton;
    rbGray: TRadioButton;
    rbWhite: TRadioButton;
    ScrollBar: TScrollBar;
    Splitter: TSplitter;
    sbox: TScrollBox;
    pbox: TPaintBox;
    procedure listDblClick(Sender: TObject);
    procedure listDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure mBrowseFolderClick(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure mhintClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure edSearchExit(Sender: TObject);
    procedure edSearchEnter(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: boolean);
    procedure rbWhiteClick(Sender: TObject);
    procedure edSearchKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure pboxMouseLeave(Sender: TObject);
    procedure pboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pboxPaint(Sender: TObject);
    procedure pboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure listClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FImagesFolder : string;
    files: TStrings;
    ItemIndex: integer;
    PrevItemIndex: integer;
    ColCount: integer;
    TotalRowCount: integer;
    RowCount: integer;
    IconSize: integer;
    IconBorder: integer;
    ThumbSize: integer;
    Offset: integer;
    MouseDownPoint: Windows.TPoint;
    MouseDown: boolean;
    loading: boolean;
    updating: boolean;
    PreviewBackgroundColor: uint;
    PreviewSelectionColor: uint;
    bmp: TBitmap;
    panelWindowProcStd: TWindowProc;
    SearchResults: boolean;
    procedure Clear;
    procedure DisposeImages;
    procedure SelectFile(afile: string);
    procedure SelectItem(index: integer; gotoit: boolean = False);
    procedure LoadImages(path, mask: string);
    procedure Draw;
    procedure DrawBuffer;
    procedure DrawBufferSelection;
    procedure DrawPercent(Value: integer);
    procedure SetOffset(Value: integer);
    function ItemFromPoint(x, y: integer): integer;
    procedure panelWindowProc(var msg: TMessage);
  public
  end;

resourcestring
  XCollection = 'Collection';
  XSearch = 'Search...';
  XLoadingProgress = 'Loading. %d%% complete';
  XNothingFound = 'Nothing found';
  XBrowseImagesFolderTitle = 'Select a folder with images in PNG or ICO format';
  XHint = 'Select a folder in the list at the left side. Select an icon and drag&drop it onto any shortcut on the dock.';

var
  frmCollection: TfrmCollection;

implementation
uses DropSrcU;
{$R *.lfm}
//------------------------------------------------------------------------------
procedure TfrmCollection.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Clear;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.Clear;
var
  i, li: integer;
  temp_files: TStrings;
begin
  // dispose images if search results present //
  if SearchResults then
  begin
    i := 0;
    while i < files.Count do
    begin
      try
        GdipDisposeImage(pointer(files.objects[i]));
      except
      end;
      inc(i);
    end;
    files.clear;
  end;

  // dispose images //
  li := 0;
  while li < list.Items.Count do
  begin
    if list.Items.Objects[li] <> TObject(0) then
    begin
      temp_files := TStringList(list.Items.Objects[li]);
      i := 0;
      while i < temp_files.Count do
      begin
        try
          GdipDisposeImage(pointer(temp_files.objects[i]));
        except
        end;
        inc(i);
      end;
      temp_files.free;
      list.Items.Objects[li] := TObject(0);
    end;
    inc(li);
  end;

  list.Clear;
  bmp.Free;
  bmp := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.FormCreate(Sender: TObject);
begin
  font.Name := GetFont;
  font.size := GetFontSize;
  IconSize := 64;
  IconBorder := 8;
  ThumbSize := 128;
  Offset := 0;
  loading := False;
  updating := False;
  FImagesFolder := UnzipPath('%pp%\Images\');
  PreviewBackgroundColor := $ffffffff;
  panelWindowProcStd := sbox.WindowProc;
  sbox.WindowProc := panelWindowProc;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.FormShortCut(var Msg: TWMKey; var Handled: boolean);
begin
  if msg.CharCode = 27 {ESC} then
  begin
    Close;
    handled := True;
  end
  else
  if msg.CharCode = 33 {PGUP} then
  begin
    SetOffset(Offset - RowCount * (IconSize + IconBorder * 2));
    handled := True;
  end
  else
  if msg.CharCode = 34 {PGDN} then
  begin
    SetOffset(Offset + RowCount * (IconSize + IconBorder * 2));
    handled := True;
  end
  else
  if msg.CharCode = 36 {HOME} then
  begin
    SetOffset(0);
    handled := True;
  end
  else
  if msg.CharCode = 35 {END} then
  begin
    SetOffset(1000000);
    handled := True;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.FormShow(Sender: TObject);
begin
  try
    SearchResults := false;
    ItemIndex := -1;
    PrevItemIndex := -1;
    if not assigned(bmp) then bmp := TBitmap.Create;
    rbWhiteClick(nil);
    edSearch.SetFocus;
    list.SetFocus;

    if list.Items.Count = 0 then searchfolders(FImagesFolder, list.Items);
  except
    on e: Exception do messagebox(Handle, pchar('frmCollection.FormShow'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.SelectFile(afile: string);
var
  str: string;
begin
  str := cut(cutafter(afile, FImagesFolder), '\');
  if list.Items.IndexOf(str) >= 0 then
  begin
    list.ItemIndex := list.Items.IndexOf(str);
    list.OnClick(nil);
    if files.IndexOf(afile) >= 0 then SelectItem(files.IndexOf(afile), True)
    else Caption := XCollection;
  end
  else Caption := XCollection;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.SelectItem(index: integer; gotoit: boolean = False);
var
  ay: integer;
begin
  if (index >= 0) and (index < files.Count) then
  begin
    Caption := files[index];
    PrevItemIndex := ItemIndex;
    ItemIndex := index;
    if ItemIndex <> PrevItemIndex then
    begin
      DrawBufferSelection;
      ay := ItemIndex div ColCount * (IconSize + IconBorder * 2);
      if gotoit and (ay + IconSize + IconBorder * 2 > pbox.Height) then
        SetOffset(ay + IconSize + IconBorder * 2 - pbox.Height);
      Draw;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.rbWhiteClick(Sender: TObject);
begin
  if rbWhite.Checked then
  begin
    PreviewBackgroundColor := $ffffffff;
    PreviewSelectionColor := $ffa0b0ff;
  end
  else if rbGray.Checked then
  begin
    PreviewBackgroundColor := $ff808080;
    PreviewSelectionColor := $ffb0c0ff;
  end
  else if rbBlack.Checked then
  begin
    PreviewBackgroundColor := $ff000000;
    PreviewSelectionColor := $ffb0c0ff;
  end;
  DrawBuffer;
  Draw;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.edSearchKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 13) and (shift = []) and (trim(edSearch.Text) <> '') then
  begin
    LoadImages(FImagesFolder, edSearch.Text);
    SearchResults := true;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.listDblClick(Sender: TObject);
begin
  if list.ItemIndex >= 0 then
  begin
    if list.Items.Objects[list.ItemIndex] <> TObject(0) then
    begin
      files := TStringList(list.Items.Objects[list.ItemIndex]);
      DisposeImages;
      list.Items.Objects[list.ItemIndex] := TObject(0);
    end;
    LoadImages(FImagesFolder + list.Items[list.ItemIndex], '');
    list.Items.Objects[list.ItemIndex] := TObject(files);
    SearchResults := false;
  end;
  sbox.SetFocus;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.listClick(Sender: TObject);
begin
  if list.ItemIndex >= 0 then
  begin
    if list.Items.Objects[list.ItemIndex] = TObject(0) then
    begin
      LoadImages(FImagesFolder + list.Items[list.ItemIndex], '');
      list.Items.Objects[list.ItemIndex] := TObject(files);
    end
    else
    begin
      ItemIndex := -1;
      PrevItemIndex := -1;
      Offset := 0;
      files := TStringList(list.Items.Objects[list.ItemIndex]);
      DrawBuffer;
      Draw;
      if files.Count > 0 then SelectItem(0);
    end;
    SearchResults := false;
  end;
  sbox.SetFocus;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.DisposeImages;
var
  i: integer;
begin
  if assigned(files) then
  try
    i := 0;
    while i < files.Count do
    begin
      try GdipDisposeImage(pointer(files.objects[i]));
      except end;
      inc(i);
    end;
    files.free;
  except
    on e: Exception do messagebox(handle, pchar('Collection.DisposeImages'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.LoadImages(path, mask: string);
var
  i, j: integer;
  img: pointer;
  s: string;
  w, h: cardinal;
begin
  try
    try
      loading := True;
      DrawPercent(0);
      Draw;
      application.ProcessMessages;

      ItemIndex := -1;
      PrevItemIndex := -1;
      Offset := 0;

      if SearchResults then DisposeImages;
      files := TStringList.Create;

      // search files //
      if mask = '' then
      begin
        searchfilesrecurse(path, '*.png', files, 0);
        searchfilesrecurse(path, '*.ico', files, 1);
      end else begin
        searchfilesrecurse(path, '*' + mask + '*.png', files, 0);
        searchfilesrecurse(path, '*' + mask + '*.ico', files, 1);
      end;
      // sort //
      i := 0;
      while i < files.Count - 1 do
      begin
        j := i + 1;
        while j < files.Count do
        begin
          if AnsiLowerCase(ExtractFilename(files[j])) <
            AnsiLowerCase(ExtractFilename(files[i])) then
          begin
            s := files[i];
            files[i] := files[j];
            files[j] := s;
          end;
          Inc(j);
        end;
        Inc(i);
      end;
      // load files //
      i := 0;
      while i < files.Count do
      begin
        application.ProcessMessages;
        if not loading then exit;

        img := nil;
        LoadImage(files[i], ThumbSize, false, img, w, h);
        if img = nil then files.Delete(i)
        else
        begin
          files.objects[i] := TObject(img);
          DrawPercent(round(i * 100 / files.Count));
          Draw;
          Inc(i);
        end;
      end;
      // draw //
      DrawBuffer;
      Draw;
      if files.Count > 0 then SelectItem(0);
    except
      on e: Exception do messagebox(handle, pchar('Collection.LoadImages'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
    end;
  finally
    loading := False;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.DrawBuffer;
var
  i, ax, ay: integer;
  hgdip, hbrush: pointer;
begin
  if not assigned(files) or ((list.ItemIndex < 0) and not SearchResults) then
  begin
    DrawPercent(-2);
    exit;
  end
  else if files.Count = 0 then
  begin
    DrawPercent(-1);
    exit;
  end;

  try
    updating := True;

    ColCount := pbox.Width div (IconSize + IconBorder * 2);
    RowCount := pbox.Height div (IconSize + IconBorder * 2);
    TotalRowCount := ceil(files.Count / ColCount);

    bmp.Height := max(pbox.Height, TotalRowCount * (IconSize + IconBorder * 2));
    ScrollBar.Max := bmp.Height - pbox.Height;
    bmp.Width := pbox.Width;
    bmp.Canvas.Pen.Color := clWhite;
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.Rectangle(0, 0, bmp.Width, bmp.Height);

    GdipCreateFromHDC(bmp.canvas.handle, hgdip);
    GdipGraphicsClear(hgdip, PreviewBackgroundColor);
    GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);
    GdipCreateSolidFill(PreviewSelectionColor, hbrush);
    i := 0;
    while i < files.Count do
    begin
      ax := i mod ColCount * (IconSize + IconBorder * 2);
      ay := i div ColCount * (IconSize + IconBorder * 2);
      if i = ItemIndex then
        GdipFillRectangle(hgdip, hbrush, ax, ay,
          IconSize + IconBorder * 2, IconSize + IconBorder * 2);
      GdipDrawImageRectI(hgdip, pointer(files.objects[i]),
        ax + IconBorder, ay + IconBorder, IconSize, IconSize);
      Inc(i);
    end;
    GdipdeletePen(hbrush);
    GdipDeleteGraphics(hgdip);

  finally
    updating := False;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.DrawBufferSelection;
var
  i, ax, ay: integer;
  hgdip, hbrush, hbrush_sel: pointer;
begin
  if not assigned(files) then
    exit;

  try
    updating := True;

    ColCount := pbox.Width div (IconSize + IconBorder * 2);
    RowCount := pbox.Height div (IconSize + IconBorder * 2);
    TotalRowCount := ceil(files.Count / ColCount);

    GdipCreateFromHDC(bmp.canvas.handle, hgdip);
    GdipSetInterpolationMode(hgdip, InterpolationModeHighQualityBicubic);
    GdipCreateSolidFill(PreviewBackgroundColor, hbrush);
    GdipCreateSolidFill(PreviewSelectionColor, hbrush_sel);
    i := PrevItemIndex;
    if (i >= 0) and (i < files.Count) then
    begin
      ax := i mod ColCount * (IconSize + IconBorder * 2);
      ay := i div ColCount * (IconSize + IconBorder * 2);
      GdipFillRectangle(hgdip, hbrush, ax, ay, IconSize + IconBorder *
        2, IconSize + IconBorder * 2);
      GdipDrawImageRectI(hgdip, pointer(files.objects[i]),
        ax + IconBorder, ay + IconBorder, IconSize, IconSize);
    end;
    i := ItemIndex;
    if (i >= 0) and (i < files.Count) then
    begin
      ax := i mod ColCount * (IconSize + IconBorder * 2);
      ay := i div ColCount * (IconSize + IconBorder * 2);
      GdipFillRectangle(hgdip, hbrush_sel, ax, ay, IconSize + IconBorder * 2,
        IconSize + IconBorder * 2);
      GdipDrawImageRectI(hgdip, pointer(files.objects[i]),
        ax + IconBorder, ay + IconBorder, IconSize, IconSize);
    end;
    GdipdeleteBrush(hbrush);
    GdipdeleteBrush(hbrush_sel);
    GdipDeleteGraphics(hgdip);

  finally
    updating := False;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.DrawPercent(Value: integer);
var
  ax, ay: integer;
  hgdip, hbrush, hff, hfont: pointer;
  r: TRectF;
  msg: string;
begin
  if assigned(bmp) then
  try
    if Value >= 0 then msg := format(UTF8ToAnsi(XLoadingProgress), [Value])
    else if Value = -1 then msg := UTF8ToAnsi(XNothingFound);

    bmp.Height := pbox.Height;
    ScrollBar.Max := 0;
    Offset := 0;
    bmp.Width := pbox.Width;
    bmp.Canvas.Pen.Color := clWhite;
    bmp.Canvas.Brush.Color := clWhite;
    bmp.Canvas.Rectangle(0, 0, bmp.Width, bmp.Height);

    ax := pbox.Width div 2;
    ay := pbox.Height div 2;
    GdipCreateFromHDC(bmp.canvas.handle, hgdip);
    GdipGraphicsClear(hgdip, PreviewBackgroundColor);
    GdipSetTextRenderingHint(hgdip, TextRenderingHintAntiAlias);
    GdipSetSmoothingMode(hgdip, SmoothingModeAntiAlias);
    GdipCreateSolidFill(PreviewSelectionColor, hbrush);
    GdipCreateFontFamilyFromName(PWideChar(WideString(GetContentFont)), nil, hff);
    GdipCreateFont(hff, 16, 0, 2, hfont);
    // draw //
    if Value >= 0 then GdipFillPie(hgdip, hbrush, ax - 50, ay - 50, 100, 100, -90, 3.6 * Value);
    // measure //
    r.x := 0;
    r.y := ay + 55;
    r.Width := 0;
    r.Height := 0;
    GdipMeasureString(hgdip, PWideChar(WideString(msg)), -1, hfont, @r, nil, @r, nil, nil);
    r.Width := r.Width + 1;
    r.Height := r.Height + 1;
    r.x := ax - r.Width / 2;
    if Value < 0 then r.y := ay - r.Height / 2;
    // draw //
    GdipDrawString(hgdip, PWideChar(WideString(msg)), -1, hfont, @r, nil, hbrush);
    // cleanup //
    GdipDeleteBrush(hbrush);
    GdipDeleteFont(hfont);
    GdipDeleteFontFamily(hff);
    GdipDeleteGraphics(hgdip);
  except
    on e: Exception do messagebox(handle, pchar('Collection.DrawPercent'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.Draw;
begin
  if updating then exit;

  try
    if assigned(bmp) then
    begin
      // icons //
      BitBlt(pbox.Canvas.handle, 1, 1, pbox.Width - 2, pbox.Height - 2, bmp.Canvas.Handle, 1, 1 + Offset, SRCCOPY);
      // border frame //
      pbox.Canvas.Pen.Color := cl3DDkShadow;
      pbox.Canvas.MoveTo(0, 0);
      pbox.Canvas.LineTo(pbox.Width - 1, 0);
      pbox.Canvas.LineTo(pbox.Width - 1, pbox.Height - 1);
      pbox.Canvas.LineTo(0, pbox.Height - 1);
      pbox.Canvas.LineTo(0, 0);
    end
    else
    begin
      pbox.Canvas.Pen.Color := cl3DDkShadow;
      pbox.Canvas.Brush.Color := PreviewBackgroundColor and $ffffff;
      pbox.Canvas.FillRect(rect(0, 0, pbox.Width - 1, pbox.Height - 1));
    end;
  except
    on e: Exception do
      raise Exception.Create('frmCollection.Draw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TfrmCollection.ItemFromPoint(x, y: integer): integer;
var
  row, col: integer;
begin
  col := x div (IconSize + IconBorder * 2);
  if col >= ColCount then col := -1;
  row := (y + Offset) div (IconSize + IconBorder * 2);
  if row >= TotalRowCount then row := -1;
  Result := col + row * ColCount;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.FormResize(Sender: TObject);
begin
  DrawBuffer;
  SetOffset(Offset);
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.pboxPaint(Sender: TObject);
begin
  SetOffset(Offset);
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.panelWindowProc(var msg: TMessage);
begin
  try
    if (msg.Msg = WM_ERASEBKGND) then msg.Result := 0
    else if (msg.Msg = WM_MOUSEWHEEL) then
    begin
      if msg.wparam > 0 then SetOffset(Offset - IconSize div 2) else SetOffset(Offset + IconSize div 2);
      msg.Result := 1;
    end
    else if (msg.Msg = WM_KEYDOWN) then msg.Result := 0
    else if (msg.Msg = WM_KEYUP) then
    begin
      if msg.wparam = 38 then SetOffset(Offset - IconSize)
      else if msg.wparam = 40 then SetOffset(Offset + IconSize);
      msg.Result := 0;
    end
    else if (msg.Msg = WM_SIZE) then FormResize(nil) else panelWindowProcStd(msg);
  except
    on e: Exception do messagebox(Handle, pchar('frmCollection.panelWindowProc'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.pboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MouseDownPoint.X := X;
  MouseDownPoint.Y := Y;
  MouseDown := True;
  SelectItem(ItemFromPoint(x, y));
  sbox.SetFocus;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.pboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  DropSource : TFileDropSource;
  DropData : THDropDataObject;
  dwEffect : DWORD;
begin
  if MouseDown then
  begin
    if (abs(MouseDownPoint.X - X) > 5) and (ItemIndex >= 0) then
    begin
      MouseDown := false;
      DropSource := TFileDropSource.Create;
      DropData := THDropDataObject.Create(classes.Point(0, 0), true);
      DropData.Add(Caption);
      DoDragDrop(DropData, DropSource, DROPEFFECT_COPY, @dwEffect);
      try DropData.Free;
      except end;
      try DropSource.Free;
      except end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.ScrollBarChange(Sender: TObject);
begin
  try
    if assigned(bmp) then Offset := ScrollBar.Position else Offset := 0;
    Draw;
  except
    on e: Exception do messagebox(Handle, pchar('frmCollection.ScrollBarChange'#10#13 + e.message), pchar(UTF8ToAnsi(Caption)), MB_ICONERROR);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.listDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control) do
  begin
    if (Items.Objects[Index] <> TObject(0)) or (Index = ItemIndex) then Canvas.font.Color := clBlack
    else Canvas.font.Color := clGray;
    Canvas.FillRect(ARect);
    Canvas.TextOut(ARect.Left, ARect.Top, Items[Index]);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.mBrowseFolderClick(Sender: TObject);
begin
  FImagesFolder := toolu.BrowseFolder(Handle, UTF8ToAnsi(XBrowseImagesFolderTitle), FImagesFolder);
  searchfolders(FImagesFolder, list.Items);
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.mhintClick(Sender: TObject);
begin
  messagebox(Handle, pchar(UTF8ToAnsi(XHint)), pchar(UTF8ToAnsi(Caption)), MB_ICONINFORMATION);
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.mExitClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.SetOffset(Value: integer);
begin
  try
    if ScrollBar.Position <> Value then ScrollBar.Position := Value else Draw;
  except
    on e: Exception do raise Exception.Create('frmCollection.SetOffset'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.pboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MouseDown := False;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.pboxMouseLeave(Sender: TObject);
begin
  MouseDown := False;
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.edSearchEnter(Sender: TObject);
begin
  edSearch.ParentFont := True;
  edSearch.Text := '';
end;
//------------------------------------------------------------------------------
procedure TfrmCollection.edSearchExit(Sender: TObject);
begin
  edSearch.ParentFont := True;
  edSearch.Font.Color := $a0a0a0;
  edSearch.Text := ' ' + XSearch;
end;
//------------------------------------------------------------------------------
end.

