unit taskitemu;

{$t+}

interface
uses Windows, Messages, SysUtils, Controls, Classes, ComObj,
  Math, GDIPAPI, gdip_gfx, declu, customitemu, processhlp;

type
  TTaskItem = class(TCustomItem)
  private
    FAppHWnd: THandle;
    procedure UpdateItemInternal;
    procedure Exec;
    function ContextMenu(pt: Windows.TPoint): boolean;
    procedure Configure;
  public
    property AppHWnd: THandle read FAppHWnd;
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    procedure UpdateTaskItem(h: THandle);
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure MouseHeld(button: TMouseButton); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    function GetItemFilename: string; override;
    function CanOpenFolder: boolean; override;
    procedure OpenFolder; override;
    function DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
  end;

implementation
uses toolu;
//------------------------------------------------------------------------------
constructor TTaskItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FCanDrag := false;
  FDontSave := true;
  SetTimer(FHWnd, ID_TIMER, 2000, nil);
end;
//------------------------------------------------------------------------------
destructor TTaskItem.Destroy;
begin
  FFreed := true;
  KillTimer(FHWnd, ID_TIMER);
  try GdipDisposeImage(FImage);
  except end;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateTaskItem(h: THandle);
begin
  if FFreed then exit;
  FAppHWnd := h;
  UpdateItemInternal;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.UpdateItemInternal;
begin
  if FFreed or FUpdating then exit;

  try
    try
      FUpdating := true;

      FIW := 0;
      FIH := 0;
      if FAppHWnd = THandle(0) then
      begin
        FCaption := '';
      end else begin
        LoadImageFromHWnd(FAppHWnd, FBigItemSize, false, false, FImage, FIW, FIH, 3000);
        Caption := ProcessHelper.GetWindowText(FAppHWnd);
      end;
    finally
      FUpdating:= false;
    end;
  except
    on e: Exception do raise Exception.Create('TaskItem.UpdateItemInternal'#10#13 + e.message);
  end;

  Draw(Fx, Fy, FSize, true, 0, FShowItem);
end;
//------------------------------------------------------------------------------
function TTaskItem.cmd(id: TGParam; param: integer): integer;
var
  temp: uint;
begin
  try
    result := inherited cmd(id, param);

    case id of
      // parameters //
      gpBigItemSize:
        begin
          if FBigItemSize <= 128 then temp := 128
          else if FBigItemSize <= 160 then temp := 160
          else if FBigItemSize <= 192 then temp := 192;
          if temp <> FIW then UpdateItemInternal;
        end;

      // commands //
      icIsItem: result := 0;
    end;

  except
    on e: Exception do raise Exception.Create('TaskItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TTaskItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst: Pointer;
  hattr, brush: Pointer;
  l_matrix: ColorMatrix;
  brightness, tmp_color_data: integer;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
begin
  if FFreed or FUpdating or (FFloating and not AForce) then exit;

  // set position //
  try
    ItemRect := GetRectFromSize(ASize);
    Fx := Ax;
    Fy := Ay;
    FShowItem := AShowItem;
    if need_dock then
    begin
      Ax := FxDocking;
      Ay := FyDocking;
    end;
    xReal := Ax - ItemRect.Left;
    yReal := Ay - ItemRect.Top;

    if (FSize = ASize) and not AForce then
    begin

      if wpi > 0 then
      begin
        DeferWindowPos(wpi, FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        UpdateHint(xReal, yReal);
      end else
        SetWindowPos(FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
      exit;

    end else
      if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

    FSize := ASize;
    if FShowItem and SWP_HIDEWINDOW = SWP_HIDEWINDOW then exit;

    UpdateHint(xReal, yReal);
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.SetPosition(' + caption + ')'#10#13 + e.message);
  end;

  // init drawing //
  try
    bmp.topleft.x := xReal;
    bmp.topleft.y := yReal;
    bmp.width := FSize + ItemRect.Left * 2;
    bmp.height := FSize + ItemRect.Top * 2;
    if not CreateBitmap(bmp) then raise Exception.Create('TaskItem.Draw CreateBitmap error');
    if FFloating then dst := CreateGraphics(bmp.dc, ITEM_BACKGROUND) else dst := CreateGraphics(bmp.dc, 0);
    if not assigned(dst) then raise Exception.Create('TaskItem.Draw CreateGraphics error');
    GdipCreateSolidFill(ITEM_BACKGROUND, brush);
    GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
    GdipDeleteBrush(brush);

    GdipSetCompositingMode(dst, CompositingModeSourceOver);
    GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
    GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
    GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
    GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

    xBitmap := 0;
    yBitmap := 0;
    inc(xBitmap, ItemRect.Left);
    inc(yBitmap, ItemRect.Top);
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.InitDraw'#10#13 + e.message);
  end;

  // color and alpha-blending matrices //
  hattr := nil;
  try
    tmp_color_data := DEFAULT_COLOR_DATA;
    if FSelected then
    begin
      brightness := max(byte(tmp_color_data shr 16) - $10, 0);
      tmp_color_data := cardinal(tmp_color_data and $ff00ffff) + cardinal(brightness shl 16);
      CreateColorMatrix(tmp_color_data, l_matrix);
      GdipCreateImageAttributes(hattr);
      GdipSetImageAttributesColorMatrix(hattr, ColorAdjustTypeBitmap, true, @l_matrix, nil, ColorMatrixFlagsDefault);
    end;
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.Matrices'#10#13 + e.message);
  end;

  // draw icon //
  try
    if assigned(FImage) then GdipDrawImageRectRectI(dst, FImage,
      xBitmap, yBitmap, FSize, FSize,
      0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.Icons'#10#13 + e.message);
  end;

  // reflection //
  try
    if FReflection and not FFloating and assigned(FImage) then
      BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.Reflection'#10#13 + e.message);
  end;

  // update window content //
  try
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.UpdateWindow'#10#13 + e.message);
  end;

  // cleanup //
  try
    if FSelected then GdipDisposeImageAttributes(hattr);
    DeleteGraphics(dst);
    DeleteBitmap(bmp);
  except
    on e: Exception do raise Exception.Create('TaskItem.Draw.Cleanup'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TTaskItem.GetItemFilename: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
function TTaskItem.ToString: string;
begin
  result := '';
end;
//------------------------------------------------------------------------------
procedure TTaskItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
begin
  if button = mbLeft then Exec;

  if button = mbRight then
  begin
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;

  UpdateItemInternal; // update item icon and text just in case //
end;
//------------------------------------------------------------------------------
procedure TTaskItem.MouseHeld(button: TMouseButton);
begin
  inherited;
end;
//------------------------------------------------------------------------------
function TTaskItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING, $f001, pchar(UTF8ToAnsi(XCloseWindow)));
  LME(true);

  if not result then msg.WParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
begin
  result := 0;
  DestroyMenu(FHMenu);
  LME(false);
  case wParam of // f001 to f020
    $f001: postmessage(FAppHWnd, WM_SYSCOMMAND, SC_CLOSE, 0);
    $f002..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Configure;
begin
end;
//------------------------------------------------------------------------------
procedure TTaskItem.WndMessage(var msg: TMessage);
begin
  if FFreed then exit;

  // update application title //
  if (msg.msg = WM_TIMER) and (msg.wParam = ID_TIMER) then
  begin
    Caption := ProcessHelper.GetWindowText(FAppHWnd);
  end;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Exec;
begin
  ProcessHelper.ActivateWindow(FAppHWnd);
end;
//------------------------------------------------------------------------------
function TTaskItem.CanOpenFolder: boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.OpenFolder;
begin
end;
//------------------------------------------------------------------------------
function TTaskItem.DropFile(hWnd: HANDLE; pt: windows.TPoint; filename: string): boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------
procedure TTaskItem.Save(szIni: pchar; szIniGroup: pchar);
begin
end;
//------------------------------------------------------------------------------
end.

