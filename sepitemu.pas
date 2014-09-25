unit sepitemu;

interface
uses Windows, SysUtils, Controls, Classes, Math,
  GDIPAPI, gdip_gfx, themeu, declu, customitemu, dockh, toolu;

type TSeparatorItem = class(TCustomItem)
  private
    Margins: windows.TRect;
    procedure UpdateItemInternal;
    function ContextMenu(pt: Windows.TPoint): boolean;
  public
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    destructor Destroy; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TGParam; param: integer): integer; override;
    procedure Save(szIni: pchar; szIniGroup: pchar); override;
end;

implementation
//------------------------------------------------------------------------------
constructor TSeparatorItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FDontSave := FetchValue(AData, 'dontsave="', '";') <> '';
  FCanDrag := FetchValue(AData, 'candrag="', '";') = '';
  UpdateItemInternal;
end;
//------------------------------------------------------------------------------
destructor TSeparatorItem.Destroy;
begin
  try GdipDisposeImage(FImage);
  except end;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.UpdateItemInternal;
begin
  if not FFreed and assigned(theme) then
  try
    if FImage <> nil then GdipDisposeImage(FImage);
    FImage := nil;
    Margins := theme.CorrectMargins(theme.Separator.Margins);
    FIW := theme.Separator.W;
    FIH := theme.Separator.H;
    if assigned(theme.Separator.Image) then GdipCloneBitmapAreaI(0, 0, FIW, FIH, PixelFormat32bppPARGB, theme.Separator.Image, FImage);
    Redraw;
  except
    on e: Exception do raise Exception.Create('SeparatorItem.UpdateItemInternal'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TSeparatorItem.cmd(id: TGParam; param: integer): integer;
begin
  try
    result := inherited cmd(id, param);
    case id of
      tcThemeChanged: UpdateItemInternal;
      gpSite: UpdateItemInternal;
    end;
  except
    on e: Exception do raise Exception.Create('SeparatorItem.Cmd'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  sepx, sepy, sepw, seph: integer;
  dst, brush: Pointer;
  bmp: _SimpleBitmap;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
begin
  try
    if FFreed or (FFloating and not AForce) then exit;

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
    except
      on e: Exception do raise Exception.Create('SetPosition'#10#13 + e.message);
    end;

    xBitmap := ItemRect.Left;
    yBitmap := ItemRect.Top;

    if FImage <> nil then
    begin

      try
        if FSite = 0 then
        begin
          sepw:= FItemSize;
          seph:= FIH;
          sepx:= xBitmap;
          sepy:= yBitmap + (FSize - seph) div 2;
        end;
        if FSite = 1 then
        begin
          sepw:= FIW;
          seph:= FItemSize;
          sepx:= xBitmap + (FSize - sepw) div 2;
          sepy:= yBitmap;
        end;
        if FSite = 2 then
        begin
          sepw:= FItemSize;
          seph:= FIH;
          sepx:= xBitmap + FSize - FItemSize;
          sepy:= yBitmap + (FSize - seph) div 2;
        end;
        if FSite = 3 then
        begin
          sepw:= FIW;
          seph:= FItemSize;
          sepx:= xBitmap + (FSize - sepw) div 2;
          sepy:= yBitmap + FSize - FItemSize;
        end;

        try
          bmp.topleft.x:= xReal;
          bmp.topleft.y:= yReal;
          bmp.width:= FSize + ItemRect.Left * 2;
          bmp.height:= FSize + ItemRect.Top * 2;
          if not CreateBitmap(bmp) then raise Exception.Create('CreateBitmap failed');
          GdipCreateFromHDC(bmp.dc, dst);
          if not assigned(dst) then raise Exception.Create('CreateGraphics failed');
          GdipSetCompositingMode(dst, CompositingModeSourceOver);
          GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
          GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
          GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
          GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

          GdipCreateSolidFill(ITEM_BACKGROUND, brush);
          GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
          GdipDeleteBrush(brush);
          DrawEx(dst, FImage, FIW, FIH, classes.rect(sepx, sepy, sepw, seph), Margins, ssStretch);
          UpdateLWindow(FHWnd, bmp, 255);
        finally
          DeleteGraphics(dst);
          DeleteBitmap(bmp);
        end;
      except
        on e: Exception do raise Exception.Create('DrawWithImage'#10#13 + e.message);
      end;

    end
    else
    begin

      try
        try
          bmp.topleft.x := xReal;
          bmp.topleft.y := yReal;
          bmp.width := FSize + FReflectionSize * 2;
          bmp.height := FSize + FReflectionSize * 2;
          if not CreateBitmap(bmp) then raise Exception.Create('SeparatorItem.Draw CreateBitmap error');
          GdipCreateFromHDC(bmp.dc, dst);
          if not assigned(dst) then raise Exception.Create('SeparatorItem.Draw CreateGraphics error');
          GdipSetCompositingMode(dst, CompositingModeSourceOver);
          GdipSetCompositingQuality(dst, CompositingQualityHighSpeed);
          GdipSetSmoothingMode(dst, SmoothingModeHighSpeed);
          GdipSetPixelOffsetMode(dst, PixelOffsetModeHighSpeed);
          GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

          GdipCreateSolidFill(ITEM_BACKGROUND, brush);
          GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 2, ItemRect.Bottom - ItemRect.Top + 2);
          GdipDeleteBrush(brush);
          UpdateLWindow(FHWnd, bmp, 255);
        finally
          DeleteGraphics(dst);
          DeleteBitmap(bmp);
        end;
      except
        on e: Exception do raise Exception.Create('DrawWithNoImage'#10#13 + e.message);
      end;

    end;

  except
    on e: Exception do raise Exception.Create('SeparatorItem.Draw(' + caption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TSeparatorItem.ToString: string;
begin
  result:= 'class="separator";';
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer);
var
  pt: windows.TPoint;
begin
  if button = mbRight then
  begin
    windows.GetCursorPos(pt);
    ContextMenu(pt);
  end;
end;
//------------------------------------------------------------------------------
function TSeparatorItem.ContextMenu(pt: Windows.TPoint): boolean;
var
  msg: TMessage;
begin
  result := false;

  FHMenu := CreatePopupMenu;
  AppendMenu(FHMenu, MF_STRING + ifthen(FDontSave, MF_DISABLED, MF_ENABLED), $f004, pchar(UTF8ToAnsi(XDeleteSeparator)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  msg.wParam := uint(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
  WMCommand(msg.wParam, msg.lParam, msg.Result);
  Result := True;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.WndMessage(var msg: TMessage);
begin
  msg.Result := 0;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT);
begin
  result := 0;
  DestroyMenu(FHMenu);
  LME(false);
  case wParam of // f001 to f020
    $f001..$f003: ;
    $f004: Delete;
    $f005..$f020: ;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.Save(szIni: pchar; szIniGroup: pchar);
begin
  if FFreed or FDontSave or (szIni = nil) or (szIniGroup = nil) then exit;
  try
    WritePrivateProfileString(szIniGroup, nil, nil, szIni);
    WritePrivateProfileString(szIniGroup, 'class', 'separator', szIni);
  except
    on e: Exception do raise Exception.Create('SeparatorItem.Save'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

