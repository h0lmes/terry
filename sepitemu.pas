unit sepitemu;

interface
uses Windows, SysUtils, Controls, Classes, Math,
  GDIPAPI, gfx, themeu, declu, dockh, customitemu;

type

  { TSeparatorItem }

  TSeparatorItem = class(TCustomItem)
  private
    Margins: windows.TRect;
    FItemsArea2: windows.TRect;
    FSeparatorAlpha: integer;
    procedure UpdateItemInternal;
    function ContextMenu(pt: Windows.TPoint): boolean;
  public
    constructor Create(wndParent: HWND; var AParams: TDItemCreateParams); override;
    destructor Destroy; override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi: HDWP; AShowItem: uint); override;
    function ToString: string; override;
    procedure MouseClick(button: TMouseButton; shift: TShiftState; x, y: integer); override;
    procedure WndMessage(var msg: TMessage); override;
    procedure WMCommand(wParam: WPARAM; lParam: LPARAM; var Result: LRESULT); override;
    function cmd(id: TDParam; param: PtrInt): PtrInt; override;
    procedure Save(ini, section: string); override;
    class function Make: string;
end;

implementation
//------------------------------------------------------------------------------
constructor TSeparatorItem.Create(wndParent: HWND; var AParams: TDItemCreateParams);
begin
  inherited;
  FSeparatorAlpha := AParams.SeparatorAlpha;
  UpdateItemInternal;
end;
//------------------------------------------------------------------------------
destructor TSeparatorItem.Destroy;
begin
  FFreed := true;
  try if assigned(FImage) then GdipDisposeImage(FImage);
  except end;
  inherited;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.UpdateItemInternal;
begin
  if not FFreed and assigned(theme) then
  try
    if assigned(FImage) then GdipDisposeImage(FImage);
    FImage := nil;
    Margins := theme.CorrectMargins(theme.Separator.Margins);
    FIW := theme.Separator.W;
    FIH := theme.Separator.H;
    if assigned(theme.Separator.Image) then GdipCloneBitmapAreaI(0, 0, FIW, FIH, PixelFormat32bppPARGB, theme.Separator.Image, FImage);
    //
    FItemsArea2 := theme.ItemsArea2;
    FBorder := max(FItemsArea2.Bottom, max(FReflectionSize, MIN_BORDER));
    //
    Redraw;
  except
    on e: Exception do raise Exception.Create('SeparatorItem.UpdateItemInternal ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
function TSeparatorItem.cmd(id: TDParam; param: PtrInt): PtrInt;
begin
  try
    result := inherited cmd(id, param);
    case id of
      tcThemeChanged:   UpdateItemInternal;
      gpSite:           UpdateItemInternal;
      gpSeparatorAlpha:
        if FSeparatorAlpha <> param then
        begin
          FSeparatorAlpha := param;
          Redraw;
        end;
    end;
  except
    on e: Exception do raise Exception.Create('SeparatorItem.Cmd ' + LineEnding + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi: HDWP; AShowItem: uint);
var
  sepx, sepy, sepw, seph: integer;
  dst, brush: Pointer;
  bmp: _SimpleBitmap;
  xBitmap, yBitmap: integer; // coord of image within window
  xReal, yReal: integer; // coord of window
  ItemRect: windows.TRect;
begin
  try
    if FFreed or (FUndocked and not AForce) then exit;

    // set position //
    try
      ItemRect := GetRectFromSize(ASize);
      FX := Ax;
      FY := Ay;
      FShowItem := AShowItem;
      if need_dock then
      begin
        Ax := FDockingX;
        Ay := FDockingY;
      end;
      xReal := Ax - ItemRect.Left;
      yReal := Ay - ItemRect.Top;

      if (FSize = ASize) and not AForce then
      begin
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem)
        else SetWindowPos(FHWnd, 0, xReal, yReal, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        exit;
      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW <> 0 then exit;
    except
      on e: Exception do raise Exception.Create('SetPosition ' + LineEnding + e.message);
    end;

    xBitmap := ItemRect.Left;
    yBitmap := ItemRect.Top;

    if FImage <> nil then
    begin

      try
        if FSite = 0 then
        begin
          sepw:= FItemSize + FItemsArea2.Bottom + FItemsArea2.Top;
          seph:= FIH;
          sepx:= xBitmap - FItemsArea2.Top;
          sepy:= yBitmap + (FSize - seph) div 2;
        end;
        if FSite = 1 then
        begin
          sepw:= FIW;
          seph:= FItemSize + FItemsArea2.Bottom + FItemsArea2.Top;
          sepx:= xBitmap + (FSize - sepw) div 2;
          sepy:= yBitmap - FItemsArea2.Top;
        end;
        if FSite = 2 then
        begin
          sepw:= FItemSize + FItemsArea2.Bottom + FItemsArea2.Top;
          seph:= FIH;
          sepx:= xBitmap + FSize - FItemSize - FItemsArea2.Top;
          sepy:= yBitmap + (FSize - seph) div 2;
        end;
        if FSite = 3 then
        begin
          sepw:= FIW;
          seph:= FItemSize + FItemsArea2.Bottom + FItemsArea2.Top;
          sepx:= xBitmap + (FSize - sepw) div 2;
          sepy:= yBitmap + FSize - FItemSize - FItemsArea2.Top;
        end;

        try
          bmp.topleft.x:= xReal;
          bmp.topleft.y:= yReal;
          bmp.width:= FSize + ItemRect.Left * 2;
          bmp.height:= FSize + ItemRect.Top * 2;
          if not CreateBitmap(bmp, FHWnd) then exit; //raise Exception.Create('CreateBitmap failed');
          GdipCreateFromHDC(bmp.dc, dst);
          if not assigned(dst) then
          begin
            DeleteBitmap(bmp);
            exit; //raise Exception.Create('CreateGraphics failed');
          end;
          GdipCreateSolidFill(ITEM_BACKGROUND, brush);
          GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + 1);
          GdipDeleteBrush(brush);
          GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);
          if FSeparatorAlpha > 0 then
            DrawEx(dst, FImage, FIW, FIH, classes.rect(sepx, sepy, sepw, seph), Margins, ssStretch, FSeparatorAlpha);
          if FUndocked then DrawItemIndicator(dst, DII_MOVE, ItemRect.Left - 10, ItemRect.Top - 10, FSize, FSize);
          UpdateLWindow(FHWnd, bmp, 255);
        finally
          DeleteGraphics(dst);
          DeleteBitmap(bmp);
        end;
      except
        on e: Exception do raise Exception.Create('DrawWithImage ' + LineEnding + e.message);
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
          if not CreateBitmap(bmp, FHWnd) then exit; //raise Exception.Create('SeparatorItem.Draw CreateBitmap error');
          GdipCreateFromHDC(bmp.dc, dst);
          if not assigned(dst) then
          begin
            DeleteBitmap(bmp);
            exit; //raise Exception.Create('SeparatorItem.Draw CreateGraphics error');
          end;
          GdipCreateSolidFill(ITEM_BACKGROUND, brush);
          GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + 2, ItemRect.Bottom - ItemRect.Top + 2);
          GdipDeleteBrush(brush);
          UpdateLWindow(FHWnd, bmp, 255);
        finally
          DeleteGraphics(dst);
          DeleteBitmap(bmp);
        end;
      except
        on e: Exception do raise Exception.Create('DrawWithNoImage ' + LineEnding + e.message);
      end;

    end;

  except
    on e: Exception do raise Exception.Create('SeparatorItem.Draw ' + LineEnding + e.message);
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
    GetCursorPos(pt);
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
  AppendMenuW(FHMenu, MF_STRING, $f004, pwchar(UTF8Decode(XDeleteSeparator)));
  dockh.DockAddMenu(FHMenu);
  LME(true);

  msg.wParam := WPARAM(TrackPopupMenuEx(FHMenu, TPM_RETURNCMD, pt.x, pt.y, FHWnd, nil));
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
  LME(false);
  DestroyMenu(FHMenu);
  FHMenu := 0;
  case wParam of // f001 to f020
    // $f001..$f003: ;
    $f004: Delete;
    else sendmessage(FHWndParent, WM_COMMAND, wParam, lParam);
  end;
end;
//------------------------------------------------------------------------------
procedure TSeparatorItem.Save(ini, section: string);
begin
  if (ini = '') or (section = '') then exit;
  WritePrivateProfileString(pchar(section), nil, nil, pchar(ini));
  WritePrivateProfileString(pchar(section), 'class', 'separator', pchar(ini));
end;
//------------------------------------------------------------------------------
class function TSeparatorItem.Make: string;
begin
  result := 'class="separator";';
end;
//------------------------------------------------------------------------------
end.

