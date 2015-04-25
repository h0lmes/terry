unit customdrawitemu;

{$t+}

interface

uses Windows, Messages, SysUtils, Controls, Classes, ShellAPI, Math,
  declu, dockh, GDIPAPI, gfx, customitemu, themeu;

type
  TOnBeforeDraw = procedure of object;
  TOnAfterDraw = procedure of object;
  TOnDrawOverlay = procedure(graphics: Pointer; x, y, size: integer) of object;

  { TCustomDrawItem }

  TCustomDrawItem = class(TCustomItem)
  protected
    FRunning: boolean;
    OnBeforeDraw: TOnBeforeDraw;
    OnAfterDraw: TOnAfterDraw;
    OnDrawOverlay: TOnDrawOverlay;
  public
    property Running: boolean read FRunning;
    constructor Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams); override;
    procedure Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint); override;
  end;

implementation
//------------------------------------------------------------------------------
constructor TCustomDrawItem.Create(AData: string; AHWndParent: cardinal; AParams: _ItemCreateParams);
begin
  inherited;
  FRunning := false;
end;
//------------------------------------------------------------------------------
// set position, size, repaint window //
procedure TCustomDrawItem.Draw(Ax, Ay, ASize: integer; AForce: boolean; wpi, AShowItem: uint);
var
  bmp: _SimpleBitmap;
  dst, hattr, brush: Pointer;
  bitmapX, bitmapY: integer; // coord of image within window
  screenX, screenY: integer; // coord of window
  ItemRect: windows.TRect;
  animX, animY, animSize: integer;
  button: boolean;
begin
  try
    if FFreed or not FEnabled or FUpdating or (FFloating and not AForce)
       or ((FX = Ax) and (FY = Ay) and (FSize = ASize) and (FShowItem = AShowItem) and not AForce and not need_dock) then exit;

    // OnBeforeDraw
    if assigned(OnBeforeDraw) then OnBeforeDraw;

    animX := 0;
    animY := 0;
    animSize := 0;

    // set position //
    try
      ItemRect := GetRectFromSize(ASize);
      FX := Ax;
      FY := Ay;
      FShowItem := AShowItem;
      if need_dock then
      begin
        Ax := FXDocking;
        Ay := FYDocking;
      end;
      screenX := Ax - ItemRect.Left;
      screenY := Ay - ItemRect.Top;

      // bounce animation //
      if FAnimationProgress > 0 then
        if (FAnimationType >= 2) and (FAnimationType <= 4) then
        begin
          animX := FAnimationProgress mod 30;
          if animX > 15 then animX := 30 - animX;
          animX := round(anim_bounce[animX] * min(FBorder, 40));
          if (FSite = 0) or (FSite = 2) then dec(screenY, animX);
        end;

      if (FSize = ASize) and not AForce then
      begin
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, screenX, screenY, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem)
        else SetWindowPos(FHWnd, 0, screenX, screenY, 0, 0, swp_nosize + swp_noactivate + swp_noreposition + swp_nozorder + FShowItem);
        UpdateHint(screenX, screenY);
        // OnAfterDraw
        if assigned(OnAfterDraw) then OnAfterDraw;
        exit;
      end else
        if wpi > 0 then DeferWindowPos(wpi, FHWnd, 0, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_nozorder + swp_noreposition + FShowItem);

      FSize := ASize;
      if FShowItem and SWP_HIDEWINDOW <> 0 then exit;
      UpdateHint(screenX, screenY);
    except
      on e: Exception do raise Exception.Create('SetPosition'#10#13 + e.message);
    end;

    // init bitmap //
    try
      bmp.topleft.x := screenX;
      bmp.topleft.y := screenY;
      bmp.width := FSize + ItemRect.Left * 2;
      bmp.height := FSize + ItemRect.Top * 2;
      if not CreateBitmap(bmp, FHWnd) then exit; //raise Exception.Create('CreateBitmap failed');
      GdipCreateFromHDC(bmp.dc, dst);
      if not assigned(dst) then
      begin
        DeleteBitmap(bmp);
        exit; //raise Exception.Create('CreateGraphics failed');
      end;
      GdipCreateSolidFill(ITEM_BACKGROUND, brush);
      if (FSite = 1) or (FSite = 3) then
        GdipFillRectangleI(dst, brush, ItemRect.Left - FItemSpacing div 2, ItemRect.Top - 1, ItemRect.Right - ItemRect.Left + FItemSpacing, ItemRect.Bottom - ItemRect.Top + 1)
      else
        GdipFillRectangleI(dst, brush, ItemRect.Left - 1, ItemRect.Top - FItemSpacing div 2, ItemRect.Right - ItemRect.Left + 1, ItemRect.Bottom - ItemRect.Top + FItemSpacing);
      GdipDeleteBrush(brush);
      GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

      // draw the button
      button := false;
      if FRunning and not FFloating then button := theme.DrawButton(dst, ItemRect.Left, ItemRect.Top, FSize, FAttention);
      FNCHitTestNC := button;

      bitmapX := 0;
      bitmapY := 0;

      if FAnimationProgress > 0 then
      begin
        // rotate //
        if FAnimationType = 1 then
        begin
          dec(bitmapX, ItemRect.Left);
          dec(bitmapY, ItemRect.Top);
          dec(bitmapX, FSize div 2);
          dec(bitmapY, FSize div 2);
          GdipTranslateWorldTransform(dst, -bitmapX, -bitmapY, MatrixOrderPrepend);
          GdipRotateWorldTransform(dst, FAnimationProgress * 6, MatrixOrderPrepend);
        end;
        // bounce //
        if (FAnimationType >= 2) and (FAnimationType <= 4) then
        begin
          animX := FAnimationProgress mod 30;
          if animX > 15 then animX := 30 - animX;
          animX := round(anim_bounce[animX] * min(FBorder, 40));
          if FSite = 1 then inc(bitmapY, animX)
          else if FSite = 3 then dec(bitmapY, animX);
        end;
        // quake //
        if FAnimationType = 5 then
        begin
          if FAnimationProgress mod 2 = 1 then exit;
          animX := random(FItemSize div 3) - FItemSize div 6;
          animY := random(FItemSize div 3) - FItemSize div 6;
          inc(bitmapX, animX);
          inc(bitmapY, animY);
        end;
        // swing //
        if FAnimationType = 6 then
        begin
          dec(bitmapX, ItemRect.Left);
          dec(bitmapY, ItemRect.Top);
          dec(bitmapX, FSize div 2);
          dec(bitmapY, FSize div 2);
          GdipTranslateWorldTransform(dst, -bitmapX, -bitmapY, MatrixOrderPrepend);
          GdipRotateWorldTransform(dst, sin(FAnimationProgress * 6) * 30, MatrixOrderPrepend);
        end;
        // vibrate //
        if FAnimationType = 7 then
        begin
          animSize := round(sin(FAnimationProgress * 3) * 6);
          dec(bitmapX, animSize div 2);
          dec(bitmapY, animSize div 2);
        end;
        // zoom //
        if FAnimationType = 8 then
        begin
          animSize := round(sin(FAnimationProgress * 6) * 10);
          dec(bitmapX, animSize div 2);
          dec(bitmapY, animSize div 2);
        end;
      end;

      inc(bitmapX, ItemRect.Left);
      inc(bitmapY, ItemRect.Top);
    except
      on e: Exception do raise Exception.Create('InitBitmap'#10#13 + e.message);
    end;

    // draw icon //
    CreateColorAttributes(FColorData, FSelected, hattr);
    if assigned(FImage) then GdipDrawImageRectRectI(dst, FImage, bitmapX, bitmapY, FSize + animSize, FSize + animSize, 0, 0, FIW, FIH, UnitPixel, hattr, nil, nil);
    if hattr <> nil then GdipDisposeImageAttributes(hattr);

    // OnDrawOverlay
    if assigned(OnDrawOverlay) then OnDrawOverlay(dst, bitmapX, bitmapY, FSize + animSize);

    if FAnimationProgress > 0 then GdipResetWorldTransform(dst);
    if not button then
    begin
      if FReflection and (FReflectionSize > 0) and not FFloating then
        BitmapReflection(bmp, ItemRect.Left, ItemRect.Top, FSize, FReflectionSize, FSite);
      if FRunning and not FFloating then theme.DrawIndicator(dst, ItemRect.Left, ItemRect.Top, FSize, FSite);
    end;
    UpdateLWindow(FHWnd, bmp, ifthen(FFloating, 127, 255));
    DeleteGraphics(dst);
    DeleteBitmap(bmp);
    // OnAfterDraw
    if assigned(OnAfterDraw) then OnAfterDraw;
  except
    on e: Exception do raise Exception.Create('CustomDrawItem.Draw(' + FCaption + ')'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
end.

