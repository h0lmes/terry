unit dropindicatoru;

{$mode delphi}

interface

uses Windows, Classes, SysUtils, declu, themeu, GDIPAPI, gdip_gfx, dockh;

type
  TDropIndicatorType = (DropIndicatorTypeDrop, DropIndicatorTypeAdd, DropIndicatorTypeRun);

  TDropIndicator = class(TObject)
  private
    FHWnd: THandle;
  public
    class procedure CreateIndicator;
    class procedure DestroyIndicator;
    constructor Create;
    destructor Destroy;
    procedure Show(Ax, Ay, ASite: integer; AHWndParent: THandle; AType: TDropIndicatorType);
    procedure Hide;
  end;

var
  DropIndicator: TDropIndicator;

implementation
//------------------------------------------------------------------------------
class procedure TDropIndicator.CreateIndicator;
begin
  if DropIndicator = nil then DropIndicator := TDropIndicator.Create;
end;
//------------------------------------------------------------------------------
class procedure TDropIndicator.DestroyIndicator;
begin
  if assigned(DropIndicator) then DropIndicator.Free;
end;
//------------------------------------------------------------------------------
constructor TDropIndicator.Create;
begin
  FHWnd := CreateWindowEx(ws_ex_layered + ws_ex_toolwindow, WINITEM_CLASS, nil, ws_popup, -32, -32, 32, 32, 0, 0, hInstance, nil);
end;
//------------------------------------------------------------------------------
destructor TDropIndicator.Destroy;
begin
  DestroyWindow(FHWnd);
end;
//------------------------------------------------------------------------------
procedure TDropIndicator.Show(Ax, Ay, ASite: integer; AHWndParent: THandle; AType: TDropIndicatorType);
var
  x, y, w, h: integer;
  bmp: _SimpleBitmap;
  dst, brush: Pointer;
begin
  case ASite of
    0: begin
      x := Ax + 10;
      y := Ay - h div 2;
    end;
    1: begin
      x := Ax - w div 2;
      y := Ay + 10;
    end;
    2: begin
      x := Ax - 10 - w;
      y := Ay - h div 2;
    end;
    else begin
      x := Ax - w div 2;
      y := Ay - 10 - h;
    end;
  end;
  SetWindowPos(FHWnd, AHWndParent, 0, 0, 0, 0, swp_nomove + swp_nosize + swp_noactivate + swp_noreposition + swp_showwindow);

  try
    bmp.topleft.x := x;
    bmp.topleft.y := y;
    bmp.width := w;
    bmp.height := h;
    if not CreateBitmap(bmp) then raise Exception.Create('in DropIndicator.Draw CreateBitmap error');
    dst := CreateGraphics(bmp.dc, 0);
    if not assigned(dst) then raise Exception.Create('in DropIndicator.Draw CreateGraphics error');
    GdipSetInterpolationMode(dst, InterpolationModeHighQualityBicubic);

    if assigned(theme.DropIndicatorAdd.Image) then
      GdipDrawImageRectRectI(dst, theme.DropIndicatorAdd.Image, 0, 0, w, h, 0, 0, w, h, UnitPixel, nil, nil, nil);
    UpdateLWindow(FHWnd, bmp, 255);

    DeleteGraphics(dst);
    DeleteBitmap(bmp);
  except
    on e: Exception do raise Exception.Create('in DropIndicator.Draw.InitDraw'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure TDropIndicator.Hide;
begin
  ShowWindow(FHWnd, SW_HIDE);
end;
//------------------------------------------------------------------------------
end.

