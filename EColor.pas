unit EColor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math;

const
  HLSMAX = 240;
  RGBMAX = 255;
  UNDEFINED = (HLSMAX * 2) div 3;

type
  TEColor = class(TWinControl)
    procedure Paint;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; x, y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer); override;
    constructor Create(AOwner: TComponent); override;
  protected
    procedure WMPaint(var Msg: TMsg); message WM_PAINT;
    procedure WMSize(var Msg: TMsg); message WM_SIZE;
  private
    h: integer;
    w: integer;
    FColorRect: TRect;
    FLightnessRect: TRect;
    FHue: integer;
    FLightness: integer;
    FSaturation: integer;
    FBackColor: TColor;
    FColor: TColor;
    FBorderColor: TColor;
    FVSpace: integer;
    FHSpace: integer;
    FLightnessBarWidth: integer;
    FSplitWidth: integer;
    FOnChange: TNotifyEvent;
    valuetype: integer;
    procedure SetHue(Value: integer);
    procedure SetLightness(Value: integer);
    procedure SetSaturation(Value: integer);
    procedure SetColor(Value: Tcolor);
    procedure SetBackColor(Value: Tcolor);
    procedure SetBorderColor(Value: Tcolor);
    procedure SetVSpace(Value: integer);
    procedure SetHSpace(Value: integer);
    procedure SetLightnessBarWidth(Value: integer);
    procedure SetSplitWidth(Value: integer);
    procedure CheckValues;
    procedure RGBtoHLS(color: Tcolor; out h, l, s: integer);
    function HLStoRGB(h, l, s: integer): TColor;
    procedure DoOnChange;
  public
  published
    property Hue: integer read FHue write SetHue;
    property Lightness: integer read FLightness write SetLightness;
    property Saturation: integer read FSaturation write SetSaturation;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBtnFace;
    property VSpace: integer read FVSpace write SetVSpace;
    property HSpace: integer read FHSpace write SetHSpace;
    property LightnessBarWidth: integer read FLightnessBarWidth write SetLightnessBarWidth default 15;
    property SplitWidth: integer read FSplitWidth write SetSplitWidth default 10;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
//--------------------------------------------------------------------------------------------------
constructor TEColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  Width := 220;
  Height := 200;
  FVSpace := 8;
  FHSpace := 8;
  FSplitWidth := 10;
  valuetype := 0;
  FColor := clBtnFace;
  FBackColor := clBtnFace;
  FBorderColor := 0;
  FLightnessBarWidth := 15;
  resize;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.WMPaint(var Msg: TMsg);
begin
  inherited;
  Paint;
end;
//------------------------------------------------------------------------------
procedure TEColor.WMSize(var Msg: TMsg);
begin
  inherited;
  Resize;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetHue(Value: integer);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    CheckValues;
    paint;
    DoOnChange;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetLightness(Value: integer);
begin
  if FLightness <> Value then
  begin
    FLightness := Value;
    CheckValues;
    paint;
    DoOnChange;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetSaturation(Value: integer);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    CheckValues;
    paint;
    DoOnChange;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetColor(Value: TColor);
begin
  FColor := Value;
  RGBtoHLS(FColor, FHue, FLightness, FSaturation);
  paint;
  DoOnChange;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.DoOnChange;
begin
  FColor := HLStoRGB(FHue, FLightness, FSaturation);
  if assigned(FOnChange) then
    FOnChange(Self);
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetBackColor(Value: Tcolor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    paint;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetBorderColor(Value: Tcolor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    paint;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetVSpace(Value: integer);
begin
  if FVSpace <> Value then
  begin
    FVSpace := Value;
    if FVSpace < 1 then
      FVSpace := 1;
    Resize;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetHSpace(Value: integer);
begin
  if FHSpace <> Value then
  begin
    FHSpace := Value;
    if FHSpace < 1 then
      FHSpace := 1;
    Resize;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetLightnessBarWidth(Value: integer);
begin
  if FLightnessBarWidth <> Value then
  begin
    FLightnessBarWidth := Value;
    if FLightnessBarWidth < 5 then
      FLightnessBarWidth := 5;
    Resize;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.SetSplitWidth(Value: integer);
begin
  if FSplitWidth <> Value then
  begin
    FSplitWidth := Value;
    if FSplitWidth < 0 then
      FSplitWidth := 0;
    Resize;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.Resize;
begin
  h := Height - VSpace * 2;
  w := Width - HSpace * 2;

  FColorRect.left := HSpace;
  FColorRect.top := VSpace;
  FColorRect.right := Width - HSpace - FLightnessBarWidth - FSplitWidth;
  FColorRect.bottom := Height - VSpace;

  FLightnessRect.left := Width - HSpace - FLightnessBarWidth;
  FLightnessRect.top := VSpace;
  FLightnessRect.right := Width - HSpace;
  FLightnessRect.bottom := Height - VSpace;

  paint;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.Paint;
var
  pic: Tbitmap;
  x, y: integer;
begin
  pic := Tbitmap.Create;
  pic.Height := Height;
  pic.Width := Width;
  pic.canvas.brush.style := bsSolid;
  pic.canvas.pen.color := BackColor;
  pic.canvas.brush.color := BackColor;
  pic.canvas.Rectangle(0, 0, Width, Height);
  pic.canvas.brush.style := bsClear;
  pic.canvas.pen.color := BorderColor;
  pic.canvas.RoundRect(0, 0, Width, Height, 10, 10);
  pic.canvas.brush.style := bsSolid;

  for x := FColorRect.left to FColorRect.right do
  begin
    for y := FColorRect.top to FColorRect.bottom do
    begin
      pic.canvas.Pixels[x, y] :=
        HLStoRGB(
          round(239 / max(1, FColorRect.right - FColorRect.left) *
        (x - FColorRect.left)), 120, 240 -
        round(240 / max(1, FColorRect.bottom - FColorRect.top) * (y - FColorRect.top)));
    end;
  end;
  for y := FLightnessRect.top to FLightnessRect.bottom do
  begin
    pic.canvas.pen.color :=
      HLStoRGB(FHue, 240 - round(240 / max(1, FLightnessRect.bottom - FLightnessRect.top) * (y - FLightnessRect.top)), FSaturation);
    pic.canvas.line(FLightnessRect.left, y, FLightnessRect.right, y);
  end;

  pic.canvas.brush.style := bsClear;
  pic.canvas.pen.color := clBlack;
  // marker at the color field
  pic.canvas.line(
    round((FColorRect.right - FColorRect.left) / 240 * FHue) + FColorRect.left - 3,
    FColorRect.bottom - round((FColorRect.bottom - FColorRect.top) / 240 * FSaturation),
    round((FColorRect.right - FColorRect.left) / 240 * FHue) + FColorRect.left + 4,
    FColorRect.bottom - round((FColorRect.bottom - FColorRect.top) / 240 * FSaturation));
  pic.canvas.line(
    round((FColorRect.right - FColorRect.left) / 240 * FHue) + FColorRect.left,
    FColorRect.bottom - round((FColorRect.bottom - FColorRect.top) / 240 * FSaturation) - 3,
    round((FColorRect.right - FColorRect.left) / 240 * FHue) + FColorRect.left,
    FColorRect.bottom - round((FColorRect.bottom - FColorRect.top) / 240 * FSaturation) + 4);
  // marker at the lightness bar
  pic.canvas.rectangle(
    FLightnessRect.left - 2,
    round(FLightnessRect.bottom - (FLightnessRect.bottom - FLightnessRect.top) / 240 * FLightness) - 1,
    FLightnessRect.right + 2,
    round(FLightnessRect.bottom - (FLightnessRect.bottom - FLightnessRect.top) / 240 * FLightness) + 2);

  BitBlt(GetDC(handle), 0, 0, Width, Height, pic.Canvas.Handle, 0, 0, SRCCOPY);
  pic.Free;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.MouseMove(Shift: TShiftState; x, y: integer);
begin
  inherited;

  if valuetype = 1 then
  begin
    FHue := round((x - FColorRect.left) /
      max(1, FColorRect.right - FColorRect.left) * 239);
    FSaturation := round((FColorRect.bottom - y) /
      max(1, FColorRect.bottom - FColorRect.top) * 240);
  end;
  if valuetype = 2 then
    FLightness := 240 - round((y - FLightnessRect.top) /
      max(1, FLightnessRect.bottom - FLightnessRect.top) * 240);

  if valuetype > 0 then
  begin
    checkValues;
    paint;
    DoOnChange;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin
  inherited;

  if button = mbLeft then
  begin
    if PtInRect(FColorRect, Classes.Point(x, y)) then
    begin
      valuetype := 1;
      FHue := round((x - FColorRect.left) / max(1, FColorRect.right - FColorRect.left) * 239);
      FSaturation := round((FColorRect.bottom - y) / max(1, FColorRect.bottom - FColorRect.top) * 240);
    end
    else if PtInRect(FLightnessRect, Classes.Point(x, y)) then
    begin
      valuetype := 2;
      FLightness := 240 - round((y - FLightnessRect.top) / max(1, FLightnessRect.bottom - FLightnessRect.top) * 240);
    end
    else
    begin
      valuetype := 0;
      exit;
    end;

    checkValues;
    paint;
    DoOnChange;
  end;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: integer);
begin
  inherited;
  valuetype := 0;
end;
//--------------------------------------------------------------------------------------------------
procedure TEColor.CheckValues;
begin
  if FHue < 0 then
    FHue := 0;
  if FHue > 239 then
    FHue := 239;
  if FLightness < 0 then
    FLightness := 0;
  if FLightness > 240 then
    FLightness := 240;
  if FSaturation < 0 then
    FSaturation := 0;
  if FSaturation > 240 then
    FSaturation := 240;
end;
//------------------------------------------------------------------------------
procedure TEColor.RGBtoHLS(color: Tcolor; out h, l, s: integer);
var
  cMax, cMin: integer;
  r, g, b: integer;
  Rdelta, Gdelta, Bdelta: single;
begin
  r := GetRValue(color);
  g := GetGValue(color);
  b := GetBValue(color);
  cMax := max(max(R, G), B);
  cMin := min(min(R, G), B);
  l := round((((cMax + cMin) * HLSMAX) + RGBMAX) / (2 * RGBMAX));

  if cMax = cMin then
  begin
    s := 0;
    h := UNDEFINED;
  end
  else
  begin
    if L <= (HLSMAX / 2) then
      s := round((((cMax - cMin) * HLSMAX) + ((cMax + cMin) / 2)) / (cMax + cMin))
    else
      s := round((((cMax - cMin) * HLSMAX) + ((2 * RGBMAX - cMax - cMin) / 2)) /
        (2 * RGBMAX - cMax - cMin));

    Rdelta := (((cMax - R) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Gdelta := (((cMax - G) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);
    Bdelta := (((cMax - B) * (HLSMAX / 6)) + ((cMax - cMin) / 2)) / (cMax - cMin);

    if R = cMax then
      H := round(Bdelta - Gdelta)
    else if G = cMax then
      H := round((HLSMAX / 3) + Rdelta - Bdelta)
    else
      H := round(((2 * HLSMAX) / 3) + Gdelta - Rdelta);
    if H < 0 then
      Inc(H, HLSMAX);
    if H > HLSMAX then
      Dec(H, HLSMAX);
  end;

  if S < 0 then
    S := 0;
  if S > HLSMAX then
    S := HLSMAX;
  if L < 0 then
    L := 0;
  if L > HLSMAX then
    L := HLSMAX;
end;
//--------------------------------------------------------------------------------------------------
function TEColor.HLStoRGB(h, l, s: integer): TColor;

  function HueToRGB(n1, n2, hue: single): single;
  begin
    if hue < 0 then
      hue := hue + HLSMAX;
    if hue > HLSMAX then
      hue := hue - HLSMAX;
    if hue < HLSMAX / 6 then
      Result :=
        (n1 + (((n2 - n1) * hue + (HLSMAX / 12)) / (HLSMAX / 6)))
    else if hue < HLSMAX / 2 then
      Result := n2
    else if hue < (HLSMAX * 2) / 3 then
      Result :=
        (n1 + (((n2 - n1) * (((HLSMAX * 2) / 3) - hue) + (HLSMAX / 12)) / (HLSMAX / 6)))
    else
      Result := n1;
  end;

var
  r, g, b, Magic1, Magic2: single;
begin
  if S = 0 then
  begin
    b := round((L * RGBMAX) / HLSMAX);
    R := B;
    G := B;
  end
  else
  begin
    if L <= (HLSMAX / 2) then
      Magic2 := (L * (HLSMAX + S) + (HLSMAX / 2)) / HLSMAX
    else
      Magic2 := L + S - ((L * S) + (HLSMAX / 2)) / HLSMAX;
    Magic1 := 2 * L - Magic2;
    R := round((HueToRGB(Magic1, Magic2, H + (HLSMAX / 3)) * RGBMAX +
      (HLSMAX / 2)) / HLSMAX);
    G := round((HueToRGB(Magic1, Magic2, H) * RGBMAX + (HLSMAX / 2)) / HLSMAX);
    B := round((HueToRGB(Magic1, Magic2, H - (HLSMAX / 3)) * RGBMAX +
      (HLSMAX / 2)) / HLSMAX);
  end;

  if R < 0 then
    R := 0;
  if R > RGBMAX then
    R := RGBMAX;
  if G < 0 then
    G := 0;
  if G > RGBMAX then
    G := RGBMAX;
  if B < 0 then
    B := 0;
  if B > RGBMAX then
    B := RGBMAX;

  Result := RGB(round(r), round(g), round(b));
end;
//--------------------------------------------------------------------------------------------------
end.

