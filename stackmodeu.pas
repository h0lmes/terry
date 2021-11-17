unit stackmodeu;

{$mode delphi}

interface

uses Classes, SysUtils, Math, declu;

const
  MODE_COUNT = 9;
  MAX_DISTORT = 10;
  PI = 3.14159;
  DEFMODE_BIG = 4;
  DEFMODE_SMALL = 1;

type
  TStackItemData = packed record
    x: integer;
    y: integer;
    s: integer;
    alpha: integer;
    angle: single;
    hint_align: integer;
    hint_alpha: integer;
  end;

  TStackModeController = class(TObject)
  private
    names: array [0..MODE_COUNT] of string;
    function GetFan(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetFanAlt(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetTable(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetCards(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetLine(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetDoubleLine(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetSun(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetParallelWave(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
    function GetParallel(Opening, ShowHint: boolean; Index: integer; Progress: extended;
        ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
  public
    constructor Create;
    function GetModeCount: integer;
    function GetModeName(Mode: integer): string;
    function GetStep(Mode: integer; ItemCount: integer): extended;
    function GetItemData(Mode: integer; Opening, ShowHint: boolean; Progress: extended;
        Index, ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
  end;

var
  mc: TStackModeController;

implementation
//------------------------------------------------------------------------------
constructor TStackModeController.Create;
begin
  names[0] := 'Auto';
  names[1] := 'Fan';
  names[2] := 'Fan Alt';
  names[3] := 'Table';
  names[4] := 'Cards';
  names[5] := 'Line';
  names[6] := 'Double Line';
  names[7] := 'Sun';
  names[8] := 'Parallel Wave';
  names[9] := 'Parallel';
end;
//------------------------------------------------------------------------------
function TStackModeController.GetModeCount: integer;
begin
  result := MODE_COUNT + 1;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetModeName(Mode: integer): string;
begin
  result := names[mode];
end;
//------------------------------------------------------------------------------
function TStackModeController.GetStep(Mode: integer; ItemCount: integer): extended;
begin
  if mode = 0 then
  begin
    if itemCount > 15 then mode := DEFMODE_BIG else mode := DEFMODE_SMALL;
  end;
  case Mode of
    7, 8:
      begin
        result := 0.1 / ItemCount;
        if result < 0.01 then result := 0.01;
      end;
    else result := 0.05;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetItemData(Mode: integer; Opening, ShowHint: boolean; Progress: extended;
    Index, ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
begin
  if Mode = 0 then
  begin
    if ItemCount > 15 then Mode := DEFMODE_BIG else Mode := DEFMODE_SMALL;
  end;

  if Progress < 0 then Progress := 0;
  if Progress > 1 then Progress := 1;

  case Mode of
    1: result := GetFan         (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    2: result := GetFanAlt      (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    3: result := GetTable       (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    4: result := GetCards       (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    5: result := GetLine        (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    6: result := GetDoubleLine  (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    7: result := GetSun         (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    8: result := GetParallelWave(Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
    9: result := GetParallel    (Opening, ShowHint, Index, Progress, ItemCount, Site, ItemSize, Offset, Distort);
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetFan(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  result.s := ItemSize;
  result.alpha := round(255 * max(0, (progress - index / ItemCount) * 1 / (1 - index / ItemCount)));
  result.hint_alpha := 0;
  if Progress = 1 then result.hint_alpha := 255;
  // one degree per step (per icon) //
  inc(index);
  x := (ItemSize + 4) * index * cos(progress * index / 360 * PI * Distort) * progress;
  y := (ItemSize + 4) * index * sin(progress * index / 360 * PI * Distort) * progress;
  x := x + 4 + Offset;
  result.angle := progress * index * Distort;
  case Site of
    0: begin
        result.x := round(x);
        result.y := round(y);
        result.hint_align := HA_VERTICAL_TOP;
      end;
    2: begin
        result.x := -round(x);
        result.y := round(y);
        result.angle := 360 - result.angle;
        result.hint_align := HA_VERTICAL_TOP;
      end;
    1: begin
        result.x := -round(y);
        result.y := round(x);
        result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
    3: begin
        result.x := -round(y);
        result.y := -round(x);
        result.angle := 360 - result.angle;
        result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetFanAlt(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  result.s := ItemSize;
  result.alpha := round(255 * max(0, (progress - index / ItemCount) * 1 / (1 - index / ItemCount)));
  result.hint_alpha := 0;
  if Progress = 1 then result.hint_alpha := 255;
  // one degree per step (per icon) //
  inc(index);
  x := (ItemSize + 4) * index * cos(progress * index / 360 * PI * Distort) * progress;
  y := (ItemSize + 4) * index * sin(progress * index / 360 * PI * Distort) * progress;
  x := x + 4 + Offset;
  result.angle := 360 - progress * index * Distort;
  case Site of
    0: begin
        result.x := round(x);
        result.y := -round(y);
        result.hint_align := HA_VERTICAL_BOTTOM;
      end;
    2: begin
        result.x := -round(x);
        result.y := -round(y);
        result.angle := 360 - result.angle;
        result.hint_align := HA_VERTICAL_BOTTOM;
      end;
    1: begin
        result.x := round(y);
        result.y := round(x);
        result.hint_align := HA_HORIZONTAL_LEFT;
      end;
    3: begin
        result.x := round(y);
        result.y := -round(x);
        result.angle := 360 - result.angle;
        result.hint_align := HA_HORIZONTAL_LEFT;
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetTable(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  cols, rows, hSpace, vSpace: integer;
  x, y, d, s: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  hSpace := 50;
  vSpace := 40;
  if not ShowHint then vSpace := 10;
  s := 0.9 + (sin(progress * PI / 2) * 0.1);
  result.alpha := round(255 * progress);
  result.hint_alpha := 255;
  result.hint_align := HA_HORIZONTAL_BOTTOM;
  d := Distort * 3;
  result.s := ItemSize;
  cols := ceil(sqrt(ItemCount));
  rows := ceil(ItemCount / cols);
  if rows * cols > ItemCount then
  begin
    inc(cols);
    rows := ceil(ItemCount / cols);
  end;
  if rows * cols > ItemCount then
  begin
    dec(cols);
    rows := ceil(ItemCount / cols);
  end;
  Offset += 20 + vSpace;

  case Site of
    0: begin
        x := (Offset + ItemSize div 2) + (ItemSize + hSpace + d) * (index mod cols);
        if (index div cols = rows - 1) and (ItemCount mod cols <> 0) then x := x + (ItemSize + hSpace + d) * (cols - ItemCount mod cols) / 2;
        y := (ItemSize + vSpace) * (index div cols) - (ItemSize + vSpace) * (rows - 1) / 2;
        result.x := round(x * s);
        result.y := round(y * s);
      end;
    2: begin
        x := progress * (Offset + ItemSize div 2) + (ItemSize + hSpace + d) * (cols - 1 - index mod cols);
        if (index div cols = rows - 1) and (ItemCount mod cols <> 0) then x := x + (ItemSize + hSpace + d) * (cols - ItemCount mod cols) / 2;
        y := (ItemSize + vSpace) * (index div cols) - (ItemSize + vSpace) * (rows - 1) / 2;
        result.x := -round(x * s);
        result.y := round(y * s);
      end;
    1: begin
        x := (ItemSize + hSpace + d) * (index mod cols) - (ItemSize + hSpace + d) * (cols - 1) / 2;
        if (index div cols = rows - 1) and (ItemCount mod cols <> 0) then x := x + (ItemSize + hSpace + d) * (cols - ItemCount mod cols) / 2;
        y := (Offset + ItemSize div 2) + (ItemSize + vSpace) * (index div cols);
        result.x := round(x * s);
        result.y := round(y * s);
      end;
    3: begin
        x := (ItemSize + hSpace + d) * (index mod cols) - (ItemSize + hSpace + d) * (cols - 1) / 2;
        if (index div cols = rows - 1) and (ItemCount mod cols <> 0) then x := x + (ItemSize + hSpace + d) * (cols - ItemCount mod cols) / 2;
        y := (Offset + ItemSize div 2) + (ItemSize + vSpace) * (rows - 1 - index div cols);
        result.x := round(x * s);
        result.y := -round(y * s);
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetCards(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  cols, rows, hSpace, vSpace: integer;
  x, y, d: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  hSpace := 50;
  vSpace := 40;
  if not ShowHint then vSpace := 10;
  //s := 0.9 + (sin(progress * PI / 2) * 0.1);
  result.alpha := round(255 * progress);
  result.hint_alpha := 0;
  if Progress = 1 then result.hint_alpha := 255;
  result.hint_align := HA_HORIZONTAL_BOTTOM;
  d := Distort * 3;
  result.s := ItemSize;
  cols := ceil(sqrt(ItemCount));
  rows := ceil(ItemCount / cols);
  if rows * cols > ItemCount then
  begin
    inc(cols);
    rows := ceil(ItemCount / cols);
  end;
  if rows * cols > ItemCount then
  begin
    dec(cols);
    rows := ceil(ItemCount / cols);
  end;
  Offset += 20 + vSpace;

  case Site of
    0: begin
        y := (ItemSize + vSpace) * (index div cols) * progress - (ItemSize + vSpace) * (rows - 1) * progress / 2;
        y := y * (1 - power(cos(progress * PI / 2), 2 / 3));
        result.x := round(progress * (Offset + ItemSize div 2) + (ItemSize + vSpace + d) * (index mod cols) * progress);
        result.y := round(y);
      end;
    2: begin
        y := (ItemSize + vSpace) * (index div cols) * progress - (ItemSize + vSpace) * (rows - 1) * progress / 2;
        y := y * (1 - power(cos(progress * PI / 2), 1 / 2));
        result.x := -round(progress * (Offset + ItemSize div 2) + (ItemSize + vSpace + d) * (cols - 1 - index mod cols) * progress);
        result.y := round(y);
      end;
    1: begin
        x := (ItemSize + hSpace + d) * (index mod cols) * progress - (ItemSize + hSpace + d) * (cols - 1) * progress / 2;
        x := x * (1 - power(cos(progress * PI / 2), 1 / 2));
        result.x := round(x);
        result.y := round(progress * (Offset + ItemSize div 2) + (ItemSize + vSpace) * (index div cols) * progress);
      end;
    3: begin
        x := (ItemSize + hSpace + d) * (index mod cols) * progress - (ItemSize + hSpace + d) * (cols - 1) * progress / 2;
        x := x * (1 - power(cos(progress * PI / 2), 1 / 2));
        result.x := round(x);
        result.y := -round(progress * (Offset + ItemSize div 2) + (ItemSize + vSpace) * (rows - 1 - index div cols) * progress);
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetLine(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y, d: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;
  Distort *= 2;

  // simultaneous item count = 4 //
  d := (progress * (ItemCount+4-1) / ItemCount - index / ItemCount) * ItemCount / 4;
  if d < 0 then d := 0;
  if d > 1 then d := 1;
  d := sin(d * PI / 2);
  result.s := round(ItemSize * d);
  result.alpha := round(255 * d);
  result.hint_alpha := trunc(max(progress - 0.5, 0) * 510);
  x := ItemSize + 4 + Offset + (ItemSize + Distort + 4) * index;
  y := 0;
  result.angle := 0;
  case Site of
    0: begin
        result.x := round(x);
        result.y := round(y);
        result.hint_align := HA_VERTICAL_TOP;
      end;
    2: begin
        result.x := -round(x);
        result.y := round(y);
        result.hint_align := HA_VERTICAL_TOP;
      end;
    1: begin
        result.x := -round(y);
        result.y := round(x);
        result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
    3: begin
        result.x := -round(y);
        result.y := -round(x);
        result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetDoubleLine(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y, d: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;
  Distort *= 2;

  // simultaneous item count = 4 //
  d := (progress * (ItemCount+4-1) / ItemCount - trunc(index div 2) / ItemCount) * ItemCount / 4;
  if d < 0 then d := 0;
  if d > 1 then d := 1;
  d := sin(d * PI / 2);
  result.s := round(ItemSize * d);
  result.alpha := round(255 * d);
  result.hint_alpha := trunc(max(progress - 0.5, 0) * 510);
  result.angle := 0;
  x := ItemSize + 4 + Offset + (ItemSize + Distort + 4) * (index div 2);
  y := ItemSize div 2 + 2 + Distort;
  if index mod 2 = 0 then y := -y;
  case Site of
    0: begin
        result.x := round(x);
        result.y := round(y);
        result.hint_align := HA_VERTICAL_BOTTOM;
        if index mod 2 = 0 then result.hint_align := HA_VERTICAL_TOP;
      end;
    2: begin
        result.x := -round(x);
        result.y := round(y);
        result.hint_align := HA_VERTICAL_BOTTOM;
        if index mod 2 = 0 then result.hint_align := HA_VERTICAL_TOP;
      end;
    1: begin
        result.x := -round(y);
        result.y := round(x);
        result.hint_align := HA_HORIZONTAL_LEFT;
        if index mod 2 = 0 then result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
    3: begin
        result.x := -round(y);
        result.y := -round(x);
        result.hint_align := HA_HORIZONTAL_LEFT;
        if index mod 2 = 0 then result.hint_align := HA_HORIZONTAL_RIGHT;
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetSun(
    Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y, ItemProgress, r, degreesPerStep, radiusMultiplier: extended;
  SimultItemCount: integer;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;
  radiusMultiplier := 9;
  SimultItemCount := ItemCount * 2;

  if Opening then
    ItemProgress := (Progress * (ItemCount + SimultItemCount - 1) / ItemCount - index / ItemCount) * ItemCount / SimultItemCount
  else
    ItemProgress := (Progress * (ItemCount + SimultItemCount - 1) / ItemCount - index / ItemCount) * ItemCount / SimultItemCount;
    //ItemProgress := (Progress * (ItemCount + SimultItemCount - 1) / ItemCount - (ItemCount - index - 1) / ItemCount) * ItemCount / SimultItemCount;
  ItemProgress := 0.5 + ItemProgress * 0.5;

  // (0 * (14 + 28 - 1) / 14 - 0 / 14) * 14 / 28
  // (0 * (14 + 28 - 1) / 14 - (14 - 0 - 1) / 14) * 14 / 28

  if ItemProgress < 0 then ItemProgress := 0;
  if ItemProgress > 1 then ItemProgress := 1;
  ItemProgress := sin(ItemProgress * PI / 2);

  result.alpha := round(max(ItemProgress - 0.75, 0) * 1020);
  result.hint_alpha := round(max(ItemProgress - 0.75, 0) * 1020);

  r := (ItemSize + 3) * 20 * radiusMultiplier / 2 / PI * (1 + Distort / 10);
  degreesPerStep := 18 / radiusMultiplier;
  if Opening then
  begin
    x := r * cos((index - (ItemCount - 1) / 2) * PI * degreesPerStep / 180) * (0.5 + ItemProgress / 2) * sin(ItemProgress * 1.3 * PI / 2) / sin(1.3 * PI / 2);
    y := r * sin((index - (ItemCount - 1) / 2) * PI * degreesPerStep / 180) * (0.5 + ItemProgress / 2) * sin(ItemProgress * 1.3 * PI / 2) / sin(1.3 * PI / 2);
    result.s := round(ItemSize * ItemProgress * sin(ItemProgress * 1.3 * PI / 2) / sin(1.3 * PI / 2));
  end else begin
    x := r * cos((index - (ItemCount - 1) / 2) * PI * degreesPerStep / 180) * (0.5 + ItemProgress / 2);
    y := r * sin((index - (ItemCount - 1) / 2) * PI * degreesPerStep / 180) * (0.5 + ItemProgress / 2);
    result.s := round(ItemSize * ItemProgress);
  end;
  x := x + Offset + ItemSize * 1.5 - r * cos(((ItemCount + 1) / 2) * PI * degreesPerStep / 180);
  result.angle := (index - (ItemCount - 1) / 2) * degreesPerStep;
  case Site of
    0: begin
        result.hint_align := HA_HORIZONTAL_RIGHT;
        result.x := round(x);
        result.y := round(y);
        if result.angle < 0 then result.angle := 360 + result.angle;
      end;
    2: begin
        result.hint_align := HA_HORIZONTAL_LEFT;
        result.x := -round(x);
        result.y := round(y);
        result.angle := -result.angle;
        if result.angle < 0 then result.angle := 360 + result.angle;
      end;
    1: begin
        result.hint_align := HA_VERTICAL_BOTTOM;
        result.x := round(y);
        result.y := round(x);
        result.angle := -result.angle;
        if result.angle < 0 then result.angle := 360 + result.angle;
      end;
    3: begin
        result.hint_align := HA_VERTICAL_TOP;
        result.x := round(y);
        result.y := -round(x);
        if result.angle < 0 then result.angle := 360 + result.angle;
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetParallelWave(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y, d: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  // simultaneous item count = 4 //
  if Opening then d := (progress * (ItemCount+4-1) / ItemCount - index / ItemCount) * ItemCount / 4
  else d := (progress * (ItemCount+4-1) / ItemCount - (ItemCount - index - 1) / ItemCount) * ItemCount / 4;
  if d < 0 then d := 0;
  if d > 1 then d := 1;
  d := sin(d * PI / 2);
  x := Offset + d * ItemSize * 2.5 - ItemSize;
  if Opening then
  begin
    y := (ItemSize + 3 + abs(Distort * 2)) * ((index - (ItemCount - 1) / 2));
    result.s := round(ItemSize * d * sin(d * 1.3 * PI / 2) / sin(1.3 * PI / 2));
  end else begin
    y := (ItemSize + 3 + abs(Distort * 2)) * ((index - (ItemCount - 1) / 2));
    result.s := round(ItemSize * d);
  end;
  result.angle := 0;
  result.alpha := round(255 * d);
  result.hint_alpha := round(max(d - 0.5, 0) * 510);
  case Site of
    0: begin
        result.hint_align := HA_HORIZONTAL_RIGHT;
        result.x := round(x);
        result.y := round(y);
      end;
    2: begin
        result.hint_align := HA_HORIZONTAL_LEFT;
        result.x := -round(x);
        result.y := round(y);
        result.angle := -result.angle;
      end;
    1: begin
        result.hint_align := HA_VERTICAL_BOTTOM;
        result.x := round(y);
        result.y := round(x);
        result.angle := -result.angle;
      end;
    3: begin
        result.hint_align := HA_VERTICAL_TOP;
        result.x := round(y);
        result.y := -round(x);
      end;
  end;
end;
//------------------------------------------------------------------------------
function TStackModeController.GetParallel(Opening, ShowHint: boolean; Index: integer; Progress: extended;
    ItemCount, Site, ItemSize, Offset, Distort: integer): TStackItemData;
var
  x, y, p: extended;
begin
  result.x := 0;
  result.y := 0;
  result.alpha := 0;
  result.angle := 0;

  p := 0.7 + (sin(progress * PI / 2) * 0.3);
  result.alpha := round(255 * Progress);
  result.hint_alpha := round(max(Progress - 0.5, 0) * 510);
  result.s := ItemSize;
  result.angle := 0;
  x := Offset + p * ItemSize * 2.5 - ItemSize;
  y := (ItemSize + 3 + abs(Distort * 2)) * ((index - (ItemCount - 1) / 2)) * p;
  case Site of
    0: begin
        result.hint_align := HA_HORIZONTAL_RIGHT;
        result.x := round(x);
        result.y := round(y);
      end;
    2: begin
        result.hint_align := HA_HORIZONTAL_LEFT;
        result.x := -round(x);
        result.y := round(y);
        result.angle := -result.angle;
      end;
    1: begin
        result.hint_align := HA_VERTICAL_BOTTOM;
        result.x := round(y);
        result.y := round(x);
        result.angle := -result.angle;
      end;
    3: begin
        result.hint_align := HA_VERTICAL_TOP;
        result.x := round(y);
        result.y := -round(x);
      end;
  end;
end;
//------------------------------------------------------------------------------
initialization
  mc := TStackModeController.Create;
finalization
  mc.free;
end.

