unit themeu;

interface

uses Windows, Classes, SysUtils, Forms, Dialogs, StdCtrls, IniFiles,
  ActiveX, declu, toolu, gdip_gfx, GDIPAPI;

const MAX_REGION_POINTS = 128;

type
  PLayerBackground = ^TLayerBackground;
  TLayerBackground = record
    ImageFile: string;
    Image: Pointer;
    W: uint;
    H: uint;
    Margins: Windows.TRect;
    StretchStyle: TStretchStyle;
  end;

  TLayerSeparator = record
    ImageFile: string;
    Image: Pointer;
    W: uint;
    H: uint;
    Margins: Windows.TRect;
  end;

  TLayerImage = record
    Image: Pointer;
    W: uint;
    H: uint;
  end;

  { _Theme }

  _Theme = class
  private
    FSite: TBaseSite;
    FThemeName: string;
    FBlurRegion: string;
    FBlurRegionPoints: array [0..MAX_REGION_POINTS - 1] of windows.TPoint;
    FBlurRegionPointsCount: integer;
    FBlurRect: Windows.TRect;
    FBlurR: Windows.TSize;
    procedure SetBlurRegion(value: string);
    procedure SetSite(value: TBaseSite);
  public
    is_default: boolean;
    Background: TLayerBackground;
    Separator: TLayerSeparator;
    Indicator: TLayerImage;
    Stack: TLayerImage;
    ReflectionSize: integer;
    ItemsArea: Windows.TRect;
    ItemsArea2: Windows.TRect;
    BaseOffset: integer;
    Path: string;
    property BlurRegion: string read FBlurRegion write SetBlurRegion;
    property Site: TBaseSite read FSite write SetSite;

    constructor Create(aTheme: string; aSite: TBaseSite);
    destructor Destroy; override;

    procedure Clear;
    procedure ClearGraphics;
    function Load: boolean;
    procedure ReloadGraphics;
    function Save: boolean;
    procedure ImageAdjustRotate(image: Pointer);
    procedure SetTheme(atheme: string);
    function CorrectMargins(margins: Windows.TRect): Windows.TRect;
    function CorrectSize(size: Windows.TSize): Windows.TSize;
    function CorrectCoords(coord: Windows.TPoint; W, H: integer): Windows.TPoint;
    procedure DrawBackground(hGDIPGraphics: Pointer; r: GDIPAPI.TRect);
    function GetBackgroundRgn(r: GDIPAPI.TRect): HRGN;
    function BlurEnabled: boolean;

    procedure MakeDefaultTheme;
    procedure CheckExtractFileFromResource(ResourceName: PChar; Filename: string);
    procedure ExtractFileFromResource(ResourceName: PChar; Filename: string);
    procedure SearchThemes(ThemeName: string; lb: TListBox);
  end;

var theme: _Theme;

implementation
//------------------------------------------------------------------------------
constructor _Theme.Create(aTheme: string; aSite: TBaseSite);
begin
  FThemeName := aTheme;
  FSite := aSite;
  Clear;
  ClearGraphics;
  CheckExtractFileFromResource('DEFAULT_ICON', UnzipPath('%pp%\default.png'));
  CheckExtractFileFromResource('DEFAULT_BACKGROUND', UnzipPath('%pp%\themes\background.png'));
  CheckExtractFileFromResource('DEFAULT_INDICATOR', UnzipPath('%pp%\themes\indicator.png'));
  CheckExtractFileFromResource('DEFAULT_STACK', UnzipPath('%pp%\themes\stack.png'));
end;
//------------------------------------------------------------------------------
procedure _Theme.Clear;
begin
  Background.StretchStyle := ssStretch;
  Background.Margins := rect(0, 0, 0, 0);
  ItemsArea := rect(0, 0, 0, 0);
  ItemsArea2 := rect(5, 5, 5, 5);
  BaseOffset := 10;
  Separator.Margins := rect(0, 0, 0, 0);
  ReflectionSize := 0;
end;
//------------------------------------------------------------------------------
procedure _Theme.ClearGraphics;
begin
  if Indicator.image <> nil then
  begin
    try GdipDisposeImage(Indicator.image);
    except end;
    Indicator.image := nil;
  end;

  if Separator.image <> nil then
  begin
    try GdipDisposeImage(Separator.image);
    except end;
    Separator.image := nil;
  end;

  if Stack.image <> nil then
  begin
    try GdipDisposeImage(Stack.image);
    except end;
    Stack.image := nil;
  end;

  if Background.image <> nil then
  begin
    try GdipDisposeImage(Background.image);
    except end;
    Background.image := nil;
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.SetTheme(aTheme: string);
begin
  FThemeName := aTheme;
  Load;
end;
//------------------------------------------------------------------------------
function _Theme.Load: boolean;
var
  ini: TIniFile;
  section: string;
begin
  Result := false;
  is_default := false;

  Clear;

  // default theme //

  if not DirectoryExists(UnzipPath('%pp%\themes\') + FThemeName + '\') then
  begin
    FThemeName := 'Aero';
    if not DirectoryExists(UnzipPath('%pp%\themes\') + FThemeName + '\') then
    begin
      MakeDefaultTheme;
      Result := True;
      exit;
    end;
  end;

  // loading theme data //
  try
    Path := toolu.UnzipPath('%pp%\themes\') + FThemeName + '\';

    // background //
    ini := TIniFile.Create(Path + 'background.ini');
    Background.ImageFile := Trim(ini.ReadString('Background', 'Image', 'background.png'));
    // ObjectDock format //
    section := 'Background';
    if ini.SectionExists('BackgroundBottom') then section := 'BackgroundBottom';
    if ini.ValueExists(section, 'LeftWidth') then
    begin
      ItemsArea.Left := ini.ReadInteger(section, 'OutsideBorderLeft', 0);
      ItemsArea.Top := ini.ReadInteger(section, 'OutsideBorderTop', 0);
      ItemsArea.Right := ini.ReadInteger(section, 'OutsideBorderRight', 0);
      ItemsArea.Bottom := ini.ReadInteger(section, 'OutsideBorderBottom', 0);
      Background.Margins.Left := ini.ReadInteger(section, 'LeftWidth', 0);
      Background.Margins.Top := ini.ReadInteger(section, 'TopHeight', 0);
      Background.Margins.Right := ini.ReadInteger(section, 'RightWidth', 0);
      Background.Margins.Bottom := ini.ReadInteger(section, 'BottomHeight', 0);
    end;
    // RocketDock format //
    if ini.ValueExists(section, 'LeftMargin') then
    begin
      Background.Margins.Left := ini.ReadInteger(section, 'LeftMargin', 0);
      Background.Margins.Top := ini.ReadInteger(section, 'TopMargin', 0);
      Background.Margins.Right := ini.ReadInteger(section, 'RightMargin', 0);
      Background.Margins.Bottom := ini.ReadInteger(section, 'BottomMargin', 0);
      ItemsArea.Left := ini.ReadInteger(section, 'Outside-LeftMargin', 0);
      ItemsArea.Top := ini.ReadInteger(section, 'Outside-TopMargin', 0);
      ItemsArea.Right := ini.ReadInteger(section, 'Outside-RightMargin', 0);
      ItemsArea.Bottom := ini.ReadInteger(section, 'Outside-BottomMargin', 0);
    end;
    // Terry-specific keys //
    ItemsArea2.Left := ini.ReadInteger(section, 'OutsideBorderLeft2', 5);
    ItemsArea2.Top := ini.ReadInteger(section, 'OutsideBorderTop2', 5);
    ItemsArea2.Right := ini.ReadInteger(section, 'OutsideBorderRight2', 5);
    ItemsArea2.Bottom := ini.ReadInteger(section, 'OutsideBorderBottom2', 5);
    BaseOffset :=     ini.ReadInteger(section, 'BaseOffset', 10);
    ReflectionSize := ini.ReadInteger(section, 'ReflectionSize', 16);
    BlurRegion :=     ini.ReadString (section, 'BlurRegion', '');
    ini.Free;

    // separator //
    if FileExists(Path + 'separator.ini') then
    begin
      ini := TIniFile.Create(Path + 'separator.ini');
      section := 'Separator';
      if ini.SectionExists('SeparatorBottom') then section := 'SeparatorBottom';
      Separator.ImageFile := Trim(ini.ReadString(section, 'Image', 'separator.png'));
      if ini.ValueExists(section, 'LeftWidth') then Separator.Margins.Left := ini.ReadInteger(section, 'LeftWidth', 0);
      if ini.ValueExists(section, 'RightWidth') then Separator.Margins.Right := ini.ReadInteger(section, 'RightWidth', 0);
      if ini.ValueExists(section, 'TopHeight') then Separator.Margins.Top := ini.ReadInteger(section, 'TopHeight', 0);
      if ini.ValueExists(section, 'BottomHeight') then Separator.Margins.Bottom := ini.ReadInteger(section, 'BottomHeight', 0);
      if ini.ValueExists(section, 'LeftMargin') then Separator.Margins.Left := ini.ReadInteger(section, 'LeftMargin', 0);
      if ini.ValueExists(section, 'RightMargin') then Separator.Margins.Right := ini.ReadInteger(section, 'RightMargin', 0);
      if ini.ValueExists(section, 'TopMargin') then Separator.Margins.Top := ini.ReadInteger(section, 'TopMargin', 0);
      if ini.ValueExists(section, 'BottomMargin') then Separator.Margins.Bottom := ini.ReadInteger(section, 'BottomMargin', 0);
      ini.Free;
    end else begin
      Separator.ImageFile := 'separator.png';
      Separator.Margins := rect(0, 0, 0, 0);
    end;

    ReloadGraphics;
    Result := True;
  except
    on e: Exception do raise Exception.Create('Error loading theme'#13#10#13#10 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _Theme.Save: boolean;
var
  ini: TIniFile;
  themes_path: string;
begin
  result := false;
  themes_path := toolu.UnzipPath('%pp%\themes\');

  if not DirectoryExists(themes_path) then CreateDirectory(PChar(themes_path), nil);

  if FThemeName = '' then FThemeName := 'Aero';
  if not DirectoryExists(themes_path + FThemeName + '\') then
    CreateDirectory(PChar(themes_path + FThemeName + '\'), nil);

  try
    windows.DeleteFile(PChar(themes_path + FThemeName + '\background.ini'));
    windows.DeleteFile(PChar(themes_path + FThemeName + '\separator.ini'));

    // background //
    ini := TIniFile.Create(themes_path + FThemeName + '\background.ini');
    ini.WriteString('Background', 'Image', Background.ImageFile);
    ini.WriteInteger('Background', 'OutsideBorderLeft',   ItemsArea.Left);
    ini.WriteInteger('Background', 'OutsideBorderTop',    ItemsArea.Top);
    ini.WriteInteger('Background', 'OutsideBorderRight',  ItemsArea.Right);
    ini.WriteInteger('Background', 'OutsideBorderBottom', ItemsArea.Bottom);
    ini.WriteInteger('Background', 'LeftWidth',    Background.Margins.Left);
    ini.WriteInteger('Background', 'TopHeight',    Background.Margins.Top);
    ini.WriteInteger('Background', 'RightWidth',   Background.Margins.Right);
    ini.WriteInteger('Background', 'BottomHeight', Background.Margins.Bottom);
    // terry-specific
    if ItemsArea2.Left <> 5 then   ini.WriteInteger('Background', 'OutsideBorderLeft2',   ItemsArea2.Left);
    if ItemsArea2.Top <> 5 then    ini.WriteInteger('Background', 'OutsideBorderTop2',    ItemsArea2.Top);
    if ItemsArea2.Right <> 5 then  ini.WriteInteger('Background', 'OutsideBorderRight2',  ItemsArea2.Right);
    if ItemsArea2.Bottom <> 5 then ini.WriteInteger('Background', 'OutsideBorderBottom2', ItemsArea2.Bottom);
    if BaseOffset <> 10 then       ini.WriteInteger('Background', 'BaseOffset', BaseOffset);
    if ReflectionSize <> 16 then   ini.WriteInteger('Background', 'ReflectionSize', ReflectionSize);
    if BlurRegion <> '' then       ini.WriteString ('Background', 'BlurRegion', BlurRegion);
    ini.Free;
    // separator //
    ini := TIniFile.Create(themes_path + FThemeName + '\separator.ini');
    ini.WriteString('Separator', 'Image', Separator.ImageFile);
    ini.WriteInteger('Separator', 'LeftWidth', Separator.Margins.Left);
    ini.WriteInteger('Separator', 'TopHeight', Separator.Margins.Top);
    ini.WriteInteger('Separator', 'RightWidth', Separator.Margins.Right);
    ini.WriteInteger('Separator', 'BottomHeight', Separator.Margins.Bottom);
    ini.Free;

    result := true;
  except
    on e: Exception do raise Exception.Create('Error saving theme'#13#10#13#10 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.SetBlurRegion(value: string);
var
  idx: integer;
  str: string;
begin
  FBlurRegion := value;
  FBlurRegionPointsCount := 0;
  str := FBlurRegion;
  idx := 0;
  try
    while (str <> '') and (idx < 1024) do
    begin
      FBlurRegionPoints[idx].x := strtoint(trim( fetch(str, ',', true) ));
      FBlurRegionPoints[idx].y := strtoint(trim( fetch(str, ',', true) ));
      inc(idx);
    end;
  except
  end;
  FBlurRegionPointsCount := idx;

  if FBlurRegionPointsCount = 3 then
  begin
    FBlurRect.Left := FBlurRegionPoints[0].x;
    FBlurRect.Top := FBlurRegionPoints[0].y;
    FBlurRect.Right := FBlurRegionPoints[1].x;
    FBlurRect.Bottom := FBlurRegionPoints[1].y;
    FBlurR.cx := FBlurRegionPoints[2].x;
    FBlurR.cy := FBlurRegionPoints[2].y;
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.SetSite(value: TBaseSite);
begin
  FSite := value;
  ReloadGraphics;
end;
//------------------------------------------------------------------------------
procedure _Theme.ReloadGraphics;
var
  img: Pointer;
begin
  ClearGraphics;

  try
    // background image //
    try
      if FileExists(Path + Background.ImageFile) then
        GdipLoadImageFromFile(PWideChar(WideString(Path + Background.ImageFile)), Background.Image);
      if Background.image = nil then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\themes\background.png'))), Background.Image);
      if Background.image <> nil then
      begin
        ImageAdjustRotate(Background.Image);
        GdipGetImageWidth(Background.Image, Background.W);
        GdipGetImageHeight(Background.Image, Background.H);
        GdipCloneBitmapAreaI(0, 0, Background.W, Background.H, PixelFormat32bppPARGB, Background.Image, img);
        GdipDisposeImage(Background.Image);
        Background.Image := img;
        img := nil;
      end;
    except
      on e: Exception do raise Exception.Create('Error loading background: ' + Path + Background.ImageFile + #13#10#13#10 + e.message);
    end;

    // separator //
    try
      if FileExists(Path + Separator.ImageFile) then
        GdipLoadImageFromFile(PWideChar(WideString(Path + Separator.ImageFile)), Separator.Image);
      if Separator.Image <> nil then
      begin
        ImageAdjustRotate(Separator.Image);
        GdipGetImageWidth(Separator.Image, Separator.W);
        GdipGetImageHeight(Separator.Image, Separator.H);
      end;
    except
      on e: Exception do raise Exception.Create('Error loading separator: ' + Path + Separator.ImageFile + #13#10#13#10 + e.message);
    end;

    // stack default icon //
    try
      if FileExists(Path + 'stack.png') then
        GdipLoadImageFromFile(PWideChar(WideString(Path + 'stack.png')), Stack.Image);
      if Stack.image = nil then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\themes\stack.png'))), Stack.Image);
      if Stack.Image <> nil then
      begin
        GdipGetImageWidth(Stack.Image, Stack.W);
        GdipGetImageHeight(Stack.Image, Stack.H);
      end;
    except
      on e: Exception do raise Exception.Create('Error loading stack icon: ' + Path + 'stack.png' + #13#10#13#10 + e.message);
    end;

    // running indicator //
    try
      if FileExists(Path + 'indicator.png') then
        GdipLoadImageFromFile(PWideChar(WideString(Path + 'indicator.png')), Indicator.Image);
      if Indicator.image = nil then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\themes\indicator.png'))), Indicator.Image);
      if Indicator.Image <> nil then
      begin
        ImageAdjustRotate(Indicator.Image);
        GdipGetImageWidth(Indicator.Image, Indicator.W);
        GdipGetImageHeight(Indicator.Image, Indicator.H);
      end;
    except
      on e: Exception do raise Exception.Create('Error loading indicator: ' + Path + 'indicator.png' + #13#10#13#10 + e.message);
    end;

  except
    on e: Exception do raise Exception.Create('Error loading theme files. ' + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.ImageAdjustRotate(image: Pointer);
begin
  if image <> nil then
  begin
    if Fsite = bsLeft then GdipImageRotateFlip(image, Rotate90FlipNone)
    else if Fsite = bsTop then GdipImageRotateFlip(image, Rotate180FlipX)
    else if Fsite = bsRight then GdipImageRotateFlip(image, Rotate270FlipNone);
  end;
end;
//------------------------------------------------------------------------------
function _Theme.CorrectMargins(margins: Windows.TRect): Windows.TRect;
begin
  Result := margins;
  if Fsite = bsLeft then
  begin
    Result.Left := margins.Bottom;
    Result.Top := margins.Left;
    Result.Right := margins.Top;
    Result.Bottom := margins.Right;
  end
  else if Fsite = bsTop then
  begin
    Result.Top := margins.Bottom;
    Result.Bottom := margins.Top;
  end
  else if Fsite = bsRight then
  begin
    Result.Left := margins.Top;
    Result.Top := margins.Right;
    Result.Right := margins.Bottom;
    Result.Bottom := margins.Left;
  end;
end;
//------------------------------------------------------------------------------
function _Theme.CorrectSize(size: Windows.TSize): Windows.TSize;
begin
  Result := size;
  if (Fsite = bsLeft) or (Fsite = bsRight) then
  begin
    Result.cx := size.cy;
    Result.cy := size.cx;
  end;
end;
//------------------------------------------------------------------------------
function _Theme.CorrectCoords(coord: Windows.TPoint; W, H: integer): Windows.TPoint;
begin
  result.x := coord.x;
  result.y := coord.y;
  if Fsite = bsLeft then
  begin
    result.y := result.x;
    result.x := W - coord.y;
  end else
  if Fsite = bsTop then
  begin
    result.y := H - result.y;
  end else
  if Fsite = bsRight then
  begin
    result.x := result.y;
    result.y := H - coord.x;
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.DrawBackground(hGDIPGraphics: Pointer; r: GDIPAPI.TRect);
var
  marg, area: Windows.TRect;
begin
  marg := CorrectMargins(Background.Margins);
  area := CorrectMargins(ItemsArea);
  inc(marg.Left, area.Left);
  inc(marg.Top, area.Top);
  inc(marg.Right, area.Right);
  inc(marg.Bottom, area.Bottom);
  gdip_gfx.DrawEx(hGDIPGraphics, Background.Image, Background.W, Background.H,
    rect(r.x, r.y, r.Width, r.Height), marg, Background.StretchStyle);
end;
//------------------------------------------------------------------------------
function _Theme.BlurEnabled: boolean;
begin
  result := FBlurRegionPointsCount > 2;
end;
//------------------------------------------------------------------------------
function _Theme.GetBackgroundRgn(r: GDIPAPI.TRect): HRGN;
var
  bm: windows.TRect;
  ba: windows.TRect;
  br: windows.TSize;
  pts: array [0..MAX_REGION_POINTS - 1] of windows.TPoint;
  idx: integer;
begin
  result := 0;
  if not BlurEnabled then exit;

  if FBlurRegionPointsCount = 3 then
  begin
    ba := CorrectMargins(FBlurRect);
    br := CorrectSize(FBlurR);
    result := CreateRoundRectRgn(r.x + ba.Left, r.y + ba.Top, r.x + r.Width - ba.Right, r.y + r.Height - ba.Bottom, br.cx, br.cy);
  end else begin
    bm := CorrectMargins(Background.Margins);
    ba := CorrectMargins(ItemsArea);
    inc(bm.Left, ba.Left);
    inc(bm.Top, ba.Top);
    inc(bm.Right, ba.Right);
    inc(bm.Bottom, ba.Bottom);
    idx := 0;
    while idx < FBlurRegionPointsCount do
    begin
      pts[idx] := CorrectCoords(FBlurRegionPoints[idx], Background.W, Background.H);

      if pts[idx].x <= bm.Left then pts[idx].x := r.x + pts[idx].x
      else
      if pts[idx].x >= Background.W - bm.Right then pts[idx].x := r.x + r.Width - (Background.W - pts[idx].x)
      else
        pts[idx].x := r.x + round((pts[idx].x - bm.Left) * (r.Width - bm.Left - bm.Right) / (Background.W - bm.Left - bm.Right)) + bm.Left;

      if pts[idx].y <= bm.Top then pts[idx].y := r.y + pts[idx].y
      else
      if pts[idx].y >= Background.H - bm.Bottom then pts[idx].y := r.y + r.Height - (Background.H - pts[idx].y)
      else
        pts[idx].y := r.y + round((pts[idx].y - bm.Top) * (r.Height - bm.Top - bm.Bottom) / (Background.H - bm.Top - bm.Bottom)) + bm.Top;

      inc(idx);
    end;
    result := CreatePolygonRgn(pts, FBlurRegionPointsCount, WINDING);
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.MakeDefaultTheme;
begin
  is_default := True;

  Background.StretchStyle := ssStretch;
  Background.Margins := rect(0, 4, 0, 0);
  ItemsArea := rect(16, 11, 16, 3);
  BlurRegion := '';
  ReflectionSize := 8;
  Separator.Margins := rect(0, 0, 0, 0);

  Path := '';
  ReloadGraphics;
end;
//------------------------------------------------------------------------------
procedure _Theme.CheckExtractFileFromResource(ResourceName: PChar; filename: string);
begin
  if not FileExists(filename) then ExtractFileFromResource(ResourceName, filename);
end;
//------------------------------------------------------------------------------
procedure _Theme.ExtractFileFromResource(ResourceName: PChar; filename: string);
var
  rs: TResourceStream;
  fs: TFileStream;
  irs: IStream;
  ifs: IStream;
  Read, written: int64;
begin
  rs := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
  fs := TFileStream.Create(filename, fmCreate);
  irs := TStreamAdapter.Create(rs, soReference) as IStream;
  ifs := TStreamAdapter.Create(fs, soReference) as IStream;
  irs.CopyTo(ifs, rs.Size, Read, written);
  if (Read <> written) or (Read <> rs.Size) then
    messagebox(application.mainform.handle, 'Error writing file from resource', 'Terry.Theme.ExtractFileFromResource', 0);
  rs.Free;
  fs.Free;
end;
//------------------------------------------------------------------------------
procedure _Theme.SearchThemes(ThemeName: string; lb: TListBox);
var
  ThemesDir: string;
  fhandle: HANDLE;
  f: TWin32FindData;
  idx: integer;
begin
  ThemesDir := toolu.UnzipPath('%pp%\themes\');
  lb.items.BeginUpdate;
  lb.items.Clear;
  ThemesDir := IncludeTrailingPathDelimiter(ThemesDir);

  fhandle := FindFirstFile(PChar(ThemesDir + '*.*'), f);
  if not (fhandle = HANDLE(-1)) then
    if ((f.dwFileAttributes and 16) = 16) then lb.items.add(AnsiToUTF8(f.cFileName));
  while FindNextFile(fhandle, f) do
    if ((f.dwFileAttributes and 16) = 16) then lb.items.add(AnsiToUTF8(f.cFileName));
  if not (fhandle = HANDLE(-1)) then Windows.FindClose(fhandle);

  idx := 0;
  while idx < lb.items.Count do
    if (lb.items.strings[idx] = '.') or (lb.items.strings[idx] = '..') or
      not FileExists(ThemesDir + UTF8ToAnsi(lb.items.strings[idx]) + '\background.ini') then
      lb.items.Delete(idx)
    else
      Inc(idx);

  lb.ItemIndex := lb.items.indexof(AnsiToUTF8(ThemeName));
  lb.items.EndUpdate;
end;
//------------------------------------------------------------------------------
destructor _Theme.Destroy;
begin
  Clear;
  ClearGraphics;
  inherited;
end;
//------------------------------------------------------------------------------
end.

