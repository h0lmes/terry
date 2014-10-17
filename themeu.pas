unit themeu;

interface

uses Windows, Classes, SysUtils, Forms, Dialogs, StdCtrls, IniFiles,
  ActiveX, GDIPAPI, declu, gdip_gfx;

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
    BaseCmd: TBaseCmd;
    FBlurRegion: string;
    FBlurRegionPoints: array [0..MAX_REGION_POINTS - 1] of windows.TPoint;
    FBlurRegionPointsCount: integer;
    FBlurRect: Windows.TRect;
    FBlurR: Windows.TSize;
    procedure SetBlurRegion(value: string);
  public
    is_default: boolean;
    Background: TLayerBackground;
    Separator: TLayerSeparator;
    Indicator: TLayerImage;
    Stack: TLayerImage;
    DropIndicatorAdd: TLayerImage;
    DropIndicatorRun: TLayerImage;
    ReflectionSize: integer;
    ItemsArea: Windows.TRect;
    Path: string;
    property BlurRegion: string read FBlurRegion write SetBlurRegion;

    constructor Create(ABaseCmd: TBaseCmd);
    destructor Destroy; override;

    procedure Clear;
    procedure ClearGraphics;
    procedure DoThemeChanged;
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

var
  theme: _Theme;

implementation
uses setsu, toolu;
//------------------------------------------------------------------------------
constructor _Theme.Create(ABaseCmd: TBaseCmd);
begin
  BaseCmd := ABaseCmd;
  Clear;
  ClearGraphics;

  CheckExtractFileFromResource('DEFAULT_ICON', UnzipPath('%pp%\default.png'));
  CheckExtractFileFromResource('DEFAULT_BACKGROUND', UnzipPath('%pp%\themes\background.png'));
  CheckExtractFileFromResource('DEFAULT_INDICATOR', UnzipPath('%pp%\themes\indicator.png'));
  CheckExtractFileFromResource('DEFAULT_DROPINDICATOR_ADD', UnzipPath('%pp%\themes\dropindicator_add.png'));
  CheckExtractFileFromResource('DEFAULT_DROPINDICATOR_RUN', UnzipPath('%pp%\themes\dropindicator_run.png'));
  CheckExtractFileFromResource('DEFAULT_STACK', UnzipPath('%pp%\themes\stack.png'));
end;
//------------------------------------------------------------------------------
procedure _Theme.Clear;
begin
  Background.StretchStyle := ssStretch;
  Background.Margins := rect(0, 0, 0, 0);
  ItemsArea := rect(0, 0, 0, 0);
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

  if DropIndicatorAdd.image <> nil then
  begin
    try GdipDisposeImage(DropIndicatorAdd.image);
    except end;
    DropIndicatorAdd.image := nil;
  end;

  if DropIndicatorRun.image <> nil then
  begin
    try GdipDisposeImage(DropIndicatorRun.image);
    except end;
    DropIndicatorRun.image := nil;
  end;

  if Background.image <> nil then
  begin
    try GdipDisposeImage(Background.image);
    except end;
    Background.image := nil;
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.DoThemeChanged;
begin
  BaseCmd(tcThemeChanged, 0);
end;
//------------------------------------------------------------------------------
procedure _Theme.SetTheme(aTheme: string);
begin
  StrCopy(sets.container.ThemeName, PChar(aTheme));
  Load;
  DoThemeChanged;
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

  if not DirectoryExists(toolu.UnzipPath('%pp%\themes\') + PChar(@sets.container.ThemeName) + '\') then
  begin
    StrCopy(sets.container.ThemeName, 'Aero');
    if not DirectoryExists(toolu.UnzipPath('%pp%\themes\') + PChar(@sets.container.ThemeName) + '\') then
    begin
      MakeDefaultTheme;
      Result := True;
      exit;
    end;
  end;

  // loading theme data //
  try
    Path := toolu.UnzipPath('%pp%\themes\') + PChar(@sets.container.ThemeName) + '\';

    // background //
    ini := TIniFile.Create(Path + 'background.ini');
    Background.ImageFile := Trim(ini.ReadString('Background', 'Image', 'background.png'));
    // ObjectDock format //
    section := 'Background';
    if ini.SectionExists('BackgroundBottom') then section := 'BackgroundBottom';
    if ini.ValueExists(section, 'LeftWidth') then
    begin
      ItemsArea.Left := strtoint(Trim(ini.ReadString(section, 'OutsideBorderLeft', '0')));
      ItemsArea.Top := strtoint(Trim(ini.ReadString(section, 'OutsideBorderTop', '0')));
      ItemsArea.Right := strtoint(Trim(ini.ReadString(section, 'OutsideBorderRight', '0')));
      ItemsArea.Bottom := strtoint(Trim(ini.ReadString(section, 'OutsideBorderBottom', '0')));
      Background.Margins.Left := strtoint(Trim(ini.ReadString(section, 'LeftWidth', '0')));
      Background.Margins.Top := strtoint(Trim(ini.ReadString(section, 'TopHeight', '0')));
      Background.Margins.Right := strtoint(Trim(ini.ReadString(section, 'RightWidth', '0')));
      Background.Margins.Bottom := strtoint(Trim(ini.ReadString(section, 'BottomHeight', '0')));
    end;
    // RocketDock format //
    if ini.ValueExists(section, 'LeftMargin') then
    begin
      Background.Margins.Left := strtoint(Trim(ini.ReadString(section, 'LeftMargin', '0')));
      Background.Margins.Top := strtoint(Trim(ini.ReadString(section, 'TopMargin', '0')));
      Background.Margins.Right := strtoint(Trim(ini.ReadString(section, 'RightMargin', '0')));
      Background.Margins.Bottom := strtoint(Trim(ini.ReadString(section, 'BottomMargin', '0')));
      ItemsArea.Left := strtoint(Trim(ini.ReadString(section, 'Outside-LeftMargin', '0')));
      ItemsArea.Top := strtoint(Trim(ini.ReadString(section, 'Outside-TopMargin', '0')));
      ItemsArea.Right := strtoint(Trim(ini.ReadString(section, 'Outside-RightMargin', '0')));
      ItemsArea.Bottom := strtoint(Trim(ini.ReadString(section, 'Outside-BottomMargin', '0')));
    end;
    // Terry-specific keys //
    BlurRegion := ini.ReadString(section, 'BlurRegion', '');
    ReflectionSize := StrToInt(ini.ReadString(section, 'ReflectionHeight', '0'));
    ini.Free;

    // separator //
    if FileExists(Path + 'separator.ini') then
    begin
      ini := TIniFile.Create(Path + 'separator.ini');
      section := 'Separator';
      if ini.SectionExists('SeparatorBottom') then section := 'SeparatorBottom';
      Separator.ImageFile := Trim(ini.ReadString(section, 'Image', 'separator.png'));
      Separator.Margins.Left := strtoint(Trim(ini.ReadString(section, 'LeftWidth', '0')));
      Separator.Margins.Top := strtoint(Trim(ini.ReadString(section, 'TopHeight', '0')));
      Separator.Margins.Right := strtoint(Trim(ini.ReadString(section, 'RightWidth', '0')));
      Separator.Margins.Bottom := strtoint(Trim(ini.ReadString(section, 'BottomHeight', '0')));
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

  if PChar(@sets.container.ThemeName) = '' then StrCopy(sets.container.ThemeName, 'Aero');
  if not DirectoryExists(themes_path + PChar(@sets.container.ThemeName) + '\') then
    CreateDirectory(PChar(themes_path + PChar(@sets.container.ThemeName) + '\'), nil);

  try
    windows.DeleteFile(PChar(themes_path + PChar(@sets.container.ThemeName) + '\background.ini'));
    windows.DeleteFile(PChar(themes_path + PChar(@sets.container.ThemeName) + '\separator.ini'));

    // background //
    ini := TIniFile.Create(themes_path + PChar(@sets.container.ThemeName) + '\background.ini');
    ini.WriteString('Background', 'Image', Background.ImageFile);
    ini.WriteString('Background', 'OutsideBorderLeft', inttostr(ItemsArea.Left));
    ini.WriteString('Background', 'OutsideBorderTop', inttostr(ItemsArea.Top));
    ini.WriteString('Background', 'OutsideBorderRight', inttostr(ItemsArea.Right));
    ini.WriteString('Background', 'OutsideBorderBottom', inttostr(ItemsArea.Bottom));
    ini.WriteString('Background', 'LeftWidth', inttostr(Background.Margins.Left));
    ini.WriteString('Background', 'TopHeight', inttostr(Background.Margins.Top));
    ini.WriteString('Background', 'RightWidth', inttostr(Background.Margins.Right));
    ini.WriteString('Background', 'BottomHeight', inttostr(Background.Margins.Bottom));
    ini.WriteString('Background', 'BlurRegion', BlurRegion);
    ini.WriteString('Background', 'ReflectionHeight', IntToStr(ReflectionSize));
    ini.Free;
    // separator //
    ini := TIniFile.Create(themes_path + PChar(@sets.container.ThemeName) + '\separator.ini');
    ini.WriteString('Separator', 'Image', Separator.ImageFile);
    ini.WriteString('Separator', 'LeftWidth', inttostr(Separator.Margins.Left));
    ini.WriteString('Separator', 'TopHeight', inttostr(Separator.Margins.Top));
    ini.WriteString('Separator', 'RightWidth', inttostr(Separator.Margins.Right));
    ini.WriteString('Separator', 'BottomHeight', inttostr(Separator.Margins.Bottom));
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

    // drop indicator add //
    try
      if FileExists(Path + 'dropindicator_add.png') then
        GdipLoadImageFromFile(PWideChar(WideString(Path + 'dropindicator_add.png')), DropIndicatorAdd.Image);
      if DropIndicatorAdd.image = nil then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\themes\dropindicator_add.png'))), DropIndicatorAdd.Image);
      if DropIndicatorAdd.Image <> nil then
      begin
        GdipGetImageWidth(DropIndicatorAdd.Image, DropIndicatorAdd.W);
        GdipGetImageHeight(DropIndicatorAdd.Image, DropIndicatorAdd.H);
      end;
    except
      on e: Exception do raise Exception.Create('Error loading drop indicator add: ' + Path + 'dropindicator_add.png' + #13#10#13#10 + e.message);
    end;

    // drop indicator run //
    try
      if FileExists(Path + 'dropindicator_run.png') then
        GdipLoadImageFromFile(PWideChar(WideString(Path + 'dropindicator_run.png')), DropIndicatorRun.Image);
      if DropIndicatorRun.image = nil then
        GdipLoadImageFromFile(PWideChar(WideString(UnzipPath('%pp%\themes\dropindicator_run.png'))), DropIndicatorRun.Image);
      if DropIndicatorRun.Image <> nil then
      begin
        GdipGetImageWidth(DropIndicatorRun.Image, DropIndicatorRun.W);
        GdipGetImageHeight(DropIndicatorRun.Image, DropIndicatorRun.H);
      end;
    except
      on e: Exception do raise Exception.Create('Error loading drop indicator run: ' + Path + 'dropindicator_run.png' + #13#10#13#10 + e.message);
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
    if sets.container.site = bsLeft then GdipImageRotateFlip(image, Rotate90FlipNone)
    else if sets.container.site = bsTop then GdipImageRotateFlip(image, Rotate180FlipX)
    else if sets.container.site = bsRight then GdipImageRotateFlip(image, Rotate270FlipNone);
  end;
end;
//------------------------------------------------------------------------------
function _Theme.CorrectMargins(margins: Windows.TRect): Windows.TRect;
begin
  Result := margins;
  if sets.container.site = bsLeft then
  begin
    Result.Left := margins.Bottom;
    Result.Top := margins.Left;
    Result.Right := margins.Top;
    Result.Bottom := margins.Right;
  end
  else if sets.container.site = bsTop then
  begin
    Result.Top := margins.Bottom;
    Result.Bottom := margins.Top;
  end
  else if sets.container.site = bsRight then
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
  if (sets.container.site = bsLeft) or (sets.container.site = bsRight) then
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
  if sets.container.site = bsLeft then
  begin
    result.y := result.x;
    result.x := W - coord.y;
  end else
  if sets.container.site = bsTop then
  begin
    result.y := H - result.y;
  end else
  if sets.container.site = bsRight then
  begin
    result.x := result.y;
    result.y := H - coord.x;
  end;
end;
//------------------------------------------------------------------------------
procedure _Theme.DrawBackground(hGDIPGraphics: Pointer; r: GDIPAPI.TRect);
var
  marg: Windows.TRect;
begin
  marg := CorrectMargins(Background.Margins);
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
  Background.Margins := rect(15, 15, 15, 2);
  ItemsArea := rect(16, 11, 16, 3);
  BlurRegion := '';
  ReflectionSize := 0;
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

