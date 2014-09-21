unit itemmgru;

interface
uses Windows, Messages, Classes, SysUtils, Forms, IniFiles, Math, GDIPAPI,
  declu, gdip_gfx, themeu, setsu, processhlp,
  customitemu, scitemu, sepitemu, plgitemu, stackitemu, taskitemu;

const
  MAX_ITEM_COUNT = 128;

type
  PItem = ^TItem;
  TItem = record
    h: THandle;
    x: integer;
    y: integer;
    s: integer;
    xe: extended;
    ye: extended;
    se: extended;
  end;

  { _ItemManager }

  _ItemManager = class
  private
    ItemSize: integer;
    BigItemSize: integer;
    ZoomWidth: integer;
    ItemSpacing: integer;
    ZoomItems: boolean; // enables zoom //
    FZoomSmoothingLevel: integer; // indicates the nuber of transitional frames //
    Reflection: boolean;
    ReflectionSize: integer;
    // for smooth zooming in and out //
    // 0 <= ZoomItemSizeDiff <= (BigItemSize - ItemSize) //
    ZoomItemSizeDiff: integer;
    ZoomSpeed: integer;
    MoveSpeed: integer;
    // monitor index //
    Monitor: integer;
    BaseSite: TBaseSite;
    BaseSiteVertical: boolean;
    // Wnd Item Vars //
    FVisible: boolean;
    Enabled: boolean;
    FItemArea: windows.TRect; // updates in SetItems1
    LockMouseEffect: boolean;
    SetsFilename: string;
    FFirstRun: boolean;
    //
    procedure err(where: string; e: Exception; Critical: boolean = false);
    procedure notify(message: string);
    procedure DoSetForeground;
    procedure DoBaseDraw(flags: integer);
    procedure DoSaveSets;
    procedure SetVisible(value: boolean);

    // load/save //
    procedure AllItemsSave;
    procedure ItemSave(HWnd: uint);

    function GetTaskItemIndex(h: THandle): integer;

    //
    procedure SetItems1;
    procedure RecalcDock;
    procedure SetItems2(force_draw: boolean);

    //
    function IASize: integer;
    function ItemFromPoint(Ax, Ay, distance: integer): extended;
    function ItemRectFromPoint(Ax, Ay: integer): integer;

    // items //
    function ItemIndex(HWnd: HANDLE): integer;
    function ItemHWnd(index: integer): HANDLE;
    function AddItem(data: string; Update: boolean = false; Save: boolean = true): THandle;
    procedure CopyItemDescriptor(pFrom, pTo: PItem);
  public
    items: array [0..MAX_ITEM_COUNT - 1] of TItem; // static = more stable
    ItemCount: integer;
    TaskItemCount: integer;
    HoverItemHWnd: THandle; // handle of the item over which the mouse is //
    SelectedItemHWnd: THandle;
    itemsDeleted: TFPList;
    // pos/size of ItemManager //
    x: integer;
    y: integer;
    width: integer;
    height: integer;
    widthZoomed: integer;
    heightZoomed: integer;
    widthOverhead: integer; // used to stabilize file DragOver
    heightOverhead: integer;
    MonitorRect: Windows.TRect;
    BaseWindowRect: GDIPAPI.TRect; // rect of the dock main window. updated on every RecalcDock //
    BaseImageRect: GDIPAPI.TRect; // rect of the dock image. updated on every RecalcDock //
    ZoomInOutItem: extended; // relative mouse position on last WHMouseMove //
    Zooming: boolean; // true if mouse is over the panel and any items are zoomed //
    DropDistance: integer;
    LockDragging: boolean;
    Dragging: boolean; // indicates that item is being dragged over //
    DragHWnd: THandle; // handle of the item that is being dragged //
    DraggingFile: boolean; // indicates that file is being dragged over //
    DropPlace: integer; // index of free space //
    DropPlaceEx: integer; // index of a place to drop to //
    ParentHWnd: cardinal;
    BaseCmd: TBaseCmd;

    property FirstRun: boolean read FFirstRun;
    property Visible: boolean read FVisible write SetVisible;

    constructor Create(AEnabled, AVisible: boolean; Handle: THandle; ABaseCmd: TBaseCmd);
    destructor Destroy; override;
    procedure Enable(value: boolean);
    procedure SetParam(id: TGParam; value: integer);
    procedure command(cmd, params: string);
    function GetRect: windows.TRect;
    function GetZoomEdge: integer;

    procedure Timer;
    procedure SetTheme;
    procedure ItemsChanged(FullUpdate: boolean = false);

    // load & save //
    procedure Load(fsets: string);
    procedure Save(fsets: string);

    // task item procs //
    procedure Taskbar;
    procedure ClearTaskbar;

    // items //
    procedure Clear;
    procedure ClearDeleted;
    procedure UnDelete;
    function ZOrder(InsertAfter: uint): uint;
    procedure InsertItems(list: TStrings);
    procedure InsertItem(AData: string);
    function CreateItem(data: string): THandle;
    procedure DeleteItem(HWnd: THandle);

    // mouse effects //
    procedure CalcDropPlace(pt: windows.TPoint);
    procedure SetDropPlace(index: integer);
    procedure SetDropPlaceEx(index: integer);
    procedure Zoom(x, y: integer);
    procedure UnZoom(do_now: boolean = false);
    function CheckMouseOn: boolean;
    procedure WHMouseMove(pt: windows.TPoint; allow_zoom: boolean = true);
    procedure DragEnter;
    procedure DragOver;
    procedure DragLeave;
    procedure WMDeactivateApp;

    // Win Item Procs //
    procedure Undock(HWnd: HANDLE);
    procedure DockAdd(HWnd: THandle);
    procedure Dock(HWnd: HANDLE);
    function IsItem(HWnd: HANDLE): HANDLE;
    function ItemDropFile(HWndItem: HANDLE; pt: windows.TPoint; filename: string): boolean;
    function ItemDropFiles(HWndItem: HANDLE; pt: windows.TPoint; files: TStrings): boolean;
    function ItemCmd(HWnd: HANDLE; id: TGParam; param: integer): integer;
    function AllItemCmd(id: TGParam; param: integer): integer;
    procedure PluginCallCreate(HWnd: HANDLE);
    function GetPluginFile(HWnd: HANDLE): string;
    procedure SetPluginImage(HWnd: HANDLE; lpImageNew: Pointer; AutoDelete: boolean);
    procedure SetPluginOverlay(HWnd: HANDLE; lpOverlayNew: Pointer; AutoDelete: boolean);
    procedure PluginAnimate(HWnd: HANDLE);
    procedure SetPluginCaption(HWnd: HANDLE; NewCaption: string);
    function GetPluginCaption(HWnd: HANDLE): string;
    function GetPluginRect(HWnd: HANDLE): windows.TRect;
    function IsPluginUndocked(HWnd: HANDLE): boolean;
    function IsSeparator(HWnd: HANDLE): boolean;
    function IsTask(HWnd: HANDLE): boolean;
end;

implementation
uses frmterryu, DockH, toolu;
//------------------------------------------------------------------------------
constructor _ItemManager.Create(AEnabled, AVisible: boolean; Handle: THandle; ABaseCmd: TBaseCmd);
begin
  inherited Create;
  Enabled := AEnabled;
  FVisible := AVisible;
  ParentHWnd := Handle;
  BaseCmd := ABaseCmd;
  ItemCount := 0;
  TaskItemCount := 0;
  Zooming := false;
  ZoomItems := false;
  FZoomSmoothingLevel := 1;
  ItemSize := 40;
  BigItemSize := 90;
  ZoomWidth := 6;
  ZoomItemSizeDiff := 0;
  Reflection := false;
  Monitor := 0;
  DropDistance := 80;
  DropPlace := NOT_AN_ITEM;
  DropPlaceEx := NOT_AN_ITEM;
  ZoomSpeed := 1;
  MoveSpeed := 1;
  LockDragging := false;
  HoverItemHWnd := 0;
  SelectedItemHWnd := 0;
  Dragging := false;
  DragHWnd := 0;
  LockMouseEffect := false;
  itemsDeleted := TFPList.Create;
end;
//------------------------------------------------------------------------------
destructor _ItemManager.Destroy;
begin
  Enabled := false;
  Clear;
  ClearDeleted;
  itemsDeleted.Free;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.Enable(value: boolean);
begin
  Enabled := value;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.err(where: string; e: Exception; Critical: boolean = false);
begin
  if assigned(e) then where := where + ' '#10#13 + e.Message else where := where + ' '#10#13'Error';
  if Critical then
  begin
    messagebox(frmterry.handle,
      pchar(UTF8ToAnsi(XErrorCritical + ' ' + XErrorContactDeveloper) + #10#13#10#13 + where),
      'Terry', MB_ICONERROR);
    halt;
  end else
    raise Exception.Create(where);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.notify(message: string);
begin
  frmterry.notify(message);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetParam(id: TGParam; value: integer);
begin
  try
    AllItemCmd(id, value);

    case id of
      gpItemSize:
        begin
          ItemSize := value;
          ItemsChanged(true);
        end;
      gpBigItemSize: BigItemSize := value;
      gpZoomWidth:
        begin
          ZoomWidth := value;
          WHMouseMove(classes.point(-100, -100));
        end;
      gpItemSpacing:
        begin
          ItemSpacing := value;
          ItemsChanged(true);
        end;
      gpZoomItems:
        begin
          ZoomItems := boolean(value);
          ItemsChanged(true);
        end;
      gpSite:
        begin
          BaseSite := TBaseSite(value);
          BaseSiteVertical := (BaseSite = bsLeft) or (BaseSite = bsRight);
          ItemsChanged(true);
        end;
      gpDropDistance: DropDistance := value;
      gpLockDragging: LockDragging := boolean(value);
      gpReflection:
        begin
          Reflection := boolean(value);
          ItemsChanged(true);
        end;
      gpReflectionSize:
        begin
          ReflectionSize := value;
          ItemsChanged(true);
        end;
      gpZoomSpeed:
        begin
          ZoomSpeed := value;
          if ZoomSpeed > 6 then ZoomSpeed:= 6;
          if ZoomSpeed < 1 then ZoomSpeed:= 1;
        end;
      gpMoveSpeed:
        begin
          MoveSpeed := value;
          if MoveSpeed > 6 then MoveSpeed:= 6;
          if MoveSpeed < 1 then MoveSpeed:= 1;
        end;
      gpMonitor:
        begin
          Monitor := value;
          MonitorRect := frmterry.GetMonitorBoundsRect;
          ItemsChanged(true);
        end;
      gpLockMouseEffect: LockMouseEffect := value <> 0;
    end;
  except
    on e: Exception do err('ItemManager.SetParam', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.command(cmd, params: string);
var
  data: string;
begin
  if Enabled then
  try
    if cmd = 'clear' then Clear;
    if cmd = 'load' then
    begin
      if params = '' then params := sets.SetsPathFile;
      Load(params);
    end;
    if cmd = 'paste' then InsertItem(GetClipboard);

    if cmd = 'shortcut' then AddItem('class="shortcut";', true);
    if cmd = 'separator' then AddItem('class="separator";', true);
    if cmd = 'plugin' then AddItem('class="plugin";file="' + params + '";', true);
    if cmd = 'stack' then
    begin
      if length(params) = 0 then data := 'class="stack";'
      else data := 'class="stack";caption="::::";special_folder="' + params + '";';
      AddItem(data, true);
    end;
  except
    on e: Exception do err('ItemManager.Command::' + cmd + '(' + params + ')', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DoSetForeground;
begin
  if assigned(BaseCmd) then BaseCmd(tcZOrder, 0);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DoBaseDraw(flags: integer);
begin
  if assigned(BaseCmd) then BaseCmd(tcRepaintBase, flags);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DoSaveSets;
begin
  if assigned(BaseCmd) then BaseCmd(tcSaveSets, 0);
end;
//------------------------------------------------------------------------------
function _ItemManager.GetRect: windows.TRect;
begin
  result.Left := x;
  result.Top := y;
  result.Right := x + Width;
  result.Bottom := y + Height;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetZoomEdge: integer;
begin
  case BaseSite of
    bsLeft: result := BaseWindowRect.X + x + widthZoomed;
    bsTop: result := BaseWindowRect.Y + y + heightZoomed;
    bsRight: result := BaseWindowRect.X + x + width - widthZoomed;
    bsBottom: result := BaseWindowRect.Y + y + height - heightZoomed;
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//   LOAD AND SAVE
//
//
//
//------------------------------------------------------------------------------
procedure _ItemManager.Load(fsets: string);
var
  i: integer;
  cls_name, data: string;
  ini: TIniFile;
  list: TStrings;
  stack: TStackItem;
begin
  if fsets = '' then exit;
  FFirstRun := false;
  Dragging := false;
  DragHWnd := 0;

  try
    SetsFilename := toolu.UnzipPath(fsets);
    ini := TIniFile.Create(SetsFilename);
    list := TStringList.Create;
    ini.ReadSections(list);
  except
    on e: Exception do err('ItemManager.Load.ReadIni', e, true);
  end;

  // read items //
  try
    i := 0;
    while i < list.count do
    begin
      if pos('item', list.strings[i]) = 1 then
      begin
        cls_name := ini.ReadString(list.strings[i], 'class', 'shortcut');
        data := 'class="' + cls_name + '";inifile="' + fsets + '";inisection="' + list.strings[i] + '";';
        AddItem(data, false, false);
      end;
      inc(i);
    end;
    ini.free;
  except
    on e: Exception do err('ItemManager.Load.ReadItems', e, true);
  end;

  // create default items if nothing loaded //
  try
    if ItemCount = 0 then
    begin
      // "end session" stack //
      AddItem(TStackItem.Make(0, 'End session...', ''));
      stack := TStackItem(GetWindowLong(items[ItemCount - 1].h, GWL_USERDATA));
      if stack is TStackItem then
      begin
        stack.AddSubitem(TShortcutItem.Make(0, 'Shutdown', '/shutdown', '', '', 'images\apps\gnome-session-halt.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Reboot', '/reboot', '', '', 'images\apps\gnome-session-reboot-2.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Log out', '/logoff', '', '', 'images\apps\gnome-session-logout.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Suspend', '/suspend', '', '', 'images\apps\gnome-session-suspend-2.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Hibernate', '/hibernate', '', '', 'images\apps\gnome-session-hibernate-2.png'));
      end;
      AddItem('class="separator";');
      AddItem(TShortcutItem.Make(0, 'Computer', '::::14001F50E04FD020EA3A6910A2D808002B30309D0000', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Documents', '::::14001F4225481E03947BC34DB131E946B44C8DD5200000001A00EEBBFE23000010007DB10D7BD29C934A973346CC89022E7C00000000', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Control panel', '::::14001F706806EE260AA0D7449371BEB064C986830C0001008421DE390000000000000000', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Trash bin', '::::14001F7840F05F6481501B109F0800AA002F954E0000', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Program settings', '/sets', '', '', ICON_SETTINGS));
      FFirstRun := true;
    end;
  except
    on e: Exception do err('ItemManager.Load.Default', e);
  end;

  ItemsChanged;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.Save(fsets: string);
begin
  try
    if fsets <> '' then SetsFilename := toolu.UnzipPath(fsets);
    AllItemsSave;
  except
    on e: Exception do err('ItemManager.Save', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.AllItemsSave;
var
  i: integer;
begin
  if Enabled and (ItemCount > 0) then
  try
    for i := 0 to ItemCount - 1 do
    begin
      if items[i].h <> 0 then TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA)).Save(pchar(SetsFilename), pchar('item' + inttostr(i + 1)));
    end;
  except
    on e: Exception do raise Exception.Create('ItemManager.AllItemsSave::' + inttostr(i) + #10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.ItemSave(HWnd: uint);
var
  index: integer;
  Inst: TCustomItem;
begin
  try
    index := ItemIndex(HWnd);
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then Inst.Save(pchar(SetsFilename), pchar('item' + inttostr(index + 1)));
  except
    on e: Exception do raise Exception.Create('ItemManager.ItemSave::' + inttostr(index) + #10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//   ITEMS
//
//
//
//------------------------------------------------------------------------------
procedure _ItemManager.Clear;
var
  i: integer;
  ItemInstance: TCustomItem;
begin
  if ItemCount > 0 then
  try
    for i := 0 to ItemCount - 1 do
    begin
      ItemInstance := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
      FreeAndNil(ItemInstance);
      DestroyWindow(items[i].h);
      items[i].h := 0;
    end;
    ItemCount := 0;
    TaskItemCount := 0;
    //ItemsChanged;
  except
    on e: Exception do err('ItemManager.Clear', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.ClearDeleted;
var
  i: integer;
  Inst: TCustomItem;
begin
  if itemsDeleted.Count > 0 then
  try
    for i := 0 to itemsDeleted.Count - 1 do
    begin
      Inst := TCustomItem(GetWindowLong(THandle(itemsDeleted.Items[i]), GWL_USERDATA));
      FreeAndNil(Inst);
    end;
    itemsDeleted.Clear;
  except
    on e: Exception do err('ItemManager.ClearDeleted', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.UnDelete;
var
  h: THandle;
  i: integer;
  Inst: TCustomItem;
begin
  try
    // clear task items //
    if itemsDeleted.Count > 0 then
    begin
      for i := itemsDeleted.Count - 1 downto 0 do
      begin
        h := THandle(itemsDeleted.Items[i]);
        Inst := TCustomItem(GetWindowLong(h, GWL_USERDATA));
        if Inst is TTaskItem then
        begin
          FreeAndNil(Inst);
          itemsDeleted.Delete(i);
        end;
      end;
    end;

    // restore most recent item //
    if itemsDeleted.Count > 0 then
    begin
      DockAdd(THandle(itemsDeleted.Items[itemsDeleted.Count - 1]));
      itemsDeleted.Delete(itemsDeleted.Count - 1);
      ItemsChanged;
    end;
  except
    on e: Exception do err('ItemManager.UnDelete', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemIndex(HWnd: HANDLE): integer;
var
  i: integer;
begin
  try
    result := NOT_AN_ITEM;
    if (HWnd = 0) or (ItemCount <= 0) then exit;

    for i := 0 to ItemCount - 1 do
    begin
      if items[i].h = HWnd then
      begin
        result := i;
        break;
      end;
    end;
  except
    on e: Exception do err('ItemManager.ItemIndex', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemHWnd(index: integer): HANDLE;
begin
  result := 0;
  if (index >= 0) and (index < ItemCount) then result := items[index].h;
end;
//------------------------------------------------------------------------------
function _ItemManager.ZOrder(InsertAfter: uint): uint;
var
  i: integer;
begin
  result := 0;
  if Enabled then
  try
    if ItemCount > 0 then result := ItemHWnd(0);
    i := 0;
    while i < ItemCount do
    begin
      SetWindowPos(ItemHWnd(i), InsertAfter, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION);
      inc(i);
    end;
  except
    on e: Exception do err('ItemManager.ZOrder', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.Timer;
var
  zoom_minstep: integer;
  zoom_stepcount: integer;
  xstep: integer;
  doUpdate: boolean;
  item: integer;
  Inst: TCustomItem;
begin
  if not Enabled then exit;
  doUpdate := false;
  zoom_minstep := 2 + ZoomSpeed;
  zoom_stepcount := 9 - ZoomSpeed;

  // smooth zoom in/out //
  if ZoomItems or (ZoomItemSizeDiff > 0) then
  try
      if Zooming and (ZoomItemSizeDiff < BigItemSize - ItemSize) then
      begin
        doUpdate := true;
        xstep := max(abs(BigItemSize - ItemSize - ZoomItemSizeDiff) div zoom_stepcount, zoom_minstep);
        if abs(BigItemSize - ItemSize - ZoomItemSizeDiff) <= zoom_minstep
        then ZoomItemSizeDiff := BigItemSize - ItemSize
        else if ZoomItemSizeDiff < BigItemSize - ItemSize then inc(ZoomItemSizeDiff, round(xstep));
      end;

      if not Zooming and (ZoomItemSizeDiff > 0) then
      begin
        doUpdate := true;
        xstep := max(abs(ZoomItemSizeDiff) div zoom_stepcount, zoom_minstep);
        if abs(ZoomItemSizeDiff) <= zoom_minstep then ZoomItemSizeDiff := 0
        else if ZoomItemSizeDiff > 0 then dec(ZoomItemSizeDiff, round(xstep));
      end;
  except
    on e: Exception do err('ItemManager.Timer.SmoothZoom', e);
  end;

  try
    item := 0;
    while item < ItemCount do
    begin
      Inst := TCustomItem(GetWindowLong(items[item].h, GWL_USERDATA));
      if Inst is TCustomItem then Inst.Timer;
      inc(item);
    end;
  except
    on e: Exception do err('ItemManager.Timer.ItemTimer', e);
  end;

  if doUpdate then ItemsChanged;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetTheme;
begin
  MonitorRect := frmterry.GetMonitorBoundsRect;
  AllItemCmd(tcThemeChanged, 0);
  ItemsChanged(true);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.ItemsChanged(FullUpdate: boolean = false);
begin
  if Enabled then
  try
    SetItems1;
    RecalcDock;
    SetItems2(FullUpdate);
    DoBaseDraw(ifthen(FullUpdate, 1, 0));
  except
    on e: Exception do err('ItemManager.ItemsChanged', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetVisible(value: boolean);
begin
  if FVisible <> value then
  begin
    UnZoom(true);
    AllItemCmd(icHover, 0);
    AllItemCmd(icSelect, 0);
    HoverItemHWnd := 0;
    SelectedItemHWnd := 0;
  end;
  FVisible := value;
  ItemsChanged;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetItems1;
function getHalfBubble: extended;
var
  i: extended;
begin
  result := ZoomItemSizeDiff / 2;
  i := 0.5;
  while i < ZoomWidth / 2 do
  begin
    result := result + (ZoomItemSizeDiff - 1) * (cos(PI * i * 2 / ZoomWidth) + 1) / 2;
    i := i + 1;
  end;
end;

var
  i, itemPos: integer;
  sizeInc: extended;
  offset: extended;
begin
  if not Enabled or not assigned(theme) then exit;

  try
    FItemArea := theme.CorrectMargins(theme.ItemsArea);
    if BaseSiteVertical then
    begin
      y := (MonitorRect.Bottom - MonitorRect.Top - IASize) * sets.container.CenterOffsetPercent div 100;
    end else begin
      x := (MonitorRect.Right - MonitorRect.Left - IASize) * sets.container.CenterOffsetPercent div 100;
    end;

    // zoomed bubble additional size //
    offset := getHalfBubble;

    // calc items' pos and size //
    i := 0;
    while i < ItemCount do
    begin
      // icon size //
      sizeInc := 0;
      if ZoomItemSizeDiff > 0 then
      begin
        if (i < trunc(ZoomInOutItem) - ZoomWidth / 2) or (i > trunc(ZoomInOutItem) + ZoomWidth / 2) then sizeInc := 0
        else if i = trunc(ZoomInOutItem) then sizeInc := ZoomItemSizeDiff
        else if i < trunc(ZoomInOutItem) then sizeInc := (ZoomItemSizeDiff - 1) * (cos(PI * 2 * (i - ZoomInOutItem + 1) / ZoomWidth) + 1) / 2
        else if i > trunc(ZoomInOutItem) then sizeInc := (ZoomItemSizeDiff - 1) * (cos(PI * 2 * (i - ZoomInOutItem) / ZoomWidth) + 1) / 2;
      end;
      items[i].se := ItemSize + sizeInc;
      items[i].s := round(items[i].se);

      // icon position when not zooming //
      itemPos := i * (ItemSize + ItemSpacing);
      if BaseSite = bsBottom then
      begin
        items[i].y := FItemArea.Top + ItemSize - items[i].s;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = bsTop then
      begin
        items[i].y := FItemArea.Top;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = bsLeft then
      begin
        items[i].x := FItemArea.Left;
        items[i].y := y + itemPos;
      end
      else
      if BaseSite = bsRight then
      begin
        items[i].x := FItemArea.Left + ItemSize - items[i].s;
        items[i].y := y + itemPos;
      end;

      // icon position when zooming (icons out of bubble and center bubble icon) //
      if ZoomItemSizeDiff > 0 then
      begin
        if BaseSiteVertical then
        begin
          if i < trunc(ZoomInOutItem) then items[i].ye := items[i].y - offset
          else if i > trunc(ZoomInOutItem) then items[i].ye := items[i].y + offset
          else if i = trunc(ZoomInOutItem) then items[i].ye := items[i].y - ZoomItemSizeDiff * frac(ZoomInOutItem);
          items[i].y := round(items[i].ye);
        end
        else
        begin
          if i < trunc(ZoomInOutItem) then items[i].xe := items[i].x - offset
          else if i > trunc(ZoomInOutItem) then items[i].xe := items[i].x + offset
          else if i = trunc(ZoomInOutItem) then items[i].xe := items[i].x - ZoomItemSizeDiff * frac(ZoomInOutItem);
          items[i].x := round(items[i].xe);
        end;
      end;

      inc(i);
    end;

    // icon position when zooming (within bubble excluding center bubble icon) //
    if ZoomItemSizeDiff > 0 then
    begin
      i := trunc(ZoomInOutItem) - 1;
      while (i > trunc(ZoomInOutItem) - ZoomWidth / 2) and (i >= 0)  do
      begin
        if BaseSiteVertical then
        begin
          items[i].ye := items[i + 1].ye - items[i].se - ItemSpacing;
          items[i].y := round(items[i].ye);
        end
        else
        begin
          items[i].xe := items[i + 1].xe - items[i].se - ItemSpacing;
          items[i].x := round(items[i].xe);
        end;
        dec(i);
      end;
      i := trunc(ZoomInOutItem) + 1;
      while (i <= trunc(ZoomInOutItem) + ZoomWidth / 2) and (i < ItemCount)  do
      begin
        if BaseSiteVertical then
        begin
          items[i].ye := items[i - 1].ye + items[i - 1].se + ItemSpacing;
          if i = trunc(ZoomInOutItem) + ZoomWidth / 2 then items[i].y := items[i].y + ItemSize - items[i].s
          else items[i].y := round(items[i].ye);
        end
        else
        begin
          items[i].xe := items[i - 1].xe + items[i - 1].se + ItemSpacing;
          if i = trunc(ZoomInOutItem) + ZoomWidth / 2 then items[i].x := items[i].x + ItemSize - items[i].s
          else items[i].x := round(items[i].xe);
        end;
        inc(i);
      end;
    end;

  except
    on e: Exception do err('ItemManager.SetItems1', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.RecalcDock;
var
  zi_int: integer;
  zi_frac: extended;
begin
  if Enabled and assigned(theme) then
  try
    // width and height //
    width := 0;
    height := 0;
    widthZoomed := 0;
    heightZoomed := 0;
    widthOverhead := 0;
    heightOverhead := 0;
    // vertical //
    if BaseSiteVertical then
    begin
      width := ItemSize + FItemArea.Left + FItemArea.Right;
      widthZoomed := max(width, ifthen(BaseSite = bsLeft, FItemArea.Left, FItemArea.Right) + ItemSize + ZoomItemSizeDiff);
      widthOverhead := 0;
      if Zooming and DraggingFile then widthOverhead := BigItemSize - ItemSize;
    // horizontal //
    end else begin
      height := ItemSize + FItemArea.Top + FItemArea.Bottom;
      heightZoomed := max(height, ifthen(BaseSite = bsTop, FItemArea.Top, FItemArea.Bottom) + ItemSize + ZoomItemSizeDiff);
      heightOverhead := 0;
      if Zooming and DraggingFile then heightOverhead := BigItemSize - ItemSize;
    end;

    // self XY relative to BaseWindowRect //
    if BaseSite = bsLeft then x := 0
    else
    if BaseSite = bsRight then x := widthOverhead
    else
    begin
      if ItemCount = 0 then
      begin
        width := ItemSize + FItemArea.Left + FItemArea.Right;
        x := (MonitorRect.Right - MonitorRect.Left - IASize) * sets.container.CenterOffsetPercent div 100
          - FItemArea.Left + (IASize - width + FItemArea.Left + FItemArea.Right - ItemSpacing) div 2;
      end
      else
      begin
        x := items[0].x - ItemSpacing div 2 - FItemArea.Left;
        width := items[ItemCount - 1].x + items[ItemCount - 1].s - x + ItemSpacing div 2 + FItemArea.Right;
      end;
    end;

    if BaseSite = bsTop then y := 0
    else
    if BaseSite = bsBottom then y := heightOverhead
    else
    begin
      if ItemCount = 0 then
      begin
        height := ItemSize + FItemArea.Top + FItemArea.Bottom;
        y := (MonitorRect.Bottom - MonitorRect.Top - IASize) * sets.container.CenterOffsetPercent div 100
          - FItemArea.Top + (IASize - height + FItemArea.Top + FItemArea.Bottom - ItemSpacing) div 2;
      end
      else
      begin
        y := items[0].y - ItemSpacing div 2 - FItemArea.Top;
        height := items[ItemCount - 1].y + items[ItemCount - 1].s - y + ItemSpacing div 2 + FItemArea.Bottom;
      end;
    end;

    // background image rect //
    BaseImageRect.x := x;
    BaseImageRect.y := y;
    BaseImageRect.Width := Width;
    BaseImageRect.Height := Height;

    // main form rect //
    BaseWindowRect.x := MonitorRect.Left;
    BaseWindowRect.y := MonitorRect.Top;
    if BaseSite = bsLeft then BaseWindowRect.x := MonitorRect.Left - sets.wndoffset + sets.container.EdgeOffset
    else if BaseSite = bsTop then BaseWindowRect.y := MonitorRect.Top - sets.wndoffset + sets.container.EdgeOffset
    else if BaseSite = bsRight then BaseWindowRect.x := MonitorRect.Right - Width - widthOverhead + sets.wndoffset - sets.container.EdgeOffset
    else if BaseSite = bsBottom then BaseWindowRect.y := MonitorRect.Bottom - Height - heightOverhead + sets.wndoffset - sets.container.EdgeOffset;
    if BaseSiteVertical then
    begin
      BaseWindowRect.Width := Width + widthOverhead;
      BaseWindowRect.Height := MonitorRect.Bottom - MonitorRect.Top;
    end else begin
      BaseWindowRect.Width := MonitorRect.Right - MonitorRect.Left;
      BaseWindowRect.Height := Height + heightOverhead;
    end;

    // finally //
    sets.Width := BaseWindowRect.Width;
    sets.Height := BaseWindowRect.Height;
  except
    on e: Exception do raise Exception.Create('ItemManager.RecalcDock'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetItems2(force_draw: boolean);
var
  i: integer;
  wpi, show_items: uint;
begin
  if Enabled then
  try
    i := ItemCount;
    wpi := BeginDeferWindowPos(i);
    show_items := swp_hidewindow;
    if FVisible then show_items := swp_showwindow;

    // draw items //
    i := 0;
    while i < ItemCount do
    begin
      if items[i].h <> 0 then
        TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA)).Draw(
          BaseWindowRect.X + items[i].x, BaseWindowRect.Y + items[i].y,
          items[i].s, force_draw, wpi, show_items);
      inc(i);
    end;

    EndDeferWindowPos(wpi);
  except
    on e: Exception do err('ItemManager.SetItems2', e);
  end;
end;
//------------------------------------------------------------------------------
// insert the list of the items at DropPlace position
// if DropPlace not exists, then insert at the end of the items array
procedure _ItemManager.InsertItems(list: TStrings);
var
  i, dplace: integer;
begin
  if not Enabled then exit;

  dplace := DropPlace;
  for i := 0 to list.Count - 1 do
  begin
    InsertItem(list.strings[i]);
    if dplace <> NOT_AN_ITEM then
      if i < list.Count - 1 then
      begin
        inc(dplace);
        SetDropPlace(dplace);
        SetDropPlaceEx(dplace);
      end;
  end;
end;
//------------------------------------------------------------------------------
// insert item at current DropPlace
// or at the end if DropPlace not exists (e.g. DropPlace = NOT_AN_ITEM)
procedure _ItemManager.InsertItem(AData: string);
begin
  if Enabled then AddItem(AData, true, true);
end;
//------------------------------------------------------------------------------
// create an item and put it onto dock
function _ItemManager.AddItem(data: string; Update: boolean = false; Save: boolean = true): THandle;
begin
  result := 0;
  if ItemCount > MAX_ITEM_COUNT then exit;
  AllItemCmd(icHover, 0); // hide hint //
  AllItemCmd(icSelect, 0);

  result := CreateItem(data);
  if result <> THandle(0) then
  begin
    DockAdd(result);
    PluginCallCreate(result);
  end;
  if Update then ItemsChanged(true);
  // save settings //
  if Save then AllItemsSave;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.PluginCallCreate(HWnd: HANDLE);
var
  Inst: TCustomItem;
begin
  try
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TPluginItem then TPluginItem(Inst).CallCreate;
  except
    on e: Exception do raise Exception.Create('ItemManager.PluginCallCreate'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.CreateItem(data: string): THandle;
var
  class_name: string;
  Inst: TCustomItem;
  icp: _ItemCreateParams;
begin
  result := 0;
  Inst := nil;
  try
    class_name := AnsiLowerCase(FetchValue(data, 'class="', '"'));
    if (class_name = 'plugin') and (data = '') then exit;

    icp.ItemSize := sets.container.ItemSize;
    icp.BigItemSize := sets.container.BigItemSize;
    icp.LaunchInterval := sets.container.LaunchInterval;
    icp.ActivateRunning := sets.container.ActivateRunning;
    icp.UseShellContextMenus := sets.container.UseShellContextMenus;
    icp.Site := integer(BaseSite);
    icp.Reflection := sets.container.Reflection;
    icp.ReflectionSize := theme.ReflectionSize;
    icp.ShowHint := sets.container.ShowHint;
    icp.Animation := sets.container.ItemAnimation;
    icp.LockDragging := sets.container.LockDragging;
    icp.StackOpenAnimation := sets.container.StackOpenAnimation;

    if class_name = 'shortcut' then Inst := TShortcutItem.Create(data, ParentHWnd, icp)
    else
    if class_name = 'separator' then Inst := TSeparatorItem.Create(data, ParentHWnd, icp)
    else
    if class_name = 'plugin' then Inst := TPluginItem.Create(data, ParentHWnd, icp)
    else
    if class_name = 'stack' then Inst := TStackItem.Create(data, ParentHWnd, icp)
    else
    if class_name = 'task' then Inst := TTaskItem.Create(data, ParentHWnd, icp);
  except
    on e: Exception do raise Exception.Create('ItemManager.CreateItem.' + class_name + #10#13 + e.message);
  end;

  try
    if assigned(Inst) then
      if Inst.Freed then FreeAndNil(Inst) else result := Inst.HWnd;
  except
    on e: Exception do raise Exception.Create('ItemManager.CreateItem.Fin'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// do not call directly !!!
// item can call this using DockH.DockDeleteItem from TCustomItem.Delete
procedure _ItemManager.DeleteItem(HWnd: THandle);
var
  i: integer;
begin
  if Enabled then
  try
    i := ItemIndex(HWnd);
    enabled := false;
    // add to "deleted" list //
    itemsDeleted.Add(Pointer(HWnd));
    // erase it from "items" list //
    if i <> NOT_AN_ITEM then
    begin
      while i < ItemCount - 1 do
      begin
        CopyItemDescriptor(@items[i + 1], @items[i]);
        inc(i);
      end;
      dec(ItemCount);
    end;
    // update dock //
    enabled := true;
    ItemsChanged;
    // save settings !!!
    DoSaveSets;
  except
    on e: Exception do raise Exception.Create('ItemManager.DeleteItem'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.CopyItemDescriptor(pFrom, pTo: PItem);
begin
  pTo^.h := pFrom^.h;
  pTo^.x := pFrom^.x;
  pTo^.y := pFrom^.y;
  pTo^.s := pFrom^.s;
end;
//------------------------------------------------------------------------------
//
//
//     Misc
//
//
//------------------------------------------------------------------------------
// calculate and set DropPlace and DropPlaceEx
procedure _ItemManager.CalcDropPlace(pt: windows.TPoint);
var
  tmp: extended;
  cx, cy, prevDropPlace, prevDropPlaceEx: integer;
begin
  prevDropPlace := DropPlace;
  prevDropPlaceEx := DropPlaceEx;

  if Enabled then
  try
    cx := pt.x;
    cy := pt.y;
    if BaseSiteVertical then dec(cy, ItemSize div 2) else dec(cx, ItemSize div 2);

    // DropPlace //
    tmp := ItemFromPoint(cx, cy, DropDistance);
    if tmp = NOT_AN_ITEM then
    begin
      DropPlace := NOT_AN_ITEM;
    end else begin
      if (abs(DropPlace - tmp) > 1.2) or (tmp = -1) then DropPlace := round(tmp);
    end;

    if DropPlace <> NOT_AN_ITEM then
    begin
      if DropPlace < 0 then DropPlace := 0;
      if ItemCount < 1 then DropPlace := 0
      else begin
        if DraggingFile then
        begin
          if prevDropPlace = NOT_AN_ITEM then
          begin
            if DropPlace > ItemCount - TaskItemCount then DropPlace := NOT_AN_ITEM;
          end else begin
            if DropPlace >= ItemCount - TaskItemCount then DropPlace := NOT_AN_ITEM;
          end;
        end else
        if prevDropPlace = NOT_AN_ITEM then
        begin
          if DropPlace > ItemCount - TaskItemCount then DropPlace := ItemCount - TaskItemCount;
        end else begin
          if DropPlace >= ItemCount - TaskItemCount then DropPlace := ItemCount - TaskItemCount - 1;
        end;
      end;
    end;
    if prevDropPlace <> DropPlace then SetDropPlace(DropPlace);

    // DropPlaceEx //
    tmp := ItemRectFromPoint(pt.x, pt.y);
    if tmp = NOT_AN_ITEM then tmp := DropPlace;
    if tmp = NOT_AN_ITEM then DropPlaceEx := NOT_AN_ITEM else DropPlaceEx := round(tmp);
    if DropPlaceEx <> NOT_AN_ITEM then
    begin
      if DropPlaceEx < 0 then DropPlaceEx := 0;
      if DropPlaceEx > ItemCount - TaskItemCount - 1 then DropPlaceEx := NOT_AN_ITEM;
    end;
    if prevDropPlaceEx <> DropPlaceEx then SetDropPlaceEx(DropPlaceEx);
  except
    on e: Exception do err('ItemManager.CalcDropPlace', e);
  end;
end;
//------------------------------------------------------------------------------
// physically create/move/delete empty item at DropPlace
procedure _ItemManager.SetDropPlace(index: integer);
var
  i, currentDropPlace: integer;
begin
  try
    AllItemCmd(icDropIndicator, 0);

    DropPlace := index;
    // seek for current DropPlace in the items array //
    // DropPlace item has its Handle = 0 //
    currentDropPlace := NOT_AN_ITEM;
    if ItemCount > 0 then
      for i := 0 to ItemCount - 1 do
      begin
        if items[i].h = 0 then
        begin
          currentDropPlace := i;
          break;
        end;
      end;

    if (currentDropPlace = NOT_AN_ITEM) and (DropPlace = NOT_AN_ITEM) then exit;
    if currentDropPlace = DropPlace then exit;

    // add empty item //
    if (currentDropPlace = NOT_AN_ITEM) and (DropPlace <> NOT_AN_ITEM) then
    begin
      if DropPlace > ItemCount then DropPlace := ItemCount;
      items[ItemCount].h := 0;
      items[ItemCount].s := ItemSize;
      if DropPlace < ItemCount then
      begin
        for i := ItemCount downto DropPlace + 1 do items[i].h := items[i - 1].h;
      end;
      inc(ItemCount);
      items[DropPlace].h := 0;
      ItemsChanged(true);
      exit;
    end;

    // move empty item //
    if (currentDropPlace <> NOT_AN_ITEM) and (DropPlace <> NOT_AN_ITEM) then
    begin
      if DropPlace < currentDropPlace then
      begin
        for i := currentDropPlace downto DropPlace + 1 do items[i].h := items[i - 1].h;
      end;
      if DropPlace > currentDropPlace then
      begin
        for i := currentDropPlace to DropPlace - 1 do items[i].h := items[i + 1].h;
      end;
      items[DropPlace].h := 0;
      ItemsChanged;
      exit
    end;

    // delete empty item //
    if (currentDropPlace <> NOT_AN_ITEM) and (DropPlace = NOT_AN_ITEM) then
    begin
      if currentDropPlace < ItemCount - 1 then
      begin
        for i := currentDropPlace to ItemCount - 2 do items[i].h := items[i + 1].h;
      end;
      dec(ItemCount);
      ItemsChanged(true);
    end;
  except
    on e: Exception do err('ItemManager.SetDropPlace', e);
  end;
end;
//------------------------------------------------------------------------------
// set appropriate indicator for an item at DropPlaceEx
procedure _ItemManager.SetDropPlaceEx(index: integer);
var
  DragInst, Inst: TCustomItem;
  atype: integer;
begin
  try
    DropPlaceEx := index;
    atype := 0;

    // correct disallowed drop cases //
    if DropPlaceEx <> NOT_AN_ITEM then
    begin
      Inst := TCustomItem(GetWindowLong(items[DropPlaceEx].h, GWL_USERDATA));
      if Dragging and not DraggingFile then
      begin
        DragInst := TCustomItem(GetWindowLong(DragHWnd, GWL_USERDATA));
        if ((Inst is TStackItem) and (DragInst is TStackItem)) or
          ((Inst is TStackItem) and (DragInst is TShortcutItem)) or
          ((Inst is TShortcutItem) and (DragInst is TShortcutItem)) then atype := 1; // add
      end else begin
        if Inst is TStackItem then atype := 1; // add
        if Inst is TShortcutItem then atype := 2; // run
      end;
      if atype = 0 then DropPlaceEx := DropPlace;
    end;

    AllItemCmd(icDropIndicator, 0);
    if atype > 0 then ItemCmd(items[DropPlaceEx].h, icDropIndicator, atype);
  except
    on e: Exception do err('ItemManager.SetDropPlaceEx', e);
  end;
end;
//------------------------------------------------------------------------------
// items area width or height //
function _ItemManager.IASize: integer;
begin
  result := ItemCount * (ItemSize + ItemSpacing);
end;
//------------------------------------------------------------------------------
// calculate item index based on mouse position
// do not care of the current items positions
function _ItemManager.ItemFromPoint(Ax, Ay, distance: integer): extended;
var
  BasePoint: integer; // left or top of the first item
  rItemArea: windows.TRect;
begin
  result := NOT_AN_ITEM;
  if not enabled then exit;

  try
    if ItemCount = 0 then
    begin
      rItemArea := DockGetRect;
      if PtInRect(rItemArea, classes.Point(Ax, Ay)) then result := 0;
      exit;
    end;

    // calc position relative to the beginning of the first item //

    if BaseSiteVertical then
    begin
      BasePoint := MonitorRect.Top + (MonitorRect.Bottom - MonitorRect.Top - IASize) * sets.container.CenterOffsetPercent div 100;
      result := (Ay - BasePoint) / (ItemSize + ItemSpacing);
    end else begin
      BasePoint := MonitorRect.Left + (MonitorRect.Right - MonitorRect.Left - IASize) * sets.container.CenterOffsetPercent div 100;
      result := (Ax - BasePoint) / (ItemSize + ItemSpacing);
    end;
    if result < -1 then result := NOT_AN_ITEM;
    if result >= ItemCount + 1 then result := NOT_AN_ITEM;

    // check boundaries //

    rItemArea := FItemArea;
    if BaseSite = bsLeft then rItemArea.Right := 0
    else if BaseSite = bsTop then rItemArea.Bottom := 0
    else if BaseSite = bsRight then rItemArea.Left := 0
    else if BaseSite = bsBottom then rItemArea.Top := 0;

    if BaseSite = bsBottom then
    begin
      if (Ay < BaseWindowRect.Y + BaseWindowRect.Height - rItemArea.Bottom - ItemSize - rItemArea.Top - ZoomItemSizeDiff - distance) or
        (Ay > BaseWindowRect.Y + BaseWindowRect.Height + distance) or
        (Ax < BasePoint - BigItemSize) or
        (Ax > BasePoint + width)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = bsLeft then
    begin
      if (Ax < BaseWindowRect.X - distance) or
        (Ax > BaseWindowRect.X + x + rItemArea.Left + ItemSize + rItemArea.Right + ZoomItemSizeDiff + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = bsRight then
    begin
      if (Ax < BaseWindowRect.X + BaseWindowRect.Width - rItemArea.Right - ItemSize - rItemArea.Left - ZoomItemSizeDiff - distance) or
        (Ax > BaseWindowRect.X + BaseWindowRect.Width + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    begin
      if (Ay < BaseWindowRect.Y - distance) or
        (Ay > BaseWindowRect.Y + y + rItemArea.Top + ItemSize + rItemArea.Bottom + ZoomItemSizeDiff + distance) or
        (Ax < BasePoint - BigItemSize) or
        (Ax > BasePoint + width)
        then result := NOT_AN_ITEM;
    end;

  except
    on e: Exception do err('ItemManager.ItemFromPoint', e);
  end;
end;
//------------------------------------------------------------------------------
// calculate item index based on mouse position and items positions
function _ItemManager.ItemRectFromPoint(Ax, Ay: integer): integer;
var
  i: integer;
  Inst: TCustomItem;
begin
  result := NOT_AN_ITEM;
  if not enabled then exit;

  try
    if ItemCount = 0 then
    begin
      if PtInRect(DockGetRect, classes.Point(Ax, Ay)) then result := 0;
      exit;
    end;

    // calc position relative to the beginning of the first item //

    for i := 0 to ItemCount - 1 do
    begin
      Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
      if Inst is TCustomItem then
        if PtInRect(Inst.ScreenRect, classes.Point(Ax, Ay)) then
        begin
          result := i;
          break;
        end;
    end;
  except
    on e: Exception do err('ItemManager.ItemFromPoint', e);
  end;
end;
//------------------------------------------------------------------------------
// enter/exit zooming mode, maintain zooming
procedure _ItemManager.Zoom(x, y: integer);
var
  item, saved: extended;
begin
  try
    if not enabled then exit;

    item := NOT_AN_ITEM;
    if Zooming or CheckMouseOn or Dragging or DraggingFile then
    begin
      item := ItemFromPoint(x, y, ifthen(Dragging or DraggingFile, DropDistance, 0));
      if item <> NOT_AN_ITEM then
      begin
        if item < 0 then item := NOT_AN_ITEM;
        if item >= ItemCount then item := NOT_AN_ITEM;
      end;
    end;

    if not Zooming and (item <> NOT_AN_ITEM) then
    begin
      ZoomInOutItem := item;
      Zooming := true;
    end;

    if Zooming then
    begin
      if item = NOT_AN_ITEM then Unzoom
      else begin
          if ZoomItemSizeDiff = BigItemSize - ItemSize then
          begin
              if ZoomInOutItem <> item then
              begin
                ZoomInOutItem := item;
                ItemsChanged;
              end;
          end
          else ZoomInOutItem := item;
      end;
    end;
  except
    on e: Exception do err('ItemManager.Zoom', e);
  end;
end;
//------------------------------------------------------------------------------
// exit zooming mode
procedure _ItemManager.UnZoom(do_now: boolean = false);
begin
  if enabled and (Zooming or do_now) then
  begin
    Zooming := false;
    if (ItemCount <= 0) or do_now then
    begin
      ZoomItemSizeDiff := 0;
      ItemsChanged;
    end;
  end;
end;
//------------------------------------------------------------------------------
// check mouse is over the dock
function _ItemManager.CheckMouseOn: boolean;
var
  pt: windows.TPoint;
  wnd: uint;
  item: integer;
begin
  result := false;
  if enabled then
  try
    windows.GetCursorPos(pt);
    wnd := WindowFromPoint(pt);
    if Dragging or DraggingFile or Zooming then
      item := trunc(ItemFromPoint(pt.x, pt.y, ifthen(Dragging or DraggingFile, DropDistance, 0)))
    else
      item := ItemIndex(wnd);
    result := (item <> NOT_AN_ITEM) or (wnd = ParentHWnd);
  except
    on e: Exception do err('ItemManager.CheckMouseOn', e);
  end;
end;
//------------------------------------------------------------------------------
// entry point for MouseMove events
procedure _ItemManager.WHMouseMove(pt: windows.Tpoint; allow_zoom: boolean = true);
var
  wnd: cardinal;
begin
  try
    if not enabled or not FVisible then exit;
    if ItemCount < 0 then exit;

    // hint //
    wnd := WindowFromPoint(pt);
    if ItemIndex(wnd) = NOT_AN_ITEM then wnd := 0;
    if (wnd <> HoverItemHWnd) and not Dragging {avoid hint flickering} then
    begin
      if HoverItemHWnd <> 0 then
      begin
        ItemCmd(HoverItemHWnd, icSelect, 0);
        ItemCmd(HoverItemHWnd, icHover, 0);
      end;
      if wnd <> 0 then ItemCmd(wnd, icHover, 1);
    end;
    HoverItemHWnd := wnd;

    // zoom //
    if allow_zoom and ZoomItems then Zoom(pt.x, pt.y);

    // drop place //
    if Dragging or DraggingFile then
    begin
      CalcDropPlace(pt);
      if allow_zoom and ZoomItems then Zoom(pt.x, pt.y);
    end;
  except
    on e: Exception do raise Exception.Create('ItemManager.WHMouseMove'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DragEnter;
begin
  Dragging := false;
  DraggingFile := true;
  DragOver;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DragLeave;
begin
  DraggingFile := false;
  SetDropPlaceEx(NOT_AN_ITEM);
  SetDropPlace(NOT_AN_ITEM);
  ItemsChanged;
  ItemCmd(SelectedItemHWnd, icDragLeave, 0);
  ItemCmd(SelectedItemHWnd, icSelect, 0);
  SelectedItemHWnd := 0;
  AllItemCmd(icHover, 0);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DragOver;
var
  pt: windows.TPoint;
  wnd: THandle;
begin
  if Enabled then
  try
    GetCursorPos(pt);
    if not LockMouseEffect then WHMouseMove(pt);
    wnd := WindowFromPoint(pt);
    wnd := IsItem(wnd);
    if wnd <> SelectedItemHWnd then
    begin
      ItemCmd(SelectedItemHWnd, icSelect, 0);
      ItemCmd(SelectedItemHWnd, icDragLeave, 0);
      SelectedItemHWnd := wnd;
      ItemCmd(SelectedItemHWnd, icSelect, 1);
      ItemCmd(SelectedItemHWnd, icDragEnter, 0);
    end;
    ItemCmd(wnd, icDragOver, 0);
  except
    on e: Exception do err('ItemManager.DragOver', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.WMDeactivateApp;
var
  i: integer;
begin
  if not CheckMouseOn then Unzoom;
  AllItemCmd(icHover, 0);

  if Enabled then
  try
    i := 0;
    while i < ItemCount do
    begin
      if items[i].h <> 0 then sendmessage(items[i].h, WM_ACTIVATEAPP, 0, 0);
      inc(i);
    end;
  except
    on e: Exception do err('ItemManager.WMDeactivate', e);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//   Window Item Procs
//
//
//
//------------------------------------------------------------------------------
// detach the item
procedure _ItemManager.Undock(HWnd: HANDLE);
var
  index: integer;
  pt: windows.TPoint;
begin
  if enabled and not Dragging then
  try
    AllItemCmd(icHover, 0);
    Dragging := true;
    DragHWnd := HWnd;
    index := ItemIndex(HWnd);
    try if index <> NOT_AN_ITEM then items[index].h := 0;
    except end;
    ItemsChanged;
    windows.GetCursorPos(pt);
    CalcDropPlace(pt);
    SetWindowPos(HWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOREPOSITION + SWP_NOSENDCHANGING);
    ReleaseCapture;
    DefWindowProc(HWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  except
    on e: Exception do err('ItemManager.UndockWindowItem', e);
  end;
end;
//------------------------------------------------------------------------------
// add new item to dock
// if there is a DropPlace, then put item to DropPlace
// if DropPlace not exists, then put item at the end of the items array
procedure _ItemManager.DockAdd(HWnd: THandle);
begin
  if (DropPlace >= 0) and (DropPlace < ItemCount) then
  begin
    // if DropPlace exists, then it is not TaskItem //
    if DropPlace > ItemCount - TaskItemCount then DropPlace := ItemCount - TaskItemCount;
    items[DropPlace].h := HWnd;
  end else begin
    // else check where to dock //
    if IsTask(HWnd) then SetDropPlace(ItemCount) else SetDropPlace(ItemCount - TaskItemCount);
    items[DropPlace].h := HWnd;
    items[DropPlace].s := ItemSize;
  end;
  ItemCmd(HWnd, icFree, 0); // enable item
  ItemCmd(HWnd, icFloat, 0);
  SetWindowPos(HWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE + SWP_NOSENDCHANGING);
  SetDropPlaceEx(NOT_AN_ITEM);
  SetDropPlace(NOT_AN_ITEM);
  ItemsChanged;
end;
//------------------------------------------------------------------------------
// put the item to dock
// if necessary create a new stack or put into existing one
procedure _ItemManager.Dock(HWnd: HANDLE);
var
  i: integer;
  DragInst, Inst, NewInst: TCustomItem;
  NewItemHWnd: THandle;
  pt: windows.TPoint;
begin
  //if not enabled or (DragHWnd <> HWnd) then exit;
  if Enabled then
  try
    SetWindowPos(HWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE + SWP_NOSENDCHANGING);
    GetCursorPos(pt);
    CalcDropPlace(pt);

    DragInst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));

    // delete //
    if (DropPlace < 0) or (DropPlace >= ItemCount) then
    begin
      DragInst.Delete;
    end else
    // put to a free slot //
    if DropPlace = DropPlaceEx then
    begin
      items[DropPlace].h := HWnd;
      ItemCmd(HWnd, icFloat, 0);
    end else
    // combine with existing item //
    begin
      Inst := TCustomItem(GetWindowLong(items[DropPlaceEx].h, GWL_USERDATA));
      // shortcut to stack //
      if (DragInst is TShortcutItem) and (Inst is TStackItem) then
      begin
        TStackItem(Inst).AddSubitem(TShortcutItem(DragInst).ToString);
        DragInst.Delete;
      end else
      // stack to stack //
      if (DragInst is TStackItem) and (Inst is TStackItem) then
      begin
        i := 0;
        while i < TStackItem(DragInst).ItemCount do
        begin
          TStackItem(Inst).AddSubitem(TStackItem(DragInst).SubitemToString(i));
          inc(i);
        end;
        DragInst.Delete;
      end else
      // shortcut to shortcut //
      if (DragInst is TShortcutItem) and (Inst is TShortcutItem) then
      begin
        if (DropPlace >= 0) and (DropPlace < ItemCount) then
        begin
          NewItemHWnd := CreateItem(TStackItem.Make(0, '', ''));
          if NewItemHWnd <> THandle(0) then
          begin
            items[DropPlace].h := NewItemHWnd;
            NewInst := TCustomItem(GetWindowLong(NewItemHWnd, GWL_USERDATA));
            TStackItem(NewInst).AddSubitem(TShortcutItem(Inst).ToString);
            TStackItem(NewInst).AddSubitem(TShortcutItem(DragInst).ToString);
            DragInst.Delete;
            Inst.Delete;
            ItemsChanged(true);
          end;
        end;
      end else begin
          if (DropPlace >= 0) and (DropPlace < ItemCount) then
          begin
            items[DropPlace].h := HWnd;
            ItemCmd(HWnd, icFloat, 0);
          end else begin
            DragInst.Delete;
          end;
      end;
    end;

    Dragging := false;
    DragHWnd := 0;
    SetDropPlaceEx(NOT_AN_ITEM);
    SetDropPlace(NOT_AN_ITEM);
    ItemsChanged;
    AllItemsSave;
  except
    on e: Exception do err('ItemManager.DockWindowItem', e);
  end;
end;
//------------------------------------------------------------------------------
// search all items and their subitems for a given HWnd
// if item is found - result is HWnd
// if subitem is found - result is its parent item HWnd
// if no match found result is 0
function _ItemManager.IsItem(HWnd: HANDLE): HANDLE;
var
  i: integer;
  Inst: TCustomItem;
begin
  result := THandle(0);
  if Enabled then
  try
    i := 0;
    while i < ItemCount do
    begin
      if items[i].h = HWnd then
      begin
        result := HWnd;
        break;
      end;

      Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
      if Inst is TCustomItem then result := Inst.cmd(icIsItem, integer(HWnd));
      if result <> 0 then break;
      inc(i);
    end;
  except
    on e: Exception do err('ItemManager.IsItem', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemDropFile(HWndItem: HANDLE; pt: windows.TPoint; filename: string): boolean;
var
  Inst: TCustomItem;
  HWndChild: HANDLE;
begin
  try
    result := false;
    HWndChild := HWndItem;
    HWndItem := IsItem(HWndItem);
    if HWndItem <> THandle(0) then
    begin
      Inst := TCustomItem(GetWindowLong(HWndItem, GWL_USERDATA));
      if Inst is TCustomItem then result := Inst.DropFile(HWndChild, pt, filename);
    end;
  except
    on e: Exception do err('ItemManager.ItemDrop', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemDropFiles(HWndItem: HANDLE; pt: windows.TPoint; files: TStrings): boolean;
var
  i: integer;
  Inst: TCustomItem;
  HWndChild: HANDLE;
begin
  try
    result := false;
    HWndChild := HWndItem;
    HWndItem := IsItem(HWndItem);
    if HWndItem <> THandle(0) then
    begin
      Inst := TCustomItem(GetWindowLong(HWndItem, GWL_USERDATA));
      if Inst is TCustomItem then
        for i := 0 to files.Count - 1 do
          result := result or Inst.DropFile(HWndChild, pt, files.strings[i]);
    end;
  except
    on e: Exception do err('ItemManager.ItemDrop', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemCmd(HWnd: HANDLE; id: TGParam; param: integer): integer;
var
  Inst: TCustomItem;
begin
  try
    result := 0;
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then result := Inst.cmd(id, param);
  except
    on e: Exception do err('ItemManager.ItemCmd', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.AllItemCmd(id: TGParam; param: integer): integer;
var
  item: integer;
  Inst: TCustomItem;
begin
  result := 0;
  if Enabled then
  try
    item := 0;
    while item < ItemCount do
    begin
      Inst := TCustomItem(GetWindowLong(items[item].h, GWL_USERDATA));
      if Inst is TCustomItem then result := Inst.cmd(id, param);
      inc(item);
    end;
  except
    on e: Exception do err('ItemManager.AllItemCmd', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetPluginFile(HWnd: HANDLE): string;
var
  Inst: TCustomItem;
begin
  try
    result := '';
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TPluginItem then result := TPluginItem(Inst).GetFilename;
  except
    on e: Exception do err('ItemManager.GetPluginFile', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetPluginImage(HWnd: HANDLE; lpImageNew: Pointer; AutoDelete: boolean);
var
  Inst: TCustomItem;
begin
  try
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TPluginItem then TPluginItem(Inst).UpdateImage(lpImageNew, AutoDelete);
  except
    on e: Exception do err('ItemManager.SetPluginImage', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetPluginOverlay(HWnd: HANDLE; lpOverlayNew: Pointer; AutoDelete: boolean);
var
  Inst: TCustomItem;
begin
  try
    if HWnd <> 0 then Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TPluginItem then TPluginItem(Inst).UpdateOverlay(lpOverlayNew, AutoDelete);
  except
    on e: Exception do err('ItemManager.SetPluginOverlay', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.PluginAnimate(HWnd: HANDLE);
var
  Inst: TCustomItem;
begin
  try
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if (Inst is TCustomItem) and (sets.container.ItemAnimation > 0) then
      Inst.Animate(sets.container.ItemAnimation);
  except
    on e: Exception do err('ItemManager.PluginAnimate', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetPluginCaption(HWnd: HANDLE; NewCaption: string);
var
  Inst: TCustomItem;
begin
  try
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then Inst.Caption := NewCaption;
  except
    on e: Exception do err('ItemManager.SetPluginCaption', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetPluginCaption(HWnd: HANDLE): string;
var
  Inst: TCustomItem;
begin
  try
    result := '';
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then result := Inst.Caption;
  except
    on e: Exception do err('ItemManager.GetPluginCaption', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetPluginRect(HWnd: HANDLE): windows.TRect;
var
  Inst: TCustomItem;
begin
  try
    result := classes.Rect(0, 0, 0, 0);
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then result := Inst.ScreenRect;
  except
    on e: Exception do err('ItemManager.GetPluginRect', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.IsPluginUndocked(HWnd: HANDLE): boolean;
var
  Inst: TCustomItem;
begin
  try
    result := false;
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then result := Inst.Floating;
  except
    on e: Exception do err('ItemManager.IsPluginUndocked', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.IsSeparator(HWnd: HANDLE): boolean;
begin
  try
    result := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA)) is TSeparatorItem;
  except
    on e: Exception do err('ItemManager.IsSeparator', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.IsTask(HWnd: HANDLE): boolean;
begin
  try
    result := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA)) is TTaskItem;
  except
    on e: Exception do err('ItemManager.IsSeparator', e);
  end;
end;
//------------------------------------------------------------------------------
//
//
//
//   Task Item Procs
//
//
//
//------------------------------------------------------------------------------
procedure _ItemManager.Taskbar;
var
  i: integer;
  Inst: TCustomItem;
  index: integer;
  HWndTask, HWndItem: THandle;
begin
  // add items //
  i := 0;
  while i < ProcessHelper.GetAppWindowsCount do
  begin
    HWndTask := ProcessHelper.GetAppWindowHandle(i);
    index := GetTaskItemIndex(HWndTask);
    if index = -1 then // there is no item for the window
    begin
      if TaskItemCount = 0 then // if there is no task items yet - add separator
      begin
        AddItem('class="separator";dontsave="1";candrag="0";', true, false);
        inc(TaskItemCount);
      end;
      // add task item at the end of list //
      SetDropPlace(NOT_AN_ITEM);
      HWndItem := AddItem('class="task";', true, false);
      Inst := TCustomItem(GetWindowLong(HWndItem, GWL_USERDATA));
      if Inst is TTaskItem then
      begin
        inc(TaskItemCount);
        TTaskItem(Inst).UpdateTaskItem(HWndTask);
      end;
    end;
    inc(i);
  end;


  // remove items //
  i := ItemCount - 1;
  while i >= 0 do
  begin
    Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
    if Inst is TTaskItem then
      if TTaskItem(Inst).AppHWnd <> THandle(0) then
        if ProcessHelper.GetAppWindowIndex(TTaskItem(Inst).AppHWnd) < 0 then
        begin
          Inst.Delete;
          if TaskItemCount > 0 then dec(TaskItemCount);
        end;
    dec(i);
  end;

  // if not task items left on the dock - delete task items separator //
  if TaskItemCount = 1 then
  begin
    i := ItemCount - 1;
    while (i >= 0) and (TaskItemCount > 0) do
    begin
      Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
      if Inst is TSeparatorItem then
        if Inst.DontSave then
        begin
          Inst.Delete;
          if TaskItemCount > 0 then dec(TaskItemCount);
        end;
      dec(i);
    end;
  end;

end;
//------------------------------------------------------------------------------
function _ItemManager.GetTaskItemIndex(h: THandle): integer;
var
  i: integer;
  Inst: TCustomItem;
begin
  result := -1;
  i := 0;
  while i < ItemCount do
  begin
    Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
    if Inst is TTaskItem then
      if TTaskItem(Inst).AppHWnd = h then
      begin
        result := i;
        break;
      end;
    inc(i);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.ClearTaskbar;
var
  i: integer;
  Inst: TCustomItem;
begin
  if TaskItemCount > 0 then
  begin
      i := ItemCount - 1;
      while i >= 0 do
      begin
        Inst := TCustomItem(GetWindowLong(items[i].h, GWL_USERDATA));
        if Inst is TTaskItem then Inst.Delete;
        if Inst is TSeparatorItem then if Inst.DontSave then Inst.Delete;
        dec(i);
      end;
      TaskItemCount := 0;
  end;
end;
//------------------------------------------------------------------------------
end.
