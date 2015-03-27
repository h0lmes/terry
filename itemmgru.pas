unit itemmgru;

interface
uses Windows, Messages, Classes, SysUtils, Forms, IniFiles, Math,
  declu, DockH, toolu, gfx, GDIPAPI, processhlp,
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
    ZoomTime: integer;
    ZoomStartTime: integer; // timestamp when zoming in or out has begun
    Reflection: boolean;
    ReflectionSize: integer;
    ItemAnimation: integer;
    LaunchInterval: integer;
    ActivateRunning: boolean;
    UseShellContextMenus: boolean;
    ShowHint: boolean;
    StackOpenAnimation: boolean;
    FSeparatorAlpha: integer;
    FOccupyFullMonitor: boolean;
    FFont: _FontData;
    LockMouseEffect: boolean;
    SetsFilename: string;
    FVisible: boolean;
    Enabled: boolean;
    FTaskLivePreviews: boolean;
    FTaskThumbSize: integer;
    FTaskGrouping: boolean;
    // for smooth zooming in and out //
    // 0 <= ZoomItemSizeDiff <= (BigItemSize - ItemSize) //
    ZoomItemSizeDiff: integer;
    // monitor index //
    Monitor: integer;
    BaseSite: TBaseSite;
    BaseSiteVertical: boolean;
    FCenterOffsetPercent: integer;
    FEdgeOffset: integer;
    FWndOffset: integer;
    // Wnd Item Vars //
    FItemsArea: windows.TRect;
    FItemsArea2: windows.TRect;
    FMargin: integer; // dock icons offset from a monitor edge
    FMargin2: integer; // dock icons additional offset from a monitor edge

    procedure err(where: string; e: Exception; Critical: boolean = false);
    procedure notify(message: string);
    procedure DoBaseDraw(flags: integer);
    procedure SetVisible(value: boolean);

    // load/save //
    procedure AllItemsSave;
    procedure ItemSave(HWnd: uint);

    procedure SetItems1;
    procedure RecalcDock;
    procedure SetItems2(force_draw: boolean);

    function IASize: integer;
    function ItemFromPoint(Ax, Ay, distance: integer): extended;
    function ItemRectFromPoint(Ax, Ay: integer): integer;

    // items //
    function ItemIndex(HWnd: HANDLE): integer;
    function ItemHWnd(index: integer): HANDLE;
    function AddItem(data: string; Update: boolean = false): THandle;
    procedure AddTaskWindow(HWndTask: THandle);
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
    MonitorRect: Windows.TRect;
    BaseWindowRect: GDIPAPI.TRect; // rect of the dock main window. updated on every RecalcDock //
    BaseImageRect: GDIPAPI.TRect; // rect of the dock image. updated on every RecalcDock //
    ZoomInOutItem: extended; // relative mouse position on last WHMouseMove //
    Zooming: boolean; // true if mouse is over the panel and any items are zoomed //
    DropDistance: integer;
    LockDragging: boolean;
    DraggingItem: boolean; // indicates that item is being dragged //
    DragHWnd: THandle; // handle of the item that is being dragged //
    DraggingFile: boolean; // indicates that file is being dragged //
    DropPlace: integer; // index of free space //
    DropPlaceEx: integer; // index of a place to drop to //
    ParentHWnd: cardinal;
    BaseCmd: TBaseCmd;
    //
    FRegisteredPrograms: TStrings;

    property Visible: boolean read FVisible write SetVisible;
    property ItemsArea: windows.TRect read FItemsArea write FItemsArea;
    property ItemsArea2: windows.TRect read FItemsArea2 write FItemsArea2;
    property Margin: integer read FMargin write FMargin;
    property Margin2: integer read FMargin2 write FMargin2;
    property WndOffset: integer read FWndOffset write FWndOffset;

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
    procedure CheckDeleted;
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
    procedure SetFont(var Value: _FontData);
    procedure PluginCallCreate(HWnd: HANDLE);
    function GetPluginFile(HWnd: HANDLE): string;
    procedure SetPluginImage(HWnd: HANDLE; lpImageNew: Pointer; AutoDelete: boolean);
    procedure SetPluginOverlay(HWnd: HANDLE; lpOverlayNew: Pointer; AutoDelete: boolean);
    procedure PluginAnimate(HWnd: HANDLE);
    procedure SetPluginCaption(HWnd: HANDLE; NewCaption: string);
    function GetPluginCaption(HWnd: HANDLE): string;
    function GetPluginRect(HWnd: HANDLE; var r: windows.TRect): boolean;
    function IsPluginUndocked(HWnd: HANDLE): boolean;
    function IsSeparator(HWnd: HANDLE): boolean;
    function IsTask(HWnd: HANDLE): boolean;
end;

implementation
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
  ItemSize := 40;
  BigItemSize := 90;
  ZoomWidth := 6;
  ZoomTime := 120;
  ZoomItemSizeDiff := 0;
  Reflection := false;
  Monitor := 0;
  DropDistance := 80;
  DropPlace := NOT_AN_ITEM;
  DropPlaceEx := NOT_AN_ITEM;
  LockDragging := false;
  HoverItemHWnd := 0;
  SelectedItemHWnd := 0;
  DraggingItem := false;
  DragHWnd := 0;
  DraggingFile := false;
  LockMouseEffect := false;
  FCenterOffsetPercent := 50;
  FEdgeOffset := 0;
  FWndOffset := 0;
  itemsDeleted := TFPList.Create;
  FRegisteredPrograms := TStringList.Create;
end;
//------------------------------------------------------------------------------
destructor _ItemManager.Destroy;
begin
  Enabled := false;
  Clear;
  ClearDeleted;
  itemsDeleted.Free;
  FRegisteredPrograms.free;
  inherited;
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
    messagebox(ParentHWnd,
      pchar(UTF8ToAnsi(XErrorCritical + ' ' + XErrorContactDeveloper) + #10#13#10#13 + where),
      PROGRAM_NAME, MB_ICONERROR);
    halt;
  end else
    raise Exception.Create(where);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.notify(message: string);
begin
  dockh.notify(0, pchar(message));
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
      gpBigItemSize:            BigItemSize := value;
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
      gpMonitor:
        begin
          Monitor := value;
          ItemsChanged(true);
          Unzoom(true);
        end;
      gpSite:
        begin
          BaseSite := TBaseSite(value);
          BaseSiteVertical := (BaseSite = bsLeft) or (BaseSite = bsRight);
          ItemsChanged(true);
          Unzoom(true);
        end;
      gpCenterOffsetPercent:
        begin
          FCenterOffsetPercent := value;
          ItemsChanged;
        end;
      gpEdgeOffset:
        begin
          FEdgeOffset := value;
          ItemsChanged;
        end;
      gpDropDistance:           DropDistance := value;
      gpLockDragging:           LockDragging := boolean(value);
      gpReflection:
        begin
          Reflection := boolean(value);
          ItemsChanged(true);
        end;
      gpReflectionSize:         ReflectionSize := value;
      gpTaskbar:                if value = 0 then ClearTaskbar;
      gpTaskLivePreviews:
        begin
          ClearTaskbar;
          FTaskLivePreviews := value <> 0;
        end;
      gpTaskThumbSize: FTaskThumbSize := value;
      gpTaskGrouping:
        begin
          ClearTaskbar;
          FTaskGrouping := value <> 0;
        end;
      gpItemAnimation:          ItemAnimation := value;
      gpLaunchInterval:         LaunchInterval := value;
      gpActivateRunning:        ActivateRunning := value <> 0;
      gpUseShellContextMenus:   UseShellContextMenus := value <> 0;
      gpShowHint:               ShowHint := value <> 0;
      gpStackOpenAnimation:     StackOpenAnimation := value <> 0;
      gpZoomTime:               ZoomTime := value;
      gpLockMouseEffect:        LockMouseEffect := value <> 0;
      gpSeparatorAlpha:         FSeparatorAlpha := value;
      gpOccupyFullMonitor:      FOccupyFullMonitor := value <> 0;
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
      if params <> '' then Load(params);
    end;
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
procedure _ItemManager.DoBaseDraw(flags: integer);
begin
  if assigned(BaseCmd) then BaseCmd(tcRepaintBase, flags);
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
  idx: integer;
  cls_name, data: string;
  ini: TIniFile;
  list: TStrings;
  stack: TStackItem;
begin
  if fsets = '' then exit;
  DraggingItem := false;
  DraggingFile := false;
  DragHWnd := 0;

  try
    SetsFilename := fsets;
    ini := TIniFile.Create(SetsFilename);
    list := TStringList.Create;
    ini.ReadSections(list);
  except
    on e: Exception do err('ItemManager.Load.ReadIni', e, true);
  end;

  // read items //
  try
    idx := 0;
    while idx < list.count do
    begin
      if pos('item', list.strings[idx]) = 1 then
      begin
        cls_name := ini.ReadString(list.strings[idx], 'class', 'shortcut');
        data := 'class="' + cls_name + '";inifile="' + SetsFilename + '";inisection="' + list.strings[idx] + '";';
        AddItem(data, false);
      end;
      inc(idx);
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
      AddItem(TStackItem.Make(0, 'End session', ''));
      stack := TStackItem(GetWindowLong(items[ItemCount - 1].h, GWL_USERDATA));
      if stack is TStackItem then
      begin
        stack.AddSubitem(TShortcutItem.Make(0, 'Shutdown', '/shutdown', '', '', 'images\default\shutdown.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Reboot', '/reboot', '', '', 'images\default\reboot.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Suspend', '/suspend', '', '', 'images\default\suspend.png'));
        stack.AddSubitem(TShortcutItem.Make(0, 'Hibernate', '/hibernate', '', '', 'images\default\hibernate.png'));
      end;
      // basic items //
      AddItem(TShortcutItem.Make(0, UTF8ToAnsi(XStartButtonText), '/startmenu', '', '', 'images\default\start.png'));
      AddItem(TShortcutItem.Make(0, 'Computer', 'CSIDL_DRIVES', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Documents', '%doc%', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Control panel', 'CSIDL_CONTROLS', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Recycle bin', 'CSIDL_BITBUCKET', '', '', ''));
      AddItem(TShortcutItem.Make(0, 'Dock settings', '/sets', '', '', 'images\default\settings.png'));
      AddItem(TShortcutItem.Make(0, 'Theme', '/theme', '', '', 'images\default\theme.png'));
      AddItem(TSeparatorItem.Make);
      AddItem(TShortcutItem.Make(0, 'Tray', '/tray', '', '', 'images\default\tray.png'));
      AddItem(TShortcutItem.Make(0, '', '', '', '', 'images\default\{LANGID}.png'));
      AddItem(TShortcutItem.Make(0, '', '/networks', '', '', 'images\default\network-{NETWORK}.png'));
      AddItem(TShortcutItem.Make(0, '', '/volume', '', '', 'images\default\audio-volume-{VOLUME}.png'));
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
  idx: integer;
begin
  if Enabled and (ItemCount > 0) then
  try
    for idx := 0 to ItemCount - 1 do
    begin
      if items[idx].h <> 0 then TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA)).Save(pchar(SetsFilename), pchar('item' + inttostr(idx + 1)));
    end;
  except
    on e: Exception do raise Exception.Create('ItemManager.AllItemsSave::' + inttostr(idx) + #10#13 + e.message);
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
// clears the entire items array //
procedure _ItemManager.Clear;
var
  idx: integer;
  Inst: TCustomItem;
begin
  if ItemCount > 0 then
  try
    for idx := 0 to ItemCount - 1 do
    begin
      Inst := TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA));
      Inst.Freed := true;
      FreeAndNil(Inst);
      items[idx].h := 0;
    end;
    ItemCount := 0;
    TaskItemCount := 0;
  except
    on e: Exception do err('ItemManager.Clear', e);
  end;
end;
//------------------------------------------------------------------------------
// clears the "deleted items" array //
procedure _ItemManager.ClearDeleted;
var
  idx: integer;
  Inst: TCustomItem;
begin
  if itemsDeleted.Count > 0 then
  try
    for idx := 0 to itemsDeleted.Count - 1 do
    begin
      Inst := TCustomItem(GetWindowLong(THandle(itemsDeleted.Items[idx]), GWL_USERDATA));
      if Inst is TCustomItem then
      begin
        Inst.Freed := true;
        FreeAndNil(Inst);
      end;
    end;
    itemsDeleted.Clear;
  except
    on e: Exception do err('ItemManager.ClearDeleted', e);
  end;
end;
//------------------------------------------------------------------------------
// restores deleted items //
procedure _ItemManager.UnDelete;
begin
  try
    CheckDeleted;

    // restore most recent deleted item //
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
// clears out unwanted items from "deleted items" array //
procedure _ItemManager.CheckDeleted;
var
  h: THandle;
  idx: integer;
  Inst: TCustomItem;
begin
  try
    // clear task items //
    if itemsDeleted.Count > 0 then
      for idx := itemsDeleted.Count - 1 downto 0 do
      begin
        h := THandle(itemsDeleted.Items[idx]);
        Inst := TCustomItem(GetWindowLong(h, GWL_USERDATA));
        if Inst is TTaskItem then
        begin
          FreeAndNil(Inst);
          itemsDeleted.Delete(idx);
        end;
      end;
  except
    on e: Exception do err('ItemManager.UnDelete', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemIndex(HWnd: HANDLE): integer;
var
  idx: integer;
begin
  try
    result := NOT_AN_ITEM;
    if (HWnd = 0) or (ItemCount <= 0) then exit;

    for idx := 0 to ItemCount - 1 do
    begin
      if items[idx].h = HWnd then
      begin
        result := idx;
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
  idx: integer;
begin
  result := 0;
  if Enabled then
  try
    if ItemCount > 0 then result := ItemHWnd(0);
    idx := 0;
    while idx < ItemCount do
    begin
      SetWindowPos(ItemHWnd(idx), InsertAfter, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE + SWP_NOREPOSITION);
      inc(idx);
    end;
  except
    on e: Exception do err('ItemManager.ZOrder', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.Timer;
var
  elapsed: integer;
  doUpdate: boolean;
  item: integer;
  Inst: TCustomItem;
begin
  if not Enabled then exit;
  doUpdate := false;

  // zoom in/out smoothly //
  if ZoomItems or (ZoomItemSizeDiff > 0) then
  try
      if Zooming and (ZoomItemSizeDiff < BigItemSize - ItemSize) then
      begin
        doUpdate := true;
        elapsed := abs(GetTickCount - ZoomStartTime);
        if elapsed > ZoomTime then elapsed := ZoomTime;
        ZoomItemSizeDiff := (BigItemSize - ItemSize) * elapsed div ZoomTime;
      end;

      if not Zooming and (ZoomItemSizeDiff > 0) then
      begin
        doUpdate := true;
        elapsed := abs(GetTickCount - ZoomStartTime);
        if elapsed > ZoomTime then elapsed := ZoomTime;
        ZoomItemSizeDiff := BigItemSize - ItemSize - (BigItemSize - ItemSize) * elapsed div ZoomTime;
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
  if not Enabled then exit;

  try
    if BaseSiteVertical then
    begin
      y := MonitorRect.Top + (MonitorRect.Bottom - MonitorRect.Top - IASize) * FCenterOffsetPercent div 100;
    end else begin
      x := MonitorRect.Left + (MonitorRect.Right - MonitorRect.Left - IASize) * FCenterOffsetPercent div 100;
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
      itemPos := ItemSpacing div 2 + i * (ItemSize + ItemSpacing);
      if BaseSite = bsBottom then
      begin
        items[i].y := MonitorRect.Bottom - items[i].s - FMargin + FWndOffset - FEdgeOffset;
        if not IsSeparator(items[i].h) then items[i].y -= FMargin2;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = bsTop then
      begin
        items[i].y := MonitorRect.Top + FMargin + FEdgeOffset - FWndOffset;
        if not IsSeparator(items[i].h) then items[i].y += FMargin2;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = bsLeft then
      begin
        items[i].x := MonitorRect.Left + FMargin + FEdgeOffset - FWndOffset;
        if not IsSeparator(items[i].h) then items[i].x += FMargin2;
        items[i].y := y + itemPos;
      end
      else
      if BaseSite = bsRight then
      begin
        items[i].x := MonitorRect.Right - items[i].s - FMargin + FWndOffset - FEdgeOffset;
        if not IsSeparator(items[i].h) then items[i].x -= FMargin2;
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
begin
  if Enabled then
  try
    // width if vertical and height if horizontal //
    width := 0;
    height := 0;
    widthZoomed := 0;
    heightZoomed := 0;
    if BaseSite = bsLeft then
    begin
      width := FItemsArea.Left + FItemsArea2.Left + ItemSize + FItemsArea.Right + FItemsArea2.Right;
      widthZoomed := max(width, FItemsArea.Left + FItemsArea2.Left + ItemSize + ZoomItemSizeDiff);
    end else
    if BaseSite = bsRight then
    begin
      width := FItemsArea.Left + FItemsArea2.Left + ItemSize + FItemsArea.Right + FItemsArea2.Right;
      widthZoomed := max(width, FItemsArea.Right + FItemsArea2.Right + ItemSize + ZoomItemSizeDiff);
    end else
    if BaseSite = bsTop then
    begin
      height := FItemsArea.Top + FItemsArea2.Top + ItemSize + FItemsArea.Bottom + FItemsArea2.Bottom;
      heightZoomed := max(height, FItemsArea.Top + FItemsArea2.Top + ItemSize + ZoomItemSizeDiff);
    end else
    begin
      height := FItemsArea.Top + FItemsArea2.Top + ItemSize + FItemsArea.Bottom + FItemsArea2.Bottom;
      heightZoomed := max(height, FItemsArea.Bottom + FItemsArea2.Bottom + ItemSize + ZoomItemSizeDiff);
    end;

    // self XY relative to BaseWindowRect //
    if BaseSiteVertical then
    begin
      // x
      x := 0;
      if BaseSite = bsLeft then x := FMargin - FItemsArea.Left - FItemsArea2.Left;
      // y, height
      if FOccupyFullMonitor then
      begin
        y := 0;
        height := MonitorRect.Bottom - MonitorRect.Top;
      end else
      begin
        if ItemCount > 0 then
        begin
          y := items[0].y - ItemSpacing div 2 - FItemsArea.Top - FItemsArea2.Top - MonitorRect.Top;
          height := FItemsArea.Top + FItemsArea2.Top + items[ItemCount - 1].y + items[ItemCount - 1].s - items[0].y +
            ItemSpacing + FItemsArea.Bottom + FItemsArea2.Bottom;
        end
        else
        begin
          height := ItemSize + FItemsArea.Top + FItemsArea.Bottom + FItemsArea2.Top + FItemsArea2.Bottom;
          y := (MonitorRect.Bottom - MonitorRect.Top - IASize) * FCenterOffsetPercent div 100 -
            FItemsArea.Top - FItemsArea2.Top + (IASize - height + FItemsArea.Top + FItemsArea.Bottom + FItemsArea2.Top + FItemsArea2.Bottom - ItemSpacing) div 2;
        end;
      end;

    end else
    begin

      // y
      y := 0;
      if BaseSite = bsTop then y := FMargin - FItemsArea.Top - FItemsArea2.Top;
      // x, width
      if FOccupyFullMonitor then
      begin
        x := 0;
        width := MonitorRect.Right - MonitorRect.Left;
      end else
      begin
        if ItemCount > 0 then
        begin
          x := items[0].x - ItemSpacing div 2 - FItemsArea.Left - FItemsArea2.Left - MonitorRect.Left;
          width := FItemsArea.Left + FItemsArea2.Left + items[ItemCount - 1].x + items[ItemCount - 1].s - items[0].x +
            ItemSpacing + FItemsArea.Right + FItemsArea2.Right;
        end
        else
        begin
          width := ItemSize + FItemsArea.Left + FItemsArea.Right + FItemsArea2.Left + FItemsArea2.Right;
          x := (MonitorRect.Right - MonitorRect.Left - IASize) * FCenterOffsetPercent div 100 -
            FItemsArea.Left - FItemsArea2.Left +
            (IASize - width + FItemsArea.Left + FItemsArea.Right + FItemsArea2.Left + FItemsArea2.Right - ItemSpacing) div 2;
        end;
      end;

    end;

    // background image rect //
    BaseImageRect.x := x;
    BaseImageRect.y := y;
    BaseImageRect.Width := Width;
    BaseImageRect.Height := Height;

    // main form rect //
    if BaseSiteVertical then
    begin
      BaseWindowRect.Width := Width;
      if BaseSite = bsRight then BaseWindowRect.Width += FMargin - FItemsArea.Right - FItemsArea2.Right;
      BaseWindowRect.Height := MonitorRect.Bottom - MonitorRect.Top;
    end else begin
      BaseWindowRect.Width := MonitorRect.Right - MonitorRect.Left;
      BaseWindowRect.Height := Height;
      if BaseSite = bsBottom then BaseWindowRect.Height += FMargin - FItemsArea.Bottom - FItemsArea2.Bottom;
    end;

    BaseWindowRect.x := MonitorRect.Left;
    BaseWindowRect.y := MonitorRect.Top;
    if BaseSite = bsLeft then BaseWindowRect.x := MonitorRect.Left - FWndOffset + FEdgeOffset
    else if BaseSite = bsTop then BaseWindowRect.y := MonitorRect.Top - FWndOffset + FEdgeOffset
    else if BaseSite = bsRight then BaseWindowRect.x := MonitorRect.Right - BaseWindowRect.Width + FWndOffset - FEdgeOffset
    else if BaseSite = bsBottom then BaseWindowRect.y := MonitorRect.Bottom - BaseWindowRect.Height + FWndOffset - FEdgeOffset;
  except
    on e: Exception do raise Exception.Create('ItemManager.RecalcDock'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetRect: windows.TRect;
begin
  result.Left := BaseWindowRect.X + X;
  result.Top := BaseWindowRect.Y + Y;
  result.Right := result.Left + Width;
  result.Bottom := result.Top + Height;
  case BaseSite of
    bsLeft: result.Right := max(result.Right, GetZoomEdge);
    bsTop: result.Bottom := max(result.Bottom, GetZoomEdge);
    bsRight: result.Left := min(result.Left, GetZoomEdge);
    bsBottom: result.Top := min(result.Top, GetZoomEdge);
  end;
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
procedure _ItemManager.SetItems2(force_draw: boolean);
var
  idx: integer;
  wpi, show_items: uint;
begin
  if Enabled then
  try
    idx := ItemCount;
    wpi := BeginDeferWindowPos(idx);
    show_items := swp_hidewindow;
    if FVisible then show_items := swp_showwindow;

    // draw items //
    idx := 0;
    while idx < ItemCount do
    begin
      if items[idx].h <> 0 then
        TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA)).Draw(
          items[idx].x, items[idx].y, items[idx].s, force_draw, wpi, show_items);
      inc(idx);
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
  if Enabled then AddItem(AData, true);
end;
//------------------------------------------------------------------------------
// create an item and put it onto dock
function _ItemManager.AddItem(data: string; Update: boolean = false): THandle;
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
// create item and return its window handle
// item should be defined by a string. example:
// class="shortcut";caption="Cmd";command="cmd.exe";
//
// if class = stack there could be more than 1 string
// in this case each one starting from the 2nd should be of shortcut class
function _ItemManager.CreateItem(data: string): THandle;
var
  class_name, str: string;
  Inst: TCustomItem;
  icp: _ItemCreateParams;
begin
  result := 0;
  Inst := nil;
  try
    class_name := AnsiLowerCase(FetchValue(data, 'class="', '"'));

    icp.ItemSize := ItemSize;
    icp.BigItemSize := BigItemSize;
    icp.LaunchInterval := LaunchInterval;
    icp.ActivateRunning := ActivateRunning;
    icp.UseShellContextMenus := UseShellContextMenus;
    icp.Site := integer(BaseSite);
    icp.Reflection := Reflection;
    icp.ReflectionSize := ReflectionSize;
    icp.ShowHint := ShowHint;
    icp.Animation := ItemAnimation;
    icp.LockDragging := LockDragging;
    icp.StackOpenAnimation := StackOpenAnimation;
    icp.SeparatorAlpha := FSeparatorAlpha;
    icp.TaskLivePreviews := FTaskLivePreviews;
    icp.TaskThumbSize := FTaskThumbSize;
    icp.TaskGrouping := FTaskGrouping;
    CopyFontData(FFont, icp.Font);

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
      if Inst.Freed then // if something went wrong
      begin
        FreeAndNil(Inst);
      end else // if everything is okay
      begin
        result := Inst.HWnd;
        // add to registered programs list
        str := Inst.RegisterProgram;
        if str <> '' then FRegisteredPrograms.Add(AnsiLowerCase(str));
      end;
  except
    on e: Exception do raise Exception.Create('ItemManager.CreateItem.Fin'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// do not call directly !!!
// items call this using DockH.DockDeleteItem
// use TCustomItem.Delete instead
procedure _ItemManager.DeleteItem(HWnd: THandle);
var
  index, rpIndex: integer;
  is_task: boolean;
  Inst: TCustomItem;
begin
  if Enabled then
  try
    index := ItemIndex(HWnd);
    is_task := IsTask(HWnd);
    // add to "deleted" list //
    itemsDeleted.Add(Pointer(HWnd));
    // remove from registered programs list
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    rpIndex := FRegisteredPrograms.IndexOf(AnsiLowerCase(Inst.RegisterProgram));
    if rpIndex >= 0 then FRegisteredPrograms.Delete(rpIndex);

    if index <> NOT_AN_ITEM then
    begin
      // erase it from "items" list //
      while index < ItemCount - 1 do
      begin
        items[index].h := items[index + 1].h;
        items[index].x := items[index + 1].x;
        items[index].y := items[index + 1].y;
        items[index].s := items[index + 1].s;
        inc(index);
      end;
      // decrement item count
      dec(ItemCount);
      if is_task and (TaskItemCount > 0) then dec(TaskItemCount);
    end;

    // update dock //
    ItemsChanged;
  except
    on e: Exception do raise Exception.Create('ItemManager.DeleteItem'#10#13 + e.message);
  end;
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
  prevDropPlace, prevDropPlaceEx: integer;
begin
  prevDropPlace := DropPlace;
  prevDropPlaceEx := DropPlaceEx;

  if Enabled then
  try
    // DropPlace //
    tmp := ItemFromPoint(pt.x, pt.y, DropDistance);
    if tmp = NOT_AN_ITEM then
    begin
      DropPlace := NOT_AN_ITEM;
    end else begin
      if DropPlace = NOT_AN_ITEM then DropPlace := round(tmp)
      else
      if (abs(DropPlace + 0.5 - tmp) > 1.2) or (tmp = -1) then DropPlace := round(tmp - 0.5);
      // "+0.5" to count from the center of the DropPlace. And "-0.5" to compensate the "+0.5"
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
    // seek for current DropPlace in the items array (item.h = 0) //
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
      if DraggingItem then
      begin
        Inst := TCustomItem(GetWindowLong(items[DropPlaceEx].h, GWL_USERDATA));
        DragInst := TCustomItem(GetWindowLong(DragHWnd, GWL_USERDATA));
        if ((Inst is TStackItem) and (DragInst is TStackItem)) or
          ((Inst is TStackItem) and (DragInst is TShortcutItem)) or
          ((Inst is TShortcutItem) and (DragInst is TShortcutItem)) then atype := DII_ADD; // add
        if atype = 0 then DropPlaceEx := DropPlace;
      end;

    if DraggingItem then
    begin
      AllItemCmd(icDropIndicator, 0);
      if atype > 0 then ItemCmd(items[DropPlaceEx].h, icDropIndicator, atype);
    end;
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
      BasePoint := MonitorRect.Top + (MonitorRect.Bottom - MonitorRect.Top - IASize) * FCenterOffsetPercent div 100;
      result := (Ay - BasePoint) / (ItemSize + ItemSpacing);
    end else begin
      BasePoint := MonitorRect.Left + (MonitorRect.Right - MonitorRect.Left - IASize) * FCenterOffsetPercent div 100;
      result := (Ax - BasePoint) / (ItemSize + ItemSpacing);
    end;
    if result < -1 then result := NOT_AN_ITEM;
    if result >= ItemCount + 1 then result := NOT_AN_ITEM;

    // check boundaries //

    rItemArea := FItemsArea;
    if BaseSite = bsLeft then rItemArea.Right := 0
    else if BaseSite = bsTop then rItemArea.Bottom := 0
    else if BaseSite = bsRight then rItemArea.Left := 0
    else if BaseSite = bsBottom then rItemArea.Top := 0;

    if BaseSite = bsBottom then
    begin
      if (Ay < BaseWindowRect.Y + BaseWindowRect.Height - FMargin - ItemSize - rItemArea.Top - ZoomItemSizeDiff - distance) or
        (Ay > BaseWindowRect.Y + BaseWindowRect.Height + distance) or
        (Ax < BasePoint - BigItemSize) or
        (Ax > BasePoint + width)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = bsLeft then
    begin
      if (Ax < BaseWindowRect.X - distance) or
        (Ax > BaseWindowRect.X + x + FMargin + ItemSize + rItemArea.Right + ZoomItemSizeDiff + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = bsRight then
    begin
      if (Ax < BaseWindowRect.X + BaseWindowRect.Width - FMargin - ItemSize - rItemArea.Left - ZoomItemSizeDiff - distance) or
        (Ax > BaseWindowRect.X + BaseWindowRect.Width + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    begin
      if (Ay < BaseWindowRect.Y - distance) or
        (Ay > BaseWindowRect.Y + y + FMargin + ItemSize + rItemArea.Bottom + ZoomItemSizeDiff + distance) or
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
  idx: integer;
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

    for idx := 0 to ItemCount - 1 do
    begin
      Inst := TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA));
      if Inst is TCustomItem then
        if PtInRect(Inst.ScreenRect, classes.Point(Ax, Ay)) then
        begin
          result := idx;
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
  if enabled and not DraggingFile then
  try
    item := NOT_AN_ITEM;
    if Zooming or CheckMouseOn or DraggingItem then
    begin
      item := ItemFromPoint(x, y, ifthen(DraggingItem, DropDistance, 0));
      if item <> NOT_AN_ITEM then
      begin
        if item < 0 then item := NOT_AN_ITEM;
        if item >= ItemCount then item := NOT_AN_ITEM;
      end;
    end;

    // enter zooming mode //
    if not Zooming and (item <> NOT_AN_ITEM) then
    begin
      ZoomStartTime := GetTickCount;
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
    ZoomStartTime := GetTickCount;
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
    if DraggingItem or DraggingFile or Zooming then
      item := trunc(ItemFromPoint(pt.x, pt.y, ifthen(DraggingItem or DraggingFile, DropDistance, 0)))
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

    if ItemCount > 0 then
    begin
      // hint //
      wnd := WindowFromPoint(pt);
      if ItemIndex(wnd) = NOT_AN_ITEM then wnd := 0;
      if wnd <> HoverItemHWnd then
      begin
        if HoverItemHWnd <> 0 then ItemCmd(HoverItemHWnd, icHover, 0);
        if wnd <> 0 then ItemCmd(wnd, icHover, 1);
      end;
      HoverItemHWnd := wnd;
      // zoom //
      if allow_zoom and ZoomItems then Zoom(pt.x, pt.y);
    end;

    // drop place //
    if DraggingItem or DraggingFile then
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
  DraggingItem := false;
  DraggingFile := true;
  DragOver;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.DragLeave;
begin
  DraggingItem := false;
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
  idx: integer;
begin
  if not CheckMouseOn then
  begin
    Unzoom;
    AllItemCmd(icHover, 0);
  end;

  if Enabled then
  try
    idx := 0;
    while idx < ItemCount do
    begin
      if items[idx].h <> 0 then postmessage(items[idx].h, WM_ACTIVATEAPP, 0, 0);
      inc(idx);
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
  if enabled and not DraggingItem then
  try
    //AllItemCmd(icHover, 0);
    DraggingFile := false;
    DraggingItem := true;
    DragHWnd := HWnd;
    index := ItemIndex(HWnd);
    try if index <> NOT_AN_ITEM then
      begin
        items[index].h := 0;
        SetDropPlace(index);
        SetDropPlaceEx(index);
      end;
    except end;
    //ItemsChanged;
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
  // if DropPlace exists, then it is not TaskItem //
  begin
    if DropPlace > ItemCount - TaskItemCount then DropPlace := ItemCount - TaskItemCount;
    items[DropPlace].h := HWnd;
  end
  // else check where to dock //
  else begin
    if IsTask(HWnd) then
    begin
      SetDropPlace(ItemCount);
      inc(TaskItemCount);
    end
    else
      SetDropPlace(ItemCount - TaskItemCount);
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
  idx: integer;
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
        idx := 0;
        while idx < TStackItem(DragInst).ItemCount do
        begin
          TStackItem(Inst).AddSubitem(TStackItem(DragInst).SubitemToString(idx));
          inc(idx);
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

    DraggingItem := false;
    DragHWnd := 0;
    SetDropPlaceEx(NOT_AN_ITEM);
    SetDropPlace(NOT_AN_ITEM);
    ItemsChanged;
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
  idx: integer;
  Inst: TCustomItem;
begin
  result := THandle(0);
  if Enabled then
  try
    idx := 0;
    while idx < ItemCount do
    begin
      if items[idx].h = HWnd then
      begin
        result := HWnd;
        break;
      end;

      Inst := TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA));
      if Inst is TCustomItem then result := Inst.cmd(icIsItem, integer(HWnd));
      if result <> 0 then break;
      inc(idx);
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
  idx: integer;
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
        for idx := 0 to files.Count - 1 do
          result := result or Inst.DropFile(HWndChild, pt, files.strings[idx]);
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
procedure _ItemManager.SetFont(var Value: _FontData);
var
  item: integer;
  Inst: TCustomItem;
begin
  try
    CopyFontData(Value, FFont);
    item := 0;
    while item < ItemCount do
    begin
      Inst := TCustomItem(GetWindowLong(items[item].h, GWL_USERDATA));
      if Inst is TCustomItem then Inst.SetFont(Value);
      inc(item);
    end;
  except
    on e: Exception do err('ItemManager.SetFont', e);
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
    if (Inst is TCustomItem) and (ItemAnimation > 0) then Inst.Animate(ItemAnimation);
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
function _ItemManager.GetPluginRect(HWnd: HANDLE; var r: windows.TRect): boolean;
var
  Inst: TCustomItem;
begin
  try
    result := Visible;
    r := classes.Rect(0, 0, 0, 0);
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then r := Inst.ScreenRect;
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
    on e: Exception do err('ItemManager.IsTask', e);
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
  index: integer;
  Inst: TCustomItem;
begin
  try
    // remove deleted windows //
    try
      index := 0;
      while index < ItemCount do
      begin
        Inst := TCustomItem(GetWindowLong(items[index].h, GWL_USERDATA));
        if Inst is TTaskItem then TTaskItem(Inst).RemoveNonExisting;
        inc(index);
      end;
      Inst := nil;
    except
      on e: Exception do err('ItemManager.RemoveTaskWindow', e);
    end;

    // add existing windows //
    index := 0;
    while index < ProcessHelper.GetAppWindowsCount do
    begin
      AddTaskWindow(ProcessHelper.GetAppWindowHandle(index));
      inc(index);
    end;

    // delete empty items and update not empty ones
    index := ItemCount - 1;
    while index >= 0 do
    begin
      Inst := TCustomItem(GetWindowLong(items[index].h, GWL_USERDATA));
      if Inst is TTaskItem then
        if TTaskItem(Inst).IsEmpty then TTaskItem(Inst).Delete else TTaskItem(Inst).UpdateItem;
      dec(index);
    end;
  except
    on e: Exception do err('ItemManager.Taskbar', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.AddTaskWindow(HWndTask: THandle);
var
  index, found: integer;
  HWndItem: THandle;
  Inst: TCustomItem;
  str: string;
begin
  try
    // do not add registered programs, so check for it
    str := AnsiLowerCase(ProcessHelper.GetWindowProcessName(HWndTask));
    index := FRegisteredPrograms.IndexOf(str);
    if index < 0 then index := FRegisteredPrograms.IndexOf(ExtractFileName(str));
    if index >= 0 then exit;

    // search existing TaskItem for the given window
    found := -1;
    index := 0;
    while index < ItemCount do
    begin
      Inst := TCustomItem(GetWindowLong(items[index].h, GWL_USERDATA));
      if Inst is TTaskItem then
        if TTaskItem(Inst).WindowInList(HWndTask) then
        begin
          found := index;
          break;
        end;
      inc(index);
    end;

    // if there is no item for the window - add a new item //
    if found = -1 then
    begin
      SetDropPlace(NOT_AN_ITEM);
      HWndItem := AddItem(TTaskItem.Make, true);
      Inst := TCustomItem(GetWindowLong(HWndItem, GWL_USERDATA));
      if Inst is TTaskItem then TTaskItem(Inst).UpdateTaskItem(HWndTask);
    end;
  except
    on e: Exception do err('ItemManager.AddTaskWindow', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.ClearTaskbar;
var
  idx: integer;
  Inst: TCustomItem;
begin
  if TaskItemCount > 0 then
  begin
      idx := ItemCount - 1;
      while idx >= 0 do
      begin
        Inst := TCustomItem(GetWindowLong(items[idx].h, GWL_USERDATA));
        if Inst is TTaskItem then Inst.Delete;
        dec(idx);
      end;
      TaskItemCount := 0;
  end;
end;
//------------------------------------------------------------------------------
end.
