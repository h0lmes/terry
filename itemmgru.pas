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
    // Wnd Item Vars //
    Visible: boolean;
    Enabled: boolean;
    BaseSite: integer;
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
  public
    items: array [0..MAX_ITEM_COUNT - 1] of TItem;
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

    constructor Create(AEnabled, AVisible: boolean; Handle: THandle; ABaseCmd: TBaseCmd);
    destructor Destroy; override;
    procedure Enable(value: boolean);
    procedure SetParam(id: TGParam; value: integer);
    procedure command(cmd, params: string);
    function GetRect: windows.TRect;
    function GetZoomEdge: integer;
    function GetMonitorBoundsRect: Windows.TRect;

    // notify about events //
    procedure Timer;
    procedure SetTheme;
    procedure ItemsChanged(FullUpdate: boolean = false);
    procedure SetItems1;
    procedure RecalcDock;
    procedure SetItems2(force_draw: boolean);
    procedure SetVisible(value: boolean);

    // load & save //
    procedure Load(fsets: string);
    procedure Save(fsets: string);
    procedure AllItemsSave;
    procedure ItemSave(HWnd: uint);

    // task item procs //
    procedure Taskbar;
    function GetTaskItemIndex(h: THandle): integer;
    procedure ClearTaskbar;

    // items //
    procedure Clear;
    procedure ClearDeleted;
    procedure UnDelete;
    function ItemIndex(HWnd: HANDLE): integer;
    function ItemHWnd(index: integer): HANDLE;
    procedure SetItemSize(index, value: integer);
    procedure ResetItemsSize(update: boolean = true);
    function ZOrder(InsertAfter: uint): uint;
    procedure InsertItem(AData: string);
    function AddItem(data: string; Update: boolean = false; Save: boolean = true): THandle;
    function CreateItem(data: string): THandle;
    procedure DeleteItem(HWnd: THandle);
    procedure CopyItemDescriptor(pFrom, pTo: PItem);

    // mouse effects //
    procedure CalcDropPlace(pt: windows.TPoint);
    procedure SetDropPlace(index: integer);
    procedure SetDropPlaceEx(index: integer);
    function IASize: integer;
    function ItemFromPoint(Ax, Ay, distance: integer): extended;
    function ItemRectFromPoint(Ax, Ay: integer): integer;
    procedure Zoom(x, y: integer);
    procedure ZoomInternal(item: extended);
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
    function ItemDrop(HWnd: HANDLE; HWndChild: HANDLE; pt: windows.TPoint; filename: string): boolean;
    function ItemCmd(HWnd: HANDLE; id: TGParam; param: integer): integer;
    function AllItemCmd(id: TGParam; param: integer): integer;
    function GetPluginFile(HWnd: HANDLE): string;
    procedure SetPluginImage(HWnd: HANDLE; lpImageNew: Pointer; AutoDelete: boolean);
    procedure SetPluginOverlay(HWnd: HANDLE; lpOverlayNew: Pointer; AutoDelete: boolean);
    procedure PluginAnimate(HWnd: HANDLE);
    procedure SetPluginCaption(HWnd: HANDLE; NewCaption: string);
    function GetPluginCaption(HWnd: HANDLE): string;
    function GetPluginRect(HWnd: HANDLE): windows.TRect;
    function IsPluginVisible(HWnd: HANDLE): boolean;
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
  Visible := AVisible;
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
          ResetItemsSize(false);
          ItemsChanged(true);
        end;
      gpBigItemSize: BigItemSize := value;
      gpZoomWidth:
        begin
          ZoomWidth := value;
          WHMouseMove(classes.point(-100, -100));
        end;
      gpZoomSmoothingLevel: FZoomSmoothingLevel := value;
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
          BaseSite := value;
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
    if cmd = 'tray' then AddItem('class="shortcut";caption="tray";command="/tray";image="Images\default\tray.png";', true);
    if cmd = 'separator' then AddItem('class="separator";', true);
    if cmd = 'plugin' then AddItem('class="plugin";file="' + params + '";', true);
    if cmd = 'stack' then
    begin
      if length(params) = 0 then data := 'class="stack";caption="' + UTF8ToAnsi(XStack) + '";'
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
    0: result := BaseWindowRect.X + x + widthZoomed;
    1: result := BaseWindowRect.Y + y + heightZoomed;
    2: result := BaseWindowRect.X + x + width - widthZoomed;
    3: result := BaseWindowRect.Y + y + height - heightZoomed;
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.GetMonitorBoundsRect: Windows.TRect;
begin
  Result := screen.Monitors[Monitor].BoundsRect;
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
      AddItem(TShortcutItem.Make(0, 'Computer', '::::14001F50E04FD020EA3A6910A2D808002B30309D0000', '', '', ICON_COMPUTER));
      AddItem(TShortcutItem.Make(0, 'Documents', '::::14001F4225481E03947BC34DB131E946B44C8DD5200000001A00EEBBFE23000010007DB10D7BD29C934A973346CC89022E7C00000000', '', '', ICON_FOLDER_DOCUMENTS));
      AddItem(TShortcutItem.Make(0, 'Control panel', '::::14001F706806EE260AA0D7449371BEB064C986830C0001008421DE390000000000000000', '', '', ICON_CONTROL_PANEL));
      AddItem(TShortcutItem.Make(0, 'Trash bin', '::::14001F7840F05F6481501B109F0800AA002F954E0000', '', '', ICON_TRASH_BIN));
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
    if index = -1 then
    begin
      if TaskItemCount = 0 then
      begin
        // add task items separator //
        AddItem('class="separator";dontsave="1";candrag="0";', true, false);
        inc(TaskItemCount);
      end;
      SetDropPlace(NOT_AN_ITEM); // insert after the last item //
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

  // delete task items separator //
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
    ItemsChanged;
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
    ItemsChanged;
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
procedure _ItemManager.ResetItemsSize(update: boolean = true);
var
  i: integer;
begin
  if Enabled then
  try
    ZoomItemSizeDiff := 0;
    i := 0;
    while i < ItemCount do
    begin
      items[i].s := ItemSize;
      inc(i);
    end;
    if update then ItemsChanged;
  except
    on e: Exception do err('ItemManager.ResetItemsSize', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetItemSize(index, value: integer);
begin
  if Enabled then
    if (index >= 0) and (index < ItemCount) then items[index].s := value;
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
  need_zoom_items: boolean;
  item: integer;
  Inst: TCustomItem;
begin
  if not Enabled then exit;
  need_zoom_items := false;
  zoom_minstep := 2 + ZoomSpeed;
  zoom_stepcount := 9 - ZoomSpeed;

  // smooth zoom in/out //
  if ZoomItems or (ZoomItemSizeDiff > 0) then
  try
      if Zooming and (ZoomItemSizeDiff < BigItemSize - ItemSize) then
      begin
        need_zoom_items := true;
        xstep := max(abs(BigItemSize - ItemSize - ZoomItemSizeDiff) div zoom_stepcount, zoom_minstep);
        if abs(BigItemSize - ItemSize - ZoomItemSizeDiff) <= zoom_minstep
        then ZoomItemSizeDiff := BigItemSize - ItemSize
        else if ZoomItemSizeDiff < BigItemSize - ItemSize then inc(ZoomItemSizeDiff, round(xstep));
      end;

      if not Zooming and (ZoomItemSizeDiff > 0) then
      begin
        need_zoom_items := true;
        xstep := max(abs(ZoomItemSizeDiff) div zoom_stepcount, zoom_minstep);
        if abs(ZoomItemSizeDiff) <= zoom_minstep then ZoomItemSizeDiff := 0
        else if ZoomItemSizeDiff > 0 then dec(ZoomItemSizeDiff, round(xstep));
      end;

      if need_zoom_items then ZoomInternal(ZoomInOutItem);
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
  if value <> Visible then
  begin
    UnZoom(true);
    AllItemCmd(icHover, 0);
    AllItemCmd(icSelect, 0);
    HoverItemHWnd := 0;
    SelectedItemHWnd := 0;
  end;
  Visible := value;
  ItemsChanged;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetItems1;
function getHalfBubble: integer;
var
  i: extended;
begin
  result := round(ZoomItemSizeDiff / 2);
  i := 0.5;
  while i < ZoomWidth / 2 do
  begin
    result := result + round((ZoomItemSizeDiff - 1) * (cos(PI * i * 2 / ZoomWidth) + 1) / 2);
    i := i + 1;
  end;
end;

var
  i, sizeInc, offset, itemPos: integer;
begin
  if not Enabled or not assigned(theme) then exit;

  try
    FItemArea := theme.CorrectMargins(theme.ItemsArea);
    if BaseSite mod 2 = 0 then
    begin
      y := MonitorRect.Top + (MonitorRect.Bottom - MonitorRect.Top - IASize) * sets.container.CenterOffsetPercent div 100;
    end else begin
      x := MonitorRect.Left + (MonitorRect.Right - MonitorRect.Left - IASize) * sets.container.CenterOffsetPercent div 100;
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
        else if i < trunc(ZoomInOutItem) then sizeInc := round((ZoomItemSizeDiff - 1) * (cos(PI * 2 * (i - ZoomInOutItem + 1) / ZoomWidth) + 1) / 2)
        else if i > trunc(ZoomInOutItem) then sizeInc := round((ZoomItemSizeDiff - 1) * (cos(PI * 2 * (i - ZoomInOutItem) / ZoomWidth) + 1) / 2);
      end;
      items[i].s := ItemSize + sizeInc;

      // icon position when not zooming //
      itemPos := i * (ItemSize + ItemSpacing);
      if BaseSite = 3 then
      begin
        items[i].y := FItemArea.Top + ItemSize - items[i].s;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = 1 then
      begin
        items[i].y := FItemArea.Top;
        items[i].x := x + itemPos;
      end
      else
      if BaseSite = 0 then
      begin
        items[i].x := FItemArea.Left;
        items[i].y := y + itemPos;
      end
      else
      if BaseSite = 2 then
      begin
        items[i].x := FItemArea.Left + ItemSize - items[i].s;
        items[i].y := y + itemPos;
      end;

      // icon position when zooming //
      if ZoomItemSizeDiff > 0 then
      begin
        if (BaseSite = 3) or (BaseSite = 1) then
        begin
          if i < trunc(ZoomInOutItem) - ZoomWidth / 2 then items[i].x := x + itemPos - offset
          else if i > trunc(ZoomInOutItem) + ZoomWidth / 2 then items[i].x := x + itemPos + offset
          else if i = trunc(ZoomInOutItem) then items[i].x := x + itemPos - round(ZoomItemSizeDiff * frac(ZoomInOutItem));
        end
        else
        begin
          if i < trunc(ZoomInOutItem) - ZoomWidth / 2 then items[i].y := y + itemPos - offset
          else if i > trunc(ZoomInOutItem) + ZoomWidth / 2 then items[i].y := y + itemPos + offset
          else if i = trunc(ZoomInOutItem) then items[i].y := y + itemPos - round(ZoomItemSizeDiff * frac(ZoomInOutItem));
        end;
      end;

      inc(i);
    end;

    // icon position when zooming //
    if ZoomItemSizeDiff > 0 then
    begin
      i := trunc(ZoomInOutItem) - 1;
      while (i >= trunc(ZoomInOutItem) - ZoomWidth / 2) and (i >= 0)  do
      begin
        if (BaseSite = 3) or (BaseSite = 1) then
          items[i].x := items[i + 1].x - items[i].s - ItemSpacing
        else
          items[i].y := items[i + 1].y - items[i].s - ItemSpacing;
        dec(i);
      end;
      i := trunc(ZoomInOutItem) + 1;
      while (i <= trunc(ZoomInOutItem) + ZoomWidth / 2) and (i < ItemCount)  do
      begin
        if (BaseSite = 3) or (BaseSite = 1) then
          items[i].x := items[i - 1].x + items[i - 1].s + ItemSpacing
        else
          items[i].y := items[i - 1].y + items[i - 1].s + ItemSpacing;
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
  vbo: boolean;
  zi_int: integer;
  zi_frac: extended;
begin
  if Enabled and assigned(theme) then
  try
    vbo := (BaseSite = 0) or (BaseSite = 2);

    // width and height //
    width := 0;
    height := 0;
    widthZoomed := 0;
    heightZoomed := 0;
    widthOverhead := 0;
    heightOverhead := 0;
    // vertical //
    if vbo then
    begin
      width := ItemSize + FItemArea.Left + FItemArea.Right;
      widthZoomed := max(width, ifthen(BaseSite = 0, FItemArea.Left, FItemArea.Right) + ItemSize + ZoomItemSizeDiff);
      widthOverhead := 0;
      if Zooming and DraggingFile then widthOverhead := BigItemSize - ItemSize;
    // horizontal //
    end else begin
      height := ItemSize + FItemArea.Top + FItemArea.Bottom;
      heightZoomed := max(height, ifthen(BaseSite = 1, FItemArea.Top, FItemArea.Bottom) + ItemSize + ZoomItemSizeDiff);
      heightOverhead := 0;
      if Zooming and DraggingFile then heightOverhead := BigItemSize - ItemSize;
    end;

    // self XY relative to BaseWindowRect //
    if BaseSite = 0 then x := 0
    else
    if BaseSite = 2 then x := widthOverhead
    else
    begin
      if ItemCount = 0 then
      begin
        width := ItemSize + FItemArea.Left + FItemArea.Right;
        x := MonitorRect.Left + (MonitorRect.Right - MonitorRect.Left - IASize) * sets.container.CenterOffsetPercent div 100
          - FItemArea.Left + (IASize - width + FItemArea.Left + FItemArea.Right - ItemSpacing) div 2;
      end
      else
      begin
        x := items[0].x - ItemSpacing div 2 - FItemArea.Left;
        width := items[ItemCount - 1].x + items[ItemCount - 1].s - x + ItemSpacing div 2 + FItemArea.Right;
      end;
    end;

    if BaseSite = 1 then y := 0
    else
    if BaseSite = 3 then y := heightOverhead
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
    if BaseSite = 0 then BaseWindowRect.x := MonitorRect.Left - sets.wndoffset + sets.container.EdgeOffset
    else if BaseSite = 1 then BaseWindowRect.y := MonitorRect.Top - sets.wndoffset + sets.container.EdgeOffset
    else if BaseSite = 2 then BaseWindowRect.x := MonitorRect.Right - Width - widthOverhead + sets.wndoffset - sets.container.EdgeOffset
    else if BaseSite = 3 then BaseWindowRect.y := MonitorRect.Bottom - Height - heightOverhead + sets.wndoffset - sets.container.EdgeOffset;
    if vbo then
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
    if visible then show_items := swp_showwindow;

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
procedure _ItemManager.InsertItem(AData: string);
begin
  if Enabled then AddItem(AData, true, true);
end;
//------------------------------------------------------------------------------
function _ItemManager.AddItem(data: string; Update: boolean = false; Save: boolean = true): THandle;
begin
  result := 0;
  if ItemCount > MAX_ITEM_COUNT then exit;
  AllItemCmd(icHover, 0); // avoid hint hang up //
  AllItemCmd(icSelect, 0);

  result := CreateItem(data);
  if not (result = THandle(0)) then DockAdd(result);
  if Update then ItemsChanged(true);
  // save settings //
  if Save then ItemSave(result);
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
    icp.Site := integer(sets.container.Site);
    icp.Reflection := sets.container.Reflection;
    icp.ReflectionSize := theme.ReflectionSize;
    icp.ShowHint := sets.container.ShowHint;
    icp.Animation := sets.container.ItemAnimation;
    icp.LockDragging := sets.container.LockDragging;

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
    if Inst <> nil then
      if Inst.Freed then FreeAndNil(Inst) else result := Inst.HWnd;
  except
    on e: Exception do raise Exception.Create('ItemManager.CreateItem.Fin'#10#13 + e.message);
  end;
end;
//------------------------------------------------------------------------------
// do not call directly !!!
// item call this using DockH.DockDeleteItem from TCustomItem.Delete
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
    if basesite mod 2 = 0 then dec(cy, ItemSize div 2) else dec(cx, ItemSize div 2);
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
      if DropPlaceEx > ItemCount - TaskItemCount - 1 then DropPlaceEx := NOT_AN_ITEM;//ItemCount - TaskItemCount - 1;
    end;
    if prevDropPlaceEx <> DropPlaceEx then SetDropPlaceEx(DropPlaceEx);
  except
    on e: Exception do err('ItemManager.CalcDropPlace', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetDropPlace(index: integer);
var
  i, current: integer;
begin
  try
    AllItemCmd(icDropIndicator, 0);

    DropPlace := index;
    // seek for current DropPlace in items array //
    current := NOT_AN_ITEM;
    if ItemCount > 0 then
      for i := 0 to ItemCount - 1 do
      begin
        if items[i].h = 0 then
        begin
          current := i;
          break;
        end;
      end;

    if (current = NOT_AN_ITEM) and (DropPlace = NOT_AN_ITEM) then exit;
    if current = DropPlace then exit;

    // add an item //
    if (current = NOT_AN_ITEM) and (DropPlace <> NOT_AN_ITEM) then
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

    // move an item //
    if (current <> NOT_AN_ITEM) and (DropPlace <> NOT_AN_ITEM) then
    begin
      if DropPlace < current then
      begin
        for i := current downto DropPlace + 1 do items[i].h := items[i - 1].h;
      end;
      if DropPlace > current then
      begin
        for i := current to DropPlace - 1 do items[i].h := items[i + 1].h;
      end;
      items[DropPlace].h := 0;
      ItemsChanged;
      exit
    end;

    // delete an item //
    if (current <> NOT_AN_ITEM) and (DropPlace = NOT_AN_ITEM) then
    begin
      if current < ItemCount - 1 then
      begin
        for i := current to ItemCount - 2 do items[i].h := items[i + 1].h;
      end;
      dec(ItemCount);
      ItemsChanged(true);
    end;
  except
    on e: Exception do err('ItemManager.SetDropPlace', e);
  end;
end;
//------------------------------------------------------------------------------
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
function _ItemManager.IASize: integer; // items area width or height //
begin
  result := ItemCount * (ItemSize + ItemSpacing);
end;
//------------------------------------------------------------------------------
function _ItemManager.ItemFromPoint(Ax, Ay, distance: integer): extended;
var
  mbr: windows.TRect; // monitor bounds rect
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

    mbr := GetMonitorBoundsRect;
    if (BaseSite = 1) or (BaseSite = 3) then
    begin
      BasePoint := mbr.Left + (mbr.Right - mbr.Left - IASize) * sets.container.CenterOffsetPercent div 100;
      result := (Ax - BasePoint) / (ItemSize + ItemSpacing);
    end else begin
      BasePoint := mbr.Top + (mbr.Bottom - mbr.Top - IASize) * sets.container.CenterOffsetPercent div 100;
      result := (Ay - BasePoint) / (ItemSize + ItemSpacing);
    end;
    if result < -1 then result := NOT_AN_ITEM;
    if result >= ItemCount + 1 then result := NOT_AN_ITEM;

    // check boundaries //

    rItemArea := FItemArea;
    if BaseSite = 0 then rItemArea.Right := 0
    else if BaseSite = 1 then rItemArea.Bottom := 0
    else if BaseSite = 2 then rItemArea.Left := 0
    else if BaseSite = 3 then rItemArea.Top := 0;

    if BaseSite = 0 then
    begin
      if (Ax < BaseWindowRect.X - distance) or
        (Ax > BaseWindowRect.X + x + rItemArea.Left + ItemSize + rItemArea.Right + ZoomItemSizeDiff + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = 2 then
    begin
      if (Ax < BaseWindowRect.X + BaseWindowRect.Width - rItemArea.Right - ItemSize - rItemArea.Left - ZoomItemSizeDiff - distance) or
        (Ax > BaseWindowRect.X + BaseWindowRect.Width + distance) or
        (Ay < BasePoint - BigItemSize) or
        (Ay > BasePoint + height)
        then result := NOT_AN_ITEM;
    end else
    if BaseSite = 3 then
    begin
      if (Ay < BaseWindowRect.Y + BaseWindowRect.Height - rItemArea.Bottom - ItemSize - rItemArea.Top - ZoomItemSizeDiff - distance) or
        (Ay > BaseWindowRect.Y + BaseWindowRect.Height + distance) or
        (Ax < BasePoint - BigItemSize) or
        (Ax > BasePoint + width)
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
                saved := ZoomInOutItem - item;
                if FZoomSmoothingLevel = 1 then ZoomInternal(item + saved / 2); // 1 transitional frame
                ZoomInternal(item); // original frame
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
procedure _ItemManager.ZoomInternal(item: extended);
begin
  try
    if not enabled or (ItemCount < 1) then exit;
    ZoomInOutItem := item;
    if ZoomItemSizeDiff = 0 then
    begin
      ResetItemsSize;
      exit;
    end;
    ItemsChanged;
  except
    on e: Exception do err('ItemManager.ZoomInternal', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.UnZoom(do_now: boolean = false);
begin
  if enabled and (Zooming or do_now) then
  begin
    Zooming := false;
    if (ItemCount <= 0) or do_now then ZoomItemSizeDiff := 0;
    if do_now then ResetItemsSize;
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
// 'mouse move' events entry point
procedure _ItemManager.WHMouseMove(pt: windows.Tpoint; allow_zoom: boolean = true);
var
  wnd: cardinal;
begin
  try
    if not enabled or not visible then exit;
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
    on e: Exception do err('ItemManager.WHMouseMove', e);
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
  if not LockMouseEffect then UnZoom;
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
  Unzoom;
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
procedure _ItemManager.DockAdd(HWnd: THandle);
begin
  if (DropPlace >= 0) and (DropPlace < ItemCount) then
  begin
    // if DropPlace is defined - then it's not a TaskItem //
    if DropPlace > ItemCount - TaskItemCount then DropPlace := ItemCount - TaskItemCount;
    items[DropPlace].h := HWnd;
  end else begin
    if IsTask(HWnd) then SetDropPlace(ItemCount) else SetDropPlace(ItemCount - TaskItemCount);
    items[DropPlace].h := HWnd;
    items[DropPlace].s := ItemSize;
  end;
  ItemCmd(HWnd, icFree, 0); // restore item functioning //
  ItemCmd(HWnd, icFloat, 0);
  SetWindowPos(HWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE + SWP_NOSENDCHANGING);
  SetDropPlaceEx(NOT_AN_ITEM);
  SetDropPlace(NOT_AN_ITEM);
  ItemsChanged;
  if zooming then zoominternal(ZoomInOutItem);
end;
//------------------------------------------------------------------------------
procedure _ItemManager.Dock(HWnd: HANDLE);
var
  i: integer;
  DragInst, Inst, NewInst: TCustomItem;
  NewItemHWnd: THandle;
  pt: windows.TPoint;
begin
  if not enabled or (DragHWnd <> HWnd) then exit;
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
        if TStackItem(DragInst).ItemCount > 0 then
        begin
          for i := 0 to TStackItem(DragInst).ItemCount - 1 do
            TStackItem(Inst).AddSubitem(TStackItem(DragInst).SubitemToString(i));
        end;
        DragInst.Delete;
      end else
      // shortcut to shortcut //
      if (DragInst is TShortcutItem) and (Inst is TShortcutItem) then
      begin
        if (DropPlace >= 0) and (DropPlace < ItemCount) then
        begin
          NewItemHWnd := CreateItem(TStackItem.Make(0, UTF8ToAnsi(XStack), ''));
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
    if zooming then zoominternal(ZoomInOutItem);
  except
    on e: Exception do err('ItemManager.DockWindowItem', e);
  end;
end;
//------------------------------------------------------------------------------
// searches all items and their subitems for a given HWnd
// when item with HWnd found - result is HWnd
// when subitem with HWnd found - result is its parent item HWnd
// when no match found - result is 0
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
function _ItemManager.ItemDrop(HWnd: HANDLE; HWndChild: HANDLE; pt: windows.TPoint; filename: string): boolean;
var
  Inst: TCustomItem;
begin
  try
    result := false;
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then result := Inst.DropFile(HWndChild, pt, filename);
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
    if Inst is TCustomItem then result := Inst.GetItemFilename;
  except
    on e: Exception do err('Terry.ItemManager.GetPluginFile', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetPluginImage(HWnd: HANDLE; lpImageNew: Pointer; AutoDelete: boolean);
var
  Inst: TCustomItem;
begin
  try
    Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then Inst.UpdateImage(lpImageNew, AutoDelete);
  except
    on e: Exception do err('Terry.ItemManager.SetPluginImage', e);
  end;
end;
//------------------------------------------------------------------------------
procedure _ItemManager.SetPluginOverlay(HWnd: HANDLE; lpOverlayNew: Pointer; AutoDelete: boolean);
var
  Inst: TCustomItem;
begin
  try
    if HWnd <> 0 then Inst := TCustomItem(GetWindowLong(HWnd, GWL_USERDATA));
    if Inst is TCustomItem then Inst.UpdateOverlay(lpOverlayNew, AutoDelete);
  except
    on e: Exception do err('Terry.ItemManager.SetPluginOverlay', e);
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
    on e: Exception do err('Terry.ItemManager.PluginAnimate', e);
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
    on e: Exception do err('Terry.ItemManager.SetPluginCaption', e);
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
    on e: Exception do err('Terry.ItemManager.GetPluginCaption', e);
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
    on e: Exception do err('Terry.ItemManager.GetPluginRect', e);
  end;
end;
//------------------------------------------------------------------------------
function _ItemManager.IsPluginVisible(HWnd: HANDLE): boolean;
begin
  result := Visible;
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
end.
