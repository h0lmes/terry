unit frmsetsu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, DefaultTranslator,
  Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, Buttons, Math, LCLType, LCLProc,
  declu, toolu, gfx, GDIPAPI, dwm_unit, DividerBevel;

type

  { Tfrmsets }

  Tfrmsets = class(TForm)
    btnAutoRunAdd: TSpeedButton;
    btnAutoRunAddMinimized: TSpeedButton;
    btnBackColor: TButton;
    btnDebug: TButton;
    btnFontBold: TSpeedButton;
    btnFontItalic: TSpeedButton;
    btnRemoveDock: TButton;
    btnRestore: TButton;
    btnRunNow: TButton;
    btn_ok1: TBitBtn;
    btnColor: TButton;
    cbAutoHide: TCheckBox;
		cbShowHint: TCheckBox;
		chbAutoHideOnFullScreenApp: TCheckBox;
    chbGlobalConsole: TCheckBox;
    chbGlobalHide: TCheckBox;
		chbHintEffects: TCheckBox;
    chbReserveScreenEdge: TCheckBox;
    cboItemAnimationType: TComboBox;
    chbStackOpenAnimation: TCheckBox;
    chbRunInThread: TCheckBox;
    chbOccupyFullMonitor: TCheckBox;
    chbTaskbar: TCheckBox;
    chbTaskbarGrouping: TCheckBox;
    chbTaskbarLivePreviews: TCheckBox;
    chbTaskbarSameMonitor: TCheckBox;
    chbUseShellContextMenus: TCheckBox;
    chbReflection: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    edAutoHideTime: TEdit;
    edAutoShowTime: TEdit;
    edFontSize: TEdit;
    edFontSize2: TEdit;
		edActivateOnMouseInterval: TEdit;
    edRolledVisiblePixels: TEdit;
    edStartOffset: TEdit;
    edEndOffset: TEdit;
    hkConsole: TEdit;
    hkHide: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblHideDelay: TLabel;
    lblRemoveDock: TLabel;
    lblBackgroundTransparency1: TLabel;
    lblBackgroundTransparency2: TLabel;
    lblCredits6: TLabel;
    lblCredits7: TLabel;
    lblIconSpacing: TLabel;
    lblBackgroundTransparency: TLabel;
    lblAnimateIcons: TLabel;
    lblCredits: TLabel;
    lblCredits1: TLabel;
    lblCredits2: TLabel;
    lblCredits3: TLabel;
    lblCredits4: TLabel;
    lblCredits5: TLabel;
    lblShowDelay: TLabel;
    lblVisiblePart: TLabel;
    lblZoomTime: TLabel;
    lbTheme: TListBox;
    listFont: TListBox;
    memAutorun: TMemo;
    pages: TPageControl;
    btnAutoRunDel: TSpeedButton;
    pbox: TPaintBox;
    stMoveDockHint: TStaticText;
    tsActions: TTabSheet;
    tbAeroPeekThumbSize: TTrackBar;
    tbBaseAlpha: TTrackBar;
    tbSeparatorAlpha: TTrackBar;
    tbBigIconSize: TTrackBar;
    tbReflectionSize: TTrackBar;
    tbZoomTime: TTrackBar;
    tbReserveScreenEdgePercent: TTrackBar;
    tbIconSpacing: TTrackBar;
    tbZoomWidth: TTrackBar;
    tbIconSize: TTrackBar;
    tsPosition: TTabSheet;
    tsGeneral: TTabSheet;
    tsSystem: TTabSheet;
    tsAbout: TTabSheet;
    lblName: TLabel;
    lblCommand: TLabel;
    lblParams: TLabel;
    lblWindow: TLabel;
    tsIcons: TTabSheet;
    cbZoomItems: TCheckBox;
    lblZoomWideness: TLabel;
    lblIconSize: TLabel;
    cbActivateOnMouse: TCheckBox;
    lblLaunchInterval: TLabel;
    edLaunchInterval: TEdit;
    lblLaunchIntervalTip: TLabel;
    cbUseShell: TCheckBox;
    edShell: TEdit;
    btnBrowseShell: TButton;
    cboBaseSite: TComboBox;
    lblCenterOffsetPercent: TLabel;
    lblEdge: TLabel;
    lblTitle: TLabel;
    tsStyle: TTabSheet;
    cbAutorun: TCheckBox;
    tbCenterOffsetPercent: TTrackBar;
    btn_ok: TBitBtn;
    lblMonitor: TLabel;
    cboMonitor: TComboBox;
    lblZoomedIconSize: TLabel;
    lblEdgeOffset: TLabel;
    tbEdgeOffset: TTrackBar;
    lblMouseOverTip: TLabel;
    cbHideTaskBar: TCheckBox;
    chbBlur: TCheckBox;
    chbShowRunningIndicator: TCheckBox;
    chbActivateRunning: TCheckBox;
    btn_cancel: TBitBtn;
    lv: TListView;
    images: TImageList;
    udFontSize: TUpDown;
    udFontSize2: TUpDown;
    procedure btnAutoRunAddMinimizedClick(Sender: TObject);
    procedure btnBackColorClick(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure btnRemoveDockClick(Sender: TObject);
		procedure btnRestoreClick(Sender: TObject);
    procedure cbAutorunChange(Sender: TObject);
    procedure cbHideTaskBarChange(Sender: TObject);
    procedure cboItemAnimationTypeChange(Sender: TObject);
    procedure chbActivateRunningChange(Sender: TObject);
		procedure chbGlobalConsoleChange(Sender: TObject);
		procedure chbGlobalHideChange(Sender: TObject);
    procedure chbHintEffectsChange(Sender: TObject);
    procedure chbReserveScreenEdgeChange(Sender: TObject);
    procedure chbRunInThreadChange(Sender: TObject);
    procedure chbShowRunningIndicatorChange(Sender: TObject);
    procedure chbStackOpenAnimationChange(Sender: TObject);
    procedure chbTaskbarChange(Sender: TObject);
    procedure chbTaskbarGroupingChange(Sender: TObject);
    procedure chbTaskbarLivePreviewsChange(Sender: TObject);
    procedure chbTaskbarSameMonitorChange(Sender: TObject);
    procedure chbOccupyFullMonitorChange(Sender: TObject);
    procedure chbUseShellContextMenusChange(Sender: TObject);
		procedure edActivateOnMouseIntervalChange(Sender: TObject);
    procedure edEndOffsetChange(Sender: TObject);
    procedure edStartOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_cancelClick(Sender: TObject);
    procedure chbBlurChange(Sender: TObject);
    procedure chbReflectionChange(Sender: TObject);
		procedure hkConsoleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure hkHideKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbThemeDblClick(Sender: TObject);
    procedure lbThemeSelectionChange(Sender: TObject; User: boolean);
    procedure lvCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure pboxPaint(Sender: TObject);
    procedure tbAeroPeekThumbSizeChange(Sender: TObject);
    procedure tbBaseAlphaChange(Sender: TObject);
    procedure tbEdgeOffsetChange(Sender: TObject);
    procedure edAutoShowTimeChange(Sender: TObject);
    procedure cboMonitorChange(Sender: TObject);
    procedure lbl_linkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbAutoHideChange(Sender: TObject);
    procedure edAutoHideTimeChange(Sender: TObject);
    procedure cbUseShellChange(Sender: TObject);
    procedure edShellChange(Sender: TObject);
    procedure btnBrowseShellClick(Sender: TObject);
    procedure cbZoomItemsChange(Sender: TObject);
    procedure edRolledVisiblePixelsChange(Sender: TObject);
    procedure cbActivateOnMouseChange(Sender: TObject);
    procedure lblMailToClick(Sender: TObject);
    procedure tbCenterOffsetPercentChange(Sender: TObject);
    procedure cbShowHintChange(Sender: TObject);
    procedure edLaunchIntervalChange(Sender: TObject);
    procedure cboBaseSiteChange(Sender: TObject);
    procedure tbIconSizeChange(Sender: TObject);
    procedure tbBigIconSizeChange(Sender: TObject);
    procedure tbIconSpacingChange(Sender: TObject);
    procedure tbReflectionSizeChange(Sender: TObject);
    procedure tbReserveScreenEdgePercentChange(Sender: TObject);
    procedure tbSeparatorAlphaChange(Sender: TObject);
    procedure tbZoomTimeChange(Sender: TObject);
    procedure tbZoomWidthChange(Sender: TObject);
    procedure chbAutoHideOnFullScreenAppChange(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memAutorunChange(Sender: TObject);
    procedure btnAutoRunAddClick(Sender: TObject);
  private
    PageIndex: integer;
    AutorunListChanged: boolean;
    FFont: TDFontData;
    function FontPreview: boolean;
    procedure UpdateReserveScreenEdgePercentLabel;
    procedure UpdateLblCenterOffsetPercent;
    procedure UpdateItemSizeLabels;
    procedure ReadAutorun;
    procedure SaveAutorun;
    procedure ApplyFont(Sender: TObject);
  public
    class procedure Open(APageIndex: integer = 0);
  end;

var
  frmsets: Tfrmsets;

implementation
uses setsu, themeu, frmmainu, frmthemeeditoru, frmDebugU;
{$R *.lfm}
//------------------------------------------------------------------------------
procedure err(s: string);
begin
  messagebox(frmmain.handle, pchar(s), PROGRAM_NAME, MB_ICONERROR);
end;
//------------------------------------------------------------------------------
class procedure Tfrmsets.Open(APageIndex: integer);
begin
  if not assigned(sets) then
  begin
    messagebox(application.mainform.handle, 'Settings container does not exist', 'frmSets.Open', mb_iconexclamation);
    exit;
  end;

  sets.StoreSetsContainer;
  try
	    if not sets.Backup then
	    begin
	      AddLog('frmsets.Open.Backup failed');
	      messagebox(application.mainform.handle, pchar(UTF8ToAnsi(XErrorSetsBackupFailed)), PROGRAM_NAME, MB_ICONERROR);
	    end;
  except
    on e: Exception do
    begin
      AddLog('frmsets.Open.Backup exception');
      AddLog(e.message);
      messagebox(application.mainform.handle, pchar(UTF8ToAnsi(e.message)), PROGRAM_NAME, MB_ICONEXCLAMATION);
    end;
  end;
  if not assigned(frmsets) then application.CreateForm(self, frmsets);
  frmsets.PageIndex := APageIndex;
  frmsets.show;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormCreate(Sender: TObject);
begin
  lblTitle.Font.Size := 18;
  lblTitle.Font.Color := clGray;
  lblCredits.Font.Color := clGray;
  lblCredits1.Font.Color := clGray;
  lblCredits2.Font.Color := clGray;
  lblCredits3.Font.Color := clGray;
  lblCredits4.Font.Color := clGray;
  lblCredits5.Font.Color := clGray;
  lblCredits6.Font.Color := clGray;
  lblCredits7.Font.Color := clGray;

  lv.Items[0].Caption := XPageGeneral;
  lv.Items[1].Caption := XPagePosition;
  lv.Items[2].Caption := XPageStyle;
  lv.Items[3].Caption := XPageIcons;
  lv.Items[4].Caption := XPageMisc;
  lv.Items[5].Caption := XPageActions;
  lv.Items[6].Caption := XPageAbout;

  cboBaseSite.Items.Add(XSiteLeft);
  cboBaseSite.Items.Add(XSiteTop);
  cboBaseSite.Items.Add(XSiteRight);
  cboBaseSite.Items.Add(XSiteBottom);

  cboItemAnimationType.Items.Add(XAnimationNoAnimation);
  cboItemAnimationType.Items.Add(XAnimationRotate);
  cboItemAnimationType.Items.Add(XAnimationBounceOnce);
  cboItemAnimationType.Items.Add(XAnimationBounceTwice);
  cboItemAnimationType.Items.Add(XAnimationBounce3Times);
  cboItemAnimationType.Items.Add(XAnimationQuake);
  cboItemAnimationType.Items.Add(XAnimationSwing);
  cboItemAnimationType.Items.Add(XAnimationVibrate);
  cboItemAnimationType.Items.Add(XAnimationZoom);
end;
//------------------------------------------------------------------------------
// preserves OnChange event
procedure SetInitValue(control: TCheckBox; value: boolean); overload;
var
  event: TNotifyEvent;
begin
  event := control.OnChange;
  control.OnChange := nil;
  control.Checked := value;
  control.OnChange := event;
end;
//------------------------------------------------------------------------------
// preserves OnChange event
procedure SetInitValue(control: TEdit; value: string); overload;
var
  event: TNotifyEvent;
begin
  event := control.OnChange;
  control.OnChange := nil;
  control.Text := value;
  control.OnChange := event;
end;
//------------------------------------------------------------------------------
// preserves OnKeyDown event
procedure SetInitValue(control: TEdit; value: TShortCut); overload;
var
  event: TKeyEvent;
begin
  event := control.OnKeyDown;
  control.OnKeyDown := nil;
  control.Text := ShortCutToText(value);
  control.OnKeyDown := event;
end;
//------------------------------------------------------------------------------
// preserves OnChange event
procedure SetInitValue(control: TTrackBar; value: integer); overload;
var
  event: TNotifyEvent;
begin
  event := control.OnChange;
  control.OnChange := nil;
  control.Position := value;
  control.OnChange := event;
end;
//------------------------------------------------------------------------------
// preserves OnChange event
procedure SetInitValue(control: TComboBox; value: integer); overload;
var
  event: TNotifyEvent;
begin
  event := control.OnChange;
  control.OnChange := nil;
  control.ItemIndex := value;
  control.OnChange := event;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormShow(Sender: TObject);
var
  maj, min, rel, build, i, mCount: integer;
  selChange: TSelectionChangeEvent;
begin
  try
    font.name := GetFont;
    font.size := GetFontSize;
    constraints.MinHeight := Height;
    constraints.MaxHeight := Height;
    constraints.MinWidth := Width;
    constraints.MaxWidth := Width;
    lv.ItemIndex := PageIndex;
    toolu.GetFileVersion(paramstr(0), maj, min, rel, build);
    lblTitle.Caption := PROGRAM_NAME + ' ' + inttostr(maj) + '.' + inttostr(min) + '.' + inttostr(rel);
    {$ifdef CPU64}
    lblTitle.Caption := lblTitle.Caption + ' (64 bit)';
    {$else}
    lblTitle.Caption := lblTitle.Caption + ' (32 bit)';
    {$endif}
  except
    on e: Exception do frmmain.err('frmSets.Show', e);
  end;

  //
  // tsGeneral //
  //

  try
    SetInitValue(cbAutorun, toolu.CheckAutoRun);
    SetInitValue(cbHideTaskBar, sets.container.HideSystemTaskbar);
    SetInitValue(chbReserveScreenEdge, sets.container.ReserveScreenEdge);
    SetInitValue(tbReserveScreenEdgePercent, sets.container.ReserveScreenEdgePercent);
    UpdateReserveScreenEdgePercentLabel;
    SetInitValue(cbActivateOnMouse, sets.container.ActivateOnMouse);
    SetInitValue(edActivateOnMouseInterval, inttostr(sets.container.ActivateOnMouseInterval));
    SetInitValue(chbAutoHideOnFullScreenApp, sets.container.AutoHideOnFullScreenApp);
    SetInitValue(chbTaskbar, sets.container.Taskbar);
    SetInitValue(chbTaskbarLivePreviews, sets.container.TaskLivePreviews);
    SetInitValue(chbTaskbarGrouping, sets.container.TaskGrouping);
    SetInitValue(chbTaskbarSameMonitor, sets.container.TaskSameMonitor);
    SetInitValue(tbAeroPeekThumbSize, sets.container.TaskThumbSize);
    SetInitValue(hkHide, TShortCut(sets.container.GlobalHotkeyValue_Hide));
    SetInitValue(hkConsole, TShortCut(sets.container.GlobalHotkeyValue_Console));
    SetInitValue(chbGlobalHide, sets.container.GlobalHotkeyFlag_Hide);
    SetInitValue(chbGlobalConsole, sets.container.GlobalHotkeyFlag_Console);
    SetInitValue(cbShowHint, sets.container.ShowHint);
    SetInitValue(chbHintEffects, sets.container.HintEffects);
  except
    on e: Exception do frmmain.err('frmSets.Show.General', e);
  end;

  //
  // tsPosition //
  //

  try
    cboMonitor.OnChange := nil;
    cboMonitor.clear;
    cboMonitor.Items.AddObject(sets.GetMonitorName(-1), TObject(-1));
    i := 0;
    mCount := sets.GetMonitorCount;
    while i < mCount do
    begin
      cboMonitor.Items.AddObject(sets.GetMonitorName(i), TObject(i));
      inc(i);
    end;
    cboMonitor.ItemIndex := cboMonitor.Items.IndexOfObject(TObject(sets.GetParam(gpMonitor)));
    cboMonitor.OnChange := cboMonitorChange;

    SetInitValue(cboBaseSite, integer(sets.container.site));
    SetInitValue(tbCenterOffsetPercent, sets.container.CenterOffsetPercent);
    SetInitValue(tbEdgeOffset, sets.container.EdgeOffset);
    UpdateLblCenterOffsetPercent;
    SetInitValue(chbOccupyFullMonitor, sets.container.OccupyFullMonitor);
    SetInitValue(edStartOffset, inttostr(sets.container.StartOffset));
    SetInitValue(edEndOffset, inttostr(sets.container.EndOffset));
    SetInitValue(cbAutoHide, sets.container.autohide);
    SetInitValue(edAutoHideTime, inttostr(sets.container.autohidetime));
    SetInitValue(edAutoShowTime, inttostr(sets.container.autoshowtime));
    SetInitValue(edRolledVisiblePixels, inttostr(sets.container.AutoHidePixels));
  except
    on e: Exception do frmmain.err('frmSets.Show.Position', e);
  end;

  //
  // tsStyle
  //

  try
    selChange := lbTheme.OnSelectionChange;
    lbTheme.OnSelectionChange := nil;
    theme.SearchThemes(sets.container.ThemeName, lbTheme);
    lbTheme.OnSelectionChange := selChange;

    chbBlur.Enabled := dwm.IsCompositionEnabled;
    SetInitValue(chbBlur, sets.container.BlurEnabled);
    SetInitValue(tbBaseAlpha, sets.container.BaseAlpha);
    SetInitValue(tbSeparatorAlpha, sets.container.SeparatorAlpha);
    SetInitValue(tbReflectionSize, sets.container.ReflectionSize);

    CopyFontData(sets.container.Font, FFont);
    listFont.OnClick := nil;
    listFont.Items := screen.Fonts;
    listFont.ItemIndex := listFont.items.IndexOf(PChar(@FFont.Name));
    listFont.OnClick := ApplyFont;

    edFontSize.OnChange := nil;
    udFontSize.Position := FFont.size;
    edFontSize.OnChange := ApplyFont;

    edFontSize2.OnChange := nil;
    udFontSize2.Position := FFont.size2;
    edFontSize2.OnChange := ApplyFont;

    btnFontBold.Down := FFont.bold;
    btnFontBold.OnClick := ApplyFont;

    btnFontItalic.Down := FFont.Italic;
    btnFontItalic.OnClick := ApplyFont;
  except
    on e: Exception do frmmain.err('frmSets.Show.Style', e);
  end;

  //
  // tsIcons //
  //

  try
    SetInitValue(tbIconSize, sets.container.itemsize);
    SetInitValue(tbBigIconSize, sets.container.BigItemSize);
    SetInitValue(tbIconSpacing, sets.container.ItemSpacing);
    SetInitValue(tbZoomWidth, sets.container.ZoomWidth div 2);
    SetInitValue(tbZoomTime, sets.container.ZoomTime);
    UpdateItemSizeLabels;
    SetInitValue(cbZoomItems, sets.container.ZoomEnabled);
    SetInitValue(chbReflection, sets.container.ReflectionEnabled);
    SetInitValue(cboItemAnimationType, sets.container.ItemAnimationType);
    SetInitValue(chbUseShellContextMenus, sets.container.UseShellContextMenus);
    SetInitValue(chbActivateRunning, sets.container.ActivateRunningApps);
    SetInitValue(chbShowRunningIndicator, sets.container.ShowRunningIndicator);
    SetInitValue(chbStackOpenAnimation, sets.container.StackAnimationEnabled);
  except
    on e: Exception do frmmain.err('frmSets.Show.Icons', e);
  end;

  //
  // tsSystem //
  //

  try
    SetInitValue(chbRunInThread, sets.container.RunInThread);
    SetInitValue(cbUseShell, sets.container.useshell);
    SetInitValue(edShell, AnsiToUTF8(sets.container.shell));
    SetInitValue(edLaunchInterval, inttostr(sets.container.LaunchInterval));
  except
    on e: Exception do frmmain.err('frmSets.Show.Misc', e);
  end;

  //
  // tsAutorun //
  //

  try ReadAutorun;
  except
    on e: Exception do frmmain.err('frmSets.Show.Autorun', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
  if (key = 112) and (shift = []) then frmmain.execute_cmdline('/help');
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetActiveWindow(frmmain.handle);
  action := cafree;
  frmsets := nil;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btn_okClick(Sender: TObject);
begin
  if AutorunListChanged then SaveAutorun;
  close;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btn_cancelClick(Sender: TObject);
begin
  try
    sets.RestoreSetsContainer;
    theme.setTheme(pchar(sets.container.ThemeName));
    frmmain.ApplyParams;
    close;
  except
    on e: Exception do frmmain.err('frmSets.Cancel', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if lv.ItemIndex > -1 then pages.ActivePageIndex := lv.ItemIndex;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.lvCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  r: windows.TRect;
begin
  DefaultDraw := false;
  r := item.DisplayRect(drIcon);
  images.Draw(sender.canvas, r.Left + (r.Right - r.Left - images.Width) div 2, r.Top, Item.ImageIndex);

  if Item.Selected then
  begin
    sender.canvas.Font.color := clHighlight;
    sender.canvas.Font.Underline := true;
  end;
  r := item.DisplayRect(drLabel);
  r.Left := r.Left + (r.Right - r.Left - sender.Canvas.TextWidth(Item.Caption)) div 2;
  sender.Canvas.TextOut(r.Left, r.Top, Item.Caption);
  sender.canvas.Font.color := sender.Font.color;
  sender.canvas.Font.Underline := sender.Font.Underline;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnDebugClick(Sender: TObject);
begin
  TfrmDebug.Open;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnRemoveDockClick(Sender: TObject);
begin
  if idYes = messagebox(handle, pchar(UTF8ToAnsi(XMsgRemoveDockWarning)), 'Warning', mb_yesno or mb_iconexclamation) then
    frmmain.execute_cmdline('/removedock');
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnRestoreClick(Sender: TObject);
begin
  frmmain.execute_cmdline('/restore');
end;
//------------------------------------------------------------------------------
//
//
//
//  GENERAL
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.cbAutorunChange(Sender: TObject);
begin
  toolu.setautorun(cbAutorun.checked);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbShowHintChange(Sender: TObject);
begin
  frmmain.SetParam(gpShowHint, integer(cbShowHint.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbHintEffectsChange(Sender: TObject);
begin
  frmmain.SetParam(gpHintEffects, integer(chbHintEffects.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboBaseSiteChange(Sender: TObject);
begin
  frmmain.SetParam(gpSite, cboBaseSite.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbAutoHideChange(Sender: TObject);
begin
  frmmain.SetParam(gpAutoHide, integer(cbAutoHide.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edAutoHideTimeChange(Sender: TObject);
var
  value: integer;
begin
  if trystrtoint(edAutoHideTime.Text, value) then frmmain.SetParam(gpAutoHideTime, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edAutoShowTimeChange(Sender: TObject);
var
  value: integer;
begin
  if trystrtoint(edAutoShowTime.Text, value) then frmmain.SetParam(gpAutoShowTime, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edRolledVisiblePixelsChange(Sender: TObject);
var
  value: integer;
begin
  if trystrtoint(edRolledVisiblePixels.Text, value) then frmmain.SetParam(gpAutoHidePixels, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbActivateOnMouseChange(Sender: TObject);
begin
  frmmain.SetParam(gpActivateOnMouse, integer(cbActivateOnMouse.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edActivateOnMouseIntervalChange(Sender: TObject);
var
  value: integer;
begin
  if trystrtoint(edActivateOnMouseInterval.Text, value) then frmmain.SetParam(gpActivateOnMouseInterval, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbCenterOffsetPercentChange(Sender: TObject);
begin
  frmmain.SetParam(gpCenterOffsetPercent, tbCenterOffsetPercent.Position);
  UpdateLblCenterOffsetPercent;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbEdgeOffsetChange(Sender: TObject);
begin
  frmmain.SetParam(gpEdgeOffset, tbEdgeOffset.Position);
  UpdateLblCenterOffsetPercent;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.UpdateLblCenterOffsetPercent;
begin
  lblCenterOffsetPercent.Caption := format(XLabelCenterOffset, [sets.container.CenterOffsetPercent]);
  lblEdgeOffset.Caption := format(XLabelEdgeOffset, [sets.container.EdgeOffset]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.hkHideKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  sets.container.GlobalHotkeyValue_Hide := ShortCut(Key, Shift);
  TEdit(Sender).Text := ShortCutToText(sets.container.GlobalHotkeyValue_Hide);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.hkConsoleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  sets.container.GlobalHotkeyValue_Console := ShortCut(Key, Shift);
  TEdit(Sender).Text := ShortCutToText(sets.container.GlobalHotkeyValue_Console);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbGlobalHideChange(Sender: TObject);
begin
  frmmain.SetParam(gpGlobalHotkeyFlag_Hide, integer(chbGlobalHide.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbGlobalConsoleChange(Sender: TObject);
begin
  frmmain.SetParam(gpGlobalHotkeyFlag_Console, integer(chbGlobalConsole.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbAutoHideOnFullScreenAppChange(Sender: TObject);
begin
  frmmain.SetParam(gpAutoHideOnFullScreenApp, integer(chbAutoHideOnFullScreenApp.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboMonitorChange(Sender: TObject);
begin
  frmmain.SetParam(gpMonitor, integer(cboMonitor.items.Objects[cboMonitor.ItemIndex]));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbHideTaskBarChange(Sender: TObject);
begin
  frmmain.SetParam(gpHideSystemTaskbar, integer(cbHideTaskBar.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbReserveScreenEdgeChange(Sender: TObject);
begin
  frmmain.SetParam(gpReserveScreenEdge, integer(chbReserveScreenEdge.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbReserveScreenEdgePercentChange(Sender: TObject);
begin
  frmmain.SetParam(gpReserveScreenEdgePercent, tbReserveScreenEdgePercent.Position);
  UpdateReserveScreenEdgePercentLabel;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.UpdateReserveScreenEdgePercentLabel;
begin
  chbReserveScreenEdge.Caption := format(XLabelReserveScreenEdgePercent, [sets.container.ReserveScreenEdgePercent]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbTaskbarChange(Sender: TObject);
begin
  frmmain.SetParam(gpTaskbar, integer(chbTaskbar.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbTaskbarGroupingChange(Sender: TObject);
begin
  frmmain.SetParam(gpTaskGrouping, integer(chbTaskbarGrouping.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbTaskbarLivePreviewsChange(Sender: TObject);
begin
  frmmain.SetParam(gpTaskLivePreviews, integer(chbTaskbarLivePreviews.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbTaskbarSameMonitorChange(Sender: TObject);
begin
  frmmain.SetParam(gpTaskSameMonitor, integer(chbTaskbarSameMonitor.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbAeroPeekThumbSizeChange(Sender: TObject);
begin
  frmmain.SetParam(gpTaskThumbSize, tbAeroPeekThumbSize.Position);
end;
//------------------------------------------------------------------------------
//
//
//
//  THEMES
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.lbThemeSelectionChange(Sender: TObject; User: boolean);
begin
  try
    if lbTheme.ItemIndex > -1 then
       frmmain.setTheme(UTF8ToAnsi(lbTheme.Items[lbTheme.ItemIndex]));
  except
    on e: Exception do frmmain.err('frmSets.ThemeSelectionChange', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbOccupyFullMonitorChange(Sender: TObject);
begin
  frmmain.SetParam(gpOccupyFullMonitor, integer(chbOccupyFullMonitor.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edEndOffsetChange(Sender: TObject);
var
  offset: integer;
begin
  if not trystrtoint(edEndOffset.Text, offset) then offset := 0;
  frmmain.SetParam(gpEndOffset, offset);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edStartOffsetChange(Sender: TObject);
var
  offset: integer;
begin
  if not trystrtoint(edStartOffset.Text, offset) then offset := 0;
  frmmain.SetParam(gpStartOffset, offset);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbBlurChange(Sender: TObject);
begin
  frmmain.SetParam(gpBlurEnabled, integer(chbBlur.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbBaseAlphaChange(Sender: TObject);
begin
  frmmain.SetParam(gpBaseAlpha, tbBaseAlpha.Position);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbSeparatorAlphaChange(Sender: TObject);
begin
  frmmain.SetParam(gpSeparatorAlpha, tbSeparatorAlpha.Position);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbReflectionSizeChange(Sender: TObject);
begin
  frmmain.SetParam(gpReflectionSize, tbReflectionSize.Position);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.lbThemeDblClick(Sender: TObject);
begin
  TfrmThemeEditor.Open;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnBackColorClick(Sender: TObject);
begin
  with TColorDialog.Create(self) do
  begin
    Color := gfx.SwapColor(FFont.backcolor and $ffffff);
    if Execute then
    begin
      FFont.backcolor := $ff000000 + gfx.SwapColor(Color and $ffffff);
      ApplyFont(nil);
    end;
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnColorClick(Sender: TObject);
begin
  with TColorDialog.Create(self) do
  begin
    Color := gfx.SwapColor(FFont.color and $ffffff);
    if Execute then
    begin
      FFont.color := $ff000000 + gfx.SwapColor(Color and $ffffff);
      ApplyFont(nil);
    end;
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.pboxPaint(Sender: TObject);
begin
  FontPreview;
end;
//------------------------------------------------------------------------------
function Tfrmsets.FontPreview: boolean;
var
  dst, ffamily, font, format, brush: pointer;
  rect: GDIPAPI.TRectF;
  lf: LOGFONT;
  tm: TTextMetric;
  oldfont, newfont: HFONT;
  isTTF: boolean;
begin
  result := false;
  try
    // check if font is TrueType
    isTTF := false;
    try
      lf.lfCharSet := DEFAULT_CHARSET;
      strlcopy(pchar(@lf.lfFaceName), pchar(@FFont.Name), LF_FACESIZE - 1);
      newfont := CreateFontIndirect(@lf);
      oldfont := SelectObject(canvas.handle, newfont);
      GetTextMetrics(canvas.handle, @tm);
      isTTF := tm.tmPitchAndFamily and TMPF_TRUETYPE <> 0;
      SelectObject(canvas.handle, oldfont);
      DeleteObject(newfont);
    except end;
    if not isTTF then exit;

    // create graphics object
    if Ok = GdipCreateFromHDC(pbox.Canvas.Handle, dst) then
    try
      GdipSetSmoothingMode(dst, SmoothingModeAntiAlias);
      GdipSetTextRenderingHint(dst, TextRenderingHintAntiAlias);
      rect.X := 0;
      rect.Y := 0;
      rect.Width := pbox.Width;
      rect.Height := pbox.Height;
      // draw background
      if Ok = GdipCreateSolidFill(FFont.backcolor, brush) then
      try
        GdipFillRectangle(dst, brush, rect.X, rect.Y, rect.Width, rect.Height);
      finally
        GdipDeleteBrush(brush);
      end;
      // draw font name
      if Ok <> GdipCreateFontFamilyFromName(PWideChar(WideString(PChar(@FFont.Name))), nil, ffamily) then exit;
      try
        if Ok <> GdipCreateFont(ffamily, FFont.size, ifthen(FFont.bold, 1, 0) + ifthen(FFont.italic, 2, 0), 2, font) then exit;
        try
          GdipCreateSolidFill(FFont.color, brush);
          GdipCreateStringFormat(0, 0, format);
          GdipSetStringFormatAlign(format, StringAlignmentCenter);
          GdipSetStringFormatLineAlign(format, StringAlignmentCenter);
          GdipDrawString(dst, PWideChar(WideString(PChar(@FFont.Name))), -1, font, @rect, format, brush);
          GdipDeleteStringFormat(format);
          GdipDeleteBrush(brush);
        finally
          GdipDeleteFont(font);
        end;
      finally
        GdipDeleteFontFamily(ffamily);
      end;
    finally
      GdipDeleteGraphics(dst);
    end;
    result := true;
  except
    on e: Exception do frmmain.err('frmSets.FontPreview', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.ApplyFont(Sender: TObject);
begin
  if listFont.ItemIndex >= 0 then StrCopy(pchar(@FFont.Name), PChar(listFont.Items[listFont.ItemIndex]));
  FFont.size := StrToInt(edFontSize.Text);
  FFont.size2 := StrToInt(edFontSize2.Text);
  FFont.bold := btnFontBold.down;
  FFont.italic := btnFontItalic.down;

  if FontPreview then frmmain.SetFont(FFont);
end;
//------------------------------------------------------------------------------
//
//
//
//  GRAPHICS
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.tbIconSizeChange(Sender: TObject);
begin
  frmmain.SetParam(gpItemSize, tbIconSize.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbBigIconSizeChange(Sender: TObject);
begin
  frmmain.SetParam(gpBigItemSize, tbBigIconSize.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbIconSpacingChange(Sender: TObject);
begin
  frmmain.SetParam(gpItemSpacing, tbIconSpacing.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbZoomWidthChange(Sender: TObject);
begin
  frmmain.SetParam(gpZoomWidth, tbZoomWidth.Position * 2);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbZoomTimeChange(Sender: TObject);
begin
  frmmain.SetParam(gpZoomTime, tbZoomTime.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.UpdateItemSizeLabels;
begin
  lblIconSize.caption := format(XLabelIconSize, [sets.container.itemsize]);
  lblZoomedIconSize.caption := format(XLabelZoomedIconSize, [sets.container.BigItemSize]);
  lblIconSpacing.caption := format(XLabelIconSpacing, [sets.container.ItemSpacing]);
  lblZoomWideness.caption := format(XLabelZoomWidth, [sets.container.ZoomWidth]);
  lblZoomTime.caption := format(XLabelZoomTime, [sets.container.ZoomTime]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbZoomItemsChange(Sender: TObject);
begin
  frmmain.SetParam(gpZoomEnabled, integer(cbZoomItems.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbReflectionChange(Sender: TObject);
begin
  frmmain.SetParam(gpReflectionEnabled, integer(chbReflection.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboItemAnimationTypeChange(Sender: TObject);
begin
  frmmain.SetParam(gpItemAnimationType, cboItemAnimationType.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbStackOpenAnimationChange(Sender: TObject);
begin
  frmmain.SetParam(gpStackAnimationEnabled, integer(chbStackOpenAnimation.Checked));
end;
//------------------------------------------------------------------------------
//
//
//
//  MISC
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.chbRunInThreadChange(Sender: TObject);
begin
  frmmain.SetParam(gpRunInThread, integer(chbRunInThread.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbUseShellChange(Sender: TObject);
begin
  sets.container.UseShell := cbUseShell.Checked;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edShellChange(Sender: TObject);
begin
  edShell.text:= AnsiToUTF8(toolu.ZipPath(UTF8ToAnsi(edShell.text)));
  StrCopy(sets.container.shell, pchar(UTF8ToAnsi(edShell.text)));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnBrowseShellClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    InitialDir := ExtractFilePath(sets.container.shell);
    if execute then edshell.text := AnsiToUTF8(filename);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edLaunchIntervalChange(Sender: TObject);
var
  value: integer;
begin
  if trystrtoint(edLaunchInterval.text, value) then frmmain.SetParam(gpLaunchInterval, value);
end;
//------------------------------------------------------------------------------
//
//
//
//  MENU ITEMS
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.chbActivateRunningChange(Sender: TObject);
begin
  frmmain.SetParam(gpActivateRunning, integer(chbActivateRunning.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbShowRunningIndicatorChange(Sender: TObject);
begin
  frmmain.SetParam(gpShowRunningIndicator, integer(chbShowRunningIndicator.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbUseShellContextMenusChange(Sender: TObject);
begin
  frmmain.SetParam(gpUseShellContextMenus, integer(chbUseShellContextMenus.checked));
end;
//------------------------------------------------------------------------------
//
//
//
//  AUTORUN
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.ReadAutorun;
var
  idx: integer;
begin
  if sets.AutoRunList = nil then exit;
  memAutorun.Lines.BeginUpdate;
  memAutorun.Clear;
  idx := 0;
  while idx < sets.AutoRunList.Count do
  begin
    memAutorun.Lines.Add(AnsiToUTF8(sets.AutoRunList.strings[idx]));
    inc(idx);
  end;
  memAutorun.Lines.EndUpdate;
  AutorunListChanged := false;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.SaveAutorun;
var
  idx: integer;
begin
  if sets.AutoRunList = nil then exit;
  sets.AutoRunList.Clear;
  idx := 0;
  while idx < memAutorun.Lines.Count do
  begin
    sets.AutoRunList.Add(UTF8ToAnsi(memAutorun.Lines.strings[idx]));
    inc(idx);
  end;
  AutorunListChanged := false;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnAutoRunAddClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if execute then memAutorun.Lines.Add('"' + AnsiToUTF8(toolu.ZipPath(FileName)) + '"');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnAutoRunAddMinimizedClick(Sender: TObject);
begin
  with TOpenDialog.Create(self) do
  try
    if execute then memAutorun.Lines.Add('#"' + AnsiToUTF8(toolu.ZipPath(FileName)) + '"');
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.memAutorunChange(Sender: TObject);
begin
  AutorunListChanged := true;
end;
//------------------------------------------------------------------------------
//
//
//
//  ABOUT ///
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.lblMailToClick(Sender: TObject);
begin
  frmmain.Run('mailto:mr_holmes@list.ru?subject=tdock');
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.lbl_linkClick(Sender: TObject);
begin
  frmmain.Run(TLabel(sender).caption);
end;
//------------------------------------------------------------------------------
end.
