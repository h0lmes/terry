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
		cbShowHint: TCheckBox;
		chbAutoHideOnFullScreenApp: TCheckBox;
		chbGlobalConsole: TCheckBox;
		chbHintEffects: TCheckBox;
    chbReserveScreenEdge: TCheckBox;
    cboItemAnimationType: TComboBox;
    chbStackOpenAnimation: TCheckBox;
    chbRunInThread: TCheckBox;
    chbTaskbar: TCheckBox;
    chbTaskbarLivePreviews: TCheckBox;
    chbTaskbarGrouping: TCheckBox;
    chbTaskbarSameMonitor: TCheckBox;
    chbOccupyFullMonitor: TCheckBox;
    chbUseShellContextMenus: TCheckBox;
    chb_reflection: TCheckBox;
		chbGlobalHide: TCheckBox;
    DividerBevel1: TDividerBevel;
    edFontSize: TEdit;
    edFontSize2: TEdit;
		edActivateOnMouseInterval: TEdit;
    edStartOffset: TEdit;
    edEndOffset: TEdit;
    gbAutostart: TGroupBox;
    gbTaskbar: TGroupBox;
		hkConsole: TEdit;
    Label1: TLabel;
    Label2: TLabel;
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
    lblZoomTime: TLabel;
    lbTheme: TListBox;
    listFont: TListBox;
    memAutorun: TMemo;
    pages: TPageControl;
    btnAutoRunDel: TSpeedButton;
		panelRemove: TPanel;
    pbox: TPaintBox;
    stMoveDockHint: TStaticText;
    tbBaseAlpha: TTrackBar;
    tbSeparatorAlpha: TTrackBar;
    tbBigIconSize: TTrackBar;
    tbAeroPeekThumbSize: TTrackBar;
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
    gbHide: TGroupBox;
    hkHide: TEdit;
    lblMonitor: TLabel;
    cbo_monitor: TComboBox;
    lblZoomedIconSize: TLabel;
    lblEdgeOffset: TLabel;
    tbEdgeOffset: TTrackBar;
    gbAutoSlide: TGroupBox;
    lblHideDelay: TLabel;
    lblVisiblePart: TLabel;
    lblShowDelay: TLabel;
    cbAutoHide: TCheckBox;
    edRolledVisiblePixels: TEdit;
    edAutoHideTime: TEdit;
    edAutoShowTime: TEdit;
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
    procedure cboItemAnimationTypeChange(Sender: TObject);
    procedure chbActivateRunningChange(Sender: TObject);
		procedure chbGlobalConsoleClick(Sender: TObject);
		procedure chbGlobalHideClick(Sender: TObject);
    procedure chbHintEffectsClick(Sender: TObject);
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
    procedure chbBlurClick(Sender: TObject);
    procedure chb_reflectionClick(Sender: TObject);
    procedure chbShowRunningIndicatorClick(Sender: TObject);
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
    procedure cbo_monitorChange(Sender: TObject);
    procedure lbl_linkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbAutoHideClick(Sender: TObject);
    procedure edAutoHideTimeChange(Sender: TObject);
    procedure cbUseShellClick(Sender: TObject);
    procedure edShellChange(Sender: TObject);
    procedure btnBrowseShellClick(Sender: TObject);
    procedure cbautorunClick(Sender: TObject);
    procedure cbZoomItemsClick(Sender: TObject);
    procedure edRolledVisiblePixelsChange(Sender: TObject);
    procedure cbActivateOnMouseClick(Sender: TObject);
    procedure lblMailToClick(Sender: TObject);
    procedure cbHideTaskBarClick(Sender: TObject);
    procedure tbCenterOffsetPercentChange(Sender: TObject);
    procedure cbShowHintClick(Sender: TObject);
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
    procedure chbAutoHideOnFullScreenAppClick(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure chbUseShellContextMenusClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memAutorunChange(Sender: TObject);
    procedure btnAutoRunAddClick(Sender: TObject);
  private
    AutorunListChanged: boolean;
    FFont: _FontData;
    function FontPreview: boolean;
  public
    PageIndex: integer;
    class procedure Open(APageIndex: integer = 0);
    procedure UpdateLblCenterOffsetPercent;
    procedure UpdateItemSizeLabels;
    procedure ReadAutorun;
    procedure SaveAutorun;
    procedure ApplyFont(Sender: TObject);
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
{
insert the following into function ShortCutToText of unit LCLProc
$6A: Name := 'Num *';
$6B: Name := 'Num +';
$6D: Name := 'Num -';
$6E: Name := 'Num Del';
$6F: Name := 'Num /';
$C0: Name := '`';
}
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
  lv.Items[5].Caption := XPageAbout;

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
procedure Tfrmsets.FormShow(Sender: TObject);
var
  maj, min, rel, build, i, mCount: integer;
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
    lblTitle.Caption:= PROGRAM_NAME + '  ' + inttostr(maj) + '.' + inttostr(min) + '.' + inttostr(rel);
  except
    on e: Exception do frmmain.err('frmSets.Show.1', e);
  end;
  //
  // поведение //
  //
  try
    cbAutorun.OnChange := nil;
    cbAutorun.Checked := toolu.CheckAutoRun;
    cbAutorun.OnChange := cbAutorunChange;

    cbAutoHide.Checked := sets.container.autohide;

    cbActivateOnMouse.Checked := sets.container.ActivateOnMouse;

    edActivateOnMouseInterval.OnChange := nil;
    edActivateOnMouseInterval.Text := inttostr(sets.container.ActivateOnMouseInterval);
    edActivateOnMouseInterval.OnChange := edActivateOnMouseIntervalChange;

    edAutoHideTime.Text := inttostr(sets.container.autohidetime);

    edAutoShowTime.Text := inttostr(sets.container.autoshowtime);

    edRolledVisiblePixels.Text := inttostr(sets.container.AutoHidePixels);

    hkHide.Text := ShortCutToText(sets.container.GlobalHotkeyValue_Hide);

    hkConsole.Text := ShortCutToText(sets.container.GlobalHotkeyValue_Console);

    chbGlobalHide.Checked := sets.container.GlobalHotkeyFlag_Hide;

    chbGlobalConsole.Checked := sets.container.GlobalHotkeyFlag_Console;

    chbAutoHideOnFullScreenApp.checked := sets.container.AutoHideOnFullScreenApp;

    cbShowHint.Checked := sets.container.ShowHint;

    chbHintEffects.Checked := sets.container.HintEffects;

    cbHideTaskBar.Checked := sets.container.HideSystemTaskbar;

    chbReserveScreenEdge.OnChange := nil;
    chbReserveScreenEdge.Checked := sets.container.ReserveScreenEdge;
    chbReserveScreenEdge.OnChange := chbReserveScreenEdgeChange;

    tbReserveScreenEdgePercent.OnChange := nil;
    tbReserveScreenEdgePercent.Position := sets.container.ReserveScreenEdgePercent;
    tbReserveScreenEdgePercent.OnChange := tbReserveScreenEdgePercentChange;

    chbTaskbar.OnChange := nil;
    chbTaskbar.Checked := sets.container.Taskbar;
    chbTaskbar.OnChange := chbTaskbarChange;

    chbTaskbarLivePreviews.OnChange := nil;
    chbTaskbarLivePreviews.Checked := sets.container.TaskLivePreviews;
    chbTaskbarLivePreviews.OnChange := chbTaskbarLivePreviewsChange;

    chbTaskbarGrouping.OnChange := nil;
    chbTaskbarGrouping.Checked := sets.container.TaskGrouping;
    chbTaskbarGrouping.OnChange := chbTaskbarGroupingChange;

    chbTaskbarSameMonitor.OnChange := nil;
    chbTaskbarSameMonitor.Checked := sets.container.TaskSameMonitor;
    chbTaskbarSameMonitor.OnChange := chbTaskbarSameMonitorChange;

    tbAeroPeekThumbSize.OnChange := nil;
    tbAeroPeekThumbSize.Position := sets.container.TaskThumbSize;
    tbAeroPeekThumbSize.OnChange := tbAeroPeekThumbSizeChange;
  except
    on e: Exception do frmmain.err('frmSets.Show.2', e);
  end;
  //
  // расположение //
  //
  try
    cbo_monitor.OnChange := nil;
    cbo_monitor.clear;
    cbo_monitor.Items.AddObject(sets.GetMonitorName(-1), TObject(-1));
    i := 0;
    mCount := sets.GetMonitorCount;
    while i < mCount do
    begin
      cbo_monitor.Items.AddObject(sets.GetMonitorName(i), TObject(i));
      inc(i);
    end;
    cbo_monitor.ItemIndex := cbo_monitor.Items.IndexOfObject(TObject(sets.GetParam(gpMonitor)));
    cbo_monitor.OnChange := cbo_monitorChange;


    cboBaseSite.OnChange := nil;
    cboBaseSite.ItemIndex := integer(sets.container.site);
    cboBaseSite.OnChange := cboBaseSiteChange;

    tbCenterOffsetPercent.OnChange := nil;
    tbCenterOffsetPercent.Position := sets.container.CenterOffsetPercent;
    tbCenterOffsetPercent.OnChange := tbCenterOffsetPercentChange;

    tbEdgeOffset.OnChange := nil;
    tbEdgeOffset.Position := sets.container.EdgeOffset;
    tbEdgeOffset.OnChange := tbEdgeOffsetChange;
    UpdateLblCenterOffsetPercent;

    chbOccupyFullMonitor.OnChange := nil;
    chbOccupyFullMonitor.Checked := sets.container.OccupyFullMonitor;
    chbOccupyFullMonitor.OnChange := chbOccupyFullMonitorChange;

    edStartOffset.OnChange := nil;
    edStartOffset.Text := inttostr(sets.container.StartOffset);
    edStartOffset.OnChange := edStartOffsetChange;

    edEndOffset.OnChange := nil;
    edEndOffset.Text := inttostr(sets.container.EndOffset);
    edEndOffset.OnChange := edEndOffsetChange;
  except
    on e: Exception do frmmain.err('frmSets.Show.3', e);
  end;
  //
  // тема
  //
  try
    lbTheme.OnSelectionChange := nil;
    theme.SearchThemes(sets.container.ThemeName, lbTheme);
    lbTheme.OnSelectionChange := lbThemeSelectionChange;

    chbBlur.OnClick := nil;
    chbBlur.Enabled := dwm.IsCompositionEnabled;
    chbBlur.checked := sets.container.BlurEnabled;
    chbBlur.OnClick := chbBlurClick;

    tbBaseAlpha.OnChange := nil;
    tbBaseAlpha.Position := sets.container.BaseAlpha;
    tbBaseAlpha.OnChange := tbBaseAlphaChange;

    tbSeparatorAlpha.OnChange := nil;
    tbSeparatorAlpha.Position := sets.container.SeparatorAlpha;
    tbSeparatorAlpha.OnChange := tbSeparatorAlphaChange;

    tbReflectionSize.OnChange := nil;
    tbReflectionSize.Position := sets.container.ReflectionSize;
    tbReflectionSize.OnChange := tbReflectionSizeChange;

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

    btnFontBold.down := FFont.bold;
    btnFontBold.OnClick := ApplyFont;

    btnFontItalic.down := FFont.Italic;
    btnFontItalic.OnClick := ApplyFont;
  except
    on e: Exception do frmmain.err('frmSets.Show.4', e);
  end;
  //
  // icons //
  //
  try
    tbIconSize.OnChange := nil;
    tbIconSize.Position := sets.container.itemsize;
    tbIconSize.OnChange := tbIconSizeChange;

    tbBigIconSize.OnChange := nil;
    tbBigIconSize.Position := sets.container.BigItemSize;
    tbBigIconSize.OnChange := tbBigIconSizeChange;

    tbIconSpacing.OnChange := nil;
    tbIconSpacing.Position := sets.container.ItemSpacing;
    tbIconSpacing.OnChange := tbIconSpacingChange;

    tbZoomWidth.OnChange := nil;
    tbZoomWidth.Position := sets.container.ZoomWidth div 2;
    tbZoomWidth.OnChange := tbZoomWidthChange;

    tbZoomTime.OnChange := nil;
    tbZoomTime.Position := sets.container.ZoomTime;
    tbZoomTime.OnChange := tbZoomTimeChange;

    UpdateItemSizeLabels;

    cbZoomItems.OnClick := nil;
    cbZoomItems.checked := sets.container.ZoomEnabled;
    cbZoomItems.OnClick := cbZoomItemsClick;

    chb_reflection.OnClick := nil;
    chb_reflection.checked := sets.container.ReflectionEnabled;
    chb_reflection.OnClick := chb_reflectionClick;

    cboItemAnimationType.OnChange := nil;
    cboItemAnimationType.ItemIndex := sets.container.ItemAnimationType;
    cboItemAnimationType.OnChange := cboItemAnimationTypeChange;

    chbUseShellContextMenus.OnChange := nil;
    chbUseShellContextMenus.Checked := sets.container.UseShellContextMenus;
    chbUseShellContextMenus.OnChange := chbUseShellContextMenusChange;

    chbActivateRunning.OnChange := nil;
    chbActivateRunning.Checked := sets.container.ActivateRunningApps;
    chbActivateRunning.OnChange := chbActivateRunningChange;

    chbShowRunningIndicator.OnChange := nil;
    chbShowRunningIndicator.Checked := sets.container.ShowRunningIndicator;
    chbShowRunningIndicator.OnChange := chbShowRunningIndicatorChange;

    chbStackOpenAnimation.OnChange := nil;
    chbStackOpenAnimation.Checked := sets.container.StackAnimationEnabled;
    chbStackOpenAnimation.OnChange := chbStackOpenAnimationChange;
  except
    on e: Exception do frmmain.err('frmSets.Show.5', e);
  end;
  //
  // system //
  //
  try
    chbRunInThread.OnChange := nil;
    chbRunInThread.Checked := sets.container.RunInThread;
    chbRunInThread.OnChange := chbRunInThreadChange;

    cbUseShell.checked := sets.container.useshell;

    edShell.text := AnsiToUTF8(sets.container.shell);

    edLaunchInterval.OnChange := nil;
    edLaunchInterval.Text := inttostr(sets.container.LaunchInterval);
    edLaunchInterval.OnChange := edLaunchIntervalChange;
  except
    on e: Exception do frmmain.err('frmSets.Show.6', e);
  end;
  // autorun //
  try
    ReadAutorun;
  except
    on e: Exception do frmmain.err('frmSets.Show.7', e);
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

procedure Tfrmsets.chbUseShellContextMenusClick(Sender: TObject);
begin

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
procedure Tfrmsets.cbShowHintClick(Sender: TObject);
begin
  frmmain.SetParam(gpShowHint, integer(cbShowHint.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbHintEffectsClick(Sender: TObject);
begin
  frmmain.SetParam(gpHintEffects, integer(chbHintEffects.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboBaseSiteChange(Sender: TObject);
begin
  frmmain.SetParam(gpSite, cboBaseSite.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbAutoHideClick(Sender: TObject);
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
procedure Tfrmsets.cbActivateOnMouseClick(Sender: TObject);
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
procedure Tfrmsets.chbGlobalHideClick(Sender: TObject);
begin
  frmmain.SetParam(gpGlobalHotkeyFlag_Hide, integer(chbGlobalHide.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbGlobalConsoleClick(Sender: TObject);
begin
  frmmain.SetParam(gpGlobalHotkeyFlag_Console, integer(chbGlobalConsole.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbAutoHideOnFullScreenAppClick(Sender: TObject);
begin
  frmmain.SetParam(gpAutoHideOnFullScreenApp, integer(chbAutoHideOnFullScreenApp.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbo_monitorChange(Sender: TObject);
begin
  frmmain.SetParam(gpMonitor, integer(cbo_monitor.items.Objects[cbo_monitor.ItemIndex]));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbHideTaskBarClick(Sender: TObject);
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
procedure Tfrmsets.chbBlurClick(Sender: TObject);
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
procedure Tfrmsets.cbZoomItemsClick(Sender: TObject);
begin
  frmmain.SetParam(gpZoomEnabled, integer(cbZoomItems.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chb_reflectionClick(Sender: TObject);
begin
  frmmain.SetParam(gpReflectionEnabled, integer(chb_reflection.checked));
end;

procedure Tfrmsets.chbShowRunningIndicatorClick(Sender: TObject);
begin

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
procedure Tfrmsets.cbUseShellClick(Sender: TObject);
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

procedure Tfrmsets.cbautorunClick(Sender: TObject);
begin

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
