unit frmsetsu;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, DefaultTranslator,
  Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, Buttons, LCLType, LCLProc,
  declu, gdip_gfx, dwm_unit;

type

  { Tfrmsets }

  Tfrmsets = class(TForm)
    btnAutoRunAdd: TSpeedButton;
    btnDebug: TButton;
    btnRunNow: TButton;
    btnSelectHintFont: TBitBtn;
    btnSelectStackFont: TBitBtn;
    btn_ok1: TBitBtn;
    cbShowHint: TCheckBox;
    chbHintEffects: TCheckBox;
    chbReserveScreenEdge: TCheckBox;
    cboItemAnimationType: TComboBox;
    chbTaskbar: TCheckBox;
    gbAutostart: TGroupBox;
    lblCredits6: TLabel;
    lblCredits7: TLabel;
    lblIconSpacing: TLabel;
    lblBackgroundTransparency: TLabel;
    lblAnimateIcons: TLabel;
    Label6: TLabel;
    lblCredits: TLabel;
    lblCredits1: TLabel;
    lblCredits2: TLabel;
    lblCredits3: TLabel;
    lblCredits4: TLabel;
    lblCredits5: TLabel;
    lbTheme: TListBox;
    memAutorun: TMemo;
    pages: TPageControl;
    btnAutoRunDel: TSpeedButton;
    stMoveDockHint: TStaticText;
    tbBaseAlpha: TTrackBar;
    tbBigIconSize: TTrackBar;
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
    cbautorun: TCheckBox;
    tbCenterOffsetPercent: TTrackBar;
    btn_ok: TBitBtn;
    gbHide: TGroupBox;
    cbCtrl: TCheckBox;
    cbAlt: TCheckBox;
    cbShift: TCheckBox;
    hkAutoSlide: TEdit;
    chbAutoHideOnFullScreenApp: TCheckBox;
    lblMonitor: TLabel;
    cbo_monitor: TComboBox;
    btnLayersEditor: TBitBtn;
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
    cbStayOnTop: TCheckBox;
    cbHideTaskBar: TCheckBox;
    chbBlur: TCheckBox;
    chb_reflection: TCheckBox;
    chb_show_running_indicator: TCheckBox;
    chb_activate_running: TCheckBox;
    lblActivateRunningProgramTip: TLabel;
    chb_use_shell_context_menus: TCheckBox;
    btn_cancel: TBitBtn;
    lv: TListView;
    images: TImageList;
    procedure btnDebugClick(Sender: TObject);
    procedure btnSelectStackFontClick(Sender: TObject);
    procedure cboItemAnimationTypeChange(Sender: TObject);
    procedure chbHintEffectsClick(Sender: TObject);
    procedure chbReserveScreenEdgeClick(Sender: TObject);
    procedure chbTaskbarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hkAutoSlideKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btn_cancelClick(Sender: TObject);
    procedure chbBlurClick(Sender: TObject);
    procedure chb_reflectionClick(Sender: TObject);
    procedure chb_show_running_indicatorClick(Sender: TObject);
    procedure chb_activate_runningClick(Sender: TObject);
    procedure lbThemeSelectionChange(Sender: TObject; User: boolean);
    procedure lvCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
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
    procedure cbStayOnTopClick(Sender: TObject);
    procedure tbCenterOffsetPercentChange(Sender: TObject);
    procedure cbShowHintClick(Sender: TObject);
    procedure cbCtrlClick(Sender: TObject);
    procedure edLaunchIntervalChange(Sender: TObject);
    procedure btnSelectHintFontClick(Sender: TObject);
    procedure cboBaseSiteChange(Sender: TObject);
    procedure tbIconSizeChange(Sender: TObject);
    procedure tbBigIconSizeChange(Sender: TObject);
    procedure tbIconSpacingChange(Sender: TObject);
    procedure tbReserveScreenEdgePercentChange(Sender: TObject);
    procedure tbZoomWidthChange(Sender: TObject);
    procedure chbAutoHideOnFullScreenAppClick(Sender: TObject);
    procedure btnLayersEditorClick(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure chb_use_shell_context_menusClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure memAutorunChange(Sender: TObject);
    procedure btnAutoRunAddClick(Sender: TObject);
  private
    AutorunChanged: boolean;
  public
    PageIndex: integer;
    class procedure StartForm(APageIndex: integer = 0);
    procedure UpdateLblCenterOffsetPercent;
    procedure UpdateItemSizeLabels;
    procedure ReadAutorun;
    procedure SaveAutorun;
    procedure HintFontCallback(font: _FontData);
    procedure StackHintFontCallback(font: _FontData);
  end;

var
  frmsets: Tfrmsets;

implementation
uses setsu, themeu, frmterryu, toolu, frmFontU, frmLayersEditorU, frmDebugU;
{$R *.lfm}
//------------------------------------------------------------------------------
procedure err(s: string);
begin
  messagebox(frmterry.handle, pchar(s), 'Terry', MB_ICONERROR);
end;
//------------------------------------------------------------------------------
function ShortCutToTextEx(ShortCut: TShortCut): string;
begin
  case ShortCut and $ff of
    $6A: result := 'Num *';
    $6B: result := 'Num +';
    $6D: result := 'Num -';
    $6E: result := 'Num Del';
    $6F: result := 'Num /';
    else result:= ShortCutToText(ShortCut);
  end;
end;
//------------------------------------------------------------------------------
class procedure Tfrmsets.StartForm(APageIndex: integer);
begin
  if not assigned(sets) then
  begin
    messagebox(application.mainform.handle, 'Settings container does not exist', 'Terry.frmSets.StartForm', mb_iconexclamation);
    exit;
  end;

  sets.StoreSetsContainer;
  if not assigned(frmsets) then application.CreateForm(self, frmsets);
  frmsets.PageIndex := APageIndex;
  frmsets.show;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormCreate(Sender: TObject);
begin
  font.name := GetFont;
  font.size := GetFontSize;
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
  constraints.MinHeight := Height;
  constraints.MaxHeight := Height;
  constraints.MinWidth := Width;
  constraints.MaxWidth := Width;
  lv.ItemIndex := PageIndex;

  toolu.GetFileVersion(paramstr(0), maj, min, rel, build);
  lblTitle.Caption:= 'Terry  ' + inttostr(maj) + '.' + inttostr(min) + '.' + inttostr(rel);

  //
  // поведение //
  //

  cbAutoRun.checked := toolu.CheckAutoRun;

  cbAutoHide.Checked := sets.container.autohide;
  cbActivateOnMouse.Checked := sets.container.ActivateOnMouse;
  edAutoHideTime.Text := inttostr(sets.container.autohidetime);
  edAutoShowTime.Text := inttostr(sets.container.autoshowtime);
  edRolledVisiblePixels.Text := inttostr(sets.container.AutoHidePixels);
  cbStayOnTop.Checked := sets.container.StayOnTop;

  hkAutoSlide.Text := ShortCutToTextEx(sets.container.HideKeys and not (scCtrl + scAlt + scShift));
  cbCtrl.checked := (sets.container.HideKeys and scCtrl <> 0);
  cbAlt.checked := (sets.container.HideKeys and scAlt <> 0);
  cbShift.checked := (sets.container.HideKeys and scShift <> 0);

  chbAutoHideOnFullScreenApp.checked := sets.container.AutoHideOnFullScreenApp;

  cbShowHint.Checked := sets.container.ShowHint;
  chbHintEffects.Checked := sets.container.HintEffects;

  cbHideTaskBar.Checked := sets.container.HideTaskBar;
  chbReserveScreenEdge.Checked := sets.container.ReserveScreenEdge;
  tbReserveScreenEdgePercent.Position := sets.container.ReserveScreenEdgePercent;
  chbTaskbar.Checked := sets.container.Taskbar;

  //
  // расположение //
  //

  cbo_monitor.OnChange := nil;
  cbo_monitor.clear;
  i := 0;
  mCount := sets.GetMonitorCount;
  while i < mCount do
  begin
    cbo_monitor.Items.Add(sets.GetMonitorName(i));
    inc(i);
  end;
  cbo_monitor.ItemIndex := sets.GetParam(gpMonitor);
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

  tbBaseAlpha.OnChange := nil;
  tbBaseAlpha.Position := sets.container.BaseAlpha;
  tbBaseAlpha.OnChange := tbBaseAlphaChange;

  //
  // тема
  //

  lbTheme.OnSelectionChange := nil;
  theme.SearchThemes(sets.container.ThemeName, lbTheme);
  lbTheme.OnSelectionChange := lbThemeSelectionChange;
  chbBlur.Enabled := dwm.CompositingEnabled;
  chbBlur.OnClick := nil;
  chbBlur.checked := sets.container.Blur;
  chbBlur.OnClick := chbBlurClick;

  //
  // icons //
  //

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

  UpdateItemSizeLabels;

  cbZoomItems.OnClick := nil;
  cbZoomItems.checked := sets.container.ZoomItems;
  cbZoomItems.OnClick := cbZoomItemsClick;
  chb_reflection.OnClick := nil;
  chb_reflection.checked := sets.container.Reflection;
  chb_reflection.OnClick := chb_reflectionClick;

  cboItemAnimationType.OnChange := nil;
  cboItemAnimationType.ItemIndex := sets.container.ItemAnimation;
  cboItemAnimationType.OnChange := cboItemAnimationTypeChange;

  chb_use_shell_context_menus.checked := sets.container.UseShellContextMenus;
  chb_activate_running.checked := sets.container.ActivateRunning;
  chb_show_running_indicator.checked := sets.container.ShowRunningIndicator;

  //
  // system //
  //

  cbUseShell.checked := sets.container.useshell;
  edShell.text := AnsiToUTF8(sets.container.shell);
  edLaunchInterval.OnChange := nil;
  edLaunchInterval.Text := inttostr(sets.container.LaunchInterval);
  edLaunchInterval.OnChange := edLaunchIntervalChange;

  // autorun //

  ReadAutorun;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
  if (key = 112) and (shift = []) then frmterry.Help;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetActiveWindow(frmterry.handle);
  action := cahide;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btn_okClick(Sender: TObject);
begin
  if AutorunChanged then SaveAutorun;
  close;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btn_cancelClick(Sender: TObject);
begin
  try
    sets.RestoreSetsContainer;
    theme.setTheme(pchar(sets.container.ThemeName));
    frmterry.BaseCmd(tcApplyParams, 0);
    close;
  except
    on e: Exception do frmterry.err('frmSets.Cancel', e);
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
//
//
//
//  GENERAL
//
//
//
//------------------------------------------------------------------------------
procedure Tfrmsets.cbautorunClick(Sender: TObject);
begin
  toolu.setautorun(cbautorun.checked);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboBaseSiteChange(Sender: TObject);
begin
  frmterry.SetParam(gpSite, cboBaseSite.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbAutoHideClick(Sender: TObject);
begin
  frmterry.SetParam(gpAutoHide, integer(cbAutoHide.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edAutoHideTimeChange(Sender: TObject);
var
  value: integer;
begin
  try value:= strtoint(edAutoHideTime.Text);
  except exit;
  end;
  frmterry.SetParam(gpAutoHideTime, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edAutoShowTimeChange(Sender: TObject);
var
  value: integer;
begin
  try value:= strtoint(edAutoShowTime.Text);
  except exit;
  end;
  frmterry.SetParam(gpAutoShowTime, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edRolledVisiblePixelsChange(Sender: TObject);
var
  value: integer;
begin
  try value:= strtoint(edRolledVisiblePixels.Text);
  except value:= 2;
  end;
  frmterry.SetParam(gpRolledVisiblePixels, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbActivateOnMouseClick(Sender: TObject);
begin
  frmterry.SetParam(gpActivateOnMouse, integer(cbActivateOnMouse.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbStayOnTopClick(Sender: TObject);
begin
  frmterry.SetParam(gpStayOnTop, integer(cbStayOnTop.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbCenterOffsetPercentChange(Sender: TObject);
begin
  frmterry.SetParam(gpCenterOffsetPercent, tbCenterOffsetPercent.Position);
  UpdateLblCenterOffsetPercent;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbEdgeOffsetChange(Sender: TObject);
begin
  frmterry.SetParam(gpEdgeOffset, tbEdgeOffset.Position);
  UpdateLblCenterOffsetPercent;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.UpdateLblCenterOffsetPercent;
begin
  lblCenterOffsetPercent.Caption := format(XLabelCenterOffset, [sets.container.CenterOffsetPercent]);
  lblEdgeOffset.Caption := format(XLabelEdgeOffset, [sets.container.EdgeOffset]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbCtrlClick(Sender: TObject);
begin
  if cbCtrl.checked then sets.container.HideKeys := sets.container.HideKeys or scCtrl
  else sets.container.HideKeys := sets.container.HideKeys and not scCtrl;
  if cbAlt.checked then sets.container.HideKeys := sets.container.HideKeys or scAlt
  else sets.container.HideKeys := sets.container.HideKeys and not scAlt;
  if cbShift.checked then sets.container.HideKeys := sets.container.HideKeys or scShift
  else sets.container.HideKeys := sets.container.HideKeys and not scShift;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.hkAutoSlideKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  sets.container.HideKeys := sets.container.HideKeys and $ffffff00 + Key;
  TEdit(Sender).Text := ShortCutToTextEx(ShortCut(Key, Shift));
  Key := 0;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbAutoHideOnFullScreenAppClick(Sender: TObject);
begin
  frmterry.SetParam(gpAutoHideOnFullScreenApp, integer(chbAutoHideOnFullScreenApp.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbo_monitorChange(Sender: TObject);
begin
  frmterry.SetParam(gpMonitor, cbo_monitor.ItemIndex);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbHideTaskBarClick(Sender: TObject);
begin
  frmterry.SetParam(gpHideTaskBar, integer(cbHideTaskBar.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbReserveScreenEdgeClick(Sender: TObject);
begin
  frmterry.SetParam(gpReserveScreenEdge, integer(chbReserveScreenEdge.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbReserveScreenEdgePercentChange(Sender: TObject);
begin
  frmterry.SetParam(gpReserveScreenEdgePercent, tbReserveScreenEdgePercent.Position);
  chbReserveScreenEdge.Caption := format(XLabelReserveScreenEdgePercent, [sets.container.ReserveScreenEdgePercent]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbTaskbarClick(Sender: TObject);
begin
  frmterry.SetParam(gpTaskbar, integer(chbTaskbar.Checked));
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
       theme.setTheme(UTF8ToAnsi(lbTheme.Items[lbTheme.ItemIndex]));
  except
    on e: Exception do frmterry.err('Sets.ThemeSelectionChange', e);
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnLayersEditorClick(Sender: TObject);
begin
  TfrmLayersEditor.StartForm;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbBlurClick(Sender: TObject);
begin
  frmterry.SetParam(gpBlur, integer(chbBlur.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbBaseAlphaChange(Sender: TObject);
begin
  frmterry.SetParam(gpBaseAlpha, tbBaseAlpha.Position);
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
  frmterry.SetParam(gpItemSize, tbIconSize.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbBigIconSizeChange(Sender: TObject);
begin
  frmterry.SetParam(gpBigItemSize, tbBigIconSize.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbIconSpacingChange(Sender: TObject);
begin
  frmterry.SetParam(gpItemSpacing, tbIconSpacing.Position);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.tbZoomWidthChange(Sender: TObject);
begin
  frmterry.SetParam(gpZoomWidth, tbZoomWidth.Position * 2);
  UpdateItemSizeLabels;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.UpdateItemSizeLabels;
begin
  lblIconSize.caption := format(XLabelIconSize, [sets.container.itemsize]);
  lblZoomedIconSize.caption := format(XLabelZoomedIconSize, [sets.container.BigItemSize]);
  lblIconSpacing.caption := format(XLabelIconSpacing, [sets.container.ItemSpacing]);
  lblZoomWideness.caption := format(XLabelZoomWidth, [sets.container.ZoomWidth]);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cbZoomItemsClick(Sender: TObject);
begin
  frmterry.SetParam(gpZoomItems, integer(cbZoomItems.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chb_reflectionClick(Sender: TObject);
begin
  frmterry.SetParam(gpReflection, integer(chb_reflection.checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.cboItemAnimationTypeChange(Sender: TObject);
begin
  frmterry.SetParam(gpItemAnimation, cboItemAnimationType.ItemIndex);
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
procedure Tfrmsets.cbUseShellClick(Sender: TObject);
begin
  sets.container.UseShell:= cbUseShell.Checked;
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
    InitialDir:= ExtractFilePath(sets.container.shell);
    if execute then
      edshell.text:= AnsiToUTF8(filename);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.edLaunchIntervalChange(Sender: TObject);
var
  value: integer;
begin
  try value:= strtoint(edLaunchInterval.text);
  except value:= 0;
  end;
  frmterry.SetParam(gpLaunchInterval, value);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chb_activate_runningClick(Sender: TObject);
begin
  frmterry.SetParam(gpActivateRunning, integer(chb_activate_running.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chb_show_running_indicatorClick(Sender: TObject);
begin
  frmterry.SetParam(gpShowRunningIndicator, integer(chb_show_running_indicator.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chb_use_shell_context_menusClick(Sender: TObject);
begin
  frmterry.SetParam(gpUseShellContextMenus, integer(chb_use_shell_context_menus.checked));
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
procedure Tfrmsets.cbShowHintClick(Sender: TObject);
begin
  frmterry.SetParam(gpShowHint, integer(cbShowHint.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.chbHintEffectsClick(Sender: TObject);
begin
  frmterry.SetParam(gpHintEffects, integer(chbHintEffects.Checked));
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnSelectHintFontClick(Sender: TObject);
begin
  TfrmFont.StartForm(sets.container.font, HintFontCallback);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.HintFontCallback(font: _FontData);
begin
  frmterry.SetFont(font);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.btnSelectStackFontClick(Sender: TObject);
begin
  TfrmFont.StartForm(sets.container.StackFont, StackHintFontCallback);
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.StackHintFontCallback(font: _FontData);
begin
  frmterry.SetStackFont(font);
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
  i: integer;
begin
  if sets.AutoRunList = nil then exit;
  memAutorun.Lines.BeginUpdate;
  memAutorun.Clear;
  i := 0;
  while i < sets.AutoRunList.Count do
  begin
    memAutorun.Lines.Add(AnsiToUTF8(sets.AutoRunList.strings[i]));
    inc(i);
  end;
  memAutorun.Lines.EndUpdate;
  AutorunChanged := false;
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.SaveAutorun;
var
  i: integer;
begin
  if sets.AutoRunList = nil then exit;
  sets.AutoRunList.Clear;
  i := 0;
  while i < memAutorun.Lines.Count do
  begin
    sets.AutoRunList.Add(UTF8ToAnsi(memAutorun.Lines.strings[i]));
    inc(i);
  end;
  AutorunChanged := false;
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
procedure Tfrmsets.memAutorunChange(Sender: TObject);
begin
  AutorunChanged := true;
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
  frmterry.Run('mailto:roman.holmes@gmail.com?subject=terry');
end;
//------------------------------------------------------------------------------
procedure Tfrmsets.lbl_linkClick(Sender: TObject);
begin
  frmterry.Run(TLabel(sender).caption);
end;
//------------------------------------------------------------------------------
end.
