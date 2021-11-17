unit frmColorU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, EColor;

type
  _proc = procedure(color: uint); stdcall;

  {TfrmColor}

  TfrmColor = class(TForm)
    btnok: TButton;
    btncancel: TButton;
    edr: TEdit;
    edg: TEdit;
    edb: TEdit;
    edh: TEdit;
    edl: TEdit;
    eds: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblBackgroundAlpha: TLabel;
    rbrgb: TRadioButton;
    rbhls: TRadioButton;
    tbAlpha: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure cbarChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnokClick(Sender: TObject);
    procedure btncancelClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edrgbChange(Sender: TObject);
    procedure edhlsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    cbar: TEColor;
    FColor: uint;
    FFirstColor: uint;
    callback: _proc;
    procedure SetColor(color: uint);
    procedure updrgb;
    procedure updhls;
  public
    class procedure StartForm(color: uint; callback_proc: pointer);
  end;

var
  frmColor: TfrmColor;

{$t+}
implementation
uses gfx, toolu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmColor.StartForm(color: uint; callback_proc: pointer);
begin
  if not assigned(frmColor) then
  begin
    Application.CreateForm(self, frmColor);
    frmColor.cbar := TEColor.Create(TComponent(self));
    with frmColor.cbar do
    begin
      Left := 5;
      Top := 5;
      Width := 261;
      Height := 236;
      Hue := 0;
      Lightness := 0;
      Saturation := 0;
      BorderColor := clBlack;
      VSpace := 8;
      HSpace := 8;
      SplitWidth := 10;
      OnChange := cbarChange;
    end;
  end;
  frmColor.callback := callback_proc;
  frmColor.SetColor(color);
  frmColor.show;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormShow(Sender: TObject);
begin
  font.name := GetFont;
  font.size := GetFontSize;
  clientheight := Height;
  clientwidth := Width;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  callback := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.SetColor(color: uint);
begin
  FFirstColor := color;
  tbAlpha.OnChange := nil;
  tbAlpha.Position := color shr 24;
  tbAlpha.OnChange := cbarChange;
  cbar.OnChange := nil;
  cbar.Color := color;
  cbar.OnChange := cbarChange;
  updrgb;
  updhls;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.cbarChange(Sender: TObject);
begin
  updrgb;
  updhls;
  FColor := tbAlpha.Position shl 24 + cbar.Color and $ffffff;
  if assigned(callback) then callback(FColor);
end;
//------------------------------------------------------------------------------
procedure TfrmColor.btnokClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.btncancelClick(Sender: TObject);
begin
  if assigned(callback) then callback(FFirstColor);
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  releasecapture;
  perform($a1, 2, 0);
end;
//------------------------------------------------------------------------------
procedure TfrmColor.updhls;
begin
  edh.OnChange := nil;
  edl.OnChange := nil;
  eds.OnChange := nil;
  edh.text     := inttostr(cbar.Hue);
  edl.text     := inttostr(cbar.Lightness);
  eds.text     := inttostr(cbar.Saturation);
  edh.OnChange := edhlsChange;
  edl.OnChange := edhlsChange;
  eds.OnChange := edhlsChange;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.updrgb;
begin
  edr.OnChange := nil;
  edg.OnChange := nil;
  edb.OnChange := nil;
  edr.text     := inttostr(cbar.color and $ff);
  edg.text     := inttostr(cbar.color shr 8 and $ff);
  edb.text     := inttostr(cbar.color shr 16 and $ff);
  edr.OnChange := edrgbChange;
  edg.OnChange := edrgbChange;
  edb.OnChange := edrgbChange;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.edrgbChange(Sender: TObject);
begin
  rbrgb.checked := true;
  try
    cbar.OnChange := nil;
    cbar.Color := strtoint(edr.text) + strtoint(edg.text) shl 8 + strtoint(edb.text) shl 16;
    updhls;
    FColor := tbAlpha.Position shl 24 + cbar.Color and $ffffff;
    if assigned(callback) then callback(FColor);
    cbar.OnChange := cbarChange;
  except end;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.edhlsChange(Sender: TObject);
begin
  rbhls.checked := true;
  try
    cbar.OnChange := nil;
    cbar.Hue := strtoint(edh.text);
    cbar.Lightness := strtoint(edl.text);
    cbar.Saturation := strtoint(eds.text);
    updrgb;
    FColor := tbAlpha.Position shl 24 + cbar.Color and $ffffff;
    if assigned(callback) then callback(FColor);
    cbar.OnChange := cbarChange;
  except end;
end;
//------------------------------------------------------------------------------
end.
