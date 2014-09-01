unit frmColorU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, EColor;

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
    rbrgb: TRadioButton;
    rbhls: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure cbarChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnokClick(Sender: TObject);
    procedure btncancelClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edrgbChange(Sender: TObject);
    procedure edhlsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    cbar: TEColor;
    color: uint;
    first_color: uint;
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
uses gdip_gfx, toolu;
{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmColor.StartForm(color: uint; callback_proc: pointer);
begin
  if not assigned(frmColor) then
  begin
    Application.CreateForm(self, frmColor);
    frmColor.cbar:= TEColor.Create(TComponent(self));
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
  frmColor.callback:= callback_proc;
  frmColor.SetColor(color);
  frmColor.show;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormShow(Sender: TObject);
begin
  font.name:= GetFont;
  font.size:= GetFontSize;
  clientheight:= 247;
  clientwidth:= 380;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  callback:= nil;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.SetColor(color: uint);
begin
  first_color:= color;
  cbar.OnChange:= nil;
  cbar.Color:= color;
  cbar.OnChange:= cbarChange;
  updrgb;
  updhls;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.cbarChange(Sender: TObject);
begin
  updrgb;
  updhls;
  color:= $ff000000 or uint(cbar.Color);
  if assigned(callback) then callback(color);
end;
//------------------------------------------------------------------------------
procedure TfrmColor.btnokClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.btncancelClick(Sender: TObject);
begin
  if assigned(callback) then callback(first_color);
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) and (shift = []) then close;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  releasecapture;
  perform($a1, 2, 0);
end;
//------------------------------------------------------------------------------
procedure TfrmColor.updhls;
begin
  edh.OnChange:= nil;
  edl.OnChange:= nil;
  eds.OnChange:= nil;
  edh.text:= inttostr(cbar.Hue);
  edl.text:= inttostr(cbar.Lightness);
  eds.text:= inttostr(cbar.Saturation);
  edh.OnChange:= edhlsChange;
  edl.OnChange:= edhlsChange;
  eds.OnChange:= edhlsChange;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.updrgb;
begin
  edr.OnChange:= nil;
  edg.OnChange:= nil;
  edb.OnChange:= nil;
  edr.text:= inttostr(cbar.color and $ff);
  edg.text:= inttostr(cbar.color shr 8 and $ff);
  edb.text:= inttostr(cbar.color shr 16 and $ff);
  edr.OnChange:= edrgbChange;
  edg.OnChange:= edrgbChange;
  edb.OnChange:= edrgbChange;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.edrgbChange(Sender: TObject);
begin
  rbrgb.checked:= true;
  try
    cbar.OnChange:= nil;
    cbar.Color:= strtoint(edr.text) + strtoint(edg.text) shl 8 +
      strtoint(edb.text) shl 16;
    updhls;
    color:= $ff000000 or uint(cbar.Color);
    if assigned(callback) then callback(color);
    cbar.OnChange:= cbarChange;
  except end;
end;
//------------------------------------------------------------------------------
procedure TfrmColor.edhlsChange(Sender: TObject);
begin
  rbhls.checked:= true;
  try
    cbar.OnChange:= nil;
    cbar.Hue:= strtoint(edh.text);
    cbar.Lightness:= strtoint(edl.text);
    cbar.Saturation:= strtoint(eds.text);
    updrgb;
    color:= $ff000000 or uint(cbar.Color);
    if assigned(callback) then callback(color);
    cbar.OnChange:= cbarChange;
  except end;
end;
//------------------------------------------------------------------------------
end.
