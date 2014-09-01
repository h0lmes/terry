unit frmColorData_unit;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  DefaultTranslator, Dialogs, StdCtrls, ComCtrls, gdip_gfx;

type
  TColorCallback = procedure(id, cdata: uint) of object;

  TfrmColorData = class(TForm)
    Label2: TLabel;
    tbHue: TTrackBar;
    tbSat: TTrackBar;
    tbBr: TTrackBar;
    tbCont: TTrackBar;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    btnDefault: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure tbHueChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
  private
    id: uint;
    color_data: uint;
    cancel_color_data: uint;
    callback: TColorCallback;
  public
    class procedure open(id_, cdata: uint; cback: TColorCallback);
  end;

var
  frmColorData: TfrmColorData;

implementation

{$R *.lfm}
//------------------------------------------------------------------------------
class procedure TfrmColorData.open(id_, cdata: uint; cback: TColorCallback);
begin
  if not assigned(frmColorData) then application.CreateForm(self, frmColorData);
  frmColorData.id:= id_;
  frmColorData.color_data:= cdata;
  frmColorData.cancel_color_data:= cdata;
  frmColorData.callback:= cback;
  frmColorData.showmodal;
end;
//------------------------------------------------------------------------------
procedure TfrmColorData.btnOkClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColorData.btnCancelClick(Sender: TObject);
begin
  if assigned(callback) then callback(id, cancel_color_data);
  close;
end;
//------------------------------------------------------------------------------
procedure TfrmColorData.FormShow(Sender: TObject);
begin
  tbHue.OnChange:= nil;
  tbSat.OnChange:= nil;
  tbBr.OnChange:= nil;
  tbCont.OnChange:= nil;
  tbHue.position:= byte(color_data);
  tbSat.position:= byte(color_data shr 8);
  tbBr.position:= byte(color_data shr 16);
  tbCont.position:= byte(color_data shr 24);
  tbHue.OnChange:= tbHueChange;
  tbSat.OnChange:= tbHueChange;
  tbBr.OnChange:= tbHueChange;
  tbCont.OnChange:= tbHueChange;
end;
//------------------------------------------------------------------------------
procedure TfrmColorData.tbHueChange(Sender: TObject);
begin
  color_data:= byte(tbHue.Position) +
    byte(tbSat.Position) shl 8 +
    byte(tbBr.Position) shl 16 +
    byte(tbCont.Position) shl 24;

  if assigned(callback) then callback(id, color_data);
end;
//------------------------------------------------------------------------------
procedure TfrmColorData.btnDefaultClick(Sender: TObject);
begin
  color_data := DEFAULT_COLOR_DATA;

  tbHue.OnChange:= nil;
  tbSat.OnChange:= nil;
  tbBr.OnChange:= nil;
  tbCont.OnChange:= nil;
  tbHue.position:= byte(color_data);
  tbSat.position:= byte(color_data shr 8);
  tbBr.position:= byte(color_data shr 16);
  tbCont.position:= byte(color_data shr 24);
  tbHue.OnChange:= tbHueChange;
  tbSat.OnChange:= tbHueChange;
  tbBr.OnChange:= tbHueChange;
  tbCont.OnChange:= tbHueChange;

  tbHueChange(nil);
end;
//------------------------------------------------------------------------------
end.
