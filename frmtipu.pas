unit frmtipu;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfrmTip }

  TfrmTip = class(TForm)
    btnEnough: TButton;
    btnNext: TButton;
    btnPrevious: TButton;
    memo: TMemo;
    procedure btnEnoughClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
    class procedure Open;
  end;

var
  frmTip: TfrmTip;

implementation
{$R *.lfm}
uses toolu, setsu, frmmainu;
//------------------------------------------------------------------------------
class procedure TfrmTip.Open;
begin
  Application.CreateForm(TfrmTip, frmTip);
  frmTip.Show;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnEnoughClick(Sender: TObject);
begin
  close;
  frmTip := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnNextClick(Sender: TObject);
begin

end;
//------------------------------------------------------------------------------
procedure TfrmTip.btnPreviousClick(Sender: TObject);
begin

end;
//------------------------------------------------------------------------------
procedure TfrmTip.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;
//------------------------------------------------------------------------------
end.

