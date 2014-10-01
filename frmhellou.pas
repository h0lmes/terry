unit frmhellou;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmHello }

  TfrmHello = class(TForm)
    btnAdd: TButton;
    btnAutorun: TButton;
    btnClose: TButton;
    btnLearn: TButton;
    btnEnough: TButton;
    btnNext: TButton;
    btnPrevious: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memo: TMemo;
    p1: TPanel;
    p2: TPanel;
    procedure btnAutorunClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEnoughClick(Sender: TObject);
    procedure btnLearnClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
    class procedure Open;
  end;

var
  frmHello: TfrmHello;

implementation
{$R *.lfm}
uses toolu, frmterryu;
//------------------------------------------------------------------------------
class procedure TfrmHello.Open;
begin
  Application.CreateForm(TfrmHello, frmHello);
  frmHello.Show;
  frmHello.p1.BringToFront;
  frmHello.btnAutorun.Enabled := not CheckAutorun;
end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnAutorunClick(Sender: TObject);
begin
  SetAutorun(true);
  btnAutorun.Enabled := not CheckAutorun;
end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnAddClick(Sender: TObject);
begin
  frmterry.execute_cmdline('/apps');
end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnLearnClick(Sender: TObject);
begin
  p2.BringToFront;
end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnEnoughClick(Sender: TObject);
begin
  p1.BringToFront;
end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnNextClick(Sender: TObject);
begin

end;
//------------------------------------------------------------------------------
procedure TfrmHello.btnCloseClick(Sender: TObject);
begin
  close;
  frmHello := nil;
end;
//------------------------------------------------------------------------------
procedure TfrmHello.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;
//------------------------------------------------------------------------------
end.

