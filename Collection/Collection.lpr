program Collection;

{$mode Delphi}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  frmCollectionU,
  gdip_gfx,
  toolu, dropsrcu, EnumFmt;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCollection, frmCollection);
  Application.Run;
end.

