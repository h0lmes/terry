unit iitemmgru;

{$mode delphi}

interface
uses Classes, SysUtils;

type
  IItemManager = interface
    procedure ActivateHint(hwnd: cardinal; ACaption: string; x, y: integer);
    procedure DeactivateHint(hwnd: cardinal);
  end;

implementation

end.

