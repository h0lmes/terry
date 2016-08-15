//------------------------------------------------------------------------------
//
//
//
// This unit incorporates functionality to interact with
// windows network manager and handle network related events
//
//
//
//------------------------------------------------------------------------------

unit networksu;

{$mode delphi}
{$undef EXT_DEBUG}

interface
uses Windows, Classes, SysUtils, ActiveX, iphlpapi2, networklist_tlb, toolu;

type

  { TNetworks }

  TNetworks = class(TInterfacedObject, INetworkEvents)
  private
    FReady: boolean;
    FNLM: INetworkListManager;
    dwCookie1: dword;
    FWired: boolean;
    FWireless: boolean;
    FConnected: boolean;
    FInternet: boolean;
    FConnections: integer;
    FDescription: string;
    FAdapterTypes: TStrings;
    procedure Reset;
    procedure ReadNetworks;
    procedure GetAdapterType(AdapterID: string; var wired, wireless: boolean);
    procedure ReadInterfaces;
    function getState: integer;
    function getStateString: string;
  public
    function NetworkAdded(networkId: TGUID): HResult; stdcall;
    function NetworkDeleted(networkId: TGUID): HResult; stdcall;
    function NetworkConnectivityChanged(networkId: TGUID; NewConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
    function NetworkPropertyChanged(networkId: TGUID; fFlags: NLM_NETWORK_PROPERTY_CHANGE): HResult; stdcall;
    //
    property Description: string read FDescription;
    property State: integer read getState;
    property StateString: string read getStateString;
    //
    class function CUpdate: integer;
    class function CStateString: string;
    constructor Create;
    destructor Destroy; override;
  end;

var Networks: TNetworks;

implementation
uses frmmainu;
//------------------------------------------------------------------------------
class function TNetworks.CUpdate: integer;
begin
  if not assigned(Networks) then
  begin
    Networks := TNetworks.Create;
    {$ifdef EXT_DEBUG} AddLog('TNetworks.Create'); {$endif}
    Networks.ReadNetworks;
    {$ifdef EXT_DEBUG} AddLog('Networks.ReadNetworks'); {$endif}
  end;
  result := Networks.State;
end;
//------------------------------------------------------------------------------
class function TNetworks.CStateString: string;
begin
  result := 'idle';
  if assigned(Networks) then result := Networks.StateString;
end;
//------------------------------------------------------------------------------
constructor TNetworks.Create;
var
  pContainer: IConnectionPointContainer;
  pConnectionPoint: IConnectionPoint;
begin
  ReadInterfaces;
  {$ifdef EXT_DEBUG} AddLog('Networks.Create.ReadInterfaces'); {$endif}
  FReady := assigned(FAdapterTypes)
         and SUCCEEDED(CoCreateInstance(CLASS_NetworkListManager, nil, CLSCTX_ALL, IID_INetworkListManager, FNLM));
  {$ifdef EXT_DEBUG} AddLog('Networks.Create.get FReady'); {$endif}

  if FReady then
  begin
    {$ifdef EXT_DEBUG} AddLog('FReady = true'); {$endif}
    if Succeeded(FNLM.QueryInterface(IID_IConnectionPointContainer, pContainer)) then
    begin
      {$ifdef EXT_DEBUG} AddLog('Succeeded(FNLM.QueryInterface)'); {$endif}
      if Succeeded(pContainer.FindConnectionPoint(IID_INetworkEvents, pConnectionPoint)) then
      begin
        {$ifdef EXT_DEBUG} AddLog('Succeeded(pContainer.FindConnectionPoint)'); {$endif}
        pConnectionPoint.Advise(self as IUnknown, dwCookie1);
        pConnectionPoint := nil;
        {$ifdef EXT_DEBUG} AddLog('pConnectionPoint.Advise'); {$endif}
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
destructor TNetworks.Destroy;
var
  pCpc: IConnectionPointContainer;
  pConnectionPoint: IConnectionPoint;
begin
  if FReady then
    if Succeeded(FNLM.QueryInterface(IID_IConnectionPointContainer, pCpc)) then
    begin
      if Succeeded(pCpc.FindConnectionPoint(IID_INetworkEvents, pConnectionPoint)) then
      begin
        pConnectionPoint.Unadvise(dwCookie1);
        pConnectionPoint := nil;
      end;
    end;

  inherited;
end;
//------------------------------------------------------------------------------
function TNetworks.getState: integer;
begin
  result := FConnections;
  if FWired then inc(result, $100);
  if FWireless then inc(result, $200);
  if FConnected then inc(result, $400);
  if FInternet then inc(result, $800);
end;
//------------------------------------------------------------------------------
function TNetworks.getStateString: string;
begin
  result := 'idle';
  if FWireless and not FWired then result := 'wireless-full';
  if not FConnected then result := 'offline';
end;
//------------------------------------------------------------------------------
procedure TNetworks.Reset;
begin
  FDescription := '';
  FConnected := false;
  FInternet := false;
  FWired := false;
  FWireless := false;
  FConnections := 0;
end;
//------------------------------------------------------------------------------
procedure TNetworks.ReadNetworks;
  function BoolToStrInternetAccess(value: boolean): string;
  begin
    if value then result := 'Internet access' else result := 'No internet access';
  end;
var
  EnumNetworks: IEnumNetworks;
  Network: INetwork;
  EnumConnections: IEnumNetworkConnections;
  Connection: INetworkConnection;
  fetched: ULONG;
begin
  Reset;
  if FReady then
  begin
    EnumNetworks := FNLM.GetNetworks(NLM_ENUM_NETWORK_CONNECTED);
    if assigned(EnumNetworks) then
    begin
      EnumNetworks.Next(1, Network, fetched);
      while fetched > 0 do
      begin
        FDescription := FDescription + string(WideString(Network.GetName)) +
          #10#13 + BoolToStrInternetAccess(Network.IsConnectedToInternet);
        FConnected := FConnected or Network.IsConnected;
        FInternet := FInternet or Network.IsConnectedToInternet;

        EnumConnections := Network.GetNetworkConnections;
        if assigned(EnumConnections) then
        begin
          EnumConnections.Next(1, Connection, fetched);
          while fetched > 0 do
          begin
            GetAdapterType(GUIDToString(Connection.GetAdapterId), FWired, FWireless);
            inc(FConnections);
            Connection := nil;
            EnumConnections.Next(1, Connection, fetched);
          end;
        end;
        EnumConnections := nil;

        Network := nil;
        EnumNetworks.Next(1, Network, fetched);
        if fetched > 0 then FDescription := FDescription + #10#13#10#13;
      end;
    end;
    EnumNetworks := nil;
  end;
end;
//------------------------------------------------------------------------------
procedure TNetworks.GetAdapterType(AdapterID: string; var wired, wireless: boolean);
  function IsWireless(type_: cardinal): boolean;
  begin
    result := (type_ = NdisPhysicalMediumWirelessLan) or (type_ = NdisPhysicalMediumWirelessWan) or (type_ = NdisPhysicalMediumNative802_11);
  end;
var
  index: integer;
begin
  index := FAdapterTypes.IndexOf(AdapterID);
  if index > -1 then
    if IsWireless(cardinal(FAdapterTypes.Objects[index])) then
      wireless := wireless or true
    else wired := wired or true;
end;
//------------------------------------------------------------------------------
procedure TNetworks.ReadInterfaces;
var
  ii: integer;
  table: PMIB_IF_TABLE2;
begin
  {$ifdef EXT_DEBUG} AddLog('TNetworks.ReadInterfaces start'); {$endif}
  if not assigned(FAdapterTypes) then FAdapterTypes := TStringList.Create;
  if assigned(FAdapterTypes) then
  begin
    {$ifdef EXT_DEBUG} AddLog('assigned(FAdapterTypes)'); {$endif}
    FAdapterTypes.Clear;
    if GetIFTable2(table) <> ERROR_NOT_ENOUGH_MEMORY then
    begin
      {$ifdef EXT_DEBUG} AddLog('GetIFTable2 -> table.NumEntries=' + inttostr(table.NumEntries)); {$endif}
      ii := 0;
      while ii < table.NumEntries do
      begin
        if table.Table[ii].PhysicalMediumType <> NdisPhysicalMediumUnspecified then
          FAdapterTypes.AddObject(GUIDToString(table.Table[ii].InterfaceGuid), tobject(table.Table[ii].PhysicalMediumType));
        inc(ii);
      end;
      {$ifdef EXT_DEBUG} AddLog('end GetIFTable2'); {$endif}
      FreeMibTable(table);
      {$ifdef EXT_DEBUG} AddLog('FreeMibTable'); {$endif}
    end;
  end;
end;
//------------------------------------------------------------------------------
function TNetworks.NetworkAdded(networkId: TGUID): HResult; stdcall;
begin
  ReadNetworks;
  Result := S_OK;
end;
//------------------------------------------------------------------------------
function TNetworks.NetworkConnectivityChanged(networkId: TGUID; NewConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
begin
  ReadNetworks;
  Result := S_OK;
end;
//------------------------------------------------------------------------------
function TNetworks.NetworkDeleted(networkId: TGUID): HResult; stdcall;
begin
  ReadNetworks;
  Result := S_OK;
end;
//------------------------------------------------------------------------------
function TNetworks.NetworkPropertyChanged(networkId: TGUID; fFlags: NLM_NETWORK_PROPERTY_CHANGE): HResult; stdcall;
begin
  ReadNetworks;
  Result := S_OK;
end;
//------------------------------------------------------------------------------
end.
