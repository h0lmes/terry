unit networksu;

{$mode delphi}

interface
uses Windows, Classes, SysUtils, ActiveX, iphlpapi2, networklist_tlb;

type
  TNetworks = class(TInterfacedObject, INetworkEvents)
  private
    FReady: boolean;
    FMgr: INetworkListManager;
    dwCookie: dword;
    FWired: boolean;
    FWireless: boolean;
    FConnected: boolean;
    FInternet: boolean;
    FConnections: integer;
    FDescription: string;
    adapterPhysMedium: TStrings;
    procedure Reset;
    procedure ReadNetworks;
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
    class function CUpdate: integer;
    constructor Create;
  end;

var Networks: TNetworks;

implementation
//------------------------------------------------------------------------------
class function TNetworks.CUpdate: integer;
begin
  if not assigned(Networks) then
  begin
    Networks := TNetworks.Create;
    Networks.ReadNetworks;
  end;
  result := Networks.State;
end;
//------------------------------------------------------------------------------
constructor TNetworks.Create;
var
  pCpc: IConnectionPointContainer;
  pConnectionPoint: IConnectionPoint;
  pSink: INetworkEvents;
begin
  ReadInterfaces;
  FReady := assigned(adapterPhysMedium)
         and SUCCEEDED(CoCreateInstance(CLASS_NetworkListManager, nil, CLSCTX_ALL, IID_INetworkListManager, FMgr));

  if FReady then
    if Succeeded(FMgr.QueryInterface(IID_IConnectionPointContainer, pCpc)) then
    begin
      pSink := self as INetworkEvents;
      if Succeeded(pCpc.FindConnectionPoint(IID_INetworkEvents, pConnectionPoint)) then
      begin
        pConnectionPoint.Advise(pSink, dwCookie);
        pConnectionPoint := nil;
      end;
    end;
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
  function IsWireless(type_: cardinal): boolean;
  begin
    result := (type_ = NdisPhysicalMediumWirelessLan) or (type_ = NdisPhysicalMediumWirelessWan) or (type_ = NdisPhysicalMediumNative802_11);
  end;
var
  EnumNetworks: IEnumNetworks;
  Network: INetwork;
  EnumConnections: IEnumNetworkConnections;
  Connection: INetworkConnection;
  fetched: ULONG;
  adapterIndex: integer;
begin
  Reset;
  if FReady then
  begin
    EnumNetworks := FMgr.GetNetworks(NLM_ENUM_NETWORK_CONNECTED);
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
            inc(FConnections);
            adapterIndex := adapterPhysMedium.IndexOf(GUIDToString(Connection.GetAdapterId));
            if adapterIndex > -1 then
              if IsWireless(cardinal(adapterPhysMedium.Objects[adapterIndex])) then
                FWireless := FWireless or true
              else FWired := FWired or true;

            Connection := nil;
            EnumConnections.Next(1, Connection, fetched);
          end;
        end;

        Network := nil;
        EnumNetworks.Next(1, Network, fetched);
        if fetched > 0 then FDescription := FDescription + #10#13#10#13;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TNetworks.ReadInterfaces;
var
  ii: integer;
  table: PMIB_IF_TABLE2;
begin
  if not assigned(adapterPhysMedium) then adapterPhysMedium := TStringList.Create;
  if assigned(adapterPhysMedium) then
  begin
    adapterPhysMedium.Clear;
    if GetIFTable2(table) <> ERROR_NOT_ENOUGH_MEMORY then
    begin
      ii := 0;
      while ii < table.NumEntries do
      begin
        if table.Table[ii].PhysicalMediumType <> NdisPhysicalMediumUnspecified then
          adapterPhysMedium.AddObject(GUIDToString(table.Table[ii].InterfaceGuid), tobject(table.Table[ii].PhysicalMediumType));
        inc(ii);
      end;
      FreeMibTable(table);
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
