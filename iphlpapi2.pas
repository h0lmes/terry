unit iphlpapi2;

{$mode delphi}
interface
uses
  Windows, Classes, SysUtils;

const
  IPHelper = 'iphlpapi.dll';

  // GetAdaptersInfo
  MAX_ADAPTER_NAME_LENGTH        = 256;
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
  MAX_ADAPTER_ADDRESS_LENGTH     = 8;

  // GetIfTable2
  IF_MAX_STRING_SIZE             = 256;
  IF_MAX_PHYS_ADDRESS_LENGTH     = 32;

  // adapter types
  IF_TYPE_OTHER             = 1;
  IF_TYPE_ETHERNET          = 6;
  IF_TYPE_TOKENRING         = 9;
  IF_TYPE_FDDI              = 15;
  IF_TYPE_PPP               = 23;
  IF_TYPE_SOFTWARE_LOOPBACK = 24;
  IF_TYPE_SLIP              = 28;
  IF_TYPE_ATM               = 37;
  IF_TYPE_IEEE80211         = 71;
  IF_TYPE_TUNNEL            = 131;
  IF_TYPE_IEEE1394          = 144;
  IF_TYPE_IEEE80216_WMAN    = 237;
  IF_TYPE_WWANPP            = 243;
  IF_TYPE_WWANPP2           = 244;

  // InterfaceAndOperStatusFlags
  HardwareInterface  = 1; // interface is for hardware
  FilterInterface    = 2; // interface is for a filter module
  ConnectorPresent   = 4; // this is a physical network adapter
  NotAuthenticated   = 8; // intf not in an operational mode
  NotMediaConnected  = 16; // network cable is unplugged or not connected to a wireless network
  Paused             = 32; // network stack for the network interface is in the paused or pausing state
  LowPower           = 64; // interface is in a low power state
  EndPointInterface  = 128; // not a true intf

  // IF_OPER_STATUS
  IfOperStatusUp             = 1;
  IfOperStatusDown           = 2;
  IfOperStatusTesting        = 3;
  IfOperStatusUnknown        = 4;
  IfOperStatusDormant        = 5;
  IfOperStatusNotPresent     = 6;
  IfOperStatusLowerLayerDown = 7;

  // PhysicalMediumType
  NdisPhysicalMediumUnspecified  = 0; //The physical medium is none of the below values. For example, a one-way satellite feed is an unspecified physical medium.
  NdisPhysicalMediumWirelessLan  = 1; // Packets are transferred over a wireless LAN network through a miniport driver that conforms to the 802.11 interface.
  NdisPhysicalMediumCableModem   = 2; // Packets are transferred over a DOCSIS-based cable network.
  NdisPhysicalMediumPhoneLine    = 3; // Packets are transferred over standard phone lines. This includes HomePNA media, for example.
  NdisPhysicalMediumPowerLine    = 4; // Packets are transferred over wiring that is connected to a power distribution system.
  NdisPhysicalMediumDSL          = 5; // Packets are transferred over a Digital Subscriber Line (DSL) network. This includes ADSL, UADSL (G.Lite), and SDSL, for example.
  NdisPhysicalMediumFibreChannel = 6; // Packets are transferred over a Fibre Channel interconnect.
  NdisPhysicalMedium1394         = 7; // Packets are transferred over an IEEE 1394 bus.
  NdisPhysicalMediumWirelessWan  = 8; // Packets are transferred over a Wireless WAN link. This includes mobile broadband devices that support CDPD, CDMA, GSM, and GPRS, for example.
  NdisPhysicalMediumNative802_11 = 9; // Packets are transferred over a wireless LAN network through a miniport driver that conforms to the Native 802.11 interface. Note: The Native 802.11 interface is supported in NDIS 6.0 and later versions.
  NdisPhysicalMediumBluetooth    = 10; // Packets are transferred over a Bluetooth network/
  NdisPhysicalMediumInfiniband   = 11; // Packets are transferred over an Infiniband interconnect.
  NdisPhysicalMediumWiMax        = 12; // Packets are transferred over a WiMax network.
  NdisPhysicalMediumUWB          = 13; // Packets are transferred over an ultra wide band network.
  NdisPhysicalMedium802_3        = 14; // Packets are transferred over an Ethernet (802.3) network.
  NdisPhysicalMedium802_5        = 15; // Packets are transferred over a Token Ring (802.5) network.
  NdisPhysicalMediumIrda         = 16; // Packets are transferred over an infrared (IrDA) network.
  NdisPhysicalMediumWiredWAN     = 17; // Packets are transferred over a wired WAN network.
  NdisPhysicalMediumWiredCoWan   = 18; // Packets are transferred over a wide area network in a connection-oriented environment.
  NdisPhysicalMediumOther        = 19; // Packets are transferred over a network that is not described by other possible values.

type
  // GetAdaptersInfo
  time_t = Longint;

  IP_ADDRESS_STRING = record
    S: array [0..15] of Char;
  end;
  IP_MASK_STRING = IP_ADDRESS_STRING;
  PIP_MASK_STRING = ^IP_MASK_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
    AddressLength: UINT;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;

  // GetIfTable2

  NET_LUID = record
   case Integer of
     0: (Value: DWORD); // UInt64 --> DWORD
     1: (Reserved: DWORD; NetLuidIndex: DWORD; IfType: DWORD);
  end;

  NET_IFINDEX = ULONG;

  PMIB_IF_ROW2 = ^MIB_IF_ROW2;
  _MIB_IF_ROW2 = packed record
    InterfaceLuid: NET_LUID;
    InterfaceIndex: NET_IFINDEX;
    InterfaceGuid: TGUID;
    Alias: array [0..IF_MAX_STRING_SIZE] of WideChar;
    Description: array [0..IF_MAX_STRING_SIZE] of WideChar;
    PhysicalAddressLength: Cardinal;
    PhysicalAddress: array [0..IF_MAX_PHYS_ADDRESS_LENGTH - 1] of Byte;
    PermanentPhysicalAddress: array [0..IF_MAX_PHYS_ADDRESS_LENGTH - 1] of Byte;
    Mtu: Cardinal; // ULONG;
    aType: Cardinal;
    TunnelType: Cardinal;
    MediaType: Cardinal;
    PhysicalMediumType: Cardinal;
    AccessType: Cardinal;
    DirectionType: Cardinal;
    InterfaceAndOperStatusFlags: Cardinal; // bitwise OR, 8 flags TInterfaceAndOperStatusFlags
    OperStatus: Cardinal;
    AdminStatus: Cardinal;
    MediaConnectState: Cardinal;
    NetworkGuid: TGUID;
    ConnectionType: Cardinal;
    TransmitLinkSpeed: UInt64;
    ReceiveLinkSpeed: UInt64;
    InOctets: UInt64;
    InUcastPkts: UInt64;
    InNUcastPkts: UInt64;
    InDiscards: UInt64;
    InErrors: UInt64;
    InUnknownProtos: UInt64;
    InUcastOctets: UInt64;
    InMulticastOctets: UInt64;
    InBroadcastOctets: UInt64;
    OutOctets: UInt64;
    OutUcastPkts: UInt64;
    OutNUcastPkts: UInt64;
    OutDiscards: UInt64;
    OutErrors: UInt64;
    OutUcastOctets: UInt64;
    OutMulticastOctets: UInt64;
    OutBroadcastOctets: UInt64;
    OutQLen: UInt64;
  end;
  MIB_IF_ROW2 = _MIB_IF_ROW2;
  TMibIfRow2 = MIB_IF_ROW2;
  PMibIfRow2 = PMIB_IF_ROW2;

  TMibIfArray2 = array [0..0] of TMibIfRow2;
  PMibIfArray2 = ^TMibIfArray2;

  PMIB_IF_TABLE2 = ^MIB_IF_TABLE2;
  _MIB_IF_TABLE2 = packed record
    NumEntries: Cardinal;
    Table: array[0..0] of _MIB_IF_ROW2;
  end;
  MIB_IF_TABLE2 = _MIB_IF_TABLE2;
  TMibIfTable2 = MIB_IF_TABLE2;
  PMibIfTable2 = PMIB_IF_TABLE2;

  // imports
  function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall; external IPHelper;
  function GetIfTable2(var Table: PMIB_IF_TABLE2): DWORD; stdcall; external IPHelper;
  procedure FreeMibTable(var Table: PMIB_IF_TABLE2); stdcall; external IPHelper;

  // helper functions
  function MACToStr(Addr: array of Byte; Len: Integer): string;
  function TimeToDateTimeStr(Value: Integer): string;
  function IFOperStatusToStr(status: uint): string;
  function InterfaceAndStatusFlagsToStr(flags: uint): string;
  function IFTypeToStr(type_: uint): string;
  function PhysicalMediumToStr(type_: uint): string;
  function PhysicalMediumToStrType(type_: uint): string;

implementation
//------------------------------------------------------------------------------
function MACToStr(Addr: array of Byte; Len: Integer): string;
var
  ii: integer;
begin
  if Len = 0 then Result := '00-00-00-00-00-00' else
  begin
    Result := '';
    for ii := 0 to Len - 2 do Result := Result + IntToHex(Addr[ii], 2) + '-';
    Result := Result + IntToHex(Addr[Len - 1], 2);
  end;
end;
//------------------------------------------------------------------------------
function TimeToDateTimeStr(Value: Integer): string;
const
  UnixDateDelta = 25569; // количество дней между 12.31.1899 и 1.1.1970
  MinPerDay = 24 * 60;
  SecPerDay = 24 * 60 * 60;
var
  Data: TDateTime;
  TimeZoneInformation: TTimeZoneInformation;
  AResult: DWORD;
begin
  Result := '';
  if Value = 0 then exit;

  // Формат Unix-ового TIME_T кол-во секунд от 1.1.1970
  Data := UnixDateDelta + (Value / SecPerDay);
  AResult := GetTimeZoneInformation(TimeZoneInformation);
  case AResult of
    TIME_ZONE_ID_INVALID: RaiseLastOSError;
    TIME_ZONE_ID_STANDARD:
    begin
      Data := Data - ((TimeZoneInformation.Bias + TimeZoneInformation.StandardBias) / MinPerDay);
      Result := DateTimeToStr(Data) + ' ' + WideCharToString(TimeZoneInformation.StandardName);
    end;
  else
    Data := Data - ((TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias) / MinPerDay);
    Result := DateTimeToStr(Data) + ' ' + WideCharToString(TimeZoneInformation.DaylightName);
  end;
end;
//------------------------------------------------------------------------------
function IFTypeToStr(type_: uint): string;
begin
  result := 'Unknown';
  case type_ of
    IF_TYPE_OTHER:              result := 'OTHER';
    IF_TYPE_ETHERNET:           result := 'ETHERNET';
    IF_TYPE_TOKENRING:          result := 'TOKENRING';
    IF_TYPE_FDDI:               result := 'FDDI';
    IF_TYPE_PPP:                result := 'PPP';
    IF_TYPE_SOFTWARE_LOOPBACK : result := 'SOFTWARE_LOOPBACK';
    IF_TYPE_SLIP :              result := 'SLIP';
    IF_TYPE_ATM :               result := 'ATM';
    IF_TYPE_IEEE80211 :         result := 'IEEE80211 wireless';
    IF_TYPE_TUNNEL :            result := 'TUNNEL';
    IF_TYPE_IEEE1394 :          result := 'IEEE1394';
    IF_TYPE_IEEE80216_WMAN :    result := 'IEEE80216_WMAN';
    IF_TYPE_WWANPP :            result := 'WWANPP';
    IF_TYPE_WWANPP2 :           result := 'WWANPP2';
  end;
end;
//------------------------------------------------------------------------------
function InterfaceAndStatusFlagsToStr(flags: uint): string;
begin
  result := '';
  if flags and HardwareInterface <> 0 then result := result + '  |  HardwareInterface';
  if flags and FilterInterface <> 0 then   result := result + '  |  FilterInterface';
  if flags and ConnectorPresent <> 0 then  result := result + '  |  ConnectorPresent';
  if flags and NotAuthenticated <> 0 then  result := result + '  |  NotAuthenticated';
  if flags and NotMediaConnected <> 0 then result := result + '  |  NotMediaConnected';
  if flags and Paused <> 0 then            result := result + '  |  Paused';
  if flags and LowPower <> 0 then          result := result + '  |  LowPower';
  if flags and EndPointInterface <> 0 then result := result + '  |  EndPointInterface';
  if result <> '' then result := copy(result, 6, length(result));
end;
//------------------------------------------------------------------------------
function IFOperStatusToStr(status: uint): string;
begin
  result := '';
  case status of
    IfOperStatusUp:              result := 'UP';
    IfOperStatusDown:            result := 'DOWN';
    IfOperStatusTesting:         result := 'TESTING';
    IfOperStatusUnknown:         result := 'UNKNOWN';
    IfOperStatusDormant:         result := 'DORMANT';
    IfOperStatusNotPresent :     result := 'NOT_PRESENT';
    IfOperStatusLowerLayerDown : result := 'LOWER_LAYER_DOWN';
  end;
end;
//------------------------------------------------------------------------------
function PhysicalMediumToStr(type_: uint): string;
begin
  result := '';
  case type_ of
    NdisPhysicalMediumUnspecified:      result := 'Unspecified';
    NdisPhysicalMediumWirelessLan:      result := 'WirelessLan';
    NdisPhysicalMediumCableModem:       result := 'CableModem';
    NdisPhysicalMediumPhoneLine:        result := 'PhoneLine';
    NdisPhysicalMediumPowerLine:        result := 'PowerLine';
    NdisPhysicalMediumDSL:              result := 'DSL';
    NdisPhysicalMediumFibreChannel:     result := 'FibreChannel';
    NdisPhysicalMedium1394:             result := '1394';
    NdisPhysicalMediumWirelessWan:      result := 'WirelessWan';
    NdisPhysicalMediumNative802_11:     result := 'Native802_11';
    NdisPhysicalMediumBluetooth:        result := 'Bluetooth';
    NdisPhysicalMediumInfiniband:       result := 'Infiniband';
    NdisPhysicalMediumWiMax:            result := 'WiMax';
    NdisPhysicalMediumUWB:              result := 'UWB';
    NdisPhysicalMedium802_3:            result := '802_3';
    NdisPhysicalMedium802_5:            result := '802_5';
    NdisPhysicalMediumIrda:             result := 'Irda';
    NdisPhysicalMediumWiredWAN:         result := 'WiredWAN';
    NdisPhysicalMediumWiredCoWan:       result := 'WiredCoWan';
    NdisPhysicalMediumOther:            result := 'Other';
  end;
end;
//------------------------------------------------------------------------------
function PhysicalMediumToStrType(type_: uint): string;
begin
  result := 'wired';
  case type_ of
    NdisPhysicalMediumWirelessLan,
    NdisPhysicalMediumWirelessWan,
    NdisPhysicalMediumNative802_11: result := 'wireless';
  end;
end;
//------------------------------------------------------------------------------
end.

