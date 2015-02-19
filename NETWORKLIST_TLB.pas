unit NETWORKLIST_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 17244 $
// File generated on 17.02.2015 17:16:00 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\system32\netprofm.dll (1)
// LIBID: {DCB00D01-570F-4A9B-8D69-199FDBA5723B}
// LCID: 0
// Helpfile: 
// HelpString: Network List 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// Errors:
//   Error creating palette bitmap of (TNetworkListManager) : Server C:\Windows\System32\netprofm.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, OleServer, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  NETWORKLISTMajorVersion = 1;
  NETWORKLISTMinorVersion = 0;

  LIBID_NETWORKLIST: TGUID = '{DCB00D01-570F-4A9B-8D69-199FDBA5723B}';

  IID_INetworkListManager: TGUID = '{DCB00000-570F-4A9B-8D69-199FDBA5723B}';
  IID_INetworkEvents: TGUID = '{DCB00004-570F-4A9B-8D69-199FDBA5723B}';
  IID_IEnumNetworks: TGUID = '{DCB00003-570F-4A9B-8D69-199FDBA5723B}';
  IID_INetwork: TGUID = '{DCB00002-570F-4A9B-8D69-199FDBA5723B}';
  IID_IEnumNetworkConnections: TGUID = '{DCB00006-570F-4A9B-8D69-199FDBA5723B}';
  IID_INetworkConnection: TGUID = '{DCB00005-570F-4A9B-8D69-199FDBA5723B}';
  IID_INetworkConnectionEvents: TGUID = '{DCB00007-570F-4A9B-8D69-199FDBA5723B}';
  IID_INetworkListManagerEvents: TGUID = '{DCB00001-570F-4A9B-8D69-199FDBA5723B}';
  IID_IPropertyBag: TGUID = '{55272A00-42CB-11CE-8135-00AA004BB851}';
  CLASS_NetworkListManager: TGUID = '{DCB00C01-570F-4A9B-8D69-199FDBA5723B}';
  IID_IErrorLog: TGUID = '{3127CA40-446E-11CE-8135-00AA004BB851}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum NLM_ENUM_NETWORK
type
  NLM_ENUM_NETWORK = TOleEnum;
const
  NLM_ENUM_NETWORK_CONNECTED = $00000001;
  NLM_ENUM_NETWORK_DISCONNECTED = $00000002;
  NLM_ENUM_NETWORK_ALL = $00000003;

// Constants for enum NLM_DOMAIN_TYPE
type
  NLM_DOMAIN_TYPE = TOleEnum;
const
  NLM_DOMAIN_TYPE_NON_DOMAIN_NETWORK = $00000000;
  NLM_DOMAIN_TYPE_DOMAIN_NETWORK = $00000001;
  NLM_DOMAIN_TYPE_DOMAIN_AUTHENTICATED = $00000002;

// Constants for enum NLM_CONNECTIVITY
type
  NLM_CONNECTIVITY = TOleEnum;
const
  NLM_CONNECTIVITY_DISCONNECTED = $00000000;
  NLM_CONNECTIVITY_IPV4_NOTRAFFIC = $00000001;
  NLM_CONNECTIVITY_IPV6_NOTRAFFIC = $00000002;
  NLM_CONNECTIVITY_IPV4_SUBNET = $00000010;
  NLM_CONNECTIVITY_IPV4_LOCALNETWORK = $00000020;
  NLM_CONNECTIVITY_IPV4_INTERNET = $00000040;
  NLM_CONNECTIVITY_IPV6_SUBNET = $00000100;
  NLM_CONNECTIVITY_IPV6_LOCALNETWORK = $00000200;
  NLM_CONNECTIVITY_IPV6_INTERNET = $00000400;

// Constants for enum NLM_NETWORK_CATEGORY
type
  NLM_NETWORK_CATEGORY = TOleEnum;
const
  NLM_NETWORK_CATEGORY_PUBLIC = $00000000;
  NLM_NETWORK_CATEGORY_PRIVATE = $00000001;
  NLM_NETWORK_CATEGORY_DOMAIN_AUTHENTICATED = $00000002;

// Constants for enum NLM_NETWORK_PROPERTY_CHANGE
type
  NLM_NETWORK_PROPERTY_CHANGE = TOleEnum;
const
  NLM_NETWORK_PROPERTY_CHANGE_CONNECTION = $00000001;
  NLM_NETWORK_PROPERTY_CHANGE_DESCRIPTION = $00000002;
  NLM_NETWORK_PROPERTY_CHANGE_NAME = $00000004;
  NLM_NETWORK_PROPERTY_CHANGE_ICON = $00000008;
  NLM_NETWORK_PROPERTY_CHANGE_CATEGORY_VALUE = $00000010;

// Constants for enum NLM_CONNECTION_PROPERTY_CHANGE
type
  NLM_CONNECTION_PROPERTY_CHANGE = TOleEnum;
const
  NLM_CONNECTION_PROPERTY_CHANGE_AUTHENTICATION = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  INetworkListManager = interface;
  INetworkListManagerDisp = dispinterface;
  INetworkEvents = interface;
  IEnumNetworks = interface;
  IEnumNetworksDisp = dispinterface;
  INetwork = interface;
  INetworkDisp = dispinterface;
  IEnumNetworkConnections = interface;
  IEnumNetworkConnectionsDisp = dispinterface;
  INetworkConnection = interface;
  INetworkConnectionDisp = dispinterface;
  INetworkConnectionEvents = interface;
  INetworkListManagerEvents = interface;
  IPropertyBag = interface;
  IErrorLog = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  NetworkListManager = INetworkListManager;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}
  PUserType1 = ^EXCEPINFO; {*}


// *********************************************************************//
// Interface: INetworkListManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00000-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkListManager = interface(IDispatch)
    ['{DCB00000-570F-4A9B-8D69-199FDBA5723B}']
    function GetNetworks(Flags: NLM_ENUM_NETWORK): IEnumNetworks; safecall;
    function GetNetwork(gdNetworkId: TGUID): INetwork; safecall;
    function GetNetworkConnections: IEnumNetworkConnections; safecall;
    function GetNetworkConnection(gdNetworkConnectionId: TGUID): INetworkConnection; safecall;
    function Get_IsConnectedToInternet: WordBool; safecall;
    function Get_IsConnected: WordBool; safecall;
    function GetConnectivity: NLM_CONNECTIVITY; safecall;
    property IsConnectedToInternet: WordBool read Get_IsConnectedToInternet;
    property IsConnected: WordBool read Get_IsConnected;
  end;

// *********************************************************************//
// DispIntf:  INetworkListManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00000-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkListManagerDisp = dispinterface
    ['{DCB00000-570F-4A9B-8D69-199FDBA5723B}']
    function GetNetworks(Flags: NLM_ENUM_NETWORK): IEnumNetworks; dispid 1;
    function GetNetwork(gdNetworkId: {??TGUID}OleVariant): INetwork; dispid 2;
    function GetNetworkConnections: IEnumNetworkConnections; dispid 3;
    function GetNetworkConnection(gdNetworkConnectionId: {??TGUID}OleVariant): INetworkConnection; dispid 4;
    property IsConnectedToInternet: WordBool readonly dispid 5;
    property IsConnected: WordBool readonly dispid 6;
    function GetConnectivity: NLM_CONNECTIVITY; dispid 7;
  end;

// *********************************************************************//
// Interface: INetworkEvents
// Flags:     (256) OleAutomation
// GUID:      {DCB00004-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkEvents = interface(IUnknown)
    ['{DCB00004-570F-4A9B-8D69-199FDBA5723B}']
    function NetworkAdded(networkId: TGUID): HResult; stdcall;
    function NetworkDeleted(networkId: TGUID): HResult; stdcall;
    function NetworkConnectivityChanged(networkId: TGUID; newConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
    function NetworkPropertyChanged(networkId: TGUID; Flags: NLM_NETWORK_PROPERTY_CHANGE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumNetworks
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00003-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  IEnumNetworks = interface(IDispatch)
    ['{DCB00003-570F-4A9B-8D69-199FDBA5723B}']
    function Get__NewEnum: IEnumVARIANT; safecall;
    procedure Next(celt: LongWord; out rgelt: INetwork; var pceltFetched: LongWord); safecall;
    procedure Skip(celt: LongWord); safecall;
    procedure Reset; safecall;
    function Clone: IEnumNetworks; safecall;
    property _NewEnum: IEnumVARIANT read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IEnumNetworksDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00003-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  IEnumNetworksDisp = dispinterface
    ['{DCB00003-570F-4A9B-8D69-199FDBA5723B}']
    property _NewEnum: IEnumVARIANT readonly dispid -4;
    procedure Next(celt: LongWord; out rgelt: INetwork; var pceltFetched: LongWord); dispid 1;
    procedure Skip(celt: LongWord); dispid 2;
    procedure Reset; dispid 3;
    function Clone: IEnumNetworks; dispid 4;
  end;

// *********************************************************************//
// Interface: INetwork
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00002-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetwork = interface(IDispatch)
    ['{DCB00002-570F-4A9B-8D69-199FDBA5723B}']
    function GetName: WideString; safecall;
    procedure SetName(const szNetworkNewName: WideString); safecall;
    function GetDescription: WideString; safecall;
    procedure SetDescription(const szDescription: WideString); safecall;
    function GetNetworkId: TGUID; safecall;
    function GetDomainType: NLM_DOMAIN_TYPE; safecall;
    function GetNetworkConnections: IEnumNetworkConnections; safecall;
    procedure GetTimeCreatedAndConnected(out pdwLowDateTimeCreated: LongWord; 
                                         out pdwHighDateTimeCreated: LongWord; 
                                         out pdwLowDateTimeConnected: LongWord; 
                                         out pdwHighDateTimeConnected: LongWord); safecall;
    function Get_IsConnectedToInternet: WordBool; safecall;
    function Get_IsConnected: WordBool; safecall;
    function GetConnectivity: NLM_CONNECTIVITY; safecall;
    function GetCategory: NLM_NETWORK_CATEGORY; safecall;
    procedure SetCategory(NewCategory: NLM_NETWORK_CATEGORY); safecall;
    property IsConnectedToInternet: WordBool read Get_IsConnectedToInternet;
    property IsConnected: WordBool read Get_IsConnected;
  end;

// *********************************************************************//
// DispIntf:  INetworkDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00002-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkDisp = dispinterface
    ['{DCB00002-570F-4A9B-8D69-199FDBA5723B}']
    function GetName: WideString; dispid 1;
    procedure SetName(const szNetworkNewName: WideString); dispid 2;
    function GetDescription: WideString; dispid 3;
    procedure SetDescription(const szDescription: WideString); dispid 4;
    function GetNetworkId: {??TGUID}OleVariant; dispid 5;
    function GetDomainType: NLM_DOMAIN_TYPE; dispid 6;
    function GetNetworkConnections: IEnumNetworkConnections; dispid 7;
    procedure GetTimeCreatedAndConnected(out pdwLowDateTimeCreated: LongWord; 
                                         out pdwHighDateTimeCreated: LongWord; 
                                         out pdwLowDateTimeConnected: LongWord; 
                                         out pdwHighDateTimeConnected: LongWord); dispid 8;
    property IsConnectedToInternet: WordBool readonly dispid 9;
    property IsConnected: WordBool readonly dispid 10;
    function GetConnectivity: NLM_CONNECTIVITY; dispid 11;
    function GetCategory: NLM_NETWORK_CATEGORY; dispid 12;
    procedure SetCategory(NewCategory: NLM_NETWORK_CATEGORY); dispid 13;
  end;

// *********************************************************************//
// Interface: IEnumNetworkConnections
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00006-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  IEnumNetworkConnections = interface(IDispatch)
    ['{DCB00006-570F-4A9B-8D69-199FDBA5723B}']
    function Get__NewEnum: IEnumVARIANT; safecall;
    procedure Next(celt: LongWord; out rgelt: INetworkConnection; var pceltFetched: LongWord); safecall;
    procedure Skip(celt: LongWord); safecall;
    procedure Reset; safecall;
    function Clone: IEnumNetworkConnections; safecall;
    property _NewEnum: IEnumVARIANT read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IEnumNetworkConnectionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00006-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  IEnumNetworkConnectionsDisp = dispinterface
    ['{DCB00006-570F-4A9B-8D69-199FDBA5723B}']
    property _NewEnum: IEnumVARIANT readonly dispid -4;
    procedure Next(celt: LongWord; out rgelt: INetworkConnection; var pceltFetched: LongWord); dispid 1;
    procedure Skip(celt: LongWord); dispid 2;
    procedure Reset; dispid 3;
    function Clone: IEnumNetworkConnections; dispid 4;
  end;

// *********************************************************************//
// Interface: INetworkConnection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00005-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkConnection = interface(IDispatch)
    ['{DCB00005-570F-4A9B-8D69-199FDBA5723B}']
    function GetNetwork: INetwork; safecall;
    function Get_IsConnectedToInternet: WordBool; safecall;
    function Get_IsConnected: WordBool; safecall;
    function GetConnectivity: NLM_CONNECTIVITY; safecall;
    function GetConnectionId: TGUID; safecall;
    function GetAdapterId: TGUID; safecall;
    function GetDomainType: NLM_DOMAIN_TYPE; safecall;
    property IsConnectedToInternet: WordBool read Get_IsConnectedToInternet;
    property IsConnected: WordBool read Get_IsConnected;
  end;

// *********************************************************************//
// DispIntf:  INetworkConnectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DCB00005-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkConnectionDisp = dispinterface
    ['{DCB00005-570F-4A9B-8D69-199FDBA5723B}']
    function GetNetwork: INetwork; dispid 1;
    property IsConnectedToInternet: WordBool readonly dispid 2;
    property IsConnected: WordBool readonly dispid 3;
    function GetConnectivity: NLM_CONNECTIVITY; dispid 4;
    function GetConnectionId: {??TGUID}OleVariant; dispid 5;
    function GetAdapterId: {??TGUID}OleVariant; dispid 6;
    function GetDomainType: NLM_DOMAIN_TYPE; dispid 7;
  end;

// *********************************************************************//
// Interface: INetworkConnectionEvents
// Flags:     (0)
// GUID:      {DCB00007-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkConnectionEvents = interface(IUnknown)
    ['{DCB00007-570F-4A9B-8D69-199FDBA5723B}']
    function NetworkConnectionConnectivityChanged(connectionId: TGUID; 
                                                  newConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
    function NetworkConnectionPropertyChanged(connectionId: TGUID; 
                                              Flags: NLM_CONNECTION_PROPERTY_CHANGE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: INetworkListManagerEvents
// Flags:     (256) OleAutomation
// GUID:      {DCB00001-570F-4A9B-8D69-199FDBA5723B}
// *********************************************************************//
  INetworkListManagerEvents = interface(IUnknown)
    ['{DCB00001-570F-4A9B-8D69-199FDBA5723B}']
    function ConnectivityChanged(newConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IPropertyBag
// Flags:     (0)
// GUID:      {55272A00-42CB-11CE-8135-00AA004BB851}
// *********************************************************************//
  IPropertyBag = interface(IUnknown)
    ['{55272A00-42CB-11CE-8135-00AA004BB851}']
    function RemoteRead(pszPropName: PWideChar; out pVar: OleVariant; const pErrorLog: IErrorLog; 
                        varType: LongWord; const pUnkObj: IUnknown): HResult; stdcall;
    function Write(pszPropName: PWideChar; var pVar: OleVariant): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IErrorLog
// Flags:     (0)
// GUID:      {3127CA40-446E-11CE-8135-00AA004BB851}
// *********************************************************************//
  IErrorLog = interface(IUnknown)
    ['{3127CA40-446E-11CE-8135-00AA004BB851}']
    function AddError(pszPropName: PWideChar; var pExcepInfo: EXCEPINFO): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoNetworkListManager provides a Create and CreateRemote method to          
// create instances of the default interface INetworkListManager exposed by              
// the CoClass NetworkListManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNetworkListManager = class
    class function Create: INetworkListManager;
    class function CreateRemote(const MachineName: string): INetworkListManager;
  end;

  TNetworkListManagerNetworkAdded = procedure(ASender: TObject; networkId: TGUID) of object;
  TNetworkListManagerNetworkDeleted = procedure(ASender: TObject; networkId: TGUID) of object;
  TNetworkListManagerNetworkConnectivityChanged = procedure(ASender: TObject; networkId: TGUID; 
                                                                              newConnectivity: NLM_CONNECTIVITY) of object;
  TNetworkListManagerNetworkPropertyChanged = procedure(ASender: TObject; networkId: TGUID; 
                                                                          Flags: NLM_NETWORK_PROPERTY_CHANGE) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TNetworkListManager
// Help String      : Network List Manager
// Default Interface: INetworkListManager
// Def. Intf. DISP? : No
// Event   Interface: INetworkEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TNetworkListManagerProperties= class;
{$ENDIF}
  TNetworkListManager = class(TOleServer)
  private
    FOnNetworkAdded: TNetworkListManagerNetworkAdded;
    FOnNetworkDeleted: TNetworkListManagerNetworkDeleted;
    FOnNetworkConnectivityChanged: TNetworkListManagerNetworkConnectivityChanged;
    FOnNetworkPropertyChanged: TNetworkListManagerNetworkPropertyChanged;
    FIntf: INetworkListManager;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TNetworkListManagerProperties;
    function GetServerProperties: TNetworkListManagerProperties;
{$ENDIF}
    function GetDefaultInterface: INetworkListManager;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_IsConnectedToInternet: WordBool;
    function Get_IsConnected: WordBool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: INetworkListManager);
    procedure Disconnect; override;
    function GetNetworks(Flags: NLM_ENUM_NETWORK): IEnumNetworks;
    function GetNetwork(gdNetworkId: TGUID): INetwork;
    function GetNetworkConnections: IEnumNetworkConnections;
    function GetNetworkConnection(gdNetworkConnectionId: TGUID): INetworkConnection;
    function GetConnectivity: NLM_CONNECTIVITY;
    property DefaultInterface: INetworkListManager read GetDefaultInterface;
    property IsConnectedToInternet: WordBool read Get_IsConnectedToInternet;
    property IsConnected: WordBool read Get_IsConnected;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TNetworkListManagerProperties read GetServerProperties;
{$ENDIF}
    property OnNetworkAdded: TNetworkListManagerNetworkAdded read FOnNetworkAdded write FOnNetworkAdded;
    property OnNetworkDeleted: TNetworkListManagerNetworkDeleted read FOnNetworkDeleted write FOnNetworkDeleted;
    property OnNetworkConnectivityChanged: TNetworkListManagerNetworkConnectivityChanged read FOnNetworkConnectivityChanged write FOnNetworkConnectivityChanged;
    property OnNetworkPropertyChanged: TNetworkListManagerNetworkPropertyChanged read FOnNetworkPropertyChanged write FOnNetworkPropertyChanged;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TNetworkListManager
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TNetworkListManagerProperties = class(TPersistent)
  private
    FServer:    TNetworkListManager;
    function    GetDefaultInterface: INetworkListManager;
    constructor Create(AServer: TNetworkListManager);
  protected
    function Get_IsConnectedToInternet: WordBool;
    function Get_IsConnected: WordBool;
  public
    property DefaultInterface: INetworkListManager read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoNetworkListManager.Create: INetworkListManager;
begin
  Result := CreateComObject(CLASS_NetworkListManager) as INetworkListManager;
end;

class function CoNetworkListManager.CreateRemote(const MachineName: string): INetworkListManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NetworkListManager) as INetworkListManager;
end;

procedure TNetworkListManager.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{DCB00C01-570F-4A9B-8D69-199FDBA5723B}';
    IntfIID:   '{DCB00000-570F-4A9B-8D69-199FDBA5723B}';
    EventIID:  '{DCB00004-570F-4A9B-8D69-199FDBA5723B}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TNetworkListManager.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as INetworkListManager;
  end;
end;

procedure TNetworkListManager.ConnectTo(svrIntf: INetworkListManager);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TNetworkListManager.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TNetworkListManager.GetDefaultInterface: INetworkListManager;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TNetworkListManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TNetworkListManagerProperties.Create(Self);
{$ENDIF}
end;

destructor TNetworkListManager.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TNetworkListManager.GetServerProperties: TNetworkListManagerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TNetworkListManager.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
(*{The DispID for this method is DISPID_UNKNOWN!?. }
    -1: if Assigned(FOnNetworkAdded) then
         FOnNetworkAdded(Self, Params[0] { ??TGUID OleVariant});
*)
(*{The DispID for this method is DISPID_UNKNOWN!?. }
    -1: if Assigned(FOnNetworkDeleted) then
         FOnNetworkDeleted(Self, Params[0] { ??TGUID OleVariant});
*)
(*{The DispID for this method is DISPID_UNKNOWN!?. }
    -1: if Assigned(FOnNetworkConnectivityChanged) then
         FOnNetworkConnectivityChanged(Self,
                                       Params[0] { ??TGUID OleVariant},
                                       Params[1] {NLM_CONNECTIVITY});
*)
(*{The DispID for this method is DISPID_UNKNOWN!?. }
    -1: if Assigned(FOnNetworkPropertyChanged) then
         FOnNetworkPropertyChanged(Self,
                                   Params[0] { ??TGUID OleVariant},
                                   Params[1] {NLM_NETWORK_PROPERTY_CHANGE});
*)
  end; {case DispID}
end;

function TNetworkListManager.Get_IsConnectedToInternet: WordBool;
begin
    Result := DefaultInterface.IsConnectedToInternet;
end;

function TNetworkListManager.Get_IsConnected: WordBool;
begin
    Result := DefaultInterface.IsConnected;
end;

function TNetworkListManager.GetNetworks(Flags: NLM_ENUM_NETWORK): IEnumNetworks;
begin
  Result := DefaultInterface.GetNetworks(Flags);
end;

function TNetworkListManager.GetNetwork(gdNetworkId: TGUID): INetwork;
begin
  Result := DefaultInterface.GetNetwork(gdNetworkId);
end;

function TNetworkListManager.GetNetworkConnections: IEnumNetworkConnections;
begin
  Result := DefaultInterface.GetNetworkConnections;
end;

function TNetworkListManager.GetNetworkConnection(gdNetworkConnectionId: TGUID): INetworkConnection;
begin
  Result := DefaultInterface.GetNetworkConnection(gdNetworkConnectionId);
end;

function TNetworkListManager.GetConnectivity: NLM_CONNECTIVITY;
begin
  Result := DefaultInterface.GetConnectivity;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TNetworkListManagerProperties.Create(AServer: TNetworkListManager);
begin
  inherited Create;
  FServer := AServer;
end;

function TNetworkListManagerProperties.GetDefaultInterface: INetworkListManager;
begin
  Result := FServer.DefaultInterface;
end;

function TNetworkListManagerProperties.Get_IsConnectedToInternet: WordBool;
begin
    Result := DefaultInterface.IsConnectedToInternet;
end;

function TNetworkListManagerProperties.Get_IsConnected: WordBool;
begin
    Result := DefaultInterface.IsConnected;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TNetworkListManager]);
end;

end.
