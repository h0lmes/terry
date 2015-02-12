unit mixeru;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, ActiveX, ShlObj, mmdevapi_tlb;

type
  TMixer = class
  private
    FReady: boolean;
    FmmDev: IMMDevice;
    FmmDevEnum: IMMDeviceEnumerator;
    FmmEndpoint: IMMAudioEndpointVolume;
  public
    constructor Create;
    function getMute: boolean;
    function getVolume: integer;
    procedure setVolume(Value: integer);
    procedure setMute(Value: boolean);
    function getVolumeString: string;
    function getVolumeState: integer;
    function getVolumeStateString(state: integer): string;
  end;

var mixer: TMixer;

implementation
//------------------------------------------------------------------------------
constructor TMixer.Create;
begin
  FReady := false;
  if SUCCEEDED(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, FmmDevEnum)) then
    if SUCCEEDED(FmmDevEnum.GetDefaultAudioEndpoint(eRender, eMultimedia, FmmDev)) then
      if SUCCEEDED(FmmDev.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, FmmEndpoint)) then FReady := true;
end;
//------------------------------------------------------------------------------
function TMixer.getMute: boolean;
begin
  Result := false;
  if FReady then
    if not SUCCEEDED(FmmEndpoint.GetMute(Result)) then Result := false;
end;
//------------------------------------------------------------------------------
function TMixer.getVolume: integer;
var
  vol: Single;
begin
  result := -1;
  if FReady then
  begin
    if SUCCEEDED(FmmEndpoint.GetMasterVolumeLevelScalar(vol)) then result := round(vol * 100);
  end;
end;
//------------------------------------------------------------------------------
function TMixer.getVolumeString: string;
const
  PKEY_Device_DeviceDesc: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 2);
  //PKEY_Device_FriendlyName: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 14);
  //PKEY_DeviceInterface_FriendlyName: PROPERTYKEY = (fmtid: '{b3f8fa53-0004-438e-9003-51a46e139bfc}'; pid: 6);
var
  vol: integer;
  props: IPropertyStore;
  prop: PROPVARIANT;
begin
  result := '--';
  vol := getVolume;
  if vol >= 0 then result := inttostr(vol) + '%';

  if SUCCEEDED(FmmDev.OpenPropertyStore(STGM_READ, props)) then
    if SUCCEEDED(props.GetValue(@PKEY_Device_DeviceDesc, prop)) then
      result := string(PWideChar(prop.pwszVal)) + ': ' + result;
end;
//------------------------------------------------------------------------------
function TMixer.getVolumeState: integer;
begin
  result := getVolume;
  if getMute then result := -2;
end;
//------------------------------------------------------------------------------
function TMixer.getVolumeStateString(state: integer): string;
begin
  result := 'muted';
  if (state >= 0) and (state < 33) then result := 'low';
  if (state >= 33) and (state < 66) then result := 'medium';
  if (state >= 66) and (state <= 100) then result := 'high';
end;
//------------------------------------------------------------------------------
procedure TMixer.setMute(Value: boolean);
begin
  //if FReady then FmmEndpoint.SetMute(Value, nil);
end;
//------------------------------------------------------------------------------
procedure TMixer.setVolume(Value: integer);
var
  fValue: Single;
begin
  if FReady then
  begin
    if value < 0 then value := 0;
    if value > 100 then value := 100;
    fValue := Value / 100;
    //FmmEndpoint.SetMasterVolumeLevelScalar(fValue, nil);
  end;
end;
//------------------------------------------------------------------------------
end.

