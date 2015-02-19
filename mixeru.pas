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
    FVolume: integer;
    FMute: boolean;
    FDescription: string;
    function getState: integer;
    function getStateString: string;
  public
    property Description: string read FDescription;
    property State: integer read getState;
    property StateString: string read getStateString;
    class function CUpdate: integer;
    constructor Create;
    procedure Update;
  end;

var Mixer: TMixer;

implementation
//------------------------------------------------------------------------------
class function TMixer.CUpdate: integer;
begin
  if not assigned(Mixer) then Mixer := TMixer.Create;
  Mixer.Update;
  result := Mixer.State;
end;
//------------------------------------------------------------------------------
constructor TMixer.Create;
begin
  FReady := false;
  FMute := false;
  FVolume := -1;
  if SUCCEEDED(CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, FmmDevEnum)) then
    if SUCCEEDED(FmmDevEnum.GetDefaultAudioEndpoint(eRender, eMultimedia, FmmDev)) then
      if SUCCEEDED(FmmDev.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, FmmEndpoint)) then FReady := true;
end;
//------------------------------------------------------------------------------
procedure TMixer.Update;
const
  PKEY_Device_DeviceDesc: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 2);
  //PKEY_Device_FriendlyName: PROPERTYKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 14);
  //PKEY_DeviceInterface_FriendlyName: PROPERTYKEY = (fmtid: '{b3f8fa53-0004-438e-9003-51a46e139bfc}'; pid: 6);
var
  oldvol: integer;
  oldmute: boolean;
  vol: Single;
  props: IPropertyStore;
  prop: PROPVARIANT;
begin
  oldvol := FVolume;
  oldmute := FMute;
  FMute := false;
  FVolume := -1;

  if FReady then
  begin
    if SUCCEEDED(FmmEndpoint.GetMasterVolumeLevelScalar(vol)) then FVolume := round(vol * 100);
    if not SUCCEEDED(FmmEndpoint.GetMute(FMute)) then FMute := false;
  end;

  if (oldvol <> FVolume) or (oldmute <> FMute) then
  begin
    FDescription := '--';
    if FVolume >= 0 then FDescription := inttostr(FVolume) + '%';
    if SUCCEEDED(FmmDev.OpenPropertyStore(STGM_READ, props)) then
      if SUCCEEDED(props.GetValue(@PKEY_Device_DeviceDesc, prop)) then
        FDescription := string(PWideChar(prop.pwszVal)) + ': ' + FDescription;
  end;
end;
//------------------------------------------------------------------------------
function TMixer.getState: integer;
begin
  result := FVolume;
  if FMute then result := -2;
end;
//------------------------------------------------------------------------------
function TMixer.getStateString: string;
begin
  result := 'muted';
  if (State >= 0) and (State < 33) then result := 'low';
  if (State >= 33) and (State < 66) then result := 'medium';
  if (State >= 66) and (State <= 100) then result := 'high';
end;
//------------------------------------------------------------------------------
end.

