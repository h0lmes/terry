unit adCpuUsage;

// This source has been altered.

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CPU Usage Measurement routines for Delphi and C++ Builder

Author:       Alexey A. Dynnikov
EMail:        aldyn@chat.ru
WebSite:      http://www.aldyn.ru/
Support:      Use the e-mail aldyn@chat.ru
                          or support@aldyn.ru

Creation:     Jul 8, 2000
Version:      1.02

Legal issues: Copyright (C) 2000 by Alexey A. Dynnikov <aldyn@chat.ru>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
USAGE:

1. Include this unit into project.

2. Call GetCPUCount to obtain the numbr of processors in the system

3. Each time you need to know the value of CPU usage call the CollectCPUData
   to refresh the CPU usage information. Then call the GetCPUUsage to obtain
   the CPU usage for given processor. Note that succesive calls of GetCPUUsage
   without calling CollectCPUData will return the same CPU usage value.

Example:

procedure TTestForm.TimerTimer(Sender: TObject);
var i: Integer;
begin
    CollectCPUData; // Get the data for all processors

    for i:=0 to GetCPUCount-1 do // Show data for each processor
        MInfo.Lines[i]:=Format('CPU #%d - %5.2f%%',[i,GetCPUUsage(i)*100]);
end;
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

interface

uses
    Windows, SysUtils;

// Call CollectCPUData to refresh information about CPU usage
procedure CollectCPUData;

// Call it to obtain the number of CPU's in the system
function GetCPUCount: Integer;

// Call it to obtain the % of usage for given CPU
function GetCPUUsage(Index: Integer): Double;

function getError: integer;

implementation

{$ifndef ver110}

    {$ifndef ver90}
    {$ifndef ver100}
    {$define UseInt64}
    {$endif}
    {$endif}


    {$ifdef UseInt64}
    type TInt64 = Int64;
    {$else}
    type TInt64 = Comp;
    {$endif}

{$else}

    type TInt64 = TLargeInteger;

{$endif}

type
    PInt64 = ^TInt64;

type
    TPERF_DATA_BLOCK = record
        Signature : array[0..4 - 1] of WCHAR;
        LittleEndian : DWORD;
        Version : DWORD;
        Revision : DWORD;
        TotalByteLength : DWORD;
        HeaderLength : DWORD;
        NumObjectTypes : DWORD;
        DefaultObject : Longint;
        SystemTime : TSystemTime;
        Reserved: DWORD;
        PerfTime : TInt64;
        PerfFreq : TInt64;
        PerfTime100nSec : TInt64;
        SystemNameLength : DWORD;
        SystemNameOffset : DWORD;
    end;

    PPERF_DATA_BLOCK = ^TPERF_DATA_BLOCK;

    TPERF_OBJECT_TYPE = record
        TotalByteLength : DWORD;
        DefinitionLength : DWORD;
        HeaderLength : DWORD;
        ObjectNameTitleIndex : DWORD;
        ObjectNameTitle : LPWSTR;
        ObjectHelpTitleIndex : DWORD;
        ObjectHelpTitle : LPWSTR;
        DetailLevel : DWORD;
        NumCounters : DWORD;
        DefaultCounter : DWORD;
        NumInstances : DWORD;
        CodePage : DWORD;
        PerfTime : TInt64;
        PerfFreq : TInt64;
    end;

    PPERF_OBJECT_TYPE = ^TPERF_OBJECT_TYPE;

type
    TPERF_COUNTER_DEFINITION = record
        ByteLength : DWORD;
        CounterNameTitleIndex : DWORD;
        CounterNameTitle : LPWSTR;
        CounterHelpTitleIndex : DWORD;
        CounterHelpTitle : LPWSTR;
        DefaultScale : Longint;
        DetailLevel : DWORD;
        CounterType : DWORD;
        CounterSize : DWORD;
        CounterOffset : DWORD;
    end;

    PPERF_COUNTER_DEFINITION = ^TPERF_COUNTER_DEFINITION;

    TPERF_COUNTER_BLOCK = record
        ByteLength : DWORD;
    end;

    PPERF_COUNTER_BLOCK = ^TPERF_COUNTER_BLOCK;

    TPERF_INSTANCE_DEFINITION = record
        ByteLength : DWORD;
        ParentObjectTitleIndex : DWORD;
        ParentObjectInstance : DWORD;
        UniqueID : DWORD;
        NameOffset : DWORD;
        NameLength : DWORD;
    end;

    PPERF_INSTANCE_DEFINITION = ^TPERF_INSTANCE_DEFINITION;

//------------------------------------------------------------------------------
{$ifndef ver110}
type
    TInt64F = TInt64;
{$else}
type
    TInt64F = Extended;
{$endif}

{$ifdef ver110}
function FInt64(Value: TInt64): TInt64F;
function Int64D(Value: DWORD): TInt64;
{$else}
type
    FInt64 = TInt64F;
    Int64D = TInt64;
{$endif}

{$ifdef ver110}
function FInt64(Value: TInt64): TInt64F;
var V: TInt64;
begin
    if (Value.HighPart and $80000000) = 0 then // positive value
    begin
        result:=Value.HighPart;
        result:=result*$10000*$10000;
        result:=result+Value.LowPart;
    end else
    begin
        V.HighPart:=Value.HighPart xor $FFFFFFFF;
        V.LowPart:=Value.LowPart xor $FFFFFFFF;
        result:= -1 - FInt64(V);
    end;
end;

function Int64D(Value: DWORD): TInt64;
begin
    result.LowPart:=Value;
    result.HighPart := 0; // positive only
end;
{$endif}

//------------------------------------------------------------------------------

const
    Processor_IDX_Str = '238';
    Processor_IDX = 238;
    CPUUsageIDX = 6;

type
    AInt64F = array[0..$FFFF] of TInt64F;
    PAInt64F = ^AInt64F;

var
    _PerfData : PPERF_DATA_BLOCK;
    _BufferSize: Integer;
    _POT : PPERF_OBJECT_TYPE;
    _PCD: PPerf_Counter_Definition;
    _ProcessorsCount: Integer;
    _Counters: PAInt64F;
    _PrevCounters: PAInt64F;
    _SysTime: TInt64F;
    _PrevSysTime: TInt64F;
    _Error: integer;


//------------------------------------------------------------------------------
function GetCPUCount: Integer;
begin
    if _ProcessorsCount < 0 then CollectCPUData;
    result:=_ProcessorsCount;
end;
//------------------------------------------------------------------------------
function GetCPUUsage(Index: Integer): Double;
begin
    if _ProcessorsCount < 0 then CollectCPUData;

    if (Index >= _ProcessorsCount) or (Index < 0) then
        raise Exception.Create('CPU index out of bounds');

    if _PrevSysTime = _SysTime then
        result := 0
    else
        result := 1 - (_Counters[index] - _PrevCounters[index]) / (_SysTime - _PrevSysTime);
end;
//------------------------------------------------------------------------------
procedure CollectCPUData;
var
    BS: DWORD;
    i: Integer;
    _PCB_Instance: PPERF_COUNTER_BLOCK;
    _PID_Instance: PPERF_INSTANCE_DEFINITION;
    ST: TFileTime;
begin
    _Error := 0;

    BS := _BufferSize;
    while RegQueryValueExW( HKEY_PERFORMANCE_DATA, '238', nil, nil, PByte(_PerfData), @BS ) = ERROR_MORE_DATA do
    begin
        // Get a buffer that is big enough.
        inc(_BufferSize, $1000);
        BS := _BufferSize;
        ReallocMem( _PerfData, _BufferSize );
    end;
    _Error := 1;

    // Locate the performance object
    _POT := PPERF_OBJECT_TYPE(PtrUInt(_PerfData) + _PerfData.HeaderLength);
    for i := 1 to _PerfData.NumObjectTypes do
    begin
        if _POT.ObjectNameTitleIndex = Processor_IDX then Break;
        _POT := PPERF_OBJECT_TYPE(PtrUInt(_POT) + _POT.TotalByteLength);
    end;
    _Error := 2;

    // Check for success
    if _POT.ObjectNameTitleIndex <> Processor_IDX then
    begin
        _Error := 3;
        raise Exception.Create('Unable to locate the "Processor" performance object');
    end;

    if _ProcessorsCount < 0 then
    begin
        _ProcessorsCount:=_POT.NumInstances;
        GetMem(_Counters,     _ProcessorsCount * SizeOf(TInt64));
        GetMem(_PrevCounters, _ProcessorsCount * SizeOf(TInt64));
    end;
    _Error := 4;

    // Locate the "% CPU usage" counter definition
    _PCD := PPERF_COUNTER_DEFINITION(PtrUInt(_POT) + _POT.HeaderLength);
    for i := 1 to _POT.NumCounters do
    begin
        if _PCD.CounterNameTitleIndex = CPUUsageIDX then break;
        _PCD := PPERF_COUNTER_DEFINITION(PtrUInt(_PCD) + _PCD.ByteLength);
    end;
    _Error := 5;

    // Check for success
    if _PCD.CounterNameTitleIndex <> CPUUsageIDX then
    begin
        _Error := 6;
        raise Exception.Create('Unable to locate the "% of CPU usage" performance counter');
    end;

    // Collecting coutners
    _PID_Instance := PPERF_INSTANCE_DEFINITION(PtrUInt(_POT) + _POT.DefinitionLength);
    for i := 0 to _ProcessorsCount-1 do
    begin
        _PCB_Instance    := PPERF_COUNTER_BLOCK(PtrUInt(_PID_Instance) + _PID_Instance.ByteLength );

        _PrevCounters[i] :=_Counters[i];
        _Counters[i]     := FInt64(PInt64(PtrUInt(_PCB_Instance) + _PCD.CounterOffset)^);

        _PID_Instance    := PPERF_INSTANCE_DEFINITION(PtrUInt(_PCB_Instance) + _PCB_Instance.ByteLength);
    end;
    _Error := 7;

    _PrevSysTime := _SysTime;
    SystemTimeToFileTime(_PerfData.SystemTime, ST);
    _SysTime := FInt64(TInt64(ST));
    _Error := 8;
end;
//------------------------------------------------------------------------------
function getError: integer;
begin
    result := _Error;
end;
//------------------------------------------------------------------------------
initialization
    _ProcessorsCount:= -1;
    _BufferSize:= $2000;
    _PerfData := AllocMem(_BufferSize);
finalization
    FreeMem(_PerfData);
end.

