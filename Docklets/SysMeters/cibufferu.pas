unit cibufferu;

{$mode delphi}

interface
uses
  Classes, SysUtils;

// circular buffer of integers //
// procedural implementation //

type
  TCIBuffer = record
    buffer: array [0..31] of integer;
    size: integer;
    count: integer;
    tail: integer;
  end;

procedure CIBuffer_Init(var buf: TCIBuffer; asize, avalue: integer);
procedure CIBuffer_Put(var buf: TCIBuffer; avalue: integer);
procedure CIBuffer_Get(var buf: TCIBuffer; aindex: integer; out avalue: integer);

implementation

procedure CIBuffer_Init(var buf: TCIBuffer; asize, avalue: integer);
var
  i: integer;
begin
  buf.size := asize;
  buf.tail := asize - 1;
  buf.count := 0;
  for i := 0 to buf.size - 1 do buf.buffer[i] := avalue;
end;

procedure CIBuffer_Put(var buf: TCIBuffer; avalue: integer);
begin
  inc(buf.tail);
  if buf.tail >= buf.size then buf.tail := 0;
  buf.buffer[buf.tail] := avalue;
  if buf.count < buf.size then inc(buf.count);
end;

procedure CIBuffer_Get(var buf: TCIBuffer; aindex: integer; out avalue: integer);
var
  i: integer;
begin
  if aindex < 0 then aindex := 0;
  if aindex >= buf.size then aindex := buf.size - 1;

  i := buf.tail + 1 + aindex;
  if i >= buf.size then dec(i, buf.size);
  avalue := buf.buffer[i];
end;

end.

