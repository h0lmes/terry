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
  for i := 0 to buf.size - 1 do buf.buffer[i] := avalue;
end;

procedure CIBuffer_Put(var buf: TCIBuffer; avalue: integer);
begin
  inc(buf.tail);
  if buf.tail >= buf.size then buf.tail := 0;
  buf.buffer[buf.tail] := avalue;
end;

procedure CIBuffer_Get(var buf: TCIBuffer; aindex: integer; out avalue: integer);
var
  i: integer;
begin
  i := buf.tail + 1;
  if i >= buf.size then dec(i, buf.size);
  i := i + aindex;
  if i >= buf.size then dec(i, buf.size);
  avalue := buf.buffer[i];
end;

end.

