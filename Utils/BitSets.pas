unit BitSets;

interface

type
 //Simple boolean bit set class
  TFlagSet = record
    Bits: array of cardinal;
    procedure Reset; inline;
    procedure SetSize(const Value: integer); inline;
    function GetItem(const Index: integer): boolean; inline;
    procedure SetItem(const Index: integer; const Value: boolean); inline;
    property Size: integer write SetSize; //no GetSize because we don't store it
    property Item[const Index: integer]: boolean read GetItem write SetItem; default;
  const
    bitno = sizeof(integer)*8;
  end;

implementation

procedure TFlagSet.Reset;
var i: integer;
begin
  for i := 0 to Length(Bits)-1 do
    Bits[i] := 0;
end;

procedure TFlagSet.SetSize(const Value: integer);
begin
  SetLength(Bits, (Value div bitno) + 1);
end;

function TFlagSet.GetItem(const Index: integer): boolean;
begin
  Result := Bits[Index div bitno] and (1 shl ((Index mod bitno)-1)) <> 0;
end;

procedure TFlagSet.SetItem(const Index: integer; const Value: boolean);
begin
  if Value then
    Bits[Index div bitno] := Bits[Index div bitno] or (1 shl ((Index mod bitno)-1))
  else
    Bits[Index div bitno] := Bits[Index div bitno] and not (1 shl ((Index mod bitno)-1));
end;

end.
