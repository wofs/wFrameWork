unit wfCustomOrderByListU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, LazUTF8;

type

  { TwfOrderBy }

  TwfOrderBy = class
    Field: string;
    Direction: TDirection;
    constructor Create(aField: string; aDirection: TDirection);
  end;

  { TwfCustomOrderByList }

  TwfCustomOrderByList = class(TList)
  private
    function GetAsString: string;
    function IndexOf(aField: string): int64;

  public
    destructor Destroy; override;

    procedure ClearItemsAndData;
    procedure SetItem(const uField: string; const aDirection: TDirection);
    function GetItem(const uField: string): TDirection;
    procedure DelItem(const uField: string);

    property AsString: string read GetAsString;
  end;

implementation

{ TwfOrderBy }

constructor TwfOrderBy.Create(aField: string; aDirection: TDirection);
begin
  Field:= aField;
  Direction:= aDirection;
end;

{ TwfCustomOrderByList }

procedure TwfCustomOrderByList.ClearItemsAndData;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
      TwfOrderBy(Items[i]).Free;

  Clear;
end;

function TwfCustomOrderByList.GetAsString: string;
var
  i: Integer;
begin
  if not Assigned(self) then exit; //=>

  Result:= '';
  for i:=0 to Count-1 do
    begin
      if (i>0) then
        Result:= Result+', ';
        case TwfOrderBy(Items[i]).Direction of
          dASC: Result:= Result+TwfOrderBy(Items[i]).Field+' ASC';
          dDESC: Result:= Result+TwfOrderBy(Items[i]).Field+' DESC';
        end;
    end;
end;

function TwfCustomOrderByList.IndexOf(aField: string): int64;
var
  i: Integer;
begin
  Result:= -1;
  aField:= UTF8UpperCase(aField);

  for i:=0 to Count-1 do
    if TwfOrderBy(Items[i]).Field = aField then
    begin
      Result:= i;
      Break;
    end;
end;

destructor TwfCustomOrderByList.Destroy;
begin
  ClearItemsAndData;
  inherited Destroy;
end;

procedure TwfCustomOrderByList.SetItem(const uField: string;
  const aDirection: TDirection);
var
  aIndex: Int64;
  aField: String;
begin
  aField:= UTF8UpperCase(uField);

  aIndex:= IndexOf(aField);
  if aIndex>-1 then
  begin
    TwfOrderBy(Items[aIndex]).Direction:= aDirection;
  end
  else
    Add(TwfOrderBy.Create(aField, aDirection));

end;

function TwfCustomOrderByList.GetItem(const uField: string): TDirection;
var
  aIndex: Int64;
  aField: String;
begin
  aField:= UTF8UpperCase(uField);
  Result:= dNone;

  aIndex:= IndexOf(aField);
  if aIndex>-1 then
    Result:= TwfOrderBy(Items[aIndex]).Direction;
end;

procedure TwfCustomOrderByList.DelItem(const uField: string);
var
  aIndex: Int64;
begin
  aIndex:= IndexOf(uField);
  if aIndex>-1 then
  begin
    TwfOrderBy(Items[aIndex]).Free;
    Delete(aIndex);
  end;
end;

end.

