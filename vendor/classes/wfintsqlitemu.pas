unit wfIntSQLItemU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfParamsU, wfTypes, db;

type
  { TwfIntSQLItem }

  TwfIntSQLItem = class
      Name: string;
      Value: string;
      Params: TwfParams;
      ItemType: TSQLItemType;
    public
      constructor Create(aName, aValue: string; var aParams: TwfParams; const uType: TSQLItemType = stUnknown);
  end;

implementation
{ TwfIntSQLItem }

constructor TwfIntSQLItem.Create(aName, aValue: string; var aParams: TwfParams;
  const uType: TSQLItemType);
var
  i: Integer;
  aParam: TParam;
begin
  Name:= aName;
  Value:= aValue;
  ItemType:= uType;

  FreeAndNil(Params);

  Params:= TwfParams.Create(nil);

    for i:= 0 to aParams.Count-1 do
      begin
        aParam:= TParam.Create(Params, ptInput);
        aParam.Assign(aParams.Items[i]);
      end;
end;
end.

