{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfDesignSQLItemsU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfParamsU, wfResourceStrings, wfClasses, db, LazUTF8;

type
  { TwfDesignSQLItem }

  TwfDesignSQLItem = class(TCollectionItem)
    protected
      function GetDisplayName: string; override;
    private
      fDescription: TStrings;
      fName: string;
      fParseSQL: Boolean;
      fSQL: TStrings;
      fParams: TwfParams;
      fTableName: string;

      function GetParams: TwfParams;
      function GetSQL: TStrings;
      procedure SetParams(aValue: TwfParams);
      procedure SetSQL(aValue: TStrings);

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

    published
      property Name: string read fName write fName;
      // The name of the table (reference)
      property TableName: string read fTableName write fTableName;
      property SQL: TStrings read GetSQL write SetSQL;
      property ParseSQL : Boolean read fParseSQL write fParseSQL default true;
      property Params: TwfParams read GetParams write SetParams;
      property Description: TStrings read fDescription write fDescription;

 end;

  { TwfDesignSQLItems }

  TwfDesignSQLItems = class(TOwnedCollection)

  private

  protected

  public
    function ItemByName(aItemName: string):TwfDesignSQLItem;
    function GetSQL(aItemName: string): string;
    function GetTableName(aItemName: string): string;

  end;

implementation


{ TwfDesignSQLItems }

function TwfDesignSQLItems.ItemByName(aItemName: string): TwfDesignSQLItem;
var
  i: Integer;
begin
Result:= nil;

for i:=0 to Count-1 do
 if UTF8UpperCase(TwfDesignSQLItem(Items[i]).Name) = UTF8UpperCase(aItemName) then
   begin
     Result:= TwfDesignSQLItem(Items[i]);
     Break;
   end;

if not Assigned(Result) then
  raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));
end;

function TwfDesignSQLItems.GetSQL(aItemName: string): string;
begin
  Result:= ItemByName(aItemName).SQL.Text;
end;

function TwfDesignSQLItems.GetTableName(aItemName: string): string;
begin
  Result:= ItemByName(aItemName).TableName;
end;

{ TwfDesignSQLItem }

function TwfDesignSQLItem.GetDisplayName: string;
begin
if Length(fName)>0 then
  Result:= fName
else
  Result:=inherited GetDisplayName;
end;

function TwfDesignSQLItem.GetSQL: TStrings;
begin
  Result:= fSQL;
end;

function TwfDesignSQLItem.GetParams: TwfParams;
begin
  Result:= fParams;
end;

procedure TwfDesignSQLItem.SetParams(aValue: TwfParams);
begin
  fParams.Assign(aValue);
end;

procedure TwfDesignSQLItem.SetSQL(aValue: TStrings);
begin
  fSQL.Assign(aValue);

  if ParseSQL then
    begin
      Params.Clear;
      Params.ParseSQL(fSQL.Text,true);
    end;
end;

constructor TwfDesignSQLItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fSQL:= TStringList.Create();
  fDescription:= TStringList.Create();
  fParams:= TwfParams.Create(self);
  fParseSQL:= true;
end;

destructor TwfDesignSQLItem.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fSQL);
  FreeAndNil(fDescription);
  FreeAndNil(fParams);
end;

end.

