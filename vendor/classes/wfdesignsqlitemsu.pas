unit wfDesignSQLItemsU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfParamsU, wfResourceStrings, LazUTF8;

type
  { TwfDesignSQLItem }

  TwfDesignSQLItem = class(TCollectionItem)
    protected
      function GetDisplayName: string; override;
    private
      fDescription: string;
      fName: string;
      fSQL: TStrings;
      fParams: TwfParams;
      function GetParams: TwfParams;
      function GetSQL: TStrings;
      procedure SetParams(aValue: TwfParams);
      procedure SetSQL(aValue: TStrings);

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

    published
      property Name: string read fName write fName;
      property SQL: TStrings read GetSQL write SetSQL;
      property Params: TwfParams read GetParams write SetParams;
      property Description: string read fDescription write fDescription;
  end;

  { TwfDesignSQLItems }

  TwfDesignSQLItems = class(TOwnedCollection)

  private

  protected

  public
    function ItemByName(aName: string):TwfDesignSQLItem;
    function ItemByNameSQL(aName: string): string;
  end;

implementation

{ TwfDesignSQLItems }

function TwfDesignSQLItems.ItemByName(aName: string): TwfDesignSQLItem;
var
  i: Integer;
begin
Result:= nil;

for i:=0 to Count-1 do
 if UTF8UpperCase(TwfDesignSQLItem(Items[i]).Name) = UTF8UpperCase(aName) then
   begin
     Result:= TwfDesignSQLItem(Items[i]);
     Break;
   end;

if not Assigned(Result) then
  raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));
end;

function TwfDesignSQLItems.ItemByNameSQL(aName: string): string;
begin
  Result:= ItemByName(aName).SQL.Text;
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
end;

constructor TwfDesignSQLItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fSQL:= TStringList.Create();
  fParams:= TwfParams.Create(self);
end;

destructor TwfDesignSQLItem.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fSQL);
  FreeAndNil(fParams);
end;

end.
