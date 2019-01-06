{
This file is part of wfFrameWork.

 -= Vendor/Classes =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfClasses;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, fgl, wfTypes, wfFunc, LazUTF8, Buttons, db;

type

   //{ TEntity }
   //
   //TEntity = class (TObject)
   //
   //private
   //  fCreateTable: ArrayOfString;
   //  fDel: string;
   //  fDropTable: ArrayOfString;
   //  fFind: string;
   //  fItem: string;
   //  fList: string;
   //  fNew: string;
   //  published
   //    property CreateTable: ArrayOfString read fCreateTable write fCreateTable;
   //    property DropTable: ArrayOfString read fDropTable write fDropTable;
   //
   //    property List: string read fList write fList;
   //    property Item: string read fItem write fItem;
   //    property Find: string read fFind write fFind;
   //
   //    property New: string read fNew write fNew;
   //    property Del: string read fDel write fDel;
   //end;

   { TwfParams }

   TwfParams = class (TParams)
     private
       fFreeAfterUse: boolean;
     public
       property FreeAfterUse: boolean read fFreeAfterUse;
       constructor Create(AOwner: TPersistent; const aFreeAfterUse: boolean = false);
   end;

   { TwfBaseField }

   TwfBaseField = record
     Value: variant;
   end;

   TwfBaseFields = array of TwfBaseField;

   TwfBaseRow = record
     Index: LargeInt;
     Fields: TwfBaseFields;
   end;

   TwfBaseRows = array of TwfBaseRow;

   TwfField = record
     Name: string;
     DataType: TFieldType;
   end;

   TwfFields = array of TwfField;

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

   { TwfSQLItem }

   TwfSQLItem = class
       Name: string;
       Value: string;
       Params: TwfParams;
       ItemType: TSQLItemType;
     public
       constructor Create(aName, aValue: string; var aParams: TwfParams; const uType: TSQLItemType = stUnknown);
   end;


   { TwfCustomSQLItemList }

   TwfCustomSQLItemList = class(TList)
   private
     fonLog: TTextEvent;
     fSearchFieldsContaining: string;
     fSearchFieldsIn: string;
     fSearchFieldsLike: string;
     procedure ClearData;
     function ConvertCommaStringToQuotedStr(aCommaString: string): string;
     function GetAsParams: TwfParams;
     function GetAsString: string;
     function GetParams: TwfParams;
     procedure Log(aText: string);

   public
     destructor Destroy; override;

     procedure SetItem(const uName: string; const aValue: string;
       var aParams: TwfParams; const uType:TSQLItemType = stUnknown);
     procedure GetItem(const uName: string; out aValue: string; out aParams: TwfParams);
     procedure DelItem(const aIndex: integer);
     function IndexOf(aName: string): int64;

     procedure GenerateSearchList(aSearchText: string; aSearchType: TSQLItemType; const uNameSearch: string = ''; const uFieldList: string = '');

     property AsString: string read GetAsString;
     property AsParams: TwfParams read GetAsParams;
     property Params: TwfParams read GetParams;

     property SearchFieldsContaining: string read fSearchFieldsContaining write fSearchFieldsContaining;
     property SearchFieldsLike: string read fSearchFieldsLike write fSearchFieldsLike;
     property SearchFieldsIn: string read fSearchFieldsIn write fSearchFieldsIn;

     property onLog: TTextEvent read fonLog write fonLog;
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

{ TwfParams }

constructor TwfParams.Create(AOwner: TPersistent; const aFreeAfterUse: boolean);
begin
  inherited Create(AOwner);
  fFreeAfterUse:= aFreeAfterUse;
end;


{ TwfSQLItem }

constructor TwfSQLItem.Create(aName, aValue: string; var aParams: TwfParams;
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

{ TwfCustomSQLItemList }
function TwfCustomSQLItemList.IndexOf(aName: string):int64;
var
  i: Integer;
begin
  Result:= -1;
  aName:= UTF8UpperCase(aName);

  for i:=0 to Count-1 do
    if TwfSQLItem(Items[i]).Name = aName then
    begin
      Result:= i;
      Break;
    end;
end;

function TwfCustomSQLItemList.ConvertCommaStringToQuotedStr(aCommaString: string): string;
var
  aList: TStringList;
  aTest: Longint;
  i: Integer;
begin
  aList:= TStringList.Create;
  try
    aList.Delimiter:=',';
    aList.DelimitedText:=aCommaString;
    Result:='';
    for i:=0 to aList.Count-1 do
    begin
      if i>0 then Result:= Result+',';
        if TryStrToInt(aList.Strings[i],aTest) then
          Result:= Result+aList.Strings[i]
        else
          Result:= Result+QuotedStr(aList.Strings[i]);
    end;
  finally
    FreeAndNil(aList);
  end;

end;

procedure TwfCustomSQLItemList.GenerateSearchList(aSearchText: string;
  aSearchType: TSQLItemType; const uNameSearch: string;
  const uFieldList: string);
var
  aSearchStrings, aFieldsList: TStringList;
  i, iField: Integer;
  aParams: TwfParams;
  aParamName, aField, aSearchTemp, aNameSearch: String;

  procedure WriteParam;
  var
    aParam: TParam;
  begin
    aParam:= TParam.Create(aParams, ptInput);
    aParam.Name:=aParamName+IntToStr(i);
    aParam.Value:= aSearchStrings.Strings[i];
  end;

begin
  aSearchStrings:= TStringList.Create;
  aParams:= TwfParams.Create(aSearchStrings);
  aFieldsList:= TStringList.Create;

  if Length(uNameSearch) = 0 then
  begin
    case aSearchType of
      stContaining  : aNameSearch:= 'SEARCH_CONTAINING';
      stLike        : aNameSearch:= 'SEARCH_LIKE';
      stIn          : aNameSearch:= 'SEARCH_IN';
    end;
  end else
    aNameSearch:= uNameSearch;

  try

    aFieldsList.Delimiter:=',';
    case aSearchType of
      stContaining  :
                  begin
                    if Length(uFieldList)>0 then
                      aFieldsList.DelimitedText:= uFieldList
                    else
                      aFieldsList.DelimitedText:= fSearchFieldsContaining;
                    aSearchStrings.CommaText:= aSearchText;
                  end;
      stLike        :
                  begin
                    if Length(uFieldList)>0 then
                      aFieldsList.DelimitedText:= uFieldList
                    else
                      aFieldsList.DelimitedText:= fSearchFieldsLike;
                    aSearchStrings.CommaText:= aSearchText;
                  end;
      stIn          :
                  begin
                    if Length(uFieldList)>0 then
                      aFieldsList.DelimitedText:= uFieldList
                    else
                      aFieldsList.DelimitedText:= fSearchFieldsIn;

                    aSearchStrings.Text:= aSearchText;
                  end;
    end;

    if (aSearchStrings.Count = 0) or (aFieldsList.Count = 0) then
      begin
        i:= IndexOf(aNameSearch);
        if i>-1 then
          DelItem(i);
        exit; //=>
      end;

    aSearchText:= '';
    aSearchTemp:= '';

    for i:=0 to aSearchStrings.Count-1 do
      begin
        aSearchTemp:='';
        for iField:= 0 to aFieldsList.Count-1 do
          begin
            aField:= aFieldsList.Strings[iField];

            if iField>0 then  aSearchTemp:= aSearchTemp+' OR ';

            aParamName:= GetOnlyCorrectChars(aField);

            case aSearchType of
              stContaining  :
                          begin
                            aSearchTemp:= aSearchTemp+aField+' CONTAINING :'+aParamName+IntToStr(i);
                            WriteParam;
                          end;
              stLike        :
                          begin
                            aSearchTemp:= aSearchTemp+ ' LOWER('+aField+') LIKE LOWER(:'+aParamName+IntToStr(i)+')';
                            WriteParam;
                          end;
              stIn          : aSearchTemp:= aSearchTemp+ aField+' IN ('+ConvertCommaStringToQuotedStr(aSearchStrings.Text)+')';
            end;

          end;
        if i>0 then aSearchText:= aSearchText+' AND ';
        aSearchText:= aSearchText+ '('+aSearchTemp+')';
      end;

    Log(aSearchText);
    for i:=0 to aParams.Count-1 do
      Log(Format('Name: %s | Value %s',[aParams.Items[i].Name, aParams.Items[i].Value]));

    SetItem(aNameSearch, aSearchText, aParams, aSearchType);

  finally
    FreeAndNil(aParams);
    FreeAndNil(aFieldsList);
    FreeAndNil(aSearchStrings);
  end;

end;

procedure TwfCustomSQLItemList.GetItem(const uName: string; out aValue: string; out
  aParams: TwfParams);
var
  aIndex: Int64;
  aName: String;
begin
  aName:= UTF8UpperCase(uName);
  aIndex:= IndexOf(aName);
  if aIndex>-1 then
  begin
    aValue:= TwfSQLItem(Items[aIndex]).Value;
    aParams:= TwfSQLItem(Items[aIndex]).Params;
  end;
end;

procedure TwfCustomSQLItemList.SetItem(const uName: string;
  const aValue: string; var aParams: TwfParams; const uType: TSQLItemType);
var
  aIndex: Int64;
  aName: String;
  iParam: Integer;
  aParam: TParam;
begin
  aName:= UTF8UpperCase(uName);

  aIndex:= IndexOf(aName);
  if aIndex>-1 then
  begin
    with TwfSQLItem(Items[aIndex]) do
    begin
      Value:= aValue;
      ItemType:= uType;
      Params.Clear;
      for iParam:= 0 to aParams.Count-1 do
        begin
          aParam:= TParam.Create(Params, ptInput);
          aParam.Assign(aParams.Items[iParam]);
        end;
    end;
  end
  else
    Add(TwfSQLItem.Create(aName, aValue, aParams, uType));
end;

destructor TwfCustomSQLItemList.Destroy;
begin
  ClearData;
  inherited Destroy;
end;

procedure TwfCustomSQLItemList.ClearData;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    begin
      FreeAndNil(TwfSQLItem(Items[i]).Params);
      TwfSQLItem(Items[i]).Free;
    end;
  Clear;
end;

function TwfCustomSQLItemList.GetAsParams: TwfParams;
var
  i, iParam: Integer;
  aParams: TwfParams;
begin

  Result:= TwfParams.Create(nil);

  for i:=0 to Count-1 do
    begin
      aParams:= TwfSQLItem(Items[i]).Params;
      if Assigned(aParams) then
        for iParam:= 0 to aParams.Count-1 do
          Result.AddParam(aParams.Items[iParam]);
    end;

end;

function TwfCustomSQLItemList.GetAsString: string;
var
  i: Integer;
  aInText: String;
  aIn: TStringList;
begin
  Result:= '';
  aInText:= '';

  aIn:= TStringList.Create;
  try
    for i:=0 to Count-1 do
    begin
      if TwfSQLItem(Items[i]).ItemType = stIn then
        aIn.Add(TwfSQLItem(Items[i]).Value)
      else
        begin
        if (i>0) and (UTF8Length(Result)>0) then
          Result:= Result+' OR ';

          Result:= Result+' '+TwfSQLItem(Items[i]).Value+' ';
        end;
    end;

    if aIn.Count>0 then
    begin
      for i:=0 to aIn.Count-1 do
      begin
        if (i>0) and (UTF8Length(Result)>0) then
          aInText:= aInText+' AND ';

          aInText:= aInText+' '+aIn.Strings[i]+' ';
      end;

      if UTF8Length(Result)>0 then
        Result:= '('+Result+') AND ';

      Result:= Result+aInText;
    end;
  finally
    FreeAndNil(aIn);
  end;
end;

function TwfCustomSQLItemList.GetParams: TwfParams;
var
  i, iParam: Integer;
  aParam: TParam;
  aParams: TwfParams;
begin
  if not Assigned(self) then exit; //=>

  Result:= TwfParams.Create(nil);

  for i:=0 to Count-1 do
    begin
      aParams:= TwfSQLItem(Items[i]).Params;

      if Assigned(aParams) then
        begin
         for iParam:=0 to aParams.Count-1 do
           begin
             aParam:= TParam.Create(Result, ptInput);
             aParam.Assign(aParams.Items[iParam]);
           end;
        end;
     end;

  for i:=0 to Result.Count-1 do
    Log(Format('Name: %s | Value %s',[Result.Items[i].Name, Result.Items[i].Value]));

end;

procedure TwfCustomSQLItemList.Log(aText: string);
begin
  if Assigned(fonLog) then fonLog(self, aText);
end;

procedure TwfCustomSQLItemList.DelItem(const aIndex: integer);
begin
  FreeAndNil(TwfSQLItem(Items[aIndex]).Params);
  TwfSQLItem(Items[aIndex]).Free;
  Delete(aIndex);
end;

end.

