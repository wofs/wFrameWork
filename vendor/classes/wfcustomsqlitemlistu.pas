{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfCustomSQLItemListU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, wfFunc, db, LazUTF8, wfParamsU, wfIntSQLItemU;

type


     { TwfCustomSQLItemList }
     TwfCustomSQLItemList = class(TList)
     private
       fonLog: TTextEvent;
       fSearchFieldsContaining: string;
       fSearchFieldsIn: string;
       fSearchFieldsLike: string;
       fSQLEngine: TwfSQLEngine;
       procedure ClearData;
       function ConvertCommaStringToQuotedStr(aCommaString: string): string;
       function GetAsParams: TwfParams;
       function GetAsString: string;
       function GetParams: TwfParams;
       procedure Log(aText: string);

     public
       constructor Create;
       destructor Destroy; override;

       procedure SetItem(const uName: string; const aValue: string;
         var aParams: TwfParams; const uType:TSQLItemType = stUnknown);
       procedure GetItem(const uName: string; out aValue: string; out aParams: TwfParams);
       procedure DelItem(const aIndex: integer);
       function IndexOf(aName: string): int64;

       procedure GenerateSearchList(doSplitIntoSubstrings:boolean; aSearchText: string; aSearchType: TSQLItemType; const uNameSearch: string = ''; const uFieldList: string = '');

       property AsString: string read GetAsString;
       property AsParams: TwfParams read GetAsParams;
       property Params: TwfParams read GetParams;

       property SearchFieldsContaining: string read fSearchFieldsContaining write fSearchFieldsContaining;
       property SearchFieldsLike: string read fSearchFieldsLike write fSearchFieldsLike;
       property SearchFieldsIn: string read fSearchFieldsIn write fSearchFieldsIn;
       property SQLEngine: TwfSQLEngine read fSQLEngine write fSQLEngine;

       property onLog: TTextEvent read fonLog write fonLog;
     end;

implementation

{ TwfCustomSQLItemList }
function TwfCustomSQLItemList.IndexOf(aName: string):int64;
var
  i: Integer;
begin
  Result:= -1;
  aName:= UTF8UpperCase(aName);

  for i:=0 to Count-1 do
    if TwfIntSQLItem(Items[i]).Name = aName then
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

procedure TwfCustomSQLItemList.GenerateSearchList(
  doSplitIntoSubstrings: boolean; aSearchText: string;
  aSearchType: TSQLItemType; const uNameSearch: string; const uFieldList: string
  );
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
    case SQLEngine of
        seFirebird: aParam.Value:= aSearchStrings.Strings[i];
      else
        aParam.Value:= '%'+aSearchStrings.Strings[i]+'%';
    end;
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
      { TODO : Добавить опциональную вохможность не разделять строки на подстроки }
    aFieldsList.Delimiter:=',';
    case aSearchType of
      stContaining  :
                  begin
                    if Length(uFieldList)>0 then
                      aFieldsList.DelimitedText:= uFieldList
                    else
                      aFieldsList.DelimitedText:= fSearchFieldsContaining;

                    if doSplitIntoSubstrings then
                      aSearchStrings.CommaText:= aSearchText
                    else
                      aSearchStrings.Text:= aSearchText;
                  end;
      stLike        :
                  begin
                    if Length(uFieldList)>0 then
                      aFieldsList.DelimitedText:= uFieldList
                    else
                      aFieldsList.DelimitedText:= fSearchFieldsLike;

                    if doSplitIntoSubstrings then
                      aSearchStrings.CommaText:= aSearchText
                    else
                      aSearchStrings.Text:= aSearchText;
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
                              case SQLEngine of
                                  seFirebird: aSearchTemp:= aSearchTemp+aField+' CONTAINING :'+aParamName+IntToStr(i);
                                else
                                  aSearchTemp:= aSearchTemp+ ' LOWER('+aField+') LIKE LOWER(:'+aParamName+IntToStr(i)+')';
                              end;
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
    aValue:= TwfIntSQLItem(Items[aIndex]).Value;
    aParams:= TwfIntSQLItem(Items[aIndex]).Params;
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
    with TwfIntSQLItem(Items[aIndex]) do
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
    Add(TwfIntSQLItem.Create(aName, aValue, aParams, uType));
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
      FreeAndNil(TwfIntSQLItem(Items[i]).Params);
      TwfIntSQLItem(Items[i]).Free;
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
      aParams:= TwfIntSQLItem(Items[i]).Params;
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
      if TwfIntSQLItem(Items[i]).ItemType = stIn then
        aIn.Add(TwfIntSQLItem(Items[i]).Value)
      else
        begin
        if (i>0) and (UTF8Length(Result)>0) then
          Result:= Result+' OR ';

          Result:= Result+' '+TwfIntSQLItem(Items[i]).Value+' ';
        end;
    end;

    if aIn.Count>0 then
    begin
      for i:=0 to aIn.Count-1 do
      begin
        if (i>0) and (UTF8Length(aInText)>0) then
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
      aParams:= TwfIntSQLItem(Items[i]).Params;

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

constructor TwfCustomSQLItemList.Create;
begin
  fSQLEngine:= seFirebird;
  inherited;
end;

procedure TwfCustomSQLItemList.DelItem(const aIndex: integer);
begin
  FreeAndNil(TwfIntSQLItem(Items[aIndex]).Params);
  TwfIntSQLItem(Items[aIndex]).Free;
  Delete(aIndex);
end;

end.

