{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfImportReaderU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, wfClasses, wfFunc, wfFormatParserDefU, wfBase, LazUTF8, wfFormatParserU, wfCalculatorU,
  fpspreadsheet, fpsTypes, fpsallformats, fpsutils;

type

  { TwfImportReader }

  TwfImportReader = class
  private
    fCalc: TwfCalculator;
    fFormat: TwfFormatPaser;
    fContentRow: TwfImportContentRow;
    fCotentGroups: TwfImportGroups;
    fonWriteContentRow: TwfWriteContentRowEvent;

    fSource: string;
    fDataSection: TwfFormatSection;
    fParamsSection: TwfFormatSection;
    fLogicSection: TwfFormatSection;
    // Flag to stop import. To handle on their own.
    fTerminated: Boolean;
    // Returns the value calculated by the formula
    function GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfImportContentRow): Currency;
    // Fixes an issue in the calculation due to an incorrect fraction separator
    function GetCorrectTextValue(aValue: Variant): string;
    // Writes the calculated value to the result string
    procedure WriteCalculatedValue(var aContentRow: TwfImportContentRow);
    // Writes the concatenated string to the result string
    procedure WriteComplexValue(var aContentRow: TwfImportContentRow);

  protected
    // Clearing the content line between fill interations
    procedure ContentRowClear();
    // Is this string content (based on the logic spelled out in the format)
    function IsContent(): boolean; virtual;
    // Raises an event for the row's entry.
    procedure WriteContentRow(aGroups: TwfImportGroups; aContentRow: TwfImportContentRow); virtual;

    procedure ContentRowSetLength(aLength: integer);

    // Replaces the specified parameters for string concatenation
    function GetComplexValue(aComplexString: string; var aContentRow: TwfImportContentRow): string;
    // Data section
    property DataSection: TwfFormatSection read fDataSection write fDataSection;
    // Parameters section
    property ParamsSection: TwfFormatSection read fParamsSection write fParamsSection;
    // Logic section
    property LogicSection: TwfFormatSection read fLogicSection write fLogicSection;

    property ContentRow: TwfImportContentRow read fContentRow;
    property CotentGroups: TwfImportGroups read fCotentGroups;
  public
    constructor Create(aSource: string; aFormat: TStrings); virtual;
    destructor Destroy; override;

    // Starts reading-implement yourself
    procedure Start; virtual; abstract;
    // Data source
    property Source: string read fSource;
    // Format parser reference
    property Format: TwfFormatPaser read fFormat;
    // Flag to stop import. To install and to handle on their own!
    property Terminated: boolean read fTerminated write fTerminated;
    // Calculator
    property Calc: TwfCalculator read fCalc write fCalc;

    {Events}
    // You must implement the data write event yourself
    property onWriteContentRow: TwfWriteContentRowEvent read fonWriteContentRow write fonWriteContentRow;
  end;

implementation

{ TwfImportReader }

procedure TwfImportReader.ContentRowClear();
var
  i: Integer;
begin
  for i:=0 to High(ContentRow.Row) do begin
    ContentRow.Row[i].Name:= wfEmptyStr;
    ContentRow.Row[i].Field:= wfEmptyStr;
    ContentRow.Row[i].Value:= wfEmptyInt;
  end;
end;

procedure TwfImportReader.WriteComplexValue(var aContentRow: TwfImportContentRow);
var
  i: Integer;
  aValue: String;
begin
 for i:=0 to High(DataSection) do begin
   if DataSection[i].aComplexType then
   begin
     aValue:= UTF8StringReplace(DataSection[i].Value, ufpComplexType, '', []);
     aContentRow.Row[i].Value:= GetComplexValue(aValue, aContentRow);
   end;
 end;
end;

procedure TwfImportReader.WriteCalculatedValue(var aContentRow: TwfImportContentRow);
var
  i: Integer;
  aValue: String;
begin
 for i:=0 to High(DataSection) do begin
   if DataSection[i].aCalculatedType then
   begin
     aValue:= UTF8StringReplace(DataSection[i].Value, ufpCalculatedType, '', []);
     aContentRow.Row[i].Value:= GetCalculatedValue(aValue, aContentRow);
   end;
 end;
end;

function TwfImportReader.GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfImportContentRow):Currency;
var
  aFormula: String;
begin
  aFormula:= GetComplexValue(aCalculatedString, aContentRow);
  Result:= Calc.Calculate(aFormula);
end;

procedure TwfImportReader.WriteContentRow(aGroups: TwfImportGroups; aContentRow: TwfImportContentRow);
begin
  WriteComplexValue(aContentRow);
  WriteCalculatedValue(aContentRow);

  if Assigned(fonWriteContentRow) then
    fonWriteContentRow(self, aGroups, aContentRow);
end;

procedure TwfImportReader.ContentRowSetLength(aLength: integer);
begin
  SetLength(fContentRow.Row, aLength);
end;

function TwfImportReader.GetCorrectTextValue(aValue: Variant):string;
begin
 case TVarData(aValue).VType of
     varSmallInt,
     varInteger       : Result := IntToStr(aValue);
     varint64         : Result := IntToStr(int64(aValue));
     varSingle,
     varDouble,
     varCurrency      : begin
       Result := CurrToStr(aValue);
       Result:= StringReplace(Result,',','.',[]);
     end;
     varDate          : Result:= FormatDateTime('dd.mm.yyyy', aValue);
     varBoolean       : if aValue then
                       Result := 'T'
                     else
                       Result := 'F';
     varString        : Result := aValue
     else
       WriteStr(Result, TVarData(aValue).VType)
   end;
end;

function TwfImportReader.GetComplexValue(aComplexString: string; var aContentRow: TwfImportContentRow): string;
var
  i: Integer;
  aSearchParam: String;
  aValue: Variant;
begin
  Result:= aComplexString;

  for i:= 0 to High(aContentRow.Row) do begin
    aSearchParam:= '{'+aContentRow.Row[i].Name+'}';

    if IsEntry(aSearchParam, Result) then
      begin
        aValue:= GetCorrectTextValue(aContentRow.Row[i].Value);

        Result:= UTF8StringReplace(Result, aSearchParam, VarToStr(aValue), [rfReplaceAll, rfIgnoreCase]);
      end;
  end;
end;

function TwfImportReader.IsContent(): boolean;
var
  aRecordCondition: ArrayOfString;
  i, k, aCount: Integer;
begin
 Result:= false;
 aCount:= 0;
 aRecordCondition:= ArrayAsString(Format.GetValueByParam(ufpIsContent, fLogicSection), '+');

 for i:=0 to High(ContentRow.Row) do begin
   for k:= 0 to High(aRecordCondition) do begin
     if ContentRow.Row[i].Name = aRecordCondition[k] then
     inc(aCount);
   end;
 end;

 Result:= aCount = Length(aRecordCondition);
end;

constructor TwfImportReader.Create(aSource: string; aFormat: TStrings);
begin
  fTerminated:= true;
  fSource:= aSource;
  fFormat:= TwfFormatPaser.Create(aFormat);
  fDataSection:= fFormat.DataSection;
  fParamsSection:= fFormat.ParamsSection;
  fLogicSection:= fFormat.LogicSection;
  fCalc:= TwfCalculator.Create(nil);
  //Use the Start procedure to start the import
end;

destructor TwfImportReader.Destroy;
begin
  FreeAndNil(fFormat);
  FreeAndNil(fCalc);
  inherited Destroy;
end;

end.

