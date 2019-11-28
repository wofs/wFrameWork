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
    fContentRow: TwfContentRow;
    fGroups: TwfGroups;
    fonWriteContentRow: TwfWriteContentRowEvent;

    fSource: string;
    fGroupsSection: TwfFormatSection;
    fDataSection: TwfFormatSection;
    fParamsSection: TwfFormatSection;
    fLogicSection: TwfFormatSection;
    // Flag to stop import. To handle on their own.
    fTerminated: Boolean;
    // Returns the value calculated by the formula
    function GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfContentRow): Currency;
    // Fixes an issue in the calculation due to an incorrect fraction separator
    function GetCorrectTextValue(aValue: Variant): string;
    function GetGroupBreadCrumbs(aIndex: Integer): string;
    function GetGroupCurrent: TwfGroupCell;
    procedure SetCurrentGroup(aIndex: integer);
    // Writes the calculated value to the result string
    procedure WriteCalculatedValue(var aContentRow: TwfContentRow);
    // Writes the concatenated string to the result string
    procedure WriteComplexValue(var aContentRow: TwfContentRow);

    //Собираем и возвращаем TwfGroupCell
    function CreateContentGroup(aName, aField: string; aValue: string; aBgColor: TsColor): TwfGroupCell;

  protected
    // Clearing the content line between fill interations
    procedure ContentRowClear();
    // Is this string content (based on the logic spelled out in the format)
    function IsContent(): boolean; virtual;
    // Raises an event for the row's entry.
    procedure WriteContentRow(aGroups: TwfGroups; aContentRow: TwfContentRow); virtual;
    // Sets the ContentRow length.Row
    procedure ContentRowSetLength(aLength: integer);
    procedure ContentRowSetGroup(aGroupIndex: integer);

    // Adds a group and returns the current index
    function AddGroup(aName, aField: string; aValue: string; const aBgColor: TsColor = 0): Integer;
    // Returns a group by index
    function GetGroup(aIndex: Integer):TwfGroupCell;

    // Replaces the specified parameters for string concatenation
    function GetComplexValue(aComplexString: string; var aContentRow: TwfContentRow): string;
    // Groups section
    property GroupsSection: TwfFormatSection read fGroupsSection write fGroupsSection;
    // Data section
    property DataSection: TwfFormatSection read fDataSection write fDataSection;
    // Parameters section
    property ParamsSection: TwfFormatSection read fParamsSection write fParamsSection;
    // Logic section
    property LogicSection: TwfFormatSection read fLogicSection write fLogicSection;
    // Row of read values
    property ContentRow: TwfContentRow read fContentRow;
    // List of read groups
    property Groups: TwfGroups read fGroups;

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
    // Returns a group by index
    property Group[aIndex: Integer]: TwfGroupCell read GetGroup;
    property GroupBreadCrumbs[aIndex: Integer]:string read GetGroupBreadCrumbs;
    property GroupCurrent: TwfGroupCell read GetGroupCurrent;

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
    ContentRow.Row[i].Color:= wfEmptyInt;
  end;
end;

procedure TwfImportReader.WriteComplexValue(var aContentRow: TwfContentRow);
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

function TwfImportReader.CreateContentGroup(aName, aField: string; aValue: string; aBgColor: TsColor): TwfGroupCell;
begin
  with Result do begin
    Name:= aName;
    Field:= aField;
    Value:= aValue;
    Color:= aBgColor;
    IsCurrent:= false;
  end;
end;

procedure TwfImportReader.WriteCalculatedValue(var aContentRow: TwfContentRow);
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

function TwfImportReader.GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfContentRow):Currency;
var
  aFormula: String;
begin
  aFormula:= GetComplexValue(aCalculatedString, aContentRow);
  Result:= Calc.Calculate(aFormula);
end;

procedure TwfImportReader.WriteContentRow(aGroups: TwfGroups; aContentRow: TwfContentRow);
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

procedure TwfImportReader.ContentRowSetGroup(aGroupIndex: integer);
var
  aContentRow: TwfContentRow;
begin
  aContentRow:= fContentRow;
  aContentRow.GroupIndex:= aGroupIndex;
  fContentRow:= aContentRow;

  SetCurrentGroup(aGroupIndex);
end;

function TwfImportReader.AddGroup(aName, aField: string; aValue: string; const aBgColor: TsColor): Integer;
begin
  Groups.PushBack(CreateContentGroup(aName, aField, aValue, aBgColor));
  Result:= Groups.Size-1;
end;

procedure TwfImportReader.SetCurrentGroup(aIndex: integer);
var
  i: Integer;
  aCell: TwfGroupCell;
begin
  for i:= 0 to Groups.Size-1 do begin
    if (aIndex = i) then
    begin
      aCell:= Groups[i];
      aCell.IsCurrent:= true;
      Groups[i]:= aCell;
    end
    else
    begin
      aCell:= Groups[i];
      aCell.IsCurrent:= false;
      Groups[i]:= aCell;
    end
  end;
end;

function TwfImportReader.GetGroup(aIndex: Integer): TwfGroupCell;
begin
  Result:= Groups.Items[aIndex];
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

function TwfImportReader.GetGroupBreadCrumbs(aIndex: Integer): string;
var
  i: Integer;
begin
 Result:= wfEmptyStr;

  for i:= 0 to aIndex do begin
    if not IsEmpty(Result) then
      Result += '\';
    Result +=Groups[i].Value;
  end;
end;

function TwfImportReader.GetGroupCurrent: TwfGroupCell;
var
  i: Integer;
begin
  for i:=0 to Groups.Size do begin
    if Groups[i].IsCurrent then
    begin
      Result:= Groups[i];
      break;
    end;
  end;
end;

function TwfImportReader.GetComplexValue(aComplexString: string; var aContentRow: TwfContentRow): string;
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
  aValue: variant;
begin
 Result:= false;
 aCount:= 0;
 aRecordCondition:= ArrayAsString(Format.GetValueByParam(ufpIsContent, fLogicSection), '+');

 for i:=0 to High(ContentRow.Row) do begin
   aValue:= ContentRow.Row[i].Value;
   for k:= 0 to High(aRecordCondition) do begin
     if ContentRow.Row[i].Name = aRecordCondition[k] then
       if  not IsEmpty(aValue) then
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
  fGroupsSection:= fFormat.GroupsSection;
  fDataSection:= fFormat.DataSection;
  fParamsSection:= fFormat.ParamsSection;
  fLogicSection:= fFormat.LogicSection;
  fCalc:= TwfCalculator.Create(nil);
  fGroups:= TwfGroups.Create;
  //Use the Start procedure to start the import
end;

destructor TwfImportReader.Destroy;
begin
  FreeAndNil(fFormat);
  FreeAndNil(fCalc);
  FreeAndNil(fGroups);
  inherited Destroy;
end;

end.

