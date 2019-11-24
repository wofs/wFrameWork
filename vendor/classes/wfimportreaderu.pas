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
    fonWriteContentRow: TwfWriteContentRowEvent;

    fSource: string;
    fDataSection: TwfFormatSection;
    fParamsSection: TwfFormatSection;
    fLogicSection: TwfFormatSection;
    // Flag to stop import. To handle on their own.
    fTerminated: Boolean;
    function GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfImportContentRow): Double;
    procedure WriteCalculatedValue(var aContentRow: TwfImportContentRow);
    procedure WriteComplexValue(var aContentRow: TwfImportContentRow);


  protected
    // Clearing the content line between fill interations
    procedure ContentRowClear(var aContentRow: TwfImportContentRow);
    // Is this string content (based on the logic spelled out in the format)
    function IsContent(var aContentRow: TwfImportContentRow): boolean; virtual;
    // Raises an event for the row's entry.
    procedure WriteContentRow(var aContentRow: TwfImportContentRow); virtual;
    // Replaces the specified parameters for string concatenation
    function GetComplexValue(aComplexString: string; var aContentRow: TwfImportContentRow): string;

    property DataSection: TwfFormatSection read fDataSection write fDataSection;
    property ParamsSection: TwfFormatSection read fParamsSection write fParamsSection;
    property LogicSection: TwfFormatSection read fLogicSection write fLogicSection;

  public
    constructor Create(aSource: string; aFormat: TStrings); virtual;
    destructor Destroy; override;

    // Starts reading-implement yourself
    procedure Start; virtual; abstract;

    property Source: string read fSource;
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

procedure TwfImportReader.ContentRowClear(var aContentRow: TwfImportContentRow);
var
  i: Integer;
begin
  for i:=0 to High(aContentRow.Row) do begin
    aContentRow.Row[i].Name:= wfEmptyStr;
    aContentRow.Row[i].Field:= wfEmptyStr;
    aContentRow.Row[i].Value:= wfEmptyInt;
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

function TwfImportReader.GetCalculatedValue(aCalculatedString: string; var aContentRow: TwfImportContentRow):Double;
var
  aFormula: String;
begin
  aFormula:= GetComplexValue(aCalculatedString, aContentRow);
  Result:= Calc.Calculate(aFormula);
end;

procedure TwfImportReader.WriteContentRow(var aContentRow: TwfImportContentRow);
begin
  WriteComplexValue(aContentRow);
  WriteCalculatedValue(aContentRow);

  if Assigned(fonWriteContentRow) then
    fonWriteContentRow(self, aContentRow);
end;

function TwfImportReader.GetComplexValue(aComplexString: string; var aContentRow: TwfImportContentRow): string;
var
  i: Integer;
  aSearchParam: String;
begin
  Result:= aComplexString;

  for i:= 0 to High(aContentRow.Row) do begin
    aSearchParam:= '{'+aContentRow.Row[i].Name+'}';

    if IsEntry(aSearchParam, Result) then
      begin
        Result:= UTF8StringReplace(Result, aSearchParam, '%s', [rfReplaceAll, rfIgnoreCase]);
        Result:= SysUtils.Format(Result, [VarToStr(aContentRow.Row[i].Value)]);
      end;
  end;
end;

function TwfImportReader.IsContent(var aContentRow: TwfImportContentRow):boolean;
var
  aRecordCondition: ArrayOfString;
  i, k, aCount: Integer;
begin
 Result:= false;
 aCount:= 0;
 aRecordCondition:= ArrayAsString(Format.GetValueByParam(ufpIsContent, fLogicSection), '+');

 for i:=0 to High(aContentRow.Row) do begin
   for k:= 0 to High(aRecordCondition) do begin
     if aContentRow.Row[i].Name = aRecordCondition[k] then
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

