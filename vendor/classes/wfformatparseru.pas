{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfFormatParserU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, IniFiles, LazUTF8, LazFileUtils, LazStringUtils, wfTypes, wfFunc, wfFormatParserDefU;

type
   // Enumeration: groups in rows
   TwfGroupInRows = (girYes, girNo, girNotUsed);

   { TwfIniRecord }

   // Record of ini file
   TwfIniRecord = record
     Param: string;
     Value: variant;
   end;

   // Records of ini file
   TwfIniRecords = array of TwfIniRecord;

   { TwfIniSection }

   // Ini file section
   TwfIniSection = class
   private
     fContent: TwfIniRecords;
     fName: string;
     function GetCount: integer;

   public
     // Returns the value by parameter name
     function ValueByParam(aParam: string):variant;
     // The name of the section
     property Name: string read fName write fName;
     // Number of records per section
     property Count: integer read GetCount;
     // Content section
     property Content: TwfIniRecords read fContent;
   end;

   { TwfIniSections }

   // Ini file sections
   TwfIniSections = specialize TFPGObjectList<TwfIniSection>;

   // Record of Format
   TwfFormatRecord = record
     Name: string;
     DataType: TDataType;
     Value: string;
     aComplexType: boolean;
     aCalculatedType: boolean;
   end;

   // Format section
   TwfFormatSection = array of TwfFormatRecord;

  { TwfFormatPaser }
   // Parser of a format represented as strings written in the INI file notation.
   TwfFormatPaser = class
   private
   // Converts the format to UpperCase
   procedure FormatRawUpperCase;
   function GetCleanParam(aParam: string; DataType: TDataType): string;
   // Returns the required data type
   function GetDataType(aParam: string; aValue: variant): TDataType;
   // Returns the format section by name
   function GetSection(aSectionName: string; const aQueryDataType: boolean=false): TwfFormatSection;
   // If a value is computed
   function IsCalculatedType(aValue: string): boolean;
   // If a value is the result of combining a string of records
   function IsComplexType(aValue: string): boolean;
   // If the interpretation of the read data as a number is specified
   function IsNumberType(aParam: string): boolean;
   // If the read data is interpreted as a string
   function IsStringType(aParam: string): boolean;

   private
     fFormatRaw: TStrings;
     fSections: TwfIniSections;
     // The parsing of the format from the list of strings in the structure
     procedure FillContent;
     // Adds content to the section
     procedure AddContent(aSectionName: string; aParams, aValues: TStrings);

     function GetDataSection: TwfFormatSection;
     function GetGroupsSection: TwfFormatSection;
     function GetParamsSection: TwfFormatSection;
     function GetLogicSection: TwfFormatSection;

     function GetFirstCol: integer;
     function GetFirstRow: integer;
     function GetGroupInRows: TwfGroupInRows;

     function GetSectionByName(aSectionName: string):TwfIniSection;
     function GetWorkSheet: integer;

   public
     constructor Create(aFormat: TStrings); virtual;
     destructor Destroy; override;

     // Returns the value by parameter name
     function GetValueByParam(aParam: string; aSection: TwfFormatSection): variant;
     // Numeric value of the sheet to load (starts with 1)
     property WorkSheet: integer read GetWorkSheet;
     // From which line to read data
     property FirstRow: integer read GetFirstRow;
     // From which column to read data
     property FirstCol: integer read GetFirstCol;
     // Groups in rows
     property GroupInRows: TwfGroupInRows read GetGroupInRows;
     // Groups section
     property GroupsSection: TwfFormatSection read GetGroupsSection;
     // Data section
     property DataSection: TwfFormatSection read GetDataSection;
     // Parameters section
     property ParamsSection: TwfFormatSection read GetParamsSection;
     // Logic section
     property LogicSection: TwfFormatSection read GetLogicSection;

   end;

implementation

{ TwfIniSection }

function TwfIniSection.GetCount: integer;
begin
  Result:= Length(fContent);
end;

function TwfIniSection.ValueByParam(aParam: string): variant;
var
  i: Integer;
begin
  Result:= 0;
  for i:=0 to High(fContent) do begin
    if aParam = fContent[i].Param then
       begin
         Result:= fContent[i].Value;
       end;
  end;
end;

{ TwfFormatPaser }

constructor TwfFormatPaser.Create(aFormat: TStrings);
begin
  fSections:= TwfIniSections.Create();
  fFormatRaw:= TStringList.Create;

  fFormatRaw.Assign(aFormat);
  FormatRawUpperCase;
  FillContent;
end;

procedure TwfFormatPaser.FormatRawUpperCase;
var
  i: Integer;
begin
  for i:=0 to fFormatRaw.Count-1 do
    fFormatRaw.Strings[i]:= UTF8UpperCase(fFormatRaw.Strings[i]);
end;

procedure TwfFormatPaser.FillContent;
var
  aFile, aSectionName: String;
  aIni: TIniFile;
  aSections, aParams, aValues: TStrings;
  i, k: Integer;
begin
    aFile:= Format('%s.tmp',[GetUID]);

    fFormatRaw.SaveToFile(aFile);
    aIni:= TIniFile.Create(aFile, [ifoStripComments]);
    aSections:= TStringList.Create;
    aParams:= TStringList.Create;
    aValues:= TStringList.Create;

    try
      aIni.ReadSections(aSections);


      for i:= 0 to aSections.Count-1 do begin
        aParams.Clear;
        aValues.Clear;
        aSectionName:= aSections[i];
        aIni.ReadSection(aSectionName, aParams);

        for k:= 0 to aParams.Count-1 do begin
          aValues.Append(aIni.ReadString(aSectionName, aParams[k],''));
        end;

        AddContent(aSectionName, aParams, aValues);
      end;

      if FileExistsUTF8(aFile) then
         DeleteFileUTF8(aFile);
    finally
      FreeAndNil(aIni);
      FreeAndNil(aSections);
      FreeAndNil(aParams);
      FreeAndNil(aValues);
    end;
end;

function TwfFormatPaser.GetDataType(aParam: string; aValue: variant): TDataType;
begin
  if IsCalculatedType(aValue) then Result:= dtCalculated
  else
    if IsComplexType(aValue) then Result:= dtComplex
  else
    if IsNumberType(aParam) then Result:= dtNumber
    else
      if IsStringType(aParam) then Result:= dtString
      else
        Result:= dtDefault;
end;

function TwfFormatPaser.GetCleanParam(aParam: string; DataType: TDataType): string;
begin
  case DataType of
    dtNumber: Result:= UTF8StringReplace(aParam, ufpNumberType, '',[]);
    dtString: Result:= UTF8StringReplace(aParam, ufpStringType, '',[]);
    else
      Result:= aParam;
  end;
end;

function TwfFormatPaser.IsNumberType(aParam: string):boolean;
begin
  Result:= IsEntry(ufpNumberType, aParam);
end;

function TwfFormatPaser.IsStringType(aParam: string):boolean;
begin
  Result:= IsEntry(ufpStringType, aParam);
end;

function TwfFormatPaser.IsCalculatedType(aValue: string):boolean;
begin
  Result:= IsEntry(ufpCalculatedType, aValue);
end;

function TwfFormatPaser.IsComplexType(aValue: string):boolean;
var
  i: Integer;
begin
  Result:= IsEntry(ufpComplexType, aValue);
end;

function TwfFormatPaser.GetSection(aSectionName: string; const aQueryDataType: boolean = false): TwfFormatSection;
var
  aSection: TwfIniSection;
  i: Integer;
begin
  aSection:= GetSectionByName(aSectionName);

  SetLength(Result, aSection.Count);

  for i:= 0 to aSection.Count-1 do begin

    if aQueryDataType then
      Result[i].DataType := GetDataType(aSection.Content[i].Param, aSection.Content[i].Value)
    else
      Result[i].DataType := dtNotUsed;

    //Result[i].Name := GetOnlyCorrectCharsUTF8(aSection.Content[i].Param);
    Result[i].Name := GetCleanParam(aSection.Content[i].Param, Result[i].DataType);
    Result[i].Value := aSection.Content[i].Value;
    Result[i].aComplexType:= IsComplexType(Result[i].Value);
    Result[i].aCalculatedType:= IsCalculatedType(Result[i].Value);
  end;
end;

function TwfFormatPaser.GetDataSection: TwfFormatSection;

begin
  Result:= GetSection(ufpSectionData, true);
end;

function TwfFormatPaser.GetGroupsSection: TwfFormatSection;
begin
  Result:= GetSection(ufpSectionGroups, true);
end;

function TwfFormatPaser.GetFirstCol: integer;
begin
  Result:= VarToInt64(GetSectionByName(ufpSectionInit).ValueByParam(ufpParamFistCol));
end;

function TwfFormatPaser.GetFirstRow: integer;
begin
  Result:= VarToInt64(GetSectionByName(ufpSectionInit).ValueByParam(ufpParamFistRow));
end;

function TwfFormatPaser.GetGroupInRows: TwfGroupInRows;
var
  aResultText: String;
begin
  aResultText:= VarToStr(GetSectionByName(ufpSectionInit).ValueByParam(ufpParamGroupInRows));

  case aResultText of
    ufpGroupInRowsYes  : Result:= girYes;
    ufpGroupInRowsNo   : Result:= girNo;
   else
     Result:= girNotUsed;
  end;
end;

function TwfFormatPaser.GetLogicSection: TwfFormatSection;
begin
  Result:= GetSection(ufpSectionDataLogic);
end;

function TwfFormatPaser.GetParamsSection: TwfFormatSection;
begin
  Result:= GetSection(ufpSectionDataParams);
end;

function TwfFormatPaser.GetSectionByName(aSectionName: string): TwfIniSection;
var
  i: Integer;
begin
    Result:= nil;
  for i:=0 to fSections.Count-1 do begin
    if aSectionName = fSections[i].Name then
       begin
         Result:= fSections[i];
         Break;
       end;
  end;
end;

function TwfFormatPaser.GetWorkSheet: integer;
begin
  Result:= VarToInt64(GetSectionByName(ufpSectionInit).ValueByParam(ufpParamWorkSheet))-1;
end;

destructor TwfFormatPaser.Destroy;
begin
  FreeAndNil(fSections);
  FreeAndNil(fFormatRaw);
  inherited Destroy;
end;

function TwfFormatPaser.GetValueByParam(aParam: string; aSection: TwfFormatSection): variant;
var
  i: Integer;
begin
  Result:= 0;
  for i:=0 to High(aSection) do begin
    if aParam = aSection[i].Name then
       begin
         Result:= aSection[i].Value;
         break;
       end;
  end;
end;

procedure TwfFormatPaser.AddContent(aSectionName: string; aParams, aValues: TStrings);
var
  aIndex: LongInt;
  aIniSection: TwfIniSection;
  i: Integer;
begin
  aIndex:= fSections.Add(TwfIniSection.Create());
  aIniSection:= TwfIniSection(fSections.Items[aIndex]);

  SetLength(aIniSection.fContent, aParams.Count);
  aIniSection.Name := aSectionName;
  for i:=0 to aParams.Count-1 do begin
    aIniSection.fContent[i].Param:= aParams[i];
    aIniSection.fContent[i].Value:= StringToVar(aValues[i]);
  end;
end;

end.

