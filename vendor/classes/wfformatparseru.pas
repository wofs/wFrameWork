unit wfFormatParserU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, IniFiles, LazUTF8, LazFileUtils, LazStringUtils, wfTypes, wfFunc;

type
   TwfGroupInRows = (girYes, girNo, girNotUsed);

   { TwfIniRecord }

   TwfIniRecord = record
     Param: string;
     Value: variant;
   end;


   TwfIniRecords = array of TwfIniRecord;

   { TwfIniSection }

   TwfIniSection = class
   private
     fContent: TwfIniRecords;
     fName: string;
     function GetCount: integer;

   public
     function ValueByParam(aParam: string):variant;

     property Name: string read fName write fName;
     property Count: integer read GetCount;
     property Content: TwfIniRecords read fContent;
   end;

   { TwfIniSections }

   TwfIniSections = specialize TFPGObjectList<TwfIniSection>;

   TwfFormatRecord = record
     Name: string;
     DataType: TValueType;
     Value: string;
     aComplexType: boolean;
     aCalculatedType: boolean;
   end;

   TwfFormatSection = array of TwfFormatRecord;

  { TwfFormatPaser }

   TwfFormatPaser = class
   private const
     uSectionInit         = 'ИНИТ';
     uParamWorkSheet      = 'ЛИСТ';
     uParamFistRow        = 'ПЕРВАЯСТРОКА';
     uParamFistCol        = 'ПЕРВАЯКОЛОНКА';
     uParamGroupInRows    = 'ГРУППЫВСТРОКАХ';

     uSectionData         = 'ДАННЫЕ';
     uSectionDataParams   = 'ПАРАМЕТРЫ';
     uSectionDataLogic    = 'ЛОГИКА';

   procedure FormatRawUpperCase;
   function GetDataType(aData: string): TValueType;
   function GetSection(aSectionName: string; const aQueryDataType: boolean=false): TwfFormatSection;
   function IsCalculatedType(aValue: string): boolean;
   function IsComplexType(aValue: string): boolean;
   private
     fFormatRaw: TStrings;
     fSections: TwfIniSections;

     procedure AddContent(aSectionName: string; aParams, aValues: TStrings);
     procedure FillContent;
     function GetDataSection: TwfFormatSection;
     function GetFirstCol: integer;
     function GetFirstRow: integer;
     function GetGroupInRows: TwfGroupInRows;
     function GetLogicSection: TwfFormatSection;
     function GetParamsSection: TwfFormatSection;

     function GetSectionByName(aSectionName: string):TwfIniSection;
     function GetWorkSheet: integer;

   public
     constructor Create(aFormat: TStrings); virtual;
     destructor Destroy; override;

     function GetValueByParam(aParam: string; aSection: TwfFormatSection): variant;

     property WorkSheet: integer read GetWorkSheet;
     property FirstRow: integer read GetFirstRow;
     property FirstCol: integer read GetFirstCol;
     property GroupInRows: TwfGroupInRows read GetGroupInRows;
     property DataSection: TwfFormatSection read GetDataSection;
     property ParamsSection: TwfFormatSection read GetParamsSection;
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

function TwfFormatPaser.GetDataType(aData: string): TValueType;
begin
  { TODO -owofs -cTwfFormatPaser : Добавить детект формата данных }
  Result:= vtDefault;
end;

function TwfFormatPaser.IsCalculatedType(aValue: string):boolean;
const
  uSymbols = ['+','-','/','*'];
var
  i: Integer;
begin
  for i:= Length(aValue) downto 1 do
    if (aValue[i] in uSymbols) then
      begin
        Result:= true;
        break;
      end;
end;

function TwfFormatPaser.IsComplexType(aValue: string):boolean;
const
  uSymbols = ['{','}'];
var
  i: Integer;
begin
  for i:= Length(aValue) downto 1 do
    if (aValue[i] in uSymbols) then
      begin
        Result:= true;
        break;
      end;
end;

function TwfFormatPaser.GetSection(aSectionName: string; const aQueryDataType: boolean = false): TwfFormatSection;
var
  aSection: TwfIniSection;
  i: Integer;
begin
  aSection:= GetSectionByName(aSectionName);

  SetLength(Result, aSection.Count);

  for i:= 0 to aSection.Count-1 do begin
    Result[i].Name := GetOnlyCorrectCharsUTF8(aSection.Content[i].Param);

    if aQueryDataType then
      Result[i].DataType := GetDataType(aSection.Content[i].Param)
    else
      Result[i].DataType := vtNotUsed;

    Result[i].Value := aSection.Content[i].Value;
    Result[i].aComplexType:= IsComplexType(Result[i].Value);
    Result[i].aCalculatedType:= IsCalculatedType(Result[i].Value);
  end;
end;

function TwfFormatPaser.GetDataSection: TwfFormatSection;

begin
  Result:= GetSection(uSectionData, true);
end;

function TwfFormatPaser.GetFirstCol: integer;
begin
  Result:= VarToInt64(GetSectionByName(uSectionInit).ValueByParam(uParamFistCol));
end;

function TwfFormatPaser.GetFirstRow: integer;
begin
  Result:= VarToInt64(GetSectionByName(uSectionInit).ValueByParam(uParamFistRow));
end;

function TwfFormatPaser.GetGroupInRows: TwfGroupInRows;
var
  aResultText: String;
begin
  aResultText:= VarToStr(GetSectionByName(uSectionInit).ValueByParam(uParamGroupInRows));

  case aResultText of
    'ДА'             : Result:= girYes;
    'НЕТ'            : Result:= girNo;
   else
     Result:= girNotUsed;
  end;
end;

function TwfFormatPaser.GetLogicSection: TwfFormatSection;
begin
  Result:= GetSection(uSectionDataLogic);
end;

function TwfFormatPaser.GetParamsSection: TwfFormatSection;
begin
  Result:= GetSection(uSectionDataParams);
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
  Result:= VarToInt64(GetSectionByName(uSectionInit).ValueByParam(uParamWorkSheet))-1;
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

