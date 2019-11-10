{
This file is part of wfFrameWork.

 -= Vendor/Functions =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfFunc;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, db, LazUTF8, Graphics, Dialogs, StdCtrls, wfTypes,
  md5, fileinfo, fpspreadsheet, fpsTypes, fpspreadsheetctrls;

function VarToStr(Value: variant): string;
function VarToBool(Value: variant): boolean;
function VarToInt64(Value: variant): Int64;
function VarToCurr(Value: variant): Currency;
function VarToBaseID(aValue: variant):BaseID;
function BoolToInt(aValue: Boolean):Integer;
function BaseIDToStr(aValue: BaseID):string;
function GetVarType(aValue: variant):tvartype;
function GetOnlyCorrectChars(aValue: string): string;
function GetOnlyChars(aValue: string): string;
function GetOnlyNumbers(aValue: string): Double;
function GetOnlyDateTime(aValue: string): TDateTime;

procedure GetExcelFilesList(aPath: string; aList: TStrings );

procedure DeleteEmplyItems(aStringList: TStringList);

function getUTFSymbol(S:string;i:integer):string; // Retrieves the required UTF character and string
procedure WriteUTF8String(aFileStream: TFileStream; aText: RawByteString);

procedure WriteValue(aWorksheet:TsWorksheet; aRow, aCol: integer; aField: TField;
  const aFontStyles: TsFontStyles = [];
  aCellColor: TsColor = clDefault;
  aFontColor: TsColor = clBlack;
  aBorders: boolean = true);

procedure WriteValue(aWorksheet:TsWorksheet; aRow, aCol: integer; aValue: variant;
  const aFontStyles: TsFontStyles = [];
  aCellColor: TsColor = clDefault;
  aFontColor: TsColor = clBlack;
  aBorders: boolean = true);

procedure WriteValue(aWorksheet:TsWorksheet; aRow, aCol: integer; aField: TField;
  aFont: TsFont;
  aCellFormat: TsCellFormat;
  aNumFormat: TsNumberFormat;
  aNumFormatStr: string;
  aCellBorders: TsCellBorders);

procedure ChangeColor(Sender: TObject);

function IsEmpty(aString: string): boolean;
function EmptyBaseID:variant;
function IsEmpty(aID: BaseID):boolean;
function IsEmpty(aStrings:TStrings):boolean;
function IsEmpty(aObject:TObject):boolean;

function IsEntry(aSearch:string; aInText:string):boolean;

function GetApplicationPathUnsafe:string;

function GetSpreadSheetFormat(aFileName:string):TsSpreadsheetFormat;
function GetMD5Hash(aStr: string): string;
function GetUID:string;

function GetVersion: string;
function CalcMD5File(aFileName: string): string;
implementation

procedure WriteValue(aWorksheet: TsWorksheet; aRow, aCol: integer; aField: TField; const aFontStyles: TsFontStyles; aCellColor: TsColor; aFontColor: TsColor;
  aBorders: boolean);
var
  _S: string;
begin

  case aField.DataType of
    ftSmallInt,
    ftLargeint,
    ftInteger          : aWorksheet.WriteNumber(aRow, aCol, aField.AsInteger, nfGeneral);
    ftFloat,
    ftBCD,
    ftCurrency         : aWorksheet.WriteNumber(aRow, aCol, aField.AsFloat, nfFixedTh);
    ftDateTime         : aWorksheet.WriteDateTime(aRow, aCol,aField.AsDateTime, nfShortDateTime);
    ftBoolean          : aWorksheet.WriteBoolValue(aRow, aCol,aField.AsBoolean);
    ftString,
    ftFixedChar,
    ftWideString,
    ftFixedWideChar    : aWorksheet.WriteText(aRow, aCol,aField.AsString)
    else
      begin
        WriteStr(_S, aField.DataType);
        aWorksheet.WriteText(aRow,aCol,_S);
      end;
  end;

  if aBorders then
    aWorksheet.WriteBorders(aRow,aCol,[cbNorth, cbWest, cbEast, cbSouth]);

  aWorksheet.WriteFontStyle(aRow,aCol,aFontStyles);
  aWorksheet.WriteFontColor(aRow,aCol,aFontColor);
  aWorksheet.WriteBackgroundColor(aRow,aCol,aCellColor);

end;

procedure WriteValue(aWorksheet: TsWorksheet; aRow, aCol: integer; aValue: variant; const aFontStyles: TsFontStyles; aCellColor: TsColor; aFontColor: TsColor;
  aBorders: boolean);
var
  _S: string;
begin

  case TVarData(aValue).VType of
    varSmallInt,
    varInteger,
    varint64        : aWorksheet.WriteNumber(aRow, aCol, aValue, nfGeneral);
    varSingle,
    varDouble,
    varCurrency     : aWorksheet.WriteNumber(aRow, aCol, aValue, nfFixedTh);
    varDate         : aWorksheet.WriteDateTime(aRow, aCol, aValue, nfShortDateTime);
    varBoolean      : aWorksheet.WriteBoolValue(aRow, aCol, aValue);
    varString       : aWorksheet.WriteText(aRow, aCol, aValue);
    else
      begin
        WriteStr(_S, TVarData(aValue).VType);
        aWorksheet.WriteText(aRow,aCol,_S);
      end;
  end;

  if aBorders then
    aWorksheet.WriteBorders(aRow,aCol,[cbNorth, cbWest, cbEast, cbSouth]);

  aWorksheet.WriteFontStyle(aRow,aCol,aFontStyles);
  aWorksheet.WriteFontColor(aRow,aCol,aFontColor);
  aWorksheet.WriteBackgroundColor(aRow,aCol,aCellColor);

end;

procedure WriteValue(aWorksheet: TsWorksheet; aRow, aCol: integer;
  aField: TField; aFont: TsFont; aCellFormat: TsCellFormat;
  aNumFormat: TsNumberFormat; aNumFormatStr: string; aCellBorders: TsCellBorders
  );
var
  _S: string;
  aCell: PCell;
begin

  case aField.DataType of
    ftSmallInt,
    ftLargeint,
    ftInteger          : aCell:= aWorksheet.WriteNumber(aRow, aCol, aField.AsInteger, aNumFormat, aNumFormatStr);
    ftFloat,
    ftBCD,
    ftCurrency         : aCell:= aWorksheet.WriteNumber(aRow, aCol, aField.AsFloat, aNumFormat, aNumFormatStr);
    ftDateTime         : aCell:= aWorksheet.WriteDateTime(aRow, aCol,aField.AsDateTime, nfShortDateTime);
    ftBoolean          : aCell:= aWorksheet.WriteBoolValue(aRow, aCol,aField.AsBoolean);
    ftString,
    ftFixedChar,
    ftWideString,
    ftFixedWideChar    : aCell:= aWorksheet.WriteText(aRow, aCol,aField.AsString)
    else
      begin
        WriteStr(_S, aField.DataType);
        aCell:= aWorksheet.WriteText(aRow,aCol,_S);
      end;
  end;

  aWorksheet.WriteBorders(aCell,aCellBorders);
  aWorksheet.WriteFont(aCell, aFont.FontName, aFont.Size, aFont.Style, aFont.Color, aFont.Position);
  aWorksheet.WriteCellFormat(aCell, aCellFormat);

end;

procedure ChangeColor(Sender: TObject);
begin
  if (Sender is TEdit) then
    if TEdit(Sender).Color = clDefault then
      TEdit(Sender).Color:= clSkyBlue
    else
      TEdit(Sender).Color:= clDefault
end;

function IsEmpty(aString: string): boolean;
begin
  Result:= (Length(aString)=0) or (aString = null);
end;

function EmptyBaseID: variant;
begin
  Result:= wfEmptyBaseID;
end;

function IsEmpty(aID: BaseID): boolean;
begin
  Result:= (aID = EmptyBaseID);
end;

function IsEmpty(aStrings: TStrings): boolean;
begin
  Result:= Length(Trim(aStrings.Text))=0;
end;

function IsEmpty(aObject: TObject): boolean;
begin
  Result:= not Assigned(aObject);
end;

function IsEntry(aSearch: string; aInText: string): boolean;
begin
  Result:= UTF8Pos(aSearch, aInText)>0;
end;

function GetApplicationPathUnsafe: string;
begin
  Result:= SysToUTF8(ExtractFilePath(ParamStr(0)));
end;

function GetSpreadSheetFormat(aFileName:string):TsSpreadsheetFormat;
begin
case UTF8LowerCase(ExtractFileExt(aFileName)) of
    '.xls': Result:= sfExcel8;
    '.xlsx': Result:= sfOOXML;
    '.ods': Result:= sfOpenDocument;
  end;
end;

function GetMD5Hash(aStr: string): string;
var
  a: TMDDigest;
  i: integer;
begin
  Result := '';
  a      := MD5String(aStr);
  for i := Low(a) to High(a) do
    Result := Result + IntToHex(a[i], 1);
end;

function GetUID: string;
var
  aGuid: TGUID;
begin
  CreateGUID(aGuid);
  Result:=GetOnlyCorrectChars(GUIDToString(aGuid))+'1';
end;

function GetVersion: string;
var
  aVersion: string;
  aInfo: TVersionInfo;
begin
  aInfo := TVersionInfo.Create;
  try
    aInfo.Load(HINSTANCE);
    //[0] = Major aVersion, [1] = Minor ver, [2] = Revision, [3] = Build Number
    aVersion := IntToStr(aInfo.FixedInfo.FileVersion[0]) + '.' + IntToStr(
      aInfo.FixedInfo.FileVersion[1]) + '.' + IntToStr(aInfo.FixedInfo.FileVersion[2]) +
      '.' + IntToStr(aInfo.FixedInfo.FileVersion[3]);
    Result := aVersion;
  finally
    aInfo.Free;
  end;
end;

function CalcMD5File(aFileName: string): string;
var
  aMD5Stream: TMD5Digest;
  aFileMD5: TMemoryStream;
begin
  Result := '';
  if FileExists(aFileName) then
  begin
    aFileMD5 := TMemoryStream.Create;

    try
      aFileMD5.LoadFromFile(aFileName);
      aFileMD5.Position := 0;
      aMD5Stream := MD5Buffer(aFileMD5.Memory^, aFileMD5.Size);
      Result := MD5Print(aMD5Stream);
      aFileMD5.Free;
    except
      aFileMD5.Free;
    end;
  end;

end;

function VarToStr(Value: variant): string;
begin
  Result := '';
  if Value <> null then
    case TVarData(Value).VType of
      varSmallInt,
      varInteger       : Result := IntToStr(Value);
      varint64         : Result := IntToStr(int64(Value));
      varSingle,
      varDouble,
      varCurrency      : Result := FloatToStr(Value);
      varDate          : Result:= FormatDateTime('dd.mm.yyyy', Value);
      varBoolean       : if Value then
                        Result := 'T'
                      else
                        Result := 'F';
      varString        : Result := Value
      else
        WriteStr(Result, TVarData(Value).VType)
    end;
end;

function VarToBool(Value: variant): boolean;
begin
  Result:= (TVarData(Value).VType = varboolean) and Value;
end;

function VarToInt64(Value: variant): Int64;
begin
  TryStrToInt64(VarToStr(Value),Result);
end;

function VarToCurr(Value: variant): Currency;
begin
  TryStrToCurr(VarToStr(Value),Result);
end;

function VarToBaseID(aValue: variant): BaseID;
begin
  {$IFDEF USEGUID}
    Result:= VarToStr(aValue);
  {$ELSE}
    Result:= VarToInt64(aValue);
  {$ENDIF}
end;

function BoolToInt(aValue: Boolean): Integer;
begin
  Result:= 0;
  if aValue then Result:= 1;
end;

function BaseIDToStr(aValue: BaseID): string;
begin
  {$IFDEF USEGUID}
    Result:= QuotedStr(aValue);
  {$ELSE}
    Result:= IntToStr(aValue);
  {$ENDIF}
end;

function GetVarType(aValue: variant):tvartype;
begin
  Result:= TVarData(aValue).VType;
end;

function GetOnlyCorrectChars(aValue: string): string;
var
  N: Integer;
begin
  for N:= Length(aValue) downto 1 do
        if not (aValue[N] in ['a'..'z','A'..'Z', '0'..'9']) then Delete(aValue, N, 1);

  Result:= aValue;
end;

function GetOnlyChars(aValue: string): string;
var
  N: Integer;
begin
  for N:= Length(aValue) downto 1 do
        if not (aValue[N] in ['a'..'z','A'..'Z','_']) then Delete(aValue, N, 1);

  Result:= aValue;
end;

function GetOnlyNumbers(aValue: string): Double;
var
  N: Integer;
begin
  for N:= Length(aValue) downto 1 do
        if not (aValue[N] in [DefaultFormatSettings.DecimalSeparator, '0'..'9']) then Delete(aValue, N, 1);
  TryStrToFloat(aValue, Result);
end;

function GetOnlyDateTime(aValue: string): TDateTime;
var
  N: Integer;
begin
  for N:= Length(aValue) downto 1 do
        if not (aValue[N] in ['.',':',' ', '0'..'9']) then Delete(aValue, N, 1);

  TryStrToDate(UTF8Trim(aValue),Result);
end;

procedure GetExcelFilesList(aPath: string; aList: TStrings );
var
sRec: TSearchRec;
isFound: boolean;
begin
  isFound := FindFirst( aPath + '\*.*', faAnyFile, sRec ) = 0;

  while isFound do
  begin
    if ( sRec.Name <> '.' ) and ( sRec.Name <> '..' ) then
    begin
      if ( sRec.Attr and faDirectory ) = faDirectory then
        GetExcelFilesList( aPath + '\' + sRec.Name, aList )
      else
        if (UTF8LowerCase(ExtractFileExt(sRec.Name)) = '.xls')
        or (UTF8LowerCase(ExtractFileExt(sRec.Name)) = '.xlsx') then
          aList.Add( aPath + '\' + sRec.Name );
    end;

    isFound := FindNext( sRec ) = 0;
  end;

  FindClose( sRec );
end;

procedure DeleteEmplyItems(aStringList: TStringList);
var
  i: Integer;
begin
  for i:=aStringList.Count-1 downto 0 do
    if Length(trim(aStringList.Strings[i]))=0
      then aStringList.Delete(i);
end;

function getUTFSymbol(S:string;i:integer):string;
begin
Result:= UTF8Copy(S,i,1);
end;

procedure WriteUTF8String(aFileStream: TFileStream; aText: RawByteString);
begin
  aText := aText + LineEnding;
  SetCodePage(aText, CP_UTF8, True);
  SetCodePage(aText, CP_NONE, False);
  aFileStream.WriteBuffer(aText[1], Length(aText));
end;

end.

