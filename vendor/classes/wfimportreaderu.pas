unit wfImportReaderU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, wfClasses, wfFunc, wfBase, LazUTF8, wfFormatParserU, fpspreadsheet, fpsTypes,
  fpsallformats, fpsutils;

type

  TwfImportContentCell = Record
    Name: string;
    Field: string;
    Value: variant;
  end;

  TwfImportContentRow = Record
    Row: array of TwfImportContentCell;
  end;

  TwfWriteContentRowEvent = procedure (Sender: TObject; var aContentRow: TwfImportContentRow) of object;

  { TwfImportReaderXLS }

  TwfImportReaderXLS = class
  private
    fFormat: TwfFormatPaser;
    fonWriteContentRow: TwfWriteContentRowEvent;

    fWorkBook: TsWorkbook;
    fWorksheet: TsWorksheet;
    fSource: string;
    fDataSection: TwfFormatSection;
    fParamsSection: TwfFormatSection;
    fLogicSection: TwfFormatSection;
    // Clearing the content line between fill interations
    procedure ContentRowClear(var aContentRow: TwfImportContentRow);
    procedure FOpenWorkBook(Sender: TObject);
    // Returns cell background color
    function GetBackground(aCell: PCell): TsColor;
    function IsContent(var aContentRow: TwfImportContentRow): boolean;
    procedure SetContentCells(var aContentRow: TwfImportContentRow; aCell: PCell);
    // Returns the line contained in the cell
    function GetDataString(aCell: PCell; const aValueType: TValueType=vtDefault): string;
    function GetField(aParam: string): string;
    // Returns font style
    function GetFontStyles(aCell: PCell): TsFontStyles;
    // Returns cell format
    function GetFormat(aRow, aCol: cardinal): TsUsedFormattingFields;
    // Returns cell format
    function GetFormat(aCell: PCell): TsUsedFormattingFields;
    // Converts address to TwfRowCol
    function GetRowCol(aAddress: string): TwfRowCol;
    // Returns Row Col of Cell
    procedure GetRowCol(aDataCell: PCell; out aRow: Cardinal; out aCol: Cardinal);
    // Returns the index sheet
    function GetWorkSheet(aIndex: integer): TsWorksheet;
    procedure WriteContentRow(var aContentRow: TwfImportContentRow);

  public
    constructor Create(aSource: string; aFormat: TStrings);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Format: TwfFormatPaser read fFormat;

    {Events}
    // You must implement the data write event yourself
    property onWriteContentRow: TwfWriteContentRowEvent read fonWriteContentRow write fonWriteContentRow;
  end;

implementation

{ TwfImportReaderXLS }

procedure TwfImportReaderXLS.ContentRowClear(var aContentRow: TwfImportContentRow);
var
  i: Integer;
begin
  for i:=0 to High(aContentRow.Row) do begin
    aContentRow.Row[i].Name:= wfEmptyStr;
    aContentRow.Row[i].Field:= wfEmptyStr;
    aContentRow.Row[i].Value:= wfEmptyInt;
  end;
end;

//TwfFormatRecord = record
//  Data: string;
//  DataType: TwfFormatDataType;
//  Value: string;
//  aComplexType: boolean;
//  aCalculatedType: boolean;
//end;


//Section
//  Data: string;
//  DataType: TwfFormatDataType;
//  Value: string;
//  aComplexType: boolean;
//  aCalculatedType: boolean;
//end;
//fDataSection
//fParamsSection
//fLogicSection

function TwfImportReaderXLS.GetField(aParam: string):string;
begin
  Result:= Format.GetValueByParam(aParam, fParamsSection);
end;

// We record the found content cells
procedure TwfImportReaderXLS.SetContentCells(var aContentRow: TwfImportContentRow; aCell: PCell);
var
  aRowCol: TwfRowCol;
  i: Integer;
  aName, aValue: String;
begin
  for i:= 0 to High(fDataSection) do begin
    aValue:= VarToStr(fDataSection[i].Value);
    aName:= fDataSection[i].Name;
    aRowCol:= GetRowCol(fDataSection[i].Value);

    if (aRowCol.Col = aCell^.Col) then
    begin
       aContentRow.Row[i].Name:= fDataSection[i].Name;
       aContentRow.Row[i].Field:= GetField(fDataSection[i].Name);
       aContentRow.Row[i].Value:= GetDataString(aCell, fDataSection[i].DataType);
    end;
  end;
end;

procedure TwfImportReaderXLS.FOpenWorkBook(Sender: TObject);
var
  ADataCell: PCell;
  aRow, aCol: Cardinal;
  aRowCurrent, aFirstRow, aFirstCol: integer;
  aContentRow: TwfImportContentRow;
begin
{ TODO -owofs -cTwfImportReader : Дописать работу со сложными данными:
Объединение строк
Суммирование чисел }
  fWorksheet:= GetWorkSheet(Format.WorkSheet);
  aFirstRow:= Format.FirstRow;
  aFirstCol:= Format.FirstCol;
  aRowCurrent:= -1;
  SetLength(aContentRow.Row, Length(Format.DataSection));

     for ADataCell in fWorksheet.Cells do
     begin
       aRow:= aDataCell^.Row;
       aCol:= aDataCell^.Col;

       if (aRow>= aFirstRow) and (aCol>=aFirstCol) then begin
         if (aRow <> aRowCurrent) then
         begin
           ContentRowClear(aContentRow);
           aRowCurrent:= aRow;
         end;

          SetContentCells(aContentRow, ADataCell);

         if IsContent(aContentRow) then
         begin
            WriteContentRow(aContentRow);
            ContentRowClear(aContentRow);
         end;
       end;
     end;
end;

procedure TwfImportReaderXLS.WriteContentRow(var aContentRow: TwfImportContentRow);
begin
  if Assigned(fonWriteContentRow) then
    fonWriteContentRow(self, aContentRow);
end;

function TwfImportReaderXLS.IsContent(var aContentRow: TwfImportContentRow):boolean;
const
  uIsContent = 'ЗаписьЕсли';
var
  aRecordCondition: ArrayOfString;
  i, k, aCount: Integer;
  aRecord, aName: String;
begin
 Result:= false;
 aCount:= 0;
 aRecordCondition:= ArrayAsString(Format.GetValueByParam(uIsContent, fLogicSection), '+');

 for i:=0 to High(aContentRow.Row) do begin
   for k:= 0 to High(aRecordCondition) do begin
     aRecord:= aRecordCondition[k];
     aName:= aContentRow.Row[i].Name;
     if UTF8UpperCase(aName) = UTF8UpperCase(aRecord) then
     inc(aCount);
   end;
 end;

 Result:= aCount = Length(aRecordCondition);
end;

constructor TwfImportReaderXLS.Create(aSource: string; aFormat: TStrings);
begin
  fSource:= aSource;
  fFormat:= TwfFormatPaser.Create(aFormat);
  fDataSection:= fFormat.DataSection;
  fParamsSection:= fFormat.ParamsSection;
  fLogicSection:= fFormat.LogicSection;
  fWorkBook := TsWorkbook.Create;

  fWorkBook.Options := fWorkBook.Options + [boBufStream];
  fWorkBook.OnOpenWorkbook:= @FOpenWorkBook;
  //Use the Start procedure to start the import
end;

destructor TwfImportReaderXLS.Destroy;
begin
  FreeAndNil(fFormat);
  FreeAndNil(fWorkBook);

  inherited Destroy;
end;

procedure TwfImportReaderXLS.Start;
begin
  fWorkBook.ReadFromFile(fSource);
end;

procedure TwfImportReaderXLS.Stop;
begin
 { TODO -owofs -cTwfImportReader : Дописать останов импорта }
end;

function TwfImportReaderXLS.GetWorkSheet(aIndex: integer):TsWorksheet;
begin
  Result:= fWorkBook.GetWorksheetByIndex(aIndex);
end;

function TwfImportReaderXLS.GetFormat(aRow, aCol: cardinal): TsUsedFormattingFields;
begin
  Result := GetFormat(FWorksheet.Cells.FindCell(aRow, aCol));
end;

function TwfImportReaderXLS.GetFormat(aCell: PCell): TsUsedFormattingFields;
begin
  Result := fWorksheet.ReadUsedFormatting(aCell);
end;

function TwfImportReaderXLS.GetBackground(aCell: PCell): TsColor;
begin
  Result := fWorksheet.ReadBackground(aCell).FgColor;
end;

function TwfImportReaderXLS.GetFontStyles(aCell: PCell): TsFontStyles;
begin
  Result := fWorksheet.ReadCellFont(aCell).Style;
end;

function TwfImportReaderXLS.GetRowCol(aAddress: string): TwfRowCol;
var
  aAddressTemp: String;
  aCol: Longint;
begin
  //aAddress:= 'A5*10';
  aAddressTemp:= aAddress;

  with Result do begin
    case GetCellAdressType(aAddress) of
      catNumberOnly: begin
        Row:=0;
        TryStrToInt(aAddress, aCol);
        Col:= aCol;
      end;
      catCharOnly: begin
        ParseCellString(aAddressTemp+'1', Row, Col);
      end;
      catFull: ParseCellString(aAddressTemp, Row, Col);
    end;
  end;
end;

procedure TwfImportReaderXLS.GetRowCol(aDataCell: PCell; out aRow: Cardinal; out aCol: Cardinal);
begin
  aRow := aDataCell^.Row;
  aCol := aDataCell^.Col;
end;

function TwfImportReaderXLS.GetDataString(aCell: PCell; const aValueType: TValueType = vtDefault): string;
var
  aValTmp: Double;
  N: Integer;
begin
  Result:='';
  if Assigned(aCell) then
  begin
    case aValueType of
      vtDefault, vtNotUsed:
            begin
              case aCell^.ContentType of
                cctNumber: Result := FloatToStr(aCell^.NumberValue);
                cctDateTime: Result := DateToStr(aCell^.DateTimeValue);
                else
                  Result := UTF8StringReplace(aCell^.UTF8StringValue, #39, #39+#39,[]);
              end
            end;
      vtNumber:
            begin
              Result := UTF8StringReplace(aCell^.UTF8StringValue, ',',DefaultFormatSettings.DecimalSeparator,[]);
              Result := UTF8StringReplace(Result, '.',DefaultFormatSettings.DecimalSeparator,[]);

              for N:= Length(Result) downto 1 do
              if not (Result[N] in [DefaultFormatSettings.DecimalSeparator, '0'..'9']) then Delete(Result, N, 1);

              TryStrToFloat(Result,aValTmp);
              Result:= FloatToStr(aValTmp);
            end;
      vtString:
            begin
                case aCell^.ContentType of
                  cctNumber: Result := FloatToStr(aCell^.Numbervalue);
                  cctUTF8String: Result :=  UTF8Trim(UTF8StringReplace(aCell^.UTF8StringValue, #39, '',[]));
                end;
            end;
    end;
  end
  else
  Result:='';
end;

end.

