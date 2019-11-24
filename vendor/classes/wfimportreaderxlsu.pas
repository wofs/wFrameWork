unit wfImportReaderXLSU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, wfClasses, wfFunc, wfBase, LazUTF8, wfFormatParserU, wfImportReaderU, fpspreadsheet,
  fpsTypes, fpsallformats, fpsutils;

type

  { TwfImportReaderXLS }

  TwfImportReaderXLS = class (TwfImportReader)
  private

    fWorkBook: TsWorkbook;
    fWorksheet: TsWorksheet;

    procedure FOpenWorkBook(Sender: TObject);
    // Returns cell background color
    function GetBackground(aCell: PCell): TsColor;
    // We record the found content cells
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
    // Initializes a record event of the collected data row.

  public
    constructor Create(aSource: string; aFormat: TStrings); override;
    destructor Destroy; override;

    procedure Start;
  end;

implementation

{ TwfImportReaderXLS }

function TwfImportReaderXLS.GetField(aParam: string):string;
begin
  Result:= Format.GetValueByParam(aParam, ParamsSection);
end;

// We record the found content cells
procedure TwfImportReaderXLS.SetContentCells(var aContentRow: TwfImportContentRow; aCell: PCell);
var
  aRowCol: TwfRowCol;
  i, a: Integer;
  aName, aValue: String;
begin
  for i:= 0 to High(DataSection) do begin
    aValue:= VarToStr(DataSection[i].Value);
    aName:= DataSection[i].Name;
    aRowCol:= GetRowCol(DataSection[i].Value);

    if (aRowCol.Col = aCell^.Col) then
    begin
       aContentRow.Row[i].Name:= DataSection[i].Name;
       aContentRow.Row[i].Field:= GetField(DataSection[i].Name);

      if DataSection[i].aComplexType then
         aContentRow.Row[i].Value:= GetComplexTypeValue(DataSection[i].Value, aContentRow)
      else
         aContentRow.Row[i].Value:= GetDataString(aCell, DataSection[i].DataType);
    end;
  end;
end;

procedure TwfImportReaderXLS.FOpenWorkBook(Sender: TObject);
var
  ADataCell: PCell;
  aRow, aCol, aLastCol: Cardinal;
  aRowCurrent, aFirstRow, aFirstCol: integer;
  aContentRow: TwfImportContentRow;
begin
{ TODO -owofs -cTwfImportReader : Дописать работу со сложными данными:
Объединение строк
Суммирование чисел }
  fWorksheet:= GetWorkSheet(Format.WorkSheet);
  aFirstRow:= Format.FirstRow-1;
  aFirstCol:= Format.FirstCol;
  aLastCol:= fWorksheet.GetLastColNumber;

  aRowCurrent:= -1;
  SetLength(aContentRow.Row, Length(Format.DataSection));

     for ADataCell in fWorksheet.Cells do
     begin
       aRow:= aDataCell^.Row;
       aCol:= aDataCell^.Col;

       if (aRow >= aFirstRow) and (aCol >= aFirstCol) then begin
         if (aRow <> aRowCurrent) then
         begin
           ContentRowClear(aContentRow);
           aRowCurrent:= aRow;
         end;

          SetContentCells(aContentRow, ADataCell);

         if (aCol = aLastCol) and (IsContent(aContentRow)) then
         begin
            WriteContentRow(aContentRow);
            ContentRowClear(aContentRow);
         end;
       end;
     end;
end;


constructor TwfImportReaderXLS.Create(aSource: string; aFormat: TStrings);
begin
  inherited;
  fWorkBook := TsWorkbook.Create;

  fWorkBook.Options := fWorkBook.Options + [boBufStream];
  fWorkBook.OnOpenWorkbook:= @FOpenWorkBook;
  //Use the Start procedure to start the import
end;

destructor TwfImportReaderXLS.Destroy;
begin
  inherited;
  FreeAndNil(fWorkBook);
end;

procedure TwfImportReaderXLS.Start;
begin
  fWorkBook.ReadFromFile(Source);
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

