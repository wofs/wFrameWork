unit wfImportReaderU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, wfClasses, LazUTF8, wfFormatParserU, fpspreadsheet, fpsTypes, fpsallformats, fpsutils;

type

  { TwfImportReaderXLS }

  TwfImportReaderXLS = class
  private
    fFormat: TwfFormatPaser;
    fWorkBook: TsWorkbook;
    fWorksheet: TsWorksheet;
    fSource: string;
    procedure FOpenWorkBook(Sender: TObject);
    // Returns cell background color
    function GetBackground(aCell: PCell): TsColor;
    // Returns the line contained in the cell
    function GetDataString(aCell: PCell; const aValueType: TValueType=vtDefault): string;
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

  public
    constructor Create(aSource: string; aFormat: TStrings);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Format: TwfFormatPaser read fFormat;
  end;

implementation

{ TwfImportReaderXLS }

procedure TwfImportReaderXLS.FOpenWorkBook(Sender: TObject);
var
  ADataCell: PCell;
  aRow, aCol, aRowCurrent: Cardinal;
begin
  fWorksheet:= GetWorkSheet(Format.WorkSheet);
  aRowCurrent:= -1;

     for ADataCell in fWorksheet.Cells do
     begin
       aRow:= aDataCell^.Row;
       aCol:= aDataCell^.Col;

       if (aRow <> aRowCurrent) then
       begin
         //VarContentClear();
         aRowCurrent:= aRow;
       end;
  { TODO : Дописать импорт }
       //if IsProject(aDataCell) then
       //   EstResult.ConstructionProject:= GetDataString(aDataCell);


       // Контент

       //if IsContentNumber(aDataCell) then
       //   aContentNumber:= GetDataString(aDataCell);
       //
       //if IsContentCode(aDataCell) then
       //   aContentCode:= GetDataString(aDataCell);



       if 1=1 then //IsContent()
       begin
          //EstResult.AddContent();
          //VarContentClear();
       end;
     end;
end;

constructor TwfImportReaderXLS.Create(aSource: string; aFormat: TStrings);
begin
  fSource:= aSource;
  fFormat:= TwfFormatPaser.Create(aFormat);
  fWorkBook := TsWorkbook.Create;

  fWorkBook.Options := fWorkBook.Options + [boBufStream];
  fWorkBook.OnOpenWorkbook:= @FOpenWorkBook;
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
  aPosAsterisk: PtrInt;
  aAddressTemp: String;
begin
  //aAddress:= 'A5*10';
  aAddressTemp:= aAddress;

  with Result do begin
    aPosAsterisk:= UTF8Pos('*', aAddressTemp);

    if aPosAsterisk>0 then
      aAddressTemp:= UTF8Copy(aAddressTemp,1,aPosAsterisk-1);

      ParseCellString(aAddressTemp, Row, Col);
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
      vtDefault:
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

