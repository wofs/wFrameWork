{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfImportReaderXLSU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gvector, wfTypes, wfClasses, wfFunc, wfBase, LazUTF8, wfFormatParserU, wfImportReaderU,
  fpspreadsheet, fpsTypes, fpsallformats, fpsutils;

type

  TwfGroupImportFlags = record
    ColorGroup  : TsColor;
    ColorCell   : TsColor;
    FormatSaved : boolean;
  end;

  { TwfImportReaderXLS }

  TwfImportReaderXLS = class (TwfImportReader)
  private

    fWorkBook: TsWorkbook;
    fWorksheet: TsWorksheet;
    fGroupImportFlags: TwfGroupImportFlags;

    // Procedure performed after opening a spreadsheet
    procedure FOpenWorkBook(Sender: TObject);
    // Returns cell background color
    function GetBackground(aCell: PCell): TsColor;
    function GetGroupIndexByName(aName: string): integer;
    // We record the found content cells
    procedure ParseCell(aCell: PCell);
    // Returns the line contained in the cell
    function GetDataString(aCell: PCell; const aValueType: TDataType=dtDefault): string;
    // Returns the variant value contained in the cell.
    function GetDataVariant(aCell: PCell; const aValueType: TDataType=dtDefault): variant;
    // Returns a table field by parameter name
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
    // Reading groups
    procedure ParseGroup(aCell: PCell);
    // If the groups in the rows
    procedure ParseGroupInRows(aName: string; aCell: PCell);
    // If groups are in columns
    procedure ParseGroupNoInRows(aName: string; aCell: PCell);
    // Initializes a record event of the collected data row.

  public
    constructor Create(aSource: string; aFormat: TStrings); override;
    destructor Destroy; override;
    // Start reading a spreadsheet file
    procedure Start;
  end;

implementation

{ TwfImportReaderXLS }

function TwfImportReaderXLS.GetField(aParam: string):string;
begin
  Result:= Format.GetValueByParam(aParam, ParamsSection);
end;

function TwfImportReaderXLS.GetGroupIndexByName(aName: string):integer;
var
  i: QWord;
  aGroup: TwfGroupCell;
begin
  Result:= -1;

  for aGroup in Groups do begin
    if aName = aGroup.Value then
    begin
      Result:= i+1;
      Break;
    end;
  end;
end;

// We record the found content cells
procedure TwfImportReaderXLS.ParseCell(aCell: PCell);
var
  aRowCol: TwfRowCol;
  i: Integer;
begin
  for i:= 0 to High(DataSection) do begin
    aRowCol:= GetRowCol(DataSection[i].Value);

    if (aRowCol.Col = aCell^.Col) then
    begin
      //Content add
      ContentRow.Row[i].Name:= DataSection[i].Name;
      ContentRow.Row[i].Field:= GetField(DataSection[i].Name);
      ContentRow.Row[i].Value:= GetDataVariant(aCell, DataSection[i].DataType);
    end;
  end;

  ParseGroup(aCell);
end;

procedure TwfImportReaderXLS.ParseGroupNoInRows(aName: string; aCell: PCell);
var
  i, aIndexGroup: Integer;
  aRowCol: TwfRowCol;
begin
  if (Groups.IsEmpty or
     (GroupCurrent.Value <> aCell^.UTF8StringValue))
   and
     (UTF8Length(aCell^.UTF8StringValue)>0) then
     begin
       aIndexGroup:= GetGroupIndexByName(aCell^.UTF8StringValue);
       if (aIndexGroup<0) then
         aIndexGroup:= AddGroup(aName, GetField(aName), aCell^.UTF8StringValue);

       ContentRowSetGroup(aIndexGroup);
     end;
end;

procedure TwfImportReaderXLS.ParseGroupInRows(aName: string; aCell: PCell);
var
  i, aIndexGroup: Integer;
  aRowCol: TwfRowCol;
  aValue: String;
begin
  if not fGroupImportFlags.FormatSaved then
  begin
    fGroupImportFlags.ColorGroup := GetBackground(aCell);
    fGroupImportFlags.FormatSaved := True;
  end;

  fGroupImportFlags.ColorCell:= GetBackground(aCell);
  aValue:= UTF8Trim(aCell^.UTF8StringValue);

  if (fGroupImportFlags.ColorGroup = fGroupImportFlags.ColorCell) and (UTF8Length(aValue) = 0) then
  begin
   Groups.Clear;
   aIndexGroup:= AddGroup(aName, GetField(aName), aValue, fGroupImportFlags.ColorCell);

   ContentRowSetGroup(aIndexGroup);

   //GroupCunnrentName := UTF8Copy(Trim(ADataCell^.UTF8StringValue),1,255);
   //GroupCunnrentIndex:= Base.SQLInsert('PL_GROUP',['IDPARENT','NAME','IDOWNER','IDFORMATS','FTIMESTAMP'],[GroupRootIndex,GroupCunnrentName,OwnerID ,FormatID ,TimeStamp],'IDOWNER, NAME, IDPARENT',false);
   //
   //GroupCunnrentLevel := FGroup.AddObject(IntToStr(GroupRootIndex), TwData.Create(GroupCunnrentIndex,_bgColorCell));
   //
  end
  else   // иначе
  begin
    { TODO -owofs -cwfImportReaderXLSU : Дописать детект групп }
      // GroupAlgorithmCol / проверяем не пусто ли 0 - цена, 1 - идентификатор
     if (Length(GetDataString(FWorksheet.Cells.FindCell(ADataCell^.Row, CollArray[DetectGroupColumn]-1))) = 0) and (Length(ADataCell^.UTF8StringValue)>0) then
     begin
       _LevelFromColor := ColorInUsed(ADataCell, FGroup);

       if _LevelFromColor > 0 then
       begin

         GroupCunnrentName := UTF8Copy(Trim(ADataCell^.UTF8StringValue),1,255);
         GroupCunnrentLevel:= _LevelFromColor;

         for igroup:= FGroup.Count-1 downto GroupCunnrentLevel+1 do
          begin
            TwData(FGroup.Objects[igroup]).Free;
            FGroup.Delete(igroup);
          end;
         GroupCunnrentIndex:= Base.SQLInsert('PL_GROUP',['IDPARENT','NAME','IDOWNER','IDFORMATS','FTIMESTAMP'],[FGroup.Strings[GroupCunnrentLevel],GroupCunnrentName,OwnerID ,FormatID ,TimeStamp],'IDOWNER, NAME, IDPARENT',false);

         TwData(FGroup.Objects[GroupCunnrentLevel]).ID:=GroupCunnrentIndex;
       end
       else
       begin
         GroupCunnrentName := UTF8Copy(Trim(ADataCell^.UTF8StringValue),1,255);
         GroupCunnrentIndex:= Base.SQLInsert('PL_GROUP',['IDPARENT','NAME','IDOWNER','IDFORMATS','FTIMESTAMP'],[TwData(FGroup.Objects[GroupCunnrentLevel]).ID,GroupCunnrentName,OwnerID ,FormatID ,TimeStamp],'IDOWNER, NAME, IDPARENT',false);


         GroupCunnrentLevel := FGroup.AddObject(IntToStr(TwData(FGroup.Objects[GroupCunnrentLevel]).ID), TwData.Create(GroupCunnrentIndex,_bgColorCell));
       end;
     end;
  end;

end;

procedure TwfImportReaderXLS.ParseGroup(aCell: PCell);
var
  aName, aValue: String;
  i: Integer;
  aRowCol: TwfRowCol;
begin
  for i:= 0 to High(GroupsSection) do begin
    aName:= GroupsSection[i].Name;
    aValue:= GroupsSection[i].Value;
    aRowCol:= GetRowCol(aValue);

    if (aRowCol.Col = aCell^.Col) then
      case Format.GroupInRows of
        girNo   : ParseGroupNoInRows(aName, aCell);
        girYes  : ParseGroupInRows(aName, aCell);
      end;
  end;
end;

procedure TwfImportReaderXLS.FOpenWorkBook(Sender: TObject);
var
  ADataCell: PCell;
  aRow, aCol, aLastCol: Cardinal;
  aRowCurrent, aFirstRow, aFirstCol: integer;
begin
    fWorksheet:= GetWorkSheet(Format.WorkSheet);
    aFirstRow:= Format.FirstRow-1;
    aFirstCol:= Format.FirstCol;
    aLastCol:= fWorksheet.GetLastColNumber;

    aRowCurrent:= -1;
    ContentRowSetLength(Length(Format.DataSection));

       for ADataCell in fWorksheet.Cells do
       begin
         aRow:= aDataCell^.Row;
         aCol:= aDataCell^.Col;

         if (aRow >= aFirstRow) and (aCol >= aFirstCol) then begin
           if (aRow <> aRowCurrent) then
           begin
             if (Format.GroupInRows = girNo) then
               Groups.Clear;

             ContentRowClear();
             aRowCurrent:= aRow;
           end;

            ParseCell(ADataCell);

           if (aCol = aLastCol) and (IsContent()) then
           begin
              WriteContentRow(Groups, ContentRow);
              ContentRowClear();
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

  with fGroupImportFlags do begin
    ColorCell:= 0;
    ColorGroup:= 0;
    FormatSaved:= false;
  end;
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

function TwfImportReaderXLS.GetDataString(aCell: PCell; const aValueType: TDataType = dtDefault): string;
var
  aValTmp: Double;
  N: Integer;
begin
  Result:='';
  if Assigned(aCell) then
  begin
    case aValueType of
      dtDefault, dtNotUsed:
            begin
              case aCell^.ContentType of
                cctNumber: Result := FloatToStr(aCell^.NumberValue);
                cctDateTime: Result := DateToStr(aCell^.DateTimeValue);
                else
                  Result := UTF8StringReplace(aCell^.UTF8StringValue, #39, #39+#39,[]);
              end
            end;
      dtNumber:
            begin
              Result := UTF8StringReplace(aCell^.UTF8StringValue, ',',DefaultFormatSettings.DecimalSeparator,[]);
              Result := UTF8StringReplace(Result, '.',DefaultFormatSettings.DecimalSeparator,[]);

              for N:= Length(Result) downto 1 do
              if not (Result[N] in [DefaultFormatSettings.DecimalSeparator, '0'..'9']) then Delete(Result, N, 1);

              TryStrToFloat(Result,aValTmp);
              Result:= FloatToStr(aValTmp);
            end;
      dtString:
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

function TwfImportReaderXLS.GetDataVariant(aCell: PCell; const aValueType: TDataType = dtDefault): variant;
var
  aValTmp: Double;
  N: Integer;
begin
  Result:='';
  if Assigned(aCell) then
  begin
    case aValueType of
      dtDefault, dtNotUsed:
            begin
              case aCell^.ContentType of
                cctNumber: Result := aCell^.NumberValue;
                cctDateTime: Result := aCell^.DateTimeValue;
                else
                  Result := aCell^.UTF8StringValue;
              end
            end;
      dtNumber:  Result:= aCell^.NumberValue;
      dtString:  Result:= aCell^.UTF8StringValue;
    end;
  end
  else
    Result:='';
end;

end.

