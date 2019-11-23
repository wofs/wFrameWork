{
This file is part of wfFrameWork.

 -= Report =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfReport;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
      cthreads,
      cmem,
    {$ENDIF}{$ENDIF}
    Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, LazUTF8, PropEdits, wfTypes,
    wfResourceStrings, wfFunc, wfDialogs, wfClasses, wfThreadU, wfSQLPropertyEditorU, wfParamsU,
    wfStringsPropertyEditor, TwfProgressU, wfreportviewer, wfBase, wfSQLQuery, db, fpspreadsheetctrls, fpspreadsheet,
    fpsTypes;

type

  TwfReportType = (rtSpreadSheet, rtCSV);

  TwfReportColumn = record
    Title: string;
    FieldName: string;
    Column: word;
    Width: word;
    Font: TsFont;
    NumFormat: TsNumberFormat;
    NumFormatStr: string;
    CellFormat: TsCellFormat;
    CellBorders: TsCellBorders;
  end;

  TwfReportColumns = array of TwfReportColumn;

  TwfReportItem = class;

  TwfReportExecuteEvent =  procedure(aReport: TwfReportItem; const Msg: Word; var Param: Variant) of object;
  TwfReportFinishEvent = procedure(aReport: TwfReportItem; const Msg: Word; const Param: Variant) of object;
  TwfReportProgress = procedure(aReport: TwfReportItem; const Msg: Word; const Value: Word) of object;
  TwfReportMessage = procedure(aReport: TwfReportItem; const Msg: Word; const Param: Variant) of object;

  TwfReportThread = class(TwfThread);

  { TwfReportItem }

  TwfReportItem = class(TCollectionItem)

  private
    procedure AsyncViewerShow(Data: PtrInt);
    function FieldInColumns(var aColumns: TwfReportColumns; aFieldName: string
      ): integer;
    function GetBase: TwfBase;
    procedure ReportExecuteDefaultCSV;
    procedure ReportExecuteDefaultSpreadSheet;
  protected
    function GetDisplayName: string; override;
  private
    fColumnsString: TStrings;
    fDefaultColWidth: word;
    fDescription: TStrings;
    fDescriptionShort: string;
    fExportFileName: string;
    fExportTemplateDir: string;
    fFirstCol: word;
    fFirstRow: word;
    fHeaderColor: DWord;
    fName: string;
    fonFinish: TwfReportFinishEvent;
    fonExecute: TwfReportExecuteEvent;
    fonForceFinish: TwfThreadNotify;
    fonMessage: TwfReportMessage;
    fonProgress: TwfReportProgress;
    fProgressBarStyle: TProgressBarStyle;
    fReportThread: TwfReportThread;
    fProgress: TwfProgress;
    fReportType: TwfReportType;
    fSQLRecord: TwfSQLRecord;
    fExportTemplateFile: string;
    fSQLQueryGroup: TStrings;
    fSQLQueryStep: word;
    fSQLQuery: TStrings;
    fTerminated: boolean;
    fViewer: TwfReportViewer;
    fUseProgressBar: boolean;
    fUseViewer: boolean;

    procedure AllTasksFinished(const Sender: TwfThread);
    function GetHeaderColor: TColor;
    function GetRootPath: string;
    function GetSilentMode: boolean;
    function GetSQLQuery: TStrings;
    function GetUsedColumnString: boolean;
    function GetUsedTemplate: boolean;
    procedure ReportExecute(Sender: TwfThread; const Msg: Word;
      var Param: Variant);
    procedure ReportFinish(Sender: TwfThread; const Msg: Word;
      const Param: Variant);
    procedure ReportProgress(Sender: TwfThread; const Msg: Word;
      const Value: Word);
    procedure ReportMessage(Sender: TwfThread; const Msg: Word;
      const Param: Variant);
    procedure ReportForceFinish(Sender: TObject);
    procedure SetColumnsString(aValue: TStrings);
    procedure SetDescription(aValue: TStrings);
    procedure SetHeaderColor(aValue: TColor);
    procedure SetSQLQuery(aValue: TStrings);
    procedure SetSQLQueryGroup(aValue: TStrings);
    procedure SetSQLRecord(aValue: TwfSQLRecord);
    procedure SetTerminated(AValue: boolean);
    function ThreadInit: boolean;
    procedure ThreadStop;

  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    // Initializing progress bar
    procedure ProgressInit(const aMax, aStep: integer);
    // Writes the value of a dataset field to a spreadsheet cell
    //aColumns must be downloaded from Salona function GetColumnsAsTemplate
    procedure WriteFieldToTemlate(var aWorkSheet: TsWorksheet; var wsRow: Int64;
          var wsCol: integer; aColumns: TwfReportColumns; aField: TField);
    //sets the width of all columns registered in aColumns
    procedure WriteFieldToViewerColumns(var aWorkSheet: TsWorksheet; var wsRow: Int64;
          var wsCol: integer; aColumns: TwfReportColumns; aField: TField);
    //sets the width of all columns registered in aColumns
    procedure WriteFieldToViewer(var aWorkSheet: TsWorksheet; var wsRow: Int64;
      var wsCol: integer; aField: TField);
    //Write column headers to a spreadsheet
    procedure SpreadSheetWriteHeaders(var aWorkSheet: TsWorksheet;
      var wsRow: int64; aColumns: TwfReportColumns; aDataSet: TDataSet);
    //sets the width of all columns registered in aColumns
    procedure SpreadSheetSetColWidth(aWorksheet: TsWorksheet;
      aColumns: TwfReportColumns);
    //returns TwfReportColumns from a string
    function GetColumnsAsString:TwfReportColumns;
    // returns TwfReportColumns from the template (the template must exist and have a path to It)
    function GetColumnsAsTemplate(aWorkSheet: TsWorksheet): TwfReportColumns;

    property Viewer: TwfReportViewer read fViewer write fViewer;
    property Report: TwfReportThread read fReportThread;

    property Base: TwfBase read GetBase;
    property SilentMode: boolean read GetSilentMode;
    property RootPath: string read GetRootPath;
    property UsedTemplate: boolean read GetUsedTemplate;
    property UsedColumnString: boolean read GetUsedColumnString;
    property Terminated: boolean read fTerminated write SetTerminated;
    property SQLRecord: TwfSQLRecord read fSQLRecord write SetSQLRecord;
  published

    property Name: string read fName write fName;
    property Description: TStrings read fDescription write SetDescription;
    property DescriptionShort: string read fDescriptionShort write fDescriptionShort;
    //SQL query to retrieve groups in the report
    //Use rtSpreadSheet report type
    property SQLQueryGroup: TStrings read fSQLQueryGroup write SetSQLQueryGroup;
    // SQL query to get report data
    property SQLQuery: TStrings read GetSQLQuery write SetSQLQuery;

    //Use only DefaultExport
    //SQL query step. Allows you to split the retrieval of rows from the database into portions.
    //0 - fetching in a single query
    property SQLQueryStep: word read fSQLQueryStep write fSQLQueryStep default 0;
    //Use rtSpreadSheet report type
    property FirstRow: word read fFirstRow write fFirstRow;
    //Use rtSpreadSheet report type
    property FirstCol: word read fFirstCol write fFirstCol;
    //Use rtSpreadSheet report type
    property DefaultColWidth: word read fDefaultColWidth write fDefaultColWidth default 10;
    //Example: ID{Code}<15>; NAME{Good Name}<80>
    property ColumnsString: TStrings read fColumnsString write SetColumnsString;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor default clWhite;

    //Use ProgressBar
    //Use Task.PostProgress() and aReport.ProgressInit() procedure to control the ProgressBar
    property UseProgressBar: boolean read fUseProgressBar write fUseProgressBar default false;
    property ProgressBarStyle: TProgressBarStyle read fProgressBarStyle write fProgressBarStyle default pbstNormal;

    //Viewer is available in the "Viewer" variable
    //only for rtSpreadSheet report type
    property UseViewer: boolean read fUseViewer write fUseViewer default false;

    property ReportType: TwfReportType read fReportType write fReportType default rtSpreadSheet;
    //Use rtSpreadSheet report type
    property ExportTemplateFile: string read fExportTemplateFile write fExportTemplateFile;
    //Use rtSpreadSheet report type
    property ExportTemplateDir: string read fExportTemplateDir write fExportTemplateDir;
    //Export FileName
    property ExportFileName: string read fExportFileName write fExportFileName;
    //onExecute
    //Use PostProgress or PostMessage to communicate with LCL
    //If not assigned, export will be performed by default
    //If the export is stopped (as wel onForceFinish), the will be set Task.Terminated:= true;
    property onExecute: TwfReportExecuteEvent read fonExecute write fonExecute;
    //Event indicating completion of report generation
    property onFinish: TwfReportFinishEvent read fonFinish write fonFinish;
    //Event marked the premature end of report
    property onForceFinish: TwfThreadNotify read fonForceFinish write fonForceFinish;
    //Event, PostProgress
    property onProgress: TwfReportProgress read fonProgress write fonProgress;
    //Event, PostMessage
    property onMessage: TwfReportMessage read fonMessage write fonMessage;
  end;

  { TwfReportItems }

  TwfReportItems = class(TOwnedCollection)
    public
      function ItemByName(aName: string):TwfReportItem;

    published
      property OwnerComponent: TPersistent read GetOwner;
  end;

  TwfReport = class(TComponent)
  private
    fBase: TwfBase;
    fItems: TwfReportItems;
    fRootPath: RawByteString;
    fSilentMode: boolean;
    function GetBase: TwfBase;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Running(aReportName: string): boolean;
    procedure Start(aReportName: string);
    procedure Stop(aReportName: string);
    function GetRootPath:string;

  published
    property Items: TwfReportItems read fItems write fItems;
    //wfBase to work with database
    property Base: TwfBase read GetBase write fBase;
    // Disables all questions to the user during export.
    //An exception will be thrown if there is a lack of data.
    property SilentMode: boolean read fSilentMode write fSilentMode default false;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfreport_icon.lrs}
  RegisterComponents('WF',[TwfReport]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfReportItem, 'ColumnsString', TwfSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfReportItem, 'SQLQuery', TwfSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfReportItem, 'SQLQueryGroup', TwfSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfReportItem, 'Description', TwfStringsPropertyEditor);
end;

{ TwfReportItems }

function TwfReportItems.ItemByName(aName: string): TwfReportItem;
var
  i: Integer;
begin
  Result:= nil;

  for i:=0 to Count-1 do
   if TwfReportItem(Items[i]).Name = aName then
     begin
       Result:= TwfReportItem(Items[i]);
       Break;
     end;

  if not Assigned(Result) then
    raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));
end;

{ TwfReport }

function TwfReport.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

constructor TwfReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems:= TwfReportItems.Create(self, TwfReportItem);
  fSilentMode:= false;
  fRootPath:= IncludeTrailingBackslash(ExtractFileDir(Application.ExeName));
end;

destructor TwfReport.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

function DialogsOpenSaveDialog(aCaption: string; aFilter: string; const aFilterIndex: word = 1): string;
var
  SaveDialog: TSaveDialog;
begin
  Result:= '';
  SaveDialog:= TSaveDialog.Create(nil);
  try
    SaveDialog.Options:= [ofOverwritePrompt];
    SaveDialog.Filter:=aFilter;//'CSV (*.csv)|*.csv';
    SaveDialog.FilterIndex:=aFilterIndex;
    SaveDialog.FileName:= aCaption;
    SaveDialog.Title:= aCaption;

    if SaveDialog.Execute then
      Result:= SaveDialog.FileName;
  finally
    FreeAndNil(SaveDialog);
  end;
end;

function TwfReport.Running(aReportName: string): boolean;
var
  aReport: TwfReportItem;
begin
  aReport:= nil;
  aReport:= fItems.ItemByName(aReportName);

  Result:= Assigned(aReport) and Assigned(aReport.Report);
end;

procedure TwfReport.Start(aReportName: string);
var
  aReport: TwfReportItem;
  aDialogFilter: String;
begin
  aReport:= fItems.ItemByName(aReportName);
  if Assigned(aReport) then
    begin
      if aReport.SQLQuery.Count = 0 then
        raise Exception.Create(rsReportNoSQLQuerySpecified);

      if (Length(aReport.ExportFileName)=0) and (not aReport.UseViewer and not SilentMode) then
        begin
          if SilentMode then
            raise Exception.Create(rsReportNoFileNameSpecified);

          case aReport.ReportType of
            rtSpreadSheet     : aDialogFilter:= 'OpenDocument (*.ods)|*.ods|Excel (*.xls)|*.xls|Excel (*.xlsx)|*.xlsx|Comma Text (*.csv)|*.csv';
            rtCSV             : aDialogFilter:= 'Comma Text (*.csv)|*.csv'
            else
              aDialogFilter:='';
          end;

          aReport.ExportFileName:= DialogsOpenSaveDialog(aReport.DescriptionShort, aDialogFilter);

          if Length(aReport.ExportFileName)=0 then
            exit;
        end;

      if aReport.ThreadInit then
        begin
          with aReport do begin
            if UseProgressBar then
              begin
                fProgress:= TwfProgress.Create(nil);
                  with fProgress do begin
                    onStopForce:=@ReportForceFinish;
                    Bar.Style:= ProgressBarStyle;
                    ShowInTaskBar:= stAlways;
                    if Length(DescriptionShort)>0 then
                      Caption:= DescriptionShort;
                    Show;
                  end;
              end;

            if fUseViewer then
              fViewer:= TwfReportViewer.Create(nil);

            fTerminated:= false;
            Report.Start;
          end;
        end;
    end;
end;

procedure TwfReport.Stop(aReportName: string);
var
  aReport: TwfReportItem;
begin
  aReport:= fItems.ItemByName(aReportName);
  if Assigned(aReport) and Assigned(aReport.Report) then
     aReport.ThreadStop;
end;

function TwfReport.GetRootPath: string;
begin
  Result:= fRootPath;
end;


{ TwfReportItem }

procedure TwfReportItem.WriteFieldToTemlate(var aWorkSheet: TsWorksheet;
  var wsRow: Int64; var wsCol: integer; aColumns: TwfReportColumns;
  aField: TField);
var
  aColumnIndex: integer;
begin
  aColumnIndex:= FieldInColumns(aColumns, aField.FieldName);

  if aColumnIndex>-1 then
    begin
      with aColumns[aColumnIndex] do begin
        WriteValue(aWorkSheet, wsRow, Column,
            aField, Font,
            CellFormat,
            NumFormat,
            NumFormatStr,
            CellBorders);
      end;

      Inc(wsCol);
    end;
end;

procedure TwfReportItem.WriteFieldToViewerColumns(var aWorkSheet: TsWorksheet;
  var wsRow: Int64; var wsCol: integer; aColumns: TwfReportColumns;
  aField: TField);
var
  aColumnIndex: Integer;
begin
  aColumnIndex:= FieldInColumns(aColumns,aField.FieldName);

  if aColumnIndex>-1 then
    WriteFieldToViewer(aWorkSheet, wsRow, wsCol, aField);
end;

procedure TwfReportItem.WriteFieldToViewer(var aWorkSheet: TsWorksheet;
  var wsRow: Int64; var wsCol: integer; aField: TField);
begin
  WriteValue(aWorkSheet, wsRow, wsCol, aField);
  Inc(wsCol);
end;

procedure TwfReportItem.AsyncViewerShow(Data: PtrInt);
begin
  if (Data = 1) and not SilentMode then
    begin
      //fViewer.Position:= po;
      fViewer.Show;
      fViewer.SetFocus;
    end
  else
    FreeAndNil(fViewer);
end;

function TwfReportItem.GetBase: TwfBase;
begin
  Result:=TwfReport(TwfReportItems(GetOwner).Owner).Base;
end;

function TwfReportItem.GetColumnsAsString: TwfReportColumns;
const
  CharCaptionStart     = '{';
  CharCaptionEnd       = '}';
  CharSizeStart        = '<';
  CharSizeEnd          = '>';
  //CharColumn           = '|';
  CharDelimiter        = ';';
var
  i: Integer;
  aFieldName, aCaption, aWidth, aText: String;
  aStringList: TStringList;
  CurrentPos, NextPos: PtrInt;
  aInt: Longint;
begin
  // ID{ID}<70>|1; NAME{NAME}<840>|1

  if fColumnsString.Count = 0 then exit; //=>

  aFieldName:='';
  aCaption:= '';
  aWidth:= '';
  aText:= '';
  aInt:= 0;

  aStringList:= TStringList.Create;

  try
    aStringList.Delimiter:= CharDelimiter;
    aStringList.StrictDelimiter:= true;
    aStringList.DelimitedText:= fColumnsString.Text;

    SetLength(Result, aStringList.Count);

    for i:=0 to aStringList.Count-1 do
      begin
        aText:= aStringList.Strings[i];

        CurrentPos:= UTF8Pos(CharCaptionStart, aText, 1);
        if CurrentPos>0 then
          aFieldName:= Trim(UTF8Copy(aText,1, CurrentPos-1));

          NextPos:= UTF8Pos(CharCaptionEnd, aText, CurrentPos);
          if NextPos>0 then
            aCaption:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos-1));

        CurrentPos:= UTF8Pos(CharSizeStart, aText, 1);
        NextPos:= UTF8Pos(CharSizeEnd, aText, CurrentPos);
        if NextPos>0 then
          aWidth:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos-1));

        //CurrentPos:= UTF8Pos(CharColumn, aText, NextPos);
        //NextPos:= Length(aText);
        //
        //aColumn:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos));

        Result[i].FieldName:= aFieldName;
        Result[i].Title:= aCaption;
        if TryStrToInt(aWidth, aInt) then
          Result[i].Width:=aInt
        else
          Result[i].Width:= fDefaultColWidth;
        //TryStrToInt(aColumn, aInt);
        //Result[i].Column:=aInt;
      end;
  finally
    FreeAndNil(aStringList);
  end;

end;

function TwfReportItem.GetColumnsAsTemplate(aWorkSheet: TsWorksheet): TwfReportColumns;
var
  i: Integer;
  aCurrCell: PCell;
begin
  SetLength(Result,aWorkSheet.Cols.Count);

  for i:=0 to aWorkSheet.GetLastColNumber-1 do
   begin
     aCurrCell:= aWorkSheet.Cells.FindCell(fFirstRow,fFirstCol+i);
     with Result[i] do begin
       FieldName:=GetOnlyCorrectChars(aCurrCell^.UTF8StringValue);
       Column:=fFirstCol+i;
       Title:='';
       Width:=0;
       Font:= aWorkSheet.ReadCellFont(aCurrCell);
       CellFormat:= aWorkSheet.ReadCellFormat(aCurrCell);
       CellBorders:= aWorkSheet.ReadCellBorders(aCurrCell);

       aWorkSheet.ReadNumFormat(aCurrCell, NumFormat, NumFormatStr);
     end;
   end;
end;

procedure TwfReportItem.ReportMessage(Sender: TwfThread; const Msg: Word; const Param: Variant);
begin
  if Assigned(fonMessage) then fonMessage(self, Msg, Param);
end;

function TwfReportItem.GetDisplayName: string;
begin
  if Length(fName)>0 then
    Result:= fName
  else
    Result:=inherited GetDisplayName;
end;

procedure TwfReportItem.ProgressInit(const aMax, aStep: integer
  );
begin
  if Assigned(fProgress) then
    fProgress.InitBar(aMax, aStep);
end;

function TwfReportItem.ThreadInit: boolean;
begin
  Result:= false;
  if Assigned(fReportThread) then exit;

  fReportThread:= TwfReportThread.Create(true);
  with fReportThread do begin
    onExecute:=@ReportExecute;
    onFinish:=@ReportFinish;
    onProgress:=@ReportProgress;
    onMessage:=@ReportMessage;
  end;
  Result:= true;
end;

function TwfReportItem.FieldInColumns(var aColumns:TwfReportColumns; aFieldName: string): integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to High(aColumns) do
    if UTF8UpperCase(aColumns[i].FieldName) = UTF8UpperCase(aFieldName) then
      begin
        Result:= i;
        Break;
      end;
end;

procedure TwfReportItem.ReportExecuteDefaultCSV;
var
  aDataSet: TwfSQLQuery;
  aColumns: TwfReportColumns;
  i: Integer;
  aColumnIndex, aStep, iDS, iRows: integer;
  aCSV: TFileStream;
  aCSVText: String;
  aSQL, aSQLTemp: string;
  aPosSelect: PtrInt;
  aRowsCount: Int64;
  aStepIt: Boolean;
  aParams: TwfParams;
const
  uCommaChar     = ';';

  procedure AddComma;
  begin
    if Length(aCSVText)>0 then
      aCSVText := aCSVText+uCommaChar;
  end;

  procedure WriteHeaders;
  var
    iHead: Integer;
  begin
    aCSVText:= '';

    for iHead:=0 to aDataSet.FieldCount-1 do
     begin


       if Length(aColumns)>0 then
         begin
           aColumnIndex:= FieldInColumns(aColumns,aDataSet.Fields[iHead].FieldName);

           if aColumnIndex>-1 then
             begin
               AddComma;
               aCSVText:= aCSVText+'"'+aColumns[aColumnIndex].Title+'"';
             end;
         end
       else
       begin
         AddComma;
         aCSVText:= aCSVText+aDataSet.Fields[iHead].FieldName;
       end;

     end;
    WriteUTF8String(aCSV,aCSVText);
  end;

begin

  aDataSet:= nil;
  aStep:= fSQLQueryStep;
  aStepIt:= (aStep>0);
  iRows:= 0;
  aSQL:= fSQLRecord.aText;
  aParams:= fSQLRecord.aParams;

  aRowsCount:= Base.GetRowsCount(fSQLRecord);
  ProgressInit(aRowsCount,1);

  aPosSelect := UTF8Pos('SELECT', aSQL)+UTF8Length('SELECT');

  if aStepIt then
    // modify the query for sampling in batches
    UTF8Insert(' first %d skip %d ', aSQL, aPosSelect);

  try

    if FileExists(fExportFileName) then
          DeleteFile(fExportFileName);

    aCSV  := TFileStream.Create(fExportFileName,fmCreate);
    aColumns:= GetColumnsAsString;

  while iRows<aRowsCount do begin

    if Terminated then break;

    aSQLTemp:= aSQL;

    if aStepIt then
      aSQLTemp:= Format(aSQLTemp,[aStep,iRows]);

      aDataSet:= Base.OpenSQL(aSQLTemp, aParams);

    aDataSet.First;

      if iRows = 0 then
        WriteHeaders;

      iDS:= 0;
      while (not aDataSet.EOF) do
      begin
        if Terminated then break;

        aCSVText:= '';
        for i:=0 to aDataSet.FieldCount-1 do
         begin
           if Length(aColumns)>0 then
             begin
               aColumnIndex:= FieldInColumns(aColumns,aDataSet.Fields[i].FieldName);

               if aColumnIndex>-1 then
                 begin
                   AddComma;
                   aCSVText:= aCSVText+Base.DataToStr(aDataSet.Fields[i],true,true);
                 end;
             end
           else
           begin
             AddComma;
             aCSVText:= aCSVText+Base.DataToStr(aDataSet.Fields[i],true,true);
           end;
         end;

        WriteUTF8String(aCSV,aCSVText);
        aDataSet.Next;

        Inc(iDS);
        ReportProgress(self.fReportThread, 0, 0);
      end;

      aDataSet.Close;

      if aStepIt then
        iRows:= iRows+aStep
      else
        iRows:= aRowsCount;
  end;

  finally
    FreeAndNil(aDataSet);
    FreeAndNil(aCSV);
  end;
end;

procedure TwfReportItem.SpreadSheetWriteHeaders(var aWorkSheet: TsWorksheet;
  var wsRow: int64; aColumns: TwfReportColumns; aDataSet: TDataSet);
var
  i, wsCol, aColumnIndex: Integer;
begin
  wsCol:= fFirstCol;
  for i:=0 to aDataSet.FieldCount-1 do
   begin
     if Length(aColumns)>0 then
       begin
         aColumnIndex:= FieldInColumns(aColumns,aDataSet.Fields[i].FieldName);

         if aColumnIndex>-1 then
           begin
             WriteValue(aWorkSheet,wsRow, wsCol, aColumns[aColumnIndex].Title,[fssBold],HeaderColor);
             Inc(wsCol);
           end;
       end
     else
     begin
       WriteValue(aWorkSheet,wsRow, wsCol, aDataSet.Fields[i].FieldName,[fssBold],HeaderColor);
       Inc(wsCol);
     end;
   end;
  Inc(wsRow);
end;

procedure TwfReportItem.ReportExecuteDefaultSpreadSheet;
var
  aDataSet: TwfSQLQuery;
  aColumns: TwfReportColumns;
  i: Integer;
  aStep, iRows, wsCol: integer;
  aSQL, aFileName, aExportTemplateFile, aSQLTemp: String;
  aPosSelect: PtrInt;
  aRowsCount: Int64;
  aStepIt: Boolean;
  aWorkbookSource: TsWorkbookSource;
  aWorkSheet: TsWorksheet;
  wsRow: Int64;
  aParams: TwfParams;

begin
  { TODO : добавить экспорт с группировкой }
  aDataSet:= nil;
  aFileName:= fExportFileName;

  if Assigned(fViewer) then
    aWorkbookSource:= fViewer.Source
  else
    aWorkbookSource:= TsWorkbookSource.Create(nil);

  aWorkbookSource.Options:= aWorkbookSource.Options+[boFileStream];

  if UsedTemplate then
    begin
      if not IsEmpty(ExportTemplateDir) then
        aExportTemplateFile:= RootPath+fExportTemplateDir+DirectorySeparator+fExportTemplateFile
      else
        aExportTemplateFile:= RootPath+fExportTemplateFile;

      aWorkbookSource.LoadFromSpreadsheetFile(aExportTemplateFile,GetSpreadSheetFormat(fExportTemplateFile));
    end;

  aWorkSheet:= aWorkbookSource.Workbook.ActiveWorksheet;

  aStep:= fSQLQueryStep;
  aStepIt:= (aStep>0);
  iRows:= 0;
  wsRow:= fFirstRow;

  aSQL:= fSQLRecord.aText;
  aParams:= fSQLRecord.aParams;

  aRowsCount:= Base.GetRowsCount(fSQLRecord);

  ProgressInit(aRowsCount,1);

  aPosSelect := UTF8Pos('SELECT', aSQL)+UTF8Length('SELECT');

  if aStepIt then
    // modify the query for sampling in batches
    UTF8Insert(' first %d skip %d ', aSQL, aPosSelect);

 try
    if FileExists(aFileName) then
          DeleteFile(aFileName);

    if UsedColumnString then
      aColumns:= GetColumnsAsString
    else
      if UsedTemplate then
        aColumns:= GetColumnsAsTemplate(aWorkSheet);

    aWorkSheet.WriteDefaultColWidth(fDefaultColWidth, suChars);


  while iRows<aRowsCount do begin
    if Terminated then break;

    aSQLTemp:= aSQL;

    if aStepIt then
      aSQLTemp:= Format(aSQLTemp,[aStep,iRows]);

      aDataSet:= Base.OpenSQL(aSQLTemp, aParams);

    aDataSet.First;

      if (iRows = 0) and IsEmpty(fExportTemplateFile) then
        begin
          SpreadSheetWriteHeaders(aWorkSheet, wsRow, aColumns, aDataSet);
          SpreadSheetSetColWidth(aWorkSheet, aColumns);
        end;

     //fDefaultColWidth
      while (not aDataSet.EOF)  do
      begin

        if Terminated then break;

        wsCol:= fFirstCol;
        for i:=0 to aDataSet.FieldCount-1 do
         begin
           if UsedColumnString then
               WriteFieldToViewerColumns(aWorkSheet, wsRow, wsCol, aColumns, aDataSet.Fields[i])
           else
           begin
             if UsedTemplate then
               WriteFieldToTemlate(aWorkSheet, wsRow, wsCol, aColumns, aDataSet.Fields[i])
             else
               WriteFieldToViewer(aWorkSheet, wsRow, wsCol, aDataSet.Fields[i]);
            end;
          end;

        aDataSet.Next;

        Inc(wsRow);
        ReportProgress(self.fReportThread, 0, 0);
      end;

      aDataSet.Close;

      if aStepIt then
        iRows:= iRows+aStep
      else
        iRows:= aRowsCount;
  end;

  if not Assigned(fViewer) then
    aWorkbookSource.SaveToSpreadsheetFile(aFileName)


  finally
    FreeAndNil(aDataSet);
    if not Assigned(fViewer) then
      FreeAndNil(aWorkbookSource);
  end;
end;

procedure TwfReportItem.ReportExecute(Sender: TwfThread; const Msg: Word; var Param: Variant);
begin
  if Assigned(fonExecute) then fonExecute(self, Msg, Param)
  else
    case fReportType of
      rtCSV          : ReportExecuteDefaultCSV;
      rtSpreadSheet  : ReportExecuteDefaultSpreadSheet;
    end;
end;

procedure TwfReportItem.AllTasksFinished(const Sender: TwfThread);
begin
   Terminated:= true;
  if Assigned(onForceFinish) then onForceFinish(Sender);

  //ReportFinish(nil, 0, false);
end;

function TwfReportItem.GetHeaderColor: TColor;
begin
  Result:= TColor(fHeaderColor);
end;

function TwfReportItem.GetRootPath: string;
begin
  Result:=TwfReport(TwfReportItems(GetOwner).Owner).GetRootPath;
end;

function TwfReportItem.GetSilentMode: boolean;
begin
  Result:=TwfReport(TwfReportItems(GetOwner).Owner).SilentMode;
end;

function TwfReportItem.GetSQLQuery: TStrings;
begin
  fSQLQuery.Text:= fSQLRecord.aText;
  //Base.WriteParamsToLog(fSQLRecord.aParams);
  Result:= fSQLQuery;
end;

function TwfReportItem.GetUsedColumnString: boolean;
begin
  Result:= not IsEmpty(fColumnsString);
end;

function TwfReportItem.GetUsedTemplate: boolean;
begin
  Result:= not IsEmpty(fExportTemplateFile);
end;

procedure TwfReportItem.ReportFinish(Sender: TwfThread; const Msg: Word; const Param: Variant);
var
  ResultIndex: Integer;
begin
  if not Terminated then
    Terminated:= true;

  fReportThread:= nil;

  if Assigned(fonFinish) then fonFinish(self, Msg, Param);

  if Assigned(fProgress) then
      fProgress.ForceClose;

  if Assigned(fViewer) and (TVarData(Param).VType = varBoolean) and (fReportType = rtSpreadSheet) then
    begin
      if Param then
        ResultIndex:= 1
      else
        ResultIndex:= 0;

      Application.QueueAsyncCall(@AsyncViewerShow,ResultIndex);
    end;
end;

procedure TwfReportItem.ReportProgress(Sender: TwfThread; const Msg: Word; const Value: Word);
begin
  if Assigned(fProgress) then
    fProgress.SetBar(Value);
  if Assigned(onProgress) then onProgress(self, Msg, Value);
end;

procedure TwfReportItem.ReportForceFinish(Sender: TObject);
begin

  AllTasksFinished(fReportThread);
end;

procedure TwfReportItem.SetColumnsString(aValue: TStrings);
begin
  fColumnsString.Assign(aValue);
end;

procedure TwfReportItem.SetDescription(aValue: TStrings);
begin
  fDescription.Assign(aValue);
end;

procedure TwfReportItem.SetHeaderColor(aValue: TColor);
begin
  fHeaderColor:= ColorToRGB(aValue);
end;

procedure TwfReportItem.SetSQLQuery(aValue: TStrings);
begin
  fSQLRecord.aText:= aValue.Text;
end;

procedure TwfReportItem.SetSQLQueryGroup(aValue: TStrings);
begin
  fSQLQueryGroup.Assign(aValue);
end;

procedure TwfReportItem.SetSQLRecord(aValue: TwfSQLRecord);
var
  aParams: TwfParams;
begin
  fSQLRecord:= aValue;

  aParams:= fSQLRecord.aParams;
end;

procedure TwfReportItem.SetTerminated(AValue: boolean);
begin
  if fTerminated=AValue then Exit;

  if not fTerminated and Assigned(fReportThread) then
    fReportThread.Terminate;

  fTerminated:=AValue;
end;

procedure TwfReportItem.ThreadStop;
begin
   ReportForceFinish(self);
end;

constructor TwfReportItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fReportThread:= nil;
  fTerminated:= true;
                       { TODO : Группировка в отчете }
  fSQLQueryGroup:= TStringList.Create;
  fSQLQuery:= TStringList.Create;
  fColumnsString:= TStringList.Create;
  fDescription:= TStringList.Create;
  fHeaderColor:= clWhite;
  fReportType:= rtSpreadSheet;
  fSQLQueryStep:= 0;
  fDefaultColWidth:= 10;
  fProgress:= nil;
  fViewer:= nil;
end;

destructor TwfReportItem.Destroy;
begin
  FreeAndNil(fSQLQueryGroup);
  FreeAndNil(fSQLQuery);
  FreeAndNil(fColumnsString);
  FreeAndNil(fDescription);
  inherited Destroy;
end;

procedure TwfReportItem.SpreadSheetSetColWidth(aWorksheet: TsWorksheet; aColumns: TwfReportColumns);
var
  i: Integer;
begin
  for i:=0 to High(aColumns) do
    aWorksheet.WriteColWidth(i, aColumns[i].Width, suChars, cwtCustom);
end;

end.
