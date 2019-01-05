{
This file is part of wfFrameWork.

 -= Forms/ReportViewer =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfReportViewer;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, LCLIntf, wfDialogs, wfFunc, fpspreadsheetgrid, fpspreadsheetctrls,
  fpsTypes, fpsallformats;

type

  { TReportViewer }

  { TwfReportViewer }

  TwfReportViewer = class(TForm)
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    pBottom: TPanel;
    WorkSource: TsWorkbookSource;
    WorkTabControl: TsWorkbookTabControl;
    WorkGrid: TsWorksheetGrid;
    procedure btnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    constructor Create(TheOwner: TComponent); override;
    property Grid: TsWorksheetGrid read WorkGrid write WorkGrid;
    property Source: TsWorkbookSource read WorkSource write WorkSource;
  end;

var
  ReportViewer: TwfReportViewer;

implementation

{$R *.lfm}

{ TReportViewer }

procedure TwfReportViewer.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TwfReportViewer.btnSaveClick(Sender: TObject);
var
  aDialogFilter, aFileName: String;
begin
  aFileName:= '';
  aDialogFilter:= 'OpenDocument (*.ods)|*.ods|Excel (*.xls)|*.xls|Excel (*.xlsx)|*.xlsx|Comma Text (*.csv)|*.csv';
  aFileName:= DialogsOpenSaveDialog(Caption, aDialogFilter);

  if not IsEmpty(aFileName) then
     WorkSource.SaveToSpreadsheetFile(aFileName);

  if MessageDlg('Открыть полученный файл в программе просмотра?',
       mtConfirmation, mbOKCancel, 0) = mrOK then
       OpenDocument(aFileName);

  Close;
end;

procedure TwfReportViewer.FormShow(Sender: TObject);
begin
  WorkGrid.WorkbookSource:= WorkSource;
end;

constructor TwfReportViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  WorkGrid.WorkbookSource:= nil;
end;

end.

