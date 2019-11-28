unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, LazStringUtils,
  LazUTF8, SynEdit, SynHighlighterPas, SynHighlighterIni, SynHighlighterAny, SynHighlighterSQL, ShortPathEdit,
  uPSComponent, uPSComponent_DB, wfEntity, wfReport, wfValueListEditor, wfComboBox, wfSQLScript, wfSQLQuery, wfDBGrid,
  wfPQConnection, wfBase, wfImport, wfSettings, wfVersions, wfTreeView, wfSQLTransaction, wfLog, wfStatusProgressBar,
  wfFunc, wfClasses, wfTypes, wfImportReaderXLSU, fpspreadsheetctrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStartImport: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    rgFormats: TRadioGroup;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    wfImport1: TwfImport;
    wfLog1: TwfLog;
    procedure btnStartImportClick(Sender: TObject);
    procedure wfImport1Log(Sender: TObject; const aValue: string);
    procedure wfImport1WriteContentRow(Sender: TObject; aGroups: TwfGroups; aContentRow: TwfContentRow);
  private
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartImportClick(Sender: TObject);
var
  aFormatName: string;
begin
case rgFormats.ItemIndex of
  0: aFormatName:= 'GroupInRows';
  1: aFormatName:= 'GroupInCols';
  2: aFormatName:= 'NoGrouping';
end;
  wfImport1.Start('ImportTest', aFormatName);
end;

procedure TForm1.wfImport1Log(Sender: TObject; const aValue: string);
begin
  wfLog1.SendDebug(aValue);
  StatusBar1.Panels[0].Text:= aValue;
end;

procedure TForm1.wfImport1WriteContentRow(Sender: TObject; aGroups: TwfGroups; aContentRow: TwfContentRow);
var
  i, k: Integer;
begin
// Here you need to process the data yourself
  SynEdit1.BeginUpdate(false);

  if aGroups.Size>0 then
    SynEdit1.Append(Format('Group: %s',[TwfImportReaderXLS(Sender).GroupBreadCrumbs[aContentRow.GroupIndex]]));

  for k:= 0 to High(aContentRow.Row) do
    SynEdit1.Append(Format('GroupIndex: %d: | %s | %s | %s', [aContentRow.GroupIndex, aContentRow.Row[k].Name, aContentRow.Row[k].Field, aContentRow.Row[k].Value]));

  SynEdit1.Append(wfEmptyStr);
  SynEdit1.EndUpdate;


end;

end.

