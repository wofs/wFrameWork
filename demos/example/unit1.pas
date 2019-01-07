unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, StdCtrls, ExtCtrls, wfIBConnection, wfBase, wfDBGrid,
  wfEntity, wfTreeView, wfReport, wfSQLQuery, wfComboBox, wfstatusprogressbar,
  wfVersions, wfSQLTransaction, wfFunc, wfClasses, sqldb, db, wcthread, Classes,
  sysutils, Grids, DBGrids, ComCtrls, Menus, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    mExport: TButton;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    wfBase1: TwfBase;
    wfDBGrid1: TwfDBGrid;
    DEPARTMENTS: TwfEntity;
    SETTINGS: TwfEntity;
    wfReport1: TwfReport;
    wfSQLTransaction1: TwfSQLTransaction;
    wfTreeView1: TwfTreeView;
    WORKERS: TwfEntity;
    wfIBConnection1: TwfIBConnection;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure mExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure Log(aText:string);
    procedure onLog(Sender: TObject; const aValue: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  wfTreeView1.Fill(nil);
  //wfComboBox1.Fill;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //wfBase1.ExecSQLScript(SETTINGS.Script[esctCreate]);

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  aSQL: String;
  i: Integer;
  aDataSet: TwfSQLQuery;
begin
  aSQL:= 'SELECT * FROM WORKERS';

  wfDBGrid1.wSQLText.Text:= aSQL;
  wfDBGrid1.Fill;

  aDataSet:= wfBase1.OpenSQL(aSQL);

  try
    for i:= 0 to aDataSet.RecordCount-1 do
      begin
        Log(aDataSet.FirldAsVariant('NAME'));
        aDataSet.Next;
      end;

   finally
    FreeAndNil(aDataSet);
  end;

end;

procedure TForm1.mExportClick(Sender: TObject);
begin
  //if not wfReport1.Running('TEST') then
    wfReport1.Start('TEST')
    //wfReport1.Start('WORKERS')
  //else
  //  wfReport1.Stop('TEST');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  wfBase1.onLog:=@onLog;
end;

procedure TForm1.Log(aText: string);
begin
  Memo1.Lines.Add(aText);
end;

procedure TForm1.onLog(Sender: TObject; const aValue: string);
begin
  Log(aValue);
end;

end.

