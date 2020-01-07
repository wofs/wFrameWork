unit wfSimpleDataEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, DBCtrls, ExtCtrls, Buttons, StdCtrls, wfSQLQuery,
  wfDBGrid, wfSQLTransaction, wfBase, db, sqldb;

type

  { TFmSimpleDataEditor }

  TFmSimpleDataEditor = class(TForm)
    BitBtn1: TBitBtn;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    pBottom: TPanel;
    Panel2: TPanel;
    SQLQuery: TwfSQLQuery;
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
  private
    function GetConnected: boolean;
    function GetConnection: TDatabase;
    function GetSQL: string;
    procedure SetConnected(aValue: boolean);
    procedure SetConnection(aValue: TDatabase);
    procedure SetSQL(aValue: string);

  public
    property Connection: TDatabase read GetConnection write SetConnection;
    property SQL: string read GetSQL write SetSQL;
    property Connected: boolean read GetConnected write SetConnected;

  end;

var
  FmSimpleDataEditor: TFmSimpleDataEditor;

implementation

{$R *.lfm}

{ TFmSimpleDataEditor }

procedure TFmSimpleDataEditor.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
begin
  case Button of
    nbPost: SQLQuery.ApplyUpdates;
    nbCancel: SQLQuery.CancelUpdates;
  end;
end;

function TFmSimpleDataEditor.GetConnected: boolean;
begin
  Result:= SQLQuery.Active;
end;

function TFmSimpleDataEditor.GetConnection: TDatabase;
begin
  Result:= SQLQuery.DataBase;
end;

function TFmSimpleDataEditor.GetSQL: string;
begin
  Result:= SQLQuery.SQL.Text;
end;

procedure TFmSimpleDataEditor.SetConnected(aValue: boolean);
begin
  SQLQuery.Active:= aValue;
end;

procedure TFmSimpleDataEditor.SetConnection(aValue: TDatabase);
begin
  SQLQuery.DataBase:= aValue;
end;

procedure TFmSimpleDataEditor.SetSQL(aValue: string);
begin
  SQLQuery.SQL.Text:= aValue;
end;

end.

