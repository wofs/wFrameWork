{
This file is part of wfFrameWork.

 -= ODBCConnection =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfODBCConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfTypes,
  wfResourceStrings, odbcconn, SQLDB;

type

  { TwfODBCConnection }

  TwfODBCConnection = class(TODBCConnection)
  private
    fonLog: TTextEvent;
    procedure AsyncInit(Data: PtrInt);

  protected
    procedure wfLog(const aValue: string);

  public
    constructor Create(AOwner: TComponent); override;

    function Connect(aHost, aUserName, aPassword, aDataBase: string;
      const aReadOnly:boolean = true; const aDriver: string = 'ODBC Driver 11 for SQL Server'): boolean;

    function SQLFieldIsExists(const uTable, uFieldName: string): string; virtual; abstract;
    function SQLProcIsExists(const uProcName: string): string; virtual; abstract;
    function SQLTriggerIsExists(const uTriggerName: string): string; virtual; abstract;
    function SQLTableIsExists(const uTable: string): string; virtual; abstract;
    function SQLGetTables: string; virtual; abstract;

    procedure Disconnect;

  published
    property onLog:TTextEvent read fonLog write fonLog;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfodbcconnection_icon.lrs}
  RegisterComponents('WF',[TwfODBCConnection]);
end;

procedure TwfODBCConnection.AsyncInit(Data: PtrInt);
begin
  if not(csDesigning in ComponentState) then Connected:= false;
end;

{@@ ----------------------------------------------------------------------------
  // Connect to DataBase
  @param    aHost          Host
  @param    aBaseName      BaseName
  @param    aUserName      UserName
  @param    aPassword      Password
  @param    aDataBase      Password
  @param    aReadOnly      Password
  @param    aDriver        Password
  @result   boolean        status of connect
-------------------------------------------------------------------------------}
function TwfODBCConnection.Connect(aHost, aUserName, aPassword,
  aDataBase: string; const aReadOnly: boolean; const aDriver: string): boolean;
begin
  Result:= false;

  if Connected then Connected:= false;
  try
    Params.Clear;

    Driver:= aDriver;
    UserName:= aUserName;
    Password:= aPassword;
    if aReadOnly then
      Params.Add('ApplicationIntent=READONLY');

    Params.Add('server='+aHost+'');
    Params.Add('database='+aDataBase+'');

    Connected:= true;
    Result:= true;

    wfLog(rsMessageConectedSucefull);
  except
    raise
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Disconnect from DataBase
-------------------------------------------------------------------------------}
procedure TwfODBCConnection.Disconnect;
begin
  if not Assigned(self) then exit;
   try
     Connected:= false;
     wfLog(rsMessageDisconnectSucefull);
   except
     raise;
   end;
end;

{@@ ----------------------------------------------------------------------------
  // Log write
  @param    aValue     Text for write to Log
-------------------------------------------------------------------------------}
procedure TwfODBCConnection.wfLog(const aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

constructor TwfODBCConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.QueueAsyncCall(@AsyncInit,0);
end;

end.
