{
This file is part of wfFrameWork.

 -= PQConnection =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfPQConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfTypes,
  wfResourceStrings, wfFunc, pqconnection;

type

  { TwfPQConnection }

  TwfPQConnection = class(TPQConnection)
  private
    fonLog: TTextEvent;
    procedure AsyncInit(Data: PtrInt);

  protected
    procedure wfLog(const aValue: string);

  public
    constructor Create(AOwner: TComponent); override;

    function Connect(const uHost, uPort, uBaseName, uUserName, uPassword: string
      ): boolean;

    function GetDomainOrProcedureString:string;

    function SQLFieldIsExists(const uTable, uFieldName: string): string; virtual;
    function SQLProcIsExists(const uProcName: string): string; virtual;
    function SQLTriggerIsExists(const uTriggerName: string): string; virtual;
    function SQLTableIsExists(const uTable: string): string; virtual;
    function SQLGetTables: string; virtual;

    procedure Disconnect;

  published
    property onLog:TTextEvent read fonLog write fonLog;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfpqconnection_icon.lrs}
  RegisterComponents('WF',[TwfPQConnection]);
end;

{ TwfPQConnection }

procedure TwfPQConnection.AsyncInit(Data: PtrInt);
begin
  if not(csDesigning in ComponentState) then Connected:= false;
end;

{@@ ----------------------------------------------------------------------------
  // Log write
  @param    aValue     Text for write to Log
-------------------------------------------------------------------------------}
procedure TwfPQConnection.wfLog(const aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

constructor TwfPQConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.QueueAsyncCall(@AsyncInit,0);
end;

{@@ ----------------------------------------------------------------------------
  // Connect to DataBase
  @param    aHost          Host
  @param    aPort          Port
  @param    aBaseName      BaseName
  @param    aUserName      UserName
  @param    aPassword      Password
  @result   boolean        status of connect
-------------------------------------------------------------------------------}
function TwfPQConnection.Connect(const uHost, uPort, uBaseName, uUserName,
  uPassword: string): boolean;
begin
  try
    Result:= false;

    Connected   := False;
    LoginPrompt := False;
    Params.Clear;

    DatabaseName := uBaseName;
    HostName:= uHost;
    UserName:= uUserName;
    Password:= uPassword;
    Params.Clear;

    if not IsEmpty(uPort) then
       Params.Add('port='+uPort);

    wfLog('Host: '+uHost);
    wfLog('Port: '+uPort);
    wfLog('BaseName: '+uBaseName);
    wfLog('UserName: '+uUserName);

    Connected := True;
    Result:= true;

    wfLog(rsMessageConectedSucefull);
  except
    raise;
  end;
end;

function TwfPQConnection.GetDomainOrProcedureString: string;
begin
  {$IFDEF USEGUID}
    Result:= 'CREATE EXTENSION "uuid-ossp";';
  {$ELSE}
    Result:='';
  {$ENDIF}
end;

function TwfPQConnection.SQLFieldIsExists(const uTable, uFieldName: string
  ): string;
const
  uSQL = 'SELECT column_name '
          +' FROM information_schema.columns '
          +' WHERE LOWER(table_name)=LOWER(%s) and LOWER(column_name)=LOWER(%s)';
begin
  Result:= Format(uSQL, [QuotedStr(uTable), QuotedStr(uFieldName)]);
end;

function TwfPQConnection.SQLProcIsExists(const uProcName: string): string;
const
  uSQL = 'SELECT * '
          +' FROM pg_proc '
          +' WHERE LOWER(proname) = LOWER(%s)';
begin
  Result:= Format(uSQL, [QuotedStr(uProcName)]);
end;

function TwfPQConnection.SQLTriggerIsExists(const uTriggerName: string): string;
const
  uSQL = 'SELECT tgname '
          +' FROM pg_trigger '
          +' WHERE LOWER(tgname) = LOWER(%s)';
begin
  Result:= Format(uSQL, [QuotedStr(uTriggerName)]);
end;

function TwfPQConnection.SQLTableIsExists(const uTable: string): string;
const
  uSQL = 'SELECT * '
          +' FROM INFORMATION_SCHEMA.TABLES '
          +' WHERE LOWER(TABLE_NAME) = LOWER(%s)';
begin
  Result:= Format(uSQL, [QuotedStr(uTable)]);
end;

function TwfPQConnection.SQLGetTables: string;
const
  uSQL = 'SELECT table_name '
          +' FROM INFORMATION_SCHEMA.TABLES '
          +' WHERE LOWER(table_schem)a NOT IN (LOWER(''information_schema''),LOWER(''pg_catalog''))';
begin
  Result:= uSQL;
end;

{@@ ----------------------------------------------------------------------------
  // Disconnect from DataBase
-------------------------------------------------------------------------------}
procedure TwfPQConnection.Disconnect;
begin
  if not Assigned(self) then exit;
   try
     Connected:= false;
     wfLog(rsMessageDisconnectSucefull);
   except
     raise;
   end;
end;

end.
