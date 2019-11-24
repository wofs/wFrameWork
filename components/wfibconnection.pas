{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfIBConnection;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfTypes,
  wfResourceStrings, IBConnection;

type

  { TwfIBConnection }

  TwfIBConnection = class(TIBConnection)
  private
    fonLog: TTextEvent;
    procedure AsyncInit(Data: PtrInt);

  protected
    procedure wfLog(const aValue: string);

  public
    constructor Create(AOwner: TComponent); override;

    function GetDomainOrProcedureString:string;
    function GetBeforeInsertTrigger(aTableName, aIDField: string): string;

    function Connect(const uHost, uPort, uBaseName, uUserName, uPassword: string
      ): boolean;

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
  {$I wfibconnection_icon.lrs}
  RegisterComponents('WF',[TwfIBConnection]);
end;

{ TwfIBConnection }

procedure TwfIBConnection.AsyncInit(Data: PtrInt);
begin
  if not(csDesigning in ComponentState) then Connected:= false;
end;

{@@ ----------------------------------------------------------------------------
  // Log write
  @param    aValue     Text for write to Log
-------------------------------------------------------------------------------}
procedure TwfIBConnection.wfLog(const aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

constructor TwfIBConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Application.QueueAsyncCall(@AsyncInit,0);
end;

function TwfIBConnection.GetDomainOrProcedureString: string;
begin
  {$IFDEF USEGUID}
    Result:= 'CREATE DOMAIN GUID AS '+wfLE
      +' CHAR(32) CHARACTER SET OCTETS '+wfLE
      +' COLLATE OCTETS;';
  {$ELSE}
    Result:='CREATE TABLE WF_SETTINGS ('+wfLE
      +' NAME    VARCHAR(50),'+wfLE
      +' FVALUE  VARCHAR(255)'+wfLE
      +' );'
      +'COMMIT RETAIN ;'+wfLE
      +'INSERT INTO WF_SETTINGS (NAME, FVALUE) VALUES (''WF_DEPARTMENT'', ''0''); '+wfLE
      +'COMMIT RETAIN ;'+wfLE
      +'SET TERM ^ ;'+wfLE
      +' create or alter procedure WF_GET_DEPARTMENTSALT'+wfLE
      +' returns ('+wfLE
      +'     SALT bigint)'+wfLE
      +' as'+wfLE
      +' begin'+wfLE
        +'   SALT = (SELECT FVALUE FROM WF_SETTINGS WHERE NAME=''WF_DEPARTMENT'');'+wfLE
        +'   SALT = :SALT*1000000000000000000;'+wfLE
        +'   suspend;'+wfLE
        +' end'+wfLE
      +' ^'+wfLE
      +' SET TERM ; ^';

  {$ENDIF}
end;

function TwfIBConnection.GetBeforeInsertTrigger(aTableName, aIDField:string): string;
begin
  {$IFDEF USEGUID}
    Result:= 'CREATE OR ALTER TRIGGER %s_BI0 FOR %s '+wfLE
      +' ACTIVE BEFORE INSERT POSITION 0 '+wfLE
      +' AS '+wfLE
      +' begin '+wfLE
      +'    IF (NEW.%s IS NULL) THEN '+wfLE
      +'     NEW.%s = REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'',''''); '+wfLE
      +' end';
  {$ELSE}
    Result:='SET TERM ^ ;'+wfLE
      +' CREATE OR ALTER TRIGGER %s_BI FOR %s'+wfLE
      +' ACTIVE BEFORE INSERT POSITION 0'+wfLE
      +' AS'+wfLE
      +' BEGIN'+wfLE
      +'   IF (NEW.%s IS NULL) THEN'+wfLE
      +'     NEW.%s = (SELECT SALT FROM WF_GET_DEPARTMENTSALT)+GEN_ID(GEN_%s_%s,1);'+wfLE
      +' END'+wfLE
      +' ^'+wfLE
      +' SET TERM ; ^';
  {$ENDIF}

  Result:= Format(Result,[aTableName,aTableName,aIDField,aIDField, aTableName, aIDField]);
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
function TwfIBConnection.Connect(const uHost, uPort, uBaseName, uUserName,
  uPassword: string): boolean;
begin
  try
    Result:= false;

    Connected   := False;
    LoginPrompt := False;
    Params.Clear;
    //LibraryName := fLibraryName;

    if (Length(uHost)=0) or (Length(uPort)=0) then
      DatabaseName := uBaseName
    else
      DatabaseName := uHost+'/'+uPort+':'+uBaseName;
    UserName:= uUserName;
    Password:= uPassword;
    Params.Clear;
    //Params.Add('user_name='+uUserName);
    //Params.Add('password='+uPassword);
    Params.Add('lc_ctype=UTF8');

    wfLog('Host: '+uHost);
    wfLog('Port: '+uPort);
    wfLog('BaseName: '+uBaseName);

    Connected := True;


    Result:= true;

    wfLog(rsMessageConectedSucefull);
  except
    raise;
  end;
end;

function TwfIBConnection.SQLFieldIsExists(const uTable, uFieldName: string
  ): string;
const
  uSQL = 'SELECT RDB$FIELD_NAME '
        +' FROM RDB$RELATION_FIELDS '
        +' WHERE RDB$RELATION_NAME=%s AND RDB$FIELD_NAME=%s';
begin
  Result:= Format(uSQL, [QuotedStr(uTable), QuotedStr(uFieldName)])
end;

function TwfIBConnection.SQLProcIsExists(const uProcName: string): string;
const
  uSQL = 'SELECT rdb$procedure_source '
          +' FROM rdb$procedures '
          +' WHERE rdb$procedure_name = %s';
begin
  Result:= Format(uSQL, [QuotedStr(uProcName)])
end;

function TwfIBConnection.SQLTriggerIsExists(const uTriggerName: string): string;
const
  uSQL = 'SELECT * FROM RDB$TRIGGERS '
      +' WHERE RDB$SYSTEM_FLAG = 0 '
      +' AND RDB$TRIGGER_NAME=%s';
begin
  Result:= Format(uSQL, [QuotedStr(uTriggerName)])
end;

function TwfIBConnection.SQLTableIsExists(const uTable: string): string;
const
  uSQL = 'SELECT DISTINCT RDB$RELATION_NAME '
        +' FROM RDB$RELATION_FIELDS '
        +' WHERE RDB$SYSTEM_FLAG=0 '
        +' AND RDB$RELATION_NAME=%s';
begin
  Result:= Format(uSQL, [QuotedStr(uTable)])
end;

function TwfIBConnection.SQLGetTables: string;
begin
  Result:= 'SELECT DISTINCT RDB$RELATION_NAME FROM RDB$RELATION_FIELDS WHERE RDB$SYSTEM_FLAG=0;';
end;

{@@ ----------------------------------------------------------------------------
  // Disconnect from DataBase
-------------------------------------------------------------------------------}
procedure TwfIBConnection.Disconnect;
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
