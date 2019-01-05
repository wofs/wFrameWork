{
This file is part of wfFrameWork.

 -= IBConnection =-

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
    Result:= 'CREATE DOMAIN GUID AS '+wfLineEnding
      +' CHAR(32) CHARACTER SET OCTETS '+wfLineEnding
      +' COLLATE OCTETS;';
  {$ELSE}
    Result:='CREATE TABLE WF_SETTINGS ('+wfLineEnding
      +' NAME    VARCHAR(50),'+wfLineEnding
      +' FVALUE  VARCHAR(255)'+wfLineEnding
      +' );'
      +'COMMIT RETAIN ;'+wfLineEnding
      +'INSERT INTO WF_SETTINGS (NAME, FVALUE) VALUES (''WF_DEPARTMENT'', ''0''); '+wfLineEnding
      +'COMMIT RETAIN ;'+wfLineEnding
      +'SET TERM ^ ;'+wfLineEnding
      +' create or alter procedure WF_GET_DEPARTMENTSALT'+wfLineEnding
      +' returns ('+wfLineEnding
      +'     SALT bigint)'+wfLineEnding
      +' as'+wfLineEnding
      +' begin'+wfLineEnding
        +'   SALT = (SELECT FVALUE FROM WF_SETTINGS WHERE NAME=''WF_DEPARTMENT'');'+wfLineEnding
        +'   SALT = :SALT*1000000000000000000;'+wfLineEnding
        +'   suspend;'+wfLineEnding
        +' end'+wfLineEnding
      +' ^'+wfLineEnding
      +' SET TERM ; ^';

  {$ENDIF}
end;

function TwfIBConnection.GetBeforeInsertTrigger(aTableName, aIDField:string): string;
begin
  {$IFDEF USEGUID}
    Result:= 'CREATE OR ALTER TRIGGER %s_BI0 FOR %s '+wfLineEnding
      +' ACTIVE BEFORE INSERT POSITION 0 '+wfLineEnding
      +' AS '+wfLineEnding
      +' begin '+wfLineEnding
      +'    IF (NEW.%s IS NULL) THEN '+wfLineEnding
      +'     NEW.%s = REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'',''''); '+wfLineEnding
      +' end';
  {$ELSE}
    Result:='SET TERM ^ ;'+wfLineEnding
      +' CREATE OR ALTER TRIGGER %s_BI FOR %s'+wfLineEnding
      +' ACTIVE BEFORE INSERT POSITION 0'+wfLineEnding
      +' AS'+wfLineEnding
      +' BEGIN'+wfLineEnding
      +'   IF (NEW.%s IS NULL) THEN'+wfLineEnding
      +'     NEW.%s = (SELECT SALT FROM WF_GET_DEPARTMENTSALT)+GEN_ID(GEN_%s_%s,1);'+wfLineEnding
      +' END'+wfLineEnding
      +' ^'+wfLineEnding
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
