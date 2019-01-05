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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, odbcconn,
  SQLDB;

type

  { TwfODBCConnection }

  TwfODBCConnection = class(TODBCConnection)
  private
    function Connect(aHost, aUserName, aPassword, aDataBase: string;
      const aReadOnly:boolean = true; const aDriver: string = 'ODBC Driver 11 for SQL Server'): boolean;

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfodbcconnection_icon.lrs}
  RegisterComponents('WF',[TwfODBCConnection]);
end;

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
  except
    raise
  end;
end;

end.
