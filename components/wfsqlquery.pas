{
This file is part of wfFrameWork.

 -= Base =-

 wofs(c)2017-2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfSQLQuery;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfFunc,
  wfTypes, sqldb, db;

type

  { TwfSQLQuery }

  TwfSQLQuery = class(TSQLQuery)
  private


  protected

  public
     function FirldAsVariant(aFieldName: string): variant;
     function Field(aFieldIndex: Integer): TField;
     function Field(aFieldName: string): TField;
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfsqlquery_icon.lrs}
  RegisterComponents('WF',[TwfSQLQuery]);
end;

{ TwfSQLQuery }

function TwfSQLQuery.FirldAsVariant(aFieldName: string): variant;
begin
  Result:= EmptyStr;
  if not Active then exit;
  Result:= FieldByName(aFieldName).AsVariant;
end;

function TwfSQLQuery.Field(aFieldIndex: Integer): TField;
begin
  Result:= Fields[aFieldIndex];
end;

function TwfSQLQuery.Field(aFieldName: string): TField;
begin
  Result:= FieldByName(aFieldName);
end;

end.
