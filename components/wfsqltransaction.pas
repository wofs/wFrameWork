{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}unit wfSQLTransaction;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, sqldb;

type
  TwfSQLTransactionType = (wtRead, wtWrite);

  { TwfSQLTransaction }

  TwfSQLTransaction = class(TSQLTransaction)
  private
    fTransactionType: TwfSQLTransactionType;
    procedure AddParamsRead;
    procedure AddParamsWrite;
    procedure SetTransactionType(aValue: TwfSQLTransactionType);

  protected

  public
    constructor Create(AOwner: TComponent); override;
  published
    property TransactionType:TwfSQLTransactionType read fTransactionType write SetTransactionType;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfsqltransaction_icon.lrs}
  RegisterComponents('WF',[TwfSQLTransaction]);
end;

{ TwfSQLTransaction }

procedure TwfSQLTransaction.AddParamsRead;
begin
  Params.Clear;
  Params.Add('read_committed');
  Params.Add('rec_version');
  Params.Add('nowait');
  Params.Add('read');
end;

procedure TwfSQLTransaction.AddParamsWrite;
begin
  Params.Clear;
  Params.Add('write');
  Params.Add('read_committed');
  Params.Add('nowait');
end;

procedure TwfSQLTransaction.SetTransactionType(aValue: TwfSQLTransactionType);
begin
  self.EndTransaction;

  fTransactionType:=aValue;
  Action:=caRollback;

case fTransactionType of
  wtRead: AddParamsRead;
  wtWrite: AddParamsWrite;
end;

end;

constructor TwfSQLTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddParamsRead;
end;

end.
