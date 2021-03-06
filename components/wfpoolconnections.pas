unit wfPoolConnections;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfTypes, wfFunc, wfClasses, wfIBConnection,
  wfODBCConnection, wfPQConnection, wfSQLTransaction, wfResourceStrings, sqldb;

type

  //SQLConnection:=GetConnFromPool;
  //if SQLConnection=NIL then
  //// sadness, there are no free connections
  //  exit;

  TwfSQLConnectRec = Record
    SQLEngine: TwfSQLEngine;
    Connect: TSQLConnection;
    Transaction: TwfSQLTransaction;
  end;

  { TwfPoolConnections }

  TwfPoolConnections = class(TComponent)
  private
    const
      fMaxConnectionsCount = 15;
  private

    fConnPool: TThreadList;
    fBaseName: string;
    fHost: string;
    fonLog: TTextEvent;
    fonNoAvailableConnections: TTextEvent;
    fPassword: string;
    fPort: string;
    fSQLEngine: TwfSQLEngine;
    fUserName: string;

    function CreateNewConnection(): TSQLConnection;
    procedure Disconnect(aConnect: TSQLConnection);
    procedure DisConnect;
    function GetCount: integer;
    procedure InitConnect(aConnect: TSQLConnection);
    procedure wOnLog(Sender: TObject; const aValue: string);
    procedure wOnNoAvailableConnections(Sender: TObject; const aValue: string);
    procedure SetSQLEngine(aValue: TwfSQLEngine);
  protected

  public
     constructor Create(AOwner: TComponent); override;
     constructor Create(AOwner: TComponent; const uHost, uPort, uBaseName, uUserName, uPassword: string);

     destructor Destroy; override;

     function GetConnFromPool(): TSQLConnection;
     function GetRecConnFromPool(): TwfSQLConnectRec;

     procedure ReturnConnToPool(aConnect: TSQLConnection);
     procedure ReturnRecConnToPool(aRecConnect: TwfSQLConnectRec);

     procedure Connect;

     property Count: integer read GetCount;
  published
     property SQLEngine: TwfSQLEngine read fSQLEngine write SetSQLEngine;
     property Host: string read fHost write fHost;
     property Port: string read fPort write fPort;
     property BaseName: string read fBaseName write fBaseName;
     property UserName: string read fUserName write fUserName;
     property Password: string read fPassword write fPassword;
     property onLog:TTextEvent read fonLog write fonLog;
     property onNoAvailableConnections:TTextEvent read fonNoAvailableConnections write fonNoAvailableConnections;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfpoolconnections_icon.lrs}
  RegisterComponents('WF',[TwfPoolConnections]);
end;

{ TwfPoolConnections }

constructor TwfPoolConnections.Create(AOwner: TComponent);
var
  fList: TList;
  i: Integer;
begin
  inherited Create(AOwner);
  fConnPool:= TThreadList.Create;
  SQLEngine:= sePostgreSQL;

  fList:= fConnPool.LockList;

  try
    for i := 0 to fMaxConnectionsCount - 1 do
       fList.Add(CreateNewConnection());

  finally
    fConnPool.UnlockList;
  end;
end;

constructor TwfPoolConnections.Create(AOwner: TComponent; const uHost, uPort, uBaseName, uUserName, uPassword: string);
begin
  inherited Create(AOwner);

  Host:= uHost;
  Port:= uPort;
  BaseName:= uBaseName;
  UserName:= uUserName;
  Password:= uPassword;
end;

destructor TwfPoolConnections.Destroy;
var
  fList:TList;
  fConn:TSQLConnection;
begin
  DisConnect;

  // kill the connection
  fList := fConnPool.LockList;

  try
      while fList.Count>0 do
      begin
        fConn:=TSQLConnection(fList[0]);
        fConn.Free;
        fList.Delete(0);
      end;
  finally
    fConnPool.UnlockList;
  end;

  FreeAndNil(fConnPool);

  inherited Destroy;
end;

procedure TwfPoolConnections.Connect;
var
  i, fCount: Integer;
  fList: TList;
begin
  fList:= fConnPool.LockList;

  try
    for i := 0 to fMaxConnectionsCount - 1 do
       InitConnect(TSQLConnection(fList.Items[i]));

    fCount:= fList.Count;
  finally
    fConnPool.UnlockList;
  end;

  wOnLog(self, Format(rsPoolConnectionsConnected,[fCount, fMaxConnectionsCount]));
end;

procedure TwfPoolConnections.DisConnect;
var
  i, fCount: Integer;
  fList: TList;
begin
  fList:= fConnPool.LockList;

  try
    for i := 0 to fMaxConnectionsCount - 1 do
       Disconnect(TSQLConnection(fList.Items[i]));

    fCount:= fList.Count;
  finally
    fConnPool.UnlockList;
  end;

  wOnLog(self, Format(rsPoolConnectionsConnected,[fCount, fMaxConnectionsCount]));
end;

procedure TwfPoolConnections.InitConnect(aConnect: TSQLConnection);
begin
  case SQLEngine of
    seFirebird: begin
      TwfIBConnection(aConnect).Connect(Host, Port, BaseName, UserName, Password);
    end;
    sePostgreSQL: begin
      TwfPQConnection(aConnect).Connect(Host, Port, BaseName, UserName, Password);
    end;
  end;
end;

procedure TwfPoolConnections.Disconnect(aConnect: TSQLConnection);
begin
  case SQLEngine of
    seFirebird: begin
      TwfIBConnection(aConnect).Disconnect;
    end;
    sePostgreSQL: begin
      TwfPQConnection(aConnect).Disconnect;
    end;
  end;
end;

function TwfPoolConnections.CreateNewConnection():TSQLConnection;
begin
  case SQLEngine of
    seFirebird: begin
      Result:= TwfIBConnection.Create(self);
      TwfIBConnection(Result).OnLog:= @wOnLog;
      //TwfIBConnection(Result).Connect(Host, Port, BaseName, UserName, Password);
    end;
    sePostgreSQL: begin
      Result:= TwfPQConnection.Create(self);
      TwfPQConnection(Result).OnLog:=@wOnLog;
      //TwfPQConnection(Result).Connect(Host, Port, BaseName, UserName, Password);
    end;
  end;
end;


function TwfPoolConnections.GetCount: integer;
var
  fList: TList;
begin
  fList:=fConnPool.LockList;

  try
    Result:= fList.Count;
  finally
    fConnPool.UnlockList;
  end;
end;

procedure TwfPoolConnections.wOnLog(Sender: TObject; const aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

procedure TwfPoolConnections.wOnNoAvailableConnections(Sender: TObject; const aValue: string);
begin
  if Assigned(onNoAvailableConnections) then onNoAvailableConnections(self, aValue);
end;

function TwfPoolConnections.GetConnFromPool():TSQLConnection;
var
  aAttempt:integer;
  fList:TList;
  fOk:boolean;
begin
  aAttempt:=0;
  fOk:=false;
  Result := nil;

  while (Result=nil) and (aAttempt<10) do
  begin
    fList:=fConnPool.LockList;
    try
      if fList.Count=0 then
        inc(aAttempt)
      else
        begin
          Result:=TSQLConnection(fList.Extract(fList.Items[Random(fList.Count)]));
          wOnLog(self, Format(rsPoolConnectionsIssConnect,[fList.Count]));
        end;
    finally
      fConnPool.UnlockList;
    end;

    if Result=nil then sleep(10);
  end;

  if Result = nil then
    wOnNoAvailableConnections(self,rsPoolConnectionsNoAvConnect);
end;

function TwfPoolConnections.GetRecConnFromPool(): TwfSQLConnectRec;
begin
  Result.SQLEngine:= SQLEngine;
  Result.Connect:= GetConnFromPool();
  Result.Transaction:= TwfSQLTransaction.Create(self);
  Result.Transaction.TransactionType:= wtBlank;
  Result.Connect.Transaction:= Result.Transaction;
end;

procedure TwfPoolConnections.ReturnConnToPool(aConnect:TSQLConnection);
var
  fList: TList;
begin
  fList:= fConnPool.LockList;

  try
    fList.Add(aConnect);
    wOnLog(self, Format(rsPoolConnectionsRetConnect,[fList.Count]));
  finally
    fConnPool.UnlockList;
  end;
end;

procedure TwfPoolConnections.ReturnRecConnToPool(aRecConnect: TwfSQLConnectRec);
begin
  aRecConnect.Transaction.Rollback;
  aRecConnect.Connect.Transaction:= nil;
  ReturnConnToPool(aRecConnect.Connect);
  FreeAndNil(aRecConnect.Transaction);
end;

procedure TwfPoolConnections.SetSQLEngine(aValue: TwfSQLEngine);
begin
  if fSQLEngine=aValue then Exit;
  fSQLEngine:=aValue;
  UserName:= 'postgres';
  Port:= '5432';
end;

end.
