{
This file is part of wfFrameWork.

 -= Base =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfBase;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LazUTF8,
  wfTypes, wfClasses, wfResourceStrings, wfFunc, wfSQLQuery, wfSQLTransaction,
  wfIBConnection, wfSQLScript, wfODBCConnection, db, sqldb;

type

  //If you change TwfSQLEngine to make changes in the same way
  //QuotedGUID() , GetEngine(), GetUseGUID, GetDomainOrProcedureString, GetBeforeInsertTrigger
  TwfSQLEngine = (seFirebird, seODBC, seUnknown);

  { TwfData }

  TwfData = class
  private
    fFields: TwfFields;
    fRows: TwfBaseRows;

    function ConvertNullFieldToVar(aField: TwfField): Variant;
    function GetAsArray(aFieldName: string): ArrayOfString;
    function GetFieldCount: integer;
    function GetRowCount: integer;

    public
      constructor Create(aFields: TwfFields; aRows: TwfBaseRows);
      destructor Destroy; override;

      property Fields: TwfFields read fFields write fFields;
      property Rows: TwfBaseRows read fRows write fRows;
      property AsArray[aField:string]: ArrayOfString read GetAsArray;

      property RowCount: integer read GetRowCount;
      property FieldCount: integer read GetFieldCount;
      function Data(const uRowIndex: integer; const uFieldName: string): variant;
      function Data(const uRowIndex, uColIndex: integer): variant;
  end;

  { TwfBase }

  TwfBase = class(TComponent)
  private
    fConnection: TSQLConnection;
    //fLibraryName: RawByteString;
    fLimitLoadedRows: integer;
    fLongTransaction: TwfSQLTransaction;
    fonLog: TTextEvent;
    fonOverloadLimitLoadedRows: TNotifyEvent;
    fQueryRead: TwfSQLQuery;
    fQueryWrite: TwfSQLQuery;
    fTransactionRead: TwfSQLTransaction;
    fTransactionWrite: TwfSQLTransaction;

    procedure CreateNewDataBaseFireBird(const uHost, uPort, uBaseName,
      uUserName, uPassword: string);
    function GetInitializedDefaultProc: boolean;
    function GetLongTransactionStatus: boolean;

    // Кeturns the type of engine used. It is necessary to use specific functions.
    function GetEngine: TwfSQLEngine;
    function GetNewBaseID: BaseID;
    function GetTransactionRead: TwfSQLTransaction;
    function GetTransactionWrite: TwfSQLTransaction;
    procedure SetInitializedDefaultProc(aValue: boolean);
    procedure SetTransactionRead(aValue: TwfSQLTransaction);
    procedure TransactionReadInit(var aTransaction: TwfSQLTransaction;
      var aDataSet: TwfSQLQuery);

    {Services functions}
    procedure wfLog(aValue: string);

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      {Transactions}
      // Create Read Transaction
      procedure CreateReadTransaction(const Sender: TComponent;
        var aTransaction: TwfSQLTransaction);
      // Create Write Transaction
      procedure CreateWriteTransaction(const Sender: TComponent;
        var aTransaction: TwfSQLTransaction);

      { -= Working with the database =-}
      {Read}
      function OpenSQL(const uSQL: string; var aParams: TwfParams):TwfSQLQuery;
      function OpenSQL(const uSQL: string):TwfSQLQuery;
      function OpenSQLFmt (const uSQL : string; const Args : array of const):TwfSQLQuery;

      function GetData(const uSQL: string; var aParams: TwfParams):TwfData;
      function GetData(const uSQL: string):TwfData;

      {Write}
      function Insert(const uTable: string; const uFields: array of string; const uValues: array of variant; const aResultField: string = 'ID'; aMatchingFields:string = ''): variant;
      function Update(const uTable: string; const uFields: array of string; const uValues: array of variant; const uWhere: string = ''): boolean;
      function Delete(const uTable: string; aBaseID: BaseID): boolean;

      function ExecSQL(const uSQL: string): boolean;
      function ExecSQL(const uSQL: string; var aParams: TwfParams; const aResultField: string = ''): variant;
      procedure ExecSQLRaw(const uSQL: string; uParamCheck: boolean = true);
      procedure ExecSQLScript(const uSQLScript: TStrings);

      {Check}
      function FieldIsExists(const uTable, uFieldName:string):boolean;
      function ProcIsExists(const uProcName:string):boolean;
      function TriggerIsExists(const uTriggerName:string):boolean;
      function TableIsExists(const uTable: string): boolean;

      {Lists}
      function GetTables: ArrayOfString;

      {Get}
      function GetRowsCount(const uSQL: string): int64;

      {Convert}
      function DataToStr(aField: TField; aCSVComma: boolean= false; aBr: boolean = false): string;

      {Aggregate}
      function WriteWhere(const uSQL: string; aWhere: string; const aClearOldWhere: boolean = true): string;
      function WriteOrderBy(const uSQL: string; aOrderBy: string): string;

      {Connect / Disconnect}
      procedure CreateNewDataBase(const uHost, uPort, uBaseName, uUserName, uPassword: string);

      {Long Transaction Managment}
      procedure LongTransactionStart;
      procedure LongTransactionCommit;
      procedure LongTransactionRolback;

      procedure TransactionCommit(aTransaction: TwfSQLTransaction);
      procedure TransactionRolback(aTransaction: TwfSQLTransaction);

      {-= Wokring with params =-}
      // Create Params List
      procedure CreateParam(var aParams: TwfParams; const aSQL: string; const aFreeAfterUse: boolean = false);
      // Convert the parameter value to the correct variant type.
      function ParamToVar(aParam: TParam): Variant;

      {Convert functions}
      function GetFieldTypeByVar(aVariantValue: Variant):TFieldType;
      function GetDomainOrProcedureString:string;
      function GetBeforeInsertTrigger(aTableName, aIDField: string): string;

      //Var To String
      function AsString(const uVariant: variant): string;
      function AsString(const uFieldType: TFieldType): string;

      function AsString(aArr: ArrayOfString): string;
      function AsString(aArr: ArrayOfBaseID): string;
      function AsString(aArr: ArrayOfInt64): string;

      {-= Properties =-}
      property LongTransaction: boolean read GetLongTransactionStatus;
  published
    property Connection: TSQLConnection read fConnection write fConnection default nil;
    property QueryRead: TwfSQLQuery read fQueryRead write fQueryRead default nil;
    property QueryWrite: TwfSQLQuery read fQueryWrite write fQueryWrite default nil;
    property TransactionRead: TwfSQLTransaction read GetTransactionRead write SetTransactionRead default nil;
    property TransactionWrite: TwfSQLTransaction read GetTransactionWrite write fTransactionWrite default nil;
    //The maximum number of rows that can be loaded into TwfData.
    //Use onOverloadLimitLoadedRows for signaling the exceeding the limit
    property LimitLoadedRows: integer read fLimitLoadedRows write fLimitLoadedRows default 5000;

    //Used Engine. ReadOnly
    property Engine: TwfSQLEngine read GetEngine;

    property InitializedDefaultProc: boolean read GetInitializedDefaultProc write SetInitializedDefaultProc;

    {Events}
    property onLog: TTextEvent read fonLog write fonLog;
    property onOverloadLimitLoadedRows: TNotifyEvent read fonOverloadLimitLoadedRows write fonOverloadLimitLoadedRows;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfbase_icon.lrs}
  RegisterComponents('WF',[TwfBase]);
end;

{ TwfData }

{@@ ----------------------------------------------------------------------------
  // Get Data of Field
  @param    uRowIndex          Row
  @param    uFieldName         Field Name
  @result   variant            Field Data
-------------------------------------------------------------------------------}
function TwfData.Data(const uRowIndex: integer; const uFieldName: string
  ): variant;
var
  i: Integer;
  aFieldName: String;
begin
    aFieldName:= UTF8UpperCase(uFieldName);

    Result:= null;

    for i:=0 to FieldCount-1 do
      if UTF8UpperCase(Fields[i].Name) = aFieldName then
       begin
         Result:= Data(uRowIndex, i);
         Break;
       end;

    if Result = null then
       raise Exception.Create(Format(rsExceptErrorFindFieldName,[uFieldName]));
end;

{@@ ----------------------------------------------------------------------------
  // Convert Null Field to Variant
  @param    aField
  @result   vatiant
-------------------------------------------------------------------------------}
function TwfData.ConvertNullFieldToVar(aField: TwfField): Variant;
begin
  case aField.DataType of
    ftSmallInt,
    ftLargeint,
    ftInteger          : Result:= 0;
    ftFloat,
    ftCurrency,
    ftBCD              : Result:= 0.0;
    ftDate,
    ftTime,
    ftDateTime         : Result:= now;
    ftBoolean          : Result:= false;
    ftString,
    ftWideString       : Result := ''
    else
      Result:= aField.DataType;
  end;
end;

function TwfData.GetAsArray(aFieldName: string): ArrayOfString;
var
  i: Integer;
begin
  SetLength(Result, RowCount);
  for i:=0 to RowCount-1 do
    Result[i]:= VarToStr(Data(i,aFieldName));
end;

{@@ ----------------------------------------------------------------------------
  // Get Data of Cell Index
  @param    uRowIndex          Row
  @param    uColIndex          Col
  @result   variant            Field Data
-------------------------------------------------------------------------------}
function TwfData.Data(const uRowIndex, uColIndex: integer): variant;
begin
  Result:= fRows[uRowIndex].Fields[uColIndex].Value;
  if Result = null then
     Result:= ConvertNullFieldToVar(Fields[uColIndex]);
end;

{@@ ----------------------------------------------------------------------------
  // Get Fields Count in Base Rows
  @result   integer      Fields Count
-------------------------------------------------------------------------------}
function TwfData.GetFieldCount: integer;
begin
  Result:= Length(fFields);
end;

{@@ ----------------------------------------------------------------------------
  // Rows Count
  @result   integer            Rows count
-------------------------------------------------------------------------------}
function TwfData.GetRowCount: integer;
begin
  Result:= Length(fRows);
end;

constructor TwfData.Create(aFields: TwfFields; aRows: TwfBaseRows);
begin
  inherited Create;
  fFields:= aFields;
  fRows:= aRows;
end;

destructor TwfData.Destroy;
begin
  fFields:= nil;
  fRows:= nil;
  inherited Destroy;
end;

{ TwfBase }

{@@ ----------------------------------------------------------------------------
  // Convert Variant To String
  @param    aVariant    Variant Value
  @result   string      String Value
-------------------------------------------------------------------------------}
function TwfBase.AsString(const uVariant: variant): string;
begin
  Result:= VarToStr(uVariant);
end;

{@@ ----------------------------------------------------------------------------
  // Get Field Type By Var
  @param    aVariantValue    Variant Value
  @result   TFieldType       TFieldType Value
-------------------------------------------------------------------------------}

function TwfBase.GetFieldTypeByVar(aVariantValue: Variant): TFieldType;
begin
  case GetVarType(aVariantValue) of
    varSmallInt      : Result := ftSmallint;
    varInteger       : Result := ftInteger;
    varint64         : Result := ftLargeint;
    varSingle        : Result := ftInteger;
    varDouble        : Result := ftFloat;
    varCurrency      : Result := ftCurrency;
    varDate          : Result := ftDateTime;
    varBoolean       : Result := ftBoolean;
    varstring        : Result := ftString;
    else
      Result := ftUnknown;
  end;

end;

{@@ ----------------------------------------------------------------------------
  // Get Domain Or Procedure String
  @result   string           string Value
-------------------------------------------------------------------------------}

function TwfBase.GetDomainOrProcedureString: string;
begin
  Result:='';
  case GetEngine of
    seFirebird: Result:= TwfIBConnection(fConnection).GetDomainOrProcedureString;
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Get Before Insert Trigger
  @param    aTableName       string Value
  @param    aIDField         string Value
  @result   string           string Value
-------------------------------------------------------------------------------}
function TwfBase.GetBeforeInsertTrigger(aTableName, aIDField:string): string;
begin
 Result:='';
 case GetEngine of
   seFirebird: Result:= TwfIBConnection(fConnection).GetBeforeInsertTrigger(aTableName, aIDField);
 end;
end;

{@@ ----------------------------------------------------------------------------
  // Convert ArrayOfInt64 To String
  @param    aArr        ArrayOfInt64 Value
  @result   string      String Value
-------------------------------------------------------------------------------}
function TwfBase.AsString(aArr: ArrayOfInt64): string;
var
  i: Integer;
begin
 Result:='';
 for i:=0 to High(aArr) do
 begin
   if Length(Result)>0 then
     Result:= Result+',';

   Result:= Result+ IntToStr(aArr[i]);
 end;
end;

function TwfBase.GetLongTransactionStatus: boolean;
begin
  Result:= Assigned(fLongTransaction);
end;

function TwfBase.GetInitializedDefaultProc: boolean;
begin
  Result:= false;
  if not Assigned(self) or not Assigned(fConnection) or not fConnection.Connected then exit;
  Result:= ProcIsExists('WF_GET_DEPARTMENTSALT');
end;

function TwfBase.GetEngine: TwfSQLEngine;
begin
 Result:= seUnknown;

 if fConnection is TwfIBConnection then
   Result:= seFirebird;

 if fConnection is TwfODBCConnection then
   Result:= seODBC;
end;

function TwfBase.GetNewBaseID: BaseID;
var
  aSQL: String;
  aDataset: TwfSQLQuery;
begin
 Result:= EmptyBaseID;
 aSQL:='';
 {$IFDEF USEGUID}
 case Engine of
   seFirebird: aSQL:='SELECT REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'','''') FROM RDB$DATABASE';
    else
     exit;
 end;

 if Length(aSQL)>0 then
  begin
    aDataset:=nil;
    OpenSQL(aSQL, aDataset);
    try
      if aDataset.RecordCount>0 then
        Result:= VarToBaseID(aDataset.Fields[0].AsVariant);
    finally
      FreeAndNil(aDataset);
    end;
  end;
 {$ENDIF}
end;

function TwfBase.GetTransactionRead: TwfSQLTransaction;
begin
  if not Assigned(fTransactionRead) then fTransactionRead:= nil;
  Result:= fTransactionRead;
end;

function TwfBase.GetTransactionWrite: TwfSQLTransaction;
begin
  if not Assigned(fTransactionWrite) then fTransactionWrite:= nil;
  Result:= fTransactionWrite;
end;

procedure TwfBase.SetInitializedDefaultProc(aValue: boolean);
var
  aStrings: TStringList;
begin
 if not Assigned(self) or (csLoading in ComponentState) or not Assigned(fConnection) or not fConnection.Connected then exit;

  aStrings:= TStringList.Create;

  try
    aStrings.Text:= GetDomainOrProcedureString;

    if aValue then
      ExecSQLScript(aStrings);
  finally
    FreeAndNil(aStrings);
  end;
end;

procedure TwfBase.SetTransactionRead(aValue: TwfSQLTransaction);
begin
  //if Assigned(fTransactionRead) and fTransactionRead.Active then
  //  fTransactionRead.Commit;

  fTransactionRead:= aValue;

  if Assigned(fTransactionRead) then
    if not fTransactionRead.Active then
      if not Assigned(fTransactionRead.DataBase) then fTransactionRead.DataBase:= fConnection;
        //fTransactionRead.StartTransaction;
end;

{@@ ----------------------------------------------------------------------------
  // Log write
  @param    aValue     Text for write to Log
-------------------------------------------------------------------------------}
procedure TwfBase.wfLog(aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

constructor TwfBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LimitLoadedRows:= 5000;
  fTransactionRead:= nil;
end;


{@@ ----------------------------------------------------------------------------
  // Destroy
-------------------------------------------------------------------------------}
destructor TwfBase.Destroy;
begin
  inherited Destroy;
end;

{@@ ----------------------------------------------------------------------------
  // Create Read Transaction
  @result  Transaction
-------------------------------------------------------------------------------}
procedure TwfBase.CreateReadTransaction(const Sender:TComponent; var aTransaction: TwfSQLTransaction);
begin
  aTransaction:=TwfSQLTransaction.Create(Sender);
  with aTransaction do begin
    Database:=fConnection;
    Action:=caRollback;
    Params.Clear;
    Params.Add('read');
    Params.Add('read_committed');
    Params.Add('rec_version');
    Params.Add('nowait');
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Create Write Transaction
  @result  Transaction
-------------------------------------------------------------------------------}
procedure TwfBase.CreateWriteTransaction(const Sender:TComponent; var aTransaction: TwfSQLTransaction);
begin
  aTransaction:=TwfSQLTransaction.Create(Sender);
  with aTransaction do begin
    Database:=fConnection;
    Action:=caRollback;
    Params.Clear;
    Params.Add('write');
    Params.Add('read_committed');
    Params.Add('nowait');
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Selection of rows with data from DB
  @param   uSQL          SQL Text
  @param   uParamValue   SQL Params
  @result  TwfData       BaseRows Object
-------------------------------------------------------------------------------}
function TwfBase.GetData(const uSQL: string; var aParams: TwfParams): TwfData;
var
  aDataSet: TwfSQLQuery;
  i, iField, aFieldCount: integer;
  aRows: TwfBaseRows;
  aFields: TwfFields;
begin
  aDataSet:= nil;
  aRows:= nil;
  aFields:= nil;
  i:= 0;

  aDataSet:= OpenSQL(uSQL, aParams);
  try
    SetLength(aRows, LimitLoadedRows);

    aFieldCount:= aDataSet.FieldCount;

    SetLength(aFields, aFieldCount);

    for iField:=0 to aFieldCount-1 do
    begin
      aFields[iField].Name:= aDataSet.Fields[iField].FieldName;
      aFields[iField].DataType:= aDataSet.Fields[iField].DataType;
    end;

    while not aDataSet.Eof do
    begin
      if i> LimitLoadedRows-1 then
       begin
         {Signal about exceeding the limit}
         if Assigned(fonLog) then fonLog(self,Format(rsWarningNumberOfEntriesWasLimited,[LimitLoadedRows]));
         if Assigned(fonOverloadLimitLoadedRows) then fonOverloadLimitLoadedRows(self);
       end;

      aRows[i].Index:= i;

      for iField:=0 to aFieldCount-1 do
      begin
        SetLength(aRows[i].Fields, aFieldCount);
        aRows[i].Fields[iField].Value:= aDataSet.Fields[iField].Value;
      end;
      inc(i);
      aDataSet.Next;
    end;

    SetLength(aRows, i);

    Result:= TwfData.Create(aFields, aRows);

  finally
    FreeAndNil(aDataSet);
  end;
end;

{@@ ----------------------------------------------------------------------------
// Selection of rows with data from DB
@param   uSQL          SQL Text
@result  TwfData       BaseRows Object
-------------------------------------------------------------------------------}
function TwfBase.GetData(const uSQL: string): TwfData;
var
  aParams: TwfParams;
begin
  aParams:= TwfParams.Create(self);
  try
    Result:= GetData(uSQL, aParams);
  finally
    aParams.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  // SQL query to Update Data
  @param  uSQL          SQL Text
  @param  uParamValue   SQL Params
  @param  aResultField  A result field name
  @param  aCommit       Flag of Commit
  @result Result        Inserted ID
-------------------------------------------------------------------------------}
function TwfBase.ExecSQL(const uSQL: string; var aParams: TwfParams;
  const aResultField: string): variant;
var
  aTransaction: TwfSQLTransaction;
  i: Integer;
  aQuery: TwfSQLQuery;
begin
  Result:= false;

  if not Assigned(fQueryWrite) then
     aQuery:= TwfSQLQuery.Create(self)
   else
     aQuery:= fQueryWrite;

   try
     if LongTransaction then
       aTransaction:= fLongTransaction
     else
       if not Assigned(fTransactionWrite) then
         CreateWriteTransaction(aQuery, aTransaction)
       else
         aTransaction:= fTransactionWrite;

     if not aTransaction.Active then aTransaction.StartTransaction;

     with aQuery do begin
       Close;
       Database:= fConnection;
       Transaction:= aTransaction;
       SQL.Text:= uSQL;
       wfLog(SQL.Text);

       if Params.Count>0 then
           wfLog('-= Params =-');

       for i:=0 to Params.Count-1 do
       begin
         Params.ParamByName(aParams[i].Name).Value:= ParamToVar(aParams[i]);
         wfLog(':'+Params[i].Name+' = '+Params[i].AsString);
       end;

       try
         if wfFunc.IsEmpty(aResultField) then
           ExecSQL
         else
           Open;

         if wfFunc.IsEmpty(aResultField) then
           Result:= true
         else
           Result:= FieldByName(aResultField).AsVariant;

       except
         TransactionRolback(aTransaction);
         raise
       end;

     end;
     TransactionCommit(aTransaction);
   finally
     if aParams.FreeAfterUse then FreeAndNil(aParams);
     if not Assigned(fQueryWrite) then
       FreeAndNil(aQuery);
   end;

end;

procedure TwfBase.ExecSQLRaw(const uSQL: string; uParamCheck: boolean);
var
  aTransaction: TwfSQLTransaction;
  aQuery: TwfSQLQuery;
begin
  if not Assigned(fQueryWrite) then
     aQuery:= TwfSQLQuery.Create(self)
   else
     aQuery:= fQueryWrite;

   try
     if LongTransaction then
       aTransaction:= fLongTransaction
     else
       if not Assigned(fTransactionWrite) then
         CreateWriteTransaction(aQuery, aTransaction)
       else
         aTransaction:= fTransactionWrite;

     if not aTransaction.Active then aTransaction.StartTransaction;

     with aQuery do begin
       Close;
       ParamCheck:= uParamCheck;
       Database:= fConnection;
       Transaction:= aTransaction;
       SQL.Text:= uSQL;
       wfLog(SQL.Text);

       try
         ExecSQL;
       except
         TransactionRolback(aTransaction);
         raise
       end;

     end;

     TransactionCommit(aTransaction);
   finally
     if not Assigned(fQueryWrite) then
       FreeAndNil(aQuery);
   end;
end;

procedure TwfBase.ExecSQLScript(const uSQLScript: TStrings);
var
  aTransaction: TwfSQLTransaction;
  aQuery: TwfSQLScript;
begin
  aTransaction:= nil;
  aQuery:= TwfSQLScript.Create(self);

   try
     if LongTransaction then
       raise Exception.Create(rsExceptErrorLongTransactionIsActive);

     CreateWriteTransaction(aQuery, aTransaction);

     if not aTransaction.Active then aTransaction.StartTransaction;

     with aQuery do begin
       UseCommit:= true;
       Database:= fConnection;
       Transaction:= aTransaction;
       Script.Assign(uSQLScript);
       wfLog(Script.Text);

       try
         ExecuteScript;
       except
         aTransaction.Rollback;
         raise
       end;

     end;

     aTransaction.Commit;
   finally
     FreeAndNil(aQuery);
   end;
end;

{@@ ----------------------------------------------------------------------------
  // Check Field Is Exists
  @param  uTable
  @param  uFieldName
  @result boolean
-------------------------------------------------------------------------------}
function TwfBase.FieldIsExists(const uTable, uFieldName: string): boolean;
const
  uSQL = 'SELECT RDB$FIELD_NAME '
        +' FROM RDB$RELATION_FIELDS '
        +' WHERE RDB$RELATION_NAME=%s AND RDB$FIELD_NAME=%s';
var
  aDataSet: TwfSQLQuery;
begin
   aDataSet:= nil;
   aDataSet:= OpenSQLFmt(uSQL, [QuotedStr(uTable), QuotedStr(uFieldName)]);
   try
     Result:= aDataSet.RecordCount>0;
   finally
     FreeAndNil(aDataSet);
   end;
end;

{@@ ----------------------------------------------------------------------------
  // Check Proc Is Exists
  @param  uProcName
  @result boolean
-------------------------------------------------------------------------------}
function TwfBase.ProcIsExists(const uProcName: string): boolean;
const
  uSQL = 'SELECT rdb$procedure_source '
          +' FROM rdb$procedures '
          +' WHERE rdb$procedure_name = %s';
var
  aDataSet: TwfSQLQuery;
begin
   aDataSet:= nil;
   aDataSet:= OpenSQLFmt(uSQL, [QuotedStr(uProcName)]);
   try
     Result:= aDataSet.RecordCount>0;
   finally
     FreeAndNil(aDataSet);
   end;
end;

{@@ ----------------------------------------------------------------------------
  // Check Trigger Is Exists
  @param  uProcName
  @result boolean
-------------------------------------------------------------------------------}
function TwfBase.TriggerIsExists(const uTriggerName: string): boolean;
const
  uSQL = 'SELECT * FROM RDB$TRIGGERS '
      +' WHERE RDB$SYSTEM_FLAG = 0 '
      +' AND RDB$TRIGGER_NAME=%s';
var
  aDataSet: TwfSQLQuery;
begin
   aDataSet:= nil;
   aDataSet:= OpenSQLFmt(uSQL, [QuotedStr(uTriggerName)]);
   try
     Result:= aDataSet.RecordCount>0;
   finally
     FreeAndNil(aDataSet);
   end;
end;

{@@ ----------------------------------------------------------------------------
  // Check Table Is Exists
  @param  uTable
  @result boolean
-------------------------------------------------------------------------------}
function TwfBase.TableIsExists(const uTable: string): boolean;
const
  uSQL = 'SELECT DISTINCT RDB$RELATION_NAME '
        +' FROM RDB$RELATION_FIELDS '
        +' WHERE RDB$SYSTEM_FLAG=0 '
        +' AND RDB$RELATION_NAME=%s';
var
  aDataSet: TwfSQLQuery;
begin
   aDataSet:= nil;
   aDataSet:= OpenSQLFmt(uSQL, [QuotedStr(uTable)]);
   try
     Result:= aDataSet.RecordCount>0;
   finally
     FreeAndNil(aDataSet);
   end;
end;

function TwfBase.GetTables: ArrayOfString;
var
  aData: TwfData;
  i: Integer;
  aSQL: string;
begin

case Engine of
  seFirebird  : aSQL:= 'SELECT DISTINCT RDB$RELATION_NAME FROM RDB$RELATION_FIELDS WHERE RDB$SYSTEM_FLAG=0;';
  seODBC      : aSQL:= 'SELECT name FROM sys.databases d WHERE d.database_id>4;';
end;
  aData:= nil;

  aData:= GetData(aSQL);
  try
    SetLength(Result, aData.RowCount);

    for i:= 0 to aData.RowCount-1 do
      Result[i]:= aData.Data(i,0);
  finally
    FreeAndNil(aData);
  end;
end;

function TwfBase.WriteOrderBy(const uSQL: string; aOrderBy: string): string;
var
  aPosEnd: Integer;
  aPosOrderBy: PtrInt;
begin
 Result:= UTF8UpperCase(uSQL);
 aPosEnd:= Length(Result);
 aPosOrderBy:= UTF8Pos('ORDER',Result,1);

  if aPosOrderBy>0 then
    UTF8Delete(Result,aPosOrderBy,aPosEnd-aPosOrderBy+1)
  else
    aPosOrderBy:= aPosEnd+1;

  if Length(aOrderBy)>0 then
    UTF8Insert(' ORDER BY '+aOrderBy,Result,aPosOrderBy);
end;

procedure TwfBase.CreateNewDataBaseFireBird(const uHost, uPort, uBaseName, uUserName,
  uPassword: string);
begin
 try
   with fConnection do begin
     Connected   := False;
     LoginPrompt := False;
     Params.Clear;
     //LibraryName := fLibraryName;

     if (Length(uHost)=0) or (Length(uPort)=0) then
       DatabaseName := uBaseName
     else
       DatabaseName := uHost+'/'+uPort+':'+uBaseName;

     Params.Clear;
     Params.Add(Format('user ''%s'' password ''%s'' ',[uUserName, uPassword]));
     Params.Add('page_size 4096');
     Params.Add('default character set UTF8');

     wfLog('Host: '+uHost);
     wfLog('Port: '+uPort);
     wfLog('BaseName: '+uBaseName);

     CreateDB;
     Connected := false;
   end;

   wfLog(rsMessageCreatedDataBaseSucefull);
 except
   raise;
 end;
end;

procedure TwfBase.CreateNewDataBase(const uHost, uPort, uBaseName, uUserName,
  uPassword: string);
begin
    case Engine of
      seFirebird: CreateNewDataBaseFireBird(uHost, uPort, uBaseName, uUserName, uPassword)
      else
        begin
          ShowMessage(Format(rsMessageCreatedDataBaseInterrupted+'%s',['This database engine is not available.']));
          wfLog(Format(rsMessageCreatedDataBaseInterrupted+'%s',['This database engine is not available.']));
        end;
    end;
end;

function TwfBase.WriteWhere(const uSQL: string; aWhere: string;
  const aClearOldWhere: boolean): string;
var
  aPosWhere, aPosWhereRes, aPosWhereEnd, aLengthWhere: Integer;
  aWhereOld: String;
begin
  Result:= UTF8UpperCase(uSQL);
  aPosWhere:= 1;
  aPosWhereRes:= 0;
  aPosWhereEnd:= 0;
  aLengthWhere:= Length(' WHERE ');
  aWhereOld:= '';

  while aPosWhere>0 do begin
    aPosWhere:= UTF8Pos(' WHERE ',Result,aPosWhere+1);

    if (aPosWhere<>0) then
       aPosWhereRes:= aPosWhere;
  end;

  aPosWhereEnd:= UTF8Pos(' GROUP ',Result,aPosWhereRes+1);

  if aPosWhereEnd = 0 then
     aPosWhereEnd:= UTF8Pos(' ORDER ',Result,aPosWhereRes+1);


  if aPosWhereEnd = 0 then aPosWhereEnd:= UTF8Length(Result)+1;

  //wfLog(Format('aPosWhereRes %d | aLengthWhere %d | aPosWhereEnd %d',[aPosWhereRes,aLengthWhere,aPosWhereEnd]));

  if aPosWhereRes>0 then
   begin
    if not aClearOldWhere then
       aWhereOld:= Trim(UTF8Copy(Result, aPosWhereRes+aLengthWhere, aPosWhereEnd-(aPosWhereRes+aLengthWhere)));

    UTF8Delete(Result,aPosWhereRes,aPosWhereEnd-aPosWhereRes)
   end
  else
    aPosWhereRes:= aPosWhereEnd;

  if (Length(aWhereOld)>0) and (Length(aWhere)>0) then
     aWhere:= '('+aWhereOld+') AND ('+aWhere+')'
   else
     if (Length(aWhereOld)>0) and (Length(aWhere)=0) then
       aWhere:= aWhereOld;

  if Length(aWhere)>0 then
    UTF8Insert(' WHERE '+aWhere,Result,aPosWhereRes);

end;

function TwfBase.GetRowsCount(const uSQL: string): int64;
var
  aPosFrom, aPosSelect, aPosFromRes, aPosOrderBy, aPosJoin: PtrInt;
  aSQL: String;
  aData: TwfData;
begin
   aSQL:= uSQL;
   aPosFromRes:= 0;
   aData:= nil;

   aSQL:= UTF8UpperCase(aSQL);
   aPosSelect:= UTF8Pos('SELECT',aSQL,1)+Length('SELECT');

   aPosFrom:=aPosSelect;
   aPosJoin:= UTF8Pos('JOIN',aSQL,1);
   if aPosJoin = 0 then
      aPosJoin:= Length(aSQL);

   while aPosFrom>0 do begin
   aPosFrom:= UTF8Pos('FROM',aSQL,aPosFrom+1);
   if (aPosFrom<>0) and (aPosFrom<aPosJoin) then
      aPosFromRes:= aPosFrom;
  end;


   UTF8Delete(aSQL,aPosSelect,aPosFromRes-aPosSelect);
   UTF8Insert(' COUNT(*) ',aSQL,aPosSelect);

   aPosOrderBy:= UTF8Pos('ORDER BY',aSQL,1);

   UTF8Delete(aSQL,aPosOrderBy,Length(aSQL)-aPosOrderBy+1);

   aData:= GetData(aSQL);
   try
     Result:= aData.Data(0,'COUNT');
   finally
     FreeAndNil(aData);
   end;

end;

function TwfBase.DataToStr(aField: TField; aCSVComma: boolean; aBr: boolean
  ): string;
begin
 case aField.DataType of
   ftSmallInt,
   ftLargeint,
   ftInteger: Result  := IntToStr(aField.AsInteger);
   ftFloat,
   ftCurrency,
   ftBCD: Result      := FloatToStr(aField.AsFloat);
   ftDate: Result     := FormatDateTime('dd.mm.yyyy', aField.AsDateTime);
   ftTime: Result     := FormatDateTime('hh:mm:ss', aField.AsDateTime);
   ftDateTime: Result :=
       FormatDateTime('dd.mm.yyyy hh:mm:ss', aField.AsDateTime);
   ftBoolean: if aField.AsBoolean then
       Result := 'T'
     else
       Result := 'F';
   ftString,
   ftWideString:
   begin
     if aField.AsString<>null then
       Result := aField.AsString
     else
       Result := '';

     if aBr and (Length(Result)>0) then
     begin
       Result := UTF8StringReplace(Result,#13#10,'&br;',[rfReplaceAll]);
       Result := UTF8StringReplace(Result,#10,'&br;',[rfReplaceAll]);
     end;

     if aCSVComma and (Length(Result)>0) then
       Result := '"'+UTF8StringReplace(Result,#34,'&quot;',[rfReplaceAll])+'"';

   end
   else
     WriteStr(Result, aField.DataType);
     //Result := GetEnumName(aField.DataType);
 end;
end;

{@@ ----------------------------------------------------------------------------
  // SQL query to Update Data
  @param  uSQL          SQL Text
  @param  aCommit          Flag of Commit
  @result boolean
-------------------------------------------------------------------------------}
function TwfBase.ExecSQL(const uSQL: string): boolean;
var
  aParams: TwfParams;
begin
  Result:= false;
  aParams:= nil;

  CreateParam(aParams, uSQL, true);
  ExecSQL(uSQL,aParams,'');
  Result:= true;
end;

{@@ ----------------------------------------------------------------------------
  // Simple Insert
  @param  uTable        Table name
  @param  uFields       Array of Fields
  @param  uValues       Array of Value
  @param  uResultField  Result Field name
  @param  aCommit       Flag of Commit
  @result Result        Inserted ID
-------------------------------------------------------------------------------}
function TwfBase.Insert(const uTable: string; const uFields: array of string;
  const uValues: array of variant; const aResultField: string;
  aMatchingFields: string): variant;
var
  aSQL, aFields, aParamNames: String;
  aParams: TwfParams;
  i: Integer;
begin
  if Length(aMatchingFields)>0 then
    aSQL:='UPDATE OR INSERT INTO "%s" (%s) VALUES (%s) MATCHING('+aMatchingFields+') RETURNING '+aResultField
  else
    aSQL:='INSERT INTO "%s" (%s) VALUES (%s) RETURNING '+aResultField;

  aFields:= '';
  aParamNames:='';
  aParams:=nil;

  CreateParam(aParams, '', true);

    for i:=0 to High(uFields) do
    begin
      if i>0 then
       begin
         aFields:= aFields+',';
         aParamNames:= aParamNames+',';
       end;
      aFields:= aFields+uFields[i];
      aParamNames:= aParamNames+':'+uFields[i];
      aParams.CreateParam(GetFieldTypeByVar(uValues[i]),uFields[i],ptInput);
      aParams[i].Value:=  uValues[i];
    end;


    aSQL:= Format(aSQL,[uTable, aFields, aParamNames]);

    Result:= ExecSQL(aSQL, aParams, aResultField);

    if Assigned(onLog) then onLog(self,'InsertID:'+BaseIDToStr(Result));
end;

{@@ ----------------------------------------------------------------------------
  // Simple Update
  @param  uTable        Table name
  @param  uFields       Array of Fields
  @param  uValues       Array of Value
  @param  uWhere        Where string
  @param  uResultField  Result Field name
  @param  aCommit       Flag of Commit
  @result Result        Inserted ID
-------------------------------------------------------------------------------}
function TwfBase.Update(const uTable: string; const uFields: array of string;
  const uValues: array of variant; const uWhere: string): boolean;
var
  aSQL, aFieldsAndValues: String;
  aParams: TwfParams;
  i: Integer;
begin
  Result:= false;
  aSQL:='UPDATE "%s" SET %s ';

  if Length(uWhere)>0 then
    aSQL:= aSQL+' WHERE '+uWhere;

  aFieldsAndValues:='';
  aParams:=nil;

  CreateParam(aParams, '', true);

    for i:=0 to High(uFields) do
    begin
      if i>0 then
         aFieldsAndValues:= aFieldsAndValues+',';

      aFieldsAndValues:= aFieldsAndValues+uFields[i]+'='+':'+uFields[i];
      aParams.CreateParam(GetFieldTypeByVar(uValues[i]),uFields[i],ptInput);
      aParams[i].Value:=  uValues[i];
    end;

    aSQL:= Format(aSQL,[uTable, aFieldsAndValues]);

    Result:= ExecSQL(aSQL, aParams);

end;

{@@ ----------------------------------------------------------------------------
  // Simple Delete
  @param  uTable        Table name
  @param  aBaseID       Base ID
  @result Result        Boolean
-------------------------------------------------------------------------------}
function TwfBase.Delete(const uTable: string; aBaseID: BaseID): boolean;
var
  aSQL: String;
begin
  Result:= false;
  aSQL:='DELETE FROM "%s" WHERE ID= %s ';

  Result:= ExecSQL(Format(aSQL, [uTable, BaseIDToStr(aBaseID)]));
end;

{@@ ----------------------------------------------------------------------------
  // SQL query to retrieve data
  @param  aTransaction  Transaction Object
  @param  aDataSet      DataSet Object
  @result aDataSet      DataSet Object
  @result aTransaction  Transaction Object
-------------------------------------------------------------------------------}
procedure TwfBase.TransactionReadInit(var aTransaction: TwfSQLTransaction; var aDataSet: TwfSQLQuery);
begin
  if LongTransaction then
    aTransaction:= fLongTransaction
  else
    if not Assigned(fTransactionRead) then
     begin
       if Assigned(aDataSet.Transaction) then
         aTransaction:= TwfSQLTransaction(aDataSet.Transaction)
       else
         CreateReadTransaction(aDataSet, aTransaction);
     end
     else
       aTransaction:= fTransactionRead;

  if not aTransaction.Active then aTransaction.StartTransaction
    else
      if Assigned(aDataSet.Transaction) and not LongTransaction and not Assigned(fTransactionRead) then
       aTransaction.Commit;
end;

{@@ ----------------------------------------------------------------------------
  // SQL query to retrieve data
  @param  uSQL          SQL Text
  @param  uParamValue   SQL Params
  @param  aDataSet      DataSet Object
  @result aDataSet      DataSet Object
-------------------------------------------------------------------------------}
function TwfBase.OpenSQL(const uSQL: string; var aParams: TwfParams
  ): TwfSQLQuery;
var
  aTransaction: TwfSQLTransaction;
  i: Integer;
begin
   Result:= nil;
   aTransaction:= nil;

    if not Assigned(fQueryRead) then
       Result:= TwfSQLQuery.Create(self);

   TransactionReadInit(aTransaction, Result);

   with Result do begin
     Close;
     Database:= fConnection;
     Transaction:= aTransaction;
     SQL.Text:= uSQL;
     wfLog(SQL.Text);
     if aParams.Count>0 then
       wfLog('-= Params =-');
     for i:=0 to aParams.Count-1 do
     begin
       Params.ParamByName(aParams[i].Name).Value:= ParamToVar(aParams[i]);
       wfLog(':'+Params[i].Name+' = '+Params[i].AsString);
     end;

     try
       Open;
     finally
       if aParams.FreeAfterUse then FreeAndNil(aParams);
     end;
   end;
end;

{@@ ----------------------------------------------------------------------------
  // SQL query to retrieve data
  @param  uSQL          SQL Text
  @param  aDataSet      DataSet Object
  @result aDataSet      DataSet Object
-------------------------------------------------------------------------------}
function TwfBase.OpenSQL(const uSQL: string): TwfSQLQuery;
var
  aParams: TwfParams;
begin
  aParams:= TwfParams.Create(self);
  try
    Result:= OpenSQL(uSQL, aParams);
  finally
    aParams.Free;
  end;
end;

{@@ ----------------------------------------------------------------------------
  // SQL query Wripper for Format
  @param  uSQL          SQL Text
  @param  Args          array of const
  @param  aDataSet      DataSet Object
  @result aDataSet      DataSet Object
-------------------------------------------------------------------------------}
function TwfBase.OpenSQLFmt(const uSQL: string;
  const Args: array of const): TwfSQLQuery;
begin
  Result:= OpenSQL(Format(uSQL, Args));
end;


{@@ ----------------------------------------------------------------------------
  // Long Transaction Start
-------------------------------------------------------------------------------}
procedure TwfBase.LongTransactionStart;
begin
  if Assigned(fLongTransaction) then
   raise Exception.CreateFmt(rsExceptErrorDefault,['Long Transaction is running!']);

  CreateWriteTransaction(self, fLongTransaction);
end;

{@@ ----------------------------------------------------------------------------
  // Long Transaction Commit
-------------------------------------------------------------------------------}
procedure TwfBase.LongTransactionCommit;
begin
  if not LongTransaction then
    raise Exception.CreateFmt(rsExceptErrorDefault,['Not found Long Transaction!']);
  fLongTransaction.Commit;
  FreeAndNil(fLongTransaction);
end;

{@@ ----------------------------------------------------------------------------
  // Long Transaction Rolback
-------------------------------------------------------------------------------}
procedure TwfBase.LongTransactionRolback;
begin
  if not LongTransaction then
    raise Exception.CreateFmt(rsExceptErrorDefault,['Not found Long Transaction!']);
  fLongTransaction.Rollback;
  FreeAndNil(fLongTransaction);
end;

{@@ ----------------------------------------------------------------------------
  // Convert FieldType to String
  @param    aDataType
  @result   string
-------------------------------------------------------------------------------}
function TwfBase.AsString( const uFieldType : TFieldType) : string;
begin
 case uFieldType of
    ftUnknown: Result := 'ftUnknown';
    ftString: Result := 'ftString';
    ftSmallint: Result := 'ftSmallint';
    ftInteger: Result := 'ftInteger';
    ftWord: Result := 'ftWord';
    ftBoolean: Result := 'ftBoolean';
    ftFloat: Result := 'ftFloat';
    ftCurrency: Result := 'ftCurrency';
    ftBCD: Result := 'ftBCD';
    ftDate: Result := 'ftDate';
    ftTime: Result := 'ftTime';
    ftDateTime: Result := 'ftDateTime';
    ftBytes: Result := 'ftBytes';
    ftVarBytes: Result := 'ftVarBytes';
    ftAutoInc: Result := 'ftAutoInc';
    ftBlob: Result := 'ftBlob';
    ftMemo: Result := 'ftMemo';
    ftGraphic: Result := 'ftGraphic';
    ftFmtMemo: Result := 'ftFmtMemo';
    ftParadoxOle: Result := 'ftParadoxOle';
    ftDBaseOle: Result := 'ftDBaseOle';
    ftTypedBinary: Result := 'ftTypedBinary';
    ftCursor: Result := 'ftCursor';
    ftFixedChar: Result := 'ftFixedChar';
    ftWideString: Result := 'ftWideString';
    ftLargeInt: Result := 'ftLargeInt';
    ftADT: Result := 'ftADT';
    ftArray: Result := 'ftArray';
    ftReference: Result := 'ftReference';
    ftDataSet: Result := 'ftDataSet';
    ftOraBlob: Result := 'ftOraBlob';
    ftOraClob: Result := 'ftOraClob';
    ftVariant: Result := 'ftVariant';
    ftInterface: Result := 'ftInterface';
    ftIDispatch: Result := 'ftIDispatch';
    ftGuid: Result := 'ftGuid';
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Convert ArrayOfString to String
  @param    ArrayOfString
  @result   string
-------------------------------------------------------------------------------}
function TwfBase.AsString(aArr: ArrayOfString): string;
var
  i: Integer;
begin
 Result:='';
 for i:=0 to High(aArr) do
 begin
   if Length(Result)>0 then
     Result:= Result+',';

   Result:= Result+ aArr[i];
 end;
end;

{@@ ----------------------------------------------------------------------------
  // Convert ArrayOfBaseID to String
  @param    ArrayOfBaseID
  @result   string
-------------------------------------------------------------------------------}
function TwfBase.AsString(aArr: ArrayOfBaseID): string;
var
  i: Integer;
begin
 Result:='';
 for i:=0 to High(aArr) do
 begin
   if Length(Result)>0 then
     Result:= Result+',';

   {$IFDEF USEGUID}
     Result:= Result+ aArr[i]
   {$ELSE}
     Result:= Result+ IntToStr(aArr[i])
   {$ENDIF}
 end;
end;

{@@ ----------------------------------------------------------------------------
  // Convert Param to Variant
  @param    aParam
  @result   vatiant
-------------------------------------------------------------------------------}
function TwfBase.ParamToVar(aParam: TParam): Variant;
begin
  case aParam.DataType of
    ftSmallInt,
    ftInteger          : Result:= aParam.AsInteger;
    ftLargeint         : Result:= aParam.AsLargeInt;
    ftFloat,
    ftCurrency,
    ftBCD              : Result:= aParam.AsFloat;
    ftDate,
    ftTime,
    ftDateTime         : Result:= aParam.AsDateTime;
    ftBoolean          : Result:= aParam.AsBoolean;
    ftString,
    ftWideString       : Result := aParam.AsString
    else
      Result:= aParam.DataType;
  end;
end;

{@@ ----------------------------------------------------------------------------
  // Create Param List
  @param    aParam (return)
  @param    aSQL
-------------------------------------------------------------------------------}
procedure TwfBase.CreateParam(var aParams: TwfParams; const aSQL: string;
  const aFreeAfterUse: boolean);
begin
   aParams:= nil;
   aParams:= TwfParams.Create(self, aFreeAfterUse);
   aParams.ParseSQL(aSQL,true);
end;

{@@ ----------------------------------------------------------------------------
  // Transaction Commit
  @param    aTransaction (return)
-------------------------------------------------------------------------------}
procedure TwfBase.TransactionCommit(aTransaction: TwfSQLTransaction);
begin
  if LongTransaction then exit; //=>
  aTransaction.Commit;
  FreeAndNil(aTransaction);
end;

{@@ ----------------------------------------------------------------------------
  // Transaction FolBack
  @param    aTransaction (return)
-------------------------------------------------------------------------------}
procedure TwfBase.TransactionRolback(aTransaction: TwfSQLTransaction);
begin
  aTransaction.Rollback;
  FreeAndNil(aTransaction);
end;

end.
