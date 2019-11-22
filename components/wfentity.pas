{
This file is part of wfFrameWork.

 -= Entity =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfEntity;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, db, sqldb, LResources, Forms, Controls, Graphics, Dialogs,
  LazUTF8, wfTypes, wfFunc, wfResourceStrings, wfClasses, wfBase, PropEdits;

type

  { TwfEntity }

  TwfEntity = class(TComponent)
  private
    faSQLPresets: TwfEntitySQLPresets;
    fDescription: TStrings;
    fGridColumnsString: TStrings;
    fonLog: TTextEvent;
    fonProgress: TProgressEvent;
    fonProgressInit: TProgressInitEvent;
    fonProgressMarquee: TProgressMarqueeEvent;
    fonStatus: TTextEvent;
    fSQLCreate: TStrings;
    fSQLDrop: TStrings;
    fSQLGetList: TStrings;
    fSQLGetListFull: TStrings;
    fSQLGetListShort: TStrings;
    fSQLItemDel: TStrings;
    fSQLItemGet: TStrings;
    fSQLItemNew: TStrings;
    fSQLItems: TwfDesignSQLItems;
    fSQLItemUpdate: TStrings;
    fSQLTreeDragNode: TStrings;
    fSQLTreeGetRoot: TStrings;
    fTableName: string;
    fBase: TwfBase;
    procedure AppendText(var aStrings: TStrings; const aText: string);
    function GetBase: TwfBase;
    function GetTableName: string;
    procedure SetBase(aValue: TwfBase);
    procedure SetGridColumnsString(aValue: TStrings);
    procedure SetLog(aValue: string);
    procedure SetProgress(aValue: integer);
    procedure SetProgressMarquee(aValue: boolean);
    procedure SetSQL(aType: TwfEntitySQLType; AValue: string);
    procedure SetSQLListPresets();
    procedure SetSQLPresets(aValue: TwfEntitySQLPresets);
    procedure SetDescription(aValue: TStrings);
    procedure SetSQLCreate(aValue: TStrings);
    procedure SetSQLDrop(aValue: TStrings);
    procedure SetSQLGetList(aValue: TStrings);
    procedure SetSQLGetListFull(aValue: TStrings);
    procedure SetSQLGetListShort(aValue: TStrings);
    procedure SetSQLItemDel(aValue: TStrings);
    procedure SetSQLItemGet(aValue: TStrings);
    procedure SetSQLItemNew(aValue: TStrings);
    procedure SetSQLItemUpdate(aValue: TStrings);
    procedure SetSQLCreateTable();
    procedure SetSQLTreeDragNode(aValue: TStrings);
    procedure SetSQLTreeGetRoot(aValue: TStrings);
    procedure SetSQLTreePresets();
    procedure SetStatus(aValue: string);
    procedure SetStatusLog(aValue: string);

  protected
    function GetScript(aType: TwfEntityScriptType): TStrings; virtual;
    function GetSQL(aType: TwfEntitySQLType): string; virtual;
    function GetEngine():TwfSQLEngine; virtual;

    procedure ProgressInit(const aMax, aStep: integer);
    procedure ProgressStep;

    property Progress: integer write SetProgress;
    property ProgressMarquee:boolean write SetProgressMarquee;
    property Status:string write SetStatus;
    property Log:string write SetLog;
    property StatusLog:string write SetStatusLog;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SQL[aType: TwfEntitySQLType]:string read GetSQL write SetSQL;
    property Script[aType: TwfEntityScriptType]:TStrings read GetScript;
  published

    //Database connection
    property aBase: TwfBase read GetBase write SetBase;
    // Description of the entity
    property Description: TStrings read fDescription write SetDescription;
    // The table name in the database
    property TableName: string read GetTableName write fTableName;

    property SQLCreate: TStrings read fSQLCreate write SetSQLCreate;

    property SQLTreeGetRoot: TStrings read fSQLTreeGetRoot write SetSQLTreeGetRoot;
    property SQLTreeDragNode: TStrings read fSQLTreeDragNode write SetSQLTreeDragNode;

    property SQLGetList: TStrings read fSQLGetList write SetSQLGetList;
    property SQLGetListShort: TStrings read fSQLGetListShort write SetSQLGetListShort;
    property SQLGetListFull: TStrings read fSQLGetListFull write SetSQLGetListFull;

    property SQLItemGet: TStrings read fSQLItemGet write SetSQLItemGet;

    property SQLItemNew: TStrings read fSQLItemNew write SetSQLItemNew;
    property SQLItemUpdate: TStrings read fSQLItemUpdate write SetSQLItemUpdate;
    property SQLItemDel: TStrings read fSQLItemDel write SetSQLItemDel;

    property SQLDrop: TStrings read fSQLDrop write SetSQLDrop;

    property SQLItems: TwfDesignSQLItems read fSQLItems write fSQLItems;

    //Set SQL presets
    property aSQLPresets: TwfEntitySQLPresets read faSQLPresets write SetSQLPresets default spNone;

    //Example: ID{ID}<70>[##0.00]|1; NAME{NAME}<840>|1
    property GridColumnsString: TStrings read fGridColumnsString write SetGridColumnsString;
    {$IFDEF NOT USEGUID}
    property SQLSequenceName: string read fSQLSequenceName write fSQLSequenceName;
    {$ENDIF}

      property onLog: TTextEvent read fonLog write fonLog;
      property onStatus: TTextEvent read fonStatus write fonStatus;
      property onProgress: TProgressEvent read fonProgress write fonProgress;
      property onProgressInit: TProgressInitEvent read fonProgressInit write fonProgressInit;
      property onProgressMarquee: TProgressMarqueeEvent read fonProgressMarquee write fonProgressMarquee;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfentity_icon.lrs}
  RegisterComponents('WF',[TwfEntity]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLCreate', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLDrop', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLGetList', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLGetListFull', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLGetListShort', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLItemDel', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLItemGet', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLItemNew', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLItemUpdate', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'SQLTreeDragNode', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfEntity, 'fSQLTreeGetRoot', TSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfDesignSQLItem, 'SQL', TSQLPropertyEditor);
end;


{ TwfEntity }

procedure TwfEntity.SetDescription(aValue: TStrings);
begin
  fDescription.Assign(aValue);
end;

procedure TwfEntity.SetSQLCreate(aValue: TStrings);
begin
  fSQLCreate.Assign(aValue);
end;

procedure TwfEntity.SetSQLDrop(aValue: TStrings);
begin
  fSQLDrop.Assign(aValue);
end;

procedure TwfEntity.SetSQLGetList(aValue: TStrings);
begin
  fSQLGetList.Assign(aValue);
end;

procedure TwfEntity.SetSQLGetListFull(aValue: TStrings);
begin
  fSQLGetListFull.Assign(aValue);
end;

procedure TwfEntity.SetSQLGetListShort(aValue: TStrings);
begin
  fSQLGetListShort.Assign(aValue);
end;

procedure TwfEntity.SetSQLItemDel(aValue: TStrings);
begin
  fSQLItemDel.Assign(aValue);
end;

procedure TwfEntity.SetSQLItemGet(aValue: TStrings);
begin
  fSQLItemGet.Assign(aValue);
end;

procedure TwfEntity.SetSQLItemNew(aValue: TStrings);
begin
  fSQLItemNew.Assign(aValue);
end;

procedure TwfEntity.SetSQLItemUpdate(aValue: TStrings);
begin
  fSQLItemUpdate.Assign(aValue);
end;

procedure TwfEntity.SetSQLTreeGetRoot(aValue: TStrings);
begin
  fSQLTreeGetRoot.Assign(aValue);
end;

function TwfEntity.GetTableName: string;
begin
  if Length(fTableName)=0 then
    Result:= UTF8UpperCase(self.Name)
  else
    Result:= fTableName;
end;

function TwfEntity.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

procedure TwfEntity.SetBase(aValue: TwfBase);
begin
  fBase:=aValue;
end;

procedure TwfEntity.SetGridColumnsString(aValue: TStrings);
begin
  fGridColumnsString.Assign(aValue);
end;

procedure TwfEntity.SetLog(aValue: string);
begin
  if Assigned(onLog) then
    onLog(self, aValue);
end;

procedure TwfEntity.SetProgress(aValue: integer);
begin
  if Assigned(onProgress) then
     onProgress(self, aValue);
end;

procedure TwfEntity.SetProgressMarquee(aValue: boolean);
begin
  if Assigned(onProgressMarquee) then
     onProgressMarquee(self, aValue);
end;

procedure TwfEntity.SetSQL(aType: TwfEntitySQLType; AValue: string);
begin
  case aType of
    estList: SQLGetList.Text:= AValue;
    estListFull: SQLGetListFull.Text:= AValue;
    estListShort: SQLGetListShort.Text:= AValue;
    estItemDel: SQLItemDel.Text:= AValue;
    estItemGet: SQLItemGet.Text:= AValue;
    estItemNew: SQLItemNew.Text:= AValue;
    estItemUpdate: SQLItemUpdate.Text:= AValue;
    estTreeGetRoot: SQLTreeGetRoot.Text:= AValue;
    estTreeDragNode: SQLTreeDragNode.Text:= AValue;
  end;
end;

function TwfEntity.GetSQL(aType: TwfEntitySQLType): string;
begin
  Result:= '';
  case aType of
    estList: Result:= SQLGetList.Text;
    estListFull: Result:= SQLGetListFull.Text;
    estListShort: Result:= SQLGetListShort.Text;
    estItemDel: Result:= SQLItemDel.Text;
    estItemGet: Result:= SQLItemGet.Text;
    estItemNew: Result:= SQLItemNew.Text;
    estItemUpdate: Result:= SQLItemUpdate.Text;
    estTreeGetRoot: Result:= SQLTreeGetRoot.Text;
    estTreeDragNode: Result:= SQLTreeDragNode.Text;
  end;
end;

function TwfEntity.GetEngine(): TwfSQLEngine;
begin
  if IsEmpty(fBase) then
    Result:= seUnknown
  else
    Result:= fBase.Engine;
end;

function TwfEntity.GetScript(aType: TwfEntityScriptType): TStrings;
begin
case aType of
  esctCreate : Result:= SQLCreate;
  esctDrop   : Result:= SQLDrop;
end;
end;

procedure TwfEntity.SetSQLCreateTable();
begin
  case GetEngine of
    seFirebird: begin
      {$IFDEF USEGUID}
            AppendText(fSQLCreate,Format('CREATE TABLE %s ( '+wfLE
               +'     ID        GUID NOT NULL /* GUID = CHAR(32) NOT NULL */, '+wfLE
               +'     IDPARENT  GUID /* GUID = CHAR(32) NOT NULL */, '+wfLE
               +'     NAME      VARCHAR(500) '+wfLE
               +' );',[TableName]));
            AppendText(fSQLCreate,'COMMIT;');
            AppendText(fSQLCreate,Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
            AppendText(fSQLCreate,'SET TERM ^ ;');
            AppendText(fSQLCreate,Format('CREATE OR ALTER TRIGGER %s_BI0 FOR %s '+wfLE
               +' ACTIVE BEFORE INSERT POSITION 0 '+wfLE
               +' AS '+wfLE
               +' begin '+wfLE
               +'    IF (NEW.ID IS NULL) THEN '+wfLE
               +'     NEW.ID = REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'',''''); '+wfLE
               +' end '+wfLE
               +' ^',[TableName,TableName]));
            AppendText(fSQLCreate,'SET TERM ; ^');
         {$ELSE}
             AppendText(fSQLCreate,Format('CREATE TABLE %s ( '+wfLE
                +'     ID        BIGINT NOT NULL, '+wfLE
                +'     IDPARENT  BIGINT, '+wfLE
                +'     NAME      VARCHAR(500) '+wfLE
                +' );',[TableName]));
             AppendText(fSQLCreate,'COMMIT;');
             AppendText(fSQLCreate,Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
             AppendText(fSQLCreate,Format('CREATE SEQUENCE GEN_%s_ID START WITH 0 INCREMENT BY 1;',[TableName]));
             AppendText(fSQLCreate,'SET TERM ^ ;');
             AppendText(fSQLCreate,Format('CREATE OR ALTER TRIGGER %s_BI FOR %s'+wfLE
                +' ACTIVE BEFORE INSERT POSITION 0'+wfLE
                +' AS'+wfLE
                +' BEGIN'+wfLE
                +'   IF (NEW.ID IS NULL) THEN'+wfLE
                +'     NEW.ID = (SELECT SALT FROM WF_GET_DEPARTMENTSALT)+GEN_ID(GEN_%s_ID,1);'+wfLE
                +' END '+wfLE
                +' ^',[TableName,TableName,TableName]));
             AppendText(fSQLCreate,'SET TERM ; ^');

         {$ENDIF}
    end;

    { TODO -owofs : Tree. To make PostgreSQL to create a table by using a GUID. }
    sePostgreSQL: begin
      {$IFDEF USEGUID}
          //CREATE EXTENSION "uuid-ossp";
          AppendText(fSQLCreate,Format('CREATE TABLE %s ( '+wfLE
             +'     ID        uuid DEFAULT uuid_generate_v4 () PRIMARY KEY, '+wfLE
             +'     IDPARENT  BIGINT, '+wfLE
             +'     NAME      CHARACTER VARYING(500) '+wfLE
             +' );',[TableName]));
      {$ELSE}
          AppendText(fSQLCreate,Format('CREATE TABLE %s ( '+wfLE
             +'     ID        BIGSERIAL PRIMARY KEY, '+wfLE
             +'     IDPARENT  BIGINT, '+wfLE
             +'     NAME      CHARACTER VARYING(500) '+wfLE
             +' );',[TableName]));
      {$ENDIF}
    end;
    else
      fSQLCreate.Append('');
  end;
end;

procedure TwfEntity.AppendText(var aStrings:TStrings; const aText: string);
begin
  if fSQLCreate.Count>0 then
    fSQLCreate.Text := fSQLCreate.Text + wfLE;

  fSQLCreate.Text:= fSQLCreate.Text + aText;
end;

procedure TwfEntity.SetSQLTreeDragNode(aValue: TStrings);
begin
  fSQLTreeDragNode.Assign(aValue);
end;

procedure TwfEntity.SetSQLTreePresets();
begin
  if fSQLCreate.Count = 0 then
      SetSQLCreateTable();

   if fSQLGetList.Count = 0 then
      fSQLGetList.Text:= Format('SELECT '+wfLE+'T1.ID, T1.IDPARENT, T1.NAME, '+wfLE+'(SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT '+wfLE+'FROM %s T1 '+wfLE+'ORDER BY T1.IDPARENT, T1.NAME',[TableName, TableName]);

   if fSQLGetListFull.Count = 0 then
      fSQLGetListFull.Text:= Format('SELECT * FROM %s',[TableName]);

   if fSQLGetListShort.Count = 0 then
      fSQLGetListShort.Text:= Format('SELECT * FROM %s',[TableName]);

   if fSQLItemDel.Count = 0 then
      fSQLItemDel.Text:= Format('DELETE FROM %s WHERE ID=:ID',[TableName]);

   if fSQLItemGet.Count = 0 then
      fSQLItemGet.Text:= Format('SELECT * FROM %s WHERE ID=:ID',[TableName]);

   if fSQLItemNew.Count = 0 then
      fSQLItemNew.Text:= Format('INSERT INTO %s '+wfLE+'(IDPARENT, NAME) '+wfLE+'VALUES (:IDPARENT, :NAME) RETURNING ID',[TableName]);

   if fSQLItemUpdate.Count = 0 then
      fSQLItemUpdate.Text:= Format('UPDATE %s '+wfLE+'SET IDPARENT=:IDPARENT, NAME=:NAME '+wfLE+'WHERE ID=:ID RETURNING ID',[TableName]);

   if fSQLTreeGetRoot.Count = 0 then
   {$IFDEF USEGUID}
      fSQLTreeGetRoot.Text:= Format('SELECT T1.ID, T1.IDPARENT, T1.NAME, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT '+wfLE+'FROM %s T1 WHERE T1.IDPARENT=''''',[TableName,TableName]);
   {$ELSE}
      fSQLTreeGetRoot.Text:= Format('SELECT '+wfLE+'T1.ID, T1.IDPARENT, T1.NAME, '+wfLE+'(SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT '+wfLE+'FROM %s T1 '+wfLE+'WHERE T1.IDPARENT=0',[TableName,TableName]);
   {$ENDIF}

   if fSQLTreeDragNode.Count = 0 then
     fSQLTreeDragNode.Text:= Format('UPDATE '+wfLE+'%s SET %s=:%s WHERE ID=:ID RETURNING ID',[fTableName,'%s','%s']);

   if fSQLDrop.Count = 0 then
      fSQLDrop.Text:= Format('DROP TABLE %s;',[TableName]);

   {$IFDEF NOT USEGUID}
   fSQLSequenceName:= Format('GEN_%s_ID',[TableName]);
   {$ENDIF}
end;

procedure TwfEntity.SetStatus(aValue: string);
begin
   if Assigned(onStatus) then
      onStatus(self, aValue);
end;

procedure TwfEntity.SetStatusLog(aValue: string);
begin
  Status:= aValue;
  Log:= aValue;
end;


procedure TwfEntity.ProgressInit(const aMax, aStep: integer);
begin
   if Assigned(onProgressInit) then
      onProgressInit(self, aMax, aStep);
end;

procedure TwfEntity.ProgressStep;
begin
  Progress:= -1;
end;

procedure TwfEntity.SetSQLListPresets();
begin
   if fSQLCreate.Count = 0 then
      SetSQLCreateTable();

   if fSQLGetList.Count = 0 then
      fSQLGetList.Text:= Format('SELECT ID, IDPARENT, NAME FROM %s',[TableName]);

   if fSQLGetListFull.Count = 0 then
      fSQLGetListFull.Text:= Format('SELECT * FROM %s',[TableName]);

   if fSQLGetListShort.Count = 0 then
      fSQLGetListShort.Text:= Format('SELECT * FROM %s',[TableName]);

   if fSQLItemDel.Count = 0 then
      fSQLItemDel.Text:= Format('DELETE FROM %s WHERE ID=:ID',[TableName]);

   if fSQLItemGet.Count = 0 then
      fSQLItemGet.Text:= Format('SELECT * FROM %s WHERE ID=:ID',[TableName]);

   if fSQLItemNew.Count = 0 then
      fSQLItemNew.Text:= Format('INSERT INTO %s '+wfLE+'(IDPARENT, NAME) '+wfLE+'VALUES (:IDPARENT, :NAME) RETURNING ID',[TableName]);

   if fSQLItemUpdate.Count = 0 then
      fSQLItemUpdate.Text:= Format('UPDATE '+wfLE+'%s '+wfLE+'SET IDPARENT=:IDPARENT, NAME=:NAME '+wfLE+'WHERE ID=:ID RETURNING ID',[TableName]);

   if fSQLTreeGetRoot.Count = 0 then
      fSQLTreeGetRoot.Text:= '';

   if fSQLDrop.Count = 0 then
      fSQLDrop.Text:= Format('DROP TABLE %s;',[TableName]);
end;

procedure TwfEntity.SetSQLPresets(aValue: TwfEntitySQLPresets);
begin
  if faSQLPresets = aValue then exit;


  faSQLPresets:=aValue;

case aValue of
  spNone:
    begin
      fDescription.Clear;
      fSQLCreate.Clear;
      fSQLGetList.Clear;
      fSQLGetListFull.Clear;
      fSQLGetListShort.Clear;
      fSQLItemDel.Clear;
      fSQLItemGet.Clear;
      fSQLItemNew.Clear;
      fSQLItemUpdate.Clear;
      fSQLTreeGetRoot.Clear;
      fSQLDrop.Clear;
    end;
  spTree: SetSQLTreePresets();
  spList: SetSQLListPresets();
end;
end;

constructor TwfEntity.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDescription:= TStringList.Create;
  fSQLCreate:= TStringList.Create;
  fSQLGetList:= TStringList.Create;
  fSQLGetListFull:= TStringList.Create;
  fSQLGetListShort:= TStringList.Create;
  fSQLItemDel:= TStringList.Create;
  fSQLItemGet:= TStringList.Create;
  fSQLItemNew:= TStringList.Create;
  fSQLItemUpdate:= TStringList.Create;
  fSQLTreeGetRoot:= TStringList.Create;
  fSQLTreeDragNode:= TStringList.Create;
  fSQLDrop:= TStringList.Create;
  fGridColumnsString:= TStringList.Create;
  fSQLItems:= TwfDesignSQLItems.Create(self, TwfDesignSQLItem);
end;

destructor TwfEntity.Destroy;
begin
  FreeAndNil(fDescription);
  FreeAndNil(fSQLCreate);
  FreeAndNil(fSQLGetList);
  FreeAndNil(fSQLGetListFull);
  FreeAndNil(fSQLGetListShort);
  FreeAndNil(fSQLItemDel);
  FreeAndNil(fSQLItemGet);
  FreeAndNil(fSQLItemNew);
  FreeAndNil(fSQLItemUpdate);
  FreeAndNil(fSQLTreeGetRoot);
  FreeAndNil(fSQLTreeDragNode);
  FreeAndNil(fSQLDrop);
  FreeAndNil(fGridColumnsString);
  FreeAndNil(fSQLItems);
  inherited Destroy;
end;


{ TwfEntity }

end.
