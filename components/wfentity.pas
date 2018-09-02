{
This file is part of wfFrameWork.

 -= Entity =-

 wofs(c)2017-2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfEntity;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, db, sqldb, LResources, Forms, Controls, Graphics, Dialogs,
  LazUTF8, wfTypes, wfFunc, PropEdits;

type

  //  { TSQLStringsPropertyEditor }
  //
  //TSQLStringsPropertyEditor = class(TStringsPropertyEditor)
  //private
  //  procedure EditSQL;
  //public
  //  procedure Edit; override;
  //  function CreateEnhancedDlg(s: TStrings): TSQLStringsPropertyEditorDlg; virtual;
  //  function GetAttributes: TPropertyAttributes; override;
  //end;

  { TwfEntity }

  TwfEntity = class(TComponent)
  private
    faSQLPresets: TwfEntitySQLPresets;
    fDescription: TStrings;
    fGridColumnsString: TStrings;
    fSQLCreate: TStrings;
    fSQLDrop: TStrings;
    fSQLGetList: TStrings;
    fSQLGetListFull: TStrings;
    fSQLGetListShort: TStrings;
    fSQLItemDel: TStrings;
    fSQLItemGet: TStrings;
    fSQLItemNew: TStrings;
    fSQLItemUpdate: TStrings;
    fSQLSequenceName: string;
    fSQLTreeGetRoot: TStrings;
    fTableName: string;
    function GetScript(aType: TwfEntityScriptType): TStrings;
    function GetSQL(aType: TwfEntitySQLType): string;
    function GetTableName: string;
    procedure SetGridColumnsString(aValue: TStrings);
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
    procedure SetSQLTreeGetRoot(aValue: TStrings);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SQL[aType: TwfEntitySQLType]:string read GetSQL;
    property Script[aType: TwfEntityScriptType]:TStrings read GetScript;
  published
    // Description of the entity
    property Description: TStrings read fDescription write SetDescription;
    // The table name in the database
    property TableName: string read GetTableName write fTableName;

    property SQLCreate: TStrings read fSQLCreate write SetSQLCreate;

    property SQLTreeGetRoot: TStrings read fSQLTreeGetRoot write SetSQLTreeGetRoot;

    property SQLGetList: TStrings read fSQLGetList write SetSQLGetList;
    property SQLGetListShort: TStrings read fSQLGetListShort write SetSQLGetListShort;
    property SQLGetListFull: TStrings read fSQLGetListFull write SetSQLGetListFull;

    property SQLItemGet: TStrings read fSQLItemGet write SetSQLItemGet;

    property SQLItemNew: TStrings read fSQLItemNew write SetSQLItemNew;
    property SQLItemUpdate: TStrings read fSQLItemUpdate write SetSQLItemUpdate;
    property SQLItemDel: TStrings read fSQLItemDel write SetSQLItemDel;

    property SQLDrop: TStrings read fSQLDrop write SetSQLDrop;

    //Set SQL presets
    property aSQLPresets: TwfEntitySQLPresets read faSQLPresets write SetSQLPresets default spNone;

    //Example: ID{ID}<70>[##0.00]|1; NAME{NAME}<840>|1
    property GridColumnsString: TStrings read fGridColumnsString write SetGridColumnsString;
    {$IFDEF NOT USEGUID}
    property SQLSequenceName: string read fSQLSequenceName write fSQLSequenceName;
    {$ENDIF}
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfentity_icon.lrs}
  RegisterComponents('WF',[TwfEntity]);
  //RegisterPropertyEditor(TStrings.ClassInfo, TwfEntity,  'SQLCreate', TStringsPropertyEditor);
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

procedure TwfEntity.SetGridColumnsString(aValue: TStrings);
begin
  fGridColumnsString.Assign(aValue);
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
  end;
end;

function TwfEntity.GetScript(aType: TwfEntityScriptType): TStrings;
begin
case aType of
  esctCreate : Result:= SQLCreate;
  esctDrop   : Result:= SQLDrop;
end;
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
  spTree:
    begin
      if fSQLCreate.Count = 0 then
        begin
        {$IFDEF USEGUID}
           fSQLCreate.Append(Format('CREATE TABLE %s ( '+wfLineEnding
              +'     ID        GUID NOT NULL /* GUID = CHAR(32) NOT NULL */, '+wfLineEnding
              +'     IDPARENT  GUID /* GUID = CHAR(32) NOT NULL */, '+wfLineEnding
              +'     NAME      VARCHAR(500) '+wfLineEnding
              +' );',[TableName]));
           fSQLCreate.Append('COMMIT;');
           fSQLCreate.Append(Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
           fSQLCreate.Append('SET TERM ^ ;');
           fSQLCreate.Append(Format('CREATE OR ALTER TRIGGER %s_BI0 FOR %s '+wfLineEnding
              +' ACTIVE BEFORE INSERT POSITION 0 '+wfLineEnding
              +' AS '+wfLineEnding
              +' begin '+wfLineEnding
              +'    IF (NEW.ID IS NULL) THEN '+wfLineEnding
              +'     NEW.ID = REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'',''''); '+wfLineEnding
              +' end '+wfLineEnding
              +' ^',[TableName,TableName]));
           fSQLCreate.Append('SET TERM ; ^');
        {$ELSE}
            fSQLCreate.Append(Format('CREATE TABLE %s ( '+wfLineEnding
               +'     ID        BIGINT NOT NULL, '+wfLineEnding
               +'     IDPARENT  BIGINT, '+wfLineEnding
               +'     NAME      VARCHAR(500) '+wfLineEnding
               +' );',[TableName]));
            fSQLCreate.Append('COMMIT;');
            fSQLCreate.Append(Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
            fSQLCreate.Append(Format('CREATE SEQUENCE GEN_%s_ID START WITH 0 INCREMENT BY 1;',[TableName]));
            fSQLCreate.Append('SET TERM ^ ;');
            fSQLCreate.Append(Format('CREATE OR ALTER TRIGGER %s_BI FOR %s'+wfLineEnding
               +' ACTIVE BEFORE INSERT POSITION 0'+wfLineEnding
               +' AS'+wfLineEnding
               +' BEGIN'+wfLineEnding
               +'   IF (NEW.ID IS NULL) THEN'+wfLineEnding
               +'     NEW.ID = (SELECT SALT FROM WF_GET_DEPARTMENTSALT)+GEN_ID(GEN_%s_ID,1);'+wfLineEnding
               +' END '+wfLineEnding
               +' ^',[TableName,TableName,TableName]));
            fSQLCreate.Append('SET TERM ; ^');

        {$ENDIF}
        end;

      if fSQLGetList.Count = 0 then
         fSQLGetList.Text:= Format('SELECT T1.ID, T1.IDPARENT, T1.NAME, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 ORDER BY T1.IDPARENT, T1.NAME',[TableName, TableName]);

      if fSQLGetListFull.Count = 0 then
         fSQLGetListFull.Text:= Format('SELECT * FROM %s',[TableName]);

      if fSQLGetListShort.Count = 0 then
         fSQLGetListShort.Text:= Format('SELECT * FROM %s',[TableName]);

      if fSQLItemDel.Count = 0 then
         fSQLItemDel.Text:= Format('DELETE FROM %s WHERE ID=:ID',[TableName]);

      if fSQLItemGet.Count = 0 then
         fSQLItemGet.Text:= Format('SELECT * FROM %s WHERE ID=:ID',[TableName]);

      if fSQLItemNew.Count = 0 then
         fSQLItemNew.Text:= Format('INSERT INTO %s (IDPARENT, NAME) VALUES (:IDPARENT, :NAME) RETURNING ID',[TableName]);

      if fSQLItemUpdate.Count = 0 then
         fSQLItemUpdate.Text:= Format('UPDATE %s SET IDPARENT=:IDPARENT, NAME=:NAME WHERE ID=:ID RETURNING ID',[TableName]);

      if fSQLTreeGetRoot.Count = 0 then
      {$IFDEF USEGUID}
         fSQLTreeGetRoot.Text:= Format('SELECT T1.ID, T1.IDPARENT, T1.NAME, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 WHERE T1.IDPARENT=''''',[TableName,TableName]);
      {$ELSE}
         fSQLTreeGetRoot.Text:= Format('SELECT T1.ID, T1.IDPARENT, T1.NAME, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 WHERE T1.IDPARENT=0',[TableName,TableName]);
      {$ENDIF}

      if fSQLDrop.Count = 0 then
         fSQLDrop.Text:= Format('DROP TABLE %s;',[TableName]);

      {$IFDEF NOT USEGUID}
      fSQLSequenceName:= Format('GEN_%s_ID',[TableName]);
      {$ENDIF}
    end;
  spList:
    begin
      if fSQLCreate.Count = 0 then
        begin
        {$IFDEF USEGUID}
           fSQLCreate.Append(Format('CREATE TABLE %s ( '+wfLineEnding
              +'     ID        GUID NOT NULL /* GUID = CHAR(32) NOT NULL */, '+wfLineEnding
              +'     IDPARENT  GUID /* GUID = CHAR(32) NOT NULL */, '+wfLineEnding
              +'     NAME      VARCHAR(500) '+wfLineEnding
              +' );',[TableName]));
           fSQLCreate.Append(Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
           fSQLCreate.Append('SET TERM ^ ;');
           fSQLCreate.Append(Format('CREATE OR ALTER TRIGGER %s_BI0 FOR %s '+wfLineEnding
              +' ACTIVE BEFORE INSERT POSITION 0 '+wfLineEnding
              +' AS '+wfLineEnding
              +' begin '+wfLineEnding
              +'    IF (NEW.ID IS NULL) THEN '+wfLineEnding
              +'     NEW.ID = REPLACE(UUID_TO_CHAR(GEN_UUID()),''-'',''''); '+wfLineEnding
              +' end '+wfLineEnding
              +' ^',[TableName,TableName]));
           fSQLCreate.Append('SET TERM ; ^');
        {$ELSE}
            fSQLCreate.Append(Format('CREATE TABLE %s ( '+wfLineEnding
               +'     ID        BIGINT NOT NULL, '+wfLineEnding
               +'     IDPARENT  BIGINT, '+wfLineEnding
               +'     NAME      VARCHAR(500) '+wfLineEnding
               +' );',[TableName]));
            fSQLCreate.Append(Format('ALTER TABLE %s ADD CONSTRAINT PK_%s PRIMARY KEY (ID);',[TableName,TableName]));
            fSQLCreate.Append(Format('CREATE SEQUENCE GEN_%s_ID START WITH 0 INCREMENT BY 1;',[TableName]));
            fSQLCreate.Append('SET TERM ^ ;');
            fSQLCreate.Append(Format('CREATE OR ALTER TRIGGER %s_BI FOR %s'+wfLineEnding
               +' ACTIVE BEFORE INSERT POSITION 0'+wfLineEnding
               +' AS'+wfLineEnding
               +' BEGIN'+wfLineEnding
               +'   IF (NEW.ID IS NULL) THEN'+wfLineEnding
               +'     NEW.ID = (SELECT SALT FROM WF_GET_DEPARTMENTSALT)+GEN_ID(GEN_%s_ID,1);'+wfLineEnding
               +' END '+wfLineEnding
               +' ^',[TableName,TableName,TableName]));
            fSQLCreate.Append('SET TERM ; ^');
        {$ENDIF}
        end;

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
         fSQLItemNew.Text:= Format('INSERT INTO %s (IDPARENT, NAME) VALUES (:IDPARENT, :NAME) RETURNING ID',[TableName]);

      if fSQLItemUpdate.Count = 0 then
         fSQLItemUpdate.Text:= Format('UPDATE %s SET IDPARENT=:IDPARENT, NAME=:NAME WHERE ID=:ID RETURNING ID',[TableName]);

      if fSQLTreeGetRoot.Count = 0 then
         fSQLTreeGetRoot.Text:= '';

      if fSQLDrop.Count = 0 then
         fSQLDrop.Text:= Format('DROP TABLE %s;',[TableName]);
    end;
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
  fSQLDrop:= TStringList.Create;
  fGridColumnsString:= TStringList.Create;
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
  FreeAndNil(fSQLDrop);
  FreeAndNil(fGridColumnsString);
  inherited Destroy;
end;


{ TwfEntity }

end.
