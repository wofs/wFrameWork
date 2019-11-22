{
This file is part of wfFrameWork.

 -= Settings =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfSettings;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfFunc,
  wfTypes, wfClasses, wfParamsU, wfBase;

type

  { TwfSettingItem }

  TwfSettingItem = class(TCollectionItem)
  protected
    function GetDisplayName: string; override;
  private
    fDefaultValue: variant;
    fDescription: string;
    fName: string;

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

  published
    property Name: string read fName write fName;
    property Description: string read fDescription write fDescription;
    property DefaultValue: variant read fDefaultValue write fDefaultValue;
  end;

  TwfSettingItems = class(TOwnedCollection);

  { TwfSettings }

  TwfSettings = class(TComponent)
  private
  const
    uSQLRead = 'SELECT FVALUE FROM %s WHERE NAME=%s';
    uSQLWrite = 'UPDATE OR INSERT INTO %s (NAME, FVALUE) VALUES(%s, :FVALUE) MATCHING (NAME)';
    uSQLCreate = 'CREATE TABLE %s ('
          +' NAME VARCHAR(50),'
          +' FVALUE VARCHAR(255))';
    uSQLDrop = 'DROP TABLE %s';
  var
    fBase: TwfBase;
    fItems: TwfSettingItems;
    fPrefix: string;
    fTableName: string;
  function GetInitialized: boolean;
    function GetItem(aName: string): variant;
    procedure SetInitialized(aValue: boolean);
    procedure SetItem(aName: string; aValue: variant);

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Item[aName:string]:variant read GetItem write SetItem;

    function CreateTable: boolean;
    function DropTable: boolean;
  published
    property TableName: string read fTableName write fTableName;
    property Prefix: string read fPrefix write fPrefix;
    property Items: TwfSettingItems read fItems write fItems;
    property Base: TwfBase read fBase write fBase;

    property Initialized: boolean read GetInitialized write SetInitialized;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfsettings_icon.lrs}
  RegisterComponents('WF',[TwfSettings]);
end;

{ TwfSettings }

function TwfSettings.GetItem(aName: string): variant;
var
  aData: TwfData;
  aSQL: string;
begin
  aData:= nil;
  aSQL:= Format(uSQLRead,[fTableName, QuotedStr(fPrefix+aName)]);

  aData:= Base.GetData(aSQL);
  try
    Result:= aData.Data(0,'FVALUE');
  finally
    FreeAndNil(aData);
  end;
end;

function TwfSettings.GetInitialized: boolean;
begin
  Result:= false;
  if not Assigned(Base) then exit;

  Result:= Base.TableIsExists(fTableName);
end;

procedure TwfSettings.SetInitialized(aValue: boolean);
var
  aItem: TwfSettingItem;
  i: Integer;
begin
  if not Assigned(Base) then exit;
  if not Base.TableIsExists(fTableName) then
    begin
      CreateTable;
      for i:= 0 to Items.Count-1 do
        begin
          aItem:= TwfSettingItem(Items.Items[i]);
          Item[aItem.Name]:= aItem.DefaultValue;
        end;
    end
  else
    DropTable;
end;

procedure TwfSettings.SetItem(aName: string; aValue: variant);
var
  aSQL: String;
  aParams: TwfParams;
begin
  aParams:= nil;
  aSQL:= Format(uSQLWrite,[fTableName, QuotedStr(fPrefix+aName)]);

  Base.CreateParam(aParams, aSQL, true);
  aParams.ParamValues['FVALUE']:= aValue;

  Base.ExecSQL(aSQL, aParams);
end;

constructor TwfSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTableName:= 'WF_SETTINGS';
  fPrefix:= 'WF_';
  fItems:= TwfSettingItems.Create(self, TwfSettingItem);
end;

destructor TwfSettings.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TwfSettings.CreateTable: boolean;
begin
  Result:= false;
  if Base.TableIsExists(fTableName) then exit;

  Result:= Base.ExecSQL(Format(uSQLCreate,[fTableName]));
end;

function TwfSettings.DropTable: boolean;
begin
  Result:= Base.ExecSQL(Format(uSQLDrop,[fTableName]));
end;

{ TwfSettingItem }

function TwfSettingItem.GetDisplayName: string;
begin
  if not IsEmpty(fName) then
    Result:= fName
  else
    Result:=inherited GetDisplayName;
end;

constructor TwfSettingItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TwfSettingItem.Destroy;
begin
  inherited Destroy;
end;

end.
