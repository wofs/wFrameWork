{
This file is part of wfFrameWork.

 -= ComboBox =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfComboBox;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, PropEdits,
  wfBase, wfEntity, wfClasses, wfTypes, wfFunc, wfSQLPropertyEditorU, wfParamsU;

type
  TwfComboBoxData = class(TObject)
    private
      Id: BaseID;
  end;

  { TwfComboBox }

  TwfComboBox = class(TComboBox)
  private
    fBase: TwfBase;
    fEntity: TwfEntity;
    fGridGroupField: string;
    fonFilled: TNotifyEvent;
    fonLog: TTextEvent;
    fSQLGetList: TStrings;
    fwDirectGridFill: boolean;
    fwOnGridFiltering: TNotifyEvent;
    fOrderBy: string;
    function GetBase: TwfBase;
    function GetData(aIndex: integer): TwfComboBoxData;
    function GetEntity: TwfEntity;
    function GetFilled: boolean;
    function GetIndexByBaseID(aBaseID: BaseID): Int64;
    function GetSelectID: BaseID;
    procedure SetFilled(aValue: boolean);
    procedure SetSelectID(aValue: BaseID);
    procedure SetSQLGetList(aValue: TStrings);
    procedure SetwEntity(aValue: TwfEntity);

  protected
    procedure Change; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure Fill;
    procedure Clear; override;

    procedure Delete(aBaseID: BaseID);

    property Data[aIndex: integer]: TwfComboBoxData read GetData;
    property SelectID: BaseID read GetSelectID write SetSelectID;

    //Used only in conjunction with DBGrid. Automatically installed.
    property GridGroupField: string read fGridGroupField write fGridGroupField;
    property wOnGridFiltering: TNotifyEvent read fwOnGridFiltering write fwOnGridFiltering;
  published
    {Properties}
    property wBase: TwfBase read GetBase write fBase;

    // When you select an entity, the value is substituted
    // SQLGetListShort in wSQLGetList
    property wEntity: TwfEntity read GetEntity write SetwEntity;

    //Ex: SELECT ID, NAME FROM TABLE
    property wSQLGetList: TStrings read fSQLGetList write SetSQLGetList;
    //Order By to SQLList
    property wOrderBy: string read fOrderBy write fOrderBy;
    //For mode Design.
    //Allows you to fill in the tree data in the form design mode.
    property wFilled: boolean read GetFilled write SetFilled;

    {Events}
    property wOnLog: TTextEvent read fonLog write fonLog;
    property wOnFilled: TNotifyEvent read fonFilled write fonFilled;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfcombobox_icon.lrs}
  RegisterComponents('WF',[TwfComboBox]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfComboBox, 'wSQLGetList', TwfSQLPropertyEditor);
end;

{ TwfComboBox }

function TwfComboBox.GetFilled: boolean;
begin
  Result:= Items.Count>0;
end;

function TwfComboBox.GetSelectID: BaseID;
var
  aData: TwfComboBoxData;
begin
  aData:= nil;
  aData:= Data[ItemIndex];

  if Assigned(aData) then
    Result:= aData.Id
  else
    Result:= EmptyBaseID;
end;

function TwfComboBox.GetData(aIndex: integer): TwfComboBoxData;
begin
  Result:= nil;
  if Assigned(Items.Objects[aIndex]) then
    Result:= TwfComboBoxData(Items.Objects[aIndex]);
end;

function TwfComboBox.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

function TwfComboBox.GetEntity: TwfEntity;
begin
  if not Assigned(fEntity) then
    fEntity:= nil;
  Result:= fEntity;
end;

procedure TwfComboBox.SetFilled(aValue: boolean);
begin
  if not Assigned(fBase) then exit;

  if not Assigned(Items) or (Items.Count=0) then
    begin
      Fill;
      if Items.Count>0 then ItemIndex:=0;
    end
  else
  begin
    Clear;
  end;
end;

function TwfComboBox.GetIndexByBaseID(aBaseID: BaseID):Int64;
var
  i: Integer;
begin
  Result:= EmptyBaseID;

  for i:= 0 to Items.Count-1 do
    if Data[i].Id = aBaseID then
      begin
        Result:=i;
        Break;
      end;
end;

procedure TwfComboBox.SetSelectID(aValue: BaseID);
begin
  ItemIndex:= GetIndexByBaseID(aValue);
end;

procedure TwfComboBox.SetSQLGetList(aValue: TStrings);
begin
  fSQLGetList.Assign(aValue);
end;

procedure TwfComboBox.SetwEntity(aValue: TwfEntity);
begin
  if Assigned(aValue) then
    begin
      if fSQLGetList.Count = 0 then
        fSQLGetList.Assign(aValue.SQLGetListShort)
    end
  else
    fSQLGetList.Clear;

  fEntity:=aValue;
end;

procedure TwfComboBox.Change;
begin
  if Assigned(wOnGridFiltering) then wOnGridFiltering(self);
  inherited Change;
end;

constructor TwfComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fSQLGetList:= TStringList.Create;
  fOrderBy:= EmptyStr;
end;

destructor TwfComboBox.Destroy;
begin
  FreeAndNil(fSQLGetList);
  Clear;
  inherited Destroy;
end;

procedure TwfComboBox.Fill;
var
  aParams: TwfParams;
  aData: TwfData;
  aNewId, i: Integer;
  aSQL: String;
begin
  if (wSQLGetList.Count = 0) or not Assigned(fBase) then exit;

  aParams:= nil;
  aData:= nil;
  aSQL:= wSQLGetList.Text;
  if not IsEmpty(fOrderBy) then
    aSQL:= fBase.WriteOrderBy(aSQL, fOrderBy);

  try

    fBase.CreateParam(aParams, aSQL, true);
    aData:= fBase.GetData(aSQL, aParams);

    Clear;

    for i:= 0 to aData.RowCount-1 do
    begin
      aNewId:= Items.AddObject(aData.Data(i,'NAME'), TwfComboBoxData.Create);
      TwfComboBoxData(Items.Objects[aNewId]).Id:= aData.Data(i,'ID');
    end;

    if Length(GridGroupField)>0 then
      begin
       Items.Insert(0,'Все');
      end;

    ItemIndex:= 0;
  finally
    FreeAndNil(aParams);
    FreeAndNil(aData);
  end;

  if Assigned(fonFilled) then fonFilled(self);
end;

procedure TwfComboBox.Clear;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    begin
      if Assigned(Items.Objects[i]) then
        TwfComboBoxData(Items.Objects[i]).Free;
    end;
  inherited Clear;
end;

procedure TwfComboBox.Delete(aBaseID: BaseID);
var
  aIndex: Int64;
begin
  aIndex:= GetIndexByBaseID(aBaseID);
  Data[aIndex].Free;

  Items.Delete(aIndex);
end;

end.
