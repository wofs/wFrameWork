{
This file is part of wfFrameWork.

 -= ValueListEditor =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfValueListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, windows, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Buttons, Spin, wfFunc, wfTypes, wfBase, wfComboBox;

type
  TwfValueListEditor = class;
  TwfValue = class;

  TwfValueType = (vtInt, vtFloat, vtText, vtStrings, vtCheckBox, vtComboBox, vtColor);
  TwfValueState = (vsInit, vsLoading, vsReady);

  TwfNotifyDelete = procedure(aIndex: integer) of object;
  TwfValueOnChange = procedure(Sender: TwfValue) of object;
  TwfValueOnClick = procedure(Sender: TwfValue) of object;
  TwfValueOnChangeState = procedure(Sender: TwfValue) of object;

  TwfValueComboBoxData = class(TObject)
    private
      Id: BaseID;
  end;

  { TwfValueStrings }

  TwfValueStrings = class (TCustomBitBtn)
  private
    fStrings: TStrings;
    function GetText: string;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Strings: TStrings read fStrings write fStrings;
    property Text: string read GetText;
  end;

  { TwfValueColor }

  TwfValueColor = class (TColorButton)
  protected
  private
    function GetValue: string;
    procedure SetValue(aValue: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Value: string read GetValue write SetValue;
  end;

  TwfValueText = class (TCustomEdit)

    published
      property Text;
  end;

  { TwfValueInt }

  TwfValueInt = class (TCustomSpinEdit)

  private
    function GetText: string;
    procedure SetText(aValue: string);
    published
      property Value;
      property Text: string read GetText write SetText;
  end;

  { TwfValueFloat }

  TwfValueFloat = class (TCustomFloatSpinEdit)

  private
    function GetText: string;
    procedure SetText(aValue: string);
    published
      property Value;
      property Text: string read GetText write SetText;
  end;

  TwfValueCheckBox = class (TCheckBox)

    //published onChange
  end;

  { TwfValueComboBox }

  TwfValueComboBox = class (TwfComboBox)
  private
    public
      procedure Fill(aData: TwfData; const InsertAll:boolean = false);
  end;


  { TwfValue }

  TwfValue = class(TCollectionItem)
    protected
      function GetDisplayName: string; override;
      procedure SetIndex(Value: Integer); override;

    private
      fCaption: TCaption;
      fKey: string;
      fObjectHint: TCaption;
      fonInit: TwfValueOnChangeState;
      fonLoading: TwfValueOnChangeState;
      fonReady: TwfValueOnChangeState;
      fonChange: TwfValueOnChange;
      fonClick: TwfValueOnClick;
      fonCreate: TNotifyEvent;
      fonDelete: TwfNotifyDelete;
      fonUpdate: TNotifyEvent;
      fObjectStrings: TStrings;
      fObjectValue: variant;
      fValueState: TwfValueState;
      fValueType: TwfValueType;

      function GetAsCheckBox: TwfValueCheckBox;
      function GetAsColor: TwfValueColor;
      function GetAsComboBox: TwfValueComboBox;
      function GetAsFloat: TwfValueFloat;
      function GetAsInt: TwfValueInt;
      function GetAsStrings: TwfValueStrings;
      function GetAsText: TwfValueText;
      function GetObject: TWinControl;
      function GetObjectHint: TCaption;
      function GetObjectStrings: TStrings;
      function GetObjectValue: string;
      function GetOwnerComponent: TwfValueListEditor;
      procedure SetCaption(aValue: TCaption);
      procedure SetObjectHint(aValue: TCaption);
      procedure SetObjectStrings(aValue: TStrings);
      procedure SetObjectValue(aValue: string);
      procedure SetValueState(aValue: TwfValueState);
      procedure SetValueType(aValue: TwfValueType);
      function ValueTypeAsString: string;

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

      property OwnerComponent: TwfValueListEditor read GetOwnerComponent;
      property TheObject: TWinControl read GetObject;
      property ValueState: TwfValueState read fValueState write SetValueState;

      property AsInt: TwfValueInt read GetAsInt;
      property AsFloat: TwfValueFloat read GetAsFloat;
      property AsText: TwfValueText read GetAsText;
      property AsStrings: TwfValueStrings read GetAsStrings;
      property AsCheckBox: TwfValueCheckBox read GetAsCheckBox;
      property AsComboBox: TwfValueComboBox read GetAsComboBox;
      property AsColor: TwfValueColor read GetAsColor;

      property onCreate: TNotifyEvent read fonCreate write fonCreate;
      property onDelete: TwfNotifyDelete read fonDelete write fonDelete;
      property onUpdate: TNotifyEvent read fonUpdate write fonUpdate;
    published
      //vtInt - Value
      //vtFloat - Value, Delimiter - Default system delimiter
      //vtText - Text
      //vtCheckBox - 1 checked, 0 unchecked
      //vtComboBox - ItemIndex
      //vtColor - Value (Integer)
      property ObjectValue: string read GetObjectValue write SetObjectValue;
      //Object Hint
      //vtInt, vtFloat, vtStrings(Button), vtCheckBox, vtComboBox, vtColor
      property ObjectHint: TCaption read GetObjectHint write SetObjectHint;
      //vtStrings - Strings
      //vtComboBox - Items
      property ObjectStrings: TStrings read GetObjectStrings write SetObjectStrings;
      //Caption value
      property Caption: TCaption read fCaption write SetCaption;
      //Key
      property Key: string read fKey write fKey;
      //Object Type
      property ValueType: TwfValueType read fValueType write SetValueType default vtText;
      //Object OnChange
      property onChange: TwfValueOnChange read fonChange write fonChange;
      //Object OnClick
      property onClick: TwfValueOnClick read fonClick write fonClick;
      //On Value State is Init
      property onInit: TwfValueOnChangeState read fonInit write fonInit;
      //On Value State is Loading
      //Before initialization of object
      property onLoading: TwfValueOnChangeState read fonLoading write fonLoading;
      //On Value State is Ready
      //After initialization of object
      //Here you can reassign object events.
      //Access to the Sender.TheObject. Object type Sender.ValueType
      //As well AsText, AsStrings, AsCheckBox, AsComboBox
      property onReady: TwfValueOnChangeState read fonReady write fonReady;
  end;

  TwfValues = class(TOwnedCollection)
    private
      function AddRow: integer;
      function GetItemIndex(aKey: string): integer;
      function GetOwnerComponent: TwfValueListEditor;
      procedure InitCell(aIndex: integer);
      procedure ObjectOnClick(Sender: TObject);
      procedure onValueCreate(Sender: TObject);
      procedure onValueDelete(aIndex: integer);
      procedure onValueUpdate(Sender: TObject);
      procedure UpdateRow(aIndex: integer);
      procedure ObjectOnChange(Sender: TObject);

    protected
      procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;

      procedure DeleteRow(aIndex: integer);

    public
      function ValueItem(aIndex: integer):TwfValue;
      function ValueItem(aKey: string):TwfValue;

      property OwnerComponent: TwfValueListEditor read GetOwnerComponent;
  end;

  { TwfValueListEditor }

  TwfValueListEditor = class(TCustomStringGrid)
  private
    fValueColWidth: Integer;
    fValues: TwfValues;
    function AddObject(aCol, aRow: integer; aValueType: TwfValueType): TObject;
    procedure AutoFillLastColumn;
    function CheckBoxCreate: TwfValueCheckBox;
    function ColorCreate: TwfValueColor;
    function ComboBoxCreate: TwfValueComboBox;
    function FloatCreate: TwfValueFloat;
    function GetHorzLine: boolean;
    function GetValueColWidth: integer;
    function IntCreate: TwfValueInt;
    procedure SetHorzLine(aValue: boolean);
    procedure SetValueColWidth(aValue: integer);
    function StringsCreate: TwfValueStrings;
    procedure SyncTagRow;
    function TextCreate: TwfValueText;
    procedure DelObject(aCol, aRow: integer);
    function GetFixedColWidth: integer;
    procedure SetFixedColWidth(aValue: integer);

  protected
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
    procedure TopLeftChanged; override;
    function  SelectCell(aCol,aRow: Integer): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MoveRow(FromIndex, ToIndex: Integer);
    function ValueItem(aIndex: integer): TwfValue;
    function ValueItem(aKey: string): TwfValue;
  published
    property Values: TwfValues read fValues write fValues;
    property FixedColWidth: integer read GetFixedColWidth write SetFixedColWidth;
    //0 - AutoFill
    property ValueColWidth: integer read GetValueColWidth write SetValueColWidth default 0;
    property HorzLine: boolean read GetHorzLine write SetHorzLine;
    property BorderStyle;
    property Align;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfvaluelisteditor_icon.lrs}
  RegisterComponents('WF',[TwfValueListEditor]);
end;

{ TwfValueColor }

function TwfValueColor.GetValue: string;
begin
  Result:= IntToStr(ButtonColor);
end;

procedure TwfValueColor.SetValue(aValue: string);
var
  aResult: Integer;
begin
  aResult:= 0;
  TryStrToInt(aValue, aResult);
  ButtonColor:= aResult;
end;

constructor TwfValueColor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TwfValueColor.Destroy;
begin
  inherited Destroy;
end;

{ TwfValueFloat }

function TwfValueFloat.GetText: string;
begin
  Result:= FloatToStr(Value);
end;

procedure TwfValueFloat.SetText(aValue: string);
begin
  Value:= GetOnlyNumbers(aValue);
end;

{ TwfValueInt }

function TwfValueInt.GetText: string;
begin
  Result:= FloatToStr(Value);
end;

procedure TwfValueInt.SetText(aValue: string);
var
  aResult: Longint;
begin
  aResult:= 0;
  TryStrToInt(aValue, aResult);
  Value:= aResult;
end;

{ TwfValueComboBox }

procedure TwfValueComboBox.Fill(aData: TwfData; const InsertAll: boolean);
var
  i, aNewId: Integer;
begin
  Clear;

  for i:= 0 to aData.RowCount-1 do
  begin
    aNewId:= Items.AddObject(aData.Data(i,'NAME'), TwfValueComboBoxData.Create);
    TwfValueComboBoxData(Items.Objects[aNewId]).Id:= aData.Data(i,'ID');
  end;

  if InsertAll then
    begin
     Items.Insert(0,'Все');
    end;

  ItemIndex:= 0;
end;

{ TwfValueStrings }

function TwfValueStrings.GetText: string;
begin
  Result:= StringReplace(fStrings.Text, fStrings.LineBreak, ' /n ', [rfReplaceAll]);
end;

constructor TwfValueStrings.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fStrings:= TStringList.Create;
end;

destructor TwfValueStrings.Destroy;
begin
  FreeAndNil(fStrings);
  inherited Destroy;
end;

{ TwfValues }

function TwfValues.GetOwnerComponent: TwfValueListEditor;
begin
  Result:= TwfValueListEditor(GetOwner);
end;

function TwfValues.ValueItem(aIndex: integer): TwfValue;
begin
   Result:= TwfValue(Items[aIndex]);
end;

function TwfValues.GetItemIndex(aKey: string):integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:=0 to Count-1 do
    if ValueItem(i).Key = aKey then
      begin
        Result:= i;
        Break;
      end;
end;

function TwfValues.ValueItem(aKey: string): TwfValue;
var
  aIndex: Integer;
begin
  Result:= nil;
  aIndex:= GetItemIndex(aKey);
  if aIndex>-1 then
    Result:= ValueItem(aIndex);
end;

procedure TwfValues.onValueCreate(Sender: TObject);
var
  aValueItem: TwfValue;
begin
  aValueItem:= TwfValue(Sender);
  aValueItem.onUpdate:=@onValueUpdate;

  AddRow;
end;

procedure TwfValues.onValueDelete(aIndex: integer);
begin
  DeleteRow(aIndex);
end;

procedure TwfValues.onValueUpdate(Sender: TObject);
begin
  UpdateRow(TwfValue(Sender).Index);
end;

function TwfValues.AddRow:integer;
begin
  with OwnerComponent do
  begin
    RowCount:= RowCount+1;
    Result:= RowCount-1;

    InitCell(Result);
  end;
end;

procedure TwfValues.InitCell(aIndex:integer);
var
  aObject: TObject;
  aValue: TwfValue;
begin
  aValue:= ValueItem(aIndex);
  aValue.ValueState:= vsLoading;

  try
    with OwnerComponent do
      begin
        BeginUpdate;

        Cells[0,aIndex]:= aValue.Caption;
        aObject:= AddObject(1, aIndex, aValue.ValueType);

        TWinControl(aObject).Tag:= aIndex;

        case aValue.ValueType of
          vtInt:
              begin
                Cells[1,aIndex]:= '';
                aValue.ObjectStrings.Clear;

                TwfValueInt(aObject).Hint:= aValue.ObjectHint;
                TwfValueInt(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueInt(aObject).Text:= aValue.ObjectValue;
                TwfValueInt(aObject).OnChange:=@ObjectOnChange;
                TwfValueInt(aObject).OnClick:=@ObjectOnClick;
              end;
          vtFloat:
              begin
                Cells[1,aIndex]:= '';
                aValue.ObjectStrings.Clear;

                TwfValueFloat(aObject).Hint:= aValue.ObjectHint;
                TwfValueFloat(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueFloat(aObject).Text:= aValue.ObjectValue;
                TwfValueFloat(aObject).OnChange:=@ObjectOnChange;
                TwfValueFloat(aObject).OnClick:=@ObjectOnClick;
              end;
          vtText:
              begin
                Cells[1,aIndex]:= '';
                aValue.ObjectStrings.Clear;

                TwfValueText(aObject).Hint:= aValue.ObjectHint;
                TwfValueText(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueText(aObject).Text:= aValue.ObjectValue;
                TwfValueText(aObject).OnChange:=@ObjectOnChange;
                TwfValueText(aObject).OnClick:=@ObjectOnClick;
              end;
          vtStrings:
              begin
                Cells[1,aIndex]:= '';
                aValue.ObjectValue:= EmptyStr;

                TwfValueStrings(aObject).Hint:= aValue.ObjectHint;
                TwfValueStrings(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueStrings(aObject).Strings.Assign(aValue.ObjectStrings);
                TwfValueStrings(aObject).OnChange:=@ObjectOnChange;
                TwfValueStrings(aObject).OnClick:=@ObjectOnClick;
              end;
          vtCheckBox:
              begin
                Cells[1,aIndex]:= '';
                aValue.ObjectStrings.Clear;
                TwfValueCheckBox(aObject).Caption:= EmptyStr;
                TwfValueCheckBox(aObject).Hint:= aValue.ObjectHint;
                TwfValueCheckBox(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueCheckBox(aObject).Checked:= (VarToInt64(aValue.ObjectValue) = 1);
                TwfValueCheckBox(aObject).OnChange:=@ObjectOnChange;
                TwfValueCheckBox(aObject).OnClick:=@ObjectOnClick;
              end;
          vtComboBox:
              begin
                Cells[1,aIndex]:= '';
                TwfValueComboBox(aObject).Hint:= aValue.ObjectHint;
                TwfValueComboBox(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueComboBox(aObject).Items.Assign(aValue.ObjectStrings);
                TwfValueComboBox(aObject).ItemIndex:= VarToInt64(aValue.ObjectValue);
                TwfValueComboBox(aObject).OnChange:=@ObjectOnChange;
                TwfValueComboBox(aObject).OnClick:=@ObjectOnClick;
              end;
          vtColor:
              begin
                Cells[1,aIndex]:= '';
                TwfValueColor(aObject).Hint:= aValue.ObjectHint;
                TwfValueColor(aObject).Value:= aValue.ObjectValue;
                TwfValueColor(aObject).ShowHint:= not IsEmpty(aValue.ObjectHint);
                TwfValueColor(aObject).OnColorChanged:=@ObjectOnChange;
                TwfValueColor(aObject).OnClick:=@ObjectOnClick;
              end;
        end;

        EndUpdate();
      end;
  finally
    aValue.ValueState:= vsReady;
  end;
end;

procedure TwfValues.UpdateRow(aIndex: integer);
var
  aValue: TwfValue;
begin
  with OwnerComponent do
    begin
      DelObject(1,aIndex);
      InitCell(aIndex);
    end;
end;

procedure TwfValues.ObjectOnClick(Sender: TObject);
var
  aValue: TwfValue;
begin
  aValue:= ValueItem(TWinControl(Sender).Tag);
  if Assigned(aValue.onClick) then aValue.onClick(aValue);
end;

procedure TwfValues.ObjectOnChange(Sender: TObject);
var
  aValue: TwfValue;
begin
  aValue:= ValueItem(TWinControl(Sender).Tag);
  if Assigned(aValue.onChange) then aValue.onChange(aValue);
end;

procedure TwfValues.DeleteRow(aIndex: integer);
var
  aGrid: TwfValueListEditor;
begin
  aGrid:= OwnerComponent;
  aGrid.DelObject(1,aIndex);
  //aGrid.Cells[0,0]:= 'test '+IntTOStr(aIndex);
  if aIndex <= aGrid.RowCount then
    aGrid.DeleteRow(aIndex);

end;

procedure TwfValues.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  aValueItem: TwfValue;
begin
  aValueItem:= ValueItem(Item.Index);

  case Action of
      cnAdded       :
        begin
          aValueItem.onCreate:=@onValueCreate;
          aValueItem.onDelete:=@onValueDelete;
        end;

      cnExtracting  : ;
      cnDeleting    : ;

    end;

  inherited Notify(Item, Action);
end;

{ TwfValue }

function TwfValue.GetDisplayName: string;
begin
  Result:= fKey+' {'+ValueTypeAsString+'} '+fCaption;
  //Result:=inherited GetDisplayName;
end;

procedure TwfValue.SetIndex(Value: Integer);
begin
  if Index<>Value then
    OwnerComponent.MoveRow(Index,Value);

  inherited SetIndex(Value);
end;

procedure TwfValue.SetValueType(aValue: TwfValueType);
begin
  if fValueType=aValue then Exit;
  fValueType:=aValue;
  if Assigned(onUpdate) then onUpdate(self);
end;

constructor TwfValue.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  ValueState:= vsInit;

  fKey:= Format('Key_%d',[Index]);
  fCaption:= Format('NewValue_%d',[Index]);
  fValueType:= vtText;
  fObjectStrings:= TStringList.Create;

  if Assigned(fonCreate) then fonCreate(self);
end;

destructor TwfValue.Destroy;
begin
  FreeAndNil(fObjectStrings);
  if Assigned(onDelete) then onDelete(Index);
  inherited Destroy;
end;

function TwfValue.GetOwnerComponent: TwfValueListEditor;
begin
  Result:=  TwfValueListEditor(TwfValues(GetOwner).GetOwner);
end;

function TwfValue.GetObject: TWinControl;
begin
  Result:= nil;
  Result:= TWinControl(OwnerComponent.Objects[1,Index]);
end;

function TwfValue.GetAsCheckBox: TwfValueCheckBox;
begin
  Result:= TwfValueCheckBox(TheObject);
end;

function TwfValue.GetAsColor: TwfValueColor;
begin
  Result:= TwfValueColor(TheObject);
end;

function TwfValue.GetAsComboBox: TwfValueComboBox;
begin
  Result:= TwfValueComboBox(TheObject);
end;

function TwfValue.GetAsFloat: TwfValueFloat;
begin
  Result:= TwfValueFloat(TheObject);
end;

function TwfValue.GetAsInt: TwfValueInt;
begin
  Result:= TwfValueInt(TheObject);
end;

function TwfValue.GetAsStrings: TwfValueStrings;
begin
  Result:= TwfValueStrings(TheObject);
end;

function TwfValue.GetAsText: TwfValueText;
begin
  Result:= TwfValueText(TheObject);
end;

function TwfValue.GetObjectHint: TCaption;
begin
  Result:= fObjectHint;

  if ValueState = vsReady then
    if Assigned(TheObject) then
      case ValueType of
        vtInt           : Result:= AsInt.Hint;
        vtFloat         : Result:= AsFloat.Hint;
        vtText          : Result := AsText.Hint;
        vtCheckBox      : Result := AsCheckBox.Hint;
        vtStrings       : Result := AsStrings.Hint;
        vtComboBox      : Result := AsComboBox.Hint;
        vtColor         : Result := AsColor.Hint;
      end;
end;

function TwfValue.GetObjectStrings: TStrings;
begin
  Result:= fObjectStrings;

  if ValueState = vsReady then
    if Assigned(TheObject) then
      case ValueType of
        //vtInt           : Result:= AsInt.Hint;
        //vtFloat         : Result:= AsFloat.Hint;
        //vtText          : Result := TwfValueText(TheObject).Hint;
        //vtCheckBox      : Result := TwfValueCheckBox(TheObject).Hint;
        vtStrings       : Result := AsStrings.Strings;
        vtComboBox      : Result := AsComboBox.Items;
        //vtColor         : Result := AsColor.Hint;
      end;
end;

function TwfValue.GetObjectValue: string;
begin
  Result:= fObjectValue;

  if ValueState = vsReady then
    if Assigned(TheObject) then
      case ValueType of
        vtInt           : Result:= AsInt.Text;
        vtFloat         : Result:= AsFloat.Text;
        vtText          : Result := AsText.Text;
        vtCheckBox      : if AsCheckBox.Checked then Result := '1' else Result:= '0';
        //vtStrings       : Result := '';
        vtComboBox      : Result := IntToStr(AsComboBox.ItemIndex);
        vtColor         : Result := AsColor.Value;
      end;
end;

procedure TwfValue.SetCaption(aValue: TCaption);
begin
  if fCaption=aValue then Exit;
  fCaption:=aValue;

  OwnerComponent.Cells[0,Index]:= fCaption;
end;

function TwfValue.ValueTypeAsString:string;
begin
  case ValueType of
    vtInt           : Result:= 'Int';
    vtFloat         : Result:= 'Float';
    vtText          : Result:= 'Text';
    vtStrings       : Result:= 'Strings';
    vtCheckBox      : Result:= 'CheckBox';
    vtComboBox      : Result:= 'ComboBox';
    vtColor         : Result := 'Color';
  end;
end;

procedure TwfValue.SetObjectHint(aValue: TCaption);
begin
  fObjectHint:=aValue;

  if Assigned(TheObject) then
    case ValueType of
      vtInt           : AsInt.Hint:= aValue;
      vtFloat         : AsFloat.Hint:= aValue;
      vtText          : AsText.Hint:= aValue;
      vtCheckBox      : AsCheckBox.Hint:= aValue;
      vtStrings       : AsStrings.Hint:= aValue;
      vtComboBox      : AsComboBox.Hint:= aValue;
      vtColor         : AsColor.Hint:= aValue;
    end;
end;

procedure TwfValue.SetObjectStrings(aValue: TStrings);
begin
  fObjectStrings.Assign(aValue);

  if Assigned(TheObject) then
    case ValueType of
      vtInt           : ;
      vtFloat         : ;
      vtText          : ;
      vtCheckBox      : ;
      vtStrings       :
              begin
                AsStrings.Strings.Assign(aValue);
                if IsEmpty(AsStrings.Strings) then
                  OwnerComponent.Cells[1,Index]:= '<пусто>'
                else
                  OwnerComponent.Cells[1,Index]:= '<не пусто>';
              end;
      vtComboBox      : AsComboBox.Items.Assign(aValue);
      vtColor         : ;
    end;
end;

procedure TwfValue.SetObjectValue(aValue: string);
begin
  fObjectValue:=aValue;

  if Assigned(TheObject) then
    case ValueType of
      vtInt           : AsInt.Text:= aValue;
      vtFloat         : AsFloat.Text:= aValue;
      vtText          : AsText.Text:= aValue;
      vtCheckBox      : AsCheckBox.Checked:= (VarToInt64(aValue) = 1);
      vtStrings       : ;
      vtComboBox      : AsComboBox.ItemIndex:= VarToInt64(aValue);
      vtColor         : AsColor.Value:= aValue;
    end;
end;

procedure TwfValue.SetValueState(aValue: TwfValueState);
begin
  fValueState:=aValue;
case fValueState of
  vsInit         : if Assigned(fonInit) then fonInit(self);
  vsLoading      : if Assigned(fonLoading) then fonLoading(self);
  vsReady        : if Assigned(fonReady) then fonReady(self);
end;
end;

{ TwfValueListEditor }

function TwfValueListEditor.GetFixedColWidth: integer;
begin
  Result:= 0;
    if ColCount=0 then exit;
  Result:= ColWidths[0];
end;

procedure TwfValueListEditor.SetFixedColWidth(aValue: integer);
begin
  if ColCount=0 then exit;

   ColWidths[0]:= aValue;

   AutoFillLastColumn;
end;

procedure TwfValueListEditor.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  aObject: TObject;
  aValue: TwfValue;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  aValue:= nil;
  aObject:= nil;

  aObject:= Objects[aCol, aRow];

  if aRow< fValues.Count then
  begin
    aValue:= fValues.ValueItem(aRow);

    if Assigned(aObject) then
      case aValue.ValueType of
        vtInt:
                begin
                  with TwfValueInt(aObject) do
                    begin
                      Width:=aRect.Width-5;
                      Left:=aRect.Left+2;
                      Top:=aRect.Top+2;
                      Visible:= true;
                      RowHeights[aRow]:= Height+4;
                    end;
                end;
        vtFloat:
                begin
                  with TwfValueFloat(aObject) do
                    begin
                      Width:=aRect.Width-5;
                      Left:=aRect.Left+2;
                      Top:=aRect.Top+2;
                      Visible:= true;
                      RowHeights[aRow]:= Height+4;
                    end;
                end;
        vtText:
                begin
                  with TwfValueText(aObject) do
                    begin
                      Width:=aRect.Width-5;
                      Left:=aRect.Left+2;
                      Top:=aRect.Top+2;
                      Visible:= true;
                      RowHeights[aRow]:= Height+7;
                    end;
                end;
        vtStrings:
                begin
                  with TwfValueStrings(aObject) do
                    begin
                      Width:=18;
                      Left:=ColWidths[aCol-1]+ColWidths[aCol]-Width-4;
                      Height:=aRect.Height-4;
                      Top:=arect.Top+1;
                      Visible:=True;
                    end;
                end;
        vtCheckBox:
                begin
                  with TwfValueCheckBox(aObject) do
                    begin
                      Left:= (aRect.Left+ColWidths[aCol] div 2)-10;
                      Top:=aRect.Top+2;
                      Visible:= true;
                    end;
                  RowHeights[aRow]:= DefaultRowHeight+1;
                end;
        vtComboBox:
                begin
                  with TwfValueComboBox(aObject) do
                    begin
                      Width:=aRect.Width-4;
                      Left:=aRect.Left+2;
                      Top:=aRect.Top+2;
                      Visible:= true;
                      RowHeights[aRow]:= Height+4;
                    end;
                end;
        vtColor:
                begin
                  with TwfValueColor(aObject) do
                    begin
                      Width:=aRect.Width-4;
                      Left:=aRect.Left+2;
                      Height:=aRect.Height-2;
                      Top:=arect.Top+1;
                      Visible:=True;
                    end;
                end;
      end;
  end;

end;

procedure TwfValueListEditor.TopLeftChanged;
var
  i: Integer;
  aObject: TObject;
begin
  for i:=VisibleRowCount to RowCount-1 do
  begin
    aObject:= Objects[1, i];

    if Assigned(aObject) then
    begin
      TWinControl(aObject).Visible:= false;
      Repaint;
    end;
  end;

  inherited TopLeftChanged;
end;

function TwfValueListEditor.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:= false;
  if aCol<2 then
    Result:=inherited SelectCell(aCol, aRow);
end;

procedure TwfValueListEditor.SyncTagRow;
var
  i: Integer;
begin
  for i:=0 to RowCount-1 do
  begin
    if Assigned(Objects[1,i]) then
        TWinControl(Objects[1,i]).Tag:=i;

  end;
end;

procedure TwfValueListEditor.MoveRow(FromIndex, ToIndex: Integer);
begin
  if (FromIndex<RowCount) and (ToIndex<RowCount) then
    DoOPMoveColRow(false, FromIndex, ToIndex);

  SyncTagRow;
end;

function TwfValueListEditor.ValueItem(aIndex: integer): TwfValue;
begin
  Result:= Values.ValueItem(aIndex);
end;

function TwfValueListEditor.ValueItem(aKey: string): TwfValue;
begin
  Result:= fValues.ValueItem(aKey);
end;

function TwfValueListEditor.CheckBoxCreate: TwfValueCheckBox;
begin
  Result:= TwfValueCheckBox.Create(self);
  Result.Caption:='';
  Result.Parent:= TWinControl(self);
  Result.Visible:= false;
end;

function TwfValueListEditor.ComboBoxCreate: TwfValueComboBox;
begin
  Result:= TwfValueComboBox.Create(self);
  Result.Parent:= TWinControl(self);
  Result.Style:= csDropDownList;
  Result.Visible:= false;
end;

function TwfValueListEditor.StringsCreate: TwfValueStrings;
begin
  Result:= TwfValueStrings.Create(self);
  Result.Parent:= TWinControl(self);
  Result.Caption:= '...';
  Result.Visible:= false;
end;

function TwfValueListEditor.ColorCreate: TwfValueColor;
begin
  Result:= TwfValueColor.Create(self);
  Result.Parent:= TWinControl(self);
  Result.BorderWidth:=0;
  Result.Flat:= true;
  Result.Cursor:= crHandPoint;
  //Result.Caption:= '...';
  Result.Visible:= false;
end;

function TwfValueListEditor.TextCreate: TwfValueText;
begin
  Result:= TwfValueText.Create(self);
  Result.Parent:= TWinControl(self);
  Result.BorderStyle:= bsNone;
  Result.Visible:= false;
end;

function TwfValueListEditor.IntCreate: TwfValueInt;
begin
  Result:= TwfValueInt.Create(self);
  Result.Parent:= TWinControl(self);
  //Result.BorderStyle:= bsNone;
  Result.Value:=0;
  Result.MinValue:=0;
  Result.MaxValue:=0;
  Result.Visible:= false;
end;

procedure TwfValueListEditor.SetHorzLine(aValue: boolean);
begin
  if aValue then
    Options:= Options+[goHorzLine]
  else
    Options:= Options-[goHorzLine];
end;

procedure TwfValueListEditor.AutoFillLastColumn;
begin
  if ColCount>2 then
    ColWidths[2]:= Width-fValueColWidth-ColWidths[0]-5;
end;

procedure TwfValueListEditor.SetValueColWidth(aValue: integer);
begin
  if (fValueColWidth = aValue) or (ColCount=0) then exit;
  fValueColWidth:= aValue;

  if fValueColWidth>0 then
  begin
    AutoFillColumns:= false;
    ColCount:=3;
    ColWidths[1]:= fValueColWidth;
    AutoFillLastColumn;
  end else
  begin
    AutoFillColumns:= true;
    ColCount:= 2;
  end;
end;

function TwfValueListEditor.FloatCreate: TwfValueFloat;
begin
  Result:= TwfValueFloat.Create(self);
  Result.Parent:= TWinControl(self);
  //Result.BorderStyle:= bsNone;
  Result.Value:=0;
  Result.MinValue:=0;
  Result.MaxValue:=0;
  Result.Visible:= false;
end;

function TwfValueListEditor.GetHorzLine: boolean;
begin
  Result:= (goHorzLine in Options);
end;

function TwfValueListEditor.GetValueColWidth: integer;
begin
  Result:= fValueColWidth;
end;

function TwfValueListEditor.AddObject(aCol, aRow: integer; aValueType: TwfValueType): TObject;
begin
  case aValueType of
    vtInt         : Result:= IntCreate;
    vtFloat       : Result:= FloatCreate;
    vtText        : Result:= TextCreate;
    vtStrings     : Result:= StringsCreate;
    vtCheckBox    : Result:= CheckBoxCreate;
    vtComboBox    : Result:= ComboBoxCreate;
    vtColor       : Result:= ColorCreate;
  end;

  Objects[aCol, aRow]:= Result;
end;

procedure TwfValueListEditor.DelObject(aCol, aRow: integer);
var
  aObject: TObject;
begin
  if aRow< RowCount then
    begin
      aObject:= Objects[aCol, aRow];
      TWinControl(aObject).Visible:= false;
      if Assigned(aObject) then FreeAndNil(aObject);
    end;
end;

constructor TwfValueListEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultColWidth:= 100;
  fValueColWidth:= 0;
  FixedRows:= 0;
  ColCount:= 2;
  RowCount:= 0;
  ScrollBars:= ssAutoVertical;

  //AutoFillColumns:= true;
  Options:= Options+[goSmoothScroll]-[goVertLine,goRangeSelect];//
  fValues:= TwfValues.Create(self, TwfValue);
end;

destructor TwfValueListEditor.Destroy;
begin
  FreeAndNil(fValues);
  inherited Destroy;
end;

end.
