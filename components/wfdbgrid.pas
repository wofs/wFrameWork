{
This file is part of wfFrameWork.

 -= DBGrid =-

 wofs(c)2017-2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfDBGrid;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  ComCtrls, StdCtrls, Grids, LCLType, LazUTF8, PropEdits, wfBase,
  wfSQLQuery, wfEntity, wfTreeView, wfComboBox, wfTypes, wfClasses,
  wfResourceStrings, wfFunc, db;

  var
    wfGrid_Images: TImageList;

type

  { TwfGroupTree }

  TwfGroupTree = class(TCollectionItem)
  protected
    function GetDisplayName: string; override;
  private
    fDescription: string;
    fDirectFind: Boolean;
    fGridGroupField: string;
    fOnGridFiltering: TNotifyEvent;
    fTree: TwfTreeView;
    procedure SetGridGroupField(aValue: string);
    procedure SetTree(aValue: TwfTreeView);
    procedure wfOnGridFiltering(Sender: TObject);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

      property OnGridFiltering: TNotifyEvent read fOnGridFiltering write fOnGridFiltering;
  published
      //wfTreeView whose selected branches
      //will be used in the wfDBGrid filter
      property Tree: TwfTreeView read fTree write SetTree;
      property Description: string read fDescription write fDescription;
      //The field that will be filtered wfDBGrid
      property GridGroupField: string read fGridGroupField write SetGridGroupField;
      // Direct search.
      //TRUE-the search occurs immediately when the selection in the component is changed.
      //FALSE - you have to organize the search yourself
      property DirectFind: boolean read fDirectFind write fDirectFind default true;
  end;

  { TwfGroupTrees }

  TwfGroupTrees = class(TOwnedCollection)

  private
    fOnGridFiltering: TNotifyEvent;
    procedure wfonSelectionChanged(Sender: TObject);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure ClearIsNil;
  public
    function GetTree(aName:string):TwfGroupTree;
    property OnGridFiltering: TNotifyEvent read fOnGridFiltering write fOnGridFiltering;
  end;
//

{ TwfGroupComboBox }

TwfGroupComboBox = class(TCollectionItem)
  protected
    function GetDisplayName: string; override;
  private
    fDescription: string;
    fGridGroupField: string;
    fOnGridFiltering: TNotifyEvent;
    fComboBox: TwfComboBox;
    fDirectFind: boolean;

    procedure SetGridGroupField(aValue: string);
    procedure SetComboBox(aValue: TwfComboBox);
    procedure wfOnGridFiltering(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    property OnGridFiltering: TNotifyEvent read fOnGridFiltering write fOnGridFiltering;
  published
    //wfTreeView whose selected branches
    //will be used in the wfDBGrid filter
    property ComboBox: TwfComboBox read fComboBox write SetComboBox;
    property Description: string read fDescription write fDescription;
    //The field that will be filtered wfDBGrid
    property GridGroupField: string read fGridGroupField write SetGridGroupField;
    // Direct search.
    //TRUE-the search occurs immediately when the selection in the component is changed.
    //FALSE - you have to organize the search yourself
    property DirectFind: boolean read fDirectFind write fDirectFind default true;
end;

  { TwfGroupComboBoxes }

  TwfGroupComboBoxes = class(TOwnedCollection)

  private
    fOnGridFiltering: TNotifyEvent;
    procedure wfonChange(Sender: TObject);
    protected
      procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
      procedure ClearIsNil;
    public
      property OnGridFiltering: TNotifyEvent read fOnGridFiltering write fOnGridFiltering;
  end;

  { TwfDBGrid }

  TwfDBGrid = class(TDBGrid)
  protected
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  private
    fBase: TwfBase;
    fDataSource: TDataSource;
    fDataSet: TwfSQLQuery;
    fImageList: TImageList;
    fMultiSelect: boolean;
    fColumnsString: TStrings;
    fonFilled: TNotifyEvent;
    fonLog: TTextEvent;
    fonSelectionChanged: TNotifyEvent;
    fSearchComboBox: TComboBox;
    fSelectedItems: TBaseIDList;
    fSelectGridId: BaseID;
    fShiftState: TShiftState;
    fSQLText: TStrings;
    fstCtrl: boolean;
    fEntity: TwfEntity;
    fGroupComboBoxes: TwfGroupComboBoxes;
    fGroupTrees: TwfGroupTrees;

    fWhereList: TwfCustomSQLItemList;
    fOrderByList: TwfCustomOrderByList;
    fwSearchComboBoxHistoryCount: integer;

    fGridImageList: TImageList;

    function GetBase: TwfBase;
    function GetColumnIndexByName(aFieldName: string): integer;
    function GetColumns: TStrings;
    function GetCurrentId: BaseID;
    function GetDataSet: TDataSet;
    function GetEntity: TwfEntity;
    function GetFilled: boolean;
    function GetGroupComboBoxes: TwfGroupComboBoxes;
    function GetGroupTrees: TwfGroupTrees;

    function GetOrderBy: string;
    function GetSearchComboBox: TComboBox;
    function GetSearchFieldsContaining: string;
    function GetSearchFieldsIn: string;
    function GetSearchFieldsLike: string;
    function GetSelectedCount: Int64;
    function GetSelectedItems: ArrayOfBaseID;
    function GetSQLText: TStrings;
    function ExistsField(aFieldName: string): boolean;
    procedure Search(aText: string);
    procedure SetBase(aValue: TwfBase);

    procedure SetColumns(aValue: TStrings);
    procedure SetColumnsFromString(aValue: TStrings);
    procedure SetEntity(aValue: TwfEntity);
    procedure SetFilled(aValue: boolean);
    procedure SetfSQLText(aValue: TStrings);
    procedure SetMultiSelect(aValue: boolean);
    procedure SetSearchComboBox(aValue: TComboBox);
    procedure SetSearchComboBoxHistoryCount(aValue: integer);
    procedure SetSearchComboBoxStyle(aValue: integer);
    procedure SetSearchFieldsContaining(aValue: string);
    procedure SetSearchFieldsIn(aValue: string);
    procedure SetSearchFieldsLike(aValue: string);
    procedure SetSearchHistory(aText: string);
    procedure SetSelectAll(aValue: boolean);
    procedure wfOnDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure wfOnEndDrag(Sender, Target: TObject; X, Y: Integer);
    //procedure SetSQLText(aValue: string);

    procedure wfonLog(Sender: TObject; const aValue: string);

    procedure Log(aText: string);
    procedure wfOnGridFiltering(Sender: TObject);
    procedure wfSearchComboBox_OnChange(Sender: TObject);
    procedure wfSearchComboBox_OnExit(Sender: TObject);
    procedure wfSearchComboBox_OnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure wfSearchComboBox_OnKeyPress(Sender: TObject; var Key: char);
  protected
    procedure SelectRow;
    procedure wfDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Fill;
    property DataSet: TDataSet read GetDataSet;

    procedure GetWhereList(const aName: string; out aValue: string; out aParams: TwfParams);
    procedure SetWhereList(aName: string; aValue: string; var aParams: TwfParams);
    procedure GenerateSearchList(aSearchText: string; aSearchType: TSQLItemType; const uNameSearch: string = ''; const uFieldList: string = '');
    procedure SetGroup(aGroupObject: TObject);

    procedure wfOnTitleClick(Column: TColumn);

    procedure OrderByClearAll;

    property OrderBy : string read GetOrderBy;

    property SelectAll: boolean write SetSelectAll;
    property SelectedItems: ArrayOfBaseID read GetSelectedItems;
    property SelectedCount: Int64 read GetSelectedCount;
    property CurrentId: BaseID read GetCurrentId;
  published
    //Trees to automatically generate the Where the grid.
    //Setting method OnSelectionChanged TwfTreeView
    //will be linked to the procedure of preparation Where clause of the query.
    //Do not forget to specify GridGroupField - field wfDBGrid, according to which filtration is necessary.
    property wGroupTrees: TwfGroupTrees read GetGroupTrees write fGroupTrees;
    //ComboBoxes to automatically generate the Where the grid.
    //Setting method OnChange TwfComboBox
    //will be linked to the procedure of preparation Where clause of the query.
    //Do not forget to specify GridGroupField - field wfDBGrid, according to which filtration is necessary.
    property wGroupComboBoxes: TwfGroupComboBoxes read GetGroupComboBoxes write fGroupComboBoxes;
    //ComboBox to automatically generate the Where clause.
    //Search fields to specify wSearchFieldsContaining and wSearchFieldsLike.
    property wSearchComboBox: TComboBox read GetSearchComboBox write SetSearchComboBox;

    //The maximum number of entries in the search history
    property wSearchComboBoxHistoryCount: integer read fwSearchComboBoxHistoryCount write SetSearchComboBoxHistoryCount default 0;

    //Field of the table on which the CONTAINING search will be performed.
    //Specify multiple fields separated by commas.
    //Several fields will be merged by the OR expression.
    property wSearchFieldsContaining: string read GetSearchFieldsContaining write SetSearchFieldsContaining;
    //Field of the table on which the LIKE search will be performed.
    //Specify multiple fields separated by commas.
    //Several fields will be merged by the OR expression.
    property wSearchFieldsLike: string read GetSearchFieldsLike write SetSearchFieldsLike;
    //Field of the table on which the Field IN (..) search will be performed.
    property wSearchFieldsIn: string read  GetSearchFieldsIn write SetSearchFieldsIn;
    //Allow multiple selection in grid.
    property wMultiSelect: boolean read fMultiSelect write SetMultiSelect;

    property wImageList: TImageList read fImageList write fImageList;

    //Database connection
    property wBase: TwfBase read GetBase write SetBase;
    //The text of the SQL query to fetch data. If the text is written in Entity, then this field can be left empty.
    property wSQLText: TStrings read GetSQLText write SetfSQLText;
    //Example: ID{ID}<70>[##0.00]|1; NAME{NAME}<840>|1
    property wColumnsString: TStrings read GetColumns write SetColumns;
    //For mode Design.
    //Allows you to fill in the grid data in the form design mode.
    property wFilled: boolean read GetFilled write SetFilled;
    //Specify the related entity. From the entity, in particular,
    //a field will be taken to fetch data to the grid (provided the field is empty wSQLText).
    property wEntity: TwfEntity read GetEntity write SetEntity;

    {Events}
    property wOnLog: TTextEvent read fonLog write fonLog;
    property wOnFilled: TNotifyEvent read fonFilled write fonFilled;

    property wOnSelectionChanged: TNotifyEvent read fonSelectionChanged write fonSelectionChanged;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfdbgrid_icon.lrs}
  RegisterComponents('WF',[TwfDBGrid]);

  //RegisterPropertyEditor(TypeInfo(TDBGridClickEvent), TwfDBGrid, 'OnTitleClick', nil);
end;

{ TwfGroupComboBox }

function TwfGroupComboBox.GetDisplayName: string;
begin
  if Assigned(fComboBox) then
    Result:= fComboBox.Name+' {'+fGridGroupField+'}'
  else
    Result:= inherited;
end;

procedure TwfGroupComboBox.SetGridGroupField(aValue: string);
begin
  fGridGroupField:=aValue;
  if Assigned(fComboBox) then fComboBox.GridGroupField:= aValue;
end;

procedure TwfGroupComboBox.SetComboBox(aValue: TwfComboBox);
begin
  if Assigned(aValue) then
    begin
      if fDirectFind then
        aValue.wOnGridFiltering:=@wfOnGridFiltering
      else
        aValue.wOnGridFiltering:= nil;
      aValue.GridGroupField:=fGridGroupField;
      aValue.Style:=csDropDownList;
    end
  else
    if Assigned(fComboBox) then
      begin
        fComboBox.wOnGridFiltering:= nil;
        fComboBox.GridGroupField:='';
      end;

  fComboBox:= aValue;
end;

procedure TwfGroupComboBox.wfOnGridFiltering(Sender: TObject);
begin
  if Assigned(fOnGridFiltering) then fOnGridFiltering(Sender);
end;

constructor TwfGroupComboBox.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDirectFind:= true;
  fGridGroupField:='ID';
end;

destructor TwfGroupComboBox.Destroy;
begin
  inherited Destroy;
end;

{ TwfGroupComboBoxes }

procedure TwfGroupComboBoxes.wfonChange(Sender: TObject);
begin
  if Assigned(fOnGridFiltering) then fOnGridFiltering(Sender);
end;

procedure TwfGroupComboBoxes.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  case Action of
      cnAdded,
      cnExtracting:
            if Assigned(Item) then
                TwfGroupComboBox(Item).OnGridFiltering:=@wfonChange;
      cnDeleting: if Assigned(Item) and Assigned(TwfGroupComboBox(Item).ComboBox) then TwfGroupComboBox(Item).ComboBox.OnChange:=nil;
    end;

  inherited Notify(Item, Action);
end;

procedure TwfGroupComboBoxes.ClearIsNil;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if not Assigned(TwfGroupComboBox(Items[i]).ComboBox) then
      TwfGroupComboBox(Items[i]).ComboBox:= nil
end;

{ TwfGroupTrees }

procedure TwfGroupTrees.wfonSelectionChanged(Sender: TObject);
begin
  if Assigned(fOnGridFiltering) then fOnGridFiltering(Sender);
end;

procedure TwfGroupTrees.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
case Action of
    cnAdded,
    cnExtracting:
          if Assigned(Item) then
              TwfGroupTree(Item).OnGridFiltering:=@wfonSelectionChanged;
    cnDeleting: if Assigned(Item) and Assigned(TwfGroupTree(Item).Tree) then TwfGroupTree(Item).Tree.OnSelectionChanged:=nil;
  end;

  inherited Notify(Item, Action);
end;

procedure TwfGroupTrees.ClearIsNil;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    if not Assigned(TwfGroupTree(Items[i]).Tree) then
      TwfGroupTree(Items[i]).Tree:= nil
end;

function TwfGroupTrees.GetTree(aName: string): TwfGroupTree;
var
  i: Integer;
begin
  Result:= nil;
  for i:=0 to Count-1 do
    if TwfGroupTree(Items[i]).Tree.Name = aName then
      begin
        Result:= TwfGroupTree(Items[i]);
        break;
      end;
end;

{ TwfGroupTrees }

{ TwfGroupTree }

function TwfGroupTree.GetDisplayName: string;
begin
  if Assigned(fTree) then
    Result:= fTree.Name+' {'+fGridGroupField+'}'
  else
    Result:= inherited;
end;

procedure TwfGroupTree.SetGridGroupField(aValue: string);
begin
  fGridGroupField:=aValue;
  if Assigned(fTree) then fTree.GridGroupField:= aValue;
end;

procedure TwfGroupTree.SetTree(aValue: TwfTreeView);
begin
  if Assigned(aValue) then
    begin
      if fDirectFind then
        aValue.wOnGridFiltering:=@wfOnGridFiltering
      else
        aValue.wOnGridFiltering:=nil;

      aValue.GridGroupField:=fGridGroupField;
    end
  else
    if Assigned(fTree) then
      begin
        fTree.wOnGridFiltering:= nil;
        fTree.GridGroupField:='';
      end;

  fTree:= aValue;
end;

procedure TwfGroupTree.wfOnGridFiltering(Sender: TObject);
begin
  if Assigned(fOnGridFiltering) then fOnGridFiltering(Sender);
end;

constructor TwfGroupTree.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  DirectFind:= true;
end;

destructor TwfGroupTree.Destroy;
begin
  inherited Destroy;
end;

{ TwfDBGrid }

//function TwfDBGrid.GetSQLText: string;
//begin
//  if Length(fSQLText)>0 then
//    Result:= fSQLText
//  else
//    Result:= wfGridSQLDefaults.SQLText;
//end;

procedure TwfDBGrid.GetWhereList(const aName: string; out aValue: string;
  out aParams: TwfParams);
begin
  fWhereList.GetItem(aName, aValue, aParams);
end;

procedure TwfDBGrid.SetColumnsFromString(aValue: TStrings);
const
  CharCaptionStart       = '{';
  CharCaptionEnd         = '}';
  CharWidthStart         = '<';
  CharWidthEnd           = '>';
  CharSizePriority       = '|';
  CharDisplayFormatStart = '[';
  CharDisplayFormatEnd   = ']';
  CharDelimiter          = ';';
var
  i: Integer;
  aFieldName, aCaption, aWidth, aSizePriority, aText, aDisplayFormat: String;
  aStringList: TStringList;
  CurrentPos, NextPos: PtrInt;
  aColumns: TDBGridColumns;
  aColumn: TColumn;
  aInt: Longint;
  aAutoFillColumns: Boolean;
begin
  // ID{ID}<70>[###.##]|1; NAME{NAME}<840>|1

  aColumns:= self.Columns;
  aColumns.Clear;

  if aValue.Count=0 then exit; //=>

  aFieldName:='';
  aCaption:= '';
  aWidth:='';
  aText:= '';
  aDisplayFormat:= '';
  aInt:= 0;

  aStringList:= TStringList.Create;

  aAutoFillColumns:= self.AutoFillColumns;
  AutoFillColumns:= false;

  try
    aStringList.Delimiter:= CharDelimiter;
    aStringList.StrictDelimiter:= true;
    aStringList.DelimitedText:= aValue.Text;

    for i:=0 to aStringList.Count-1 do
      begin
        aText:= aStringList.Strings[i];

        CurrentPos:= UTF8Pos(CharCaptionStart, aText, 1);
        if CurrentPos>0 then
          aFieldName:= Trim(UTF8Copy(aText,1, CurrentPos-1));

          NextPos:= UTF8Pos(CharCaptionEnd, aText, CurrentPos);
          if NextPos>0 then
            aCaption:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos-1));

        CurrentPos:= UTF8Pos(CharWidthStart, aText, 1);
        NextPos:= UTF8Pos(CharWidthEnd, aText, CurrentPos);
        if NextPos>0 then
          aWidth:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos-1));

        CurrentPos:= UTF8Pos(CharDisplayFormatStart, aText, 1);
        NextPos:= UTF8Pos(CharDisplayFormatEnd, aText, CurrentPos);
        if NextPos>0 then
          aDisplayFormat:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos-1));

        CurrentPos:= UTF8Pos(CharSizePriority, aText, NextPos);
        NextPos:= Length(aText);

        aSizePriority:= Trim(UTF8Copy(aText,CurrentPos+1, NextPos-CurrentPos));

        aColumn:= aColumns.Add;

        aColumn.FieldName:= aFieldName;
        aColumn.Title.Caption:= aCaption;
        TryStrToInt(aWidth, aInt);
        aColumn.Width:= aInt;
        TryStrToInt(aSizePriority, aInt);
        aColumn.SizePriority:= aInt;
        aColumn.DisplayFormat:= aDisplayFormat;
      end;
  finally
    AutoFillColumns:= aAutoFillColumns;
    FreeAndNil(aStringList);
  end;
end;

procedure TwfDBGrid.SetColumns(aValue: TStrings);
begin
  // ID{ID}<70>|1; NAME{NAME}<840>|1
  fColumnsString.Assign(aValue);

  SetColumnsFromString(aValue);
end;

procedure TwfDBGrid.SetEntity(aValue: TwfEntity);
begin
  fEntity:= aValue;

  if Assigned(aValue) then
    begin
      if IsEmpty(fColumnsString) then
        SetColumnsFromString(aValue.GridColumnsString);
    end
    else
      fColumnsString.Clear;
end;

procedure TwfDBGrid.SetFilled(aValue: boolean);
begin
  if not Assigned(fBase) then exit;

  if (csDesigning in ComponentState) and not fDataSet.Active then
    Fill
  else
    fDataSet.Active:= false;
end;

procedure TwfDBGrid.SetfSQLText(aValue: TStrings);
begin
  fSQLText.Assign(aValue);
end;

function TwfDBGrid.GetColumnIndexByName(aFieldName: string):integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:=0 to Columns.Count-1 do
    if Columns.Items[i].FieldName = aFieldName then
      begin
        Result:= i;
        break;
      end;
end;

function TwfDBGrid.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

procedure TwfDBGrid.SetMultiSelect(aValue: boolean);
var
  aColIdIndex: Integer;
begin
  fMultiSelect:=aValue;

  aColIdIndex:= GetColumnIndexByName('ID');

  if aValue and (aColIdIndex>-1) then
     Columns.Items[aColIdIndex].Title.Caption:='';
end;

procedure TwfDBGrid.SetSearchComboBox(aValue: TComboBox);
begin
  if fSearchComboBox=aValue then exit; //=>
  if Assigned(aValue) then
    begin
      SetSearchComboBoxStyle(fwSearchComboBoxHistoryCount);
      aValue.OnChange:=@wfSearchComboBox_OnChange;
      aValue.OnExit:=@wfSearchComboBox_Onexit; //=>
      aValue.OnKeyPress:=@wfSearchComboBox_OnKeyPress;
      aValue.OnKeyDown:=@wfSearchComboBox_OnKeyDown;
    end else
    begin
      if Assigned(fSearchComboBox) then
        begin
          fSearchComboBox.OnChange:= nil;
          fSearchComboBox.OnExit:= nil;
          fSearchComboBox.OnKeyPress:= nil;
          fSearchComboBox.OnKeyDown:= nil;
        end;
    end;

    fSearchComboBox:=aValue;
end;

procedure TwfDBGrid.SetSearchComboBoxStyle(aValue: integer);
begin
  if Assigned(fSearchComboBox) then
    if aValue = 0 then
      fSearchComboBox.Style:=csSimple
    else
      fSearchComboBox.Style:=csDropDown;
end;

procedure TwfDBGrid.SetSearchComboBoxHistoryCount(aValue: integer);
begin
  SetSearchComboBoxStyle(aValue);
  fwSearchComboBoxHistoryCount:=aValue;
end;

procedure TwfDBGrid.SetSearchFieldsContaining(aValue: string);
begin
  fWhereList.SearchFieldsContaining:= aValue;
end;

procedure TwfDBGrid.SetSearchFieldsIn(aValue: string);
begin
  fWhereList.SearchFieldsIn:= aValue;
end;

procedure TwfDBGrid.SetSearchFieldsLike(aValue: string);
begin
  fWhereList.SearchFieldsLike:= aValue;
end;

procedure TwfDBGrid.SetSelectAll(aValue: boolean);
var
  aDataSet: TDataSet;
  aBookMark: TBookMark;
  aCurrentId: BaseID;
begin
  if not fMultiSelect then
    raise Exception.Create(rsDBGridErrorMultiselectOff);

  if aValue then
   begin
      aDataSet:= self.DataSource.DataSet;
      try
        aBookMark:= aDataSet.Bookmark;
        aDataSet.DisableControls;
        aDataSet.First;

        while not aDataSet.EOF do
        begin
          aCurrentId:= VarToBaseID(aDataSet.FieldByName('ID').AsVariant);
          if fSelectedItems.IndexOf(aCurrentId)= -1 then
             fSelectedItems.Add(aCurrentId);
          aDataSet.Next;
        end;
      finally
        aDataSet.Bookmark := aBookMark;
        aDataSet.EnableControls;
      end;
   end else
     fSelectedItems.Clear;

  if Assigned(wonSelectionChanged) then wonSelectionChanged(self);
end;

procedure TwfDBGrid.wfOnDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if fSelectedItems.Count>1 then DragCursor:=crMultiDrag else DragCursor:=crDrag;
  fSelectGridId:= CurrentId;
end;

procedure TwfDBGrid.wfOnEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  aGroupTree: TwfGroupTree;
  aTableName, aGroupField: String;
  aReceiverNodeId: BaseID;
  i: Integer;
  aNode: TTreeNode;
begin
  if not (Target is TTreeVIew) then
     exit;

  aGroupTree:= fGroupTrees.GetTree(TTreeVIew(Target).Name);

  if not Assigned(aGroupTree) then exit;

   aTableName:= fEntity.TableName;
   aGroupField:= aGroupTree.GridGroupField;
   aReceiverNodeId:= aGroupTree.Tree.ReceiverNodeId;

    if fSelectedItems.Count>0 then
     begin
      if MessageDlg('Выделено '+IntToStr(fSelectedItems.Count)+' позиций. Переместить их все?',mtConfirmation, mbOKCancel, 0) = mrOK then
       begin
        for i:=0 to fSelectedItems.Count-1 do
          fBase.Update(aTableName,[aGroupField],[aReceiverNodeId],'ID='+BaseIDToStr(fSelectedItems.Items[i]));

        fSelectedItems.Clear;
       end;
     end else
        fBase.Update(aTableName,[aGroupField],[aReceiverNodeId],'ID='+BaseIDToStr(fSelectGridId));

     aNode:= aGroupTree.Tree.FindNode(aReceiverNodeId);
     aGroupTree.Tree.Select(aNode);
     DataSource.DataSet.Locate('ID',fSelectGridId,[]);
end;

function TwfDBGrid.GetSearchFieldsContaining: string;
begin
  Result:= fWhereList.SearchFieldsContaining;
end;

function TwfDBGrid.GetSearchFieldsIn: string;
begin
  Result:= fWhereList.SearchFieldsIn;
end;

function TwfDBGrid.GetSearchFieldsLike: string;
begin
  Result:= fWhereList.SearchFieldsLike;
end;

function TwfDBGrid.GetSelectedCount: Int64;
begin
  Result:= fSelectedItems.Count;
end;

function TwfDBGrid.GetSelectedItems: ArrayOfBaseID;
var
  i: Integer;
begin
  if fSelectedItems.Count = 0 then
    begin
      SetLength(Result, 1);
      Result[0]:= VarToBaseID(self.DataSource.DataSet.FieldByName('ID').AsVariant);
    end
  else
    begin
      SetLength(Result, fSelectedItems.Count);
        for i:=0 to fSelectedItems.Count-1 do
          begin
            Result[i]:= fSelectedItems.Items[i];
          end;
    end;
end;

function TwfDBGrid.GetSQLText: TStrings;
begin
  Result:= fSQLText;
end;

procedure TwfDBGrid.SetBase(aValue: TwfBase);
begin
  fBase:=aValue;
end;

function TwfDBGrid.GetOrderBy: string;
begin
 Result:= fOrderByList.AsString;
end;

function TwfDBGrid.GetSearchComboBox: TComboBox;
begin
  if not Assigned(fSearchComboBox) then
    fSearchComboBox:= nil;
  Result:= fSearchComboBox;
end;

function TwfDBGrid.GetDataSet: TDataSet;
begin
  Result:= self.DataSource.DataSet;
end;

function TwfDBGrid.GetEntity: TwfEntity;
begin
  if not Assigned(fEntity) then
    fEntity:= nil;
  Result:= fEntity;
end;

function TwfDBGrid.GetColumns: TStrings;
var
  aColumns: TDBGridColumns;
  i: Integer;
  aText: String;
begin
 if not Assigned(self) then exit;
  aText:= '';
  aColumns:= self.Columns;

  for i:=0 to aColumns.Count-1 do
    begin
      if i>0 then
        aText:= aText+'; ';
        aText:= aText+ Format('%s{%s}<%d>|%d',[aColumns[i].FieldName, aColumns[i].Title.Caption, aColumns[i].Width, aColumns[i].SizePriority]);
    end;
  fColumnsString.Clear;
  fColumnsString.Add(aText);

  Result:= fColumnsString;
end;

function TwfDBGrid.GetCurrentId: BaseID;
begin
 if Assigned(DataSet) and ExistsField('ID') then
   Result:= VarToBaseID(DataSet.FieldByName('ID').AsVariant)
 else
   Result:= EmptyBaseID;
end;

function TwfDBGrid.GetFilled: boolean;
begin
  Result:= Assigned(fDataSet) and fDataSet.Active;
end;

function TwfDBGrid.GetGroupComboBoxes: TwfGroupComboBoxes;
begin
  fGroupComboBoxes.ClearIsNil;
  Result:= fGroupComboBoxes;
end;

function TwfDBGrid.GetGroupTrees: TwfGroupTrees;
begin
  fGroupTrees.ClearIsNil;
  Result:= fGroupTrees;
end;

procedure TwfDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  DataCol: Integer;
begin
  PrepareCanvas(aCol, aRow, aState);

  {$ifdef dbgGridPaint}
  DbgOut(' ',IntToStr(aCol));
  if gdSelected in aState then DbgOut('S');
  if gdFocused in aState then DbgOut('*');
  if gdFixed in aState then DbgOut('F');
  {$endif dbgGridPaint}

  if (gdFixed in aState) or DefaultDrawing then
    DefaultDrawCell(aCol, aRow, aRect, aState)
  else
  if not DefaultDrawing then
    DrawCellBackground(aCol, aRow, aRect, aState);

  DataCol := ColumnIndexFromGridColumn(aCol);

  if (ARow>=FixedRows) and (DataCol>=0) and not (csDesigning in ComponentState) then
    wfDrawColumnCell(aRect, DataCol, TColumn(Columns[DataCol]), aState);

  if (ARow>=FixedRows) and Assigned(OnDrawColumnCell) and
    not (csDesigning in ComponentState) then begin

    if DataCol>=0 then
      OnDrawColumnCell(Self, aRect, DataCol, TColumn(Columns[DataCol]), aState);

  end;

  DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TwfDBGrid.wfDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  aColumnText: Char;
begin
    if (gdFocused in State) then // если строка не выделена, то
      begin
        Canvas.Brush.Color:= FixedHotColor;
        Canvas.Font.Color:= clBlack;
        Canvas.FillRect(Rect);
        DefaultDrawColumnCell(Rect,DataCol,Column,State);
      end;

    if (Column.FieldName = 'ID') and wMultiSelect then
    begin
      Canvas.FillRect(Rect);

      if (fSelectedItems.IndexOf(VarToBaseID(Column.Field.AsVariant))>-1) then
      begin
        if Assigned(wfGrid_Images) then
        begin
          wfGrid_Images.Draw(Canvas,Rect.Left,Rect.Top, 2 );
          aColumnText:=' '
        end
        else
          aColumnText:='+'
      end
      else
        aColumnText:=' ';

       Canvas.TextOut(Rect.Right - 2 - Canvas.TextWidth(aColumnText), Rect.Top + 2, aColumnText);
    end;

end;

procedure TwfDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  fstCtrl:= (ssCtrl in Shift);

  if ((ssShift in Shift) ) and ((Key = VK_UP) or (Key = VK_DOWN)) then SelectRow;
  inherited;
end;

procedure TwfDBGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  fstCtrl:=  (ssCtrl in Shift);

  if Key = VK_SPACE then
   begin
    SelectRow;
    DataSource.DataSet.Next;
   end;

  inherited;
end;

procedure TwfDBGrid.wfonLog(Sender: TObject; const aValue: string);
begin
  Log(Sender.ToString+' | '+aValue);
end;

procedure TwfDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  fShiftState:= Shift;
  inherited;
end;

procedure TwfDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if fstCtrl then SelectRow;

  inherited;
end;

procedure TwfDBGrid.Log(aText: string);
begin
  if Assigned(wOnLog) then wOnLog(self, aText);
end;

procedure TwfDBGrid.SetGroup(aGroupObject: TObject);
var
  aTree: TwfTreeView;
  aComboBox: TwfComboBox;
  aSelectID: BaseID;
begin
  if aGroupObject is TwfTreeView then
   begin
     aTree:= TwfTreeView(aGroupObject);
     if not Assigned(aTree.Items) and (aTree.Items.Count=0) then exit;

     if aTree.CurrentId <> aTree.RootId then
       GenerateSearchList(fBase.AsString(aTree.SelectedEtems()), stIn, aTree.Name, aTree.GridGroupField)
     else
       GenerateSearchList('',stIn, aTree.Name);
   end;

  if aGroupObject is TwfComboBox then
   begin
     aComboBox:= TwfComboBox(aGroupObject);
     if not Assigned(aComboBox.Items) and (aComboBox.Items.Count=0) then exit;

     aSelectID:= aComboBox.SelectID;
     if not IsEmpty(aSelectID) then
       GenerateSearchList(BaseIDToStr(aSelectID), stIn, aComboBox.Name, aComboBox.GridGroupField)
     else
       GenerateSearchList('',stIn, aComboBox.Name);
   end;
end;

procedure TwfDBGrid.wfOnGridFiltering(Sender: TObject);
begin
  SetGroup(Sender);
  Fill;
end;

function TwfDBGrid.ExistsField(aFieldName:string):boolean;
var
  i: Integer;
begin
  Result:= false;
  for i:= 0 to DataSet.FieldCount-1 do
    begin
       Result:= DataSet.Fields[i].FieldName = aFieldName;
       if Result then
         Break;
    end;
end;

procedure TwfDBGrid.OrderByClearAll;
var
  i: Integer;
begin
  for i:=0 to Columns.Count-1 do
    Columns.Items[i].Title.ImageIndex:= -1;

  fOrderByList.ClearItemsAndData;
end;

procedure TwfDBGrid.wfOnTitleClick(Column: TColumn);
var
  aDirection: TDirection;
begin
  if (not ExistsField('ID') or (Column.FieldName = 'ID')) and wMultiSelect then exit; //=>

  aDirection:= fOrderByList.GetItem(Column.FieldName);

  case aDirection of
    dNone  :
        begin
          fOrderByList.SetItem(Column.FieldName, dASC);
          Column.Title.ImageIndex:= 0;
        end;
    dASC   :
        begin
          fOrderByList.SetItem(Column.FieldName, dDESC);
          Column.Title.ImageIndex:= 1;
        end;
    dDESC  :
        begin
          fOrderByList.DelItem(Column.FieldName);
          Column.Title.ImageIndex:= -1;
        end;
  end;

  Fill;
end;

procedure TwfDBGrid.wfSearchComboBox_OnChange(Sender: TObject);
var
  aEdit: TComboBox;
begin
  aEdit:= (Sender as TComboBox);

end;

procedure TwfDBGrid.wfSearchComboBox_OnExit(Sender: TObject);
var
  aEdit: TComboBox;
begin
  aEdit:= (Sender as TComboBox);

end;

procedure TwfDBGrid.wfSearchComboBox_OnKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  aEdit: TComboBox;
begin
  aEdit:= (Sender as TComboBox);
end;

procedure TwfDBGrid.SetSearchHistory(aText:string);
var
  aCombobox: TComboBox;
  i: Integer;
begin
  if not Assigned(fSearchComboBox) or (Length(aText)=0) or (fwSearchComboBoxHistoryCount=0) then exit;

  aCombobox:= fSearchComboBox;
  with aCombobox do begin
    Items.Append(aText);
    if Items.Count>fwSearchComboBoxHistoryCount then
       for i:= Items.Count downto fwSearchComboBoxHistoryCount do
         Items.Delete(i);
  end;
end;

procedure TwfDBGrid.Search(aText: string);
begin
  if (Length(aText)>0) and Assigned(fSearchComboBox) then
    fSearchComboBox.Color:= clSkyBlue
  else
    fSearchComboBox.Color:= clDefault;

  GenerateSearchList(aText, stContaining);
  GenerateSearchList(aText, stLike);
  GenerateSearchList(aText, stIn);
  SetSearchHistory(aText);
  Fill;
end;

procedure TwfDBGrid.wfSearchComboBox_OnKeyPress(Sender: TObject;
  var Key: char);
var
  aEdit: TComboBox;
  aText: TCaption;
begin
  aEdit:= (Sender as TComboBox);
  aText:= aEdit.Text;
  if Key = #13 then
    begin
       Search(aText);
    end;
end;

procedure TwfDBGrid.SelectRow;
var
  aCurrentId: BaseID;
  aDataSet: TDataSet;
  aIndex: Integer;
begin
  if not fMultiSelect or (DataSet.RecordCount=0) then exit;

  aDataSet:= self.DataSource.DataSet;
  aDataSet.DisableControls;

  try
    aCurrentId:= VarToBaseID(aDataSet.FieldByName('ID').AsVariant);

    aIndex:= fSelectedItems.IndexOf(aCurrentId);
    if aIndex>-1 then
      fSelectedItems.Delete(aIndex)
    else
      fSelectedItems.Add(aCurrentId);
  finally
    aDataSet.EnableControls;
    self.Repaint;

    if Assigned(wonSelectionChanged) then wonSelectionChanged(self);
  end;
end;

procedure TwfDBGrid.SetWhereList(aName: string; aValue: string;
  var aParams: TwfParams);
begin
  fWhereList.SetItem(aName, aValue, aParams);
end;

procedure TwfDBGrid.GenerateSearchList(aSearchText: string;
  aSearchType: TSQLItemType; const uNameSearch: string;
  const uFieldList: string);
begin
  fWhereList.GenerateSearchList(aSearchText, aSearchType, uNameSearch, uFieldList);
end;

constructor TwfDBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSQLText:= TStringList.Create;
  fColumnsString:= TStringList.Create;
  fColumnsString.Clear;

  fGroupTrees:= TwfGroupTrees.Create(self, TwfGroupTree);
  fGroupTrees.OnGridFiltering:= @wfOnGridFiltering;

  fGroupComboBoxes:= TwfGroupComboBoxes.Create(self, TwfGroupComboBox);
  fGroupComboBoxes.OnGridFiltering:= @wfOnGridFiltering;

  if not Assigned(TitleImageList) then
    TitleImageList:= wfGrid_Images;

  fMultiSelect:= false;
  fstCtrl:= false;

  self.SelectedColor:= self.FixedHotColor;
  self.Options:=self.Options-[dgMultiselect, dgEditing]+[dgRowHighlight, dgAlwaysShowSelection, dgTruncCellHints];

  fDataSource:= TDataSource.Create(self);
  self.DataSource:= fDataSource;

  if not Assigned(OnTitleClick) then
    self.OnTitleClick:=@wfOnTitleClick;

  //if not Assigned(OnDrawColumnCell) then
  //  self.OnDrawColumnCell:=@wfDrawColumnCell;

  if not Assigned(onDragOver) then
    self.onDragOver:=@wfOnDragOver;

  if not Assigned(OnEndDrag) then
    self.OnEndDrag:=@wfOnEndDrag;

  fDataSet:= TwfSQLQuery.Create(self);

  fSelectedItems:= TBaseIDList.Create;

  fWhereList:= TwfCustomSQLItemList.Create;
  fWhereList.onLog:=@wfonLog;

  fOrderByList:= TwfCustomOrderByList.Create;

  wSearchComboBoxHistoryCount:= 0;
end;

destructor TwfDBGrid.Destroy;
begin
    FreeAndNil(fGroupComboBoxes);
    FreeAndNil(fGroupTrees);
    FreeAndNil(fSQLText);
    FreeAndNil(fColumnsString);
    FreeAndNil(fSelectedItems);
    FreeAndNil(fWhereList);
    FreeAndNil(fOrderByList);
  //if Assigned(fDataSource) then
  //  FreeAndNil(fDataSource);
  //if Assigned(fDataSet) then
  //  FreeAndNil(fDataSet);
  inherited Destroy;
end;

procedure TwfDBGrid.Fill;
var
  aParams: TwfParams;
  aSQL: String;
begin
  if Assigned(fEntity) and IsEmpty(fSQLText) then
    aSQL:= Trim(fEntity.SQLGetList.Text)
  else
    aSQL:= Trim(fSQLText.Text);

  if not Assigned(self) or not Assigned(fBase) or (Length(aSQL)=0) then exit; //=>

  aSQL:= fBase.WriteOrderBy(aSQL, fOrderByList.AsString);
  aSQL:= fBase.WriteWhere(aSQL, fWhereList.AsString, false);
  aParams:= fWhereList.Params;

  try
    fBase.OpenSQL(aSQL, aParams, fDataSet);
    fDataSource.DataSet:= fDataSet;

    if Assigned(wOnFilled) then wOnFilled(self);
  finally
    FreeAndNil(aParams);
  end;
end;

initialization
  {$i wfdbgrid_icon.lrs}

  wfGrid_Images := TImageList.Create(nil);

  with wfGrid_Images do
  begin
    AddLazarusResource('gd_sort_up');
    AddLazarusResource('gd_sort_down');
    AddLazarusResource('gd_select');
  end;

finalization
  wfGrid_Images.Free();
end.
