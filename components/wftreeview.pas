{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfTreeView;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, PropEdits, wfBase, wfSQLQuery, wfEntity, wfResourceStrings, wfTypes,
  wfClasses, wfFunc, wfParamsU, wfSQLPropertyEditorU;

var
  wfTree_Images: TImageList;

type

  TwfWriteNodeData = procedure(Sender: TObject; var aNode: TTreeNode; var aData: TwfData; const aIndex: Int64) of object;
  TwfNewNodeWriteParams =  procedure(Sender: TObject; var aParams: TwfParams; aParentId: BaseID; aName: string) of object;

  TwfTreeData = class(TObject)
    public
      Id: BaseID;
      IdParent: BaseID;
      CCount: Int64;
      fSmallInt: SmallInt;
      fInt: integer;
      fInt64: Int64;
      fBaseID: BaseID;
      fString: string;
      Tag: Int64;
  end;

  TOnBuildNodeProc = procedure(Sender: TObject;
    StateFieldValue: variant;
    var StateImage: integer) of object;

  { TwfTreeView }

  TwfTreeView = class(TTreeView)
  protected
    procedure DoSelectionChanged; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Delete(Node: TTreeNode); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure GetImageIndex(Node: TTreeNode); override;
    procedure GetSelectedIndex(Node: TTreeNode); override;
  private
    fEntity: TwfEntity;
    fOrderByFields: string;
    fReceiverNodeId: BaseID;
    fShowChildrenItems: boolean;
    fSQLGetFillList: String;
    fSQLGetParentsAll: String;
    fTableName: String;
    fFieldId: string;
    fFieldIdParent: string;
    fFieldName: string;

    fSQLGetRoot: TStrings;
    fSQLDeleteNode: string;
    fSQLDragNode: string;
    fSQLEditNode: string;
    fSQLGetChildrens: string;
    fSQLGetChildrensAll: string;
    fSQLNewNode: String;

    fBase: TwfBase;
    fCurrentId: BaseID;
    fExpanded: boolean;
    fMoveNode: Boolean;
    fonLog: TTextEvent;
    fonNewNodeWriteParams: TwfNewNodeWriteParams;
    fonWriteNodeData: TwfWriteNodeData;
    fParentId: BaseID;
    fRootId: BaseID;
    fGridGroupField: string;
    fOnAddBefore: TAcceptEvent;
    fOnDeleteBefore: TAcceptEvent;
    fOnEditBefore: TAcceptEvent;
    fOnFilled: TNotifyEvent;
    fAllowEditing: boolean;
    fOnGridFiltering: TNotifyEvent;
    fwOnSetedCurrentId: TBaseEvent;
    ssCtrlDown: Boolean;

    procedure AsyncInit(Data: PtrInt);
    function GetBase: TwfBase;
    function GetEntity: TwfEntity;
    function GetFilled: boolean;
    function GetSQLGetRoot: TStrings;
    procedure mItemDeleteClick(Sender: TObject);
    procedure mItemEditClick(Sender: TObject);
    procedure mItemNewClick(Sender: TObject);
    procedure SetCurrentId(aValue: BaseID);
    procedure SetFilled(aValue: boolean);
    procedure SetShowChildrenItems(aValue: boolean);
    procedure SetSQLGetRoot(aValue: TStrings);
    procedure SetTableNameSQLGetRoot;
    procedure SetwEntity(aValue: TwfEntity);
    procedure WriteNodeData(var aNode: TTreeNode; var aData: TwfData; const aIndex: Int64);
    procedure NewNodeWriteParams(var aParams: TwfParams; aParentId: BaseID; aName: string);

    procedure Log(aText: string); virtual;
    procedure wfDoDragDrop(aSender, aSource: TObject; aX, aY: Integer);
    procedure GetExpandingItems(var aNode: TTreeNode);
    function GetCurrentId: BaseID;
    function GetNodesFromDB(const uSQLGet: string; const uID: BaseID): TwfData;
    function GetParrentId: BaseID;
    function GetData(aNode: TTreeNode): TwfTreeData;
    function GetRootId: BaseID;

    procedure SetExpanded(aValue: boolean);

    procedure wfOnDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure wfOnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    {Functions and Proceduries}
    procedure Fill(const uRootId: variant);
    procedure ExpandThis(const uId: BaseID);

    function GetParents(uID: BaseID): TwfData;
    function GetChildrens(uID: BaseID; const uAll: boolean = false): TwfData;

    function NewNode: BaseID;
    function EditNode: BaseID;
    function DeleteNode: boolean;


    function FindNode(const uID: BaseID): TTreeNode;

    function SelectedItems(const uAll: boolean = true): ArrayOfBaseID;

    property RootId: BaseID read fRootId;
    property ParentId: BaseID read fParentId;
    property CurrentId: BaseID read fCurrentId write SetCurrentId;

    property Data[aNode: TTreeNode]: TwfTreeData read GetData;
    property Expanded: boolean read fExpanded write SetExpanded default false;

    //Used only in conjunction with DBGrid. Automatically installed.
    property ShowChildrenItems: boolean read fShowChildrenItems write SetShowChildrenItems;
    property GridGroupField: string read fGridGroupField write fGridGroupField;
    property wOnGridFiltering: TNotifyEvent read fOnGridFiltering write fOnGridFiltering;
    property ReceiverNodeId: BaseID read fReceiverNodeId write fReceiverNodeId;

  published

    {Properties}
    property wBase: TwfBase read GetBase write fBase;
    property wEntity: TwfEntity read GetEntity write SetwEntity;
    property wTableName: string read fTableName write fTableName;
    //Ex: SELECT ID, IDPARENT, NAME FROM TABLE WHERE IDPARENT=0
    property wSQLGetRoot: TStrings read GetSQLGetRoot write SetSQLGetRoot;
    //For mode Design.
    //Allows you to fill in the tree data in the form design mode.
    property wFilled: boolean read GetFilled write SetFilled;
    // Flag allows editing and drag and drop of tree branches
    property wAllowEditing: boolean read fAllowEditing write fAllowEditing;
    // Fields to sort the selection
    property wOrderByFields: string read fOrderByFields write fOrderByFields;
    // Field by PARENT
    property wFieldIdParent: string read fFieldIdParent write fFieldIdParent;
    {Events}
    property wOnLog: TTextEvent read fonLog write fonLog;
    property wOnWriteNodeData: TwfWriteNodeData read fonWriteNodeData write fonWriteNodeData;
    property wOnNewNodeWriteParams: TwfNewNodeWriteParams read fonNewNodeWriteParams write fonNewNodeWriteParams;
    property wOnFilled: TNotifyEvent read fOnFilled write fOnFilled;
    property wOnSetedCurrentId: TBaseEvent read fwOnSetedCurrentId write fwOnSetedCurrentId;
    property wOnAddBefore:TAcceptEvent read fOnAddBefore write fOnAddBefore;
    property wOnEditBefore:TAcceptEvent read fOnEditBefore write fOnEditBefore;
    property wOnDeleteBefore:TAcceptEvent read fOnDeleteBefore write fOnDeleteBefore;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wftreeview_icon.lrs}
  RegisterComponents('WF', [TwfTreeView]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfTreeView, 'wSQLGetRoot', TwfSQLPropertyEditor);
end;

{ TwfTreeView }

procedure TwfTreeView.WriteNodeData(var aNode: TTreeNode; var aData: TwfData;
  const aIndex: Int64);
begin
  if Assigned(wOnWriteNodeData) then
    wOnWriteNodeData(self, aNode, aData, aIndex)
  else
    begin
      with GetData(aNode) do
      begin
        Id:= aData.Data(aIndex,fFieldId);
        IdParent:= aData.Data(aIndex,fFieldIdParent);
        CCount:= aData.Data(aIndex,'CCOUNT');
        aNode.HasChildren:= CCount>0;
      end;
    end;
end;

procedure TwfTreeView.SetTableNameSQLGetRoot;
  procedure SetGetRootDefault;
  begin
    {$IFDEF USEGUID}
       fSQLGetRoot.Text:= 'SELECT T1.*, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 WHERE T1.IDPARENT=''''';
    {$ELSE}
       fSQLGetRoot.Text:= 'SELECT T1.*, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 WHERE T1.IDPARENT=0';
    {$ENDIF}
  end;
begin
  if Assigned(fEntity) then
  begin
    fTableName:= fEntity.TableName;

    if fEntity.SQLTreeGetRoot.Count>0 then
      fSQLGetRoot.Assign(fEntity.SQLTreeGetRoot)
    else
      SetGetRootDefault;

  end else
  begin
    fTableName:= 'TABLE_TREE';
    SetGetRootDefault;
  end;
end;

procedure TwfTreeView.SetSQLGetRoot(aValue: TStrings);
begin
  fSQLGetRoot.Assign(aValue);
end;

function TwfTreeView.GetFilled: boolean;
begin
  Result:= Items.Count>0;
end;

function TwfTreeView.GetSQLGetRoot: TStrings;
begin
  Result:= fSQLGetRoot;
end;

function TwfTreeView.GetEntity: TwfEntity;
begin
  if not Assigned(fEntity) then
    fEntity:= nil;
  Result:= fEntity;
end;

procedure TwfTreeView.AsyncInit(Data: PtrInt);
var
  aMenuItem: TMenuItem;
begin
  if IsEmpty(Images) then
    Images:= wfTree_Images;

  if IsEmpty(PopupMenu) and fAllowEditing then
  begin
    PopupMenu:= TPopupMenu.Create(self);
    PopupMenu.Images:= wfTree_Images;

    PopupMenu.Items.Clear;

    aMenuItem:= NewItem(rsPopupMenuAdd, 0, False, True, @mItemNewClick, 0, 'mItemAdd');
    aMenuItem.ImageIndex:= 2;
    PopupMenu.Items.Add(aMenuItem);

    aMenuItem:= NewItem(rsPopupMenuEdit, 0, False, True, @mItemEditClick, 0, 'mItemEdit');
    aMenuItem.ImageIndex:= 3;
    PopupMenu.Items.Add(aMenuItem);

    aMenuItem:= NewItem(rsPopupMenuDel, 0, False, True, @mItemDeleteClick, 0, 'mItemDelete');
    aMenuItem.ImageIndex:= 4;
    PopupMenu.Items.Add(aMenuItem);
  end;

  if Assigned(fBase) then
    case fBase.Engine of
      seFirebird: begin
        fSQLGetChildrensAll:= 'with recursive tree '
                +' as (select t.id '
                +'     from %s t '
                +'     where t.%s = :ID '
                +'     union all '
                +'     select t.id '
                +'     from %s t '
                +'     inner join tree prior on prior.id = t.%s) '
                +' select * from tree';
      end;
      sePostgreSQL: begin
        fSQLGetChildrensAll:= 'with recursive tree '
                +' as (select t.id, t.name '
                +'     from %s t '
                +'     where %s = :ID '
                +'     union all '
                +'     select t.id, t.name '
                +'     from %s t '
                +'     inner join tree prior on prior.id = t.%s) '
                +' select * from tree';
      end;
    end;
end;

function TwfTreeView.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

procedure TwfTreeView.mItemDeleteClick(Sender: TObject);
begin
  DeleteNode;
end;

procedure TwfTreeView.mItemEditClick(Sender: TObject);
begin
  EditNode;
end;

procedure TwfTreeView.mItemNewClick(Sender: TObject);
begin
  NewNode;
end;

procedure TwfTreeView.SetCurrentId(aValue: BaseID);
begin
  fCurrentId:= aValue;

  ExpandThis(fCurrentId);

  if Assigned(fwOnSetedCurrentId) then fwOnSetedCurrentId(self, aValue);
end;

procedure TwfTreeView.SetFilled(aValue: boolean);
begin
  if not Assigned(fBase) then exit;

  if (csDesigning in ComponentState) and (not Assigned(Items) or (Items.Count=0)) then
    Fill(nil)
  else
  begin
    Items.Clear;
  end;
end;

procedure TwfTreeView.SetShowChildrenItems(aValue: boolean);
var
  aCurrentId: BaseID;
begin
  //if fShowChildrenItems=aValue then Exit;
  fShowChildrenItems:=aValue;

  aCurrentId:= CurrentId;

  if Assigned(fBase) and fBase.Connection.Connected then
  begin
    Fill(nil);
    ExpandThis(aCurrentId);
  end;
end;

procedure TwfTreeView.SetwEntity(aValue: TwfEntity);
begin
  fEntity:=aValue;
  if Assigned(aValue) then
    SetTableNameSQLGetRoot;
end;

procedure TwfTreeView.NewNodeWriteParams(var aParams: TwfParams;
  aParentId: BaseID; aName: string);
begin
  if Assigned(wOnNewNodeWriteParams) then
    wOnNewNodeWriteParams(self, aParams, aParentId, aName)
  else
    begin
      aParams.ParamValues[fFieldIdParent]:= aParentId;
      aParams.ParamValues[fFieldName]:= aName;
    end;
end;

procedure TwfTreeView.Log(aText: string);
begin
  if Assigned(wOnLog) then wOnLog(self, aText);
end;

procedure TwfTreeView.wfDoDragDrop(aSender, aSource: TObject; aX, aY: Integer);
var
   AnItem: TTreeNode;
   AttachMode: TNodeAttachMode;
   HT: THitTests;
   aSourceId: BaseID;
   aSQL: String;
   aParams: TwfParams;
begin
   if (not ssCtrlDown or not fAllowEditing) and (aSource is TwfTreeView) then exit;

   if (aSource is TwfTreeView) then
   begin
       if IsEmpty(fCurrentId) then exit; //=>

       HT := GetHitTestInfoAt(aX, aY) ;
       AnItem := GetNodeAt(aX, aY) ;

       if not Assigned(AnItem) then
          Exception.Create(rsDBTreeErrorMovingNode);

       fReceiverNodeId:=GetData(AnItem).Id;

       aSourceId:= CurrentId;

       if (aSourceId = fReceiverNodeId) or (fParentId = aSourceId) then exit; //=>

       if Assigned(fEntity) then
         aSQL:= Format(fEntity.SQL[estTreeDragNode],[fFieldIdParent,fFieldIdParent])
       else
         aSQL:= Format(fSQLDragNode,[fTableName,fFieldIdParent,fFieldIdParent]);

       aParams:= nil;
       fBase.CreateParam(aParams, aSQL, true);
       aParams.ParamValues[fFieldId]:= aSourceId;
       aParams.ParamValues[fFieldIdParent]:= fReceiverNodeId;

       fBase.ExecSQL(aSQL, aParams);

       if (HT - [htOnItem, htOnIcon, htNowhere, htOnIndent] <> HT) then
       begin
         if (htOnItem in HT) or
            (htOnIcon in HT) then
             AttachMode := naAddChild
         else if htNowhere in HT then
             AttachMode := naAdd
         else if htOnIndent in HT then
             AttachMode := naInsert;
         Selected.MoveTo(AnItem, AttachMode) ;
       end;
       ExpandThis(aSourceId);
   end
   else
   begin
     // drag from another element
      AnItem := GetNodeAt(aX, aY) ;
      if Assigned(AnItem) then
         fReceiverNodeId:=GetData(AnItem).Id;
   end;
end;

procedure TwfTreeView.GetExpandingItems(var aNode: TTreeNode);
var
  aNewNode: TTreeNode;
  aData: TwfData;
  i: Integer;
begin
  aData:= nil;
  aNewNode:= nil;
  if not Assigned(aNode.Data) then exit;

   aData:= GetChildrens(GetData(aNode).Id);
   aNode.DeleteChildren;

  for i:=0 to aData.RowCount-1 do
    begin
      aNewNode:= Items.AddChildObject(aNode, fBase.AsString(aData.Data(i,fFieldName)), TwfTreeData.Create);
      WriteNodeData(aNewNode, aData, i);
    end;
  FreeAndNil(aData);
end;

function TwfTreeView.GetCurrentId: BaseID;
var
  aNode: TTreeNode;
begin
  Result:= EmptyBaseID;
  aNode:= Selected;
  if Assigned(aNode) and Assigned(aNode.Data) then
   Result:= GetData(aNode).Id;
end;

function TwfTreeView.GetNodesFromDB(const uSQLGet: string; const uID: BaseID
  ): TwfData;
var
  aParams: TwfParams;
begin
  aParams:= nil;
  Result:= nil;

  fBase.CreateParam(aParams,uSQLGet,true);
  aParams.ParamValues[fFieldId]:= uID;
  Result:= fBase.GetData(uSQLGet, aParams);
end;

function TwfTreeView.GetParrentId: BaseID;
var
  aNode: TTreeNode;
begin
  Result:= EmptyBaseID;
  aNode:= Selected;
  if Assigned(aNode) and Assigned(aNode.Data) then
   if aNode.Level = 0 then
     Result:= GetRootId
   else
     Result:= GetData(aNode).IdParent;

end;

function TwfTreeView.GetData(aNode: TTreeNode): TwfTreeData;
begin
  Result:= nil;
  if Assigned(aNode.Data) then
    Result:= TwfTreeData(aNode.Data);
end;

function TwfTreeView.GetRootId: BaseID;
var
  aData: TwfData;
begin
  aData:= nil;
  Result:= EmptyBaseID;
  aData:= fBase.GetData(Format(wSQLGetRoot.Text,[fTableName]));
  try
    if aData.RowCount=0 then
       raise Exception.Create(rsDBTreeErrorRootTreeNotFound);

    Result:= aData.Data(0,fFieldId);
  finally
    FreeAndNil(aData);
  end;
end;

procedure TwfTreeView.SetExpanded(aValue: boolean);
begin
  if fExpanded=aValue then exit; //=>
  fExpanded:=aValue;
  if aValue then
    Items[0].Expand(true)
  else
    Items[0].Expand(false);
end;

procedure TwfTreeView.Delete(Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    TwfTreeData(Node.Data).Free;
  inherited;
end;

procedure TwfTreeView.wfOnDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  fMoveNode:= true;
  wfDoDragDrop(Sender, Source, X, Y);
  fMoveNode:= false;
end;

procedure TwfTreeView.wfOnDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aNode: TTreeNode;
begin
  Accept:= ssCtrlDown or not (Source is TwfTreeView);

 if not (Source is TwfTreeView) then
   begin
      aNode:= GetNodeAt(X, Y);

      if aNode = nil then exit; //=>
      if  Selected<>aNode then
      begin
         ClearSelection(true);
         Selected:=aNode;
      end;
   end;

end;

function TwfTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  GetExpandingItems(Node);
  Result:= true;
  inherited;
end;

procedure TwfTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
   ssCtrlDown:= (ssCtrl in Shift);
   inherited;
end;

procedure TwfTreeView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  ssCtrlDown:=  (ssCtrl in Shift);
  inherited;
end;

procedure TwfTreeView.GetImageIndex(Node: TTreeNode);
begin
   if not Assigned(OnGetImageIndex) then
     if Node.Expanded then
     Node.ImageIndex:=1 else
     Node.ImageIndex:=0;

  inherited;
end;

procedure TwfTreeView.GetSelectedIndex(Node: TTreeNode);
begin
  if not Assigned(OnGetSelectedIndex) then
    if Node.Expanded then
    Node.SelectedIndex:=1 else
    Node.SelectedIndex:=0;

  inherited;
end;

procedure TwfTreeView.DoSelectionChanged;
begin
   fCurrentId:= GetCurrentId;
   fParentId:= GetParrentId;

   inherited;
   if Assigned(fOnGridFiltering) then fOnGridFiltering(self);
end;

constructor TwfTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  DragMode:= dmAutomatic;
  ReadOnly:= true;
  fAllowEditing:= false;
  MultiSelect:= true;

  fSQLGetRoot:= TStringList.Create;

  fFieldId:= 'ID';
  fFieldIdParent:= 'IDPARENT';
  fFieldName:= 'NAME';

  SetTableNameSQLGetRoot;

  fSQLGetFillList:= 'SELECT T1.*, (SELECT COUNT(*) FROM %s T WHERE T.IDPARENT=T1.ID) CCOUNT FROM %s T1 WHERE T1.ID=:ID';

  fSQLGetParentsAll:='with recursive tree '
     +'  as (select t.%s '
     +' from %s t '
     +'      where ID = :ID '
     +'      union all '
     +'      select t.%s '
     +'      from %s t '
     +'      inner join tree prior on prior.%s = t.ID) '
     {$IFDEF USEGUID}
       +'  select %s AS ID from tree WHERE %s<>''''';
     {$ELSE}
       +'  select %s AS ID from tree WHERE %s>0';
     {$ENDIF}

  fSQLGetChildrens:= 'SELECT T1.*, (SELECT COUNT(*) FROM %s T WHERE T.%s=T1.ID) CCOUNT FROM %s T1 WHERE T1.%s=:ID';
  //fSQLGetChildrensAll:= 'with recursive tree '
  //        +' as (select t.id '
  //        +'     from %s t '
  //        +'     where IDPARENT = :ID '
  //        +'     union all '
  //        +'     select t.id '
  //        +'     from %s t '
  //        +'     inner join tree prior on prior.id = t.IDPARENT) '
  //        +' select * from tree';

  fSQLNewNode:= 'INSERT INTO %s (%s, %s) VALUES(:%s, :%s) RETURNING ID';
  fSQLDeleteNode:= 'DELETE FROM %s WHERE ID=:ID';
  fSQLEditNode:= 'UPDATE %s SET NAME=:NAME WHERE ID=:ID RETURNING ID';
  fSQLDragNode:= 'UPDATE %s SET %s=:%s WHERE ID=:ID RETURNING ID';

  onDragDrop:=@wfOnDragDrop;
  OnDragOver:=@wfOnDragOver;

  Application.QueueAsyncCall(@AsyncInit,0);
end;


destructor TwfTreeView.Destroy;
begin
  FreeAndNil(fSQLGetRoot);
  inherited Destroy;
end;

procedure TwfTreeView.Fill(const uRootId: variant);
var
  aNewNode: TTreeNode;
  aData: TwfData;
  aParams: TwfParams;
  aSQL: String;
begin
  if not fBase.Connection.Connected then exit;

  aNewNode:= nil;
  aData:= nil;
  aParams:= nil;
  Items.Clear;

  if uRootId = nil then
    fRootId:= GetRootId;

   aSQL:= fBase.WriteOrderBy(fSQLGetFillList, fOrderByFields);

   fBase.CreateParam(aParams, aSQL, true);
   aParams.ParamValues[fFieldId]:= fRootId;
   aData:= fBase.GetData(Format(aSQL,[fTableName,fTableName]), aParams);

  try
    aNewNode:= Items.AddObject(nil, fBase.AsString(aData.Data(0,fFieldName)), TwfTreeData.Create);

    WriteNodeData(aNewNode, aData, 0);
  finally
    FreeAndNil(aData);
  end;

  GetExpandingItems(aNewNode);

  Items[0].Selected:= true;
  Items[0].Expand(false);

  if Assigned(fOnGridFiltering) then fOnGridFiltering(self);
  if Assigned(fOnFilled) then fOnFilled(self);
end;

function TwfTreeView.GetParents(uID: BaseID): TwfData;
begin
  Result:= GetNodesFromDB(Format(fSQLGetParentsAll,[fFieldIdParent,fTableName,fFieldIdParent, fTableName, fFieldIdParent, fFieldIdParent, fFieldIdParent]), uID);
end;

function TwfTreeView.GetChildrens(uID: BaseID; const uAll: boolean): TwfData;
var
  aSQL: String;
begin
  if uAll then
    aSQL:= Format(fSQLGetChildrensAll,[fTableName, fFieldIdParent, fTableName, fFieldIdParent])
  else
    aSQL:= Format(fSQLGetChildrens,[fTableName, fFieldIdParent, fTableName, fFieldIdParent]);

    aSQL:= fBase.WriteOrderBy(aSQL, fOrderByFields);
    Result:= GetNodesFromDB(aSQL, uID);
end;

function TwfTreeView.NewNode: BaseID;
var
  aSQL: String;
  aParams: TwfParams;
  aName: string;
  aParentId: BaseID;
  aNode: TTreeNode;
  Accept: Boolean;
begin
  if Items.Count = 0 then
   raise Exception.Create(rsDBTreeErrorItemsIsEmpty);

  Accept:= true;

  if Assigned(fOnAddBefore) then
    fOnAddBefore(self, Accept);

  if not Accept then exit;

  anode:= nil;

  aName:= rsDBTreeTextCreateNodeName;

  if not InputQuery(rsDBTreeTextCreateNodeCaption, rsDBTreeTextNodePromt, aName) then exit; //=>

  Result:= EmptyBaseID;
  aParams:= nil;

  if Assigned(wEntity) then
    aSQL:= wEntity.SQL[estItemNew]
  else
    aSQL:= Format(fSQLNewNode,[fTableName, fFieldIdParent, fFieldName, fFieldIdParent, fFieldName]);

  aParentId:= CurrentId;

  fBase.CreateParam(aParams, aSQL, true);

  NewNodeWriteParams(aParams, aParentId, aName);

  Result:= fBase.ExecSQL(aSQL,aParams,fFieldId);

  if FindNode(fCurrentId).Level = 0 then
     Fill(nil);

  aNode:= FindNode(fParentId);

  try
    if not Assigned(aNode) then
       raise Exception.Create(rsDBTreeErrorNodeIsEmpty);

    aNode.Expand(false);
  finally
    aNode:= nil;
  end;

  ExpandThis(Result);

end;

function TwfTreeView.EditNode: BaseID;
var
  aSQL: String;
  aParams: TwfParams;
  aName: string;
  aNode: TTreeNode;
  Accept: Boolean;
begin
  if Items.Count = 0 then
   raise Exception.Create(rsDBTreeErrorItemsIsEmpty);

  if CurrentId = fRootId then exit; //=>

  Accept:= true;

  if Assigned(fOnEditBefore) then
    fOnEditBefore(self, Accept);

  if not Accept then exit;

  aNode:= FindNode(CurrentId);

  if not Assigned(aNode) then
   raise Exception.Create(rsDBTreeErrorNodeIsEmpty);

  try
    aName:= aNode.Text;

    if not InputQuery(rsDBTreeTextEditNodeCaption,rsDBTreeTextNodePromt, aName) then exit; //=>

    Result:= EmptyBaseID;
    aParams:= nil;

    if Assigned(wEntity) then
      aSQL:= wEntity.SQL[estItemUpdate]
    else
      aSQL:= Format(fSQLEditNode,[fTableName]);

    fBase.CreateParam(aParams, aSQL, true);

    aParams.ParamValues[fFieldName]:= aName;
    aParams.ParamValues[fFieldId]:= CurrentId;

    Result:= fBase.ExecSQL(aSQL,aParams,fFieldId);

    aNode.Text:= aName;

    ExpandThis(Result);
  finally
    aNode:= nil;
  end;

end;

function TwfTreeView.DeleteNode: boolean;
var
  aNode: TTreeNode;
  aSQL, aDialogText: String;
  aParams: TwfParams;
  aSelectedItems, aSelectedMouseNodes: ArrayOfBaseID;
  i: Integer;
  Accept: Boolean;
begin
  Accept:= true;
  Result:= false;

  if Assigned(fOnDeleteBefore) then
    fOnDeleteBefore(self, Accept);

  if not Accept then exit;

  aSelectedItems:= nil;
  aNode:= nil;

  if CurrentId = fRootId then exit; //=>

  if Items.Count = 0 then
   raise Exception.Create(rsDBTreeErrorItemsIsEmpty);

  aNode:= FindNode(CurrentId);

  aSelectedItems:= SelectedItems();
  aSelectedMouseNodes:= SelectedItems(false);

  if Length(aSelectedMouseNodes)>1 then
    aDialogText:= Format(rsDBTreeTextDeleteNodePromt,['('+IntToStr(Length(aSelectedMouseNodes))+')'])
  else
    aDialogText:= Format(rsDBTreeTextDeleteNodePromt,[aNode.Text]);

    if MessageDlg(rsDBTreeTextDeleteNodeCaption,aDialogText,mtConfirmation,[mbOK, mbCancel], 0) = mrCancel then exit; //=>

  try

    if not Assigned(aNode) then
     raise Exception.Create(rsDBTreeErrorNodeIsEmpty);

    aNode:= aNode.GetPrevSibling;

    if not Assigned(aNode) then
       aNode:= FindNode(fParentId);


      aParams:= nil;

      if Assigned(wEntity) then
        aSQL:= wEntity.SQL[estItemDel]
      else
        aSQL:= Format(fSQLDeleteNode,[fTableName]);

      fBase.CreateParam(aParams, aSQL);

      fBase.LongTransactionStart;

      for i:=0 to High(aSelectedItems) do
      begin
        aParams.ParamValues[fFieldId]:= aSelectedItems[i];
        Result:= fBase.ExecSQL(aSQL,aParams,'');
      end;

      for i:=0 to High(aSelectedMouseNodes) do
        FindNode(aSelectedMouseNodes[i]).Delete;

      fBase.LongTransactionCommit;
      ExpandThis(GetData(aNode).Id);

  finally
    FreeAndNil(aParams);
    aNode:= nil;
  end;

end;

function TwfTreeView.FindNode(const uID: BaseID): TTreeNode;
var
  aSearching: Boolean;
begin
  Result := Items[0];
  aSearching := true;
  while (aSearching) and (Result <> nil) do
  begin
    if GetData(Result).Id = uID then
    begin
      aSearching := False;
    end
    else
    begin
      Result := Result.GetNext
    end;
  end;

end;

function TwfTreeView.SelectedItems(const uAll: boolean): ArrayOfBaseID;
var
  aData: TwfData;
  i: Integer;
  aSelectedList: TBaseIDList;

  procedure ShowChilds (aNode: TTreeNode);
  var
    iData: Integer;
  begin
    aData:= GetChildrens(GetData(aNode).Id, true);
    try
      for iData:=0 to aData.RowCount-1 do
        aSelectedList.Add(aData.Data(iData,0));

    finally
      FreeAndNil(aData);
    end;
  end;

begin
  if not Assigned(self) or not Assigned(Selected) then exit; //=>

  aSelectedList:= TBaseIDList.Create;
  Result:= nil;
  try
       for i:=0 to Items.Count-1 do
         if  Items[i].Selected then
         begin
           if not Assigned(Items[i].Data) then break;

           aSelectedList.Add(GetData(Items[i]).Id);
           if uAll and (Selected.Level <> 0) then ShowChilds(Items[i]);
         end;

       SetLength(Result, aSelectedList.Count);

       for i:=0 to aSelectedList.Count-1 do
         Result[i]:= aSelectedList.Items[i];
  finally
    FreeAndNil(aSelectedList);
  end;
end;

procedure TwfTreeView.ExpandThis(const uId: BaseID);
var
  aData: TwfData;
  aNode: TTreeNode;
  i: Integer;
begin
  aData:= GetParents(uId);
  if not Assigned(aData) then exit;
  try
    for i:=aData.RowCount-1 downto 0 do
    begin
      aNode:= FindNode(aData.Data(i,fFieldId));
      if Assigned(aNode) and (i>=0) then
        aNode.Expanded:= true;
    end;

    aNode:= FindNode(uId);
    if Assigned(aNode) then
    begin
      Select(aNode);
      aNode.Expanded:= true;
    end;
    SetFocus;
  finally
    FreeAndNil(aData);
  end;
end;

initialization
  {$i wftreeview_icon.lrs}

  wfTree_Images := TImageList.Create(nil);

  with wfTree_Images do
  begin
    AddLazarusResource('folder');
    AddLazarusResource('folder_open');
    AddLazarusResource('add');
    AddLazarusResource('edit');
    AddLazarusResource('del');
  end;

finalization
  wfTree_Images.Free();

end.
