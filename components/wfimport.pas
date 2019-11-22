unit wfImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, PropEdits, wfBase, wfTypes, wfClasses, wfThreadU,
  wfResourceStrings, wfDesignSQLItemsU, wfSQLPropertyEditorU;

type

  TwfImportType = (itSpreadSheet, itCSV);

  TwfImportItem = class;

  TwfImportExecuteEvent =  procedure(aReport: TwfImportItem; const Msg: Word; var Param: Variant) of object;
  TwfImportFinishEvent = procedure(aReport: TwfImportItem; const Msg: Word; const Param: Variant) of object;
  TwfImportProgress = procedure(aReport: TwfImportItem; const Msg: Word; const Value: Word) of object;
  TwfImportMessage = procedure(aReport: TwfImportItem; const Msg: Word; const Param: Variant) of object;

  TwfImportThread = class(TwfThread);

  { TwfImportItem }

  TwfImportItem = class(TCollectionItem)

  private
      fTerminated: boolean;

      fImportThread: TwfImportThread;
      fName: string;
      fonExecute: TwfImportExecuteEvent;
      fonFinish: TwfImportFinishEvent;
      fonForceFinish: TwfThreadNotify;
      fonMessage: TwfImportMessage;
      fonProgress: TwfImportProgress;
      fSQLItems: TwfDesignSQLItems;

      procedure SetTerminated(aValue: boolean);
      procedure ThreadStop;
      procedure ImportForceFinish(Sender: TObject);
      procedure AllTasksFinished(const Sender: TwfThread);

      property Terminated: boolean read fTerminated write SetTerminated;

    protected
      function GetDisplayName: string; override;

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

      property Import: TwfImportThread read fImportThread;

    published
      property SQLItems: TwfDesignSQLItems read fSQLItems write fSQLItems;
      property Name: string read fName write fName;
      //onExecute
      //Use PostProgress or PostMessage to communicate with LCL
      //If not assigned, export will be performed by default
      //If the export is stopped (as wel onForceFinish), the will be set Task.Terminated:= true;
      property onExecute: TwfImportExecuteEvent read fonExecute write fonExecute;
      //Event indicating completion of report generation
      property onFinish: TwfImportFinishEvent read fonFinish write fonFinish;
      //Event marked the premature end of report
      property onForceFinish: TwfThreadNotify read fonForceFinish write fonForceFinish;
      //Event, PostProgress
      property onProgress: TwfImportProgress read fonProgress write fonProgress;
      //Event, PostMessage
      property onMessage: TwfImportMessage read fonMessage write fonMessage;
  end;

  { TwfImportItems }

  TwfImportItems = class(TOwnedCollection)
    protected

    public
      function ItemByName(aName: string):TwfImportItem;

    published
      property OwnerComponent: TPersistent read GetOwner;
  end;


  { TwfImport }

  TwfImport = class(TComponent)
  private
    fBase: TwfBase;
    fItems: TwfImportItems;
    fRootPath: String;
    fSilentMode: boolean;
    function GetBase: TwfBase;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Running(aName: string): boolean;
    procedure Start(aName: string);
    procedure Stop(aName: string);
    function GetRootPath:string;
  published
    property Items: TwfImportItems read fItems write fItems;
    //wfBase to work with database
    property Base: TwfBase read GetBase write fBase;
    // Disables all questions to the user during export.
    //An exception will be thrown if there is a lack of data.
    property SilentMode: boolean read fSilentMode write fSilentMode default false;
  end;


procedure Register;

implementation

procedure Register;
begin
  {$I wfimport_icon.lrs}
  RegisterComponents('WF',[TwfImport]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfDesignSQLItem, 'SQL', TwfSQLPropertyEditor);
end;

{ TwfImportItems }

function TwfImportItems.ItemByName(aName: string): TwfImportItem;
var
  i: Integer;
begin
  Result:= nil;

  for i:=0 to Count-1 do
   if TwfImportItem(Items[i]).Name = aName then
     begin
       Result:= TwfImportItem(Items[i]);
       Break;
     end;

  if not Assigned(Result) then
    raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));

end;

{ TwfImportItem }

procedure TwfImportItem.SetTerminated(aValue: boolean);
begin
  if fTerminated=AValue then Exit;

  if not fTerminated and Assigned(fImportThread) then
    fImportThread.Terminate;

  fTerminated:=AValue;
end;

procedure TwfImportItem.ThreadStop;
begin
  ImportForceFinish(self);
end;

procedure TwfImportItem.ImportForceFinish(Sender: TObject);
begin
  AllTasksFinished(fImportThread);
end;

procedure TwfImportItem.AllTasksFinished(const Sender: TwfThread);
begin
  Terminated:= true;
 if Assigned(onForceFinish) then onForceFinish(Sender);
end;

function TwfImportItem.GetDisplayName: string;
begin
 if Length(fName)>0 then
   Result:= fName
 else
   Result:=inherited GetDisplayName;
end;

constructor TwfImportItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fSQLItems:= TwfDesignSQLItems.Create(self, TwfDesignSQLItem);
end;

destructor TwfImportItem.Destroy;
begin
  FreeAndNil(fSQLItems);
  inherited Destroy;
end;

{ TwfImport }

function TwfImport.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

constructor TwfImport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems:= TwfImportItems.Create(self, TwfImportItem);
  fSilentMode:= false;
  fRootPath:= IncludeTrailingBackslash(ExtractFileDir(Application.ExeName));
end;

destructor TwfImport.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TwfImport.Running(aName: string): boolean;
var
  aImport: TwfImportItem;
begin
  aImport:= nil;
  aImport:= fItems.ItemByName(aName);

  Result:= Assigned(aImport) and Assigned(aImport.Import);

end;

procedure TwfImport.Start(aName: string);
begin
  { TODO : Процедура старта импорта }

end;

procedure TwfImport.Stop(aName: string);
var
  aImport: TwfImportItem;
begin
  aImport:= fItems.ItemByName(aName);
  if Assigned(aImport) and Assigned(aImport.Import) then
     aImport.ThreadStop;

end;

function TwfImport.GetRootPath: string;
begin
  Result:= fRootPath;
end;


end.
