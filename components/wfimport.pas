unit wfImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, PropEdits, LazUTF8, wfBase, wfEntity,
  wfTypes, wfClasses, wfThreadU, wfResourceStrings, TwfProgressU, wfDesignSQLItemsU, wfSQLPropertyEditorU;

type

  TwfImportType = (itSpreadSheet, itCSV);

  TwfImportItem = class;

  TwfImportExecuteEvent =  procedure(aImport: TwfImportItem; const Msg: Word; var Param: Variant) of object;
  TwfImportFinishEvent = procedure(aImport: TwfImportItem; const Msg: Word; const Param: Variant) of object;
  TwfImportProgress = procedure(aImport: TwfImportItem; const Msg: Word; const Value: Word) of object;
  TwfImportMessage = procedure(aImport: TwfImportItem; const Msg: Word; const Param: Variant) of object;

  TwfImportThread = class(TwfThread);

  { TwfFormatItem }

  TwfFormatItem = class(TCollectionItem)
    private
      fDescription: TStrings;
      fName: string;

    protected

    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

    published
      property Name: string read fName write fName;
      property Description: TStrings read fDescription write fDescription;
  end;

  { TwfFormatItems }

  TwfFormatItems = class(TOwnedCollection)
    protected

    public
      function ItemByName(aName: string):TwfFormatItem;

    published
      property OwnerComponent: TPersistent read GetOwner;
  end;

  { TwfImportItem }

  TwfImportItem = class(TCollectionItem)

  private
      fDescription: string;
      fEntity: TwfEntity;
      fFormats: TwfFormatItems;
      fImportType: TwfImportType;
      fProgressBarStyle: TProgressBarStyle;
      fSource: string;
      fTerminated: boolean;

      fImportThread: TwfImportThread;
      fProgress: TwfProgress;
      fName: string;
      fonExecute: TwfImportExecuteEvent;
      fonFinish: TwfImportFinishEvent;
      fonForceFinish: TwfThreadNotify;
      fonMessage: TwfImportMessage;
      fonProgress: TwfImportProgress;
      fSQLItems: TwfDesignSQLItems;
      fUseProgressBar: boolean;

      function GetEntity: TwfEntity;
      procedure ImportExecute(Sender: TwfThread; const Msg: Word; var Param: Variant);
      procedure ImportFinish(Sender: TwfThread; const Msg: Word; const Param: Variant);
      procedure ImportMessage(Sender: TwfThread; const Msg: Word; const Param: Variant);
      procedure ImportProgress(Sender: TwfThread; const Msg: Word; const Value: Word);
      function ThreadInit: boolean;

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
      property OwnerComponent: TPersistent read GetOwner;

    published
      property SQLItems: TwfDesignSQLItems read fSQLItems write fSQLItems;
      property ImportType: TwfImportType read fImportType write fImportType;
      property Source: string read fSource write fSource;

      property Name: string read fName write fName;
      property Entity: TwfEntity read GetEntity write fEntity;
      property Description: string read fDescription write fDescription;
      property Formats: TwfFormatItems read fFormats write fFormats;

      //Use ProgressBar
      //Use Task.PostProgress() and aImport.ProgressInit() procedure to control the ProgressBar
      property UseProgressBar: boolean read fUseProgressBar write fUseProgressBar default false;
      property ProgressBarStyle: TProgressBarStyle read fProgressBarStyle write fProgressBarStyle default pbstNormal;

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
  private
    protected

    public
      function ItemByName(aName: string):TwfImportItem;

      property OwnerComponent: TPersistent read GetOwner;
    published

  end;


  { TwfImport }

  TwfImport = class(TComponent)
  private
    fBase: TwfBase;
    fItems: TwfImportItems;
    fRootPath: String;
    fSilentMode: boolean;
    function DialogsOpenLoadDialog(aCaption: string; aFilter: string; const aFilterIndex: word=1): string;
    function GetBase: TwfBase;
    function GetIsDesigning: boolean;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Running(aImportName: string): boolean;
    procedure Start(aImportName: string);
    procedure Stop(aImportName: string);

    function GetRootPath:string;

    property OwnerComponent: TPersistent read GetOwner;
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
  RegisterPropertyEditor(TypeInfo(TStrings), TwfFormatItem, 'Description', TwfSQLPropertyEditor);
end;

{ TwfFormatItem }

constructor TwfFormatItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDescription:= TStringList.Create;
end;

destructor TwfFormatItem.Destroy;
begin
  FreeAndNil(fDescription);
  inherited Destroy;
end;

{ TwfFormatItems }

function TwfFormatItems.ItemByName(aName: string): TwfFormatItem;
var
  i: Integer;
begin
  Result:= nil;

  for i:=0 to Count-1 do
   if TwfFormatItem(Items[i]).Name = aName then
     begin
       Result:= TwfFormatItem(Items[i]);
       Break;
     end;

  if not Assigned(Result) then
    raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));
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

function TwfImportItem.ThreadInit: boolean;
begin
  Result:= false;
  if Assigned(fImportThread) then exit;

  fImportThread:= TwfImportThread.Create(true);
  with fImportThread do begin
    onExecute:= @ImportExecute;
    onFinish:= @ImportFinish;
    onProgress:= @ImportProgress;
    onMessage:= @ImportMessage;
  end;
  Result:= true;
end;

procedure TwfImportItem.ImportExecute(Sender: TwfThread; const Msg: Word; var Param: Variant);
begin
  if Assigned(fonExecute) then fonExecute(self, Msg, Param)
  { TODO : onExecute прописать! }
  //else
  //  case fImportType of
  //    itCSV          : ImportExecuteDefaultCSV;
  //    itSpreadSheet  : ImportExecuteDefaultSpreadSheet;
  //  end;
end;

function TwfImportItem.GetEntity: TwfEntity;
begin
  if not Assigned(fEntity) then fEntity:= nil;
  Result:= fEntity;
end;

procedure TwfImportItem.ImportFinish(Sender: TwfThread; const Msg: Word; const Param: Variant);
begin
  if not Terminated then
    Terminated:= true;

  fImportThread:= nil;

  if Assigned(fonFinish) then fonFinish(self, Msg, Param);

  if Assigned(fProgress) then
      fProgress.ForceClose;
end;

procedure TwfImportItem.ImportMessage(Sender: TwfThread; const Msg: Word; const Param: Variant);
begin
  if Assigned(fonMessage) then fonMessage(self, Msg, Param);
end;

procedure TwfImportItem.ImportProgress(Sender: TwfThread; const Msg: Word; const Value: Word);
begin
  if Assigned(fProgress) then
    fProgress.SetBar(Value);
  if Assigned(onProgress) then onProgress(self, Msg, Value);
end;

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
  fFormats:= TwfFormatItems.Create(self, TwfFormatItem);

  fImportThread:= nil;
  fTerminated:= true;
  fImportType:= itSpreadSheet;
  fProgress:= nil;
end;

destructor TwfImportItem.Destroy;
begin
  FreeAndNil(fSQLItems);
  FreeAndNil(fFormats);
  inherited Destroy;
end;

{ TwfImport }

function TwfImport.GetBase: TwfBase;
begin
  if not Assigned(fBase) then fBase:= nil;
  Result:= fBase;
end;

function TwfImport.GetIsDesigning: boolean;
begin
  Result:= csDesigning in ComponentState;
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

function TwfImport.Running(aImportName: string): boolean;
var
  aImport: TwfImportItem;
begin
  aImport:= nil;
  aImport:= fItems.ItemByName(aImportName);

  Result:= Assigned(aImport) and Assigned(aImport.Import);

end;

function TwfImport.DialogsOpenLoadDialog(aCaption: string; aFilter: string; const aFilterIndex: word = 1): string;
var
  OpenDialog: TOpenDialog;
begin
  Result:= '';
  OpenDialog:= TSaveDialog.Create(nil);
  try
    OpenDialog.Filter:=aFilter;//'CSV (*.csv)|*.csv';
    OpenDialog.FilterIndex:=aFilterIndex;
    OpenDialog.FileName:= aCaption;
    OpenDialog.Title:= aCaption;

    if OpenDialog.Execute then
      Result:= OpenDialog.FileName;
  finally
    FreeAndNil(OpenDialog);
  end;
end;

procedure TwfImport.Start(aImportName: string);
var
  aImport: TwfImportItem;
  aDialogFilter: String;
begin
  aImport:= fItems.ItemByName(aImportName);
  if Assigned(aImport) then
    begin
      if aImport.SQLItems.Count = 0 then
        raise Exception.Create(rsImportNoSQLQuerySpecified);

      if (UTF8Length(aImport.Source)=0) and (not SilentMode) then
        begin
          if SilentMode then
            raise Exception.Create(rsImportNoSourceSpecified);

          case aImport.ImportType of
            itSpreadSheet     : aDialogFilter:= 'OpenDocument (*.ods)|*.ods|Excel (*.xls)|*.xls|Excel (*.xlsx)|*.xlsx|Comma Text (*.csv)|*.csv';
            itCSV             : aDialogFilter:= 'Comma Text (*.csv)|*.csv'
            else
              aDialogFilter:='';
          end;

          aImport.Source:= DialogsOpenLoadDialog('', aDialogFilter);

          if UTF8Length(aImport.Source)=0 then
            exit;
        end;

      if aImport.ThreadInit then
        begin
          with aImport do begin
            if UseProgressBar then
              begin
                fProgress:= TwfProgress.Create(nil);
                  with fProgress do begin
                    onStopForce:=@ImportForceFinish;
                    Bar.Style:= ProgressBarStyle;
                    ShowInTaskBar:= stAlways;
                    if UTF8Length(fDescription)>0 then
                      Caption:= fDescription;
                    Show;
                  end;
              end;

            fTerminated:= false;
            Import.Start;
          end;
        end;
    end;
end;

procedure TwfImport.Stop(aImportName: string);
var
  aImport: TwfImportItem;
begin
  aImport:= fItems.ItemByName(aImportName);
  if Assigned(aImport) and Assigned(aImport.Import) then
     aImport.ThreadStop;

end;

function TwfImport.GetRootPath: string;
begin
  Result:= fRootPath;
end;


end.
