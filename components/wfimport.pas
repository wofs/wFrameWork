unit wfImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfBase, wfTypes, wfClasses;

type

  //TwfImportType = (itSpreadSheet, itCSV);
  //
  //TwfImportItem = class;
  //
  //TwfImportExecuteEvent =  procedure(aReport: TwfImportItem; const Msg: Word; var Param: Variant) of object;
  //TwfImportFinishEvent = procedure(aReport: TwfImportItem; const Msg: Word; const Param: Variant) of object;
  //TwfImportProgress = procedure(aReport: TwfImportItem; const Msg: Word; const Value: Word) of object;
  //TwfImportMessage = procedure(aReport: TwfImportItem; const Msg: Word; const Param: Variant) of object;
  //
  //
  //  { TwfImportThread }
  //
  //TwfImportItem = class(TCollectionItem)
  //  private
  //
  //  protected
  //
  //  published
  //    property Name: string read fName write fName;
  //    property Description: string read fDescription write fDescription;
  //    property FirstRow: word read fFirstRow write fFirstRow;
  //    property FirstCol: word read fFirstCol write fFirstCol;
  //
  //    //Use ProgressBar
  //    //Use Task.PostProgress() and aReport.ProgressInit() procedure to control the ProgressBar
  //    property UseProgressBar: boolean read fUseProgressBar write fUseProgressBar default false;
  //    property ProgressBarStyle: TProgressBarStyle read fProgressBarStyle write fProgressBarStyle default pbstNormal;
  //
  //    property ImportType: TwfImportType read fImportType write fImportType default itSpreadSheet;
  //    //Import Dir
  //    property ImportDir: string read fImportDir write fImportDir;
  //    //Import FileName
  //    property ImportFileName: string read fImportFileName write fImportFileName;
  //    //onExecute
  //    //Use PostProgress or PostMessage to communicate with LCL
  //    //If not assigned, export will be performed by default
  //    //If the export is stopped (as wel onForceFinish), the will be set Task.Terminated:= true;
  //    property onExecute: TwfReportExecuteEvent read fonExecute write fonExecute;
  //    //Event indicating completion of report generation
  //    property onFinish: TwfReportFinishEvent read fonFinish write fonFinish;
  //    //Event marked the premature end of report
  //    property onForceFinish: TwfThreadNotify read fonForceFinish write fonForceFinish;
  //    //Event, PostProgress
  //    property onProgress: TwfReportProgress read fonProgress write fonProgress;
  //    //Event, PostMessage
  //    property onMessage: TwfReportMessage read fonMessage write fonMessage;
  //
  //end;
  //
  //{ TwfImportItems }
  //
  //TwfImportItems = class(TOwnedCollection)
  //  public
  //    function ItemByName(aName: string):TwfImportItem;
  //
  //  published
  //    property OwnerComponent: TPersistent read GetOwner;
  //end;
  //
  { TwfImport }

  TwfImport = class(TComponent)
  private
    //fBase: TwfBase;
    //fItems: TwfImportItemsItems;
    //fRootPath: String;
    //fSilentMode: boolean;
    //function GetBase: TwfBase;
    //
  protected

  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    //
    //function Running(aImportName: string): boolean;
    //procedure Start(aImportName: string);
    //procedure Stop(aImportName: string);
    //function GetRootPath:string;
  published
    //property Items: TwfImportItemsItems read fItems write fItems;
    ////wfBase to work with database
    //property Base: TwfBase read GetBase write fBase;
    //// Disables all questions to the user during export.
    ////An exception will be thrown if there is a lack of data.
    //property SilentMode: boolean read fSilentMode write fSilentMode default false;
  end;


procedure Register;

implementation

procedure Register;
begin
  {$I wfimport_icon.lrs}
  RegisterComponents('WF',[TwfImport]);
end;

//{ TwfImportItems }
//
//function TwfImportItems.ItemByName(aName: string): TwfImportItem;
//var
//  i: Integer;
//begin
//  Result:= nil;
//
//  for i:=0 to Count-1 do
//   if TwfImportItem(Items[i]).Name = aName then
//     begin
//       Result:= TwfImportItem(Items[i]);
//       Break;
//     end;
//
//  if not Assigned(Result) then
//    raise Exception.Create(Format(rsExceptObjectNotAssigned,['']));
//end;
//
//{ TwfImport }
//
//function TwfImport.GetBase: TwfBase;
//begin
//  if not Assigned(fBase) then fBase:= nil;
//    Result:= fBase;
//end;
//
//constructor TwfImport.Create(AOwner: TComponent);
//begin
//  inherited Create(AOwner);
//
//  fItems:= TwfImportItems.Create(self, TwfImportItem);
//  fSilentMode:= false;
//  fRootPath:= IncludeTrailingBackslash(ExtractFileDir(Application.ExeName));
//end;
//
//destructor TwfImport.Destroy;
//begin
//  FreeAndNil(fItems);
//  inherited Destroy;
//end;
//
//function TwfImport.Running(aImportName: string): boolean;
//var
//  aImport: TwfImportItem;
//begin
//  aImport:= nil;
//  aImport:= fItems.ItemByName(aImportName);
//
//  Result:= Assigned(aImport) and Assigned(aImport.Report);
//
//end;
//
//procedure TwfImport.Start(aImportName: string);
//begin
//  { TODO 1 -owofs : Записать процедуру старта импорта. }
//
//end;
//
//procedure TwfImport.Stop(aImportName: string);
//var
//  aImport: TwfImportItem;
//begin
//  aImport:= fItems.ItemByName(aImportName);
//  if Assigned(aImport) and Assigned(aImport.Report) then
//     aImport.ThreadStop;
//end;
//
//function TwfImport.GetRootPath: string;
//begin
//  Result:= fRootPath;
//end;

end.
