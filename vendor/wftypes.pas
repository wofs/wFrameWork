{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfTypes;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, ComCtrls, MultiLog, wfClasses, db, gvector;

type
  {$IFDEF USEGUID}
    BaseID = string[32];
  {$ELSE}
    BaseID = Int64;
  {$ENDIF}
  img = integer;

  ArrayOfInteger  = array of integer;
  ArrayOfInt64    = array of int64;
  ArrayOfDouble   = array of double;
  ArrayOfCurrency   = array of currency;
  ArrayOfString   = array of string;
  ArrayOfChar   = array of char;
  ArrayOfConst    = array of TVarRec;
  ArrayOfVariant  = array of variant;
  ArrayOfDateTime = array of TDateTime;

  ArrayOfBaseID     = array of BaseID;

  ArrayOfArrayVariant = array of array of variant;
  ArrayOfArrayInteger = array of array of integer;

  TwfGroups = specialize TVector<TwfGroupCell>;
  TInt64List = specialize TFPGList<Int64>;
  TBaseIDList = specialize TFPGList<BaseID>;

  TErrorEvent = procedure(Sender: TObject; const E: Exception) of object;
  TVarTextEvent = procedure(Sender: TObject; var aValue: string) of object;
  TTextEvent = procedure(Sender: TObject; const aValue: string) of object;
  TVariantEvent = procedure(Sender: TObject; const aValue: variant) of object;
  TBaseEvent = procedure(Sender: TObject; const aValue: BaseID) of object;
  TStringsEvent = procedure(Sender: TObject; const aText: string; const aStrings: TStrings) of object;
  TResultEvent = procedure(Sender: TObject; const aResult: boolean) of object;
  TResultsEvent = procedure(Sender: TObject; const aResults: ArrayOfInt64) of object;
  TResultCountEvent = procedure(Sender: TObject; const aCount: Integer) of object;
  TDataSourceEvent = procedure(Sender: TObject; const aDS: TDataSource) of object;
  TAcceptWhereEvent = procedure(Sender: TObject; const aWhere: string) of object;
  TAcceptEvent = procedure(Sender: TObject; var Accept: boolean) of object;
  TDefaultEvent = procedure(Sender: TObject) of object;
  TCollectionItemNotify = procedure (Item: TCollectionItem; Action: TCollectionNotification) of object;

  TProgressEvent = procedure(Sender: TObject; const aPosition: integer) of object;
  TProgressInitEvent = procedure(Sender: TObject; const aMax, aStep: integer) of object;
  TProgressMarqueeEvent = procedure(Sender: TObject; const aMarquee: Boolean) of object;

  TwfWriteContentRowEvent = procedure (Sender: TObject; aGroups: TwfGroups; aContentRow: TwfContentRow) of object;


  TDataType = (dtDefault, dtNumber, dtString, dtComplex, dtCalculated, dtNotUsed);
  TBaseTaskType = (bttArray, bttDataSource, bttDataSet);
  TDirection = (dNone, dASC, dDESC);
  TSQLItemType = (stLike, stContaining, stIn, stUnknown);
  TwfSQLEngine = (seFirebird, seODBC, sePostgreSQL, seUnknown);

  TwfEntitySQLPresets = (spNone, spTree, spList, spShortList);
  TwfEntitySQLType = (estList, estListFull, estListShort, estItemDel, estItemGet, estItemNew, estItemUpdate, estTreeGetRoot, estTreeDragNode);
  TwfEntityScriptType = (esctCreate, esctDrop);

  TwfLogClass =(lcDebug, lcError, lcInfo, lcWarning, lcEvent);
  TwfLogClasses = set of TwfLogClass;

  TwfStringsEditorMode = (stmSQL, stmIni, stmText);

  TwfCellAdressType = (catCharOnly, catNumberOnly, catFull);


const
  wfLE = LineEnding;
  wfEmptyStr = '';
  wfEmptyDouble = 0.0;
  wfEmptyInt = 0;
  wfEmptyDateTime = wfEmptyDouble;

  {$IFDEF USEGUID}
    wfEmptyBaseID = '';
  {$ELSE}
    wfEmptyBaseID = 0;
  {$ENDIF}

implementation




end.

