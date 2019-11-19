{
This file is part of wfFrameWork.

 -= Vendor/Types =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfTypes;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, ComCtrls, MultiLog, db, gvector;

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
  ArrayOfConst    = array of TVarRec;
  ArrayOfVariant  = array of variant;
  ArrayOfDateTime = array of TDateTime;

  ArrayOfBaseID     = array of BaseID;

  ArrayOfArrayVariant = array of array of variant;
  ArrayOfArrayInteger = array of array of integer;

  TErrorEvent = procedure(Sender: TObject; const E: Exception) of object;
  TVarTextEvent = procedure(Sender: TObject; var aValue: string) of object;
  TTextEvent = procedure(Sender: TObject; const aValue: string) of object;
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

  TInt64List = specialize TFPGList<Int64>;
  TBaseIDList = specialize TFPGList<BaseID>;

  TValueType = (vtDefault, vtNumber, vtString);
  TBaseTaskType = (bttArray, bttDataSource, bttDataSet);
  TDirection = (dNone, dASC, dDESC);
  TSQLItemType = (stLike, stContaining, stIn, stUnknown);
  TwfSQLEngine = (seFirebird, seODBC, sePostgreSQL, seUnknown);

  TwfEntitySQLPresets = (spNone, spTree, spList);
  TwfEntitySQLType = (estList, estListFull, estListShort, estItemDel, estItemGet, estItemNew, estItemUpdate, estTreeGetRoot, estTreeDragNode);
  TwfEntityScriptType = (esctCreate, esctDrop);

  TwfLogClass =(lcDebug, lcError, lcInfo, lcWarning, lcEvent);
  TwfLogClasses = set of TwfLogClass;

const
  wfLE = #10;
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

