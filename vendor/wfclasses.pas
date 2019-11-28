{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfClasses;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, fgl, wfParamsU,
  UITypes, Buttons, fpsTypes, db;

type

   // Запись строка + колонка
   TwfRowCol = Record
     Row: Cardinal;
     Col: Cardinal;
   end;
   { TwfBaseField }

   TwfBaseField = record
     Value: variant;
   end;

   TwfBaseFields = array of TwfBaseField;

   { TwfSQLRecord }

   TwfSQLRecord = record
     aText: string;
     aParams: TwfParams;
   end;

   TwfBaseRow = record
     Index: LargeInt;
     Fields: TwfBaseFields;
   end;

   TwfBaseRows = array of TwfBaseRow;

   TwfField = record
     Name: string;
     DataType: TFieldType;
   end;

   TwfFields = array of TwfField;

   TwfContentCell = Record
     Name: string;
     Field: string;
     Value: variant;
     Color: TsColor;
   end;

   TwfGroupCell = Record
     Name: string;
     Field: string;
     Value: variant;
     IsCurrent: boolean;
     Color: TsColor;
   end;

   TwfContentRow = Record
     Row: array of TwfContentCell;
     GroupIndex: Integer;
   end;

implementation


end.

