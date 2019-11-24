{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfDialogs;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, Dialogs;

function DialogsOpenSaveDialog(aCaption: string; aFilter: string; const aFilterIndex: word = 1): string;

implementation

function DialogsOpenSaveDialog(aCaption: string; aFilter: string; const aFilterIndex: word = 1): string;
var
  SaveDialog: TSaveDialog;
begin
  Result:= '';
  SaveDialog:= TSaveDialog.Create(nil);
  try
    SaveDialog.Options:= [ofOverwritePrompt];
    SaveDialog.Filter:=aFilter;//'CSV (*.csv)|*.csv';
    SaveDialog.FilterIndex:=aFilterIndex;
    SaveDialog.FileName:= aCaption;
    SaveDialog.Title:= aCaption;

    if SaveDialog.Execute then
      Result:= SaveDialog.FileName;
  finally
    FreeAndNil(SaveDialog);
  end;
end;

end.

