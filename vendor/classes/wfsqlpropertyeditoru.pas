{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfSQLPropertyEditorU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfStringsEditor, wfStringsPropertyEditor, wfTypes, PropEdits;

type

  { TwfSQLPropertyEditor }

  TwfSQLPropertyEditor = class(TwfStringsPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;

  end;

implementation


{ TwfSQLPropertyEditor }

constructor TwfSQLPropertyEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  Mode:= stmSQL;
end;

end.

