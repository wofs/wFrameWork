{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfSQLScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfClasses, wfSQLPropertyEditorU, PropEdits, sqldb;

type
  TwfSQLScript = class(TSQLScript)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfsqlscript_icon.lrs}
  RegisterComponents('WF',[TwfSQLScript]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfSQLScript, 'Script', TwfSQLPropertyEditor);
end;

end.
