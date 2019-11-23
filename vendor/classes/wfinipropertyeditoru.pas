unit wfIniPropertyEditorU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfStringsEditor, wfStringsPropertyEditor, wfTypes, PropEdits;

type

  { TwfIniPropertyEditor }

  TwfIniPropertyEditor = class(TwfStringsPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;

  end;

implementation

{ TwfIniPropertyEditor }

constructor TwfIniPropertyEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  Mode:= stmIni;
end;

end.

