unit wfSQLPropertyEditorU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, UITypes, wfSQLEditor;

type
  { TwfSQLPropertyEditor }

  TwfSQLPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

implementation

{ TwfSQLPropertyEditor }

procedure TwfSQLPropertyEditor.Edit;
var
  aSQLDialog: TFmSQLEditor;
  aNewValue, aOldValue: TStrings;
begin
  aOldValue := TStrings(GetObjectValue);
  aSQLDialog:= TFmSQLEditor.Create(aOldValue);

  try
    aSQLDialog.Editor.Lines:= TStrings(GetObjectValue(TStrings));
    if (aSQLDialog.ShowModal = mrOK) then
      begin
         aNewValue := aSQLDialog.Editor.Lines;
         SetPtrValue(aNewValue);
      end;
  finally
    FreeAndNil(aSQLDialog);
  end;
end;

function TwfSQLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.

