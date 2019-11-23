unit wfStringsPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,PropEdits, UITypes, wfTypes, wfStringsEditor;

type

  { TwfStringsPropertyEditor }

  TwfStringsPropertyEditor = class(TClassPropertyEditor)
  private
    fMode: TwfStringsEditorMode;

  protected
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
    property Mode: TwfStringsEditorMode read fMode write fMode;

  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;


implementation

{ TwfStringsPropertyEditor }

constructor TwfStringsPropertyEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  Mode:= stmText;
end;

procedure TwfStringsPropertyEditor.Edit;
var
  aSQLDialog: TFmStringsEditor;
  aNewValue, aOldValue: TStrings;
begin
  aOldValue := TStrings(GetObjectValue);
  aSQLDialog:= TFmStringsEditor.Create(aOldValue, Mode);

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

function TwfStringsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.

