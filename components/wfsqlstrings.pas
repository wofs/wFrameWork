unit wfSQLStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, PropEdits, wfDesignSQLItemsU, wfSQLPropertyEditorU;

type

  { TwfSQLStrings }

  TwfSQLStrings = class(TComponent)
  private
    fItems: TwfDesignSQLItems;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Items: TwfDesignSQLItems read fItems write fItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfsqlstrings_icon.lrs}
  RegisterComponents('WF',[TwfSQLStrings]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfDesignSQLItem, 'SQL', TwfSQLPropertyEditor);
end;

{ TwfSQLStrings }

constructor TwfSQLStrings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems:= TwfDesignSQLItems.Create(self, TwfDesignSQLItem);
end;

destructor TwfSQLStrings.Destroy;
begin
  FreeAndNil(fItems);
  inherited Destroy;
end;

end.
