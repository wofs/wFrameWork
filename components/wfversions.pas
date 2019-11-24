{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfVersions;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, PropEdits, wfEntity, wfSQLPropertyEditorU,
  wfStringsPropertyEditor;

type
  { TwfVersion }

  TwfVersion = class(TCollectionItem)
  protected
    function GetDisplayName: string; override;
  private
    fDescription: TStrings;
    fSQL: TStrings;
    fVersion: string;
    procedure SetDescription(aValue: TStrings);
    procedure SetSQLStrings(aValue: TStrings);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

  published
      property Version: string read fVersion write fVersion;
      property Description: TStrings read fDescription write SetDescription;
      property SQL: TStrings read fSQL write SetSQLStrings;
  end;

  { TwfVersions }

  TwfVersions = class(TComponent)
  private
    fVersions: TOwnedCollection;

  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Versions: TOwnedCollection read fVersions write fVersions;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfversions_icon.lrs}
  RegisterComponents('WF',[TwfVersions]);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfVersion, 'SQL', TwfSQLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TwfVersion, 'Description', TwfStringsPropertyEditor);
end;

{ TwfVersions }

constructor TwfVersions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fVersions:= TOwnedCollection.Create(self, TwfVersion);
end;

destructor TwfVersions.Destroy;
begin
  FreeAndNil(fVersions);
  inherited Destroy;
end;

{ TwfVersion }

function TwfVersion.GetDisplayName: string;
begin
  Result:= fVersion;
end;

procedure TwfVersion.SetDescription(aValue: TStrings);
begin
  fDescription.Assign(aValue);
end;

procedure TwfVersion.SetSQLStrings(aValue: TStrings);
begin
  fSQL.Assign(aValue);
end;

constructor TwfVersion.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDescription:= TStringList.Create;
  fSQL:= TStringList.Create;
end;

destructor TwfVersion.Destroy;
begin
  FreeAndNil(fDescription);
  FreeAndNil(fSQL);
  inherited Destroy;
end;

end.
