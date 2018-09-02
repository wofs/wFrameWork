{
This file is part of wfFrameWork.

 -= Base =-

 wofs(c)2017-2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfVersions;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, wfEntity;

type
  { TwfVersion }

  TwfVersion = class(TCollectionItem)
  protected
    function GetDisplayName: string; override;
  private
    fDescription: TStrings;
    fSQLStrings: TStrings;
    fVersion: string;
    procedure SetDescription(aValue: TStrings);
    procedure SetSQLStrings(aValue: TStrings);
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;

  published
      property Version: string read fVersion write fVersion;
      property Description: TStrings read fDescription write SetDescription;
      property SQLStrings: TStrings read fSQLStrings write SetSQLStrings;
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
  fSQLStrings.Assign(aValue);
end;

constructor TwfVersion.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fDescription:= TStringList.Create;
  fSQLStrings:= TStringList.Create;
end;

destructor TwfVersion.Destroy;
begin
  FreeAndNil(fDescription);
  inherited Destroy;
end;

end.
