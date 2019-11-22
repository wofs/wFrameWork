unit wfParamsU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

type
  { TwfParams }

  TwfParams = class (TParams)
    private
      fFreeAfterUse: boolean;
    public
      property FreeAfterUse: boolean read fFreeAfterUse;
      constructor Create(AOwner: TPersistent; const aFreeAfterUse: boolean = false);
  end;

implementation

{ TwfParams }

constructor TwfParams.Create(AOwner: TPersistent; const aFreeAfterUse: boolean);
begin
  inherited Create(AOwner);
  fFreeAfterUse:= aFreeAfterUse;
end;

end.

