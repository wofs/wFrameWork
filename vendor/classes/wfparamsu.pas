{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

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

