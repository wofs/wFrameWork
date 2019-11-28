{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfCalculatorU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes, fpexprpars;

type

  { TwfCalculator }

  TwfCalculator = class (TFPExpressionParser)

  public
    function Calculate(aFormula: string): Currency;

  end;

implementation

{ TwfCalculator }

function TwfCalculator.Calculate(aFormula: string): Currency;
begin
  try
    self.Expression:=aFormula;
    Result:=self.Evaluate.ResFloat;
  except
    Result:= wfEmptyDouble;
  end;
end;

end.

