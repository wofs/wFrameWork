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
  Classes, SysUtils, wfTypes, wfFunc, fpexprpars;

type

  { TwfCalculator }

  TwfCalculator = class (TFPExpressionParser)
  private
    function TrimFormula(aFormula: string):string;

  public
    function Calculate(aFormula: string): Currency;

  end;

implementation

{ TwfCalculator }

function TwfCalculator.TrimFormula(aFormula: string): string;
var
  i: Integer;
begin
  if not (aFormula[1] in ['0'..'9']) then Delete(aFormula, 1, 1);

  Result:= aFormula;
end;

function TwfCalculator.Calculate(aFormula: string): Currency;
begin
  try
    self.Expression:=TrimFormula(aFormula);
    Result:=self.Evaluate.ResFloat;
  except
    raise;
  end;
end;

end.

