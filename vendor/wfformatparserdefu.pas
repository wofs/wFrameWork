unit wfFormatParserDefU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  ufpSectionInit         = 'ИНИТ';
  ufpParamWorkSheet      = 'ЛИСТ';
  ufpParamFistRow        = 'ПЕРВАЯСТРОКА';
  ufpParamFistCol        = 'ПЕРВАЯКОЛОНКА';
  ufpParamGroupInRows    = 'ГРУППЫВСТРОКАХ';

  ufpSectionData         = 'ДАННЫЕ';
  ufpSectionDataParams   = 'ПАРАМЕТРЫ';
  ufpSectionDataLogic    = 'ЛОГИКА';

  ufpComplexType         = '[ОБЪЕДИНИТЬ]';
  ufpCalculatedType      = '[ВЫЧИСЛИТЬ]';

  ufpGroupInRowsYes      = 'ДА';
  ufpGroupInRowsNo       = 'НЕТ';

  ufpIsContent           = 'ЗАПИСЬЕСЛИ';

implementation

end.

