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

  ufpSectionGroups       = 'ГРУППЫ';
  ufpSectionData         = 'ДАННЫЕ';
  ufpSectionDataParams   = 'ПАРАМЕТРЫ';
  ufpSectionDataLogic    = 'ЛОГИКА';

  ufpComplexType         = '[ОБЪЕДИНИТЬ]';
  ufpCalculatedType      = '[ВЫЧИСЛИТЬ]';
  ufpNumberType          = '[ЧИСЛО]';
  ufpStringType          = '[ТЕКСТ]';

  ufpGroupInRowsYes      = 'ДА';
  ufpGroupInRowsNo       = 'НЕТ';

  ufpIsContent           = 'ЗАПИСЬЕСЛИ';
  ufpIsGroup             = 'ГРУППАЕСЛИ';
  ufpGroupBackground     = 'ФОН';

implementation

end.

