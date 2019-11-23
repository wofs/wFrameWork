unit wfImportTemplatesU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes;

const
uTemplateFormatHelp = '[Инит]'+wfLE
          +';Таблица, куда записываем данные'+wfLE
          +'ТаблицаВБазеДанных = TABLENAME'+wfLE
          +';Первая строка с данными'+wfLE
          +'ПерваяСтрока = 10'+wfLE
          +';Первая колонка с данными'+wfLE
          +'ПерваяКолонка = B'+wfLE
          +';ГруппыВСтроках Да / Нет / НеИспользуется'+wfLE
          +'ГруппыВСтроках = Да'+wfLE
          +'[Данные]'+wfLE
          +';Группа1...ГруппаN,'+wfLE
          +';если Группы не используются, не указывать'+wfLE
          +'Группа1 = B'+wfLE
          +';Перечень импортируемых данных.'+wfLE
          +';Формат: Наименование[ТипДанных]'+wfLE
          +';Наименование - любое слово'+wfLE
          +';ТипДанных: число, строка, авто. "авто" можно не указывать.'+wfLE
          +'Код[число] = E'+wfLE
          +'Наименование[строка] = D'+wfLE
          +'Цена = F'+wfLE
          +'Остаток1 = H'+wfLE
          +'Остаток2 = J'+wfLE
          +';Доступно объединение числовых данных по строке'+wfLE
          +'НаименованиеКод = Наименование [Код]'+wfLE
          +';Доступно сложение строк'+wfLE
          +'ОстатокСум = H+J'+wfLE
          +'[ДанныеВБазе]'+wfLE
          +';Соответствие импортируемых данных и полей таблицы'+wfLE
          +';Могут не совпадать количеством с "Данными"'+wfLE
          +'Код = CODE'+wfLE
          +'Наименование = NAME'+wfLE
          +'НаименованиеКод = NANECODE'+wfLE
          +'Цена = PRICE'+wfLE
          +'Остаток1 = STOCK1'+wfLE
          +'Остаток2 = STOCK2'+wfLE
          +'ОстатокСумм = STOCKSUM'+wfLE
          +'[Логика]'+wfLE
          +';Простая логика импорта'+wfLE
          +'НаименованиеЕсли = Код+Наименование+Цена'+wfLE
          +'ГруппаЕсли = Группа1+Фон';

implementation

end.

