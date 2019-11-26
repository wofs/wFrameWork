{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}

unit wfImportTemplatesU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes;

const
  uTemplateFormatHelpRus = '[Инит]'+wfLE
          +';Лист электронной таблицы (только для ftSpreadsheet)'+wfLE
          +'Лист = 1'+wfLE
          +';Первая строка с данными'+wfLE
          +'ПерваяСтрока = 10'+wfLE
          +';Первая колонка с данными'+wfLE
          +'ПерваяКолонка = B'+wfLE
          +';ГруппыВСтроках Да / Нет / НеИспользуется'+wfLE
          +'ГруппыВСтроках = Да'+wfLE
          +'[Группы]'+wfLE
          +';Группа1...ГруппаN,'+wfLE
          +'Группа1 = A'+wfLE
          +'[Данные]'+wfLE
          +';Перечень импортируемых данных.'+wfLE
          +';Формат: Наименование[ТипДанных]'+wfLE
          +';Наименование - любое слово'+wfLE
          +';ТипДанных: число, строка, авто. "авто" можно не указывать.'+wfLE
          +'Код[число] = E'+wfLE
          +'Наименование[строка] = D'+wfLE
          +'Цена = F'+wfLE
          +'Остаток1 = H'+wfLE
          +'Остаток2 = J'+wfLE
          +';Доступно объединение строковых данных по строке'+wfLE
          +'НаименованиеКод = [Объединить]{Наименование} [{Код}]'+wfLE
          +';следует указать команду [Объединить]'+wfLE
          +';Доступно сложение числовых данных по строке'+wfLE
          +';следует указать команду [Вычислить]'+wfLE
          +'ОстатокСум = [Вычислить]{Остаток1}+{Остаток2}'+wfLE
          +'[Параметры]'+wfLE
          +';Соответствие импортируемых данных и параметров запроса на добавление записи в БД'+wfLE
          +';Могут не совпадать количеством с "Данными"'+wfLE
          +'Код = CODE'+wfLE
          +'Наименование = NAME'+wfLE
          +'НаименованиеКод = NANECODE'+wfLE
          +'Цена = PRICE'+wfLE
          +'Остаток1 = STOCK1'+wfLE
          +'Остаток2 = STOCK2'+wfLE
          +'ОстатокСумм = STOCKSUM'+wfLE
          +'[Логика]'+wfLE
          +';Простая логика импорта'+wfLE  //
          +';ЗаписьЕсли. Перечисление значимых полей через знак +'+wfLE
          +'ЗаписьЕсли = Код+Наименование+Цена'+wfLE
          +'ГруппаЕсли = Группа1+Фон';

implementation

end.

