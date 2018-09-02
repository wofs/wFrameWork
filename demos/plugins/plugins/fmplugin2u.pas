unit FmPlugin2U;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Spin, Grids, ColorBox, ComboEx, Arrow,
  wfValueListEditor, wfFunc;

type

  { TFmPlugin2 }

  TFmPlugin2 = class(TForm)
    Panel1: TPanel;
    wfValueListEditor1: TwfValueListEditor;
  private

  public

  end;

var
  FmPlugin2: TFmPlugin2;

implementation

{$R *.lfm}

{ TFmPlugin2 }

end.

