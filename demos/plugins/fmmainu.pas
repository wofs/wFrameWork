unit FmMainU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, wfPlugins, wfStatusProgressBar, FmPlugin1U, FmPlugin2U;

type

  { TFmMain }

  TFmMain = class(TForm)
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    wfPlugins1: TwfPlugins;
    wfStatusProgressBar1: TwfStatusProgressBar;
    procedure ToolButton1Click(Sender: TObject);
    procedure wfPlugins1RegisterPlugins(Sender: TwfPlugins);
  private

  public
    procedure SetStatus(aText:string);
  end;

var
  FmMain: TFmMain;

implementation

{$R *.lfm}

{ TFmMain }

procedure TFmMain.wfPlugins1RegisterPlugins(Sender: TwfPlugins);
begin
  wfPlugins1.RegisterPlugins([TFmPlugin1, TFmPlugin2]);
  wfPlugins1.LoadPlugin(0);
end;

procedure TFmMain.ToolButton1Click(Sender: TObject);
begin
  ShowMessage('This is help content');
end;

procedure TFmMain.SetStatus(aText: string);
begin
  wfStatusProgressBar1.Status:=aText;
end;


end.

