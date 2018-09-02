unit FmPlugin1U;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, wfPlugins;

type

  { TFmPlugin1 }

  TFmPlugin1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Plugin: TwfPlugin;

  public

  end;

var
  FmPlugin1: TFmPlugin1;

implementation

{$R *.lfm}

{ TFmPlugin1 }

procedure TFmPlugin1.Button2Click(Sender: TObject);
begin
  Plugin.Status:=Memo1.Lines[0];
end;

procedure TFmPlugin1.Button1Click(Sender: TObject);
begin
  Plugin.Progress:= SpinEdit1.Value;
end;

procedure TFmPlugin1.FormCreate(Sender: TObject);
begin
  Plugin:= TwfPlugin(GetOwner);
end;

end.

