unit TwfProgressU;

{$mode objfpc}{$H+}
{$INCLUDE def.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TwfProgress }

  TwfProgress = class(TForm)
    Bar: TProgressBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    fonStopForce: TNotifyEvent;
    fNoClose: boolean;

    function GetMarquee: boolean;
    procedure SetMarquee(aValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure ForceClose;

    procedure InitBar(aMax: integer; const aStep: integer = 1);
    procedure SetBar(const aPosition: integer = -1); // Top

    property Marquee: boolean read GetMarquee write SetMarquee;
    property NoClose:boolean read fNoClose write fNoClose;

    property onStopForce:TNotifyEvent read fonStopForce write fonStopForce;
  end;

var
  Form2: TwfProgress;

implementation

{$R *.lfm}

{ TwfProgress }

procedure TwfProgress.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if fNoClose and Assigned(self) and Showing then
     if MessageDlg('Отменить текущую операцию?',mtConfirmation, mbOKCancel, 0) = mrOk then
        if Assigned(onStopForce) then  onStopForce(self);

  if fNoClose then
    CanClose:= false;
end;

function TwfProgress.GetMarquee: boolean;
begin
  Result:= (Bar.Style = pbstMarquee);
end;

procedure TwfProgress.SetMarquee(aValue: boolean);
begin
  if aValue then
    Bar.Style:= pbstMarquee
  else
    Bar.Style:= pbstNormal;
end;

constructor TwfProgress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fNoClose:= true;
  Height:= Bar.Height;
end;

procedure TwfProgress.ForceClose;
begin
  fNoClose:= false;
  Close();
end;

procedure TwfProgress.InitBar(aMax: integer; const aStep: integer);
begin
  fNoClose:= true;
  Bar.Max:= aMax;
  Bar.Step:= aStep;
  Bar.Position:= 0;
end;

procedure TwfProgress.SetBar(const aPosition: integer);
begin
  if (aPosition = 0) and (Bar.Step>0) then
    Bar.StepIt
  else
    Bar.Position:=aPosition;
end;

end.

