{
This file is part of wfFrameWork.

 -= Forms/Progress =-

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    fonStopForce: TNotifyEvent;
    fNoClose: boolean;

    procedure AsyncFree(Data: PtrInt);
    function GetMarquee: boolean;
    procedure SetMarquee(aValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

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

procedure TwfProgress.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

function TwfProgress.GetMarquee: boolean;
begin
  Result:= (Bar.Style = pbstMarquee);
end;

procedure TwfProgress.AsyncFree(Data: PtrInt);
begin
  FreeAndNil(self);
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

destructor TwfProgress.Destroy;
begin
  inherited Destroy;
end;

procedure TwfProgress.ForceClose;
begin
  fNoClose:= false;
  Close();
  //Application.QueueAsyncCall(@AsyncFree,0);
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

