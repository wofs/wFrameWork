{
This file is part of wfFrameWork.

 -= Base =-

 wofs(c)2017-2018 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}
unit wfStatusProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  LCLType, LCLProc;

type
  TwfStatusProgressBar = class;

  { TwfCurrentStatus }

  TwfCurrentStatus = record
    StatusProgressBar: TwfStatusProgressBar;
    Max: integer;
    Progress: integer;
    Status: string;
    Step: integer;
    Marquee: boolean;
  end;

  { TwfStatusProgressBar }

  TwfStatusProgressBar = class(TStatusBar)
  private
    fCurrentStatus: TwfCurrentStatus;
    fonProgressInit: TNotifyEvent;
    fProgressBar: TProgressBar;
    function GetProgress: integer;
    function GetProgressBarMarquee: boolean;
    function GetStatus: string;
    procedure SetProgress(aValue: integer);
    procedure SetProgressBarMarquee(aValue: boolean);
    procedure SetStatus(aValue: string);

  protected
    procedure DrawProgressBar(Panel: TStatusPanel; const Rect: TRect);
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); override;
    procedure DoOnResize; override;// call OnResize
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProgressBarInit(const aMax, aStep: integer);
    procedure ProgressBarSet(const aPosition: integer = -1);

    property ProgressBarMarquee:boolean read GetProgressBarMarquee write SetProgressBarMarquee;

    property Progress: integer read GetProgress write SetProgress;
    property Status:string read GetStatus write SetStatus;
    property CurrentStatus: TwfCurrentStatus read fCurrentStatus write fCurrentStatus;
  published
    property ProgressBar: TProgressBar read fProgressBar;
    property onProgressInit: TNotifyEvent read fonProgressInit write fonProgressInit;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfstatusprogressbar_icon.lrs}
  RegisterComponents('WF',[TwfStatusProgressBar]);
end;


{ TwfStatusProgressBar }

constructor TwfStatusProgressBar.Create(TheOwner: TComponent);
var
  aPanel: TStatusPanel;
begin
  inherited Create(TheOwner);

  AutoSize:= false;
  Height:= 19;
  SizeGrip:= false;
  SimplePanel:= false;

  fProgressBar := TProgressBar.Create(self);
  With fProgressBar do
  Begin
    Visible := false;
    Smooth := true;
    Height := self.Height-3;
  end;

  if Panels.Count =0 then
  begin
    aPanel:= Panels.Add;
    aPanel.Style:= psText;
    aPanel:= Panels.Add;
    aPanel.Width:=150;
    aPanel.Style:= psOwnerDraw;
  end;

  Progress:= 0;
  Status:= EmptyStr;
end;

destructor TwfStatusProgressBar.Destroy;
begin
  inherited Destroy;
end;

function TwfStatusProgressBar.GetProgressBarMarquee: boolean;
begin
  Result:= ProgressBar.Style = pbstMarquee;
end;

function TwfStatusProgressBar.GetProgress: integer;
begin
  Result:= fProgressBar.Position;
end;

function TwfStatusProgressBar.GetStatus: string;
begin
  Result:= Panels[0].Text;
end;

procedure TwfStatusProgressBar.SetProgress(aValue: integer);
begin
  ProgressBarSet(aValue);
end;

procedure TwfStatusProgressBar.SetProgressBarMarquee(aValue: boolean);
begin
  if aValue then
    ProgressBar.Style:= pbstMarquee
  else
    ProgressBar.Style:= pbstNormal;

  if Assigned(fonProgressInit) then fonProgressInit(self);
end;

procedure TwfStatusProgressBar.SetStatus(aValue: string);
begin
  Panels[0].Text:= ' '+aValue;
end;

procedure TwfStatusProgressBar.DrawProgressBar(Panel: TStatusPanel; const Rect: TRect);
begin
  case Panel.Index of
    //0: ImageList1.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, 0);
    1:  begin
          with fProgressBar do
          begin
            Width := Rect.Right - Rect.Left + 2;
            Left := Rect.Left - 1 ;
            Top := 2;
            if not Assigned(Parent) then
              begin
                Parent := self;
                Visible := True;
              end;
          end;
        end;
  end;
end;

procedure TwfStatusProgressBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect
  );
begin
  DrawProgressBar(Panel, Rect);

  inherited DrawPanel(Panel, Rect);
end;

procedure TwfStatusProgressBar.DoOnResize;
begin
  Panels[0].Width:= self.Width-Panels[1].Width;
  inherited DoOnResize;
end;

procedure TwfStatusProgressBar.ProgressBarInit(const aMax, aStep: integer);
begin

  fProgressBar.Max:= aMax;
  fProgressBar.Position:= 0;

  if aStep = 0 then
    fProgressBar.Step:= 1
  else
    fProgressBar.Step:= aStep;

  if Assigned(fonProgressInit) then fonProgressInit(self);
end;

procedure TwfStatusProgressBar.ProgressBarSet(const aPosition: integer);
begin

  if aPosition >-1 then
    fProgressBar.Position:= aPosition
  else
    fProgressBar.StepIt;
end;

end.
