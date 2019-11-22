unit wfThreadU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes;

type
  TwfThread = class;

  TwfThreadExecuteEvent =  procedure(const Sender: TwfThread; const Msg: Word; var Param: Variant) of object;
  TwfThreadFinishEvent = procedure(const Sender: TwfThread; const Msg: Word; const Param: Variant) of object;
  TwfThreadProgress = procedure(const Sender: TwfThread; const Msg: Word; const Value: Word) of object;
  TwfThreadMessage = procedure(const Sender: TwfThread; const Msg: Word; const Param: Variant) of object;
  TwfThreadNotify = procedure(const Sender: TwfThread) of object;

  { TwfThread }

  TwfThread = class(TThread)
  private
    fonException: TErrorEvent;
    fonExecute: TwfThreadExecuteEvent;
    fonFinish: TwfThreadFinishEvent;
    fonMessage: TwfThreadMessage;
    fonProgress: TwfThreadProgress;

    fMsg: Word;
    fParam: variant;

  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean);

    property onExecute: TwfThreadExecuteEvent read fonExecute write fonExecute;
    property onFinish: TwfThreadFinishEvent read fonFinish write fonFinish;
    property onProgress: TwfThreadProgress read fonProgress write fonProgress;
    property onMessage: TwfThreadMessage read fonMessage write fonMessage;
    property onException: TErrorEvent read fonException write fonException;
  end;

implementation



{ TwfThread }

{ TwfThread }

constructor TwfThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:= true;
end;

procedure TwfThread.Execute;
begin
  try
     fParam:= true;
     if Assigned(fonExecute) then fonExecute(self, fMsg, fParam);
  except
    on E: Exception do
      if Assigned(fonException) then fonException(self, E);
  end;

  if Assigned(fonFinish) then fonFinish(self, fMsg, fParam);
end;

end.

