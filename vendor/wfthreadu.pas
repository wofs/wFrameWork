unit wfThreadU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wfTypes;

type

  TThreadExceptionEvent = procedure(Sender: TThread; E: Exception) of object;
  TThreadEndEvent = procedure(Sender: TThread; aResult: variant) of object;
  TThreadStatusEvent = TTextEvent;
  TThreadLogEvent = TTextEvent;
  TThreadProgressEvent = TProgressEvent;

  { TwfThread }

  TwfThread = class(TThread)
    private
      fonEndThread: TThreadEndEvent;
      fonEndThreadForce: TThreadEndEvent;
      fonLog: TThreadLogEvent;
      fonProgress: TThreadProgressEvent;
      fonProgressInit: TProgressInitEvent;
      fonProgressMarquee: TProgressMarqueeEvent;
      fonStatus: TThreadStatusEvent;
      fonExceptionEvent: TThreadExceptionEvent;
      fStopForce: boolean;
      procedure SetLog(aValue: string);
      procedure SetProgress(aValue: integer);
      procedure SetProgressMarquee(aValue: boolean);
      procedure SetStatus(aValue: string);
      procedure SetStatusLog(aValue: string);

    protected
      procedure EndThread(Sender: TThread;const aResult: variant);
      procedure EndThreadForce(Sender: TThread;const aResult: variant);
      procedure ExceptionEvent(Sender: TThread; E: Exception);

      procedure ProgressInit(Sender: TThread; const aMax, aStep: integer);
      procedure ProgressStep;
      property ProgressMarquee: boolean write SetProgressMarquee;
      property Progress: integer write SetProgress;
      property Status: string write SetStatus;
      property StatusLog: string write SetStatusLog;
      property Log: string write SetLog;
    published

    public
      constructor Create(CreateSuspended: boolean);
      destructor Destroy(); override;

      procedure Stop();
      // Событие: завершение потока
      property onEndThread: TThreadEndEvent read fonEndThread write fonEndThread;
      // Событие: завершение потока по команде пользователя
      property onEndThreadForce: TThreadEndEvent read fonEndThreadForce write fonEndThreadForce;
      // Событие: исключение
      property onExceptionEvent: TThreadExceptionEvent read fonExceptionEvent write fonExceptionEvent;
      // Событие: запись в статус
      property onStatus: TThreadStatusEvent read fonStatus write fonStatus;
      // Событие: запись в лог
      property onLog: TThreadLogEvent read fonLog write fonLog;
      // Событие: установка значения прогресс бара
      property onProgress: TThreadProgressEvent read fonProgress write fonProgress;
      // Событие: инициализация прогресс бара
      property onProgressInit: TProgressInitEvent read fonProgressInit write fonProgressInit;
      // Событие: установка типа прогресс бара
      property onProgressMarquee: TProgressMarqueeEvent read fonProgressMarquee write fonProgressMarquee;
      // Флаг: завершение потока по команде пользователя
      property StopForce: boolean read fStopForce write fStopForce;
  end;


implementation

{ TwfThread }

procedure TwfThread.ProgressInit(Sender: TThread; const aMax, aStep: integer);
begin
  if Assigned(onProgressInit) then
     onProgressInit(Sender, aMax, aStep);
end;

procedure TwfThread.ProgressStep;
begin
  Progress:= -1;
end;

procedure TwfThread.SetProgressMarquee(aValue: boolean);
begin
  if Assigned(onProgressMarquee) then
    onProgressMarquee(self, aValue);
end;

procedure TwfThread.SetStatus(aValue: string);
begin
  if Assigned(onStatus) then
     onStatus(Self, aValue);
end;

procedure TwfThread.SetStatusLog(aValue: string);
begin
  Log:= aValue;
  Status:= aValue;
end;

procedure TwfThread.SetProgress(aValue: integer);
begin
  if Assigned(onProgress) then
     onProgress(Self, aValue);
end;

procedure TwfThread.SetLog(aValue: string);
begin
  if Assigned(fonLog) then
       fonLog(Self, aValue);
end;

procedure TwfThread.EndThread(Sender: TThread; const aResult: variant);
begin
  if Assigned(onEndThread) then
     onEndThread(Sender, aResult);
end;

procedure TwfThread.EndThreadForce(Sender: TThread; const aResult: variant);
begin
  if Assigned(onEndThreadForce) then
     onEndThreadForce(Sender, aResult);
end;

procedure TwfThread.ExceptionEvent(Sender: TThread; E: Exception);
begin
  if Assigned(onExceptionEvent) then
     onExceptionEvent(Sender, E);
end;

constructor TwfThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TwfThread.Destroy();
begin
  inherited Destroy();
end;

procedure TwfThread.Stop();
begin
  fStopForce:= true;
end;

end.

