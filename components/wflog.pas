unit wfLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LazDialogs, LazFileUtils, wfTypes, wfFunc, wfBase,
  dateutils, MultiLog, FileChannel;

type

  { TwfLog }

  TwfLog = class(TComponent)
  private
    fChannelOptions: TFileChannelOptions;
    fLogClasses: TwfLogClasses;
    fLogFileName: string;
    procedure SetChannelOptions(aValue: TFileChannelOptions);
    procedure SetLogClasses(aValue: TwfLogClasses);
    procedure SetLogFileName(aValue: string);

  protected

  public
    constructor Create(AOwner: TComponent);

    function GetActiveLogClasses:string;
    function ConvertLogClass(aLogClass: TwfLogClass): string;

    procedure InitLogger;
    procedure Log(Sender: TObject; const aValue: string);
    procedure Send(aClasses: TwfLogClasses; const aText: String);
    procedure SendDebug(const aText: String);
    procedure SendInfo(const aText: String);
    procedure SendEvent(const aText: String);
    procedure SendWarning(const aText: String);
    procedure SendError(const aText: String); overload;
    procedure SendException(const aText: String; aException: Exception);
    procedure EnterMethod(const aMethodName: String);
    procedure EnterMethod(Sender: TObject; const aMethodName: String);
    procedure ExitMethod(const aMethodName: String);
    procedure ExitMethod(Sender: TObject; const aMethodName: String);
    procedure SendCallStack(const aText: String);
    procedure Watch(const aText, aValue: String);
    procedure AddCheckPoint;
    procedure AddCheckPoint(const aCheckName: String);

  published
    // File name. If empty - the name of the file will be formed automatically by mask: Current_date.txt
    property LogFileName:string read fLogFileName write SetLogFileName;
    // Options
    property ChannelOptions: TFileChannelOptions read fChannelOptions write SetChannelOptions;
    property LogClasses:TwfLogClasses read fLogClasses write SetLogClasses;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wflog_icon.lrs}
  RegisterComponents('WF',[TwfLog]);
end;

{ TwfLog }

procedure TwfLog.SetChannelOptions(aValue: TFileChannelOptions);
begin
  if fChannelOptions=aValue then Exit;
  fChannelOptions:=aValue;
end;

procedure TwfLog.SetLogClasses(aValue: TwfLogClasses);
begin
  if fLogClasses=aValue then Exit;
  fLogClasses:=aValue;
end;

procedure TwfLog.SetLogFileName(aValue: string);
begin
  if fLogFileName=aValue then Exit;
  fLogFileName:=aValue;
end;

procedure TwfLog.InitLogger;
var
  aLogFileName, aPath: String;
begin
  aPath:= GetApplicationPathUnsafe+'logs';

  if not DirectoryExistsUTF8(aPath) then ForceDirectoriesUTF8(aPath);
  aPath += DirectorySeparator;

  if IsEmpty(LogFileName) then
    aLogFileName:= Format('%s.txt',[FormatDateTime('dd.mm.yyyy', Today)]);

  Logger.ActiveClasses:= TDebugClasses(LogClasses);
  Logger.Channels.Add(TFileChannel.Create(aPath+aLogFileName,ChannelOptions));
  Logger.Send(GetActiveLogClasses);

{

OldClasses:=ActiveClasses;
ActiveClasses:=lcAll;
EnterMethod(Sender,'SubLogClick');
SendIf('Only show if called by TestLogClick',CalledBy('TestLogClick'));
Send('AText inside DoIt');
SendWarning('AWarning');
SendCallStack('CallStack example');
Send('A String','sadjfgadsfbmsandfb');
Send('AInteger',4957);
Send('A Boolean',True);
ExitMethod(Sender,'SubLogClick');
ActiveClasses:=OldClasses;
}
end;

procedure TwfLog.Log(Sender: TObject; const aValue: string);
var
  aClasses: TwfLogClasses;
  aText: String;
begin
  aClasses:= [lcDebug];
  aText:= Format('%s: %s',[Sender.ToString, aValue]);
  Logger.Send(TDebugClasses(aClasses), aText);
end;

constructor TwfLog.Create(AOwner: TComponent);
begin
  //if not (csDesigning in ComponentState) then
  //  InitLogger;
end;

function TwfLog.GetActiveLogClasses: string;
var
  i: Integer;
begin
  Result:= 'Уровни логирования:';
  if lcDebug in LogClasses then
    Result += ' '+ConvertLogClass(lcDebug);

  if lcError in LogClasses then
    Result += ' '+ConvertLogClass(lcError);

  if lcInfo in LogClasses then
    Result += ' '+ConvertLogClass(lcInfo);

  if lcWarning in LogClasses then
    Result += ' '+ConvertLogClass(lcWarning);

  if lcEvent in LogClasses then
    Result += ' '+ConvertLogClass(lcEvent);
end;

function TwfLog.ConvertLogClass(aLogClass: TwfLogClass): string;
begin
  case aLogClass of
    lcDebug: Result:= Format('Debug (%d)',[lcDebug]);
    lcError: Result:= Format('Error (%d)',[lcError]);
    lcInfo: Result:= Format('Information (%d)',[lcInfo]);
    lcWarning: Result:= Format('Warning (%d)',[lcWarning]);
    lcEvent: Result:= Format('Events (%d)',[lcEvent]);
  end;
end;

procedure TwfLog.Send(aClasses: TwfLogClasses; const aText: String);
begin
  Logger.Send(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendDebug(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.Send(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendInfo(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcInfo];
  Logger.Send(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendEvent(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcEvent];
  Logger.Send(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendWarning(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcWarning];
  Logger.SendWarning(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendError(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcError];
  Logger.SendError(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.SendException(const aText: String; aException: Exception);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcError];
  Logger.SendException(TDebugClasses(aClasses), aText, aException);
end;

procedure TwfLog.EnterMethod(const aMethodName: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.EnterMethod(TDebugClasses(aClasses), aMethodName);
end;

procedure TwfLog.EnterMethod(Sender: TObject; const aMethodName: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.EnterMethod(TDebugClasses(aClasses), Sender, aMethodName);
end;

procedure TwfLog.ExitMethod(const aMethodName: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.ExitMethod(TDebugClasses(aClasses), aMethodName);
end;

procedure TwfLog.ExitMethod(Sender: TObject; const aMethodName: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.ExitMethod(TDebugClasses(aClasses), Sender, aMethodName);
end;

procedure TwfLog.SendCallStack(const aText: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.SendCallStack(TDebugClasses(aClasses), aText);
end;

procedure TwfLog.Watch(const aText, aValue: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.Watch(TDebugClasses(aClasses), aText, aValue);
end;

procedure TwfLog.AddCheckPoint;
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.AddCheckPoint(TDebugClasses(aClasses));
end;

procedure TwfLog.AddCheckPoint(const aCheckName: String);
var
  aClasses: TwfLogClasses;
begin
  aClasses:= [lcDebug];
  Logger.AddCheckPoint(TDebugClasses(aClasses), aCheckName);

end;

end.
