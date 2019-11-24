{
This file is part of wfFrameWork.

 wofs(c)2017-2019 [wofssirius@yandex.ru]
 GNU LESSER GENERAL PUBLIC LICENSE v.2.1

 Git: https://github.com/wofs/wFrameWork.git
}unit wfPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Menus, wfResourceStrings, wfFunc, wfTypes,
  wfStatusProgressBar;

var
  wfPlugin_Images: TImageList;

type
  TwfPlugin = class;
  TwfPlugins = class;

  TwfPluginLoadedList = specialize TFPGObjectList<TwfPlugin>;

  TwfPluginsEvent = procedure(Sender: TwfPlugins) of object;
  { TwfPlugin }

  TwfPlugin = class(TComponent)
  protected

  private
    fCurrentStatus: TwfCurrentStatus;
    fForm: TForm;
    fFormClass: TFormClass;
    fName: string;
    fonLog: TTextEvent;
    fonProgress: TProgressEvent;
    fonStatus: TTextEvent;
    fOwner: TwfPlugins;
    fStatusBar: TwfStatusProgressBar;
    fIndexToPluginListLoaded: integer;
    fIndexToPluginList: integer;
    fonFormClose: TNotifyEvent;
    fPageIndex: integer;

    function GetProgress: integer;
    function GetProgressMarquee: boolean;
    function GetStatus: string;
    procedure SetCurrentStatus(aValue: TwfCurrentStatus);
    procedure SetProgress(aValue: integer);
    procedure SetProgressMarquee(aValue: boolean);
    procedure SetStatus(aValue: string);
    procedure SetStatusBar(aValue: TwfStatusProgressBar);
    procedure wfOnFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure wfOnLog(Sender: TObject; const aValue: string);
    procedure wfOnProgressInit(Sender: TObject);
    procedure Log(const aValue: string);

    property StatusBar: TwfStatusProgressBar read fStatusBar write SetStatusBar;

  public
    constructor Create(Sender:TwfPlugins; aPlugIndex: integer);
    destructor Destroy; override;

    procedure StatusBarFree;
    procedure ProgressInit(aMax, aStep: integer);

    property Form: TForm read fForm;
    property Name:string read fName;
    property FormClass: TFormClass read fFormClass;

    property Progress: integer read GetProgress write SetProgress;
    property ProgressMarquee:boolean read GetProgressMarquee write SetProgressMarquee;
    property Status:string read GetStatus write SetStatus;
    property CurrentStatus: TwfCurrentStatus read fCurrentStatus write SetCurrentStatus;

    property Owner: TwfPlugins read fOwner write fOwner;

    property IndexToPluginListLoaded: integer read fIndexToPluginListLoaded;
    property IndexToPluginList: integer read fIndexToPluginList;
    property PageIndex: integer read fPageIndex write fPageIndex;

    {Events}
    property onFormClose:TNotifyEvent read fonFormClose write fonFormClose;
    property onStatus: TTextEvent read fonStatus write fonStatus;
    property onProgress: TProgressEvent read fonProgress write fonProgress;
    property onLog: TTextEvent read fonLog write fonLog;

  end;

  { TwfPlugins }

  { TwfTabSheet }

  TwfTabSheet = class(TCustomPage)
  private
    function GetPagePlugins: TwfPlugins;
    procedure SetPagePlugins(aValue: TwfPlugins);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property PagePlugins: TwfPlugins read GetPagePlugins write SetPagePlugins;
  published
    property DragCursor;
    property DragKind;
    property DragMode;
  end;

  TwfPlugins = class(TCustomTabControl)
  private
    CurrentX: Integer;
    CurrentY: Integer;
    FBevelOuter: TBevelCut;
    fCurrentStatus: TwfCurrentStatus;
    fonLog: TTextEvent;
    fonStatus: TTextEvent;
    fPageIndexFromMouseDown: Integer;
    fonRegisterPlugins: TwfPluginsEvent;
    fPluginIcons: TImageList;
    fPluginList: TStringList;
    fPluginsLoaded: TwfPluginLoadedList;
    fPopupMenu: TPopupMenu;
    fShowFormInTaskBar: boolean;
    fStatusBar: TwfStatusProgressBar;
    fStatusBarInPluginForm: boolean;
    fToolBar: TToolBar;
    MouseLeftStateClick: Boolean;
    procedure AsyncInit(Data: PtrInt);
    procedure ClosePage(aPageIndex: integer; aUnloadPlugin: boolean);
    function GetProgress: integer;
    function GetStatus: string;
    function GetTabSheet(Index: Integer): TwfTabSheet;
    procedure mPluginPageHeadClick(Sender: TObject);
    procedure SetCurrentStatus(aValue: TwfCurrentStatus);
    procedure SetCurrentStatus2();
    procedure SetProgress(aValue: integer);
    procedure SetStatus(aValue: string);
    procedure UpdatePageIndex(aDelIndex: integer);
    procedure wfOnLog(Sender: TObject; const aValue: string);
    procedure wfOnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure wfOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure wfOnPluginFormClose(Sender: TObject);
    procedure wfOnPluginProgress(Sender: TObject; const aPosition: integer);
    procedure wfOnPluginStatus(Sender: TObject; const aValue: string);
    procedure wfOnProgressInit(Sender: TObject);
    procedure wfOnToolButtolClick(Sender: TObject);
    procedure Log(const aValue: string);
  protected
    procedure DoChange; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function AddTabSheet: TwfTabSheet;
    function GetPlugin(aPageIndex: integer):TwfPlugin;
    function GetPluginFromList(aListIndex: integer):TwfPlugin;
    function GetPlugin(aFormName: TComponentName):TwfPlugin;

    procedure LoadPlugin(aIndex: Integer);
    procedure UnLoadPlugin(aPageIndex: Integer);
    procedure PluginToPage(aPlugin: TwfPlugin);
    procedure PluginToForm(aPageIndex: integer);

    procedure RegisterPlugins(TFormsName: array of TFormClass;
      aHints: array of string);
    procedure RegisterPlugins(TFormsName: array of TFormClass;
          aHints: array of string; aImages: array of integer);
    procedure RegisterPlugins(TFormsName: array of TFormClass);

    property PluginList: TwfPluginLoadedList read fPluginsLoaded write fPluginsLoaded;
    property Pages[Index: Integer]: TwfTabSheet read GetTabSheet;

    property Progress: integer read GetProgress write SetProgress;
    property Status:string read GetStatus write SetStatus;
    property CurrentStatus: TwfCurrentStatus read fCurrentStatus write SetCurrentStatus;
  published
    property Align;

    //Show checkout button plugin in the task bar
    property ShowFormInTaskBar: boolean read fShowFormInTaskBar write fShowFormInTaskBar default true;
    property StatusBarInPluginForm: boolean read fStatusBarInPluginForm write fStatusBarInPluginForm;
    property StatusBar: TwfStatusProgressBar read fStatusBar write fStatusBar;
    property ToolBar: TToolBar read fToolBar write fToolBar;

    {Events}
    //Use RegisterPlugins()
    property onRegisterPlugins: TwfPluginsEvent read fonRegisterPlugins write fonRegisterPlugins;
    property onLog: TTextEvent read fonLog write fonLog;
    property onStatus: TTextEvent read fonStatus write fonStatus;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I wfplugins_icon.lrs}
  RegisterComponents('WF',[TwfPlugins]);
end;

{ TwfTabSheet }

function TwfTabSheet.GetPagePlugins: TwfPlugins;
begin
  if (Parent is TwfPlugins) then
    Result := TwfPlugins(Parent)
  else
    Result := nil;
end;

procedure TwfTabSheet.SetPagePlugins(aValue: TwfPlugins);
begin
  if PagePlugins=aValue then exit;
  Parent:=aValue;
end;

constructor TwfTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TwfTabSheet.Destroy;
begin
  inherited Destroy;
end;

{ TwfPlugin }

procedure TwfPlugin.wfOnFormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if Assigned(fonFormClose) then fonFormClose(self);
end;

procedure TwfPlugin.wfOnLog(Sender: TObject; const aValue: string);
begin
  Log(aValue);
end;

procedure TwfPlugin.wfOnProgressInit(Sender: TObject);
var
  aProgressBar: TwfStatusProgressBar;
begin
  aProgressBar:= TwfStatusProgressBar(Sender);
  fCurrentStatus.Max:= aProgressBar.ProgressBar.Max;
  fCurrentStatus.Step:= aProgressBar.ProgressBar.Step;
  fCurrentStatus.Marquee:= aProgressBar.ProgressBarMarquee;
end;

procedure TwfPlugin.Log(const aValue: string);
begin
  if Assigned(fonLog) then
    fonLog(self, aValue);
end;

function TwfPlugin.GetProgress: integer;
begin
  Result:= -1;
  if Assigned(StatusBar) then
    Result:= StatusBar.Progress;
end;

function TwfPlugin.GetProgressMarquee: boolean;
begin
  Result:= StatusBar.ProgressBarMarquee;
end;

function TwfPlugin.GetStatus: string;
begin
  Result:= EmptyStr;
  if Assigned(StatusBar) then
    Result:= StatusBar.Status;
end;

procedure TwfPlugin.SetCurrentStatus(aValue: TwfCurrentStatus);
begin
  fCurrentStatus:=aValue;
  StatusBar.Progress:= aValue.Progress;
  StatusBar.Status:= aValue.Status;
  StatusBar.ProgressBar.Max:= aValue.Max;
  StatusBar.ProgressBar.Step:= aValue.Step;
  StatusBar.ProgressBarMarquee:= aValue.Marquee;
end;

procedure TwfPlugin.SetProgress(aValue: integer);
begin
  fCurrentStatus.Progress:= aValue;

  if Assigned(StatusBar) then
    StatusBar.Progress:= aValue;

  if Assigned(fonProgress) then fonProgress(self, aValue);
end;

procedure TwfPlugin.SetProgressMarquee(aValue: boolean);
begin
  fCurrentStatus.Marquee:= aValue;

  if Assigned(StatusBar) then
    StatusBar.ProgressBarMarquee:= aValue;
end;

procedure TwfPlugin.SetStatus(aValue: string);
begin
  fCurrentStatus.Status:= aValue;

  if Assigned(StatusBar) then
    StatusBar.Status:= aValue;

  if Assigned(fonStatus) then fonStatus(self,aValue);
end;

procedure TwfPlugin.SetStatusBar(aValue: TwfStatusProgressBar);
begin
  fStatusBar:=aValue;

  if Assigned(fStatusBar) then
    begin
      fStatusBar.onProgressInit:=@wfOnProgressInit;
    end;
end;

constructor TwfPlugin.Create(Sender: TwfPlugins; aPlugIndex: integer);
var
  aCurrentStatus: TwfCurrentStatus;
begin
  fOwner:= Sender;
  fIndexToPluginListLoaded := Sender.PluginList.Count;
  fIndexToPluginList:=aPlugIndex;
  fName:='Plugin'+IntToStr(fIndexToPluginList);
  fFormClass := TFormClass(FindClass(fName));
  fPageIndex:=-1;
  fForm := fFormClass.Create(self);
  fForm.OnClose:=@wfOnFormClose;

  aCurrentStatus.Max:= 100;
  aCurrentStatus.Progress:=0;
  aCurrentStatus.Marquee:= false;
  aCurrentStatus.Status:='';

  fCurrentStatus:= aCurrentStatus;

    //fForm.Show;
end;

destructor TwfPlugin.Destroy;
begin
  inherited Destroy;
end;

procedure TwfPlugin.StatusBarFree;
begin
  if Assigned(fStatusBar) then
    FreeAndNil(fStatusBar);
end;

procedure TwfPlugin.ProgressInit(aMax, aStep: integer);
begin
  fCurrentStatus.Max:= aMax;
  fCurrentStatus.Step:= aStep;

  if Assigned(StatusBar) then
    StatusBar.ProgressBarInit(aMax, aStep)
end;

{ TwfPlugins }

procedure TwfPlugins.AsyncInit(Data: PtrInt);
begin
  if Assigned(fonRegisterPlugins) then
    fonRegisterPlugins(self);

  if Assigned(StatusBar) then
    begin
      fCurrentStatus.Status:= StatusBar.Status;
      fCurrentStatus.Progress:= StatusBar.Progress;
      fCurrentStatus.Max:=StatusBar.ProgressBar.Max;
      fCurrentStatus.Marquee:= (pbstMarquee = StatusBar.ProgressBar.Style);
      StatusBar.onProgressInit:=@wfOnProgressInit;
    end;
end;

function TwfPlugins.GetTabSheet(Index: Integer): TwfTabSheet;
begin
  Result:=TwfTabSheet(inherited Page[Index]);
end;

procedure TwfPlugins.mPluginPageHeadClick(Sender: TObject);
begin
  if PageIndex=-1 then exit;

  case TMenuItem(Sender).Name of
    'mPluginUnpin': ClosePage(PageIndex, false);
    'mPluginClose': ClosePage(PageIndex, true);
  end;
end;

procedure TwfPlugins.SetCurrentStatus(aValue: TwfCurrentStatus);
begin
  fCurrentStatus:=aValue;

  Progress:= aValue.Progress;
  Status:= aValue.Status;
  StatusBar.ProgressBar.Max:= aValue.Max;
  StatusBar.ProgressBar.Step:= aValue.Step;
  StatusBar.ProgressBarMarquee:= aValue.Marquee;
end;

procedure TwfPlugins.SetCurrentStatus2();
var
  aPlugin: TwfPlugin;
begin
  aPlugin:= GetPlugin(PageIndex);

  if Assigned(aPlugin) then
     CurrentStatus:= aPlugin.CurrentStatus;
end;

procedure TwfPlugins.SetProgress(aValue: integer);
begin
  fCurrentStatus.Progress:= aValue;

  if Assigned(StatusBar) then
    begin
      StatusBar.Progress:= aValue;
      Log(Format(rsProgressBarLogPosition,[aValue,StatusBar.Progress.MaxValue]));
    end;
end;

procedure TwfPlugins.SetStatus(aValue: string);
begin
  fCurrentStatus.Status:= aValue;

  if Assigned(StatusBar) then
    begin
      StatusBar.Status:= aValue;
      Log(aValue);
    end;
end;

procedure TwfPlugins.wfOnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fPageIndexFromMouseDown := IndexOfPageAt(X,Y);
  CurrentX:= X;
  CurrentY:= Y;
  MouseLeftStateClick:= True;
end;

procedure TwfPlugins.UpdatePageIndex(aDelIndex:integer);
var
  i: Integer;
  aPlugin: TwfPlugin;
begin
  for i:= 0 to PageCount do
  begin
    if i>aDelIndex then
      begin
        aPlugin:= GetPlugin(i);
        if Assigned(aPlugin) then
          aPlugin.PageIndex:= aPlugin.PageIndex-1;
      end;
  end;
end;

procedure TwfPlugins.wfOnLog(Sender: TObject; const aValue: string);
begin
  Log(Format('%s: %s',[TwfPlugin(Sender).Name, aValue]));
end;

procedure TwfPlugins.ClosePage(aPageIndex: integer; aUnloadPlugin: boolean);
begin
  if PageCount = 0 then exit;

  UpdatePageIndex(aPageIndex);

  if aUnloadPlugin then
    UnLoadPlugin(aPageIndex)
  else
    PluginToForm(aPageIndex);

  Pages[aPageIndex].Free;
{ TODO : пересчет индекса плагинов
 }
  DoChange;
end;

function TwfPlugins.GetProgress: integer;
begin
  Result:= fCurrentStatus.Progress;

  if Assigned(StatusBar) then
    Result:= StatusBar.Progress;
end;

function TwfPlugins.GetStatus: string;
begin
  Result:= fCurrentStatus.Status;

  if Assigned(StatusBar) then
    Result:= StatusBar.Status;
end;

procedure TwfPlugins.wfOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fPageIndexFromMouseDown = -1 then exit;

  PageIndex:=fPageIndexFromMouseDown;

  SetCurrentStatus2();

  case Button of
    mbMiddle:
        begin
          ClosePage(PageIndex, false);
        end;
    mbRight:
        begin
          fPopupMenu.PopUp;
        end;
  end;
  MouseLeftStateClick:= false;
end;

procedure TwfPlugins.wfOnPluginFormClose(Sender: TObject);
var
  aPlugin: TwfPlugin;
begin
  aPlugin:= TwfPlugin(Sender);
  PluginToPage(aPlugin);
end;

procedure TwfPlugins.wfOnPluginProgress(Sender: TObject;
  const aPosition: integer);
var
  aPlugin: TwfPlugin;
begin
  aPlugin:= TwfPlugin(Sender);
  if not Assigned(aPlugin.StatusBar) and (aPlugin.PageIndex = self.PageIndex) then
    Progress:= aPosition;
end;

procedure TwfPlugins.wfOnPluginStatus(Sender: TObject; const aValue: string);
var
  aPlugin: TwfPlugin;
begin
  aPlugin:= TwfPlugin(Sender);
  if not Assigned(aPlugin.StatusBar) and (aPlugin.PageIndex = self.PageIndex) then
    Status:= aValue;
end;

procedure TwfPlugins.wfOnProgressInit(Sender: TObject);
var
  aProgressBar: TwfStatusProgressBar;
begin
  aProgressBar:= TwfStatusProgressBar(Sender);
  fCurrentStatus.Max:= aProgressBar.ProgressBar.Max;
  fCurrentStatus.Step:= aProgressBar.ProgressBar.Step;
  fCurrentStatus.Marquee:= aProgressBar.ProgressBarMarquee;
end;

procedure TwfPlugins.wfOnToolButtolClick(Sender: TObject);
var
  aPluginIndex: Double;
begin
  aPluginIndex:= GetOnlyNumbers(TToolButton(Sender).Name);
  LoadPlugin(trunc(aPluginIndex));
end;

procedure TwfPlugins.Log(const aValue: string);
begin
  if Assigned(onLog) then onLog(self, aValue);
end;

procedure TwfPlugins.DoChange;
var
  aPlugin: TwfPlugin;
  aCurrentStatus: TwfCurrentStatus;
begin
  if PageCount=0 then
    begin
      aCurrentStatus.Max:= 100;
      aCurrentStatus.Progress:=0;
      aCurrentStatus.Marquee:= false;
      aCurrentStatus.Status:='';

      CurrentStatus:= aCurrentStatus;
    end
    else
      SetCurrentStatus2();

  inherited DoChange;
end;

constructor TwfPlugins.Create(TheOwner: TComponent);
var
  aMenuItem: TMenuItem;
begin
  inherited Create(TheOwner);

  fShowFormInTaskBar:= true;

  fPluginList := TStringList.Create(); // created a common list of plugins
  fPluginsLoaded:= TwfPluginLoadedList.Create();

  fPluginIcons:= TImageList.Create(self);
  self.Images:= fPluginIcons;

  OnMouseUp:=@wfOnMouseUp;
  OnMouseDown:=@wfOnMouseDown;

  if IsEmpty(PopupMenu) then
  begin
    fPopupMenu:= TPopupMenu.Create(self);
    fPopupMenu.Parent:= self;
    fPopupMenu.Images:= wfPlugin_Images;

    fPopupMenu.Items.Clear;

    aMenuItem:= NewItem(rsPluginPopupMenuToForm, 0, False, True, @mPluginPageHeadClick, 0, 'mPluginUnpin');
    aMenuItem.ImageIndex:= 0;
    fPopupMenu.Items.Add(aMenuItem);

    aMenuItem:= NewItem(rsPluginPopupMenuClose, 0, False, True, @mPluginPageHeadClick, 0, 'mPluginClose');
    aMenuItem.ImageIndex:= 1;
    fPopupMenu.Items.Add(aMenuItem);
  end;

  Application.QueueAsyncCall(@AsyncInit,0);
end;

destructor TwfPlugins.Destroy;
begin
  FreeAndNil(fPluginIcons);
  FreeAndNil(fPluginList);
  FreeAndNil(fPluginsLoaded);
  inherited Destroy;
end;

function TwfPlugins.AddTabSheet: TwfTabSheet;
begin
  Result := TwfTabSheet.Create(Self);
  Result.PagePlugins := Self;
end;

function TwfPlugins.GetPlugin(aPageIndex: integer): TwfPlugin;
var
  i: Integer;
begin
  for i:= 0 to fPluginsLoaded.Count-1 do
  begin
    Result:= TwfPlugin(fPluginsLoaded.Items[i]);
    if Result.PageIndex = aPageIndex then
      exit;
  end;
  Result:= nil;
end;

function TwfPlugins.GetPluginFromList(aListIndex: integer): TwfPlugin;
begin
  Result:= TwfPlugin(PluginList.Items[aListIndex]);
end;

function TwfPlugins.GetPlugin(aFormName: TComponentName): TwfPlugin;
var
  i: Integer;
begin
  for i:= 0 to fPluginsLoaded.Count-1 do
  begin
    Result:= TwfPlugin(fPluginsLoaded.Items[i]);
    if Result.Form.Name = aFormName then
      exit;
  end;
  Result:= nil;

end;

procedure TwfPlugins.PluginToPage(aPlugin: TwfPlugin);
var
  aTabSheet: TwfTabSheet;
begin
  if (StatusBarInPluginForm) and Assigned(aPlugin.StatusBar) then
  begin
    aPlugin.StatusBarFree;
  end;

  aTabSheet:= AddTabSheet;
  aTabSheet.Caption := aPlugin.Form.Caption;

  PageIndex:= aTabSheet.PageIndex;
  aPlugin.PageIndex:= PageIndex;

  aPlugin.Form.Controls[0].Parent := aTabSheet;
  if aPlugin.Form.Visible then aPlugin.Form.Visible:= false;

  aTabSheet.Enabled := True;
  aTabSheet.ImageIndex:= aPlugin.IndexToPluginList;

  Log(Format(rsPluginToPage,[aPlugin.Form.ClassName, aPlugin.Form.Name]));
  //Event
  DoChange;
end;

procedure TwfPlugins.PluginToForm(aPageIndex: integer);
var
  aPlugin: TwfPlugin;
  aTabSheet: TwfTabSheet;
  pnt: TPoint;
begin
  aPlugin:= GetPlugin(aPageIndex);
  if not Assigned(aPlugin) then exit;

  aTabSheet:= Pages[aPageIndex];
  aTabSheet.Controls[0].Parent:= aPlugin.Form;
  aPlugin.PageIndex:= -1;

  aPlugin.Form.WindowState := wsNormal;
  aPlugin.Form.Height := aTabSheet.Height-100;
  aPlugin.Form.Width := aTabSheet.Width-100;


  aTabSheet.Enabled := False;

  pnt := Mouse.CursorPos;
  aPlugin.Form.Left := pnt.x - 0;
  aPlugin.Form.Top := pnt.y - 0;

  if (StatusBarInPluginForm) and not Assigned(aPlugin.StatusBar) then
  begin
     aPlugin.StatusBar:= TwfStatusProgressBar.Create(aPlugin.Form);
     aPlugin.Form.InsertControl(aPlugin.StatusBar);
     aPlugin.CurrentStatus:= CurrentStatus;
  end;

  aPlugin.Form.Visible:= true;

  Log(Format(rsPluginToForm,[aPlugin.Form.ClassName, aPlugin.Form.Name]));
end;

procedure TwfPlugins.LoadPlugin(aIndex: Integer);
var
  aPluginIndex: LongInt;
  aPlugin: TwfPlugin;
begin
  aPluginIndex:= fPluginsLoaded.Add(TwfPlugin.Create(self,aIndex));
  aPlugin:= TwfPlugin(fPluginsLoaded.Items[aPluginIndex]);
  aPlugin.Form.Visible:= false;

  if fShowFormInTaskBar then
    aPlugin.Form.ShowInTaskBar:= stAlways
  else
    aPlugin.Form.ShowInTaskBar:= stNever;

  aPlugin.onFormClose:=@wfOnPluginFormClose;
  aPlugin.onStatus:=@wfOnPluginStatus;
  aPlugin.onProgress:=@wfOnPluginProgress;
  aPlugin.onLog:= @wfOnLog;

  if aPlugin.Form.Icon.Count>0 then
     fPluginIcons.AddIcon(aPlugin.Form.Icon);

  PluginToPage(aPlugin);

  Log(Format(rsPluginLoaded,[aPlugin.Form.ClassName, aPlugin.Form.Name]));
end;

procedure TwfPlugins.UnLoadPlugin(aPageIndex: Integer);
var
  i: Integer;
  aPlugin: TwfPlugin;
begin
  for i:= 0 to fPluginsLoaded.Count-1 do
  begin
    aPlugin:= TwfPlugin(fPluginsLoaded.Items[i]);
    if aPlugin.PageIndex = aPageIndex then
      begin
        Log(Format(rsPluginUnLoaded,[aPlugin.Form.ClassName, aPlugin.Form.Name]));
        fPluginsLoaded.Delete(i);
        Break;
      end;
  end;
end;

procedure TwfPlugins.RegisterPlugins(TFormsName: array of TFormClass;
  aHints: array of string);
begin
  RegisterPlugins(TFormsName, aHints,[]);
end;

procedure TwfPlugins.RegisterPlugins(TFormsName: array of TFormClass;
  aHints: array of string; aImages: array of integer);
var
  i: Integer;
  aPluginName: String;
  aToolButton: TToolButton;
begin
  for i := 0 to Length(TFormsName) - 1 do
  begin
    Log(Format(rsPluginRegistered,[TFormsName[i].ClassName]));

    aPluginName:= 'Plugin'+IntToStr(i);
    RegisterClassAlias(TFormsName[i],aPluginName); // register classes from array
    fPluginList.Add(aPluginName);
  end;

  if Assigned(fToolBar) then
    begin
      for i:= fPluginList.Count-1 downto 0 do
      begin
        aToolButton:= TToolButton.Create(Self);
        if Length(aHints)>0 then
          begin
            if i<=High(aHints) then
              begin
              aToolButton.ShowHint:= true;
              aToolButton.Hint:= aHints[i];
              end;
          end;

        if Length(aImages)>0 then
          begin
            aToolButton.ImageIndex:=aImages[i];
          end
          else
            aToolButton.ImageIndex:=i;

        aToolButton.Name:= fPluginList.Strings[i];
        aToolButton.OnClick:=@wfOnToolButtolClick;
        fToolBar.ButtonList.Add(aToolButton);
        fToolBar.Buttons[fToolBar.ButtonCount -1].Parent := fToolBar;
      end;
    end;

end;

procedure TwfPlugins.RegisterPlugins(TFormsName: array of TFormClass);
begin
  RegisterPlugins(TFormsName, []);
end;

initialization
  {$i wfplugins_icon.lrs}

  wfPlugin_Images := TImageList.Create(nil);

  with wfPlugin_Images do
  begin
    AddLazarusResource('unpin');
    AddLazarusResource('close');
  end;

finalization
  wfPlugin_Images.Free();
end.
