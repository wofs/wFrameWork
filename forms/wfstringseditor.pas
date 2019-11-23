unit wfStringsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, LCLType, ComCtrls, wfTypes,
  SynEdit, SynCompletion, SynHighlighterSQL, SynHighlighterMulti, SynPluginSyncroEdit, SynEditTypes,
  SynEditMarkupHighAll, SynHighlighterIni, SynHighlighterTeX, SynHighlighterAny, LazUTF8, LazStringUtils;

type

  TOnAfterSearch = procedure (Sender:TObject; cnt: Integer) of object;

  { TFmStringsEditor }

  TFmStringsEditor = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbReplace: TCheckBox;
    cbReplaceAll: TCheckBox;
    cbCaseSensitive: TCheckBox;
    edSearch: TEdit;
    edReplace: TEdit;
    gbSearch: TGroupBox;
    gbHelp: TGroupBox;
    gbReplace: TGroupBox;
    lbHelp: TLabel;
    lbSearch: TLabel;
    mHelp: TMemo;
    pSearch: TPanel;
    pSearchVisible: TPanel;
    pBottom: TPanel;
    pCenter: TPanel;
    Editor: TSynEdit;
    splitSearch: TSplitter;
    StatusBar1: TStatusBar;
    SynText: TSynAnySyn;
    SynAutoComplete: TSynAutoComplete;
    SynCompletion: TSynCompletion;
    SynIni: TSynIniSyn;
    SynPluginSyncroEdit: TSynPluginSyncroEdit;
    SynSQL: TSynSQLSyn;
    procedure cbReplaceChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure pSearchVisibleClick(Sender: TObject);
    procedure AfterSearch(Sender: TObject; cnt: Integer);

  private
    function ClearLineFromComment(const aText: string): string;
    function CommentText(const aText: string): string;
    function GetCaretPositionAsString: string;
    procedure InitMode(aMode: TwfStringsEditorMode);
    procedure SetModeIni();
    procedure SetModeSQL();
    procedure SetModeText();
    procedure SetSelectCurrentRow;
    function GetOptions: TSynSearchOptions;
    procedure InitHighlightAllCaret();
    procedure SetStatus(aText: string; const aPanel: integer=0);
    function UncommentLines(const aText: string): string;

  private
    FBackwards: Boolean;
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure DoSearch;

  public
    constructor Create(aStrings: TStrings; aMode: TwfStringsEditorMode);

  end;

var
  FmStringsEditor: TFmStringsEditor;

implementation

{$R *.lfm}

{ TFmStringsEditor }

procedure TFmStringsEditor.pSearchVisibleClick(Sender: TObject);
begin
  pSearch.Visible:= not pSearch.Visible;
  splitSearch.Enabled:= pSearch.Visible;

  if pSearch.Visible then
    Self.Width:= self.Width + pSearch.Width
  else
    Self.Width:= Self.Width - pSearch.Width;
end;

procedure TFmStringsEditor.AfterSearch(Sender: TObject; cnt: Integer);
begin
  SetStatus('Last search : "'+edSearch.Text+'" matched: '+inttostr(cnt),1);
end;

procedure TFmStringsEditor.SetStatus(aText: string; const aPanel: integer = 0);
begin
  StatusBar1.Panels[aPanel].Text := aText;
end;

procedure TFmStringsEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F) and (ssCtrl in Shift) then
  begin
    pSearchVisible.OnClick(self);
    if pSearch.Visible then edSearch.SetFocus;
  end;
end;

function  TFmStringsEditor.GetCaretPositionAsString:string;
begin
  Result:= Format('[Cursor] Row:%d Col:%d',[Editor.CaretY, Editor.CaretX]);
end;

procedure TFmStringsEditor.SetSelectCurrentRow;
var
  i, aResult: Integer;
begin
  aResult:= 0;

   for i:=0 to Editor.CaretY-2 do begin
      aResult += Editor.Lines[i].Length+ UTF8Length(LineEnding);
   end;

   inc(aResult);

   begin
     Editor.SelStart:= aResult;
     Editor.SelEnd:= UTF8Length(Editor.Lines[Editor.CaretY-1])+aResult;;
   end;
end;

procedure TFmStringsEditor.EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  aSelText: String;
begin
  SetStatus(GetCaretPositionAsString);
  if (Key = VK_ESCAPE) then
    SetStatus('',1);

  if (Key = VK_LCL_SLASH) and (ssCtrl in Shift) then
  begin
    if (UTF8Length(Editor.SelText) = 0) then
      SetSelectCurrentRow;

     aSelText:= Editor.SelText;

   if UTF8Length(aSelText)>0 then
     Editor.SelText:= UTF8Trim(CommentText(aSelText));
  end;
end;

procedure TFmStringsEditor.EditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetStatus(GetCaretPositionAsString);
end;

procedure TFmStringsEditor.cbReplaceChange(Sender: TObject);
begin
  gbReplace.Visible:= TCheckBox(Sender).Checked;
end;

function TFmStringsEditor.CommentText(const aText: string): string;
begin
  Result:=LazStringUtils.CommentText(aText,comtCPP);

  if (UTF8Pos('/*', aText)>0) and (UTF8Pos('*/', aText)>0) then
    Result:=UncommentLines(aText)
  else
    Result:=LazStringUtils.CommentText(aText,comtCPP);
end;

function TFmStringsEditor.ClearLineFromComment(const aText: string):string;
var
  i,k: Integer;
  aChar: Char;
begin
    Result:= UTF8Trim(aText);
    k:= 2;

    for i:= 1 to UTF8Length(aText) do
      begin
        aChar:= aText[i];
        if (aChar in ['/','*','\']) then
           UTF8Delete(Result, 1, 1);

        //if (aChar in [' ']) then
        //  inc(k);

        if i>k then break;
      end;
end;

function TFmStringsEditor.UncommentLines(const aText: string): string;
var
  aStrings: TStringList;
  i: Integer;
  aCurString: String;
begin
  Result:= EmptyStr;
  aStrings:= TStringList.Create;

  try
    aStrings.Text:=aText;

    for i:= 0 to aStrings.Count-1 do begin
      begin
        aCurString:= aStrings.Strings[i];
        if UTF8Length(Result)>0 then
          Result += EndOfLine;

        Result += ClearLineFromComment(aCurString);
      end;
    end;
  finally
    FreeAndNil(aStrings);
  end;

end;

procedure TFmStringsEditor.edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0;
    if ssShift in Shift then
      actFindPreviousExecute(nil)
    else
      actFindNextExecute(nil);
  end;

  if (Key = VK_ESCAPE) OR (Key = VK_F) and (ssCtrl in Shift) then begin
    Key := 0;
    pSearchVisible.OnClick(self);
    Editor.SetFocus;
  end;
end;

procedure TFmStringsEditor.FormShow(Sender: TObject);
begin
  Editor.SetFocus;
end;

procedure TFmStringsEditor.actFindPreviousExecute(Sender :TObject);
begin
  FBackwards := True;
  DoSearch;
end;

procedure TFmStringsEditor.actFindNextExecute(Sender :TObject);
begin
  FBackwards := False;
  DoSearch;
end;

procedure TFmStringsEditor.DoSearch;
var
  cnt: Integer;
begin
  if cbReplace.Checked then
    cnt := Editor.SearchReplace(edSearch.Text, edReplace.Text, GetOptions)
  else
    cnt := Editor.SearchReplace(edSearch.Text, '', GetOptions);

  AfterSearch(Editor, cnt);
end;

function TFmStringsEditor.GetOptions :TSynSearchOptions;
begin
  Result := [ssoFindContinue];
  if FBackwards then
    Result := Result+[ssoBackwards];

  if cbReplace.Checked then
  begin
    Result := Result+[ssoReplace];

   if cbReplaceAll.Checked then
     Result := Result+[ssoReplaceAll];
   end;

  //if actWholeScope.Checked then Result := [ssoEntireScope];
  //if actSelectOnly.Checked then Result := [ssoSelectedOnly];
  //
  if cbCaseSensitive.Checked    then Result := Result+[ssoMatchCase];
  //if actWholeWords.Checked       then Result := Result+[ssoWholeWord];
  //if FBackwards                  then Result := Result+[ssoBackwards];
  //if cbReplace.Checked  then begin
  //  Result := Result+[ssoReplace];
  //  if actReplaceAll.Checked       then Result := Result+[ssoReplaceAll];
  //  if actPromptOnReplace.Checked  then Result := Result+[ssoPrompt];

end;

constructor TFmStringsEditor.Create(aStrings: TStrings; aMode: TwfStringsEditorMode);
begin
  inherited Create(nil);
  FBackwards:= false;
  Editor.Lines.Assign(aStrings);
  InitMode(aMode);
  InitHighlightAllCaret();
end;

procedure TFmStringsEditor.InitMode(aMode: TwfStringsEditorMode);
begin
  case aMode of
    stmText  : SetModeText;
    stmSQL   : SetModeSQL;
    stmIni   : SetModeIni;
  end;
end;

procedure TFmStringsEditor.SetModeSQL();
begin
   self.Caption:= Format('%s [%s]',[self.Caption, 'SQL']);
   Editor.Highlighter:= SynSQL;
end;

procedure TFmStringsEditor.SetModeText();
begin
   self.Caption:= Format('%s [%s]',[self.Caption, 'Text']);
   Editor.Highlighter:= SynText;
end;

procedure TFmStringsEditor.SetModeIni();
begin
   self.Caption:= Format('%s [%s]',[self.Caption, 'Ini']);
   Editor.Highlighter:= SynIni;
   SynCompletion.ItemList.Clear;
   SynAutoComplete.AutoCompleteList.Clear;
end;

procedure TFmStringsEditor.InitHighlightAllCaret();
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;
begin
  SynMarkup := TSynEditMarkupHighlightAllCaret(Editor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);

  SynMarkup.MarkupInfo.FrameColor := clGreen;
  SynMarkup.MarkupInfo.Background := clGradientInactiveCaption;

  SynMarkup.WaitTime := 100; // millisec
  SynMarkup.Trim := True;     // no spaces, if using selection
  SynMarkup.FullWord := True; // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := False;
end;

end.

