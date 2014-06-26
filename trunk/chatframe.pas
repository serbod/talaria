unit ChatFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, ExtCtrls, ComCtrls,
  StdCtrls, ActnList, LCLType;

type

  { TFrameChat }

  TFrameChat = class(TFrame)
    actBold: TAction;
    actColor: TAction;
    actClearText: TAction;
    actFreezeScrolling: TAction;
    actInsertNick: TAction;
    actInsertPrivate: TAction;
    actInfoAboutUser: TAction;
    actSmiles: TAction;
    actUnderline: TAction;
    actItalic: TAction;
    alChat: TActionList;
    MemoText: TMemo;
    TextToSend: TMemo;
    panAvatar: TPanel;
    panMessage: TPanel;
    panRight: TPanel;
    panLeft: TPanel;
    SplitterH: TSplitter;
    SplitterAvatar: TSplitter;
    SplitterV: TSplitter;
    ToolBarChat: TToolBar;
    tbBold: TToolButton;
    tbItalic: TToolButton;
    tbUnderline: TToolButton;
    ToolButton1: TToolButton;
    tbColor: TToolButton;
    tbSmiles: TToolButton;
    ToolButton2: TToolButton;
    tbFreezeScrolling: TToolButton;
    tvUserList: TTreeView;
    procedure actItalicExecute(Sender: TObject);
    procedure TextToSendChange(Sender: TObject);
    procedure TextToSendKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    procedure AddText(AText: string);
    procedure AddBBCode(AText: string);
  end;

implementation

{$R *.lfm}

{ TFrameChat }

procedure TFrameChat.actItalicExecute(Sender: TObject);
begin

end;

procedure TFrameChat.TextToSendChange(Sender: TObject);
begin

end;

procedure TFrameChat.TextToSendKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
  begin
    AddText(TextToSend.Text);
    TextToSend.Text:='';
    Key:=0;
  end;
end;

procedure TFrameChat.AddText(AText: string);
begin
  MemoText.Append(AText);
end;

procedure TFrameChat.AddBBCode(AText: string);
begin
  //MemoText.Lines;
end;

end.

