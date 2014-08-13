unit ChatFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls,
  StdCtrls, ActnList, LCLType, Core;

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
    procedure TextToSendKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    ChatRoom: TChatRoom;
    ContactItemList: TContactItemList;
    procedure AddText(AText: string);
    procedure AddBBCode(AText: string);
    procedure UpdateContactList();
  end;

implementation

{$R *.lfm}

{ TFrameChat }

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

procedure TFrameChat.UpdateContactList();
var
  tv: TTreeView;
  i: integer;
  Item: TContactItem;
  tvItem: TTreeNode;
begin
  if not Assigned(ContactItemList) then Exit;
  tv:=tvUserList;
  tv.BeginUpdate();
  tv.Items.Clear();
  for i:=0 to ContactItemList.Count-1 do
  begin
    Item:=(ContactItemList.Items[i] as TContactItem);
    tvItem:=tv.Items.AddChild(nil, Item.Caption);
    if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    else tvItem.StateIndex:=ciIconUser;
  end;
  tv.EndUpdate();
end;

end.

