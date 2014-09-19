unit ChatFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls,
  StdCtrls, ActnList, LCLType, Menus, Core;

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
    actUpdateChatText: TAction;
    actUpdateContactList: TAction;
    actSmiles: TAction;
    actUnderline: TAction;
    actItalic: TAction;
    alChat: TActionList;
    imgAvatar: TImage;
    MemoText: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    pmContactlist: TPopupMenu;
    pmChatText: TPopupMenu;
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
    procedure actUpdateChatTextExecute(Sender: TObject);
    procedure actUpdateContactListExecute(Sender: TObject);
    procedure TextToSendKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvUserListSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FChatRoom: TChatRoom;
    procedure FSetChatRoom(Value: TChatRoom);
  public
    { public declarations }
    property ChatRoom: TChatRoom read FChatRoom write FSetChatRoom;
    procedure AddText(AText: string);
    procedure AddBBCode(AText: string);
    procedure UpdateContactList();
    procedure UpdateText();
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFrameChat }

procedure TFrameChat.TextToSendKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
  begin
    //AddText(TextToSend.Text);
    if Assigned(ChatRoom) then ChatRoom.SendText(TextToSend.Text);
    TextToSend.Text:='';
    Key:=0;
  end;
end;

procedure TFrameChat.tvUserListSelectionChanged(Sender: TObject);
var
  Item: TContactItem;
begin
  if not Assigned(tvUserList.Selected) then Exit;
  if not Assigned(tvUserList.Selected.Data) then Exit;
  Item:=TContactItem(tvUserList.Selected.Data);
  Core.PictureFromString(imgAvatar.Picture, Item.Picture);
end;

procedure TFrameChat.FSetChatRoom(Value: TChatRoom);
begin
  FChatRoom:=Value;
  Update();
end;

procedure TFrameChat.actUpdateContactListExecute(Sender: TObject);
begin
  UpdateContactList();
end;

procedure TFrameChat.actUpdateChatTextExecute(Sender: TObject);
begin
  UpdateText();
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
  if not Assigned(ChatRoom) then Exit;
  tv:=tvUserList;
  tv.BeginUpdate();
  tv.Items.Clear();
  for i:=0 to ChatRoom.ContactCount-1 do
  begin
    Item:=ChatRoom.GetContact(i);
    tvItem:=tv.Items.AddChild(nil, Item.Caption);
    tvItem.Data:=Item;
    tvItem.StateIndex:=Item.StateIcon();
    //if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    //else tvItem.StateIndex:=ciIconUserBlue;
  end;
  tv.EndUpdate();
end;

procedure TFrameChat.UpdateText();
var
  i: integer;
  ChatText: TChatText;
begin
  if not Assigned(ChatRoom) then Exit;
  MemoText.Lines.Clear();
  for i:=0 to ChatRoom.TextCount-1 do
  begin
    ChatText:=ChatRoom.GetText(i);
    MemoText.Lines.AddObject(FormatDateTime('[hh:nn:ss]', ChatText.Timestamp)+'<'+ChatText.AuthorName+'> '+ChatText.Text, nil);
  end;
end;

procedure TFrameChat.Update();
begin
  UpdateContactList();
  UpdateText();
  inherited Update();
end;

end.

