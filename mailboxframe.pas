unit MailboxFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  ActnList, Menus, Core, dnmp_mail;

type

  { TFrameMailbox }

  TFrameMailbox = class(TFrame)
    actCreateMessage: TAction;
    actDeleteMessage: TAction;
    alMessageText: TActionList;
    alMessages: TActionList;
    alMailboxes: TActionList;
    gbMailboxTree: TGroupBox;
    gbMessagesList: TGroupBox;
    gbMessageText: TGroupBox;
    memoMessageText: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    panRight: TPanel;
    pmMailboxes: TPopupMenu;
    pmMessages: TPopupMenu;
    pmMessageText: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tvMessagesTree: TTreeView;
    tvMailboxes: TTreeView;
    procedure actCreateMessageExecute(Sender: TObject);
    procedure actDeleteMessageExecute(Sender: TObject);
    procedure tvMailboxesSelectionChanged(Sender: TObject);
    procedure tvMessagesTreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    function FGetSelectedMailbox(): TDnmpMailbox;
    function FGetSelectedMailMessage(): TDnmpMailMessage;
  public
    { public declarations }
    MailRoom: TDnmpMail;
    // current mailbox
    Mailbox: TDnmpMailbox;
    // current message
    MailMessage: TDnmpMailMessage;
    property SelectedMailbox: TDnmpMailbox read FGetSelectedMailbox;
    property SelectedMailMessage: TDnmpMailMessage read FGetSelectedMailMessage;
    procedure UpdateMailboxes();
    procedure UpdateMessages();
    procedure UpdateMessageText();
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFrameMailbox }

procedure TFrameMailbox.tvMailboxesSelectionChanged(Sender: TObject);
begin
  if Assigned(SelectedMailbox) then Mailbox:=SelectedMailbox;
  UpdateMessages();
end;

procedure TFrameMailbox.actCreateMessageExecute(Sender: TObject);
begin
  if not Assigned(Mailbox) then Exit;
  if not Assigned(MailRoom) then Exit;
  // ...
  // test message
  Mailbox.AddItem(Now(), 'test topic '+IntToStr(Mailbox.Count+1), 'test message text '+IntToStr(Mailbox.Count+1), MailRoom.Author.GUID);
  UpdateMessages();
end;

procedure TFrameMailbox.actDeleteMessageExecute(Sender: TObject);
begin
  if not Assigned(Mailbox) then Exit;
  if not Assigned(MailMessage) then Exit;
  Mailbox.DeleteItem(MailMessage);
  MailMessage:=nil;
  UpdateMessages();
  UpdateMessageText();
end;

procedure TFrameMailbox.tvMessagesTreeSelectionChanged(Sender: TObject);
begin
  if Assigned(SelectedMailMessage) then MailMessage:=SelectedMailMessage;
  UpdateMessageText();
end;

function TFrameMailbox.FGetSelectedMailbox(): TDnmpMailBox;
begin
  Result:=nil;
  if not Assigned(tvMailboxes.Selected) then Exit;
  if not Assigned(tvMailboxes.Selected.Data) then Exit;
  Result:=TDnmpMailBox(tvMailboxes.Selected.Data);
end;

function TFrameMailbox.FGetSelectedMailMessage: TDnmpMailMessage;
begin
  Result:=nil;
  if not Assigned(tvMessagesTree.Selected) then Exit;
  if not Assigned(tvMessagesTree.Selected.Data) then Exit;
  Result:=TDnmpMailMessage(tvMessagesTree.Selected.Data);
end;

procedure TFrameMailbox.UpdateMailboxes();
var
  tv: TTreeView;
  i: integer;
  Item: TDnmpMailBox;
  tvItem: TTreeNode;
begin
  if not Assigned(MailRoom) then Exit;
  tv:=tvMailboxes;
  tv.BeginUpdate();
  tv.Items.Clear();
  for i:=0 to MailRoom.MailboxCount()-1 do
  begin
    Item:=MailRoom.GetMailbox(i);
    tvItem:=tv.Items.AddChild(nil, Item.Name);
    tvItem.Data:=Item;
    //if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    //else tvItem.StateIndex:=ciIconNote;
    tvItem.StateIndex:=ciIconFolder;
  end;
  tv.EndUpdate();
end;

procedure TFrameMailbox.UpdateMessages();
var
  tv: TTreeView;
  i: integer;
  Item: TDnmpMailMessage;
  tvItem: TTreeNode;
begin
  if not Assigned(Mailbox) then Exit;

  gbMessagesList.Caption:=Mailbox.Name+' - '
  +IntToStr(Mailbox.Count)+' messages, '
  +IntToStr(Mailbox.CountUnread)+' unread';

  tv:=tvMessagesTree;
  tv.BeginUpdate();
  tv.Items.Clear();
  for i:=0 to Mailbox.Count-1 do
  begin
    Item:=Mailbox.Items[i];
    tvItem:=tv.Items.AddChild(nil, Item.Topic);
    tvItem.Data:=Item;
    //if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    //else tvItem.StateIndex:=ciIconNote;
    tvItem.StateIndex:=ciIconNote;
  end;
  tv.EndUpdate();
end;

procedure TFrameMailbox.UpdateMessageText();
begin
  if not Assigned(MailMessage) then
  begin
    memoMessageText.Clear();
    gbMessageText.Caption:='';
    Exit;
  end;

  gbMessageText.Caption:=MailMessage.Topic+' - '
  +IntToStr(Length(MailMessage.Text))+' bytes';

  memoMessageText.Lines.Text:=MailMessage.Text;
end;

procedure TFrameMailbox.Update();
begin
  UpdateMailboxes();
  inherited Update();
end;


end.

