unit MailboxFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  ActnList, Menus, Core;

type

  { TFrameMailbox }

  TFrameMailbox = class(TFrame)
    alMessageText: TActionList;
    alMessages: TActionList;
    alMailboxes: TActionList;
    gbMailboxTree: TGroupBox;
    gbMessagesList: TGroupBox;
    gbMessageText: TGroupBox;
    memoMessageText: TMemo;
    panRight: TPanel;
    pmMailboxes: TPopupMenu;
    pmMessages: TPopupMenu;
    pmMessageText: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tvMessagesTree: TTreeView;
    tvMailboxes: TTreeView;
    procedure tvMailboxesSelectionChanged(Sender: TObject);
    procedure tvMessagesTreeSelectionChanged(Sender: TObject);
  private
    { private declarations }
    function FGetSelectedMailbox(): TMailBox;
    function FGetSelectedMailMessage(): TMailMessage;
  public
    { public declarations }
    MailRoom: TMailRoom;
    // current mailbox
    Mailbox: TMailbox;
    // current message
    MailMessage: TMailMessage;
    property SelectedMailbox: TMailBox read FGetSelectedMailbox;
    property SelectedMailMessage: TMailMessage read FGetSelectedMailMessage;
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

procedure TFrameMailbox.tvMessagesTreeSelectionChanged(Sender: TObject);
begin
  if Assigned(SelectedMailMessage) then MailMessage:=SelectedMailMessage;
  UpdateMessageText();
end;

function TFrameMailbox.FGetSelectedMailbox(): TMailBox;
begin
  Result:=nil;
  if not Assigned(tvMailboxes.Selected) then Exit;
  if not Assigned(tvMailboxes.Selected.Data) then Exit;
  Result:=TMailBox(tvMailboxes.Selected.Data);
end;

function TFrameMailbox.FGetSelectedMailMessage: TMailMessage;
begin
  Result:=nil;
  if not Assigned(tvMessagesTree.Selected) then Exit;
  if not Assigned(tvMessagesTree.Selected.Data) then Exit;
  Result:=TMailMessage(tvMessagesTree.Selected.Data);
end;

procedure TFrameMailbox.UpdateMailboxes();
var
  tv: TTreeView;
  i: integer;
  Item: TMailBox;
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
    if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    else tvItem.StateIndex:=ciIconNote;
  end;
  tv.EndUpdate();
end;

procedure TFrameMailbox.UpdateMessages();
var
  tv: TTreeView;
  i: integer;
  Item: TMailMessage;
  tvItem: TTreeNode;
begin
  if not Assigned(Mailbox) then Exit;

  gbMessagesList.Caption:=Mailbox.Name+' - '
  +IntToStr(Mailbox.MessagesCount())+' messages, '
  +IntToStr(Mailbox.UnreadMessagesCount())+' unread';

  tv:=tvMessagesTree;
  tv.BeginUpdate();
  tv.Items.Clear();
  for i:=0 to Mailbox.MessagesCount()-1 do
  begin
    Item:=Mailbox.GetMessage(i);
    tvItem:=tv.Items.AddChild(nil, Item.Topic);
    tvItem.Data:=Item;
    //if Item.IsGroup then tvItem.StateIndex:=ciIconFolder
    //else tvItem.StateIndex:=ciIconNote;
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

