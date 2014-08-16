unit adapters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_mail;

type

  { TMailMessage }

  TMailMessage = class(TInterfacedObject)
  protected
    function FGetTopic(): string; virtual;
    function FGetText(): string; virtual;
  public
    DataObject: TObject;
    property Topic: string read FGetTopic;
    property Text: string read FGetText;
  end;

  { TMailBox }

  TMailBox = class(TInterfacedObject)
  protected
    function FGetName(): string; virtual;
    function FGetParent(): TMailBox; virtual;
  public
    DataObject: TObject;
    property Parent: TMailBox read FGetParent;
    property Name: string read FGetName;
    function MessagesCount(): integer; virtual;
    function UnreadMessagesCount(): integer; virtual;
    function GetMessage(Index: integer): TMailMessage; virtual;
    function IsGroup(): boolean; virtual;
    function CreateMessage(): TMailMessage; virtual;
  end;

  { TMailRoom }

  TMailRoom = class(TInterfacedObject)
  public
    DataObject: TObject;
    function MailboxCount(): integer; virtual;
    function GetMailbox(Index: integer): TMailBox; virtual;
  end;


implementation

{ TMailRoom }

function TMailRoom.MailboxCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMail) then
  begin
    Result:=(DataObject as TDnmpMail).MailboxCount();
  end;
end;

function TMailRoom.GetMailbox(Index: integer): TMailBox;
var
  TmpItem: TDnmpMailbox;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMail) then
  begin
    TmpItem:=(DataObject as TDnmpMail).GetMailbox(Index);
    if not Assigned(TmpItem) then Exit;
    Result:=TMailBox.Create();
    Result.DataObject:=TmpItem;
  end;
end;

{ TMailBox }

function TMailBox.FGetName(): string;
begin
  Result:='Mailbox';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailbox) then Result:=(DataObject as TDnmpMailbox).Name;
end;

function TMailBox.FGetParent(): TMailBox;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;

end;

function TMailBox.MessagesCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailbox) then Result:=(DataObject as TDnmpMailbox).Count;
end;

function TMailBox.UnreadMessagesCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
end;

function TMailBox.GetMessage(Index: integer): TMailMessage;
var
  TmpItem: TDnmpMailMessage;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailbox) then
  begin
    TmpItem:=(DataObject as TDnmpMailbox).Items[Index];
    if not Assigned(TmpItem) then Exit;
    Result:=TMailMessage.Create();
    Result.DataObject:=TmpItem;
  end;
end;

function TMailBox.IsGroup(): boolean;
begin
  Result:=False;
end;

function TMailBox.CreateMessage(): TMailMessage;
var
  TmpItem: TDnmpMailMessage;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailbox) then
  begin
    TmpItem:=TDnmpMailMessage.Create();
    if not Assigned(TmpItem) then Exit;
    (DataObject as TDnmpMailbox).AddItem(TmpItem);
    //TmpItem.Author:=;
    Result:=TMailMessage.Create();
    Result.DataObject:=TmpItem;
  end;

end;

{ TMailMessage }

function TMailMessage.FGetTopic(): string;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessage) then Result:=(DataObject as TDnmpMailMessage).Topic;
end;

function TMailMessage.FGetText(): string;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessage) then Result:=(DataObject as TDnmpMailMessage).Text;
end;


end.

