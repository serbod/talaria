unit dnmp_mail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, dnmp_unit, dnmp_services;

type
  { TDnmpMailMessage }
  // Mail message
  // Contain text and basic info about author
  // Serializable
  TDnmpMailMessage = class(TInterfacedObject)
  public
    Timestamp: TDateTime;
    Author: TDnmpContact;
    AuthorAddr: TAddr;
    AuthorName: string;
    AuthorGUID: string;
    Topic: string;
    Text: string;
    Unread: boolean;
    procedure FillMsg(Msg: TDnmpMsg);
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  TDnmpMail = class;
  TDnmpMailboxType = (mbtInbox, mbtOutbox, mbtStore);

  { TDnmpMailbox }
  // Mail messages list
  // Serializable
  TDnmpMailbox = class(TInterfacedObject)
  private
    FMailer: TDnmpMail;
    FMessagesList: TObjectList;
    function GetItem(Index: Integer): TDnmpMailMessage;
    procedure SetItem(Index: Integer; Value: TDnmpMailMessage);
    function FCount(): integer;
    function FCountUnread(): integer;
  public
    Name: string;
    BoxType: TDnmpMailboxType;
    MaxCount: Integer;
    DestAddr: TAddr;
    constructor Create(AMailer: TDnmpMail);
    destructor Destroy(); override;
    property Mailer: TDnmpMail read FMailer;
    property Items[Index: Integer]: TDnmpMailMessage read GetItem write SetItem; default;
    property Count: integer read FCount;
    property CountUnread: integer read FCountUnread;
    function AddItem(Item: TDnmpMailMessage): Integer; overload;
    { Add message to list:
      Timestamp - datetime
      Topic - message topic
      Text - message text
      AuthorGUID
      Abonent - Author record (optional), for name and address }
    function AddItem(ATimestamp: TDateTime; ATopic: string; AText: string; AAuthorGUID: string; AAbonent: TDnmpContact = nil): TDnmpMailMessage; overload;
    function DeleteItem(Item: TDnmpMailMessage): boolean;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): AnsiString;
    function LoadFromString(s: AnsiString): boolean;
  end;

  { TDnmpMail }
  // DNMP Mail base (client-server independent)
  // Serializable
  TDnmpMail = class(TDnmpService)
  protected
    // Create channel message with specified text and author
    function CreateMailMsg(AbonGUID, sTopic, sText: string): TDnmpMailMessage;
    // Send channel message to specified address
    function SendMailMsg(Addr: TAddr; MailMsg: TDnmpMailMessage): Boolean;
    { Send mailbox content to addr }
    function SendData(Addr: TAddr; sBoxName, sData: string): boolean;
  public
    Author: TDnmpContact;
    MailboxList: TStringList;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    function SendCmd(Text: string; Addr: TAddr): string;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
    function MailboxCount(): integer;
    function GetMailbox(Index: integer): TDnmpMailbox;
  end;


implementation

{ TDnmpMailbox }

function TDnmpMailbox.GetItem(Index: Integer): TDnmpMailMessage;
begin
  Result:=(FMessagesList.Items[index] as TDnmpMailMessage);
end;

procedure TDnmpMailbox.SetItem(Index: Integer; Value: TDnmpMailMessage);
begin
  FMessagesList.Items[Index]:=Value
end;

function TDnmpMailbox.FCount(): integer;
begin
  Result:=FMessagesList.Count;
end;

function TDnmpMailbox.FCountUnread(): integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to self.Count-1 do
  begin
    if self.GetItem(i).Unread then Inc(Result);
  end;
end;

constructor TDnmpMailbox.Create(AMailer: TDnmpMail);
begin
  inherited Create();
  FMailer:=AMailer;
  FMessagesList:=TObjectList.Create(True);
end;

destructor TDnmpMailbox.Destroy();
begin
  FreeAndNil(FMessagesList);
  inherited Destroy();
end;

function TDnmpMailbox.AddItem(Item: TDnmpMailMessage): Integer;
begin
  Result:=FMessagesList.Add(Item);
end;

function TDnmpMailbox.AddItem(ATimestamp: TDateTime; ATopic: string;
  AText: string; AAuthorGUID: string; AAbonent: TDnmpContact): TDnmpMailMessage;
var
  i: Integer;
begin
  Result:=nil;
  // Check for duplicate
  for i:=0 to Count-1 do
  begin
    Result:=self.GetItem(i);
    if (Result.Timestamp = ATimestamp) and (Result.AuthorGUID = AAuthorGUID) then Exit;
  end;
  Result:=TDnmpMailMessage.Create();
  Result.Timestamp:=ATimestamp;
  Result.Topic:=ATopic;
  Result.Text:=AText;
  Result.AuthorGUID:=AAuthorGUID;
  Result.Author:=AAbonent;
  if Assigned(AAbonent) then
  begin
    Result.AuthorAddr:=AAbonent.Addr;
    Result.AuthorName:=AAbonent.Nick;
  end;
  self.AddItem(Result);
end;

function TDnmpMailbox.DeleteItem(Item: TDnmpMailMessage): boolean;
begin
  Result:=(self.FMessagesList.Remove(Item) <> -1);
end;

function TDnmpMailbox.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
  i: Integer;
begin
  Storage:=TDnmpStorage.Create(stDictionary);
  for i:=0 to Self.Count-1 do
  begin
    Storage.Add(IntToStr(i), Self.Items[i].ToStorage());
  end;

  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpMailbox');
  Result.Add('name', Self.Name);
  Result.Add('items', Storage);
end;

function TDnmpMailbox.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpMailMessage;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpMailbox' then Exit;
  Self.Name:=Storage.GetString('name');
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=TDnmpMailMessage.Create();
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    self.AddItem(Item);
  end;
  Result:=True;
end;

function TDnmpMailbox.SaveToString: AnsiString;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpMailbox.LoadFromString(s: AnsiString): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  if not StorageFromJson(Storage, s) then Exit;
  Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;


{ TDnmpMail }

function TDnmpMail.CreateMailMsg(AbonGUID, sTopic, sText: string
  ): TDnmpMailMessage;
var
  Abon: TDnmpContact;
begin
  Result:=nil;
  Result:=TDnmpMailMessage.Create();
  Result.Timestamp:=Now;
  Result.Topic:=sTopic;
  Result.AuthorGUID:=AbonGUID;
  Result.Text:=sText;

  Abon:=Self.GetAbonentByGUID(AbonGUID);
  Result.Author:=Abon;
  if Assigned(Abon) then
  begin
    Result.AuthorAddr:=Abon.Addr;
    Result.AuthorName:=Abon.Nick;
  end;
end;

function TDnmpMail.SendMailMsg(Addr: TAddr; MailMsg: TDnmpMailMessage): Boolean;
var
  sl: TStringList;
begin
  Result:=False;
  if not Assigned(MailMsg) then Exit;
  sl:=TStringList.Create();
  sl.Values['name']:=self.ServiceInfo.Name;
  sl.Values['author_addr']:=AddrToStr(MailMsg.AuthorAddr);
  sl.Values['author_name']:=MailMsg.AuthorName;
  sl.Values['author_guid']:=MailMsg.AuthorGUID;
  sl.Values['topic']:=MailMsg.Topic;
  sl.Values['timestamp']:=TimestampToStr(MailMsg.Timestamp);

  Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, sl.Text, MailMsg.Text);
  sl.Free();
  Result:=True;
end;

function TDnmpMail.SendData(Addr: TAddr; sBoxName, sData: string): boolean;
begin

end;

constructor TDnmpMail.Create(AMgr: TDnmpManager;
  AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
var
  Item: TDnmpMailbox;
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  Self.MailboxList:=TStringList.Create();
  Self.MailboxList.OwnsObjects:=True;

  // default mailboxes
  Item:=TDnmpMailbox.Create(self);
  Item.Name:='Inbox';
  Item.BoxType:=mbtInbox;
  Self.MailboxList.AddObject(Item.Name, Item);

  Item:=TDnmpMailbox.Create(self);
  Item.Name:='Outbox';
  Item.BoxType:=mbtOutbox;
  Self.MailboxList.AddObject(Item.Name, Item);

  Item:=TDnmpMailbox.Create(self);
  Item.Name:='Store';
  Item.BoxType:=mbtStore;
  Self.MailboxList.AddObject(Item.Name, Item);
end;

destructor TDnmpMail.Destroy();
begin
  FreeAndNil(Self.MailboxList);
  inherited Destroy();
end;

function TDnmpMail.SendCmd(Text: string; Addr: TAddr): string;
begin

end;

function TDnmpMail.ToStorage: TDnmpStorage;
begin
  Result:=inherited ToStorage;
end;

function TDnmpMail.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=inherited FromStorage(Storage);
end;

function TDnmpMail.MailboxCount(): integer;
begin
  Result:=Self.MailboxList.Count;
end;

function TDnmpMail.GetMailbox(Index: integer): TDnmpMailbox;
begin
  Result:=(Self.MailboxList.Objects[Index] as TDnmpMailbox);
end;


{ TDnmpMailMessage }

procedure TDnmpMailMessage.FillMsg(Msg: TDnmpMsg);
begin
  if not Assigned(Msg) then Exit;
  //sl.Values['name']:=self.ServiceInfo.Name;
  Msg.MsgType:='MAIL';
  Msg.Info.Values['author_addr']:=AddrToStr(AuthorAddr);
  Msg.Info.Values['author_name']:=AuthorName;
  Msg.Info.Values['author_guid']:=AuthorGUID;
  Msg.Info.Values['timestamp']:=TimestampToStr(Timestamp);
  Msg.Info.Values['topic']:=Topic;
  StrToStream(Text, Msg.Data);
end;

function TDnmpMailMessage.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('timestamp', Timestamp);
  //Result.Add('', Author);
  Result.Add('author_addr', AddrToStr(AuthorAddr));
  Result.Add('author_name', AuthorName);
  Result.Add('author_guid', AuthorGUID);
  Result.Add('topic', Topic);
  Result.Add('text', Text);
end;

function TDnmpMailMessage.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  Self.Timestamp:=Storage.GetReal('timestamp');
  Self.AuthorAddr:=StrToAddr(Storage.GetString('author_addr'));
  Self.AuthorName:=Storage.GetString('author_name');
  Self.AuthorGUID:=Storage.GetString('author_guid');
  Self.Topic:=Storage.GetString('topic');
  // Self.Author:=??;
  Self.Text:=Storage.GetString('text');
  Result:=True;
end;

end.

