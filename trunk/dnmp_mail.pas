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
    Author: TDnmpAbonent;
    AuthorAddr: TAddr;
    AuthorName: string;
    AuthorGUID: string;
    Topic: string;
    Text: string;
    procedure FillMsg(Msg: TDnmpMsg);
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  { TDnmpMailMessagesList }
  // Mail messages list
  // Serializable
  TDnmpMailMessagesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpMailMessage;
    procedure SetItem(Index: Integer; Value: TDnmpMailMessage);
  public
    property Items[Index: Integer]: TDnmpMailMessage read GetItem write SetItem; default;
    function AddItem(Item: TDnmpMailMessage): Integer; overload;
    { Add message to list:
      Timestamp - datetime
      Topic - message topic
      Text - message text
      AuthorGUID
      Abonent - Author record (optional), for name and address }
    function AddItem(ATimestamp: TDateTime; ATopic: string; AText: string; AAuthorGUID: string; AAbonent: TDnmpAbonent = nil): TDnmpMailMessage; overload;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): AnsiString;
    function LoadFromString(s: AnsiString): boolean;
  end;

  TDnmpMailboxType = (mbtInbox, mbtOutbox, mbtStore);

  { TDnmpMailbox }

  TDnmpMailbox = class(TInterfacedObject)
  public
    Name: string;
    BoxType: TDnmpMailboxType;
    MaxCount: Integer;
    DestAddr: TAddr;
    MessagesList: TDnmpMailMessagesList;
    constructor Create;
    destructor Destroy; override;
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
    Author: TDnmpAbonent;
    MessagesList: TDnmpMailMessagesList;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    function SendCmd(Text: string; Addr: TAddr): string;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
  end;


implementation

{ TDnmpMailbox }

constructor TDnmpMailbox.Create();
begin
  inherited Create();
  MessagesList:=TDnmpMailMessagesList.Create(True);
end;

destructor TDnmpMailbox.Destroy();
begin
  FreeAndNil(MessagesList);
  inherited Destroy();
end;

{ TDnmpMail }

function TDnmpMail.CreateMailMsg(AbonGUID, sTopic, sText: string
  ): TDnmpMailMessage;
var
  Abon: TDnmpAbonent;
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
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  Self.MessagesList:=TDnmpMailMessagesList.Create(True);
end;

destructor TDnmpMail.Destroy();
begin
  FreeAndNil(MessagesList);
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

{ TDnmpMailMessagesList }

function TDnmpMailMessagesList.GetItem(Index: Integer): TDnmpMailMessage;
begin
  Result:=TDnmpMailMessage(inherited Items[index]);
end;

procedure TDnmpMailMessagesList.SetItem(Index: Integer; Value: TDnmpMailMessage
  );
begin
  inherited Items[Index]:=Value;
end;

function TDnmpMailMessagesList.AddItem(Item: TDnmpMailMessage): Integer;
begin
  Result:=self.Add(Item);
end;

function TDnmpMailMessagesList.AddItem(ATimestamp: TDateTime; ATopic: string;
  AText: string; AAuthorGUID: string; AAbonent: TDnmpAbonent): TDnmpMailMessage;
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
  self.Add(Result);
end;

function TDnmpMailMessagesList.ToStorage: TDnmpStorage;
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
  Result.Add('type', 'DnmpMailMessagesList');
  Result.Add('items', Storage);
end;

function TDnmpMailMessagesList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpMailMessage;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpMailMessagesList' then Exit;
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
    self.Add(Item);
  end;
  Result:=True;
end;

function TDnmpMailMessagesList.SaveToString: AnsiString;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpMailMessagesList.LoadFromString(s: AnsiString): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  if not StorageFromJson(Storage, s) then Exit;
  Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
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

