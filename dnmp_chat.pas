unit dnmp_chat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_unit, dnmp_services, contnrs;

type

  { TDnmpChatMessage }
  // GRPC Chat message
  // Contain text and basic info about author
  // Serializable
  TDnmpChatMessage = class(TCollectionItem)
  public
    Timestamp: TDateTime;
    IsIncoming: boolean;
    RemoteAddr: TAddr;
    RemoteName: string;
    RemoteGUID: string;
    Text: string;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  { TDnmpChatMessagesList }
  // GRPC Chat messages list
  // When MaxCount reached, first item will be deleted
  // Serializable
  TDnmpChatMessagesList = class(TInterfacedObject)
  protected
    FObservers: TComponentList;
    FItems: TCollection;
    function GetCount(): integer;
    function GetItem(Index: Integer): TDnmpChatMessage; reintroduce;
    procedure SetItem(Index: Integer; Value: TDnmpChatMessage); reintroduce;
  public
    MaxCount: Integer;
    { source of messages }
    ParentList: TDnmpChatMessagesList;
    { Remote contact for filtering }
    Contact: TDnmpContact;
    constructor Create();
    destructor Destroy(); override;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TDnmpChatMessage read GetItem write SetItem; default;
    { not used }
    function AddItem(Item: TDnmpChatMessage): Integer; overload;
    { Add message to list:
      Timestamp - datetime
      IsIncoming - true if incoming message
      Text - message text
      RemoteGUID
      RemoteContact - (optional), for name and address }
    function AddItem(Timestamp: TDateTime; IsIncoming: boolean; Text: string; RemoteAddr: TAddr; RemoteContact: TDnmpContact = nil): TDnmpChatMessage; overload;
    //function GetAbonentByGUID(sGUID: string): TDnmpChannelMessage;
    { Add observer visual control }
    function AddObserver(AControl: TComponent): boolean;
    { Remove observer visual control }
    function DelObserver(AControl: TComponent): boolean;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    procedure UpdateView();
  end;

  TDnmpChatSession = class(TCollectionItem)
  public
    CreationTime: TDateTime;
    Contact: TDnmpContact;
    MessagesList: TDnmpChatMessagesList;
  end;


  TDnmpChat = class(TDnmpService)
  protected
    FOnSay: TNotifyEvent;
    procedure AddChatMessage(AContact: TDnmpContact; IsIncoming: boolean; sText: string);
  public
    Author: TDnmpContact;
    MessagesList: TDnmpChatMessagesList;
    ChatSessions: TCollection;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    //function SendCmd(Text: string; Addr: TAddr): string;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
    //function SayText(AbonentGUID: string; sText: string): string;
    function SayToContact(Contact: TDnmpContact; sText: string): string;
    { return messages list for contact }
    function GetChatSessionForContact(AContact: TDnmpContact): TDnmpChatSession;
    //function Msg(Msg: TDnmpMsg): string; override;
    property OnSay: TNotifyEvent read FOnSay write FOnSay;
    function ParseMsg(AMsg: TDnmpMsg): string; override;
  end;

implementation

{ TDnmpChatMessage }

function TDnmpChatMessage.ToStorage: TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('time', Timestamp);
  Result.Add('in', IsIncoming);
  Result.Add('addr', AddrToStr(RemoteAddr));
  Result.Add('name', RemoteName);
  Result.Add('guid', RemoteGUID);
  Result.Add('text', Text);
end;

function TDnmpChatMessage.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  Self.Timestamp:=Storage.GetReal('time');
  Self.IsIncoming:=Storage.GetBool('in');
  Self.RemoteAddr:=StrToAddr(Storage.GetString('addr'));
  Self.RemoteName:=Storage.GetString('name');
  Self.RemoteGUID:=Storage.GetString('guid');
  // Self.Author:=??;
  Self.Text:=Storage.GetString('text');
  Result:=True;
end;

{ TDnmpChatMessagesList }

function TDnmpChatMessagesList.GetCount(): integer;
var
  i: integer;
begin
  if not Assigned(ParentList) then Result:=FItems.Count
  else
  begin
    if Assigned(Contact) then
    begin
      Result:=0;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID=Contact.GUID then Result:=Result+1;
      end;
    end
    else Result:=ParentList.Count;
  end;
end;

function TDnmpChatMessagesList.GetItem(Index: Integer): TDnmpChatMessage;
var
  i, n: integer;
begin
  if not Assigned(ParentList) then Result:=(FItems.Items[Index] as TDnmpChatMessage)
  else
  begin
    if Assigned(Contact) then
    begin
      n:=-1;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID<>Contact.GUID then Continue;
        n:=n+1;
        if n<Index then Continue;
        Result:=ParentList.Items[i];
        Exit;
      end;
    end
    else Result:=ParentList.Items[Index];
  end;
end;

procedure TDnmpChatMessagesList.SetItem(Index: Integer; Value: TDnmpChatMessage
  );
var
  i, n: integer;
begin
  if not Assigned(ParentList) then FItems.Items[Index]:=Value
  else
  begin
    if Assigned(Contact) then
    begin
      n:=-1;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID<>Contact.GUID then Continue;
        n:=n+1;
        if n<Index then Continue;
        ParentList.Items[i]:=Value;
        Exit;
      end;
    end
    else ParentList.Items[Index]:=Value;
  end;
end;

constructor TDnmpChatMessagesList.Create();
begin
  inherited Create();
  FItems:=TCollection.Create(TDnmpChatMessage);
  FObservers:=TComponentList.Create(False);
end;

destructor TDnmpChatMessagesList.Destroy();
begin
  FreeAndNil(FObservers);
  FreeAndNil(FItems);
  inherited Destroy();
end;

function TDnmpChatMessagesList.AddItem(Item: TDnmpChatMessage): Integer;
begin
  {
  if not Assigned(ParentList) then Result:=inherited Add()
  else
  begin
    if Assigned(Contact) then
    begin
      Result:=0;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID=Contact.GUID then Result:=Result+1;
      end;
    end
    else Result:=ParentList.Count;
  end;
  }
end;

function TDnmpChatMessagesList.AddItem(Timestamp: TDateTime;
  IsIncoming: boolean; Text: string; RemoteAddr: TAddr;
  RemoteContact: TDnmpContact): TDnmpChatMessage;
var
  TmpContact: TDnmpContact;
begin
  Result:=nil;
  if not Assigned(ParentList) then
  begin
    // { TODO : find }
    // add
    Result:=(FItems.Add() as TDnmpChatMessage);
    Result.Timestamp:=Timestamp;
    Result.IsIncoming:=IsIncoming;
    Result.Text:=Text;
    if Assigned(RemoteContact) then
    begin
      Result.RemoteGUID:=RemoteContact.GUID;
      Result.RemoteName:=RemoteContact.Name;
      Result.RemoteAddr:=RemoteContact.Addr;
    end
    else
    begin
      Result.RemoteAddr:=RemoteAddr;
      Result.RemoteName:='';
      Result.RemoteGUID:='';
    end;
  end

  else
  begin
    if Assigned(Contact) then
    begin
      if not SameAddr(Contact.Addr, RemoteAddr) then Exit;
    end;
    Result:=ParentList.AddItem(Timestamp, IsIncoming, Text, RemoteAddr, RemoteContact);
  end;
end;

function TDnmpChatMessagesList.AddObserver(AControl: TComponent): boolean;
begin
  Result:=False;
  if FObservers.IndexOf(AControl)>-1 then Exit;
  FObservers.Add(AControl);
  Result:=True;
end;

function TDnmpChatMessagesList.DelObserver(AControl: TComponent): boolean;
begin
  Result:=False;
  if FObservers.IndexOf(AControl)=-1 then Exit;
  FObservers.Extract(AControl);
  Result:=True;
end;

function TDnmpChatMessagesList.ToStorage(): TDnmpStorage;
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
  Result.Add('type', 'DnmpChatMessagesList');
  Result.Add('items', Storage);
end;

function TDnmpChatMessagesList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpChatMessage;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpChatMessagesList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=(FItems.Add() as TDnmpChatMessage);
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
  end;
  Result:=True;
end;

procedure TDnmpChatMessagesList.UpdateView();
var
  i: integer;
  AMsg: ShortString;
begin
  AMsg:='UpdateView';
  for i:=0 to FObservers.Count-1 do FObservers.Items[i].DispatchStr(AMsg);
end;

{ TDnmpChat }

procedure TDnmpChat.AddChatMessage(AContact: TDnmpContact; IsIncoming: boolean;
  sText: string);
var
  i: integer;
  ChatSession: TDnmpChatSession;
begin
  ChatSession:=Self.GetChatSessionForContact(AContact);
  if not Assigned(ChatSession) then Exit;
  ChatSession.MessagesList.AddItem(Now(), IsIncoming, sText, AContact.Addr, AContact);
  ChatSession.MessagesList.UpdateView();
end;

constructor TDnmpChat.Create(AMgr: TDnmpManager;
  AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  Self.Author:=AMgr.MyInfo;
  Self.MessagesList:=TDnmpChatMessagesList.Create();
  Self.ChatSessions:=TCollection.Create(TDnmpChatSession);
end;

destructor TDnmpChat.Destroy();
begin
  FreeAndNil(Self.ChatSessions);
  FreeAndNil(Self.MessagesList);
  inherited Destroy();
end;

function TDnmpChat.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpChat');

  Storage:=Self.ServiceInfo.ToStorage();
  Result.Add('service_info', Storage);

  Storage:=Self.MessagesList.ToStorage();
  Result.Add('messages_list', Storage);
end;

function TDnmpChat.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpChat' then Exit;

  { TODO : Is really need to restore service info? }
  SubStorage:=Storage.GetObject('service_info');
  Self.ServiceInfo.FromStorage(SubStorage);

  SubStorage:=Storage.GetObject('messages_list');
  Self.MessagesList.FromStorage(SubStorage);

  Result:=True;
end;

function TDnmpChat.SayToContact(Contact: TDnmpContact; sText: string): string;
begin
  // add to local messages list
  AddChatMessage(Contact, False, sText);
  // send message to remote contact
  Mgr.SendDataMsg(Contact.Addr, 'CHAT', 'cmd=MSG', sText);
end;

function TDnmpChat.GetChatSessionForContact(AContact: TDnmpContact
  ): TDnmpChatSession;
var
  i: integer;
begin
  Result:=nil;
  if not Assigned(AContact) then Exit;
  // find chat session
  for i:=0 to ChatSessions.Count-1 do
  begin
    Result:=(ChatSessions.Items[i] as TDnmpChatSession);
    if Result.Contact=AContact then Exit;
  end;
  Result:=nil;

  if not Assigned(Result) then // create new session
  begin
    Result:=(ChatSessions.Add() as TDnmpChatSession);
    Result.CreationTime:=Now();
    Result.Contact:=AContact;
    Result.MessagesList:=TDnmpChatMessagesList.Create();
    Result.MessagesList.ParentList:=Self.MessagesList;
    Result.MessagesList.Contact:=AContact;
  end;
end;

function TDnmpChat.ParseMsg(AMsg: TDnmpMsg): string;
var
  sCmd: string;
  RemoteContact: TDnmpContact;
begin
  Result:='';
  if not Assigned(AMsg) then Exit;
  if AMsg.MsgType <> Self.ServiceInfo.ServiceType then Exit;

  RemoteContact:=Mgr.GetContactByAddr(AMsg.SourceAddr);
  if not Assigned(RemoteContact) then
  begin
    Mgr.DebugText('CHAT message from unknown addr: '+AddrToStr(AMsg.SourceAddr));
    Exit;
  end;

  sCmd:=Trim(AMsg.Info.Values['cmd']);

  if sCmd = 'MSG' then // Это сообщение
  begin
    // update chat session
    AddChatMessage(RemoteContact, True, StreamToStr(AMsg.Data));
    if Assigned(OnEvent) then OnEvent('MSG', Self);
  end

  else if sCmd = 'INVITE' then // Это приглашение
  begin
    if Assigned(OnEvent) then OnEvent('INVITE', Self);
  end

  else if sCmd = 'ACK' then // Это подтверждение
  begin
    if Assigned(OnEvent) then OnEvent('ACK', Self);
  end

  else if sCmd = 'BYE' then // Это отказ
  begin
    if Assigned(OnEvent) then OnEvent('BYE', Self);
  end

  else if sCmd = 'FILE' then // Это файл
  begin
    if Assigned(OnEvent) then OnEvent('FILE', Self);
  end;
end;



end.

