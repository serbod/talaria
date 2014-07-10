unit dnmp_grpc;

interface
uses SysUtils, Classes, Contnrs, dnmp_unit, dnmp_services;

type
  { TDnmpChannelMessage }
  // GRPC Channel message
  // Contain text and basic info about author
  // Serializable
  TDnmpChannelMessage = class(TObject)
  public
    Timestamp: TDateTime;
    MessageType: Integer;
    Author: TDnmpAbonent;
    AuthorAddr: TAddr;
    AuthorName: string;
    AuthorGUID: string;
    Text: string;
    procedure FillMsg(Msg: TDnmpMsg);
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  { TDnmpChannelMessagesList }
  // GRPC Channel messages list
  // When MaxCount reached, first item will be deleted
  // Serializable
  TDnmpChannelMessagesList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpChannelMessage;
    procedure SetItem(Index: Integer; Value: TDnmpChannelMessage);
  public
    MaxCount: Integer;
    property Items[Index: Integer]: TDnmpChannelMessage read GetItem write SetItem; default;
    function AddItem(Item: TDnmpChannelMessage): Integer; overload;
    function AddItem(Timestamp: TDateTime; MessageType: Integer; Text: string; AuthorGUID: string; Abonent: TDnmpAbonent): TDnmpChannelMessage; overload;
    //function GetAbonentByGUID(sGUID: string): TDnmpChannelMessage;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function ToString(): AnsiString;
    function FromString(s: AnsiString): boolean;
  end;

  { TGrpcBanItem }

  TGrpcBanItem = class(TCollectionItem)
  public
    AuthorGUID: string;
    AbonentGUID: string;
    Reason: string;
    EndDate: TDateTime;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  { TGrpcBanList }

  TGrpcBanList = class(TCollection)
  public
    constructor Create(); reintroduce;
    function GetItem(Index: integer): TGrpcBanItem;
    function AddBan(AAbonentGUID, AReason: string; AEndDate: TDateTime): boolean;
    function UpdateBan(AAbonentGUID, AReason: string; AEndDate: TDateTime): boolean;
    function DelBan(AAbonentGUID: string): boolean;
    function GetBan(AAbonentGUID: string): TGrpcBanItem;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
  end;

  { TDnmpGrpc }
  // GRPC Channel base (client-server independent)
  // Serializable
  // == Events ==
  // ('TOPIC <topic>', nil)
  // ('STATE', Abonent)
  // ('USERS', self.UsersList)
  // ('ABONENTS', Self.ServiceInfo.Abonents)
  // ('BANLIST', Self.BanList)
  // ('MODE <mode>', nil)
  TDnmpGrpc = class(TDnmpService)
  protected
    FOnSay: TNotifyEvent;
    FOnTopicChange: TNotifyEvent;
    FOnModeChange: TNotifyEvent;
    FOnAbonentsChange: TNotifyEvent;
    FOnUsersChange: TNotifyEvent;
    FOnBanlistChange: TNotifyEvent;
    // Create channel message with specified text and author
    function CreateChannelMsg(AbonGUID, sText: string): TDnmpChannelMessage;
    // Send channel message to specified address
    function SendChannelMsg(Addr: TAddr; ChanMsg: TDnmpChannelMessage): Boolean;
    // Send Topic message to specified address
    function SendTopic(Addr: TAddr): Boolean;
    // Read data response
    function ReadData(sDataType, sData: string): string;
    { Read BANLIST response CSV
    [0] author_guid - GUID автора
    [1] abonent_guid - GUID абонента
    [2] reason - причина бана
    [3] end_date - дата окончания бана }
    function ReadBanList(sData: string): boolean;
    // Read abonent list response
    { Загрузить абонентов из сериализованого списка в формате CSV.
    Каждый элемент списка содержит сведения:
    [0] guid - GUID абонента
    [1] state - состояние (подключен или отключен)
    [2] nick - ник (имя на канале, не зависит от реального имени)
    [3] addr - адрес
    [4] rights - полномочия (набор полномочий)
    [5] status - статус (сообщение абонента)
    Пераметр sListType - тип списка:
      USERS - список активных пользователей
      ABONENTS - список известных подписчиков }
    function ReadAbonList(sAbonList, sListType: string): Boolean;
    // Parse and update abonent state response
    function SetAbonentState(sData: string): TDnmpAbonent;
  public
    Topic: string;
    UsersList: TDnmpAbonentList;
    //BanList: TDnmpAbonentList;
    BanList: TGrpcBanList;
    MessagesList: TDnmpChannelMessagesList;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    procedure DebugText(s: string);
    function SendCmd(Text: string; Addr: TAddr): string;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
    { Add abonent to users list }
    function JoinAbonent(AbonentGUID: string): string;
    { Remove abonent from users list }
    function LeaveAbonent(AbonentGUID: string): string;
    { Kick abonent from users list }
    function KickAbonent(AbonentGUID: string): string;
    { Move user to ban list }
    function BanUser(AbonentGUID: string): string;
    { Remove user from ban list }
    function UnbanAbonent(AbonentGUID: string): string;
    //function Msg(Msg: TDnmpMsg): string; override;
    property OnSay: TNotifyEvent read FOnSay write FOnSay;
    property OnTopicChange: TNotifyEvent read FOnTopicChange write FOnTopicChange;
    property OnModeChange: TNotifyEvent read FOnModeChange write FOnModeChange;
    property OnAbonentsChange: TNotifyEvent read FOnAbonentsChange write FOnAbonentsChange;
    property OnUsersChange: TNotifyEvent read FOnUsersChange write FOnUsersChange;
    property OnBanlistChange: TNotifyEvent read FOnBanlistChange write FOnBanlistChange;
  end;

  { TDnmpGrpcClient }
  // GRPC Channel Client
  // Server -> This: messages, data
  // Server <- This: messages, commands, data
  // Serializable
  // == Events ==
  // ('MSG', ChanMsg)
  TDnmpGrpcClient = class(TDnmpGrpc)
  private
    function SayCmd(AbonGUID, sText: string): string;
  public
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    function Cmd(Text: string; Addr: TAddr): string; override;
    function ParseMsg(AMsg: TDnmpMsg): string; override;
    procedure Say(sText: string);
  end;

  { TDnmpGrpcServer }
  // GRPC Channel Server
  // Client -> This: messages, commands, data
  // Client <- This: messages, data
  // Server -> This: messages, commands, data
  // Server <- This: messages, commands, data
  // Serializable
  TDnmpGrpcServer = class(TDnmpGrpc)
  private
    function SayCmd(AbonGUID, sText: string): string;
    function SendAbonList(Addr: TAddr; ListType: AnsiChar): Boolean;
    function SendBanList(Addr: TAddr): Boolean;
    function SendLastMsgList(Addr: TAddr): Boolean;
    procedure BroadcastMsg(ChanMsg: TDnmpChannelMessage);
    procedure BroadcastCmd(sCmd: string);
  public
    LastVisit: TDateTime;
    Modes: string;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    { Обработка команды, отправленной с указанного адреса
    JOIN <GUID_абонента> [текст]  Добавляет абонента в список подписчиков и пользователей канала.
    LEAVE <GUID_абонента> [текст]    Убирает абонента из списка подписчиков и пользователей канала.
    SET_TOPIC <текст>    Устанавливает тему канала
    GET_TOPIC    Возвращает сообщение, содержащее заголовок (тему) канала.
    GET_USERS    Возвращает список активных подписчиков
    GET_ABONENTS    Возвращает список всех подписчиков
    GET_LAST_MESSAGES [число сообщений]    Возвращает последние сообщения
    GET_MODE    Возвращает строку режимов канала
    SET_MODE    Устанавливает один или несколько режимов канала
    KICK <GUID_абонента> <причина>    Убирает абонента из списка подписчиков с указанием причины
    BAN <GUID_абонента> <срок> <причина>    Добавляет абонента в "черный список" на заданый срок в указанием причины.
    UNBAN <GUID_абонента>    Удаляет абонента из "черного списка".
    GET_BANLIST    Возвращает "черный список" абонентов
    SAY <GUID_абонента> <текст>    Сообщение на канал от абонента
    }
    function Cmd(Text: string; Addr: TAddr): string; override;
    function ParseMsg(AMsg: TDnmpMsg): string; override;
  end;

const
  ciMaxMsgCount = 25;
  ciMaxMsgCountServer = 50;
  ciMaxMsgCountClient = 1000;

implementation
uses Misc;

{ TGrpcBanItem }

function TGrpcBanItem.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('author_guid', self.AuthorGUID);
  Result.Add('abonent_guid', self.AbonentGUID);
  Result.Add('reason', self.Reason);
  Result.Add('end_date', self.EndDate);
end;

function TGrpcBanItem.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  Self.AuthorGUID:=Storage.GetString('author_guid');
  Self.AbonentGUID:=Storage.GetString('abonent_guid');
  Self.Reason:=Storage.GetString('reason');
  Self.EndDate:=Storage.GetReal('end_date');
  Result:=True;
end;

{ TGrpcBanList }

constructor TGrpcBanList.Create();
begin
  inherited Create(TGrpcBanItem);
end;

function TGrpcBanList.GetItem(Index: integer): TGrpcBanItem;
begin
  Result:=(self.Items[Index] as TGrpcBanItem);
end;

function TGrpcBanList.AddBan(AAbonentGUID, AReason: string; AEndDate: TDateTime
  ): boolean;
begin
  Result:=False;
  // search for exists
  if Assigned(self.GetBan(AAbonentGUID)) then Exit;

  // add new
  Result:=self.UpdateBan(AAbonentGUID, AReason, AEndDate);
end;

function TGrpcBanList.UpdateBan(AAbonentGUID, AReason: string;
  AEndDate: TDateTime): boolean;
var
  i: integer;
  Item: TGrpcBanItem;
begin
  Result:=False;
  // search for exists
  Item:=nil;
  for i:=0 to self.Count-1 do
  begin
    Item:=self.GetItem(i);
    if Item.AbonentGUID=AAbonentGUID then Break;
  end;
  // add new if not found
  if not Assigned(Item) then Item:=(self.Add() as TGrpcBanItem);

  Item.AbonentGUID:=AAbonentGUID;
  Item.Reason:=AReason;
  Item.EndDate:=AEndDate;
  Result:=True;
end;

function TGrpcBanList.DelBan(AAbonentGUID: string): boolean;
var
  i: integer;
  Item: TGrpcBanItem;
begin
  Result:=False;
  // search for exists
  for i:=0 to self.Count-1 do
  begin
    Item:=self.GetItem(i);
    if Item.AbonentGUID=AAbonentGUID then
    begin
      self.Delete(Item.Index);
      Result:=True;
      Exit;
    end;
  end;
end;

function TGrpcBanList.GetBan(AAbonentGUID: string): TGrpcBanItem;
var
  i: integer;
begin
  // search for exists
  for i:=0 to self.Count-1 do
  begin
    Result:=self.GetItem(i);
    if Result.AbonentGUID=AAbonentGUID then Exit;
  end;
  Result:=nil;
end;

function TGrpcBanList.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
  i: Integer;
begin
  Storage:=TDnmpStorage.Create(stDictionary);
  for i:=0 to Self.Count-1 do
  begin
    Storage.Add(IntToStr(i), Self.GetItem(i).ToStorage());
  end;

  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'GrpcBanList');
  Result.Add('items', Storage);
end;

function TGrpcBanList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TGrpcBanItem;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'GrpcBanList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=(self.Add as TGrpcBanItem);
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      self.Delete(Item.Index);
      Continue;
    end;
  end;
  Result:=True;
end;

// === TDnmpChannelMessage ===
procedure TDnmpChannelMessage.FillMsg(Msg: TDnmpMsg);
begin
  if not Assigned(Msg) then Exit;
  //sl.Values['name']:=self.ServiceInfo.Name;
  Msg.Info.Values['author_addr']:=AddrToStr(AuthorAddr);
  Msg.Info.Values['author_name']:=AuthorName;
  Msg.Info.Values['author_guid']:=AuthorGUID;
  Msg.Info.Values['timestamp']:=TimestampToStr(Timestamp);
  StrToStream(Text, Msg.Data);
end;


function TDnmpChannelMessage.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('timestamp', Timestamp);
  Result.Add('msg_type', MessageType);
  //Result.Add('', Author);
  Result.Add('author_addr', AddrToStr(AuthorAddr));
  Result.Add('author_name', AuthorName);
  Result.Add('author_guid', AuthorGUID);
  Result.Add('text', Text);
end;

function TDnmpChannelMessage.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  Self.Timestamp:=Storage.GetReal('timestamp');
  Self.MessageType:=Storage.GetInteger('msg_type');
  Self.AuthorAddr:=StrToAddr(Storage.GetString('author_addr'));
  Self.AuthorName:=Storage.GetString('author_name');
  Self.AuthorGUID:=Storage.GetString('author_guid');
  // Self.Author:=??;
  Self.Text:=Storage.GetString('text');
  Result:=True;
end;

// === TDnmpChannelMessagesList ===
function TDnmpChannelMessagesList.GetItem(Index: Integer): TDnmpChannelMessage;
begin
  Result:=TDnmpChannelMessage(inherited Items[index]);
end;

procedure TDnmpChannelMessagesList.SetItem(Index: Integer; Value: TDnmpChannelMessage);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpChannelMessagesList.AddItem(Item: TDnmpChannelMessage): Integer;
var
  i: Integer;
  TempItem: TDnmpChannelMessage;
begin
  Result:=-1;
  // Check for duplicate
  for i:=0 to Count-1 do
  begin
    TempItem:=self.GetItem(i);
    if (TempItem.Timestamp = Item.Timestamp) and (TempItem.AuthorGUID = Item.AuthorGUID) then Exit;
  end;
  if (Count >= MaxCount) and (MaxCount>0) then Delete(0);
  Result:=self.Add(Item);
end;

function TDnmpChannelMessagesList.AddItem(Timestamp: TDateTime; MessageType: Integer; Text: string; AuthorGUID: string; Abonent: TDnmpAbonent): TDnmpChannelMessage;
var
  i: Integer;
  TempItem: TDnmpChannelMessage;
begin
  Result:=nil;
  // Check for duplicate
  for i:=0 to Count-1 do
  begin
    TempItem:=self.GetItem(i);
    if (TempItem.Timestamp = Timestamp) and (TempItem.AuthorGUID = AuthorGUID) then Exit;
  end;
  TempItem:=TDnmpChannelMessage.Create();
  TempItem.Timestamp:=Timestamp;
  TempItem.MessageType:=MessageType;
  TempItem.Text:=Text;
  TempItem.AuthorGUID:=AuthorGUID;
  TempItem.Author:=Abonent;
  if Assigned(Abonent) then
  begin
    TempItem.AuthorAddr:=Abonent.Addr;
    TempItem.AuthorName:=Abonent.Nick;
  end;
  self.Add(TempItem);
  Result:=TempItem;
end;

function TDnmpChannelMessagesList.ToStorage(): TDnmpStorage;
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
  Result.Add('type', 'DnmpChannelMessagesList');
  Result.Add('items', Storage);
end;

function TDnmpChannelMessagesList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpChannelMessage;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpChannelMessagesList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=TDnmpChannelMessage.Create();
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    self.Add(Item);
  end;
  Result:=True;
end;

function TDnmpChannelMessagesList.ToString: AnsiString;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpChannelMessagesList.FromString(s: AnsiString): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  if not StorageFromJson(Storage, s) then Exit;
  Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;


// === TDnmpGrpc ===
constructor TDnmpGrpc.Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  Self.UsersList:=TDnmpAbonentList.Create(False);
  Self.UsersList.ParentList:=Self.ServiceInfo.Abonents.ParentList;
  //Self.BanList:=TDnmpAbonentList.Create(False);
  //Self.BanList.ParentList:=Self.ServiceInfo.Abonents.ParentList;
  Self.BanList:=TGrpcBanList.Create();
  Self.MessagesList:=TDnmpChannelMessagesList.Create(True);
end;

destructor TDnmpGrpc.Destroy();
begin
  FreeAndNil(MessagesList);
  FreeAndNil(BanList);
  FreeAndNil(UsersList);
  inherited Destroy();
end;

procedure TDnmpGrpc.DebugText(s: string);
begin
  if Assigned(Mgr) then Mgr.DebugText('GRPC: '+s);
end;

function TDnmpGrpc.CreateChannelMsg(AbonGUID, sText: string): TDnmpChannelMessage;
var
  Abon: TDnmpAbonent;
begin
  Result:=nil;
  Result:=TDnmpChannelMessage.Create();
  Result.Timestamp:=Now;
  Result.MessageType:=0;
  Result.AuthorGUID:=AbonGUID;
  Result.Text:=sText;
  Abon:=Self.UsersList.GetAbonentByGUID(AbonGUID);
  if not Assigned(Abon) then Abon:=Self.GetAbonentByGUID(AbonGUID);
  Result.Author:=Abon;
  if Assigned(Abon) then
  begin
    Result.AuthorAddr:=Abon.Addr;
    Result.AuthorName:=Abon.Nick;
  end;
end;

function TDnmpGrpc.SendChannelMsg(Addr: TAddr; ChanMsg: TDnmpChannelMessage): Boolean;
var
  sl: TStringList;
begin
  Result:=False;
  if not Assigned(ChanMsg) then Exit;
  sl:=TStringList.Create();
  sl.Values['name']:=self.ServiceInfo.Name;
  sl.Values['author_addr']:=AddrToStr(ChanMsg.AuthorAddr);
  sl.Values['author_name']:=ChanMsg.AuthorName;
  sl.Values['author_guid']:=ChanMsg.AuthorGUID;
  sl.Values['timestamp']:=TimestampToStr(ChanMsg.Timestamp);

  Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, sl.Text, ChanMsg.Text);
  sl.Free();
  Result:=True;
end;

function TDnmpGrpc.SendTopic(Addr: TAddr): Boolean;
begin
  Result:=True;
  Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, 'name='+Self.ServiceInfo.Name+#13+#10+'data=topic', Self.Topic);
end;

function TDnmpGrpc.SetAbonentState(sData: string): TDnmpAbonent;
var
  s, sGUID, sState: string;
  i: Integer;
begin
  Result:=nil;
  s:=sData;
  i:=Pos(' ', s);
  if i=0 then Exit;
  sGUID:=Copy(s, 1, i-1);
  Delete(s, 1, i);

  i:=Pos(' ', s);
  if i=0 then i:=999999;
  sState:=Copy(s, 1, i-1);
  Delete(s, 1, i);

  Result:=self.UsersList.GetAbonentByGUID(sGUID);
  if not Assigned(Result) then Result:=self.ServiceInfo.Abonents.GetAbonentByGUID(sGUID);
  if not Assigned(Result) then Exit;
  Result.StateFromStr(sState);
  Result.Status:=s;

  if sState='OFF' then
  begin
    self.UsersList.Extract(Result);
  end

  else if sState='ON' then
  begin
    self.UsersList.Add(Result);
  end;

end;

function TDnmpGrpc.SendCmd(Text: string; Addr: TAddr): string;
var
  sCmd, sParams: string;
begin
  Result:='';
  if SameAddr(Addr, NewAddr()) then Addr:=ServiceInfo.HostAddr;
  ExtractCmd(Text, sCmd, sParams);
  Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, 'name='+Self.ServiceInfo.Name+#13+#10+'cmd='+sCmd, sParams);
end;

function TDnmpGrpc.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpGrpc');
  Result.Add('topic', self.Topic);

  Storage:=Self.ServiceInfo.ToStorage();
  Result.Add('service_info', Storage);

  Storage:=Self.UsersList.ToStorage();
  Result.Add('users_list', Storage);

  Storage:=Self.BanList.ToStorage();
  Result.Add('ban_list', Storage);

  Storage:=Self.MessagesList.ToStorage();
  Result.Add('messages_list', Storage);
end;

function TDnmpGrpc.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpGrpc' then Exit;
  self.Topic:=Storage.GetString('topic');

  { TODO : Is really need to restore service info? }
  SubStorage:=Storage.GetObject('service_info');
  Self.ServiceInfo.FromStorage(SubStorage);

  SubStorage:=Storage.GetObject('users_list');
  Self.UsersList.FromStorage(SubStorage);

  SubStorage:=Storage.GetObject('ban_list');
  Self.BanList.FromStorage(SubStorage);

  SubStorage:=Storage.GetObject('messages_list');
  Self.MessagesList.FromStorage(SubStorage);

  Result:=True;
end;

function TDnmpGrpc.JoinAbonent(AbonentGUID: string): string;
var
  Abonent: TDnmpAbonent;
begin
  Result:='';

  // Banned?
  if BanList.GetBan(AbonentGUID)<>nil then
  begin
    Result:='Cannot JOIN - abonent banned: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;

  // Exists?
  Abonent:=GetAbonentByGUID(AbonentGUID);
  if not Assigned(Abonent) then Abonent:=Self.ServiceInfo.Abonents.AddAbonentByGUID(AbonentGUID);
  if not Assigned(Abonent) then
  begin
    Result:='Cannot JOIN - abonent not exists: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;

  // Add abonent to abonents list
  Self.ServiceInfo.Abonents.UpdateAbonent(Abonent);
  Self.UsersList.UpdateAbonent(Abonent);

  if Assigned(OnUsersChange) then OnUsersChange(self);
  if Assigned(OnAbonentsChange) then OnAbonentsChange(self);
end;

function TDnmpGrpc.LeaveAbonent(AbonentGUID: string): string;
var
  Abonent: TDnmpAbonent;
begin
  Result:='';

  // Exists?
  Abonent:=Self.UsersList.GetAbonentByGUID(AbonentGUID);
  if not Assigned(Abonent) then
  begin
    Result:='Cannot LEAVE - abonent not joined: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;

  // Remove abonent from users list
  Self.UsersList.Extract(Abonent);
  if Assigned(OnUsersChange) then OnUsersChange(self);
end;

function TDnmpGrpc.KickAbonent(AbonentGUID: string): string;
var
  Abonent: TDnmpAbonent;
begin
  Result:='';

  // Joined?
  Abonent:=UsersList.GetAbonentByGUID(AbonentGUID);
  if Abonent=nil then
  begin
    Result:='Cannot KICK - abonent not joined: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;

  // Remove abonent from users list
  Self.UsersList.Extract(Abonent);
  if Assigned(OnUsersChange) then OnUsersChange(self);
end;

function TDnmpGrpc.BanUser(AbonentGUID: string): string;
var
  Abonent: TDnmpAbonent;
begin
  Result:='';
  Abonent:=self.UsersList.GetAbonentByGUID(AbonentGUID);
  if not Assigned(Abonent) then
  begin
    Result:='Cannot BAN - user not exists: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;
  if self.BanList.GetBan(AbonentGUID)<>nil then
  begin
    Result:=('Cannot BAN - abonent banned: '+AbonentGUID);
    DebugText(Result);
    Exit;
  end;
  self.UsersList.Extract(Abonent);
  // TODO: ban prams
  self.BanList.AddBan(AbonentGUID, '', 0);
  if Assigned(OnBanlistChange) then OnBanlistChange(self);
  if Assigned(OnUsersChange) then OnUsersChange(self);
end;

function TDnmpGrpc.UnbanAbonent(AbonentGUID: string): string;
begin
  Result:='';
  if not Assigned(self.BanList.GetBan(AbonentGUID)) then
  begin
    Result:='Cannot UNBAN - user not banned: '+AbonentGUID;
    DebugText(Result);
    Exit;
  end;
  self.BanList.DelBan(AbonentGUID);
  if Assigned(OnBanlistChange) then OnBanlistChange(self);
end;

function TDnmpGrpc.ReadData(sDataType, sData: string): string;
var
  Abonent: TDnmpAbonent;
begin
  Result:='';
  if sDataType='TOPIC' then
  begin
    self.Topic:=sData;
    if Assigned(OnEvent) then OnEvent(sDataType+' '+sData, self);
  end

  else if sDataType='STATE' then
  begin
    Abonent:=SetAbonentState(sData);
    if Assigned(OnEvent) then OnEvent(sDataType, Abonent);
  end

  else if sDataType='USERS' then
  begin
    ReadAbonList(sData, sDataType);
    if Assigned(OnEvent) then OnEvent(sDataType, Self.UsersList);
  end

  else if sDataType='ABONENTS' then
  begin
    ReadAbonList(sData, sDataType);
    if Assigned(OnEvent) then OnEvent(sDataType, Self.ServiceInfo.Abonents);
  end

  else if sDataType='BANLIST' then
  begin
    ReadBanList(sData);
    if Assigned(OnEvent) then OnEvent(sDataType, Self.BanList);
  end

  else if sDataType='MODE' then
  begin
    if Assigned(OnEvent) then OnEvent(sDataType+' '+sData, nil);
  end;
  Mgr.AddCmd('GRPC UPDATE '+ServiceInfo.Name+' '+sDataType);
end;

function TDnmpGrpc.ReadBanList(sData: string): boolean;
var
  sl, slData: TStringList;
begin
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  slData.Text:=sData;
  for i:=0 to slData.Count-1 do
  begin
    sl.Clear();
    sl.DelimitedText:=slData[i];
    if sl.Count<3 then Continue;
    self.BanList.UpdateBan(sl[1], sl[3], StrToTimestamp(sl[2]));
  end;
  FreeAndNil(slData);
  FreeAndNil(sl);
end;

function TDnmpGrpc.ReadAbonList(sAbonList, sListType: string): Boolean;
var
  sl, slData: TStringList;
  i: Integer;
  Abonent: TDnmpAbonent;

procedure UpdateAbonentList(al: TDnmpAbonentList; sl: TStrings);
begin
  al.UpdateAbonent(sl[0], sl[2], sl[1], sl[4], sl[5], StrToAddr(sl[3]));
end;

begin
  Result:=False;
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  slData.Text:=sAbonList;
  for i:=0 to slData.Count-1 do
  begin
    sl.Clear();
    sl.DelimitedText:=slData[i];
    if sl.Count<6 then Continue;

    if sListType='USERS' then
    begin
      UpdateAbonentList(Self.UsersList, sl);
      Result:=True;
    end
    else if sListType='ABONENTS' then
    begin
      UpdateAbonentList(Self.ServiceInfo.Abonents, sl);
      Result:=True;
    end
    else if sListType='BANLIST' then
    begin
      //UpdateAbonentList(Self.BanList, sl);
      //Result:=True;
    end;
  end;
  FreeAndNil(slData);
  FreeAndNil(sl);
end;




// === TDnmpGrpcServer ===
constructor TDnmpGrpcServer.Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  MessagesList.MaxCount:=ciMaxMsgCountServer;
end;

destructor TDnmpGrpcServer.Destroy();
begin
  inherited Destroy();
end;

function TDnmpGrpcServer.Cmd(Text: string; Addr: TAddr): string;
var
  sCmd, s: string;
  Params: TStringArray;
  i, n: Integer;
  Abonent: TDnmpAbonent;
begin
  Result:='';
  Params:=ParseStr(Text);
  n:=Length(Params);
  if n=0 then Exit;
  sCmd:=Params[0];
  if sCmd='' then Exit

  else if sCmd='JOIN' then
  begin
    if n<2 then Exit;
    Result:=Self.JoinAbonent(Params[1]);
    if Result<>'' then Mgr.SendErrorMsg(Addr, 'GRPC '+ServiceInfo.Name, Result)
    else
    begin
      // broadcast to others
      BroadcastCmd(Text);
      if SameNode(Addr, self.Mgr.MyInfo.Addr) then
      begin
        // Send channel description
        SendTopic(Addr);
        // Send active abonents list
        SendAbonList(Addr, 'U');
        // Send last messages
        SendLastMsgList(Addr);
      end;
    end;
  end

  else if sCmd='LEAVE' then
  begin
    if n<2 then Exit;
    Result:=Self.LeaveAbonent(Params[1]);
    if Result<>'' then Mgr.SendErrorMsg(Addr, 'GRPC '+ServiceInfo.Name, Result)
    else
    begin
      // broadcast to others
      BroadcastCmd(Text);
    end;
  end

  else if sCmd='GET_TOPIC' then
  begin
    SendTopic(Addr);
  end

  else if sCmd='SET_TOPIC' then
  begin
    // { TODO: Проверка полномочий }
    // ...
    if n=1 then Self.Topic:='' else
    begin
      i:=Pos(' ', Text);
      s:=Copy(Text, i+1, MaxInt);
      Self.Topic:=Trim(s);
    end;
    BroadcastCmd(Text);
    if Assigned(OnTopicChange) then OnTopicChange(self);
  end

  else if sCmd='GET_USERS' then
  begin
    SendAbonList(Addr, 'U');
  end

  else if sCmd='GET_ABONENTS' then
  begin
    SendAbonList(Addr, 'A');
  end

  else if sCmd='GET_BANLIST' then
  begin
    SendBanList(Addr);
  end

  else if sCmd='GET_LAST_MESSAGES' then
  begin
    SendLastMsgList(Addr);
  end

  else if sCmd='KICK' then
  begin
    if n<2 then Exit;
    Result:=Self.KickAbonent(Params[1]);
    if Result<>'' then Mgr.SendErrorMsg(Addr, 'GRPC '+ServiceInfo.Name, Result)
    else
    begin
      // broadcast to others
      BroadcastCmd(Text);
      if SameNode(Addr, self.Mgr.MyInfo.Addr) then
      begin
        SendAbonList(Addr, 'U');
      end;
    end;
  end

  else if sCmd='SAY' then
  begin
    if n<3 then Exit;
    i:=Pos(' ', Text);
    s:=Copy(Text, i+1, MaxInt);
    i:=Pos(' ', Text);
    s:=Copy(Text, i+1, MaxInt);
    Result:=SayCmd(Params[1], s);
  end

  else if sCmd='BAN' then
  begin
    if n<1 then Exit;
    Result:=self.BanUser(Params[1]);
    if Result<>'' then Mgr.SendErrorMsg(Addr, 'GRPC '+ServiceInfo.Name, Result)
    else
    begin
      // broadcast to others
      BroadcastCmd(Text);
      //SendAbonList(Addr, 'U');
      //SendBanList(Addr);
    end;
  end

  else if sCmd='UNBAN' then
  begin
    if n<1 then Exit;
    Result:=self.UnbanAbonent(Params[1]);
    if Result<>'' then Mgr.SendErrorMsg(Addr, 'GRPC '+ServiceInfo.Name, Result)
    else
    begin
      // broadcast to others
      BroadcastCmd(Text);
      //SendAbonList(Addr, 'U');
      //SendBanList(Addr);
    end;
  end

  else
  begin

  end;

end;

function TDnmpGrpcServer.ParseMsg(AMsg: TDnmpMsg): string;
var
  s, sChan: string;
  ChanMsg: TDnmpChannelMessage;
begin
  Result:='';
  if not Assigned(AMsg) then Exit;
  if AMsg.MsgType <> Self.ServiceInfo.ServiceType then Exit;
  sChan:=Trim(AMsg.Info.Values['name']);
  if sChan <> self.ServiceInfo.Name then Exit;
  // Это сообщение?
  s:=Trim(AMsg.Info.Values['author_addr']);
  if s <> '' then
  begin
    ChanMsg:=TDnmpChannelMessage.Create();
    ChanMsg.Timestamp:=AMsg.TimeStamp;
    ChanMsg.AuthorAddr:=StrToAddr(s);
    ChanMsg.AuthorName:=AMsg.Info.Values['author_name'];
    ChanMsg.AuthorGUID:=AMsg.Info.Values['author_guid'];
    ChanMsg.Text:=StreamToStr(AMsg.Data);
    if self.MessagesList.AddItem(ChanMsg) = -1 then ChanMsg.Free()
    else BroadcastMsg(ChanMsg);
    Exit;
  end;

  // Это команда?
  s:=Trim(AMsg.Info.Values['cmd']);
  if s <> '' then
  begin
    s:=s+' '+StreamToStr(AMsg.Data);
    Result:=Self.Cmd(s, AMsg.SourceAddr);
    Exit;
  end;

  // Это данные?
  s:=Trim(AMsg.Info.Values['data']);
  if s <> '' then
  begin
    Result:=Self.ReadData(s, StreamToStr(AMsg.Data));
    Exit;
  end;
end;

// USERS - список активных подписчиков
// Содержит список активных подписчиков в формате CSV. Каждый элемент списка содержит сведения:
// guid - GUID абонента
// state - состояние (подключен или отключен)
// nick - ник (имя на канале, не зависит от реального имени)
// addr - адрес
// rights - полномочия (набор полномочий)
// status - статус (сообщение абонента)
function TDnmpGrpcServer.SendAbonList(Addr: TAddr; ListType: AnsiChar): Boolean;
var
  i: Integer;
  al: TDnmpAbonentList;
  Abonent: TDnmpAbonent;
  sl, slData: TStringList;
  sDataType: string;
begin
  Result:=False;
  al:=self.UsersList;
  sDataType:='USERS';
  if ListType='A' then
  begin
    al:=self.ServiceInfo.Abonents;
    sDataType:='ABONENTS';
  end
  else if ListType='B' then
  begin
    //al:=self.BanList;
    sDataType:='BANLIST';
  end;

  sl:=TStringList.Create();
  slData:=TStringList.Create();
  for i:=0 to al.Count-1 do
  begin
    Abonent:=al[i];

    sl.Clear();
    // Send service info
//    sl.Values['guid']:=Abonent.GUID;
//    sl.Values['state']:=Abonent.Status;
//    sl.Values['nick']:=Abonent.Name;
//    sl.Values['addr']:=AddrToStr(Abonent.Addr);
//    sl.Values['rights']:='';
//    sl.Values['status']:=Abonent.StatusMsg;

    sl.Add(Abonent.GUID);
    sl.Add(Abonent.StateStr());
    sl.Add(Abonent.Nick);
    sl.Add(AddrToStr(Abonent.Addr));
    sl.Add(Abonent.Rights);
    sl.Add(Abonent.Status);
    slData.Add(sl.DelimitedText);
  end;
  sl.Free();

  if slData.Count>0 then
  begin
    Result:=True;
    Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, 'name='+Self.ServiceInfo.Name+#13+#10+'data='+sDataType, slData.Text);
  end;
  slData.Free();
end;

function TDnmpGrpcServer.SendBanList(Addr: TAddr): Boolean;
var
  i: Integer;
  sl, slData: TStringList;
  Item: TGrpcBanItem;
begin
  Result:=False;
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  for i:=0 to self.BanList.Count-1 do
  begin
    Item:=(self.BanList.Items[i] as TGrpcBanItem);

    sl.Clear();
    // item info
    sl.Add(Item.AuthorGUID);
    sl.Add(Item.AbonentGUID);
    sl.Add(TimestampToStr(Item.EndDate));
    sl.Add(Item.Reason);
    slData.Add(sl.DelimitedText);
  end;
  sl.Free();

  try
    if slData.Count>0 then
    begin
      Mgr.SendDataMsg(Addr, Self.ServiceInfo.ServiceType, 'name='+Self.ServiceInfo.Name+#13+#10+'data=BANLIST'+#13+#10+'format=CSV', slData.Text);
      Result:=True;
    end;
  finally
    slData.Free();
  end;
end;

function TDnmpGrpcServer.SendLastMsgList(Addr: TAddr): Boolean;
var
  i: Integer;
begin
  Result:=True;
  for i:=0 to self.MessagesList.Count-1 do
  begin
    SendChannelMsg(Addr, Self.MessagesList[i]);
  end;
end;

procedure TDnmpGrpcServer.BroadcastMsg(ChanMsg: TDnmpChannelMessage);
var
  i: Integer;
  Msg: TDnmpMsg;
begin
  if not Assigned(ChanMsg) then Exit;
  Msg:=TDnmpMsg.Create(Mgr.MyInfo.Addr, NewAddr, 'GRPC', '', '');
  Msg.Info.Values['name']:=ServiceInfo.Name;
  ChanMsg.FillMsg(Msg);
  // Broadcast to my points
  for i:=0 to Self.UsersList.Count-1 do
  begin
    if SameAddr(UsersList[i].Addr, ChanMsg.AuthorAddr) then Continue;
    if SameNode(UsersList[i].Addr, Mgr.MyInfo.Addr) then
    begin
      //SendChannelMsg(UsersList[i].Addr, ChanMsg);
      Msg.TargetAddr:=UsersList[i].Addr;
      Mgr.SendMsg(Msg);
    end;
  end;
  { TODO : Broadcast to nodes }
  if not SameNode(ServiceInfo.HostAddr, ChanMsg.AuthorAddr) then
  begin
    //SendChannelMsg(ServiceInfo.HostAddr, ChanMsg);
    Msg.TargetAddr:=ServiceInfo.HostAddr;
    Mgr.SendMsg(Msg);
  end;
  Msg.Free();
end;

procedure TDnmpGrpcServer.BroadcastCmd(sCmd: string);
var
  i: Integer;
  Msg: TDnmpMsg;
  sCmdName, sCmdParams: string;
begin
  Misc.ExtractCmd(sCmd, sCmdName, sCmdParams);

  Msg:=TDnmpMsg.Create(Mgr.MyInfo.Addr, NewAddr, 'GRPC', '', sCmdParams);
  Msg.Info.Values['name']:=ServiceInfo.Name;
  Msg.Info.Values['cmd']:=sCmdName;

  // Broadcast to my points
  for i:=0 to Self.UsersList.Count-1 do
  begin
    if SameNode(UsersList[i].Addr, Mgr.MyInfo.Addr) then
    begin
      Msg.TargetAddr:=UsersList[i].Addr;
      Mgr.SendMsg(Msg);
    end;
  end;
  { TODO : Broadcast to nodes }
  if not SameNode(ServiceInfo.HostAddr, Mgr.MyInfo.Addr) then
  begin
    Msg.TargetAddr:=ServiceInfo.HostAddr;
    Mgr.SendMsg(Msg);
  end;
  Msg.Free();
end;

function TDnmpGrpcServer.SayCmd(AbonGUID, sText: string): string;
var
  ChanMsg: TDnmpChannelMessage;
begin
  Result:='';
  if BanList.GetBan(AbonGUID)<>nil then
  begin
    // Banned!
    Exit;
  end;
  if Self.ServiceInfo.Abonents.GetAbonentByGUID(AbonGUID)=nil then
  begin
    // Not in abonents list
    Exit;
  end;

  ChanMsg:=CreateChannelMsg(AbonGUID, sText);
  if not Assigned(ChanMsg) then Exit;
  MessagesList.Add(ChanMsg);
  if MessagesList.Count > MessagesList.MaxCount then MessagesList.Delete(0);
  BroadcastMsg(ChanMsg);
  if Assigned(OnSay) then OnSay(self);
end;


// === TDnmpGrpcClient ===
constructor TDnmpGrpcClient.Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  MessagesList.MaxCount:=ciMaxMsgCountClient;
end;

destructor TDnmpGrpcClient.Destroy();
begin
  inherited Destroy();
end;

function TDnmpGrpcClient.SayCmd(AbonGUID, sText: string): string;
var
  ChanMsg: TDnmpChannelMessage;
begin
  Result:='';
  if BanList.GetBan(AbonGUID)<>nil then
  begin
    // Banned!
    Mgr.DebugText('GRPC Cannot SAY - abonent banned '+AbonGUID);
    Exit;
  end;
  if Self.UsersList.GetAbonentByGUID(AbonGUID)=nil then
  begin
    // Not in users list
    Mgr.DebugText('GRPC Cannot SAY - not in users list '+AbonGUID);
    Exit;
  end;

  ChanMsg:=CreateChannelMsg(AbonGUID, sText);
  if not Assigned(ChanMsg) then Exit;
  MessagesList.AddItem(ChanMsg);
  SendChannelMsg(Mgr.Uplink.LinkInfo.Addr, ChanMsg);
  Mgr.AddCmd('GRPC UPDATE '+ServiceInfo.Name+' TEXT');
  if Assigned(OnSay) then OnSay(self);
end;

function TDnmpGrpcClient.Cmd(Text: string; Addr: TAddr): string;
var
  sCmd, s: string;
  Params: TStringArray;
  i, n: Integer;
  Abonent: TDnmpAbonent;
begin
  Result:='';
  Params:=ParseStr(Text);
  n:=Length(Params);
  if n=0 then Exit;
  sCmd:=Params[0];
  if sCmd='' then Exit

  else if sCmd='JOIN' then
  begin
    if n<2 then Exit;
    Result:=self.JoinAbonent(Params[1]);
    if Result<>'' then Exit;
    Mgr.AddCmd('GRPC UPDATE '+self.ServiceInfo.Name);
  end

  else if sCmd='LEAVE' then
  begin
    if n<2 then Exit;
    Abonent:=Self.UsersList.GetAbonentByGUID(Params[1]);
    if not Assigned(Abonent) then Exit;
    self.UsersList.Remove(Abonent);
    Mgr.AddCmd('GRPC UPDATE '+self.ServiceInfo.Name);
    if Assigned(OnUsersChange) then OnUsersChange(self);
    if Assigned(OnAbonentsChange) then OnAbonentsChange(self);
  end

  else if sCmd='KICK' then
  begin
    if n<2 then Exit;
    Abonent:=Self.UsersList.GetAbonentByGUID(Params[1]);
    if not Assigned(Abonent) then Exit;
    self.UsersList.Remove(Abonent);
    Mgr.AddCmd('GRPC UPDATE '+self.ServiceInfo.Name);
    if Assigned(OnUsersChange) then OnUsersChange(self);
  end

  else if sCmd='SAY' then
  begin
    if n<3 then Exit;
    i:=Pos(' ', Text);
    s:=Copy(Text, i+1, MaxInt);
    i:=Pos(' ', Text);
    s:=Copy(Text, i+1, MaxInt);
    Result:=SayCmd(Params[1], s);
    Mgr.AddCmd('GRPC UPDATE '+self.ServiceInfo.Name+' TEXT');
  end

  else
  begin

  end;

end;

function TDnmpGrpcClient.ParseMsg(AMsg: TDnmpMsg): string;
var
  s, sChan, sAName, sAGUID: string;
  ChanMsg: TDnmpChannelMessage;
  Author: TDnmpAbonent;
begin
  Result:='';
  if not Assigned(AMsg) then Exit;
  if AMsg.MsgType <> Self.ServiceInfo.ServiceType then Exit;
  sChan:=Trim(AMsg.Info.Values['name']);
  if sChan <> self.ServiceInfo.Name then Exit;

  // Это сообщение?
  sAGUID:=Trim(AMsg.Info.Values['author_guid']);
  if sAGUID <> '' then
  begin
    //if Assigned(OnTextMsg) then OnTextMsg();
    Author:=Self.UsersList.GetAbonentByGUID(sAGUID);
    s:=Trim(AMsg.Info.Values['timestamp']);
    sAName:=Trim(AMsg.Info.Values['author_name']);

    ChanMsg:=self.MessagesList.AddItem(StrToTimeStamp(s), 0, StreamToStr(AMsg.Data), sAGUID, Author);
    //self.MessagesList.AddItem(ChanMsg)
    if Assigned(OnEvent) then OnEvent('GRPC TEXT', ChanMsg);
    Mgr.AddCmd('GRPC UPDATE '+ServiceInfo.Name+' TEXT');
    Exit;
  end;

  // Это команда?
  s:=Trim(AMsg.Info.Values['cmd']);
  if s <> '' then
  begin
    s:=s+' '+StreamToStr(AMsg.Data);
    Result:=Self.Cmd(s, AMsg.SourceAddr);
    Exit;
  end;

  // Это данные?
  s:=Trim(AMsg.Info.Values['data']);
  if s <> '' then
  begin
    Result:=Self.ReadData(s, StreamToStr(AMsg.Data));
    Exit;
  end;
end;

procedure TDnmpGrpcClient.Say(sText: string);
begin
  SayCmd(Mgr.MyInfo.GUID, sText);
end;  


end.
