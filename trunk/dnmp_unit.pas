unit dnmp_unit;

interface
uses SysUtils, Classes, Contnrs, IniFiles;

type
  TNodeID = Cardinal;
  TPointID = Cardinal;

  TAddr = record
    Node: TNodeID;
    Point: TPointID;
  end;

  TSeenBy = array of TNodeID;

  TDnmpManager = class;
  TDnmpParser = class;

  { TDnmpConf }

  TDnmpConf = class(TMemIniFile);

  TDnmpStorageType = (stUnknown, stString, stInteger, stNumber, stList, stDictionary);

  { TDnmpStorage }
  TDnmpStorage = class(TObject)
  private
    { name:object items storage }
    FItems: TStringList;
  public
    { stUnknown, stString, stInteger, stNumber, stList, stDictionary }
    StorageType: TDnmpStorageType;
    { Value for (String, Integer, Number) types }
    Value: AnsiString;
    constructor Create(AStorageType: TDnmpStorageType);
    destructor Destroy; override;
    { Items count for (List, Dictionary) types }
    function Count(): integer;
    procedure Add(AName: string; AValue: TDnmpStorage);
    procedure Add(AName, AValue: string); overload;
    procedure Add(AName: string; AValue: Integer); overload;
    procedure Add(AName: string; AValue: Real); overload;
    { Get storage item by name }
    function GetObject(AName: string): TDnmpStorage;
    { Get storage item by index }
    function GetObject(Index: integer): TDnmpStorage; overload;
    { Get name by index }
    function GetObjectName(Index: integer): string;
    function GetString(AName: string): string;
    function GetInteger(AName: string): Integer;
    function GetReal(AName: string): Real;
  end;

  { TDnmpMsg Single message. Refcounted. }

  TDnmpMsg = class(TInterfacedObject)
  public
    MsgType: AnsiString;
    TimeStamp: TDateTime;
    SourceAddr: TAddr;
    TargetAddr: TAddr;
    Info: TStringList;
    Data: TMemoryStream;
    SeenBy: TSeenBy;
    constructor Create(SAddr, TAddr: TAddr; AMsgType, Params, DataStr: string);
    destructor Destroy(); override;
    /// Читает сообщение из цифрового потока AStream
    function FromStream(AStream: TStream): boolean;
    /// Пишет сообщение в цифровой поток AStream
    function ToStream(AStream: TStream): boolean;
    /// Читает сообщение из строки Str. Формат строки - цифровой поток
    function FromString(Str: AnsiString): boolean;
    /// Возвращает сообщение в виде строки. Формат строки - цифровой поток
    function ToString(): AnsiString;
    /// Заполняет секцию параметров из строки, разделенной символами "|"
    function ParseInfo(Str: string): boolean;
    /// Проверяет наличие адреса в синбаях
    function HaveSeenBy(Addr: TAddr): boolean;
    /// Добавляет адрес в синбаи
    function AddSeenBy(Addr: TAddr): boolean;
  //private
  end;

  // { TODO: Сделать своп сообщений на диск }
  TDnmpMsgQueue = class(TObjectList)
  private
    function GetMsg(Index: Integer): TDnmpMsg;
    procedure SetMsg(Index: Integer; Value: TDnmpMsg);
  public
    property Items[Index: Integer]: TDnmpMsg read GetMsg write SetMsg; default;
    function GetMsgByAddr(FAddr: TAddr): TDnmpMsg;
    procedure SaveToFile(Filename: string);
    function LoadFromFile(Filename: string): Boolean;
  end;

  TDnmpLinkType = (ltPoint, ltNode, ltTemporary);

  { Link information. Refcounted. }

  { TLinkInfo }

  TLinkInfo = class(TInterfacedObject)
  public
    Addr: TAddr;
    GUID: string;
    SeniorGUID: string;
    Name: string;
    Owner: string;
    Location: string;
    IpAddr: string;
    PhoneNo: string;
    OtherInfo: string;
    Key: string;
    Rating: integer;
    Online: Boolean;
    LinkType: TDnmpLinkType;
    //SubscribedGroups: TGroupsList;
    function AddrStr(): string;
    function SameAddr(FAddr: TAddr): Boolean;
    function SameNode(FAddr: TAddr): Boolean;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToCSV(): string;
    function LoadFromCSV(sData: string): boolean;
    function FromConf(Conf: TDnmpConf; SectName: string): Boolean;
    function ToConf(Conf: TDnmpConf; SectName: string): Boolean;
    procedure Assign(li: TLinkInfo);
  end;

  TLinkInfoList = class(TObjectList)
  private
    function GetLinkInfo(Index: Integer): TLinkInfo;
    procedure SetLinkInfo(Index: Integer; Value: TLinkInfo);
  public
    Filename: string;
    property Items[Index: Integer]: TLinkInfo read GetLinkInfo write SetLinkInfo; default;
    function GetLinkInfoByAddr(FAddr: TAddr): TLinkInfo;
    function GetLinkInfoByGUID(SomeGUID: string): TLinkInfo;
    procedure SaveToFile();
    function LoadFromFile(): Boolean;
    function GetFreePointID(): TPointID;
    function GetFreeNodeID(): TNodeID;
  end;

  TNodeList = class(TLinkInfoList);
  TPointList = class(TLinkInfoList);

  TIncomingMsgEvent = procedure(Sender: TObject; Msg: TDnmpMsg) of object;

  // Базовый класс линка
  TDnmpLink = class(TObject)
  protected
    FOnIncomingMsg: TIncomingMsgEvent;
    FActive: boolean;
  public
    Mgr: TDnmpManager;

    MyInfo: TLinkInfo;
    LinkType: TDnmpLinkType;
    LinkInfo: TLinkInfo;
    Speed: Integer;

    Parser: TDnmpParser;

    constructor Create(AMgr: TDnmpManager);
    destructor Destroy(); override;
    // Установить соединение
    function Connect(): boolean; virtual;
    // Разорвать соединение
    function Disconnect(): boolean; virtual;
    // Принимать входящие подключения
    function Listen(): boolean; virtual;
    // Проверить соединение. Возвращает FALSE, если соединение невозможно восстановить
    function Check(): boolean; virtual;
    property Active: Boolean read FActive;
    // Отправить сообщение через этот линк
    function SendMsg(Msg: TDnmpMsg): boolean; virtual;
    property OnIncomingMsg: TIncomingMsgEvent read FOnIncomingMsg write FOnIncomingMsg;
    // Утвердить линк, принять его в сеть
    function Approve(): Boolean;
  end;

  TDnmpLinkList = class(TObjectList)
  private
    function GetLink(Index: Integer): TDnmpLink;
    procedure SetLink(Index: Integer; Value: TDnmpLink);
  public
    property Items[Index: Integer]: TDnmpLink read GetLink write SetLink; default;
    function GetLinkByAddr(FAddr: TAddr): TDnmpLink;
  end;

  TIncomingLinkEvent = procedure(Sender: TObject; Link: TDnmpLink) of object;

  // Базовый класс приемника подключений
  // Создает новые входящие подключения, прозводит опознание клиентов
  // NOT USED, see TDnmpLink
  TDnmpListener = class(TObject)
  protected
    FOnIncomingLink: TIncomingLinkEvent;
  public
    Active: boolean;
    Mgr: TDnmpManager;

    MyInfo: TLinkInfo;

    TmpLinkList: TDnmpLinkList;
    constructor Create(AMgr: TDnmpManager);
    destructor Destroy(); override;
    function Start(): boolean; virtual;
    function Stop(): boolean; virtual;
    property OnIncomingLink: TIncomingLinkEvent read FOnIncomingLink write FOnIncomingLink;
  end;

  TDnmpPassport = class(TObject)
  public
    LinkInfo: TLinkInfo;
    ContactsList: TLinkInfoList;
    ServicesList: TObjectList;
    DelayedMsgList: TDnmpMsgQueue;
  end;

  // Запись таблицы маршрутизации
  TDnmpRoutingTableRecord = record
    DestNodeID: TNodeID; // Узел назначения
    GateNodeID: TNodeID; // Соседний узел, за которым прячется узел назначения
    TraceID: Cardinal;   // Таймштамп трассировки
  end;

  TDnmpRoutingTableRecordArray = array of TDnmpRoutingTableRecord;

  // Таблица маршрутизации сервера
  TDnmpRoutingTable = class(TObject)
  private
    FItems: TDnmpRoutingTableRecordArray;
    FCount: Integer;
    function FGetGateForDest(DestID: TNodeID; var GateID: TNodeID): boolean;
  public
    Links: TDnmpLinkList;
    constructor Create(ALinks: TDnmpLinkList);
    destructor Destroy; override;
    property Items: TDnmpRoutingTableRecordArray read FItems;
    property Count: Integer read FCount;
    // Определяет линк для заданного адреса
    function LinkForDestAddr(Addr: TAddr): TDnmpLink;
    // Добавляет запись таблицы маршрутизации
    procedure AddItem(GateID, DestID: TNodeID; TraceID: Cardinal);
    // Добавляет записи таблицы маршрутизации из синбаев
    procedure AddRountingItems(GateID: TNodeID; TraceID: Cardinal; SeenBy: TSeenBy);
    // Удаляет все записи с указанным GateID
    procedure DelGate(GateID: TNodeID);
    // Удаляет запись с указанным DestID
    procedure DelDest(DestID: TNodeID);
    // Удаляет все записи
    procedure Clear();
  end;

  // Базовый класс обработчика входящих сообщений
  // for links only
  TDnmpParser = class(TObject)
  public
    Mgr: TDnmpManager;
    Link: TDnmpLink;
    // Запуск парсера
    function Start(): Boolean; virtual; abstract;
    // Разбор сообщения и выполнение требуемых действий
    function ParseMessage(Msg: TDnmpMsg): Boolean; virtual; abstract;
  end;

  {// Список поддерживаемых типов сообщений
  TDnmpMsgTypes = class(TList)
  public
    constructor Create(sTypes: string);
    function AddType(sType: string): Boolean;
    function DelType(sType: string): Boolean;
    function HasType(sType: string): Boolean;
  end;}

  // Базовый класс обработчика входящих сообщений
  // services

  { TDnmpMsgHandler }

  TDnmpMsgHandler = class(TObject)
  protected
    FMgr: TDnmpManager;
    {MsgTypes: TDnmpMsgTypes; }
  public
    property Mgr: TDnmpManager read FMgr;
    { Add self to Mgr.MsgHandlers }
    constructor Create(AMgr: TDnmpManager);
    { Remove self from Mgr.MsgHandlers }
    destructor Destroy(); override;
    { Обработка команды (Thread-safe) от указанного адреса }
    function Cmd(Text: string; Addr: TAddr): string; virtual; abstract;
    // Разбор сообщения и выполнение требуемых действий
    // Возвращает True если сообщение обработано и дальнейшая обработка не требуется
    function ParseMsg(AMsg: TDnmpMsg): Boolean; virtual; abstract;
  end;

  TLogEvent = procedure(Sender: TObject; LogMsg: string) of object;
  TMgrEvent = procedure(Sender, AText: string) of object;
  TChatMsgEvent = procedure(SrcAddr: TAddr; TAext: string) of object;
  TPvtMsgEvent = procedure(SrcAddr: TAddr; AInfo, AText: string) of object;
  TChannelMsgEvent = procedure(SrcAddr: TAddr; AGroupName, AText: string) of object;
  TForumMsgEvent = procedure(SrcAddr: TAddr; AGroupName, ATopic, AText: string) of object;

  // Базовый класс менеджера
  TDnmpManager = class(TObject)
  private
    FOnLog: TLogEvent;
    FOnCmd: TLogEvent;
    FOnEvent: TMgrEvent;
    FOnIncomingMsg: TIncomingMsgEvent;
    FServerMode: Boolean;
    CmdQueue: TStringList;
    function CmdHandler(CmdText: string): string;
    procedure IncomingMsg(Msg: TDnmpMsg; Link: TDnmpLink);
    procedure ReadConfig();
    procedure WriteConfig();
  public
    MyInfo: TLinkInfo;
    /// Known linkable nodes
    NodeList: TNodeList;
    /// Owned points (Server only)
    PointList: TPointList;
    /// Known points
    ContactList: TLinkInfoList;
    /// Default uplink
    Uplink: TDnmpLink;
    /// Listener (Server only)
    ListenerLink: TDnmpLink;
    /// Active links
    LinkList: TDnmpLinkList;
    /// Outgoing messages queue
    MsgQueue: TDnmpMsgQueue;
    Conf: TDnmpConf;
    RoutingTable: TDnmpRoutingTable;
    /// Incoming messages handlers
    MsgHandlers: TObjectList;
    /// Path to data files
    sDataPath: string;

    constructor Create(ConfName: string);
    destructor Destroy(); override;
    property ServerMode: Boolean read FServerMode;
    // ==== Base functions
    // Send message, autodetect link for sending
    function SendMsg(Msg: TDnmpMsg): boolean;
    // Create and send message with given data
    procedure SendDataMsg(DestAddr: TAddr; MsgType, Info, Text: string);
    // Create and send error reply message
    procedure SendErrorMsg(DestAddr: TAddr; ErrCode, ErrText: string);
    procedure RunServer();
    procedure StopServer();
    procedure RunClient();
    procedure StopClient();
    procedure StartNodeConnection(NodeInfo: TLinkInfo);
    procedure StopNodeConnection(NodeInfo: TLinkInfo);
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnCmd: TLogEvent read FOnCmd write FOnCmd;
    property OnEvent: TMgrEvent read FOnEvent write FOnEvent;
    property OnIncomingMsg: TIncomingMsgEvent read FOnIncomingMsg write FOnIncomingMsg;
    procedure IncomingMsgHandler(Sender: TObject; Msg: TDnmpMsg);
    procedure DebugText(s: string);
    procedure DebugMsg(Msg: TDnmpMsg; Link: TDnmpLink; Comment: string);
    // Execute text command
    function Cmd(CmdText: string): string;
    // Internal event
    procedure Event(Sender, Text: string);
    // Add text command to commands queue
    procedure AddCmd(CmdText: string);
    // Execute text command from commands queue
    procedure Tick();
    function AddLink(Link: TDnmpLink): integer;
    function DelLink(Link: TDnmpLink): Boolean;
    function GetInfoByAddr(Addr: TAddr): TLinkInfo;
    function GetInfoByGUID(SomeGUID: string): TLinkInfo;
    function Approve(ALinkInfo: TLinkInfo): boolean;
    // ==== Сервисные функции
    procedure SendChatMsg(DestAddr: TAddr; Text: string);
    //procedure SendPvtMsg(DestAddr: TAddr; Info, Text: string); virtual; abstract;
    //procedure SendChannelMsg(GroupName, Text: string); virtual; abstract;
    //procedure SendForumMsg(GroupName, Topic, Text: string); virtual; abstract;
    //procedure SendDeliveryReport(DestAddr: TAddr; OrigTimestamp: TDateTime; ResultCode: Integer); virtual; abstract;
    // ==============
    //procedure OnChatMsg(SrcAddr: TAddr; Text: string); virtual; abstract;
    //procedure OnPvtMsg(SrcAddr: TAddr; Info, Text: string); virtual; abstract;
    //procedure OnChannelMsg(GroupName, Text: string); virtual; abstract;
    //procedure OnForumMsg(GroupName, Topic, Text: string); virtual; abstract;
    //procedure OnDeliveryReport(SrcAddr: TAddr; OrigTimestamp: TDateTime; ResultCode: Integer); virtual; abstract;
    procedure RequestInfoByAddr(Addr: TAddr);
    procedure RequestPointlist(Addr: TAddr);
    procedure SendLinkInfo(ALinkInfo: TLinkInfo; TargetAddr: TAddr); // [S]
    procedure ReadLinkInfo(Msg: TDnmpMsg); // [SC]
  end;

  /// Return addr 0.0
  function NewAddr(): TAddr;
  function AddrToStr(Addr: TAddr): string;
  function StrToAddr(StrAddr: string): TAddr;
  /// Return True, if given addresses equal
  function SameAddr(Addr1, Addr2: TAddr): boolean;
  /// Return True, if given addresses have equal nodes
  function SameNode(Addr1, Addr2: TAddr): boolean;
  // Return link type as string
  function LinkTypeToStr(lt: TDnmpLinkType): string;
  function StrToLinkType(s: string): TDnmpLinkType;

  // Convert TDateTime timestamp to string
  function TimestampToStr(dt: TDateTime): string;
  // Convert string to TDateTime timestamp
  function StrToTimestamp(s: string): TDateTime;

  // convert Stream to AnsiString
  function StreamToStr(AStream: TStream): AnsiString;
  // convert AnsiString to Stream
  function StrToStream(s: AnsiString; AStream: TStream): boolean;

  /// Save string to file with given Filename
  /// Return True on succes
  function StrToFile(FileName, Str: AnsiString): Boolean;
  /// Read string from file with given Filename
  /// Return empty string "" on failure
  function FileToStr(FileName: string): AnsiString;

  function GenerateKey(): string;
  // Extract and return first word from string
  function ExtractFirstWord(var s: string; delimiter: string = ' '): string;

const
  csConfigFileName = 'settings.ini';
  csPointlistFileName = 'PointList.lst';
  csNodelistFileName = 'NodeList.lst';
  csMsgQueueFileName = 'MsgQueue.dta';
  ciKeyLength = 64;

var
  sDnmpDataDir: string = 'data';

implementation
uses RC4, dnmp_client, dnmp_server, dnmp_ip, Misc;

// === Functions ===
{
function DWordToStr(x: Longword): AnsiString;
begin
  result:='0000';
  Move(X, result[1], SizeOf(x));
end;
}

function NewAddr(): TAddr;
begin
  Result.Node:=0;
  Result.Point:=0;
end;

function AddrToStr(Addr: TAddr): string;
begin
  result:=''+IntToStr(Addr.Node)+'.'+IntToStr(Addr.Point);
end;

function StrToAddr(StrAddr: string): TAddr;
var
  i: integer;
begin
  i:=Pos('.', StrAddr);
  result.Node:=StrToIntDef(Copy(StrAddr, 1, i-1), 0);
  result.Point:=StrToIntDef(Copy(StrAddr, i+1, maxint), 0);
end;

function SameAddr(Addr1, Addr2: TAddr): boolean;
begin
  Result := ((Addr1.Node=Addr2.Node) and (Addr1.Point=Addr2.Point));
end;

function SameNode(Addr1, Addr2: TAddr): boolean;
begin
  Result := (Addr1.Node=Addr2.Node);
end;

function LinkTypeToStr(lt: TDnmpLinkType): string;
begin
  result:='';
  case lt of
    ltPoint: Result:='point';
    ltNode:  Result:='node';
    ltTemporary: Result:='temp';
  end;
end;

function StrToLinkType(s: string): TDnmpLinkType;
begin
  Result:=ltTemporary;
  if s='point' then Result:=ltPoint
  else if s='node' then Result:=ltNode
  else if s='temp' then Result:=ltTemporary;
end;

function TimestampToStr(dt: TDateTime): string;
begin
  result:=IntToStr(DateTimeToFileDate(dt));
end;

function StrToTimestamp(s: string): TDateTime;
begin
  result:=FileDateToDateTime(StrToIntDef(s, 0));
end;

function StreamToStr(AStream: TStream): AnsiString;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('');
  AStream.Seek(0, soFromBeginning);
  ss.CopyFrom(AStream, AStream.Size);
  Result:=ss.DataString;
  ss.Free();
end;

function StrToStream(s: AnsiString; AStream: TStream): boolean;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(s);
  ss.Seek(0, soFromBeginning);
  AStream.Seek(0, soFromBeginning);
  AStream.CopyFrom(ss, ss.Size);
  ss.Free();
  Result:=true;
end;

function StrToFile(FileName, Str: AnsiString): Boolean;
var
  fs: TFileStream;
begin
  Result:=False;
  try
    fs:=TFileStream.Create(FileName, fmCreate);
    StrToStream(Str, fs);
  finally
    FreeAndNil(fs);
  end;
  Result:=True;
end;

function FileToStr(FileName: string): AnsiString;
var
  fs: TFileStream;
begin
  Result:='';
  if not FileExists(FileName) then Exit;
  fs:=TFileStream.Create(FileName, fmOpenRead);
  Result:=StreamToStr(fs);
  fs.Free();
end;

function GenerateKey(): string;
var
  sDict: string;
  i, l: Integer;
begin
  sDict:='qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890';
  l:=Length(sDict)-1;
  Result:='';
  Randomize();
  for i:=1 to ciKeyLength do Result:=Result+sDict[Random(l)+1];
end;

function ExtractFirstWord(var s: string; delimiter: string = ' '): string;
var
  i: integer;
begin
  Result:='';
  i:=Pos(delimiter, s);
  if i>0 then
  begin
    Result:=Copy(s, 1, i-1);
    s:=Copy(s, i+1, maxint);
  end
  else
  begin
    Result:=s;
    s:='';
  end;
end;


{ TDnmpStorage }

constructor TDnmpStorage.Create(AStorageType: TDnmpStorageType);
begin
  inherited Create();
  StorageType:=AStorageType;
  FItems:=TStringList.Create();
  FItems.OwnsObjects:=True;
end;

destructor TDnmpStorage.Destroy();
begin
  FItems.Free();
  inherited Destroy();
end;

function TDnmpStorage.Count(): integer;
begin
  Result:=FItems.Count;
end;

procedure TDnmpStorage.Add(AName: string; AValue: TDnmpStorage);
begin
  FItems.AddObject(AName, AValue);
end;

procedure TDnmpStorage.Add(AName, AValue: string);
var
  TmpItem: TDnmpStorage;
begin
  TmpItem:=TDnmpStorage.Create(stString);
  TmpItem.Value:=AValue;
  FItems.AddObject(AName, TmpItem);
end;

procedure TDnmpStorage.Add(AName: string; AValue: Integer);
var
  TmpItem: TDnmpStorage;
begin
  TmpItem:=TDnmpStorage.Create(stInteger);
  TmpItem.Value:=IntToStr(AValue);
  FItems.AddObject(AName, TmpItem);
end;

procedure TDnmpStorage.Add(AName: string; AValue: Real);
var
  TmpItem: TDnmpStorage;
begin
  TmpItem:=TDnmpStorage.Create(stNumber);
  TmpItem.Value:=FloatToStr(AValue);
  FItems.AddObject(AName, TmpItem);
end;

function TDnmpStorage.GetObject(AName: string): TDnmpStorage;
var
  n: integer;
  TmpItem: TDnmpStorage;
begin
  Result:=nil;
  n:=FItems.IndexOf(AName);
  if n>=0 then
  begin
    TmpItem:=(FItems.Objects[n] as TDnmpStorage);
    if TmpItem.StorageType=stDictionary then Result:=TmpItem;
  end;
end;

function TDnmpStorage.GetObject(Index: integer): TDnmpStorage;
begin
  Result:=nil;
  if (Index>=0) and (Index<Count) then
  begin
    Result:=(FItems.Objects[Index] as TDnmpStorage);
  end;
end;

function TDnmpStorage.GetObjectName(Index: integer): string;
begin
  Result:='';
  if (Index>=0) and (Index<FItems.Count) then Result:=FItems[Index];
end;

function TDnmpStorage.GetString(AName: string): string;
var
  n: integer;
  TmpItem: TDnmpStorage;
begin
  Result:='';
  n:=FItems.IndexOf(AName);
  if n>=0 then
  begin
    TmpItem:=(FItems.Objects[n] as TDnmpStorage);
    Result:=TmpItem.Value;
    //if TmpItem.StorageType=stString then Result:=TmpItem.Value;
  end;
end;

function TDnmpStorage.GetInteger(AName: string): Integer;
begin
  Result:=StrToIntDef(GetString(AName), 0);
end;

function TDnmpStorage.GetReal(AName: string): Real;
begin
  Result:=StrToFloatDef(GetString(AName), 0);
end;

// === TDnmpMsg ===
constructor TDnmpMsg.Create(SAddr, TAddr: TAddr; AMsgType, Params,
  DataStr: string);
var
  ss: TStringStream;
begin
  Self.SourceAddr:=SAddr;
  Self.TargetAddr:=TAddr;
  Self.MsgType:=AMsgType;
  self.TimeStamp:=Now();
  self.Info:=TStringList.Create();
  self.Info.Text:=Params;
  //self.Info.Values['timestamp']:=Misc.GetTimestampStr();
  self.Data:=TMemoryStream.Create();
  self.Data.Clear();
  if Length(DataStr)>0 then
  begin
    ss:=TStringStream.Create(DataStr);
    ss.Seek(0, soFromBeginning);
    Self.Data.CopyFrom(ss, ss.Size);
    ss.Free();
  end;
  SetLength(Self.SeenBy, 0);
  //self.ReadFromStr(Str);
end;

destructor TDnmpMsg.Destroy();
begin
  self.Data.Free();
  self.Info.Free();
  inherited Destroy();
end;

function TDnmpMsg.FromString(Str: AnsiString): boolean;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(Str);
  ss.Seek(0, soFromBeginning);
  Result:=Self.FromStream(ss);
  ss.Free();
end;

{function TDnmpMsg.FromString(Str: string): boolean;
var
  n, i: integer;
  s, s2: string;
begin
  s:=Str;
  i:=1;
  while i<4 do
  begin
    n:=Pos('|', s);
    if n=0 then Break;
    if n>1 then
    begin
      s2:='';
      case i of
        1: s2:='MsgType';
        2: s2:='Timestamp';
        3: s2:='SrcAddr';
        4: s2:='DstAddr';
      end;
      self.Info.Values[s2]:=Copy(s, 1, n-1);
    end;
    Inc(i);
    s:=Copy(s, n+1, maxint);
  end;
  self.Info.Values['MsgBody']:=s;
end;}

function TDnmpMsg.ToString(): AnsiString;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('');
  ss.Seek(0, soFromBeginning);
  Self.ToStream(ss);
  Result:=ss.DataString;
  ss.Free();
end;

function TDnmpMsg.FromStream(AStream: TStream): boolean;
var
  ms: TMemoryStream;
  iMsgSize, iParamsSize, iDataSize, iSeenbyOffset, iSeenbySize: Cardinal;
  TmpMsgType: array [0..3] of AnsiChar;
begin
  Result:=False;
  if not Assigned(AStream) then Exit;
  AStream.Seek(0, soFromBeginning);
  //AStream.Read(iMsgSize, SizeOf(iMsgSize));
  //if iMsgSize<>(AStream.Size-SizeOf(iMsgSize)) then Exit;
  AStream.Read(TmpMsgType, SizeOf(TmpMsgType));
  Self.MsgType:=TmpMsgType;
  AStream.Read(Self.TimeStamp, SizeOf(Self.TimeStamp));
  AStream.Read(self.SourceAddr, SizeOf(self.SourceAddr));
  AStream.Read(Self.TargetAddr, SizeOf(Self.TargetAddr));
  AStream.Read(iSeenbyOffset, SizeOf(iSeenbyOffset));
  AStream.Read(iParamsSize, SizeOf(iParamsSize));

  Self.Info.Clear();
  ms:=TMemoryStream.Create();
  ms.CopyFrom(AStream, iParamsSize);
  ms.Seek(0, soFromBeginning);
  Self.Info.LoadFromStream(ms);
  FreeAndNil(ms);

  Self.Data.Clear();
  AStream.Read(iDataSize, SizeOf(iDataSize));
  if iDataSize<>0 then Self.Data.CopyFrom(AStream, iDataSize);

  iSeenbySize:=AStream.Size-AStream.Position;
  if (iSeenbySize>0) and ((iSeenbySize mod 4)=0) then
  begin
    //ms.Write(Self.SeenBy, Length(Self.SeenBy)*SizeOf(TNodeID));
    AStream.Read(Self.SeenBy, iSeenbySize);
  end;
  Result:=True;
end;

function TDnmpMsg.ToStream(AStream: TStream): boolean;
var
  ms: TMemoryStream;
  iParamsSize, iDataSize, iSeenbyOffset: Cardinal;
  FourCC: array [0..3] of AnsiChar;
  i, n: Integer;
begin
  Result:=False;
  if not Assigned(AStream) then Exit;
  iParamsSize:=Length(Self.Info.Text);
  iDataSize:=Self.Data.Size;
  iSeenbyOffset:=iParamsSize+iDataSize+24;
  FourCC:='';
  n:=Length(MsgType);
  if n>4 then n:=4;
  for i:=0 to n-1 do FourCC[i]:=MsgType[i+1];

  ms:=TMemoryStream.Create();
  ms.Write(FourCC, SizeOf(FourCC));
  ms.Write(Self.TimeStamp, SizeOf(Self.TimeStamp));
  ms.Write(Self.SourceAddr, SizeOf(Self.SourceAddr));
  ms.Write(Self.TargetAddr, SizeOf(Self.TargetAddr));
  ms.Write(iSeenbyOffset, SizeOf(iSeenbyOffset));
  ms.Write(iParamsSize, SizeOf(iParamsSize));
  Self.Info.SaveToStream(ms);
  ms.Write(iDataSize, SizeOf(iDataSize));
  Self.Data.Seek(0, soFromBeginning);
  ms.CopyFrom(Self.Data, iDataSize);
  if Length(Self.SeenBy)>0 then
  begin
    //ms.Write(Self.SeenBy, Length(Self.SeenBy)*SizeOf(TNodeID));
    ms.Write(Self.SeenBy, SizeOf(Self.SeenBy));
  end;

  ms.Seek(0, soFromBeginning);
  AStream.Size:=ms.Size;
  AStream.Seek(0, soFromBeginning);
  ms.SaveToStream(AStream);
  FreeAndNil(ms);
  Result:=true;
end;

function TDnmpMsg.ParseInfo(Str: string): boolean;
var
  sl: TStringList;
  i: integer;
begin
  sl:=TStringList.Create();
  sl.Delimiter:='|';
  sl.DelimitedText:=Str;
  for i:=0 to sl.Count-1 do Self.Info.Add(sl[i]);
  sl.Free();
  Result:=True;
end;

/// Проверяет наличие адреса в синбаях
function TDnmpMsg.HaveSeenBy(Addr: TAddr): boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to Length(Self.SeenBy)-1 do
  begin
    if Self.SeenBy[i] = Addr.Node then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

/// Добавляет адрес в синбаи
function TDnmpMsg.AddSeenBy(Addr: TAddr): boolean;
begin
  Result:=False;
  if HaveSeenBy(Addr) then Exit;
  SetLength(Self.SeenBy, Length(Self.SeenBy)+1);
  Self.SeenBy[Length(Self.SeenBy)-1]:=Addr.Node;
  Result:=True;
end;


// === TDnmpMsgQueue ===
function TDnmpMsgQueue.GetMsg(Index: Integer): TDnmpMsg;
begin
  Result:=TDnmpMsg(inherited Items[index]);
end;

procedure TDnmpMsgQueue.SetMsg(Index: Integer; Value: TDnmpMsg);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpMsgQueue.GetMsgByAddr(FAddr: TAddr): TDnmpMsg;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if SameAddr(TDnmpMsg(Items[i]).TargetAddr, FAddr) then
    begin
      Result:=TDnmpMsg(Items[i]);
      Exit;
    end;
  end;
end;

procedure TDnmpMsgQueue.SaveToFile(Filename: string);
var
  i: Integer;
  msSize: Cardinal;
  fs: TFileStream;
  ms: TMemoryStream;
begin
  if Count=0 then
  begin
    if FileExists(Filename) then DeleteFile(Filename);
    Exit;
  end;
  fs:=TFileStream.Create(Filename, fmCreate);
  ms:=TMemoryStream.Create();
  for i:=0 to Count-1 do
  begin
    ms.Clear();
    Items[i].ToStream(ms);
    ms.Position:=0;
    msSize:=ms.Size;
    fs.WriteBuffer(msSize, SizeOf(msSize));
    fs.CopyFrom(ms, msSize);
  end;
  ms.Free();
  fs.Free();
end;

function TDnmpMsgQueue.LoadFromFile(Filename: string): Boolean;
var
  msSize: Cardinal;
  fs: TFileStream;
  ms: TMemoryStream;
  msg: TDnmpMsg;
begin
  Result:=false;
  Self.Clear();
  if not FileExists(Filename) then Exit;
  try
    fs:=TFileStream.Create(Filename, fmOpenRead);
  except
    FreeAndNil(fs);
    Exit;
  end;
  fs.Position:=0;
  ms:=TMemoryStream.Create();

  repeat
    try
      fs.ReadBuffer(msSize, SizeOf(msSize));
    except
      Break;
    end;
    ms.Clear();
    ms.CopyFrom(fs, msSize);
    msg:=TDnmpMsg.Create(NewAddr, NewAddr, '','','');
    if msg.FromStream(ms) then Self.Add(msg) else msg.Free();
  until fs.Position = fs.Size;
  ms.Free();
  fs.Free();

  Result:=True;
end;

// === TLinkInfo ===
function TLinkInfo.AddrStr(): string;
begin
  Result:=AddrToStr(Addr);
end;

function TLinkInfo.SameAddr(FAddr: TAddr): Boolean;
begin
  Result := ((Addr.Node=FAddr.Node) and (Addr.Point=FAddr.Point));
end;

function TLinkInfo.SameNode(FAddr: TAddr): Boolean;
begin
  Result := (Addr.Node=FAddr.Node);
end;

function TLinkInfo.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('addr', Self.AddrStr);
  Result.Add('guid', Self.GUID);
  Result.Add('senior_guid', Self.SeniorGUID);
  Result.Add('name', Self.Name);
  Result.Add('owner', Self.Owner);
  Result.Add('location', Self.Location);
  Result.Add('ip_addr', Self.IpAddr);
  Result.Add('phone', Self.PhoneNo);
  Result.Add('other_info', Self.OtherInfo);
  Result.Add('rating', Self.Rating);
  Result.Add('key', Self.Key);
end;

function TLinkInfo.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  Self.Addr:=StrToAddr(Storage.GetString('addr'));
  Self.GUID:=Storage.GetString('guid');
  Self.SeniorGUID:=Storage.GetString('senior_guid');
  Self.Name:=Storage.getString('name');
  Self.Owner:=Storage.getString('owner');
  Self.Location:=Storage.getString('location');
  Self.IpAddr:=Storage.getString('ip_addr');
  Self.PhoneNo:=Storage.getString('phone');
  Self.OtherInfo:=Storage.getString('other_info');
  Self.Rating:=Storage.GetInteger('rating');
  Self.Key:=Storage.getString('key');
  Result:=True;
end;

function TLinkInfo.SaveToCSV(): string;
var
  sl: TStringList;
begin
  // TODO: from Storage
  sl:=TStringList.Create();
  sl.Values['Addr']:=Self.AddrStr;
  sl.Values['Name']:=Self.Name;
  sl.Values['Owner']:=Self.Owner;
  sl.Values['IpAddr']:=Self.IpAddr;
  sl.Values['PhoneNo']:=Self.PhoneNo;
  sl.Values['Rating']:=IntToStr(Self.Rating);
  sl.Values['Key']:=Self.Key;
  Result:=sl.DelimitedText;
  sl.Free();
end;

function TLinkInfo.LoadFromCSV(sData: string): boolean;
var
  sl: TStringList;
begin
  // TODO: from Storage
  sl:=TStringList.Create();
  sl.DelimitedText:=sData;
  Self.Addr:=StrToAddr(sl.Values['Addr']);
  Self.Name:=sl.Values['Name'];
  Self.Owner:=sl.Values['Owner'];
  Self.IpAddr:=sl.Values['IpAddr'];
  Self.PhoneNo:=sl.Values['PhoneNo'];
  Self.Rating:=StrToIntDef(sl.Values['Rating'], 1);
  Self.Key:=sl.Values['Key'];
  sl.Free();
  Result:=True;
end;

function TLinkInfo.FromConf(Conf: TDnmpConf; SectName: string): Boolean;
begin
  // TODO: from Storage
  Result:=False;
  if not Assigned(Conf) then Exit;
  Self.Addr:=StrToAddr(Conf.ReadString(SectName, 'Addr', ''));
  Self.GUID:=Conf.ReadString(SectName, 'GUID', Self.GUID);
  Self.SeniorGUID:=Conf.ReadString(SectName, 'SeniorGUID', Self.SeniorGUID);
  Self.Name:=Conf.ReadString(SectName, 'Name', Self.Name);
  Self.Owner:=Conf.ReadString(SectName, 'Owner', Self.Owner);
  Self.Location:=Conf.ReadString(SectName, 'Location', Self.Location);
  Self.IpAddr:=Conf.ReadString(SectName, 'IpAddr', Self.IpAddr);
  Self.PhoneNo:=Conf.ReadString(SectName, 'PhoneNo', Self.PhoneNo);
  Self.OtherInfo:=Conf.ReadString(SectName, 'OtherInfo', Self.OtherInfo);
  Self.Rating:=StrToIntDef(Conf.ReadString(SectName, 'Rating', ''), 1);
  Self.Key:=Conf.ReadString(SectName, 'Key', Self.Key);
  Result:=True;
end;

function TLinkInfo.ToConf(Conf: TDnmpConf; SectName: string): Boolean;
begin
  // TODO: from Storage
  Result:=False;
  if not Assigned(Conf) then Exit;
  Conf.WriteString(SectName, 'Addr', Self.AddrStr());
  Conf.WriteString(SectName, 'GUID', Self.GUID);
  Conf.WriteString(SectName, 'SeniorGUID', Self.SeniorGUID);
  Conf.WriteString(SectName, 'Name', Self.Name);
  Conf.WriteString(SectName, 'Owner', Self.Owner);
  Conf.WriteString(SectName, 'Location', Self.Location);
  Conf.WriteString(SectName, 'IpAddr', Self.IpAddr);
  Conf.WriteString(SectName, 'PhoneNo', Self.PhoneNo);
  Conf.WriteString(SectName, 'OtherInfo', Self.OtherInfo);
  Conf.WriteString(SectName, 'Rating', IntToStr(Self.Rating));
  Conf.WriteString(SectName, 'Key', Self.Key);
  Result:=True;
end;

procedure TLinkInfo.Assign(li: TLinkInfo);
begin
  if not Assigned(li) then Exit;
  Self.Addr:=li.Addr;
  Self.GUID:=li.GUID;
  Self.SeniorGUID:=li.SeniorGUID;
  Self.Name:=li.Name;
  Self.Owner:=li.Owner;
  Self.Location:=li.Location;
  Self.IpAddr:=li.IpAddr;
  Self.PhoneNo:=li.PhoneNo;
  Self.OtherInfo:=li.OtherInfo;
  Self.Key:=li.Key;
  Self.Rating:=li.Rating;
end;

// === TLinkInfoList ===
function TLinkInfoList.GetLinkInfo(Index: Integer): TLinkInfo;
begin
  Result:=TLinkInfo(inherited Items[index]);
end;

procedure TLinkInfoList.SetLinkInfo(Index: Integer; Value: TLinkInfo);
begin
  inherited Items[Index]:=Value;
end;

function TLinkInfoList.GetLinkInfoByAddr(FAddr: TAddr): TLinkInfo;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if TLinkInfo(Items[i]).SameAddr(FAddr) then
    begin
      Result:=TLinkInfo(Items[i]);
      Exit;
    end;
  end;
end;

function TLinkInfoList.GetLinkInfoByGUID(SomeGUID: string): TLinkInfo;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if TLinkInfo(Items[i]).GUID = SomeGUID then
    begin
      Result:=TLinkInfo(Items[i]);
      Exit;
    end;
  end;
end;

procedure TLinkInfoList.SaveToFile();
var
  sl: TStringList;
  i: Integer;
begin
  if Count=0 then Exit;
  sl:=TStringList.Create();
  for i:=0 to Count-1 do
  begin
    sl.Add(Items[i].SaveToCSV);
  end;
  sl.SaveToFile(Filename);
  sl.Free();
end;

function TLinkInfoList.LoadFromFile(): Boolean;
var
  sl: TStringList;
  i: Integer;
  li: TLinkInfo;
begin
  Result:=false;
  Self.Clear();
  if not FileExists(Filename) then Exit;
  sl:=TStringList.Create();
  try
    sl.LoadFromFile(Filename);
  except
    sl.Free();
    Exit;
  end;

  for i:=0 to sl.Count-1 do
  begin
    li:=TLinkInfo.Create();
    if li.LoadFromCSV(sl[i]) then self.Add(li);
  end;
  sl.Free();
  Result:=True;
end;

function TLinkInfoList.GetFreePointID(): TPointID;
var
  i: Integer;
  LastPointID: TPointID;
begin
  LastPointID:=0;
  for i:=0 to self.Count-1 do
  begin
    if Items[i].Addr.Point > LastPointID then LastPointID:=Items[i].Addr.Point;
  end;
  Result:=LastPointID+1;
end;

function TLinkInfoList.GetFreeNodeID(): TNodeID;
var
  i: Integer;
  LastNodeID: TNodeID;
begin
  LastNodeID:=0;
  for i:=0 to self.Count-1 do
  begin
    if Items[i].Addr.Node > LastNodeID then LastNodeID:=Items[i].Addr.Node;
  end;
  Result:=LastNodeID+1;
end;

// === TLinkList ===
function TDnmpLinkList.GetLink(Index: Integer): TDnmpLink;
begin
  Result:=TDnmpLink(inherited Items[index]);
end;

procedure TDnmpLinkList.SetLink(Index: Integer; Value: TDnmpLink);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpLinkList.GetLinkByAddr(FAddr: TAddr): TDnmpLink;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if TDnmpLink(Items[i]).LinkInfo.SameAddr(FAddr) then
    begin
      Result:=TDnmpLink(Items[i]);
      Exit;
    end;
  end;
end;

// === TDnmpRoutingTable ===
constructor TDnmpRoutingTable.Create(ALinks: TDnmpLinkList);
begin
  Self.Links:=ALinks;
  SetLength(FItems, 0);
end;

destructor TDnmpRoutingTable.Destroy();
begin
  SetLength(FItems, 0);
end;

function TDnmpRoutingTable.FGetGateForDest(DestID: TNodeID; var GateID: TNodeID): boolean;
var
  i: Integer;
begin
  Result:=False;
  for i:=0 to FCount-1 do
  begin
    if FItems[i].DestNodeID = DestID then
    begin
      GateID:=FItems[i].GateNodeID;
      Result:=True;
      Exit;
    end;
  end;
end;

procedure TDnmpRoutingTable.AddItem(GateID, DestID: TNodeID; TraceID: Cardinal);
var
  i: Integer;
begin
  for i:=0 to FCount-1 do
  begin
    if FItems[i].DestNodeID = DestID then
    begin
      if GateID = FItems[i].GateNodeID then Exit;
      if TraceID = FItems[i].TraceID then Exit;
      GateID:=FItems[i].GateNodeID;
      Exit;
    end;
  end;

  Inc(FCount);
  SetLength(FItems, FCount);
  FItems[FCount-1].DestNodeID:=DestID;
  FItems[FCount-1].GateNodeID:=GateID;
  FItems[FCount-1].TraceID:=TraceID;
end;

procedure TDnmpRoutingTable.AddRountingItems(GateID: TNodeID; TraceID: Cardinal; SeenBy: TSeenBy);
var
  i: Integer;
begin
  for i:=0 to Length(SeenBy)-1 do
  begin
    AddItem(GateID, SeenBy[i], TraceID);
  end;
end;

function TDnmpRoutingTable.LinkForDestAddr(Addr: TAddr): TDnmpLink;
var
  i: Integer;
  GateID: TNodeID;
begin
  Result:=nil;
  if not Assigned(Links) then Exit;
  if not FGetGateForDest(Addr.Node, GateID) then Exit;

  for i:=0 to Links.Count-1 do
  begin
    if (Links[i] as TDnmpLink).LinkInfo.Addr.Node = GateID then
    begin
      Result:=(Links[i] as TDnmpLink);
      Exit;
    end;
  end;
end;

// Удаляет все записи с указанным GateID
procedure TDnmpRoutingTable.DelGate(GateID: TNodeID);
var
  i: Integer;
begin
  for i:=FCount-1 downto 0 do
  begin
    if FItems[i].GateNodeID = GateID then
    begin
      Dec(FCount);
      if i<(FCount) then FItems[i]:=FItems[FCount];
      SetLength(FItems, FCount);
    end;
  end;
end;

// Удаляет запись с указанным DestID
procedure TDnmpRoutingTable.DelDest(DestID: TNodeID);
var
  i: Integer;
begin
  for i:=FCount-1 downto 0 do
  begin
    if FItems[i].DestNodeID = DestID then
    begin
      Dec(FCount);
      if i<(FCount) then FItems[i]:=FItems[FCount];
      SetLength(FItems, FCount);
      Exit;
    end;
  end;
end;

// Удаляет все записи
procedure TDnmpRoutingTable.Clear();
begin
  FCount:=0;
  SetLength(FItems, FCount);
end;

// === TDnmpLink ===
constructor TDnmpLink.Create(AMgr: TDnmpManager);
begin
  inherited Create();
  Mgr:=AMgr;
  MyInfo:=Mgr.MyInfo;
  LinkType:=ltPoint;
  LinkInfo:=TLinkInfo.Create();
end;

destructor TDnmpLink.Destroy();
begin
  // Инфа о линке может попасть в поинтлист или нодлист
  // Если мы ее убьем здесь, то в другом месте может возникнуть ошибка при попытке
  // убить инфу второй раз
  if (Mgr.PointList.IndexOf(LinkInfo)=-1) and (Mgr.NodeList.IndexOf(LinkInfo)=-1) then
  begin
    FreeAndNil(LinkInfo);
  end;
  inherited Destroy();
end;

function TDnmpLink.SendMsg(Msg: TDnmpMsg): boolean;
begin
  if Assigned(Mgr) then Mgr.DebugMsg(Msg, Self, '@>');
  Result:=Active;
end;

function TDnmpLink.Approve(): Boolean;
begin
  Result:=false;
  if LinkType = ltTemporary then Exit;
  if not Assigned(Mgr) then Exit;
  Result:=Mgr.Approve(LinkInfo);
end;

function TDnmpLink.Connect(): boolean;
begin
  Result:=False;
end;

function TDnmpLink.Disconnect(): boolean;
begin
  FActive:=False;
  Result:=True;
end;

function TDnmpLink.Listen(): boolean;
begin
  Result:=False;
end;

function TDnmpLink.Check(): boolean;
begin
  Result:=Active;
end;


// === TDnmpListener ===
constructor TDnmpListener.Create(AMgr: TDnmpManager);
begin
  inherited Create();
  Mgr:=AMgr;
  MyInfo:=Mgr.MyInfo;
end;

destructor TDnmpListener.Destroy();
begin
  Self.Stop();
  inherited Destroy();
end;

function TDnmpListener.Start(): boolean;
begin
  Result:=False;
end;

function TDnmpListener.Stop(): boolean;
begin
  Active:=False;
  Result:=True;
end;

// === TDnmpMsgParser ===
constructor TDnmpMsgHandler.Create(AMgr: TDnmpManager);
begin
  inherited Create();
  self.FMgr:=AMgr;
  if Assigned(AMgr) then AMgr.MsgHandlers.Add(self);
end;

destructor TDnmpMsgHandler.Destroy();
begin
  if Assigned(Mgr) then Mgr.MsgHandlers.Extract(self);
  inherited Destroy();
end;

// ===================
// === TDnmpManager ===
// ===================
constructor TDnmpManager.Create(ConfName: string);
begin
  inherited Create();
  sDataPath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(sDnmpDataDir)+ConfName);
  Misc.CheckPath(sDataPath);

  DebugText('Config file='+sDataPath+csConfigFileName);
  Conf:=TDnmpConf.Create(sDataPath+csConfigFileName);
  MyInfo:=TLinkInfo.Create();

  NodeList:=TNodeList.Create();
  PointList:=TPointList.Create();
  ContactList:=TLinkInfoList.Create();

  LinkList:=TDnmpLinkList.Create();
  RoutingTable:=TDnmpRoutingTable.Create(LinkList);

  CmdQueue:=TStringList.Create;
  DebugText('MsgQueue file='+sDataPath+csMsgQueueFileName);
  MsgQueue:=TDnmpMsgQueue.Create(true);
  MsgQueue.LoadFromFile(sDataPath+csMsgQueueFileName);

  MsgHandlers:=TObjectList.Create(true);

  ReadConfig();
  //self.Parser:=
end;

destructor TDnmpManager.Destroy();
begin
  WriteConfig();
  // ServiceDirectory created not in constructor
  if Assigned(MsgHandlers) then FreeAndNil(MsgHandlers);

  MsgQueue.SaveToFile(sDataPath+csMsgQueueFileName);
  FreeAndNil(MsgQueue);
  FreeAndNil(CmdQueue);

  FreeAndNil(RoutingTable);
  FreeAndNil(LinkList);

  FreeAndNil(ContactList);
  FreeAndNil(PointList);
  FreeAndNil(NodeList);

  FreeAndNil(MyInfo);
  FreeAndNil(Conf);
  inherited Destroy();
end;

procedure TDnmpManager.RunServer();
var
  tmpLink: TDnmpLink;
  sTcpPort: string;
begin
  self.FServerMode:=True;

  // Create server listener link
  sTcpPort:=Conf.ReadString('Options', 'ListenTcpPort', '4044');
  tmpLink:=TIpLink.Create(self, '', sTcpPort, 'TCP');
  tmpLink.LinkType:=ltTemporary;
  tmpLink.Mgr:=Self;
  tmpLink.Parser:=TDnmpParserServer.Create(Self, tmpLink);
  tmpLink.OnIncomingMsg:=@IncomingMsgHandler;
  tmpLink.MyInfo:=Self.MyInfo;
  if tmpLink.Listen() then
  begin
    ListenerLink:=tmpLink;
    LinkList.Add(tmpLink);
    DebugText('Server started. '+TIpLink(tmpLink).LinkHost+':'+TIpLink(tmpLink).LinkPort);
    Event('MGR','REFRESH');
  end
  else FreeAndNil(tmpLink);
end;

procedure TDnmpManager.RunClient();
var
  tmpLink: TDnmpLink;
  sUplinkAddr, sTcpPort: string;
begin
  self.FServerMode:=false;

  // Create connection to server
  sUplinkAddr:=Conf.ReadString('Options', 'UplinkAddr', 'localhost');
  sTcpPort:=Conf.ReadString('Options', 'TcpPort', '4044');
  tmpLink:=TIpLink.Create(Self, sUplinkAddr, sTcpPort, 'TCP');
  tmpLink.LinkType:=ltPoint;
  tmpLink.Mgr:=Self;
  tmpLink.Parser:=TDnmpParserClient.Create(Self, tmpLink);
  tmpLink.OnIncomingMsg:=@IncomingMsgHandler;
  tmpLink.MyInfo:=Self.MyInfo;
  if tmpLink.Connect() then
  begin
    UpLink:=tmpLink;
    LinkList.Add(tmpLink);
    DebugText('Client started. '+TIpLink(tmpLink).LinkHost+':'+TIpLink(tmpLink).LinkPort);
    Event('MGR','REFRESH');
  end
  else FreeAndNil(tmpLink);
end;

procedure TDnmpManager.StopServer();
begin
  if not Assigned(ListenerLink) then Exit;
  LinkList.Remove(ListenerLink);
  //FreeAndNil(ListenerLink);
  ListenerLink:=nil;
  DebugText('Server stopped.');
end;

procedure TDnmpManager.StopClient();
begin
  if not Assigned(Uplink) then Exit;
  if Uplink.Disconnect() then LinkList.Remove(Uplink);
  //FreeAndNil(Uplink);
  Uplink:=nil;
  DebugText('Client stopped.');
end;

procedure TDnmpManager.StartNodeConnection(NodeInfo: TLinkInfo);
var
  tmpLink: TIpLink;
  sIpHost, sIpPort: string;
begin
  if not self.ServerMode then Exit;

  sIpHost:=Misc.GetIpHost(NodeInfo.IpAddr);
  sIpPort:=Misc.GetIpPort(NodeInfo.IpAddr);
  if sIpHost='' then sIpHost:='localhost';
  if sIpPort='' then sIpPort:='4044';

  // Create connection to server
  tmpLink:=TIpLink.Create(Self, sIpHost, sIpPort, 'TCP');
  tmpLink.LinkType:=ltNode;
  tmpLink.Mgr:=Self;
  tmpLink.Parser:=TDnmpParserServer.Create(Self, tmpLink);
  tmpLink.OnIncomingMsg:=@IncomingMsgHandler;
  tmpLink.MyInfo:=Self.MyInfo;
  if tmpLink.Connect() then
  begin
    //UpLink:=tmpLink;
    LinkList.Add(tmpLink);
    DebugText('Node link started. '+tmpLink.LinkHost+':'+tmpLink.LinkPort);
    Event('MGR','REFRESH');
  end
  else FreeAndNil(tmpLink);

end;

procedure TDnmpManager.StopNodeConnection(NodeInfo: TLinkInfo);
var
  i: Integer;
begin
  if Assigned(Uplink) then
  begin
    if SameAddr(Uplink.LinkInfo.Addr, NodeInfo.Addr) then Uplink:=nil;
  end;
  for i:=0 to LinkList.Count-1 do
  begin
    if SameAddr(TDnmpLink(LinkList[i]).LinkInfo.Addr, NodeInfo.Addr) then
    begin
      DebugText('Removing node link: '+NodeInfo.AddrStr()+' '+NodeInfo.Name);
      LinkList.Delete(i);
      Event('MGR','REFRESH');
      Exit;
    end;
  end;
end;

function TDnmpManager.AddLink(Link: TDnmpLink): integer;
begin
  Result:=self.LinkList.Add(Link);
  if ServerMode then
  begin
    Link.Parser:=TDnmpParserServer.Create(Self, Link);
  end
  else
  begin
    Link.Parser:=TDnmpParserClient.Create(Self, Link);
  end;
  if Assigned(Link.Parser) then Link.Parser.Start();
end;

function TDnmpManager.DelLink(Link: TDnmpLink): Boolean;
begin
  //Result:=self.LinkList.Remove(Link);
  Result:=True;
  self.LinkList.Extract(Link);
end;

procedure TDnmpManager.DebugText(s: string);
begin
  if Assigned(FOnLog) then OnLog(Self, s);
end;

procedure TDnmpManager.DebugMsg(Msg: TDnmpMsg; Link: TDnmpLink; Comment: string);
var
  s, sLinkInfo: string;
  i: integer;
begin
  // Log message
  if Assigned(FOnLog) then
  begin
    sLinkInfo:='';
    if Assigned(Link) then
    begin
      sLinkInfo:=Link.LinkInfo.AddrStr+' '+Link.LinkInfo.Name;
    end;
    s:='---------------------------------------------'+#13+#10
    +Comment+' '+sLinkInfo+#13+#10
    +'['+Msg.MsgType+']  '+AddrToStr(Msg.SourceAddr)+' -> '+AddrToStr(Msg.TargetAddr)+'  ('+TimestampToStr(Msg.TimeStamp)+')'+#13+#10;
    for i:=0 to Msg.Info.Count-1 do
    begin
      s:=s+Msg.Info[i]+#13+#10;
    end;
    if Msg.Data.Size > 0 then s:=s+'Data size = '+IntToStr(Msg.Data.Size)+#13+#10;
    OnLog(Self, s);
  end;
end;

procedure TDnmpManager.IncomingMsg(Msg: TDnmpMsg; Link: TDnmpLink);
var
  i: Integer;
begin
  // Log message
  //DebugMsg(Msg, Link, '<<');

  // Pre-parse incomong msg
  if not Link.Parser.ParseMessage(Msg) then
  begin
    // Message type unknown for parser, maybe it's service
    if SameAddr(Msg.TargetAddr, MyInfo.Addr) then
    begin
      if Assigned(MsgHandlers) then
      begin
        for i:=0 to MsgHandlers.Count-1 do (MsgHandlers[i] as TDnmpMsgHandler).ParseMsg(Msg);
      end;
    end;
  end;

  if Assigned(OnIncomingMsg) then OnIncomingMsg(Link, Msg);
end;

procedure TDnmpManager.IncomingMsgHandler(Sender: TObject; Msg: TDnmpMsg);
begin
  if (Sender is TDnmpLink) then
  begin
    IncomingMsg(Msg, (Sender as TDnmpLink));
  end;
end;

procedure TDnmpManager.Event(Sender, Text: string);
begin
  if Assigned(OnEvent) then OnEvent(Sender, Text);
end;

function TDnmpManager.Cmd(CmdText: string): string;
begin
  Result:=CmdHandler(CmdText);
  if Assigned(OnCmd) then OnCmd(Self, CmdText);
end;

procedure TDnmpManager.AddCmd(CmdText: string);
begin
  if Assigned(CmdQueue) then CmdQueue.Add(CmdText);
end;

procedure TDnmpManager.Tick(); // Выполняет команды из очереди команд
begin
  if Assigned(CmdQueue) then
  begin
    while CmdQueue.Count>0 do
    begin
      Cmd(CmdQueue[0]);
      CmdQueue.Delete(0);
    end;
  end;
end;

procedure TDnmpManager.ReadConfig();
var
  Sect, MainSect : string;
  i, n: Integer;
begin
  // MyInfo
  MyInfo.FromConf(Conf, 'MyInfo');
  MainSect:='Main';

  // Nodelist
  n:=Conf.ReadInteger(MainSect, 'nodelist_count', 0);
  for i:=0 to n-1 do
  begin
    Sect:='Node_'+IntToStr(i);
    NodeList.Add(TLinkInfo.Create());
    NodeList[i].FromConf(Conf, Sect);
  end;

  // Pointlist
  n:=Conf.ReadInteger(MainSect, 'pointlist_count', 0);
  for i:=0 to n-1 do
  begin
    Sect:='Point_'+IntToStr(i);
    PointList.Add(TLinkInfo.Create());
    PointList[i].FromConf(Conf, Sect);
  end;

  // Contacts
  n:=Conf.ReadInteger(MainSect, 'contacts_count', 0);
  for i:=0 to n-1 do
  begin
    Sect:='Contact_'+IntToStr(i);
    ContactList.Add(TLinkInfo.Create());
    ContactList[i].FromConf(Conf, Sect);
  end;
end;

procedure TDnmpManager.WriteConfig();
var
  Sect: string;
  i: Integer;
begin
  // Main
  Sect:='Main';
  Conf.WriteString(Sect, 'tcp_port', '4044');
  Conf.WriteInteger(Sect, 'nodelist_count', NodeList.Count);
  Conf.WriteInteger(Sect, 'pointlist_count', PointList.Count);
  Conf.WriteInteger(Sect, 'contacts_count', ContactList.Count);

  // MyInfo
  MyInfo.ToConf(Conf, 'MyInfo');

  // Nodelist
  for i:=0 to NodeList.Count-1 do
  begin
    Sect:='Node_'+IntToStr(i);
    NodeList[i].ToConf(Conf, Sect);
  end;

  // Pointlist
  for i:=0 to PointList.Count-1 do
  begin
    Sect:='Point_'+IntToStr(i);
    PointList[i].ToConf(Conf, Sect);
  end;

  // Contacts
  for i:=0 to ContactList.Count-1 do
  begin
    Sect:='Contact_'+IntToStr(i);
    ContactList[i].ToConf(Conf, Sect);
  end;
  Conf.UpdateFile();
end;

function TDnmpManager.SendMsg(Msg: TDnmpMsg): boolean;
var
  i: integer;
  TargetPoint: TPointID;
  DnmpLink: TDnmpLink;
begin
  Result:=true;

  if self.ServerMode then
  begin
    // Сообщение другому поинту этого узла
    if Msg.TargetAddr.Node = MyInfo.Addr.Node then
    begin
      TargetPoint:=Msg.TargetAddr.Point;

      // Сообщение самому узлу
      if TargetPoint = MyInfo.Addr.Point then
      begin
        //Mgr.IncomingMsg(Msg, Link);
        Exit;
      end;

      // Поиск поинта назначения в списке поинт-линков
      for i:=0 to LinkList.Count-1 do
      begin
        if not LinkList[i].Active then Continue;
        if not LinkList[i].LinkInfo.SameAddr(Msg.TargetAddr) then Continue;
        // Отправка сообщения поинту
        LinkList[i].SendMsg(Msg);
        Exit;
      end;

      // Поиск поинта назначения в поинтлисте
      for i:=0 to PointList.Count-1 do
      begin
        if PointList[i].Addr.Point = TargetPoint then
        begin
          // Помещаем сообщение в очередь отправки поинту
          //DelayMsg(Msg);
          //SendInfoMsg(Msg, '201', 'Message delayed');
          DebugText('201 - Message delayed');
          Exit;
        end;
      end;

      // Поинта назначения нет в поинтлисте, возвращаем ошибку
      //SendErrorMsg(Msg, '101', 'Destination point not found');
      DebugText('101 - Destination point not found');
      Result:=False;
      Exit;
    end;

    // Сообщение на другой узел
    // Сообщение всем узлам
    if SameAddr(Msg.TargetAddr, NewAddr()) then
    begin
      // Отправка сообщения на узлы-линки
      for i:=0 to LinkList.Count-1 do
      begin
        if not LinkList[i].Active then Continue;
        if LinkList[i].LinkType <> ltNode then Continue;
        if Msg.HaveSeenBy(LinkList[i].LinkInfo.Addr) then Continue;
        LinkList[i].SendMsg(Msg);
      end;
      // Отправка на аплинк
      if Assigned(Uplink) then
      begin
        if not Msg.HaveSeenBy(Uplink.LinkInfo.Addr) then Uplink.SendMsg(Msg);
      end;
      Exit;
    end;

    // Поиск среди узлов-линков
    for i:=0 to LinkList.Count-1 do
    begin
      //if LinkList[i].LinkType <> ltNode then Continue;
      if not LinkList[i].Active then Continue;
      if not LinkList[i].LinkInfo.SameNode(Msg.TargetAddr) then Continue;
      if Msg.HaveSeenBy(LinkList[i].LinkInfo.Addr) then Continue;
      // Отправка сообщения на узел
      LinkList[i].SendMsg(Msg);
      Exit;
    end;

    // Отправка по правилам роутинга
    DnmpLink:=RoutingTable.LinkForDestAddr(Msg.TargetAddr);
    if Assigned(DnmpLink) then
    begin
      if (DnmpLink.Active) and (not Msg.HaveSeenBy(DnmpLink.LinkInfo.Addr)) then
      begin
        DnmpLink.SendMsg(Msg);
        Exit;
      end;
    end;
  end;

  // Если аплинк существует
  if Assigned(Uplink) then
  begin
    if (Uplink.Active) and (not Msg.HaveSeenBy(Uplink.LinkInfo.Addr)) then
    begin
      // Отправка на аплинк
      Uplink.SendMsg(Msg);
      Exit;
    end;
  end;

  // Отправка сообщения об ошибке
  //SendErrorMsg(Msg, '102', 'Destination address not found');
  DebugText('102 - Destination address not found');
  Result:=False;
  Exit;

end;

procedure TDnmpManager.SendDataMsg(DestAddr: TAddr; MsgType, Info, Text: string);
var
  NewMsg: TDnmpMsg;
begin
  NewMsg:=TDnmpMsg.Create(Self.MyInfo.Addr, DestAddr, MsgType, Info, Text);
  self.SendMsg(NewMsg);
  FreeAndNil(NewMsg);
end;

procedure TDnmpManager.SendErrorMsg(DestAddr: TAddr; ErrCode, ErrText: string);
begin
  SendDataMsg(DestAddr, 'ERRR', 'err_code='+ErrCode, ErrText);
end;

procedure TDnmpManager.SendChatMsg(DestAddr: TAddr; Text: string);
begin
  SendDataMsg(DestAddr, 'CHAT', '', Text);
end;

function TDnmpManager.CmdHandler(CmdText: string): string;
var
  sCmd, sParams: string;
  saParams: TStringArray;
  n, m: Integer;
  TraceID: Cardinal;
  Msg: TDnmpMsg;
  li: TLinkInfo;
begin
  Result:='';
  ExtractCmd(CmdText, sCmd, sParams);
  if sCmd='AUTH_OK' then
  begin
    // Кто-то успешно авторизировался
    Event('MGR','REFRESH');
  end

  else if sCmd='EVENT' then
  begin
    ExtractCmd(sParams, sCmd, sParams);
    Event(sCmd, sParams);
  end

  else if sCmd='ASK' then
  begin
    // Команда отправки запроса чего-то
  end

  else if sCmd='APPROVE' then
  begin
    // APPROVE <GUID>
    // Подтверждает авторизацию линка с указанным GUID
    if sParams='' then Exit;
    li:=GetInfoByGUID(sParams);
    if Assigned(li) then Approve(li);
  end

  else if sCmd='GET_INFO' then
  begin
    // GET_INFO <addr>
    // Отправляет на указанный адрес запрос на получение информации
    if sParams='' then Exit;
    RequestInfoByAddr(StrToAddr(sParams));
  end

  else if sCmd='GET_POINTLIST' then
  begin
    // GET_POINTLIST <addr>
    // Отправляет на указанный адрес запрос на получение списка поинтов
    if sParams='' then Exit;
    RequestPointlist(StrToAddr(sParams));
  end

  else if sCmd='ROUTE' then
  begin
    // ROUTE VIA <gate_id> <node_id> [node2_id] ..
    // Добавляет маршруты на указанные узлы через узел gate.
    // Если на узел уже есть маршрут, то он будет заменен
    //
    // ROUTE DEL <node_id>|ALL [node2_id] ...
    // Удаляет маршруты на указанные узлы
    //
    if sParams='' then Exit;
    saParams:=ParseStr(sParams);
    if saParams[0]='VIA' then
    begin
      TraceID:=DateTimeToFileDate(Now());
      for m:=2 to Length(saParams)-1 do
      begin
        RoutingTable.AddItem(StrToIntDef(saParams[1], 0), StrToIntDef(saParams[m], 0), TraceID);
      end;
    end

    else if saParams[0]='DEL' then
    begin
      for m:=1 to Length(saParams)-1 do
      begin
        if (m=1) and (UpperCase(saParams[m])='ALL') then RoutingTable.Clear()
        else RoutingTable.DelDest(StrToIntDef(saParams[m], 0));
      end;

    end;
    Event('MGR','UPDATE ROUTING');
  end;
end;

function TDnmpManager.GetInfoByAddr(Addr: TAddr): TLinkInfo;
begin
  Result:=nil;
  if Addr.Point=0 then Result:=NodeList.GetLinkInfoByAddr(Addr)
  else Result:=PointList.GetLinkInfoByAddr(Addr);

  if not Assigned(Result) then Result:=ContactList.GetLinkInfoByAddr(Addr);
  if not Assigned(Result) then
  begin
    if SameAddr(Addr, MyInfo.Addr) then Result:=MyInfo;
  end;
end;

function TDnmpManager.GetInfoByGUID(SomeGUID: string): TLinkInfo;
begin
  Result:=ContactList.GetLinkInfoByGUID(SomeGUID);
  if not Assigned(Result) then Result:=PointList.GetLinkInfoByGUID(SomeGUID);
  if not Assigned(Result) then Result:=NodeList.GetLinkInfoByGUID(SomeGUID);
end;

function TDnmpManager.Approve(ALinkInfo: TLinkInfo): boolean;
var
  NewGUID: TGUID;
begin
  Result:=false;
  if ServerMode then Exit;

  // Создаем новый GUID (???)
  CreateGUID(NewGUID);
  ALinkInfo.GUID:=GUIDToString(NewGUID);
  ALinkInfo.SeniorGUID:=MyInfo.GUID;

  if ALinkInfo.LinkType = ltPoint then
  begin
    // Выделяем новый номер поинта
    if PointList.IndexOf(ALinkInfo)>=0 then Exit;
    ALinkInfo.Addr.Node:=MyInfo.Addr.Node;
    ALinkInfo.Addr.Point:=PointList.GetFreePointID();
    PointList.Add(ALinkInfo);
  end
  else if ALinkInfo.LinkType = ltNode then
  begin
    // Выделяем новый номер узла (!!!)
    // { TODO : Нужна проверка незанятости номера узла в сегменте }
    if NodeList.IndexOf(ALinkInfo)>=0 then Exit;
    ALinkInfo.Addr.Node:=NodeList.GetFreeNodeID();
    { TODO : Нужно учитывать свой адрес в нодлисте }
    if ALinkInfo.Addr.Node=MyInfo.Addr.Node then Inc(ALinkInfo.Addr.Node);
    ALinkInfo.Addr.Point:=0;
    NodeList.Add(ALinkInfo);
    // Сообщаем другим узлам данные нового узла
    //Mgr.SendLinkInfo(LinkInfo, NewAddr());
  end;
  if ContactList.IndexOf(ALinkInfo)>=0 then ContactList.Extract(ALinkInfo);
  Result:=true;
  Event('MGR','APPROVE');
end;

procedure TDnmpManager.RequestInfoByAddr(Addr: TAddr);
var
  TmpAddr: TAddr;
begin
  if SameAddr(Addr, MyInfo.Addr) then
  begin
    Exit;
  end;

  TmpAddr.Node:=Addr.Node;
  TmpAddr.Point:=0;
  SendDataMsg(TmpAddr, 'GINF', 'addr='+AddrToStr(Addr), '');
end;

procedure TDnmpManager.RequestPointlist(Addr: TAddr);
var
  TmpAddr: TAddr;
begin
  TmpAddr.Node:=Addr.Node;
  TmpAddr.Point:=0;
  if SameAddr(TmpAddr, MyInfo.Addr) then
  begin
    Exit;
  end;

  SendDataMsg(TmpAddr, 'GINF', 'addr=points', '');
end;

procedure TDnmpManager.SendLinkInfo(ALinkInfo: TLinkInfo; TargetAddr: TAddr);
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, TargetAddr, 'LNKI','','');
  MsgOut.Info.Values['addr']:=ALinkInfo.AddrStr;
  MsgOut.Info.Values['guid']:=ALinkInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=ALinkInfo.SeniorGUID;
  MsgOut.Info.Values['name']:=ALinkInfo.Name;
  MsgOut.Info.Values['owner']:=ALinkInfo.Owner;
  MsgOut.Info.Values['location']:=ALinkInfo.Location;
  MsgOut.Info.Values['ip_addr']:=ALinkInfo.IpAddr;
  MsgOut.Info.Values['phone_no']:=ALinkInfo.PhoneNo;
  MsgOut.Info.Values['other_info']:=ALinkInfo.OtherInfo;
  MsgOut.Info.Values['rating']:=IntToStr(ALinkInfo.Rating);

  if (TargetAddr.Point=0) and (ALinkInfo.Addr.Point=0) then
  begin
    StrToStream(ALinkInfo.Key, MsgOut.Data);
  end;

  SendMsg(MsgOut);
  MsgOut.Free();
end;

procedure TDnmpManager.ReadLinkInfo(Msg: TDnmpMsg);
var
  i: integer;
  SomeAddr: TAddr;
  li: TLinkInfo;
  ReadAllInfo: Boolean;
  s, s2: string;
begin
  li:=nil;
  ReadAllInfo:=False;
  s:='';
  SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
  if SomeAddr.Point=0 then
  begin
    li:=NodeList.GetLinkInfoByAddr(SomeAddr);
    ReadAllInfo:=True;
    s:='NODELIST';
  end
  // Server only
  else if ServerMode and SameNode(SomeAddr, MyInfo.Addr) then
  begin
    li:=PointList.GetLinkInfoByAddr(SomeAddr);
    s:='POINTLIST';
    if not Assigned(li) then Exit; // ???
  end
  else
  begin
    li:=ContactList.GetLinkInfoByAddr(SomeAddr);
    s:='CONTACTS';
    ReadAllInfo:=True;
  end;

  if not Assigned(li) then
  begin
    li:=TLinkInfo.Create();
    if SomeAddr.Point=0 then
    begin
      NodeList.Add(li);
      s:='NODELIST';
    end
    else
    begin
      ContactList.Add(li);
      s:='CONTACTS';
    end;
    ReadAllInfo:=True;
  end;

  li.Name:=Msg.Info.Values['name'];
  li.Owner:=Msg.Info.Values['owner'];
  li.Location:=Msg.Info.Values['location'];
  li.IpAddr:=Msg.Info.Values['ip_addr'];
  li.PhoneNo:=Msg.Info.Values['phone_no'];
  li.OtherInfo:=Msg.Info.Values['other_info'];

  if ReadAllInfo then
  begin
    li.Addr:=SomeAddr;
    li.GUID:=Msg.Info.Values['guid'];
    li.SeniorGUID:=Msg.Info.Values['senior_guid'];
    li.Rating:=StrToIntDef(Msg.Info.Values['rating'], 0);
    s2:=StreamToStr(Msg.Data);
    if s2<>'' then li.Key:=s2;
  end;

  AddCmd('EVENT MGR UPDATE '+s);
end;


end.
