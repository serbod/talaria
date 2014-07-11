unit dnmp_services;

interface
uses SysUtils, Classes, Contnrs, StrUtils, dnmp_unit
  {, uLkJSON}, fpjson, jsonparser;

type
  TDnmpAbonentState = (asUnknown, asOnline, asOffline, asBusy);
  { TDnmpAbonent }
  // Service subscriber
  TDnmpAbonent = class(TInterfacedObject)
  public
    GUID: string;
    State: TDnmpAbonentState;  // asUnknown, asOnline, asOffline, asBusy
    Nick: string;
    Addr: TAddr;
    Rights: string; //
    Status: string; // Status message
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): string;
    function LoadFromString(s: string): boolean;
    function StateStr(): string;
    function StateFromStr(s: string): boolean;
    procedure Assign(ab: TDnmpAbonent);
  end;

  { TDnmpAbonentList }

  TDnmpAbonentList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpAbonent;
    procedure SetItem(Index: Integer; Value: TDnmpAbonent);
  public
    ParentList: TDnmpAbonentList;
    property Items[Index: Integer]: TDnmpAbonent read GetItem write SetItem; default;
    function GetAbonentByGUID(sGUID: string): TDnmpAbonent;
    function AddAbonentByGUID(sGUID: string): TDnmpAbonent;
    function DelAbonentByGUID(sGUID: string): TDnmpAbonent;
    function UpdateAbonent(sGUID, sNick, sState, sRights, sStatus: string; Addr: TAddr): TDnmpAbonent; overload;
    function UpdateAbonent(ab: TDnmpAbonent): TDnmpAbonent; overload;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): string;
    function LoadFromString(s: string): boolean;
    { Содержит список активных подписчиков в формате CSV.
    Каждый элемент списка содержит сведения:
    [0] guid - GUID абонента
    [1] state - состояние (подключен или отключен)
    [2] nick - ник (имя на канале, не зависит от реального имени)
    [3] addr - адрес
    [4] rights - полномочия (набор полномочий)
    [5] status - статус (сообщение абонента) }
    function SaveToCSV(): string;
    { Загрузить абонентов из сериализованого списка в формате CSV.
    Состав сведений как в SaveToCSV() }
    function UpdateFromCSV(sData: string): boolean;
  end;

  { TDnmpServiceInfo }

  TDnmpServiceInfo = class(TObject)
  public
    ServiceType: string; // FourCC
    Name: string;        //
    ParentName: string;
    Descr: string;
    HostAddr: TAddr;
    Rating: Integer;
    Abonents: TDnmpAbonentList;
    Owners: TDnmpAbonentList;
    AbonentsCount: Integer;
    constructor Create();
    destructor Destroy(); override;
    //function AbonentsCount(): Integer;
    function Owner(): TDnmpAbonent;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): string;
    function LoadFromString(s: string): boolean;
  end;

  { TDnmpServiceInfoList }

  TDnmpServiceInfoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpServiceInfo;
    procedure SetItem(Index: Integer; Value: TDnmpServiceInfo);
  public
    property Items[Index: Integer]: TDnmpServiceInfo read GetItem write SetItem; default;
    function GetServiceByTypeName(sType, sName: string): TDnmpServiceInfo;
    function UpdateServiceInfo(sType, sName, sParent, sProvider, sRating, sAbonCount, sDescr: string): TDnmpServiceInfo;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): string;
    function LoadFromString(s: string): boolean;
  end;

  TDnmpServiceManager = class;
  TDnmpServiceEvent = procedure(ACmd: string; AObject: TObject) of object;

  TDnmpService = class(TObject)
  protected
    FEvent: TDnmpServiceEvent;
    FMgr: TDnmpManager;
    FServiceMgr: TDnmpServiceManager;
  public
    ServiceInfo: TDnmpServiceInfo;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); virtual;
    destructor Destroy; override;
    property Mgr: TDnmpManager read FMgr;
    { Получить абрнента по его GUID
      Ищет сперва в списке абонентов сервиса, затем в общем списке абонентов
      затем в списках линков и контактов
      Если не находит, то создает нового }
    function GetAbonentByGUID(sGUID: string): TDnmpAbonent; virtual;
    // Обработка команды (Thread-safe) с указанного адреса
    function ParseCmd(Text: string; Addr: TAddr): string; virtual;
    // Обработка сообщения
    function ParseMsg(AMsg: TDnmpMsg): string; virtual;
    // Обработчик события
    property OnEvent: TDnmpServiceEvent read FEvent write FEvent;
    function ToStorage(): TDnmpStorage; virtual;
    function FromStorage(Storage: TDnmpStorage): boolean; virtual;
    function SaveToString(): string; virtual;
    function LoadFromString(s: string): boolean; virtual;
  end;

  { TDnmpServiceList }

  TDnmpServiceList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpService;
    procedure SetItem(Index: Integer; Value: TDnmpService);
  public
    Mgr: TDnmpManager;
    property Items[Index: Integer]: TDnmpService read GetItem write SetItem; default;
    function GetService(sType, sName: string): TDnmpService;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function SaveToString(): string;
    function LoadFromString(s: string): boolean;
  end;

  { TDnmpServiceManager }

  TDnmpServiceManager = class(TDnmpMsgHandler)
  private
    FEvent: TDnmpServiceEvent;
    FServiceType: string;
    /// === Server mode
    /// Send service info to Addr
    function SendServiceInfo(SType, SName: string; Addr: TAddr): Boolean;
    /// Send services list to Addr
    function SendServList(Addr: TAddr; Filter: string = ''): Boolean;
    /// Add service to services list
    function AddService(AService: TDnmpService): string;
    /// === Client mode
    /// Read DATA message
    function ReadData(sDataType: string; Msg: TDnmpMsg): string;
    /// Read service info message
    function ReadServiceInfo(Msg: TDnmpMsg): Boolean;
    /// Read service list of specified type from DATA message
    function ReadServiceList(sDataList, sListType: string): Boolean;
  public
    // Supported services types
    ServiceTypes: TStringList;
    // Active services
    ServiceList: TDnmpServiceList;
    // Passive services
    ServiceInfoList: TDnmpServiceInfoList;
    // Known services
    RemoteServiceInfoList: TDnmpServiceInfoList;
    // All abonents
    AllAbonents: TDnmpAbonentList;
    // Default owner
    DefaultOwner: TDnmpAbonent;
    constructor Create(AMgr: TDnmpManager);
    destructor Destroy; override;
    { Service type. For example, 'SRVD' }
    property ServiceType: string read FServiceType;
    procedure SaveToFile();
    procedure LoadFromFile();
    { (Server) Find service by sType and sName, then modify it, sAction:
    add - add service info (if not found), set owner by GUID from sParams
    del - delete service and service info
    set_parent - set ParentName from sParams
    set_descr - set Descr from sParams
    set_owner - clear Owners, add owner by GUID from sParams
    add_owner - add owner by GUID from sParams
    del_owner - delete owner by GUID from sParams }
    function ModService(sType, sName, sAction, sParams: string): Boolean;
    { Create service with given ServiceInfo }
    function CreateService(ServiceInfo: TDnmpServiceInfo): TDnmpService;
    // Обработка команды от указанного адреса
    function Cmd(Text: string; Addr: TAddr): string; override;
    // Разбор сообщения и выполнение требуемых действий
    // Возвращает True если сообщение обработано и дальнейшая обработка не требуется
    function ParseMsg(AMsg: TDnmpMsg): boolean; override;
    property OnEvent: TDnmpServiceEvent read FEvent write FEvent;
  end;

var
  MyAbonent: TDnmpAbonent;
  sPrefixAbonent: string = '@abonent:';

const
  csSRVDInfoFileName = 'SRVD_info_list.json';
  csSRVDAbonFileName = 'SRVD_abon_list.json';
  csSRVDKnownFileName = 'SRVD_known_list.json';

function StorageToJson(AStorage: TDnmpStorage): string;
function StorageFromJson(AStorage: TDnmpStorage; s: string): boolean;


implementation
uses Misc, dnmp_grpc;

function StorageToJson(AStorage: TDnmpStorage): string;
var
  jj: TJSONData;

  function StorageAsJsonData(Storage: TDnmpStorage): TJSONData;
  var
    i: integer;
    TmpItem: TDnmpStorage;
  begin
    Result:=nil;
    if not Assigned(Storage) then Exit;

    if Storage.StorageType=stUnknown then Exit

    else if Storage.StorageType=stString then
    begin
      Result:=TJSONString.Create(Storage.Value);
    end

    else if Storage.StorageType=stInteger then
    begin
      Result:=TJSONIntegerNumber.Create(StrToIntDef(Storage.Value, 0));
    end

    else if Storage.StorageType=stNumber then
    begin
      if (Pos('.', Storage.Value)>0) or (Pos(',', Storage.Value)>0) then
      begin
        Result:=TJSONFloatNumber.Create(StrToFloatDef(Storage.Value, 0));
      end

      else // integer
      begin
        Result:=TJSONIntegerNumber.Create(StrToIntDef(Storage.Value, 0));
      end
    end

    else if Storage.StorageType=stList then
    begin
      Result:=TJSONArray.Create();
      for i:=0 to Storage.Count-1 do
      begin
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        (Result as TJSONArray).Add(StorageAsJsonData(TmpItem));
      end;
    end

    else if Storage.StorageType=stDictionary then
    begin
      Result:=TJSONObject.Create();
      for i:=0 to Storage.Count-1 do
      begin
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        (Result as TJSONObject).Add(Storage.GetObjectName(i), StorageAsJsonData(TmpItem));
      end;
    end;
  end;

begin
  Result:='';
  jj:=StorageAsJsonData(AStorage);
  if not Assigned(jj) then Exit;
  Result:=jj.AsJSON;
  jj.Free();
end;

function StorageFromJson(AStorage: TDnmpStorage; s: string): boolean;
var
  parser: TJSONParser;
  jj: TJSONData;

  function JsonDataToStorage(j: TJSONData; Storage: TDnmpStorage): boolean;
  var
    SubStorage: TDnmpStorage;
    i: integer;
  begin
    Result:=False;
    if not Assigned(j) then Exit;
    if not Assigned(Storage) then Exit;

    if j.JSONType=TJSONtype.jtString then
    begin
      Storage.StorageType:=stString;
      Storage.Value:=j.AsString;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtNumber then
    begin
      Storage.StorageType:=stNumber;
      Storage.Value:=j.AsString;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtArray then
    begin
      Storage.StorageType:=stDictionary;
      for i:=0 to j.Count-1 do
      begin
        SubStorage:=TDnmpStorage.Create(stUnknown);
        if JsonDataToStorage(j.Items[i], SubStorage) then Storage.Add(IntToStr(i), SubStorage)
        else SubStorage.Free();
      end;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtObject then
    begin
      Storage.StorageType:=stDictionary;
      for i:=0 to j.Count-1 do
      begin
        SubStorage:=TDnmpStorage.Create(stUnknown);
        if JsonDataToStorage(j.Items[i], SubStorage) then Storage.Add((j as TJSONObject).Names[i], SubStorage)
        else SubStorage.Free();
      end;
      Result:=True;
    end;
  end;

begin
  Result:=False;
  parser:=TJSONParser.Create(s);
  try
    jj:=parser.Parse();
  finally
    parser.Free();
  end;
  if not Assigned(jj) then Exit;

  try
    Result:=JsonDataToStorage(jj, AStorage);
  finally
    jj.Free();
  end;

end;

// === TDnmpAbonent ===
function TDnmpAbonent.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('guid', Self.GUID);
  Result.Add('state', Self.StateStr());
  Result.Add('nick', Self.Nick);
  Result.Add('addr', AddrToStr(Self.Addr));
  Result.Add('rights', Self.Rights);
  Result.Add('status', Self.Status);
end;

function TDnmpAbonent.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  Self.GUID:=Storage.GetString('guid');
  Self.StateFromStr(Storage.GetString('state'));
  Self.Nick:=Storage.getString('nick');
  Self.Addr:=StrToAddr(Storage.GetString('addr'));
  Self.Rights:=Storage.GetString('rights');
  Self.Status:=Storage.GetString('status');
  Result:=True;
end;

function TDnmpAbonent.SaveToString(): string;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpAbonent.LoadFromString(s: string): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  Result:=StorageFromJson(Storage, s);
  if Result then Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;

function TDnmpAbonent.StateStr: string;
begin
  Result:='Unknown';
  case State of
    asUnknown: Result:='Unknown';
    asOnline: Result:='Online';
    asOffline: Result:='Offline';
    asBusy: Result:='Busy';
  end;
end;

function TDnmpAbonent.StateFromStr(s: string): boolean;
begin
  Result:=True;
  if s='Unknown' then State:=asUnknown;
  if s='Online' then State:=asOnline;
  if s='Offline' then State:=asOffline;
  if s='Busy' then State:=asBusy;
end;

procedure TDnmpAbonent.Assign(ab: TDnmpAbonent);
begin
  if not Assigned(ab) then Exit;
  Self.GUID:=ab.GUID;
  Self.State:=ab.State;
  Self.Nick:=ab.Nick;
  Self.Addr:=ab.Addr;
  Self.Rights:=ab.Rights;
  Self.Status:=ab.Status;
end;

// === TDnmpAbonentList ===
function TDnmpAbonentList.GetItem(Index: Integer): TDnmpAbonent;
begin
  Result:=TDnmpAbonent(inherited Items[index]);
end;

procedure TDnmpAbonentList.SetItem(Index: Integer; Value: TDnmpAbonent);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpAbonentList.GetAbonentByGUID(sGUID: string): TDnmpAbonent;
var
  i: Integer;
begin
  // Search only in this list
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if TDnmpAbonent(Items[i]).GUID=sGUID then
    begin
      Result:=TDnmpAbonent(Items[i]);
      Exit;
    end;
  end;
end;

function TDnmpAbonentList.AddAbonentByGUID(sGUID: string): TDnmpAbonent;
var
  i: Integer;
begin
  Result:=nil;
  if sGuid='' then Exit;
  // Search in this list and parent list
  Result:=GetAbonentByGUID(sGUID);
  if Assigned(Result) then Exit;

  if Assigned(ParentList) then Result:=ParentList.GetAbonentByGUID(sGUID);
  if not Assigned(Result) then
  begin
    Result:=TDnmpAbonent.Create();
    Result.GUID:=sGUID;
    if Assigned(ParentList) then ParentList.Add(Result);
  end;
  Self.Add(Result);
end;

function TDnmpAbonentList.DelAbonentByGUID(sGUID: string): TDnmpAbonent;
begin
  Result:=GetAbonentByGUID(sGUID);
  if Assigned(Result) then
  begin
    self.Extract(Result);
  end;
end;

function TDnmpAbonentList.UpdateAbonent(sGUID, sNick, sState, sRights, sStatus: string; Addr: TAddr): TDnmpAbonent;
begin
  Result:=GetAbonentByGUID(sGUID);
  if not Assigned(Result) then
  begin
    // Not found in this list, look in parent list
    if Assigned(ParentList) then Result:=ParentList.GetAbonentByGUID(sGUID);
    if Assigned(Result) then self.Add(Result);
  end;

  if not Assigned(Result) then
  begin
    // Abonent not found anywhere, create new
    Result:=TDnmpAbonent.Create();
    Result.GUID:=sGUID;
    self.Add(Result);
    if Assigned(ParentList) then ParentList.Add(Result);
  end;
  Result.StateFromStr(sState);
  Result.Nick:=sNick;
  Result.Addr:=Addr;
  Result.Rights:=sRights;
  Result.Status:=sStatus;
end;

function TDnmpAbonentList.UpdateAbonent(ab: TDnmpAbonent): TDnmpAbonent;
begin
  Result:=Self.GetAbonentByGUID(ab.GUID);
  if not Assigned(Result) then
  begin
    // Not found in this list, look in parent list
    if Assigned(ParentList) then Result:=ParentList.GetAbonentByGUID(ab.GUID);
    if Assigned(Result) then self.Add(Result);
  end;

  if not Assigned(Result) then
  begin
    // Abonent not found anywhere (fishy!)
    Result:=ab;
    self.Add(Result);
    if Assigned(ParentList) then ParentList.Add(Result);
    Exit;
  end;
  Result.State:=ab.State;
  Result.Nick:=ab.Nick;
  Result.Addr:=ab.Addr;
  Result.Rights:=ab.Rights;
  Result.Status:=ab.Status;
end;

function TDnmpAbonentList.ToStorage(): TDnmpStorage;
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
  Result.Add('type', 'DnmpAbonentList');
  Result.Add('items', Storage);
end;

function TDnmpAbonentList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Abonent: TDnmpAbonent;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpAbonentList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Abonent:=TDnmpAbonent.Create();
    if not Abonent.FromStorage(SubStorage.GetObject(i)) then
    begin
      Abonent.Free();
      Continue;
    end;
    self.Add(Abonent);
  end;
  Result:=True;
end;

function TDnmpAbonentList.SaveToString(): string;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpAbonentList.LoadFromString(s: string): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  Result:=StorageFromJson(Storage, s);
  if Result then Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;

function TDnmpAbonentList.SaveToCSV(): string;
var
  i: Integer;
  Abonent: TDnmpAbonent;
  sl, slData: TStringList;
begin
  Result:='';
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  for i:=0 to self.Count-1 do
  begin
    Abonent:=self[i];

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

  if slData.Count>0 then Result:=slData.Text;
  slData.Free();
end;

function TDnmpAbonentList.UpdateFromCSV(sData: string): boolean;
var
  sl, slData: TStringList;
  i: Integer;
begin
  Result:=False;
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  slData.Text:=sData;
  for i:=0 to slData.Count-1 do
  begin
    sl.Clear();
    sl.DelimitedText:=slData[i];
    if sl.Count<6 then Continue;
    self.UpdateAbonent(sl[0], sl[2], sl[1], sl[4], sl[5], StrToAddr(sl[3]));
    Result:=True;
  end;
  FreeAndNil(slData);
  FreeAndNil(sl);
end;

// === TDnmpServiceInfo ===
constructor TDnmpServiceInfo.Create();
begin
  inherited Create();
  Self.Abonents:=TDnmpAbonentList.Create(False);
  Self.Owners:=TDnmpAbonentList.Create(False);
end;

destructor TDnmpServiceInfo.Destroy();
begin
  FreeAndNil(Self.Abonents);
  FreeAndNil(Self.Owners);
  inherited Destroy();
end;

function TDnmpServiceInfo.Owner(): TDnmpAbonent;
begin
  Result:=nil;
  if Owners.Count>0 then Result:=Owners.GetItem(0);
end;

//function TDnmpServiceInfo.AbonentsCount(): Integer;
//begin
//  Result:=Abonents.Count;
//end;

function TDnmpServiceInfo.ToStorage(): TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  // Basic info
  Result.Add('type', self.ServiceType);
  Result.Add('name', self.Name);
  Result.Add('parent_name', self.ParentName);
  Result.Add('description', self.Descr);
  Result.Add('host_addr', AddrToStr(Self.HostAddr));
  Result.Add('rating', self.Rating);

  // Abonents list
  Result.Add('abonents', self.Abonents.ToStorage());
  // Owners list
  Result.Add('owners', Self.Owners.ToStorage());
end;

function TDnmpServiceInfo.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  // Basic info
  self.ServiceType:=Storage.GetString('type');
  self.Name:=Storage.GetString('name');
  self.ParentName:=Storage.GetString('parent_name');
  self.Descr:=Storage.GetString('description');
  Self.HostAddr:=StrToAddr(Storage.GetString('host_addr'));
  self.Rating:=Storage.GetInteger('rating');

  // Abonents list
  self.Abonents.FromStorage(Storage.GetObject('abonents'));
  // Owners list
  self.Owners.FromStorage(Storage.GetObject('owners'));

  Result:=True;
end;

function TDnmpServiceInfo.SaveToString(): string;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpServiceInfo.LoadFromString(s: string): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  Result:=StorageFromJson(Storage, s);
  if Result then Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;

// === TDnmpServiceInfoList ===
function TDnmpServiceInfoList.GetItem(Index: Integer): TDnmpServiceInfo;
begin
  Result:=TDnmpServiceInfo(inherited Items[index]);
end;

procedure TDnmpServiceInfoList.SetItem(Index: Integer; Value: TDnmpServiceInfo);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpServiceInfoList.GetServiceByTypeName(sType, sName: string): TDnmpServiceInfo;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    if TDnmpServiceInfo(Items[i]).ServiceType = sType then
    begin
      if TDnmpServiceInfo(Items[i]).Name = sName then
      begin
        Result:=TDnmpServiceInfo(Items[i]);
        Break;
      end;
    end;
  end;
end;

function TDnmpServiceInfoList.UpdateServiceInfo(sType, sName, sParent, sProvider, sRating, sAbonCount, sDescr: string): TDnmpServiceInfo;
var
  si: TDnmpServiceInfo;
begin
  si:=self.GetServiceByTypeName(sType, sName);
  if not Assigned(si) then
  begin
    si:=TDnmpServiceInfo.Create();
    self.Add(si);
    si.ServiceType:=sType;
    si.Name:=sName;
  end;
  if Length(sParent)>0 then si.ParentName:=sParent;
  if Length(sProvider)>0 then si.HostAddr:=StrToAddr(sProvider);
  if Length(sRating)>0 then si.Rating:=StrToIntDef(sRating, 0);
  if Length(sAbonCount)>0 then si.AbonentsCount:=StrToIntDef(sAbonCount, 0);
  if Length(sDescr)>0 then si.Descr:=sDescr;
  Result:=si;
end;

function TDnmpServiceInfoList.ToStorage: TDnmpStorage;
var
  SubStorage: TDnmpStorage;
  i: Integer;
begin
  SubStorage:=TDnmpStorage.Create(stDictionary);
  for i:=0 to Self.Count-1 do
  begin
    SubStorage.Add(IntToStr(i), Self.Items[i].ToStorage());
  end;

  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpServiceInfoList');
  Result.Add('items', SubStorage);
end;

function TDnmpServiceInfoList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpServiceInfo;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpServiceInfoList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=TDnmpServiceInfo.Create();
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    self.Add(Item);
  end;
  Result:=True;
end;

function TDnmpServiceInfoList.SaveToString(): string;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpServiceInfoList.LoadFromString(s: string): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  Result:=StorageFromJson(Storage, s);
  if Result then Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;


// === TDnmpService ===
constructor TDnmpService.Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  Self.FMgr:=AMgr;
  Self.FServiceMgr:=AServiceMgr;
  Self.ServiceInfo:=AServiceInfo;
  if Assigned(FMgr) and Assigned(ServiceInfo) then
  begin
    if Assigned(FServiceMgr) then
    begin
      ServiceInfo.Abonents.ParentList:=FServiceMgr.AllAbonents;
      ServiceInfo.Owners.ParentList:=ServiceInfo.Abonents.ParentList;
      if ServiceInfo.Abonents.IndexOf(FServiceMgr.DefaultOwner)=-1 then ServiceInfo.Abonents.Add(FServiceMgr.DefaultOwner);
    end;
  end;  
end;

destructor TDnmpService.Destroy();
begin
  inherited Destroy();
end;

// Обработка команды
function TDnmpService.ParseCmd(Text: string; Addr: TAddr): string;
begin
  Result:='';
end;

// Обработка сообщения
function TDnmpService.ParseMsg(AMsg: TDnmpMsg): string;
begin
  Result:='';
end;

function TDnmpService.GetAbonentByGUID(sGUID: string): TDnmpAbonent;
var
  li: TLinkInfo;
begin
  Result:=nil;
  if Assigned(Self.ServiceInfo) then
  begin
    Result:=Self.ServiceInfo.Abonents.GetAbonentByGUID(sGUID);
  end;

  if not Assigned(Result) then
  begin
    if Assigned(Mgr) and Assigned(FServiceMgr) then
    begin
      Result:=FServiceMgr.AllAbonents.GetAbonentByGUID(sGUID);
    end;
  end;

  if not Assigned(Result) then
  begin
    li:=Mgr.GetInfoByGUID(sGUID);
    if Assigned(li) and Assigned(Self.ServiceInfo) then
    begin
      Result:=Self.ServiceInfo.Abonents.UpdateAbonent(sGUID, li.Name, 'NONE', '', '', li.Addr);
    end;
  end;
end;


function TDnmpService.ToStorage(): TDnmpStorage;
begin
  Result:=Self.ServiceInfo.ToStorage();
end;

function TDnmpService.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=self.ServiceInfo.FromStorage(Storage);
end;

function TDnmpService.SaveToString(): string;
begin
  Result:=Self.ServiceInfo.SaveToString();
end;

function TDnmpService.LoadFromString(s: string): boolean;
begin
  Result:=Self.ServiceInfo.LoadFromString(s);
end;


// === TDnmpServiceList ===
function TDnmpServiceList.GetItem(Index: Integer): TDnmpService;
begin
  Result:=TDnmpService(inherited Items[index]);
end;

procedure TDnmpServiceList.SetItem(Index: Integer; Value: TDnmpService);
begin
  inherited Items[Index]:=Value;
end;

function TDnmpServiceList.GetService(sType, sName: string): TDnmpService;
var
  i: Integer;
  si: TDnmpServiceInfo;
begin
  Result:=nil;
  for i:=0 to Count-1 do
  begin
    si:=TDnmpService(Items[i]).ServiceInfo;
    if Assigned(si) then
    begin
      if si.ServiceType <> sType then Continue;
      if Trim(sName)<>'' then
      begin
        if si.Name <> sName then Continue;
      end;
      Result:=TDnmpService(Items[i]);
      Break;
    end;
  end;
end;

function TDnmpServiceList.ToStorage(): TDnmpStorage;
var
  SubStorage: TDnmpStorage;
  i: Integer;
begin
  SubStorage:=TDnmpStorage.Create(stDictionary);
  for i:=0 to Self.Count-1 do
  begin
    SubStorage.Add(IntToStr(i), Self.Items[i].ToStorage());
  end;

  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpServiceList');
  Result.Add('items', SubStorage);
end;

function TDnmpServiceList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpService;
begin
  Result:=False;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpServiceList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    { TODO : Сервис создается без менеджера - непорядок }
    Item:=TDnmpService.Create(nil, nil, nil); // <- !!
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    self.Add(Item);
  end;
  Result:=True;
end;

function TDnmpServiceList.SaveToString(): string;
var
  Storage: TDnmpStorage;
begin
  Storage:=self.ToStorage();
  Result:=StorageToJson(Storage);
  FreeAndNil(Storage);
end;

function TDnmpServiceList.LoadFromString(s: string): boolean;
var
  Storage: TDnmpStorage;
begin
  Result:=False;
  Storage:=TDnmpStorage.Create(stDictionary);
  Result:=StorageFromJson(Storage, s);
  if Result then Result:=self.FromStorage(Storage);
  FreeAndNil(Storage);
end;


// === TDnmpServiceManager ===
constructor TDnmpServiceManager.Create(AMgr: TDnmpManager);
begin
  inherited Create(AMgr);
  FServiceType:='SRVD';
  ServiceTypes:=TStringList.Create();
  ServiceTypes.Add(ServiceType);
  ServiceTypes.Add('GRPC');

  Self.AllAbonents:=TDnmpAbonentList.Create(True);
  self.ServiceInfoList:=TDnmpServiceInfoList.Create(True);
  self.RemoteServiceInfoList:=TDnmpServiceInfoList.Create(True);
  Self.ServiceList:=TDnmpServiceList.Create(True);

  self.DefaultOwner:=self.AllAbonents.GetAbonentByGUID(AMgr.MyInfo.GUID);
  if not Assigned(self.DefaultOwner) then
  begin
    // sGUID, sNick, sState, sRights, sStatus: string; Addr: TAddr
    self.DefaultOwner:=self.AllAbonents.UpdateAbonent(AMgr.MyInfo.GUID, AMgr.MyInfo.Name, '', '', '', AMgr.MyInfo.Addr);
  end;
end;

destructor TDnmpServiceManager.Destroy();
begin
  self.OnEvent:=nil;
  FreeAndNil(Self.ServiceList);
  FreeAndNil(Self.RemoteServiceInfoList);
  FreeAndNil(Self.ServiceInfoList);
  FreeAndNil(Self.AllAbonents);
  FreeAndNil(ServiceTypes);
  inherited Destroy();
end;

procedure TDnmpServiceManager.SaveToFile();
begin
  StrToFile(Self.Mgr.sDataPath+csSRVDAbonFileName, self.AllAbonents.SaveToString());
  StrToFile(Self.Mgr.sDataPath+csSRVDInfoFileName, self.ServiceInfoList.SaveToString());
  StrToFile(Self.Mgr.sDataPath+csSRVDKnownFileName, self.RemoteServiceInfoList.SaveToString());
end;

procedure TDnmpServiceManager.LoadFromFile();
begin
  self.AllAbonents.LoadFromString(FileToStr(Self.Mgr.sDataPath+csSRVDAbonFileName));
  self.ServiceInfoList.LoadFromString(FileToStr(Self.Mgr.sDataPath+csSRVDInfoFileName));
  self.RemoteServiceInfoList.LoadFromString(FileToStr(Self.Mgr.sDataPath+csSRVDKnownFileName));
end;

function TDnmpServiceManager.ParseMsg(AMsg: TDnmpMsg): boolean;
var
  sType, sName, s: string;
  tmpServiceInfo: TDnmpServiceInfo;
  tmpService: TDnmpService;
begin
  Result:=False;
  sType:=AMsg.MsgType;
  if sType = ServiceType then
  begin
    // Это команда?
    s:=Trim(AMsg.Info.Values['cmd']);
    if s <> '' then
    begin
      s:=s+' '+StreamToStr(AMsg.Data);
      Self.Cmd(s, AMsg.SourceAddr);
      Mgr.AddCmd('UPDATE SRVD');
      Exit;
    end;

    // Это данные?
    s:=Trim(AMsg.Info.Values['data']);
    if s <> '' then
    begin
      Self.ReadData(s, AMsg);
      Mgr.AddCmd('UPDATE SRVD');
      Exit;
    end;

    // Это описание сервиса?
    s:=Trim(AMsg.Info.Values['type']);
    if s <> '' then
    begin
      Self.ReadServiceInfo(AMsg);
      Exit;
    end;
  end;

  if ServiceTypes.IndexOf(sType) = -1 then
  begin
    // Maybe, it not service..
    Mgr.SendErrorMsg(AMsg.SourceAddr, ServiceType, 'Service type not supported: '+sType);
    Exit;
  end;

  sName:=AMsg.Info.Values['name'];
  tmpService:=ServiceList.GetService(sType, sName);

  // Try to create service
  if not Assigned(tmpService) then
  begin
    tmpServiceInfo:=ServiceInfoList.GetServiceByTypeName(sType, sName);
    if Assigned(tmpServiceInfo) then
    begin
      tmpService:=CreateService(tmpServiceInfo);
    end;
  end;

  if Assigned(tmpService) then
  begin
    tmpService.ParseMsg(AMsg);
  end
  else
  begin
    Mgr.SendErrorMsg(AMsg.SourceAddr, ServiceType, 'Service name not found: '+sName);
  end;
end;

function TDnmpServiceManager.AddService(AService: TDnmpService): string;
begin
  Result:='';
  if not Assigned(AService) then Exit;
  if ServiceTypes.IndexOf(AService.ServiceInfo.ServiceType) = -1 then
  begin
    ServiceTypes.Add(AService.ServiceInfo.ServiceType);
  end;
  if ServiceList.IndexOf(AService) = -1 then ServiceList.Add(AService);
  Result:='OK';
end;

function TDnmpServiceManager.CreateService(ServiceInfo: TDnmpServiceInfo): TDnmpService;
begin
  Result:=self.ServiceList.GetService(ServiceInfo.ServiceType, ServiceInfo.Name);
  if Assigned(Result) then Exit;
  if ServiceInfo.ServiceType='GRPC' then
  begin
    if Mgr.ServerMode then
      Result:=TDnmpGrpcServer.Create(Mgr, Self, ServiceInfo)
    else
      Result:=TDnmpGrpcClient.Create(Mgr, Self, ServiceInfo);
    (Result as TDnmpGrpc).Author:=self.DefaultOwner;
  end;
  if not Assigned(Result) then
  begin
    Result:=CreateService(ServiceInfo);
  end;
  if Assigned(Result) then Self.ServiceList.Add(Result);
end;

function TDnmpServiceManager.ModService(sType, sName, sAction, sParams: string): Boolean;
var
  item: TDnmpServiceInfo;
  srvc: TDnmpService;
  ab: TDnmpAbonent;
  li: TLinkInfo;
begin
  Result:=False;
  item:=self.ServiceInfoList.GetServiceByTypeName(sType, sName);
  if sAction='add' then
  begin
    if Assigned(item) then Exit;
    item:=TDnmpServiceInfo.Create();
    item.ServiceType:=sType;
    item.Name:=sName;
    if sParams='' then
    begin
      if Assigned(DefaultOwner) then item.Owners.AddAbonentByGUID(DefaultOwner.GUID);
    end;
    item.Owners.AddAbonentByGUID(sParams);
    item.HostAddr:=Mgr.MyInfo.Addr;
    self.ServiceInfoList.Add(item);
    //self.AddService(Self.CreateService(item));
  end

  else if sAction='del' then
  begin
    if not Assigned(item) then Exit;
    srvc:=ServiceList.GetService(sType, sName);
    ServiceList.Remove(srvc);
    Self.ServiceInfoList.Remove(item);
  end

  else if sAction='set_parent' then
  begin
    if not Assigned(item) then Exit;
    item.ParentName:=sParams;
  end

  else if sAction='set_descr' then
  begin
    if not Assigned(item) then Exit;
    item.Descr:=sParams;
  end

  else if sAction='set_owner' then
  begin
    if not Assigned(item) then Exit;
    item.Owners.Clear();
    item.Owners.AddAbonentByGUID(sParams);
  end

  else if sAction='add_owner' then
  begin
    if not Assigned(item) then Exit;
    if not Assigned(item.Owners.GetAbonentByGUID(sParams)) then
    begin
      item.Owners.AddAbonentByGUID(sParams);
    end;
  end

  else if sAction='del_owner' then
  begin
    if not Assigned(item) then Exit;
    ab:=item.Owners.GetAbonentByGUID(sParams);
    if Assigned(ab) then
    begin
      item.Owners.Remove(ab);
    end;
  end;
  Result:=True;
end;

function TDnmpServiceManager.SendServiceInfo(SType, SName: string; Addr: TAddr): Boolean;
var
  i, n: Integer;
  si: TDnmpServiceInfo;
  sl: TStringList;
begin
  Result:=False;
  if not Assigned(ServiceInfoList) then Exit;
  for i:=0 to ServiceInfoList.Count-1 do
  begin
    si:=ServiceInfoList[i];
    if (si.ServiceType <> SType) or not SameText(si.Name, SName) then Continue;

    // Send service info
    sl:=TStringList.Create();
    sl.Values['data']:='SERVICE_INFO';
    sl.Values['type']:=si.ServiceType;
    sl.Values['name']:=si.Name;
    sl.Values['parent']:=si.ParentName;
    sl.Values['abonent_count']:=IntToStr(si.Abonents.Count);
    sl.Values['provider']:=AddrToStr(si.HostAddr);
    sl.Values['rating']:=IntToStr(si.Rating);
    //sl.Values['provider']:=si.;
    for n:=0 to si.Owners.Count-1 do
    begin
      sl.Values['owner'+IntToStr(n)]:=si.Owners[n].GUID;
    end;
    Mgr.SendDataMsg(Addr, ServiceType, sl.Text, si.Descr);
    sl.Free();
    Result:=True;
    Break;
  end;
end;

function TDnmpServiceManager.SendServList(Addr: TAddr; Filter: string = ''): Boolean;
var
  i, n: Integer;
  sFilter: string;
  si: TDnmpServiceInfo;
  sl, slData: TStringList;
begin
  Result:=False;
  if not Assigned(ServiceInfoList) then Exit;
  sFilter:=Trim(Filter);
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  for i:=0 to ServiceInfoList.Count-1 do
  begin
    si:=ServiceInfoList[i];

    if Length(sFilter)>0 then
      if not AnsiContainsText(si.Name, Filter) then Continue;
    sl.Clear();
    // Compose service info
    sl.Add(si.ServiceType);
    sl.Add(si.Name);
    sl.Add(si.ParentName);
    sl.Add(IntToStr(si.Abonents.Count));
    sl.Add(AddrToStr(si.HostAddr));
    sl.Add(IntToStr(si.Rating));
    slData.Add(sl.DelimitedText);
  end;
  Result:=True;
  sl.Free();

  Mgr.SendDataMsg(Addr, ServiceType, 'data=SERVICES_LIST', slData.Text);
  slData.Free();
end;

function TDnmpServiceManager.Cmd(Text: string; Addr: TAddr): string;
var
  sCmd, s: string;
  Params: TStringArray;
  i, n: Integer;
  li: TLinkInfo;
begin
  Result:='';
  Params:=ParseStr(Text);
  n:=Length(Params);
  if n=0 then Exit;
  sCmd:=Params[0];
  if sCmd='' then Exit

  else if sCmd='GET_TYPES' then
  begin
    s:='';
    for i:=0 to Self.ServiceTypes.Count-1 do
    begin
      if s<>'' then s:=s+',';
      s:=s+ServiceTypes[i];
    end;
    Mgr.SendDataMsg(Addr, ServiceType, 'data=types', s);
  end

  else if sCmd='GET_LOCAL_LIST' then
  begin
    if Length(Params) < 2 then SendServList(Addr)
    else SendServList(Addr, Params[1]);
  end

  else if sCmd='GET_LIST' then
  begin
    if Length(Params) < 2 then SendServList(Addr)
    else SendServList(Addr, Params[1]);
  end

  else if sCmd='GET_INFO' then
  begin
    if Length(Params) < 3 then Exit;
    SendServiceInfo(Params[1], Params[2], Addr);
  end

  else if sCmd='ADD_SERVICE' then
  begin
    if Length(Params) < 3 then Exit;
    s:='';
    if Length(Params) = 3 then
    begin
      li:=Mgr.GetInfoByAddr(Addr);
      if Assigned(li) then s:=li.GUID;
    end
    else s:=Params[3];
    ModService(Params[1], Params[2], 'add', s);
    SendServiceInfo(Params[1], Params[2], Addr);
  end

  else if sCmd='SET_PARENT' then
  begin
    if Length(Params) < 3 then Exit;
    ModService(Params[1], Params[2], 'set_parent', Params[3]);
  end

  else if sCmd='SET_OWNER' then
  begin
    if Length(Params) < 3 then Exit;
    ModService(Params[1], Params[2], 'set_owner', Params[3]);
  end

  else if sCmd='ADD_OWNER' then
  begin
    if Length(Params) < 3 then Exit;
    ModService(Params[1], Params[2], 'add_owner', Params[3]);
  end

  else if sCmd='DEL_OWNER' then
  begin
    if Length(Params) < 3 then Exit;
    ModService(Params[1], Params[2], 'del_owner', Params[3]);
  end

  else if sCmd='SET_DESCR' then
  begin
    if Length(Params) < 3 then Exit;
    ModService(Params[1], Params[2], 'set_descr', Params[3]);
  end

  else if sCmd='DEL_SERVICE' then
  begin
    if Length(Params) < 2 then Exit;
    ModService(Params[1], Params[2], 'del', '');
  end

  else
  begin

  end;

end;

function TDnmpServiceManager.ReadData(sDataType: string; Msg: TDnmpMsg): string;
var
  sData: string;
begin
  Result:='';

  if sDataType='SERVICE_INFO' then
  begin
    ReadServiceInfo(Msg);
    Exit;
  end;

  sData:=StreamToStr(Msg.Data);

  if sDataType='SERVICES_LIST' then
  begin
    ReadServiceList(sData, sDataType);
  end

  else if sDataType='SERVICES_LIST_LOCAL' then
  begin
    ReadServiceList(sData, sDataType);
  end

  else if sDataType='SERVICES_LIST_SUBSCRIBED' then
  begin
    ReadServiceList(sData, sDataType);
  end

  else if sDataType='ABONENTS_LIST' then
  begin
    //ReadServiceList(sDataType, sData);
    //if Assigned(OnEvent) then OnEvent(sDataType, Self.UsersList);
  end;
end;

// Информация о сервисе
// Параметры:
// type - Тип сервиса
// name - Название сервиса (до 256 печатаемых символов)
// parent - Название сервиса-владельца (до 256 печатаемых символов)
// abonent_count - Число подписчиков
// provider - Адрес узла для подписки
// rating - Рейтинг сервиса
// ownerNN - Список GUID владельцев, где NN порядковый номер
// Данные:
// Описание сервиса (строка символов)
//
function TDnmpServiceManager.ReadServiceInfo(Msg: TDnmpMsg): Boolean;
var
  si: TDnmpServiceInfo;
  sName, sType, sParent, sProvider, sAbonCount, sRating, sDescr: string;
begin
  sType:=Msg.Info.Values['type'];
  sName:=Msg.Info.Values['name'];
  sParent:=Msg.Info.Values['parent'];
  sProvider:=Msg.Info.Values['provider'];
  sAbonCount:=Msg.Info.Values['abonent_count'];
  sRating:=Msg.Info.Values['rating'];
  sDescr:=StreamToStr(Msg.Data);

  si:=self.ServiceInfoList.GetServiceByTypeName(sType, sName);
  if si<>nil then
  begin
    ServiceInfoList.UpdateServiceInfo(sType, sName, sParent, sProvider, sRating, sAbonCount, sDescr);
  end
  else
  begin
    si:=RemoteServiceInfoList.UpdateServiceInfo(sType, sName, sParent, sProvider, sRating, sAbonCount, sDescr);
  end;

  if Assigned(OnEvent) then OnEvent('SERVICE_INFO', si);
  Result:=True;
end;

//
// SERVICES_LIST
// Содержит список сервисов в формате CSV.
// type - тип сервиса
// name - название сервиса
// parent - имя вышестоящего сервиса-родителя
// abonent_count - число абонентов
// provider - адрес узла для подписки
// rating - рейтинг сервиса
//
function TDnmpServiceManager.ReadServiceList(sDataList, sListType: string): Boolean;
var
  sl, slData: TStringList;
  i, n: Integer;
  Abonent: TDnmpAbonent;
  si: TDnmpServiceInfo;

procedure UpdateServiceInfoList(siList: TDnmpServiceInfoList; sl: TStrings);
begin
  siList.UpdateServiceInfo(sl[0], sl[1], sl[2], sl[4], sl[5], sl[3], '');
end;

begin
  Result:=False;
  sl:=TStringList.Create();
  slData:=TStringList.Create();
  slData.Text:=sDataList;
  for i:=0 to slData.Count-1 do
  begin
    sl.Clear();
    sl.DelimitedText:=slData[i];
    if sl.Count<6 then Continue;

    if sListType='SERVICES_LIST_LOCAL' then
    begin
      //Self.UsersList.UpdateAbonent(sl[0], sl[1], sl[2], sl[4], sl[5], StrToAddr(sl[3]));
      UpdateServiceInfoList(self.RemoteServiceInfoList, sl);
      if Assigned(OnEvent) then OnEvent(sListType, self.RemoteServiceInfoList);
      Result:=True;
    end

    else if sListType='SERVICES_LIST' then
    begin
      //Self.ServiceInfo.Abonents.UpdateAbonent(sl[0], sl[1], sl[2], sl[4], sl[5], StrToAddr(sl[3]));
      UpdateServiceInfoList(self.RemoteServiceInfoList, sl);
      if Assigned(OnEvent) then OnEvent(sListType, self.RemoteServiceInfoList);
      Result:=True;
    end

    else if sListType='SERVICES_LIST_SUBSCRIBED' then
    begin
      //Self.BanList.UpdateAbonent(sl[0], sl[1], sl[2], sl[4], sl[5], StrToAddr(sl[3]));
      UpdateServiceInfoList(self.ServiceInfoList, sl);
      if Assigned(OnEvent) then OnEvent(sListType, self.ServiceInfoList);
      Result:=True;
    end;
  end;
  FreeAndNil(slData);
  FreeAndNil(sl);
end;


end.
