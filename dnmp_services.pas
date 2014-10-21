unit dnmp_services;

interface
uses SysUtils, Classes, Contnrs, StrUtils, dnmp_unit
  {, uLkJSON}, fpjson, jsonparser;

type

  { TDnmpServiceInfo }

  TDnmpServiceInfo = class(TInterfacedObject)
  public
    ServiceType: string; // FourCC
    Name: string;        // service name
    ParentName: string;  // parent service name
    Descr: string;       // multi-line description
    ProviderAddr: TAddr; // service uplink
    Rating: Integer;     // service rating
    Owners: TDnmpContactList;  // owners list
    //Abonents: TDnmpAbonentList;
    Subscribers: TDnmpContactList; // downlinks
    AbonentsCount: Integer;    // abonents count (approximately)
    { ParentContactList needed for Owners and Subscribers }
    constructor Create(ParentContactList: TDnmpContactList);
    destructor Destroy(); override;
    //function AbonentsCount(): Integer;
    function Owner(): TDnmpContact; // first owner
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    procedure Assign(Item: TDnmpServiceInfo);
  end;

  { TDnmpServiceInfoList }

  TDnmpServiceInfoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TDnmpServiceInfo;
    procedure SetItem(Index: Integer; Value: TDnmpServiceInfo);
  public
    ParentContactList: TDnmpContactList;
    property Items[Index: Integer]: TDnmpServiceInfo read GetItem write SetItem; default;
    function GetServiceByTypeName(sType, sName: string): TDnmpServiceInfo;
    function UpdateServiceInfo(sType, sName, sParent, sProvider, sRating, sAbonCount, sDescr: string): TDnmpServiceInfo;
    function ToStorage(): TDnmpStorage;
    function ToStorageShort(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    function FromStorageShort(Storage: TDnmpStorage): boolean;
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
    property ServiceMgr: TDnmpServiceManager read FServiceMgr;
    { Получить абрнента по его GUID
      Ищет сперва в списке абонентов сервиса, затем в общем списке абонентов
      затем в списках линков и контактов
      Если не находит, то создает нового }
    function GetAbonentByGUID(sGUID: string): TDnmpContact; virtual;
    // Обработка команды (Thread-safe) с указанного адреса
    function ParseCmd(Text: string; Addr: TAddr): string; virtual;
    // Обработка сообщения
    function ParseMsg(AMsg: TDnmpMsg): string; virtual;
    // Обработчик события
    property OnEvent: TDnmpServiceEvent read FEvent write FEvent;
    // Отладочное сообщение
    procedure DebugText(s: string);
    function ToStorage(): TDnmpStorage; virtual;
    function FromStorage(Storage: TDnmpStorage): boolean; virtual;
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
    //AllAbonents: TDnmpContactList;
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil); override;
    destructor Destroy; override;
    { Service type. For example, 'SRVD' }
    property ServiceType: string read FServiceType;
    procedure SaveToFile();
    procedure LoadFromFile();
    function Start(): boolean; override;
    function Stop(): boolean; override;
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
    { Get service by type and name }
    function GetService(AServiceType, AServiceName: string; DoCreate: boolean = false): TDnmpService;
    { Обработка команды от указанного адреса
      GET_TYPES - send types list
      GET_LOCAL_LIST [filter] - send local services list
      GET_LIST [filter] - same as GET_LOCAL_LIST
      GET_INFO <type> <name> - send service info
      ADD_SERVICE <type> <name> [guid] - add new service and send service info
      SET_PARENT <type> <name> <parent_name> - set ParentName
      SET_OWNER <type> <name> <guid> - clear owners, then add owner guid
      ADD_OWNER <type> <name> <guid> - add owner guid
      DEL_OWNER <type> <name> <guid> - delete owner guid
      SET_DESCR <type> <name> [descr] - set description
      DEL_SERVICE <type> <name> - delete service
    }
    function Cmd(Text: string; Addr: TAddr): string; override;
    // Разбор сообщения и выполнение требуемых действий
    // Возвращает True если сообщение обработано и дальнейшая обработка не требуется
    // Try to create service !!!
    function ParseMsg(AMsg: TDnmpMsg): boolean; override;
    procedure RequestTypes();
    procedure RequestList(sType: string);
    procedure RequestInfo(sType, sName: string);
    // Default owner
    function DefaultOwner(): TDnmpContact;
    property OnEvent: TDnmpServiceEvent read FEvent write FEvent;
  end;

var
  //MyAbonent: TDnmpContact;
  sPrefixAbonent: string = '@abonent:';

const
  csGRPC = 'GRPC';
  csMAIL = 'MAIL';
  csCHAT = 'CHAT';
  csSRVDInfoFileName = 'SRVD_info_list';
  csSRVDAbonFileName = 'SRVD_abon_list';
  csSRVDKnownFileName = 'SRVD_known_list';


implementation
uses Misc, dnmp_grpc, dnmp_mail, dnmp_chat;


// === TDnmpServiceInfo ===
constructor TDnmpServiceInfo.Create(ParentContactList: TDnmpContactList);
begin
  inherited Create();
  //Self.Abonents:=TDnmpAbonentList.Create(False);
  Self.Subscribers:=TDnmpContactList.Create(ParentContactList);
  Self.Owners:=TDnmpContactList.Create(ParentContactList);
end;

destructor TDnmpServiceInfo.Destroy();
begin
  //FreeAndNil(Self.Abonents);
  FreeAndNil(Self.Owners);
  FreeAndNil(Self.Subscribers);
  inherited Destroy();
end;

function TDnmpServiceInfo.Owner(): TDnmpContact;
begin
  Result:=nil;
  if Owners.Count>0 then Result:=Owners.Items[0];
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
  Result.Add('provider_addr', AddrToStr(Self.ProviderAddr));
  Result.Add('rating', self.Rating);

  // Abonents list
  //Result.Add('abonents', self.Abonents.ToStorage());
  // Owners list
  Result.Add('owners', Self.Owners.ToStorage(ctBrief));
  // Subscribers
  Result.Add('subscribers', Self.Subscribers.ToStorage(ctBrief));
end;

function TDnmpServiceInfo.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  // Basic info
  self.ServiceType:=Storage.GetString('type');
  self.Name:=Storage.GetString('name');
  self.ParentName:=Storage.GetString('parent_name');
  self.Descr:=Storage.GetString('description');
  Self.ProviderAddr:=StrToAddr(Storage.GetString('provider_addr'));
  self.Rating:=Storage.GetInteger('rating');

  // Abonents list
  //self.Abonents.FromStorage(Storage.GetObject('abonents'));
  // Owners list
  self.Owners.FromStorage(Storage.GetObject('owners'));
  // Subscribers
  self.Subscribers.FromStorage(Storage.GetObject('subscribers'));
  Result:=True;
end;

procedure TDnmpServiceInfo.Assign(Item: TDnmpServiceInfo);
begin
  Self.ServiceType:=Item.ServiceType;
  Self.Name:=Item.Name;
  Self.Descr:=Item.Descr;
  Self.AbonentsCount:=Item.AbonentsCount;
  Self.ProviderAddr:=Item.ProviderAddr;
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
    si:=TDnmpServiceInfo.Create(Self.ParentContactList);
    self.Add(si);
    si.ServiceType:=sType;
    si.Name:=sName;
  end;
  if Length(sParent)>0 then si.ParentName:=sParent;
  if Length(sProvider)>0 then si.ProviderAddr:=StrToAddr(sProvider);
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

function TDnmpServiceInfoList.ToStorageShort(): TDnmpStorage;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  si: TDnmpServiceInfo;
begin
  Result:=TDnmpStorage.Create(stList);

  for i:=0 to Self.Count-1 do
  begin
    si:=Self.Items[i];
    SubStorage:=TDnmpStorage.Create(stDictionary);
    SubStorage.Add('type', si.ServiceType);
    SubStorage.Add('name', si.Name);
    SubStorage.Add('parent_name', si.ParentName);
    SubStorage.Add('description', si.Descr);
    SubStorage.Add('provider_addr', AddrToStr(si.ProviderAddr));
    SubStorage.Add('rating', si.Rating);
    Result.Add(IntToStr(i), SubStorage);
  end;
end;

function TDnmpServiceInfoList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpServiceInfo;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpServiceInfoList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    Item:=TDnmpServiceInfo.Create(Self.ParentContactList);
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    if not Assigned(self.GetServiceByTypeName(Item.ServiceType, Item.Name)) then self.Add(Item)
    else Item.Free();
  end;
  Result:=True;
end;

function TDnmpServiceInfoList.FromStorageShort(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item, Item2: TDnmpServiceInfo;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stList then Exit;
  for i:=0 to Storage.Count()-1 do
  begin
    SubStorage:=Storage.GetObject(i);
    if SubStorage.StorageType <> stDictionary then Continue;
    Item:=TDnmpServiceInfo.Create(Self.ParentContactList);
    // Basic info
    Item.ServiceType:=SubStorage.GetString('type');
    Item.Name:=SubStorage.GetString('name');
    Item.ParentName:=SubStorage.GetString('parent_name');
    Item.Descr:=SubStorage.GetString('description');
    Item.ProviderAddr:=StrToAddr(SubStorage.GetString('provider_addr'));
    Item.Rating:=SubStorage.GetInteger('rating');

    Item2:=self.GetServiceByTypeName(Item.ServiceType, Item.Name);
    if not Assigned(Item2) then
      self.Add(Item)
    else
    begin
      Item2.Assign(Item);
      Item.Free();
    end;
  end;
  Result:=True;
end;


// === TDnmpService ===
constructor TDnmpService.Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  Self.FMgr:=AMgr;
  Self.FServiceMgr:=AServiceMgr;
  Self.ServiceInfo:=AServiceInfo;
  {
  if Assigned(FMgr) and Assigned(ServiceInfo) then
  begin
    ServiceInfo.Owners.ParentList:=FMgr.ContactList;
  end;
  }
end;

destructor TDnmpService.Destroy();
begin
  OnEvent:=nil;
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

procedure TDnmpService.DebugText(s: string);
begin
  if Assigned(Mgr) and Assigned(ServiceInfo) then Mgr.DebugText(self.ServiceInfo.ServiceType +': '+s);
end;

function TDnmpService.GetAbonentByGUID(sGUID: string): TDnmpContact;
var
  li: TDnmpContact;
begin
  Result:=nil;
  if Assigned(Self.ServiceInfo) then
  begin
    //Result:=Self.ServiceInfo.Abonents.GetByGUID(sGUID);
  end;

  if not Assigned(Result) then
  begin
    if Assigned(Mgr) then
    begin
      Result:=Mgr.ContactList.GetByGUID(sGUID);
    end;
  end;

  if not Assigned(Result) then
  begin
    li:=Mgr.GetLinkInfoByGUID(sGUID);
    if Assigned(li) and Assigned(Self.ServiceInfo) then
    begin
      //Result:=Self.ServiceInfo.Abonents.UpdateAbonent(sGUID, li.Name, 'NONE', '', '', li.Addr);
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
  if not Assigned(Storage) then Exit;
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


// === TDnmpServiceManager ===
constructor TDnmpServiceManager.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
  FServiceType:='SRVD';
  ServiceTypes:=TStringList.Create();
  ServiceTypes.Add(ServiceType);
  ServiceTypes.Add(csGRPC);
  ServiceTypes.Add(csMAIL);
  ServiceTypes.Add(csCHAT);

  // local services
  self.ServiceInfoList:=TDnmpServiceInfoList.Create(True);
  self.ServiceInfoList.ParentContactList:=AMgr.ContactList;
  // remote services
  self.RemoteServiceInfoList:=TDnmpServiceInfoList.Create(True);
  self.RemoteServiceInfoList.ParentContactList:=AMgr.ContactList;

  Self.ServiceList:=TDnmpServiceList.Create(True);
end;

destructor TDnmpServiceManager.Destroy();
begin
  self.OnEvent:=nil;
  FreeAndNil(Self.ServiceList);
  FreeAndNil(Self.RemoteServiceInfoList);
  FreeAndNil(Self.ServiceInfoList);
  FreeAndNil(ServiceTypes);
  inherited Destroy();
end;

procedure TDnmpServiceManager.SaveToFile();
var
  i: integer;
  SomeService: TDnmpService;
  s: string;
begin
  if not Assigned(Mgr.Serializer) then Exit;
  Mgr.Serializer.StorageToFile(Self.ServiceInfoList.ToStorage(), Self.Mgr.sDataPath+csSRVDInfoFileName);
  Mgr.Serializer.StorageToFile(Self.RemoteServiceInfoList.ToStorage(), Self.Mgr.sDataPath+csSRVDKnownFileName);
  // save services
  for i:=0 to Self.ServiceList.Count-1 do
  begin
    SomeService:=Self.ServiceList.GetItem(i);
    s:=SomeService.ServiceInfo.ServiceType+'_'+SomeService.ServiceInfo.Name;
    Self.Mgr.DebugText('Save service '+s);
    Self.Mgr.Serializer.StorageToFile(SomeService.ToStorage(), Self.Mgr.sDataPath+s);
  end;
end;

procedure TDnmpServiceManager.LoadFromFile();
var
  Storage: TDnmpStorage;
begin
  if not Assigned(Mgr.Serializer) then Exit;

  Storage:=TDnmpStorage.Create(stUnknown);
  if Mgr.Serializer.StorageFromFile(Storage, Self.Mgr.sDataPath+csSRVDInfoFileName) then Self.ServiceInfoList.FromStorage(Storage);
  Storage.Free();

  Storage:=TDnmpStorage.Create(stUnknown);
  if Mgr.Serializer.StorageFromFile(Storage, Self.Mgr.sDataPath+csSRVDKnownFileName) then Self.RemoteServiceInfoList.FromStorage(Storage);
  Storage.Free();
end;

function TDnmpServiceManager.Start(): boolean;
var
  i: integer;
  si: TDnmpServiceInfo;
  SomeService: TDnmpService;
  TmpStorage: TDnmpStorage;
  s: string;
begin
  Result:=inherited Start();
  // default services
  // Mail
  si:=ServiceInfoList.UpdateServiceInfo(csMAIL, '', '', '', '', '', 'Mailer');
  if Assigned(si) then self.CreateService(si);

  // Chat
  si:=ServiceInfoList.UpdateServiceInfo(csCHAT, '', '', '', '', '', 'Private chat');
  if Assigned(si) then self.CreateService(si);

  // local services
  for i:=0 to Self.ServiceInfoList.Count-1 do
  begin
    si:=Self.ServiceInfoList.Items[i];
    Self.CreateService(si);
  end;

  // load services data
  for i:=0 to Self.ServiceList.Count-1 do
  begin
    SomeService:=Self.ServiceList.GetItem(i);
    s:=SomeService.ServiceInfo.ServiceType+'_'+SomeService.ServiceInfo.Name;
    TmpStorage:=TDnmpStorage.Create(stUnknown);
    Self.Mgr.DebugText('Load service '+s);
    if Self.Mgr.Serializer.StorageFromFile(TmpStorage, Self.Mgr.sDataPath+s) then
    begin
      SomeService.FromStorage(TmpStorage);
    end;
    TmpStorage.Free();
  end;

  Result:=True;
end;

function TDnmpServiceManager.Stop(): boolean;
begin
  Result:=inherited Stop;
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

  // Try to create service (!!!)
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

procedure TDnmpServiceManager.RequestTypes();
var
  addr: TAddr;
begin
  if Assigned(Mgr.Uplink) then
  begin
    addr:=Mgr.Uplink.RemoteInfo.Addr;
    Mgr.SendDataMsg(addr, self.ServiceType, 'cmd=GET_TYPES', '');
  end;
end;

procedure TDnmpServiceManager.RequestList(sType: string);
var
  addr: TAddr;
begin
  if Assigned(Mgr.Uplink) then
  begin
    addr:=Mgr.Uplink.RemoteInfo.Addr;
    Mgr.SendDataMsg(addr, self.ServiceType, 'cmd=GET_LIST '+sType, '');
  end;
end;

procedure TDnmpServiceManager.RequestInfo(sType, sName: string);
var
  addr: TAddr;
begin
  if Assigned(Mgr.Uplink) then
  begin
    addr:=Mgr.Uplink.RemoteInfo.Addr;
    Mgr.SendDataMsg(addr, self.ServiceType, 'cmd=GET_INFO '+sType+' '+sName, '');
  end;
end;

function TDnmpServiceManager.DefaultOwner(): TDnmpContact;
begin
  Result:=Mgr.MyInfo;
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

  if ServiceInfo.ServiceType=csGRPC then
  begin
    if Mgr.ServerMode then
      Result:=TDnmpGrpcServer.Create(Mgr, Self, ServiceInfo)
    else
      Result:=TDnmpGrpcClient.Create(Mgr, Self, ServiceInfo);
  end;

  if ServiceInfo.ServiceType=csMAIL then
  begin
    if Mgr.ServerMode then
      Result:=TDnmpMail.Create(Mgr, Self, ServiceInfo)
    else
      Result:=TDnmpMail.Create(Mgr, Self, ServiceInfo);
  end;

  if ServiceInfo.ServiceType=csChat then
  begin
    if Mgr.ServerMode then
      Result:=TDnmpChat.Create(Mgr, Self, ServiceInfo)
    else
      Result:=TDnmpChat.Create(Mgr, Self, ServiceInfo);
  end;

  if Assigned(Result) then
  begin
    Mgr.DebugText('Created service: '+Result.ServiceInfo.ServiceType+' '+Result.ServiceInfo.Name);
    Self.ServiceList.Add(Result);
  end;
end;

function TDnmpServiceManager.GetService(AServiceType, AServiceName: string;
  DoCreate: boolean): TDnmpService;
begin
  Result:=self.ServiceList.GetService(AServiceType, AServiceName);
  if not Assigned(Result) and DoCreate then
  begin
    ModService(AServiceType, AServiceName, 'add', '');
    Result:=Self.CreateService(Self.ServiceInfoList.GetServiceByTypeName(AServiceType, AServiceName));
  end;
end;

function TDnmpServiceManager.ModService(sType, sName, sAction, sParams: string): Boolean;
var
  item: TDnmpServiceInfo;
  srvc: TDnmpService;
  ab: TDnmpContact;
begin
  Result:=False;
  item:=self.ServiceInfoList.GetServiceByTypeName(sType, sName);
  if sAction='add' then
  begin
    if Assigned(item) then Exit;
    item:=TDnmpServiceInfo.Create(self.ServiceInfoList.ParentContactList);
    item.ServiceType:=sType;
    item.Name:=sName;
    if sParams='' then
    begin
      if Assigned(DefaultOwner) then item.Owners.AddByGUID(DefaultOwner.GUID);
    end;
    item.Owners.AddByGUID(sParams);
    item.ProviderAddr:=Mgr.MyInfo.Addr;
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
    item.Owners.AddByGUID(sParams);
  end

  else if sAction='add_owner' then
  begin
    if not Assigned(item) then Exit;
    if not Assigned(item.Owners.GetByGUID(sParams)) then
    begin
      item.Owners.AddByGUID(sParams);
    end;
  end

  else if sAction='del_owner' then
  begin
    if not Assigned(item) then Exit;
    ab:=item.Owners.GetByGUID(sParams);
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
    sl.Values['abonent_count']:=IntToStr(si.AbonentsCount);
    sl.Values['provider_addr']:=AddrToStr(si.ProviderAddr);
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
  i: Integer;
  sFilter, sData: string;
  si: TDnmpServiceInfo;
  //sl, slData: TStringList;
  TmpList: TDnmpServiceInfoList;
  Storage: TDnmpStorage;
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if not Assigned(ServiceInfoList) then Exit;
  sFilter:=Trim(Filter);

  // default serializer
  TmpList:=TDnmpServiceInfoList.Create(False);
  for i:=0 to ServiceInfoList.Count-1 do
  begin
    si:=ServiceInfoList[i];
    if Length(sFilter)>0 then
    begin
      //if not AnsiContainsText(si.Name, Filter) then Continue;
      if si.ServiceType<>Filter then Continue;
    end;
    TmpList.Add(si);
  end;
  Storage:=TmpList.ToStorageShort();
  if Assigned(Storage) then
  begin
    sData:=Mgr.Serializer.StorageToString(Storage);
    Storage.Free();
    Mgr.SendDataMsg(Addr, ServiceType, 'data=SERVICES_LIST'+CRLF+'serializer='+Mgr.Serializer.GetName(), sData);
  end;
  TmpList.Free();

end;

function TDnmpServiceManager.Cmd(Text: string; Addr: TAddr): string;
var
  sCmd, s: string;
  Params: TStringArray;
  i, n: Integer;
  li: TDnmpContact;
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
      li:=Mgr.GetLinkInfoByAddr(Addr);
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

function TDnmpServiceManager.ReadServiceList(sDataList, sListType: string): Boolean;
var
  i: Integer;
  TmpList: TDnmpServiceInfoList;
  TmpStorage: TDnmpStorage;

begin
  Result:=False;

  if sListType='SERVICES_LIST' then TmpList:=self.RemoteServiceInfoList
  else Exit;

  TmpStorage:=TDnmpStorage.Create(stUnknown);
  if Mgr.Serializer.StorageFromString(TmpStorage, sDataList) then
  begin
    if TmpList.FromStorageShort(TmpStorage) then
    begin
      if Assigned(OnEvent) then OnEvent(sListType, TmpList);
      Result:=True;
    end;
  end;
  TmpStorage.Free();
end;


end.
