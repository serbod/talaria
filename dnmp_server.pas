// Server unit - server part
// парсер служебных сообщений для сервера
// обеспечивает авторизацию, обмен узлами
unit dnmp_server;

interface
uses SysUtils, Classes, dnmp_unit;

type

  { TDnmpParserServer }

  TDnmpParserServer = class(TDnmpMsgHandler)
  private
    MyInfo: TLinkInfo;
    LinkInfo: TLinkInfo;
    TempKey: string;

    //=====================================
    // Отправка сообщения получателю по правилам роутинга
    // Если получатель - сам узел, то разбор сообщения.
    //procedure RouteMessage(Msg: TDnmpMsg);  // [S]

    //=== Авторизация ===
    //=====================================
    // Хозяин -> Гость
    // ИД, инфо о себе, кодовое слово (строка случайных символов)
    procedure SendAuthRequest();  // [S]
    procedure OnAuthRequest(Msg: TDnmpMsg); // [SC]

    // Гость -> Хозяин
    // ИД, инфо о себе и кодовое слово, шифрованое своим ключом
    procedure SendAuthReply(sKey: string); // [SC]
    procedure OnAuthReply(Msg: TDnmpMsg);  // [S]

    // Хозяин -> Гость
    // результат опознания
    // Если клиент неизвестен, то кодовое слово становится ключом клиента.
    procedure SendAuthResult(sResult: string); // [S]
    procedure OnAuthResult(Msg: TDnmpMsg); // [SC]

    // Выполняется при успешной авторизации входящего линка
    procedure OnAuthOkPointIn(); // [SC]
    procedure OnAuthOkNodeIn(); // [SC]
    // Выполняется при успешной авторизации исходящего линка
    procedure OnAuthOkNodeOut(); // [S]

    //=====================================
    // Сервер -> Сервер
    // Запрос списка узлов
    procedure SendNodelistRequest(); // [SС]
    procedure OnNodelistRequest(Msg: TDnmpMsg); // [S]
    // Запрос информации
    procedure OnGetInfoRequest(Msg: TDnmpMsg); // [SC]
    // Сервер -> Сервер
    // Отправка информации об узле-линке
    procedure SendNodeLinkInfo(); // [S]
    procedure OnNodeLinkInfo(Msg: TDnmpMsg); // [S]
    procedure SendLocalNodeLinksList(); // [S]
    procedure OnNodeLinksList(Msg: TDnmpMsg); // [S]
    //procedure OnLinkInfo(Msg: TDnmpMsg); // [SC]

    //=====================================
    // Server -> Client
    // Сообщение об ошибке (!! NOT USED)
    procedure SendErrorMsg(OrigMsg: TDnmpMsg; ErrCode, ErrText: string); // [S]
    // Информационное сообщение
    procedure SendInfoMsg(OrigMsg: TDnmpMsg; InfoCode, InfoText: string); // [SC]


    //=====================================
    // Отправка сообщения по каналу линка
    function SendMsg(Msg: TDnmpMsg): boolean; // [SC]

  public
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil);
    // Запуск парсера
    function Start(): Boolean; override;
    // Разбор сообщения и выполнение требуемых действий
    function ParseMsg(Msg: TDnmpMsg): Boolean; override;
  end;

implementation
uses RC4, dnmp_services;


//=====================================
constructor TDnmpParserServer.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
  Self.MyInfo:=Link.MyInfo;
  LinkInfo:=Link.LinkInfo;
end;

//=====================================
function TDnmpParserServer.Start(): Boolean;
begin
  if self.MyInfo.Key='' then Self.MyInfo.Key:=GenerateKey();
  self.LinkInfo.Addr:=EmptyAddr();
  self.SendAuthRequest();
  Result:=True;
end;

//=====================================
// Отправка сообщения получателю по правилам роутинга
// Если получатель - сам узел, то разбор сообщения.
//procedure TDnmpParserServer.RouteMessage(Msg: TDnmpMsg);
//var
//  i: integer;
//  TargetPoint: TPointID;
//  DnmpLink: TDnmpLink;
//begin
//
//  // Сообщение другому поинту этого узла
//  if SameNode(Msg.TargetAddr, Mgr.MyInfo.Addr) then
//  begin
//    TargetPoint:=Msg.TargetAddr.Point;
//
//    // Сообщение самому узлу
//    if TargetPoint = Mgr.MyInfo.Addr.Point then
//    begin
//      //Mgr.IncomingMsg(Msg, Link);
//      Exit;
//    end;
//
//    // Поиск поинта назначения в списке линков
//    for i:=0 to Mgr.LinkList.Count-1 do
//    begin
//      DnmpLink:=Mgr.LinkList[i];
//      //if DnmpLink.LinkType <> ltPoint then Continue;
//      if not DnmpLink.LinkInfo.SameAddr(Msg.TargetAddr) then Continue;
//      // Отправка сообщения поинту
//      if DnmpLink.Active then
//      begin
//        DnmpLink.SendMsg(Msg);
//        Exit;
//      end;
//    end;
//
//    // Поиск поинта назначения в поинтлисте
//    for i:=0 to Mgr.PointList.Count-1 do
//    begin
//      if not Mgr.PointList[i].Addr.Point = TargetPoint then Continue;
//      // Помещаем сообщение в очередь отправки поинту
//      //DelayMsg(Msg);
//      SendInfoMsg(Msg, '201', 'Message delayed');
//      Exit;
//    end;
//
//    // Поинта назначения нет в поинтлисте, возвращаем ошибку
//    SendErrorMsg(Msg, '101', 'Destination point not found');
//    Exit;
//  end;
//
//  // Сообщение на другой узел
//  // Поиск среди узлов-линков
//  for i:=0 to Mgr.LinkList.Count-1 do
//  begin
//    DnmpLink:=Mgr.LinkList[i];
//    //if (Mgr.LinkList[i] as TDnmpLink).LinkType <> ltNode then Continue;
//    if not DnmpLink.LinkInfo.SameNode(Msg.TargetAddr) then Continue;
//    // Отправка сообщения на узел
//    if DnmpLink.Active then
//    begin
//      DnmpLink.SendMsg(Msg);
//      Exit;
//    end;
//  end;
//
//  // Поиск в таблице маршрутизации
//  DnmpLink:=Mgr.RoutingTable.LinkForDestAddr(Msg.TargetAddr);
//  if Assigned(DnmpLink) then
//  begin
//    DnmpLink.SendMsg(Msg);
//    Exit;
//  end;
//
//  // Если аплинк существует
//  if Assigned(Mgr.Uplink) then
//  begin
//    DnmpLink:=Mgr.Uplink;
//    // Отправка на аплинк
//    if DnmpLink.Active then
//    begin
//      DnmpLink.SendMsg(Msg);
//      Exit;
//    end;
//  end
//  else
//  begin
//    // Отправка сообщения об ошибке
//    SendErrorMsg(Msg, '102', 'Destination address not found');
//    Exit;
//  end;
//end;

function TDnmpParserServer.ParseMsg(Msg: TDnmpMsg): Boolean;
var
  MsgType: string;
  sCmd: string;
begin
  Result:=False;
  if not Assigned(Msg) then Exit;
  MsgType:=Msg.MsgType;

  if MsgType='' then
  begin
  end

  else if MsgType='AUTH' then // Built-in Authentication service
  begin
    sCmd:=Msg.Info.Values['cmd'];

    if sCmd='AURQ' then // Authentication request
    begin
      OnAuthRequest(Msg); // [SC]
    end
    else if sCmd='ARPL' then // Authentication reply
    begin
      OnAuthReply(Msg); // [S]
    end
    else if sCmd='ARSL' then // Authentication result
    begin
      OnAuthResult(Msg); // [SC]
    end;
  end

  else
  begin
    if SameAddr(Msg.TargetAddr, MyInfo.Addr) then
    begin
      // Msg for me
      if MsgType='INFO' then // Built-in Information service
      begin
        sCmd:=Msg.Info.Values['cmd'];

        if sCmd='NLRQ' then // Nodelist request
        begin
          OnNodelistRequest(Msg); // [S]
        end
        else if sCmd='LNKI' then // Link info
        begin
          Mgr.ReadLinkInfo(Msg); // [SC]
        end
        else if sCmd='NLST' then // Links list
        begin
          OnNodeLinksList(Msg); // [SC]
        end
        else if sCmd='GINF' then // Get info
        begin
          OnGetInfoRequest(Msg);
        end
        else if sCmd='ERRR' then // Error reply
        begin
          Mgr.DebugText('Error reply: '+AddrToStr(Msg.SourceAddr)+' '+Msg.Info.Values['err_code']+' '+StreamToStr(Msg.Data));
        end;
      end;
    end
    else
    begin
      // Msg not for me
      // Add link seen-by
      if LinkInfo.Addr.Point=0 then
      begin
        Msg.AddSeenBy(LinkInfo.Addr);
      end;
      Mgr.SendMsg(Msg);
    end;

  end;

  Result:=True;
end;


//=====================================
// Сервер -> Клинт
// ИД, инфо о себе, кодовое слово (строка случайных символов)
procedure TDnmpParserServer.SendAuthRequest();
var
  MsgOut: TDnmpMsg;
  sMsgBody: string;
begin
  //MyInfo.Key:=GenerateKey();
  TempKey:=GenerateKey();
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, EmptyAddr(), 'AUTH', '', TempKey);
  MsgOut.Info.Values['cmd']:='AURQ';
  MsgOut.Info.Values['guid']:=MyInfo.GUID;
  MsgOut.Info.Values['name']:=MyInfo.Name;
  MsgOut.Info.Values['owner']:=MyInfo.Owner;
  MsgOut.Info.Values['location']:=MyInfo.Location;
  MsgOut.Info.Values['ip_addr']:=MyInfo.IpAddr;
  MsgOut.Info.Values['phone_no']:=MyInfo.PhoneNo;
  MsgOut.Info.Values['other_info']:=MyInfo.OtherInfo;
  //StrToStream(MyInfo.Key, AuthMsg.Data);

  SendMsg(MsgOut);
  MsgOut.Free();
end;

// Сервер послал нам инфу о себе и свое кодовое слово
// Нужно отдать инфу о себе и кодовое слово, шифрованное нашим ключом
procedure TDnmpParserServer.OnAuthRequest(Msg: TDnmpMsg);
var
  RC4Data: TRC4Data;
  sRemoteKey, sLocalKey, sNewKey: AnsiString;
  li: TLinkInfo;
begin
  sRemoteKey:=StreamToStr(Msg.Data);
  // Если нет своего ключа, то делаем своим ключом кодовое слово сервера
  if self.MyInfo.Key='' then Self.MyInfo.Key:=sRemoteKey;
  sLocalKey:=self.MyInfo.Key;

  li:=Mgr.GetInfoByAddr(Msg.SourceAddr);
  if Assigned(li) then
  begin
    FreeAndNil(self.Link.LinkInfo);
    self.Link.LinkInfo:=li;
    self.LinkInfo:=li;
  end;
  LinkInfo.Addr:=Msg.SourceAddr;
  LinkInfo.Name:=Msg.Info.Values['name'];
  LinkInfo.Owner:=Msg.Info.Values['owner'];
  LinkInfo.Location:=Msg.Info.Values['location'];
  LinkInfo.GUID:=Msg.Info.Values['guid'];
  LinkInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  LinkInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  LinkInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  LinkInfo.OtherInfo:=Msg.Info.Values['other_info'];
  // Готовим место для шифра
  SetLength(sNewKey, Length(sRemoteKey));
  // Инициализируем наш ключ
  RC4.RC4Burn(RC4Data);
  RC4.RC4Init(RC4Data, sLocalKey);
  // Шифруем кодовое слово хозяина нашим ключом
  RC4.RC4Crypt(RC4Data, PChar(sRemoteKey), PChar(sNewKey), Length(sRemoteKey));

  SendAuthReply(sNewKey);
end;

//=====================================
// Клиент -> Сервер
// ИД, инфо о себе и кодовое слово, шифрованое своим ключом
procedure TDnmpParserServer.SendAuthReply(sKey: string);
var
  MsgOut: TDnmpMsg;
  sMsgBody: AnsiString;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'AUTH', '', sKey);
  MsgOut.Info.Values['cmd']:='ARPL';
  MsgOut.Info.Values['addr']:=MyInfo.AddrStr;
  MsgOut.Info.Values['name']:=MyInfo.Name;
  MsgOut.Info.Values['owner']:=MyInfo.Owner;
  MsgOut.Info.Values['location']:=MyInfo.Location;
  MsgOut.Info.Values['guid']:=MyInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=MyInfo.SeniorGUID;
  MsgOut.Info.Values['ip_addr']:=MyInfo.IpAddr;
  MsgOut.Info.Values['phone_no']:=MyInfo.PhoneNo;
  MsgOut.Info.Values['other_info']:=MyInfo.OtherInfo;
  MsgOut.Info.Values['type']:='node';
  //MsgOut.Info.Values['key']:=sKey;
  //StrToStream(sKey, MsgOut.Data);

  Self.SendMsg(MsgOut);
  MsgOut.Free();
end;

// Клиент послал нам инфу о себе и кодовое слово, шифрованное ключом клиента
// Нужно ключами из списка известных клиентов попытаться расшифровать
// кодовое слово и сравнить результат с отправленным ранее
procedure TDnmpParserServer.OnAuthReply(Msg: TDnmpMsg);
var
  RC4Data: TRC4Data;
  sRemoteKey: string;
  sLocalKey, sNewKey: string;
  i: Integer;
  Found: Boolean;
  li: TLinkInfo;

function CheckKey(Key, Secret1, Secret2: string): boolean;
var
  sKey, sSecret1, SecretNew: string;
begin
  Result:=False;
  if Length(Key)=0 then Exit;
  sKey:=Copy(Key, 1, maxint);
  sSecret1:=Copy(Secret1, 1, MaxInt);
  SetLength(SecretNew, Length(Secret1));
  RC4.RC4Burn(RC4Data);
  RC4.RC4Init(RC4Data, Key);
  RC4.RC4Crypt(RC4Data, PChar(Secret1), PChar(SecretNew), Length(Secret1));
  if SecretNew = Secret2 then Result:=True;
end;

begin
  sRemoteKey:=StreamToStr(Msg.Data);
  SetLength(sNewKey, Length(sRemoteKey));

  LinkInfo.Addr:=StrToAddr(Msg.Info.Values['addr']);
  LinkInfo.Name:=Msg.Info.Values['name'];
  LinkInfo.Owner:=Msg.Info.Values['owner'];
  LinkInfo.Location:=Msg.Info.Values['location'];
  LinkInfo.GUID:=Msg.Info.Values['guid'];
  LinkInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  LinkInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  LinkInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  LinkInfo.OtherInfo:=Msg.Info.Values['other_info'];
  if Msg.Info.Values['type']='node' then self.Link.LinkType:=ltNode;

  li:=nil;
  Found:=False;
  if LinkInfo.Addr.Node<>0 then
  begin
    // Ищем по адресу
    if LinkInfo.Addr.Node = MyInfo.Addr.Node then
    begin
      li:=Mgr.PointList.GetLinkInfoByAddr(LinkInfo.Addr);
      if Assigned(li) then Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end
    else
    begin
      li:=Mgr.NodeList.GetLinkInfoByAddr(LinkInfo.Addr);
      if Assigned(li) then Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;
  end
  else
  begin
    // Ищем в списке неавторизованных контактов
    li:=Mgr.ContactList.GetLinkInfoByGUID(LinkInfo.GUID);
    if Assigned(li) then Found:=CheckKey(li.Key, sRemoteKey, TempKey);
  end;

  // Возможно, этот блок не нужен..
  if not Found then
  begin
    // Подбираем ключ из списка контактов
    for i:=0 to Mgr.ContactList.Count-1 do
    begin
      if Found then Break;
      li:=Mgr.ContactList[i];
      Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;

    // Подбираем ключ из поинтлиста
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Found then Break;
      li:=Mgr.PointList[i];
      Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;

    // Подбираем ключ из нодлиста
    for i:=0 to Mgr.NodeList.Count-1 do
    begin
      if Found then Break;
      li:=Mgr.NodeList[i];
      Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;
  end;

  if (not Found) and (Mgr.Conf.ReadBool('Main', 'AutoApprove', False)) then
  begin
    LinkInfo.Key:=TempKey;
    LinkInfo.Addr:=EmptyAddr();
    Link.Approve();
    li:=LinkInfo;
    Found:=True;
  end;

  if Found then
  begin
    if li <> LinkInfo then
    begin
      // Сохраняем полученную от клиента второстепенную информацию (имя, владелец, статус, итд..)
      li.Name:=LinkInfo.Name;
      li.Owner:=LinkInfo.Owner;
      li.Location:=LinkInfo.Location;
      li.OtherInfo:=LinkInfo.OtherInfo;
      // Остальную информацию берем из поинтлиста
      FreeAndNil(Link.LinkInfo); // Free temporary link info
      Link.LinkInfo:=li;
      LinkInfo:=li;
    end;
    // ...
    SendAuthResult('OK');
    Mgr.Cmd('AUTH_OK '+li.AddrStr);
    //
    if LinkInfo.Addr.Point=0 then OnAuthOkNodeIn() else OnAuthOkPointIn();
  end
  else
  begin
    LinkInfo.Key:=TempKey;
    LinkInfo.Addr:=EmptyAddr();

    // Сохраняем информацию линка в списке контактов
    Mgr.ContactList.Add(LinkInfo);

    SendAuthResult('KEY_NOT_FOUND');
    Mgr.Cmd('AUTH_FAIL '+LinkInfo.GUID);
  end;
  Mgr.AddCmd('UPDATE LINKS');
end;

//=====================================
// Сервер -> Клиент
// результат опознания
procedure TDnmpParserServer.SendAuthResult(sResult: string);
var
  MsgOut: TDnmpMsg;
  sMsgBody: string;
begin
  //sMsgBody:='AuthResult=OK';

  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'AUTH','','');
  MsgOut.Info.Values['cmd']:='ARSL';
  MsgOut.Info.Values['addr']:=LinkInfo.AddrStr;
  MsgOut.Info.Values['guid']:=LinkInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=LinkInfo.SeniorGUID;
  MsgOut.Info.Values['auth_result']:=sResult;

  Self.SendMsg(MsgOut);
  MsgOut.Free();
end;

// Сервер -> Клиент
// результат опознания
// Если клиент известен:
// Хеширует ключ клиента ключом опознания, сравнивает результаты
// Если клиент неизвестен, то ключ опознания становится ключом клиента.
procedure TDnmpParserServer.OnAuthResult(Msg: TDnmpMsg);
var
  sResult: string;
begin
  sResult:=Msg.Info.Values['auth_result'];
  if sResult='OK' then
  begin
    // Опознание успешно
    if SameAddr(MyInfo.Addr, EmptyAddr()) then
    begin
      MyInfo.Addr:=Msg.TargetAddr;
      MyInfo.GUID:=Msg.Info.Values['guid'];
      MyInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
    end;

    OnAuthOkNodeOut();
  end
  else if sResult='KEY_NOT_FOUND' then
  begin
    // Линк не подтвержден
    // Сохраняем ключ сервера как свой
    //MyInfo.Key:=LinkInfo.Key;
  end
  else
  begin
    // Ошибка опознения
  end;
end;

// Выполняется при успешной авторизации входящего линка
procedure TDnmpParserServer.OnAuthOkPointIn();
begin
  // Отправка отложенных сообщений
  // SendQueuedMessages();

end;

{При подключении узла Гость к узлу Хозяин:

1. Гость сообщает Хозяину список всех своих нижестоящих узлов.
2. Хозяин сообщает Гостю свой список узлов.
3. Хозяин отсылает своему аплинку эхо-запрос с информацией о Госте.
4. Гость отсылает своим даунлинкам эхо-запрос с информацией о Хозяине.}
procedure TDnmpParserServer.OnAuthOkNodeIn();
begin
  //2. Хозяин сообщает Гостю свой список узлов.

  //3. Хозяин отсылает своему аплинку эхо-запрос с информацией о Госте.
  Self.SendNodeLinkInfo();
end;

// Выполняется при успешной авторизации исходящего линка
procedure TDnmpParserServer.OnAuthOkNodeOut();
begin
  // 1. Гость сообщает Хозяину список всех своих нижестоящих узлов.
  Self.SendLocalNodeLinksList();
  // 4. Гость отсылает своим даунлинкам эхо-запрос с информацией о Хозяине.}
  Self.SendNodeLinkInfo();
end;


//=====================================
// Клиент -> Сервер
// Запрос списка узлов
procedure TDnmpParserServer.SendNodelistRequest();
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'INFO', 'cmd=NLRQ', '');

  SendMsg(MsgOut);
  MsgOut.Free();
end;

// Клиент -> Сервер
// Запрос списка узлов
procedure TDnmpParserServer.OnNodelistRequest(Msg: TDnmpMsg);
var
  i: Integer;
begin
  if Msg.SourceAddr.Point=0 then
  begin
    // Отправка подробной информации об известных узлах
    for i:=0 to Mgr.NodeList.Count-1 do
    begin
      Mgr.SendLinkInfo(Mgr.NodeList[i], Msg.SourceAddr);
    end;
  end
  else
  begin
    // Отправка базовой информации об известных узлах
    for i:=0 to Mgr.NodeList.Count-1 do
    begin
      Mgr.SendLinkInfo(Mgr.NodeList[i], Msg.SourceAddr);
    end;
  end;
end;

procedure TDnmpParserServer.OnGetInfoRequest(Msg: TDnmpMsg);
var
  SomeAddr: TAddr;
  SomeName: string;
  i, n: Integer;
begin
  // Поиск по адресу
  if Length(Trim(Msg.Info.Values['addr']))>0 then
  begin
    SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
    if SomeAddr.Point=0 then
    begin
      // Отправляем инфо о себе
      Mgr.SendLinkInfo(MyInfo, Msg.SourceAddr);
      Exit;
    end;
    // Поиск информации об адресе
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Mgr.PointList[i].SameAddr(SomeAddr) then
      begin
        // Отправка информации об адресе
        Mgr.SendLinkInfo(Mgr.PointList[i], Msg.SourceAddr);
        Exit;
      end;
    end;
  end;

  // Поиск по имени
  SomeName:=AnsiLowerCase(Trim(Msg.Info.Values['name']));
  n:=0;
  if Length(SomeName)>0 then
  begin
    // Поиск в поинтлисте
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Pos(SomeName, AnsiLowerCase(Mgr.PointList[i].Name))>0 then
      begin
        // Отправка информации
        Mgr.SendLinkInfo(Mgr.PointList[i], Msg.SourceAddr);
        Inc(n);
      end;
    end;
    if n>0 then Exit;
  end;
  // Не нашли
  //SendErrorMsg(Msg, 'GINF_ERR', 'Point not found');
  Mgr.SendErrorMsg(Msg.SourceAddr, 'GINF_ERR', 'Point not found');
end;

// Сервер -> Сервер
// Отправка информации об узле-линке
//
// Сведения об узле:
// Тип: NINF
// Параметры:
// addr - адрес узла
// state - состояние узла по отношению к отправителю (on / off)
// rate - рейтинг узла
// avail - средний рейтинг доступности узла
// speed - средний рейтинг скорости узла
// Данные:
// нет
procedure TDnmpParserServer.SendNodeLinkInfo(); // [S]
var
  sl: TStringList;
  Msg: TDnmpMsg;
  i: Integer;
  s: string;
begin
  if Link.Active then s:='on' else s:='off';
  sl:=TStringList.Create();
  sl.Values['cmd']:='NINF';
  sl.Values['addr']:=LinkInfo.AddrStr();
  sl.Values['state']:=s;
  sl.Values['rate']:=IntToStr(LinkInfo.Rating);
  sl.Values['avail']:='';
  sl.Values['speed']:=IntToStr(Link.Speed);

  Msg:=TDnmpMsg.Create(MyInfo.Addr, EmptyAddr(), 'INFO', sl.Text, '');
  FreeAndNil(sl);

  if Mgr.Uplink = Link then
  begin
    // Если это аплинк, то сообщаем его данные всем node-линкам
    for i:=0 to Mgr.LinkList.Count-1 do
    begin
      if (Mgr.LinkList[i].Active) and (Mgr.LinkList[i].LinkType=ltNode) then Mgr.LinkList[i].SendMsg(Msg);
    end;
  end
  else
  begin
    // Если это линк, то сообщаем его данные аплинку
    if Assigned(Mgr.Uplink) then Mgr.Uplink.SendMsg(Msg);
  end;
  FreeAndNil(Msg);
end;

procedure TDnmpParserServer.OnNodeLinkInfo(Msg: TDnmpMsg);
var
  SomeAddr: TAddr;
begin
  if Msg.Info.Values['state']='off' then
  begin
    SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
    if not SameAddr(SomeAddr, EmptyAddr()) then Mgr.RoutingTable.DelDest(SomeAddr.Node);
  end;
end;

procedure TDnmpParserServer.SendLocalNodeLinksList();
var
  sl: TStringList;
  Msg: TDnmpMsg;
  i: Integer;
  s, s1: string;
  l: TDnmpLink;
begin
  Msg:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'INFO', 'cmd=NLST', '');
  sl:=TStringList.Create();
  for i:=0 to Mgr.LinkList.Count-1 do
  begin
    l:=Mgr.LinkList[i];
    if l.LinkType <> ltNode then Continue;
    if l.Active then s1:='on' else s1:='off';
    s:=l.LinkInfo.AddrStr; // addr
    s:=s+','+s1; // state
    s:=s+','+IntToStr(l.LinkInfo.Rating); // rate
    s:=s+','; // avail
    s:=s+','+IntToStr(l.Speed); // speed
    sl.Add(s);
  end;
  Msg.Info.Values['count']:=IntToStr(sl.Count);
  if sl.Count > 0 then
  begin
    StrToStream(sl.Text, Msg.Data);
  end;
  Self.SendMsg(Msg);

  FreeAndNil(sl);
  FreeAndNil(Msg);
end;

procedure TDnmpParserServer.OnNodeLinksList(Msg: TDnmpMsg);
var
  sl: TStringList;
begin

  //Mgr.RoutingTable.AddItem();
end;

//=====================================
// Server -> Client
// Сообщение об ошибке
procedure TDnmpParserServer.SendErrorMsg(OrigMsg: TDnmpMsg; ErrCode, ErrText: string);
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'INFO','','');
  MsgOut.Info.Values['cmd']:='ERRR';
  MsgOut.Info.Values['code']:=ErrCode;
  MsgOut.Info.Values['text']:=ErrText;
  if Assigned(OrigMsg) then
  begin
    MsgOut.TargetAddr:=OrigMsg.SourceAddr;
    MsgOut.Info.Values['source_timestamp']:=TimestampToStr(OrigMsg.TimeStamp);
    OrigMsg.ToStream(MsgOut.Data);
  end;

  SendMsg(MsgOut);
  MsgOut.Free();

end;

procedure TDnmpParserServer.SendInfoMsg(OrigMsg: TDnmpMsg; InfoCode, InfoText: string);
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'INFO','','');
  MsgOut.Info.Values['cmd']:='INFO';
  MsgOut.Info.Values['code']:=InfoCode;
  MsgOut.Info.Values['text']:=InfoText;
  if Assigned(OrigMsg) then
  begin
    MsgOut.TargetAddr:=OrigMsg.SourceAddr;
    MsgOut.Info.Values['source_timestamp']:=TimestampToStr(OrigMsg.TimeStamp);
  end;

  SendMsg(MsgOut);
  MsgOut.Free();
end;

function TDnmpParserServer.SendMsg(Msg: TDnmpMsg): boolean;
begin
  Result:=self.Link.SendMsg(Msg);
end;

end.

