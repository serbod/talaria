unit dnmp_auth;
{
DNMP AUTH - authorization service
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_unit;

type

  { TDnmpAuthService }

  TDnmpAuthService = class(TDnmpMsgHandler)
  private
    MyInfo: TDnmpContact;
    RemoteInfo: TDnmpContact;
    // some random text
    TestPhrase: AnsiString;

    //==== Авторизация ====
    //=== Phase 1 ===
    // Хозяин -> Гость
    // ИД, инфо о себе, кодовое слово (строка случайных символов)
    procedure SendAuthRequest();  // [S]
    procedure OnAuthRequest(Msg: TDnmpMsg); // [SC]

    //=== Phase 2 ===
    // Гость -> Хозяин
    // ИД, инфо о себе и кодовое слово, шифрованое своим ключом
    procedure SendAuthReply(sKey: string); // [SC]
    procedure OnAuthReply(Msg: TDnmpMsg);  // [S]

    //=== Phase 3 ===
    // Хозяин -> Гость
    // результат опознания
    // Если клиент неизвестен, то кодовое слово становится ключом клиента.
    procedure SendAuthResult(sResult: string); // [S]
    procedure OnAuthResult(Msg: TDnmpMsg); // [SC]

    // Выполняется при успешной авторизации входящего поинта
    procedure OnAuthOkPointIn(); // [S]
    // Выполняется при успешной авторизации входящего узла
    procedure OnAuthOkNodeIn(); // [S]
    // Выполняется при успешной авторизации исходящего линка от узла
    procedure OnAuthOkAsNode(); // [S]
    // Выполняется при успешной авторизации исходящего линка от поинта
    procedure OnAuthOkAsPoint(); // [C]

    //=====================================
    // Отправка сообщения по каналу линка
    function SendMsg(Msg: TDnmpMsg): boolean; // [SC]

  public
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil); override;
    // Запуск парсера
    function Start(): Boolean; override;
    // Разбор сообщения и выполнение требуемых действий
    function ParseMsg(Msg: TDnmpMsg): Boolean; override;
  end;

const
  csAuthResultKeyNotFound = 'KEY_NOT_FOUND';
  csAuthResultNotApproved = 'NOT_APPROVED';


implementation
uses RC4, dnmp_services;

{ TDnmpAuthService }

//=====================================
// Сервер -> Клинт
// ИД, инфо о себе, кодовое слово (строка случайных символов)
procedure TDnmpAuthService.SendAuthRequest();
var
  MsgOut: TDnmpMsg;
begin
  //MyInfo.Key:=GenerateKey();
  TestPhrase:=GenerateKey();
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, EmptyAddr(), 'AUTH', '', TestPhrase);
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

// Сервер -> Клинт
// ИД, инфо о себе, рандомный хеш (ключ опознания)
procedure TDnmpAuthService.OnAuthRequest(Msg: TDnmpMsg);
var
  sNewCypher: AnsiString;
begin
  // Сервер послал нам инфу о себе и свое кодовое слово
  // Нужно отдать инфу о себе и кодовое слово, шифрованное нашим ключом
  TestPhrase:=StreamToStr(Msg.Data);
  // Если нет своего ключа, то делаем своим ключом кодовое слово сервера
  if RemoteInfo.Key='' then RemoteInfo.Key:=TestPhrase;

  RemoteInfo.Addr:=Msg.SourceAddr;
  RemoteInfo.Name:=Msg.Info.Values['name'];
  RemoteInfo.Owner:=Msg.Info.Values['owner'];
  RemoteInfo.Location:=Msg.Info.Values['location'];
  RemoteInfo.GUID:=Msg.Info.Values['guid'];
  RemoteInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  RemoteInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  RemoteInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  RemoteInfo.OtherInfo:=Msg.Info.Values['other_info'];

  sNewCypher:=RC4.RC4EncryptText(TestPhrase, RemoteInfo.Key);
  SendAuthReply(sNewCypher);
end;

//=====================================
// Клиент -> Сервер
// ИД, инфо о себе и кодовое слово, шифрованое своим ключом
procedure TDnmpAuthService.SendAuthReply(sKey: string);
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, RemoteInfo.Addr, 'AUTH', '', sKey);
  MsgOut.Info.Values['cmd']:='ARPL';
  MsgOut.Info.Values['addr']:=AddrToStr(MyInfo.Addr);
  MsgOut.Info.Values['name']:=MyInfo.Name;
  MsgOut.Info.Values['owner']:=MyInfo.Owner;
  MsgOut.Info.Values['location']:=MyInfo.Location;
  MsgOut.Info.Values['guid']:=MyInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=MyInfo.SeniorGUID;
  MsgOut.Info.Values['ip_addr']:=MyInfo.IpAddr;
  MsgOut.Info.Values['phone_no']:=MyInfo.PhoneNo;
  MsgOut.Info.Values['other_info']:=MyInfo.OtherInfo;
  if Mgr.ServerMode then MsgOut.Info.Values['type']:='node';
  if RemoteInfo.Info['uplink_password']<>'' then MsgOut.Info.Values['password']:=RemoteInfo.Info['uplink_password'];
  //MsgOut.Info.Values['key']:=sKey;
  //StrToStream(sKey, MsgOut.Data);

  Self.SendMsg(MsgOut);
  MsgOut.Free();
end;

// Клиент послал нам инфу о себе и кодовое слово, шифрованное ключом клиента
// Нужно ключами из списка известных клиентов попытаться расшифровать
// кодовое слово и сравнить результат с отправленным ранее
procedure TDnmpAuthService.OnAuthReply(Msg: TDnmpMsg);
var
  sRemoteCypher: AnsiString;
  i: Integer;
  Approved: Boolean;
  FoundInfo: TDnmpContact;
  sPassword, sAuthResult: string;

// Шифруем PlainText и сравниваем с CypherText
function CheckKey(Key, CypherText, PlainText: AnsiString): boolean;
var
  CypherTextNew: AnsiString;
begin
  Result:=False;
  if Length(Key)=0 then Exit;
  CypherTextNew:=RC4.RC4EncryptText(PlainText, Key);
  if CypherTextNew = CypherText then Result:=True;
end;

begin
  sRemoteCypher:=StreamToStr(Msg.Data);

  RemoteInfo.Addr:=StrToAddr(Msg.Info.Values['addr']);
  RemoteInfo.Name:=Msg.Info.Values['name'];
  RemoteInfo.Owner:=Msg.Info.Values['owner'];
  RemoteInfo.Location:=Msg.Info.Values['location'];
  RemoteInfo.GUID:=Msg.Info.Values['guid'];
  RemoteInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  RemoteInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  RemoteInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  RemoteInfo.OtherInfo:=Msg.Info.Values['other_info'];
  if Msg.Info.Values['type']='node' then self.RemoteInfo.Info['addr_type']:='node';

  FoundInfo:=nil;  // ссылка на ранее известный нам контакт
  Approved:=False; // признак авторизации контакта
  sAuthResult:=csAuthResultNotApproved;

  // Ищем по GUID среди известных контактов
  FoundInfo:=Mgr.ContactList.GetByGUID(RemoteInfo.GUID);
  if Assigned(FoundInfo) then
  begin
    if FoundInfo.Key='' then
    begin
      // У нас не было ключа, поэтому делаем ключом тестовую фразу и сообщаем об
      // этом удаленной стороне
      FoundInfo.Key:=TestPhrase;
      sAuthResult:=csAuthResultKeyNotFound;
    end
    else
    begin
      // сверим ключи
      Approved:=CheckKey(FoundInfo.Key, sRemoteCypher, TestPhrase);;
      if not Approved then
      begin
        // Ключи не сошлись.
        sAuthResult:='WRONG_KEY';
      end;
    end;
  end
  else
  begin
    // Среди известных не нашли, ищем в списке неавторизованных контактов
    FoundInfo:=Mgr.UnapprovedList.GetByGUID(RemoteInfo.GUID);
    //if Assigned(FoundInfo) then Approved:=CheckKey(FoundInfo.Key, sRemoteCypher, TestPhrase);
  end;

  { TODO : Проверка подлинности гостевого контакта с другого узла }
  // Если у гостя есть "прописка" на другом узле

  {
  // Поиск методом подбора ключа
  // Возможно, этот блок не нужен..
  if not Found then
  begin
    // Подбираем ключ из списка контактов
    for i:=0 to Mgr.ContactList.Count-1 do
    begin
      if Found then Break;
      FoundInfo:=Mgr.ContactList[i];
      Found:=CheckKey(FoundInfo.Key, sRemoteCypher, TestPhrase);
    end;
  end;
  }

  if not Assigned(FoundInfo) then
  begin
    FoundInfo:=RemoteInfo;
    FoundInfo.Key:=TestPhrase;
    //FoundInfo.GUID:=GenerateGUID();
    //FoundInfo.Addr:=EmptyAddr();
    // Сохраняем информацию линка в списке контактов
    Mgr.UnapprovedList.AddItem(FoundInfo);
    sAuthResult:=csAuthResultKeyNotFound;
  end;

  if FoundInfo <> RemoteInfo then
  begin
    // Сохраняем полученную от клиента второстепенную информацию (имя, владелец, статус, итд..)
    //FoundInfo.GUID:=RemoteInfo.GUID; // уже нашли по GUID
    FoundInfo.Name:=RemoteInfo.Name;
    FoundInfo.Owner:=RemoteInfo.Owner;
    FoundInfo.Location:=RemoteInfo.Location;
    FoundInfo.OtherInfo:=RemoteInfo.OtherInfo;
    // Делаем нашу копию инфы основной
    FreeAndNil(Link.RemoteInfo); // Free temporary link info
    Link.RemoteInfo:=FoundInfo;
    RemoteInfo:=FoundInfo;
  end;

  if (not Approved) and (Mgr.Conf.ReadBool('Main', 'AutoApprove', False)) then
  begin
    sPassword:=Mgr.Conf.ReadString('Main', 'AutoApprovePassword', '');
    if (sPassword='') or (sPassword=Msg.Info.Values['password']) then
    begin
      //RemoteInfo.Addr:=EmptyAddr();
      Approved:=Mgr.Approve(RemoteInfo);
    end;
  end;

  if Approved then
  begin
    SendAuthResult('OK');
    Mgr.Cmd('IN_AUTH OK '+AddrToStr(RemoteInfo.Addr)+' '+RemoteInfo.GUID);
    //
    if RemoteInfo.Addr.Point=0 then OnAuthOkNodeIn() else OnAuthOkPointIn();
  end
  else
  begin
    SendAuthResult(sAuthResult);
    Mgr.Cmd('IN_AUTH FAIL '+AddrToStr(RemoteInfo.Addr)+' '+RemoteInfo.GUID);
    Link.Disconnect();
  end;
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
end;

//=====================================
// Сервер -> Клиент
// результат опознания
procedure TDnmpAuthService.SendAuthResult(sResult: string);
var
  MsgOut: TDnmpMsg;
begin
  //sMsgBody:='AuthResult=OK';

  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, RemoteInfo.Addr, 'AUTH','','');
  MsgOut.Info.Values['cmd']:='ARSL';
  MsgOut.Info.Values['addr']:=AddrToStr(RemoteInfo.Addr);
  MsgOut.Info.Values['guid']:=RemoteInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=RemoteInfo.SeniorGUID;
  MsgOut.Info.Values['auth_result']:=sResult;

  Self.SendMsg(MsgOut);
  MsgOut.Free();
end;

// Сервер -> Клиент
// результат опознания
procedure TDnmpAuthService.OnAuthResult(Msg: TDnmpMsg);
var
  sResult: string;
begin
  sResult:=Msg.Info.Values['auth_result'];
  if sResult='OK' then
  begin
    // Опознание успешно
    if not SameAddr(MyInfo.Addr, Msg.TargetAddr) then
    begin
      // новый адрес
      if IsEmptyAddr(MyInfo.Addr) then
        // мой адрес был пустым
        MyInfo.Addr:=Msg.TargetAddr
      else if (Msg.TargetAddr.Point<>0) and (not MyInfo.IsNode) then
        // я поинт
        MyInfo.Addr:=Msg.TargetAddr;
    end;

    if MyInfo.SeniorGUID='' then MyInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];

    Mgr.Cmd('AUTH OK');
    if MyInfo.Addr.Point=0 then OnAuthOkAsNode() else OnAuthOkAsPoint();
  end
  else if sResult=csAuthResultKeyNotFound then
  begin
    // Сервер не нашел ключ
    // Сохраняем фразу сервера как свой ключ
    RemoteInfo.Key:=TestPhrase;
    Mgr.Cmd('AUTH FAIL');
  end
  else if sResult=csAuthResultNotApproved then
  begin
    // Линк не подтвержден
    Mgr.Cmd('AUTH FAIL');
  end
  else
  begin
    // Ошибка опознения
  end;
end;

procedure TDnmpAuthService.OnAuthOkPointIn();
begin

end;

{ При подключении узла Гость к узлу Хозяин:

1. Гость сообщает Хозяину список всех своих нижестоящих узлов.
2. Хозяин сообщает Гостю свой список узлов.
3. Хозяин отсылает своему аплинку эхо-запрос с информацией о Госте.
4. Гость отсылает своим даунлинкам эхо-запрос с информацией о Хозяине.
}
procedure TDnmpAuthService.OnAuthOkNodeIn();
begin

end;

procedure TDnmpAuthService.OnAuthOkAsNode();
begin
  // 1. Гость сообщает Хозяину список всех своих нижестоящих узлов.
  //Self.SendLocalNodeLinksList();
  // 4. Гость отсылает своим даунлинкам эхо-запрос с информацией о Хозяине.}
  //Self.SendNodeLinkInfo();
end;

procedure TDnmpAuthService.OnAuthOkAsPoint();
begin

end;

function TDnmpAuthService.SendMsg(Msg: TDnmpMsg): boolean;
begin
  Result:=False;
  if Assigned(Self.Link) then Result:=Self.Link.SendMsg(Msg);
end;

constructor TDnmpAuthService.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
  Self.MyInfo:=Link.MyInfo;
  RemoteInfo:=Link.RemoteInfo;
end;

function TDnmpAuthService.Start(): Boolean;
begin
  //Result:=inherited Start;
  //self.RemoteInfo.Addr:=EmptyAddr();
  if Link.LinkType=ltIncoming then self.SendAuthRequest();
  Result:=True;
end;

function TDnmpAuthService.ParseMsg(Msg: TDnmpMsg): Boolean;
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
    Result:=True;
    sCmd:=Msg.Info.Values['cmd'];

    if sCmd='AURQ' then // Authentication request
    begin
      if Link.LinkType=ltOutcoming then OnAuthRequest(Msg); // [SC]
    end
    else if sCmd='ARPL' then // Authentication reply
    begin
      if Link.LinkType=ltIncoming then OnAuthReply(Msg); // [S]
    end
    else if sCmd='ARSL' then // Authentication result
    begin
      if Link.LinkType=ltOutcoming then OnAuthResult(Msg); // [SC]
    end;
  end;

end;

end.

