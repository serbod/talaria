unit dnmp_client;

interface
uses SysUtils, Classes, dnmp_unit;

type

  { TDnmpParserClient }

  TDnmpParserClient = class(TDnmpMsgHandler)
  private
    MyInfo: TDnmpContact;
    RemoteInfo: TDnmpContact;
    // some random text
    TestPhrase: AnsiString;

    // Сервер -> Клинт
    // ИД, инфо о себе, рандомный хеш (ключ опознания)
    //procedure SendAuthRequest();
    procedure OnAuthRequest(Msg: TDnmpMsg);

    // Клиент -> Сервер
    // ИД, инфо о себе, хеш своего ключа по ключу сервера
    procedure SendAuthReply(sKey: string);
    //procedure OnAuthReply(Msg: TDnmpMsg);

    // Сервер -> Клиент
    // результат опознания
    // Если клиент известен:
    // Хеширует ключ клиента ключом опознания, сравнивает результаты
    // Если клиент неизвестен, то ключ опознания становится ключом клиента.  public
    //procedure SendAuthResult();
    procedure OnAuthResult(Msg: TDnmpMsg);

    // Клиент -> Сервер
    // Запрос списка узлов
    procedure SendNodelistRequest();
    //procedure OnNodelistRequest(Msg: TDnmpMsg);

    // Запрос списка контактов
    //procedure OnContactListRequest(Msg: TDnmpMsg); // [S]
    procedure OnContactListResponse(Msg: TDnmpMsg); // [SC]
    //=====================================
    function SendMsg(Msg: TDnmpMsg): boolean;

  public
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil);
    // Запуск парсера
    function Start(): Boolean; override;
    // Разбор сообщения в контексте линка
    // AUTH, INFO
    function ParseMsg(Msg: TDnmpMsg): Boolean; override;
  end;

implementation
uses RC4;

//=====================================
constructor TDnmpParserClient.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
  MyInfo:=Link.MyInfo;
  RemoteInfo:=Link.RemoteInfo;
end;

//=====================================
function TDnmpParserClient.Start(): Boolean;
begin
  if self.MyInfo.Key='' then Self.MyInfo.Key:=GenerateKey();
  Result:=True;
end;

//=====================================
function TDnmpParserClient.ParseMsg(Msg: TDnmpMsg): Boolean;
var
  MsgType: string;
  sCmd: string;
begin
  Result:=True;
  MsgType:=Msg.MsgType;
  if MsgType='' then
  begin
  end
  else if MsgType='AUTH' then // Built-in Authentication service
  begin
    sCmd:=Msg.Info.Values['cmd'];
    if sCmd='AURQ' then // Authentication request
    begin
      OnAuthRequest(Msg);
    end
    else if sCmd='ARPL' then // Authentication reply
    begin
      //OnAuthReply(Msg);
    end
    else if sCmd='ARSL' then // Authentication result
    begin
      OnAuthResult(Msg);
    end;
  end

  else if MsgType='INFO' then // Built-in Information service
  begin
    sCmd:=Msg.Info.Values['cmd'];
    if sCmd='LNRQ' then // Nodelist request
    begin
      //OnNodelistRequest(Msg);
    end
    else if sCmd='CLST' then // Contact list response
    begin
      OnContactListResponse(Msg); // [SC]
    end
    else if sCmd='LNKI' then // Node info
    begin
      Mgr.ReadLinkInfo(Msg);
    end
    else if sCmd='ERRR' then // Error reply
    begin
      Mgr.DebugText('Error reply: '+AddrToStr(Msg.SourceAddr)+' '+Msg.Info.Values['err_code']+' '+StreamToStr(Msg.Data));
    end;
  end
  else
  begin
    Result:=False;
  end;  

end;

// Сервер -> Клинт
// ИД, инфо о себе, рандомный хеш (ключ опознания)
procedure TDnmpParserClient.OnAuthRequest(Msg: TDnmpMsg);
var
  sNewCipher: AnsiString;
begin
  // Сервер послал нам инфу о себе и свой рандомный ключ
  // Нужно отдать инфу о себе и ключ сервера, хешированный нашим ключом
  TestPhrase:=StreamToStr(Msg.Data);
  if RemoteInfo.Key='' then RemoteInfo.Key:=TestPhrase;
  RemoteInfo.GUID:=Msg.Info.Values['guid'];
  //RemoteInfo.Addr:=Msg.SourceAddr;
  RemoteInfo.Name:=Msg.Info.Values['name'];
  //RemoteInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  RemoteInfo.Owner:=Msg.Info.Values['owner'];
  RemoteInfo.Location:=Msg.Info.Values['location'];
  RemoteInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  RemoteInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  RemoteInfo.OtherInfo:=Msg.Info.Values['other_info'];

  sNewCipher:=RC4.RC4EncryptText(TestPhrase, RemoteInfo.Key);
  SendAuthReply(sNewCipher);
end;

// Клиент -> Сервер
// ИД, инфо о себе, свой ключ шифрованый ключом сервера
procedure TDnmpParserClient.SendAuthReply(sKey: string);
var
  MsgOut: TDnmpMsg;
  sMsgBody: AnsiString;
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
  //MsgOut.Info.Values['key']:=sKey;
  //StrToStream(sKey, MsgOut.Data);

  SendMsg(MsgOut);
  MsgOut.Free();
end;

// Сервер -> Клиент
// результат опознания
// Если клиент известен:
// Хеширует ключ клиента ключом опознания, сравнивает результаты
// Если клиент неизвестен, то ключ опознания становится ключом клиента.
procedure TDnmpParserClient.OnAuthResult(Msg: TDnmpMsg);
var
  sResult: string;
begin
  sResult:=Msg.Info.Values['auth_result'];
  if sResult='OK' then
  begin
    // Опознание успешно
    //if MyInfo.SameAddr(EmptyAddr()) then
    begin
      // Мой адрес был пустым
      MyInfo.Addr:=Msg.TargetAddr;
    end;
    Mgr.AddCmd('AUTH OK');
  end
  else if sResult='KEY_NOT_FOUND' then
  begin
    // Линк не подтвержден
    // Сохраняем ключ сервера как свой
    RemoteInfo.Key:=TestPhrase;
    MyInfo.GUID:=Msg.Info.Values['guid'];
    //MyInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
    Mgr.AddCmd('AUTH FAILED');
  end
  else
  begin
    // Ошибка опознения
  end;
end;

// Клиент -> Сервер
// Запрос списка узлов
procedure TDnmpParserClient.SendNodelistRequest();
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, RemoteInfo.Addr, 'INFO', 'cmd=LNRQ', '');

  SendMsg(MsgOut);
  MsgOut.Free();
end;

procedure TDnmpParserClient.OnContactListResponse(Msg: TDnmpMsg);
var
  Storage: TDnmpStorage;
begin
  Storage:=Mgr.MsgDataToStorage(Msg);
  if not Assigned(Storage) then Exit;
  Mgr.TmpContactList.FromStorage(Storage);
  Storage.Free();
  Mgr.AddCmd('EVENT MGR UPDATE TMP_CONTACTS');
end;

function TDnmpParserClient.SendMsg(Msg: TDnmpMsg): boolean;
begin
  Result:=self.Link.SendMsg(Msg);
end;



end.
