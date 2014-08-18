unit dnmp_client;

interface
uses SysUtils, Classes, dnmp_unit;

type
  TDnmpParserClient = class(TDnmpMsgHandler)
  private
    MyInfo: TDnmpLinkInfo;
    LinkInfo: TDnmpLinkInfo;

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

    //=====================================
    function SendMsg(Msg: TDnmpMsg): boolean;

  public
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil);
    // Запуск парсера
    function Start(): Boolean; override;
    // Разбор сообщения в контексте линка
    function ParseMsg(Msg: TDnmpMsg): Boolean; override;
  end;

implementation
uses RC4;

//=====================================
constructor TDnmpParserClient.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
  MyInfo:=Link.MyInfo;
  LinkInfo:=Link.LinkInfo;
end;

//=====================================
function TDnmpParserClient.Start(): Boolean;
begin
  if self.MyInfo.Key='' then Self.MyInfo.Key:=GenerateKey();
  Result:=True;
end;

//=====================================
function TDnmpParserClient.ParseMsg(Msg: TDnmpMsg): boolean;
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
  RC4Data: TRC4Data;
  sRemoteKey, sLocalKey, sNewKey: AnsiString;
begin
  // Сервер послал нам инфу о себе и свой рандомный ключ
  // Нужно отдать инфу о себе и ключ сервера, хешированный нашим ключом
  if self.MyInfo.Key='' then Self.MyInfo.Key:=GenerateKey();
  sLocalKey:=self.MyInfo.Key;
  sRemoteKey:=StreamToStr(Msg.Data);
  LinkInfo.Key:=sRemoteKey;
  LinkInfo.Addr:=Msg.SourceAddr;
  LinkInfo.Name:=Msg.Info.Values['name'];
  LinkInfo.Owner:=Msg.Info.Values['owner'];
  LinkInfo.Location:=Msg.Info.Values['location'];
  LinkInfo.GUID:=Msg.Info.Values['guid'];
  LinkInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
  LinkInfo.IpAddr:=Msg.Info.Values['ip_addr'];
  LinkInfo.PhoneNo:=Msg.Info.Values['phone_no'];
  LinkInfo.OtherInfo:=Msg.Info.Values['other_info'];
  SetLength(sNewKey, Length(sLocalKey));
  RC4.RC4Burn(RC4Data);
  RC4.RC4Init(RC4Data, sLocalKey);
  RC4.RC4Crypt(RC4Data, PChar(sRemoteKey), PChar(sNewKey), Length(sRemoteKey));

  SendAuthReply(sNewKey);
end;

// Клиент -> Сервер
// ИД, инфо о себе, свой ключ шифрованый ключом сервера
procedure TDnmpParserClient.SendAuthReply(sKey: string);
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
    MyInfo.Addr:=Msg.TargetAddr;
    Mgr.AddCmd('AUTH OK');
  end
  else if sResult='KEY_NOT_FOUND' then
  begin
    // Линк не подтвержден
    // Сохраняем ключ сервера как свой
    MyInfo.Key:=LinkInfo.Key;
    MyInfo.GUID:=Msg.Info.Values['guid'];
    MyInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
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
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'INFO', 'cmd=LNRQ', '');

  SendMsg(MsgOut);
  MsgOut.Free();
end;

function TDnmpParserClient.SendMsg(Msg: TDnmpMsg): boolean;
begin
  Result:=self.Link.SendMsg(Msg);
end;



end.

