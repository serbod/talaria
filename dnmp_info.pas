unit dnmp_info;
{
DNMP INFO - information service
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_unit;

type

  { TDnmpInfoService }

  TDnmpInfoService = class(TDnmpMsgHandler)
  private
    //=====================================
    // Сервер -> Сервер
    // NLRQ (Nodelist request)
    procedure SendNodelistRequest(Addr: TAddr); // [SС]
    procedure OnNodelistRequest(Msg: TDnmpMsg); // [S]

    // GINF (Get info)
    procedure OnGetInfoRequest(Msg: TDnmpMsg); // [SC]
    procedure SendContactInfo(ALinkInfo: TDnmpContact; TargetAddr: TAddr); // [S]
    // CINF (Contact info)
    procedure OnContactInfo(Msg: TDnmpMsg); // [SC]

    // Сервер -> Сервер
    // LLRQ (Link list request)
    procedure SendLocalNodeLinksList(); // [S]
    // LLST (Link list response)
    procedure OnNodeLinksList(Msg: TDnmpMsg); // [S]

    // PLRQ (Pointlist request)
    // PLST (Pointlist response

    // CLRQ (Contact list request)
    procedure OnContactListRequest(Msg: TDnmpMsg); // [S]
    // CLST (Contact list response)
    procedure OnContactListResponse(Msg: TDnmpMsg); // [SC]
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
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink = nil); override;
    // Запуск парсера
    function Start(): Boolean; override;
    // Разбор сообщения и выполнение требуемых действий
    function ParseMsg(Msg: TDnmpMsg): Boolean; override;
  end;

implementation

//=====================================
// Клиент -> Сервер
// Запрос списка узлов
procedure TDnmpInfoService.SendNodelistRequest(Addr: TAddr);
begin
  Mgr.SendDataMsg(Addr, 'INFO', 'cmd=NLRQ', '');
end;

// Клиент -> Сервер
// Запрос списка узлов
procedure TDnmpInfoService.OnNodelistRequest(Msg: TDnmpMsg);
var
  i: Integer;
  sInfo, sData: string;
begin
  //if Mgr.MyInfo.Addr.Point<>0 then Exit; // points cannot answer
  sInfo:='cmd=NLST'+CRLF
    +'format='+Mgr.Serializer.GetName();
  if Msg.SourceAddr.Point=0 then
  begin
    // Отправка подробной информации об известных узлах
    sData:=Mgr.Serializer.StorageToString(Mgr.NodeList.ToStorage(ctPrivate));
  end
  else
  begin
    // Отправка базовой информации об известных узлах
    sData:=Mgr.Serializer.StorageToString(Mgr.NodeList.ToStorage(ctPublic));
  end;
  Mgr.SendDataMsg(Msg.SourceAddr, 'INFO', sInfo, sData);
end;

procedure TDnmpInfoService.OnGetInfoRequest(Msg: TDnmpMsg);
var
  SomeAddr: TAddr;
  SomeGUID: string;
  i, n: Integer;
begin
  // Поиск по адресу
  if Length(Trim(Msg.Info.Values['addr']))>0 then
  begin
    SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
    if SameAddr(SomeAddr, Mgr.MyInfo.Addr) then
    begin
      // Отправляем инфо о себе
      Self.SendContactInfo(Mgr.MyInfo, Msg.SourceAddr);
      Exit;
    end;
    // Поиск информации об адресе
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if SameAddr(Mgr.PointList[i].Addr, SomeAddr) then
      begin
        // Отправка информации об адресе
        Self.SendContactInfo(Mgr.PointList[i], Msg.SourceAddr);
        Exit;
      end;
    end;
  end;

  // Поиск по GUID
  SomeGUID:=AnsiLowerCase(Trim(Msg.Info.Values['guid']));
  n:=0;
  if Length(SomeGUID)>0 then
  begin
    // Поиск в поинтлисте
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Pos(SomeGUID, AnsiLowerCase(Mgr.PointList[i].GUID))>0 then
      begin
        // Отправка информации
        Self.SendContactInfo(Mgr.PointList[i], Msg.SourceAddr);
        Inc(n);
      end;
    end;
    if n>0 then Exit;
  end;

  // Не нашли
  //SendErrorMsg(Msg, 'GINF_ERR', 'Point not found');
  Mgr.SendErrorMsg(Msg.SourceAddr, 'GINF_ERR', 'Point not found');
end;

procedure TDnmpInfoService.SendContactInfo(ALinkInfo: TDnmpContact;
  TargetAddr: TAddr);
var
  MsgOut: TDnmpMsg;
  sData: AnsiString;
begin
  MsgOut:=TDnmpMsg.Create(Mgr.MyInfo.Addr, TargetAddr, 'INFO','','');
  MsgOut.Info.Values['cmd']:='CINF';
  MsgOut.Info.Values['addr']:=ALinkInfo.AddrStr;
  MsgOut.Info.Values['guid']:=ALinkInfo.GUID;

  if (TargetAddr.Point=0) and (ALinkInfo.Addr.Point=0) then
  begin
    // node to node
    sData:=Mgr.Serializer.StorageToString(ALinkInfo.ToStorage(ctPrivate));
  end
  else
  begin
    sData:=Mgr.Serializer.StorageToString(ALinkInfo.ToStorage(ctPublic));
  end;

  StrToStream(sData, MsgOut.Data);

  SendMsg(MsgOut);
  MsgOut.Free();
end;

procedure TDnmpInfoService.OnContactInfo(Msg: TDnmpMsg);
var
  //i: integer;
  SomeAddr: TAddr;
  TmpInfo: TDnmpContact;
  Storage: TDnmpStorage;
  s, s2: string;
begin
  TmpInfo:=nil;
  s:='';
  SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
  if SomeAddr.Point=0 then
  begin
    s:='NODELIST';
    TmpInfo:=Mgr.NodeList.GetByAddr(SomeAddr);
    if not Assigned(TmpInfo) then
    begin
      TmpInfo:=TDnmpContact.Create();
      Mgr.NodeList.Add(TmpInfo);
    end;
  end
  // Server only
  else if Mgr.ServerMode and SameNode(SomeAddr, Mgr.MyInfo.Addr) then
  begin
    s:='POINTLIST';
    TmpInfo:=Mgr.PointList.GetByAddr(SomeAddr);
    if not Assigned(TmpInfo) then Exit; // ???
  end
  else
  begin
    s:='CONTACTS';
    TmpInfo:=Mgr.ContactList.GetByAddr(SomeAddr);
    if not Assigned(TmpInfo) then
    begin
      TmpInfo:=TDnmpContact.Create();
      Mgr.ContactList.Add(TmpInfo);
    end;
  end;

  Storage:=Mgr.MsgDataToStorage(Msg);
  if Assigned(Storage) then
  begin
    TmpInfo.FromStorage(Storage);
    Storage.Free();
  end;

  Mgr.AddCmd('EVENT MGR UPDATE '+s);
end;

procedure TDnmpInfoService.SendLocalNodeLinksList();
begin
end;

procedure TDnmpInfoService.OnNodeLinksList(Msg: TDnmpMsg);
var
  sl: TStringList;
begin

  //Mgr.RoutingTable.AddItem();
end;

procedure TDnmpInfoService.OnContactListRequest(Msg: TDnmpMsg);
var
  i, iDepth: integer;
  SomeName: string;
  TmpItemList: TDnmpContactList;
  sInfo, sData: string;
begin
  TmpItemList:=TDnmpContactList.Create(nil);
  // Поиск по имени
  SomeName:=LowerCase(Trim(Msg.Info.Values['name']));
  if Length(SomeName)=0 then
  begin
    // Имя не указано
    Mgr.DebugText('Empty name in contacts request');
    Exit;
  end;

  // Добавляем инфо о себе
  if Pos(SomeName, LowerCase(Mgr.MyInfo.Name))>0 then TmpItemList.Add(Mgr.MyInfo);

  // Pointlist
  for i:=0 to Mgr.PointList.Count-1 do
  begin
    if Pos(SomeName, LowerCase(Mgr.PointList[i].Name))>0 then TmpItemList.Add(Mgr.PointList[i]);
  end;

  // готовим ответ
  if TmpItemList.Count>0 then
  begin;
    sInfo:='cmd=CLST'+CRLF
      +'name='+Msg.Info.Values['name']+CRLF
      +'format='+Mgr.Serializer.GetName();
    sData:=Mgr.Serializer.StorageToString(TmpItemList.ToStorage(ctPublic));
    TmpItemList.Clear();

    // шлем ответ отправителю
    Mgr.SendDataMsg(Msg.SourceAddr, 'INFO', sInfo, sData);

    // рассылка другим линкам, если глубина > 0
    iDepth:=StrToIntDef(Msg.Info.Values['depth'], 0);
    if iDepth>0 then
    begin
      iDepth:=iDepth-1;
      Msg.Info.Values['depth']:=IntToStr(iDepth);
      Mgr.SendBroadcastMsg(Msg, 'nodes');
    end;
  end;
  TmpItemList.Free();
end;

procedure TDnmpInfoService.OnContactListResponse(Msg: TDnmpMsg);
var
  Storage: TDnmpStorage;
begin
  Storage:=Mgr.MsgDataToStorage(Msg);
  if not Assigned(Storage) then Exit;
  Mgr.TmpContactList.FromStorage(Storage);
  Storage.Free();
  Mgr.AddCmd('EVENT MGR UPDATE TMP_CONTACTS');
end;

//=====================================
// Server -> Client
// Сообщение об ошибке
procedure TDnmpInfoService.SendErrorMsg(OrigMsg: TDnmpMsg; ErrCode, ErrText: string);
var
  MsgOut: TDnmpMsg;
begin
  if not Assigned(OrigMsg) then Exit;
  MsgOut:=TDnmpMsg.Create(Mgr.MyInfo.Addr, OrigMsg.SourceAddr, 'INFO','','');
  MsgOut.Info.Values['cmd']:='ERRR';
  MsgOut.Info.Values['code']:=ErrCode;
  MsgOut.Info.Values['text']:=ErrText;
  MsgOut.Info.Values['source_timestamp']:=TimestampToStr(OrigMsg.TimeStamp);
  OrigMsg.ToStream(MsgOut.Data);

  SendMsg(MsgOut);
  MsgOut.Free();
end;

procedure TDnmpInfoService.SendInfoMsg(OrigMsg: TDnmpMsg; InfoCode, InfoText: string);
var
  MsgOut: TDnmpMsg;
begin
  if not Assigned(OrigMsg) then Exit;
  MsgOut:=TDnmpMsg.Create(Mgr.MyInfo.Addr, OrigMsg.SourceAddr, 'INFO','','');
  MsgOut.Info.Values['cmd']:='INFO';
  MsgOut.Info.Values['code']:=InfoCode;
  MsgOut.Info.Values['text']:=InfoText;
  MsgOut.Info.Values['source_timestamp']:=TimestampToStr(OrigMsg.TimeStamp);

  SendMsg(MsgOut);
  MsgOut.Free();
end;

function TDnmpInfoService.SendMsg(Msg: TDnmpMsg): boolean;
begin
  //Result:=self.Link.SendMsg(Msg);
  Result:=Mgr.SendMsg(Msg);
end;

constructor TDnmpInfoService.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create(AMgr, ALink);
end;

function TDnmpInfoService.Start(): Boolean;
begin
  Result:=inherited Start;
end;

function TDnmpInfoService.ParseMsg(Msg: TDnmpMsg): Boolean;
var
  MsgType: string;
  sCmd: string;
begin
  Result:=False;
  if not Assigned(Msg) then Exit;
  if not SameAddr(Msg.TargetAddr, Mgr.MyInfo.Addr) then Exit;
  MsgType:=Msg.MsgType;

  if MsgType='INFO' then // Built-in Information service
  begin
    Result:=True;
    sCmd:=Msg.Info.Values['cmd'];

    if sCmd='NLRQ' then // Nodelist request
    begin
      OnNodelistRequest(Msg); // [S]
    end
    else if sCmd='NLST' then // Nodes list
    begin
      OnNodeLinksList(Msg); // [SC]
    end
    else if sCmd='CLRQ' then // Contact list request
    begin
      OnContactListRequest(Msg); // [S]
    end
    else if sCmd='CLST' then // Contact list response
    begin
      OnContactListResponse(Msg); // [SC]
    end
    else if sCmd='GINF' then // Get info
    begin
      OnGetInfoRequest(Msg);
    end
    else if sCmd='CINF' then // Contact info
    begin
      Self.OnContactInfo(Msg); // [SC]
    end
    else if sCmd='ERRR' then // Error reply
    begin
      Mgr.DebugText('Error reply: '+AddrToStr(Msg.SourceAddr)+' '+Msg.Info.Values['err_code']+' '+StreamToStr(Msg.Data));
    end;
  end;

end;

end.

