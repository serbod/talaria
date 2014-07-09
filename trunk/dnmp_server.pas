// Server unit - server part
// ������ ��������� ��������� ��� �������
// ������������ �����������, ����� ������
unit dnmp_server;

interface
uses SysUtils, Classes, Contnrs, dnmp_unit;

type
  TDnmpParserServer = class(TDnmpParser)
  private
    MyInfo: TLinkInfo;
    LinkInfo: TLinkInfo;
    TempKey: string;

    //=====================================
    // �������� ��������� ���������� �� �������� ��������
    // ���� ���������� - ��� ����, �� ������ ���������.
    //procedure RouteMessage(Msg: TDnmpMsg);  // [S]

    //=== ����������� ===
    //=====================================
    // ������ -> �����
    // ��, ���� � ����, ������� ����� (������ ��������� ��������)
    procedure SendAuthRequest();  // [S]
    procedure OnAuthRequest(Msg: TDnmpMsg); // [SC]

    // ����� -> ������
    // ��, ���� � ���� � ������� �����, ���������� ����� ������
    procedure SendAuthReply(sKey: string); // [SC]
    procedure OnAuthReply(Msg: TDnmpMsg);  // [S]

    // ������ -> �����
    // ��������� ���������
    // ���� ������ ����������, �� ������� ����� ���������� ������ �������.
    procedure SendAuthResult(sResult: string); // [S]
    procedure OnAuthResult(Msg: TDnmpMsg); // [SC]

    // ����������� ��� �������� ����������� ��������� �����
    procedure OnAuthOkPointIn(); // [SC]
    procedure OnAuthOkNodeIn(); // [SC]
    // ����������� ��� �������� ����������� ���������� �����
    procedure OnAuthOkNodeOut(); // [S]

    //=====================================
    // ������ -> ������
    // ������ ������ �����
    procedure SendNodelistRequest(); // [S�]
    procedure OnNodelistRequest(Msg: TDnmpMsg); // [S]
    // ������ ����������
    procedure OnGetInfoRequest(Msg: TDnmpMsg); // [SC]
    // ������ -> ������
    // �������� ���������� �� ����-�����
    procedure SendNodeLinkInfo(); // [S]
    procedure OnNodeLinkInfo(Msg: TDnmpMsg); // [S]
    procedure SendLocalNodeLinksList(); // [S]
    procedure OnNodeLinksList(Msg: TDnmpMsg); // [S]
    //procedure OnLinkInfo(Msg: TDnmpMsg); // [SC]

    //=====================================
    // Server -> Client
    // ��������� �� ������ (!! NOT USED)
    procedure SendErrorMsg(OrigMsg: TDnmpMsg; ErrCode, ErrText: string); // [S]
    // �������������� ���������
    procedure SendInfoMsg(OrigMsg: TDnmpMsg; InfoCode, InfoText: string); // [SC]


    //=====================================
    // �������� ��������� �� ������ �����
    function SendMsg(Msg: TDnmpMsg): boolean; // [SC]

  public
    constructor Create(AMgr: TDnmpManager; ALink: TDnmpLink);
    // ������ �������
    function Start(): Boolean; override;
    // ������ ��������� � ���������� ��������� ��������
    function ParseMessage(Msg: TDnmpMsg): Boolean; override;
  end;

implementation
uses RC4, dnmp_services, Misc;


//=====================================
constructor TDnmpParserServer.Create(AMgr: TDnmpManager; ALink: TDnmpLink);
begin
  inherited Create();
  Self.Mgr:=AMgr;
  Self.Link:=ALink;
  Self.MyInfo:=Link.MyInfo;
  LinkInfo:=Link.LinkInfo;
end;

//=====================================
function TDnmpParserServer.Start(): Boolean;
begin
  if self.MyInfo.Key='' then Self.MyInfo.Key:=GenerateKey();
  self.LinkInfo.Addr:=NewAddr();
  self.SendAuthRequest();
  Result:=True;
end;

//=====================================
// �������� ��������� ���������� �� �������� ��������
// ���� ���������� - ��� ����, �� ������ ���������.
//procedure TDnmpParserServer.RouteMessage(Msg: TDnmpMsg);
//var
//  i: integer;
//  TargetPoint: TPointID;
//  DnmpLink: TDnmpLink;
//begin
//
//  // ��������� ������� ������ ����� ����
//  if SameNode(Msg.TargetAddr, Mgr.MyInfo.Addr) then
//  begin
//    TargetPoint:=Msg.TargetAddr.Point;
//
//    // ��������� ������ ����
//    if TargetPoint = Mgr.MyInfo.Addr.Point then
//    begin
//      //Mgr.IncomingMsg(Msg, Link);
//      Exit;
//    end;
//
//    // ����� ������ ���������� � ������ ������
//    for i:=0 to Mgr.LinkList.Count-1 do
//    begin
//      DnmpLink:=Mgr.LinkList[i];
//      //if DnmpLink.LinkType <> ltPoint then Continue;
//      if not DnmpLink.LinkInfo.SameAddr(Msg.TargetAddr) then Continue;
//      // �������� ��������� ������
//      if DnmpLink.Active then
//      begin
//        DnmpLink.SendMsg(Msg);
//        Exit;
//      end;
//    end;
//
//    // ����� ������ ���������� � ����������
//    for i:=0 to Mgr.PointList.Count-1 do
//    begin
//      if not Mgr.PointList[i].Addr.Point = TargetPoint then Continue;
//      // �������� ��������� � ������� �������� ������
//      //DelayMsg(Msg);
//      SendInfoMsg(Msg, '201', 'Message delayed');
//      Exit;
//    end;
//
//    // ������ ���������� ��� � ����������, ���������� ������
//    SendErrorMsg(Msg, '101', 'Destination point not found');
//    Exit;
//  end;
//
//  // ��������� �� ������ ����
//  // ����� ����� �����-������
//  for i:=0 to Mgr.LinkList.Count-1 do
//  begin
//    DnmpLink:=Mgr.LinkList[i];
//    //if (Mgr.LinkList[i] as TDnmpLink).LinkType <> ltNode then Continue;
//    if not DnmpLink.LinkInfo.SameNode(Msg.TargetAddr) then Continue;
//    // �������� ��������� �� ����
//    if DnmpLink.Active then
//    begin
//      DnmpLink.SendMsg(Msg);
//      Exit;
//    end;
//  end;
//
//  // ����� � ������� �������������
//  DnmpLink:=Mgr.RoutingTable.LinkForDestAddr(Msg.TargetAddr);
//  if Assigned(DnmpLink) then
//  begin
//    DnmpLink.SendMsg(Msg);
//    Exit;
//  end;
//
//  // ���� ������ ����������
//  if Assigned(Mgr.Uplink) then
//  begin
//    DnmpLink:=Mgr.Uplink;
//    // �������� �� ������
//    if DnmpLink.Active then
//    begin
//      DnmpLink.SendMsg(Msg);
//      Exit;
//    end;
//  end
//  else
//  begin
//    // �������� ��������� �� ������
//    SendErrorMsg(Msg, '102', 'Destination address not found');
//    Exit;
//  end;
//end;

function TDnmpParserServer.ParseMessage(Msg: TDnmpMsg): Boolean;
var
  MsgType: string;
begin
  Result:=True;
  MsgType:=Msg.MsgType;
  if MsgType='' then
  begin
  end
  else if MsgType='AURQ' then // Authentication request
  begin
    OnAuthRequest(Msg); // [SC]
  end
  else if MsgType='ARPL' then // Authentication reply
  begin
    OnAuthReply(Msg); // [S]
  end
  else if MsgType='ARSL' then // Authentication result
  begin
    OnAuthResult(Msg); // [SC]
  end
  else
  begin
    if SameAddr(Msg.TargetAddr, MyInfo.Addr) then
    begin
      // Msg for me
      if MsgType='NLRQ' then // Nodelist request
      begin
        OnNodelistRequest(Msg); // [S]
      end
      else if MsgType='LNKI' then // Link info
      begin
        Mgr.ReadLinkInfo(Msg); // [SC]
      end
      else if MsgType='NLST' then // Links list
      begin
        OnNodeLinksList(Msg); // [SC]
      end
      else if MsgType='GINF' then // Get info
      begin
        OnGetInfoRequest(Msg);
      end
      else if MsgType='ERRR' then // Error reply
      begin
        Mgr.DebugText('Error reply: '+AddrToStr(Msg.SourceAddr)+' '+Msg.Info.Values['err_code']+' '+StreamToStr(Msg.Data));
      end
      else
      begin
        Result:=False;
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

end;

//=====================================
// ������ -> �����
// ��, ���� � ����, ������� ����� (������ ��������� ��������)
procedure TDnmpParserServer.SendAuthRequest();
var
  MsgOut: TDnmpMsg;
  sMsgBody: string;
begin
  //MyInfo.Key:=GenerateKey();
  TempKey:=GenerateKey();
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, NewAddr, 'AURQ', '', TempKey);
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

// ������ ������ ��� ���� � ���� � ���� ������� �����
// ����� ������ ���� � ���� � ������� �����, ����������� ����� ������
procedure TDnmpParserServer.OnAuthRequest(Msg: TDnmpMsg);
var
  RC4Data: TRC4Data;
  sRemoteKey, sLocalKey, sNewKey: AnsiString;
  li: TLinkInfo;
begin
  sRemoteKey:=StreamToStr(Msg.Data);
  // ���� ��� ������ �����, �� ������ ����� ������ ������� ����� �������
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
  // ������� ����� ��� �����
  SetLength(sNewKey, Length(sRemoteKey));
  // �������������� ��� ����
  RC4.RC4Burn(RC4Data);
  RC4.RC4Init(RC4Data, sLocalKey);
  // ������� ������� ����� ������� ����� ������
  RC4.RC4Crypt(RC4Data, PChar(sRemoteKey), PChar(sNewKey), Length(sRemoteKey));

  SendAuthReply(sNewKey);
end;

//=====================================
// ������ -> ������
// ��, ���� � ���� � ������� �����, ���������� ����� ������
procedure TDnmpParserServer.SendAuthReply(sKey: string);
var
  MsgOut: TDnmpMsg;
  sMsgBody: AnsiString;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'ARPL', '', sKey);
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

// ������ ������ ��� ���� � ���� � ������� �����, ����������� ������ �������
// ����� ������� �� ������ ��������� �������� ���������� ������������
// ������� ����� � �������� ��������� � ������������ �����
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
    // ���� �� ������
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
    // ���� � ������ ���������������� ���������
    li:=Mgr.ContactList.GetLinkInfoByGUID(LinkInfo.GUID);
    if Assigned(li) then Found:=CheckKey(li.Key, sRemoteKey, TempKey);
  end;

  // ��������, ���� ���� �� �����..
  if not Found then
  begin
    // ��������� ���� �� ������ ���������
    for i:=0 to Mgr.ContactList.Count-1 do
    begin
      if Found then Break;
      li:=Mgr.ContactList[i];
      Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;

    // ��������� ���� �� ����������
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Found then Break;
      li:=Mgr.PointList[i];
      Found:=CheckKey(li.Key, sRemoteKey, TempKey);
    end;

    // ��������� ���� �� ��������
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
    LinkInfo.Addr:=NewAddr;
    Link.Approve();
    li:=LinkInfo;
    Found:=True;
  end;

  if Found then
  begin
    if li <> LinkInfo then
    begin
      // ��������� ���������� �� ������� �������������� ���������� (���, ��������, ������, ���..)
      li.Name:=LinkInfo.Name;
      li.Owner:=LinkInfo.Owner;
      li.Location:=LinkInfo.Location;
      li.OtherInfo:=LinkInfo.OtherInfo;
      // ��������� ���������� ����� �� ����������
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
    LinkInfo.Addr:=NewAddr;

    // ��������� ���������� ����� � ������ ���������
    Mgr.ContactList.Add(LinkInfo);

    SendAuthResult('KEY_NOT_FOUND');
    Mgr.Cmd('AUTH_FAIL '+LinkInfo.GUID);
  end;
  Mgr.AddCmd('UPDATE LINKS');
end;

//=====================================
// ������ -> ������
// ��������� ���������
procedure TDnmpParserServer.SendAuthResult(sResult: string);
var
  MsgOut: TDnmpMsg;
  sMsgBody: string;
begin
  //sMsgBody:='AuthResult=OK';

  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'ARSL','','');
  MsgOut.Info.Values['addr']:=LinkInfo.AddrStr;
  MsgOut.Info.Values['guid']:=LinkInfo.GUID;
  MsgOut.Info.Values['senior_guid']:=LinkInfo.SeniorGUID;
  MsgOut.Info.Values['auth_result']:=sResult;

  Self.SendMsg(MsgOut);
  MsgOut.Free();
end;

// ������ -> ������
// ��������� ���������
// ���� ������ ��������:
// �������� ���� ������� ������ ���������, ���������� ����������
// ���� ������ ����������, �� ���� ��������� ���������� ������ �������.
procedure TDnmpParserServer.OnAuthResult(Msg: TDnmpMsg);
var
  sResult: string;
begin
  sResult:=Msg.Info.Values['auth_result'];
  if sResult='OK' then
  begin
    // ��������� �������
    if SameAddr(MyInfo.Addr, NewAddr()) then
    begin
      MyInfo.Addr:=Msg.TargetAddr;
      MyInfo.GUID:=Msg.Info.Values['guid'];
      MyInfo.SeniorGUID:=Msg.Info.Values['senior_guid'];
    end;

    OnAuthOkNodeOut();
  end
  else if sResult='KEY_NOT_FOUND' then
  begin
    // ���� �� �����������
    // ��������� ���� ������� ��� ����
    //MyInfo.Key:=LinkInfo.Key;
  end
  else
  begin
    // ������ ���������
  end;
end;

// ����������� ��� �������� ����������� ��������� �����
procedure TDnmpParserServer.OnAuthOkPointIn();
begin
  // �������� ���������� ���������
  // SendQueuedMessages();

end;

{��� ����������� ���� ����� � ���� ������:

1. ����� �������� ������� ������ ���� ����� ����������� �����.
2. ������ �������� ����� ���� ������ �����.
3. ������ �������� ������ ������� ���-������ � ����������� � �����.
4. ����� �������� ����� ���������� ���-������ � ����������� � �������.}
procedure TDnmpParserServer.OnAuthOkNodeIn();
begin
  //2. ������ �������� ����� ���� ������ �����.

  //3. ������ �������� ������ ������� ���-������ � ����������� � �����.
  Self.SendNodeLinkInfo();
end;

// ����������� ��� �������� ����������� ���������� �����
procedure TDnmpParserServer.OnAuthOkNodeOut();
begin
  // 1. ����� �������� ������� ������ ���� ����� ����������� �����.
  Self.SendLocalNodeLinksList();
  // 4. ����� �������� ����� ���������� ���-������ � ����������� � �������.}
  Self.SendNodeLinkInfo();
end;

//=====================================
// ������ -> ������
// ������ ������ �����
procedure TDnmpParserServer.SendNodelistRequest();
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'NLRQ', '', '');

  SendMsg(MsgOut);
  MsgOut.Free();
end;

// ������ -> ������
// ������ ������ �����
procedure TDnmpParserServer.OnNodelistRequest(Msg: TDnmpMsg);
var
  i: Integer;
begin
  if Msg.SourceAddr.Point=0 then
  begin
    // �������� ��������� ���������� �� ��������� �����
    for i:=0 to Mgr.NodeList.Count-1 do
    begin
      Mgr.SendLinkInfo(Mgr.NodeList[i], Msg.SourceAddr);
    end;
  end
  else
  begin
    // �������� ������� ���������� �� ��������� �����
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
  // ����� �� ������
  if Length(Trim(Msg.Info.Values['addr']))>0 then
  begin
    SomeAddr:=StrToAddr(Msg.Info.Values['addr']);
    if SomeAddr.Point=0 then
    begin
      // ���������� ���� � ����
      Mgr.SendLinkInfo(MyInfo, Msg.SourceAddr);
      Exit;
    end;
    // ����� ���������� �� ������
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Mgr.PointList[i].SameAddr(SomeAddr) then
      begin
        // �������� ���������� �� ������
        Mgr.SendLinkInfo(Mgr.PointList[i], Msg.SourceAddr);
        Exit;
      end;
    end;
  end;

  // ����� �� �����
  SomeName:=AnsiLowerCase(Trim(Msg.Info.Values['name']));
  n:=0;
  if Length(SomeName)>0 then
  begin
    // ����� � ����������
    for i:=0 to Mgr.PointList.Count-1 do
    begin
      if Pos(SomeName, AnsiLowerCase(Mgr.PointList[i].Name))>0 then
      begin
        // �������� ����������
        Mgr.SendLinkInfo(Mgr.PointList[i], Msg.SourceAddr);
        Inc(n);
      end;
    end;
    if n>0 then Exit;
  end;
  // �� �����
  //SendErrorMsg(Msg, 'GINF_ERR', 'Point not found');
  Mgr.SendErrorMsg(Msg.SourceAddr, 'GINF_ERR', 'Point not found');
end;

// ������ -> ������
// �������� ���������� �� ����-�����
//
// �������� �� ����:
// ���: NINF
// ���������:
// addr - ����� ����
// state - ��������� ���� �� ��������� � ����������� (on / off)
// rate - ������� ����
// avail - ������� ������� ����������� ����
// speed - ������� ������� �������� ����
// ������:
// ���
procedure TDnmpParserServer.SendNodeLinkInfo(); // [S]
var
  sl: TStringList;
  Msg: TDnmpMsg;
  i: Integer;
  s: string;
begin
  if Link.Active then s:='on' else s:='off';
  sl:=TStringList.Create();
  sl.Values['addr']:=LinkInfo.AddrStr();
  sl.Values['state']:=s;
  sl.Values['rate']:=IntToStr(LinkInfo.Rating);
  sl.Values['avail']:='';
  sl.Values['speed']:=IntToStr(Link.Speed);

  Msg:=TDnmpMsg.Create(MyInfo.Addr, NewAddr(), 'NINF', sl.Text, '');
  FreeAndNil(sl);

  if Mgr.Uplink = Link then
  begin
    // ���� ��� ������, �� �������� ��� ������ ���� node-������
    for i:=0 to Mgr.LinkList.Count-1 do
    begin
      if (Mgr.LinkList[i].Active) and (Mgr.LinkList[i].LinkType=ltNode) then Mgr.LinkList[i].SendMsg(Msg);
    end;
  end
  else
  begin
    // ���� ��� ����, �� �������� ��� ������ �������
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
    if not SameAddr(SomeAddr, NewAddr()) then Mgr.RoutingTable.DelDest(SomeAddr.Node);
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
  Msg:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'NLST', '', '');
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
// ��������� �� ������
procedure TDnmpParserServer.SendErrorMsg(OrigMsg: TDnmpMsg; ErrCode, ErrText: string);
var
  MsgOut: TDnmpMsg;
begin
  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, LinkInfo.Addr, 'ERRR','','');
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

