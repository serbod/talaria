unit dnmp_ip;

interface
uses SysUtils, Classes, blcksock, synsock, dnmp_unit;

type
  TSockReaderEvent = procedure(LastError: integer; Data: string) of object;
  TSockReader = class(TThread)
  private
    FOnEvent: TSockReaderEvent;
  public
    Str: string;
    //Socket: TTCPBlockSocket;
    Socket: TBlockSocket;
    constructor Create(BSock: TBlockSocket; EventHandler: TSockReaderEvent);
    destructor Destroy(); override;
    property OnEvent: TSockReaderEvent read FOnEvent write FOnEvent;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  end;

  TSockListenerEvent = procedure(Sock: TSocket; LastError: integer; Data: string) of object;
  TSockListener = class(TThread)
  private
    FOnEvent: TSockListenerEvent;
  public
    Str: string;
    Socket: TTCPBlockSocket;
    //Socket: TBlockSocket;
    NewSocket: TSocket;
    constructor Create(BSock: TTCPBlockSocket; EventHandler: TSockListenerEvent);
    destructor Destroy(); override;
    property OnEvent: TSockListenerEvent read FOnEvent write FOnEvent;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  end;

  { TIpLink }

  TIpLink = class(TDnmpLink)
  public
    //Socket: TTCPBlockSocket;
    Socket: TBlockSocket;
    Reader: TSockReader;
    Listener: TSockListener;
    LastMsgType: integer;

    IgnoreTimeout: boolean;
    IpProto: string;
    LinkHost: string;
    LinkPort: string;
    Incoming: Boolean;
    IdleTimestamp: TDateTime;
    constructor Create(AMgr: TDnmpManager; ALinkInfo: TDnmpContact = nil); override;
    destructor Destroy(); override;
    procedure ReaderEventHandler(LastError: integer; Data: string);
    procedure ListenerEventHandler(Sock: TSocket; LastError: integer; Data: string);
    function StartReader(): boolean;

    function Connect(): boolean; override;
    function Listen(): boolean; override;
    function Disconnect(): boolean; override;
    function Check(): boolean; override;
    function SendMsg(Msg: TDnmpMsg): boolean; override;
  end;

var
  iTcpTimeout: Integer = 1000;

implementation

function GetIpHost(sIpAddr: string): string;
var
  i: Integer;
begin
  i:=Pos(':', sIpAddr);
  if i=0 then Result:=Trim(sIpAddr)
  else Result:=Trim(Copy(sIpAddr, 1, i-1));
end;

function GetIpPort(sIpAddr: string): string;
var
  i: Integer;
begin
  i:=Pos(':', sIpAddr);
  if i=0 then Result:=''
  else Result:=Trim(Copy(sIpAddr, i+1, maxint));
end;


// === TSockReader ===
constructor TSockReader.Create(BSock: TBlockSocket; EventHandler: TSockReaderEvent);
begin
  FreeOnTerminate:=true;
  Socket:=BSock;
  OnEvent:=EventHandler;
  inherited Create(True);
end;

destructor TSockReader.Destroy();
begin
  Self.OnEvent:=nil;
  if Assigned(Socket) then FreeAndNil(Socket);
  inherited Destroy();
end;

procedure TSockReader.SyncProc();
begin
  if Str='' then Exit;
  if Assigned(OnEvent) then OnEvent(Socket.LastError, Str);
  Str:='';
end;

procedure TSockReader.Execute();
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create('');
  while not Terminated do
  begin
    { TODO : Где-то здесь могут быть проблемы }
    if Assigned(Socket) then
    begin
      //if Socket.WaitingDataEx()>0 then
      //if Socket.CanReadEx(1) then
      //if Socket.CanRead(iTcpTimeout) then
      if Socket.CanReadEx(iTcpTimeout) then
      begin
        Socket.RecvStream(ss, iTcpTimeout);
        //Str:=Socket.RecvPacket(120000);
        str:=ss.DataString;
        ss.Size:=0;
      end;
      if Assigned(Socket) and (Socket.LastError<>0) then
      begin
        Str:=Socket.LastErrorDesc;
        //Terminate();
      end;
    end;
    if (not Terminated) and (Length(Str)>0) then Synchronize(@SyncProc);
    Sleep(1);
  end;
  FreeAndNil(ss);
end;

// === TSockListener ===
constructor TSockListener.Create(BSock: TTCPBlockSocket; EventHandler: TSockListenerEvent);
begin
  FreeOnTerminate:=true;
  Socket:=BSock;
  OnEvent:=EventHandler;
  inherited Create(True);
end;

destructor TSockListener.Destroy();
begin
  if Assigned(Socket) then FreeAndNil(Socket);
  Self.OnEvent:=nil;
  inherited Destroy();
end;

procedure TSockListener.SyncProc();
begin
  //if Str='' then Exit;
  if Assigned(OnEvent) then OnEvent(NewSocket, Socket.LastError, Str);
  Str:='';
end;

procedure TSockListener.Execute();
begin
  Socket.Listen();
  while not Terminated do
  begin
    if Assigned(Socket) then
    begin
      if Socket.CanRead(iTcpTimeout) then
      begin
        NewSocket:=Socket.Accept();
        if Socket.LastError<>0 then
        begin
          Str:=Socket.LastErrorDesc;
          //Terminate();
        end;
        if not Terminated then Synchronize(@SyncProc);
      end;
    end;
    Sleep(1);
  end;
end;

// ===================
// === TIpLink     ===
// ===================
constructor TIpLink.Create(AMgr: TDnmpManager; ALinkInfo: TDnmpContact);
var
  sHost, sPort, sProto: string;
begin
  inherited Create(AMgr, ALinkInfo);
  sHost:=GetIpHost(RemoteInfo.IpAddr);
  sPort:=GetIpPort(RemoteInfo.IpAddr);
  sProto:='TCP';
  if sHost='' then sHost:='localhost';
  if sPort='' then sPort:='4044';

  Self.LinkHost:=sHost;
  Self.LinkPort:=sPort;
  Self.IpProto:=sProto;
  if IpProto='' then IpProto:='IP';

  //LinkInfo.Addr:=MyInfo.Addr;
  //LinkInfo.Name:=IpProto+' link '+LinkHost+':'+LinkPort;
  //LinkInfo.Owner:=MyInfo.Owner;
  //LinkInfo.IpAddr:='localhost';
  IdleTimestamp:=Now();
end;

destructor TIpLink.Destroy();
begin
  Disconnect();
  inherited Destroy();
end;

function TIpLink.StartReader(): boolean;
begin
  // Set addresses
  Self.LinkHost:=Socket.GetRemoteSinIP();
  Self.LinkPort:=IntToStr(Socket.GetRemoteSinPort());
  RemoteInfo.Name:=IpProto+' link '+LinkHost+':'+LinkPort;
  Self.OnIncomingMsg:=@Mgr.IncomingMsgHandler;

  Reader:=TSockReader.Create(Self.Socket, @ReaderEventHandler);
//  Reader.FreeOnTerminate:=true;
//  Reader.Socket:=self.Socket;
//  Reader.OnEvent:=ReaderEventHandler;
  Reader.Suspended:=False;
  //FActive:=true;
  Result:=True;
end;

function TIpLink.Connect(): boolean;
begin
  Result:=false;
  if Assigned(Socket) then FreeAndNil(Socket);
  if (IpProto = 'TCP') then
    Socket:=TTCPBlockSocket.Create()
  else if (IpProto = 'UDP') then
    Socket:=TUDPBlockSocket.Create();

  IdleTimestamp:=Now();
  Socket.Connect(self.LinkHost, self.LinkPort);
  if Socket.LastError<>0 then
  begin
    Exit;
  end;
  FActive:=StartReader();
  RemoteInfo.State:=TDnmpContactState.asOnline;
  Mgr.DebugText('IpLink.Connect() OK');
  if Assigned(OnConnect) then OnConnect(Self);
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
  Result:=True;
end;

function TIpLink.Listen(): boolean;
begin
  Result:=inherited Listen();
  if not Result then Exit;
  Result:=False;
  if Assigned(Socket) then FreeAndNil(Socket);
  if self.LinkHost='' then self.LinkHost:='0.0.0.0';

  IdleTimestamp:=Now();
  if (IpProto = 'TCP') then
  begin
    Socket:=TTCPBlockSocket.Create();
    IgnoreTimeout:=False;
    Socket.bind(self.LinkHost, self.LinkPort);
    if Socket.LastError<>0 then
    begin
      Exit;
    end;
    Listener:=TSockListener.Create(TTCPBlockSocket(self.Socket), @ListenerEventHandler);
    Listener.Suspended:=False;
  end
  else if (IpProto = 'UDP') then
  begin
    Socket:=TUDPBlockSocket.Create();
    IgnoreTimeout:=True;
    Socket.bind(self.LinkHost, self.LinkPort);
    if Socket.LastError<>0 then
    begin
      Exit;
    end;
    StartReader();
  end;
  RemoteInfo.Name:=IpProto+' listener '+LinkHost+':'+LinkPort;

  FActive:=true;
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
  Result:=true;
end;

function TIpLink.Disconnect(): boolean;
begin
  Result:=False;
  Mgr.DebugText('IpLink.Disconnect()');
  if not Active then Exit;
  FActive:=False;
  RemoteInfo.State:=asOffline;
  IdleTimestamp:=Now();
  if Assigned(Reader) then
  begin
    //Reader.OnEvent:=nil;
    Reader.Terminate();
    //Reader.WaitFor();
    Reader:=nil;
    //FreeAndNil(Reader);
    Mgr.DebugText('IpLink.Reader terminated');
  end;
  if Assigned(Listener) then
  begin
    //Listener.OnEvent:=nil;
    Listener.Terminate();
    //Listener.WaitFor();
    //FreeAndNil(Listener);
    Listener:=nil;
    //FreeAndNil(Listener);
    Mgr.DebugText('IpLink.Listener terminated');
  end;
  //if Assigned(Socket) then FreeAndNil(Socket);
  Mgr.DebugText('IpLink.Disconnect() OK');
  if Assigned(OnDisconnect) then OnDisconnect(Self);
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
  Result:=True;
end;

function TIpLink.Check(): boolean;
begin
  Result:=True;
  if not Active and Incoming then Result:=False;
//  if (Now()-IdleTimestamp) > EncodeTime(0,60,0,0) then
//  begin
//
//  end;  
end;

{function TIpLink.SendMsg(Msg: TRawMsg): boolean;
var
  SendStr: string;
  TotalLen: integer;
begin
  TotalLen:=4+4+4+4+8+8+4+Length(Msg.MsgBody);
  SendStr:='';
  SendStr:=DWordToStr(TotalLen)
  +DWordToStr(Msg.MsgType)
  +DWordToStr(Msg.Timestamp.Date)
  +DWordToStr(Msg.SrcAddr.Node)
  +DWordToStr(Msg.SrcAddr.Point)
  +DWordToStr(Msg.DstAddr.Node)
  +DWordToStr(Msg.DstAddr.Point)
  +DWordToStr(Length(Msg.MsgBody))
  +Msg.MsgBody
  ;
  self.Socket.SendString(SendStr);
end;}

function TIpLink.SendMsg(Msg: TDnmpMsg): boolean;
var
  ms: TMemoryStream;
begin
  Result:=False;
  if Assigned(Listener) then Exit; // listener can't send messages
  Result := inherited SendMsg(Msg);
  if Result then
  begin
    ms:=TMemoryStream.Create();
    Msg.ToStream(ms);
    ms.Seek(0, soFromBeginning);
    if Assigned(Self.Socket) then
    begin
      try
        Self.Socket.SendStream(ms);
      except
        Mgr.DebugText('IpLink error: '+IntToStr(Self.Socket.LastError)+' '+Self.Socket.LastErrorDesc);
      end;
    end;
    FreeAndNil(ms);
  end;
end;

procedure TIpLink.ReaderEventHandler(LastError: integer; Data: string);
var
  MsgIn: TDnmpMsg;
begin
  IdleTimestamp:=Now();
  if LastError<>0 then
  begin
    //if (LastError=10060) and IgnoreTimeout then Exit;
    // Ошибка
    Mgr.DebugText('IpLink Reader error: '+IntToStr(LastError)+' '+Data);
    Disconnect();
    Exit;
  end;
  MsgIn:=TDnmpMsg.Create(EmptyAddr(), EmptyAddr(), '', '', '');
  if not MsgIn.FromString(Data) then
  begin
    Mgr.DebugText('Msg error: '+Data);
  end
  else
  begin
    Mgr.DebugMsg(MsgIn, Self, '@<');
    if Assigned(OnIncomingMsg) then OnIncomingMsg(Self, MsgIn);
  end;

  //Parser.ParseMessage(MsgIn);
  MsgIn.Free();
end;

procedure TIpLink.ListenerEventHandler(Sock: TSocket; LastError: integer; Data: string);
var
  NewIpLink: TIpLink;
begin
  IdleTimestamp:=Now();
  if LastError<>0 then
  begin
    //if (LastError=10060) and IgnoreTimeout then Exit;
    // Ошибка
    Mgr.DebugText('IpLink Listener error: '+IntToStr(LastError)+' '+Data);
    Disconnect();
    Exit;
  end;

  NewIpLink:=TIpLink.Create(Mgr, nil);
  NewIpLink.Socket:=TTCPBlockSocket.Create();
  NewIpLink.Socket.Socket:=Sock;
  //NewIpLink.OnIncomingMsg:=Mgr.OnIncomingMsg;
  NewIpLink.Incoming:=True;
  NewIpLink.LinkType:=ltIncoming;
  NewIpLink.FActive:=NewIpLink.StartReader();
  if NewIpLink.Active then
    NewIpLink.RemoteInfo.State:=asOnline
  else
    NewIpLink.RemoteInfo.State:=asOffline;

  Mgr.AddLink(NewIpLink);
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
end;


end.

