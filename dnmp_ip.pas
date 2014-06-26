unit dnmp_ip;

interface
uses {Windows,} SysUtils, Classes, blcksock, synsock, dnmp_unit;

type
  TSockReaderEvent = procedure(LastError: integer; Data: string) of object;
  TSockReader = class(TThread)
  private
    Event: TSockReaderEvent;
  public
    Str: string;
    //Socket: TTCPBlockSocket;
    Socket: TBlockSocket;
    constructor Create(BSock: TBlockSocket; EventHandler: TSockReaderEvent);
    destructor Destroy(); override;
    property OnEvent: TSockReaderEvent read Event write Event;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  end;

  TSockListenerEvent = procedure(Sock: TSocket; LastError: integer; Data: string) of object;
  TSockListener = class(TThread)
  private
    Event: TSockListenerEvent;
  public
    Str: string;
    Socket: TTCPBlockSocket;
    //Socket: TBlockSocket;
    NewSocket: TSocket;
    constructor Create(BSock: TTCPBlockSocket; EventHandler: TSockListenerEvent);
    destructor Destroy(); override;
    property OnEvent: TSockListenerEvent read Event write Event;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  end;

  // NOT USED, see TIpLink
  TIpListener = class(TDnmpListener)
  public
    Socket: TBlockSocket;
    Listener: TSockListener;
    IpProto: string;
    LinkHost: string;
    LinkPort: string;
    function Start(): boolean; override;
    function Stop(): boolean; override;
    procedure ListenerEventHandler(Sock: TSocket; LastError: integer; Data: string);
  end;

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
    constructor Create(AMgr: TDnmpManager; sHost, sPort, sProto: string);
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

// === TSockReader ===
constructor TSockReader.Create(BSock: TBlockSocket; EventHandler: TSockReaderEvent);
begin
  FreeOnTerminate:=true;
  Socket:=BSock;
  OnEvent:=EventHandler;
  inherited Create(false);
end;

destructor TSockReader.Destroy();
begin
  if Assigned(Socket) then FreeAndNil(Socket);
  Event:=nil;
  inherited Destroy();
end;

procedure TSockReader.SyncProc();
begin
  if Str='' then Exit;
  if Assigned(Event) then Event(Socket.LastError, Str);
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
  inherited Create(false);
end;

destructor TSockListener.Destroy();
begin
  if Assigned(Socket) then FreeAndNil(Socket);
  Event:=nil;
  inherited Destroy();
end;

procedure TSockListener.SyncProc();
begin
  //if Str='' then Exit;
  if Assigned(Event) then Event(NewSocket, Socket.LastError, Str);
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
constructor TIpLink.Create(AMgr: TDnmpManager; sHost, sPort, sProto: string);
begin
  inherited Create(AMgr);
  Self.LinkHost:=sHost;
  Self.LinkPort:=sPort;
  Self.IpProto:=sProto;
  if IpProto='' then IpProto:='IP';

  LinkInfo.Addr:=MyInfo.Addr;
  LinkInfo.Name:=IpProto+' link '+LinkHost+':'+LinkPort;
  LinkInfo.Owner:=MyInfo.Owner;
  LinkInfo.IpAddr:='localhost';
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
  LinkInfo.Name:=IpProto+' link '+LinkHost+':'+LinkPort;
  Self.OnIncomingMsg:=@Mgr.IncomingMsgHandler;

  Reader:=TSockReader.Create(Self.Socket, @ReaderEventHandler);
//  Reader.FreeOnTerminate:=true;
//  Reader.Socket:=self.Socket;
//  Reader.OnEvent:=ReaderEventHandler;
//  Reader.Resume();
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
  LinkInfo.Online:=Active;
  Result:=true;
end;

function TIpLink.Listen(): boolean;
begin
  Result:=false;
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
  LinkInfo.Name:=IpProto+' listener '+LinkHost+':'+LinkPort;

  FActive:=true;
  Result:=true;
end;

function TIpLink.Disconnect(): boolean;
begin
  Result:=false;
  Mgr.DebugText('IpLink.Disconnect()');
  if not Active then Exit;
  FActive:=false;
  LinkInfo.Online:=Active;
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
  Mgr.Cmd('REFRESH');
  Result:=true;
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
  MsgIn:=TDnmpMsg.Create(NewAddr(), NewAddr(), '', '', '');
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
  MsgIn: TDnmpMsg;
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

  NewIpLink:=TIpLink.Create(Mgr, '', '', 'TCP');
  NewIpLink.Socket:=TTCPBlockSocket.Create();
  NewIpLink.Socket.Socket:=Sock;
  //NewIpLink.OnIncomingMsg:=Mgr.OnIncomingMsg;
  NewIpLink.Incoming:=True;
  NewIpLink.FActive:=NewIpLink.StartReader();
  NewIpLink.LinkInfo.Online:=NewIpLink.Active;

//  { TODO : Это для отладки, можно убрать }
//  MsgOut:=TDnmpMsg.Create(MyInfo.Addr, NewIpLink.LinkInfo.Addr, 'INFO', 'CMD=Hello', 'Hello!');
//  NewIpLink.SendMsg(MsgOut);
//  MsgOut.Free();

  Mgr.AddLink(NewIpLink);
  Mgr.Cmd('REFRESH');
end;

// ===================
// === TIpListener     ===
// ===================
function TIpListener.Start(): boolean;
begin
  Result:=false;
  if Assigned(Socket) then FreeAndNil(Socket);
  if self.LinkHost='' then self.LinkHost:='0.0.0.0';

  if (IpProto = 'TCP') then
  begin
    Socket:=TTCPBlockSocket.Create();
    //IgnoreTimeout:=False;
    Socket.bind(self.LinkHost, self.LinkPort);
    if Socket.LastError<>0 then
    begin
      Exit;
    end;
    Listener:=TSockListener.Create(TTCPBlockSocket(self.Socket), @ListenerEventHandler);
  end
  else if (IpProto = 'UDP') then
  begin
    Socket:=TUDPBlockSocket.Create();
    //IgnoreTimeout:=True;
    Socket.bind(self.LinkHost, self.LinkPort);
    if Socket.LastError<>0 then
    begin
      Exit;
    end;
    //StartReader();
  end;

  Active:=true;
  Result:=true;
end;

function TIpListener.Stop(): boolean;
begin
  Listener.Terminate();
  Listener.WaitFor();
  FreeAndNil(Socket);
  Result:=True;
end;

procedure TIpListener.ListenerEventHandler(Sock: TSocket; LastError: integer; Data: string);
var
  NewIpLink: TIpLink;
begin
  if LastError<>0 then
  begin
    // Ошибка
    Mgr.DebugText('IpListener error: '+IntToStr(LastError)+' '+Data);
    //Disconnect();
    Exit;
  end;
  Mgr.DebugText('Incoming connection: '+IntToStr(Sock)+' '+Data);

  NewIpLink:=TIpLink.Create(Mgr, '','','TCP');
  NewIpLink.Socket:=TTCPBlockSocket.Create();
  NewIpLink.Socket.Socket:=Sock;
  NewIpLink.Incoming:=True;
  NewIpLink.StartReader();

  if Assigned(OnIncomingLink) then OnIncomingLink(Self, NewIpLink)
  else NewIpLink.Free();
  //Mgr.LinkList.Add(NewIpLink);
end;

end.

