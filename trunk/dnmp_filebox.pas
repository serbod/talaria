unit dnmp_filebox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, dnmp_unit;

type

  { TDnmpLinkFilebox }

  TDnmpLinkFilebox = class(TDnmpLink)
  private
    InboxTimer: TTimer;
    OutboxTimer: TTimer;
    OutNum: integer;
    procedure OnInboxTimerHandler(Sender: TObject);
    procedure OnOutboxTimerHandler(Sender: TObject);
    procedure ScanDir(ADirName, AFilePrefix: string; AFileNames: TStrings);
  public
    LastMsgType: integer;

    IgnoreTimeout: boolean;
    LastIncomingTime: TDateTime;
    OutboxDir: string;
    OutboxPrefix: string;
    InboxDir: string;
    InboxPrefix: string;
    { Inbox check for new files interval (milliseconds) }
    InboxCheckInterval: integer;
    constructor Create(AMgr: TDnmpManager; ALinkInfo: TDnmpContact = nil); override;
    destructor Destroy(); override;

    function Connect(): boolean; override;
    function Listen(): boolean; override;
    function Disconnect(): boolean; override;
    function Check(): boolean; override;
    function SendMsg(Msg: TDnmpMsg): boolean; override;
  end;

implementation

{ TDnmpLinkFilebox }

procedure TDnmpLinkFilebox.OnInboxTimerHandler(Sender: TObject);
var
  sl: TStringList;
  i: integer;
  fs: TFileStream;
  MsgIn: TDnmpMsg;
begin
  sl:=TStringList.Create();
  try
    ScanDir(InboxDir, InboxPrefix, sl);
    for i:=0 to sl.Count-1 do
    begin
      LastIncomingTime:=Now();
      // read msg from file
      MsgIn:=TDnmpMsg.Create(EmptyAddr(), EmptyAddr(), '', '', '');
      fs:=TFileStream.Create(sl[i], fmOpenRead);
      MsgIn.FromStream(fs);
      fs.Free();
      // proccess msg
      if Assigned(OnIncomingMsg) then OnIncomingMsg(Self, MsgIn);
      // dispose file
      DeleteFile(sl[i]);
    end;
  finally
    sl.Free();
  end;
end;

procedure TDnmpLinkFilebox.OnOutboxTimerHandler(Sender: TObject);
begin

end;

procedure TDnmpLinkFilebox.ScanDir(ADirName, AFilePrefix: string;
  AFileNames: TStrings);
var
  sr: TSearchRec;
  n: integer;
begin
  n:=FindFirst(IncludeTrailingPathDelimiter(ADirName)+AFilePrefix+'*.msg', faAnyFile, sr);
  while n=0 do
  begin
    AFileNames.Append(sr.Name);
    n:=FindNext(sr);
  end;
  FindClose(sr);
end;

constructor TDnmpLinkFilebox.Create(AMgr: TDnmpManager; ALinkInfo: TDnmpContact
  );
begin
  inherited Create(AMgr, ALinkInfo);
  if Assigned(Mgr) then
  begin
    OutboxDir:=Mgr.sDataPath+'';
    InboxDir:=Mgr.sDataPath+'';
  end;
  OutboxPrefix:='out';
  InboxPrefix:='in';
  InboxTimer:=TTimer.Create(nil);
  InboxTimer.OnTimer:=@OnInboxTimerHandler;
  InboxCheckInterval:=1000;
  OutboxTimer:=TTimer.Create(nil);
  OutboxTimer.OnTimer:=@OnOutboxTimerHandler;
  OutNum:=0;
end;

destructor TDnmpLinkFilebox.Destroy;
begin
  OutboxTimer.Enabled:=False;
  OutboxTimer.Free();
  InboxTimer.Enabled:=False;
  InboxTimer.Free();
  inherited Destroy;
end;

function TDnmpLinkFilebox.Connect: boolean;
begin
  //Result:=inherited Connect;
  FActive:=True;
  RemoteInfo.State:=TDnmpContactState.asOnline;
  InboxTimer.Interval:=InboxCheckInterval;
  InboxTimer.Enabled:=True;
  Mgr.DebugText('FileboxLink.Connect() OK');
  if Assigned(OnConnect) then OnConnect(Self);
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
  Result:=True;
end;

function TDnmpLinkFilebox.Listen: boolean;
begin
  //Result:=inherited Listen;
  Result:=False;
end;

function TDnmpLinkFilebox.Disconnect: boolean;
begin
  //Result:=inherited Disconnect;
  if not Active then Exit;
  FActive:=False;
  RemoteInfo.State:=asOffline;
  Mgr.DebugText('FileboxLink.Disconnect() OK');
  if Assigned(OnDisconnect) then OnDisconnect(Self);
  Mgr.Cmd('EVENT MGR UPDATE LINKS');
  Result:=True;
end;

function TDnmpLinkFilebox.Check: boolean;
begin
  //Result:=inherited Check;
  Result:=True;
end;

function TDnmpLinkFilebox.SendMsg(Msg: TDnmpMsg): boolean;
var
  sFileName: string;
  fs: TFileStream;
  n: integer;
begin
  n:=0;
  Result:=False;
  while n<10 do
  begin
    sFileName:=IncludeTrailingPathDelimiter(OutboxDir)+OutboxPrefix+IntToStr(OutNum)+'.msg');
    Inc(OutNum);
    Inc(n);
    if FileExists(sFileName) then Continue;
    Result:=StrToFile(sFileName, Msg.ToString());
    Exit;
  end;
end;

end.

