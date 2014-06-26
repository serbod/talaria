unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dnmp_unit, dnmp_services, PointListFrame, Misc;

type

  { TServiceDnmpNode }

  TServiceDnmpNode = class(TObject)
  private
    FOnLog: TGetStrProc;
    FOnEvent: TMgrEvent;
    FOnIncomingMsg: TIncomingMsgEvent;
    procedure UpdateInfo();
    procedure LogHandler(Sender: TObject; LogMsg: string);
    procedure EventHandler(Sender, Text: string);
    procedure MsgHandler(Sender: TObject; Msg: TDnmpMsg);
  public
    AppName: string;
    Mgr: TDnmpManager;
    ServMgr: TDnmpServiceManager;
    Frame: TFrame;
    constructor Create(ConfigName: string);
    destructor Destroy; override;
    property OnLog: TGetStrProc read FOnLog write FOnLog;
    property OnEvent: TMgrEvent read FOnEvent write FOnEvent;
    property OnIncomingMsg: TIncomingMsgEvent read FOnIncomingMsg write FOnIncomingMsg;
  end;

implementation

{ TServiceDnmpNode }

procedure TServiceDnmpNode.UpdateInfo();
begin
  if not Assigned(Mgr) then Exit;
  AppName:=Mgr.MyInfo.AddrStr();
  if Assigned(Frame) then Frame.Update();
end;

procedure TServiceDnmpNode.LogHandler(Sender: TObject; LogMsg: string);
begin
  if Assigned(OnLog) then OnLog(LogMsg);
end;

procedure TServiceDnmpNode.EventHandler(Sender, Text: string);
var
  sCmd, sParam: string;
begin
  if Sender='MGR' then
  begin
    ExtractCmd(Text, sCmd, sParam);

    if Text='REFRESH' then
    begin
      //UpdateInfo();
    end

    else if sCmd='UPDATE' then
    begin
      if sParam='SRVD' then
      begin
        //FrameSRVD.UpdateData();
      end
      else if sParam='LINKS' then
      begin
        //FrameLinks.UpdateView();
      end
      else if sParam='CONTACTS' then
      begin
        //if Assigned(FrameContactsList) then FrameContactsList.UpdateView();
      end
      else if sParam='NODELIST' then
      begin
        //FrameNodesList.UpdateView();
      end
      else if sParam='POINTLIST' then
      begin
        //FramePointsList.UpdateView();
      end
      else if sParam='ROUTING' then
      begin
        //FrameRouting.UpdateData();
      end;
    end;
  end

  else if Sender='GRPC' then
  begin
    ExtractCmd(Text, sCmd, sParam);

    if sCmd='OPEN' then
    begin
      //OpenGrpcChannel(sParam);
    end;
  end;

  if Assigned(OnEvent) then OnEvent(Sender, Text);
end;

procedure TServiceDnmpNode.MsgHandler(Sender: TObject; Msg: TDnmpMsg);
begin
  if Assigned(OnIncomingMsg) then OnIncomingMsg(Sender, Msg);
end;

constructor TServiceDnmpNode.Create(ConfigName: string);
var
  s: string;
begin
  inherited Create();
  s:=ConfigName;
  //sDnmpDataDir:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+sDnmpDataDir;
  //if ParamCount>0 then s:=ParamStr(1);
  AppName:=s;
  Mgr:=TDnmpManager.Create(ConfigName);
  Mgr.OnLog:=@LogHandler;
  Mgr.OnEvent:=@EventHandler;
  Mgr.OnIncomingMsg:=@MsgHandler;

  ServMgr:=TDnmpServiceManager.Create(Mgr);
end;

destructor TServiceDnmpNode.Destroy();
begin
  FreeAndNil(ServMgr);
  Mgr.OnLog:=nil;
  Mgr.OnEvent:=nil;
  Mgr.OnIncomingMsg:=nil;
  FreeAndNil(Mgr);
  inherited Destroy();
end;

end.

