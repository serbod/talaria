unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dnmp_unit, dnmp_services, dnmp_grpc, PointListFrame, Misc;

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

  TMainFormPageItem = class(TCollectionItem)
  public
    Caption: string;
    Frame: TFrame;
    Visible: boolean;
    TabSheet: TObject;
  end;

  { TMainFormPages }

  TMainFormPages = class(TCollection)
    procedure AddPage(AFrame: TFrame; ACaption: string);
    procedure ClearAll;
  end;

var
  MainFormPages: TMainFormPages;
  ServiceDnmpNode: TServiceDnmpNode;

procedure Init();
procedure AddPage(AFrame: TFrame; ACaption: string);
procedure AddServicePage(AService: TDnmpService);

implementation

uses StatusFrame, ChatFrame, DnmpNodeFrame, GrpcServiceFrame, MainForm;

procedure Init();
var
  frame: TFrame;
begin

  if not Assigned(MainFormPages) then Exit;
  MainFormPages.ClearAll();

  // status page
  AddPage(TFrameStatus.Create(nil), 'Status');
  // chat page
  AddPage(TFrameChat.Create(nil), 'Chat');
  // node
  // TODO: clearing
  // if Assigned(ServiceDnmpNode) then FreeAndNil(ServiceDnmpNode);
  ServiceDnmpNode:=TServiceDnmpNode.Create('1.0');
  frame:=TFrameDnmpNode.Create(nil);
  ServiceDnmpNode.Frame:=frame;
  (frame as TFrameDnmpNode).Mgr:=ServiceDnmpNode.Mgr;
  (frame as TFrameDnmpNode).ServMgr:=ServiceDnmpNode.ServMgr;
  Core.AddPage(frame, 'Node');
end;

procedure AddPage(AFrame: TFrame; ACaption: string);
begin
  if not Assigned(MainFormPages) then Exit;
  MainFormPages.AddPage(AFrame, ACaption);
  FormMain.UpdatePages();
end;

procedure AddServicePage(AService: TDnmpService);
var
  TmpFrame: TFrame;
begin
  if not Assigned(AService) then Exit;
  if (AService is TDnmpGrpc) then
  begin
    TmpFrame:=TFrameGrpcService.Create(nil);
    (TmpFrame as TFrameGrpcService).Grpc:=(AService as TDnmpGrpc);
    AddPage(TmpFrame, AService.ServiceInfo.Name);
  end;
end;

{ TMainFormPages }

procedure TMainFormPages.AddPage(AFrame: TFrame; ACaption: string);
var
  Item: TMainFormPageItem;
begin
  Item:=(MainFormPages.Add() as TMainFormPageItem);
  Item.Frame:=AFrame;
  Item.Caption:=ACaption;
end;

procedure TMainFormPages.ClearAll();
var
  i: integer;
  Item: TMainFormPageItem;
begin
  for i:=self.Count-1 downto 0 do
  begin
    Item:=(self.Items[i] as TMainFormPageItem);
    FreeAndNil(Item.Frame);
    //FreeAndNil(Item.TabSheet);
  end;
  self.Clear();
end;

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

initialization
  MainFormPages:=TMainFormPages.Create(TMainFormPageItem);

finalization
  MainFormPages.ClearAll();
  FreeAndNil(MainFormPages);
  //if Assigned(ServiceDnmpNode) then FreeAndNil(ServiceDnmpNode);

end.

