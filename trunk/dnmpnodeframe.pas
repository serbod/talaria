unit DnmpNodeFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ActnList,
  ExtCtrls, dnmp_unit, dnmp_services, LinkInfoListFrame, DnmpServicesFrame,
  ConfigFrame, LinkListFrame, LinkInfoFrame, Core, ContactListFrame;

type

  { TFrameDnmp }

  TFrameDnmp = class(TFrame)
    actContactList: TAction;
    actClient: TAction;
    actChatRoomList: TAction;
    actShowLog: TAction;
    actServer: TAction;
    actTest: TAction;
    alNode: TActionList;
    MemoStatus: TMemo;
    PageControlNode: TPageControl;
    panNode: TPanel;
    Splitter1: TSplitter;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tsConfig: TTabSheet;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tsPoints: TTabSheet;
    tsNodes: TTabSheet;
    tsLinks: TTabSheet;
    tsRouting: TTabSheet;
    tsServices: TTabSheet;
    tsMyInfo: TTabSheet;
    tsContacts: TTabSheet;
    ToolBarNode: TToolBar;
    procedure actChatRoomListExecute(Sender: TObject);
    procedure actClientExecute(Sender: TObject);
    procedure actContactListExecute(Sender: TObject);
    procedure actServerExecute(Sender: TObject);
    procedure actShowLogExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure PageControlNodeChange(Sender: TObject);
  private
    { private declarations }
    FServ: TServiceDnmp;
    FMgr: TDnmpManager;
    FServMgr: TDnmpServiceManager;
    FrameLinks: TFrameLinkList;
    FrameNodesList: TFrameLinkInfoList;
    FramePointsList: TFrameLinkInfoList;
    FrameContactsList: TFrameContactList;
    FrameServices: TFrameDnmpServices;
    FrameMyInfo: TFrameLinkInfo;
    FrameConfig: TFrameConfig;
    ConfigModels: TDnmpConf;
    //FrameMyInfo: TFrameMyInfo;
    //FrameSRVD: TFrameSRVD;
    //FrameRouting: TFrameRouting;
    procedure FSetServ(Value: TServiceDnmp);
    procedure FSetServMgr(Value: TDnmpServiceManager);
    procedure FSetMgr(Value: TDnmpManager);
    procedure MgrLogHandler(Sender: TObject; LogMsg: string);
    procedure MgrEventHandler(Sender, AText: string);
    procedure MgrMsgHandler(Sender: TObject; AMsg: TDnmpMsg);
    procedure SrvMgrEventHandler(ACmd: string; AObject: TObject);
  public
    { public declarations }
    AppName: string;
    property Serv: TServiceDnmp read FServ write FSetServ;
    property Mgr: TDnmpManager read FMgr write FSetMgr;
    property ServMgr: TDnmpServiceManager read FServMgr write FSetServMgr;
    procedure Update(); override;
    procedure AfterConstruction(); override;
    procedure OpenGrpcChannel(sName: string);
  end;

implementation

uses Misc;

{$R *.lfm}

{ TFrameDnmp }

procedure TFrameDnmp.actTestExecute(Sender: TObject);
var
  s: string;
begin
  if not Assigned(ServMgr) then Exit;
  ServMgr.ModService('GRPC','#test', 'add', '');
  ServMgr.ModService('GRPC','#test', 'set_descr', 'test description');

  ServMgr.ModService('GRPC','#test2', 'add', '');
  ServMgr.ModService('GRPC','#test2', 'set_descr', 'test2 description');
  ServMgr.ModService('GRPC','#test2', 'set_parent', '#test');

  ServMgr.ModService('GRPC','#test3', 'add', '');
  s:=Mgr.Serializer.StorageToString(ServMgr.ServiceInfoList.ToStorage());
  MemoStatus.Append(s);
  MemoStatus.Append('--------------------------------------------');
  ServMgr.ModService('GRPC','#test', 'del', '');
  ServMgr.ModService('GRPC','#test2', 'del', '');
  ServMgr.ModService('GRPC','#test3', 'del', '');
  MemoStatus.Append(Mgr.Serializer.StorageToString(ServMgr.ServiceInfoList.ToStorage()));
  MemoStatus.Append('--------------------------------------------');
  //ServMgr.ServiceInfoList.LoadFromString(s);
  s:=Mgr.Serializer.StorageToString(ServMgr.ServiceInfoList.ToStorage());
  MemoStatus.Append(s);
  MemoStatus.Append('--------------------------------------------');
end;

procedure TFrameDnmp.actContactListExecute(Sender: TObject);
begin
  ShowContactList(Mgr.ContactList);
end;

procedure TFrameDnmp.actClientExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Mgr.Active then Mgr.StopClient() else Mgr.StartClient();
  Update();
end;

procedure TFrameDnmp.actChatRoomListExecute(Sender: TObject);
begin
  if not Assigned(Serv) then Exit;
  Serv.ShowChatRoomList();
end;

procedure TFrameDnmp.actServerExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Mgr.Active then Mgr.StopServer() else Mgr.StartServer();
  Update();
end;

procedure TFrameDnmp.actShowLogExecute(Sender: TObject);
begin
  //Splitter1.Visible:=actShowLog.Checked;
  //MemoStatus.Visible:=actShowLog.Checked;
  if not actShowLog.Checked then MemoStatus.Width:=0 else MemoStatus.Width:=300;
end;

procedure TFrameDnmp.PageControlNodeChange(Sender: TObject);
begin
  if Assigned(PageControlNode.ActivePage) then
  begin
    if PageControlNode.ActivePage.ControlCount>0 then
    begin
      if (PageControlNode.ActivePage.Controls[0] is TFrame) then
      begin
        (PageControlNode.ActivePage.Controls[0] as TFrame).Update();
      end;
    end;
  end;
end;

procedure TFrameDnmp.FSetServ(Value: TServiceDnmp);
begin
  FServ:=Value;
  if Assigned(FServ) then
  begin
    Mgr:=FServ.Mgr;
    ServMgr:=FServ.ServMgr;
  end;
end;

procedure TFrameDnmp.FSetServMgr(Value: TDnmpServiceManager);
begin
  FServMgr:=Value;
  if Assigned(FrameServices) then FrameServices.SvcMgr:=ServMgr;
  if Assigned(FServMgr) then
  begin
    FServMgr.OnEvent:=@SrvMgrEventHandler;
  end;
end;

procedure TFrameDnmp.FSetMgr(Value: TDnmpManager);
begin
  //if Assigned(FMgr) then FMgr.Free();
  FMgr:=Value;
  if Assigned(FMgr) then
  begin
    Mgr.OnEvent:=@MgrEventHandler;
    Mgr.OnIncomingMsg:=@MgrMsgHandler;
    Mgr.OnLog:=@MgrLogHandler;
    if Assigned(FrameLinks) then FrameLinks.Mgr:=Mgr;
    if Assigned(FrameNodesList) then
    begin
      FrameNodesList.InfoList:=Mgr.NodeList;
      FrameNodesList.DnmpMgr:=Mgr;
    end;
    if Assigned(FramePointsList) then
    begin
      FramePointsList.InfoList:=Mgr.PointList;
      FramePointsList.DnmpMgr:=Mgr;
    end;
    if Assigned(FrameContactsList) then
    begin
      FrameContactsList.ContactList:=Mgr.ContactList;
      FrameContactsList.Mgr:=Mgr;
    end;
    if Assigned(FrameMyInfo) then FrameMyInfo.LinkInfo:=Mgr.MyInfo;
    if Assigned(FrameConfig) then
    begin
      FrameConfig.Config:=Mgr.Conf;
      { config_models }
      if Assigned(ConfigModels) then FreeAndNil(ConfigModels);
      if not FileExists(Mgr.sDataPath+'conf_models.ini') then StrToFile(Mgr.sDataPath+'conf_models.ini', '-');
      ConfigModels:=TDnmpConf.Create(Mgr.sDataPath+'conf_models.ini');
      FrameConfig.ConfigModels:=ConfigModels;
    end;
  end;
  Update();
end;

procedure TFrameDnmp.MgrLogHandler(Sender: TObject; LogMsg: string);
begin
  MemoStatus.Append(LogMsg);
end;

procedure TFrameDnmp.MgrEventHandler(Sender, AText: string);
var
  sCmd, sParam: string;
begin
  sCmd:='';
  sParam:='';
  if Sender='MGR' then
  begin
    ExtractCmd(AText, sCmd, sParam);

    if Text='REFRESH' then
    begin
      Update();
    end

    else if sCmd='UPDATE' then
    begin
      if sParam='SRVD' then
      begin
        if Assigned(FrameServices) then FrameServices.Update();
      end
      else if sParam='LINKS' then
      begin
        FrameLinks.Update();
      end
      else if sParam='CONTACTS' then
      begin
        if Assigned(FrameContactsList) then FrameContactsList.Update();
      end
      else if sParam='NODELIST' then
      begin
        if Assigned(FrameNodesList) then FrameNodesList.Update();
      end
      else if sParam='POINTLIST' then
      begin
        if Assigned(FramePointsList) then FramePointsList.Update();
      end
      else if sParam='ROUTING' then
      begin
        //FrameRouting.UpdateData();
      end;
    end;
  end

  else if Sender=csGRPC then
  begin
    ExtractCmd(Text, sCmd, sParam);

    if sCmd='OPEN' then
    begin
      OpenGrpcChannel(sParam);
    end;
  end;
end;

procedure TFrameDnmp.MgrMsgHandler(Sender: TObject; AMsg: TDnmpMsg);
begin

end;

procedure TFrameDnmp.SrvMgrEventHandler(ACmd: string; AObject: TObject);
begin
  MemoStatus.Append('SRVD: '+ACmd);
end;

procedure TFrameDnmp.Update();
begin
  if Assigned(Mgr) then
  begin
    AppName:=Mgr.MyInfo.AddrStr();
    actServer.Checked:=(Mgr.ServerMode and Mgr.Active);
    actClient.Checked:=(not Mgr.ServerMode and Mgr.Active);

    if Assigned(FrameLinks) then FrameLinks.Update();
    if Assigned(FrameNodesList) then FrameNodesList.Update();
    if Assigned(FramePointsList) then FramePointsList.Update();
    if Assigned(FrameContactsList) then FrameContactsList.Update();
    if Assigned(FrameMyInfo) then FrameMyInfo.Update();
  end;
  inherited Update();
end;

procedure TFrameDnmp.AfterConstruction();
begin
  inherited AfterConstruction();
  // Links
  FrameLinks:=TFrameLinkList.Create(tsLinks);
  FrameLinks.Parent:=tsLinks;
  FrameLinks.Align:=alClient;
  // Nodes
  FrameNodesList:=TFrameLinkInfoList.Create(tsNodes);
  FrameNodesList.Parent:=tsNodes;
  FrameNodesList.Align:=alClient;
  // Points
  FramePointsList:=TFrameLinkInfoList.Create(tsPoints);
  FramePointsList.Parent:=tsPoints;
  FramePointsList.Align:=alClient;
  // Contacts
  FrameContactsList:=TFrameContactList.Create(tsContacts);
  FrameContactsList.Parent:=tsContacts;
  FrameContactsList.Align:=alClient;
  // Services
  FrameServices:=TFrameDnmpServices.Create(tsServices);
  FrameServices.Parent:=tsServices;
  FrameServices.Align:=alClient;
  // My Info
  FrameMyInfo:=TFrameLinkInfo.Create(tsMyInfo);
  FrameMyInfo.Parent:=tsMyInfo;
  FrameMyInfo.Align:=alClient;
  // Config
  FrameConfig:=TFrameConfig.Create(tsConfig);
  FrameConfig.Parent:=tsConfig;
  FrameConfig.Align:=alClient;
end;

procedure TFrameDnmp.OpenGrpcChannel(sName: string);
var
  ServiceInfo: TDnmpServiceInfo;
  SomeService: TDnmpService;
begin
  if not Assigned(ServMgr) then Exit;
  SomeService:=ServMgr.ServiceList.GetService(csGRPC, sName);
  if not Assigned(SomeService) then
  begin
    ServiceInfo:=ServMgr.ServiceInfoList.GetServiceByTypeName(csGRPC, sName);
    if not Assigned(ServiceInfo) then Exit;
    SomeService:=ServMgr.CreateService(ServiceInfo);
    if Assigned(SomeService) then Core.AddServicePage(SomeService);
  end;
end;


end.

