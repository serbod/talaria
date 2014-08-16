unit DnmpServicesFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  ActnList, Menus, dnmp_unit, dnmp_services, Core, Dialogs;

type

  { TFrameDnmpServices }

  TFrameDnmpServices = class(TFrame)
    actAddGrpc: TAction;
    actDelService: TAction;
    actRunService: TAction;
    actSaveServicesList: TAction;
    actSaveInfoList: TAction;
    alServices: TActionList;
    gbOwners: TGroupBox;
    gbServicesTree: TGroupBox;
    gbServiceInfo: TGroupBox;
    lbServiceType: TLabel;
    lbTypeLabel: TLabel;
    lbServiceName: TLabel;
    lbNameLabel: TLabel;
    lvOwners: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    panService: TPanel;
    pmServices: TPopupMenu;
    Splitter1: TSplitter;
    tvServices: TTreeView;
    procedure actAddGrpcExecute(Sender: TObject);
    procedure actRunServiceExecute(Sender: TObject);
    procedure tvServicesSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FSvcMgr: TDnmpServiceManager;
    FCurServiceInfo: TDnmpServiceInfo;
    procedure FSetSvcMgr(Value: TDnmpServiceManager);
    procedure FSetCurServiceInfo(Value: TDnmpServiceInfo);
  public
    { public declarations }

    property SvcMgr: TDnmpServiceManager read FSvcMgr write FSetSvcMgr;
    property CurServiceInfo: TDnmpServiceInfo read FCurServiceInfo write FSetCurServiceInfo;
    procedure Update(); override;
  end;

implementation

uses GrpcServiceFrame;

{$R *.lfm}

{ TFrameDnmpServices }

procedure TFrameDnmpServices.tvServicesSelectionChanged(Sender: TObject);
begin
  if not Assigned(tvServices.Selected) then Exit;
  if not Assigned(tvServices.Selected.Data) then Exit;
  CurServiceInfo:=TDnmpServiceInfo(tvServices.Selected.Data);
end;

procedure TFrameDnmpServices.actRunServiceExecute(Sender: TObject);
var
  TmpService: TDnmpService;
begin
  if not Assigned(CurServiceInfo) then Exit;
  TmpService:=SvcMgr.CreateService(CurServiceInfo);
  if Assigned(TmpService) then Core.AddServicePage(TmpService);
end;

procedure TFrameDnmpServices.actAddGrpcExecute(Sender: TObject);
var
  s: string;
  TmpService: TDnmpService;
begin
  s:=InputBox('Channel name', 'Channel name: ', '');
  if Trim(s)='' then Exit;
  s:='#'+s;
  TmpService:=SvcMgr.GetService('GRPC', s, True);
  if Assigned(TmpService) then Core.AddServicePage(TmpService);
  Update();
end;

procedure TFrameDnmpServices.FSetSvcMgr(Value: TDnmpServiceManager);
begin
  FSvcMgr:=Value;
  CurServiceInfo:=nil;
  Update();
end;

procedure TFrameDnmpServices.FSetCurServiceInfo(Value: TDnmpServiceInfo);
var
  i: integer;
  Abon: TDnmpContact;
  li: TListItem;
begin
  FCurServiceInfo:=Value;
  if Assigned(FCurServiceInfo) then
  begin
    lbServiceName.Caption:=FCurServiceInfo.Name;
    lbServiceType.Caption:=FCurServiceInfo.ServiceType;

    // owners
    lvOwners.BeginUpdate();
    lvOwners.Items.Clear();

    for i:=0 to FCurServiceInfo.Owners.Count-1 do
    begin
      Abon:=FCurServiceInfo.Owners[i];
      li:=lvOwners.Items.Add();

      li.Caption:=AddrToStr(Abon.Addr);
      li.SubItems.Add(Abon.Nick);
      li.SubItems.Add(Abon.StateStr());
      li.SubItems.Add(Abon.GUID);
      li.SubItems.Add(Abon.SeniorGUID);
      li.SubItems.Add(Abon.StatusMessage);
    end;
    lvOwners.EndUpdate();

  end;
end;

procedure TFrameDnmpServices.Update();
var
  RootNode: TTreeNode;
  Item: TDnmpServiceInfo;
  i: integer;

function AddItem(ParentNode: TTreeNode; Obj: TObject; Name: string; IconIndex: integer = -1): TTreeNode;
begin
  Result:=tvServices.Items.AddChild(ParentNode, Name);
  Result.Data:=Obj;
  Result.Text:=Name;
  Result.ImageIndex:=IconIndex;
  Result.StateIndex:=IconIndex;
end;

begin
  tvServices.Items.Clear();
  if not Assigned(SvcMgr) then Exit;

  // srvd
  RootNode:=AddItem(nil, nil, SvcMgr.ServiceType+' '+SvcMgr.Mgr.MyInfo.AddrStr(), 9);
  for i:=0 to SvcMgr.ServiceTypes.Count-1 do
  begin
    AddItem(RootNode, nil, SvcMgr.ServiceTypes[i], 18);
  end;

  // service info list
  RootNode:=AddItem(nil, nil, 'info list: '+IntToStr(SvcMgr.ServiceInfoList.Count), 9);
  for i:=0 to SvcMgr.ServiceInfoList.Count-1 do
  begin
    Item:=SvcMgr.ServiceInfoList.Items[i];
    AddItem(RootNode, Item, Item.ServiceType+': '+Item.Name, 18);
  end;

  // remote service info list
  RootNode:=AddItem(nil, nil, 'remote list: '+IntToStr(SvcMgr.RemoteServiceInfoList.Count), 9);
  for i:=0 to SvcMgr.RemoteServiceInfoList.Count-1 do
  begin
    Item:=SvcMgr.RemoteServiceInfoList.Items[i];
    AddItem(RootNode, Item, Item.ServiceType+': '+Item.Name, 18);
  end;

  // service info list
  RootNode:=AddItem(nil, nil, 'services: '+IntToStr(SvcMgr.ServiceList.Count), 9);
  for i:=0 to SvcMgr.ServiceList.Count-1 do
  begin
    Item:=SvcMgr.ServiceList.Items[i].ServiceInfo;
    AddItem(RootNode, Item, Item.ServiceType+': '+Item.Name, 18);
  end;

  inherited Update();
end;

end.

