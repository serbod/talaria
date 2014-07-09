unit GrpcServiceFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ActnList, dnmp_grpc,
  dnmp_unit;

type

  { TFrameGrpcService }

  TFrameGrpcService = class(TFrame)
    actBanUser: TAction;
    actGetTopic: TAction;
    actGetUsers: TAction;
    actGetAbonents: TAction;
    actGetBanlist: TAction;
    actGetLastMessages: TAction;
    actGetAll: TAction;
    actGetMode: TAction;
    actLeaveUser: TAction;
    actJoinUser: TAction;
    actKickUser: TAction;
    actUnbanUser: TAction;
    alGrpcService: TActionList;
    edTopic: TEdit;
    gbAbonList: TGroupBox;
    gbUsersList: TGroupBox;
    gbBanList: TGroupBox;
    gbMessagesList: TGroupBox;
    lbName: TLabel;
    lbNamelb: TLabel;
    lboxBanList: TListBox;
    lboxAbonentsList: TListBox;
    lbTopic: TLabel;
    lboxUsersList: TListBox;
    memoMessages: TMemo;
    procedure edTopicEditingDone(Sender: TObject);
  private
    { private declarations }
    FGrpc: TDnmpGrpc;
    procedure FSetGrpc(Value: TDnmpGrpc);
  public
    { public declarations }
    property Grpc: TDnmpGrpc read FGrpc write FSetGrpc;
    procedure Update(); override;
  end;

implementation

uses dnmp_services;

{$R *.lfm}

{ TFrameGrpcService }

procedure TFrameGrpcService.edTopicEditingDone(Sender: TObject);
begin
  if not Assigned(Grpc) then Exit;
  Grpc.Cmd('SET_TOPIC '+Trim(edTopic.Text), NewAddr());
end;

procedure TFrameGrpcService.FSetGrpc(Value: TDnmpGrpc);
begin
  if Assigned(Grpc) then
  begin
    Grpc.OnSay:=nil;
    Grpc.OnAbonentsChange:=nil;
    Grpc.OnUsersChange:=nil;
    Grpc.OnBanlistChange:=nil;
    Grpc.OnTopicChange:=nil;
    Grpc.OnModeChange:=nil;
    Grpc.OnEvent:=nil;
  end;
  FGrpc:=Value;
  if Assigned(Grpc) then
  begin
  end;
end;

procedure TFrameGrpcService.Update();
var
  i: integer;
  Abon: TDnmpAbonent;
  ChatMsg: TDnmpChannelMessage;
begin
  if Assigned(Grpc) then
  begin
    if Assigned(Grpc.ServiceInfo) then
    begin
      lbName.Caption:=Grpc.ServiceInfo.Name;

      // abonents
      lboxAbonentsList.Clear();
      for i:=0 to Grpc.ServiceInfo.Abonents.Count-1 do
      begin
        Abon:=Grpc.ServiceInfo.Abonents[i];
        lboxAbonentsList.AddItem(AddrToStr(Abon.Addr)+': '+Abon.Nick, Abon);
      end;
    end;
    edTopic.Text:=Grpc.Topic;

    // users
    lboxUsersList.Clear();
    for i:=0 to Grpc.UsersList.Count-1 do
    begin
      Abon:=Grpc.UsersList[i];
      lboxUsersList.AddItem(AddrToStr(Abon.Addr)+': '+Abon.Nick, Abon);
    end;

    // ban list
    lboxBanList.Clear();
    for i:=0 to Grpc.BanList.Count-1 do
    begin
      Abon:=Grpc.BanList[i];
      lboxBanList.AddItem(AddrToStr(Abon.Addr)+': '+Abon.Nick, Abon);
    end;

    memoMessages.Lines.Clear();
    for i:=0 to Grpc.MessagesList.Count-1 do
    begin
      ChatMsg:=Grpc.MessagesList[i];
      memoMessages.Append(FormatDateTime('[hh:nn:ss]', ChatMsg.Timestamp)+'<'+ChatMsg.AuthorName+'> '+ChatMsg.Text);
    end;

  end;

  inherited Update();
end;

end.

