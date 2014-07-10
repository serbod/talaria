unit GrpcServiceFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ActnList, Menus,
  ExtCtrls, dnmp_grpc, dnmp_unit, dnmp_services;

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
    actSay: TAction;
    actLeaveUser: TAction;
    actJoinUser: TAction;
    actKickUser: TAction;
    actUnbanUser: TAction;
    alGrpcService: TActionList;
    btnSay: TButton;
    edSay: TEdit;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    panSay: TPanel;
    pmUsersList: TPopupMenu;
    pmBanList: TPopupMenu;
    pmAbonentList: TPopupMenu;
    pmMessagesList: TPopupMenu;
    procedure actBanUserExecute(Sender: TObject);
    procedure actJoinUserExecute(Sender: TObject);
    procedure actKickUserExecute(Sender: TObject);
    procedure actSayExecute(Sender: TObject);
    procedure actUnbanUserExecute(Sender: TObject);
    procedure edTopicEditingDone(Sender: TObject);
  private
    { private declarations }
    FGrpc: TDnmpGrpc;
    procedure FSetGrpc(Value: TDnmpGrpc);
    function FGetSelectedUser(): TDnmpAbonent;
    function FGetSelectedAbonent(): TDnmpAbonent;
    function FGetSelectedBan(): TGrpcBanItem;
  public
    { public declarations }
    property Grpc: TDnmpGrpc read FGrpc write FSetGrpc;
    property SelectedUser: TDnmpAbonent read FGetSelectedUser;
    property SelectedAbonent: TDnmpAbonent read FGetSelectedAbonent;
    property SelectedBan: TGrpcBanItem read FGetSelectedBan;
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFrameGrpcService }

procedure TFrameGrpcService.edTopicEditingDone(Sender: TObject);
begin
  if not Assigned(Grpc) then Exit;
  Grpc.ParseCmd('SET_TOPIC '+Trim(edTopic.Text), NewAddr());
end;

procedure TFrameGrpcService.actBanUserExecute(Sender: TObject);
var
  Abonent: TDnmpAbonent;
  s: string;
begin
  Abonent:=SelectedUser;
  if Assigned(Abonent) then
  begin
    s:=Grpc.BanUser(Abonent.GUID);
    if s='' then Update();
  end;
end;

procedure TFrameGrpcService.actJoinUserExecute(Sender: TObject);
begin
  if Assigned(SelectedAbonent) then Grpc.JoinAbonent(SelectedAbonent.GUID);
  Update();
end;

procedure TFrameGrpcService.actKickUserExecute(Sender: TObject);
begin
  if Assigned(SelectedUser) then Grpc.KickAbonent(SelectedUser.GUID);
  Update();
end;

procedure TFrameGrpcService.actSayExecute(Sender: TObject);
var
  s: string;
begin
  s:=Trim(edSay.Text);
  if s<>'' then Grpc.SayText(Grpc.Author.GUID, s);
end;

procedure TFrameGrpcService.actUnbanUserExecute(Sender: TObject);
begin
  if Assigned(SelectedBan) then Grpc.UnbanAbonent(SelectedBan.AbonentGUID);
  Update();
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
    //Grpc.OnEvent:=;
  end;
  Update();
end;

function TFrameGrpcService.FGetSelectedUser(): TDnmpAbonent;
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to lboxUsersList.Count-1 do
  begin
    if lboxUsersList.Selected[i] then
    begin
      Result:=(lboxUsersList.Items.Objects[i] as TDnmpAbonent);
      Exit;
    end;
  end;
end;

function TFrameGrpcService.FGetSelectedAbonent: TDnmpAbonent;
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to lboxAbonentsList.Count-1 do
  begin
    if lboxAbonentsList.Selected[i] then
    begin
      Result:=(lboxAbonentsList.Items.Objects[i] as TDnmpAbonent);
      Exit;
    end;
  end;
end;

function TFrameGrpcService.FGetSelectedBan: TGrpcBanItem;
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to lboxBanList.Count-1 do
  begin
    if lboxBanList.Selected[i] then
    begin
      Result:=(lboxBanList.Items.Objects[i] as TGrpcBanItem);
      Exit;
    end;
  end;
end;

procedure TFrameGrpcService.Update();
var
  i: integer;
  Abon: TDnmpAbonent;
  BanItem: TGrpcBanItem;
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
      BanItem:=Grpc.BanList.GetItem(i);
      Abon:=Grpc.GetAbonentByGUID(BanItem.AbonentGUID);
      if Assigned(Abon) then
        lboxBanList.AddItem(AddrToStr(Abon.Addr)+': '+Abon.Nick, BanItem)
      else
        lboxBanList.AddItem(BanItem.AbonentGUID+': '+BanItem.Reason, BanItem);
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

