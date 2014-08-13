unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dnmp_unit, dnmp_services, dnmp_grpc, PointListFrame,
  Misc, dnmp_mail;

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

  TContactItem = class(TCollectionItem)
  public
    Caption: string;
    IsGroup: boolean;
    StateIcon: integer;
    ParentItem: TContactItem;
    DataObject: TObject;
  end;

  { TContactItemList }

  TContactItemList = class(TCollection)
    procedure AddContact(ACaption: string; AParentItem: TContactItem; ADataObject: TObject; AIsGroup: boolean = False);
    procedure ClearAll();
    function GetByData(ADataObject: TObject): TContactItem;
  end;

  { TChatRoom }

  TChatRoom = class(TInterfacedObject)
  private
    FContactItemList: TContactItemList;
  public
    DataObject: TObject;
    Name: string;
    constructor Create();
    destructor Destroy(); override;
    property ContactItemList: TContactItemList read FContactItemList;
  end;

  { TMailMessage }

  TMailMessage = class(TInterfacedObject)
  protected
    function FGetTopic(): string; virtual;
    function FGetText(): string; virtual;
  public
    DataObject: TObject;
    property Topic: string read FGetTopic;
    property Text: string read FGetText;
  end;

  { TMailBox }

  TMailBox = class(TInterfacedObject)
  protected
    function FGetName(): string; virtual;
    function FGetParent(): TMailBox; virtual;
  public
    DataObject: TObject;
    property Parent: TMailBox read FGetParent;
    property Name: string read FGetName;
    function MessagesCount(): integer; virtual;
    function UnreadMessagesCount(): integer; virtual;
    function GetMessage(Index: integer): TMailMessage; virtual;
    function IsGroup(): boolean; virtual;
  end;

  { TMailRoom }

  TMailRoom = class(TInterfacedObject)
  public
    DataObject: TObject;
    function MailboxCount(): integer; virtual;
    function GetMailbox(Index: integer): TMailBox; virtual;
  end;

var
  MainFormPages: TMainFormPages;
  ServiceDnmpNode: TServiceDnmpNode;

procedure Init();
procedure AddPage(AFrame: TFrame; ACaption: string);
procedure AddServicePage(AService: TDnmpService);


const
  ciIconFolder = 9;
  ciIconNote = 18;
  ciIconUser = 20;

implementation

uses StatusFrame, ChatFrame, DnmpNodeFrame, GrpcServiceFrame, MainForm,
  MailboxFrame;

procedure Init();
var
  frame: TFrame;
begin

  if not Assigned(MainFormPages) then Exit;
  MainFormPages.ClearAll();

  // node
  // TODO: clearing
  if Assigned(ServiceDnmpNode) then FreeAndNil(ServiceDnmpNode);
  ServiceDnmpNode:=TServiceDnmpNode.Create('1.0');
  frame:=TFrameDnmpNode.Create(nil);
  ServiceDnmpNode.Frame:=frame;
  (frame as TFrameDnmpNode).Mgr:=ServiceDnmpNode.Mgr;
  (frame as TFrameDnmpNode).ServMgr:=ServiceDnmpNode.ServMgr;
  Core.AddPage(frame, 'Node');

  // status page
  AddPage(TFrameStatus.Create(nil), 'Status');
  // chat page
  AddPage(TFrameChat.Create(nil), 'Chat');

  // mail page
  frame:=TFrameMailbox.Create(nil);
  (frame as TFrameMailbox).MailRoom:=TMailRoom.Create();
  (frame as TFrameMailbox).MailRoom.DataObject:=ServiceDnmpNode.ServMgr.GetService('MAIL','');
  (frame as TFrameMailbox).Update();
  AddPage(frame, 'Mail');
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

{ TMailRoom }

function TMailRoom.MailboxCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMail) then
  begin
    Result:=1;
  end;
end;

function TMailRoom.GetMailbox(Index: integer): TMailBox;
var
  TmpItem: TDnmpMailMessagesList;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMail) then
  begin
    TmpItem:=(DataObject as TDnmpMail).MessagesList;
    if not Assigned(TmpItem) then Exit;
    Result:=TMailBox.Create();
    Result.DataObject:=TmpItem;
  end;
end;

{ TMailBox }

function TMailBox.FGetName(): string;
begin
  Result:='Mailbox';
  if not Assigned(DataObject) then Exit;

end;

function TMailBox.FGetParent(): TMailBox;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;

end;

function TMailBox.MessagesCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessagesList) then Result:=(DataObject as TDnmpMailMessagesList).Count;
end;

function TMailBox.UnreadMessagesCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
end;

function TMailBox.GetMessage(Index: integer): TMailMessage;
var
  TmpItem: TDnmpMailMessage;
begin
  Result:=nil;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessagesList) then
  begin
    TmpItem:=(DataObject as TDnmpMailMessagesList).Items[Index];
    if not Assigned(TmpItem) then Exit;
    Result:=TMailMessage.Create();
    Result.DataObject:=TmpItem;
  end;
end;

function TMailBox.IsGroup(): boolean;
begin
  Result:=False;
end;

{ TMailMessage }

function TMailMessage.FGetTopic(): string;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessage) then Result:=(DataObject as TDnmpMailMessage).Topic;
end;

function TMailMessage.FGetText(): string;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpMailMessage) then Result:=(DataObject as TDnmpMailMessage).Text;
end;

{ TChatRoom }

constructor TChatRoom.Create();
begin
  inherited Create();
  FContactItemList:=TContactItemList.Create(TContactItem);
end;

destructor TChatRoom.Destroy();
begin
  FreeAndNil(FContactItemList);
  inherited Destroy();
end;

{ TContactItemList }

procedure TContactItemList.AddContact(ACaption: string;
  AParentItem: TContactItem; ADataObject: TObject; AIsGroup: boolean);
var
  Item: TContactItem;
begin
  Item:=(self.Add() as TContactItem);
  Item.Caption:=ACaption;
  Item.IsGroup:=AIsGroup;
  Item.ParentItem:=AParentItem;
  Item.DataObject:=ADataObject;
end;

procedure TContactItemList.ClearAll();
begin
  Clear();
end;

function TContactItemList.GetByData(ADataObject: TObject): TContactItem;
var
  i: integer;
begin
  for i:=0 to self.Count-1 do
  begin
    Result:=(self.Items[i] as TContactItem);
    if Result.DataObject=ADataObject then Exit;
  end;
  Result:=nil;
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
  sCmd:='';
  sParam:='';
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
  ServMgr.LoadFromFile();
  ServMgr.Start();
end;

destructor TServiceDnmpNode.Destroy();
begin
  ServMgr.SaveToFile();
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
  if Assigned(ServiceDnmpNode) then FreeAndNil(ServiceDnmpNode);

end.

