unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dnmp_unit, dnmp_services, dnmp_grpc, LinkInfoListFrame,
  Misc, dnmp_mail, Controls, dnmp_serializers, Graphics;

type

  TMainFormPageItem = class(TCollectionItem)
  public
    Caption: string;
    Frame: TFrame;
    Visible: boolean;
    TabSheet: TObject;
    DataObject: TObject;
  end;

  { TMainFormPages }

  TMainFormPages = class(TCollection)
    procedure AddPage(AFrame: TFrame; ACaption: string; ADataObject: TObject = nil);
    procedure ActivatePage(APageItem: TMainFormPageItem);
    procedure ClearAll;
  end;

  { TContactItem }

  TContactItem = class(TInterfacedObject)
  protected
    function FGetCaption(): string; virtual;
    function FGetHint(): string; virtual;
    function FGetPicture(): AnsiString; virtual;
  public
    DataObject: TObject;
    IsGroup: boolean;
    ParentItem: TContactItem;
    property Caption: string read FGetCaption;
    property Hint: string read FGetHint;
    property Picture: AnsiString read FGetPicture;
    function StateIcon(): integer; virtual;
  end;

  TChatText = record
    Timestamp: TDateTime;
    AuthorName: string;
    Text: string;
  end;

  { TChatRoom }
  { адаптер между фреймом чата и объектом чата }
  TChatRoom = class(TInterfacedObject)
  protected
    FDataObject: TObject;
    procedure FSetDataObject(Value: TObject);
    function FGetName(): string; virtual;
    function FGetTopic(): string; virtual;
    function FGetPicture(): AnsiString; virtual;
    function FGetContactCount(): integer; virtual;
    function FGetTextCount(): integer; virtual;
    procedure OnSayHandler(Sender: TObject); virtual;
    procedure OnContactsChangeHandler(Sender: TObject); virtual;
  public
    Frame: TFrame;
    property DataObject: TObject read FDataObject write FSetDataObject;
    property Name: string read FGetName;
    property Topic: string read FGetTopic;
    property Picture: AnsiString read FGetPicture;
    property ContactCount: integer read FGetContactCount;
    property TextCount: integer read FGetTextCount;
    function GetContact(Index: integer): TContactItem; virtual;
    function GetText(Index: integer): TChatText; virtual;
    procedure SendText(AText: string); virtual;
  end;

  { TServiceDnmp }

  TServiceDnmp = class(TObject)
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
    procedure OpenChatRoom(AChatRoomName: string);
    function ChatRoomCount(): integer;
    function GetChatRoom(Index: integer): TChatRoom;
    procedure ShowChatRoomList();
    procedure ShowContactList();
    procedure ShowSetupWizard();
    //procedure ShowPrivateChatRoom(Contact: TDnmpContact);
    procedure LoadData();
    procedure SaveData();
  end;



var
  MainFormPages: TMainFormPages;
  ServiceDnmpList: TStringList;

procedure Init(ConfigName: string);
procedure AddPage(AFrame: TFrame; ACaption: string; ADataObject: TObject);
procedure ShowForm(AFrame: TFrame; ACaption: string);
procedure PictureFromString(Picture: TPicture; sPic: AnsiString);
procedure ShrinkPhoto(Picture: TPicture; x, y: integer);
procedure SetDefaultContactPicture(Picture: TPicture);

// DNMP-specific
procedure AddServicePage(AService: TDnmpService);
procedure ShowLinkInfo(ALinkInfo: TDnmpContact);

const
  ciIconFolder = 9;
  ciIconNote = 18;
  ciIconUserBlue = 20;
  ciIconUserRed = 21;

implementation

uses StatusFrame, ChatFrame, DnmpNodeFrame, GrpcServiceFrame, MainForm,
  MailboxFrame, LinkInfoFrame, ChatRoomListFrame,
  DnmpWizardFrame, dnmp_chat, DnmpContactsFrame;

procedure Init(ConfigName: string);
var
  frame: TFrame;
  Config: TDnmpConf;
  ServiceDnmp: TServiceDnmp;
  i: integer;
  s, sName: string;
begin

  if not Assigned(MainFormPages) then Exit;
  MainFormPages.ClearAll();

  ServiceDnmpList.Clear();

  if FileExists('config.ini') then
  begin
    // read from config
    Config:=TDnmpConf.Create('config.ini');

    for i:=0 to 10 do
    begin
      s:='Item_'+IntToStr(i);
      if Config.SectionExists(s) then
      begin
        sName:=Config.ReadString(s, 'Name', 'DNMP');
        ServiceDnmp:=TServiceDnmp.Create(sName);
        frame:=TFrameDnmp.Create(nil);
        ServiceDnmp.Frame:=frame;
        (frame as TFrameDnmp).Serv:=ServiceDnmp;
        Core.AddPage(frame, sName, ServiceDnmp);
        ServiceDnmp.LoadData();
        //(frame as TFrameDnmp).Update();
        ServiceDnmpList.AddObject(s, ServiceDnmp);
        // wizard
        if ServiceDnmp.Mgr.MyInfo.Name='' then ServiceDnmp.ShowSetupWizard();
      end;
    end;
  end

  else
  begin
    // default
    ServiceDnmp:=TServiceDnmp.Create('');
    ServiceDnmp.LoadData();
    ServiceDnmpList.AddObject('DNMP', ServiceDnmp);
    frame:=TFrameDnmp.Create(nil);
    ServiceDnmp.Frame:=frame;
    (frame as TFrameDnmp).Serv:=ServiceDnmp;
    Core.AddPage(frame, 'DNMP', ServiceDnmp);
    // wizard
    if ServiceDnmp.Mgr.MyInfo.Name='' then ServiceDnmp.ShowSetupWizard();
  end;

  // node
  // TODO: clearing

  // status page
  //AddPage(TFrameStatus.Create(nil), 'Status', nil);

  // chat page
  //ServiceDnmpNode.OpenChatRoom('#test');

  // mail page
  {
  frame:=TFrameMailbox.Create(nil);
  (frame as TFrameMailbox).MailRoom:=(ServiceDnmpNode.ServMgr.GetService(csMAIL,'') as TDnmpMail);
  (frame as TFrameMailbox).Update();
  AddPage(frame, 'Mail');
  }

end;

procedure AddPage(AFrame: TFrame; ACaption: string; ADataObject: TObject);
begin
  if not Assigned(MainFormPages) then Exit;
  MainFormPages.AddPage(AFrame, ACaption, ADataObject);
  FormMain.UpdatePages();
end;

procedure ShowForm(AFrame: TFrame; ACaption: string);
var
  Form: TForm;
begin
  Form:=TForm.Create(FormMain);
  Form.Height:=AFrame.Height;
  Form.Width:=AFrame.Width;
  Form.Position:=poScreenCenter;
  Form.Caption:=ACaption;
  Form.InsertComponent(AFrame);
  AFrame.Parent:=Form;
  AFrame.Align:=alClient;
  Form.ShowModal();
end;

procedure PictureFromString(Picture: TPicture; sPic: AnsiString);
var
  ss: TStringStream;
begin
  if Length(sPic)>4 then
  begin
    ss:=TStringStream.Create(sPic);
    try
      Picture.LoadFromStream(ss);
    finally
      ss.Free();
    end;
  end;
end;

procedure ShrinkPhoto(Picture: TPicture; x, y: integer);
var
  bmp: TBitmap;
  r: TRect;
  k, kx, ky: Real;
begin
  if (Picture.Height > y)
  or (Picture.Width > x) then
  begin
    bmp:=TBitmap.Create();
    { Variant 1 }
    //bmp.Canvas.Assign(img.Canvas);

    { Variant 2 }
    kx:=Picture.Height / (x);
    ky:=Picture.Width / (y);
    k:=kx;
    if ky>k then k:=ky;
    if k<0 then Exit;

    r.Left:=0;
    r.Top:=0;
    r.BottomRight.x:=Round(Picture.Width / k);
    r.BottomRight.y:=Round(Picture.Height / k);
    bmp.Width:=r.BottomRight.x;
    bmp.Height:=r.BottomRight.y;
    bmp.Canvas.StretchDraw(r, Picture.Bitmap);

    Picture.Bitmap.Assign(bmp);
    bmp.Free();
  end;
end;

procedure SetDefaultContactPicture(Picture: TPicture);
begin
  FormMain.ImageList24.GetBitmap(4, Picture.Bitmap);
end;

procedure AddServicePage(AService: TDnmpService);
var
  i: integer;
  TmpFrame: TFrame;
  PageItem: TMainFormPageItem;
begin
  if not Assigned(AService) then Exit;
  if (AService is TDnmpGrpc) then
  begin
    // search for exists
    for i:=0 to MainFormPages.Count-1 do
    begin
      PageItem:=(MainFormPages.Items[i] as TMainFormPageItem);
      if (PageItem.Frame is TFrameGrpcService) then
      begin
        TmpFrame:=PageItem.Frame;
        if (TmpFrame as TFrameGrpcService).Grpc=AService then Break;
      end;
      PageItem:=nil;
      TmpFrame:=nil;
    end;

    if not Assigned(TmpFrame) then
    begin
      // create new page
      TmpFrame:=TFrameGrpcService.Create(nil);
      (TmpFrame as TFrameGrpcService).Grpc:=(AService as TDnmpGrpc);
      AddPage(TmpFrame, AService.ServiceInfo.Name, AService);
    end;
  end;

  if (AService is TDnmpMail) then
  begin
    // search for exists
    for i:=0 to MainFormPages.Count-1 do
    begin
      PageItem:=(MainFormPages.Items[i] as TMainFormPageItem);
      if (PageItem.Frame is TFrameMailbox) then
      begin
        TmpFrame:=PageItem.Frame;
        if (TmpFrame as TFrameMailbox).MailRoom=AService then Break;
      end;
      PageItem:=nil;
      TmpFrame:=nil;
    end;

    if not Assigned(TmpFrame) then
    begin
      // create new page
      TmpFrame:=TFrameMailbox.Create(nil);
      (TmpFrame as TFrameMailbox).MailRoom:=(AService as TDnmpMail);
      AddPage(TmpFrame, AService.ServiceInfo.Name, AService);
    end;
  end;

  if Assigned(PageItem) then
  begin
    // activate page
    FormMain.ActivatePage(PageItem);
  end;
end;

procedure ShowLinkInfo(ALinkInfo: TDnmpContact);
var
  Frame: TFrameLinkInfo;
begin
  if not Assigned(ALinkInfo) then Exit;
  Frame:=TFrameLinkInfo.Create(nil);
  Frame.LinkInfo:=ALinkInfo;
  Frame.Update();
  ShowForm(Frame, 'Link info');
end;

{ TChatRoom }

procedure TChatRoom.FSetDataObject(Value: TObject);
begin
  if Assigned(FDataObject) and (FDataObject is TDnmpGrpc) then
  begin
    (DataObject as TDnmpGrpc).OnSay:=nil;
    (DataObject as TDnmpGrpc).OnUsersChange:=nil;
  end;

  FDataObject:=Value;

  if not Assigned(FDataObject) then Exit;
  if (DataObject is TDnmpGrpc) then
  begin
    (DataObject as TDnmpGrpc).OnSay:=@OnSayHandler;
    (DataObject as TDnmpGrpc).OnUsersChange:=@OnContactsChangeHandler;
  end;
end;

function TChatRoom.FGetName(): string;
begin
  Result:='#ChatRoom';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).ServiceInfo.Name;
  if (DataObject is TDnmpServiceInfo) then Result:=(DataObject as TDnmpServiceInfo).Name;
end;

function TChatRoom.FGetTopic(): string;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).Topic;
  if (DataObject is TDnmpServiceInfo) then Result:=(DataObject as TDnmpServiceInfo).Descr;
end;

function TChatRoom.FGetPicture(): AnsiString;
begin
  Result:='';
end;

function TChatRoom.FGetContactCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).UsersList.Count;
  if (DataObject is TDnmpServiceInfo) then Result:=(DataObject as TDnmpServiceInfo).AbonentsCount;
end;

function TChatRoom.FGetTextCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).MessagesList.Count;
end;

procedure TChatRoom.OnSayHandler(Sender: TObject);
begin
  if not Assigned(Frame) then Exit;
  if (Frame is TFrameChat) then (Frame as TFrameChat).UpdateText();
end;

procedure TChatRoom.OnContactsChangeHandler(Sender: TObject);
begin
  if not Assigned(Frame) then Exit;
  if (Frame is TFrameChat) then (Frame as TFrameChat).UpdateContactList();
end;

function TChatRoom.GetContact(Index: integer): TContactItem;
begin
  Result:=TContactItem.Create();
  Result.DataObject:=(DataObject as TDnmpGrpc).UsersList.Items[Index];
end;

function TChatRoom.GetText(Index: integer): TChatText;
begin
  Result.Text:='';
  Result.AuthorName:='';
  Result.Timestamp:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then
  begin
    Result.Timestamp :=(DataObject as TDnmpGrpc).MessagesList.Items[Index].Timestamp;
    Result.AuthorName :=(DataObject as TDnmpGrpc).MessagesList.Items[Index].AuthorName;
    Result.Text :=(DataObject as TDnmpGrpc).MessagesList.Items[Index].Text;
  end;
end;

procedure TChatRoom.SendText(AText: string);
begin
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then
  begin
    (DataObject as TDnmpGrpc).SayText('', AText);
  end;
end;

{ TContactItem }

function TContactItem.FGetCaption(): string;
begin
  Result:='Contact';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpContact) then Result:=(DataObject as TDnmpContact).Name;
end;

function TContactItem.FGetHint(): string;
begin
  Result:=Caption;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpContact) then
  begin
    Result:=Result+LineEnding+AddrToStr((DataObject as TDnmpContact).Addr);
    Result:=Result+LineEnding+(DataObject as TDnmpContact).GUID;
  end;
end;

function TContactItem.FGetPicture(): AnsiString;
begin
  Result:='';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpContact) then Result:=(DataObject as TDnmpContact).Picture;
end;

function TContactItem.StateIcon(): integer;
begin
  Result:=ciIconUserBlue;
end;

{ TMainFormPages }

procedure TMainFormPages.AddPage(AFrame: TFrame; ACaption: string;
  ADataObject: TObject);
var
  Item: TMainFormPageItem;
begin
  Item:=(MainFormPages.Add() as TMainFormPageItem);
  Item.Frame:=AFrame;
  Item.Caption:=ACaption;
  Item.DataObject:=ADataObject;
end;

procedure TMainFormPages.ActivatePage(APageItem: TMainFormPageItem);
begin
  FormMain.ActivatePage(APageItem);
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

{ TServiceDnmp }

procedure TServiceDnmp.UpdateInfo();
begin
  if not Assigned(Mgr) then Exit;
  AppName:=Mgr.MyInfo.AddrStr();
  if Assigned(Frame) then Frame.Update();
end;

procedure TServiceDnmp.LogHandler(Sender: TObject; LogMsg: string);
begin
  if Assigned(OnLog) then OnLog(LogMsg);
end;

procedure TServiceDnmp.EventHandler(Sender, Text: string);
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

  else if Sender=csGRPC then
  begin
    ExtractCmd(Text, sCmd, sParam);

    if sCmd='OPEN' then
    begin
      //OpenGrpcChannel(sParam);
    end;
  end;

  if Assigned(OnEvent) then OnEvent(Sender, Text);
end;

procedure TServiceDnmp.MsgHandler(Sender: TObject; Msg: TDnmpMsg);
begin
  if Assigned(OnIncomingMsg) then OnIncomingMsg(Sender, Msg);
end;

constructor TServiceDnmp.Create(ConfigName: string);
var
  s: string;
begin
  inherited Create();
  s:=ConfigName;
  AppName:=s;
  Mgr:=TDnmpManager.Create(ConfigName);
  Mgr.OnLog:=@LogHandler;
  Mgr.OnEvent:=@EventHandler;
  Mgr.OnIncomingMsg:=@MsgHandler;
  //Mgr.Serializer:=TDnmpSerializerFpJson.Create();
  Mgr.Serializer:=TDnmpSerializerBencode.Create();
  //Mgr.LoadFromFile();
  //Mgr.Serializer:=TDnmpSerializerIni.Create();
  //Mgr.Serializer:=TDnmpSerializerBencode.Create();

  ServMgr:=TDnmpServiceManager.Create(Mgr);
  //ServMgr.LoadFromFile();
  //ServMgr.Start();
end;

destructor TServiceDnmp.Destroy();
begin
  Mgr.OnLog:=nil;
  Mgr.OnEvent:=nil;
  Mgr.OnIncomingMsg:=nil;
  ServMgr.SaveToFile();
  FreeAndNil(ServMgr);
  FreeAndNil(Mgr);
  inherited Destroy();
end;

procedure TServiceDnmp.OpenChatRoom(AChatRoomName: string);
var
  i: integer;
  NewFrame: TFrameChat;
  ChatRoom: TChatRoom;
  TmpObject: TObject;
  Grpc: TDnmpGrpc;
begin
  if Copy(AChatRoomName, 1, 1)<>'#' then Exit;
  // find for exists chat room page
  for i:=0 to MainFormPages.Count-1 do
  begin
    TmpObject:=(MainFormPages.Items[i] as TMainFormPageItem).DataObject;
    if Assigned(TmpObject) and (TmpObject is TChatRoom) then
    begin
      if (TmpObject as TChatRoom).Name=AChatRoomName then
      begin
        MainFormPages.ActivatePage((MainFormPages.Items[i] as TMainFormPageItem));
        Exit;
      end;
    end;
  end;
  // create new chat room
  Grpc:=(self.ServMgr.GetService(csGRPC, AChatRoomName, True) as TDnmpGrpc);
  Grpc.JoinAbonent(Mgr.MyInfo.GUID);
  ChatRoom:=TChatRoom.Create();
  ChatRoom.DataObject:=Grpc;
  // chat page
  NewFrame:=TFrameChat.Create(nil);
  NewFrame.ChatRoom:=ChatRoom;
  ChatRoom.Frame:=NewFrame;
  AddPage(NewFrame, AChatRoomName, ChatRoom);
end;

function TServiceDnmp.ChatRoomCount(): integer;
var
  i: integer;
  si: TDnmpServiceInfo;
begin
  Result:=0;
  // local services
  for i:=0 to self.ServMgr.ServiceInfoList.Count-1 do
  begin
    si:=self.ServMgr.ServiceInfoList.Items[i];
    if si.ServiceType=csGRPC then Inc(Result);
  end;
  // remote services
  for i:=0 to self.ServMgr.RemoteServiceInfoList.Count-1 do
  begin
    si:=self.ServMgr.RemoteServiceInfoList.Items[i];
    if si.ServiceType=csGRPC then Inc(Result);
  end;
end;

function TServiceDnmp.GetChatRoom(Index: integer): TChatRoom;
var
  i, n: integer;
  ServiceInfo: TDnmpServiceInfo;
begin
  Result:=nil;
  n:=-1;
  ServiceInfo:=nil;
  // local services
  for i:=0 to self.ServMgr.ServiceInfoList.Count-1 do
  begin
    ServiceInfo:=Self.ServMgr.ServiceInfoList.Items[i];
    if ServiceInfo.ServiceType=csGRPC then Inc(n);
    if n=Index then Break;
    ServiceInfo:=nil
  end;

  if ServiceInfo=nil then
  begin
    // remote services
    for i:=0 to Self.ServMgr.RemoteServiceInfoList.Count-1 do
    begin
      ServiceInfo:=Self.ServMgr.RemoteServiceInfoList.Items[i];
      if ServiceInfo.ServiceType=csGRPC then Inc(n);
      if n=Index then Break;
      ServiceInfo:=nil
    end;
  end;

  if Assigned(ServiceInfo) then
  begin
    Result:=TChatRoom.Create();
    Result.DataObject:=ServiceInfo;
  end;
end;

procedure TServiceDnmp.ShowChatRoomList();
var
  TmpFrame: TFrameChatRoomList;
begin
  TmpFrame:=TFrameChatRoomList.Create(nil);
  TmpFrame.Serv:=self;
  TmpFrame.Update();
  ShowForm(TmpFrame, 'Chat room list');
end;

procedure TServiceDnmp.ShowContactList();
var
  TmpFrame: TFrameDnmpContacts;
begin
  TmpFrame:=TFrameDnmpContacts.Create(nil);
  TmpFrame.Serv:=Self;
  TmpFrame.Update();
  ShowForm(TmpFrame, 'Contacts');
end;

procedure TServiceDnmp.ShowSetupWizard();
var
  Form: TFormDnmpWizard;
begin
  Form:=TFormDnmpWizard.Create(FormMain);
  Form.Serv:=Self;
  Form.Start();
  Form.ShowModal();
  Form.Free();
end;

{
procedure TServiceDnmp.ShowPrivateChatRoom(Contact: TDnmpContact);
var
  TmpFrame: TFrameDnmpChat;
begin
  if not Assigned(Contact) then Exit;
  TmpFrame:=TFrameDnmpChat.Create(nil);
  //ShowForm(TmpFrame, 'Chat: '+Contact.Name);
  AddPage(TmpFrame, 'Chat: '+Contact.Name, TmpFrame.Chat);
  TmpFrame.Contact:=Contact;
  TmpFrame.Chat:=(ServMgr.GetService(csCHAT, '') as TDnmpChat);
end;
}

procedure TServiceDnmp.LoadData();
begin
  Mgr.LoadFromFile();
  ServMgr.LoadFromFile();
  ServMgr.Start();
end;

procedure TServiceDnmp.SaveData();
begin
  ServMgr.SaveToFile();
  Mgr.SaveToFile();
end;

initialization
  ServiceDnmpList:=TStringList.Create();
  ServiceDnmpList.OwnsObjects:=True;
  MainFormPages:=TMainFormPages.Create(TMainFormPageItem);

finalization
  MainFormPages.ClearAll();
  FreeAndNil(MainFormPages);
  if Assigned(ServiceDnmpList) then FreeAndNil(ServiceDnmpList);

end.

