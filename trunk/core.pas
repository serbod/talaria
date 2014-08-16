unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, dnmp_unit, dnmp_services, dnmp_grpc, PointListFrame,
  Misc, dnmp_mail, Controls;

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
    DataObject: TObject;
  end;

  { TMainFormPages }

  TMainFormPages = class(TCollection)
    procedure AddPage(AFrame: TFrame; ACaption: string);
    procedure ClearAll;
  end;

  { TContactItem }

  TContactItem = class(TInterfacedObject)
  protected
    function FGetCaption(): string; virtual;
    function FGetHint(): string; virtual;
  public
    DataObject: TObject;
    IsGroup: boolean;
    ParentItem: TContactItem;
    property Caption: string read FGetCaption;
    property Hint: string read FGetHint;
    function StateIcon(): integer; virtual;
  end;

  { TChatRoom }

  TChatRoom = class(TInterfacedObject)
  protected
    function FGetName(): string; virtual;
    function FGetContactCount(): integer; virtual;
  public
    DataObject: TObject;
    property Name: string read FGetName;
    property ContactCount: integer read FGetContactCount;
    function GetContact(Index: integer): TContactItem;
  end;



var
  MainFormPages: TMainFormPages;
  ServiceDnmpNode: TServiceDnmpNode;

procedure Init();
procedure AddPage(AFrame: TFrame; ACaption: string);
procedure ShowForm(AFrame: TFrame; ACaption: string);
procedure AddServicePage(AService: TDnmpService);
procedure ShowContactList();

const
  ciIconFolder = 9;
  ciIconNote = 18;
  ciIconUser = 20;

implementation

uses StatusFrame, ChatFrame, DnmpNodeFrame, GrpcServiceFrame, MainForm,
  MailboxFrame, ContactListFrame;

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
  frame:=TFrameChat.Create(nil);
  (frame as TFrameChat).ChatRoom:=TChatRoom.Create();
  (frame as TFrameChat).ChatRoom.DataObject:=(ServiceDnmpNode.ServMgr.GetService('GRPC','#test') as TDnmpGrpc);
  AddPage(frame, '#test');

  // mail page
  frame:=TFrameMailbox.Create(nil);
  (frame as TFrameMailbox).MailRoom:=(ServiceDnmpNode.ServMgr.GetService('MAIL','') as TDnmpMail);
  (frame as TFrameMailbox).Update();
  AddPage(frame, 'Mail');

end;

procedure AddPage(AFrame: TFrame; ACaption: string);
begin
  if not Assigned(MainFormPages) then Exit;
  MainFormPages.AddPage(AFrame, ACaption);
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
      AddPage(TmpFrame, AService.ServiceInfo.Name);
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
      AddPage(TmpFrame, AService.ServiceInfo.Name);
    end;
  end;

  if Assigned(PageItem) then
  begin
    // activate page
    FormMain.ActivatePage(PageItem);
  end;
end;

procedure ShowContactList();
var
  Frame: TFrameContactList;
begin
  Frame:=TFrameContactList.Create(nil);
  Frame.SvcMgr:=ServiceDnmpNode.ServMgr;
  Frame.Update();
  ShowForm(Frame, 'Contact list');
end;

{ TChatRoom }

function TChatRoom.FGetName(): string;
begin
  Result:='#ChatRoom';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).ServiceInfo.Name;
end;

function TChatRoom.FGetContactCount(): integer;
begin
  Result:=0;
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpGrpc) then Result:=(DataObject as TDnmpGrpc).UsersList.Count;
end;

function TChatRoom.GetContact(Index: integer): TContactItem;
begin
  Result:=TContactItem.Create();
  Result.DataObject:=(DataObject as TDnmpGrpc).UsersList.Items[Index];
end;

{ TContactItem }

function TContactItem.FGetCaption(): string;
begin
  Result:='Contact';
  if not Assigned(DataObject) then Exit;
  if (DataObject is TDnmpContact) then Result:=(DataObject as TDnmpContact).Nick;
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

function TContactItem.StateIcon(): integer;
begin
  Result:=ciIconUser;
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

