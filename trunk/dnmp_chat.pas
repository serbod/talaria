unit dnmp_chat;

{$mode objfpc}{$H+}

{
===== Msg Header =====
MsgType: CHAT
Params:
timestamp - message creation date and time
author_name - author name
author_guid - author GUID
target_name - target abonent name
target_guid - target abonent GUID
cmd - command name

===== Commands =====

=== MSG ===
  simple short message
  if author in black list, return BYE
Params:
  * cmd=MSG
  * req - request delivery report
Data:
  message text

=== BYE ===
  reject call, end of chat session
Params:
  * cmd=BYE
Data:
  reject message text

=== FILE ===
  small file, up to 16 KBytes
  or preview for large file
Params:
  * cmd=FILE
  * preview - (optional) data is preview for large file
  * file_name - file name
  * file_size - file size
  * file_date - file creation date
  * file_params - (optional) file params
  * offset - (optional, default=0) offset from beginning of file, bytes
Data:
  file contents

=== FREQ ===
  file request
Params:
  * cmd=FREQ
  * file_name - file name
  * preview - (optional) request only preview for large file
  * offset - (optional, default=0) offset from beginning of file, bytes
  * size - (optional) size of requested data
Data:
  none

}


interface

uses
  Classes, SysUtils, dnmp_unit, dnmp_services, contnrs, FileUtil;

type

  { TDnmpChatMessage }
  // GRPC Chat message
  // Contain text and basic info about author
  // Serializable
  TDnmpChatMessage = class(TInterfacedObject)
  public
    Timestamp: TDateTime;
    IsIncoming: boolean;
    RemoteAddr: TAddr;
    RemoteName: string;
    RemoteGUID: string;
    Text: string;
    function ToStorage(): TDnmpStorage; virtual;
    function FromStorage(Storage: TDnmpStorage): boolean; virtual;
  end;

  { TDnmpChatFileMessage }
  // GRPC Chat File message
  // Contain short file info and contents
  // Serializable
  TDnmpChatFileMessage = class(TDnmpChatMessage)
  public
    FileName: string;
    FileDate: TDateTime;
    FileSize: Cardinal;
    FileContent: TStream;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
  end;

  { TDnmpChatMessagesList }
  // GRPC Chat messages list
  // When MaxCount reached, first item will be deleted
  // Serializable
  TDnmpChatMessagesList = class(TInterfacedObject)
  protected
    FObservers: TComponentList;
    FItems: TObjectList;
    function GetCount(): integer;
    function GetItem(Index: Integer): TDnmpChatMessage;
    procedure SetItem(Index: Integer; Value: TDnmpChatMessage);
  public
    MaxCount: Integer;
    { source of messages }
    ParentList: TDnmpChatMessagesList;
    { Remote contact for filtering }
    Contact: TDnmpContact;
    constructor Create();
    destructor Destroy(); override;
    property Count: integer read GetCount;
    property Items[Index: Integer]: TDnmpChatMessage read GetItem write SetItem; default;
    { not used }
    function AddItem(Item: TDnmpChatMessage): Integer; overload;
    { Add message to list:
      Timestamp - datetime
      IsIncoming - true if incoming message
      Text - message text
      RemoteGUID
      RemoteContact - (optional), for name and address }
    function AddItem(Timestamp: TDateTime; IsIncoming: boolean; Text: string; RemoteAddr: TAddr; RemoteContact: TDnmpContact = nil): TDnmpChatMessage; overload;
    { Add FILE message to list:
    Timestamp - datetime
    IsIncoming - true if incoming message
    RemoteGUID
    RemoteContact - (optional), for name and address
    AFileName - file name
    AFileSize - file size (bytes)
    AFileDate - file creation date
    }
    function AddItemFile(Timestamp: TDateTime; IsIncoming: boolean; RemoteAddr: TAddr; AFileName: string; AFileSize: Cardinal; AFileDate: TDateTime; RemoteContact: TDnmpContact = nil): TDnmpChatFileMessage;
    //function GetAbonentByGUID(sGUID: string): TDnmpChannelMessage;
    { Add observer visual control }
    function AddObserver(AControl: TComponent): boolean;
    { Remove observer visual control }
    function DelObserver(AControl: TComponent): boolean;
    function ToStorage(): TDnmpStorage;
    function FromStorage(Storage: TDnmpStorage): boolean;
    procedure UpdateView();
  end;

  TDnmpChatSession = class(TCollectionItem)
  public
    CreationTime: TDateTime;
    Contact: TDnmpContact;
    { Parented list, not contain any data and use Contact as filter }
    MessagesList: TDnmpChatMessagesList;
  end;


  { TDnmpChat }
  { Contains chat sessions, messages list }
  TDnmpChat = class(TDnmpService)
  protected
    FOnSay: TNotifyEvent;
    procedure AddChatMessage(AContact: TDnmpContact; IsIncoming: boolean; sText: string);
  public
    Author: TDnmpContact;
    MessagesList: TDnmpChatMessagesList;
    ChatSessions: TCollection;
    constructor Create(AMgr: TDnmpManager; AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo); override;
    destructor Destroy(); override;
    //function SendCmd(Text: string; Addr: TAddr): string;
    function ToStorage(): TDnmpStorage; override;
    function FromStorage(Storage: TDnmpStorage): boolean; override;
    //function SayText(AbonentGUID: string; sText: string): string;
    { create new message with sText and send it to Contact }
    function SayToContact(Contact: TDnmpContact; sText: string): string;
    { send file to Contact }
    function SendFileToContact(Contact: TDnmpContact; AFileName: string): string;
    { return messages list for contact }
    function GetChatSessionForContact(AContact: TDnmpContact): TDnmpChatSession;
    //function Msg(Msg: TDnmpMsg): string; override;
    property OnSay: TNotifyEvent read FOnSay write FOnSay;
    function ParseMsg(AMsg: TDnmpMsg): string; override;
  end;

implementation

{ TDnmpChatFileMessage }

procedure TDnmpChatFileMessage.AfterConstruction();
begin
  inherited AfterConstruction();
  Self.FileContent:=TMemoryStream.Create();
end;

procedure TDnmpChatFileMessage.BeforeDestruction();
begin
  FreeAndNil(Self.FileContent);
  inherited BeforeDestruction();
end;

function TDnmpChatFileMessage.ToStorage: TDnmpStorage;
begin
  Result:=inherited ToStorage;
  Result.Add('file_name', FileName);
  Result.Add('file_size', FileSize);
  Result.Add('file_date', FileDate);
  Result.Add('file_content', StreamToStr(FileContent));
end;

function TDnmpChatFileMessage.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=inherited FromStorage(Storage);
  if not Result then Exit;
  Self.FileName:=Storage.GetString('file_name');
  Self.FileSize:=Storage.GetInteger('file_size');
  Self.FileDate:=Storage.GetReal('file_date');
  Result:=StrToStream(Storage.GetString('file_content'), Self.FileContent);
end;

{ TDnmpChatMessage }

function TDnmpChatMessage.ToStorage: TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('time', Timestamp);
  Result.Add('in', IsIncoming);
  Result.Add('addr', AddrToStr(RemoteAddr));
  Result.Add('name', RemoteName);
  Result.Add('guid', RemoteGUID);
  Result.Add('text', Text);
end;

function TDnmpChatMessage.FromStorage(Storage: TDnmpStorage): boolean;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  Self.Timestamp:=Storage.GetReal('time');
  Self.IsIncoming:=Storage.GetBool('in');
  Self.RemoteAddr:=StrToAddr(Storage.GetString('addr'));
  Self.RemoteName:=Storage.GetString('name');
  Self.RemoteGUID:=Storage.GetString('guid');
  // Self.Author:=??;
  Self.Text:=Storage.GetString('text');
  Result:=True;
end;

{ TDnmpChatMessagesList }

function TDnmpChatMessagesList.GetCount(): integer;
var
  i: integer;
begin
  if not Assigned(ParentList) then Result:=FItems.Count
  else
  begin
    if Assigned(Contact) then
    begin
      Result:=0;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID=Contact.GUID then Result:=Result+1;
      end;
    end
    else Result:=ParentList.Count;
  end;
end;

function TDnmpChatMessagesList.GetItem(Index: Integer): TDnmpChatMessage;
var
  i, n: integer;
begin
  if not Assigned(ParentList) then Result:=(FItems.Items[Index] as TDnmpChatMessage)
  else
  begin
    if Assigned(Contact) then
    begin
      n:=-1;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID<>Contact.GUID then Continue;
        n:=n+1;
        if n<Index then Continue;
        Result:=ParentList.Items[i];
        Exit;
      end;
    end
    else Result:=ParentList.Items[Index];
  end;
end;

procedure TDnmpChatMessagesList.SetItem(Index: Integer; Value: TDnmpChatMessage
  );
var
  i, n: integer;
begin
  if not Assigned(ParentList) then FItems.Items[Index]:=Value
  else
  begin
    if Assigned(Contact) then
    begin
      n:=-1;
      for i:=0 to ParentList.Count-1 do
      begin
        if ParentList[i].RemoteGUID<>Contact.GUID then Continue;
        n:=n+1;
        if n<Index then Continue;
        ParentList.Items[i]:=Value;
        Exit;
      end;
    end
    else ParentList.Items[Index]:=Value;
  end;
end;

constructor TDnmpChatMessagesList.Create();
begin
  inherited Create();
  FItems:=TObjectList.Create();
  FObservers:=TComponentList.Create(False);
end;

destructor TDnmpChatMessagesList.Destroy();
begin
  FreeAndNil(FObservers);
  FreeAndNil(FItems);
  inherited Destroy();
end;

function TDnmpChatMessagesList.AddItem(Item: TDnmpChatMessage): Integer;
begin
  Result:=-1;
  if not Assigned(ParentList) then
  begin
    Result:=FItems.IndexOf(Item);
    if Result=-1 then Result:=FItems.Add(Item)
  end
  else
  begin
    if Assigned(Contact) then
    begin
      if not SameAddr(Contact.Addr, Item.RemoteAddr) then Exit;
    end;
    Result:=ParentList.AddItem(Item);
  end;
end;

function TDnmpChatMessagesList.AddItem(Timestamp: TDateTime;
  IsIncoming: boolean; Text: string; RemoteAddr: TAddr;
  RemoteContact: TDnmpContact): TDnmpChatMessage;
var
  TmpContact: TDnmpContact;
begin
  Result:=nil;
  if not Assigned(ParentList) then
  begin
    // { TODO : find }
    // add
    Result:=TDnmpChatMessage.Create();
    FItems.Add(Result);
    Result.Timestamp:=Timestamp;
    Result.IsIncoming:=IsIncoming;
    Result.Text:=Text;
    if Assigned(RemoteContact) then
    begin
      Result.RemoteGUID:=RemoteContact.GUID;
      Result.RemoteName:=RemoteContact.Name;
      Result.RemoteAddr:=RemoteContact.Addr;
    end
    else
    begin
      Result.RemoteAddr:=RemoteAddr;
      Result.RemoteName:='';
      Result.RemoteGUID:='';
    end;
  end

  else
  begin
    if Assigned(Contact) then
    begin
      if not SameAddr(Contact.Addr, RemoteAddr) then Exit;
    end;
    Result:=ParentList.AddItem(Timestamp, IsIncoming, Text, RemoteAddr, RemoteContact);
  end;
end;

function TDnmpChatMessagesList.AddItemFile(Timestamp: TDateTime;
  IsIncoming: boolean; RemoteAddr: TAddr; AFileName: string;
  AFileSize: Cardinal; AFileDate: TDateTime; RemoteContact: TDnmpContact
  ): TDnmpChatFileMessage;
var
  TmpContact: TDnmpContact;
begin
  Result:=nil;
  if not Assigned(ParentList) then
  begin
    // { TODO : find }
    // add
    Result:=TDnmpChatFileMessage.Create();
    FItems.Add(Result);
    Result.Timestamp:=Timestamp;
    Result.IsIncoming:=IsIncoming;
    Result.Text:='';
    if Assigned(RemoteContact) then
    begin
      Result.RemoteGUID:=RemoteContact.GUID;
      Result.RemoteName:=RemoteContact.Name;
      Result.RemoteAddr:=RemoteContact.Addr;
    end
    else
    begin
      Result.RemoteAddr:=RemoteAddr;
      Result.RemoteName:='';
      Result.RemoteGUID:='';
    end;
    Result.FileName:=AFileName;
    Result.FileSize:=AFileSize;
    Result.FileDate:=AFileDate;
    //Result.FileContent;
  end

  else
  begin
    if Assigned(Contact) then
    begin
      if not SameAddr(Contact.Addr, RemoteAddr) then Exit;
    end;
    Result:=ParentList.AddItemFile(Timestamp, IsIncoming, RemoteAddr, AFileName, AFileSize, AFileDate, RemoteContact);
  end;
end;

function TDnmpChatMessagesList.AddObserver(AControl: TComponent): boolean;
begin
  Result:=False;
  if FObservers.IndexOf(AControl)>-1 then Exit;
  FObservers.Add(AControl);
  Result:=True;
end;

function TDnmpChatMessagesList.DelObserver(AControl: TComponent): boolean;
begin
  Result:=False;
  if FObservers.IndexOf(AControl)=-1 then Exit;
  FObservers.Extract(AControl);
  Result:=True;
end;

function TDnmpChatMessagesList.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
  i: Integer;
begin
  Storage:=TDnmpStorage.Create(stDictionary);
  for i:=0 to Self.Count-1 do
  begin
    Storage.Add(IntToStr(i), Self.Items[i].ToStorage());
  end;

  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpChatMessagesList');
  Result.Add('items', Storage);
end;

function TDnmpChatMessagesList.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
  i: Integer;
  Item: TDnmpChatMessage;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpChatMessagesList' then Exit;
  SubStorage:=Storage.GetObject('items');
  if SubStorage.StorageType <> stDictionary then Exit;
  for i:=0 to SubStorage.Count-1 do
  begin
    if SubStorage.HaveName('file_name') then
      Item:=TDnmpChatFileMessage.Create()
    else
      Item:=TDnmpChatMessage.Create();
    if not Item.FromStorage(SubStorage.GetObject(i)) then
    begin
      Item.Free();
      Continue;
    end;
    FItems.Add(Item);
  end;
  Result:=True;
end;

procedure TDnmpChatMessagesList.UpdateView();
var
  i: integer;
  AMsg: ShortString;
begin
  AMsg:='UpdateView';
  for i:=0 to FObservers.Count-1 do FObservers.Items[i].DispatchStr(AMsg);
end;

{ TDnmpChat }

procedure TDnmpChat.AddChatMessage(AContact: TDnmpContact; IsIncoming: boolean;
  sText: string);
var
  i: integer;
  ChatSession: TDnmpChatSession;
begin
  ChatSession:=Self.GetChatSessionForContact(AContact);
  if not Assigned(ChatSession) then Exit;
  ChatSession.MessagesList.AddItem(Now(), IsIncoming, sText, AContact.Addr, AContact);
  ChatSession.MessagesList.UpdateView();
end;

constructor TDnmpChat.Create(AMgr: TDnmpManager;
  AServiceMgr: TDnmpServiceManager; AServiceInfo: TDnmpServiceInfo);
begin
  inherited Create(AMgr, AServiceMgr, AServiceInfo);
  Self.Author:=AMgr.MyInfo;
  Self.MessagesList:=TDnmpChatMessagesList.Create();
  Self.ChatSessions:=TCollection.Create(TDnmpChatSession);
end;

destructor TDnmpChat.Destroy();
begin
  Self.OnEvent:=nil;
  Self.OnSay:=nil;
  FreeAndNil(Self.ChatSessions);
  FreeAndNil(Self.MessagesList);
  inherited Destroy();
end;

function TDnmpChat.ToStorage(): TDnmpStorage;
var
  Storage: TDnmpStorage;
begin
  Result:=TDnmpStorage.Create(stDictionary);
  Result.Add('type', 'DnmpChat');

  Storage:=Self.ServiceInfo.ToStorage();
  Result.Add('service_info', Storage);

  Storage:=Self.MessagesList.ToStorage();
  Result.Add('messages_list', Storage);
end;

function TDnmpChat.FromStorage(Storage: TDnmpStorage): boolean;
var
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if not Assigned(Storage) then Exit;
  if Storage.StorageType <> stDictionary then Exit;
  if Storage.GetString('type')<>'DnmpChat' then Exit;

  { TODO : Is really need to restore service info? }
  SubStorage:=Storage.GetObject('service_info');
  Self.ServiceInfo.FromStorage(SubStorage);

  SubStorage:=Storage.GetObject('messages_list');
  Self.MessagesList.FromStorage(SubStorage);

  Result:=True;
end;

function TDnmpChat.SayToContact(Contact: TDnmpContact; sText: string): string;
begin
  // add to local messages list
  AddChatMessage(Contact, False, sText);
  // send message to remote contact
  Mgr.SendDataMsg(Contact.Addr, 'CHAT', 'cmd=MSG', sText);
end;

function TDnmpChat.SendFileToContact(Contact: TDnmpContact; AFileName: string
  ): string;
var
  ChatFileMessage: TDnmpChatFileMessage;
  fs: TFileStream;
begin
  if not FileExistsUTF8(AFileName) then Exit;
  ChatFileMessage:=TDnmpChatFileMessage.Create();
  ChatFileMessage.FileName:=AFileName;
  ChatFileMessage.FileDate:=FileAgeUTF8();
  try
    fs:=TFileStream.Create(AFileName, fmOpenRead);
  except
    fs:=nil;
  end;
  if Assigned(fs) then
  begin
    ChatFileMessage.FileSize:=fs.Size;
    ChatFileMessage.FileContent.CopyFrom(fs, fs.Size);
    FreeAndNil(fs);
  end;

  // add to local messages list
  AddChatMessage(Contact, False, sText);
  // send message to remote contact
  Mgr.SendDataMsg(Contact.Addr, 'CHAT', 'cmd=MSG', sText);

end;

function TDnmpChat.GetChatSessionForContact(AContact: TDnmpContact
  ): TDnmpChatSession;
var
  i: integer;
begin
  Result:=nil;
  if not Assigned(AContact) then Exit;
  // find chat session
  for i:=0 to ChatSessions.Count-1 do
  begin
    Result:=(ChatSessions.Items[i] as TDnmpChatSession);
    if Result.Contact=AContact then Exit;
  end;
  Result:=nil;

  if not Assigned(Result) then // create new session
  begin
    Result:=(ChatSessions.Add() as TDnmpChatSession);
    Result.CreationTime:=Now();
    Result.Contact:=AContact;
    Result.MessagesList:=TDnmpChatMessagesList.Create();
    Result.MessagesList.ParentList:=Self.MessagesList;
    Result.MessagesList.Contact:=AContact;
  end;
end;

function TDnmpChat.ParseMsg(AMsg: TDnmpMsg): string;
var
  sCmd: string;
  RemoteContact: TDnmpContact;
begin
  Result:='';
  if not Assigned(AMsg) then Exit;
  if AMsg.MsgType <> Self.ServiceInfo.ServiceType then Exit;

  RemoteContact:=Mgr.GetContactByAddr(AMsg.SourceAddr);
  if not Assigned(RemoteContact) then
  begin
    Mgr.DebugText('CHAT message from unknown addr: '+AddrToStr(AMsg.SourceAddr));
    Exit;
  end;

  sCmd:=Trim(AMsg.Info.Values['cmd']);

  if sCmd = 'MSG' then // Это сообщение
  begin
    // update chat session
    RemoteContact.IncomingChat:=True;
    AddChatMessage(RemoteContact, True, StreamToStr(AMsg.Data));
    if Assigned(OnEvent) then OnEvent('MSG', Self);
  end

  else if sCmd = 'INVITE' then // Это приглашение
  begin
    if Assigned(OnEvent) then OnEvent('INVITE', Self);
  end

  else if sCmd = 'ACK' then // Это подтверждение
  begin
    if Assigned(OnEvent) then OnEvent('ACK', Self);
  end

  else if sCmd = 'BYE' then // Это отказ
  begin
    if Assigned(OnEvent) then OnEvent('BYE', Self);
  end

  else if sCmd = 'FILE' then // Это файл
  begin
    if Assigned(OnEvent) then OnEvent('FILE', Self);
  end;
end;



end.

