unit DnmpContactsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  ActnList, Menus, dnmp_unit, Core, dnmp_chat, Graphics, Dialogs, LCLType, LazUTF8;

type

  { TVisualContact }

  TVisualContact = class(TCollectionItem)
  private
    FSelected: boolean;
    FActive: boolean;
    procedure FSetSelected(Value: boolean);
    procedure FSetActive(Value: boolean);
  public
    Item: TDnmpContact;
    Rect: TRect;
    Image: TImage;
    lbName: TLabel;
    Background: TShape;
    property Selected: boolean read FSelected write FSetSelected;
    property Active: boolean read FActive write FSetActive;
  end;

  TVisualChatItem = class(TCollectionItem)
  private
    FSelected: boolean;
    FActive: boolean;
  public
    Item: TDnmpChatMessage;
    Rect: TRect;
    //Image: TImage;
    lbName: TLabel;
    lbTime: TLabel;
    lbText: TLabel;
    Background: TShape;
  end;

  TVisualInfoItem = class(TCollectionItem)
  private
    FSelected: boolean;
    FActive: boolean;
  public
    Name: string;
    Rect: TRect;
    //Image: TImage;
    lbName: TLabel;
    //lbTime: TLabel;
    memoText: TMemo;
    Background: TShape;
  end;

  { TFrameDnmpContacts }

  TFrameDnmpContacts = class(TFrame)
    actDeleteContact: TAction;
    actFindContacts: TAction;
    actAddToFavorites: TAction;
    actContactsAll: TAction;
    actContactsFavorites: TAction;
    actContactsNodes: TAction;
    actContactsPoints: TAction;
    actContactsGuests: TAction;
    actContactsFound: TAction;
    actRequestInfo: TAction;
    alContacts: TActionList;
    edChatSay: TEdit;
    imgDefault: TImage;
    panChatSay: TPanel;
    pgcContactInfo: TPageControl;
    panContacts: TPanel;
    panContactInfo: TPanel;
    pmContacts: TPopupMenu;
    ScrollBoxInfo: TScrollBox;
    ScrollBoxChat: TScrollBox;
    ScrollBoxContacts: TScrollBox;
    Splitter1: TSplitter;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tsInfo: TTabSheet;
    tsChat: TTabSheet;
    ToolBarContacts: TToolBar;
    procedure actAddToFavoritesExecute(Sender: TObject);
    procedure actContactsAllExecute(Sender: TObject);
    procedure actContactsFavoritesExecute(Sender: TObject);
    procedure actContactsFoundExecute(Sender: TObject);
    procedure actContactsGuestsExecute(Sender: TObject);
    procedure actContactsNodesExecute(Sender: TObject);
    procedure actContactsPointsExecute(Sender: TObject);
    procedure actDeleteContactExecute(Sender: TObject);
    procedure actFindContactsExecute(Sender: TObject);
    procedure actRequestInfoExecute(Sender: TObject);
    procedure edChatSayKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    FServ: TServiceDnmp;
    FChat: TDnmpChat;
    FContact: TDnmpContact;
    FContactList: TDnmpContactList;
    VisualContacts: TCollection;
    VisualChatItems: TCollection;
    VisualInfoItems: TCollection;
    Mgr: TDnmpManager;
    ChatSession: TDnmpChatSession;
    procedure FSetServ(Value: TServiceDnmp);
    procedure FSetChat(Value: TDnmpChat);
    procedure FSetContact(Value: TDnmpContact);
    procedure FSetContactList(Value: TDnmpContactList);
    function AddVisualContact(Item: TDnmpContact): TVisualContact;
    function AddVisualChatItem(Item: TDnmpChatMessage): TVisualChatItem;
    function AddVisualInfoItem(AName, AValue: string): TVisualInfoItem;
    function VisualContactBySender(Sender: TObject): TVisualContact;
    procedure OnMouseEnterContact(Sender: TObject);
    procedure OnMouseLeaveContact(Sender: TObject);
    procedure OnMouseDownContact(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    { public declarations }
    LastChatY: integer;
    LastInfoY: integer;
    property Serv: TServiceDnmp read FServ write FSetServ;
    property Contact: TDnmpContact read FContact write FSetContact;
    property ContactList: TDnmpContactList read FContactList write FSetContactList;
    property Chat: TDnmpChat read FChat write FSetChat;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure UpdateContactsList();
    procedure ClearChat();
    procedure UpdateChat();
    procedure UpdateInfo();
    procedure UpdateViewHandler(var AMsg); message 'UpdateView';
  end;

implementation

uses dnmp_services;

{$R *.lfm}

{ TVisualContact }

procedure TVisualContact.FSetSelected(Value: boolean);
begin
  FSelected:=Value;
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
end;

procedure TVisualContact.FSetActive(Value: boolean);
begin
  FActive:=Value;
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
end;

{ TFrameDnmpContacts }

procedure TFrameDnmpContacts.actDeleteContactExecute(Sender: TObject);
begin
  if MessageDlg('Delete item', 'Are you sure?', mtConfirmation, mbYesNo, 0)<>mrYes then Exit;
  if Assigned(ContactList) and Assigned(Contact) then ContactList.Extract(Contact);
end;

procedure TFrameDnmpContacts.actAddToFavoritesExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Assigned(Contact) then Mgr.MyPassport.ContactsList.AddItem(Contact);
end;

procedure TFrameDnmpContacts.actContactsAllExecute(Sender: TObject);
begin
  ContactList:=Mgr.ContactList;
end;

procedure TFrameDnmpContacts.actContactsFavoritesExecute(Sender: TObject);
begin
  ContactList:=Mgr.MyPassport.ContactsList;
end;

procedure TFrameDnmpContacts.actContactsFoundExecute(Sender: TObject);
begin
  ContactList:=Mgr.TmpContactList;
end;

procedure TFrameDnmpContacts.actContactsGuestsExecute(Sender: TObject);
begin
  ContactList:=Mgr.UnapprovedList;
end;

procedure TFrameDnmpContacts.actContactsNodesExecute(Sender: TObject);
begin
  ContactList:=Mgr.NodeList;
end;

procedure TFrameDnmpContacts.actContactsPointsExecute(Sender: TObject);
begin
  ContactList:=Mgr.PointList;
end;

procedure TFrameDnmpContacts.actFindContactsExecute(Sender: TObject);
var
  s: string;
begin
  if not Assigned(Mgr) then Exit;
  // ask for name
  s:='';
  if not InputQuery('Find contacts', 'Enter name:', s) then Exit;
  s:=Trim(s);
  if Length(s)<3 then Exit;
  // request
  Mgr.RequestContactsByName(s);
  actContactsFound.Execute();
end;

procedure TFrameDnmpContacts.actRequestInfoExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Assigned(Contact) then Mgr.RequestInfoByAddr(Contact.Addr);
end;

procedure TFrameDnmpContacts.edChatSayKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
begin
  if not Assigned(Contact) then Exit;
  if Key = VK_RETURN then
  begin
    if Trim(edChatSay.Text)<>'' then
    begin
      Chat.SayToContact(Contact, edChatSay.Text);
    end;
    edChatSay.Text:='';
    Key:=0;
    Update();
  end;
  if Key = VK_F7 then
  begin
    // test
    //ScrollBox.Visible:=False;
    for i:=1 to 100 do
    begin
      Chat.SayToContact(Contact, 'Абра швабра кадабра! Абра швабра кадабра! Абра швабра кадабра! Абра швабра кадабра!');
    end;
    //Update();
    //ScrollBox.Visible:=True;
    Key:=0;
  end;
end;

procedure TFrameDnmpContacts.FSetServ(Value: TServiceDnmp);
var
  MgrAssigned: Boolean;
begin
  Mgr:=nil;
  Chat:=nil;
  FServ:=Value;
  if Assigned(FServ) then
  begin
    Mgr:=FServ.Mgr;
    Chat:=(FServ.ServMgr.GetService(csCHAT, '') as TDnmpChat);
  end;
  // update visual controls
  MgrAssigned:=Assigned(Mgr);
  actContactsAll.Enabled:=MgrAssigned;
  actContactsFavorites.Enabled:=MgrAssigned;
  actContactsNodes.Enabled:=MgrAssigned;
  actContactsPoints.Visible:=MgrAssigned and Mgr.MyInfo.IsNode;
  actContactsGuests.Visible:=MgrAssigned and Mgr.MyInfo.IsNode;
  actContactsFound.Enabled:=MgrAssigned;
  if MgrAssigned then actContactsAll.Execute();
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.FSetChat(Value: TDnmpChat);
begin
  FChat:=Value;
  if Assigned(ChatSession) then ChatSession.MessagesList.DelObserver(Self);
  ChatSession:=nil;
  if Assigned(Chat) and Assigned(Contact) then
  begin
    ChatSession:=Chat.GetChatSessionForContact(Contact);
    ChatSession.MessagesList.AddObserver(Self);
  end;
  UpdateChat();
end;

procedure TFrameDnmpContacts.FSetContact(Value: TDnmpContact);
begin
  if FContact=Value then Exit;
  ClearChat();
  if Assigned(ChatSession) then ChatSession.MessagesList.DelObserver(Self);
  ChatSession:=nil;
  FContact:=Value;
  LastChatY:=0;

  if Assigned(Contact) and Assigned(Chat) then
  begin
    // update chat
    //ChatSession:=nil;
    ChatSession:=Chat.GetChatSessionForContact(Contact);
    ChatSession.MessagesList.AddObserver(Self);
    UpdateChat();

    // update info
    UpdateInfo();
  end;
end;

procedure TFrameDnmpContacts.FSetContactList(Value: TDnmpContactList);
begin
  if FContactList=Value then Exit;
  FContactList:=Value;
  // update contacts
  UpdateContactsList();
end;

function TFrameDnmpContacts.AddVisualContact(Item: TDnmpContact): TVisualContact;
var
  i, x, y, h: integer;
  //Pan: TPanel;
  Image: TImage;
  lb: TLabel;
  ss: TStringStream;
begin
  x:=2;
  h:=24;
  i:=VisualContacts.Count;
  y:=(h+2)*i;

  //Result:=TVisualItem.Create();
  Result:=(VisualContacts.Add() as TVisualContact);
  Result.Item:=Item;
  Result.Rect.Top:=y;
  Result.Rect.Left:=x;

  // selected background
  Result.Background:=TShape.Create(ScrollBoxContacts);
  Result.Background.Name:='bg'+IntToStr(i);
  Result.Background.Parent:=ScrollBoxContacts;
  Result.Background.Left:=x;
  Result.Background.Top:=y;
  Result.Background.Shape:=stRectangle;
  //Result.Background.Shape:=stRoundRect;
  Result.Background.Pen.Color:=clActiveBorder;
  //Result.Background.OnClick:=@OnClickHandler;
  //Result.Background.On

  // image
  Image:=TImage.Create(ScrollBoxContacts);
  Image.Name:='img'+IntToStr(i);
  Image.Parent:=ScrollBoxContacts;
  Image.Left:=x;
  Image.Top:=y;
  Image.Height:=h;
  Image.Width:=h;

  Image.Center:=True;
  Image.Proportional:=True;
  Image.Stretch:=True;
  if Length(Item.Picture)>4 then
  begin
    ss:=TStringStream.Create(Item.Picture);
    try
      Image.Picture.LoadFromStream(ss);
    finally
      ss.Free();
    end;
  end
  else
  begin
    Image.Picture.Assign(imgDefault.Picture);
  end;
  Result.Image:=Image;
  x:=x+Image.Width+2;

  // name
  lb:=TLabel.Create(ScrollBoxContacts);
  lb.Name:='lb'+IntToStr(i);
  lb.Parent:=ScrollBoxContacts;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=200;

  lb.Font.Size:=12;
  lb.Caption:=Item.Name;
  lb.ShowHint:=True;
  lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  //lb.Brush.Color:=clNone;
  //lb.Brush.Style:=bsClear;
  //lb.ParentColor:=False;
  //lb.Color:=clNone;
  //lb.OnClick:=@OnClickHandler;
  lb.OnMouseDown:=@OnMouseDownContact;
  lb.OnMouseEnter:=@OnMouseEnterContact;
  lb.OnMouseLeave:=@OnMouseLeaveContact;
  lb.PopupMenu:=pmContacts;
  Result.lbName:=lb;
  x:=x+lb.Width+2;

  Result.Rect.Right:=x;
  Result.Rect.Bottom:=y+h;

  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;
end;

function TFrameDnmpContacts.AddVisualChatItem(Item: TDnmpChatMessage
  ): TVisualChatItem;
var
  i, x, y, h, n, nn: integer;
  //Pan: TPanel;
  Image: TImage;
  st: TStaticText;
  lb: TLabel;
  s, ss: string;
begin
  x:=2;
  h:=16;
  y:=LastChatY+2;
  i:=VisualChatItems.Count;

  //Result:=TVisualItem.Create();
  Result:=(VisualChatItems.Add() as TVisualChatItem);
  Result.Item:=Item;
  Result.Rect.Top:=y;
  Result.Rect.Left:=x;

  // selected background
  Result.Background:=TShape.Create(ScrollBoxChat);
  Result.Background.Name:='bg'+IntToStr(i);
  Result.Background.Parent:=ScrollBoxChat;
  Result.Background.Left:=x;
  Result.Background.Top:=y;
  Result.Background.Shape:=stRectangle;
  Result.Background.Pen.Color:=clActiveBorder;
  //Result.Background.OnClick:=@OnClickHandler;
  //Result.Background.On

  {
  // image
  Image:=TImage.Create(ScrollBox);
  Image.Name:='img'+IntToStr(i);
  Image.Parent:=ScrollBox;
  Image.Left:=x;
  Image.Top:=y;
  Image.Height:=h;
  Image.Width:=h;

  Image.Center:=True;
  Image.Proportional:=True;
  Image.Stretch:=True;
  if Length(Item.Picture)>4 then
  begin
    ss:=TStringStream.Create(Item.Picture);
    try
      Image.Picture.LoadFromStream(ss);
    finally
      ss.Free();
    end;
  end
  else
  begin
    Image.Picture.Assign(imgDefault.Picture);
  end;
  Result.Image:=Image;
  x:=x+Image.Width+2;
  }

  // name
  lb:=TLabel.Create(ScrollBoxChat);
  lb.Name:='lbn'+IntToStr(i);
  lb.Parent:=ScrollBoxChat;
  lb.AutoSize:=False;
  lb.Left:=x+4;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=200;

  lb.Font.Size:=8;
  lb.Font.Style:=[fsBold];
  lb.Caption:=BoolToStr(Item.IsIncoming, Item.RemoteName, Chat.Author.Name);
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  //lb.Color:=clNone;
  //lb.OnClick:=@OnClickHandler;
  //lb.OnMouseDown:=@OnMouseDownHandler;
  //lb.OnMouseEnter:=@OnMouseEnterHandler;
  //lb.OnMouseLeave:=@OnMouseLeaveHandler;
  //lb.PopupMenu:=pmContactList;
  Result.lbName:=lb;
  x:=x+lb.Width+2;

  // time
  lb:=TLabel.Create(ScrollBoxChat);
  lb.Name:='lbt'+IntToStr(i);
  lb.Parent:=ScrollBoxChat;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=100;

  lb.Font.Size:=8;
  lb.Caption:=FormatDateTime('YYYY-MM-DD HH:NN:SS', Item.Timestamp);
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  //lb.Color:=clNone;
  //lb.OnClick:=@OnClickHandler;
  //lb.OnMouseDown:=@OnMouseDownHandler;
  //lb.OnMouseEnter:=@OnMouseEnterHandler;
  //lb.OnMouseLeave:=@OnMouseLeaveHandler;
  //lb.PopupMenu:=pmContactList;
  Result.lbTime:=lb;
  x:=x+lb.Width+4;
  Result.Rect.Right:=x;

  y:=y+h;
  x:=0;

  // text
  lb:=TLabel.Create(ScrollBoxChat);
  lb.Name:='lbx'+IntToStr(i);
  lb.Parent:=ScrollBoxChat;
  lb.AutoSize:=False;
  lb.Left:=x+4;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=300;

  lb.Font.Size:=10;
  lb.Caption:=Item.Text;
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  //lb.Color:=clNone;
  lb.WordWrap:=True;

  // calculate text height
  s:=Item.Text;
  n:=lb.Canvas.TextFitInfo(s, lb.Width-4);
  while n<UTF8Length(s) do
  begin
    s:=UTF8Copy(s, n, maxint);
    n:=lb.Canvas.TextFitInfo(s, lb.Width-4);
    lb.Height:=lb.Height+lb.Canvas.TextHeight(s);
  end;
  //lb.CalcFittingFontHeight();
  //lb.OnClick:=@OnClickHandler;
  //lb.OnMouseDown:=@OnMouseDownHandler;
  //lb.OnMouseEnter:=@OnMouseEnterHandler;
  //lb.OnMouseLeave:=@OnMouseLeaveHandler;
  //lb.PopupMenu:=pmContactList;
  Result.lbText:=lb;
  x:=x+lb.Width+4;
  y:=y+lb.Height+2;

  //Result.Rect.Right:=x;
  Result.Rect.Bottom:=y;
  LastChatY:=y;

  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;

  // scroll to message
  ScrollBoxChat.VertScrollBar.Position:=(ScrollBoxChat.VertScrollBar.Range-ScrollBoxChat.VertScrollBar.Page);
end;

function TFrameDnmpContacts.AddVisualInfoItem(AName, AValue: string
  ): TVisualInfoItem;
var
  i, x, y, h, n, nn: integer;
  //Pan: TPanel;
  Image: TImage;
  st: TStaticText;
  lb: TLabel;
  m: TMemo;
  s, ss: string;
begin
  x:=2;
  h:=16;
  i:=VisualInfoItems.Count;
  y:=LastInfoY+2;

  Result:=(VisualInfoItems.Add() as TVisualInfoItem);
  Result.Name:=AName;
  Result.Rect.Top:=y-1;
  Result.Rect.Left:=x;

  {
  // selected background
  Result.Background:=TShape.Create(ScrollBoxInfo);
  Result.Background.Name:='bg'+IntToStr(i);
  Result.Background.Parent:=ScrollBoxInfo;
  Result.Background.Left:=x;
  Result.Background.Top:=y-1;
  Result.Background.Shape:=stRectangle;
  Result.Background.Pen.Color:=clActiveBorder;
  //Result.Background.OnClick:=@OnClickHandler;
  }

  // name
  lb:=TLabel.Create(ScrollBoxInfo);
  lb.Name:='lbn'+IntToStr(i);
  lb.Parent:=ScrollBoxInfo;
  lb.AutoSize:=False;
  lb.Left:=x+4;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=100;

  lb.Font.Size:=8;
  lb.Font.Style:=[fsBold];
  lb.Caption:=AName;
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  //lb.Color:=clNone;
  //lb.OnClick:=@OnClickHandler;
  //lb.OnMouseDown:=@OnMouseDownHandler;
  //lb.OnMouseEnter:=@OnMouseEnterHandler;
  //lb.OnMouseLeave:=@OnMouseLeaveHandler;
  //lb.PopupMenu:=pmContactList;
  Result.lbName:=lb;
  x:=x+lb.Width+2;

  // text
  m:=TMemo.Create(ScrollBoxInfo);
  m.Name:='m'+IntToStr(i);
  m.Parent:=ScrollBoxInfo;
  m.Left:=x+4;
  m.Top:=y+1;
  m.Height:=h;
  m.Width:=250;

  m.Font.Size:=10;
  m.Text:=AValue;
  //m.ShowHint:=True;
  //m.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  //m.Color:=clNone;
  m.WordWrap:=True;
  m.ReadOnly:=True;
  m.BorderStyle:=bsNone;

  Result.memoText:=m;
  x:=x+m.Width+4+2;
  y:=y+m.Height+2;

  Result.Rect.Right:=x;
  Result.Rect.Bottom:=y+1;
  LastInfoY:=y;

  {
  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;
  }
end;

function TFrameDnmpContacts.VisualContactBySender(Sender: TObject
  ): TVisualContact;
var
  i: integer;
begin
  Result:=nil;
  if (Sender is TStaticText) or ((Sender is TLabel)) then
  begin
    for i:=0 to VisualContacts.Count-1 do
    begin
      Result:=(VisualContacts.Items[i] as TVisualContact);
      if Sender=Result.lbName then Exit;
    end;
    Result:=nil;
  end;
end;

procedure TFrameDnmpContacts.OnMouseEnterContact(Sender: TObject);
var
  Item: TVisualContact;
begin
  Item:=VisualContactBySender(Sender);
  if Assigned(Item) then Item.Active:=True;
end;

procedure TFrameDnmpContacts.OnMouseLeaveContact(Sender: TObject);
var
  Item: TVisualContact;
begin
  Item:=VisualContactBySender(Sender);
  if Assigned(Item) then Item.Active:=False;
end;

procedure TFrameDnmpContacts.OnMouseDownContact(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item, Item2: TVisualContact;
  i: integer;
begin
  //Item:=VisualItemAtMousePos();
  Item:=VisualContactBySender(Sender);
  if not Assigned(Item) then Exit;
  for i:=0 to VisualContacts.Count-1 do
  begin
    Item2:=(VisualContacts.Items[i] as TVisualContact);
    if (Item2<>Item) and (Item2.Selected) then Item2.Selected:=False;
  end;
  Item.Selected:=True;
  Contact:=Item.Item;
end;

procedure TFrameDnmpContacts.AfterConstruction();
begin
  inherited AfterConstruction();
  VisualContacts:=TCollection.Create(TVisualContact);
  VisualChatItems:=TCollection.Create(TVisualChatItem);
  VisualInfoItems:=TCollection.Create(TVisualInfoItem);
  FContact:=nil;
  ChatSession:=nil;
  FChat:=nil;
  LastChatY:=0;
end;

procedure TFrameDnmpContacts.BeforeDestruction();
begin
  FreeAndNil(VisualInfoItems);
  FreeAndNil(VisualChatItems);
  FreeAndNil(VisualContacts);
  inherited BeforeDestruction();
end;

procedure TFrameDnmpContacts.UpdateContactsList();
var
  i, n: integer;
  Item: TDnmpContact;
begin
  // clear list
  for i:=ScrollBoxContacts.ControlCount-1 downto 0 do ScrollBoxContacts.Controls[i].Free();
  VisualContacts.Clear();

  if Assigned(ContactList) then
  begin
    for i:=0 to ContactList.Count-1 do
    begin
      Item:=ContactList.Items[i];
      AddVisualContact(Item);
    end;
  end;
end;

procedure TFrameDnmpContacts.ClearChat();
var
  i: integer;
begin
  for i:=ScrollBoxChat.ControlCount-1 downto 0 do ScrollBoxChat.Controls[i].Free();
  VisualChatItems.Clear();
end;

procedure TFrameDnmpContacts.UpdateChat();
var
  i, ii, n: integer;
  Found: boolean;
  Item: TDnmpChatMessage;
begin
  // clear list
  //for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();
  //VisualItems.Clear();

  n:=0;
  if Assigned(ChatSession) then
  begin
    for i:=0 to ChatSession.MessagesList.Count-1 do
    begin
      Item:=ChatSession.MessagesList.Items[i];
      Found:=False;
      for ii:=n to VisualChatItems.Count-1 do
      begin
        if (VisualChatItems.Items[ii] as TVisualChatItem).Item=Item then
        begin
          Found:=True;
          n:=ii+1;
          Break;
        end;
      end;
      if not Found then AddVisualChatItem(Item);
    end;
  end;
end;

procedure TFrameDnmpContacts.UpdateInfo();
var
  i, n: integer;
  Item: TDnmpContact;
begin
  ScrollBoxInfo.Visible:=False;
  // clear list
  for i:=ScrollBoxInfo.ControlCount-1 downto 0 do ScrollBoxInfo.Controls[i].Free();
  VisualInfoItems.Clear();
  LastInfoY:=0;

  if Assigned(Contact) then
  begin
    // brief
    AddVisualInfoItem('Name', Contact.Name);
    AddVisualInfoItem('Address', Contact.AddrStr());
    AddVisualInfoItem('GUID', Contact.GUID);
    AddVisualInfoItem('State', Contact.StateStr());
    // public
    AddVisualInfoItem('Senior GUID', Contact.SeniorGUID);
    AddVisualInfoItem('Status message', Contact.StatusMessage);
    //AddVisualInfoItem('Picture', Contact.Picture);
    AddVisualInfoItem('Rating', IntToStr(Contact.Rating));
    // private
    AddVisualInfoItem('Owner', Contact.Owner);
    AddVisualInfoItem('Location', Contact.Location);
    AddVisualInfoItem('IP address', Contact.IpAddr);
    AddVisualInfoItem('Phone number', Contact.PhoneNo);
    AddVisualInfoItem('Other info', Contact.OtherInfo);
    AddVisualInfoItem('Key', Contact.Key);
    // extra
    AddVisualInfoItem('IsNode', BoolToStr(Contact.IsNode, 'True', 'False'));
    // info
    for i:=0 to Contact.InfoCount()-1 do
    begin
      AddVisualInfoItem(Contact.GetInfoName(i), Contact.GetInfo(i));
    end;
  end;
  ScrollBoxInfo.Visible:=True;
end;

procedure TFrameDnmpContacts.UpdateViewHandler(var AMsg);
begin
  UpdateChat();
end;

end.

