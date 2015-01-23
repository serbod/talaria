unit DnmpContactsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  ActnList, Menus, dnmp_unit, Core, dnmp_chat, Graphics, Dialogs, LCLType,
  LazUTF8, contnrs;

type

  { TVisualContactItem }
  TVisualContactItem = class(TGraphicControl)
  private
    FSelected: boolean;
    FActive: boolean;
    FItem: TDnmpContact;
    procedure FSetSelected(Value: boolean);
    procedure FSetActive(Value: boolean);
    procedure FSetItem(Value: TDnmpContact);
  public
    //Rect: TRect;
    Image: TImage;
    //lbName: TLabel;
    //Background: TShape;
    property Selected: boolean read FSelected write FSetSelected;
    property Active: boolean read FActive write FSetActive;
    property Item: TDnmpContact read FItem write FSetItem;
    procedure Paint(); override;
  end;

  { TVisualChatItem }

  TVisualChatItem = class(TGraphicControl)
  protected
    FSelected: boolean;
    FActive: boolean;
    FItem: TDnmpChatMessage;
    procedure FSetItem(Value: TDnmpChatMessage); virtual;
  public
    //Rect: TRect;
    //Image: TImage;
    //lbName: TLabel;
    //lbTime: TLabel;
    //lbText: TLabel;
    //Background: TShape;
    Chat: TDnmpChat;
    property Item: TDnmpChatMessage read FItem write FSetItem;
    procedure Paint(); override;
    { Calculate Height value, with multi-line text field. Text and Width must be set }
    function GetHeight(): integer; virtual;
  end;

  { TVisualChatFileItem }

  TVisualChatFileItem = class(TVisualChatItem)
  protected
    FFileItem: TDnmpChatFileMessage;
    procedure FSetItem(Value: TDnmpChatMessage); override;
  public
    Bmp: TBitmap;
    procedure Paint(); override;
    { Calculate Height value, according to contained picture }
    function GetHeight(): integer; override;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
  end;

  TVisualInfoItem = class(TCollectionItem)
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
    actConnectToNode: TAction;
    actApprove: TAction;
    actEditContact: TAction;
    actAddContact: TAction;
    actSendFileToContact: TAction;
    actRequestInfo: TAction;
    alContacts: TActionList;
    edChatSay: TEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
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
    procedure actAddContactExecute(Sender: TObject);
    procedure actAddToFavoritesExecute(Sender: TObject);
    procedure actApproveExecute(Sender: TObject);
    procedure actConnectToNodeExecute(Sender: TObject);
    procedure actContactsAllExecute(Sender: TObject);
    procedure actContactsFavoritesExecute(Sender: TObject);
    procedure actContactsFoundExecute(Sender: TObject);
    procedure actContactsGuestsExecute(Sender: TObject);
    procedure actContactsNodesExecute(Sender: TObject);
    procedure actContactsPointsExecute(Sender: TObject);
    procedure actDeleteContactExecute(Sender: TObject);
    procedure actEditContactExecute(Sender: TObject);
    procedure actFindContactsExecute(Sender: TObject);
    procedure actRequestInfoExecute(Sender: TObject);
    procedure actSendFileToContactExecute(Sender: TObject);
    procedure edChatSayKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pmContactsPopup(Sender: TObject);
    procedure ScrollBoxChatResize(Sender: TObject);
  private
    { private declarations }
    FServ: TServiceDnmp;
    FChat: TDnmpChat;
    FContact: TDnmpContact;
    FContactList: TDnmpContactList;
    VisualContacts: TComponentList;
    VisualChatItems: TComponentList;
    VisualInfoItems: TCollection;
    Mgr: TDnmpManager;
    ChatSession: TDnmpChatSession;
    LastVisualContactItem: TControl;
    LastVisualChatItem: TControl;
    procedure FSetServ(Value: TServiceDnmp);
    procedure FSetChat(Value: TDnmpChat);
    procedure FSetContact(Value: TDnmpContact);
    procedure FSetContactList(Value: TDnmpContactList);
    function AddVisualContact(Item: TDnmpContact): TVisualContactItem;
    function AddVisualChatItem(Item: TDnmpChatMessage): TVisualChatItem;
    function AddVisualInfoItem(AName, AValue: string): TVisualInfoItem;
    function VisualContactBySender(Sender: TObject): TVisualContactItem;
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
    procedure Update(); override;
    procedure UpdateViewHandler(var AMsg); message 'UpdateView';
  end;

implementation

uses dnmp_services, GraphicTools;

{$R *.lfm}

{ TVisualChatItem }

procedure TVisualChatItem.FSetItem(Value: TDnmpChatMessage);
begin
  FItem:=Value;
end;

procedure TVisualChatItem.Paint();
var
  i, x, y, h, w, n, nn: integer;
  Image: TImage;
  s, ss: string;
begin
  if Assigned(OnPaint) then
  begin
    inherited Paint();
    Exit;
  end;
  h:=16;

  Self.Canvas.AntialiasingMode:=TAntialiasingMode.amOn;

  // frame
  if FSelected then Self.Canvas.Brush.Color:=clActiveCaption
  else if FActive then Self.Canvas.Brush.Color:=clInactiveCaption
  else Self.Canvas.Brush.Color:=clNone;

  Self.Canvas.Pen.Color:=clActiveBorder;
  //Self.Canvas.RoundRect(Self.ClientRect, 4, 4);
  Self.Canvas.Rectangle(Self.ClientRect);

  // author name
  x:=6;
  y:=2;
  Self.Canvas.Font.Size:=8;
  Self.Canvas.Font.Style:=[fsBold];
  s:=BoolToStr(Item.IsIncoming, Item.RemoteName, Chat.Author.Name);
  Self.Canvas.TextOut(x, y, s);

  // time
  x:=x+200;
  //y:=;
  Self.Canvas.Font.Size:=8;
  Self.Canvas.Font.Style:=[];
  s:=FormatDateTime('YYYY-MM-DD HH:NN:SS', Item.Timestamp);
  Self.Canvas.TextOut(x, y, s);

  // text
  x:=6;
  y:=y+h;
  Self.Canvas.Font.Size:=10;
  Self.Canvas.Font.Style:=[];

  // calculate text height
  ss:=Item.Text;
  w:=Self.Width-10;
  n:=Self.Canvas.TextFitInfo(ss, w);
  while n<UTF8Length(ss) do
  begin
    s:=UTF8Copy(ss, 1, n);
    Self.Canvas.TextOut(x, y, s);
    y:=y+h;
    ss:=UTF8Copy(ss, n, maxint);
    n:=Self.Canvas.TextFitInfo(ss, w);
  end;
  Self.Canvas.TextOut(x, y, ss);
  //Self.Height:=y+h+2;

end;

function TVisualChatItem.GetHeight(): integer;
var
  h, w, n: integer;
  ss: string;
begin
  // calculate text height
  h:=16;
  Result:=2+h;
  ss:=Item.Text;
  w:=Self.Width-10;
  n:=Self.Canvas.TextFitInfo(ss, w);
  while n<UTF8Length(ss) do
  begin
    Result:=Result+h;
    ss:=UTF8Copy(ss, n, maxint);
    n:=Self.Canvas.TextFitInfo(ss, w);
  end;
  Result:=Result+h+2;
end;

{ TVisualChatFileItem }

procedure TVisualChatFileItem.FSetItem(Value: TDnmpChatMessage);
begin
  inherited FSetItem(Value);
  FFileItem:=(Value as TDnmpChatFileMessage);
  if not Assigned(FFileItem) then Exit;
  FFileItem.FileContent.Position:=0;
  Self.Bmp.LoadFromStream(FFileItem.FileContent);
end;

procedure TVisualChatFileItem.Paint();
var
  i, x, y, h, n, nn: integer;
  Image: TImage;
  s, ss: string;
begin
  if Assigned(OnPaint) then
  begin
    inherited Paint();
    Exit;
  end;
  h:=16;

  Self.Canvas.AntialiasingMode:=TAntialiasingMode.amOn;

  // frame
  Self.Canvas.Pen.Color:=clActiveBorder;
  Self.Canvas.RoundRect(Self.ReadBounds, 4, 4);

  if not Assigned(Item) then Exit;

  // author name
  x:=6;
  y:=2;
  Self.Canvas.Font.Size:=8;
  Self.Canvas.Font.Style:=[fsBold];
  s:=BoolToStr(Item.IsIncoming, Item.RemoteName, Chat.Author.Name);
  Self.Canvas.TextOut(x, y, s);

  // time
  x:=x+200;
  //y:=;
  Self.Canvas.Font.Size:=8;
  Self.Canvas.Font.Style:=[];
  s:=FormatDateTime('YYYY-MM-DD HH:NN:SS', Item.Timestamp);
  Self.Canvas.TextOut(x, y, s);

  y:=y+h;
  // image
  x:=4;
  Self.Canvas.Draw(x, y, Self.Bmp);

  // file size
  x:=x+64+8;
  //y:=;
  Self.Canvas.Font.Size:=10;
  Self.Canvas.Font.Style:=[];
  Self.Canvas.TextOut(x, y, 'Size: '+IntToStr(FFileItem.FileSize)+' bytes');

  // file name
  x:=x+100;
  Self.Canvas.Font.Size:=10;
  Self.Canvas.Font.Style:=[];
  Self.Canvas.TextOut(x, y, FFileItem.FileName);


end;

function TVisualChatFileItem.GetHeight(): integer;
begin
  Result:=Bmp.Height+4;
end;

procedure TVisualChatFileItem.AfterConstruction();
begin
  inherited AfterConstruction();
  Bmp:=TBitmap.Create();
end;

procedure TVisualChatFileItem.BeforeDestruction();
begin
  FreeAndNil(Bmp);
  inherited BeforeDestruction();
end;

{ TVisualContactItem }

procedure TVisualContactItem.FSetSelected(Value: boolean);
begin
  FSelected:=Value;
  Invalidate();
  {
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
  }
end;

procedure TVisualContactItem.FSetActive(Value: boolean);
begin
  FActive:=Value;
  Invalidate();
  {
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
  }
end;

procedure TVisualContactItem.FSetItem(Value: TDnmpContact);
var
  ss: TStringStream;
begin
  FItem:=Value;
  if Assigned(Image) then FreeAndNil(Image);
  if not Assigned(FItem) then Exit;

  // image
  Image:=TImage.Create(Self);
  //Image.Name:='img'+IntToStr(i);
  //Image.Parent:=ScrollBoxContacts;
  Image.Left:=0;
  Image.Top:=0;
  Image.Height:=Self.Height-2;
  Image.Width:=Self.Height-2;

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
    Core.SetDefaultContactPicture(Image.Picture);
    //Image.Picture.Assign(imgDefault.Picture);
  end;

end;

procedure TVisualContactItem.Paint();
var
  i, x, y, h: integer;
  //Pan: TPanel;
  StateImage: TBitmap;
  lb: TLabel;
  ss: TStringStream;
  s: string;
  r1, r2: TRect;
begin
  inherited Paint();
  h:=24;

  Self.Canvas.AntialiasingMode:=TAntialiasingMode.amOn;

  // frame
  if FSelected then Self.Canvas.Brush.Color:=clActiveCaption
  else if FActive then Self.Canvas.Brush.Color:=clInactiveCaption
  else Self.Canvas.Brush.Color:=clNone;

  Self.Canvas.Pen.Color:=clActiveBorder;
  //Self.Canvas.RoundRect(Self.BoundsRect, 4, 4);
  //Self.Canvas.Rectangle(Self.BoundsRect);
  Self.Canvas.Rectangle(Self.ClientRect);

  // state image
  i:=ciIconUserRed;
  if Item.State=TDnmpContactState.asOnline then i:=ciIconUserBlue
  else if Item.State=TDnmpContactState.asBusy then i:=ciIconUserAway;
  StateImage:=TBitmap.Create();
  Icon16ToBitmap(StateImage, i);
  x:=2;
  y:=(h div 2)-(StateImage.Height div 2);
  Self.Canvas.Draw(x, y, StateImage);

  // avatar image
  if Assigned(Image) then
  begin
    x:=2+StateImage.Width+4;
    y:=2;
    r1:=Rect(x, y, x+h-2, y+h-2);
    //r2:=Image.BoundsRect;
    //Self.Canvas.CopyRect(r1, Image.Canvas, r2);
    //Self.Canvas.Draw(x, y, Image.Picture.Bitmap);
    Self.Canvas.StretchDraw(r1, Image.Picture.Bitmap);
  end;

  x:=x+Image.Width+2;

  // contact name
  y:=2;
  Self.Canvas.Font.Size:=12;
  Self.Canvas.Font.Style:=[];
  s:=Item.Name;
  Self.Canvas.TextOut(x, y, s);

end;

{ TFrameDnmpContacts }

procedure TFrameDnmpContacts.actDeleteContactExecute(Sender: TObject);
begin
  if MessageDlg('Delete item', 'Are you sure?', mtConfirmation, mbYesNo, 0)<>mrYes then Exit;
  if Assigned(ContactList) and Assigned(Contact) then ContactList.Extract(Contact);
  if Self.ContactList = Mgr.ContactList then
  begin
    Mgr.NodeList.Extract(Contact);
    Mgr.PointList.Extract(Contact);
    Mgr.MyPassport.FavoriteContactsList.Extract(Contact);
  end;
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.actEditContactExecute(Sender: TObject);
begin
  Core.ShowLinkInfo(Contact);
end;

procedure TFrameDnmpContacts.actAddToFavoritesExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Assigned(Contact) then Mgr.MyPassport.FavoriteContactsList.AddItem(Contact);
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.actAddContactExecute(Sender: TObject);
var
  Item: TDnmpContact;
begin
  if not Assigned(ContactList) then Exit;
  Item:=TDnmpContact.Create();
  Item.Name:='New contact';
  Item.Addr:=EmptyAddr();
  ContactList.AddItem(Item);
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.actApproveExecute(Sender: TObject);
begin
  if (not Assigned(Mgr)) or (not Assigned(Contact)) then Exit;
  Mgr.Approve(Contact);
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.actConnectToNodeExecute(Sender: TObject);
begin
  if (not Assigned(Mgr)) or (not Assigned(Contact)) then Exit;
  Mgr.StartNodeConnection(Contact);
  UpdateContactsList();
end;

procedure TFrameDnmpContacts.actContactsAllExecute(Sender: TObject);
begin
  ContactList:=Mgr.ContactList;
end;

procedure TFrameDnmpContacts.actContactsFavoritesExecute(Sender: TObject);
begin
  ContactList:=Mgr.MyPassport.FavoriteContactsList;
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

procedure TFrameDnmpContacts.actSendFileToContactExecute(Sender: TObject);
var
  FileName, FileInfo: string;
  s: AnsiString;
begin
  if not Assigned(Contact) then Exit;
  FileName:=Core.SelectFileName();
  if FileName='' then Exit;
  // Extract preview for file
  s:=GetFilePreview(FileName, Point(64,64), FileInfo);
  if Length(s)>0 then
  begin
    Chat.SendFileToContact(Contact, FileName, FileInfo, s);
  end
  else
  begin
    Chat.SendFileToContact(Contact, FileName);
  end;
end;

procedure TFrameDnmpContacts.edChatSayKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: integer;
  FileInfo: string;
  s: AnsiString;
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
  if ((Key = VK_INSERT) or (Key = VK_V)) and (ssCtrl in Shift) then
  begin
    // insert picture
    s:=GetFilePreview('', Point(64, 64), FileInfo);
    if Length(s)>0 then
    begin
      Chat.SendFileToContact(Contact, '', FileInfo, s);
    end
  end;
end;

procedure TFrameDnmpContacts.pmContactsPopup(Sender: TObject);
var
  IsNode: Boolean;
begin
  IsNode:=False;
  if Assigned(Contact) then IsNode:=Contact.IsNode;
  actConnectToNode.Visible:=IsNode;
  actApprove.Visible:=actContactsGuests.Checked;
end;

procedure TFrameDnmpContacts.ScrollBoxChatResize(Sender: TObject);
begin
  //edChatSay.Text:='h='+IntToStr(ScrollBoxChat.ClientHeight)+'  v='+IntToStr(ScrollBoxChat.ClientWidth);
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
    UpdateChat();
  end;
  // else clear chat
end;

procedure TFrameDnmpContacts.FSetContact(Value: TDnmpContact);
begin
  if FContact=Value then Exit;
  ClearChat();
  if Assigned(ChatSession) then ChatSession.MessagesList.DelObserver(Self);
  ChatSession:=nil;
  FContact:=Value;
  LastChatY:=0;

  if Assigned(Contact) then
  begin
    // update chat
    Chat:=(FServ.ServMgr.GetService(csCHAT, '') as TDnmpChat);

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

{
function TFrameDnmpContacts.AddVisualContact(Item: TDnmpContact): TVisualContactItem;
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
  Result:=(VisualContacts.Add() as TVisualContactItem);
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
}

function TFrameDnmpContacts.AddVisualContact(Item: TDnmpContact): TVisualContactItem;
var
  i, x, y, h: integer;
begin
  x:=2;
  h:=24;
  i:=VisualContacts.Count;
  y:=(h+2)*i;

  Result:=TVisualContactItem.Create(ScrollBoxContacts);
  Result.Name:='vc'+IntToStr(i);
  VisualContacts.Add(Result);

  //Result.Top:=y;
  //Result.Left:=x;
  Result.Width:=ScrollBoxContacts.ClientWidth-4;
  Result.Height:=h;
  //Result.Anchors:=[akTop, akLeft];
  Result.Anchors:=[akTop, akLeft, akRight];
  Result.Parent:=ScrollBoxContacts;

  Result.Item:=Item;
  //ScrollBoxContacts.UpdateScrollbars();

  if Assigned(LastVisualContactItem) then
  begin
    Result.AnchorToNeighbour(akTop, 2, LastVisualContactItem);
  end;
  LastVisualContactItem:=Result;

  Result.OnMouseDown:=@OnMouseDownContact;
  Result.OnMouseEnter:=@OnMouseEnterContact;
  Result.OnMouseLeave:=@OnMouseLeaveContact;
  Result.PopupMenu:=pmContacts;
end;

{
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
  lb.PopupMenu:=pmContacts;
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
}

function TFrameDnmpContacts.AddVisualChatItem(Item: TDnmpChatMessage
  ): TVisualChatItem;
var
  i, x, y, h: integer;
begin
  x:=2;
  h:=16;
  y:=LastChatY+2;
  i:=VisualChatItems.Count;

  if (Item is TDnmpChatFileMessage) then
  begin
    Result:=TVisualChatFileItem.Create(ScrollBoxChat);
    (Result as TVisualChatFileItem).Item:=(Item as TDnmpChatFileMessage);
  end
  else
  begin
    Result:=TVisualChatItem.Create(ScrollBoxChat);
    Result.Item:=Item;
  end;

  Result.Name:='vchi'+IntToStr(i);
  VisualChatItems.Add(Result);

  Result.Chat:=Chat;

  Result.Parent:=ScrollBoxChat;
  Result.Top:=y;
  Result.Left:=x;
  Result.Width:=ScrollBoxChat.ClientWidth-4;
  Result.Height:=Result.GetHeight();
  //Result.Anchors:=[akTop, akLeft];
  Result.Anchors:=[akTop, akLeft, akRight];
  if Assigned(LastVisualChatItem) then
  begin
    Result.AnchorToNeighbour(akTop, 2, LastVisualChatItem);
  end;

  LastChatY:=Result.Top+Result.Height;
  //ScrollBoxChat.ClientHeight:=LastChatY;
  //ScrollBoxContacts.UpdateScrollbars();

  //Result.OnMouseDown:=@OnMouseDown;
  //Result.OnMouseEnter:=@OnMouseEnterContact;
  //Result.OnMouseLeave:=@OnMouseLeaveContact;
  //Result.PopupMenu:=pmContacts;

  LastVisualChatItem:=Result;

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
  ): TVisualContactItem;
var
  i: integer;
begin
  Result:=nil;
  if (Sender is TVisualContactItem) then
    Result:=(Sender as TVisualContactItem);
end;

procedure TFrameDnmpContacts.OnMouseEnterContact(Sender: TObject);
var
  Item: TVisualContactItem;
begin
  Item:=VisualContactBySender(Sender);
  if Assigned(Item) then Item.Active:=True;
end;

procedure TFrameDnmpContacts.OnMouseLeaveContact(Sender: TObject);
var
  Item: TVisualContactItem;
begin
  Item:=VisualContactBySender(Sender);
  if Assigned(Item) then Item.Active:=False;
end;

procedure TFrameDnmpContacts.OnMouseDownContact(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item, Item2: TVisualContactItem;
  i: integer;
begin
  //Item:=VisualItemAtMousePos();
  Item:=VisualContactBySender(Sender);
  if not Assigned(Item) then Exit;
  for i:=0 to VisualContacts.Count-1 do
  begin
    Item2:=(VisualContacts.Items[i] as TVisualContactItem);
    if (Item2<>Item) and (Item2.Selected) then Item2.Selected:=False;
  end;
  Item.Selected:=True;
  Contact:=Item.Item;
end;

procedure TFrameDnmpContacts.AfterConstruction();
begin
  inherited AfterConstruction();
  VisualContacts:=TComponentList.Create();
  VisualChatItems:=TComponentList.Create();
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
  actContactsPoints.Visible:=Assigned(Mgr) and Mgr.MyInfo.IsNode;
  actContactsGuests.Visible:=Assigned(Mgr) and Mgr.MyInfo.IsNode;

  // clear list
  for i:=ScrollBoxContacts.ControlCount-1 downto 0 do ScrollBoxContacts.Controls[i].Free();
  VisualContacts.Clear();
  LastVisualContactItem:=nil;

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
  LastVisualChatItem:=nil;
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
  //LastVisualChatItem:=nil;

  n:=0;
  if Assigned(ChatSession) then
  begin
    for i:=0 to ChatSession.MessagesList.Count-1 do
    begin
      // find items, that not have visual control
      Item:=ChatSession.MessagesList.Items[i];
      Found:=False;
      for ii:=n to VisualChatItems.Count-1 do
      begin
        if (VisualChatItems.Items[ii] as TVisualChatItem).Item.ID = Item.ID then
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

procedure TFrameDnmpContacts.Update();
begin
  UpdateContactsList();
  inherited Update;
end;

procedure TFrameDnmpContacts.UpdateViewHandler(var AMsg);
begin
  UpdateChat();
end;

end.

