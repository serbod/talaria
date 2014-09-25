unit ContactListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Core, dnmp_unit,
  dnmp_services, ExtCtrls, StdCtrls, ActnList, Menus, Graphics, Types, Dialogs;

type

  { TVisualItem }

  TVisualItem = class(TCollectionItem)
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

  { TFrameContactList }

  TFrameContactList = class(TFrame)
    actDeleteContact: TAction;
    actFindContacts: TAction;
    actAddToFavorites: TAction;
    actEditContact: TAction;
    actChat: TAction;
    actRequestInfo: TAction;
    actTest1: TAction;
    alContactList: TActionList;
    imgDefault: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    pmContactList: TPopupMenu;
    ScrollBox: TScrollBox;
    tcGroups: TTabControl;
    procedure actAddToFavoritesExecute(Sender: TObject);
    procedure actChatExecute(Sender: TObject);
    procedure actDeleteContactExecute(Sender: TObject);
    procedure actEditContactExecute(Sender: TObject);
    procedure actFindContactsExecute(Sender: TObject);
    procedure actRequestInfoExecute(Sender: TObject);
    procedure actTest1Execute(Sender: TObject);
    procedure tcGroupsChange(Sender: TObject);
  private
    { private declarations }
    FServ: TServiceDnmp;
    VisualItems: TCollection;
    procedure FSetServ(Value: TServiceDnmp);
    procedure AddTestContacts();
    procedure OnClickHandler(Sender: TObject);
    procedure OnMouseEnterHandler(Sender: TObject);
    procedure OnMouseLeaveHandler(Sender: TObject);
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function AddVisualItem(Item: TDnmpContact): TVisualItem;
    function VisualItemAtPos(pos: TPoint): TVisualItem;
    function VisualItemBySender(Sender: TObject): TVisualItem;
    function SelectedItem(): TDnmpContact;
    function SelectedList(): TDnmpContactList;
  public
    { public declarations }
    Mgr: TDnmpManager;
    //ContactList: TDnmpContactList;
    property Serv: TServiceDnmp read FServ write FSetServ;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure UpdateList();
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TVisualItem }

procedure TVisualItem.FSetSelected(Value: boolean);
begin
  FSelected:=Value;
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
end;

procedure TVisualItem.FSetActive(Value: boolean);
begin
  FActive:=Value;
  if Assigned(Background) then
  begin
    if FSelected then Background.Brush.Color:=clActiveCaption
    else if FActive then Background.Brush.Color:=clInactiveCaption
    else Background.Brush.Color:=clNone;
  end;
end;

{ TFrameContactList }

procedure TFrameContactList.tcGroupsChange(Sender: TObject);
begin
  Update();
end;

procedure TFrameContactList.FSetServ(Value: TServiceDnmp);
begin
  Mgr:=nil;
  FServ:=Value;
  if Assigned(FServ) then Mgr:=FServ.Mgr;
  Update();
end;

procedure TFrameContactList.actTest1Execute(Sender: TObject);
begin
  AddTestContacts();
  Update();
end;

procedure TFrameContactList.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TVisualItem;
begin
  //Item:=VisualItemAtPos(Point(X,Y));
  //if Assigned(Item) then Item.Selected:=not Item.Selected;
  OnClickHandler(Sender);
end;

procedure TFrameContactList.actDeleteContactExecute(Sender: TObject);
begin
  if MessageDlg('Delete item', 'Are you sure?', mtConfirmation, mbYesNo, 0)<>mrYes then Exit;
  if Assigned(SelectedList()) and Assigned(SelectedItem()) then SelectedList().Extract(SelectedItem());
end;

procedure TFrameContactList.actEditContactExecute(Sender: TObject);
begin
  if Assigned(SelectedItem()) then Core.ShowLinkInfo(SelectedItem());
end;

procedure TFrameContactList.actAddToFavoritesExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  if Assigned(SelectedItem()) then Mgr.MyPassport.ContactsList.AddItem(SelectedItem());
end;

procedure TFrameContactList.actChatExecute(Sender: TObject);
begin
  if not Assigned(Serv) then Exit;
  if Assigned(SelectedItem()) then Serv.ShowPrivateChatRoom(SelectedItem());
end;

procedure TFrameContactList.actFindContactsExecute(Sender: TObject);
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
end;

procedure TFrameContactList.actRequestInfoExecute(Sender: TObject);
var
  Item: TDnmpContact;
begin
  if not Assigned(Mgr) then Exit;
  Item:=SelectedItem();
  if Assigned(Item) then Mgr.RequestInfoByAddr(Item.Addr);
end;

procedure TFrameContactList.AddTestContacts();
var
  i: integer;
  Item: TDnmpContact;
begin
  for i:=0 to 50 do
  begin
    Item:=TDnmpContact.Create();
    Item.Addr.Node:=1;
    Item.Addr.Point:=1+i;
    Item.GUID:=GenerateGUID();
    Item.Name:='Point '+AddrToStr(Item.Addr);

    //ContactList.Add(Item);
  end;
end;

procedure TFrameContactList.OnClickHandler(Sender: TObject);
var
  Item, Item2: TVisualItem;
  i: integer;
begin
  //Item:=VisualItemAtMousePos();
  Item:=VisualItemBySender(Sender);
  if not Assigned(Item) then Exit;
  for i:=0 to VisualItems.Count-1 do
  begin
    Item2:=(VisualItems.Items[i] as TVisualItem);
    if (Item2<>Item) and (Item2.Selected) then Item2.Selected:=False;
  end;
  Item.Selected:=True;
end;

procedure TFrameContactList.OnMouseEnterHandler(Sender: TObject);
var
  Item: TVisualItem;
begin
  Item:=VisualItemBySender(Sender);
  if Assigned(Item) then Item.Active:=True;
end;

procedure TFrameContactList.OnMouseLeaveHandler(Sender: TObject);
var
  Item: TVisualItem;
begin
  Item:=VisualItemBySender(Sender);
  if Assigned(Item) then Item.Active:=False;
end;

function TFrameContactList.AddVisualItem(Item: TDnmpContact): TVisualItem;
var
  i, x, y, h: integer;
  //Pan: TPanel;
  Image: TImage;
  lb: TLabel;
  ss: TStringStream;
begin
  x:=0;
  h:=24;
  i:=VisualItems.Count;
  y:=(h+2)*i;

  //Result:=TVisualItem.Create();
  Result:=(VisualItems.Add() as TVisualItem);
  Result.Item:=Item;
  Result.Rect.Top:=y;
  Result.Rect.Left:=x;

  // selected background
  Result.Background:=TShape.Create(ScrollBox);
  Result.Background.Name:='bg'+IntToStr(i);
  Result.Background.Parent:=ScrollBox;
  Result.Background.Left:=x;
  Result.Background.Top:=y;
  //Result.Background.Shape:=stRectangle;
  Result.Background.Shape:=stRoundRect;
  Result.Background.Pen.Color:=cl3DLight;
  //Result.Background.OnClick:=@OnClickHandler;
  //Result.Background.On

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

  // name
  lb:=TLabel.Create(ScrollBox);
  lb.Name:='lb'+IntToStr(i);
  lb.Parent:=ScrollBox;
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
  lb.OnClick:=@OnClickHandler;
  lb.OnMouseDown:=@OnMouseDownHandler;
  lb.OnMouseEnter:=@OnMouseEnterHandler;
  lb.OnMouseLeave:=@OnMouseLeaveHandler;
  lb.PopupMenu:=pmContactList;
  Result.lbName:=lb;
  x:=x+lb.Width+2;

  Result.Rect.Right:=x;
  Result.Rect.Bottom:=y+h;

  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;
end;

function TFrameContactList.VisualItemAtPos(pos: TPoint): TVisualItem;
var
  i: integer;
begin
  for i:=0 to VisualItems.Count-1 do
  begin
    Result:=(VisualItems.Items[i] as TVisualItem);
    if PtInRect(Result.Rect, pos) then Exit;
  end;
  Result:=nil;
end;

function TFrameContactList.VisualItemBySender(Sender: TObject): TVisualItem;
var
  i: integer;
begin
  Result:=nil;
  if (Sender is TStaticText) or ((Sender is TLabel)) then
  begin
    for i:=0 to VisualItems.Count-1 do
    begin
      Result:=(VisualItems.Items[i] as TVisualItem);
      if Sender=Result.lbName then Exit;
    end;
    Result:=nil;
  end;
end;

function TFrameContactList.SelectedItem(): TDnmpContact;
var
  i: integer;
begin
  Result:=nil;
  for i:=0 to VisualItems.Count-1 do
  begin
    if (VisualItems.Items[i] as TVisualItem).Selected then
    begin
      Result:=(VisualItems.Items[i] as TVisualItem).Item;
      Exit;
    end;
  end;
end;

function TFrameContactList.SelectedList(): TDnmpContactList;
var
  n: integer;
begin
  Result:=nil;
  n:=tcGroups.TabIndex;

  if n=0 then // all
  begin
    Result:=Mgr.ContactList;
  end
  else if n=1 then // favorites
  begin
    Result:=Mgr.MyPassport.ContactsList;
  end
  else if n=2 then // nodes
  begin
    Result:=Mgr.NodeList;
  end
  else if n=3 then // points
  begin
    Result:=Mgr.PointList;
  end
  else if n=4 then // guests
  begin
    Result:=Mgr.UnapprovedList;
  end
  else if n=5 then // found
  begin
    Result:=Mgr.TmpContactList;
  end;
end;

procedure TFrameContactList.AfterConstruction();
begin
  inherited AfterConstruction();
  VisualItems:=TCollection.Create(TVisualItem);
end;

procedure TFrameContactList.BeforeDestruction();
begin
  FreeAndNil(VisualItems);
  inherited BeforeDestruction();
end;

procedure TFrameContactList.UpdateList();
var
  i, n: integer;
  Item: TDnmpContact;
  ItemList: TDnmpContactList;
begin
  n:=tcGroups.TabIndex;

  // clear list
  for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();
  VisualItems.Clear();

  ItemList:=SelectedList();
  if Assigned(ItemList) then
  begin
    for i:=0 to ItemList.Count-1 do
    begin
      Item:=ItemList.Items[i];
      AddVisualItem(Item);
    end;
  end;
end;

procedure TFrameContactList.Update();
begin
  UpdateList();
  inherited Update();
end;

end.

