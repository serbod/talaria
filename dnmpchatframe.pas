unit DnmpChatFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, dnmp_chat,
  dnmp_unit, Graphics, LCLType;

type

  { TVisualItem }

  TVisualItem = class(TCollectionItem)
  private
    FSelected: boolean;
    FActive: boolean;
    procedure FSetSelected(Value: boolean);
    procedure FSetActive(Value: boolean);
  public
    Item: TDnmpChatMessage;
    Rect: TRect;
    //Image: TImage;
    lbName: TStaticText;
    lbTime: TStaticText;
    lbText: TLabel;
    Background: TShape;
    property Selected: boolean read FSelected write FSetSelected;
    property Active: boolean read FActive write FSetActive;
  end;

  { TFrameDnmpChat }

  TFrameDnmpChat = class(TFrame)
    edSay: TEdit;
    panSay: TPanel;
    ScrollBox: TScrollBox;
    procedure edSayKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FChat: TDnmpChat;
    VisualItems: TCollection;
    procedure FSetChat(Value: TDnmpChat);
    function AddVisualItem(Item: TDnmpChatMessage): TVisualItem;
  public
    { public declarations }
    Contact: TDnmpContact;
    MessagesList: TDnmpChatMessagesList;
    LastY: integer;
    property Chat: TDnmpChat read FChat write FSetChat;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure Update(); override;
    procedure UpdateViewHandler(var AMsg); message 'UpdateView';
  end;

implementation

{$R *.lfm}

{ TVisualItem }

procedure TVisualItem.FSetSelected(Value: boolean);
begin

end;

procedure TVisualItem.FSetActive(Value: boolean);
begin

end;

{ TFrameDnmpChat }

procedure TFrameDnmpChat.edSayKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if Trim(edSay.Text)<>'' then
    begin
      Chat.SayToContact(Contact, edSay.Text);
    end;
    edSay.Text:='';
    Key:=0;
    Update();
  end;
end;

procedure TFrameDnmpChat.FSetChat(Value: TDnmpChat);
begin
  FChat:=Value;
  if Assigned(MessagesList) then MessagesList.DelObserver(Self);
  MessagesList:=nil;
  if Assigned(Chat) and Assigned(Contact) then
  begin
    MessagesList:=Chat.GetMessagesListForContact(Contact);
    MessagesList.AddObserver(Self);
  end;
  Update();
end;

function TFrameDnmpChat.AddVisualItem(Item: TDnmpChatMessage): TVisualItem;
var
  i, x, y, h: integer;
  //Pan: TPanel;
  Image: TImage;
  st: TStaticText;
  lb: TLabel;
  ss: TStringStream;
begin
  x:=0;
  h:=16;
  y:=LastY+2;

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
  Result.Background.Shape:=stRectangle;
  Result.Background.Pen.Color:=clNone;
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
  st:=TStaticText.Create(ScrollBox);
  st.Name:='lbn'+IntToStr(i);
  st.Parent:=ScrollBox;
  st.AutoSize:=False;
  st.Left:=x;
  st.Top:=y;
  st.Height:=h;
  st.Width:=200;

  st.Font.Size:=10;
  st.Font.Style:=[fsBold];
  st.Caption:=BoolToStr(Item.IsIncoming, Item.RemoteName, 'Me')+':';
  //st.ShowHint:=True;
  //st.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  st.Transparent:=True;
  st.Color:=clNone;
  //st.OnClick:=@OnClickHandler;
  //st.OnMouseDown:=@OnMouseDownHandler;
  //st.OnMouseEnter:=@OnMouseEnterHandler;
  //st.OnMouseLeave:=@OnMouseLeaveHandler;
  //st.PopupMenu:=pmContactList;
  Result.lbName:=st;
  x:=x+st.Width+2;

  // time
  st:=TStaticText.Create(ScrollBox);
  st.Name:='lbt'+IntToStr(i);
  st.Parent:=ScrollBox;
  st.AutoSize:=False;
  st.Left:=x;
  st.Top:=y;
  st.Height:=h;
  st.Width:=100;

  st.Font.Size:=10;
  st.Caption:=FormatDateTime('YYYY-MM-DD HH:NN:SS', Item.Timestamp);
  //st.ShowHint:=True;
  //st.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  st.Transparent:=True;
  st.Color:=clNone;
  //st.OnClick:=@OnClickHandler;
  //st.OnMouseDown:=@OnMouseDownHandler;
  //st.OnMouseEnter:=@OnMouseEnterHandler;
  //st.OnMouseLeave:=@OnMouseLeaveHandler;
  //st.PopupMenu:=pmContactList;
  Result.lbTime:=st;
  x:=x+st.Width+2;

  y:=y+h;
  x:=0;

  // text
  lb:=TLabel.Create(ScrollBox);
  lb.Name:='lbx'+IntToStr(i);
  lb.Parent:=ScrollBox;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=300;

  lb.Font.Size:=8;
  lb.Caption:=Item.Text;
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  lb.Transparent:=True;
  lb.Color:=clNone;
  lb.WordWrap:=True;
  //lb.OnClick:=@OnClickHandler;
  //lb.OnMouseDown:=@OnMouseDownHandler;
  //lb.OnMouseEnter:=@OnMouseEnterHandler;
  //lb.OnMouseLeave:=@OnMouseLeaveHandler;
  //lb.PopupMenu:=pmContactList;
  Result.lbText:=lb;
  x:=x+lb.Width+2;

  Result.Rect.Right:=x;
  Result.Rect.Bottom:=y+h;
  LastY:=y+h;

  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;
end;

procedure TFrameDnmpChat.AfterConstruction();
begin
  inherited AfterConstruction();
  VisualItems:=TCollection.Create(TVisualItem);
end;

procedure TFrameDnmpChat.BeforeDestruction();
begin
  FreeAndNil(VisualItems);
  inherited BeforeDestruction();
end;

procedure TFrameDnmpChat.Update();
var
  i: integer;
begin
  // clear list
  for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();
  VisualItems.Clear();

  LastY:=0;
  if Assigned(MessagesList) then
  begin
    for i:=0 to MessagesList.Count-1 do
    begin
      AddVisualItem(MessagesList.Items[i]);
    end;
  end;
  inherited Update();
end;

procedure TFrameDnmpChat.UpdateViewHandler(var AMsg);
begin
  Update();
end;

end.

