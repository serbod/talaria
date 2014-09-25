unit DnmpChatFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, dnmp_chat,
  dnmp_unit, Graphics, LCLType, LazUTF8;

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
    lbName: TLabel;
    lbTime: TLabel;
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
    ChatSession: TDnmpChatSession;
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
  if Assigned(ChatSession) then ChatSession.MessagesList.DelObserver(Self);
  ChatSession:=nil;
  if Assigned(Chat) and Assigned(Contact) then
  begin
    ChatSession:=Chat.GetChatSessionForContact(Contact);
    ChatSession.MessagesList.AddObserver(Self);
  end;
  Update();
end;

function TFrameDnmpChat.AddVisualItem(Item: TDnmpChatMessage): TVisualItem;
var
  i, x, y, h, n, nn: integer;
  //Pan: TPanel;
  Image: TImage;
  st: TStaticText;
  lb: TLabel;
  s, ss: string;
begin
  x:=0;
  h:=16;
  y:=LastY+2;
  i:=VisualItems.Count;

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
  Result.Background.Pen.Color:=cl3DLight;
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
  lb:=TLabel.Create(ScrollBox);
  lb.Name:='lbn'+IntToStr(i);
  lb.Parent:=ScrollBox;
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
  lb:=TLabel.Create(ScrollBox);
  lb.Name:='lbt'+IntToStr(i);
  lb.Parent:=ScrollBox;
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
  lb:=TLabel.Create(ScrollBox);
  lb.Name:='lbx'+IntToStr(i);
  lb.Parent:=ScrollBox;
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
  LastY:=y;

  Result.Background.Width:=Result.Rect.Right-Result.Rect.Left;
  Result.Background.Height:=Result.Rect.Bottom-Result.Rect.Top;
end;

procedure TFrameDnmpChat.AfterConstruction();
begin
  inherited AfterConstruction();
  VisualItems:=TCollection.Create(TVisualItem);
  Contact:=nil;
  ChatSession:=nil;
  Chat:=nil
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
  if Assigned(ChatSession) then
  begin
    for i:=0 to ChatSession.MessagesList.Count-1 do
    begin
      AddVisualItem(ChatSession.MessagesList.Items[i]);
    end;
  end;
  inherited Update();
end;

procedure TFrameDnmpChat.UpdateViewHandler(var AMsg);
begin
  Update();
end;

end.

