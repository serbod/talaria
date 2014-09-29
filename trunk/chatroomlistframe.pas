unit ChatRoomListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ActnList,
  Buttons, Core, Graphics;

type
  TVisualItem = class(TCollectionItem)
  public
    Item: TChatRoom;
    Rect: TRect;
    Image: TImage;
    lbName: TStaticText;
    lbUsers: TStaticText;
    lbTopic: TStaticText;
  end;

  { TFrameChatRoomList }

  TFrameChatRoomList = class(TFrame)
    actCreateNew: TAction;
    actRequestList: TAction;
    actUpdateView: TAction;
    alChatRooms: TActionList;
    btnCreateNew: TBitBtn;
    btnRequestList: TBitBtn;
    Button1: TButton;
    edNameFilter: TEdit;
    panBottom: TPanel;
    ScrollBox: TScrollBox;
    procedure actRequestListExecute(Sender: TObject);
    procedure actUpdateViewExecute(Sender: TObject);
  private
    { private declarations }
    VisualItems: TCollection;
    function AddVisualItem(Item: TChatRoom): TVisualItem;
    procedure OnMouseEnterHandler(Sender: TObject);
    procedure OnMouseLeaveHandler(Sender: TObject);
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function SelectedItem(): TChatRoom;
  public
    { public declarations }
    Serv: TServiceDnmp;
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFrameChatRoomList }

procedure TFrameChatRoomList.actUpdateViewExecute(Sender: TObject);
begin
  Update();
end;

procedure TFrameChatRoomList.actRequestListExecute(Sender: TObject);
begin
  if not Assigned(Serv) then Exit;
  Serv.ServMgr.RequestList('GRPC');
end;

function TFrameChatRoomList.AddVisualItem(Item: TChatRoom): TVisualItem;
var
  i, x, y, h: integer;
  //Pan: TPanel;
  Image: TImage;
  //lb: TLabel;
  lb: TStaticText;
  ss: TStringStream;
begin
  x:=0;
  h:=24;
  i:=VisualItems.Count;
  y:=(h+2)*i;

  Result:=(VisualItems.Add() as TVisualItem);
  Result.Item:=Item;
  Result.Rect.Top:=y;
  Result.Rect.Left:=x;

  // image
  Image:=TImage.Create(ScrollBox);
  Image.Name:='img'+IntToStr(i);
  Image.Parent:=ScrollBox;
  Image.Left:=x;
  Image.Top:=y;
  Image.Height:=h;
  Image.Width:=h;

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
    //Image.Picture.Assign(imgDefault.Picture);
    Image.Picture.Clear();
  end;
  Result.Image:=Image;
  x:=x+Image.Width+4;

  // users
  lb:=TStaticText.Create(ScrollBox);
  lb.Name:='lbu'+IntToStr(i);
  lb.Parent:=ScrollBox;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=30;
  lb.Alignment:=taRightJustify;
  //lb.BorderStyle:=TStaticBorderStyle.sbsSingle;
  lb.Font.Height:=h-2;
  lb.Caption:=IntToStr(Item.ContactCount);
  Result.lbUsers:=lb;
  x:=x+lb.Width+4;

  // name
  lb:=TStaticText.Create(ScrollBox);
  lb.Name:='lbn'+IntToStr(i);
  lb.Parent:=ScrollBox;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=100;
  //lb.BorderStyle:=TStaticBorderStyle.sbsSingle;
  lb.Font.Height:=h-2;
  lb.Caption:=Item.Name;
  lb.OnMouseEnter:=@OnMouseEnterHandler;
  lb.OnMouseLeave:=@OnMouseLeaveHandler;
  OnMouseLeaveHandler(lb);
  lb.OnMouseDown:=@OnMouseDownHandler;
  Result.lbName:=lb;
  x:=x+lb.Width+4;

  // topic
  lb:=TStaticText.Create(ScrollBox);
  lb.Name:='lbt'+IntToStr(i);
  lb.Parent:=ScrollBox;
  lb.AutoSize:=False;
  lb.Left:=x;
  lb.Top:=y;
  lb.Height:=h;
  lb.Width:=300;
  //lb.BorderStyle:=TStaticBorderStyle.sbsSingle;
  lb.Font.Height:=h-2;
  //lb.WordWrap:=True;
  lb.Caption:=Item.Topic;
  //lb.ShowHint:=True;
  //lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);
  x:=x+lb.Width;
  Result.lbTopic:=lb;
  Result.Rect.Right:=x;
  Result.Rect.Bottom:=y+h;

end;

procedure TFrameChatRoomList.OnMouseEnterHandler(Sender: TObject);
begin
  if not (Sender is TStaticText) then Exit;
  (Sender as TStaticText).Font.Style:=[fsUnderline];
  (Sender as TStaticText).Font.Color:=clHighlight;
end;

procedure TFrameChatRoomList.OnMouseLeaveHandler(Sender: TObject);
begin
  if not (Sender is TStaticText) then Exit;
  (Sender as TStaticText).Font.Style:=[fsUnderline];
  (Sender as TStaticText).Font.Color:=clHotLight;
end;

procedure TFrameChatRoomList.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  // open room
  if (Sender is TStaticText) then
  begin
    for i:=0 to VisualItems.Count-1 do
    begin
      if (VisualItems.Items[i] as TVisualItem).lbName=Sender then
      begin
        Serv.OpenChatRoom((VisualItems.Items[i] as TVisualItem).Item.Name);
        Exit;
      end;
    end;
  end;
end;

function TFrameChatRoomList.SelectedItem(): TChatRoom;
begin
  Result:=nil;
end;

procedure TFrameChatRoomList.AfterConstruction();
begin
  inherited AfterConstruction;
  VisualItems:=TCollection.Create(TVisualItem);
end;

procedure TFrameChatRoomList.BeforeDestruction();
begin
  FreeAndNil(VisualItems);
  inherited BeforeDestruction;
end;

procedure TFrameChatRoomList.Update();
var
  i: integer;
begin
  // Clear visual items
  for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();
  VisualItems.Clear();

  for i:=0 to Serv.ChatRoomCount()-1 do
  begin
    AddVisualItem(Serv.GetChatRoom(i));
  end;

  inherited Update;
end;

end.

