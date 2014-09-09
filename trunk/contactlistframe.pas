unit ContactListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Core, dnmp_unit,
  dnmp_services, ExtCtrls, StdCtrls, ActnList, Menus, Graphics, Types;

type

  { TVisualItem }

  TVisualItem = class(TCollectionItem)
  private
    FSelected: boolean;
    procedure FSetSelected(Value: boolean);
  public
    Item: TDnmpContact;
    Rect: TRect;
    Image: TImage;
    lbName: TStaticText;
    Background: TShape;
    property Selected: boolean read FSelected write FSetSelected;
  end;

  { TFrameContactList }

  TFrameContactList = class(TFrame)
    actDeleteContact: TAction;
    actTest1: TAction;
    alContactList: TActionList;
    imgDefault: TImage;
    MenuItem1: TMenuItem;
    pmContactList: TPopupMenu;
    ScrollBox: TScrollBox;
    tcGroups: TTabControl;
    procedure actDeleteContactExecute(Sender: TObject);
    procedure actTest1Execute(Sender: TObject);
    procedure tcGroupsChange(Sender: TObject);
  private
    { private declarations }
    VisualItems: TCollection;
    procedure AddTestContacts();
    procedure OnClickHandler(Sender: TObject);
    procedure OnMouseEnterHandler(Sender: TObject);
    procedure OnMouseLeaveHandler(Sender: TObject);
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function AddVisualItem(Item: TDnmpContact): TVisualItem;
    function VisualItemAtPos(pos: TPoint): TVisualItem;
    function SelectedItem(): TDnmpContact;
  public
    { public declarations }
    Mgr: TDnmpManager;
    ContactList: TDnmpContactList;
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
    if FSelected then Background.Brush.Color:=clSkyBlue
    else Background.Brush.Color:=clDefault;
  end;
end;

{ TFrameContactList }

procedure TFrameContactList.tcGroupsChange(Sender: TObject);
begin
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
  Item:=VisualItemAtPos(Point(X,Y));
  if Assigned(Item) then Item.Selected:=not Item.Selected;
end;

procedure TFrameContactList.actDeleteContactExecute(Sender: TObject);
begin
  //
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

    ContactList.Add(Item);
  end;
end;

procedure TFrameContactList.OnClickHandler(Sender: TObject);
var
  Item: TVisualItem;
begin
  //Item:=VisualItemAtMousePos();
  //if Assigned(Item) then Item.Selected:=True;
end;

procedure TFrameContactList.OnMouseEnterHandler(Sender: TObject);
begin

end;

procedure TFrameContactList.OnMouseLeaveHandler(Sender: TObject);
begin

end;

function TFrameContactList.AddVisualItem(Item: TDnmpContact): TVisualItem;
var
  i, x, y, h: integer;
  //Pan: TPanel;
  Image: TImage;
  lb: TStaticText;
  ss: TStringStream;
begin
  if not Assigned(ContactList) then Exit;
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
  Result.Background.Shape:=stRectangle;
  Result.Background.Pen.Color:=clNone;
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
  lb:=TStaticText.Create(ScrollBox);
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
  //lb.OnClick:=@OnClickHandler;
  lb.OnMouseDown:=@OnMouseDownHandler;
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

function TFrameContactList.SelectedItem(): TDnmpContact;
begin
  //
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
begin
  if not Assigned(ContactList) then Exit;
  n:=tcGroups.TabIndex;

  // clear list
  for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();
  VisualItems.Clear();

  for i:=0 to ContactList.Count-1 do
  begin
    Item:=ContactList.Items[i];

    if n=0 then // all

    else if n=1 then // favorites
    begin
      Continue;
    end

    else if n=2 then // nodes
    begin
      if Mgr.NodeList.IndexOf(Item)=-1 then Continue;
    end

    else if n=3 then // points
    begin
      if Mgr.PointList.IndexOf(Item)=-1 then Continue;
    end

    else if n=4 then // guests
    begin
      if Mgr.UnapprovedList.IndexOf(Item)=-1 then Continue;
    end;

    AddVisualItem(Item);
  end;
end;

procedure TFrameContactList.Update();
begin
  UpdateList();
  inherited Update();
end;

end.

