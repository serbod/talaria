unit ContactListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, Core, dnmp_unit,
  dnmp_services, ExtCtrls, StdCtrls, ActnList, Menus;

type

  { TFrameContactList }

  TFrameContactList = class(TFrame)
    actTest1: TAction;
    alContactList: TActionList;
    imgDefault: TImage;
    MenuItem1: TMenuItem;
    pmContactList: TPopupMenu;
    ScrollBox: TScrollBox;
    tcGroups: TTabControl;
    procedure actTest1Execute(Sender: TObject);
    procedure tcGroupsChange(Sender: TObject);
  private
    { private declarations }
    procedure AddTestContacts();
  public
    { public declarations }
    //SvcMgr: TDnmpServiceManager;
    ContactList: TDnmpContactList;
    procedure UpdateList();
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

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
    Item.Nick:='Point '+AddrToStr(Item.Addr);

    ContactList.Add(Item);
  end;
end;

procedure TFrameContactList.UpdateList();
var
  i, x, y, h: integer;
  Item: TDnmpContact;
  //Pan: TPanel;
  Image: TImage;
  lb: TLabel;
  ss: TStringStream;
begin
  if not Assigned(ContactList) then Exit;

  // clear list
  for i:=ScrollBox.ControlCount-1 downto 0 do ScrollBox.Controls[i].Free();

  y:=0;
  h:=24;
  for i:=0 to ContactList.Count-1 do
  begin
    Item:=ContactList.Items[i];
    // add item
    x:=0;
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
      Image.Picture.Assign(imgDefault.Picture);
    end;
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
    lb.Caption:=Item.Nick;
    lb.ShowHint:=True;
    lb.Hint:=Item.GUID+LineEnding+AddrToStr(Item.Addr);

    // next line
    y:=y+h+2;
  end;
end;

procedure TFrameContactList.Update();
begin
  UpdateList();
  inherited Update();
end;

end.

