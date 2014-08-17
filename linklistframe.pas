unit LinkListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  dnmp_unit;

type

  { TFrameLinkList }

  TFrameLinkList = class(TFrame)
    gbLinkInfo: TGroupBox;
    lvLinkList: TListView;
    lvLinkInfoList: TListView;
    pgcLinks: TPageControl;
    Splitter1: TSplitter;
    tsActiveLinks: TTabSheet;
    tsLinkInfoList: TTabSheet;
    procedure pgcLinksChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Mgr: TDnmpManager;
    procedure UpdateLinkInfoList();
    procedure UpdateLinkList();
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFrameLinkList }

procedure TFrameLinkList.pgcLinksChange(Sender: TObject);
begin
  if pgcLinks.ActivePage=tsLinkInfoList then UpdateLinkInfoList();
  if pgcLinks.ActivePage=tsActiveLinks then UpdateLinkList();
end;

procedure TFrameLinkList.UpdateLinkInfoList();
var
  lv: TListView;
  i: integer;
  Item: TLinkInfo;
  lvItem: TListItem;
begin
  if not Assigned(Mgr) then Exit;
  lv:=lvLinkInfoList;
  lv.BeginUpdate();

  lv.Clear();
  for i:=0 to Mgr.LinkInfoList.Count-1 do
  begin
    Item:=Mgr.LinkInfoList.Items[i];
    lvItem:=lv.Items.Add();
    lvItem.Data:=Item;
    // addr
    lvItem.Caption:=Item.AddrStr();
    // name
    lvItem.SubItems.Append(Item.Nick);
    // state
    lvItem.SubItems.Append(Item.StateStr());
  end;
  lv.EndUpdate();
end;

procedure TFrameLinkList.UpdateLinkList();
var
  lv: TListView;
  i: integer;
  Item: TDnmpLink;
  lvItem: TListItem;
begin
  if not Assigned(Mgr) then Exit;
  lv:=lvLinkInfoList;
  lv.BeginUpdate();

  lv.Clear();
  for i:=0 to Mgr.LinkList.Count-1 do
  begin
    Item:=Mgr.LinkList.Items[i];
    lvItem:=lv.Items.Add();
    lvItem.Data:=Item;
    // addr
    lvItem.Caption:=Item.LinkInfo.AddrStr();
    // name
    lvItem.SubItems.Append(Item.LinkInfo.Nick);
    // state
    lvItem.SubItems.Append(Item.LinkInfo.StateStr());
    // info
    lvItem.SubItems.Append(LinkTypeToStr(Item.LinkType));
  end;
  lv.EndUpdate();
end;

procedure TFrameLinkList.Update();
begin
  UpdateLinkInfoList();
  UpdateLinkList();
  inherited Update();
end;

end.

