unit LinkListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  ActnList, Grids, dnmp_unit;

type

  { TFrameLinkList }

  TFrameLinkList = class(TFrame)
    actApproveLink: TAction;
    alLinkList: TActionList;
    gbLinkInfo: TGroupBox;
    lvLinkList: TListView;
    lvLinkInfoList: TListView;
    pgcLinks: TPageControl;
    Splitter1: TSplitter;
    StringGridInfo: TStringGrid;
    tsActiveLinks: TTabSheet;
    tsLinkInfoList: TTabSheet;
    procedure actApproveLinkExecute(Sender: TObject);
    procedure lvLinkInfoListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvLinkListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure pgcLinksChange(Sender: TObject);
  private
    { private declarations }
    function GetSelectedInfo(): TDnmpLinkInfo;
  public
    { public declarations }
    Mgr: TDnmpManager;
    procedure UpdateLinkInfoList();
    procedure UpdateLinkList();
    procedure UpdateSelectedInfo();
    procedure Update(); override;
  end;

implementation

uses Core;

{$R *.lfm}

{ TFrameLinkList }

procedure TFrameLinkList.pgcLinksChange(Sender: TObject);
begin
  if pgcLinks.ActivePage=tsLinkInfoList then UpdateLinkInfoList();
  if pgcLinks.ActivePage=tsActiveLinks then UpdateLinkList();
end;

procedure TFrameLinkList.lvLinkInfoListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Selected then Exit;
  UpdateSelectedInfo();
end;

procedure TFrameLinkList.actApproveLinkExecute(Sender: TObject);
begin
  if not Assigned(Mgr) then Exit;
  Mgr.Approve(GetSelectedInfo());
end;

procedure TFrameLinkList.lvLinkListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then Exit;
  UpdateSelectedInfo();
end;

function TFrameLinkList.GetSelectedInfo(): TDnmpLinkInfo;
begin
  Result:=nil;
  if pgcLinks.ActivePage=tsLinkInfoList then
  begin
    if not Assigned(lvLinkInfoList.Selected) then Exit;
    if not Assigned(lvLinkInfoList.Selected.Data) then Exit;
    Result:=TDnmpLinkInfo(lvLinkInfoList.Selected.Data);
  end;

  if pgcLinks.ActivePage=tsActiveLinks then
  begin
    if not Assigned(lvLinkList.Selected) then Exit;
    if not Assigned(lvLinkList.Selected.Data) then Exit;
    Result:=TDnmpLink(lvLinkList.Selected.Data).LinkInfo;
  end;
end;

procedure TFrameLinkList.UpdateLinkInfoList();
var
  lv: TListView;
  i: integer;
  Item: TDnmpLinkInfo;
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
    lvItem.SubItems.Append(Item.Name);
    // state
    lvItem.SubItems.Append(BoolToStr(Item.Online, 'Online', 'Offline'));
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
  lv:=lvLinkList;
  lv.BeginUpdate();

  lv.Clear();
  for i:=0 to Mgr.LinkList.Count-1 do
  begin
    Item:=Mgr.LinkList.Items[i];
    lvItem:=lv.Items.Add();
    lvItem.Data:=Item;
    if Item.Active then lvItem.StateIndex:=ciIconUserBlue else lvItem.StateIndex:=ciIconUserRed;
    // addr
    lvItem.Caption:=Item.LinkInfo.AddrStr();
    // name
    lvItem.SubItems.Append(Item.LinkInfo.Name);
    // state
    lvItem.SubItems.Append(BoolToStr(Item.LinkInfo.Online, 'Online', 'Offline'));
    // info
    lvItem.SubItems.Append(LinkTypeToStr(Item.LinkType));
  end;
  lv.EndUpdate();
end;

procedure TFrameLinkList.UpdateSelectedInfo();
var
  n: integer;
  LinkInfo: TDnmpLinkInfo;
  sg: TStringGrid;

  procedure SetRow(RowNum: integer; Name, Value: string);
  begin
    if sg.RowCount<=RowNum then sg.RowCount:=RowNum+1;
    sg.Cells[0, RowNum]:=Name;
    sg.Cells[1, RowNum]:=Value;
    n:=n+1;
  end;

begin
  LinkInfo:=GetSelectedInfo();
  if not Assigned(LinkInfo) then Exit;

  sg:=StringGridInfo;
  sg.BeginUpdate();
  sg.Clean([gzNormal]);

  n:=1;
  SetRow(n, 'Addr', AddrToStr(LinkInfo.Addr));
  SetRow(n, 'Name', LinkInfo.Name);
  SetRow(n, 'GUID', LinkInfo.GUID);
  SetRow(n, 'SeniorGUID', LinkInfo.SeniorGUID);
  SetRow(n, 'Owner', LinkInfo.Owner);
  SetRow(n, 'Location', LinkInfo.Location);
  SetRow(n, 'IpAddr', LinkInfo.IpAddr);
  SetRow(n, 'PhoneNo', LinkInfo.PhoneNo);
  SetRow(n, 'OtherInfo', LinkInfo.OtherInfo);
  SetRow(n, 'Rating', IntToStr(LinkInfo.Rating));
  SetRow(n, 'Key', LinkInfo.Key);

  sg.EndUpdate();

end;

procedure TFrameLinkList.Update();
begin
  UpdateLinkInfoList();
  UpdateLinkList();
  inherited Update();
end;

end.

