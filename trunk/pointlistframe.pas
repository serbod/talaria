unit PointListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, ActnList, Menus, dnmp_unit;

type

  { TFramePointList }

  TFramePointList = class(TFrame)
    actConnect: TAction;
    actGetInfo: TAction;
    actApprove: TAction;
    actGenerateGUID: TAction;
    actPointSave: TAction;
    actPointUpdate: TAction;
    actPointDel: TAction;
    actPointAdd: TAction;
    actPointListUpdate: TAction;
    actPointListLoad: TAction;
    actPointListSave: TAction;
    alPointList: TActionList;
    gbPointInfo: TGroupBox;
    lvPointList: TListView;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    panPointlistTop: TPanel;
    pmPointList: TPopupMenu;
    pmPointInfo: TPopupMenu;
    Splitter1: TSplitter;
    StringGridInfo: TStringGrid;
  private
    { private declarations }
    procedure UpdatePointList();
    procedure UpdatePointInfo();
    function GetSelectedPointInfo(): TLinkInfo;
  public
    { public declarations }
    DnmpMgr: TDnmpManager;
    PointList: TLinkInfoList;
    { Update view }
    procedure Update(); override;
  end;

implementation

{$R *.lfm}

{ TFramePointList }

procedure TFramePointList.UpdatePointList();
var
  i, si, ic: Integer;
  lv: TListView;
  li: TLinkInfo;
  lil: TLinkInfoList;
  lvi: TListItem;
begin
  if not Assigned(PointList) then Exit;
  lv:=lvPointList;
  lil:=PointList;
  lv.Items.BeginUpdate();
  si:=lv.ItemIndex;
  ic:=lil.Count;
  while lv.Items.Count <> ic do
  begin
    if lv.Items.Count > ic then lv.Items.Delete(lv.Items.Count-1);
    if lv.Items.Count < ic then lv.Items.Add();
  end;
  for i:=0 to ic-1 do
  begin
    li:=lil[i];
    lvi:=lv.Items[i];
    lvi.Data:=li;
    lvi.Caption:=AddrToStr(li.Addr);
    while lvi.SubItems.Count < 3 do lvi.SubItems.Add('');
    lvi.SubItems[0]:=li.Name;
    //lvi.SubItems[1]:=LinkTypeToStr(li.LinkType);
    if li.Online then lvi.SubItems[1]:='Online' else lvi.SubItems[2]:='Offline';
  end;
  if si < lv.Items.Count then lv.ItemIndex:=si;
  lv.Items.EndUpdate();
end;

procedure TFramePointList.UpdatePointInfo();
var
  n: integer;
  LinkInfo: TLinkInfo;
  sg: TStringGrid;

  procedure SetRow(RowNum: integer; Name, Value: string);
  begin
    sg.Cells[1, RowNum]:=Name;
    sg.Cells[2, RowNum]:=Value;
    n:=n+1;
  end;

begin
  LinkInfo:=GetSelectedPointInfo();
  if not Assigned(LinkInfo) then Exit;

  sg:=StringGridInfo;
  sg.BeginUpdate();
  sg.Clean([gzNormal]);

  n:=2;
  SetRow(n, 'Addr', AddrToStr(LinkInfo.Addr));
  SetRow(n, 'GUID', LinkInfo.GUID);
  SetRow(n, 'SeniorGUID', LinkInfo.SeniorGUID);
  SetRow(n, 'Name', LinkInfo.Name);
  SetRow(n, 'Owner', LinkInfo.Owner);
  SetRow(n, 'Location', LinkInfo.Location);
  SetRow(n, 'IpAddr', LinkInfo.IpAddr);
  SetRow(n, 'PhoneNo', LinkInfo.PhoneNo);
  SetRow(n, 'OtherInfo', LinkInfo.OtherInfo);
  SetRow(n, 'Rating', IntToStr(LinkInfo.Rating));
  SetRow(n, 'Key', LinkInfo.Key);

  sg.EndUpdate();

end;

function TFramePointList.GetSelectedPointInfo(): TLinkInfo;
begin
  Result:=nil;
  if not Assigned(lvPointList.Selected) then Exit;
  if not Assigned(lvPointList.Selected.Data) then Exit;
  Result:=TLinkInfo(lvPointList.Selected.Data);
end;

procedure TFramePointList.Update();
begin
  inherited Update();
end;

end.

