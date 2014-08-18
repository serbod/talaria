unit PointListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, ActnList, Menus, dnmp_unit, Dialogs;

type

  { TFramePointList }

  TFramePointList = class(TFrame)
    actConnect: TAction;
    actGetInfo: TAction;
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
    procedure actGenerateGUIDExecute(Sender: TObject);
    procedure actPointAddExecute(Sender: TObject);
    procedure actPointDelExecute(Sender: TObject);
    procedure actPointListSaveExecute(Sender: TObject);
    procedure actPointListUpdateExecute(Sender: TObject);
    procedure actPointSaveExecute(Sender: TObject);
    procedure actPointUpdateExecute(Sender: TObject);
    procedure lvPointListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    procedure UpdatePointList();
    procedure UpdatePointInfo();
    procedure FormToPointInfo();
    function GetSelectedPointInfo(): TDnmpLinkInfo;
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

procedure TFramePointList.actPointUpdateExecute(Sender: TObject);
begin
  UpdatePointInfo();
end;

procedure TFramePointList.lvPointListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Selected then Exit;
  UpdatePointInfo();
end;

procedure TFramePointList.actPointListUpdateExecute(Sender: TObject);
begin
  UpdatePointList();
end;

procedure TFramePointList.actPointSaveExecute(Sender: TObject);
begin
  FormToPointInfo();
end;

procedure TFramePointList.actGenerateGUIDExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  Item:=GetSelectedPointInfo();
  if not Assigned(Item) then Exit;
  Item.GUID:=GenerateGUID();
  UpdatePointInfo();
end;

procedure TFramePointList.actPointAddExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  if not Assigned(PointList) then Exit;
  Item:=TDnmpLinkInfo.Create();
  PointList.Add(Item);
  UpdatePointList();
end;

procedure TFramePointList.actPointDelExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  if not Assigned(PointList) then Exit;
  Item:=GetSelectedPointInfo();
  if not Assigned(Item) then Exit;

  //Application.MessageBox('Delete selected point?', 'Attention', MB_);
  if MessageDlg('Attention', 'Delete selected point?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;

  PointList.Extract(Item);
  Update();
end;

procedure TFramePointList.actPointListSaveExecute(Sender: TObject);
begin
  PointList.SaveToFile();
end;

procedure TFramePointList.UpdatePointList();
var
  i, si, ic: Integer;
  lv: TListView;
  li: TDnmpLinkInfo;
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
  LinkInfo:=GetSelectedPointInfo();
  if not Assigned(LinkInfo) then Exit;

  sg:=StringGridInfo;
  sg.BeginUpdate();
  sg.Clean([gzNormal]);

  n:=1;
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

procedure TFramePointList.FormToPointInfo();
var
  LinkInfo: TDnmpLinkInfo;
  sg: TStringGrid;

function GetValue(Name: string): string;
var
  RowNum: integer;
begin
  Result:='';
  for RowNum:=1 to sg.RowCount-1 do
  begin
    if sg.Cells[0, RowNum]<>Name then Continue;
    Result:=sg.Cells[1, RowNum];
    Break;
  end;
end;

begin
  LinkInfo:=GetSelectedPointInfo();
  if not Assigned(LinkInfo) then Exit;
  sg:=StringGridInfo;

  LinkInfo.Addr:=StrToAddr(GetValue('Addr'));
  LinkInfo.GUID:=GetValue('GUID');
  LinkInfo.SeniorGUID:=GetValue('SeniorGUID');
  LinkInfo.Name:=GetValue('Name');
  LinkInfo.Owner:=GetValue('Owner');
  LinkInfo.Location:=GetValue('Location');
  LinkInfo.IpAddr:=GetValue('IpAddr');
  LinkInfo.PhoneNo:=GetValue('PhoneNo');
  LinkInfo.OtherInfo:=GetValue('OtherInfo');
  LinkInfo.Rating:=StrToIntDef(GetValue('Rating'), 0);
  LinkInfo.Key:=GetValue('Key');

end;

function TFramePointList.GetSelectedPointInfo(): TDnmpLinkInfo;
begin
  Result:=nil;
  if not Assigned(lvPointList.Selected) then Exit;
  if not Assigned(lvPointList.Selected.Data) then Exit;
  Result:=TDnmpLinkInfo(lvPointList.Selected.Data);
end;

procedure TFramePointList.Update();
begin
  UpdatePointList();
  UpdatePointInfo();
  inherited Update();
end;

end.

