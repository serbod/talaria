unit LinkInfoListFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, ActnList, Menus, dnmp_unit, Dialogs;

type

  { TFrameLinkInfoList }

  TFrameLinkInfoList = class(TFrame)
    actConnect: TAction;
    actInfoRequest: TAction;
    actGenerateGUID: TAction;
    actInfoEdit: TAction;
    actInfoSave: TAction;
    actInfoUpdate: TAction;
    actInfoDel: TAction;
    actInfoAdd: TAction;
    actInfoListUpdate: TAction;
    actInfoListLoad: TAction;
    actInfoListSave: TAction;
    alInfoList: TActionList;
    gbSelectedInfo: TGroupBox;
    lvInfoList: TListView;
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
    panInfoListTop: TPanel;
    pmInfoList: TPopupMenu;
    pmSelectedInfo: TPopupMenu;
    Splitter1: TSplitter;
    StringGridInfo: TStringGrid;
    procedure actConnectExecute(Sender: TObject);
    procedure actGenerateGUIDExecute(Sender: TObject);
    procedure actInfoAddExecute(Sender: TObject);
    procedure actInfoDelExecute(Sender: TObject);
    procedure actInfoEditExecute(Sender: TObject);
    procedure actInfoListSaveExecute(Sender: TObject);
    procedure actInfoListUpdateExecute(Sender: TObject);
    procedure actInfoSaveExecute(Sender: TObject);
    procedure actInfoUpdateExecute(Sender: TObject);
    procedure lvInfoListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    procedure UpdateInfoList();
    procedure UpdateSelectedInfo();
    procedure FormToSelectedInfo();
    function GetSelectedInfo(): TDnmpLinkInfo;
  public
    { public declarations }
    DnmpMgr: TDnmpManager;
    InfoList: TLinkInfoList;
    { Update view }
    procedure Update(); override;
  end;

implementation

uses Core;

{$R *.lfm}

{ TFrameLinkInfoList }

procedure TFrameLinkInfoList.actInfoUpdateExecute(Sender: TObject);
begin
  UpdateSelectedInfo();
end;

procedure TFrameLinkInfoList.lvInfoListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Selected then Exit;
  UpdateSelectedInfo();
end;

procedure TFrameLinkInfoList.actInfoListUpdateExecute(Sender: TObject);
begin
  UpdateInfoList();
end;

procedure TFrameLinkInfoList.actInfoSaveExecute(Sender: TObject);
begin
  FormToSelectedInfo();
end;

procedure TFrameLinkInfoList.actGenerateGUIDExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  Item:=GetSelectedInfo();
  if not Assigned(Item) then Exit;
  Item.GUID:=GenerateGUID();
  UpdateSelectedInfo();
end;

procedure TFrameLinkInfoList.actConnectExecute(Sender: TObject);
begin
  if not Assigned(DnmpMgr) then Exit;
  DnmpMgr.StartNodeConnection(GetSelectedInfo());
end;

procedure TFrameLinkInfoList.actInfoAddExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  if not Assigned(InfoList) then Exit;
  Item:=TDnmpLinkInfo.Create();
  InfoList.Add(Item);
  UpdateInfoList();
end;

procedure TFrameLinkInfoList.actInfoDelExecute(Sender: TObject);
var
  Item: TDnmpLinkInfo;
begin
  if not Assigned(InfoList) then Exit;
  Item:=GetSelectedInfo();
  if not Assigned(Item) then Exit;

  //Application.MessageBox('Delete selected point?', 'Attention', MB_);
  if MessageDlg('Attention', 'Delete selected point?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then Exit;

  InfoList.Extract(Item);
  Update();
end;

procedure TFrameLinkInfoList.actInfoEditExecute(Sender: TObject);
begin
  Core.ShowLinkInfo(GetSelectedInfo());
end;

procedure TFrameLinkInfoList.actInfoListSaveExecute(Sender: TObject);
begin
  DnmpMgr.WriteList(InfoList);
end;

procedure TFrameLinkInfoList.UpdateInfoList();
var
  i, si, ic: Integer;
  lv: TListView;
  li: TDnmpLinkInfo;
  lil: TLinkInfoList;
  lvi: TListItem;
begin
  if not Assigned(InfoList) then Exit;
  lv:=lvInfoList;
  lil:=InfoList;
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

procedure TFrameLinkInfoList.UpdateSelectedInfo();
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

procedure TFrameLinkInfoList.FormToSelectedInfo();
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
  LinkInfo:=GetSelectedInfo();
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

function TFrameLinkInfoList.GetSelectedInfo(): TDnmpLinkInfo;
begin
  Result:=nil;
  if not Assigned(lvInfoList.Selected) then Exit;
  if not Assigned(lvInfoList.Selected.Data) then Exit;
  Result:=TDnmpLinkInfo(lvInfoList.Selected.Data);
end;

procedure TFrameLinkInfoList.Update();
begin
  UpdateInfoList();
  UpdateSelectedInfo();
  inherited Update();
end;

end.

