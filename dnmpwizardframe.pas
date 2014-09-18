unit DnmpWizardFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Core;

type

  { TFormDnmpWizard }

  TFormDnmpWizard = class(TForm)
    btnNext: TButton;
    btnBack: TButton;
    edUplinkKey: TEdit;
    edUplunkHost: TEdit;
    edName: TEdit;
    edFullName: TEdit;
    edLocation: TEdit;
    gbPicture: TGroupBox;
    gbUserInfo: TGroupBox;
    gbUplink: TGroupBox;
    imgPicture: TImage;
    lbFinishInfo: TLabel;
    lbPageInfo: TLabel;
    lbUplinkKey: TLabel;
    lbUplinkHost: TLabel;
    lbUplinkInfo: TLabel;
    lbFullName: TLabel;
    lbName: TLabel;
    lbLocation: TLabel;
    lbLanguageInfo: TLabel;
    ListBoxLanguages: TListBox;
    nbPages: TNotebook;
    PageFinish: TPage;
    PageLanguage: TPage;
    PageUplink: TPage;
    PageUser: TPage;
    panBottom: TPanel;
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure PageFinishBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PageLanguageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PageUplinkBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
  private
    { private declarations }
  public
    { public declarations }
    Serv: TServiceDnmp;
    procedure Start();
    procedure Finish();
  end;

var
  FormDnmpWizard: TFormDnmpWizard;

implementation

uses dnmp_unit;

{$R *.lfm}

const
  ciPageLanguage = 2;
  ciPageUser = 1;
  ciPageUplink = 0;
  ciPageFinish = 3;

{ TFormDnmpWizard }

procedure TFormDnmpWizard.PageLanguageBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

procedure TFormDnmpWizard.btnNextClick(Sender: TObject);
begin
  if nbPages.PageIndex=ciPageLanguage then nbPages.PageIndex:=ciPageUser
  else if nbPages.PageIndex=ciPageUser then nbPages.PageIndex:=ciPageUplink
  else if nbPages.PageIndex=ciPageUplink then nbPages.PageIndex:=ciPageFinish
  else if nbPages.PageIndex=ciPageFinish then Finish();
end;

procedure TFormDnmpWizard.PageFinishBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

procedure TFormDnmpWizard.btnBackClick(Sender: TObject);
begin
  if nbPages.PageIndex=ciPageLanguage then nbPages.PageIndex:=ciPageLanguage
  else if nbPages.PageIndex=ciPageUser then nbPages.PageIndex:=ciPageLanguage
  else if nbPages.PageIndex=ciPageUplink then nbPages.PageIndex:=ciPageUser
  else if nbPages.PageIndex=ciPageFinish then nbPages.PageIndex:=ciPageUplink;
end;

procedure TFormDnmpWizard.PageUplinkBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

procedure TFormDnmpWizard.Start();
var
  Item: TDnmpContact;
  i: integer;
begin
  nbPages.PageIndex:=ciPageLanguage;
  if not Assigned(Serv) then Exit;
  if not Assigned(Serv.Mgr) then Exit;
  // set user
  edName.Text:=Serv.Mgr.MyInfo.Name;
  edFullName.Text:=Serv.Mgr.MyInfo.Owner;
  edLocation.Text:=Serv.Mgr.MyInfo.Location;

  // set uplink
  Item:=nil;
  for i:=0 to Serv.Mgr.NodeList.Count-1 do
  begin
    if Serv.Mgr.NodeList.Items[i]<>Serv.Mgr.MyInfo then
    begin
      Item:=Serv.Mgr.NodeList.Items[i];
      Break;
    end;
  end;
  if Assigned(Item) then
  begin
    edUplunkHost.Text:=Item.IpAddr;
    edUplinkKey.Text:=Item.Key;
  end;

end;

procedure TFormDnmpWizard.Finish();
var
  Item: TDnmpContact;
  i: integer;
begin
  if not Assigned(Serv) then Exit;
  if not Assigned(Serv.Mgr) then Exit;
  // set user
  Serv.Mgr.MyInfo.Name:=edName.Text;
  Serv.Mgr.MyInfo.Owner:=edFullName.Text;
  Serv.Mgr.MyInfo.Location:=edLocation.Text;

  // set uplink
  Item:=TDnmpContact.Create();
  Item.Name:='Uplink';
  Item.IpAddr:=Trim(edUplunkHost.Text);
  Item.Key:=Trim(edUplinkKey.Text);
  for i:=0 to Serv.Mgr.NodeList.Count-1 do
  begin
    if Serv.Mgr.NodeList.Items[i].IpAddr=Item.IpAddr then
    begin
      FreeAndNil(Item);
      Break;
    end;
  end;
  if Assigned(Item) then Serv.Mgr.NodeList.AddItem(Item);
end;

end.

