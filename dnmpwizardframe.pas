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
    btnAvatarFromFile: TButton;
    btnAvatarFromClipboard: TButton;
    btnListenPortDefault: TButton;
    edListenPort: TEdit;
    edGUID: TEdit;
    edUplinkKey: TEdit;
    edUplinkPassword: TEdit;
    edUplunkHost: TEdit;
    edName: TEdit;
    edFullName: TEdit;
    edLocation: TEdit;
    gbPicture: TGroupBox;
    gbUserInfo: TGroupBox;
    gbUplink: TGroupBox;
    gbListen: TGroupBox;
    imgPicture: TImage;
    lbListenPort: TLabel;
    lbFinishInfo: TLabel;
    lbGUID: TLabel;
    lbPageInfo: TLabel;
    lbListenerkInfo: TLabel;
    lbUplinkKey: TLabel;
    lbUplinkHost: TLabel;
    lbUplinkInfo: TLabel;
    lbFullName: TLabel;
    lbName: TLabel;
    lbLocation: TLabel;
    lbLanguageInfo: TLabel;
    lbUplinkPassword: TLabel;
    ListBoxLanguages: TListBox;
    nbPages: TNotebook;
    PageFinish: TPage;
    PageLanguage: TPage;
    PageUplink: TPage;
    PageUser: TPage;
    panBottom: TPanel;
    procedure btnAvatarFromClipboardClick(Sender: TObject);
    procedure btnAvatarFromFileClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnListenPortDefaultClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
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

uses dnmp_unit, GraphicTools;

{$R *.lfm}

const
  ciPageLanguage = 2;
  ciPageUser = 1;
  ciPageUplink = 0;
  ciPageFinish = 3;

{ TFormDnmpWizard }

procedure TFormDnmpWizard.btnNextClick(Sender: TObject);
begin
  if nbPages.PageIndex=ciPageLanguage then nbPages.PageIndex:=ciPageUser
  else if nbPages.PageIndex=ciPageUser then nbPages.PageIndex:=ciPageUplink
  else if nbPages.PageIndex=ciPageUplink then nbPages.PageIndex:=ciPageFinish
  else if nbPages.PageIndex=ciPageFinish then Finish();
end;

procedure TFormDnmpWizard.btnBackClick(Sender: TObject);
begin
  if nbPages.PageIndex=ciPageLanguage then nbPages.PageIndex:=ciPageLanguage
  else if nbPages.PageIndex=ciPageUser then nbPages.PageIndex:=ciPageLanguage
  else if nbPages.PageIndex=ciPageUplink then nbPages.PageIndex:=ciPageUser
  else if nbPages.PageIndex=ciPageFinish then nbPages.PageIndex:=ciPageUplink;
end;

procedure TFormDnmpWizard.btnListenPortDefaultClick(Sender: TObject);
begin
  edListenPort.Text:='4044';
end;

procedure TFormDnmpWizard.btnAvatarFromFileClick(Sender: TObject);
begin
  //
end;

procedure TFormDnmpWizard.btnAvatarFromClipboardClick(Sender: TObject);
var
  s: AnsiString;
  sInfo: string;
begin
  s:=GetFilePreview('', Point(imgPicture.Width, imgPicture.Height), sInfo);
  if Length(s)>4 then Core.PictureFromString(imgPicture.Picture, s);
end;

procedure TFormDnmpWizard.Start();
var
  Item: TDnmpContact;
  i: integer;
begin
  // set default language
  Self.Caption:='Мастер настройки '+Serv.AppName;
  ListBoxLanguages.Clear();
  ListBoxLanguages.AddItem('Русский', nil);
  ListBoxLanguages.AddItem('English', nil);

  nbPages.PageIndex:=ciPageLanguage;
  if not Assigned(Serv) then Exit;
  if not Assigned(Serv.Mgr) then Exit;

  // set user
  edName.Text:=Serv.Mgr.MyInfo.Name;
  edFullName.Text:=Serv.Mgr.MyInfo.Owner;
  edLocation.Text:=Serv.Mgr.MyInfo.Location;
  edGUID.Text:=Trim(Serv.Mgr.MyInfo.GUID);
  if Trim(edGUID.Text)='' then edGUID.Text:=GenerateGUID();
  // set listen port
  i:=Pos(':', Serv.Mgr.MyInfo.IpAddr);
  if i>0 then edListenPort.Text:=Copy(Serv.Mgr.MyInfo.IpAddr, i+1, 9999);

  // set avatar image
  if Length(Serv.Mgr.MyInfo.Picture)>4 then
  begin
    Core.PictureFromString(imgPicture.Picture, Serv.Mgr.MyInfo.Picture);
  end;

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
    edUplinkPassword.Text:=Item.Info['uplink_password'];
  end;

end;

procedure TFormDnmpWizard.Finish();
var
  Item: TDnmpContact;
  i: integer;
  s: string;
begin
  if not Assigned(Serv) then Exit;
  if not Assigned(Serv.Mgr) then Exit;
  // set user
  Serv.Mgr.MyInfo.Name:=edName.Text;
  Serv.Mgr.MyInfo.Owner:=edFullName.Text;
  Serv.Mgr.MyInfo.Location:=edLocation.Text;
  Serv.Mgr.MyInfo.GUID:=Trim(edGUID.Text);
  // set listen port
  s:=IntToStr(StrToIntDef(Trim(edListenPort.Text), 0));
  if s<>'0' then
  begin
    Serv.Mgr.MyInfo.IsNode:=True;
    i:=Pos(':', Serv.Mgr.MyInfo.IpAddr);
    if i>0 then
      Serv.Mgr.MyInfo.IpAddr:=Copy(Serv.Mgr.MyInfo.IpAddr, 1, i-1)+':'+s
    else
      Serv.Mgr.MyInfo.IpAddr:='localhost:'+s;
  end
  else
    Serv.Mgr.MyInfo.IsNode:=False;
  // set avatar image
  Core.PictureToString(imgPicture.Picture, Serv.Mgr.MyInfo.Picture);

  // set uplink
  s:=Trim(edUplunkHost.Text);
  Item:=nil;
  for i:=0 to Serv.Mgr.NodeList.Count-1 do
  begin
    Item:=Serv.Mgr.NodeList.Items[i];
    if Item.IpAddr=s then Break;
    Item:=nil;
  end;
  if not Assigned(Item) then
  begin
    // create new uplink
    Item:=TDnmpContact.Create();
    Serv.Mgr.NodeList.AddItem(Item);
    Item.Name:='Uplink';
    Item.GUID:=GenerateGUID();
  end;
  Item.IpAddr:=Trim(edUplunkHost.Text);
  Item.Key:=Trim(edUplinkKey.Text);
  Item.Info['uplink_password']:=Trim(edUplinkPassword.Text);
  // close
  Close();
  //if Assigned(Parent) and (Parent is TForm) then (Parent as TForm).Close();
end;

end.

