unit LinkInfoFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ActnList,
  Menus, dnmp_unit;

type

  { TFrameLinkInfo }

  TFrameLinkInfo = class(TFrame)
    actSaveInfo: TAction;
    actUpdateInfo: TAction;
    alLinkInfo: TActionList;
    btnSaveInfo: TButton;
    edAddr: TEdit;
    edIPAddr: TEdit;
    edPhoneNo: TEdit;
    edOwner: TEdit;
    edNick: TEdit;
    edGUID: TEdit;
    edLocation: TEdit;
    edOtherInfo: TEdit;
    edSGUID: TEdit;
    edStatusMessage: TEdit;
    gbContact: TGroupBox;
    gbLinkInfo: TGroupBox;
    imgPicture: TImage;
    lbAddr: TLabel;
    lbAddr1: TLabel;
    lbGUID: TLabel;
    lbIPAddr: TLabel;
    lbPhoneNo: TLabel;
    lbOwner: TLabel;
    lbLocation: TLabel;
    lbOtherInfo: TLabel;
    lbSGUID: TLabel;
    lbStatusMessage: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pmLinkInfo: TPopupMenu;
    procedure actSaveInfoExecute(Sender: TObject);
    procedure actUpdateInfoExecute(Sender: TObject);
  private
    { private declarations }
    FLinkInfo: TDnmpLinkInfo;
    procedure FSetLinkInfo(Value: TDnmpLinkInfo);
  public
    { public declarations }
    property LinkInfo: TDnmpLinkInfo read FLinkInfo write FSetLinkInfo;
    procedure LinkInfoToForm();
    procedure LinkInfoFromForm();
  end;

implementation

{$R *.lfm}

{ TFrameLinkInfo }

procedure TFrameLinkInfo.actUpdateInfoExecute(Sender: TObject);
begin
  LinkInfoToForm();
end;

procedure TFrameLinkInfo.actSaveInfoExecute(Sender: TObject);
begin
  LinkInfoFromForm();
end;

procedure TFrameLinkInfo.FSetLinkInfo(Value: TDnmpLinkInfo);
begin
  FLinkInfo:=Value;
  LinkInfoToForm();
end;

procedure TFrameLinkInfo.LinkInfoToForm();
var
  ss: TStringStream;
begin
  if not Assigned(LinkInfo) then Exit;
  edAddr.Text:=LinkInfo.AddrStr();
  edNick.Text:=LinkInfo.Nick;
  edGUID.Text:=LinkInfo.GUID;
  edSGUID.Text:=LinkInfo.SeniorGUID;
  edStatusMessage.Text:=LinkInfo.StatusMessage;

  edOwner.Text:=LinkInfo.Owner;
  edLocation.Text:=LinkInfo.Location;
  edIPAddr.Text:=LinkInfo.IpAddr;
  edPhoneNo.Text:=LinkInfo.PhoneNo;
  edOtherInfo.Text:=LinkInfo.OtherInfo;

  if Length(LinkInfo.Picture)>4 then
  begin
    ss:=TStringStream.Create(LinkInfo.Picture);
    try
      imgPicture.Picture.LoadFromStream(ss);
    finally
      ss.Free();
    end;
  end
  else
  begin
    //imgPicture.Picture.Assign(imgDefault.Picture);
    imgPicture.Picture.Clear();
  end;
end;

procedure TFrameLinkInfo.LinkInfoFromForm();
begin
  LinkInfo.Addr:=StrToAddr(Trim(edAddr.Text));
  LinkInfo.Nick:=Trim(edNick.Text);
  LinkInfo.GUID:=Trim(edGUID.Text);
  LinkInfo.SeniorGUID:=Trim(edSGUID.Text);
  LinkInfo.StatusMessage:=Trim(edStatusMessage.Text);

  LinkInfo.Owner:=Trim(edOwner.Text);
  LinkInfo.Location:=Trim(edLocation.Text);
  LinkInfo.IpAddr:=Trim(edIPAddr.Text);
  LinkInfo.PhoneNo:=Trim(edPhoneNo.Text);
  LinkInfo.OtherInfo:=Trim(edOtherInfo.Text);
end;

end.

