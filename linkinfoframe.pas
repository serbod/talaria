unit LinkInfoFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ActnList,
  Menus, dnmp_unit, Graphics, Clipbrd, LCLIntf, LCLType;

type

  { TFrameLinkInfo }

  TFrameLinkInfo = class(TFrame)
    actImageFromClipboard: TAction;
    actSaveInfo: TAction;
    actUpdateInfo: TAction;
    alLinkInfo: TActionList;
    btnSaveInfo: TButton;
    edAddr: TEdit;
    edIPAddr: TEdit;
    edPhoneNo: TEdit;
    edOwner: TEdit;
    edName: TEdit;
    edGUID: TEdit;
    edLocation: TEdit;
    edOtherInfo: TEdit;
    edSGUID: TEdit;
    edStatusMessage: TEdit;
    edKey: TEdit;
    gbContact: TGroupBox;
    gbLinkInfo: TGroupBox;
    imgPicture: TImage;
    lbAddr: TLabel;
    lbName: TLabel;
    lbGUID: TLabel;
    lbIPAddr: TLabel;
    lbPhoneNo: TLabel;
    lbOwner: TLabel;
    lbLocation: TLabel;
    lbOtherInfo: TLabel;
    lbSGUID: TLabel;
    lbStatusMessage: TLabel;
    lbKey: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    pmLinkInfo: TPopupMenu;
    procedure actImageFromClipboardExecute(Sender: TObject);
    procedure actSaveInfoExecute(Sender: TObject);
    procedure actUpdateInfoExecute(Sender: TObject);
  private
    { private declarations }
    FLinkInfo: TDnmpContact;
    procedure FSetLinkInfo(Value: TDnmpContact);
    procedure ShrinkPhoto(img: TImage);
  public
    { public declarations }
    property LinkInfo: TDnmpContact read FLinkInfo write FSetLinkInfo;
    procedure LinkInfoToForm();
    procedure LinkInfoFromForm();
    procedure Update(); override;
  end;

implementation

uses Core;

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

procedure TFrameLinkInfo.actImageFromClipboardExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfDelphiBitmap)) then
    imgPicture.Picture.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
    imgPicture.Picture.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
  ShrinkPhoto(imgPicture);
end;

procedure TFrameLinkInfo.FSetLinkInfo(Value: TDnmpContact);
begin
  FLinkInfo:=Value;
  LinkInfoToForm();
end;

procedure TFrameLinkInfo.ShrinkPhoto(img: TImage);
var
  bmp: TBitmap;
  r: TRect;
  k, kx, ky: Real;
begin
  if (img.Picture.Height > img.Height*2)
  or (img.Picture.Width > img.Width*2) then
  begin
    bmp:=TBitmap.Create();
    { Variant 1 }
    //bmp.Canvas.Assign(img.Canvas);

    { Variant 2 }
    kx:=img.Picture.Height / (img.Width*2);
    ky:=img.Picture.Width / (img.Height*2);
    k:=kx;
    if ky>k then k:=ky;
    if k<0 then Exit;

    r.Left:=0;
    r.Top:=0;
    r.BottomRight.x:=Round(img.Picture.Width / k);
    r.BottomRight.y:=Round(img.Picture.Height / k);
    bmp.Width:=r.BottomRight.x;
    bmp.Height:=r.BottomRight.y;
    bmp.Canvas.StretchDraw(r, img.Picture.Bitmap);

    img.Picture.Bitmap.Assign(bmp);
    bmp.Free();
  end;
end;

procedure TFrameLinkInfo.LinkInfoToForm();
begin
  if not Assigned(LinkInfo) then Exit;
  //imgPicture.Picture.Assign(imgDefault.Picture);
  imgPicture.Picture.Clear();

  edAddr.Text:=AddrToStr(LinkInfo.Addr);
  edName.Text:=LinkInfo.Name;
  edGUID.Text:=LinkInfo.GUID;
  edSGUID.Text:=LinkInfo.SeniorGUID;
  edStatusMessage.Text:=LinkInfo.StatusMessage;
  Core.PictureFromString(imgPicture.Picture, LinkInfo.Picture);

  edOwner.Text:=LinkInfo.Owner;
  edLocation.Text:=LinkInfo.Location;
  edIPAddr.Text:=LinkInfo.IpAddr;
  edPhoneNo.Text:=LinkInfo.PhoneNo;
  edOtherInfo.Text:=LinkInfo.OtherInfo;
  edKey.Text:=LinkInfo.Key;

end;

procedure TFrameLinkInfo.LinkInfoFromForm();
begin
  LinkInfo.Addr:=StrToAddr(Trim(edAddr.Text));
  LinkInfo.Name:=Trim(edName.Text);
  LinkInfo.GUID:=Trim(edGUID.Text);
  LinkInfo.SeniorGUID:=Trim(edSGUID.Text);
  LinkInfo.StatusMessage:=Trim(edStatusMessage.Text);

  // picture
  Core.PictureToString(imgPicture.Picture, LinkInfo.Picture);

  LinkInfo.Owner:=Trim(edOwner.Text);
  LinkInfo.Location:=Trim(edLocation.Text);
  LinkInfo.IpAddr:=Trim(edIPAddr.Text);
  LinkInfo.PhoneNo:=Trim(edPhoneNo.Text);
  LinkInfo.OtherInfo:=Trim(edOtherInfo.Text);
  LinkInfo.Key:=Trim(edKey.Text);
end;

procedure TFrameLinkInfo.Update();
begin
  LinkInfoToForm();
  inherited Update();
end;

end.

