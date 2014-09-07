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
    FLinkInfo: TDnmpLinkInfo;
    procedure FSetLinkInfo(Value: TDnmpLinkInfo);
    procedure ShrinkPhoto(img: TImage);
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

procedure TFrameLinkInfo.actImageFromClipboardExecute(Sender: TObject);
begin
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfDelphiBitmap)) then
    imgPicture.Picture.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
    imgPicture.Picture.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
  ShrinkPhoto(imgPicture);
end;

procedure TFrameLinkInfo.FSetLinkInfo(Value: TDnmpLinkInfo);
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
var
  ss: TStringStream;
begin
  if not Assigned(LinkInfo) then Exit;
  //imgPicture.Picture.Assign(imgDefault.Picture);
  imgPicture.Picture.Clear();

  if Assigned(LinkInfo.Contact) then
  begin
    edAddr.Text:=AddrToStr(LinkInfo.Contact.Addr);
    edName.Text:=LinkInfo.Contact.Name;
    edGUID.Text:=LinkInfo.Contact.GUID;
    edSGUID.Text:=LinkInfo.Contact.SeniorGUID;
    edStatusMessage.Text:=LinkInfo.Contact.StatusMessage;
    if Length(LinkInfo.Contact.Picture)>4 then
    begin
      ss:=TStringStream.Create(LinkInfo.Contact.Picture);
      try
        imgPicture.Picture.LoadFromStream(ss);
      finally
        ss.Free();
      end;
    end;
  end;

  edOwner.Text:=LinkInfo.Owner;
  edLocation.Text:=LinkInfo.Location;
  edIPAddr.Text:=LinkInfo.IpAddr;
  edPhoneNo.Text:=LinkInfo.PhoneNo;
  edOtherInfo.Text:=LinkInfo.OtherInfo;
  edKey.Text:=LinkInfo.Key;

end;

procedure TFrameLinkInfo.LinkInfoFromForm();
var
  ss: TStringStream;
begin
  if Assigned(LinkInfo.Contact) then
  begin
    LinkInfo.Contact.Addr:=StrToAddr(Trim(edAddr.Text));
    LinkInfo.Contact.Name:=Trim(edName.Text);
    LinkInfo.Contact.GUID:=Trim(edGUID.Text);
    LinkInfo.Contact.SeniorGUID:=Trim(edSGUID.Text);
    LinkInfo.Contact.StatusMessage:=Trim(edStatusMessage.Text);

    // picture
    ss:=TStringStream.Create('');
    //imgPicture.Picture.Jpeg.SaveToStream(ss);
    imgPicture.Picture.PNG.SaveToStream(ss);
    //imgPicture.Picture.SaveToStreamWithFileExt(ss, 'jpg');
    LinkInfo.Contact.Picture:=ss.DataString;
    ss.Free;
    if Length(LinkInfo.Contact.Picture)<4 then LinkInfo.Contact.Picture:='';
  end;

  LinkInfo.Owner:=Trim(edOwner.Text);
  LinkInfo.Location:=Trim(edLocation.Text);
  LinkInfo.IpAddr:=Trim(edIPAddr.Text);
  LinkInfo.PhoneNo:=Trim(edPhoneNo.Text);
  LinkInfo.OtherInfo:=Trim(edOtherInfo.Text);
  LinkInfo.Key:=Trim(edKey.Text);
end;

end.

