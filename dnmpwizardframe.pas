unit DnmpWizardFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

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
    procedure PageLanguageBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure PageUplinkBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
  private
    { private declarations }
  public
    { public declarations }
    procedure Finish();
  end;

var
  FormDnmpWizard: TFormDnmpWizard;

implementation

{$R *.lfm}

{ TFormDnmpWizard }

procedure TFormDnmpWizard.PageLanguageBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

procedure TFormDnmpWizard.btnNextClick(Sender: TObject);
begin
  if nbPages.ActivePageComponent=PageLanguage then nbPages.ActivePageComponent:=PageUser
  else if nbPages.ActivePageComponent=PageUser then nbPages.ActivePageComponent:=PageUplink
  else if nbPages.ActivePageComponent=PageUplink then nbPages.ActivePageComponent:=PageFinish
  else if nbPages.ActivePageComponent=PageFinish then Finish();
end;

procedure TFormDnmpWizard.btnBackClick(Sender: TObject);
begin
  if nbPages.ActivePageComponent=PageLanguage then nbPages.ActivePageComponent:=PageLanguage
  else if nbPages.ActivePageComponent=PageUser then nbPages.ActivePageComponent:=PageLanguage
  else if nbPages.ActivePageComponent=PageUplink then nbPages.ActivePageComponent:=PageUser
  else if nbPages.ActivePageComponent=PageFinish then nbPages.ActivePageComponent:=PageUplink;
end;

procedure TFormDnmpWizard.PageUplinkBeforeShow(ASender: TObject;
  ANewPage: TPage; ANewIndex: Integer);
begin

end;

procedure TFormDnmpWizard.Finish;
begin

end;

end.

