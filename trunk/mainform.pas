unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ActnList, Core;

type

  { TFormMain }

  TFormMain = class(TForm)
    actAbout: TAction;
    actExit: TAction;
    actOptions: TAction;
    alMain: TActionList;
    ImageList16: TImageList;
    imgSidebarBottom: TImage;
    imgSidebarTop: TImage;
    ImageList24: TImageList;
    pgcMain: TPageControl;
    panSidebar: TPanel;
    ToolBarMain: TToolBar;
    tbAbout: TToolButton;
    tbExit: TToolButton;
    tbOptions: TToolButton;
    procedure actAboutExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdatePages();
    procedure ActivatePage(PageItem: TMainFormPageItem);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.actAboutExecute(Sender: TObject);
begin
  Core.Init();
  UpdatePages();
end;

procedure TFormMain.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TFormMain.UpdatePages();
var
  i, ii: integer;
  tsheet: TTabSheet;
  //frame: TFrame;
  PageItem: TMainFormPageItem;
begin
  if not Assigned(MainFormPages) then Exit;

  // delete pages
  for i:=pgcMain.PageCount-1 downto 0 do
  begin
    tsheet:=pgcMain.Pages[i];
    for ii:=0 to MainFormPages.Count-1 do
    begin
      if (MainFormPages.Items[ii] as TMainFormPageItem).TabSheet = tsheet then
      begin
        tsheet:=nil;
        Break;
      end;
    end;
    if Assigned(tsheet) then FreeAndNil(tsheet);
  end;

  // update/add pages
  for i:=0 to MainFormPages.Count-1 do
  begin
    PageItem:=(MainFormPages.Items[i] as TMainFormPageItem);
    if Assigned(PageItem.TabSheet) then
    begin
      tsheet:=(PageItem.TabSheet as TTabSheet);
    end
    else
    begin
      tsheet:=pgcMain.AddTabSheet();
      PageItem.TabSheet:=tsheet;
      PageItem.Frame.Parent:=tsheet;
      PageItem.Frame.Align:=alClient;
    end;
    tsheet.Caption:=PageItem.Caption;
  end;
end;

procedure TFormMain.ActivatePage(PageItem: TMainFormPageItem);
begin
  if not Assigned(PageItem) then Exit;
  pgcMain.ActivePage:=(PageItem.TabSheet as TTabSheet);
end;

end.

