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
    procedure tbOptionsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ServiceDnmpNode: TServiceDnmpNode;
    procedure Init();
  end;

var
  FormMain: TFormMain;

implementation

uses StatusFrame, ChatFrame, DnmpNodeFrame;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.tbOptionsClick(Sender: TObject);
begin

end;

procedure TFormMain.actAboutExecute(Sender: TObject);
begin
  Init();
end;

procedure TFormMain.Init();
var
  i: integer;
  tsheet: TTabSheet;
  frame: TFrame;
begin

  // clear all pages
  for i:=pgcMain.PageCount-1 downto 0 do
  begin
    pgcMain.Pages[i].Free();
  end;

  // status page
  tsheet:=pgcMain.AddTabSheet();
  tsheet.Caption:='Status';
  frame:=TFrameStatus.Create(tsheet);
  frame.Parent:=tsheet;
  frame.Align:=alClient;

  // chat page
  tsheet:=pgcMain.AddTabSheet();
  tsheet.Caption:='Chat';
  frame:=TFrameChat.Create(tsheet);
  frame.Parent:=tsheet;
  frame.Align:=alClient;

  // node
  ServiceDnmpNode:=TServiceDnmpNode.Create('1.0');

  tsheet:=pgcMain.AddTabSheet();
  tsheet.Caption:='Node';
  frame:=TFrameDnmpNode.Create(tsheet);
  frame.Parent:=tsheet;
  frame.Align:=alClient;

  ServiceDnmpNode.Frame:=frame;
  (frame as TFrameDnmpNode).Mgr:=ServiceDnmpNode.Mgr;
  (frame as TFrameDnmpNode).ServMgr:=ServiceDnmpNode.ServMgr;

end;

end.

