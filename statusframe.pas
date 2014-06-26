unit StatusFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls;

type

  { TFrameStatus }

  TFrameStatus = class(TFrame)
    panItem: TPanel;
    panStatus: TPanel;
    Splitter1: TSplitter;
    ToolBarStatus: TToolBar;
    tvItems: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
    procedure UpdateTree();
  end;

implementation

{$R *.lfm}

{ TFrameStatus }

function AddTreeItem(tv: TTreeView; ParentNode: TTreeNode; Obj: TObject; Name: string; IconIndex: integer = -1): TTreeNode;
begin
  Result:=tv.Items.AddChild(ParentNode, Name);
  Result.Data:=Obj;
  Result.Text:=Name;
  Result.ImageIndex:=IconIndex;
  Result.StateIndex:=IconIndex;
end;

procedure TFrameStatus.UpdateTree();
begin
  tvItems.BeginUpdate();

  tvItems.EndUpdate();
end;

end.

