unit ConfigFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, Buttons,
  Menus, ExtCtrls, IniFiles, LCLType;

type

  { TConfigNode }

  TConfigNode = class(TTreeNode)
  public
    SectionName: string;
    ParentSectionName: string;
    ModelName: string;
    { params keys }
    Params: TStringList;
    { sub-items models names }
    Models: TStringList;
    constructor Create(AnOwner: TTreeNodes); reintroduce;
    destructor Destroy(); override;
  end;

  { TFrameConfig }

  TFrameConfig = class(TFrame)
    btnReadConfig: TBitBtn;
    btnSet: TButton;
    cbValue: TComboBox;
    gbParams: TGroupBox;
    lbParamName: TLabel;
    lvParams: TListView;
    miAddSubitem: TMenuItem;
    miDeleteSubitem: TMenuItem;
    pmOptions: TPopupMenu;
    tvOptions: TTreeView;
    procedure btnReadConfigClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure cbValueChange(Sender: TObject);
    procedure cbValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lvParamsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miAddSubitemClick(Sender: TObject);
    procedure miDeleteSubitemClick(Sender: TObject);
    procedure tvOptionsSelectionChanged(Sender: TObject);
  private
    { private declarations }
    CurConfNode: TConfigNode;
    CurParam: string;
    CurValue: string;
  public
    { public declarations }
    ConfFileName: string;
    Config: TMemIniFile;
    ConfigModels: TMemIniFile;
    procedure ReadConfig();
    procedure WriteConfig();
    function ReadModel(const ModelName: string; var ModelCaption, ParentSection: string; var Single: boolean; Params: TStrings): boolean;
  end;

const
  csConfModelsIni = 'conf_models.ini';
  csConfIni = 'config.ini';

implementation

{$R *.lfm}

{ TConfigNode }

constructor TConfigNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);
  Params:=TStringList.Create();
  Models:=TStringList.Create();
end;

destructor TConfigNode.Destroy();
begin
  FreeAndNil(Models);
  FreeAndNil(Params);
  inherited Destroy();
end;

{ TFrameConfig }

procedure TFrameConfig.tvOptionsSelectionChanged(Sender: TObject);
var
  ConfNode: TConfigNode;
  li: TListItem;
  i: integer;
begin
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ConfNode:=(tvOptions.Selected as TConfigNode);
  CurConfNode:=ConfNode;
  gbParams.Caption:=ConfNode.Text;
  lvParams.BeginUpdate();
  lvParams.Clear();
  for i:=0 to ConfNode.Params.Count-1 do
  begin
    li:=lvParams.Items.Add();
    li.Caption:=ConfNode.Params[i];
    li.SubItems.Add(Config.ReadString(ConfNode.SectionName, ConfNode.Params[i], ''));
  end;
  lvParams.EndUpdate();
  lbParamName.Caption:='';
  cbValue.Items.Clear();
  cbValue.Text:='';
  if lvParams.Items.Count>0 then lvParams.Selected:=lvParams.Items[0];
end;

procedure TFrameConfig.lvParamsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  s, ss: string;
  n, m: integer;
begin
  if not Selected then Exit;
  CurParam:=Trim(Item.Caption);
  CurValue:=Trim(Item.SubItems.Text);
  // lbParamName.Caption:=Item.Caption;
  //lbParamName.Caption:=AnsiToUtf8(ConfigModels.ReadString(CurConfNode.ModelName, CurParam, CurParam));

  cbValue.Items.Clear();
  // get values list for param from description string
  // Test param {value1|value2}
  ss:=ConfigModels.ReadString(CurConfNode.ModelName, CurParam, CurParam);
  n:=Pos('{', ss);
  m:=Pos('}', ss);
  if (n>0) and (m>n) then
  begin
    s:=Copy(ss, n+1, m-n-1);
    ss:=Copy(ss, 1, n-1);
    n:=Pos('|', s);
    while n>0 do
    begin
      cbValue.Items.Add(Copy(s, 1, n-1));
      s:=Copy(s, n+1, maxint);
      n:=Pos('|', s);
    end;
    cbValue.Items.Add(s);
  end;

  lbParamName.Caption:=ss;
  cbValue.Text:=CurValue;
end;

procedure TFrameConfig.miAddSubitemClick(Sender: TObject);
var
  ConfNode, ParentNode: TConfigNode;
  sModel, sCapt, sParentSect, sNum: string;
  Single: boolean;
  SectParams: TStringList;
begin
  // Добавление экземпляра многоразовой модели в раздел конфига
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ParentNode:=(tvOptions.Selected as TConfigNode);

  if ParentNode.Models.Count=0 then Exit;
  tvOptions.BeginUpdate();
  { TODO : Model selection }
  sModel:=ParentNode.Models[0];
  sNum:=IntToStr(ParentNode.Count+1);

  Single:=False;
  sCapt:='';
  sParentSect:='';
  SectParams:=TStringList.Create();
  ReadModel(sModel, sCapt, sParentSect, Single, SectParams);

  // create node
  ConfNode:=TConfigNode.Create(tvOptions.Items);
  ConfNode.Text:=sCapt+' '+sNum;
  ConfNode.ModelName:=sModel;
  ConfNode.SectionName:=sModel+'_'+sNum;
  ConfNode.ParentSectionName:=sParentSect;
  ConfNode.Params.Assign(SectParams);
  ConfNode.Data:=self;
  SectParams.Free();
  // add note to tree
  tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAddChild);
  tvOptions.EndUpdate();
end;

procedure TFrameConfig.miDeleteSubitemClick(Sender: TObject);
var
  ConfNode, ParentNode: TConfigNode;
begin
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ConfNode:=(tvOptions.Selected as TConfigNode);

  Config.EraseSection(ConfNode.SectionName);
  tvOptions.Items.Delete(ConfNode);
end;

procedure TFrameConfig.cbValueChange(Sender: TObject);
begin
  // запись значения параметра из поля ввода в таблицу параметров и конфиг
  if not Assigned(lvParams.Selected) then Exit;
  lvParams.BeginUpdate();
  lvParams.Selected.SubItems.Clear();
  lvParams.Selected.SubItems.Add(Trim(cbValue.Text));
  lvParams.EndUpdate();
  Config.WriteString(CurConfNode.SectionName, CurParam, Trim(cbValue.Text));
end;

procedure TFrameConfig.btnSetClick(Sender: TObject);
begin
  cbValue.Text:=CurValue;
end;

procedure TFrameConfig.btnReadConfigClick(Sender: TObject);
begin
  ReadConfig();
end;

procedure TFrameConfig.cbValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then btnSet.Click();
end;

procedure TFrameConfig.ReadConfig();
var
  Sections: TStringList;
  SectParams: TStringList;
  i, n, m: integer;
  sModel, sSectName, sCapt, sParentSect: string;
  Single: boolean;
  ConfNode, ParentNode: TConfigNode;
  tn: TTreeNode;

procedure AddNodeToTree(ASectName, AText: string);
begin
  // create node
  ConfNode:=TConfigNode.Create(tvOptions.Items);
  ConfNode.Text:=AText;
  ConfNode.ModelName:=sModel;
  ConfNode.SectionName:=ASectName;
  ConfNode.ParentSectionName:=sParentSect;
  ConfNode.Params.Assign(SectParams);
  ConfNode.Data:=self;
  //Config.ReadSection(sSectName, ConfNode.Params);
  {if Config.SectionExists(sSect) then
  begin
    Config.ReadSection(sSectName, ConfNode.Params);
  end;}
  // add note to tree
  if Assigned(ParentNode) then
  begin
    //ParentNode.Models.Add(ConfNode.ModelName);
    tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAddChild);
  end
  else
  begin
    tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAdd);
  end;
end;

begin
  // Read config models
  if not Assigned(ConfigModels) then Exit;
  //if Assigned(ConfigModels) then FreeAndNil(ConfigModels);
  //if not FileExists(csConfModelsIni) then Exit;
  //ConfigModels:=TMemIniFile.Create(csConfModelsIni);

  // Read config
  if not Assigned(Config) then Exit;
  //if Assigned(Config) then FreeAndNil(Config);
  //if not FileExists(csConfIni) then Exit;
  //Config:=TMemIniFile.Create(csConfIni);
  //Config:=TMemIniFile.Create(self.ConfFileName);

  // Build options tree
  tvOptions.BeginUpdate();
  tvOptions.Items.Clear();

  Sections:=TStringList.Create();
  SectParams:=TStringList.Create();
  ConfigModels.ReadSections(Sections);
  for i:=0 to Sections.Count-1 do
  begin
    // Read section model
    sModel:=Sections[i];
    Single:=False;
    sCapt:='';
    sParentSect:='';
    ReadModel(sModel, sCapt, sParentSect, Single, SectParams);

    // try to find parent node
    ParentNode:=nil;
    for m:=0 to tvOptions.Items.Count-1 do
    begin
      tn:=tvOptions.Items.Item[m];
      if not (tn is TConfigNode) then Continue;
      //if not Assigned(tn.Data) then Continue;
      //if TConfigNode(tn.Data).SectionName = sParentSect then
      if (tn as TConfigNode).SectionName = sParentSect then
      begin
        ParentNode:=(tn as TConfigNode);
        Break;
      end;
    end;

    // add singleton node
    if Single then
    begin
      AddNodeToTree(sModel, sCapt);
    end
    else
    begin
      // Add model to parent (if any)
      if Assigned(ParentNode) then ParentNode.Models.Add(sModel);
      // try to get corresponding sections from main config
      n:=1;
      while (n>0) and (n<32) do
      begin
        sSectName:=sModel+'_'+IntToStr(n);
        if not Config.SectionExists(sSectName) then n:=0
        else
        begin
          AddNodeToTree(sSectName, sCapt+' '+IntToStr(n));
          n:=n+1;
        end;
      end;
    end;
  end;
  SectParams.Free();
  Sections.Free();
  tvOptions.FullExpand();
  tvOptions.EndUpdate();
end;

procedure TFrameConfig.WriteConfig();
begin
  if Assigned(Config) then Config.UpdateFile();
end;

function TFrameConfig.ReadModel(const ModelName: string; var ModelCaption,
  ParentSection: string; var Single: boolean; Params: TStrings): boolean;
var
  sParam: string;
  n: integer;
begin
  Result:=False;
  if not ConfigModels.SectionExists(ModelName) then Exit;
  // Read section model
  //ModelCaption:=AnsiToUtf8(ConfigModels.ReadString(ModelName, 'Caption', ModelName));
  ModelCaption:=ConfigModels.ReadString(ModelName, 'Caption', ModelName);
  Single:=ConfigModels.ReadBool(ModelName, 'Singleton', True);
  ParentSection:=ConfigModels.ReadString(ModelName, 'ParentSection', '');

  ConfigModels.ReadSection(ModelName, Params);
  for n:=Params.Count-1 downto 0 do
  begin
    sParam:=Params[n];
    if Pos('|'+sParam+'|', '|Caption|Singleton|ParentSection|')>0 then Params.Delete(n);
  end;
  Result:=True;
end;

end.

