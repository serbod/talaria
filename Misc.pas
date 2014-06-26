unit Misc;

interface
uses SysUtils, ComCtrls, ValEdit, dnmp_unit, IniFiles, Classes, Windows;

procedure LinkInfoToVLE(LinkInfo: TLinkInfo; vle: TValueListEditor);
procedure LinkInfoFromVLE(LinkInfo: TLinkInfo; vle: TValueListEditor);

procedure ShowLinkInfoList(lv: TListView; lil: TLinkInfoList);
procedure ShowLinksList(lv: TListView; ll: TDnmpLinkList);

procedure IniToVLE(ini: TMemIniFile; SectionName: string; vle: TValueListEditor);
procedure IniFromVLE(ini: TMemIniFile; SectionName: string; vle: TValueListEditor);

// Копирует один файл в другой
function CopyFile(FileNameSrc, FileNameDst: string): boolean;
// Проверяет заданный путь. Создает каталоги, если их нет
function CheckPath(sPath: string): Boolean;

// Возвращает имя пользователя windows
function GetWinUserName(): string;
// Возвращает имя компьютера
function GetWinCompName(): string;
// Возвращает версию Windows
function GetWinVersion(): string;
//
function GetTimestampStr(): string;
//
function GetIpHost(sIpAddr: string): string;
function GetIpPort(sIpAddr: string): string;

type TStringArray = array of string;
// Парсит строку с пробелами, с учетом двойных кавычек (")
function ParseStr(s: String; bAddEmpty: boolean = false): TStringArray;
// Нормализует строку (добавляет двойные кавычки)
function Norm(s: string): string;
// Из указанной строки возвращает параметр по его номеру
function ParamFromStr(s: string; ParamNum: integer): string;
// Выделяет из строки первое слово и все остальные
procedure ExtractCmd(sText: string; var sCmd, sParam: string);


implementation

procedure LinkInfoToVLE(LinkInfo: TLinkInfo; vle: TValueListEditor);
begin
  if not Assigned(vle) then Exit;
  if not Assigned(LinkInfo) then Exit;
  vle.Strings.BeginUpdate();
  vle.Strings.Clear();
  vle.Values['Addr']:=AddrToStr(LinkInfo.Addr);
  vle.Values['GUID']:=LinkInfo.GUID;
  vle.Values['SeniorGUID']:=LinkInfo.SeniorGUID;
  vle.Values['Name']:=LinkInfo.Name;
  vle.Values['Owner']:=LinkInfo.Owner;
  vle.Values['Location']:=LinkInfo.Location;
  vle.Values['IpAddr']:=LinkInfo.IpAddr;
  vle.Values['PhoneNo']:=LinkInfo.PhoneNo;
  vle.Values['OtherInfo']:=LinkInfo.OtherInfo;
  vle.Values['Rating']:=IntToStr(LinkInfo.Rating);
  vle.Values['Key']:=LinkInfo.Key;
  vle.Strings.EndUpdate();
end;

procedure LinkInfoFromVLE(LinkInfo: TLinkInfo; vle: TValueListEditor);
begin
  if not Assigned(vle) then Exit;
  if not Assigned(LinkInfo) then Exit;
  LinkInfo.Addr:=StrToAddr(vle.Values['Addr']);
  LinkInfo.GUID:=vle.Values['GUID'];
  LinkInfo.SeniorGUID:=vle.Values['SeniorGUID'];
  LinkInfo.Name:=vle.Values['Name'];
  LinkInfo.Owner:=vle.Values['Owner'];
  LinkInfo.Location:=vle.Values['Location'];
  LinkInfo.IpAddr:=vle.Values['IpAddr'];
  LinkInfo.PhoneNo:=vle.Values['PhoneNo'];
  LinkInfo.OtherInfo:=vle.Values['OtherInfo'];
  LinkInfo.Rating:=StrToIntDef(vle.Values['Rating'], 1);
  LinkInfo.Key:=vle.Values['Key'];
end;

procedure ShowLinkInfoList(lv: TListView; lil: TLinkInfoList);
var
  i, si, ic: Integer;
  li: TLinkInfo;
  lvi: TListItem;
begin
  lv.Items.BeginUpdate();
  si:=lv.ItemIndex;
  ic:=lil.Count;
  while lv.Items.Count <> ic do
  begin
    if lv.Items.Count > ic then lv.Items.Delete(lv.Items.Count-1);
    if lv.Items.Count < ic then lv.Items.Add();
  end;
  for i:=0 to ic-1 do
  begin
    li:=lil[i];
    lvi:=lv.Items[i];
    lvi.Data:=li;
    lvi.Caption:=AddrToStr(li.Addr);
    while lvi.SubItems.Count < 3 do lvi.SubItems.Add('');
    lvi.SubItems[0]:=li.Name;
    lvi.SubItems[1]:=LinkTypeToStr(li.LinkType);
    if li.Online then lvi.SubItems[2]:='Online' else lvi.SubItems[2]:='Offline';
  end;
  if si < lv.Items.Count then lv.ItemIndex:=si;
  lv.Items.EndUpdate();
end;

procedure ShowLinksList(lv: TListView; ll: TDnmpLinkList);
var
  i, si, ic: Integer;
  li: TLinkInfo;
  lvi: TListItem;
  s: string;
begin
  lv.Items.BeginUpdate();
  si:=lv.ItemIndex;
  ic:=ll.Count;
  while lv.Items.Count <> ic do
  begin
    if lv.Items.Count > ic then lv.Items.Delete(lv.Items.Count-1);
    if lv.Items.Count < ic then lv.Items.Add();
  end;
  for i:=0 to ic-1 do
  begin
    li:=TDnmpLink(ll[i]).LinkInfo;
    lvi:=lv.Items[i];
    lvi.Data:=ll[i];
    lvi.Caption:=AddrToStr(li.Addr);
    lvi.SubItems.Clear();
    lvi.SubItems.Add(li.Name);
    lvi.SubItems.Add(LinkTypeToStr(TDnmpLink(ll[i]).LinkType));

    if TDnmpLink(ll[i]).Active then lvi.SubItems.Add('Active')
    else lvi.SubItems.Add('Down');
  end;
  if si < lv.Items.Count then lv.ItemIndex:=si;
  lv.Items.EndUpdate();
end;

 procedure IniToVLE(ini: TMemIniFile; SectionName: string; vle: TValueListEditor);
var
  i: Integer;
  sl: TStringList;
begin
  vle.Strings.BeginUpdate();
  sl:=TStringList.Create();
  ini.ReadSection(SectionName, sl);
  for i:=0 to sl.Count-1 do
  begin
    vle.Values[sl[i]]:=ini.ReadString(SectionName, sl[i], vle.Values[sl[i]]);
  end;
  vle.Strings.EndUpdate();
end;

procedure IniFromVLE(ini: TMemIniFile; SectionName: string; vle: TValueListEditor);
var
  i: Integer;
begin
  for i:=0 to vle.RowCount-1 do
  begin
    ini.WriteString(SectionName, vle.Keys[i], vle.Values[vle.Keys[i]]);
  end;
end;

// Копирует один файл в другой
function CopyFile(FileNameSrc, FileNameDst: string): boolean;
var
  fss, fsd: TFileStream;
  buf: array[1..2048] of byte;
  i: integer;
begin
  Result:=False;
  fss:=nil;
  fsd:=nil;
  try
    fss:=TFileStream.Create(FileNameSrc, fmOpenRead);
    fsd:=TFileStream.Create(FileNameDst, fmCreate);
    repeat
      i:=fss.Read(buf, SizeOf(buf));
      fsd.Write(buf, i);
    until i<=0;
    Result:=true;
  finally
    fsd.Free();
    fss.Free();
  end;
end;

// Проверяет заданный путь. Создает каталоги, если их нет
function CheckPath(sPath: string): Boolean;
begin
  Result:=True;
  if (not DirectoryExists(sPath)) then
  begin
    if (not CreateDir(sPath)) then Result:=False;
  end;
end;

// Возвращает имя пользователя windows
function GetWinUserName(): string;
var
  lpBuffer: PChar;
  n: cardinal;
begin
  n:=20;
  lpBuffer:=StrAlloc(n);
  GetUserName(lpBuffer, n);
  result := String(lpBuffer);
end;

// Возвращает имя компьютера
function GetWinCompName(): string;
var
  lpBuffer: PChar;
  n: cardinal;
begin
  n:=20;
  lpBuffer:=StrAlloc(n);
  GetComputerName(lpBuffer, n);
  result := String(lpBuffer);
end;

function GetWinVersion(): string;
var
  dwVersion: DWORD;
  lw: WORD;
begin
  dwVersion := GetVersion();
  lw:=Word(dwVersion);
  Result := ''+IntToStr(Byte(lw))+'.'+IntToStr(HiByte(lw))+'.'+IntToStr(HiWord(dwVersion));
end;

function GetTimestampStr(): string;
var
  st: TSystemTime;
begin
  GetSystemTime(st);
  result:=IntToStr(DateTimeToFileDate(SystemTimeToDateTime(st)));
end;

function GetIpHost(sIpAddr: string): string;
var
  i: Integer;
begin
  i:=Pos(':', sIpAddr);
  if i=0 then Result:=Trim(sIpAddr)
  else Result:=Trim(Copy(sIpAddr, 1, i-1));
end;

function GetIpPort(sIpAddr: string): string;
var
  i: Integer;
begin
  i:=Pos(':', sIpAddr);
  if i=0 then Result:=''
  else Result:=Trim(Copy(sIpAddr, i+1, maxint));
end;

// Нормализует строку
function Norm(s: string): string;
begin
  if (Pos(' ', s)=0) and (s<>'') then result:=s
  else
  begin
    result:='"'+StringReplace(s, '"', '""', [rfReplaceAll])+'"';
  end;
end;

// Парсит строку с пробелами, с учетом двойных кавычек (")
// bAddEmpty - признак добавления пустых строк
function ParseStr(s: String; bAddEmpty: boolean = false): TStringArray;
var
  i,l,rl: integer;
  InBracket: boolean;
  TmpStr: String;

procedure AddStr;
begin
  if (TmpStr='') and (not bAddEmpty) then Exit;
  Inc(rl);
  SetLength(result, rl);
  result[rl-1]:=TmpStr;
  TmpStr:='';
end;

begin
  i:=0;
  l:=Length(s);
  rl:=0;
  InBracket:=false;
  TmpStr:='';
  SetLength(result, rl);
  while i<l do
  begin
    Inc(i);
    case s[i] of
    ' ':
      if not InBracket then AddStr()
      else TmpStr:=TmpStr+s[i];
    '"':
    begin
      if (i+1<l) and (s[i+1]='"') then
      begin
        if ((i+2<l) and (s[i+2]=' ')) or (i+2=l) then
        begin
          // empty brackets
          InBracket:=false;
          AddStr();
          Inc(i, 2);
          continue;
        end;
        // two brackets as one bracket
        TmpStr:=TmpStr+'"';
        Inc(i);
        continue;
      end;
      if InBracket then
      begin
        InBracket:=false;
        AddStr();
      end
      else
      begin
        InBracket:=true;
        continue;
      end;
    end;
    else
    // normal char
    TmpStr:=TmpStr+s[i];
    end;
  end;
  AddStr();
end;

function ParamFromStr(s: string; ParamNum: integer): string;
var
  sa: TStringArray;
begin
  Result:='';
  sa:=ParseStr(s);
  if Length(sa)<ParamNum then Exit;
  Result:=sa[ParamNum];
end;

procedure ExtractCmd(sText: string; var sCmd, sParam: string);
var
  i: Integer;
begin
  i:=Pos(' ', sText);
  if i=0 then
  begin
    sCmd:=sText;
    sParam:='';
  end
  else
  begin
    sCmd:=Copy(sText, 1, i-1);
    sParam:=Copy(sText, i+1, MaxInt);
  end;
end;

end.
