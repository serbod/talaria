unit dnmp_serializers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_unit, fpjson, jsonparser, IniFiles;

type

  { TDnmpSerializerJson }

  TDnmpSerializerJson = class(TDnmpSerializer)
  public
    function GetName(): string; override;
    function StorageToString(AStorage: TDnmpStorage): AnsiString; override;
    function StorageFromString(AStorage: TDnmpStorage; AString: AnsiString
      ): boolean; override;
    function StorageToFile(AStorage: TDnmpStorage; AFileName: string): boolean;
      override;
    function StorageFromFile(AStorage: TDnmpStorage; AFileName: string
      ): boolean; override;
  end;

  { TDnmpSerializerIni (write only)}
  {
  @ - string
  # - integer
  $ - float
  % - dictionary
  & - list
  }
  TDnmpSerializerIni = class(TDnmpSerializer)
  public
    function GetName(): string; override;
    function StorageToString(AStorage: TDnmpStorage): AnsiString; override;
    function StorageFromString(AStorage: TDnmpStorage; AString: AnsiString
      ): boolean; override;
    function StorageToFile(AStorage: TDnmpStorage; AFileName: string): boolean;
      override;
    function StorageFromFile(AStorage: TDnmpStorage; AFileName: string
      ): boolean; override;
  end;

  { TDnmpSerializerBencode }
  {
  Bencode serializer
  integers: i<value>e
    i0e  i42e  i-42e
  strings: <lalie_len>:<value>
    3:ben  4:code
  lists: l<items>e (without any spaces)
    l i42e 3:ben 4:code e
  dictionaries: d<items>e  where items is <string_name><value>
    d 4:name 3:ben  4:code i42e e
  }
  TDnmpSerializerBencode = class(TDnmpSerializer)
  private
    function GetName(): string; override;
    function StorageToBencode(AStorage: TDnmpStorage): AnsiString;
    function ReadBencodeValue(AStorage: TdnmpStorage; AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): boolean;
    function ReadBencodeIntegerStr(var AString: AnsiString; var APos: Cardinal;
      ALen: Cardinal): AnsiString;
    function ReadBencodeString(var AString: AnsiString; var APos: Cardinal;
      ALen: Cardinal): AnsiString;
    function ReadBencodeList(AStorage: TdnmpStorage; var AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): boolean;
    function ReadBencodeDictionary(AStorage: TdnmpStorage; var AString: AnsiString;
      var APos: Cardinal; ALen: Cardinal): boolean;
  public
    function StorageToString(AStorage: TDnmpStorage): AnsiString; override;
    function StorageFromString(AStorage: TDnmpStorage; AString: AnsiString
      ): boolean; override;
    function StorageToFile(AStorage: TDnmpStorage; AFileName: string): boolean;
      override;
    function StorageFromFile(AStorage: TDnmpStorage; AFileName: string
      ): boolean; override;
  end;

implementation

{ TDnmpSerializerBencode }

function TDnmpSerializerBencode.GetName: string;
begin
  Result:='BENCODE';
end;

function TDnmpSerializerBencode.StorageToBencode(AStorage: TDnmpStorage
  ): AnsiString;
var
  sName: AnsiString;
  SubItem: TDnmpStorage;
  i: integer;
begin
  Result:='';
  if AStorage.StorageType=stString then
  begin
    Result:=Result+IntToStr(Length(AStorage.Value))+':'+AStorage.Value;
  end;

  if AStorage.StorageType=stNumber then
  begin
    Result:=Result+IntToStr(Length(AStorage.Value))+':'+AStorage.Value;
  end;

  if AStorage.StorageType=stInteger then
  begin
    Result:=Result+'i'+AStorage.Value+'e';
  end;

  if AStorage.StorageType=stDictionary then
  begin
    Result:=Result+'d';
    for i:=0 to AStorage.Count-1 do
    begin
      sName:=AStorage.GetObjectName(i);
      SubItem:=(AStorage.GetObject(i) as TDnmpStorage);
      // name
      Result:=Result+IntToStr(Length(sName))+':'+sName;
      // value
      Result:=Result+StorageToBencode(SubItem);
    end;
    Result:=Result+'e';
  end;

  if AStorage.StorageType=stList then
  begin
    Result:=Result+'l';
    for i:=0 to AStorage.Count-1 do
    begin
      SubItem:=(AStorage.GetObject(i) as TDnmpStorage);
      // value
      Result:=Result+StorageToBencode(SubItem);
    end;
    Result:=Result+'e';
  end;
end;

function TDnmpSerializerBencode.ReadBencodeIntegerStr(var AString: AnsiString;
  var APos: Cardinal; ALen: Cardinal): AnsiString;
begin
  Result:='';
  if AString[APos]='i' then Inc(APos) else Exit;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Break
    end;
    Result:=Result+AString[APos];
    Inc(APos);
  end;
end;

function TDnmpSerializerBencode.ReadBencodeString(var AString: AnsiString;
  var APos: Cardinal; ALen: Cardinal): AnsiString;
var
  sValue: AnsiString;
  ValueLen: Cardinal;
begin
  Result:='';
  sValue:='';
  while APos<=ALen do
  begin
    if AString[APos]=':' then
    begin
      ValueLen:=StrToIntDef(sValue, 0);
      Result:=Copy(AString, APos+1, ValueLen);
      APos:=APos+ValueLen+1;
      Exit;
    end;
    sValue:=sValue+AString[APos];
    Inc(APos);
  end;
end;

function TDnmpSerializerBencode.ReadBencodeDictionary(AStorage: TdnmpStorage;
  var AString: AnsiString; var APos: Cardinal; ALen: Cardinal): boolean;
var
  sName: AnsiString;
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if AString[APos]='d' then Inc(APos) else Exit;
  AStorage.StorageType:=stDictionary;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Result:=True;
      Exit;
    end;
    sName:=ReadBencodeString(AString, APos, ALen);
    SubStorage:=TDnmpStorage.Create(stUnknown);
    if ReadBencodeValue(SubStorage, AString, APos, ALen) then AStorage.Add(sName, SubStorage);
  end;
end;

function TDnmpSerializerBencode.ReadBencodeList(AStorage: TdnmpStorage;
  var AString: AnsiString; var APos: Cardinal; ALen: Cardinal): boolean;
var
  SubStorage: TDnmpStorage;
begin
  Result:=False;
  if AString[APos]='l' then Inc(APos) else Exit;
  AStorage.StorageType:=stList;
  while APos<=ALen do
  begin
    if AString[APos]='e' then
    begin
      Inc(APos);
      Result:=True;
      Exit;
    end;
    SubStorage:=TDnmpStorage.Create(stUnknown);
    if ReadBencodeValue(SubStorage, AString, APos, ALen) then AStorage.Add('', SubStorage);
  end;
end;

function TDnmpSerializerBencode.ReadBencodeValue(AStorage: TdnmpStorage;
  AString: AnsiString; var APos: Cardinal; ALen: Cardinal): boolean;
begin
  Result:=False;
  if not Assigned(AStorage) then Exit;
  if APos<=ALen then
  begin
    if AString[APos]='i' then
    begin
      // read integer value
      AStorage.StorageType:=stInteger;
      AStorage.Value:=ReadBencodeIntegerStr(AString, APos, ALen);
      Result:=True;
    end

    else if Pos(AString[APos], '0123456789')>0 then
    begin
      // read string value
      AStorage.StorageType:=stString;
      AStorage.Value:=ReadBencodeString(AString, APos, ALen);
      Result:=True;
    end

    else if AString[APos]='d' then
    begin
      // read dictionary value
      ReadBencodeDictionary(AStorage, AString, APos, ALen);
      Result:=True;
    end

    else if AString[APos]='l' then
    begin
      // read list value
      ReadBencodeList(AStorage, AString, APos, ALen);
      Result:=True;
    end

    else
    begin
      // error
      Exit;
    end;
  end;
end;

function TDnmpSerializerBencode.StorageToString(AStorage: TDnmpStorage
  ): AnsiString;
begin
  Result:=StorageToBencode(AStorage);
end;

function TDnmpSerializerBencode.StorageFromString(AStorage: TDnmpStorage;
  AString: AnsiString): boolean;
var
  n: Cardinal;
begin
  n:=1;
  Result:=ReadBencodeValue(AStorage, AString, n, Length(AString));
end;

function TDnmpSerializerBencode.StorageToFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  Result:=StrToFile(AFileName+'.be', Self.StorageToString(AStorage));
end;

function TDnmpSerializerBencode.StorageFromFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  Result:=Self.StorageFromString(AStorage, FileToStr(AFileName+'.be'));
end;

{ TDnmpSerializerIni }

function TDnmpSerializerIni.GetName: string;
begin
  Result:='INI';
end;

function TDnmpSerializerIni.StorageToString(AStorage: TDnmpStorage): AnsiString;
begin
  Result:=inherited StorageToString(AStorage);
end;

function TDnmpSerializerIni.StorageFromString(AStorage: TDnmpStorage;
  AString: AnsiString): boolean;
begin
  Result:=inherited StorageFromString(AStorage, AString);
end;

{TODO: repair}
function TDnmpSerializerIni.StorageToFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
var
  ini: TMemIniFile;

  procedure WriteStorageToIni(Storage: TDnmpStorage; sSect: string);
  var
    sName: string;
    TmpItem: TDnmpStorage;
    i: integer;
  begin
    if Storage.StorageType=stString then
    begin
      ini.WriteString(sSect, '@', Storage.GetString());
    end;

    if Storage.StorageType=stNumber then
    begin
      ini.WriteFloat(sSect, '$', Storage.GetReal());
    end;

    if Storage.StorageType=stInteger then
    begin
      ini.WriteInteger(sSect, '#', Storage.GetInteger());
    end;

    if Storage.StorageType=stDictionary then
    begin
      //ini.WriteInteger(sSect, '%'+sName, Storage.Count());
      for i:=0 to Storage.Count-1 do
      begin
        sName:=Storage.GetObjectName(i);
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        if TmpItem.StorageType=stString then ini.WriteString(sSect, '@'+sName, TmpItem.GetString())
        else if TmpItem.StorageType=stNumber then ini.WriteFloat(sSect, '$'+sName, TmpItem.GetReal())
        else if TmpItem.StorageType=stInteger then ini.WriteInteger(sSect, '#'+sName, TmpItem.GetInteger())
        else if TmpItem.StorageType=stDictionary then
        begin
          ini.WriteInteger(sSect, '%'+sName, TmpItem.Count());
          WriteStorageToIni(TmpItem, sSect+'.%'+sName);
        end
        else if TmpItem.StorageType=stList then
        begin
          ini.WriteInteger(sSect, '&'+sName, TmpItem.Count());
          WriteStorageToIni(TmpItem, sSect+'.&'+sName);
        end;
      end;
    end;

    if Storage.StorageType=stList then
    begin
      //ini.WriteInteger(sSect, '%'+sName, Storage.Count());
      for i:=0 to Storage.Count-1 do
      begin
        sName:=IntToStr(i);
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        if TmpItem.StorageType=stString then ini.WriteString(sSect, '@'+sName, TmpItem.GetString())
        else if TmpItem.StorageType=stNumber then ini.WriteFloat(sSect, '$'+sName, TmpItem.GetReal())
        else if TmpItem.StorageType=stInteger then ini.WriteInteger(sSect, '#'+sName, TmpItem.GetInteger())
        else if TmpItem.StorageType=stDictionary then
        begin
          ini.WriteInteger(sSect, '%'+sName, TmpItem.Count());
          WriteStorageToIni(TmpItem, sSect+'.%'+sName);
        end
        else if TmpItem.StorageType=stList then
        begin
          ini.WriteInteger(sSect, '&'+sName, TmpItem.Count());
          WriteStorageToIni(TmpItem, sSect+'.&'+sName);
        end;
      end;
    end;

  end;

begin
  //Result:=inherited StorageToFile(AStorage, AFileName);
  Result:=False;
  try
     ini:=TMemIniFile.Create(AFileName+'.ini');
     ini.Clear();
     WriteStorageToIni(AStorage, 'item');
     ini.UpdateFile();
     Result:=True;
  finally
    ini.Free();
  end;
end;

function TDnmpSerializerIni.StorageFromFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
begin
  Result:=inherited StorageFromFile(AStorage, AFileName);
end;

{ TDnmpSerializerJson }

function TDnmpSerializerJson.GetName: string;
begin
  Result:='JSON';
end;

function TDnmpSerializerJson.StorageToString(AStorage: TDnmpStorage
  ): AnsiString;
var
  jj: TJSONData;

  function StorageAsJsonData(Storage: TDnmpStorage): TJSONData;
  var
    i: integer;
    TmpItem: TDnmpStorage;
  begin
    Result:=nil;
    if not Assigned(Storage) then Exit;

    if Storage.StorageType=stUnknown then Exit

    else if Storage.StorageType=stString then
    begin
      Result:=TJSONString.Create(Storage.Value);
    end

    else if Storage.StorageType=stInteger then
    begin
      Result:=TJSONIntegerNumber.Create(StrToIntDef(Storage.Value, 0));
    end

    else if Storage.StorageType=stNumber then
    begin
      if (Pos('.', Storage.Value)>0) or (Pos(',', Storage.Value)>0) then
      begin
        Result:=TJSONFloatNumber.Create(StrToFloatDef(Storage.Value, 0));
      end

      else // integer
      begin
        Result:=TJSONIntegerNumber.Create(StrToIntDef(Storage.Value, 0));
      end
    end

    else if Storage.StorageType=stList then
    begin
      Result:=TJSONArray.Create();
      for i:=0 to Storage.Count-1 do
      begin
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        (Result as TJSONArray).Add(StorageAsJsonData(TmpItem));
      end;
    end

    else if Storage.StorageType=stDictionary then
    begin
      Result:=TJSONObject.Create();
      for i:=0 to Storage.Count-1 do
      begin
        TmpItem:=(Storage.GetObject(i) as TDnmpStorage);
        (Result as TJSONObject).Add(Storage.GetObjectName(i), StorageAsJsonData(TmpItem));
      end;
    end;
  end;

begin
  Result:='';
  jj:=StorageAsJsonData(AStorage);
  if not Assigned(jj) then Exit;
  //Result:=jj.AsJSON;
  Result:=jj.FormatJSON();
  jj.Free();
end;

function TDnmpSerializerJson.StorageFromString(AStorage: TDnmpStorage;
  AString: AnsiString): boolean;
var
  parser: TJSONParser;
  jj: TJSONData;

  function JsonDataToStorage(j: TJSONData; Storage: TDnmpStorage): boolean;
  var
    SubStorage: TDnmpStorage;
    i: integer;
  begin
    Result:=False;
    if not Assigned(j) then Exit;
    if not Assigned(Storage) then Exit;

    if j.JSONType=TJSONtype.jtString then
    begin
      Storage.StorageType:=stString;
      Storage.Value:=j.AsString;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtNumber then
    begin
      Storage.StorageType:=stNumber;
      Storage.Value:=j.AsString;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtArray then
    begin
      Storage.StorageType:=stDictionary;
      for i:=0 to j.Count-1 do
      begin
        SubStorage:=TDnmpStorage.Create(stUnknown);
        if JsonDataToStorage(j.Items[i], SubStorage) then Storage.Add(IntToStr(i), SubStorage)
        else SubStorage.Free();
      end;
      Result:=True;
    end;

    if j.JSONType=TJSONtype.jtObject then
    begin
      Storage.StorageType:=stDictionary;
      for i:=0 to j.Count-1 do
      begin
        SubStorage:=TDnmpStorage.Create(stUnknown);
        if JsonDataToStorage(j.Items[i], SubStorage) then Storage.Add((j as TJSONObject).Names[i], SubStorage)
        else SubStorage.Free();
      end;
      Result:=True;
    end;
  end;

begin
  Result:=False;
  parser:=TJSONParser.Create(AString);
  try
    jj:=parser.Parse();
  finally
    parser.Free();
  end;
  if not Assigned(jj) then Exit;

  try
    Result:=JsonDataToStorage(jj, AStorage);
  finally
    jj.Free();
  end;

end;

function TDnmpSerializerJson.StorageToFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  //Result:=inherited StorageToFile(AStorage, AFileName);
  Result:=StrToFile(AFileName+'.json', Self.StorageToString(AStorage));
end;

function TDnmpSerializerJson.StorageFromFile(AStorage: TDnmpStorage;
  AFileName: string): boolean;
begin
  Result:=False;
  if Trim(AFileName)='' then Exit;
  //Result:=inherited StorageFromFile(AStorage, AFileName);
  Result:=Self.StorageFromString(AStorage, FileToStr(AFileName+'.json'));
end;

end.

