unit dnmp_serializers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dnmp_unit, fpjson, jsonparser, IniFiles;

type

  { TDnmpSerializerJson }

  TDnmpSerializerJson = class(TDnmpSerializer)
  public
    function StorageToString(AStorage: TDnmpStorage): AnsiString; override;
    function StorageFromString(AStorage: TDnmpStorage; AString: AnsiString
      ): boolean; override;
    function StorageToFile(AStorage: TDnmpStorage; AFileName: string): boolean;
      override;
    function StorageFromFile(AStorage: TDnmpStorage; AFileName: string
      ): boolean; override;
  end;

  { TDnmpSerializerIni }
  {

  @ - string
  # - integer
  $ - float
  % - dictionary
  & - list
  }
  TDnmpSerializerIni = class(TDnmpSerializer)
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

{ TDnmpSerializerIni }

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
    sName, sValue: string;
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

