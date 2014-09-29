; -- Example2.iss --
; Same as Example1.iss, but creates its icon in the Programs folder of the
; Start Menu instead of in a subfolder, and also creates a desktop icon.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

#define ExeName "talaria.exe"
#define AppVersion GetFileVersion(AddBackslash(SourcePath) + "talaria.exe")

[Setup]
AppName=Talaria
AppVersion={#AppVersion}
DefaultDirName={sd}\Talaria
DefaultGroupName=Talaria
Compression=lzma2
SolidCompression=yes
;DisableWelcomePage=yes
;DisableProgramGroupPage=yes
;DisableDirPage=yes
UninstallDisplayIcon={app}\{#ExeName}
;OutputDir=userdocs:Inno Setup Examples Output
OutputBaseFilename=Talaria_{#AppVersion}_setup
OutputDir=Installer

[Languages]
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"

;[Types]
;Name: "full"; Description: "Все компоненты"
;Name: "compact"; Description: "Минимум компонентов"
;Name: "custom"; Description: "Выборочные компоненты"

[Components]
;Name: sample_db; Description: "Демонстрационная база данных"; Types: "full"
;Name: docs; Description: "Руководство администратора"; Types: "full"

[Dirs]
Name: "{app}\data"
;Name: "{app}\docs"
;Name: "{app}\logs"; Flags: uninsalwaysuninstall

[Files]
Source: {#ExeName}; DestDir: "{app}"
;Source: "sqlite3.dll"; DestDir: "{app}"
Source: "data\conf_models.ini"; DestDir: "{app}\data"
;Source: "changes.txt"; DestDir: "{app}"
Source: "docs\talaria.chm"; DestDir: "{app}" 
;Source: "docs\talaria.chm"; DestDir: "{app}"; Components: docs 
;Source: "config_demo.ini"; DestDir: "{app}"; DestName: "config.ini"; Flags: onlyifdoesntexist 
;Source: "config_demo.ini"; DestDir: "{app}"; DestName: "config.ini"; Flags: onlyifdoesntexist 
;Source: "db.sqlite3"; DestDir: "{app}"; Flags: onlyifdoesntexist; Components: sample_db 
;Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[Icons]
Name: "{group}\Talaria"; Filename: "{app}\{#ExeName}"
Name: "{group}\Talaria documentation"; Filename: "{app}\talaria.chm"
Name: "{group}\Uninstall Talaria"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Talaria"; Filename: "{app}\{#ExeName}"

;[INI]
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "0";
;Filename: "{app}\config.ini"; Section: "Lockers"; Key: "Enabled"; String: "1"; Components: lockers
;Filename: "{app}\config.ini"; Section: "Turnstiles"; Key: "Enabled"; String: "1"; Components: turnstiles
