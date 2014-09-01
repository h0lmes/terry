[Setup]
AppName=Terry
AppVerName=Terry
AppPublisher=Holmes
DefaultDirName={pf}\terry
DefaultGroupName=Terry
AllowNoIcons=yes
OutputDir=.
OutputBaseFilename=Terry_Setup
Compression=zip
SolidCompression=yes
InfoBeforeFile=info.txt

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "terry.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "Help\*"; DestDir: "{app}\Help\"; Flags: ignoreversion recursesubdirs
Source: "Images\*"; DestDir: "{app}\Images\"; Flags: ignoreversion recursesubdirs
Source: "Plugins\*"; DestDir: "{app}\Plugins\"; Flags: ignoreversion recursesubdirs
Source: "Themes\*"; DestDir: "{app}\Themes\"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\Terry"; Filename: "{app}\terry.exe"; WorkingDir: "{app}"
Name: "{group}\{cm:UninstallProgram,terry}"; Filename: "{uninstallexe}"; WorkingDir: "{app}"
Name: "{userdesktop}\Terry"; Filename: "{app}\terry.exe"; WorkingDir: "{app}"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\terry"; Filename: "{app}\terry.exe"; WorkingDir: "{app}"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\terry.exe"; Description: "{cm:LaunchProgram,Terry}"; Flags: nowait postinstall
