
[Setup]
AppName={cm:MyAppName}
AppVerName={cm:MyAppVerName,1.0}
AppPublisher={cm:Company}
AppPublisherURL=http://foundations.github.com
AppSupportURL=http://foundations.github.com
AppUpdatesURL=http://foundations.github.com
DefaultDirName=c:\foundations\win32
OutputBaseFilename=foundations_win32
Outputdir=.
;DisableDirPage=yes
;DisableProgramGroupPage=yes
;DirExistsWarning=no
Uninstallable=no

VersionInfoCompany=Foundations Co., Ltd
VersionInfoCopyright=Copyright 2012 ljjun.
VersionInfoDescription=1.0.0
VersionInfoTextVersion=1.0.0
VersionInfoVersion=1.0.0.0

WindowVisible=yes
AppCopyright={cm:Copyright}
UsePreviousAppDir=noUsePreviousLanguage=no

[LangOptions]
LanguageCodePage=0

[Types]
;Name: "full"; Description: {cm:full}
;Name: "Run"; Description: {cm:Run}
;Name: "custom"; Description: {cm:custom}; Flags: iscustom

[Components]
;Name: "Dunite"; Description: {cm:Dunite}; Types: full Run custom

[Languages]
Name: chs; MessagesFile: compiler:Default.isl;
;Name: eng; MessagesFile: compiler:Languages\english.isl;

[Messages]
chs.BeveledLabel=¼òÌåÖÐÎÄ
;eng.BeveledLabel=English

[CustomMessages]
chs.MyAppName=foundations_win32
chs.MyAppVerName=foundations
chs.Company=Foundations Co., Ltd

chs.Copyright=Copyright 2012 ljjun
chs.VersionInfoText=Foundations
chs.VersionDescription=Foundations

;eng.MyAppName=foundations_win32
;eng.MyAppVerName=foundations
;eng.Company=Foundations Co., Ltd

;eng.Copyright=Copyright 2012 ljjun
;eng.VersionInfoText=Foundations
;eng.VersionDescription=Foundations

[Files]
Source: ".\win32\*"; DestDir: "{app}"; Excludes: "*.1.tlog,*vc10.log,*.scc,.svn,*.sdf,*.ilk,*.pdb,*.obj,*.idb,*.pch,ipch,_UpgradeReport_Files"; Flags: ignoreversion recursesubdirs

[Run]
[Dirs]

[Registry]

[Code]
