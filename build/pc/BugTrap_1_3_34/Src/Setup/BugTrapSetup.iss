; This is a part of the BugTrap package.
; Copyright (c) 2005-2009 IntelleSoft.
; All rights reserved.
;
; Description: Installation script.
; Author: Maksim Pyatkovskiy.
;
; This source code is only intended as a supplement to the
; BugTrap package reference and related electronic documentation
; provided with the product. See these sources for detailed
; information regarding the BugTrap package.

#define MyAppName                   "BugTrap for Win32 & .NET"
#define MyAppPublisher              "IntelleSoft"
#define MyAppURL                    "http://www.intellesoft.net/"
#define MyAppContact                "mailto:makspyat@intellesoft.net"
#define MyAppVersion                "1.3"
#define MyAppVersionEx              "1.3.3466.24114"
#define MyAppUrlName                "BugTrap Support Web Site.url"
#define MyAppDocDir                 "{app}\doc"
#define MyAppWin32PrjDir            "{app}\Win32\BugTrap"
#define MyAppWin32BinDir            "{app}\Win32\bin"
#define MyAppWin32ExamplesDir       "{app}\Win32\Examples"
#define MyAppServerDir              "{app}\Server"
#define MyAppDotNetServiceDir       "{app}\Server\BugTrapServer"
#define MyAppJavaAppServerDir       "{app}\Server\JBugTrapServer"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=true
LicenseFile=License.txt
InfoBeforeFile=ReadMe.txt
OutputBaseFilename=BugTrapSetup
Compression=lzma
SolidCompression=true
AppCopyright=Copyright © 2005-2009 {#MyAppPublisher}
ShowLanguageDialog=auto
AppVersion={#MyAppVersion}
AppID={{E1E905B2-272D-4DEB-85AA-B483F3539DD7}
AppReadmeFile={#MyAppDocDir}\ReadMe.txt
AppContact={#MyAppContact}
VersionInfoVersion={#MyAppVersionEx}
VersionInfoCompany={#MyAppPublisher}
VersionInfoDescription={#MyAppName} Setup
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp
UninstallDisplayIcon={#MyAppDocDir}\Bug.ico
MinVersion=0,4.0.1381sp6
PrivilegesRequired=admin

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Types]
Name: ClientServerType; Description: BugTrap client and server components
Name: ClientType; Description: BugTrap client components
Name: ServerType; Description: BugTrap server components
Name: CustomType; Description: Custom BugTrap configuration; Flags: iscustom

[Components]
Name: ClientComponent; Description: BugTrap client files; Types: ClientType ClientServerType; Languages: 
Name: ServerComponent; Description: BugTrap server files; Types: ServerType ClientServerType
Name: ServerComponent\DotNetServiceComponent; Description: Install BugTrap server as Windows service; Types: ServerType ClientServerType
Name: ServerComponent\JavaAppComponent; Description: Install BugTrap server as platform-independent Java application; Types: ServerType ClientServerType
Name: ServerComponent\AspDotNetWebAppComponent; Description: Install BugTrap server as ASP.NET Web application; Types: ServerType ClientServerType
Name: ServerComponent\AspDotNetWebAppComponent\RegistrationComponent; Description: Register BugTrap server in Microsoft Internet Information Server; Types: ServerType ClientServerType

[Files]
; Auxiliary files
Source: DoNothing.exe; DestDir: {tmp}; Flags: ignoreversion
; MSXML 6.0
Source: msxml6.msi; DestDir: {tmp}; Flags: ignoreversion
; Documentation
Source: Bug.ico; DestDir: {#MyAppDocDir}; Flags: ignoreversion
Source: ReadMe.txt; DestDir: {#MyAppDocDir}; Flags: ignoreversion
Source: License.txt; DestDir: {#MyAppDocDir}; Flags: ignoreversion
Source: ThirdParties.txt; DestDir: {#MyAppDocDir}; Flags: ignoreversion
Source: ..\doc\BugTrap.pdf; DestDir: {#MyAppDocDir}; Flags: ignoreversion
Source: ..\doc\History.txt; DestDir: {#MyAppDocDir}; Flags: ignoreversion
; BugTrap headers and common files
Source: BT32ReadMe.txt; DestDir: {#MyAppWin32PrjDir}; DestName: ReadMe.txt; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\BugTrap\BugTrap.h; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\BugTrap\BTTrace.h; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\BugTrap\BTAtlWindow.h; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\BugTrap\BTMfcWindow.h; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\BugTrap\HTML\BugTrap.chm; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
; BugTrap libraries
Source: dbghelp.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\CrashExplorer.exe; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
; BugTrap libraries for x86 platform
Source: ..\Win32\bin\BugTrap.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrap.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
; BugTrap libraries for x64 platform
Source: ..\Win32\bin\BugTrap-x64.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrap-x64.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU-x64.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU-x64.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN-x64.dll; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN-x64.lib; DestDir: {#MyAppWin32PrjDir}; Flags: ignoreversion; Components: ClientComponent
; Binary files
Source: BinReadMe.txt; DestDir: {#MyAppWin32BinDir}; DestName: ReadMe.txt; Flags: ignoreversion; Components: ClientComponent
Source: dbghelp.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\CrashExplorer.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
; BugTrap binaries for x86 platform
Source: ..\Win32\bin\BugTrap.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrap.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapConsoleTest.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapConsoleTestU.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapTest.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapTestU.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapLogTest.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapLogTestU.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
; BugTrap binaries for x64 platform
Source: ..\Win32\bin\BugTrap-x64.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrap-x64.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU-x64.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapU-x64.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN-x64.dll; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapN-x64.lib; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapConsoleTest-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapConsoleTestU-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapTest-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapTestU-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapLogTest-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\bin\BugTrapLogTestU-x64.exe; DestDir: {#MyAppWin32BinDir}; Flags: ignoreversion; Components: ClientComponent
; Console example
Source: ..\Win32\Examples\BugTrapConsoleTest\*.h; DestDir: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapConsoleTest\*.cpp; DestDir: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapConsoleTest\*.txt; DestDir: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapConsoleTest\*.vcproj; DestDir: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapConsoleTest\*.sln; DestDir: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Flags: ignoreversion; Components: ClientComponent
; GUI example
Source: ..\Win32\Examples\BugTrapTest\*.h; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\*.cpp; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\*.rc; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\*.txt; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\*.vcproj; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\*.sln; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\res\*.rc2; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest\res; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\res\*.bmp; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest\res; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapTest\res\*.ico; DestDir: {#MyAppWin32ExamplesDir}\BugTrapTest\res; Flags: ignoreversion; Components: ClientComponent
; .NET example
Source: ..\Win32\Examples\BugTrapNetTest\*.cs; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\*.resx; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\*.manifest; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\*.txt; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\*.csproj; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\*.sln; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\Properties\*.cs; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest\Properties; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\Properties\*.resx; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest\Properties; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapNetTest\Properties\*.settings; DestDir: {#MyAppWin32ExamplesDir}\BugTrapNetTest\Properties; Flags: ignoreversion; Components: ClientComponent
; Managed C++ example
Source: ..\Win32\Examples\BugTrapManCppTest\*.h; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.cpp; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.rc; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.resx; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.txt; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.vcproj; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.sln; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapManCppTest\*.ico; DestDir: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Flags: ignoreversion; Components: ClientComponent
; Log test
Source: ..\Win32\Examples\BugTrapLogTest\*.h; DestDir: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapLogTest\*.cpp; DestDir: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapLogTest\*.txt; DestDir: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapLogTest\*.vcproj; DestDir: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Flags: ignoreversion; Components: ClientComponent
Source: ..\Win32\Examples\BugTrapLogTest\*.sln; DestDir: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Flags: ignoreversion; Components: ClientComponent
; Servers
Source: SrvReadMe.txt; DestDir: {#MyAppServerDir}; DestName: ReadMe.txt; Flags: ignoreversion; Components: ServerComponent
; .NET service
Source: ..\Server\BugTrapServer\bin\Release\BugTrapServer.exe; DestDir: {#MyAppDotNetServiceDir}; Flags: ignoreversion; Components: ServerComponent\DotNetServiceComponent
Source: ..\Server\ConfigTemplates\BugTrapServer.config; DestDir: {#MyAppDotNetServiceDir}; DestName: BugTrapServer.exe.config; Flags: ignoreversion; Components: ServerComponent\DotNetServiceComponent
; Java application
Source: ..\Server\JBugTrapServer\JBugTrapServer.jar; DestDir: {#MyAppJavaAppServerDir}; Flags: ignoreversion; Components: ServerComponent\JavaAppComponent
Source: ..\Server\JBugTrapServer\start.cmd; DestDir: {#MyAppJavaAppServerDir}; Flags: ignoreversion; Components: ServerComponent\JavaAppComponent
Source: ..\Server\ConfigTemplates\BugTrapServer.config; DestDir: {#MyAppJavaAppServerDir}; Flags: ignoreversion; Components: ServerComponent\JavaAppComponent
; ASP.NET Web application
Source: ..\Server\BugTrapWebServer\PrecompiledWeb\BugTrapWebServer\RequestHandler.aspx; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\BugTrapWebServer\PrecompiledWeb\BugTrapWebServer\PrecompiledApp.config; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\BugTrapWebServer\PrecompiledWeb\BugTrapWebServer\Install.js; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\BugTrapWebServer\PrecompiledWeb\BugTrapWebServer\bin\*; DestDir: {code:GetWebAppDir}\bin; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\BugTrapWebServer\Source\RequestSimulator.htm; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\BugTrapWebServer\Source\Default.css; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\ConfigTemplates\Web.config; DestDir: {code:GetWebAppDir}; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent
Source: ..\Server\ConfigTemplates\bugtrap_web_trust.config; DestDir: {code:GetDotNetFrameworkDir}\CONFIG; Flags: ignoreversion; Components: ServerComponent\AspDotNetWebAppComponent

[INI]
Filename: {#MyAppDocDir}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL}

[Registry]
Root: HKLM; Subkey: SYSTEM\CurrentControlSet\Services\Eventlog\Application\BugTrapWebServer; ValueType: string; ValueName: EventMessageFile; ValueData: {code:GetDotNetFrameworkDir}\EventLogMessages.dll; Components: ServerComponent\AspDotNetWebAppComponent

[Icons]
; Core links
Name: {group}\BugTrap Developer's Guide; Filename: {#MyAppDocDir}\BugTrap.pdf; Comment: BugTrap developer's guide
Name: {group}\BugTrap Reference; Filename: {#MyAppWin32PrjDir}\BugTrap.chm; Comment: List of available classes, functions and definitions; Components: ClientComponent
Name: {group}\BugTrap Code; Filename: {#MyAppWin32PrjDir}; Comment: C++ headers and library files; Components: ClientComponent
Name: {group}\Crash Explorer; Filename: {#MyAppWin32PrjDir}\CrashExplorer.exe; Comment: MAP and PDB file analyzer; Components: ClientComponent
Name: {group}\BugTrap Server; Filename: {app}\Server; Comment: BugTrap server files; Components: ServerComponent
; Examples
Name: {group}\Win32 Examples\Windows GUI application; Filename: {#MyAppWin32ExamplesDir}\BugTrapTest; Comment: Example of Windows GUI application; Components: ClientComponent
Name: {group}\Win32 Examples\Console application; Filename: {#MyAppWin32ExamplesDir}\BugTrapConsoleTest; Comment: Example of console application; Components: ClientComponent
Name: {group}\Win32 Examples\.NET GUI application; Filename: {#MyAppWin32ExamplesDir}\BugTrapNetTest; Comment: Example of .NET GUI application; Components: ClientComponent
Name: {group}\Win32 Examples\Managed C++ application; Filename: {#MyAppWin32ExamplesDir}\BugTrapManCppTest; Comment: Example of managed C++ application; Components: ClientComponent
Name: {group}\Win32 Examples\Log Test; Filename: {#MyAppWin32ExamplesDir}\BugTrapLogTest; Comment: Demonstrates log functions; Components: ClientComponent
Name: {group}\Win32 Examples\Executables; Filename: {#MyAppWin32BinDir}; Comment: Binary files; Components: ClientComponent
; Special links
Name: {group}\{cm:ProgramOnTheWeb,{#MyAppName}}; Filename: {#MyAppDocDir}\{#MyAppUrlName}
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}

[Run]
Filename: msiexec.exe; Parameters: "/package ""{tmp}\msxml6.msi"" /quiet /norestart"; WorkingDir: {tmp}; Flags: runhidden; StatusMsg: {cm:InstallingMSXML}; Components: ClientComponent
Filename: {#MyAppDotNetServiceDir}\BugTrapServer.exe; Parameters: /install; WorkingDir: {#MyAppDotNetServiceDir}; Flags: runhidden; Components: ServerComponent\DotNetServiceComponent; StatusMsg: {cm:RegisteringDotNetServiceMessage}
Filename: {sys}\net.exe; Parameters: "start ""BugTrap Server"""; WorkingDir: {#MyAppDotNetServiceDir}; Flags: runhidden; Components: ServerComponent\DotNetServiceComponent; StatusMsg: {cm:StartingDotNetServiceMessage}
Filename: {tmp}\DoNothing.exe; WorkingDir: {tmp}; Flags: runhidden; Components: ServerComponent\AspDotNetWebAppComponent\RegistrationComponent; StatusMsg: {cm:RegisteringWebAppMessage}

[UninstallRun]
Filename: {sys}\net.exe; Parameters: "stop ""BugTrap Server"""; WorkingDir: {#MyAppDotNetServiceDir}; Flags: runhidden; Components: ServerComponent\DotNetServiceComponent; RunOnceId: StopService
Filename: {#MyAppDotNetServiceDir}\BugTrapServer.exe; Parameters: /uninstall; WorkingDir: {#MyAppDotNetServiceDir}; Flags: runhidden; Components: ServerComponent\DotNetServiceComponent; RunOnceId: DeleteService

[UninstallDelete]
Name: {#MyAppDocDir}\{#MyAppUrlName}; Type: files
Name: {#MyAppWin32BinDir}; Type: filesandordirs
Name: {#MyAppWin32ExamplesDir}; Type: filesandordirs
Name: {#MyAppJavaAppServerDir}\BugTrapServerError.log; Type: files; Components: ServerComponent\JavaAppComponent

[CustomMessages]
InstallingMSXML=Installing Microsoft Core XML Services
StartingDotNetServiceMessage=Starting BugTrap .NET service
RegisteringDotNetServiceMessage=Registering BugTrap .NET service
RegisteringWebAppMessage=Registering BugTrap ASP.NET Web Application
DotNetInstallPrompt=This application requires the Microsoft .NET Framework 2.0. Please download and install .NET Framework and run this setup again. Do you want to download the framework now (YES), continue at your own risk (NO) or cancel the setup (CANCEL)?
WebAppRegistrationError=Can't register BugTrap ASP.NET Web Application in IIS
CheckIISMessage=Please make sure Microsoft Internet Information Server is installed on this computer
LineDelimiter=------------------------------------------------------------------------------------
Recommendation1=BugTrap Web Server files were stored in:
Recommendation2=Fix the problem and register BugTrap in IIS using Web Server installation script (Install.js) or reinstall whole package.
ErrorPrefix=Error:

[Code]
const
	DotNetFrameworkRegPath = 'SOFTWARE\Microsoft\.NETFramework';
	DotNetFrameworkDownloadUrl = 'http://www.microsoft.com/downloads/details.aspx?FamilyID=0856eacb-4362-4b0d-8edd-aab15c5e04f5&DisplayLang=en';
	BugTrapRegPath = 'SOFTWARE\IntelleSoft\BugTrap for Win32 & .NET';
	WebAppRegPath = BugTrapRegPath + '\BugTrap Web Server';
	WebAppInstalledRegValue = 'Installed';
	WebAppDirName = 'BugTrapWebServer';
	RequiredMajorVerNum = 2;
	RequiredMinorVerNum = 0;

var
	DotNetFrameworkDir : string;
	WebAppDir : string;

function GetDotNetFrameworkDir(Param : string) : string;
begin
	{ Return .NET Framework location }
	Result := DotNetFrameworkDir;
end;

function GetWebAppDir(Param : string) : string;
var
	IIS : Variant;
	WebServerice : Variant;
	WebServer : Variant;
	WebRoot : Variant;

begin
	if WebAppDir = '' then
	begin
		WebAppDir := ExpandConstant('{#MyAppServerDir}');
		if IsComponentSelected('ServerComponent\AspDotNetWebAppComponent\RegistrationComponent') then
		try
			{ Initialize IIS objects }
			IIS := CreateOleObject('IISNamespace');
			WebServerice := IIS.GetObject('IIsWebService', 'localhost/W3SVC');
			WebServer := WebServerice.GetObject('IIsWebServer', '1');
			WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
			WebAppDir := WebRoot.Path;
		except
			{ Ignore error, use default path }
		end;
		WebAppDir := AddBackslash(WebAppDir) + WebAppDirName;
	end;
	Result := WebAppDir;
end;

function CheckDotNetFramework() : boolean;
var
	RegValueNames : array of string;
	RegSubKeyNames : array of string;
	DotNetFrameworkPolicyRegPath : string;
	DotNetFrameworkPolicyVerRegPath : string;
	DotNetFrameworkRegInstallValue : string;
	DotNetFrameworkPolicyVerValid : boolean;
	RegKeyName : string;
	NumRegSubKeyNames : integer;
	NumRegValues : integer;
	RegValueNum : integer;
	SubKeyNum : integer;
	RegKeyNameLength : integer;
	RegKeyNameIndex : integer;
	MajorVerNum : integer;
	MinorVerNum : integer;
	DotPos : integer;

begin
	if RegQueryStringValue(HKLM, DotNetFrameworkRegPath, 'InstallRoot', DotNetFrameworkRegInstallValue) then
	begin
		{ Enumerate .NET Framework policy keys }
		DotNetFrameworkPolicyRegPath := AddBackslash(DotNetFrameworkRegPath) + 'policy';
		if RegGetSubkeyNames(HKLM, DotNetFrameworkPolicyRegPath, RegSubKeyNames) then
		begin
			NumRegSubKeyNames := GetArrayLength(RegSubKeyNames);
			for SubKeyNum := 0 to NumRegSubKeyNames - 1 do
			begin
				{ We are looking for keys with version number: vX.Y }
				RegKeyName := RegSubKeyNames[SubKeyNum];
				if RegKeyName[1] = 'v' then
				begin
					DotNetFrameworkPolicyVerValid := true;
					RegKeyNameLength := Length(RegKeyName);
					DotPos := -1;
					for RegKeyNameIndex := 2 to RegKeyNameLength do
					begin
						if RegKeyName[RegKeyNameIndex] = '.' then
						begin
							if DotPos > 0 then
							begin
								DotNetFrameworkPolicyVerValid := false;
								break;
							end;
							DotPos := RegKeyNameIndex;
						end
						else if (RegKeyName[RegKeyNameIndex] < '0') or (RegKeyName[RegKeyNameIndex] > '9') then
						begin
							DotNetFrameworkPolicyVerValid := false;
							break;
						end;
					end;
					{ Extract .NET Framework version number from key name }
					if DotNetFrameworkPolicyVerValid then
					begin
						if DotPos > 0 then
						begin
							MajorVerNum := StrToInt(Copy(RegKeyName, 2, DotPos - 2));
							MinorVerNum := StrToInt(Copy(RegKeyName, DotPos + 1, RegKeyNameLength - DotPos));
						end
						else
						begin
							MajorVerNum := StrToInt(Copy(RegKeyName, 2, RegKeyNameLength - 1));
							MinorVerNum := 0;
						end;
						{ Make sure this version is not older than required }
						if ((MajorVerNum > RequiredMajorVerNum) or ((MajorVerNum = RequiredMajorVerNum) and (MinorVerNum >= RequiredMinorVerNum))) then
						begin
							{ Get .NET Framework subversion }
							DotNetFrameworkPolicyVerRegPath := AddBackslash(DotNetFrameworkPolicyRegPath) + RegKeyName;
							if RegGetValueNames(HKLM, DotNetFrameworkPolicyVerRegPath, RegValueNames) then
							begin
								NumRegValues := GetArrayLength(RegValueNames);
								for RegValueNum := 0 to NumRegValues - 1 do
								begin
									{ Reconstruct absolute path to .NET Framework files for the required version }
									DotNetFrameworkDir := AddBackslash(DotNetFrameworkRegInstallValue) + RegKeyName + '.' + RegValueNames[RegValueNum];
									if DirExists(DotNetFrameworkDir) then
									begin
										Result := true;
										exit;
									end;
								end;
							end;
						end;
					end;
				end;
			end;
		end;
	end;
	Result := false;
end;

function InitializeSetup() : boolean;
var
	ResultCode : integer;

begin
	{ Check .NET Framework version }
	if not CheckDotNetFramework then
	begin
		{ Warn user and open .NET Framework download page }
		ResultCode := MsgBox(ExpandConstant('{cm:DotNetInstallPrompt}'), mbConfirmation, MB_YESNOCANCEL);
		if ResultCode = IDYES then
		begin
			{ Download .NET Framework and cancel the setup }
			ShellExec('open', DotNetFrameworkDownloadUrl, '', '', SW_SHOWNORMAL, ewNoWait, ResultCode);
			Result := false;
		end
		else if ResultCode = IDNO then
		begin
			{ Continue the setup }
			Result := true;
		end
		else
		begin
			{ Cancel the setup }
			Result := false;
		end;
	end
	else
	begin
		{ Required .NET Framework already installed }
		Result := true;
	end;
end;

procedure CurStepChanged(CurStep : TSetupStep);
var
	ResultCode : integer;
	IIS : Variant;
	WebServerice : Variant;
	WebServer : Variant;
	WebRoot : Variant;
	WebSite : Variant;

begin
	if CurStep = ssInstall then
	begin
		{ Stop .NET service before copying files - this is required if BugTrap was installed before }
		if IsComponentSelected('ServerComponent\DotNetServiceComponent') then
		begin
			Exec(ExpandConstant('{sys}\net.exe'), 'stop "BugTrap Server"', ExpandConstant('{tmp}'), SW_HIDE, ewWaitUntilTerminated, ResultCode);
		end;
	end
	else if CurStep = ssPostInstall then
	begin
		{ Register BugTrap Web application in IIS }
		if IsComponentSelected('ServerComponent\AspDotNetWebAppComponent\RegistrationComponent') then
		try
			{ Initialize IIS objects }
			IIS := CreateOleObject('IISNamespace');
			WebServerice := IIS.GetObject('IIsWebService', 'localhost/W3SVC');
			WebServer := WebServerice.GetObject('IIsWebServer', '1');
			WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
			try
				{ Delete old Web application directory (if exists) }
				WebSite := WebRoot.GetObject('IIsWebVirtualDir', WebAppDirName);
				WebSite.DeleteObject(0);
			except
				{ BugTrap Web application directory may not exist }
			end;
			{ Update target path if necessary }
			if WebAppDir = '' then
			begin
				WebAppDir := AddBackslash(WebRoot.Path) + WebAppDirName;
			end;
			{ Register new Web application directory in IIS }
			WebSite := WebRoot.Create('IIsWebVirtualDir', WebAppDirName);
			WebSite.AccessRead := true;
			WebSite.AccessScript := true;
			WebSite.EnableDefaultDoc := false;
			WebSite.AppAllowDebugging := false;
			WebSite.AppAllowClientDebug := false;
			WebSite.AspAllowSessionState := false;
			WebSite.ContentIndexed := false;
			WebSite.AppIsolated := 1; { isolated process }
			WebSite.Path := WebAppDir;
			WebSite.AppCreate(true);
			WebSite.AppFriendlyName := WebAppDirName;
			WebSite.SetInfo();
			{ Store key for uninstaller }
			RegWriteDWordValue(HKLM, WebAppRegPath, WebAppInstalledRegValue, 1);
		except
			// WebAppDir := AddBackslash(ExpandConstant('{#MyAppServerDir}')) + WebAppDirName;
			MsgBox(
				ExpandConstant('{cm:WebAppRegistrationError}') + #13#10 +
				ExpandConstant('{cm:ErrorPrefix}') + ' ' + GetExceptionMessage + #13#10 +
				ExpandConstant('{cm:CheckIISMessage}') + #13#10 +
				ExpandConstant('{cm:LineDelimiter}') + #13#10 +
				ExpandConstant('{cm:Recommendation1}') + ' ' + WebAppDir + #13#10 +
				ExpandConstant('{cm:Recommendation2}'),
				mbError, MB_OK);
		end;
	end;
end;

procedure CurUninstallStepChanged(CurStep : TUninstallStep);
var
	IIS : Variant;
	WebServerice : Variant;
	WebServer : Variant;
	WebRoot : Variant;
	WebSite : Variant;
	WebAppInstalledValue : Cardinal;

begin
	if CurStep = usUninstall then
	begin
		if RegQueryDWordValue(HKLM, WebAppRegPath, WebAppInstalledRegValue, WebAppInstalledValue) then
		begin
			if WebAppInstalledValue <> 0 then
			try
				{ Initialize IIS objects }
				IIS := CreateOleObject('IISNamespace');
				WebServerice := IIS.GetObject('IIsWebService', 'localhost/W3SVC');
				WebServer := WebServerice.GetObject('IIsWebServer', '1');
				WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
				{ Delete Web application directory }
				WebSite := WebRoot.GetObject('IIsWebVirtualDir', WebAppDirName);
				WebSite.DeleteObject(0);
			except
				{ Do nothing }
			end;
		end;
		RegDeleteKeyIncludingSubkeys(HKLM, BugTrapRegPath);
	end;
end;
