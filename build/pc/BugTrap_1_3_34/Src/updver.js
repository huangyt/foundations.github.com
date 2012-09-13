/*
 * This is a part of the BugTrap package.
 * Copyright (c) 2005-2009 IntelleSoft.
 * All rights reserved.
 *
 * Description: Update package version numbers.
 * Author: Maksim Pyatkovskiy.
 *
 * This source code is only intended as a supplement to the
 * BugTrap package reference and related electronic documentation
 * provided with the product. See these sources for detailed
 * information regarding the BugTrap package.
 */

// Major & minor version numbers.
var BugTrap_ProductMajorVersion = 1;
var BugTrap_ProductMinorVersion = 3;

var BugTrap_FileMajorVersion = 1;
var BugTrap_FileMinorVersion = 3;

var CrashExplorer_FileMajorVersion = 1;
var CrashExplorer_FileMinorVersion = 3;

var BugTrapServer_AssemblyMajorVersion = 1;
var BugTrapServer_AssemblyMinorVersion = 3;

function HandleError(err) {
	WScript.Echo(err.number + ': ' + (err.description != '' ? err.description : 'Unknown error'));
	WScript.Quit();
}

function combinePath(str1, str2) {
	if (str1.length > 0 && str1.charAt(str1.length - 1) == '\\') {
		return (str1 + str2);
	}
	else {
		return (str1 + '\\' + str2);
	}
}

try {
	// Global file system object
	var ForReading = 1;
	var ForWriting = 2;
	var TristateUseDefault = -2;
	var TristateTrue = -1;
	var TristateFalse = 0;
	var fso = new ActiveXObject('Scripting.FileSystemObject');
	// Construct source path
	var productPath = WScript.ScriptFullName;
	productPath = productPath.substr(0, productPath.lastIndexOf('\\'));
	// Date & time
	var date = new Date();
	var dateOnly = new Date(date.getFullYear(), date.getMonth(), date.getDate());
	var time = date.getTime();
	// Build number & revision number
	var buildNumber = Math.floor((time - new Date(2000, 0, 1).getTime()) / (24 * 60 * 60 * 1000));
	var revisionNumber = Math.floor((time - dateOnly.getTime()) / 2000);
}
catch (err) {
	HandleError(err);
}

function updateRcVersion(relativePath, productMajorVersion, productMinorVersion, fileMajorVersion, fileMinorVersion) {
	var absolutePath = combinePath(productPath, relativePath);
	var stream = fso.OpenTextFile(absolutePath, ForReading, false, TristateFalse);
	var text = stream.ReadAll();
	var version = productMajorVersion + ', ' + productMinorVersion + ', ' + buildNumber + ', ' + revisionNumber;
	text = text.replace(/^([ \t]*PRODUCTVERSION[ \t]+)[\d, ]+[ \t]*$/m, '$1' + version);
	version = productMajorVersion + '.' + productMinorVersion + '.' + buildNumber + '.' + revisionNumber;
	text = text.replace(/^([ \t]*VALUE[ \t]+\"ProductVersion\",[ \t]*\")[\d\. ]+\"[ \t]*$/mg, '$1' + version + '\"');
	version = fileMajorVersion + ', ' + fileMinorVersion + ', ' + buildNumber + ', ' + revisionNumber;
	text = text.replace(/^([ \t]*FILEVERSION[ \t]+)[\d, ]+[ \t]*$/m, '$1' + version);
	version = fileMajorVersion + '.' + fileMinorVersion + '.' + buildNumber + '.' + revisionNumber;
	text = text.replace(/^([ \t]*VALUE[ \t]+\"FileVersion\",[ \t]*\")[\d\. ]+\"[ \t]*$/mg, '$1' + version + '\"');
	version = fileMajorVersion + '.' + fileMinorVersion;
	text = text.replace(/[\d\.]+(\"[ \t]*,[ \t]*IDC_VERSION_STRING)/m, version + '$1');
	stream.Close();
	stream = fso.OpenTextFile(absolutePath, ForWriting, false, TristateFalse);
	stream.Write(text);
	stream.Close();
}

function updateAssemblyInfoVersion(relativePath, assemblyMajorVersion, assemblyMinorVersion) {
	var absolutePath = combinePath(productPath, relativePath);
	var stream = fso.OpenTextFile(absolutePath, ForReading, false, TristateFalse);
	var text = stream.ReadAll();
	var version = assemblyMajorVersion + '.' + assemblyMinorVersion + '.' + buildNumber + '.' + revisionNumber;
	text = text.replace(/^([ \t]*\[[ \t]*assembly[ \t]*:[ \t]*AssemblyVersion[ \t]*\(\")[\d\.]+(\"[ \t]*\)[ \t]*\][ \t]*;?[ \t]*)$/m, '$1' + version + '$2');
	text = text.replace(/^([ \t]*\[[ \t]*assembly[ \t]*:[ \t]*AssemblyFileVersion[ \t]*\(\")[\d\.]+(\"[ \t]*\)[ \t]*\][ \t]*;?[ \t]*)$/m, '$1' + version + '$2');
	stream.Close();
	stream = fso.OpenTextFile(absolutePath, ForWriting, false, TristateFalse);
	stream.Write(text);
	stream.Close();
}

function updateIssVersion(relativePath, productMajorVersion, productMinorVersion) {
	var absolutePath = combinePath(productPath, relativePath);
	var stream = fso.OpenTextFile(absolutePath, ForReading, false, TristateFalse);
	var text = stream.ReadAll();
	var version = productMajorVersion + '.' + productMinorVersion;
	text = text.replace(/^([ \t]*#define[ \t]+MyAppVersion[ \t]+\")[\d\.]+\"$/m, '$1' + version + '\"');
	version += '.' + buildNumber + '.' + revisionNumber;
	text = text.replace(/^([ \t]*#define[ \t]+MyAppVersionEx[ \t]+\")[\d\.]+\"$/m, '$1' + version + '\"');
	stream.Close();
	stream = fso.OpenTextFile(absolutePath, ForWriting, false, TristateFalse);
	stream.Write(text);
	stream.Close();
}

function createVerInfoFile(relativePath, productMajorVersion, productMinorVersion, fileMajorVersion, fileMinorVersion) {
	var absolutePath = combinePath(productPath, relativePath);
	var stream = fso.OpenTextFile(absolutePath, ForWriting, true, TristateFalse);
	stream.Write('#pragma once\n\n');
	// Build & revsion numbers
	stream.Write('#define BUILD_NUMBER\t' + buildNumber + '\n');
	stream.Write('#define REVISION_NUMBER\t' + revisionNumber + '\n\n');
	// Product version
	stream.Write('#define PRODUCT_MAJOR_VERSION\t' + productMajorVersion + '\n');
	stream.Write('#define PRODUCT_MINOR_VERSION\t' + productMinorVersion + '\n');
	var version = productMajorVersion + '.' + productMinorVersion;
	stream.Write('#define PRODUCT_SHORT_VERSION\t\"' + version + '\"\n');
	version += '.' + buildNumber + '.' + revisionNumber
	stream.Write('#define PRODUCT_LONG_VERSION\t\"' + version + '\"\n\n');
	// File version
	stream.Write('#define FILE_MAJOR_VERSION\t' + fileMajorVersion + '\n');
	stream.Write('#define FILE_MINOR_VERSION\t' + fileMinorVersion + '\n');
	version = fileMajorVersion + '.' + fileMinorVersion;
	stream.Write('#define FILE_SHORT_VERSION\t\"' + version + '\"\n');
	version += '.' + buildNumber + '.' + revisionNumber;
	stream.Write('#define FILE_LONG_VERSION\t\"' + version + '\"\n');
	stream.Close();
}

function updateSetupFiles(relativePath, productMajorVersion, productMinorVersion) {
	var absolutePath = combinePath(productPath, relativePath);
	var version = productMajorVersion + '.' + productMinorVersion + '.' + buildNumber + '.' + revisionNumber;
	// Readme file
	var readmeFilePath = combinePath(absolutePath, 'ReadMe.txt');
	var stream = fso.OpenTextFile(readmeFilePath, ForReading, false, TristateFalse);
	var text = stream.ReadAll();
	text = text.replace(/^(BugTrap for Win32 & \.NET Release )[\d\.]+$/m, '$1' + version);
	stream.Close();
	stream = fso.OpenTextFile(readmeFilePath, ForWriting, false, TristateFalse);
	stream.Write(text);
	stream.Close();
	// Setup script
	var outputPath = combinePath(absolutePath, 'Output');
	if (! fso.FolderExists(outputPath)) {
		fso.CreateFolder(outputPath);
	}
	// Date file
	var dateFilePath = combinePath(outputPath, 'date.txt');
	var stream = fso.OpenTextFile(dateFilePath, ForWriting, true, TristateFalse);
	stream.Write(date.toUTCString());
	stream.Close();
	// Version file
	var versionFilePath = combinePath(outputPath, 'version.txt');
	var stream = fso.OpenTextFile(versionFilePath, ForWriting, true, TristateFalse);
	stream.Write(version);
	stream.Close();
}

function buildSolution(solutionPath, configuration) {
	var wshShell = new ActiveXObject('WScript.Shell');
	var command = '\"%ProgramFiles%\\Microsoft Visual Studio 8\\Common7\\IDE\\devenv.exe\" ' + solutionPath + ' /build \"' + configuration + '\"';
	var wshSciptExec = wshShell.Exec(command);
	while (wshSciptExec.Status == 0) {
		WScript.Sleep(500);
	}
	if (wshSciptExec.ExitCode != 0) {
		throw (solutionPath + ' - ' + configuration + ': there were compilation errors');
	}
}

try {
	// Update RC-files
	updateRcVersion('Win32\\BugTrap\\BugTrap.rc', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion, BugTrap_FileMajorVersion, BugTrap_FileMinorVersion);
	updateRcVersion('Win32\\CrashExplorer\\CrashExplorer.rc', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion, CrashExplorer_FileMajorVersion, CrashExplorer_FileMinorVersion);
	// Update CPP-files
	updateAssemblyInfoVersion('Win32\\BugTrap\\AssemblyInfo.cpp', BugTrapServer_AssemblyMajorVersion, BugTrapServer_AssemblyMinorVersion);
	// Update CS-files
	updateAssemblyInfoVersion('Server\\BugTrapServer\\AssemblyInfo.cs', BugTrapServer_AssemblyMajorVersion, BugTrapServer_AssemblyMinorVersion);
	// Update ISS-files
	updateIssVersion('Setup\\BugTrapSetup.iss', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion);
	// Create version info files
	createVerInfoFile('Win32\\BugTrap\\VersionInfo.h', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion, BugTrap_FileMajorVersion, BugTrap_FileMinorVersion);
	createVerInfoFile('Win32\\CrashExplorer\\VersionInfo.h', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion, CrashExplorer_FileMajorVersion, CrashExplorer_FileMinorVersion);
	// Update setup files
	updateSetupFiles('Setup', BugTrap_ProductMajorVersion, BugTrap_ProductMinorVersion);
/*
	// Build BugTrap client
	buildSolution('Win32\\BugTrap\\BugTrap.sln', 'Release');
	buildSolution('Win32\\BugTrap\\BugTrap.sln', 'Unicode Release');
	buildSolution('Win32\\BugTrap\\BugTrap.sln', '.NET Release');
	// Build CrashExplorer
	buildSolution('Win32\\CrashExplorer\\CrashExplorer.sln', 'Release');
	// Build examples
	buildSolution('Win32\\Examples\\BugTrapConsoleTest\\BugTrapConsoleTest.sln', 'Release');
	buildSolution('Win32\\Examples\\BugTrapConsoleTest\\BugTrapConsoleTest.sln', 'Unicode Release');
	buildSolution('Win32\\Examples\\BugTrapTest\\BugTrapTest.sln', 'Release');
	buildSolution('Win32\\Examples\\BugTrapTest\\BugTrapTest.sln', 'Unicode Release');
	buildSolution('Win32\\Examples\\BugTrapNetTest\\BugTrapNetTest.sln', 'Release');
	// Build BugTrap server
	buildSolution('Server\\BugTrapServer\\BugTrapServer.sln', 'Release');
*/
}
catch (err) {
	HandleError(err);
}
