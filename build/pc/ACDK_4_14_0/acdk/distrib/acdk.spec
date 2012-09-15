
%define name acdk
%define version 4.14
%define release 0
%define __prefix /var/acdk

Name: %{name}
Summary: C++ framework
Version: %{version}
Release: %{release}
License: LGPL
Group: Development/Libraries
Source: %{name}-%{version}.%{release}-src.tar.gz
BuildRoot: %{_tmppath}/build-root-%{name}
Packager: Roger Rene Kommer
Prefix: %{__prefix}
Vendor: Roger Rene Kommer, Artefaktur

%description
ACDK is designed to serve as a backend technology, with a similar target of 
Microsoft's .NET or Sun's ONE platform, but instead of using Basic/C# or Java
as programming language, it uses C++ as core implementation language.

ACDK implements the standard library packages, including acdk::lang, 
acdk::lang::reflect, acdk::util, acdk::io, acdk::text (including regexpr), 
acdk::net, acdk::sql, acdk::xml and more. Todays technoligies like flexible 
Allocator/Garbage Collection, Threading and Unicode are implemented in the 
core of ACDK.

With the extensions of ACDK C++ objects are available for reflection, 
serialization, aspect oriented class attributes and [D]ynamic [M]ethod 
[I]nvocation. This DMI act as an universal object oriented call interface 
to connect C++ with scripting languages (Java, Perl, Tcl, Python, Lisp, 
Visual Basic, VBScript) and standard component technologies (CORBA, COM+).

%prep
rm -rf $RPM_BUILD_ROOT 
mkdir $RPM_BUILD_ROOT

%setup -q

%build
rm -rf $RPM_BUILD_ROOT
make src_pkg_compile

%install
set -x

perl ./distrib/backup.pl \
	--exclcvs --distignore \
	-dCVS -dtobj \
	--name acdk-src-pkg .


#fi
mkdir -p $RPM_BUILD_ROOT%{__prefix}
mv acdk-src-pkg.tgz $RPM_BUILD_ROOT%{__prefix}
cd $RPM_BUILD_ROOT%{__prefix}
tar zxf acdk-src-pkg.tgz
rm acdk-src-pkg.tgz

#make binpkg_filelist
#-f acdk-pkg-filelist.lst
%files 
%{__prefix}/

%clean
rm -rf $RPM_BUILD_ROOT


