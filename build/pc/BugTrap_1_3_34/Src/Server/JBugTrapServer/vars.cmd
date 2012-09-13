@echo off

set JAVAHOME=%ProgramFiles%\Java
set JDK=jdk1.6.0_10
set JMAIL=javamail-1.4.1
set JAF=jaf-1.1.1
set PGUARD=proguard4.2
set JAVABIN=%JAVAHOME%\%JDK%\bin
set PGUARGBIN=%JAVAHOME%\%PGUARD%\lib
set CLSPATH=".;%JAVAHOME%\%JDK%\jre\lib\rt.jar;%JAVAHOME%\%JMAIL%\mail.jar;%JAVAHOME%\%JAF%\activation.jar"
