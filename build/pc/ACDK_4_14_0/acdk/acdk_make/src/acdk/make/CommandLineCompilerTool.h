// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 

#ifndef acdk_make_CommandLineCompilerTool_h
#define acdk_make_CommandLineCompilerTool_h

#include "Tool.h"

namespace acdk {
namespace make {


ACDK_DECL_INTERFACE(ToolConfigurator);

class ACDK_ACDK_MAKE_PUBLIC ToolConfigurator
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ToolConfigurator)
public:
  virtual bool configure(IN(RString) fqToolCmd, IN(RProps) env) = 0;
};

ACDK_DECL_INTERFACE(DependencyChecker);

class ACDK_ACDK_MAKE_PUBLIC DependencyChecker
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(DependencyChecker)
public:
  virtual bool rebuild(IN(RString) sourceFile, IN(RString) targetFile, IN(RProps) env) = 0;
};

ACDK_DECL_CLASS(SimpleFileDepencyChecker);

class ACDK_ACDK_MAKE_PUBLIC SimpleFileDepencyChecker
: extends acdk::lang::Object
, implements DependencyChecker
{
  ACDK_WITH_METAINFO(SimpleFileDepencyChecker)
public:
  SimpleFileDepencyChecker() {}
  bool rebuild(IN(RString) sourceFile, IN(RString) targetFile, IN(RProps) env);
};

ACDK_DECL_INTERFACE(CommandLineOutputParser);

class ACDK_ACDK_MAKE_PUBLIC CommandLineOutputParser
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(CommandLineOutputParser)
public:
  virtual bool parse(IN(RString) line, OUT(RString) file, OUT(int) lineNo, OUT(RString) message, OUT(int) logLevel) = 0;
  virtual RString format(IN(RString) file, int lineNo, IN(RString) message, int logLevel) = 0;
};



ACDK_DECL_CLASS(CommandLineCompilerTool);

/**
  a command line based tool
*/
class ACDK_ACDK_MAKE_PUBLIC CommandLineCompilerTool
: extends AbstractTool
{
 ACDK_WITH_METAINFO(CommandLineCompilerTool)
private:
  /**
    Expects following properties:
    TOOL_BASENAME: base name of the command line utility. After the tool is configured the TOOL_FQNAME is set.
    TOOL_MASK: Mask, which evaluate the command line.
  */
  RProps _toolProps;
  bool _configured;  
  bool _toolFound;
public:
  RToolConfigurator _configurator;
  RDependencyChecker _depChecker;
  RCommandLineOutputParser _outputParser;
public:
  CommandLineCompilerTool(IN(RString) toolClass, IN(RString) toolTribe, IN(RProps) toolProps)
  : AbstractTool(toolClass, toolTribe)
  , _toolProps(toolProps)
  , _configured(false)
  , _toolFound(false)
  {
  }
  virtual bool configure(IN(RProps) env);
  bool execute(IN(RString) exec, IN(RProps) props);
  RProps getToolProps() { return _toolProps; }
  void setDependencyChecker(IN(RDependencyChecker) depChecker)
  {
    _depChecker = depChecker;
  }
  void setCommandLineOutputParser(IN(RCommandLineOutputParser) parser)
  {
    _outputParser = parser;
  }
};

} // namespace make
} // namespace acdk 
  
#endif //acdk_make_CommandLineCompilerTool_h
