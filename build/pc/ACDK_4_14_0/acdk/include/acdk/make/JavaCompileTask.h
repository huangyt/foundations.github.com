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

#ifndef acdk_make_JavaCompileTask_h
#define acdk_make_JavaCompileTask_h

#include "AbstractTask.h"



namespace acdk {
namespace make {




ACDK_DECL_CLASS(JavaCompileTask);
/**
  Uses JAVA_COMPILE_MASK to create the command line
  @see gw_ref[acdk_make_tasks].
*/
class ACDK_ACDK_MAKE_PUBLIC JavaCompileTask
: extends AbstractTask
{
  ACDK_WITH_METAINFO(JavaCompileTask)
private:
   RString _source;
   RString _targetDir;
public:
  /**
    @param fq filename of java file
    @param targetdir directory to store class files. if Nil ACDKHOME/bin is used
  */
  JavaCompileTask(IN(RString) source, IN(RString) targetdir = Nil)
  : AbstractTask(source, source, "compile java sources")
  , _source(source)
  , _targetDir(targetdir)
  {
  }
  
  virtual bool execute(IN(RString) exec, IN(RProps) props);

  /// print help
  void help();
};




} // namespace make
} // namespace acdk


#endif //acdk_make_JavaCompileTask_h
