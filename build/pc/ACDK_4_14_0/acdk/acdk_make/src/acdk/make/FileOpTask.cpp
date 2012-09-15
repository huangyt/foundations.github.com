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


#include "FileOpTask.h"
#include "FileSet.h"
#include <acdk/io/File.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace make {

USING_CLASS(::acdk::io::, File);

//virtual 
bool 
FileOpTask::execute(IN(RString) exec, IN(RProps) props)
{
  RStringArray fl = _source->getFiles();
  for (int i = 0; i < fl->length(); ++i)
  {
    RString nsource = File(fl[i]).getCanonicalPath();  
    RString ntarget = _target;
    //System::out->println("ntarget: " + ntarget + "; nsource: " + nsource);
    RString fqsbd;
    if (_sourceBaseDir != Nil)
    {
      fqsbd = File(_sourceBaseDir).getCanonicalPath();
      fqsbd = fqsbd->convert(CCAscii);
      ntarget = ntarget + nsource->substr(fqsbd->length());
    }
    if (executeFop(exec, nsource, ntarget, props) == false)
      return false;
  }
  return true;

  //return _execute(exec, _source, _target, props);
}

/*
bool 
FileOpTask::_execute(IN(RString) exec, IN(RString) source, IN(RString) target, IN(RProps) props)
{
  if (source == Nil)
  {
    ACDK_NLOG("acdk.make", Error, "FileOpTask source must not be Nil");
    return false;
  }
  RStringArray fl = FileSet(source).getFiles();
  for (int i = 0; i < fl->length(); ++i)
  {
    RString nsource = File(fl[i]).getCanonicalPath();  
    RString ntarget = target;
    if (_sourceBaseDir != Nil)
    {
      RString fqsbd = File(_sourceBaseDir).getCanonicalPath();
      ntarget = ntarget + nsource->substr(fqsbd->length());
    }
    if (executeFop(exec, nsource, ntarget, props) == false)
      return false;
  }
  return true;
}
*/

} // namespace make
} // namespace acdk



