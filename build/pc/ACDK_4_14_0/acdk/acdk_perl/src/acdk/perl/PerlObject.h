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
// $Header: /cvsroot/acdk/acdk/acdk_perl/src/acdk/perl/PerlObject.h,v 1.6 2005/04/08 10:53:20 kommer Exp $


#ifndef acdk_perl_PerlObject_h
#define acdk_perl_PerlObject_h

#include <acdk.h>
#include "Config.h"



namespace acdk {
/**
  Implements the Perl interpreter and wrapper Classes.
*/ 
namespace perl {

/** 
  Representing a PerlObject.
  Note: Object is NOT derived by acdk::lang::Object, because it's lifetime 
  is managened by the perl interpreter
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/04/08 10:53:20 $
*/

class ACDK_ACDK_PERL_PUBLIC PerlObject
{
protected:
  /** the underlying object */
  RObject _object;
public:
  PerlObject();
  virtual ~PerlObject();
};
  
} // namespace perl 
} // namespace acdk

#endif // acdk_perl_PerlObject_h
