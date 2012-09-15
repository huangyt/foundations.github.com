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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/AwtListener.h,v 1.5 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_event_ActionListener_h
#define acdk_java_awt_event_ActionListener_h

#include <acdk.h>
#include <acdk/java/Config.h>
#include <acdk/java/JavaObject.h>
namespace acdk {
namespace java {
namespace awt {
namespace event {

ACDK_DECL_INTERFACE(ActionListener);
class ACDK_ACDK_JAVA_PUBLIC ActionListener
      ACDK_INTERFACEBASE
{
public:
  virtual void actionPerformed(IN(::acdk::java::RJavaObject) event) = 0;
};

} // namespace event 
} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_ActionListener_h
