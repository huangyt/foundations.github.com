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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/acdk_util_classes.h,v 1.5 2005/02/05 10:45:07 kommer Exp $

#ifndef acdk_util_classes_h
#define acdk_util_classes_h


namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Collection);
ACDK_DECL_INTERFACE(Set);
ACDK_DECL_INTERFACE(Map);
ACDK_DECL_INTERFACE(Iterator);

ACDK_DECL_INTERFACE(List);



ACDK_DECL_CLASS(AbstractCollection);

ACDK_DECL_CLASS(HashSet);
ACDK_DECL_CLASS(HashMap);

ACDK_DECL_CLASS(AbstractList);


ACDK_DECL_CLASS(Vector);

ACDK_DECL_CLASS(ArrayList);

} // util
} // acdk

#endif //acdk_util_classes_h

