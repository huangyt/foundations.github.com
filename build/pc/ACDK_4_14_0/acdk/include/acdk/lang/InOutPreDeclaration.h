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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/InOutPreDeclaration.h,v 1.5 2005/02/05 10:44:55 kommer Exp $

#ifndef acdk_lang_InOutPreDeclaration_h
#define acdk_lang_InOutPreDeclaration_h


namespace acdk {
  namespace io {
    ACDK_DECL_INTERFACE(Serializable);
    ACDK_DECL_INTERFACE(Reader);
    ACDK_DECL_INTERFACE(Writer);
    
    ACDK_DECL_CLASS(AbstractReader);
    ACDK_DECL_CLASS(AbstractWriter);

    
    ACDK_DECL_INTERFACE(FilterWriter);
    ACDK_DECL_INTERFACE(FilterReader);

    ACDK_DECL_CLASS(AbstractFilterReader);
    ACDK_DECL_CLASS(AbstractFilterWriter);

    ACDK_DECL_CLASS(InputReader);
    ACDK_DECL_CLASS(PrintWriter);
    ACDK_DECL_CLASS(FileDescriptor);
    ACDK_DECL_CLASS(FileReader);
    ACDK_DECL_CLASS(FileWriter);
  } // namespace io
} // namespace acdk

#endif //acdk_lang_InOutPreDeclaration_h

