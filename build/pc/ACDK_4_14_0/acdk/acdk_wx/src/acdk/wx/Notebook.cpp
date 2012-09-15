// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Notebook.cpp,v 1.6 2005/02/05 10:45:35 kommer Exp $


#include "Notebook.h"
#include "ToggleButton.h"

namespace acdk {
namespace wx {


ACDK_DEFINE_WX_EVENT(NotebookEvent, EvtCommandNotebookPageChanging, wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING);
ACDK_DEFINE_WX_EVENT(NotebookEvent, EvtCommandNotebookPageChanged, wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED);



} // wx
} // acdk

