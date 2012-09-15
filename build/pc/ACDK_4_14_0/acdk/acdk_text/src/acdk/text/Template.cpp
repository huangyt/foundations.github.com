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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Template.cpp,v 1.8 2005/03/08 18:49:55 kommer Exp $




#include <acdk.h>

#include <acdk/util/Properties.h>
#include <acdk/util/ArrayList.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/util/Iterator.h>
#include <acdk/lang/String.h>
#include <acdk/io/FileReader.h>

#include "text.h"
#include "RegExp.h"
#include "Template.h"


namespace acdk {
namespace text {

using namespace acdk::lang;
using namespace acdk::util;

TemplateFilterInformation::TemplateFilterInformation(bool isPattern, IN(RString) pattern, IN(RTemplateFilter) filter)
: Object(),
  _isPattern(isPattern),
  _pattern(pattern),
  _filter(filter)
{
}

//virtual 
RString 
PropertyVarTemplateFilter::filter(IN(RString) matches)
{
  return _props->getProperty(matches, "");
}

Template::Template(RString text)
: Object(),
  _text(text),
  _filter(new acdk::util::ArrayList())
{
}  

//virtual 
void 
Template::registerTextListener(IN(RString) str, IN(RTemplateFilter) filter)
{
  _filter->add(new TemplateFilterInformation(false, str, filter));
}

//virtual 
void 
Template::registerPatternListener(IN(RString) pattern, IN(RTemplateFilter) filter)
{
  _filter->add(new TemplateFilterInformation(true, pattern, filter));
}

//virtual 
RString 
Template::filter(int startoffset/* = 0*/, int endoffset/* = 0*/)
{
  RIterator it = _filter->iterator();
  StringBuffer sb(_text);
  while (it->hasNext() == true) 
  {
    RTemplateFilterInformation fi = (RTemplateFilterInformation)it->next();
    if (fi->isPattern() == true) 
    {
      RegExp regexp(fi->pattern());
      int startpos = 0;
      while (startpos < sb.length()) 
      {
        RString cstr = sb.substring(startpos);
        RRegExpMatchPositionArray erg = regexp.matchPos(cstr);
        if (erg->length() == 0)
          break;
        RString token = cstr->substring(erg[1]->start, erg[1]->end);
        RString str = fi->filter()->filter(token);
        sb.replace(startpos + erg[1]->start - startoffset, startpos + erg[1]->end + startoffset, str);
        startpos += erg[1]->start;
        startpos += str->length();
      }
    } 
    else 
    {
      int startidx = 0;
      while (true) 
      {
        int fidx = sb.toString()->indexOf(fi->pattern(), startidx);
        if (fidx == -1)
          break;
        sb.replace(fidx, fidx + fi->pattern()->length(), fi->filter()->filter(fi->pattern()));
      }
    }
  }
  return sb.toString();
}

/*
ACDK_DECL_INTERFACE(DummyInterface);

class DummyInterface
{
public:
  virtual void foo() = 0;
};

ACDK_DECL_CLASS(DummyTemplateFilter);

class DummyTemplateFilter
: extends acdk::lang::Object,
  //implements TemplateFilter,
  implements DummyInterface
{
  //virtual RString filter(RString text) { return text; }
  //virtual RString filter(RStringArray matches) { return ""; }
public:
  virtual void foo() { }
};

static void __foo()
{
  acdk::io::RFileReader fr = new ::acdk::io::FileReader(RString("asdf"));
  acdk::io::RReader r = fr;

  ::acdk::text::RDummyTemplateFilter f = new ::acdk::text::DummyTemplateFilter();
  ::acdk::text::RDummyInterface di = f;
  //RTemplateFilter tf = f;
  //    f.iptr();
  //RTemplateFilterInformation tif = new TemplateFilterInformation(false, "", (RTemplateFilter)f);
}
*/
} // namespace text 
} // namespace acdk 


