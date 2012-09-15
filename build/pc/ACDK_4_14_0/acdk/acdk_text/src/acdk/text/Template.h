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
// $Header: /cvsroot/acdk/acdk/acdk_text/src/acdk/text/Template.h,v 1.8 2005/02/05 10:45:33 kommer Exp $

#ifndef acdk_text_Template_h
#define acdk_text_Template_h

#include <acdk_all.h>

#include <acdk/util/Properties.h>
#include <acdk/util/ArrayList.h>

namespace acdk {
namespace text {
  
using namespace acdk::lang;


ACDK_DECL_INTERFACE(TemplateFilter);
/** 
  Callbackroutine, which should filter the templates
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:33 $
  @bug Incomplete
*/
class ACDK_TEXT_PUBLIC TemplateFilter
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(TemplateFilter)
public:
  /**
    used as Callback function from Template.
    @param text the text, which was registered with Template::registerTextListener()
    @return the filtered text
  */
  virtual RString filter(IN(RString) text) = 0;
  /**
    used as Callback function from Template.
    @param text the text, which was registered with Template::registerPatternListener()
    @return the filtered text
  */
  virtual RString filter(IN(RStringArray) matches) = 0;
};

ACDK_DECL_CLASS(PropertyVarTemplateFilter);
/** 
  An filter, which replaces $VarName$ with value in an Properies
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:33 $
  @bug Incomplete
*/
class ACDK_TEXT_PUBLIC PropertyVarTemplateFilter
: extends acdk::lang::Object,
  implements acdk::text::TemplateFilter
{
  ACDK_WITH_METAINFO(PropertyVarTemplateFilter)
protected:
  acdk::util::RProperties _props;
public:
  PropertyVarTemplateFilter(IN(acdk::util::RProperties) props)
  : Object(),
    _props(props)
  {
  }
  acdk::util::RProperties properties() { return _props; }
  /**
    used as Callback function from Template.
    @param text the text, which was registered with Template::registerTextListener()
    @return the filtered text
  */
  virtual RString filter(IN(RString) text);
  /**
    used as Callback function from Template.
    @param text the text, which was registered with Template::registerPatternListener()
    @return the filtered text
  */
  virtual RString filter(IN(RStringArray) matches) { return filter(matches[1]); }
};




ACDK_DECL_CLASS(TemplateFilterInformation);
/** 
  Internal structur of Template
*/
class ACDK_TEXT_PUBLIC TemplateFilterInformation
: public ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TemplateFilterInformation)
protected:
  bool _isPattern;
  RString _pattern;
  RTemplateFilter _filter;
public:
  TemplateFilterInformation(bool isPattern, IN(RString) pattern, IN(RTemplateFilter) filter);
  bool isPattern() { return _isPattern; }
  RString pattern() { return _pattern; }
  RTemplateFilter filter() { return _filter; }
};

ACDK_DECL_CLASS(Template);
/** 
  Template functionality for generating documents
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:33 $
  @bug Incomplete
*/
class ACDK_TEXT_PUBLIC Template
: public ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Template)
protected:
  RString _text;
  acdk::util::RArrayList _filter; // contains RTemplateFilterInformation
public:
  /**
    Constructor
    @param text the template text, which should be filtered
  */
  Template(RString text);
  virtual void registerTextListener(IN(RString) str, IN(RTemplateFilter) filter);
  virtual void registerPatternListener(IN(RString) pattern, IN(RTemplateFilter) filter);
  /**
    Regexp reg("ab(.*)cdef");
    Fiter will return "XYZ";
    filter(2, 3), will call filter with matching (.*)-Expression, an will
    ignore "ab" at start and "cde" an end in replace
    Text = "12ab_cdef34":
    Template::filter returns "12XYZf34"
    @arg startoffset the offset, into the matching, which will skiped in replacement
    @arg endoffset the offset, into the matching, which will skiped in replacement
    @return the filtered text
  */
  virtual RString filter(int startoffset = 0, int endoffset = 0);
};


} // namespace text 
} // namespace acdk 

#endif //acdk_text_Template_h

