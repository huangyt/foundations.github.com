// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_doc_urlify/src/acdk/doc/urlify/HTMLStreamTokenizer.h,v 1.4 2003/06/19 14:37:18 kommer Exp $
//
// $Log: HTMLStreamTokenizer.h,v $
// Revision 1.4  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.3  2003/06/19 13:17:24  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.2.2.1  2003/05/13 13:47:23  kommer
// panta rei
//
// Revision 1.2  2001/12/09 02:40:10  kommer
// introduced IN() for method parameter
//
// Revision 1.1  2001/03/03 20:11:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:20  roger
// initial acdk sources
//
// Revision 1.2  2000/06/03 18:48:32  roger
// panta rei
//
// Revision 1.1  2000/04/25 11:57:33  roger
// initial revision
//
// Revision 1.4  2000/02/14 22:09:33  roger
// new ACDK_DECL_CLASS
//
// Revision 1.3  2000/02/08 16:29:39  roger
// RefHolder and Arrays changed
//
// Revision 1.2  1999/10/24 11:57:02  roger
// panta rei
//
// Revision 1.1  1999/10/22 19:04:12  roger
// initial revision
//

#ifndef acdk_io_HTMLStreamTokenizer_h
#define acdk_io_HTMLStreamTokenizer_h

#include <acdk.h>
#include <acdk/io/StreamTokenizer.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

ACDK_DECL_CLASS(HTMLStreamTokenizer);

class HTMLStreamTokenizer
: public StreamTokenizer
{
    
 
  
public:
  HTMLStreamTokenizer(IN(RCharReader) reader)
  : StreamTokenizer(reader)
  {
  }
  
  static const int TT_HTMLCOMMENT;
  static const int TT_TAG_UNKNOWN;
  static const int TT_TAG_UNKNOWN_END;
  static const int TT_TAG_COMMENT;
  static const int TT_TAG_COMMENT_END;
  static const int TT_TAG_A;
  static const int TT_TAG_A_END;
  static const int TT_TAG_H;
  static const int TT_TAG_H_END;

  virtual int nextToken();
protected:
  virtual int _readTag();
};  


} // IO
} // MM


#endif //acdk_io_HTMLStreamTokenizer_h
