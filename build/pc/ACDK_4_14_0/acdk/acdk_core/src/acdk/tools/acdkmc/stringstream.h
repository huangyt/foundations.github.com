// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/stringstream.h,v 1.2 2003/06/19 14:37:16 kommer Exp $
//
// $Log: stringstream.h,v $
// Revision 1.2  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.1  2001/12/02 13:47:16  kommer
// initial revision
//
// Revision 1.1.1.1  2000/12/11 18:05:18  kommer
// ACDK Free edition
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.1  2000/04/14 09:09:38  roger
// *** empty log message ***
//
// Revision 1.3  1999/10/21 18:04:32  roger
// Copyright notice updated
//
// Revision 1.2  1999/10/04 08:10:26  roger
// renamed M\MLib => ACDK
//
// Revision 1.1  1999/08/12 19:07:39  roger
// for gnuc-compatibility
//
// Revision 1.2  1999/08/12 13:52:21  kai
// Dos2Unix Konvertierung.
//
// Revision 1.1.1.1  1999/08/05 11:15:01  roger
// Inital unstable snapshot
//

/*
Some compiler doesn't have new 
<sstream>
emulating it, with old 
strstream.h
*/


#ifndef stringstream_h
#define stringstream_h

#ifdef __GNUG__

#include <strstream.h>
#include <string>

namespace std {

class stringstream 
  : public strstream 
{
public :
  stringstream() : strstream() { }
  ~stringstream() 
  {
    rdbuf()->freeze(false);
  }
  inline void unsureTerminated() const 
  {
    char* ptr = rdbuf()->str();
    if (ptr[strstream::pcount() - 1] != '\0') {
      rdbuf()->freeze(false);
      rdbuf()->sputc(0);
    } else
      rdbuf()->freeze(false);
  }
  inline const char* c_str() const 
  {
    unsureTerminated();
    char* ptr = rdbuf()->str();
    rdbuf()->freeze(false);
    return ptr;
  }
  inline const char* data() const 
  {
    const char *ptr = rdbuf()->str();
    rdbuf()->freeze(false);
    return ptr;
  }
  inline string str() const 
  {
    return string(data(), strstream::pcount());
  }
  stringstream& operator<<(const string& str)
  {
    this->operator<<(str.c_str());
    return *this;
  }
  stringstream& operator<<(double d)
  {
    ostream::operator<<(d);
    return *this;
  }
};

} //std

#endif //__GNUG__

#endif //stringstream_h
