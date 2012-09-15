// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/MimeTypeMapper.cpp,v 1.9 2004/11/21 21:56:36 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include "MimeTypeMapper.h"

namespace acdk {
namespace net {
// public:
//  static 

using namespace acdk::lang;
using namespace acdk::util;



//  static 
char* MimeTypeMapper::mime_strings[][2] = 
{
  { "application/mac-binhex40", "hqx" },
  { "application/mac-compactpro", "cpt" },
  { "application/msword", "doc" },
  { "application/octet-stream", "bin" },
  { "application/octet-stream", "dms" },
  { "application/octet-stream", "lha" },
  { "application/octet-stream", "lzh" },
  { "application/octet-stream", "exe" },
  { "application/octet-stream", "class" },
  { "application/oda", "oda" },
  { "application/pdf", "pdf" },
  { "application/postscript", "ai" },
  { "application/postscript", "eps" },
  { "application/postscript", "ps" },
  { "application/powerpoint", "ppt" },
  { "application/rtf", "rtf" },
  { "application/x-bcpio", "bcpio" },
  { "application/x-cdlink", "vcd" },
  { "application/x-compress", "Z" },
  { "application/x-cpio", "cpio" },
  { "application/x-csh", "csh" },
  { "application/x-director", "dcr" },
  { "application/x-director", "dir" },
  { "application/x-director", "dxr" },
  { "application/x-dvi", "dvi" },
  { "application/x-gtar", "gtar" },
  { "application/x-gzip", "gz" },
  { "application/x-hdf", "hdf" },
  { "application/x-httpd-cgi", "cgi" },
  { "application/x-koan", "skp" },
  { "application/x-koan", "skd" },
  { "application/x-koan", "skt" },
  { "application/x-koan", "skm" },
  { "application/x-latex", "latex" },
  { "application/x-mif", "mif" },
  { "application/x-netcdf", "nc" },
  { "application/x-netcdf", "cdf" },
  { "application/x-sh", "sh" },
  { "application/x-shar", "shar" },
  { "application/x-stuffit", "sit" },
  { "application/x-sv4cpio", "sv4cpio" },
  { "application/x-sv4crc", "sv4crc" },
  { "application/x-tar", "tar" },
  { "application/x-tcl", "tcl" },
  { "application/x-tex", "tex" },
  { "application/x-texinfo", "texinfo" },
  { "application/x-texinfo", "texi" },
  { "application/x-troff", "t" },
  { "application/x-troff", "tr" },
  { "application/x-troff", "roff" },
  { "application/x-troff-man", "man" },
  { "application/x-troff-me", "me" },
  { "application/x-troff-ms", "ms" },
  { "application/x-ustar", "ustar" },
  { "application/x-wais-source", "src" },
  { "application/zip", "zip" },
  { "audio/basic", "au" },
  { "audio/basic", "snd" },
  { "audio/mpeg", "mpga" },
  { "audio/mpeg", "mp2" },
  { "audio/mpeg", "mp3" },
  { "audio/x-aiff", "aif" },
  { "audio/x-aiff", "aiff" },
  { "audio/x-aiff", "aifc" },
  { "audio/x-pn-realaudio", "ram" },
  { "audio/x-pn-realaudio-plugin", "rpm" },
  { "audio/x-realaudio", "ra" },
  { "audio/x-wav", "wav" },
  { "chemical/x-pdb", "pdb" },
  { "chemical/x-pdb", "xyz" },
  { "image/gif", "gif" },
  { "image/ief", "ief" },
  { "image/jpeg", "jpeg" },
  { "image/jpeg", "jpg" },
  { "image/jpeg", "jpe" },
  { "image/png", "png" },
  { "image/tiff", "tiff" },
  { "image/tiff", "tif" },
  { "image/x-cmu-raster", "ras" },
  { "image/x-portable-anymap", "pnm" },
  { "image/x-portable-bitmap", "pbm" },
  { "image/x-portable-graymap", "pgm" },
  { "image/x-portable-pixmap", "ppm" },
  { "image/x-rgb", "rgb" },
  { "image/x-xbitmap", "xbm" },
  { "image/x-xpixmap", "xpm" },
  { "image/x-xwindowdump", "xwd" },
  { "text/html", "html" },
  { "text/html", "htm" },
  { "text/plain", "txt" },
  { "text/richtext", "rtx" },
  { "text/tab-separated-values", "tsv" },
  { "text/x-setext", "etx" },
  { "text/x-sgml", "sgml" },
  { "text/x-sgml", "sgm" },
  { "video/mpeg", "mpeg" },
  { "video/mpeg", "mpg" },
  { "video/mpeg", "mpe" },
  { "video/quicktime", "qt" },
  { "video/quicktime", "mov" },
  { "video/x-msvideo", "avi" },
  { "video/x-sgi-movie", "movie" },
  { "x-conference/x-cooltalk", "ice" },
  { "x-world/x-vrml", "wrl" },
  { "x-world/x-vrml", "vrml" },
  { 0, 0}
};

// private:
//  static 
RHashMap MimeTypeMapper::__mime_types;

//static 
RHashMap 
MimeTypeMapper::mime_types()
{
  if (__mime_types != Nil)
    return __mime_types;
  int i=0;
  __mime_types = new HashMap(150);
  while (mime_strings[i][0] != 0) {
    __mime_types->put((RObject)(RString)mime_strings[i][1], (RObject)(RString)mime_strings[i][0]);
    i++;
  }
  System::registerStaticReference(__mime_types);
  return __mime_types;
}

//  virtual 
RString
MimeTypeMapper::getContentTypeFor(IN(RString) fname)
{
  RString filename = fname;
  int index = filename->lastIndexOf(".");
  if (index != -1)
    {
      if (index == filename->length())
        return "application/octet-stream";
      else
        filename = filename->substring(index + 1);
    }

  RString type = (RString)mime_types()->get((RObject)filename);
  if (type == Nil)
    return "application/octet-stream";
  return type;
}


} // namespace acdk
} // namespace net
