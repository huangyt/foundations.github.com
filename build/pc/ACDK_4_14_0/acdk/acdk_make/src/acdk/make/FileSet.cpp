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


#include "FileSet.h"
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/util/StringTokenizer.h>

namespace acdk {
namespace make {

USING_CLASS(::acdk::io::, File);

bool 
FileSet::isPattern(IN(RString) spec)
{
  return spec->indexOf('*') != -1 || spec->indexOf('?')  != -1;
}

void
FileSet::collectFiles(IN(RFile) dir, 
                       IN(acdk::io::RFilenameFilter) filter, 
                       IN(RStringArray) flist)
{
  RStringArray sf = dir->list();
  for (int i = 0; i < sf->length(); ++i)
  {
    File sfile(dir, sf[i]);
    if (sfile.isDirectory() == true)
      collectFiles(&sfile, filter,flist);
    else 
    {
      if (filter->accept(dir, sf[i]) == true)
        flist->append(sfile.getCanonicalPath());
    }
  }
}

void
FileSet::expandPattern(IN(RString) pattern, IN(RStringArray) erg)
{
  /**
    dir / ** / file pattern
  */
  int fidx;
  if (pattern->startsWith("**/") == true)
  {
    File startdir(".");
    RString filepattern = pattern->substr(3);
    ::acdk::io::GlobFilenameFilter filter(filepattern);
    collectFiles(&startdir, &filter, erg);
    return;
  } 
  else if ((fidx = pattern->indexOf("/**/")) != -1)
  {
    RString commondir = pattern->substr(0, fidx);
    File startdir(commondir);
    RString filepattern = pattern->substr(fidx + 4);
    ::acdk::io::GlobFilenameFilter filter(filepattern);
    collectFiles(&startdir, &filter, erg);
    return;
  } 
  else
  {
    ::acdk::io::File pf(pattern);
    RFile dir = pf.getParentFile();
    RString fpattern = pf.getName();
    ::acdk::io::GlobFilenameFilter filter(fpattern);
    RStringArray sf = dir->list();
    for (int i = 0; sf != Nil && i < sf->length(); ++i)
    {
      File sfile(dir, sf[i]);
      if (sfile.isFile() == true)
        if (filter.accept(dir, sf[i]) == true)
          erg->append(sfile.getCanonicalPath());
    }
    return;
  }
}


void 
FileSet::expand(IN(RString) spec, IN(RStringArray) erg)
{
  if (spec->indexOf(";") != -1)
  {
    RStringArray fl = ::acdk::util::StringTokenizer(spec, ";", false).allToken();
    for (int i = 0; i < fl->length(); ++i)
    {
      expand(fl[i], erg);
    }
    return;
  }
  if (isPattern(spec) == true)
  {
    expandPattern(spec, erg);
    return;
  }
  erg->append(spec);
}

RStringArray 
FileSet::getFiles()
{
  RStringArray erg = new StringArray(0);
  for (int i = 0; i < _fileSpecs->length(); ++i)
  {
    expand(_fileSpecs[i], erg);
  }
  return erg;
}
//static 
RFileSet 
FileSet::createFileSpecs(IN(RStringArray) filesOrDirs, IN(RStringArray) filematchpattern, bool alwaysAddFiles)
{
  RFileSet fs = new FileSet();
  for (int i = 0; i < filesOrDirs->length();  ++i)
  {
    RString f =  filesOrDirs[i];
    if (File(f).isDirectory() == true)
    {
      for (int j = 0; j < filematchpattern->length(); ++j)
      {
        fs->addSpec(f + "/" + filematchpattern[j]);
      }
    }
    else
    {
      if (alwaysAddFiles == true)
        fs->addSpec(f);
      else
      {
        File file(f);
        RFile pfile = file.getParentFile();
        RString name = file.getName();
        for (int j = 0; j < filematchpattern->length(); ++j)
        {
          RString p = filematchpattern[j];
          ::acdk::io::GlobFilenameFilter gff(p);
          if (gff.accept(pfile, name) == true)
          {
            fs->addSpec(f);
            break;
          }
        }
      }
    }
  }
  return fs;
}

} // namespace make
} // namespace acdk





