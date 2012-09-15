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

#include "CppSourceDependTask.h"
#include "ChDir.h"
#include <acdk/io/File.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/lang/System.h>
#include <acdk/util/SysDate.h>
#include <acdk/io/ByteToCharReader.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace make {

//#define LOCAL_DEBUG
#if defined(LOCAL_DEBUG)

RString spaces(int count)
{
  StringBuffer sb(20);
  for (int i = 0; i < count; ++i)
    sb.append(" ");
  return sb.toString();
}

#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr) do { } while(false)
#endif


bool 
CppSourceDependTask::noSourceDeps = false;
bool 
CppSourceDependTask::onlyDirectIncludes = false;
bool 
CppSourceDependTask::recursiveDeps = true;

bool 
CppSourceDependTask::useCache = true;

void 
CppSourceDependTask::addIncludeDir(IN(RString) str)
{
  _includeDirs->append(str);
}

USING_CLASS(acdk::io::, StreamTokenizer);
USING_CLASS(acdk::io::, File);

struct DepCacheEntry
{
  RString filename;
  jlong modtime;
  jlong minmodtime;
  
  DepCacheEntry(IN(RString) fname, jlong mt)
  : filename(fname)
  , modtime(mt)
  , minmodtime(mt)
  {
  }
  
  void setMinLastModified(jlong mt)
  {
    if (minmodtime < mt)
    {
      DOUT("    MinTime: " << filename);
      minmodtime = mt;
    }
  }
};

struct DepCache
{
  ::acdk::lang::sys::core_vector<DepCacheEntry> _deps;
  ::acdk::lang::sys::core_mutex _mutex;
  typedef ::acdk::lang::sys::core_lock_guard< ::acdk::lang::sys::core_mutex> LockGuard;
  void add(const DepCacheEntry& de)
  {
    LockGuard _lock(_mutex);
    _deps.push_back(de);
  }
  DepCacheEntry find(IN(RString) filename)
  {
    LockGuard _lock(_mutex);
    for (int i = 0; i < _deps.size(); ++i)
    {
      if (_deps[i].filename->equals(filename) == true)
        return _deps[i];
    }
    return DepCacheEntry(Nil, 0);
  }
};

DepCache _depCache;

struct DepCacheStack
{
  ::acdk::lang::sys::core_vector<DepCacheEntry> _stack;
  typedef ::acdk::lang::sys::core_vector<DepCacheEntry>::iterator iterator;
  void push(const DepCacheEntry& dce)
  {
    _stack.push_back(dce);
  }
  void setMinTime()
  {
    
    jlong mintime = _stack.back().minmodtime;
    if (mintime != _stack.back().modtime)
      return;
    DOUT("Set MinTime: " << _stack.back().filename << " " << acdk::util::SysDate(mintime).toString());
    iterator it = _stack.begin();
    iterator end = _stack.end();
    for (; it < end; ++it)
    {
      it->setMinLastModified(mintime);
    }
  }
  DepCacheEntry& top()
  {
    return _stack.back();
  }
  DepCacheEntry pop()
  {
    DepCacheEntry ret = top();
    _stack.erase(_stack.end() - 1);
    return ret;
  }
};

struct DepCacheStackHolder
{
  DepCacheStack& _stack;
  DepCacheStackHolder(DepCacheStack& stack, IN(RString) filename)
  : _stack(stack)
  {
   acdk::io::FileStatus ss(filename);
   _stack.push(DepCacheEntry(filename, ss.lastModified()));
  }
  ~DepCacheStackHolder()
  {
    _stack.setMinTime();
    _depCache.add(_stack.pop());
  }
};

RString readInclude(StreamTokenizer& tin)
{
  StringBuffer sb("");
  int tk;
  while ((tk = tin.nextToken()) != StreamTokenizer::TT_EOF)
  {
    if (tk == StreamTokenizer::TT_WORD)
      sb.append(tin.sval);
    else if (tk == '/' || tk == '.')
      sb.append((char)tk);
    else if (tk == '>')
      return sb.toString();
    else
    {
      ACDK_NLOG("acdk.make", Warn, tin.getStreamPos() + ": Unexpected in #include < read_content >" + tin.lastReaded());  
      return Nil;
    }
  }
  return sb.toString();
}

bool 
CppSourceDependTask::parseFile(IN(RString) filename, jlong ts, IN(RStringArray) parsedFiles, IN(RString) cwd, DepCacheStack& dcs)
{
  
  File file(filename);
  if (file.exists() == false)
  {
    ACDK_NLOG("acdk.make", Note, "File doesn't exists: " + filename);
    return true;
  }
  RString fqfile = file.getCanonicalPath();
  if (useCache == false)
  {
    acdk::io::FileStatus ss(fqfile);
    if (ts - ss.lastModified() < 0)
    {
      ACDK_NLOG("acdk.make", Trace, 
          "Source is younger: " + filename +
          " Target: " + acdk::util::SysDate(ts).toString() +
          " Source: " + acdk::util::SysDate(ss.lastModified()).toString()
          );  
      return false;
    }
  }
  

  if (::acdk::util::Arrays::sequenceSearch(parsedFiles, fqfile) != -1)
    return true;
  if (useCache == true)
  {
    DepCacheEntry de = _depCache.find(fqfile);
    if (de.filename != Nil)
    {
      DOUT("DepCacheHit: " << fqfile);
      if (de.minmodtime > ts)
        return false;
      return true;
    }
  }
  ACDK_NLOG("acdk.make", Trace, "CppSourceDependTask: Parse File: " + fqfile);
  DOUT("ParseFile: " << fqfile);
  parsedFiles->append(fqfile);
  DepCacheStackHolder _dcsh(dcs, fqfile);

  acdk::io::FileReader fin(filename);
  acdk::io::ByteToCharReader chin(&fin, acdk::locale::Encoding::getEncoding("LATIN-1")->getDecoder());
  //ChDir cdir(file.getParent());
  RString ncwd = file.getParent();
  StreamTokenizer tin(&chin);
  int tk = 0;
  bool bret = true;
  while (true)
  {
    tk = tin.nextToken();
from_begining:
    if (tk == StreamTokenizer::TT_EOF)
      return true;

    if (tk == '#')
    {
      tk = tin.nextToken();
      if (tk == StreamTokenizer::TT_WORD && tin.sval->equals("include") == true)
      {
        tk = tin.nextToken();
        if (tk == '<') 
        {
          if (_checkOnlyDirectIncludes == true)
          {
            tin.skipLine();
            continue;
          }
          RString sfile = readInclude(tin);
          RString ffile;
          RString fdir;
          if (sfile != Nil)
          {
            File tf(ncwd, sfile);
            if (tf.exists() == true)
            {
              ffile = tf.getCanonicalPath();
              fdir = tf.getParent();
            } 
            else
            {
              for (int i = 0; i < _includeDirs->length(); ++i)
              {
                File f(_includeDirs[i], sfile);
                if (f.exists() == true)
                {
                  ffile = f.getCanonicalPath();
                  fdir = f.getParent();
                  break;
                }
              }
            }
            if (ffile != Nil)
            {
              if (File(ffile).exists() == false)
              {
                ACDK_NLOG("acdk.make", Trace, "in file " + filename + " included <File> doesn't exists: " + ffile);
              } 
              else 
              {

                bret &= parseFile(ffile, ts, parsedFiles, fdir, dcs);
                if (useCache == false && bret == false)
                  return false;
              }
            }
            else
            {
              ACDK_NLOG("acdk.make", Trace, "in file " + filename + " included <File> cannot be found: " + sfile);  
            }
            tin.skipLine();
            continue;
          }
        } 
        else if (tk == StreamTokenizer::TT_STRING)
        {
          File tf(ncwd, tin.sval);
          if (tf.exists() == false)
            ACDK_NLOG("acdk.make", Note, "in file " + filename + " included \"File\" doesn't exists: " + tin.sval);
          else 
          {
            RString ffile = tf.getCanonicalPath();
            RString fdir = tf.getParent();
            bret &= parseFile(ffile, ts, parsedFiles, fdir, dcs);
            if (useCache == false && bret == false)
              return false;
          }
          tin.skipLine();
          continue;
        }
      }
      else if (tk == StreamTokenizer::TT_WORD && tin.sval->equals("define") == true)
      {
      }
      else
      {
        tin.skipLine();
        continue;
      }
    } 
    else if (tk == StreamTokenizer::TT_WORD && tin.sval->equals("namespace") == true)
    {
      break;
    } 
    else 
    {
      tin.skipLine();
      continue;
    }
  }
  return bret;
}

//virtual 
bool 
CppSourceDependTask::execute(IN(RString) exec, IN(RProps) props)
{
  SYNCCLASS(); // cannot execute mutliple version because ChDir

  //DOUT("Parse: " << _target);
  File tf(_target);
  if (tf.exists() == false)
  {
    ACDK_NLOG("acdk.make", Note, "File doesn't exists: " + _target);
    return true;
  }
  jlong lastmod;
  {
    //RString fqfilename = tf.getCanonicalPath();
    ::acdk::io::FileStatus ts(_target);
    lastmod = ts.lastModified();
  }
  StringArray _parsedFiles(0);
  DepCacheStack dcs;
  File sf(_source);
  RString fdir = sf.getParent();
  RString ffname = sf.getName();
  return parseFile(_source, lastmod, &_parsedFiles, fdir, dcs);
}
  



} // namespace make
} // namespace acdk


