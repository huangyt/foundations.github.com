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

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/util/StringTokenizer.h>
#include <acdk/util/TreeSet.h>
#include <acdk/io/File.h>

#include "Script.h"
#include "ScriptException.h"
#include "ShellExecutor.h"

namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::util::, Iterator);
USING_CLASS(::acdk::util::, StringTokenizer);

Props::Props(IN(acdk::lang::dmi::RDmiNamedArgArray) namedArgs, short flags)
: _defaultFlags(flags)
, _castFlags(acdk::lang::dmi::SVCastStdFlags)
, _parents(new PropsArray(0))
, _curHeap(new HashMap())
, _name("")
, _singleThreaded(true)
{
  if (isStackRef() == true)
    ACDK_NLOG("acdk.cfgscript.Props", Note, "Props is allocated on stack, which may crash in connection with Scripts");

  ACDK_SAFE_CONSTRUCTOR();
  for (int i = 0; i < namedArgs->length(); ++i)
    set(namedArgs[i]->name, namedArgs[i]->value);
}

Props::Props(IN(RString) name, short flags, IN(RProps) parent, bool private_props)
: _defaultFlags(flags)
, _castFlags(acdk::lang::dmi::SVCastStdFlags)
, _parents(new PropsArray(0))
, _curHeap(new HashMap())
, _name(name)
, _singleThreaded(true)
{
  if (isStackRef() == true)
    ACDK_NLOG("acdk.cfgscript.Props", Note, "Props is allocated on stack, which may crash in connection with Scripts");

  if (parent != Nil)
  {
    _singleThreaded = parent->getSingleThreaded();
    _parents->append(parent);
    _castFlags = parent->getCastFlags();
  }
  if (_parents->length() == 0 && private_props == false)
    _init();
}

Props::Props(short flags, IN(RProps) parent, bool private_props)
: _defaultFlags(flags)
, _castFlags(acdk::lang::dmi::SVCastStdFlags)
, _parents(new PropsArray(0))
, _curHeap(new HashMap())
, _name("")
, _singleThreaded(true)
{
  if (isStackRef() == true)
    ACDK_NLOG("acdk.cfgscript.Props", Note, "Props is allocated on stack, which may crash in connection with Scripts");

  if (parent != Nil)
  {
    _parents->append(parent);
    _singleThreaded = parent->getSingleThreaded();
    _castFlags = parent->getCastFlags();
  }
  if (_parents->length() == 0 && private_props == false)
    _init();
}


bool 
Props::hasParentProps(IN(RProps) nparent)
{
  if (&nparent == this)
    return true;
  if (_parents == Nil)
    return false;
  for (int i = 0; i < _parents->length(); ++i)
    if (_parents[i]->hasParentProps(nparent) == true)
      return true;
  return false;
}

RString
Props::toString()
{
  StringBuffer sb;
  sb << "{ ";
  // ### @todo dump parents
  acdk::util::RMapEntry node;
  bool first = true;
  for (RIterator it = _curHeap->iterator();
       it->hasNext() == true;
       )
  {
    node = (acdk::util::RMapEntry)it->next();
    if (first == false)
      sb << ",";
    RDmiObject val = (RDmiObject)node->getValue();
    RObject recObj = val->getObjectVar();
    while (instanceof(recObj, DmiObject) == true)
      recObj = RDmiObject(recObj)->getObjectVar();
    if (recObj == this)
    {
      RObject recObj = val->getObjectVar();
      sb << node->getKey() << ": (<THISPROPS>)";
    }
    else
    //RObject o = _curHeap->get(&key);
      sb << node->getKey() << ": " << (node->getValue() == Nil ? RString("Nil") : node->getValue()->toString());
    first = false;
  }
  sb << " }";
  return sb.toString();
}

RObject 
Props::clone(acdk::lang::sys::Allocator* alloc)
{
  RProps cloned = new Props(_defaultFlags);
  cloned->_parents = (RPropsArray)_parents->clone(alloc);
  cloned->_curHeap = (RHashMap)_curHeap->clone(alloc);
  cloned->_name = _name;
  cloned->_singleThreaded = _singleThreaded;
  return &cloned;
}

RDmiObject
Props::get(IN(RString) name, short flags)
{
  SYNCTHIS();
  return _get(name, flags);
}

RDmiObject
Props::_get(IN(RString) name, short flags)
{
  RDmiObject ret = (RDmiObject)_curHeap->get(&name);
  if (ret == Nil && _readParent(flags))
  {
    for (int i = 0; i < _parents->length(); ++i)
    {
      ret = _parents[i]->get(name, flags | PropsNoWarnRead);
      if (ret != Nil)
        return ret;
    }
  }
  if (ret == Nil && _warnRead(flags))
    ACDK_NLOG("acdk.cfgscript", Warn, getName() + ": Props::get(): Key [" + name + "] does not exists");
  return ret;
}


RHashMap
Props::findOwnerHeap(IN(RString) key)
{
  if (_curHeap->get(&key) != Nil)
    return _curHeap;
  if (_parents != Nil)
  {
    for (int i = 0; i < _parents->length(); ++i)
    {
      RHashMap ret = _parents[i]->findOwnerHeap(key);
      if (ret != Nil)
        return ret;
    }
  }
  return Nil;
}

void
Props::set(IN(RString) name, IN(RDmiObject) value, short flags)
{
  SYNCTHIS();
  _set(name, value, flags);
}

void 
Props::_notifyListener(PropChangeEvents event, IN(RString) key)
{
  if (_listeners == Nil)
    return;
  for (int i = 0; i < _listeners->length(); ++i)
    _listeners[i]->afterChange(event, this, key);
}

void
Props::_set(IN(RString) name, IN(RDmiObject) value, short flags, bool notify)
{
  ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": set: [" + name + "]=[" + (value == Nil ? RString("Nil") : value->toString()) + "]");
  //dbg if (name->equals("AMAKE_TARGET_TAGS") == true)
  // System::out->println("AMAKE_TARGET_TAGS =" + value->toCode());
  RHashMap h = _curHeap;

  if (_writeParent(flags) == true)
    h = findOwnerHeap(name);
  if (h == Nil)
    h = _curHeap;
  // ### eval _warnWrite
  h->put(&name, &value);
  if (notify == true)
    _notifyListener(PropEventValueWrite, name);
  
}

void
Props::unset(IN(RString) name, short flags)
{
  SYNCTHIS();
  if (_writeParent(flags) == false)
    _curHeap->remove(&name);
  else
  {
    RHashMap h = findOwnerHeap(name);
    if (h == Nil)
      return;
    h->remove(&name);
  }
  _notifyListener(PropEventRemoveKey, name);
}

void
Props::create(IN(RString) name, IN(RDmiObject) val)
{
  SYNCTHIS();
  if (hasValue(name, PropsNoParentRead) == true)
    THROW1(ScriptException, "Variable " + name + " is already defined in this property scope");
  _set(name, val, PropsNoParentWrite, false);
  _notifyListener(PropEventCreate, name);
}

void
Props::assign(IN(RString) name, IN(RDmiObject) val, short flags)
{
  SYNCTHIS();
  if (hasValue(name) == false)
    THROW1(ScriptException, "Variable " + name + " is not defined in this property scope");
  RDmiObject var = _get(name, flags);
  var->assign(val, _castFlags);
  _notifyListener(PropEventAssign, name);
}

int
Props::size(short flags)
{
  SYNCTHIS();
  if (_readParent(flags) == true)
  {
    int size = _curHeap->size();
    for (int i = 0; i < _parents->length(); ++i)
      size += _parents[i]->size(flags);
    return size;
  }
  else
    return _curHeap->size();
}

void
Props::_getKeys(IN(RStringArray) sa)
{
  SYNCTHIS();
  RIterator it = _curHeap->keySet()->iterator();
  for (int i = 0; it->hasNext() == true; ++i)
  {
    RString s = (RString)it->next();
    if (sa->find(s) == -1)
      sa->append(s);
  }
}

RStringArray
Props::getKeys(short flags)
{
  SYNCTHIS();
  RStringArray erg = new StringArray(0);
  if (_readParent(flags) && _parents != Nil)
  {
    for (int i = 0; i < _parents->length(); ++i)
    {
      _parents[i]->_getKeys(erg);
    }
  }
  _getKeys(erg);
  return erg;
}

void
Props::appendObjectList(IN(RString) key, IN(RObject) val, short flags)
{
  SYNCTHIS();
  RDmiObject obj = get(key, flags & PropsWarnRead ? flags : flags | PropsNoWarnRead);
  RObjectArray a;
  if (obj == Nil || obj->getObjectVar() == Nil)
    a = new ObjectArray(0);
  else
    a = (RObjectArray)obj->getObjectVar();
  a->append(val);
  if (obj == Nil)
    setObjectVal(key, &a, flags);
}

RString
Props::getStringVal(IN(RString) name, short flags)
{
  RDmiObject ret = get(name, flags);
  if (ret != Nil && ret->getObjectVar() != Nil)
  {
    return ret->getObjectVar()->toString();
  }
  return "";
}

void
Props::setStringVal(IN(RString) key, IN(RString) value, short flags)
{
  ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": setStringVal: [" + key + "]=[" + value + "]");
  set(&key, new DmiObject(inOf(value)), flags);
}

void
Props::appendStringVal(IN(RString) key, IN(RString) value, IN(RString) joiner, short flags)
{

  RString str = (RString)getStringVal(key, flags & PropsWarnRead ? flags : flags | PropsNoWarnRead);
  if (str == Nil)
  {
    setStringVal(key, value, flags);
  }
  else
  {
    if (flags & PropsAppendPushFront)
    {
        if (joiner != Nil)
        str = value + joiner + str;
      else
        str = value + str;
    }
    else
    {
      if (joiner != Nil)
        str = str + joiner + value;
      else
        str = str + value;
    }
    setStringVal(key, str, flags);
  }
}

bool containsString(IN(RStringArray) sa, IN(RString) v)
{
  for (int i = 0; i < sa->length(); ++i)
    if (sa[i]->equals(v) == true)
      return true;
  return false;
}

void
Props::appendStringArrayVal(IN(RString) key, IN(RString) value, short flags)
{
  SYNCTHIS();
  RDmiObject arr = _get(key, flags & PropsWarnRead ? flags : flags | PropsNoWarnRead);
  if (arr == Nil)
  {
    RStringArray narr = new StringArray(1);
    narr[0] = value;
    setStringArrayVal(key, narr, flags);
  } else {

    RStringArray sa = RStringArray(arr->getObjectVar());
    if (flags & PropsNoStringDups)
    {
      if (containsString(sa, value) == true)
        return;
    }
    ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": appendStringArray: [" + key + "] + [" + value + "]");
    if (_writeParent(flags) == false && _curHeap->get(&key) == Nil)
    {
      sa = (RStringArray)sa->clone();
      _set(key, new DmiObject(inOf(sa)), flags);
    }
    sa->append(value);
  }
}

void
Props::setStringArrayVal(IN(RString) key, IN(RStringArray) value, short flags)
{
  ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": setStringArrayVal: [" + key + "]=[" + value->toString() + "]");
  set(&key, new DmiObject(inOf(value)), flags);
}

RStringArray
Props::getStringArrayVal(IN(RString) key, short flags)
{

  RDmiObject ret = get(key, flags);

  if (ret == Nil)
  {
    SYNCTHIS();

    RStringArray sa = new StringArray(0);
    set(&key, new DmiObject(inOf(sa)), flags);
    return sa;
  }
  return RStringArray(ret->getObjectVar());
}

RStringArray
Props::getAllStringArrayVal(IN(RString) key, short flags)
{
  RDmiObject ret = (RDmiObject)_curHeap->get(&key);
  RStringArray colret;
  if (ret != Nil)
    colret = (RStringArray)ret->getObjectVar();
  else
    colret = new StringArray(0);
  if (_readParent(flags) == false)
    return colret;
  for (int i = 0; i < _parents->length(); ++i)
  {
    RStringArray sub = _parents[i]->getAllStringArrayVal(key, flags | PropsNoWarnRead);
    if (sub != Nil && sub->length() > 0)
    {
      for (int i = 0; i < sub->length(); ++i)
      {
        if (containsString(colret, sub[i]) == false)
          colret->append(sub[i]);
      }
    }
  }
  return colret;
}

bool
Props::containsInStringArrayVal(IN(RString) key, IN(RString) value, short flags)
{
  SYNCTHIS();
  RStringArray erg = getStringArrayVal(key, flags);
  if (erg == Nil)
    return false;
  for (int i = 0; i < erg->length(); ++i)
  {
    if (erg[i]->equals(value) == true)
      return true;
  }
  return false;
}

void
Props::setQuotedStringVal(IN(RString) key, IN(RString) value, short flags)
{
  RString val = value;
  if (val->indexOf(' ') != -1)
  {
    RString qc = getStringVal("CMDLINE_QUOTE_CHAR", flags);
    if (val->indexOf(qc) == -1)
    {
      val = qc + val + qc;
    }
  }
  setStringVal(key, val, flags);
}

RString
Props::getQuotedStringVal(IN(RString) key, short flags)
{
  RString val = getStringVal(key, flags);
  if (val->indexOf(' ') == -1)
    return val;
  RString qc = getStringVal("CMDLINE_QUOTE_CHAR", flags);
  if (val->indexOf(qc) == 0)
    return val;
  return qc + val + qc;
}

RString
Props::getUnquotedStringVal(IN(RString) key, short flags)
{
  RString val = getStringVal(key, flags);

  RString qc = getStringVal("CMDLINE_QUOTE_CHAR", flags);
  if (val->startsWith(qc) && val->endsWith(qc) == true)
    return val->substr(1, val->length() - 2);
  return val;
}

RProps
Props::getProps(IN(RString) name, short flags)
{
  RDmiObject obj = get(name, flags);
  if (obj != Nil)
    return RProps(obj->getObjectVar());

  RProps np = new Props(PropsParentRead | PropsWarnRead);
  set(&name, new DmiObject(inOf(np)), flags);
  return np;
}

//static
RStringArray
Props::makeStringArray(IN(RString) s1, IN(RString) s2, IN(RString) s3, IN(RString) s4,
                                      IN(RString) s5, IN(RString) s6, IN(RString) s7, IN(RString) s8)
{
  RStringArray erg;
  if (s8 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(8);
    erg[7] = s8;
  }
  if (s7 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(7);
    erg[6] = s7;
  }
  if (s6 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(6);
    erg[5] = s6;
  }
  if (s5 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(5);
    erg[4] = s5;
  }
  if (s4 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(4);
    erg[3] = s4;
  }
  if (s3 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(3);
    erg[2] = s3;
  }
  if (s2 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(2);
    erg[1] = s2;
  }
  if (s1 != Nil)
  {
    if (erg == Nil)
      erg = new StringArray(1);
    erg[0] = s1;
  }
  if (erg == Nil)
    erg = new StringArray(0);
  return erg;
}


RStringArray
splitPathList(IN(RString) str)
{
  StringTokenizer st(str, ::acdk::io::File::pathSeparator());
  RStringArray  ret = new StringArray(0);
  while (st.hasNext() == true)
  {
    ret->append(st.nextToken());
  }
  return ret;
}

int 
Props::_getFlagsFromStringProps(int defaultFlags)
{
  // ### TODO implement me
  return defaultFlags;
}

void
Props::_init()
{
  ACDK_NLOG("acdk.cfgscript", Info, getName() + ": Initialize Env Props");
  RProperties props = System::getProperties();
  RIterator it = props->propertyNames();
  int stdflags = 0;
  while (it->hasNext() == true)
  {
    RString k = (RString)it->next();
    setStringVal(k, props->getProperty(k), stdflags);
  }
#if defined(ACDK_OS_WIN32)
  if (getStringVal("Path", PropsNoWarnRead) != Nil && getStringVal("Path", PropsNoWarnRead)->length() > 0) // on cygwin, this is not Path, but PATH
    setStringVal("PATH", getStringVal("Path", 0), stdflags);
#endif
  setStringArrayVal("ENV_PATH_LIST", splitPathList(getStringVal("PATH", stdflags)), stdflags);
  if (hasValue("INCLUDE", stdflags) == true)
    setStringArrayVal("ENV_INCLUDE_LIST", splitPathList(getStringVal("INCLUDE", stdflags)), stdflags);
  if (hasValue("LIB", stdflags) == true)
    setStringArrayVal("ENV_LIB_LIST", splitPathList(getStringVal("LIB", stdflags)), stdflags);
  if (hasValue("LD_LIBRARY_PATH", stdflags) == true)
    setStringArrayVal("ENV_LD_LIBRARY_PATH_LIST", splitPathList(getStringVal("LD_LIBRARY_PATH", stdflags)), stdflags);
  if (hasValue("CLASSPATH", stdflags) == true)
    setStringArrayVal("ENV_CLASSPATH_LIST", splitPathList(getStringVal("CLASSPATH", stdflags)), stdflags);
}


RString removeQuoted(IN(RString) str)
{
  int idx = str->indexOf("#{");
  if (idx == -1)
    return str;
  StringBuffer sb;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  String::iterator lstart = it;
  enum State 
  { 
    Seek,
    Collect
  };
  State state = Seek;
  for (; it < end; ++it)
  {
    if (state == Seek)
    {
      if ((it + 1) < end && *(it + 1) == '{' &&
        (*it == '#'))
      {
        sb.append(lstart, it);
        lstart = it + 2;
        state = Collect;
      }
    }
    else
    {
      if ((it + 1) < end && *(it + 1) == '#' && (*it == '}'))
      {
        sb.append(lstart, it);
        it += 2;
        lstart = it;
        state = Seek;
      }
    }
  }
  sb.append(lstart, end);
  return sb.toString();
}

RString
Props::evalShellExecute(IN(RString) str, short flags)
{
  int idx = str->indexOf('`');
  if (idx == -1)
    return str;
  StringBuffer sb;
  String::iterator it = str->begin();
  String::iterator end = str->end();
  String::iterator lstart = it;
  enum State 
  { 
    Seek,
    Collect
  };
  State state = Seek;
  for (; it < end; ++it)
  {
    if (state == Seek)
    {
     if ((it + 1) < end && *(it + 1) == '{' &&
        (*it == '`'))
      {
        sb.append(lstart, it);
        state = Collect;
      }
    }
    else 
    {
      if ((it + 1) < end && *(it + 1) == '`' && (*it == '}'))
      {
        RString iden = str->substr(lstart - str->begin() + 2, it - str->begin());
        iden = removeQuoted(iden);
        ShellExecutor exec(iden, SExecNoStdOut | SExecNoErrOut, Nil);
        if (hasValue("err") == true)
        {
          RObject o = getObjectVal("err");
          if (instanceOf(o, acdk::io::CharWriter) == true)
            exec.setErrWriter((acdk::io::RCharWriter)o);
        }
        exec.setOutWriter(Nil);
        bool erg = exec.execute(this);
        RString outp = exec.getOutString();
        outp = outp->trim(TrimNewLines | TrimRight);
        sb.append(outp);
        ++it;
        lstart = it + 1;
        state = Seek;
      }
    }
  }
  sb.append(lstart, end);
  return sb.toString();
}

RString
Props::eval(IN(RString) str, short flags)
{
  RString erg = _eval(str, flags);
  erg = evalShellExecute(erg, flags);
  return removeQuoted(erg);
}

RString
Props::_eval(IN(RString) str, short flags)
{
  int start = 0;
  StringBuffer sb;
  RString inp = str;
  RString quotechar = getStringVal("CMDLINE_QUOTE_CHAR", flags | PropsNoWarnRead);

  if (quotechar->length() == 0)
    quotechar = "\"";
  bool changed = false;
  String::iterator it = inp->begin();
  String::iterator end = inp->end();
  String::iterator lstart = it;
  for (; it < end; ++it)
  {
    if ((it + 1) < end && *(it + 1) == '{' &&
        (*it == '$' || *it == '!' || *it == '@' || *it == '#'))
    {
      sb.append(lstart, it);
      char type = *it;
      it += 2;
      String::iterator ss = it;
      int openbrackets = 1;
      while (it < end)
      {
        ++it;
        if (*it == '}')
        {
          --openbrackets;
          if (openbrackets == 0)
            break;
        }
        if (*it == '{')
          ++openbrackets;
      }
      if (it == end)
      {
        sb.append(it, end);
        break;
      }
      RString iden = str->substr(ss - str->begin(), it - str->begin());

      if (type == '$')
      {
        RString nv;
        if (hasValue(iden, flags) == true)
        {
          nv = getStringVal(iden, flags);
        }
        else
        {
          RProps  cfgprops = new Props("", PropsParentRead | PropsParentWrite, this);
          RScript script = new Script("");
          cfgprops->setObjectVal("__script", &script, PropsNoParentWrite);
          RDmiObject o = script->evalExpr(iden, cfgprops, ScriptReadWriteParent);
          if (o != Nil)
            nv = o->toString();
        }
        if (nv != Nil)
        {
          if ((flags & PropsEvalQuoteFileNameArgs) && nv->indexOf(" ") != -1 && nv->indexOf(quotechar) == -1)
          {
            sb.append(quotechar);
            sb.append(nv);
            sb.append(quotechar);
          } else
            sb.append(nv);
          changed = true;
        } else
          ; // nothing?
      }
      else if (type == '@')
      {
        RStringArray sa = getStringArrayVal(iden, flags);
        for (int i = 0; i < sa->length(); ++i)
        {
          RString nv = sa[i];
          if (i > 0)
            sb.append(" ");
          if ((flags & PropsEvalQuoteFileNameArgs) && nv->indexOf(" ") != -1 && nv->indexOf(quotechar) == -1)
          {
            sb.append(quotechar);
            sb.append(nv);
            sb.append(quotechar);
          } else
            sb.append(nv);
        }
        changed = true;

      }
      else if (type == '!')
      {
        ++it;
        if (*it != '!')
        {
          sb.append(lstart, it);
          lstart = it;
          continue;
        }
        RProps cfgprops = new Props("", PropsParentRead | PropsParentWrite, this);
        acdk::io::RStringWriter out = new acdk::io::StringWriter();
        acdk::io::RPrintWriter pout = new acdk::io::PrintWriter(&out);
        cfgprops->setObjectVal("out", &pout, PropsNoParentWrite);
        RScript script = new Script("");
        cfgprops->setObjectVal("__script", &script, PropsNoParentWrite);
        if (script->eval(iden, &cfgprops, ScriptReadWriteParent) != 0)
        {
          sb.append(lstart, end);
          return sb.toString();
        }
        pout->flush();
        sb.append(out->getString());
        changed = true;
      }
      else if (type == '#')
      {
        ++it;
        sb.append(lstart, it + 1);
      }
      lstart = it + 1;
    }

  }
  if (changed == false)
    return inp;

  sb.append(lstart, end);


  inp = sb.toString();

  ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": Props::eval: from [" + str + "] to [" + inp + "]");
  if ((flags & PropsEvalRecursive) == 0)
  {
    return inp;
  }
  return _eval(inp, flags);
}

void
Props::execScript(IN(RString) str, IN(RString) filename, short flags)
{
  RScript script = new Script(filename);
  script->initAsEnvProps(this);
  if (script->eval(str, this, ScriptReadParent | ScriptWriteParent) != 0)
  {
    // ### @todo handle error
  }
}

bool
Props::importNameSpace(IN(RString) prefix)
{
  return _importNameSpace(prefix, this);
}

bool
Props::_importNameSpace(IN(RString) prefix, IN(RProps) props)
{
  SYNCTHIS();
  bool ret = false;
  if (props->_parents != Nil)
  {
    for (int i = 0; i < props->_parents->length(); ++i)
      ret = _importNameSpace(prefix, props->_parents[i]);
  }

  RIterator it = props->_curHeap->keySet()->iterator();
  HashMap lhm;
  while (it->hasNext() == true)
  {
    RString k = (RString)it->next();
    if (k->startsWith(prefix + "_") == true)
    {
      RObject v = props->_curHeap->get(&k);
      RString nk = k->substr(prefix->length() + 1);
      ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": Props::import: from " + k + " to " + nk + "=" + v);
      lhm.put(&nk, &v);
      ret = true;
    }
    else if (k->endsWith("_" + prefix) == true)
    {
      RObject v = props->_curHeap->get(&k);
      RString nk = k->substr(0, k->length() - (prefix->length() + 1));
      ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": Props::import: from " + k + " to " + nk + "=" + v);
      lhm.put(&nk, &v);
      ret = true;
    }
    else
    {
      int f;
      if ((f = k->indexOf("_" + prefix + "_")) != -1)
      {
        RString nk = k->replace("_" + prefix + "_", "_");
        RObject v = props->_curHeap->get(&k);
        ACDK_NLOG("acdk.cfgscript", Debug, getName() + ": Props::import: from " + k + " to " + nk + "=" + v);
        lhm.put(&nk, &v);
        ret = true;
      }
    }
  }
  if (ret == true)
  {
    RIterator it = lhm.keySet()->iterator();
    while (it->hasNext() == true)
    {
      RObject k = it->next();
      RObject v = lhm.get(k);
      _curHeap->put(k, v);
    }
  }
  return ret;
}

void
Props::merge(IN(RProps) other, short flags)
{

  if ((flags & PropsMergeWithParent) && other->_parents != Nil)
  {
    for (int i = 0; i < other->_parents->length(); ++i)
      merge(other->_parents[i], flags);
  }
  SYNCTHIS();
  RIterator it = other->_curHeap->keySet()->iterator();
  while (it->hasNext() == true)
  {
    RObject key = it->next();
    RString skey = (RString)key;
    RDmiObject val = (RDmiObject)other->_curHeap->get(key);
    RDmiObject oldval = (RDmiObject)_curHeap->get(key);
    if (val != Nil && val->isObjectType() == true && instanceof(val->getObjectVar(), StringArray) == true)
    {
      if (oldval == Nil || ((PropsMergeOverWrite & flags) && ((PropsMergeAppendArrays & flags) != PropsMergeAppendArrays)))
      {
        ACDK_NLOG("acdk.cfgscript", Debug, "Merge: " + skey + "=" + val->toCode());
        set(skey, val, flags);
      }
      else if ((PropsMergeAppendArrays & flags) == PropsMergeAppendArrays)
      {
        if (oldval->isObjectType() == true && instanceof(oldval->getObjectVar(), StringArray) == true)
        {
          RStringArray sa = (RStringArray)oldval->getObjectVar();
          RStringArray na = (RStringArray)val->getObjectVar();
          for (int i = 0; i < na->length(); ++i)
          {
            sa->append(na[i]);
          }
          ACDK_NLOG("acdk.cfgscript", Debug, "Merge: " + skey + "=" + sa->toString());
          _set(skey, new DmiObject(inOf(sa)), flags);
        }
      }
    }
    else
    {
      if (oldval == Nil || PropsMergeOverWrite & flags)
      {
        ACDK_NLOG("acdk.cfgscript", Debug, "Merge: " + skey + "=" + val->toCode());
        _set(skey, val, flags);
      }
    }
  }
}


void
Props::_getAllStringVals(IN(RString) key, short flags, IN(RStringArray) values)
{
  RDmiObject p = get(&key, PropsNoParentRead | PropsWarnRead);
  if (p != Nil)
  {
    RString cv = p->getStringVar();//(RString)RDmiObject(_curHeap->get(&key))->getObjectVar();
    if (cv != Nil && containsString(values, cv) == false)
      values->append(cv);
  }
  if (_parents == Nil)
    return;
  for (int i = 0; i < _parents->length(); ++i)
    _parents[i]->_getAllStringVals(key, flags, values);
}

RStringArray
Props::getAllStringVals(IN(RString) key, short flags)
{
  RProps tp = this;
  RStringArray erg = new StringArray(0);
  _getAllStringVals(key, flags, erg);
  return erg;
}

void 
Props::addListener(IN(RPropsChangeListener) listener)
{
  if (_listeners == Nil)
    _listeners = new PropsChangeListenerArray(0);
  _listeners->append(listener);
}

void 
Props::removeListener(IN(RPropsChangeListener) listener)
{
  if (_listeners == Nil)
    return;
  for (int i  = 0; i < _listeners->length(); ++i)
  {
    if (_listeners[i] == listener)
    {
      _listeners->remove(i);
      return;
    }
  }
}

void
Props::_getKeys(INOUT(::acdk::util::TreeSet) keys, bool withParents)
{
  RIterator it = _curHeap->keySet()->iterator();
  while (it->hasNext() == true)
  {
    keys.add(it->next());
  }
  if (withParents == false)
    return;
  RPropsArray pa = getParentsProps();
  if (pa != Nil)
  {
    for (int i = 0; i < pa->length(); ++i)
    {
      pa[i]->_getKeys(keys);
    }
  }
}

void
Props::dump(int dumpFlags, ::acdk::util::RTreeSet usedkeys, IN(RString) ident)
{
  short flags = 0;

  ::acdk::util::TreeSet keys;

  _getKeys(keys, dumpFlags & DumpWithParent ? true : false);

  RIterator it = keys.iterator();
  while (it->hasNext() == true)
  {
    RString k = RString(it->next());
    RDmiObject v = get(k, flags);
    if (v->isStringType() == true)
    {
      System::out->println(ident + k + "=[\"" + v->toString() + "\"]");
    }
    else if (v->isObjectType() == true && instanceof(v->getObjectVar(), Props) == true)
    {
      if (dumpFlags & DumpWithChilds ||
          ((dumpFlags & DumpWithParent) && k->equals("_parent") == true))
      {
      if (usedkeys == Nil || k->equals("_parent") == true || usedkeys->contains(&k) == false)
      {
        if (usedkeys == Nil)
          usedkeys = new acdk::util::TreeSet();
        usedkeys->add(&k);
        System::out->println(ident + k + "=[");
        RProps(v->getObjectVar())->dump(dumpFlags, usedkeys, ident + "  ");
        System::out->println(ident + "]");
      }
      }
    }
    else
      System::out->println(ident + k + "=(" + v->getClass()->getName() + ")[" + v->toString() + "]");
  }
}


RString
Props::getAcdkHome(bool throwIfNotFound)
{
  RString erg = getStringVal("ACDKHOME", ScriptReadParent);
  if (erg != Nil && erg->length() > 0)
    return erg;
  erg = System::getAcdkHome();
  if (erg != Nil && erg->length() > 0)
    return erg;
  if (throwIfNotFound == true)
    THROW1(ScriptException, "Cannot find Property ACDKHOME or ACDK_HOME");
  return Nil;
}

RString
Props::getAcdkToolsHome(bool throwIfNotFound)
{
  RString erg = getStringVal("ACDK_TOOLS_HOME", PropsParentRead | PropsNoWarnRead);
  if (erg != Nil && erg->length() > 0)
    return erg;
  erg = System::getAcdkToolsHome();
  if (erg != Nil && erg->length() > 0)
    return erg;
  if (throwIfNotFound == true)
    THROW1(ScriptException, "Cannot find Property ACDK_TOOLS_HOME, ACDKHOME or ACDK_HOME");
  return Nil;
}

void 
Props::_asCsfLiteral(StringBuffer& sb, IN(RString) indent, short flags, IN(RString) key, IN(RDmiObject) val)
{
  if (val->isStringType() == true || val->isObjectType() == false)
  {
    sb << indent << "." << key << " = " + val->toCode() + ";\n";
    return;
  }
  if (val->isObjectType() == true)
  {
    RObject o = val->getObjectVar();
    if (instanceof(o, Props) == true)
    {
      RProps(o)->_asCsfLiteral(sb, "." + key, indent, flags);
      return;
    }
    if (instanceof(o, ObjectArray) == true)
    {
      // ### TODO support arrays
    }
    ACDK_NLOG("acdk.cfgscript", Error, "unsupported value type in render as CsfLiteral: " << o->getClass()->getName());
  }
}

void 
Props::_asCsfLiteral(StringBuffer& sb, IN(RString) keyName, IN(RString) indent, short flags)
{
  sb << indent << "with (" << keyName << " = new acdk.cfgscript.Props(\"" << keyName << "\"))\n" << indent << "{\n";
  RStringArray keys = getKeys(flags);
  for (int i = 0; i < keys->length(); ++i)
  {
    RString key = keys[i];
    _asCsfLiteral(sb, indent, flags, key, get(key));
  }
  sb << indent << "}\n";
}

void
Props_asLiteral(StringBuffer& sb, IN(RObject) val, IN(RString) indent)
{
  if (instanceof(val, Props) == true)
  {
    RProps p = (RProps)val;
    sb << "{\n";
    RString nindent = indent + "  ";
    RStringArray keys = p->getKeys(0);
    for (int i = 0; i < keys->length(); ++i)
    {
      RString key = keys[i];
      sb << nindent << key << ": ";
      Props_asLiteral(sb, &p->get(key), nindent);
      if (i != keys->length() - 1)
        sb << ",\n";
      else
        sb << "\n";
    }
    sb << indent << "}";
  }
  else if (instanceof(val, DmiObject) == true)
  {
    RDmiObject dval = (RDmiObject)val;
    if (dval->isStringType() == true || dval->isObjectType() == false)
    {
      sb << dval->toCode();
      return;
    }
    Props_asLiteral(sb, dval->getObjectVar(), indent);

  }
  else if (instanceof(val, DmiObjectArray) == true)
  {
    RDmiObjectArray oa = (RDmiObjectArray)val;
    sb << "[\n";
    RString nindent = indent + "  ";

    for (int i = 0; i < oa->length(); ++i)
    {
      sb << nindent;
      Props_asLiteral(sb, &oa[i], nindent);
      if (i != oa->length() - 1)
        sb << ",\n";
      else
        sb << "\n";
    }
    sb << indent << "]";
  }
  else
  {
    ACDK_NLOG("acdk.cfgscript", Error, "unsupported value type in render as CsfLiteral: " << val->getClass()->getName());
  }
}

RString 
Props::asCfgScriptLiteral(IN(RString) keyName, IN(RString) indent, short flags)
{
  StringBuffer sb;
  Props_asLiteral(sb, this, "");
  //_asCsfLiteral(sb, keyName, indent + "  ", flags);
  return sb.toString();
}

const acdk::lang::dmi::ClazzMethodInfo*
Props::standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret,
                                acdk::lang::dmi::ScriptVarArray& args,
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/)
{
  ASCLITERAL(peek);
  ASCLITERAL(poke);
  if (fname->equals(lit_peek) == true)
  {
    RString vn = args[0].getStringVar();
    RDmiObject obj = get(vn);
    if (obj == Nil)
      ret = Nil;
    else
      ret = *obj;
    return (const acdk::lang::dmi::ClazzMethodInfo*)1;
  }
  else if (fname->equals(lit_poke) == true)
  {
    RString vn = args[0].getStringVar();
    set(vn, new DmiObject(args[1]));
    return (const acdk::lang::dmi::ClazzMethodInfo*)1;
  }
  else
    return ACDK_FQ_SUPER_QUALIFIER(acdk::lang::dmi::, StdDispatch::)standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}

} // namespace cfgscript
} // namespace acdk



