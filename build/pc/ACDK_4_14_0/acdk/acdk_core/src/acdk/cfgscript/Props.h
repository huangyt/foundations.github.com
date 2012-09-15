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

#ifndef acdk_cfgscript_Props_h
#define acdk_cfgscript_Props_h

#include <acdk.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/TreeSet.h>
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/DmiNamedArg.h>

#include "Config.h"


namespace acdk {
namespace cfgscript {

USING_CLASS(::acdk::util::, HashMap);
USING_CLASS(::acdk::lang::dmi::, DmiObject);

ACDK_DECL_CLASS(Props);


/**
  Flags for operations on Props
*/
enum PropsFlags
{
  PropsNoFlags                = 0x0000,
  
  PropsParentRead             = 0x0001,

  PropsNoParentRead           = 0x0002,
  
  /*
    When modifiing a parameter
    copy parameter in current scope if needed.
    Default is true.
  */
  PropsParentWrite            = 0x0004,
  
  PropsNoParentWrite          = 0x0008,

  /**
    warn if value tried to readed not
    exits.
    Default warns if value try to read
    that doesn't exists
  */
  PropsWarnRead               = 0x0010,
  PropsNoWarnRead             = 0x0020,
  /**
    Warn if try to overwrite an value.
    Default is not to warn overwriting 
    values.
  */
  PropsWarnWrite              = 0x0040,
  PropsNoWarnWrite            = 0x0080,

  
  /** 
    In Appending a string to an existant, 
    insert the string on the begining
  */
  PropsAppendPushFront    = 0x0008,
  /**
    warn if value tried to readed not
    exits
  */
  /**
    Create no duplicates in string arrays
  */
  PropsNoStringDups             = 0x0001,
  
  PropsDefaultSetFlags = PropsNoParentWrite,
  PropsDefaultAppendStringArrayFlags = PropsNoStringDups | PropsNoParentWrite,

  PropsEvalQuoteFileNameArgs = 0x0100,
  PropsEvalRecursive         = 0x0200,
  PropsEvalDefault          = PropsParentRead | PropsEvalRecursive,
  PropsMergeWithParent    = 0x1000,
  PropsMergeOverWrite     = 0x2000,
  PropsMergeAppendArrays  = 0x4000,
  PropsMergeDefault       = PropsMergeWithParent | PropsMergeOverWrite | PropsMergeAppendArrays
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, PropsFlags);


enum PropChangeEvents
{
  PropEventValueWrite,
  PropEventCreate,
  PropEventAssign,
  PropEventRemoveKey
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, PropChangeEvents);

ACDK_DECL_INTERFACE(PropsChangeListener);

class ACDK_CFGSCRIPT_LIB_PUBLIC PropsChangeListener
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(PropsChangeListener)
public:
  virtual void afterChange(PropChangeEvents event, IN(RProps) props, IN(RString) key) = 0;
};


enum DumpFlags
{
  DumpWithParent  = 0x01,
  DumpWithChilds  = 0x02
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, DumpFlags);


/**
  Generic Properties holding DmiObject's.
  Can be used as more powerfull replacement for acdk::util::Properties
  the eval functions are tightly connected with the gw_ref[CfgScript, acdk_cfgscript_man]
  language.
*/

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiCiWeakBind)) 
class ACDK_CFGSCRIPT_LIB_PUBLIC Props
: extends ::acdk::lang::Object
, implements ::acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(Props)
private:
  short _defaultFlags;
  short _castFlags;
  RPropsArray _parents;
  RHashMap _curHeap;
  RString _name;
  bool _singleThreaded;
  RPropsChangeListenerArray _listeners;
public:
  /**
    
    @param flags combination of PropsFlags
  */
  Props(IN(RString) name, short flags = PropsNoFlags, IN(RProps) parent = Nil, bool private_props = true);
  Props(short flags = PropsNoFlags, IN(RProps) parent = Nil, bool private_props = true);
  Props(IN(acdk::lang::dmi::RDmiNamedArgArray) namedArgs, short flags = PropsNoFlags);
  
  /**
    this clone only clones the underlying HashMap. Keys and Values itself are not cloned
    prarents will be cloned too
  */
  foreign RObject clone(acdk::lang::sys::Allocator* alloc);
  RObject clone() { return clone(allocator()); }
  RString toString();
  void setName(IN(RString) name) { _name = name; }
  RString getName() { return _name; }
  void setSingleThreaded(bool singleThreaded) { _singleThreaded = singleThreaded; }
  bool getSingleThreaded() { return _singleThreaded; }
  inline void lock()
  {
    if (_singleThreaded == false)
      Object::lock();
  }
  inline void unlock()
  {
    if (_singleThreaded == false)
      Object::unlock();
  }
  /**
    a combination of PropsFlags 
  */
  short getDefaultFlags() { return _defaultFlags; }
  /**
    a combination of PropsFlags 
  */
  void setDefaultFlags(short flags) { _defaultFlags = flags; }
  /**
    a combination of acdk::lang::dmi::ScriptVarCastFlags
  */
  short getCastFlags() { return _castFlags; }
  /**
    a combination of acdk::lang::dmi::ScriptVarCastFlags
  */
  void setCastFlags(short castFlags) { _castFlags = castFlags; }
  /**
    @return the number of entries in this Props
    @param flags set CfgWithParents if including
           params
  */
  int size(short flags = PropsNoFlags);
  /**
    @deprecated see parents
  */
  RProps getParentProps()  
  { 
    if (_parents->length() == 0)
      return Nil;
    return _parents[0]; 
  }
  RPropsArray getParentsProps() { return _parents; }
  
  void addParentProps(IN(RProps) nparent) 
  { 
    _parents->append(nparent);
  }
  void removeParentProps(IN(RProps) nparent) 
  {
    _parents->removeSameElement(nparent);
  }
  /**
    return true if this or a parent or parent of parent has this props reference
  */
  bool hasParentProps(IN(RProps) nparent);
  RStringArray getKeys(short flags = PropsNoFlags);

  bool hasValue(IN(RString) key, short flags = PropsNoFlags) { return get(key, flags | PropsNoWarnRead) != Nil; }
  RDmiObject get(IN(RString) name, short flags = PropsNoFlags);
  //RDmiObject _get(IN(RString) name, short flags);
  /** 
    set a new value
    @param flags a bit combination of PropsOpFlags
  */
  void set(IN(RString) name, IN(RDmiObject) value, short flags = PropsNoFlags);
  
  /** 
    removes a key from heap
    @param flags a bit combination of PropsOpFlags
  */
  void unset(IN(RString) name, short flags = PropsNoFlags);

  /**
    create a new variable
    @throw ScriptException if variable with same name
           already is defined in this property
  */
  void create(IN(RString) name, IN(RDmiObject) val);
  foreign void create(IN(RString) name, const ::acdk::lang::dmi::ClazzInfo* ci) { create(name, new DmiObject(ci)); }
  
  /** 
    reset this props
  */
  void reset() 
  { 
    _curHeap->clear(); 
    _parents = Nil;
  }
  /**
    return the iterator of this scope keys
  */
  acdk::util::RIterator keys() { return _curHeap->keySet()->iterator(); }
  /**
    Assigns value to given name. 
    @throw ScriptException if variable name is not definied 
    @throw ClassCastException if variable type cannot be assigned
  */
  void assign(IN(RString) name, IN(RDmiObject) val, short flags = PropsNoFlags);
  /**
    return the container, which owns this key.
    @return Nil if no heap owns this key
  */
  RHashMap findOwnerHeap(IN(RString) key);

  RString getStringVal(IN(RString) name, short flags = PropsNoFlags);
  /**
    also CfgEval* flags will be needed
    @see also Props::eval
 */
  RString getEvaluatedStringVal(IN(RString) name, short flags = PropsNoFlags)
  {
    return eval(getStringVal(name, flags), flags);
  }
  void setStringVal(IN(RString) key, IN(RString) value, short flags = PropsNoFlags);
  /**
    helper to create a string array
  */
  static RStringArray makeStringArray(IN(RString) s1 = Nil, IN(RString) s2 = Nil, IN(RString) s3 = Nil, IN(RString) s4 = Nil,
                                      IN(RString) s5 = Nil, IN(RString) s6 = Nil, IN(RString) s7 = Nil, IN(RString) s8 = Nil);
  
  void setObjectVal(IN(RString) key, IN(RObject) value, short flags = PropsNoFlags) { set(key, new DmiObject(inOf(value)), flags); }
  RObject getObjectVal(IN(RString) key, short flags = PropsNoFlags) 
  { 
    RDmiObject dmo = get(key, flags);
    if (dmo == Nil)
      return Nil;
    return dmo->getObjectVar();
  }
   /**
    equal to setStringArrayVal but if value contains spaces 
    CMDLINE_QUOTE_CHAR will be used to quote value
  */
  void setQuotedStringVal(IN(RString) key, IN(RString) value, short flags = PropsNoFlags);
  RString getQuotedStringVal(IN(RString) key, short flags = PropsNoFlags);
  /**
    equal to getQuoteStringVal, but in case argument is quoted
    removes quotes before return value
  */
  RString getUnquotedStringVal(IN(RString) key, short flags = PropsNoFlags);

  void appendStringVal(IN(RString) key, IN(RString) value, IN(RString) joiner/* = " "*/, short flags = PropsNoFlags);
  
  void setStringArrayVal(IN(RString) key, IN(RStringArray) value, short flags = PropsNoFlags);
  void appendStringArrayVal(IN(RString) key, IN(RString) value, short flags = PropsNoFlags);
  /**
    return StringArray 
    returns a empty StringArray if not found
  */
  RStringArray getStringArrayVal(IN(RString) key, short flags = PropsNoFlags);
  /**
    collects in this an parent props all StringArrayVals in a single StringArrayVals
    @param flags if PropsNoStringDups throws away all duplicated string values
  */
  RStringArray getAllStringArrayVal(IN(RString) key, short flags = PropsNoFlags);
  bool containsInStringArrayVal(IN(RString) key, IN(RString) value, short flags = PropsNoFlags);

  void appendObjectList(IN(RString) key, IN(RObject) val, short flags = PropsNoFlags);

  bool getBoolVal(IN(RString) key, short flags = PropsNoFlags)
  {
    RDmiObject obj = get(key, flags);
    if (obj == Nil)
      return false;
    return obj->getBoolVar();
  }
  void setBoolVal(IN(RString) key, bool val, short flags = PropsNoFlags)
  {
    set(key, new DmiObject(inOf(val)), flags);
  }
  int getIntVal(IN(RString) key, short flags = PropsNoFlags)
  {
    RDmiObject obj = get(key, flags);
    if (obj == Nil)
      return -1;
    return obj->getIntVar();
  }
  void setIntVal(IN(RString) key, int val, short flags = PropsNoFlags)
  {
    set(key, new DmiObject(inOf(val)), flags);
  }
  double getDoubleVal(IN(RString) key, short flags = PropsNoFlags)
  {
    RDmiObject obj = get(key, flags);
    if (obj == Nil)
      return -1;
    return obj->getDoubleVar();
  }
  void setDoubleVal(IN(RString) key, double val, short flags = PropsNoFlags)
  {
    set(key, new DmiObject(inOf(val)), flags);
  }
  RProps getProps(IN(RString) name, short flags = PropsNoFlags);
  void setProps(IN(RString) name, IN(RProps) props, short flags = PropsNoFlags)
  {
    setObjectVal(name, &props, flags);
  }
  /**
    merge other into this props.
    @param other source
    @param flags Combination of PropsMergeFlags
  */
  void merge(IN(RProps) other, short flags = PropsNoFlags);
  /**
    evaluates embeeded script parrs
      <ul>
      <li> ${expr} where expr is either a Props key or a CfgScript expression
      
      <li> @{key} is a key to a StringArray it will be expanded inline in the string
             if PropsEvalQuoteFileNameArgs is set in the flags the single 
             strings of the StringArray will be quoted to be valid file name
             arguments.
       <li> !{ script }! script will be evaluated as CfgScript script. 
                         the !{ script }! will be replaced with the output 
                         written by the script to 'out'
       </ul>
      
   ${CCC} ${CCOPTS} ${OBJDIR}${DIRSEP}${OBJFILE} -c ${SOURCEFILE}
   After evaluating inplace variable substitution it tries also
   to eval evalShellExecute if PropsEvalWithShell is set in flags

   @param quoteArgs if true and an replaced parameter contains spaces
          the expression will be quoted with "\"" on windows or "'" on unix.
   @see gw_ref[Backtick Operator, acdk_cfgscript_hb_lang_expr_backtick]

  */ 
  RString eval(IN(RString) str, short flags = PropsNoFlags);
  /**
    execute a script
    @param str script content to execute
    @param filename filename of the script to execute (used to find includes)
    @param flags standard property flags
  */
  void execScript(IN(RString) str, IN(RString) filename = "", short flags = PropsParentRead | PropsParentWrite);
  /**
    eval embedded backtick commands like `ls`
  */
  RString evalShellExecute(IN(RString) str, short flags);
  /**
    if prefix is "PREF" imports / overwites
    KEY = PREF_KEY
    @returns true if an value is changed.
  */
  bool importNameSpace(IN(RString) prefix);
  /**
    returns all different vals of given key in this and parents
  */
  RStringArray getAllStringVals(IN(RString) key, short flags = PropsNoFlags);


  /// dumps to System::out
  void dump(int dumpFlags = DumpWithParent, ::acdk::util::RTreeSet keys = Nil, IN(RString) ident = "");
  /**
    this only works for basic types, Strings, Props and Arrays.
    @param keyName the varname of the property
    @param indent indentation
    @param flags combination of enum PropsFlags
  */
  RString asCfgScriptLiteral(IN(RString) keyName, IN(RString) indent = "", short flags = PropsNoFlags);
  /**
    find ACDKHOME or ACDK_HOME in props or System::properties.
  */
  RString getAcdkHome(bool throwIfNotFound = true);
  RString getAcdkToolsHome(bool throwIfNotFound = true);
  const acdk::lang::dmi::ClazzMethodInfo* 
  standardDispatch(  IN(acdk::lang::RString) fname, acdk::lang::dmi::ScriptVar& ret, 
                                acdk::lang::dmi::ScriptVarArray& args, 
                                acdk::lang::dmi::DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const acdk::lang::dmi::ClazzInfo* clazzinfo,
                                const acdk::lang::dmi::ClazzMethodInfo* methinf/* = 0*/);

  void addListener(IN(RPropsChangeListener) listener);
  void removeListener(IN(RPropsChangeListener) listener);
private:
  void _init();
  /**
    @returns true if an value is changed.
  */
  bool _importNameSpace(IN(RString) prefix, IN(RProps) props);
  void _getKeys(IN(RStringArray) sa);
  /**
    unsynchronized version of get
  */
  RDmiObject _get(IN(RString) name, short flags);
  /**
    unsynchronized version of set
  */
  void _set(IN(RString) name, IN(RDmiObject) val, short flags, bool notify = true);
  
  bool _readParent(short flags) 
  { 
    if (flags & PropsNoParentRead)
      return false;
    if (flags & PropsParentRead)
      return true;
    if (_parents == Nil || _parents->length() == 0)
      return false;
    return (_defaultFlags & PropsNoParentRead) != PropsNoParentRead;
  }
  bool _writeParent(short flags) 
  { 
    if (flags & PropsNoParentWrite)
      return false;
    if (flags & PropsParentWrite)
      return true;
    if (_parents == Nil || _parents->length() == 0)
      return false;
    
    return (_defaultFlags & PropsNoParentWrite) != PropsNoParentWrite;
  }
       
  bool _warnRead(short flags)
  {
    if (flags & PropsWarnRead)
      return true;
    if (flags & PropsNoWarnRead)
      return false;
    return (_defaultFlags & PropsNoWarnRead) == PropsNoWarnRead ? false : true;
  }
  bool _warnWrite(short flags)
  {
    if (flags & PropsWarnWrite)
      return true;
    if (flags & PropsNoWarnWrite)
      return false;
    return (_defaultFlags & PropsWarnWrite);
  }
  foreign void  _getKeys(INOUT(::acdk::util::TreeSet) keys, bool withParents = true);
  foreign void  _getAllStringVals(IN(RString) key, short flags, IN(RStringArray) values);
  foreign void _notifyListener(PropChangeEvents event, IN(RString) key);
  foreign void _asCsfLiteral(StringBuffer& sb, IN(RString) keyName, IN(RString) indent, short flags);
  foreign void _asCsfLiteral(StringBuffer& sb, IN(RString) indent, short flags, IN(RString) key, IN(RDmiObject) val);
  foreign RString _eval(IN(RString) str, short flags);
  /// returns the flags encoded as globals in this props
  foreign int _getFlagsFromStringProps(int defaultFlags);
};

/**
  Used to hold varaibles as stack var
*/
struct ScopedCfgVar
{
  RProps _props;
  RString _name;
  short _flags;
  bool _isSet;
  ScopedCfgVar(IN(RProps) props, IN(RString) name)
  : _props(props)
  , _name(name)
  , _flags(0)
  , _isSet(false)
  {
  }
  ScopedCfgVar(IN(RProps) props, IN(RString) name, IN(RDmiObject) val, short flags)
  : _props(props)
  , _name(name)
  , _flags(flags)
  , _isSet(true)
  {
    _props->set(name, val, flags);
  }
  void set(IN(RDmiObject) val, short flags)
  {
    _props->set(_name, val, flags);
    _isSet = true;
  }
  ~ScopedCfgVar()
  {
    if (_isSet == true)
      _props->unset(_name, _flags);
  }
};



} // namespace cfgscript
} // namespace acdk


#endif //acdk_cfgscript_Props_h
