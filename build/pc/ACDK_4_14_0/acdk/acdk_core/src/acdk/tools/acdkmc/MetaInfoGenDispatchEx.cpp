// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/MetaInfoGenDispatchEx.cpp,v 1.15 2005/03/07 14:02:13 kommer Exp $
//
// $Log: MetaInfoGenDispatchEx.cpp,v $
// Revision 1.15  2005/03/07 14:02:13  kommer
// removed reference to std::cout etc
//
// Revision 1.14  2004/02/27 00:24:57  kommer
// typo
//
// Revision 1.13  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.12  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.11.2.3  2003/05/30 15:52:41  kommer
// panta rei
//
// Revision 1.11.2.2  2003/05/22 12:37:00  kommer
// moved DMI flags from reflect::Modifier to dmi::MetaInfoFlags
//
// Revision 1.11.2.1  2003/03/10 13:09:22  kommer
// metainf as external loadable so/dll
//
// Revision 1.11  2003/02/07 14:11:20  kommer
// panta rei
//
// Revision 1.10  2002/08/30 00:45:27  kommer
// dos2unix
//
// Revision 1.9  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
//
// Revision 1.8  2002/06/30 00:43:28  kommer
// dos2unix
//
// Revision 1.7  2002/06/30 00:35:26  kommer
// panta rei
//
// Revision 1.6  2002/06/16 12:15:14  kommer
// support the ACDK_WITH_DMIPROXY class attribute
//
// Revision 1.5  2002/06/13 13:12:28  kommer
// implemented handling of DmiProxy
//
// Revision 1.4  2001/12/14 12:04:20  kommer
// dos2unix
//
// Revision 1.3  2001/12/09 00:22:15  kommer
// introduced IN() for Object parameters
//
// Revision 1.2  2001/12/07 22:53:06  kommer
// ajust namespace
//
// Revision 1.1  2001/12/02 13:48:20  kommer
// initial revision
//
// Revision 1.5  2001/11/21 20:59:10  kommer
// panta rei
//
// Revision 1.4  2001/11/18 16:43:15  kommer
// panta rei
//
// Revision 1.3  2001/11/09 13:22:01  kommer
// dos2unix
//
// Revision 1.2  2001/09/30 17:36:35  kommer
// panta rei
//
// Revision 1.1  2001/09/30 12:31:12  kommer
// initial revision
//
// Revision 1.8  2001/08/12 15:25:00  kommer
// dos2unix
//
// Revision 1.7  2001/08/11 13:52:57  kommer
// panta rei
//
// Revision 1.6  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.5  2001/04/28 11:52:40  kommer
// panta rei
//
// Revision 1.4  2001/04/27 18:50:03  kommer
// enum is valid type and ACDK2IDL first sketch
//
// Revision 1.3  2001/04/16 10:37:21  kommer
// panta rei
//
// Revision 1.2  2001/01/24 13:28:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/12/11 18:05:16  kommer
// ACDK Free edition
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:19  roger
// initial acdk sources
//
// Revision 1.4  2000/08/28 07:50:16  roger
// changed standardDispatch()
//
// Revision 1.3  2000/06/22 20:00:09  roger
// rollback of jb changes, restoring orignal (on my machine working) code
//
// Revision 1.1  2000/04/14 09:09:49  roger
// *** empty log message ***
//
// Revision 1.3  2000/03/14 11:59:03  roger
// panta rei
//
// Revision 1.2  2000/03/13 18:52:48  roger
// panta rei
//
// Revision 1.1  2000/03/05 11:21:41  roger
// panta rei
//


#include "ClassInfo.h"


namespace acdk {
namespace tools {
namespace acdkmc {

using namespace acdk::lang;
using namespace acdk::lang::reflect;
using namespace acdk::io;
using namespace acdk::util;


#if 0
static
RString 
getArgConverter(IN(RArgumentInfo) ai, int argcount) // in class or header
{
  RString tn = ai->getOrgType();
  if (ai->isEnum() == true)
    return RString("(") + tn + ")args[" + argcount + "].getIntVar()";
  if (tn->compareTo((RString)"bool") == 0)
    return RString("args[") + argcount + "].getBoolVar()";
  if (tn->compareTo((RString)"char") == 0)
    return RString("args[") + argcount + "].getCharVar()";
  if (tn->compareTo((RString)"byte") == 0)
    return RString("args[") + argcount + "].getByteVar()";
  if (tn->compareTo((RString)"short") == 0)
    return RString("args[") + argcount + "].getShortVar()";
  if (tn->compareTo((RString)"int") == 0)
    return RString("args[") + argcount + "].getIntVar()";
  if (tn->compareTo((RString)"jlong") == 0)
    return RString("args[") + argcount + "].getLongVar()";
  if (tn->compareTo((RString)"float") == 0)
    return RString("args[") + argcount + "].getFloatVar()";
  if (tn->compareTo((RString)"double") == 0)
    return RString("args[") + argcount + "].getDoubleVar()";
  return "(" + tn + ")" + RString("args[") + argcount + "].getObjectVar()";
}

void
MethodInfo::writeDispatchBodyEx(IN(RPrintWriter) out, IN(RClassInfo) clsinfo, int argcount) // ### kill code
{
  out->print("  if (&" + clsinfo->_name + "_method_" + getJavaSignature(true, argcount) + " == methinf) {\n");
  //out->print("  if (strcmp(methinf->java_signature, \"" + mi->getJavaSignature(false) + "\") == 0) {\n");
  
  bool hasret = false;
  if (isConstructor() == true) 
    out->print("    ret = (::acdk::lang::RObject)new " + name + "(");
  else if (returnType->compareTo((RString)"void") == 0)
    out->print("    " + name + "(");
  else if (isBasicType(mappedReturnType()) == true) {
    if (hasType(returnType) == Enum)
      out->print("    ret = (" + returnType + ")" + name + "(");
    else
      out->print("    ret = " + name + "(");
    hasret = true;
  } else 
    out->print("    ret = (::acdk::lang::RObject)" + name + "(");
  RIterator ait = args->iterator();
  int ac = 0;
  int argmaxcount = argcount;
  while (ait->hasNext() == true && argmaxcount > 0) {
    if (ac > 0)
      out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    out->print(getArgConverter(ai, ac));
    ++ac;
    --argmaxcount;
  }
  
  out->print(");\n");
  out->print("    return methinf;\n");
  out->print("  }\n");
}


void
MethodInfo::writeDispatchBodyEx(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)// ### kill code
{
  int nodefargslength = getNoDefaultArgCount();
  int allargslength = args->size();
  while (allargslength >= nodefargslength) {
    writeDispatchBody(out, clsinfo, nodefargslength);
    ++nodefargslength;
  }
}

void
ClassInfo::generateDispatchBodyEx(IN(RPrintWriter) out, bool statics)// ### kill code
{
  out->print("{\n");
  out->print("  if (methinf == 0)\n");
  out->print("    methinf = ::acdk::lang::Object::lookupMethod(fname, args, clazzInfo());\n");
  out->print("  if (methinf == 0)\n");
  /* ###
    THROW1_FQ(::acdk::lang::, InvocationTargetException, "Cannot find matching function for "
    + getNamespaceAccessor() + "::" _name + fname + "\" + getArgList(args) \"); \n"):
  */
  out->print("    THROW1(Exception, RString(\"Cannot find matching function for \") + fname);\n");
  /**
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->isPublic() == false) 
      continue;
    if (Modifier::isAbstract(_flags) == true && mi->isConstructor() == true)
      continue;
    if (Modifier::isCompatibleType(mi->_access) == false || (mi->isDestructor() == true))
      continue;
    if (mi->isStatic() != statics || mi->isPublic() == false)
      continue;
    mi->writeDispatchBody(out, this);
  }
  it = _derivides->iterator();
  
  while (it->hasNext() == true) {
    RSuperInfo si = (RSuperInfo)it->next();
    RString sname = si->name;
    if (sname->lastIndexOf(':') != -1)
      sname = sname->substr(sname->lastIndexOf(':') + 1);
    if (sname->compareTo((RString)"ObjectBase") != 0) {
      out->print("#ifdef ACDK_NEED_FQ_SUPER_QUALIFIER\n");
      out->print(RString("  if ((methinf = ") + si->name + "::" + (statics == true ? "S" : "s") + "tandardDispatch(fname, ret, args, methinf)) != 0)\n");
      out->print("#else //ACDK_NEED_FQ_SUPER_QUALIFIER\n");
      out->print(RString("  if ((methinf = ") + sname + "::" + (statics == true ? "S" : "s") + "tandardDispatch(fname, ret, args, methinf)) != 0)\n");
      out->print("#endif //ACDK_NEED_FQ_SUPER_QUALIFIER\n");
      out->print("    return methinf;\n");
    }
  }
  if (statics == false) 
    out->print("  return StandardDispatch(fname, ret, args, methinf);\n");
  out->print("  return 0;\n");
  */
  out->print("}\n");
}

void
ClassInfo::generateDispatchEx(IN(RPrintWriter) out)// ### kill code
{
  return; // never call this stuff here 
  if (/*_isInterface == true || */_hasScriptable == false )
    return;
  out->print("//virtual\n");
  out->print("void\n");
  out->print(_name + "::invoke(::acdk::lang::sys::ScriptGlue& dmi_glue, ::acdk::lang::dmi::ClazzMethodInfo*& methinf, const char* fname,	::acdk::lang::dmi::ScriptVarArray& in, ::acdk::lang::dmi::ScriptVarArray& out)\n");
  
  generateDispatchBodyEx(out, false);

  out->print("//static\n");
  out->print("void\n");
  out->print(_name + "::staticInvoke(::acdk::lang::sys::ScriptGlue& dmi_glue, ::acdk::lang::dmi::ClazzMethodInfo*& methinf, const char* fname,	::acdk::lang::dmi::ScriptVarArray& in, ::acdk::lang::dmi::ScriptVarArray& out)\n");
  generateDispatchBodyEx(out, true);
}


void 
MethodInfo::writeDmiProxyMethod(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (_access & MiStatic ||
      _access & MiPrivate ||
      isConstructor() == true ||
      name->equals("getClass") == true)
    return;
  
  out->print("  " + returnType + " " + name + "(");

  RIterator ait = args->iterator();
  int ac = 0;
  while (ait->hasNext() == true) 
  {
    if (ac > 0)
      out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    out->print(ai->toCode());
    ++ac;

  }
  out->print(")");
  if (_throws->size() > 0)
  {
    out->print(RString(" THROWS") + _throws->size() + "(");
    ac = 0;
    RIterator throws = _throws->iterator();
    while (throws->hasNext() == true)
    {
      RString ct = (RString)throws->next();
      if (ac > 0)
        out->print(", ");
      out->print(ct);
      ++ac;
    }
    out->print(")");
  }
  
  out->print("\n  {\n");
  out->print("    ");
  if (returnType->equals("void") == false)
      out->print("return ");
  if (hasType(returnType) == Enum)
  {
    out->print("(" + returnType + ")(int)");
  } 
  else if (isBasicType(returnType) == false) 
  {
    out->print("(" + returnType + ")(::acdk::lang::RObject)");
  }

  out->print("_dmiserver->invoke(\"" + name + "\"");
  
  ait = args->iterator();
  while (ait->hasNext() == true) 
  {
    out->print(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
      out->print("inoutOf(" + ai->name + ")");
    else if (ai->flags & MiAiOut)
      out->print("outOf(" + ai->name + ")");
    else
      out->print("inOf(" + ai->name + ")");
    ++ac;

  }
  out->print(");\n  }\n");
}

void 
ClassInfo::generateDmiProxy(IN(RPrintWriter) out)
{

  out->print("\nclass " + _name + "_DmiProxy\n");
  out->print(": extends ::acdk::lang::dmi::DmiProxy\n");
  out->print(", implements " + _name + "\n{\n");
  out->print("  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo(bool loadmetainfo = true)  { return " + _name + "::clazzInfo(loadmetainfo); } \n");
  // out->print("  virtual ::acdk::lang::RClass getClass() { return " + _name + "::getClass(); } \n");
  //out->print("  virtual const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(const char* fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0) { return  \\n");
  
  out->print("public:\n");
  out->print("  " + _name + "_DmiProxy(IN(::acdk::lang::RObject) delegate)\n");
  out->print("  : DmiProxy(delegate)\n  {}\n");
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next()); 
    mi->writeDmiProxyMethod(out, this);
  }
  out->print("};\n\n");
  out->print("::acdk::lang::dmi::RDmiProxy\n" + _name + "::getDmiProxy(IN(::acdk::lang::RObject) dmiserver)\n{\n");
  out->print("  return new " + _name + "_DmiProxy(dmiserver);\n}\n\n");

  
}

#endif //0

} // acdkmc
} // tools
} // acdk

