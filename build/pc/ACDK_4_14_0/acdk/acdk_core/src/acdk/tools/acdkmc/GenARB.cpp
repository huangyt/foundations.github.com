// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/GenARB.cpp,v 1.7 2003/06/19 14:37:16 kommer Exp $
//
// $Log: GenARB.cpp,v $
// Revision 1.7  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.6  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.5.2.1  2003/05/22 12:37:00  kommer
// moved DMI flags from reflect::Modifier to dmi::MetaInfoFlags
//
// Revision 1.5  2001/12/14 12:04:20  kommer
// dos2unix
//
// Revision 1.4  2001/12/09 22:28:22  kommer
// introduced IN() for method parameter
//
// Revision 1.3  2001/12/09 00:22:15  kommer
// introduced IN() for Object parameters
//
// Revision 1.2  2001/12/07 22:53:06  kommer
// ajust namespace
//
// Revision 1.1  2001/12/02 13:47:28  kommer
// initial revision
//
// Revision 1.5  2001/11/21 20:59:10  kommer
// panta rei
//
// Revision 1.4  2001/11/18 14:11:16  kommer
// panta rei
//
// Revision 1.3  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.2  2001/03/10 12:45:24  kommer
// panta rei
//
// Revision 1.1  2001/01/24 13:28:32  kommer
// panta rei
//

#if 0

#include "ClassInfo.h"
#include <acdk/io/File.h>


namespace acdk {
namespace tools {
namespace acdkmc {

using namespace acdk::lang;
using namespace acdk::lang::reflect;
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::util::, Iterator);

//using namespace acdk::util;


void
MethodInfo::writeArbProxy(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (Modifier::isCompatibleType(_access) == false) {
    log(Note, "not accessible Type: " + toString()
    return;
  }
  out->print("  virtual " + returnType + " " + name + "(");
  RIterator it = args->iterator();
  bool prev = false;

  int inParamCount = 0;
  int outParamCount = 0;
  bool hasReturn = false;
  if (returnType->compareTo("void") != 0)
    hasReturn = true;

  if (hasReturn == true)
    ++inParamCount;

  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (prev == true)
      out->print(", ");
    out->print(ai->toCode());
    prev = true;
    if (ai->flags & MiAiIn || ai->flags == 0)
      ++inParamCount;
    if (ai->flags & MiAiOut)
      ++outParamCount;
  }
  out->print(")");
  if (_throws->size() > 0) {
    out->print(" throw (");
    it = _throws->iterator();
    bool isFirst = true;
    while (it->hasNext() == true) {
      RString tcls = (RString)it->next();
      if (isFirst == false)
        out->print(", ");
      out->print(tcls);
      isFirst = false;
    }
    out->print(")");
  }
  out->print("\n  {\n");
  out->print("    if (isLocal() == true) {\n");
  out->print("      ");
  if (hasReturn == true)
    out->print("return ");
  out->print("dynamic_cast<" + _classInfo->_name + "*>(localImpl())->" + name + "(");
  
  prev = false;
  it = args->iterator();
  

  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (prev == true)
      out->print(", ");
    out->print(ai->name);
    prev = true;
  }
  out->print(");\n");
  out->print("    } else {\n");
  //out->print("      ::acdk::lang::dmi::ScriptVar erg;\n");
  int argcount = args->size();
  out->print("      ::acdk::lang::dmi::ScriptVarArray args(" + Integer::toString(inParamCount) + ");\n");
  //not out->print("      ::acdk::lang::dmi::ScriptVarArray ergs(" + Integer::toString(inParamCount) + ");\n");
  out->print("      ::acdk::lang::dmi::ScriptVarArray ergs(0);\n"); // will be appended
  out->print("      ::acdk::lang::dmi::ScriptVar _theEx;\n");
  it = args->iterator();
  int i = 0;
  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (ai->flags == 0 || ai->flags & MiAiIn ) {
      out->print("      args[" + Integer::toString(i) + "] = " + ArgumentInfo::castToScriptVar(ai->type) + " " + ai->name + ";\n");
      i++;
    }
  }
  out->print("      extern ::acdk::lang::dmi::ClazzMethodInfo " + _classInfo->_name + "_method_" +  getJavaSignature(true) + ";\n");
  out->print("      delegater()->invoke(::acdkx::arb::ARB::getARB(), _objID, &" 
        + _classInfo->_name + "_method_" +  getJavaSignature(true) + ", args, ergs, _theEx);\n");
  
  out->print("      if (_theEx.type == ::acdk::lang::dmi::ScriptVar::ObjectType) {\n");
  out->print("        RObject exobj = _theEx.getObjectVar();\n");
  it = _throws->iterator();
  while (it->hasNext() == true) {
    RString excl = (RString)it->next();
    out->print("        if (exobj->clazzInfo() == " + excl + "::clazzInfo()) \n");
    out->print("          throw (" + excl + ")exobj;\n");
  }
  out->print("        if (instanceof(exobj, ::acdk::lang::Throwable)) \n");
  out->print("          throw (::acdk::lang::RThrowable)exobj;\n");
  out->print("        throw exobj;\n");
  out->print("      }\n");
  if (hasReturn == true)
    i = 1;
  else
    i = 0;
  it = args->iterator();
  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (ai->flags & MiAiOut) {
      out->print("      " + ai->name + " = (" + ai->type  + ")ergs[" + Integer::toString(i) + "]." + ArgumentInfo::getScriptVarGetter(ai->type) + ";\n");
      i++;
    }
  }
  if (hasReturn == true) {
    out->print("      return (" + returnType  + ")ergs[0]." + ArgumentInfo::getScriptVarGetter(returnType) + ";\n");
  }
  out->print("    }\n");
  out->print("  }\n");
}




 
/* This is a sample for arbdispatch

virtual 
::acdk::lang::dmi::ClazzMethodInfo* arbDispatch(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::ScriptVarArray& ergs, ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0)
{
  // RInteger foo(IN(int) i, OUT(RString) s, INOUT(float) f);
  if (metodinf == foo) {
    int i;
    float f = arg[1].getFloatVar();
    RInteger erg = foo(i, (RString)arg[0].getObjectVar(), f);
    ergs.push_back(erg);
    ergs.push_back(f);
    return metodinf;
  }
    return 0;
}
*/

void 
MethodInfo::writeArbDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (isConstructor() == true) 
    return;
  bool hasret = true;
  if (returnType->compareTo((RString)"void") == 0) 
    hasret = false;
  
  out->print("  if (&" + clsinfo->_name + "_method_" + getJavaSignature(true/*, argcount*/) + " == methinf) {\n"); 
  
  int inParamCount = 0; 
  int outParamCount = 0; 
  //int curarg = 0;
  if (_throws->size() > 0) {
    out->print("    try {\n");
  }
  RIterator ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ai->flags & MiAiOut && !(ai->flags & MiAiIn)) {
      out->print("    " + ai->type + " " + ai->name + ";\n");
  } else if (ai->flags & MiAiOut && ai->flags & MiAiIn) {
      out->print("    " + ai->type + " " + ai->name  + " = (" + ai->type + ")args[" + Integer::toString(outParamCount) + "]." + ArgumentInfo::getScriptVarGetter(ai->type) + ";\n");
      ++outParamCount;
    } else  if (ai->flags & MiAiIn || ai->flags == 0) {
      ++inParamCount;
    }
  }
  out->print("    ");
  if (hasret)
    out->print(returnType + " retval = ");
  out->print(name + "(");

  
  int ac = 0;
  inParamCount = 0; 
  outParamCount = 0; 
  ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ac > 0)
      out->print(", ");
    if (ai->flags & MiAiOut && !(ai->flags & MiAiIn)) {
      out->print(ai->name);
      ++outParamCount;
    } else if (ai->flags & MiAiOut && ai->flags & MiAiIn) {
      out->print(ai->name);
      ++outParamCount;
      ++inParamCount;
    } else  if (ai->flags & MiAiIn || ai->flags == 0) {
      out->print("(" + ai->type + ")args[" + Integer::toString(outParamCount) + "]." + ArgumentInfo::getScriptVarGetter(ai->type));
      ++inParamCount;
    }
    ++ac;
  }
  out->print(");\n");
  outParamCount = 0;
  if  (hasret == true) {
    out->print("    ergs.add(" + ArgumentInfo::castToScriptVar(returnType) + "retval);\n");
    ++outParamCount;
  }

  ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ai->flags & MiAiOut) {
      out->print("    ergs.add(" + ArgumentInfo::castToScriptVar(ai->type) + ai->name + ");\n");
      ++outParamCount;
    }
  }
  if (_throws->size() > 0) {
    RIterator it = _throws->iterator();
    while (it->hasNext() == true) {
      RString ex = (RString)it->next();
      out->print("    } catch (" + ex + " ex) {\n");
      out->print("      _theEx = ex;\n");
    }
    out->print("    }\n");
  }
  out->print("    return methinf;\n");
  out->print("  }\n");
}

void 
ClassInfo::generateArbDispatch(IN(RPrintWriter) out)
{
  out->print("//virtual\nconst ::acdk::lang::dmi::ClazzMethodInfo* \n" + _name + "::arbDispatch(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::ScriptVarArray& ergs, ::acdk::lang::dmi::ScriptVar& _theEx, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n{\n");
  out->print("  if (methinf == 0)\n    methinf = ::acdkx::arb::AObjectImpl::lookupMethod(fname, args, clazzInfo());\n");
  out->print("  if (methinf == 0)\n");
  out->print("    THROW1(Exception, RString(\"Cannot find matching function for \") + fname);\n");
  
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->isConstructor() == true || mi->isDestructor() == true) {
      log(Note, "Not write because constructor: " + mi->toString());
      continue;
    }
    if (Modifier::isCompatibleType(mi->_access) == false) {
      log(Note, "Not write because not compatible: " + mi->toString());
      continue;
    }
    if (mi->isStatic() == true || mi->isPublic() == false) {
      RString t = mi->toString();
      log(Note, "Mot write because method is not puplic: " + t);
      continue;
    }
    mi->writeOrbDispatchBody(out, this);
  }
  out->print("}\n\n");
}

void 
ClassInfo::generateARB(IN(RPrintWriter) out)
{
  if (_isCorbaInterface == false)
    return;
  /*
  RFile nf = new File(_module->getParent(), _name + "_" + "Proxy.cpp");
  RPrintWriter out = new PrintWriter(new FileWriter(nf));
  out->print("// Generated by acdkmc\n\n");
  out->print("// Copyrighted by Roger Rene Kommer, artefaktur\n//\n// Dont edit this file manually\n// \n\n");
  */
  /*
  StringBuffer headersecure;
  RIterator it = _namespace->iterator();
  while (it->hasNext() == true) {
    headersecure.append(RString(it->next())); headersecure.append("_");
  } 
  headersecure.append(_name + "_" + "Proxy_h");
  out->print("#ifndef " +   headersecure.toString() + "\n");
  out->print("#define " +   headersecure.toString() + "\n");
  out->print("#include <acdk.h>\n");
  out->print("#include <acdkx/arb/arb.h>\n#include <acdkx/arb/AObjectImpl.h>\n\n"); 

  out->print("#include \"" +  _module->baseFilename() + "\"\n");
  */
  
  //writeOpenNamespace(out);

  out->print("class " + _publicDecl + " " + _name + "_Proxy\n: extends ::acdkx::arb::AObjectImpl,\n  implements " + _name + "\n{\n");
  out->print("public:\n");
  out->print("  " + _name + "_Proxy() : AObjectImpl() { }\n");
  out->print("  " + _name + "_Proxy(IN(::acdkx::arb::RObjectID) objid) : AObjectImpl(objid) { }\n");
  out->print("  " + _name + "_Proxy(IN(RObject) localObject) : AObjectImpl(localObject) { }\n");
  out->print("  static RObject create_instance() { return new " + _name + "_Proxy(); }\n");
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = RMethodInfo(it->next());
    mi->writeOrbProxy(out, this);
  }
  out->print("};\n\n");
  out->print("::acdkx::arb::ProxyClassesStruct __" + _name + "_Proxy_Info = { " + _name + "::clazzInfo(), " + _name  + "_Proxy::create_instance, 0 };\n");
  out->print("static::acdkx::arb::RegisterProxyClass _register_" + _name + "_Proxy(&__" + _name + "_Proxy_Info);\n\n");

  out->print("//static\nR" + _name + " " + _name + "::GetProxy(IN(::acdkx::arb::RObjectID) objid)\n{\n  return new " + _name + "_Proxy(objid);\n}\n");
  out->print("//static\nR" + _name + " " + _name + "::GetProxy(IN(RObject) localObject)\n{\n  return new " + _name + "_Proxy(localObject);\n}\n");
  

  //writeCloseNamespace(out);

  //out->print("#endif // " +   headersecure.toString() + "\n\n");
  //out->print("\n\n");
  generateArbDispatch(out);
}

} // acdkmc
} // tools
} // acdk
#endif //0
