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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/mc/OrbDispatchAttribute.cpp,v 1.11 2005/02/05 10:45:40 kommer Exp $


#include "OrbDispatchAttribute.h"

#include <acdk/lang/System.h>
#include <acdk/tools/mc/StringTagAttribute.h>
#include <acdk/tools/mc/SuperInfo.h>

namespace acdkx {
namespace orb {
namespace mc {

using namespace acdk::lang::dmi;

USING_CLASS(::acdk::util::, Iterator);
using ::acdk::tools::mc::TypeScope;

RString
unR(IN(RString) cname)
{

  int idx = cname->lastIndexOf(':');
  if (idx != -1) {
    RString ns = cname->substr(0, idx + 1);
    return ns + cname->substr(idx + 2);
  }
  return cname->substr(1);
}


 //static 
void 
OrbDispatchAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("OrbProxy", "acdkx/orb/mc/OrbDispatchAttribute");
}


RString 
ArgumentInfo_getCaster(IN(RArgumentInfo) ai) // #### into ArgumentInfo
{
   if (ai->isEnum() == true)
      return "(int)";
   if (ai->isBasicType(ai->getMappedType()) == true)
      return "";
   if (ai->getOrgType()->equals("RString") == true || 
       ai->getOrgType()->equals("::acdk::lang::RString") == true)
       return "";
   return "(::acdk::lang::RObject)";
}

RString 
ArgumentInfo_getCaster(IN(RString) tpname)
{
   if (TypeScope::isBasicType(tpname) == true)
      return "";
   if (tpname->equals("RString") == true || 
       tpname->equals("::acdk::lang::RString") == true)
       return "";
   return "(::acdk::lang::RObject)";
}


//static
RString
ArgumentInfo_getOrbInOutputMethod(IN(RString) readwrite, IN(RString) tpname, int flags, bool withArgsPara = false)
{
  if (tpname->equals("bool") == true)
    return readwrite + "_boolean" + (withArgsPara ? "()" : "");
  if (tpname->equals("char") == true)
    return readwrite + "_octet" + (withArgsPara ? "()" : "");
  if (tpname->equals("short") == true)
    return readwrite + "_short" + (withArgsPara ? "()" : "");
  if (tpname->equals("int") == true)
    return readwrite + "_long" + (withArgsPara ? "()" : "");
  if (tpname->equals("jlong") == true)
    return readwrite + "_long_long" + (withArgsPara ? "()" : "");
  if (tpname->equals("float") == true)
    return readwrite + "_float" + (withArgsPara ? "()" : "");
  if (tpname->equals("double") == true)
    return readwrite + "_double" + (withArgsPara ? "()" : "");
  if (tpname->equals("RString") == true || tpname->equals("::acdk::lang::RString") == true)
    return readwrite + "_string" + (withArgsPara ? "()" : "");
  if (withArgsPara == false)
    return readwrite + "_acdk_object";
  return readwrite + "_acdk_object(" + unR(tpname) + "::GetClass())";
}


RString
ArgumentInfo_getOrbOutputMethod(IN(RString) tpname, int flags)
{
  return ArgumentInfo_getOrbInOutputMethod("write", tpname, flags);
}

RString
ArgumentInfo_getOrbOutputMethod(IN(RArgumentInfo) ai)
{
  return ArgumentInfo_getOrbOutputMethod(ai->getMappedType(), ai->flags);
}

//static
RString
ArgumentInfo_getOrbInputMethod(IN(RString) tpname, int flags)
{
  return ArgumentInfo_getOrbInOutputMethod("read", tpname, flags, true);
}

RString
ArgumentInfo_getOrbInputMethod(IN(RArgumentInfo) ai)
{

  return ArgumentInfo_getOrbInputMethod(ai->getMappedType(), ai->flags);
}


void 
MethodInfo_writeOrbDispatchBody(IN(RClassInfo) ci, IN(RMethodInfo) mi, StringBuffer& sb)
{
  if (mi->isConstructor() == true) 
    return;
  bool hasret = true;
  
  if (mi->returnType->compareTo((RString)"void") == 0) 
    hasret = false;
  
  sb.append("  if (&" + ci->name + "_method_" + mi->getJavaSignature(true/*, argcount*/) + " == methinf) {\n"); 
  
  

  int inParamCount = 0; 
  int outParamCount = 0; 
  
  sb.append("    try {\n");
  
  RIterator ait = mi->args->iterator();
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (MetaInfo::isIn(ai->flags) == true) 
    {
      sb.append("      " + ai->getOrgType() + " " + ai->name  + " = (" + ai->getOrgType() + ")__input." 
          + ArgumentInfo_getOrbInputMethod(ai) + ";\n");
    } 
    else if (MetaInfo::isOut(ai->flags) == true) 
    {
       sb.append("      " + ai->getOrgType() + " " + ai->name + ";\n");
    }
  }
  sb.append("      ");
  if (hasret)
    sb.append(mi->returnType + " __retval = ");
  sb.append(mi->name + "(");
  int ac = 0;
  inParamCount = 0; 
  outParamCount = 0; 
  ait = mi->args->iterator();
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ac > 0)
      sb.append(", ");
    sb.append(ai->name);
    ++ac;
  }
  sb.append(");\n");
  sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createReply();\n");
  if (hasret) {
    sb.append("      __output->" + ArgumentInfo_getOrbOutputMethod(mi->mappedReturnType(), 0) + 
               "(" + ArgumentInfo_getCaster(mi->mappedReturnType()) +  "__retval);\n");
  }
  ait = mi->args->iterator();
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (MetaInfo::isOut(ai->flags) == true) 
    {
      sb.append("      __output->" + ArgumentInfo_getOrbOutputMethod(ai) + 
                 "(" + ArgumentInfo_getCaster(ai) + ai->name + ")"";\n");
    }
  }
  sb.append("      return __output;\n");
  /* not needed to handle each exception seperatelly.
    ::org::omg::CORBA::portable::write_exception() will do the right things
  RIterator it = _throws->iterator();
  while (it->hasNext() == true) {
    RString ex = (RString)it->next();
    sb.append("    } catch (" + ex + " ex) {\n");
    sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
    sb.append("      __output->write_exception(ex);\n");  
    sb.append("      return __output;\n");
  }
  */
  sb.append("    } catch (::acdk::lang::RThrowable ex) {\n");
  sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
  sb.append("      __output->write_exception(ex);\n");  
  sb.append("      return __output;\n");
  sb.append("    } catch (...) {\n");
  sb.append("      throw;\n");
  sb.append("    }\n");
  
  
  sb.append("  }\n");
}


void
MethodInfo_writeOrbProxy(IN(RClassInfo) ci, IN(RMethodInfo) mi, StringBuffer& sb)
{
  if ((mi->flags & MiMcKnownType) != MiMcKnownType) 
  {
    ACDK_LOG(Note, "Not compatible: " + mi->toString());
    return;
  }
  sb.append("  virtual " + mi->returnType + " " + mi->name + "(");
  
  bool prev = false;

  int inParamCount = 0;
  int outParamCount = 0;
  bool hasReturn = false;
  if (mi->returnType->compareTo("void") != 0)
    hasReturn = true;

  if (hasReturn == true)
    ++inParamCount;
  RIterator it = mi->args->iterator();
  while (it->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (prev == true)
      sb.append(", ");
    sb.append(ai->toCode());
    prev = true;
    if (MetaInfo::isIn(ai->flags) == true)
      ++inParamCount;
    if (MetaInfo::isOut(ai->flags) == true)
      ++outParamCount;
  }
  sb.append(")");
  if (mi->_throws->size() > 0) 
  {
    sb.append(" THROWS");
    sb.append(mi->_throws->size());
    sb.append("(");
    it = mi->_throws->iterator();
    bool isFirst = true;
    while (it->hasNext() == true) 
    {
      RString tcls = (RString)it->next();
      if (isFirst == false)
        sb.append(", ");
      sb.append(tcls);
      isFirst = false;
    }
    sb.append(")");
  }
  sb.append("\n  {\n");
  
  sb.append("    while (true) {\n");
  sb.append("    if (is_local() == true) {\n        ");
  if (hasReturn == true) 
    sb.append(mi->returnType + " __retvalue = ");
  
  sb.append("dynamic_cast< " + ci->name + "*>(localObject())->" + mi->name + "(");
  prev = false;
  it = mi->args->iterator();
  
  
  while (it->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (prev == true)
      sb.append(", ");
    sb.append(ai->name);
    prev = true;
  }
  sb.append(");\n");
  if (hasReturn == false) 
    sb.append("        return;\n");
  sb.append("     } else { // not local\n");
  sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = Nil;\n");
  sb.append("      ::org::omg::CORBA::portable::RInputStream __input = Nil;\n");
  sb.append("      try {\n");
  sb.append("        __output = _request(\"" + mi->name + "\", true);\n");

  it = mi->args->iterator();
  while (it->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (MetaInfo::isIn(ai->flags) == true) 
    {
      sb.append("        __output->" + ArgumentInfo_getOrbOutputMethod(ai) + "(" + ArgumentInfo_getCaster(ai) + ai->name + ");\n");
    }
  }
  sb.append("        __input = ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, CorObject)::_invoke(__output);\n");
  if (hasReturn == true) {
    sb.append("        " + mi->orgReturnType() + " __retvalue = (" + mi->orgReturnType() + ")__input->" 
                            + ArgumentInfo_getOrbInputMethod(mi->mappedReturnType(), 0) + ";\n");
  }
  it = mi->args->iterator();
  while (it->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (MetaInfo::isOut(ai->flags) == true) 
    {
      sb.append("        " + ai->name + " = (" + ai->getOrgType() + ") __input->" + ArgumentInfo_getOrbInputMethod(ai) + ";\n");
    }
  }
  if (hasReturn == true) 
    sb.append("        return __retvalue;\n");
  else
    sb.append("        return;\n");
  sb.append("      } catch (::org::omg::CORBA::portable::RRemarshalException ) {\n");
  sb.append("        ; //nothing continue;\n");
  sb.append("      } catch (::org::omg::CORBA::portable::RApplicationException _exception ) {\n");
  sb.append("         ::acdk::lang::RClass _exClass = _exception->getUserExceptionClass();\n");

  sb.append("         __input = _exception->getInputStream();\n");
  
  
  it = mi->_throws->iterator();
  while (it->hasNext() == true) 
  {
    RString excl = (RString)it->next();
    sb.append("         if (_exClass == " + unR(excl) + "::GetClass()) {\n");
    sb.append("           ::acdk::lang::RObject obj = (::acdk::lang::RObject)__input->read_acdk_object(" + unR(excl) + "::GetClass());\n");
    sb.append("           _releaseReply(__input);\n");
    sb.append("           throw (" + excl + ")obj;\n");
    sb.append("         }\n");
  }
  
  sb.append("           THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString(\"Unexpected User Exception: \") + _exClass->getName());\n");
  sb.append("      } \n");
  sb.append("    } // not local\n");
  sb.append("    } // while (true);\n");
  sb.append("  }\n");
}

void 
ClassInfo_generateOrbDispatch(IN(RClassInfo) ci, StringBuffer& sb)
{
  sb.append("//virtual\n::org::omg::CORBA::portable::ROutputStream\n" + ci->name + 
    ":: _invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& __input, ::org::omg::CORBA::portable::ResponseHandler& handler) THROWS1(::org::omg::CORBA::RSystemException)\n{\n");
  sb.append("#if defined(__BORLANDC__) // Borland 5.51 bug has to be solved\n");
  sb.append("  ::acdk::lang::RThrowable ex;\n");
  sb.append("#endif //defined(__BORLANDC__)\n");
  sb.append("  if (method->equals(\"_is_a\") == true) {\n");
  sb.append("    try {\n");
  sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createReply();\n");
  sb.append("      ::acdk::lang::RString name = __input.read_string();\n");
  sb.append("      for (int i = 0; _" + ci->name + "_Skel__ids[i]; i++) {\n");
  sb.append("        if (name->equals(_" + ci->name + "_Skel__ids[i]) == true) {\n");
  sb.append("          __output->write_boolean(true);\n");
  sb.append("          return __output;\n");
  sb.append("        }\n");
  sb.append("      }\n");
  sb.append("      __output->write_boolean(false);\n");
  sb.append("      return __output;\n");
  sb.append("    } catch (::acdk::lang::RThrowable ex) {\n");
  sb.append("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
  sb.append("      __output->write_exception(ex);\n");
  sb.append("      return __output;\n");
  sb.append("    } catch (...) {\n");
  sb.append("      throw;\n");
  sb.append("    }\n");
  sb.append("  }\n");

  sb.append("  const ::acdk::lang::dmi::ClazzMethodInfo* methinf =  ::acdkx::orb::AORB::lookupMethod(method, clazzInfo());\n");
  sb.append("  if (methinf == 0)\n");
  sb.append("    THROW1(Exception, RString(\"Cannot find matching function for \") + method);\n");
  
  RIterator it = ci->_methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->isConstructor() == true || mi->isDestructor() == true) {
      ACDK_LOG(Note, "Not ORB DMI because constructor: " + mi->toString());
      continue;
    }
    if ((mi->flags & MiMcKnownType) != MiMcKnownType) {
      ACDK_LOG(Note, "Note: No ORB DMI because noaccess: " + mi->toString());
      continue;
    }
    if (mi->isStatic() == true || mi->isPublic() == false) 
    {
      RString t = mi->toString();
      ACDK_LOG(Note, "Not ORB DMI because method is not puplic: " + t);
      continue;
    }
    MethodInfo_writeOrbDispatchBody(ci, mi, sb);
  }
  sb.append("  THROW0_FQ(::org::omg::CORBA::, BAD_OPERATION);\n  return Nil;\n");
  sb.append("}\n\n");
}


void 
ClassInfo_generateOrbProxy(IN(RClassInfo) ci, StringBuffer& sb)
{
  sb.append("char* _" + ci->name + "_Skel__ids[] = {\n  \"IDL:");
  sb.append(ci->getJTypeName());
  sb.append(":1.0\",\n  0\n};\n\n");
  if (ci->_publicDecl == Nil)
    ci->_publicDecl = "";
  RString supercls = "CorObject";
  RString superns = "::acdkx::orb::";
  if (false && ci->_derivides->size() > 0)
  {
    //System::in->readLine();
    acdk::tools::mc::RSuperInfo si(ci->_derivides->get(0));
    supercls = si->name + "_Skel";
    superns = "";
    int idx;
    if ((idx = supercls->lastIndexOf("::")) != -1)
    {
      superns = supercls->substr(0, idx + 2);
      supercls = supercls->substr(idx + 2);
    }
  }
  sb <<"class " << ci->_publicDecl << " " << ci->name << "_Skel\n: extends " << 
        superns << supercls << ",\n  implements " << ci->name << "\n{\n";
  sb.append("public:\n");
  sb.append("  char** _ids() { return _" +  ci->name + "_Skel__ids; }\n");
  if (superns->length() > 0)
  {
    sb << "  " << ci->name << "_Skel() : ACDK_FQ_SUPER_QUALIFIER(" << superns << ", " << supercls << "()) { }\n";
    sb << "  " << ci->name << "_Skel(IN(::acdk::lang::RString) objKey) : ACDK_FQ_SUPER_QUALIFIER(" << superns << ", " << supercls << "(objKey)) { }\n";
  } else {
    sb << "  " << ci->name << "_Skel() : " << supercls << "()) { }\n";
    sb << "  " << ci->name << "_Skel(IN(::acdk::lang::RString) objKey) : " << supercls << "(objKey)) { }\n";
  }
  
  sb.append("  static ::acdk::lang::RObject create_instance() { return new " + ci->name + "_Skel(); }\n");
  RIterator it = ci->_methods->iterator();
  while (it->hasNext() == true) 
  {
    RMethodInfo mi = RMethodInfo(it->next());
    MethodInfo_writeOrbProxy(ci, mi, sb);
  }
  sb.append("};\n\n");
  sb.append("::acdkx::orb::SkelInfoClassesStruct __" + ci->name + "_Skel_Info = { " + ci->name + "::clazzInfo(), " + ci->name  + "_Skel::create_instance, 0 };\n");
  sb.append("static::acdkx::orb::RegisterSkelInfoClass _register_" + ci->name + "_Skel(&__" + ci->name + "_Skel_Info);\n\n");
  sb.append("//static\nR" + ci->name + " " +  ci->name + "::GetSkel(IN(::acdk::lang::RString) objKey)\n{\n  return new " + ci->name + "_Skel(objKey);\n}\n");
  
}

//virtual 
bool 
OrbDispatchAttribute::apply(IN(RCodeInfo) codeinfo) 
{ 
  if (instanceof(codeinfo, ClassInfo) == false)
    return false;
  RClassInfo ci(codeinfo);
  StringBuffer sb;
  ClassInfo_generateOrbProxy(ci, sb);
  ClassInfo_generateOrbDispatch(ci, sb);
  ci->addCode(sb.toString(), acdk::tools::mc::ModuleAfterDispatch);
  ci->addCode("#include <acdkx/orb/CorObject.h>\n", acdk::tools::mc::ModuleInclude);
  acdk::tools::mc::RCodeAttribute sta = new acdk::tools::mc::StringTagAttribute("acdkx_orb_ClassType");
  //ci->addCodeAttribute(sta);
  //System::in->readLine();
  sta->apply(codeinfo);
  return true;
}


} // namespace mc
} // namespace orb
} // namespace acdkx

