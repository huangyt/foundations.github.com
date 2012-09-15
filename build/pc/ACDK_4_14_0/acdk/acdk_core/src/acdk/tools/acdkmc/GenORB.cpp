// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/GenORB.cpp,v 1.14 2003/06/19 14:37:16 kommer Exp $
//
// $Log: GenORB.cpp,v $
// Revision 1.14  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.13  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.12.2.2  2003/05/22 12:37:00  kommer
// moved DMI flags from reflect::Modifier to dmi::MetaInfoFlags
//
// Revision 1.12.2.1  2003/03/10 13:09:21  kommer
// metainf as external loadable so/dll
//
// Revision 1.12  2002/09/06 14:05:57  kommer
// dos2unix
//
// Revision 1.11  2002/09/04 10:45:46  kommer
// introduced MetaAttributes
//
// Revision 1.10  2002/08/30 00:45:27  kommer
// dos2unix
//
// Revision 1.9  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
//
// Revision 1.8  2002/01/13 17:20:32  kommer
// dos2unix & bcc compatiblity
//
// Revision 1.7  2002/01/13 16:12:32  kommer
// borland fixes
//
// Revision 1.6  2002/01/02 19:15:31  kommer
// panta rei
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
// Revision 1.1  2001/12/02 13:47:42  kommer
// initial revision
//
// Revision 1.10  2001/11/21 20:59:10  kommer
// panta rei
//
// Revision 1.9  2001/11/18 14:11:16  kommer
// panta rei
//
// Revision 1.8  2001/06/01 08:55:18  kommer
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
// Revision 1.2  2001/03/02 17:49:16  kommer
// enhanced for CORBA
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

/*

Sample:

namespace anamespace {

class ASampleInterface
{
  ACDK_WITH_METAINFO(ASampleInterface)
  ACDKX_ORB_INTERFACE(ASampleInterface)
public:
  virtual int foomethod(RString str) throws (RAnexception, RThrowable) = 0;
};

char* _ASampleInterface_Skel__ids[] =
{ 
  "IDL:anamespace/ASampleInterface:1.0",
  0
};

  // for Client implementation
class ASampleInterface_Skel
: extends ::org::omg::CORBA::portable::ObjectImpl,
  implements ASampleInterface
{
public:
  char** _ids () { return _ASampleInterface_Skel__ids;  }
  virtual int length(RString s) throws (RAnexception, RThrowable) 
  {
    while (true) {
      if(_is_local() == false) {
        ::org::omg::CORBA::portable::ROutputStream output = null;
        ::org::omg::CORBA::portable::RInputStream input = null;
        try {
          _output = _request("length", true);
          _output->write_string(s);
          _input = _invoke(_output);
          return _input->read_long();
        } catch (::org::omg::CORBA::portable::RRemarshalException ) {
          continue;
        } catch (::org::omg::CORBA::portable::RApplicationException _exception) {
          ::acdk::lang::RString _exception_id = _exception->getId();
          if (_exception_id->equals(Anexception::id())) {
            _input = _exception->getInputStream();
            throw Example.AnExceptionHelper.read(_input);
          }
          THROW1(UNKNOWN, RString("Unexpected User Exception: ") + _exception_id);
          _releaseReply(_input);
        } chatch (...) {
          _releaseReply(_input);
        }
      } else {
        
        ::org::omg::CORBA::portable::ServantObject _so = _servant_preinvoke("length", _opsClass);
        if (_so == null) {
          continue;
        }
        Example.AnInterfaceOperations _self = (Example.AnInterfaceOperations)_so.servant;
        try {
          return _self->length(s);
        } catch(...) {
          _servant_postinvoke(_so);
          throw;
        }
      }
    }
  }
};


  !!!!! wrong
//virtual
::acdk::lang::dmi::ClazzMethodInfo* 
ASampleInterface::_invoke(::acdk::lang::RString _opName, 
                          ::org::omg::CORBA::portable::InputStream& _input, 
                          ::org::omg::CORBA::portable::ResponseHandler& _handler,
                          ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  if (methinf == 0)
    methinf = ::acdkx::arb::AObjectImpl::lookupMethod(fname, clazzInfo());
  if (methinf == ASampleInterface_method_length) {
    try {
      RString s = input.read_string();
      int _result = foomethod(s);
      ::org::omg::CORBA::portable::ROutputStream _output = handler.createReply();
      _output.write_long(_result);
  } catch (RAnexception _exception) {
    ::org::omg::CORBA::portable::ROutputStream _output = handler.createExceptionReply();
    _output->writeObject(_exception);
    
  }
  return methinf;
}

} // namespace anamespace 

*/

using namespace acdk::lang;
using namespace acdk::lang::reflect;
USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::util::, Iterator);

//using namespace acdk::util;



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

RString
ArgumentInfo::getOrbOutputMethod()
{
  
  return getOrbOutputMethod(getMappedType(), flags);
}

RString
ArgumentInfo::getOrbInputMethod()
{

  return getOrbInputMethod(getMappedType(), flags);
}

//static
RString
ArgumentInfo::getOrbOutputMethod(IN(RString) tpname, int flags)
{
  return getOrbInOutputMethod("write", tpname, flags);
}

//static
RString
ArgumentInfo::getOrbInputMethod(IN(RString) tpname, int flags)
{
  return getOrbInOutputMethod("read", tpname, flags, true);
}

//static
RString
ArgumentInfo::getOrbInOutputMethod(IN(RString) readwrite, IN(RString) tpname, int flags, bool withArgsPara)
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
ArgumentInfo::getCaster()
{
   if (isEnum() == true)
      return "(int)";
   if (isBasicType(getMappedType()) == true)
      return "";
   if (getOrgType()->equals("RString") == true || 
       getOrgType()->equals("::acdk::lang::RString") == true)
       return "";
   return "(::acdk::lang::RObject)";
}

//static 
RString 
ArgumentInfo::getCaster(IN(RString) tpname)
{
   if (TypeScope::isBasicType(tpname) == true)
      return "";
   if (tpname->equals("RString") == true || 
       tpname->equals("::acdk::lang::RString") == true)
       return "";
   return "(::acdk::lang::RObject)";
}

void
MethodInfo::writeOrbProxy(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (Modifier::isCompatibleType(_access) == false) {
    log(Note, "Not compatible: " + toString());
    return;
  }
  out->print("  virtual " + returnType + " " + name + "(");
  
  bool prev = false;

  int inParamCount = 0;
  int outParamCount = 0;
  bool hasReturn = false;
  if (returnType->compareTo("void") != 0)
    hasReturn = true;

  if (hasReturn == true)
    ++inParamCount;
  RIterator it = args->iterator();
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
  
  out->print("    while (true) {\n");
  out->print("    if (is_local() == true) {\n        ");
  if (hasReturn == true) 
    out->print(returnType + " __retvalue = ");
  
  out->print("dynamic_cast< " + _classInfo->_name + "*>(localObject())->" + name + "(");
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
  if (hasReturn == false) 
    out->print("        return;\n");
  out->print("     } else { // not local\n");
  out->print("      ::org::omg::CORBA::portable::ROutputStream __output = Nil;\n");
  out->print("      ::org::omg::CORBA::portable::RInputStream __input = Nil;\n");
  out->print("      try {\n");
  out->print("        __output = _request(\"" + name + "\", true);\n");

  it = args->iterator();
  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (Modifier::isInParam(ai->flags) == true || ai->flags == 0) {
      out->print("        __output->" + ai->getOrbOutputMethod() + "(" + ai->getCaster() + ai->name + ");\n");
    }
  }
  out->print("        __input = ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate)::_invoke(__output);\n");
  if (hasReturn == true) {
    out->print("        " + orgReturnType() + " __retvalue = (" + orgReturnType() + ")__input->" 
                            + ArgumentInfo::getOrbInputMethod(mappedReturnType(), 0) + ";\n");
  }
  it = args->iterator();
  while (it->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)it->next();
    if (Modifier::isOutParam(ai->flags) == true) {
      out->print("        " + ai->name + " = (" + ai->getOrgType() + ") __input->" + ai->getOrbInputMethod() + ";\n");
    }
  }
  if (hasReturn == true) 
    out->print("        return __retvalue;\n");
  else
    out->print("        return;\n");
  out->print("      } catch (::org::omg::CORBA::portable::RRemarshalException ) {\n");
  out->print("        ; //nothing continue;\n");
  out->print("      } catch (::org::omg::CORBA::portable::RApplicationException _exception ) {\n");
  out->print("         ::acdk::lang::RClass _exClass = _exception->getUserExceptionClass();\n");

  out->print("         __input = _exception->getInputStream();\n");
  
  
  it = _throws->iterator();
  while (it->hasNext() == true) {
    RString excl = (RString)it->next();
    out->print("         if (_exClass == " + unR(excl) + "::GetClass()) {\n");
    out->print("           RObject obj = (RObject)__input->read_acdk_object(" + unR(excl) + "::GetClass());\n");
    out->print("           _releaseReply(__input);\n");
    out->print("           throw (" + excl + ")obj;\n");
    out->print("         }\n");
  }
  
  out->print("           THROW1_FQ(::org::omg::CORBA::, UNKNOWN, RString(\"Unexpected User Exception: \") + _exClass->getName());\n");
  out->print("      } \n");
  out->print("    } // not local\n");
  out->print("    } // while (true);\n");
  out->print("  }\n");
}


void 
MethodInfo::writeOrbDispatchBody(IN(RPrintWriter) out, IN(RClassInfo) clsinfo)
{
  if (isConstructor() == true) 
    return;
  bool hasret = true;
  
  if (returnType->compareTo((RString)"void") == 0) 
    hasret = false;
  
  out->print("  if (&" + clsinfo->_name + "_method_" + getJavaSignature(true/*, argcount*/) + " == methinf) {\n"); 
  
  

  int inParamCount = 0; 
  int outParamCount = 0; 
  
  out->print("    try {\n");
  
  RIterator ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (Modifier::isInParam(ai->flags) == true || ai->flags == 0) {
      out->print("      " + ai->getOrgType() + " " + ai->name  + " = (" + ai->getOrgType() + ")__input." + ai->getOrbInputMethod() + ";\n");
    } else if (Modifier::isOutParam(ai->flags) == true) {
       out->print("      " + ai->getOrgType() + " " + ai->name + ";\n");
    }
  }
  out->print("      ");
  if (hasret)
    out->print(returnType + " __retval = ");
  out->print(name + "(");
  int ac = 0;
  inParamCount = 0; 
  outParamCount = 0; 
  ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ac > 0)
      out->print(", ");
    out->print(ai->name);
    ++ac;
  }
  out->print(");\n");
  out->print("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createReply();\n");
  if (hasret) {
    out->print("      __output->" + ArgumentInfo::getOrbOutputMethod(mappedReturnType(), 0) + 
               "(" + ArgumentInfo::getCaster(mappedReturnType()) +  "__retval);\n");
  }
  ait = args->iterator();
  while (ait->hasNext() == true) {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (Modifier::isOutParam(ai->flags) == true) {
      out->print("      __output->" + ai->getOrbOutputMethod() + "(" + ai->getCaster() + ai->name + ")"";\n");
    }
  }
  out->print("      return __output;\n");
  /* not needed to handle each exception seperatelly.
    ::org::omg::CORBA::portable::write_exception() will do the right things
  RIterator it = _throws->iterator();
  while (it->hasNext() == true) {
    RString ex = (RString)it->next();
    out->print("    } catch (" + ex + " ex) {\n");
    out->print("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
    out->print("      __output->write_exception(ex);\n");  
    out->print("      return __output;\n");
  }
  */
  out->print("    } catch (::acdk::lang::RThrowable ex) {\n");
  out->print("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
  out->print("      __output->write_exception(ex);\n");  
  out->print("      return __output;\n");
  out->print("    } catch (...) {\n");
  out->print("      throw;\n");
  out->print("    }\n");
  
  
  out->print("  }\n");
}

void 
ClassInfo::generateOrbDispatch(IN(RPrintWriter) out)
{
  out->print("//virtual\n::org::omg::CORBA::portable::ROutputStream\n" + _name + 
    ":: _invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& __input, ::org::omg::CORBA::portable::ResponseHandler& handler) throw (::org::omg::CORBA::RSystemException, RThrowable)\n{\n");
  out->print("#if defined(__BORLANDC__) // Borland 5.51 bug has to be solved\n");
  out->print("  ::acdk::lang::RThrowable ex;\n");
  out->print("#endif //defined(__BORLANDC__)\n");
  out->print("  if (method->equals(\"_is_a\") == true) {\n");
  out->print("    try {\n");
  out->print("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createReply();\n");
  out->print("      ::acdk::lang::RString name = __input.read_string();\n");
  out->print("      for (int i = 0; _" + _name + "_Skel__ids[i]; i++) {\n");
  out->print("        if (name->equals(_" + _name + "_Skel__ids[i]) == true) {\n");
  out->print("          __output->write_boolean(true);\n");
  out->print("          return __output;\n");
  out->print("        }\n");
  out->print("      }\n");
  out->print("      __output->write_boolean(false);\n");
  out->print("      return __output;\n");
  out->print("    } catch (::acdk::lang::RThrowable ex) {\n");
  out->print("      ::org::omg::CORBA::portable::ROutputStream __output = handler.createExceptionReply();\n");
  out->print("      __output->write_exception(ex);\n");
  out->print("      return __output;\n");
  out->print("    } catch (...) {\n");
  out->print("      throw;\n");
  out->print("    }\n");
  out->print("  }\n");

  out->print("  const ::acdk::lang::dmi::ClazzMethodInfo* methinf =  ::acdkx::orb::AORB::lookupMethod(method, clazzInfo());\n");
  out->print("  if (methinf == 0)\n");
  out->print("    THROW1(Exception, RString(\"Cannot find matching function for \") + method);\n");
  
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->isConstructor() == true || mi->isDestructor() == true) {
      log(Note, "Not ORB DMI because constructor: " + mi->toString());
      continue;
    }
    if (Modifier::isCompatibleType(mi->_access) == false) {
      log(Note, "Note: No ORB DMI because noaccess: " + mi->toString());
      continue;
    }
    if (mi->isStatic() == true || mi->isPublic() == false) {
      RString t = mi->toString();
      log(Note, "Not ORB DMI because method is not puplic: " + t);
      continue;
    }
    mi->writeOrbDispatchBody(out, this);
  }
  out->print("  THROW0_FQ(::org::omg::CORBA::, BAD_OPERATION);\n");
  out->print("}\n\n");
}

void 
ClassInfo::generateOrbProxy(IN(RPrintWriter) out)
{
  out->print("char* _" + _name + "_Skel__ids[] = {\n  \"IDL:");
  out->print(getJTypeName());
  out->print(":1.0\",\n  0\n};\n\n");
  if (_publicDecl == Nil)
    _publicDecl = "";
  out->print("class " + _publicDecl + " " + _name + "_Skel\n: extends ::acdkx::orb::ServerDelegate,\n  implements " + _name + "\n{\n");
  out->print("public:\n");
  out->print("  char** _ids() { return _" +  _name + "_Skel__ids; }\n");

  out->print("  " + _name + "_Skel() : ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate()) { }\n");
  out->print("  " + _name + "_Skel(IN(::acdk::lang::RString) objKey) : ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate(objKey)) { }\n");
  /*
  out->print("  " + _name + "_Skel(RObject localObject) : ServerDelegate(localObject) { }\n");
  */
  out->print("  static RObject create_instance() { return new " + _name + "_Skel(); }\n");
  RIterator it = _methods->iterator();
  while (it->hasNext() == true) {
    RMethodInfo mi = RMethodInfo(it->next());
    mi->writeOrbProxy(out, this);
  }
  out->print("};\n\n");
  out->print("::acdkx::orb::SkelInfoClassesStruct __" + _name + "_Skel_Info = { " + _name + "::clazzInfo(), " + _name  + "_Skel::create_instance, 0 };\n");
  out->print("static::acdkx::orb::RegisterSkelInfoClass _register_" + _name + "_Skel(&__" + _name + "_Skel_Info);\n\n");
  out->print("//static\nR" + _name + " " +  _name + "::GetSkel(IN(::acdk::lang::RString) objKey)\n{\n  return new " + _name + "_Skel(objKey);\n}\n");
  //out->print("//static\nR" + _name + " " + _name + "::GetSkel(RObject localObject)\n{\n  return new " + _name + "_Skel(localObject);\n}\n");
  

 
}

void 
ClassInfo::generateORB(IN(RPrintWriter) out)
{
  if (_isCorbaInterface == false)
    return;
  generateOrbProxy(out);
  generateOrbDispatch(out);
}



} // acdkmc
} // tools
} // acdk
#endif //0
