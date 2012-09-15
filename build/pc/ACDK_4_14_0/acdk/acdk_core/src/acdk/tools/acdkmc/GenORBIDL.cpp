// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/tools/acdkmc/GenORBIDL.cpp,v 1.9 2004/02/27 00:24:57 kommer Exp $
//
// $Log: GenORBIDL.cpp,v $
// Revision 1.9  2004/02/27 00:24:57  kommer
// typo
//
// Revision 1.8  2003/06/19 14:37:16  kommer
// source comment header ajusted
//
// Revision 1.7  2003/06/19 13:17:06  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.6.2.1  2003/05/22 12:37:00  kommer
// moved DMI flags from reflect::Modifier to dmi::MetaInfoFlags
//
// Revision 1.6  2002/08/30 00:45:27  kommer
// dos2unix
//
// Revision 1.5  2002/08/29 18:31:07  kommer
// introduces acdk_tools_mc
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
// Revision 1.1  2001/12/02 13:47:54  kommer
// initial revision
//
// Revision 1.6  2001/05/18 08:32:17  kommer
// no changes
//
// Revision 1.5  2001/05/05 18:12:25  kommer
// panta rei
//
// Revision 1.4  2001/04/30 13:09:21  kommer
// panta rei
//
// Revision 1.3  2001/04/28 14:09:16  kommer
// panta rei
//
// Revision 1.2  2001/04/28 11:52:40  kommer
// panta rei
//
// Revision 1.1  2001/04/27 18:50:03  kommer
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


#include "ClassInfo.h"
#include <acdk/io/File.h>


namespace acdk {
namespace tools {
namespace acdkmc {

#if 0

using namespace acdk::lang;
using namespace acdk::lang::reflect;
using namespace acdk::tools::mc;

USING_CLASS(::acdk::io::, File);
USING_CLASS(::acdk::util::, Iterator);


/*
void 
ClassInfo::writeOpenModule(IN(RPrintWriter) out)
{
  out->print("\n");
  RIterator it = _namespace->iterator();
  while (it->hasNext() == true) {
    RString ns = RString(it->next());
    out->print("module ");
    out->print(ns);
    out->print(" { \n");
  }
  out->print("\n");
}

void 
ClassInfo::writeCloseModule(IN(RPrintWriter) out)
{
  RIterator it = _namespace->iterator();
  out->print("\n");
  while (it->hasNext() == true) {
    RString ns = RString(it->next());
    out->print("}; // module ");
    out->print(ns);
    out->print("\n");
  }
  out->print("\n");
}

bool
ClassInfo::isIdlStruct()
{
   bool isSerializable = false;
   {
   RIterator it = _derivides->iterator();
   while (it->hasNext() == true) {
      RSuperInfo si = (RSuperInfo)it->next();
      if(si->name->equals("::acdk::io::Serializable") ||
         si->name->equals("acdk::io::Serializable") ||
         si->name->equals("Serializable"))
      {
         isSerializable = true;
         break;
      }
   }
   }
   if (isSerializable == false)
      return false;
   bool hasPublicMember = false;
   { 
      RIterator it = _fields->iterator();
      while (it->hasNext() == true) {
         RFieldInfo fi = (RFieldInfo)it->next();
         if (::acdk::lang::reflect::Modifier::isPublic(fi->flags) == true) {
            hasPublicMember = true;
            break;
         }
      }
   }
   if (hasPublicMember == false)
      return false;
   return true;
}
*/



RString 
IdlTypeName(IN(RString) classname, bool isArray)
{
   if (isArray == true)
      return "" + classname + "Seq";
   return classname;
}

RString
IdlTypeFromTypeName(IN(RString) acdkname)
{
   RString ns = "";
   RString classname = acdkname;
   if (acdkname->lastIndexOf(":") != -1) {
      ns = acdkname->substr(0 , acdkname->lastIndexOf(":") + 1);
      classname = acdkname->substr(acdkname->lastIndexOf(":") + 1);
   }
   bool isSequence = false;
   bool isClass = false;
   if (classname->startsWith("R") == true) {
      isClass = true;
      classname = classname->substr(1);
      if (classname->endsWith("Array") == true) {
         isSequence = true;
         classname = classname->substr(0, classname->length() - strlen("Array"));
      }
   }
   if (classname->equals("int") == true)
      return IdlTypeName("long", isSequence);

   
   if (classname->equals("String") == true)
      return IdlTypeName("string", isSequence);

   return IdlTypeName(classname, isSequence);
}

void 
ClassInfo::generateIdlIfStruct(IN(RPrintWriter) out)
{
   if (_isCorbaStruct == false)
      return;
   out->print("\n");
   out->print("struct " + _name + "\n{\n");
   RIterator it = _fields->iterator();
   while (it->hasNext() == true) 
   {
      RFieldInfo fi = (RFieldInfo)it->next();
      out->print("  " + IdlTypeFromTypeName(fi->getMappedType()) + " " + fi->label + ";\n");
   }
   out->print("};\n");
}

void 
ArgumentInfo::asIDLType(IN(RPrintWriter) out)
{
   if (Modifier::isInOutParam(flags) == true)
      out->print("inout ");
   else if (Modifier::isOutParam(flags) == true)
      out->print("out ");
   else if (Modifier::isInParam(flags) == true)
      out->print("in ");
   
   out->print(IdlTypeFromTypeName(type) + " " + name);

}

void
MethodInfo::generateIdlIfInterface(IN(RPrintWriter) out)
{
   if (Modifier::isPublic(_access) == false ||
       Modifier::isStatic(_access) == true ||
       Modifier::isAbstract(_access) == false)
      return; 
   
   out->print("  ");

   if ((MiMiOneway  & _access) == MiMiOneway)
      out->print("oneway ");
   out->print(IdlTypeFromTypeName(returnType) + " ");
   out->print(name + "(");
   RIterator it = args->iterator();
   
   while (it->hasNext() == true) 
   {
      RArgumentInfo ai = (RArgumentInfo)it->next();
      ai->asIDLType(out);
      if (it->hasNext() == true)
         out->print(", ");
   }
   out->print(");\n");
}

void
ClassInfo::generateIdlIfInterface(IN(RPrintWriter) out)
{
   if (_isCorbaInterface == false)
      return;
   
   out->print("\n\ninterface " + _name + "\n{\n");
   RIterator it = _methods->iterator();
   while (it->hasNext() == true) 
   {
      RMethodInfo mi = (RMethodInfo)it->next();
      mi->generateIdlIfInterface(out);
   }
   out->print("};\n");
}

void
ClassInfo::generateORBIdl(IN(RPrintWriter) out)
{
   if (_isCorbaInterface == false && _isCorbaStruct == false)
    return;
   writeOpenModule(out);
   
   generateIdlIfStruct(out);
   generateIdlIfInterface(out);
   

   writeCloseModule(out);
}



void
ClassModul::generateORBIdl(IN(RPrintWriter) out)
{
  RIterator it = _classes->iterator();
  while (it->hasNext() == true) 
  {
    RClassInfo ci = RClassInfo(it->next());
    ci->generateORBIdl(out);
  }
}

bool 
ClassModulCompiler::hasOrbDefinitions()
{
  RIterator it = _modules->iterator();
  while (it->hasNext() == true) 
  {
    RClassModul cm = RClassModul(it->next());

    RIterator cit = cm->_classes->iterator();
    while (cit->hasNext() == true) 
    {
      RClassInfo ci = RClassInfo(cit->next());
      if (ci->_isCorbaInterface == true || ci->_isCorbaStruct == true)
        return true;
    }
  }
  return false;
}

void
ClassModulCompiler::writeExtIdl(IN(RString) fname)
{
  
  RPrintWriter out = new PrintWriter(new FileWriter(fname));
  RIterator it = _modules->iterator();
  while (it->hasNext() == true) 
  {
     RClassModul cm = RClassModul(it->next());
     cm->generateORBIdl(out);
  }
}

#endif //0

} // acdkmc
} // tools
} // acdk

