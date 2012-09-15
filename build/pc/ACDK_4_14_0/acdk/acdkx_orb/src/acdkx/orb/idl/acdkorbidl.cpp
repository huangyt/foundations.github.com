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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/idl/acdkorbidl.cpp,v 1.10 2005/02/05 10:45:40 kommer Exp $

#include <acdk.h>
#include <acdk/lang/CmdLineParser.h>
#include <acdk/lang/System.h>
#include <acdk/lang/ClassLoader.h>

#include <acdk/lang/reflect/Unit.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/util/StringTokenizer.h>
#include <algorithm>
#include <stdlib.h> 
#include <search.h>


namespace acdkx {
namespace orb {
namespace idl {

USING_CLASS(::acdk::lang::reflect::, Unit);
USING_CLASS(::acdk::lang::reflect::, Field);

USING_CLASS(::acdk::io::, PrintWriter);
using namespace ::acdk::lang::dmi;


using ::acdk::lang::sys::core_vector;

class AcdkOrbIdl
: extends acdk::lang::Object
{
public :
  RPrintWriter _out;
  /// contains the forward interface declarations
  core_vector<const ClazzInfo*> _interfaceTypeDefs;
  core_vector<const ClazzInfo*> _arrayTypeDefs;
  core_vector<const ClazzInfo*> _knownTypes;
  bool _noFields;
  bool _suppressDublicatedMethods;
  RString _filename;
  bool _onlyExplicitClasses;
  bool _onlyKnownClasses;
  RString _className;
  AcdkOrbIdl()
  : _out()
  , _noFields(false)
  , _suppressDublicatedMethods(true)
  , _onlyExplicitClasses(false)
  , _onlyKnownClasses(false)
  {
  }
  static int acdkmain(RStringArray args);
  void writeUnits(IN(RStringArray) units);
  void writeClasses(IN(RClassArray) classes);
  void writeClass(StringBuffer& sb, IN(RClass) cls);
  void writeStruct(StringBuffer& sb,IN(RClass) cls);
  void writeException(StringBuffer& sb, IN(RClass) cls);
  /// @return true if any constructor/ static methods are written.
  bool writeClassFactory(StringBuffer& sb, IN(RClass) cls);
  void writeForwardDeclarations(StringBuffer& sb);
  void addInterfaceTypeDefs(const ClazzInfo* ci);
  void addArrayTypeDefs(const ClazzInfo* ci);
  bool writeMethod(StringBuffer& sb, const ClazzInfo* ci, const ClazzMethodInfo* mi, int flags);
  void writeMethodArg(StringBuffer& sb, const ClazzInfo* ci, const ClazzMethodInfo* mi, const ClazzMethodArgInfo* ai);
  bool writeField(StringBuffer& sb, const ClazzInfo* ci, const ClazzFieldInfo* fi, int flags);
  
  // returns the type name without attributes
  RString getTypeName(const ClazzInfo* ci);
  RString getReturnType(const ClazzInfo* ci);
  void writeFileHeader();
  void writeFileFooter();
  RString getIncludeGuard();
  static RString objectName(const ClazzInfo* ci);
  static RString fqObjectName(const ClazzInfo* ci);
  static RString paramName(const char* name);
  static bool isException(const ClazzInfo* ci)
  {
    if (ci == Object::clazzInfo())
      return false;
    return Throwable::clazzInfo()->assignableFrom(ci );
  }
  bool isCompatible(const ClazzInfo* ci, const ClazzMethodInfo* mi);
  bool isKnown(const ClazzInfo* ci);
};

//static 
RString 
AcdkOrbIdl::objectName(const ClazzInfo* ci)
{
  if (strcmp(ci->ns, "acdk/lang") == 0 &&
      strcmp(ci->name, "Object") == 0)
      return "AObject";
  return ci->name;
}

bool 
AcdkOrbIdl::isKnown(const ClazzInfo* ci)
{
  return _knownTypes.find(ci) != _knownTypes.end();
}

//static 
RString 
AcdkOrbIdl::fqObjectName(const ClazzInfo* ci)
{
  if (ci->ns == 0 || *ci->ns == 0)
    return objectName(ci);
  return RString(ci->ns)->replace("/", "::") + "::" + objectName(ci);
}
//static 
RString 
AcdkOrbIdl::paramName(const char* name)
{
  if (strcmp(name, "out") == 0)
    return "out_";
  if (strcmp(name, "in") == 0)
    return "in_";
  if (strcmp(name, "inout") == 0)
    return "inout_";
  if (strcmp(name, "attribute") == 0)
    return "attribute_";
  if (strcmp(name, "exception") == 0)
    return "exception_";
  return name; 
}

void 
AcdkOrbIdl::addInterfaceTypeDefs(const ClazzInfo* ci)
{
  for (int i = 0; i < _interfaceTypeDefs.size(); ++i)
  {
    if (_interfaceTypeDefs[i] == ci)
      return;
  }
  _interfaceTypeDefs.push_back(ci);
}

void 
AcdkOrbIdl::addArrayTypeDefs(const ClazzInfo* ci)
{
  for (int i = 0; i < _arrayTypeDefs.size(); ++i)
  {
    if (_arrayTypeDefs[i] == ci)
      return;
  }
  _arrayTypeDefs.push_back(ci);
}


RString 
AcdkOrbIdl::getTypeName(const ClazzInfo* ci)
{
  if (ci == ClazzInfo::getCharClazz())
    return "char";
  if (ci == ClazzInfo::getByteClazz())
    return "octet";
  if (ci == ClazzInfo::getShortClazz())
    return "short";
  if (ci == ClazzInfo::getIntClazz())
    return "long";
  if (ci == ClazzInfo::getLongClazz())
    return "longlong";
  if (ci == ClazzInfo::getFloatClazz())
    return "float";
  if (ci == ClazzInfo::getDoubleClazz())
    return "double";
  if (ci == ClazzInfo::getBoolClazz())
    return "boolean";
  if (ci == ClazzInfo::getVoidClazz())
    return "void";
  if (ci == String::clazzInfo())
  {
    addInterfaceTypeDefs(ci);
    return "string";
  }
  if (ci->isArray() == true)
  {
    if (ci->userInfo == 0)
    {
      return "unknownArray";
    }
    addArrayTypeDefs(ci);
    return getTypeName(reinterpret_cast<const ClazzInfo*>(ci->userInfo)) + "Array";
  } else {
    
    addInterfaceTypeDefs(ci);
    return fqObjectName(ci);
  }
}

RString 
AcdkOrbIdl::getReturnType(const ClazzInfo* ci)
{
  return getTypeName(ci);
}

void 
AcdkOrbIdl::writeMethodArg(StringBuffer& sb, const ClazzInfo* ci, const ClazzMethodInfo* mi, const ClazzMethodArgInfo* ai)
{
  if (ai->flags & MiAiOut)
  {
    if (ai->flags & MiAiIn)
      sb << "inout ";
    else
      sb << "out ";
  }  
  else //if (ai->flags == 0 || ai->flags & MiAiIn)
    sb << "in ";
  sb << getTypeName(ai->type) << " " << paramName(ai->name);

}

inline
const char* getLabel(const ClazzMethodInfo* mi)
{
  return mi->altlabel != 0 ? mi->altlabel : mi->name;
}

bool isMethodDefinedInSuper(const ClazzInfo* ci, const char* label);
bool hasMethod(const ClazzInfo* ci, const char* label)
{
  for (int i = 0; ci->methods[i] != 0; ++i)
  {
    if (strcmp(label, getLabel(ci->methods[i])) == 0)
      return true;
  }
  return isMethodDefinedInSuper(ci, label);
}

bool
isMethodDefinedInSuper(const ClazzInfo* ci, const char* label)
{
  for (int i = 0; ci->interfaces[i] != 0; ++i)
  {
    if (hasMethod(ci->interfaces[i]->type, label) == true)
      return true;
  }
  return false;
}

bool
AcdkOrbIdl::isCompatible(const ClazzInfo* ci, const ClazzMethodInfo* mi)
{
  for (int i = 0; mi->methodArgs[i] != 0; ++i)
  {
    if (isException(mi->methodArgs[i]->type) == true)
    {
      System::err->println(RString("Method ") + ci->name + "::" + mi->name + " has exception as parameter");
      return false;
    }
  }
  return true;
}

RString 
AcdkOrbIdl::getIncludeGuard()
{
  if (_filename == Nil)
    return Nil;
  return _filename->replace('.', '_');
}

void 
AcdkOrbIdl::writeFileHeader()
{
  _out->print("// generated by acdkorbidl\n\n");
  RString inclguard = getIncludeGuard();
  if (inclguard == Nil)
    return;
  _out->print("#ifndef " + inclguard + "\n#define " + inclguard + "\n\n");
}

void 
AcdkOrbIdl::writeFileFooter()
{
  RString inclguard = getIncludeGuard();
  if (inclguard == Nil)
    return;
  _out->print("#endif // " + inclguard + "\n");
}

bool 
AcdkOrbIdl::writeMethod(StringBuffer& sb, const ClazzInfo* ci, const ClazzMethodInfo* mi, int flags)
{
  if ((mi->flags & MiPublic) == false)
    return false;
  if (isCompatible(ci, mi) == false)
    return false;
  if (_suppressDublicatedMethods == true && 
      isMethodDefinedInSuper(ci, getLabel(mi)) == true)
    return false;
  if (flags & MiStatic)
  {
    if ((mi->flags & MiMiConstructor)  == false &&
        (mi->flags & MiStatic) == false)
      return false;
  }
  else
  {
    if ((mi->flags & MiMiConstructor) ||
        (mi->flags & MiStatic))
      return false;
  }
  if (strcmp(mi->name, "getClass") == 0)
    return false;

  if (mi->altlabel != 0)
    sb << "\n  // orginal method name: " << (char*)mi->name << "\n  ";
  sb << "  ";
  sb << getReturnType(mi->returnType) << " ";
  if (flags & MiStatic && (mi->flags & MiMiConstructor))
    sb << "createCor";
  sb << (char*)getLabel(mi) << "(";
  
  for (int i = 0; mi->methodArgs[i] != 0; ++i)
  {
    if (i != 0)
      sb << ", ";
    writeMethodArg(sb, ci, mi, mi->methodArgs[i]);
  }
  sb << ");\n";
  return true;
}

int compareNames(const ClazzInfo* fci, const ClazzInfo* sci)
{
  if (strcmp(fci->ns, sci->ns) < 0)
    return -1;
  return strcmp(fci->name, sci->name); 
}

int compare(const void* fe, const void* se)
{
  const ClazzInfo** fci = (const ClazzInfo**)fe;
  const ClazzInfo** sci = (const ClazzInfo**)se;
  return compareNames(*fci, *sci);

}

void
writeBaseArrayDeclarations(StringBuffer& sb)
{
  sb << "#include <acdk_base.idl>\n";
  return;
  /*
  sb << "typedef unsigned long longlong;\n"
     << "typedef sequence<boolean> booleanArray;\n"
     << "typedef sequence<octet> octetArray;\n"
     << "typedef sequence<char> charArray;\n"
     << "typedef sequence<short> shortArray;\n"
     << "typedef sequence<long> longArray;\n"
     << "typedef sequence<longlong> longlongArray;\n"
     << "typedef sequence<float> floatArray;\n"
     << "typedef sequence<double> doubleArray;\n"
     << "typedef sequence<string> stringArray;\n"
     << "typedef sequence<stringArray> stringArrayArray;\n"
     ;
     */
}

RString getNsIdentifier(const ClazzInfo* ci)
{
  if (ci == 0 || ci->ns == 0 || *ci->ns == 0)
    return "";
  return RString(ci->ns)->replace('/', '_');
}

void 
AcdkOrbIdl::writeForwardDeclarations(StringBuffer& sb)
{
  writeBaseArrayDeclarations(sb);
  qsort((void*)_interfaceTypeDefs.begin(), _interfaceTypeDefs.end() - _interfaceTypeDefs.begin(), sizeof(_interfaceTypeDefs.begin()), compare);
  const ClazzInfo* lastUnitCi = 0;
  StringBuffer indent;
  for (int i = 0; i < _interfaceTypeDefs.size(); ++i)
  {
    const ClazzInfo* ci = _interfaceTypeDefs[i];
    if (_onlyKnownClasses == true && isKnown(ci) == false)
      continue;
    if (isException(ci) == true)
      continue;
    if (ci == String::clazzInfo())
      continue;

    if (lastUnitCi == 0 || strcmp(lastUnitCi->ns, ci->ns) != 0)
    {
      if (lastUnitCi != 0)
      {
        RStringArray nsa = acdk::util::StringTokenizer(lastUnitCi->ns, "/").allToken();
        for (int j = 0; j < nsa->length(); ++j)
        {
          indent.set(indent.toString()->substr(2));
          sb << indent.toString() << "}; // module " << nsa[j] << "\n";
        }
      }
      
      RStringArray nsa = acdk::util::StringTokenizer(ci->ns, "/").allToken();
      for (int j = 0; j < nsa->length(); ++j)
      {
        sb << indent.toString() << "module " << nsa[j] << "{\n";
        indent << "  ";
      }
      lastUnitCi = ci;
    }
    sb << indent.toString() << "interface " << objectName(ci) << ";\n";
    RString guard = getNsIdentifier(lastUnitCi) + "_" + objectName(ci);
    sb << "#ifndef " << guard << "\n"
      << "#define " << guard << "\n";
    sb << indent.toString() << "typedef sequence<" << objectName(ci) << "> " 
      << objectName(ci) << "Array;\n";
    sb << indent.toString() << "typedef sequence<" << objectName(ci) << "Array> " 
      << objectName(ci) << "ArrayArray;\n";
    sb << "#endif // " << guard << "\n";
    
  }
  if (lastUnitCi != 0)
  {
    RStringArray nsa = acdk::util::StringTokenizer(lastUnitCi->ns, "/").allToken();
    for (int j = 0; j < nsa->length(); ++j)
    {
      sb << "}; // module " << nsa[j] << "\n";
    }
  }
}

bool
AcdkOrbIdl::writeField(StringBuffer& sb, const ClazzInfo* ci, const ClazzFieldInfo* fi, int flags)
{
  if (_noFields == true)
    return false;
  if ((fi->flags & MiPublic) == false)
    return false;
  if ((flags & MiStatic) && ((fi->flags & MiStatic) == false))
    return false;
  if (((flags & MiStatic) == false) && (fi->flags & MiStatic))
    return false;
  if (isException(fi->type) == true)
    return false;
  sb << "  attribute " << getReturnType(fi->type) << " " << paramName(fi->name) << ";\n";
  return true;
}

void 
AcdkOrbIdl::writeException(StringBuffer& sb, IN(RClass) cls)
{
  const ClazzInfo* clazz = cls->objectClazzInfo();
  sb << "\nexception " << cls->getClassName() << "\n{\n";
  RFieldArray fa = cls->getFields();
  for (int i = 0; i < fa->length(); ++i)
  {
    const ClazzFieldInfo* fi = fa[i]->clazzField();
    sb << "  " << getReturnType(fi->type) << " " << paramName(fi->name) << ";\n";
  }
  sb << "};\n";
}

void
AcdkOrbIdl::writeStruct(StringBuffer& sb, IN(RClass) cls)
{
  const ClazzInfo* clazz = cls->objectClazzInfo();
  sb << "struct " << objectName(clazz) << "\n{\n";
  RFieldArray fa = cls->getFields();
  for (int i = 0; i < fa->length(); ++i)
  {
    const ClazzFieldInfo* fi = fa[i]->clazzField();
    sb << "  " << getReturnType(fi->type) << " " << paramName(fi->name) << ";\n";
  }
  sb << "};\n\n";

}

void 
AcdkOrbIdl::writeClass(StringBuffer& sb, IN(RClass) cls)
{
  const ClazzInfo* clazz = cls->objectClazzInfo();
  if (clazz == String::clazzInfo())
    return;

  if (isException(clazz) == true)
  {
    writeException(sb, cls);
    return;
  }
  if (cls->hasMetaAttribute("acdkx_orb_StructType") == true)
  {
    writeStruct(sb, cls);
    return;
  }
  if (_onlyExplicitClasses == true && 
      cls->hasMetaAttribute("acdkx_orb_ClassType") == false)
  {
    return;
  }
  sb << "interface " << objectName(clazz);
  RClass super = cls->getSuperclass();
  bool first = true;
  /*
  if (super != Nil)
  {
    sb << ": ";
    sb <<  super->getName()->replace("/", "::");
    first = false;
  }*/
  RClassArray ifaces = cls->getInterfaces();
  int i;
  for (i = 0; i < ifaces->length(); ++i)
  {
    const ClazzInfo* sci = ifaces[i]->objectClazzInfo();
    if (_onlyKnownClasses == true && isKnown(sci) == false)
      continue;
    if (first == true)
      sb << "\n: ";
    else
      sb << "\n, ";
    
    addInterfaceTypeDefs(sci);
    sb << fqObjectName(sci);
    first = false;
  }
  sb << "\n{\n";
    
  for (i = 0; clazz->fields[i] != 0; ++i)
  {
    writeField(sb, clazz, clazz->fields[i], 0);
  }
  for (i = 0; clazz->methods[i] != 0; ++i)
  {
    writeMethod(sb, clazz, clazz->methods[i], 0);
  }
  sb << "};\n\n";
}

bool isSuperOf(const ClazzInfo* super, const ClazzInfo* deriv)
{
  if (super == deriv)
    return true;
  /*if (super->interfaces[0] == 0)
    return true;
    */
  for (int i = 0; deriv->interfaces[i] != 0; ++i)
  {
    if (isSuperOf(super, deriv->interfaces[i]->type) == true)
      return true;
  }
  return false;
}

int compareClassDeps(IN(RClass) c1, IN(RClass) c2)
{
  const ClazzInfo* clazz1 = c1->objectClazzInfo();
  const ClazzInfo* clazz2 = c2->objectClazzInfo();
  if (AcdkOrbIdl::isException(clazz1) == true)
  {
    if (AcdkOrbIdl::isException(clazz2) == false)
      return -1;
  } 
  else if (AcdkOrbIdl::isException(clazz2) == true)
  {
    return 1;
  }
  if (clazz1 == clazz2)
    return 0;
  if (isSuperOf(clazz1, clazz2) == true)
      return -1;
    if (isSuperOf(clazz2, clazz1) == true)
      return 1;
    return 0;
}

inline
int compareClassDeps(const void* c1, const void* c2) 
{
  return compareClassDeps((*(RClass*)c1), (*(RClass*)c2));
}

void swap(IN(RClassArray) ca, int i, int j)
{
  RClass tcls = ca[i];
  RClass scls = ca[j];
  ca[i] = ca[j];
  ca[j] = tcls;
}

void sortDependencies(INOUT(RClassArray) classes)
{
  bool changed = false;
  do {
    changed = false;
   for (int i = 0; i < classes->length(); ++i)
   {
     for (int j = 0; j < classes->length(); ++j)
     {
       if (compareClassDeps(classes[i], classes[j]) > 0 && i < j)
       {
         swap(classes, i, j);
         //System::out->println("// Swap: " + classes[i]->getName() + " <-> " + classes[j]->getName());
         changed = true;
       }
     }
   }
  } while (changed == true);
  
}

bool 
AcdkOrbIdl::writeClassFactory(StringBuffer& sb, IN(RClass) cls)
{
  const ClazzInfo* clazz = cls->objectClazzInfo();
  if (clazz == String::clazzInfo())
    return false;
  if (isException(clazz) == true)
    return false;
  bool ret = false;
  int i;
  sb << "interface " << objectName(clazz) << "CorFactory";
  /*
  RClass super = cls->getSuperclass();
  bool first = true;
  RClassArray ifaces = cls->getInterfaces();
  
  for (i = 0; i < ifaces->length(); ++i)
  {
    if (first == true)
      sb << "\n: ";
    else
      sb << "\n, ";
    const ClazzInfo* sci = ifaces[i]->objectClazzInfo();
    //addInterfaceTypeDefs(sci);
    sb << fqObjectName(sci) << "CorFactory";
    first = false;
  }*/
  sb << "\n{\n";
    
  for (i = 0; clazz->fields[i] != 0; ++i)
  {
    ret |= writeField(sb, clazz, clazz->fields[i], MiStatic);
  }
  for (i = 0; clazz->methods[i] != 0; ++i)
  {
    ret |= writeMethod(sb, clazz, clazz->methods[i], MiStatic);
  }
  sb << "};\n\n";
  return ret;
}

void 
AcdkOrbIdl::writeClasses(IN(RClassArray) classes)
{
  
  StringBuffer sb;
  
  RStringArray nsa = Nil;
  
  RUnit lunit = Nil;
  int i;
  for (i = 0; i < classes->length(); ++i)
  {
    RClass cls = classes[i];
    if (lunit == Nil || cls->getUnit()->equals(&lunit) == false)
    {
      if (nsa != Nil)
      {
        for (int j = nsa->length() - 1; j >= 0; --j)
        {
          sb << "}; // module " << nsa[j] << "\n";  
        }
      }
      lunit = cls->getUnit();
      nsa = acdk::util::StringTokenizer(lunit->getName(), "/").allToken();    
      for (int j = 0; j < nsa->length(); ++j)
      {
        sb << "module " << nsa[j] << " {\n";  
      }
    }
    writeClass(sb, classes[i]);
    StringBuffer sb2;
    bool berg = writeClassFactory(sb2, classes[i]);
    if (berg)
      sb << sb2.toString();
  }
  if (nsa != Nil)
  {
    for (int j = nsa->length() - 1; j >= 0; --j)
    {
      sb << "}; // module " << nsa[j] << "\n";  
    }
  }
  {
    StringBuffer sbf; 
    // print forward declarations
    writeForwardDeclarations(sbf);
    _out->print(sbf.toString());
  }
  
  _out->print(sb.toString());
  
}

void 
AcdkOrbIdl::writeUnits(IN(RStringArray) unitnames)
{
  RClassArray classes = new ClassArray(0);
  RUnitArray units = new UnitArray(0);
  writeFileHeader();
  for (int i = 0; i < unitnames->length(); ++i)
  {
    RUnit unit = ClassLoader::getSystemClassLoader()->loadUnit(unitnames[i]);
    RString unitincl = "acdkx_orb_UnitInclude";
    acdk::lang::dmi::RMetaAttribute ma;
    for (int j = 1; ma = unit->getMetaAttribute(unitincl + j); ++j)
    {
      _out->print("#include <" + RString(ma->value) + ">\n");
    }
    units->append(unit);
    if (_className != Nil)
    {
      RClassArray clsar = unit->getClasses();
      for (int j = 0; j < clsar->length(); ++j)
        if (_className->equals(clsar[j]->getName()) == true)
          classes->append(clsar[j]);
    }
    else
      classes->concat(unit->getClasses());
  }
  sortDependencies(classes);
  writeClasses(classes);
  writeFileFooter();
}

void help()
{
  System::out->println(
    "acdkorbidl [options]\n"
    "  with options:\n"
    "  -o|--output <filename>   where to write output\n"
    "  -u|--unit <unitname>     unit to include\n"
    "  --no-fields              no attributes\n"
    "  -k|--only-known          generate only known declared corba-interface\n"
    "  -x|--only-explict        generate only explicit declared corba-interface\n"
    "  -c|--classname <classname> generate IDL for a single class (example acdk/text/RegExp)\n"
    );
}

#define INCR_PARAM(msg) \
++i; \
if (i >= args->length())  \
{ \
  System::out->println("option need value: " msg); \
  help(); \
  return 1; \
} \
value = args[i]

//static 
int 
AcdkOrbIdl::acdkmain(RStringArray args)
{
  try {
    
  AcdkOrbIdl oidl;
  //oidl._out = System::out;
  RStringArray units = new StringArray(0);
  for (int i = 0; i < args->length(); ++i)
  {
    RString param = args[i];
    RString value;
    if (param->equals("--output") || param->equals("-o"))
    {
      INCR_PARAM("-o");
      oidl._out = new PrintWriter(new acdk::io::FileWriter(value));
      oidl._filename = value;
    }
    else if (param->equals("-u") || param->equals("--unit"))
    {
      INCR_PARAM("-u");
      units->append(value);
    }
    else if (param->equals("--no-fields") == true)
    {
      oidl._noFields = true;
    }
    else if (param->equals("-x") == true || param->equals("--only-explict") == true)
    {
      oidl._onlyExplicitClasses = true;
    }
    else if (param->equals("-k") == true || param->equals("--only-known") == true)
    {
      oidl._onlyKnownClasses = true;
    } 
    else if (param->equals("-c") == true || param->equals("--classname") == true)
    {
      INCR_PARAM("-c");
      oidl._className = args[i];
    }
  }

  if (oidl._out == Nil)
    oidl._out = System::out;
  oidl.writeUnits(units);

  } 
  catch (RThrowable ex)
  {
    System::err->println(ex->getMessage());
  }
  return 0;
}

} //namespace idl 
} // namespace orb 
} //namespace acdkx 

    int 
main(int argc, char* argv[], char** envptr)
{

  return acdk::lang::System::main(acdkx::orb::idl::AcdkOrbIdl::acdkmain, argc, argv, envptr);
}

