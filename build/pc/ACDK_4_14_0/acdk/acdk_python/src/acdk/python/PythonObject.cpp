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
// $Header: /cvsroot/acdk/acdk/acdk_python/src/acdk/python/PythonObject.cpp,v 1.23 2005/02/06 12:58:00 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/lang/reflect/Field.h>
#include <acdk/lang/DmiException.h>
#include <acdk/lang/ClassNotFoundException.h>
#include "Config.h"

#include "PythonSys.h"


namespace acdk {
namespace python {

using namespace ::acdk::lang::dmi;

struct ObjectHolder
{
  RObject object;
  ObjectHolder(IN(RObject) o) : object(o) { }
};

class AcdkPythonProxy
{
  
public:
  PyObject_HEAD
  ObjectHolder* object;

  static PyObject* new_instance(PyObject* self, PyObject* args);
  static void destroy(PyObject* self);
  static PyObject* getattr(PyObject* self, char* args);
  static PyObject* invoke_generic(PyObject* self, PyObject* args);

  static PyObject* peek(PyObject* self, PyObject* args);
  static PyObject* poke(PyObject* self, PyObject* args);
  static PyObject* peek_static(PyObject* self, PyObject* args);
  static PyObject* poke_static(PyObject* self, PyObject* args);
  static PyObject* invoke(PyObject* self, PyObject* args);
  
  static PyObject* invoke_static(PyObject* self, PyObject* args);

  static PyObject* repr(PyObject* self, PyObject* args);
  static PyObject* create_proxy(PyObject* self, PyObject* args);

};

static struct PyMethodDef AcdkPythonProxy_std_methods[] = 
{
  { "invoke_generic", AcdkPythonProxy::invoke_generic, METH_VARARGS },
  { "invoke", AcdkPythonProxy::invoke, METH_VARARGS },
  { "__call__", AcdkPythonProxy::invoke, METH_VARARGS }, // obj(a, b, c)
  { "invoke_static", AcdkPythonProxy::invoke_static, METH_VARARGS },
  { "peek", AcdkPythonProxy::peek, METH_VARARGS },
  { "peek_static", AcdkPythonProxy::peek_static, METH_VARARGS },
  { "poke", AcdkPythonProxy::poke, METH_VARARGS },
  { "poke_static", AcdkPythonProxy::poke_static, METH_VARARGS },
  { "create_proxy", AcdkPythonProxy::create_proxy, METH_VARARGS },
  { NULL, NULL }
};

static PyTypeObject AcdkObjectType = 
{
  PyObject_HEAD_INIT(&PyType_Type)
  0,
  "Object",
  sizeof(AcdkPythonProxy),
  0,
  (destructor)AcdkPythonProxy::destroy,
  (printfunc)0,
  (getattrfunc)AcdkPythonProxy::getattr, //AcdkPythonProxy::peek,
  (setattrfunc)0, //AcdkPythonProxy::poke,
  (cmpfunc)0,
  (reprfunc)0,
  0,
  0,
  0,
  (hashfunc)0,
  (ternaryfunc)0,
  (reprfunc)0
};

bool 
python2acdk(PyObject* po, ScriptVar& sa)
{
  if (po == Py_None) {
    sa = Nil;
    return true;
  }
  if (PyString_Check(po) != 0) {
    sa = (RObject)new String(PyString_AsString(po), NormalSST | CCAscii);
    return true;
  }
  if (PyInt_Check(po) != 0) {
    sa = (int)PyInt_AS_LONG(po);
    return true;
  }
  if (PyLong_Check(po) != 0) {
#if defined(HAVE_LONG_LONG)
    sa = (jlong)PyLong_AsLongLong(po);
#else
    sa = (int)PyInt_AS_LONG(po);
#endif
    return true;
  }
  if (PyFloat_Check(po) != 0) {
    sa = (double)PyFloat_AsDouble(po);
    return true;
  }
  if (PyObject_IsInstance(po, (PyObject*)&AcdkObjectType) != 0) {
    AcdkPythonProxy* app = (AcdkPythonProxy*)po;
    sa = app->object->object;
    return true;
  }
  return false;
}

void 
python2acdk(PyObject* po, int start, ScriptVarArray& sa)
{
  for (int i = 0; i < sa.size(); i++)
  {
    python2acdk(PyTuple_GET_ITEM(po, i + start), sa[i]);
  }
}

bool acdk2python(ScriptVar& var, PyObject*& tobj)
{
   switch (var.type) {
  case ScriptVar::BoolType :
  case ScriptVar::BoolRefType :
    tobj = PyInt_FromLong(var.getBoolVar() == true ? 1 : 0);
    return true;
  case ScriptVar::CharType :
  case ScriptVar::CharRefType :
  case ScriptVar::ByteType :
  case ScriptVar::ByteRefType :
  case ScriptVar::ShortType :
  case ScriptVar::ShortRefType :
  case ScriptVar::IntType :
  case ScriptVar::IntRefType :
    tobj = PyInt_FromLong(var.getIntVar());
    return true;
  case ScriptVar::LongType :
  case ScriptVar::LongRefType :
#if defined(HAVE_LONG_LONG)
    tobj = PyLong_FromLongLong(var.getLongVar());
#else
    tobj = PyLong_FromLong(var.getLongVar());
#endif
    return true;
  case ScriptVar::FloatType :
  case ScriptVar::FloatRefType :
  case ScriptVar::DoubleType :
  case ScriptVar::DoubleRefType :
    tobj = PyFloat_FromDouble(var.getDoubleVar());
    return true;
  case ScriptVar::ObjectType : 
  case ScriptVar::ObjectRefType :
  {
    if (instanceof(var.getObjectVar(), String) == true) {
      RString tstr = (RString)var.getObjectVar();
      tobj = PyString_FromString(tstr->c_str());
      return true;
    }
    AcdkPythonProxy* app = (AcdkPythonProxy*)PyObject_NEW(AcdkPythonProxy, &AcdkObjectType);
    app->object = new ObjectHolder(var.getObjectVar());
    tobj = (PyObject*)app;
    return true;
  }
  case ScriptVar::UnknownType : // VOID
    tobj = PyInt_FromLong(1);
    return true;
  }
   
  return false;
}


#define EXTRACT_STRING(ret, pobj, msg) \
do { \
if (PyString_Check(pobj) == 0) { \
    PyErr_SetString(PyExc_RuntimeError, msg); \
    return 0; \
  } \
  ret = PyString_AS_STRING(pobj); \
} while (false)


#define CHECK_ARGS(args, size, failop, msg) \
do { \
  if (PyTuple_Check(args) == 0) \
    return 0; \
  int alength = PyTuple_Size(args); \
  if (alength failop size) { \
    PyErr_SetString(PyExc_RuntimeError, msg); \
    return 0; \
  } \
} while (false) 

#define EXTRACT_SELF_APP() \
  AcdkPythonProxy* app = (AcdkPythonProxy*)self; \
  if (app == 0 || app->object == 0 || app->object->object == Nil) { \
    PyErr_SetString(PyExc_RuntimeError, "Cannot call acdk.Object.method on a Nil object"); \
    return 0; \
  } \
  RObject This = app->object->object
  

using acdk::lang::reflect::Modifier;
using acdk::lang::dmi::AcdkDmiClient;

PyObject*
AcdkPythonProxy::new_instance(PyObject* self, PyObject* args)
{
  char *classname;
  PyObject* ret = 0;
  CHECK_ARGS(args, 1, <, "Cannot call acdk.Object needs at least 1 argument");
  
  EXTRACT_STRING( classname 
                , PyTuple_GET_ITEM(args, 0)
                , "Cannot call acdk.Object expect name of class as argument 1");
  RClass cls;
  try {
    cls = Class::forName(classname);
  } catch (RClassNotFoundException ex) {
    return NULL;
  }
  RString constructorname = classname;
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  ScriptVarArray sa(PyTuple_Size(args) - 1);
  python2acdk(args, 1, sa);
  ScriptVar retv;
  if (cls->objectClazzInfo()->static_dispatch(constructorname, retv, sa,  AcdkDmiClient::getDmiClient(), Nil
                                            , MiPublic | MiMiConstructor, cls->objectClazzInfo(), 0) == 0)
    return NULL; // ### better
  
  if (acdk2python(retv, ret) == true)
    return ret;
  return NULL;
}

void
AcdkPythonProxy::destroy(PyObject* self)
{
  AcdkPythonProxy* app = (AcdkPythonProxy*)self;
  if (app->object != 0) {
    delete app->object;
    app->object = 0;
  }
}


RString invoke_generic_function_name;

USING_CLASS(::acdk::lang::reflect::, Field);

//static 
PyObject* 
AcdkPythonProxy::getattr(PyObject* self, char* name)
{
  EXTRACT_SELF_APP();

#if defined(ACDK_PYTHON_WITH_ATTR_ACCESS)
  RClass cl = (app->object->object)->getClass();
  try {
    ScriptVar retv = cl->getStaticMember(name);
    PyObject* ret = 0;
    if (acdk2python(retv, ret) == true)
      return ret;
  } catch (::acdk::lang::RIllegalArgumentException) {
    ;//nothing
  }
#endif //ACDK_PYTHON_WITH_ATTR_ACCESS

  PyObject* pm = Py_FindMethod(AcdkPythonProxy_std_methods, self, name);
  if (pm != 0)
    return pm;
  PyErr_Clear();
  invoke_generic_function_name = name;
  return Py_FindMethod(AcdkPythonProxy_std_methods, self, "invoke_generic");

}

PyObject* 
AcdkPythonProxy::invoke_generic(PyObject* self, PyObject* args)
{
  EXTRACT_SELF_APP();
  
  int alength = PyTuple_Size(args);
  
  RString methname = invoke_generic_function_name;
  ScriptVarArray sa(alength);
  python2acdk(args,0, sa);
  ScriptVar retv;
  if (This->standardDispatch(methname, retv, sa, AcdkDmiClient::getDmiClient(), Nil, MiPublic, 0) == 0)
    return NULL; // ### better
  PyObject* ret = 0;
  if (acdk2python(retv, ret) == true)
    return ret;
  return 0;
}


PyObject* 
AcdkPythonProxy::peek(PyObject* self, PyObject* args)
{
  EXTRACT_SELF_APP();
  CHECK_ARGS(args, 1, !=, "acdk.Object.peek needs 1 argument");
  
  const char* membername;
  EXTRACT_STRING( membername
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.Object.peek expects 1 string arg");

  ScriptVar retv = This->peek(membername);
  PyObject* ret = 0;
  if (acdk2python(retv, ret) == true)
    return ret;
  return 0;
}



PyObject* 
AcdkPythonProxy::peek_static(PyObject* self, PyObject* args)
{
  CHECK_ARGS(args, 2, !=, "acdk.peek_static expects 2 string arg");
  
  const char* classname;
  const char* membername;
  EXTRACT_STRING( classname
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.peek_static expect name of class as argument 1");
  EXTRACT_STRING( membername
                , PyTuple_GET_ITEM(args, 1)
                , "acdk.peek_static expect name field as argument 2");
  /*
  RClass cls;
  try {
    cls = Class::forName(classname);
  } catch (RClassNotFoundException ex) {
    PyErr_SetString(PyExc_RuntimeError, "acdk.peek_static: classname cannot be found");
    return 0;
  }*/
  try {
    ScriptVar retv = ::acdk::lang::dmi::StdDispatch::peek_static(classname, membername);
    //ScriptVar retv = cls->getStaticMember(membername);
    PyObject* ret = 0;
    if (acdk2python(retv, ret) == true)
      return ret;
  } catch (::acdk::lang::RDmiException ex) {
    StringBuffer sb("acdk.Object.peek_static: ");
    sb.append(ex->getMessage());
    PyErr_SetString(PyExc_RuntimeError, sb.toString()->c_str());
    return 0;
  }
  return 0;
}

PyObject* 
AcdkPythonProxy::poke(PyObject* self, PyObject* args)
{
  EXTRACT_SELF_APP();
  CHECK_ARGS(args, 2, !=, "acdk.Object.pole expects 2 args");
  const char* membername;
  EXTRACT_STRING( membername
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.Object.poke expect name of method as argument 1");
  ScriptVar var;
  if (python2acdk(PyTuple_GET_ITEM(args, 1), var) == false)
    return 0;
  This->poke(membername, var);
  Py_XINCREF(Py_None);
  return Py_None;
}
 

PyObject* 
AcdkPythonProxy::poke_static(PyObject* self, PyObject* args)
{
  CHECK_ARGS(args, 3, !=, "acdk.poke_static expects 2 string arg and 1 value arg");
  
  const char* classname;
  const char* membername;
  EXTRACT_STRING( classname
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.Object.peek_static expect name of class as argument 1");
  EXTRACT_STRING( membername
                , PyTuple_GET_ITEM(args, 1)
                , "acdk.Object.peek_static expect name field as argument 2");
  /*
  RClass cls;
  try {
    cls = Class::forName(classname);
  } catch (RClassNotFoundException ex) {
    PyErr_SetString(PyExc_RuntimeError, "acdk.poke_static: classname cannot be found");
    return 0;
  }*/
  try {
    ScriptVar var;
    if (python2acdk(PyTuple_GET_ITEM(args, 2), var) == false)
      return 0;
    //cls->setStaticMember(membername, var);
    ::acdk::lang::dmi::StdDispatch::poke_static(classname, membername, var);
    Py_XINCREF(Py_None);
    return Py_None;
  } catch (::acdk::lang::RDmiException ex) {
    StringBuffer sb("acdk.Object.pokek_static: ");
    sb.append(ex->getMessage());
    PyErr_SetString(PyExc_RuntimeError, sb.toString()->c_str());
    return 0;
  }
  return 0;
}


PyObject* 
AcdkPythonProxy::invoke(PyObject* self, PyObject* args)
{
  CHECK_ARGS(args, 1, <, "acdk.Object.invoke expects >= 1 arg");

  AcdkPythonProxy* app = (AcdkPythonProxy*)self;
   
  const char* methodn;
  EXTRACT_STRING( methodn
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.Object.invoke expect name of method as argument 1");

  ScriptVarArray sa(PyTuple_Size(args) - 1);
  python2acdk(args,1, sa);
  ScriptVar retv;
  if ( (app->object->object)->standardDispatch(methodn, retv, sa, AcdkDmiClient::getDmiClient(), Nil, MiPublic, 0) == 0)
    return NULL; // ### better
  PyObject* ret = 0;
  if (acdk2python(retv, ret) == true)
    return ret;
  return 0;
}

PyObject* 
AcdkPythonProxy::invoke_static(PyObject* self, PyObject* args)
{
  CHECK_ARGS(args, 2, <, "acdk.invoke_static expects >= 2 arg");
  
  const char* classname;
  const char* membername;
  EXTRACT_STRING( classname
                , PyTuple_GET_ITEM(args, 0)
                , "acdk.Object.peek_static expect name of class as argument 1");
  EXTRACT_STRING( membername
                , PyTuple_GET_ITEM(args, 1)
                , "acdk.Object.peek_static expect name field as argument 2");
  RClass cls;
  try {
    cls = Class::forName(classname);
  } catch (RClassNotFoundException ex) {
    PyErr_SetString(PyExc_RuntimeError, "acdk.Object.peek_static: classname cannot be found");
    return 0;
  }

  ScriptVarArray sa(PyTuple_Size(args) - 2);
  python2acdk(args, 2, sa);
  ScriptVar retv;
  if (cls->objectClazzInfo()->static_dispatch(membername, retv, sa,  AcdkDmiClient::getDmiClient(), Nil, 
                                              MiPublic | MiStatic, cls->objectClazzInfo(), 0) == 0)
    return 0;
  PyObject* ret = 0;
  acdk2python(retv, ret);
  return ret;
  
}


//static 
PyObject* 
AcdkPythonProxy::create_proxy(PyObject* self, PyObject* args)
{
  int alength = PyTuple_Size(args);
  PyObject* dmiTarget = PyTuple_GET_ITEM(args, 0);
  const char* className;
  EXTRACT_STRING( className
                , PyTuple_GET_ITEM(args, 1)
                , "acdk.Object.create_proxy expect name of class as argument 1");


  return 0;
}


static struct PyMethodDef AcdkPythonProxy_methods[] = 
{
  { "Object", AcdkPythonProxy::new_instance, METH_VARARGS },
  { "invoke_static", AcdkPythonProxy::invoke_static, METH_VARARGS },
  { "peek_static", AcdkPythonProxy::peek_static, METH_VARARGS },
  { "poke_static", AcdkPythonProxy::poke_static, METH_VARARGS },
  { "create_proxy", AcdkPythonProxy::create_proxy, METH_VARARGS },
  { NULL, NULL }
};

void initAcdkPythonProxy()
{
  
  PyObject* ap = Py_InitModule("acdk", AcdkPythonProxy_methods);

}





} // namespace python 
} // namespace acdk 
