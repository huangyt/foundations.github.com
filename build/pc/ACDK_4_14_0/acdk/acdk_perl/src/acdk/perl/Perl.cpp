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
// $Header: /cvsroot/acdk/acdk/acdk_perl/src/acdk/perl/Perl.cpp,v 1.29 2005/03/07 18:39:16 kommer Exp $

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
#include <iostream>
#include <sstream>
#endif
#include <acdk.h>
#include <acdk/util/HashMap.h>
#include "PerlObject.h"
#include "PerlInterpreter.h"


#include "EXTERN.h"
#include <perl.h>
#include <XSUB.h>


#ifdef STATIC
# undef STATIC
#endif
using namespace acdk::lang;
using namespace acdk::lang::sys;
using namespace acdk::lang::dmi;


#ifdef LOCAL_DEBUG
#define DOUT(msg) do { std::cout << msg << std::endl; } while (false)
#else
#define DOUT(msg) do {  } while (false)
#endif

#ifdef LOCAL_DEBUG
std::ostream& operator<<(std::ostream& os, CV* cv)
{
  os  << "CV:\n";
  if (cv == 0)
    return os << "NULL" << std::endl;

  os << "xmg_magic: " << (void*)cv->sv_any->xmg_magic << std::endl;
  os << "xcv_stash: " << (void*)cv->sv_any->xcv_stash << std::endl;
  os << "xmg_stash: " << (void*)cv->sv_any->xmg_stash << std::endl;
  os << "xcv_gv: " << (void*)cv->sv_any->xcv_gv << std::endl;
  if (cv->sv_any->xcv_gv != 0)
  {
    os << "  xcv_gv->sv_any->xmg_magic: " << cv->sv_any->xcv_gv->sv_any->xmg_magic << std::endl;
  }
  os << "xcv_padlist: " << (void*)cv->sv_any->xcv_padlist << std::endl;
  if (cv->sv_any->xcv_padlist != 0)
  {
    int len = av_len(cv->sv_any->xcv_padlist);
    os << "  length: " << len << std::endl;
    for (int i = 0; i < len; ++i)
    {
      os << "   [" << i << "]: ";
      SV* el = *av_fetch(cv->sv_any->xcv_padlist, i, 0);

      sv_dump(el);
    }
    sv_dump((SV*)cv->sv_any->xcv_padlist);
  }
  return os;
}
#endif //LOCAL_DEBUG

RString toClazzName(IN(RString) clsname)
{
  return clsname->replace('.', '/')->replace("::", "/");
}


RObject perl2AcdkObject(SV* cv)
{

#if defined(ACDK_PERL_OBJASINT)
    return (Object*)SvIV(cv);
#else
    SV** sv = hv_fetch((HV*)SvRV(cv), "_objptr", 0, 0);
    return  (RObject)(Object*)(int)SvIV(*sv);
#endif
}



SV* acdkObject2Perl2(IN(RObject) obj)
{
  Object* optr = obj.impl();
  if (optr != 0)
    optr->addRef();
    
#if defined(ACDK_PERL_OBJASINT)
  SV* cv = sv_newmortal();
  sv_setref_pv(cv, "acdk", (SV*)(int)optr);
  return cv;
#else
    // perl here
  HV* hv = newHV();
  hv_store(hv, "_objptr", 0, newSViv((int)optr), 0);
  SV* cv = newRV_noinc((SV*)hv);
  sv_bless(cv, gv_stashpv("acdk", 0));
  return cv;
#endif
}
SV* acdkObject2Perl(IN(RObject) obj)
{
#if defined(ACTIVE_PERL) || 1==1
    return acdkObject2Perl2(obj);
#else // unix
    SV* sv = acdkObject2Perl2(obj);
    dSP ;
    ENTER ;
    SAVETMPS ;

    PUSHMARK(SP) ;
    XPUSHs(sv_2mortal(sv));
    PUTBACK ;

    int count = call_pv("pacdk::wrap_acdk_object", G_SCALAR);
    SPAGAIN ;

    SV* erg = POPs;
    PUTBACK ;
    FREETMPS ;
    LEAVE ;
    return erg;
#endif
}

// old #define PERL2THIS() (Object*)SvIV(SvRV(ST(0)))
#define PERL2THIS() perl2AcdkObject(ST(0))

bool 
perl2acdk(SV* cv, ScriptVar& erg)
{
  switch (SvTYPE(cv)) {
  case SVt_IV : // Integer
    erg = (int)SvIV(cv);
    return true;
  case SVt_NV : // Double
    erg = (double)SvNV(cv);
    return true;
  case SVt_PV : { // String 
    unsigned slen;
    erg = RObject(new String((char*)SvPV(cv, slen), NormalSST | CCAscii));
    return true;
  }
  case SVt_PVMG : { // Object
    if (sv_isa(cv, "acdk") == 0) {
      DOUT("acdk: oops cannot convert unknown Object to ScriptVar");
      sv_dump(cv);
      return false;
    }
    erg = perl2AcdkObject(cv);
    /*
      old
    erg = (RObject)(Object*)SvIV(cv);
    */
    return true;
  }
  case SVt_RV :  
    if (sv_isa(cv, "acdk") != 0) 
    {
      erg = perl2AcdkObject(cv);
      /* old
	 Object* object = (Object*)SvIV(SvRV(cv));
	 erg = (RObject)object;
      */
      return true;
    }
    else if (sv_isa(cv, "pacdk") != 0)
    {
	SV** sv = hv_fetch((HV*)SvRV(cv), "obj", 0, 0);
	return perl2acdk(*sv, erg);
    }
    return perl2acdk(SvRV(cv), erg);
  case SVt_PVAV : { // Array
    bool isBasic = true;
    bool isObject = true;
    AV* av = (AV*)cv;
    int count = av_len(av) + 1;
    ScriptVarArray sa(count);
    RObjectArray oa = new ObjectArray(count); 
    for (int i = 0; i < count; i++) 
    {
      SV* sv = *av_fetch(av, i, 0);
      if (perl2acdk(sv, sa[i]) == false)
        return false;
      oa[i] = sa[i].getObjectVar();
      //SvREFCNT_dec(sv);
    }
    erg = (RObject)oa;
    return true;
  }
  case SVt_PVHV : {// Hash
    HV* hv = (HV*)cv;
    char *key = 0;
    long keylen = 0;
    int length = hv_iterinit(hv);
    acdk::util::RHashMap hm = new acdk::util::HashMap(length);
    
    do {
      SV* sv = hv_iternextsv(hv, &key, &keylen);
      if (sv == 0)
        break;
      ScriptVar var;
      if (perl2acdk(sv, var) == false)
        return false;
      hm->put(new String(key), var.getObjectVar());
      //SvREFCNT_dec(sv);
    } while (key != 0);
    erg = (RObject)hm;
    return true;  
   }
  }
  return false;
}



bool 
acdk2perl(ScriptVar& var, SV*& cv, bool mortal = true, bool higherObjects = true)
{
  switch (var.type) {
  case ScriptVar::BoolType :
    cv = newSViv(var.getBoolVar() == true ? 1 : 0);
    if (mortal)
      cv = sv_2mortal(cv);
    return true;
  case ScriptVar::CharType : // no break
  case ScriptVar::ByteType : // no break
  case ScriptVar::ShortType : // no break
  case ScriptVar::IntType :
    cv = newSViv(var.getIntVar());
    if (mortal)
      cv = sv_2mortal(cv);
    return true;
  case ScriptVar::LongType : // i64
    cv = newSViv(var.getIntVar()); // problem will be handled as int
    if (mortal)
      cv = sv_2mortal(cv);
    return true;
  case ScriptVar::FloatType : // no break
  case ScriptVar::DoubleType :
    cv = newSVnv(var.getDoubleVar());
    if (mortal)
      cv = sv_2mortal(cv);
    return true;
  case ScriptVar::ObjectType : {
    RObject o = var.getObjectVar();
    if (higherObjects) {
      if (instanceof(o, String) == true) 
      {
        RString str  = (RString)o;
        cv = newSVpv((char*)str->c_str(), str->length());
        if (mortal)
          cv = sv_2mortal(cv);
        return true;
      } 
      if (instanceof(o, acdk::util::Map) == true) { // Hashes
        HV* hv = newHV();
        acdk::util::RMap hm = acdk::util::RMap(o);
        acdk::util::RIterator it = hm->keySet()->iterator();
        while (it->hasNext() == true) 
	{
          RObject okey = it->next();
          RString skey = okey->toString();
          RObject oval = hm->get(okey);
          ScriptVar tvar(oval);
          SV* lv = 0;
          if (acdk2perl(tvar, lv, false) == false)
            return false;
          hv_store(hv, (char*)skey->c_str(), skey->length(), lv, 0);
        }
        cv = (SV*)hv;
        if (mortal)
          cv = sv_2mortal(cv);
        return true;
      }
      if (instanceof(o, ObjectArray) == true) {
        
        AV* av = newAV();
        RObjectArray oa = RObjectArray(o);
        av_extend(av, oa->length());
        for (int i = 0; i < oa->length(); i++) {
          ScriptVar tvar(oa[i]);
          SV* lv = 0;
          if (acdk2perl(tvar, lv, false) == false)
            return false;
          av_push(av, lv);
        }
        cv = sv_2mortal((SV*)av);
        return true;
      }
    }
    cv = acdkObject2Perl(o);
    return true;
  }
  case ScriptVar::UnknownType : // may be void !!! 
    cv = newSViv(0);
    return true;
  default :
    return false;
  }
}


XS(acdk_acdk2perl)
{
  dXSARGS;
  if (items != 1)
	  croak("Usage: acdk::acdk2perl(acdkobj)");
  SV* sv = ST(0);
  ScriptVar erg;
  if (perl2acdk(sv, erg) == false)
    croak("acdk::acdk2perl: cannot convert value from acdk2perl");
  if (acdk2perl(erg, ST(0), true, true) == false)
    croak("acdk::acdk2perl: cannot convert value from acdk2perl");
  XSRETURN(1);
  PUTBACK;
}

XS(acdk_perl2acdk)
{
  dXSARGS;
  if (items != 1)
	  croak("Usage: acdk::perl2acdk(perlobj)");
  SV* sv = ST(0);
  ScriptVar erg;
  if (perl2acdk(ST(0), erg) == false)
    croak("acdk::acdk2perl: cannot convert value from perl2acdk");
  erg = (RObject)erg.getObjectVar();

  if (acdk2perl(erg, ST(0), true, false) == false)
    croak("acdk::acdk2perl: cannot convert value from acdk2perl");
  XSRETURN(1);
  PUTBACK;
}


XS(XM_acdk_ObjectWrapper_DESTROY)
{ 
  dXSARGS;
  if (items != 1)
	  croak("Usage: acdk::DESTROY(THIS)");
  {
    Object* object = PERL2THIS();
    if (object != 0)
      object->releaseRef();
  }
  XSRETURN_EMPTY;
}


void my_sv_dump(SV* sv)
{
#ifndef LOCAL_DEBUG
 return; // deactivated
#else
    StringBuffer sb;
    unsigned charlen = 0;
    switch (SvTYPE(sv)) 
    {
	case SVt_IV: std::cout << "Int Scalar: " << (int)SvIV(sv); break;
        case  SVt_NV: std::cout << "double Scalar: " << (double)SvNV(sv);  break;
	case SVt_PV: std::cout << "Pointer Scalar: " << SvPV(sv, charlen); break;
	case SVt_RV:  
	    std::cout << "reference:" << std::endl << "  "; 
	    my_sv_dump(SvRV(sv));
	    return;
	case SVt_PVAV: std::cout << "Array: "; break;
        case SVt_PVHV: std::cout << "Hash: "; break;
        case SVt_PVCV: std::cout << "Code: "; break;
        case SVt_PVGV: std::cout << "Glob (possible a file handle):"; break;
        case SVt_PVMG: std::cout << "Blessed or Magical Scalar:"; break;
	default: std::cout << "Unknown: "; break;

    }
    std::cout << std::endl;
    sv_dump(sv);
    
    return;
#endif
}


XS(XM_acdk_ObjectWrapper_AUTOLOAD)
{ 
  dXSARGS;
  //std::cout << "perl_AUTOLOAD" << std::endl;
  if (items < 1)
    croak("Usage: acdk::AUTOLOAD(THIS)");
  Object* object = PERL2THIS();
  
  SV* autoload = perl_get_sv("acdk::AUTOLOAD", 0);
  CV* aacv = get_cv("acdk::AUTOLOAD", 0);
  CV* paacv = get_cv("pacdk::AUTOLOAD", 0);
  HV* papackagestash  = gv_stashpv("pacdk", 0);
  DOUT("&acdk::AUTLOAD: " << get_cv("acdk::AUTOLOAD", 0));
  DOUT("&pacdk::AUTOLOAD CV: " << get_cv("pacdk::AUTOLOAD", 0));
  //gv_dump(perl_get_gv("acdk::AUTOLOAD", 0));

  SV* sicautoload = autoload;
  if (autoload == 0)
    croak("Oops cannot found acdk::AUTOLOAD");
  //std::cout << "acdk::AUTOLOAD(THIS):" << std::endl;
  //my_sv_dump(ST(0));
  //DOUT("acdk::AUTOLOAD:");
  //my_sv_dump(autoload);
  SV** sv = hv_fetch((HV*)SvRV(ST(0)), "AUTOLOAD", 0, 0);
  if (sv != 0 && *sv != 0)
  {
    DOUT("$this->AUTOLOAD:");
    my_sv_dump(*sv);
    unsigned slen;
    //std::cout << (char*)SvPV(autoload, slen) << std::endl;
  }
  else
  {
    DOUT("$this->AUTOLOAD == 0");
  }
  unsigned slen = 0;
  RString funcname = (char*)SvPV(autoload, slen);
  if (funcname->length() == 0)
  {
    autoload = perl_get_sv("AUTOLOAD", 0);
    if (autoload != 0)
      funcname = (char*)SvPV(autoload, slen);
    else
    {
      autoload = perl_get_sv("::AUTOLOAD", 0);
      if (autoload != 0)
        funcname = (char*)SvPV(autoload, slen);
    }
  }
  if (autoload == 0)
    autoload = sicautoload;
  int idx = funcname->lastIndexOf("::");
  if (idx == -1)
  {
    StringBuffer sb;
    ScriptVar sv;
    perl2acdk(autoload, sv);
    sb << "autoload [" << sv.toCode() << "] ";
    ScriptVarArray sargs(items);
    
    for (int i = 0; i < items; i++) 
    {
	if (perl2acdk(ST(i), sargs[i]) == false)
	    croak("Converting argument failed");
	sb << " [" << sargs[i].toCode() << "]";
    }
    RString s = "Error in $acdk::AUTOLOAD: funcname not in correct format: [" + sb.toString() + "]";
    croak(s->c_str());
  }
  funcname = funcname->substr(idx + 2);
  
  int offset = 1;
  ScriptVarArray sargs(items - offset);
  for (int i = offset; i < items; i++) 
  {
    if (perl2acdk(ST(i), sargs[i - offset]) == false)
      croak("Converting argument failed");
  }
  ScriptVar ret;
  if (object->standardDispatch(funcname, ret, sargs, AcdkDmiClient::getDmiClient(), Nil, MiPublic , 0) == 0)
      croak("Dispatching invoke to ACDK-Object failed!");
  
  if (acdk2perl(ret, ST(0)) == false)
    croak("acdk_new: cannot convert return value to perl value");
  XSRETURN(1);
  PUTBACK;
}



XS(acdk_invoke)
{
  dXSARGS;
  if (items < 2)
    croak("acdk_invoke needs object instance as first parameter and function name as second");
  unsigned slen = 0;
  Object* oinst = PERL2THIS();
  if (oinst == 0)
    croak("acdk_invoke: object is Nil!");
  if (SvPOK(ST(1)) == 0)
    croak("acdk_invoke: second arg must be an argument name!");
  RString funcname = (char*)SvPV(ST(1), slen);

  int offset = 2;
  ScriptVarArray sargs(items - offset);
  for (int i = offset; i < items; i++) {
    if (perl2acdk(ST(i), sargs[i - offset]) == false)
      croak("Converting argument failed");
  }
  ScriptVar ret;
  if (oinst->standardDispatch(funcname, ret, sargs, AcdkDmiClient::getDmiClient(), Nil, MiPublic, 0) == 0)
      croak("Dispatching invoke to ACDK-Object failed!");

  if (acdk2perl(ret, ST(0)) == false)
    croak("acdk_invoke: cannot convert return value to perl value");
  XSRETURN(1);
  PUTBACK;
}

using acdk::lang::dmi::AcdkDmiClient;
XS(acdk_invoke_static)
{
  dXSARGS;
  if (items < 2)
    croak("acdk_invoke_static needs class name as first parameter and function name as second");
  
  unsigned int slen = 0;
  char* ptr = (char*)SvPV(ST(0), slen);
  RString clsname = toClazzName(RString(ptr));
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    croak("acdk_invoke_static: class cannot be found");

  if (SvPOK(ST(1)) == 0)
    croak("acdk_invoke_static: second arg must be an function name!");
  RString funcname = (char*)SvPV(ST(1), slen);

  int offset = 2;
  ScriptVarArray sargs(items - offset);
  for (int i = offset; i < items; i++) {
    if (perl2acdk(ST(i), sargs[i - offset]) == false)
      croak("Converting argument failed");
  }
  ScriptVar ret;
  if (cls->objectClazzInfo()->static_dispatch(funcname, ret, sargs, 
                                              AcdkDmiClient::getDmiClient(), 
                                              Nil, MiPublic | MiStatic,
                                              cls->objectClazzInfo(), 0) == 0)
  {
      croak("Dispatching invoke to ACDK-Object failed!");
  }
  if (acdk2perl(ret, ST(0)) == false)
    croak("acdk_invoke_static: cannot convert return value to perl value");
  XSRETURN(1);
  PUTBACK;
}


ScriptVar
getStatic(void* address, const ClazzInfo* clazzInfo)
{
  if (clazzInfo == ClazzInfo::getBoolClazz())
    return *((bool*)address);
  if (clazzInfo == ClazzInfo::getCharClazz())
    return *((char*)address);
  if (clazzInfo == ClazzInfo::getByteClazz())
    return *((byte*)address);
  if (clazzInfo == ClazzInfo::getShortClazz())
    return *((short*)address);
  if (clazzInfo == ClazzInfo::getIntClazz())
    return *((int*)address);
  if (clazzInfo == ClazzInfo::getIntClazz())
    return *((int*)address);
  if (clazzInfo == ClazzInfo::getFloatClazz())
    return *((float*)address);
  if (clazzInfo == ClazzInfo::getDoubleClazz())
    return *((double*)address);
  return *((RObject*)address);
}




XS(acdk_new)
{
  dXSARGS;
  //std::cout << "acdk_new called" << std::endl;
  if (items == 0)
    croak("acdk_new needs class name as first parameter");
  if (SvPOK(ST(0)) == 0) {
    croak("acdk_new needs class name as first parameter");
  }
  
  unsigned int slen = 0;
  char* ptr = (char*)SvPV(ST(0), slen);
  RString clsname = toClazzName(RString(ptr));
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    croak("acdk_new: class cannot be found");
  ScriptVarArray sa(items - 1);
  int offset = 1;
  for (int i = offset; i  < items; i++) {
    if (perl2acdk(ST(i), sa[i - offset]) == false)
      croak("Converting argument failed");
  }
  

  
  RString constructorname = clsname;
  ScriptVar ret;
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  
  if (cls->objectClazzInfo()->static_dispatch(constructorname, ret, sa, AcdkDmiClient::getDmiClient(), Nil, 
                                              MiPublic | MiMiConstructor, cls->objectClazzInfo(), 0) == 0)
    croak("acdk_new: failed!");
  //SV* sv = 0;

  if (acdk2perl(ret, ST(0)) == false)
    croak("acdk_new: cannot convert return value to perl value");
  //sv_dump(ST(0));
  //sv = ST(0);
  //ST(0) = newSVrv(ST(0), "acdk");
  //sv_dump(ST(0));
  //SV* rv = sv_2mortal(newRV_noinc (sv));
  //sv_bless (rv, SvROK (rv) ? SvSTASH (SvRV (rv)) :               gv_stashsv (rv, 0));

  //Perl_sv_bless(my_perl, ST(0), newHV());
  //SV* clsn = newSVpv("acdk", 4);
  //SV* thisref = sv_newmortal();
  //sv_setref_pv(thisref, "acdk", ST(0));
//  Perl_sv_bless(my_perl, clsn, (HV*)thisref);
  //sv_bless(newRV((SV*)ST(0)), gv_stashpv("acdk", 1));

  XSRETURN(1);
  PUTBACK;
}

XS(acdk_peek)
{
  dXSARGS;
  if (items != 2)
    croak("acdk::peek needs needs an Objects as first parameter and member name as second");
  
  Object* oinst = PERL2THIS();
  if (oinst == 0)
    croak("acdk_peek: object is Nil!");

  
  if (SvPOK(ST(1)) == 0)
    croak("acdk_peek: second arg must be an function name!");
  unsigned int slen = 0;
  RString fieldname = (char*)SvPV(ST(1), slen);
  ScriptVar tvar = oinst->peek(fieldname);
  if (acdk2perl(tvar, ST(0)) == false)
    croak("acdk::peek: cannot convert return value to perl value");

  XSRETURN(1);
  PUTBACK;
}
/*
bool __get_static_member(RClass cls, RString fieldname, ScriptVar& erg)
{
  bool found = false;
  for (int i = 0; cls->objectClazzInfo()->fields[i] != 0; i++) {
    if (strcmp(cls->objectClazzInfo()->fields[i]->label, fieldname->c_str()) == 0) {
      if (cls->objectClazzInfo()->fields[i]->address != 0 && cls->objectClazzInfo()->fields[i]->address != (void*)-1) {
        ScriptVar tvar = getStatic(cls->objectClazzInfo()->fields[i]->address, cls->objectClazzInfo()->fields[i]->clazzInfo);
        if (acdk2perl(tvar, ST(0)) == false)
          croak("acdk::peek_static: cannot convert return value to perl value");
        found = true;
        break;

      }
    }
  }
}
*/



XS(acdk_peek_static)
{
  dXSARGS;
  if (items != 2)
    croak("acdk::peek_static needs needs class name as first parameter and member name as second");
  
  unsigned int slen = 0;
  char* ptr = (char*)SvPV(ST(0), slen);
  RString clsname = toClazzName(RString(ptr));
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    croak("acdk::peek_static: class cannot be found");

  if (SvPOK(ST(1)) == 0)
    croak("acdk_peek_static: second arg must be an function name!");
  RString fieldname = (char*)SvPV(ST(1), slen);
  
  bool found = false;
  for (int i = 0; cls->objectClazzInfo()->fields[i] != 0; i++) 
  {
    if (strcmp(cls->objectClazzInfo()->fields[i]->name, fieldname->c_str()) == 0) {
      if (cls->objectClazzInfo()->fields[i]->address != 0 && cls->objectClazzInfo()->fields[i]->address != (void*)-1) {
        ScriptVar tvar = getStatic(cls->objectClazzInfo()->fields[i]->address, cls->objectClazzInfo()->fields[i]->type);
        if (acdk2perl(tvar, ST(0)) == false)
          croak("acdk::peek_static: cannot convert return value to perl value");
        found = true;
        break;

      }
    }
  }
  if (found == false)
    croak("acdk::peek_static: member cannot be found");
  
  XSRETURN(1);
  PUTBACK;
}

XS(acdk_poke)
{
  dXSARGS;
  if (items != 3)
    croak("acdk::poke needs needs an Objects as first parameter and member name as second and new value as thirt");
  
  Object* oinst = PERL2THIS();
  if (oinst == 0)
    croak("acdk::poke: object is Nil!");

   
  if (SvPOK(ST(1)) == 0)
    croak("acdk::poke: second arg must be an Argument!");
  unsigned int slen = 0;
  RString fieldname = (char*)SvPV(ST(1), slen);
    
  ScriptVar tvar;
  if (perl2acdk(ST(2), tvar) == false)
    croak("acdk::poke: cannot convert value to perl value");
  
  oinst->poke(fieldname, tvar);

  acdk2perl(tvar, ST(0));
  XSRETURN(1);
  PUTBACK;
}

XS(acdk_poke_static)
{
  dXSARGS;
  if (items != 3)
    croak("acdk::poke_static needs needs a class name as first parameter and member name as second and new value as thirt");
  
  unsigned int slen = 0;
  char* ptr = (char*)SvPV(ST(0), slen);
  RString clsname = toClazzName(RString(ptr));
  /*
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    croak("acdk::peek_static: class cannot be found");
  */
  
  if (SvPOK(ST(1)) == 0)
    croak("acdk::poke: second arg must be an Argument!");

  RString fieldname = (char*)SvPV(ST(1), slen);
    
  ScriptVar tvar;
  if (perl2acdk(ST(2), tvar) == false)
    croak("acdk::poke: cannot convert value to perl value");
  
  StdDispatch::poke_static(clsname, fieldname, tvar);

  acdk2perl(tvar, ST(0));
  XSRETURN(1);
  PUTBACK;
}



void registerACDKEntries()
{
  char* file = __FILE__;

  newXS("acdk::acdk2perl", acdk_acdk2perl, file);
  newXS("acdk::perl2acdk", acdk_perl2acdk, file);
  newXS("acdk::new", acdk_new, file);
  newXS("acdk::invoke", acdk_invoke, file);
  newXS("acdk::invoke_static", acdk_invoke_static, file);
  newXS("acdk::peek", acdk_peek, file);
  newXS("acdk::peek_static", acdk_peek_static, file);
  newXS("acdk::poke", acdk_poke, file);
  newXS("acdk::poke_static", acdk_poke_static, file);
  newXS("acdk::DESTROY", XM_acdk_ObjectWrapper_DESTROY, file);
  newXS("acdk::AUTOLOAD", XM_acdk_ObjectWrapper_AUTOLOAD, file);
#ifdef LOCAL_DEBUG
  CV* aacv =  get_cv("acdk::AUTOLOAD", 0);
  HV* aastash = CvSTASH(aacv);
  HV* apackagestash  = gv_stashpv("acdk", 0);
  HV* papackagestash  = gv_stashpv("pacdk", 0);
  aacv->sv_any->xcv_stash = apackagestash;
  //overwrites autload func hv_store(apackagestash, "AUTOLOAD", strlen("AUTOLOAD"), newSVpv("", 0), 0);
  aacv->sv_any->xcv_padlist = newAV();

  //aacv->sv_any->xmg_stash = apackagestash;
  CV* paacv =  get_cv("pacdk::AUTOLOAD", 0);
  //aacv->sv_any->xmg_magic = cv->sv_any->xcv_gv->sv_any->xmg_magic;
  std::cout << "acdk::AUTOLOAD CV: " << aacv << "; packagestash: " << std::endl;
  //std::cout << "pacdk::AUTOLOAD CV: " << paacv << "; packagestash: " << std::endl;
#endif
}

