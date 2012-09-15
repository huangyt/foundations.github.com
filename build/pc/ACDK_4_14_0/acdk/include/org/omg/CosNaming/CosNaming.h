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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CosNaming/CosNaming.h,v 1.22 2005/04/10 12:52:41 kommer Exp $

#ifndef org_omg_CosNaming_h
#define org_omg_CosNaming_h

#include <acdk/io/Serializable.h>
#include <acdkx/orb/orb.h>
#include <acdkx/orb/std_orb.h>

#include <org/omg/CosNaming/Config.h>

ACDK_DECL_UNIT(org_omg_CosNaming)

namespace org {
namespace omg {
/**
  contains interfaces for the CORBA Naming service
*/
namespace CosNaming {

ACDK_DECL_CLASS(NameComponent);

/** 
  class NameComponent
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC NameComponent
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(NameComponent)
public:
  RString id;
  RString kind;  
  foreign virtual int hashCode()
  {
    return id->hashCode() * 31 + kind->hashCode();
  }
  NameComponent() { }
  foreign NameComponent(IN(RString) id_, IN(RString) kind_) 
    : id(id_)
    , kind(kind_)
  { 
  }
  
};


enum BindingType 
{
  nobject = 0, 
  ncontext
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, BindingType);


ACDK_DECL_CLASS(Binding);
/**
IDL: 
struct Binding 
{
		Name        binding_name;
		BindingType binding_type;                                  
};
*/

ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC Binding
: extends ::acdk::lang::Object
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Binding)
public:
  RNameComponentArray binding_name;
  BindingType binding_type;
  Binding() {}
  foreign Binding(IN(RNameComponentArray) binding_name_, BindingType binding_type_) 
  : binding_name(binding_name_)
  , binding_type(binding_type_)
  {
  }
};



/**
IDL: 
typedef sequence <Binding> BindingList;      
*/
/**
IDL:
interface BindingIterator;
*/

enum NotFoundReason 
{	
  missing_node, 
	not_context, 
	not_object
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, NotFoundReason);

ACDK_DECL_THROWABLE_FQ(NotFound, ::acdkx::orb::, OrbException);
using ::acdkx::orb::OrbException;

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CosNaming/NamingContext/NotFound"))
class ACDKX_ORB_PUBLIC NotFound 
: extends ::acdkx::orb::OrbException
{ 	
  ACDK_WITH_METAINFO(NotFound)
  
public:
  NotFound(RString what)
  : OrbException(what)
  , why(missing_node)
  , rest_of_name("")
  {
  }
  NotFound(NotFoundReason w, RString rn)
  : OrbException()
  , why(w)
  , rest_of_name(rn)
  {
  }
  NotFoundReason why;
	RString rest_of_name;
};

ACDK_DECL_INTERFACE(NamingContext);

ACDK_DECL_THROWABLE_FQ(CannotProceed, ::acdkx::orb::, OrbException);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CosNaming/NamingContext/CannotProceed"))
class ACDKX_ORB_PUBLIC CannotProceed 
: extends ::acdkx::orb::OrbException
{ 	
  ACDK_WITH_METAINFO(CannotProceed)
public:
  CannotProceed(IN(RNamingContext) nct, IN(RString) rn);
  RNamingContext cxt;
	RString rest_of_name;
};
		


ACDK_DECL_THROWABLE_FQ(InvalidName, ::acdkx::orb::, OrbException);
/**
  IDL: exception InvalidName{};                                                                        
*/
ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CosNaming/NamingContext/InvalidName"))
class ACDKX_ORB_PUBLIC InvalidName
: extends ::acdkx::orb::OrbException
{ 
  ACDK_WITH_METAINFO(InvalidName)  
public: 
  InvalidName() : OrbException() {} 
  InvalidName(IN(RString) msg) : OrbException(msg) {} 
};
ACDK_BCC_RTHROWABLE_DEFINITION(InvalidName);


ACDK_DECL_THROWABLE_FQ(AlreadyBound, ::acdkx::orb::, OrbException);
/**
exception AlreadyBound{};                                                                        
*/
ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CosNaming/NamingContext/AlreadyBound"))
class ACDKX_ORB_PUBLIC AlreadyBound
: extends ::acdkx::orb::OrbException
{ 
  ACDK_WITH_METAINFO(AlreadyBound)  
public: 
  AlreadyBound() : OrbException() {} 
  AlreadyBound(IN(RString) msg) : OrbException(msg) {} 
};

ACDK_DECL_THROWABLE_FQ(NotEmpty, ::acdkx::orb::, OrbException);
/**
exception NotEmpty{};                                                                        
*/
ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CosNaming/NamingContext/NotEmpty"))
class ACDKX_ORB_PUBLIC NotEmpty
: extends ::acdkx::orb::OrbException
{ 
  ACDK_WITH_METAINFO(NotEmpty)  
public: 
  NotEmpty() : OrbException() {} 
  NotEmpty(IN(RString) msg) : OrbException(msg) {} 
};


ACDK_DECL_INTERFACE(BindingIterator);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC BindingIterator 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(BindingIterator)
public:
  ACDK_CORBA_INTERFACE(BindingIterator)
public:  
  virtual bool next_one(OUT(RBinding) b) = 0;
  virtual bool next_n(int how_many, OUT(RBindingArray) bl) = 0;
  virtual void destroy() = 0;
};


ACDK_DECL_INTERFACE(NamingContext);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC NamingContext 
: implements ::org::omg::CORBA::portable::InvokeHandler
{
  ACDK_WITH_METAINFO(NamingContext)
public:
  ACDK_CORBA_INTERFACE(NamingContext)
public:  
  /**
    IDL: void bind(	in Name n, in Object obj) raises(	NotFound, CannotProceed, InvalidName, AlreadyBound); */
  virtual void bind(IN(RNameComponentArray) n, IN(::org::omg::CORBA::RObject) obj) 
      THROWS4(RNotFound, RCannotProceed, RInvalidName, RAlreadyBound) = 0;

  /**
    IDL: void rebind(in Name n, 
					in Object obj)   
			raises(	NotFound, 
					CannotProceed, 
					InvalidName);
  */
  virtual void rebind(IN(RNameComponentArray) n, IN(::org::omg::CORBA::RObject) obj) 
      THROWS3(RNotFound, RCannotProceed, RInvalidName) = 0;
  /**
    IDL: 	void bind_context(	in Name n, 
							in NamingContext nc)               
			raises(	NotFound, 
					CannotProceed, 
					InvalidName, 
					AlreadyBound);
	*/
  virtual void bind_context(IN(RNameComponentArray) n, IN(RNamingContext) nc) 
      THROWS4(RNotFound, RCannotProceed, RInvalidName, RAlreadyBound) = 0;
	/**
    IDL: 
      void rebind_context(in Name n, 
							in NamingContext nc)     
			raises(	NotFound, 
					CannotProceed, 
					InvalidName);
    */
  virtual void rebind_context(IN(RNameComponentArray) n, IN(RNamingContext) nc)     
			THROWS3(RNotFound, RCannotProceed, RInvalidName) = 0;
  /**
    IDL:
    Object resolve (in Name n)
			raises(	NotFound, 
					CannotProceed, 
					InvalidName);
  */
  virtual ::org::omg::CORBA::RObject resolve(IN(RNameComponentArray) n)
			THROWS3(RNotFound, RCannotProceed, RInvalidName) = 0;
  /**
    IDL: 
      void unbind(in Name n)
			raises(	NotFound, 
					CannotProceed, 
					InvalidName);
  */
  virtual void unbind(IN(RNameComponentArray) n) THROWS3(RNotFound, RCannotProceed, RInvalidName) = 0;
  /**
    IDL: NamingContext new_context();
  */
  virtual RNamingContext new_context() = 0;
  /**
    IDL: NamingContext bind_new_context(in Name n)
			raises(	NotFound, 
					AlreadyBound, 
					CannotProceed, 
					InvalidName);
  */
  virtual RNamingContext bind_new_context(IN(RNameComponentArray) n)
			THROWS4(RNotFound, RAlreadyBound, RCannotProceed, RInvalidName) = 0;
  /**
    IDL: void destroy( )
			raises(NotEmpty);
  */
  virtual void destroy() THROWS1(RNotEmpty) = 0;
  /**
    IDL: void list (	in unsigned long how_many, 
					out BindingList bl, 
					out BindingIterator bi);
        };
  */
  virtual void list (int how_many, OUT(RBindingArray) bl, OUT(RBindingIterator) bi) = 0;

};

inline
CannotProceed::CannotProceed(IN(RNamingContext) nct, IN(RString) rn)
  : OrbException()
  , cxt(nct)
  , rest_of_name(rn)
  {
  }



} // namespace CosNaming 
} // namespace omg
} // namespace org



#endif //org_omg_CosNaming_h
