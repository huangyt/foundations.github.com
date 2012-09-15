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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaInfoFlags.h,v 1.17 2005/02/28 08:12:54 kommer Exp $

#ifndef acdk_lang_dmi_MetaInfoFlags_h
#define acdk_lang_dmi_MetaInfoFlags_h


namespace acdk {
namespace lang {
namespace dmi {

/**
  General Flags for all MetaInfo types
  @see acdk::lang::dmi::ClazzInfoExtFlags
  @see acdk::lang::dmi::FieldInfoExtFlags
  @see acdk::lang::dmi::MethodArgInfoExtFlags
  @see acdk::lang::dmi::MethodInfoExtFlags
  @see acdk::lang::dmi::ClazzInvokeInfo
*/
enum MetaInfoFlags
{
  /**
    MetaInfo was generated dynamically
    and can be destroyed in unregister
  */
  MiDelete    =         0x00000001,
  /**
    MetaInfo is registered in database
  */
  MiRegistered  =       0x00000002,
  /**
    extended Metainfo for this class is loaded.
  */
  MiResolved  =         0x00000004,
  /**
    Aquivalent to 'const'
  */
  MiReadOnly =          0x00000008,
  /*
    type is public
  */
  MiPublic =            0x00000010,
  /**
    type is private
  */
  MiPrivate =           0x00000020,
  /**
    type is protected
  */
  MiProtected =         0x00000040,
  /**
    member is static
  */
  MiStatic =            0x00000080,
  /**
    member is not static
  */
  MiNonStatic =         0x00000100, // was NonStatic
  /**
    MetaInfo represent ClazzInfo
  */
  MiClazzInfo        =  0x00000200,
  /**
    MetaInfo represent UnitInfo
  */
  MiUnitInfo         =  0x00000400,
  /**
    MetaInfo represent EnumInfo
  */
  MiEnumInfo         =  0x00000800,
  /**
    MetaInfo represent EnumValInfo
  */
  MiEnumValInfo      =  0x00001000,
  /**
    MetaInfo represent MethodInfo
  */
  MiMethodInfo       =  0x00002000,
  /**
    MetaInfo represent MethodArgInfo
  */
  MiMethodArgInfo    =  0x00004000,
  /**
    MetaInfo represent FieldInfo
  */
  MiFieldInfo        =  0x00008000,
  /**
    MetaInfo represent SuperInfo
  */
  MiSuperInfo        =  0x00010000,
    
  MiMetaInfoTypeMask = MiClazzInfo | MiUnitInfo | MiUnitInfo | MiEnumInfo | MiMethodInfo | MiMethodArgInfo | MiFieldInfo | MiSuperInfo,

  /**
    Type definition must not be modified.
    Currently not supported
  */
  MiIsSealed         =  0x00020000,
  /**
    Tag a Method or Class not to generate 
    a DmiProxy implementation for this class or method
  */
  MiNoDmiProxy       =  0x00040000
};

/**
  MetaInfo flags specific to ClazzInfo
  [M]eta[i]nfo[C]lass[i]info
*/
enum ClazzInfoExtFlags
{
  /**
    Client should not regard method or
    member information before calling.
    In case of member access (peek/poke/peek_static/poke_static)
    call method with function name peek/poke/peek_static/poke_static
  
    @see MiIvWeakBind
  */
  MiCiWeakBind    =     0x00100000,
  /**
    This type represents a basic atomar type
  */
  MiCiBasicType =       0x00200000,
  /**
    This type represents an interface
  */
  MiCiInterface =       0x00400000,
  /**
    This type cannot be constructed, because
    it has an abstract method (pure virtual)
  */
  MiCiAbstract =        0x00800000,
  /**
    This type is an exception (derived from acdk::lang::Throwable)
  */
  MiCiThrowable =       0x01000000,
  /**
    This type represents an array of basic or object type
  */
  MiCiArray =           0x02000000,
  /**
    This type is serializable
  */
  MiCiSerializable =    0x04000000,
  /**
    this type is cloneable
  */
  MiCiCloneable     =   0x08000000,

};


/**
  MetaInfo flags specific to ClazzMethodInfo
  [M]eta[i]nfo[F]ield[i]info
*/
enum FieldInfoExtFlags
{
  /**
    This field should not be serialized
  */
  MiFiTransient    =   0x00100000
};



/**
  MetaInfo flags specific to ClazzMethodArgInfo
  [M]eta[i]nfo[A]rgument[i]info
*/

enum MethodArgInfoExtFlags
{
  /**
    This argument is sent from caller to callee.
    Callee must not modified the value
  */
  MiAiIn    =           0x00100000,
  /**
    This argument is sent from callee to caller.
  */
  MiAiOut   =           0x00200000,
  /**
    This argument is sent from caller to caller and back to caller.
  */
  MiAiInOut =     MiAiOut | MiAiIn,
  /**
    This value is sent as value.
    basic types are send by val by default.
    Object types has to support serialization.
    Flag can be combined with MiAiIn and MiAiOut
  */
  MiAiByval =           0x00400000,
  /**
    calling object reference as reference is default
    But in remote calls this forces to pass argument
    as (remote) reference, even if the class is implements the serializable interface
  */
  MiAiByref =           0x00800000,
  /**
    This argument has default initializer
  */
  MiAiHasDefaultInit =  0x01000000
};

/**
  MetaInfo flags specific to ClazzFieldInfo
*/
enum MethodInfoExtFlags
{
  /**
    return type
    @see MiAiIn
  */
  MiMiIn    =           MiAiIn, //0x00100000,
  /**
    return type
    @see MiAiOut
  */
  MiMiOut   =           MiAiOut, //0x00200000,
  /**
    return type
    @see MiAiInOut
  */
  MiMiInOut =     MiMiOut | MiMiIn,
  /**
    return type
    @see MiAiByval
  */
  MiMiByval       =     MiAiByval, //0x00400000,
  /**
    Method is abstract (pure virtual) 
  */
  MiMiAbstract    =     MiCiAbstract, //0x00800000,
  /**
    This method is virtual
  */
  MiMiVirtual     =     0x01000000,
  /**
    This method is a constructor
  */
  MiMiConstructor =     0x02000000,
  /**
    This method is a destructor
  */
  MiMiDestructor  =     0x04000000,
  /**
    This method can be called derevered
  */
  MiMiOneway      =     0x08000000,
  /**
    This method is not derived from
    methods with default parameter.
    @code
      class AClass
      {
        virtual void bar(); // has MiMiOrgPolym
        virtual void foo(int i = 0); 
          -> foo() // has NO MiMiOrgPolym
          -> foo(int i) // has MiMiOrgPolym
      };
    @codeend
  */
  MiMiOrgPoly    =     0x20000000,
  /**
    Used to tag a method, which is overwritten
    by a scripting implementation.
    used in the proxy implementation
  */
  MiMiDmiImpl   =      0x40000000
};

/**
  MetaInfo flags used in the invoke interface
*/
enum ClazzInvokeInfo
{
  /**
    force calling with weak/ late binding
    @see MiCiWeakBind
  */
  MiIvWeakBind      = MiCiWeakBind, //0x00100000
  /**
    Used for scripting.
    Constructor must be allowed to call parent protected constructor
  */
  MiIvTransientCall = 0x00200000, // TRANSIENT
  /**
    Find only members, which are declared in
    this class
  */
  MiIvDeclared      = 0x00400000, //old 0x02000000,  
  /**
    Call constructor
  */
  MiIvConstructor   = MiMiConstructor, //0x02000000
  /**
    Find method using only alternative names
  */
  MiIvViaAltName    = 0x00800000,
  /**
    Invoke method oneway. Don't wait for 
    result
  */
  MiIvOneway        = MiMiOneway, //0x01000000
  
  /**
    Find method via method hash value
    (name, return and argument types
  */
  MiIvViaHash       = 0x04000000,
  /**
    in looking for members/methods don't throw
    exception if not found
  */
  MiIvNoThrowIfNotFound = 0x10000000,
  /**
    Used by MetaInfo to search symbols
    deeply
  */
  MiIvSearchMiDeep = 0x20000000,
  /**
    ignore weak binding in this call
  */
  MiIvNoWeakBind = 0x80000000
};



/**
  Used for MetaCompiler, will not appear in MetaInfo
  directly
*/
enum ClazzMcFlags
{
  /** 
    Parsed Type is known
  */
  MiMcKnownType     = 0x10000000,
  /**
    Used in for argument types
    if argument is an enumeration (internally
    handled as int).
  */
  MiMcIsEnumeration = 0x20000000

  // following are moved
  /*
    Used to tag a method or class not to 
    generate a corresponding DMI proxy
  */
  //MiMcNoDmiProxy   =      0x40000000,
  /*
    don't generate MetaInfo for this element
  */
  //MiMcNoMetaInfo = 0x80000000,
  /*
    this class the method
    void getCollectableFields(FieldReferences& fields);
    will be implemented manually
  */
  //MiMcOwnCollectableFields = 0x00020000


};

} // namespace dmi
} // namespace acdk
} // namespace lang


#endif //acdk_lang_dmi_MetaInfoFlags_h

