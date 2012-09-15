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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaInfo.h,v 1.16 2005/04/14 10:41:55 kommer Exp $

#ifndef acdk_lang_dmi_MetaInfo_h
#define acdk_lang_dmi_MetaInfo_h

#include "MetaInfoFlags.h"


namespace acdk {
namespace lang {


class Class;
class StringBuffer;

namespace dmi {

#if ACDK_CHECK_GCC_VERSION(4,0)
class ClazzInfo;
class ClazzEnumInfo;
#else
class ACDK_CORE_PUBLIC ClazzInfo;
class ACDK_CORE_PUBLIC ClazzEnumInfo;
#endif

/**
 Used to print formated types
*/
enum TypeNameFormat
{
  /**
    Format type as AcdkClass with :: between parts
    "acdk::lang::StringArray"
  */
  TpFtAcdkType         = 0x0001,
  /**
    Format type as Java Class
    acdk.lang.String[]
  */
  TpFtJavaType         = 0x0002,
  /**
    Format type as loadable class
    "[acdk/lang/String"
  */
  TpFtLoadableClass     = 0x0004,
  /**
    return type as java signature
  */
  TpFtJavaSignature     = 0x0008,
  /**
    return type as acdk signature
    this identifier used in ClazzInfo metainfo
  */
  TpFtACDKSignature     = 0x0010,
  /**
    return string as IDL
  */
  TpFtIDL               = 0x0020,
  /**
    only used if also TpFtAcdkClass is set
    "acdk::lang::RStringArray"
  */
  TpFtRHPrefix          = 0x0040,
  /**
    print only type name
  */
  TpFtTypeName         = 0x0100,
  /**
    print only unit name
  */
  TpFtUnitName          = 0x0200,
  /**
    Print decls with flags (public, private, in, out, etc)
  */
  TpFtAttrFlags         = 0x0400,
  /**
    Print Meta attributes
  */
  TpFtAttributes        = 0x0800,
  /**
    print fully qualified
  */
  TpFtFqName            = TpFtTypeName | TpFtUnitName,
  /**
    Standard format
  */
  TpFtFormatStandard      = TpFtAcdkType | TpFtRHPrefix | TpFtFqName | TpFtAttrFlags,

  /**
    print only name
  */
  TpFtName      =  0x0000,
  /**
    Print type declaration
  */
  TpFtTypeDecl   =  0x1000,
  /**
    Print complete definition
  */
  TpFtTypeDef  =  0x2000
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, TypeNameFormat);

struct MetaInfo;

/**
  implement this to listen to changes to MetaInfo database
  use MetaInfo::registerMetaInfoListener/unregisterMetaInfoListener
  to listen for changes
*/
foreign
class MetaInfoChangeListener
{
public:
  virtual ~MetaInfoChangeListener() {}
  /**
    will be called if a new MetaInfo was created
  */
  virtual void onNewMetaInfo(MetaInfo* mi) {}
  /**
    will be called if a MetaInfo was resolved
  */
  virtual void onResolvedMetaInfo(MetaInfo* mi) {}
  /**
    will be called if one metainfo will be replaced by another
    (for example UnitInfos)
  */
  virtual void onReplaceMetaInfo(MetaInfo* oldMetaInfo, MetaInfo* newMetaInfo) {}
  virtual void onUnregisterMetaInfo(MetaInfo* mi) {}
  /**
    will be called before a MetaInfo will be disposed (deleted)
  */
  virtual void onDisposeMetaInfo(MetaInfo* mi) {}
};


/**
  All MetaInfo (UnitInfo, ClazzInfo, MethodInfo, etc.
  has the common information of flags and attributeRes
*/
foreign struct ACDK_CORE_PUBLIC MetaInfo
{
public:
  /** @see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;
  /**
    render as ACDK source
    @param sb where to append
    @param flags method flags
  */
  static void flagsToTypeDecl(StringBuffer& sb, int flags, int renderFlags);
  static int calcHashValue(int flags);

  
  /**
    Search in UnitInfo, EnumInfo and ClazzInfo
    @param flags select
    @param tryLoad if true, use classLoader to find given type. Default is false
    @return 0 if non found
  */
  static const MetaInfo* findMetaInfo(IN(RString) name, int flags = 0, bool tryLoad = false);
  /** ns and cname are in normalized form */
  static const MetaInfo* findMetaInfoNormalized(IN(RString) name, int flags = 0, bool tryLoad = false);

  static void findMetaInfos(acdk::lang::sys::core_vector<const MetaInfo*>& vec, IN(RString) name, int flags = 0, bool tryLoad = false);
  /** ns and cname are in normalized form */
  static void findMetaInfosNormalized(acdk::lang::sys::core_vector<const MetaInfo*>& vec, IN(RString) ns, int flags = 0, bool tryLoad = false);
  inline bool isDelete() const { return flags & MiDelete; }
  inline bool isClazzInfo() const { return flags & MiClazzInfo; }
  inline bool isUnitInfo() const { return flags & MiUnitInfo; }
  inline bool isEnumInfo() const { return flags & MiEnumInfo; }
  inline bool isEnumValInfo() const { return flags & MiEnumValInfo; }
  inline bool isMethodInfo() const { return flags & MiMethodInfo; }
  inline bool isMethodArgInfo() const { return flags & MiMethodArgInfo; }
  inline bool isFieldInfo() const { return flags & MiFieldInfo; }
  inline bool isSuperInfo() const { return flags & MiSuperInfo; }

  inline static bool isResolved(int flags) { return MiResolved & flags; }

  inline static bool isPublic(int mod) { return (mod & MiPublic) == MiPublic; }
  inline bool isPublic() const { return isPublic(flags); }

  inline static bool isPrivate(int mod) { return (mod & MiPrivate) == MiPrivate; }
  inline bool isPrivate() const { return isPrivate(flags); }

  inline static bool isProtected(int mod) { return (mod & MiProtected) == MiProtected; }
  inline bool isProtected() const { return isProtected(flags); }

  inline static bool isStatic(int mod) { return (mod & MiStatic) == MiStatic; }
  inline bool isStatic() const { return isStatic(flags); }

  inline static int staticMask(int flags)
  {
    return (flags & MiStatic) | (flags & MiNonStatic);
  }
  inline static int accessMask(int flags)
  {
    return (flags & MiPublic) | (flags & MiPrivate) | (flags & MiProtected);
  }
  inline static int parameterMask(int flags)
  {
    return (flags & MiAiIn) | (flags & MiAiOut) | (flags & MiAiByval) | (flags & MiReadOnly);
  }
  inline static bool isIn(int flags)
  {
    return (flags & MiAiIn) == MiAiIn || (flags & MiAiOut) != MiAiOut;
  }
  inline static bool isOut(int flags)
  {
    return (flags & MiAiOut) == MiAiOut;
  }
  inline static bool checkMemberAccess(int regflags, int hasflags)
  {
    if ((regflags & MiStatic) == MiStatic && (hasflags & MiStatic) != MiStatic)
      return false;
    if ((regflags & MiNonStatic) == MiNonStatic && (hasflags & MiStatic) == MiStatic)
      return false;
    if ((regflags & MiPublic) == MiPublic &&
        ((hasflags & MiProtected) == MiProtected || (hasflags & MiPrivate) == MiPrivate))
      return false;
    if ((regflags & MiProtected) == MiProtected && (hasflags & MiPrivate) == MiPrivate)
      return false;
    return true;
  }

  /**
    uses the toTypeString function of ClazzInfo, etc.
  */
  RString toString(int format = TpFtFormatStandard) const;
  static RString flagsToString(int flags, ClazzInfoExtFlags dummy, int formatFlags = TpFtAcdkType);
  static RString flagsToString(int flags, FieldInfoExtFlags dummy, int formatFlags = TpFtAcdkType);
  static RString flagsToString(int flags, MethodInfoExtFlags dummy, int formatFlags = TpFtAcdkType);
  static RString flagsToString(int flags, MethodArgInfoExtFlags dummy, int formatFlags = TpFtAcdkType);
  static RString flagsToString(int flags, ClazzInvokeInfo dummy, int formatFlags = TpFtAcdkType);

  void copyFrom(const MetaInfo* source, bool deep);
  void dispose();
  static char* strdup(const char* n);
  static void strdel(const char*& n);
  void setString(const char*& target, const char* n);
  static void registerMetaInfoListener(MetaInfoChangeListener* listner);
  static void unRegisterMetaInfoListener(MetaInfoChangeListener* listner);

  /// internal helper to call MetaInfoChangeListener callbacks 
  void onNewMetaInfo();
  void onResolvedMetaInfo();
  void onReplaceMetaInfo(MetaInfo* newMetaInfo);
  void onUnregisterMetaInfo();
  void onDisposeMetaInfo();
};

/*
  Metainf with name
*/
foreign struct ACDK_CORE_PUBLIC NamedMetaInfo
: public MetaInfo
{
  /**
    Name of this MetaInfo
  */
  const char* name;
  int nameHashCode;
  void copyFrom(const NamedMetaInfo* source, bool deep);
  void dispose();
  int getNameHashCode() const
  {
    if (nameHashCode != -1)
      return nameHashCode;
    _calcNameHashCode();
    return nameHashCode;
  }
  bool equalsName(const NamedMetaInfo& other) const
  {
    if (getNameHashCode() != other.getNameHashCode())
      return false;
    return strcmp(name, other.name) == 0;
  }
  /// defined in StringInline
  inline bool equalsName(IN(RString) other) const;
  inline void _calcNameHashCode() const;
};

/*
  Metainf with name and a namespace part
*/
foreign struct ACDK_CORE_PUBLIC NamedScopedMetaInfo
: public NamedMetaInfo
{
  /**
    FQ name of parent -> ns::name is FQ name
  */
  const char* ns;
  /**
    Parent MetaInfo containing this
  */
  mutable const NamedScopedMetaInfo* _scopeParent;
  /**
    list of sibling in the same scope
  */
  mutable const NamedScopedMetaInfo* _nextScopeSibling;

  RString getScopedName(const char* joiner) const;
  void copyFrom(const NamedScopedMetaInfo* source, bool deep);
  /**
    register this into parent.
    parent has to be NamedScopedParentMetaInfo (currently only UnitInfo)
  */
  void registerInParent() const;
  void unregisterFromParent() const;
  void dispose();

};

/*
  Metainf with name and a namespace part
*/
foreign struct ACDK_CORE_PUBLIC TypedMetaInfo
: public NamedScopedMetaInfo
{
  const ClazzInfo* type;

  void copyFrom(const TypedMetaInfo* source, bool deep);
  void dispose();
};


/*
  Metainf with name and a namespace part
*/
foreign struct ACDK_CORE_PUBLIC NamedScopedParentMetaInfo
: public TypedMetaInfo
{
  mutable const NamedScopedMetaInfo* _firstChild;
  /**
    find metainfo in childs 
    @param name must be normalized

  */
  const MetaInfo* findMetaInfo(IN(RString) name, int flags) const;
};




} // namespace dmi
} // namespace acdk
} // namespace lang


#endif //acdk_lang_dmi_MetaInfo_h
