// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "../util.h"
#include "../AbstractCollection.h"
#include "../AbstractList.h"
#include "../AbstractListIterator.h"
#include "../AbstractListListIterator.h"
#include "../AbstractListSubList.h"
#include "../AbstractMap.h"
#include "../AbstractSequentialList.h"
#include "../AbstractSet.h"
#include "../acdk_util_classes.h"
#include "../ArrayIterator.h"
#include "../ArrayList.h"
#include "../Arrays.h"
#include "../BasicMapEntry.h"
#include "../BitSet.h"
#include "../Bucket.h"
#include "../Calendar.h"
#include "../Collection.h"
#include "../Collections.h"
#include "../Comparator.h"
#include "../ConcurrentModificationException.h"
#include "../Date.h"
#include "../Dictionary.h"
#include "../DoubleIterator.h"
#include "../EmptyCollectionIterator.h"
#include "../Enumeration.h"
#include "../EventListener.h"
#include "../EventListenerProxy.h"
#include "../EventObject.h"
#include "../GregorianCalendar.h"
#include "../HashMap.h"
#include "../HashSet.h"
#include "../Hashtable.h"
#include "../IdentityHashMap.h"
#include "../Iterator.h"
#include "../LinkedList.h"
#include "../List.h"
#include "../ListIterator.h"
#include "../ListResourceBundle.h"
#include "../Locale.h"
#include "../Map.h"
#include "../MissingResourceException.h"
#include "../NoSuchElementException.h"
#include "../Properties.h"
#include "../PropertiesListener.h"
#include "../PropertyResourceBundle.h"
#include "../Random.h"
#include "../ResourceBundle.h"
#include "../Set.h"
#include "../SimpleCalendar.h"
#include "../SimpleListResourceBundle.h"
#include "../SimpleTimeZone.h"
#include "../SingleObjectIterator.h"
#include "../SortedMap.h"
#include "../SortedSet.h"
#include "../StringTokenizer.h"
#include "../SynchronizedCollections.h"
#include "../SysDate.h"
#include "../TAbstractCollection.h"
#include "../TAbstractList.h"
#include "../TAbstractListIterator.h"
#include "../TAbstractListListIterator.h"
#include "../TAbstractListSubList.h"
#include "../TAbstractMap.h"
#include "../TAbstractSet.h"
#include "../TArrayList.h"
#include "../TBasicMapEntry.h"
#include "../TBucket.h"
#include "../TCollection.h"
#include "../TComparator.h"
#include "../TDoubleIterator.h"
#include "../THashMap.h"
#include "../THashSet.h"
#include "../TimeZone.h"
#include "../TIterator.h"
#include "../TList.h"
#include "../TListIterator.h"
#include "../TMap.h"
#include "../TreeMap.h"
#include "../TreeSet.h"
#include "../TSet.h"
#include "../TSortedMap.h"
#include "../TTreeMap.h"
#include "../util.h"
#include "../util_all.h"
#include "../Vector.h"
#include "../WeakHashMap.h"
#include <acdk/lang/dmi/ClazzInfoInternals.h>

namespace acdk { 
namespace util { 


//static
::acdk::lang::RObject
WeakHashMapKey_create_array(int length)
{
  return new ObjectArrayImpl<RWeakHashMapKey>(length);
}

//static
::acdk::lang::RObject
WeakHashMapKey_create_array_array(int firstLength, int secondLength)
{
  return Nil;//not implemented yet
}
class WeakHashMapKey_MetainfoWrapper 
{

public:
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMapKey_method_GetClass__L_acdk_lang_RClass__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)WeakHashMapKey::GetClass();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMapKey_method_WeakHashMapKey_INLRObject__LRWeakHashMapKey__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)new WeakHashMapKey(::acdk::lang::dmi::castTo< RObject>(args[0], dc));
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMapKey_method_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)new WeakHashMapKey(::acdk::lang::dmi::castTo< RObject>(args[0], dc), ::acdk::lang::dmi::castTo< ::acdk::lang::ref::RReferenceQueue>(args[1], dc));
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMapKey_method_equals_INLRObject__Z_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    WeakHashMapKey* This = dmi_cast<WeakHashMapKey>(This_);
    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)
      ret = This->WeakHashMapKey::equals(::acdk::lang::dmi::castTo< RObject>(args[0], dc));
    else
      ret = This->equals(::acdk::lang::dmi::castTo< RObject>(args[0], dc));
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMapKey_method_hashCode__I_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    WeakHashMapKey* This = dmi_cast<WeakHashMapKey>(This_);
    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)
      ret = This->WeakHashMapKey::hashCode();
    else
      ret = This->hashCode();
    return methinf;
  }
};

::acdk::lang::dmi::ClazzFieldInfo* _WeakHashMapKey_fields[] = 
{
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMapKey_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMapKey_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMapKey_method_GetClass__L_acdk_lang_RClass_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiStatic | ::acdk::lang::dmi::MiMethodInfo,// class flags, like static, Constructor
  0, //AttributesRes
  "GetClass", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  "_4_GetClass", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMapKey_methods_GetClass__L_acdk_lang_RClass__args, // the arguments
  0, //arguments count
  WeakHashMapKey_methods_GetClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  WeakHashMapKey_MetainfoWrapper::WeakHashMapKey_method_GetClass__L_acdk_lang_RClass__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__arg_k = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn, 
  0, //AttributesRes
  "k", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  RObject::clazzInfo() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__args[] = 
{
  &WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__arg_k,
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMapKey_method_WeakHashMapKey_INLRObject__LRWeakHashMapKey_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "WeakHashMapKey", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  WeakHashMapKey::clazzInfo(), // return type
  "_0_WeakHashMapKey", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__args, // the arguments
  0, //arguments count
  WeakHashMapKey_methods_WeakHashMapKey_INLRObject__LRWeakHashMapKey__exceptions, // the declared exceptions
  WeakHashMapKey_MetainfoWrapper::WeakHashMapKey_method_WeakHashMapKey_INLRObject__LRWeakHashMapKey__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__arg_k = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn, 
  0, //AttributesRes
  "k", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  RObject::clazzInfo() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__arg_refqueue = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn, 
  0, //AttributesRes
  "refqueue", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::ref::RReferenceQueue::clazzInfo() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__args[] = 
{
  &WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__arg_k,
  &WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__arg_refqueue,
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMapKey_method_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "WeakHashMapKey", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  WeakHashMapKey::clazzInfo(), // return type
  "_1_WeakHashMapKey", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__args, // the arguments
  0, //arguments count
  WeakHashMapKey_methods_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__exceptions, // the declared exceptions
  WeakHashMapKey_MetainfoWrapper::WeakHashMapKey_method_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMapKey_methods_equals_INLRObject__Z_arg_other = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiIn, 
  0, //AttributesRes
  "other", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  RObject::clazzInfo() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMapKey_methods_equals_INLRObject__Z_args[] = 
{
  &WeakHashMapKey_methods_equals_INLRObject__Z_arg_other,
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMapKey_methods_equals_INLRObject__Z_exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMapKey_method_equals_INLRObject__Z = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "equals", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getBoolClazz(), // return type
  "_2_equals", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMapKey_methods_equals_INLRObject__Z_args, // the arguments
  0, //arguments count
  WeakHashMapKey_methods_equals_INLRObject__Z_exceptions, // the declared exceptions
  WeakHashMapKey_MetainfoWrapper::WeakHashMapKey_method_equals_INLRObject__Z_dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMapKey_methods_hashCode__I_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMapKey_methods_hashCode__I_exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMapKey_method_hashCode__I = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "hashCode", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  "_3_hashCode", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMapKey_methods_hashCode__I_args, // the arguments
  0, //arguments count
  WeakHashMapKey_methods_hashCode__I_exceptions, // the declared exceptions
  WeakHashMapKey_MetainfoWrapper::WeakHashMapKey_method_hashCode__I_dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodInfo* _WeakHashMapKey_methods[] = 
{
  &WeakHashMapKey_method_GetClass__L_acdk_lang_RClass_,
  &WeakHashMapKey_method_WeakHashMapKey_INLRObject__LRWeakHashMapKey_,
  &WeakHashMapKey_method_WeakHashMapKey_INLRObject_INL_acdk_lang_ref_RReferenceQueue__LRWeakHashMapKey_,
  &WeakHashMapKey_method_equals_INLRObject__Z,
  &WeakHashMapKey_method_hashCode__I,
  0
};


struct WeakHashMapKey_MetaInfoInitializer
{  WeakHashMapKey_MetaInfoInitializer()
  {
    ::acdk::lang::dmi::ClazzInfo* ci = WeakHashMapKey::clazzInfo();
    ci->fields =  _WeakHashMapKey_fields;
    ci->methods =  _WeakHashMapKey_methods;
    ci->_scopeParent = acdk_util_unitInfo.getMetaInfo();
    ci->registerClazzInfo(); // make sure clazzinfo is registered
    ci->_resolveMemberParents();
    ci->flags |= ::acdk::lang::dmi::MiResolved;
  }
};
WeakHashMapKey_MetaInfoInitializer WeakHashMapKey_MetaInfoInitializer_staticinstance__;


} // namespace acdk
} // namespace util


namespace acdk { 
namespace util { 


//static
::acdk::lang::RObject
WeakHashMap_create_array(int length)
{
  return new ObjectArrayImpl<RWeakHashMap>(length);
}

//static
::acdk::lang::RObject
WeakHashMap_create_array_array(int firstLength, int secondLength)
{
  return Nil;//not implemented yet
}
class WeakHashMap_MetainfoWrapper 
{

public:
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_GetClass__L_acdk_lang_RClass__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)WeakHashMap::GetClass();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_WeakHashMap__LRWeakHashMap__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)new WeakHashMap();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_WeakHashMap_I_LRWeakHashMap__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)new WeakHashMap(::acdk::lang::dmi::castTo< int>(args[0], dc));
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_WeakHashMap_IF_LRWeakHashMap__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)new WeakHashMap(::acdk::lang::dmi::castTo< int>(args[0], dc), ::acdk::lang::dmi::castTo< float>(args[1], dc));
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method__clearQueue__V_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    WeakHashMap* This = dmi_cast<WeakHashMap>(This_);
    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)
        This->WeakHashMap::_clearQueue();
    else
        This->_clearQueue();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_capacity__I_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    WeakHashMap* This = dmi_cast<WeakHashMap>(This_);
    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)
      ret = This->WeakHashMap::capacity();
    else
      ret = This->capacity();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_create_instance__LRObject__dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    ret = (::acdk::lang::RObject)WeakHashMap::create_instance();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzMethodInfo* 
  WeakHashMap_method_loadFactor__F_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
  {
    WeakHashMap* This = dmi_cast<WeakHashMap>(This_);
    if (flags & ::acdk::lang::dmi::MiIvNoWeakBind)
      ret = This->WeakHashMap::loadFactor();
    else
      ret = This->loadFactor();
    return methinf;
  }
  static const ::acdk::lang::dmi::ClazzFieldInfo*
  WeakHashMap_fields__hashMap_accessor(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& var, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf)
  {
    WeakHashMap* This = dmi_cast<WeakHashMap>(This_);
    if (flags & ::acdk::lang::dmi::MiReadOnly)
     var = ::acdk::lang::getScriptVarOf(This->_hashMap, flags);
    else
      This->_hashMap = ::acdk::lang::dmi::castTo< RHashMap>(var, dc);
    return fieldinf;
  }
  static const ::acdk::lang::dmi::ClazzFieldInfo*
  WeakHashMap_fields__refQueue_accessor(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& var, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzFieldInfo* fieldinf)
  {
    WeakHashMap* This = dmi_cast<WeakHashMap>(This_);
    if (flags & ::acdk::lang::dmi::MiReadOnly)
     var = ::acdk::lang::getScriptVarOf(This->_refQueue, flags);
    else
      This->_refQueue = ::acdk::lang::dmi::castTo< ::acdk::lang::ref::RReferenceQueue>(var, dc);
    return fieldinf;
  }
};

::acdk::lang::dmi::ClazzFieldInfo WeakHashMap_fields__hashMap = 
{
  ::acdk::lang::dmi::MiPrivate | ::acdk::lang::dmi::MiFieldInfo,
  0, //AttributesRes
  "_hashMap", // name
   -1, // hashCode
   "", // ns
  0, // _scopeParent
  0, // _nextSibling
  RHashMap::clazzInfo(),
  WeakHashMap_MetainfoWrapper::WeakHashMap_fields__hashMap_accessor, // read/write access to this fields
  (void*)0 // address of field
};

::acdk::lang::dmi::ClazzFieldInfo WeakHashMap_fields__refQueue = 
{
  ::acdk::lang::dmi::MiPrivate | ::acdk::lang::dmi::MiFieldInfo,
  0, //AttributesRes
  "_refQueue", // name
   -1, // hashCode
   "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::ref::RReferenceQueue::clazzInfo(),
  WeakHashMap_MetainfoWrapper::WeakHashMap_fields__refQueue_accessor, // read/write access to this fields
  (void*)0 // address of field
};

::acdk::lang::dmi::ClazzFieldInfo* _WeakHashMap_fields[] = 
{
  &WeakHashMap_fields__hashMap,
  &WeakHashMap_fields__refQueue,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_GetClass__L_acdk_lang_RClass_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiStatic | ::acdk::lang::dmi::MiMethodInfo,// class flags, like static, Constructor
  0, //AttributesRes
  "GetClass", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  "_7_GetClass", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_GetClass__L_acdk_lang_RClass__args, // the arguments
  0, //arguments count
  WeakHashMap_methods_GetClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_GetClass__L_acdk_lang_RClass__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_WeakHashMap__LRWeakHashMap__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_WeakHashMap__LRWeakHashMap__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_WeakHashMap__LRWeakHashMap_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor,// class flags, like static, Constructor
  0, //AttributesRes
  "WeakHashMap", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  WeakHashMap::clazzInfo(), // return type
  "_2_WeakHashMap", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_WeakHashMap__LRWeakHashMap__args, // the arguments
  0, //arguments count
  WeakHashMap_methods_WeakHashMap__LRWeakHashMap__exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_WeakHashMap__LRWeakHashMap__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__arg_initialCapacity = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiHasDefaultInit, 
  0, //AttributesRes
  "initialCapacity", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__args[] = 
{
  &WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__arg_initialCapacity,
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_WeakHashMap_I_LRWeakHashMap_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor,// class flags, like static, Constructor
  0, //AttributesRes
  "WeakHashMap", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  WeakHashMap::clazzInfo(), // return type
  "_3_WeakHashMap", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__args, // the arguments
  0, //arguments count
  WeakHashMap_methods_WeakHashMap_I_LRWeakHashMap__exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_WeakHashMap_I_LRWeakHashMap__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__arg_initialCapacity = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiHasDefaultInit, 
  0, //AttributesRes
  "initialCapacity", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__arg_initialLoadFactor = 
{
  ::acdk::lang::dmi::MiMethodArgInfo | ::acdk::lang::dmi::MiAiHasDefaultInit, 
  0, //AttributesRes
  "initialLoadFactor", // name of arg
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getFloatClazz() // type or arg
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__args[] = 
{
  &WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__arg_initialCapacity,
  &WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__arg_initialLoadFactor,
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_WeakHashMap_IF_LRWeakHashMap_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiConstructor | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "WeakHashMap", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  WeakHashMap::clazzInfo(), // return type
  "_1_WeakHashMap", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__args, // the arguments
  0, //arguments count
  WeakHashMap_methods_WeakHashMap_IF_LRWeakHashMap__exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_WeakHashMap_IF_LRWeakHashMap__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods__clearQueue__V_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods__clearQueue__V_exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method__clearQueue__V = 
{
  ::acdk::lang::dmi::MiPrivate | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "_clearQueue", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getVoidClazz(), // return type
  "_6__clearQueue", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods__clearQueue__V_args, // the arguments
  0, //arguments count
  WeakHashMap_methods__clearQueue__V_exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method__clearQueue__V_dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_capacity__I_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_capacity__I_exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_capacity__I = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "capacity", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  "_4_capacity", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_capacity__I_args, // the arguments
  0, //arguments count
  WeakHashMap_methods_capacity__I_exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_capacity__I_dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_create_instance__LRObject__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_create_instance__LRObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_create_instance__LRObject_ = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiStatic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "create_instance", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  RObject::clazzInfo(), // return type
  "_0_create_instance", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_create_instance__LRObject__args, // the arguments
  0, //arguments count
  WeakHashMap_methods_create_instance__LRObject__exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_create_instance__LRObject__dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodArgInfo* WeakHashMap_methods_loadFactor__F_args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* WeakHashMap_methods_loadFactor__F_exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo WeakHashMap_method_loadFactor__F = 
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiMethodInfo | ::acdk::lang::dmi::MiMiOrgPoly,// class flags, like static, Constructor
  0, //AttributesRes
  "loadFactor", // name of method
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::dmi::ClazzInfo::getFloatClazz(), // return type
  "_5_loadFactor", // alternative name of method
  -1, // altlabelHashCode
  WeakHashMap_methods_loadFactor__F_args, // the arguments
  0, //arguments count
  WeakHashMap_methods_loadFactor__F_exceptions, // the declared exceptions
  WeakHashMap_MetainfoWrapper::WeakHashMap_method_loadFactor__F_dispatch, // invoke this method
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, // dispatch throwing exceptions
  0 // cached method signature hash
};

::acdk::lang::dmi::ClazzMethodInfo* _WeakHashMap_methods[] = 
{
  &WeakHashMap_method_GetClass__L_acdk_lang_RClass_,
  &WeakHashMap_method_WeakHashMap__LRWeakHashMap_,
  &WeakHashMap_method_WeakHashMap_I_LRWeakHashMap_,
  &WeakHashMap_method_WeakHashMap_IF_LRWeakHashMap_,
  &WeakHashMap_method__clearQueue__V,
  &WeakHashMap_method_capacity__I,
  &WeakHashMap_method_create_instance__LRObject_,
  &WeakHashMap_method_loadFactor__F,
  0
};


struct WeakHashMap_MetaInfoInitializer
{  WeakHashMap_MetaInfoInitializer()
  {
    ::acdk::lang::dmi::ClazzInfo* ci = WeakHashMap::clazzInfo();
    ci->fields =  _WeakHashMap_fields;
    ci->methods =  _WeakHashMap_methods;
    ci->_scopeParent = acdk_util_unitInfo.getMetaInfo();
    ci->registerClazzInfo(); // make sure clazzinfo is registered
    ci->_resolveMemberParents();
    ci->flags |= ::acdk::lang::dmi::MiResolved;
  }
};
WeakHashMap_MetaInfoInitializer WeakHashMap_MetaInfoInitializer_staticinstance__;


} // namespace acdk
} // namespace util

