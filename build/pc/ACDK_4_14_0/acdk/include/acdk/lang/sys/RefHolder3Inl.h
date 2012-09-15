
#ifndef acdk_lang_sys_RefHolder3Inl_h
#define acdk_lang_sys_RefHolder3Inl_h

/**
  use DMI Cast from a object to another interface
  Maybe this returns a total different object instance, in
  case of composite objects
*/
      
template <typename T>
inline
T* dmi_cast(::acdk::lang::ObjectBase* objbase)
{
  T* t = dynamic_cast<T*>(objbase);
  if (t != 0)
    return t;
  t = dynamic_cast<T*>(objbase->_cast(T::clazzInfo()));
  if (t != 0)
    return t;
  badCast(T::clazzInfo(), objbase);
  return 0;
}

/**
  return the casted interface or 0 if cannot be casted
  @see dmi_cast
*/
template <typename T>
inline
T* try_dmi_cast(::acdk::lang::ObjectBase* objbase)
{
  T* t = dynamic_cast<T*>(objbase);
  if (t != 0)
    return t;
  t = dynamic_cast<T*>(objbase->_cast(T::clazzInfo()));
  return t;
}

/**
  return the casted interface or 0 if cannot be casted
  @see dmi_cast
*/
template <typename T>
inline
T* try_dmi_cast_object(::acdk::lang::ObjectBase* objbase)
{
  T* t = dynamic_cast<T*>(objbase);
  if (t != 0)
    return t;
  t = dynamic_cast<T*>(objbase->_cast(T::clazzInfo()));
  if (t != 0)
    return t->_getObjectPtr();
  return t;
}

/**
  try to cast from object to interface, with explict interface pointer.
  This is used in scripting language, whereas object pointer may not 
  equal interface pointers
  @see dmi_cast
*/
template <typename T>
inline
void dmi_cast2(::acdk::lang::ObjectBase* from, T*& toI, ::acdk::lang::Object*& toO)
{
  toI = dmi_cast<T>(from);
  if ((void*)toI != (void*)from)
    toO = toI->_getObjectPtr();
}

template <class T>
inline
void
RefHolder<T>::_init(::acdk::lang::Object* obj)
{
  _impl = obj;
  ACDK_REFCOUNTING_CODE( _impl->addRef(); )
}

template <class T>
inline
void
RefHolder<T>::_init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
{
  if (obj == 0)
    return;
  dmi_cast2<T>(obj, _iptr, obj);
  _init(obj);
}


template <class T>
inline
void
RefHolder<T>::_release()
{
  if (_impl == 0)
    return;
  ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
  _iptr = 0;
  ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
}

template <class T>
inline
void
RefHolder<T>::_assign(T* iface, ::acdk::lang::Object* obj)
{
  if (_impl == obj)
    return;
  if (obj != 0) {
    ACDK_REFCOUNTING_CODE( obj->addRef(); )
  }
  if (_impl != 0) {
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
  }
  _impl = obj;
  _iptr = iface;
}

template <class T>
inline
void
RefHolder<T>::_assing2(T* iface, ::acdk::lang::Object* obj)
{
  ACDK_REFCOUNTING_CODE( obj->addRef(); )
    if (_impl != 0) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
        _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    } 
    _impl = obj;
    _iptr = iface;
}

template <class T>
inline
void
RefHolder<T>::_assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
{
  if (obj == _impl)
    return;
  if (obj != 0) 
  {
    T* tptr;
    dmi_cast2<T>(iface, tptr, obj);
    _assing2(tptr, obj);
  } else
    _release();
}

template <class T>
inline
void
RefHolder<T>::_assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
{
  
  if (obj == _impl)
    return;
  if (iface != 0) 
  {
    T* tptr;
    dmi_cast2<T>(obj, tptr, obj);
    _assing2(tptr, obj);
  } else
    _release();
  
}

template <class T>
inline
RefHolder<T>::RefHolder(::acdk::lang::InterfaceBase* ibase)
: _impl(0)
, _iptr(0)
{
  if (ibase == 0)
    return;
  ::acdk::lang::Object* optr = ibase->_getObjectPtr();
  _init(optr, optr);
}


template <class T>
inline
RefHolder<T>::RefHolder(const RefHolder<T>& o)
: _impl(o.impl())
, _iptr(o.iptr())
{
  ACDK_REFCOUNTING_CODE(
    if (_impl != 0)
      _impl->addRef();
    )
}

template <class T>
inline
RefHolder<T>::~RefHolder() 
{
  if(_impl != 0) {
    _impl->releaseRef();
  }
  _impl = 0;
  _iptr = 0;
}


template <class T>
inline
RefHolder<T>::RefHolder(const ::acdk::lang::dmi::ScriptVar& sv)
: _impl(0)
, _iptr(0)
{
  *this = (RefHolder<T>) sv.getObjectVar();      
}

template <class T>
inline
T* 
RefHolder<T>::getIPtr() const 
{
  ACDK_ASSERT_NULL_PTR(_iptr)
    return _iptr;
}

template <class T>
inline
::acdk::lang::Object* 
RefHolder<T>::getImpl() 
{
  ACDK_ASSERT_NULL_PTR(_impl);
  return _impl;
}

template <class T>
inline
T* 
RefHolder<T>::operator->() const
{ 
  ACDK_ASSERT_NULL_PTR(_iptr);
  return const_cast<T*>(_iptr);
}

template <class I>
inline
void 
InterfaceHolder<I>::_init(::acdk::lang::Object* obj)
{
  InterfaceHolder<I>::_impl = obj;
  ACDK_REFCOUNTING_CODE( InterfaceHolder<I>::_impl->addRef(); )
}

template <class I>
inline
void 
InterfaceHolder<I>::_init(I* iface, ::acdk::lang::Object* obj)
{
  if (iface == 0)
    return;
  InterfaceHolder<I>::_iptr = iface;
  _init(obj);
}

template <class I>
inline
void 
InterfaceHolder<I>::_init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj) 
{
  if (iface == 0)
    return;
  dmi_cast2<I>(obj, InterfaceHolder<I>::_iptr, obj);
  _init(obj);
}


template <class I>
inline
void 
InterfaceHolder<I>::_release()
{
  if (InterfaceHolder<I>::_impl == 0)
    return;
  ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = InterfaceHolder<I>::_impl; )
    InterfaceHolder<I>::_impl = 0;
  InterfaceHolder<I>::_iptr = 0;
  ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
}

template <class I>
inline
void 
InterfaceHolder<I>::_assign(I* iface, ::acdk::lang::Object* obj)
{
  if (InterfaceHolder<I>::_impl == obj)
    return;
  // first increment, in case of cyclic references
  if (obj != 0) { 
    ACDK_REFCOUNTING_CODE( obj->addRef(); )
  }
  if (InterfaceHolder<I>::_impl != 0) 
  {
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = InterfaceHolder<I>::_impl; )
      InterfaceHolder<I>::_impl = 0;
    InterfaceHolder<I>::_iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
  }
  InterfaceHolder<I>::_impl = obj;
  InterfaceHolder<I>::_iptr = iface;
}

template <class I>
inline
void 
InterfaceHolder<I>::_assing2(I* iface, ::acdk::lang::Object* obj)
{
  ACDK_REFCOUNTING_CODE( obj->addRef(); )
  if (InterfaceHolder<I>::_impl != 0) 
  {
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = InterfaceHolder<I>::_impl; )
    InterfaceHolder<I>::_impl = 0;
    InterfaceHolder<I>::_iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
  } 
  InterfaceHolder<I>::_impl = obj;
  InterfaceHolder<I>::_iptr = iface;
}

template <class I>
inline
void 
InterfaceHolder<I>::_assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
{
  if (obj == InterfaceHolder<I>::_impl)
    return;
  if (obj != 0) 
  {
    I* tptr;
    dmi_cast2<I>(obj, tptr, obj);
    _assing2(tptr, obj);
  } else
    _release();
}

template <class I>
inline
void 
InterfaceHolder<I>::_assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
{
  
  if (obj == InterfaceHolder<I>::_impl)
    return;
  if (iface != 0) 
  {
    I* tptr;
    dmi_cast2<I>(obj, tptr, obj);
    _assing2(tptr, obj);
  } else
    _release();
}

template <class I>
inline
InterfaceHolder<I>::InterfaceHolder(I* iface)
: RefHolder<I>(Nil)
{
  _init(iface, iface == 0 ? 0 : iface->_getObjectPtr());
}


template <class I>
inline 
InterfaceHolder<I>::InterfaceHolder(::acdk::lang::InterfaceBase* ibase)
: RefHolder<I>(Nil)
{
  if (ibase == 0)
    return;
  ::acdk::lang::Object* optr = ibase->_getObjectPtr();
  _init(optr, optr);
}


template <class I>
inline 
InterfaceHolder<I>& 
InterfaceHolder<I>::operator=(I* iptr)
{
  if (iptr == 0)
    _release();
  else
    _assign(iptr, iptr->_getObjectPtr());
  return *this;
}

template <class I>
inline 
::acdk::lang::Object* 
InterfaceHolder<I>::getImpl() const 
{
  ACDK_ASSERT_NULL_PTR(InterfaceHolder<I>::_iptr)
  return const_cast< ::acdk::lang::Object*>(InterfaceHolder<I>::_impl);
}

template <class I>
inline 
I* 
InterfaceHolder<I>::operator->() const
{ 
  ACDK_ASSERT_NULL_PTR(InterfaceHolder<I>::_iptr)
  return const_cast<I*>(InterfaceHolder<I>::_iptr); 
}

template <class I>
inline 
::acdk::lang::Object* 
InterfaceHolder<I>::impl() const throw() { return const_cast< ::acdk::lang::Object*>(InterfaceHolder<I>::_impl); }

template <class I>
inline 
bool 
InterfaceHolder<I>::operator==(NilRef nil) const  throw() { return InterfaceHolder<I>::_impl == 0; }

template <class I>
inline 
bool 
InterfaceHolder<I>::operator!=(NilRef nil) const  throw() { return InterfaceHolder<I>::_impl != 0; }

#if defined(ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE)
template <class I> template <class OT>
inline
bool 
InterfaceHolder<I>::operator==(const RefHolder<OT>& other) const  throw() { return InterfaceHolder<I>::_impl == other.impl(); }

template <class I> template <class OT>
inline
bool 
InterfaceHolder<I>::operator!=(const RefHolder<OT>& other) const  throw() { return InterfaceHolder<I>::_impl != other.impl(); }
#endif //defined(ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE)

template<class T, class S>
inline 
T* 
ThrowableHolder<T, S>::getIPtr() const 
{
  ACDK_ASSERT_NULL_PTR((ThrowableHolder<T, S>::_iptr))
    T* ret;
  REFH_TRYDOWNCAST_OBJ(ret, T, (ThrowableHolder<T, S>::_iptr));
  return ret;
}


template<class T, class S>
inline 
::acdk::lang::Object*
ThrowableHolder<T, S>::getImpl() 
{
  ACDK_ASSERT_NULL_PTR((ThrowableHolder<T, S>::_impl))
  return ThrowableHolder<T, S>::_impl;
}

template<class T, class S>
inline 
T*
ThrowableHolder<T, S>::operator->() const
{ 
  ACDK_ASSERT_NULL_PTR((ThrowableHolder<T, S>::_iptr))
  return iptr();
}

#endif //acdk_lang_sys_RefHolder3Inl_h
