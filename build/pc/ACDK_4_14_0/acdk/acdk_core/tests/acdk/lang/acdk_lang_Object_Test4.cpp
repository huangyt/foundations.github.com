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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Object_Test4.cpp,v 1.6 2005/02/05 10:45:08 kommer Exp $

#ifndef ACDK_GENDOC
template <class T>
class RefHolder4
{
public:
   T* _impl;
   bool _isStack;
   

   RefHolder4(T* optr)
   :  _impl(optr)
   ,  _isStack(false)
   {
      if (_impl == 0)
         return;
      _isStack = _impl->isStack();
      addRef();
   }
   RefHolder4(T& optr)
   :  _impl(&optr)
   ,  _isStack(true)
   {
   }
   RefHolder4(const RefHolder4<T>& other)
   :  _impl(other._impl)
   ,  _isStack(other._isStack)
   {
      addRef();
   }
   ~RefHolder4()
   {
      releaseRef();
   }
   inline void addRef()
   {
      if (_impl == 0 || _isStack == true)
         return;
      _impl->addRef(); 
   }
   inline void releaseRef()
   {
      if (_impl == 0 || _isStack == true)
         return;
      _impl->releaseRef(); 
   }
};

class ObjectBase4 
{
   int _refCount;
   bool _isStack;
public:
   ObjectBase4(bool isstack = false)
   :  _refCount(0)
   ,  _isStack(isstack)
   {
   }
   virtual ~ObjectBase4() { }
   virtual void finalize()
   {
   }
   void addRef()
   {
      ++_refCount;
   }
   void releaseRef()
   {
      if (--_refCount == 0) {
         finalize();
         delete this;
      }
   }
   bool isStack() { return _isStack; }
};

class Object4
: public ObjectBase4
{
public:
   Object4(bool isstack = false) : ObjectBase4(isstack) { } 
   void doIt();
};

typedef RefHolder4<Object4> RObject;

RObject foo4(RObject obj)
{
   return new Object4(false);
}

void ObjectTest4()
{
   RObject o1 = new Object4();
   Object4 o2(true);
   foo4(o1);
   foo4(o2);
}
#endif //ACDK_GENDOC
