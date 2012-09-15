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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Object_Test3.cpp,v 1.8 2005/02/05 10:45:08 kommer Exp $


#ifndef ACDK_GENDOC
#if 0 // only scribble

template <class T> class Foo;

// where possible T's are:

class A { public: virtual ~A() { } }; 
class B : public A { }; class C : public B { };

// the simplified implementation of Foo<T>
template <class T>
class Foo
{
public:
   T* _ptr;
	/**
		Specialization
	*/
	// generic case 
	// works with all combination of T and OT
	// in case this is Foo<B>, called by Foo<B>(Foo<A>), Foo<B>(Foo<C>)
	template <class OT>	
	explicit Foo(Foo<OT>& other)	{	_ptr = dynamic_cast<T*>(other._ptr);  }
	
	// specialization with T
	// other and this must be the same type 
	// in case this is Foo<B>, called by Foo<B>(Foo<B>)
	Foo(Foo<T>& other)	{	_ptr = other._ptr;  }
	
	
	/**
		Overloading
	*/
	// works with all type of A (including B*, C*
	// in case this is Foo<B>, called by Foo<B>(A*)
	explicit Foo(A* t) 	{	_ptr = dynamic_cast<T*>(t); /* handle errors */ }
	
	// works with all type of T, including derived
	// in case this is Foo<B>, called by Foo<B>(B*) and Foo<B>(B*)
	Foo(T* t) { _ptr = t; }
	
};

void bar()
{
	Foo<B> b = new B();
	Foo<C> c = new C();
	Foo<B> b1 = b; // Foo(Foo<T>& other) works fine
	//Foo<B> b2 = c; // does't compile, because Foo(Foo<OT>& other) is explicit
                    // although b1._ptr is assignable to c._ptr
						   // I look for solution, that this should work
                    // for all OT which are same or derived from T 

	Foo<B> b3 = (Foo<B>)c; // work, but dynamic_cast will be called (performance issue)
								  					
	//Foo<C> c1 = b; // does not compile and should not compile, because
                  // you cannot assign C* = B*
	Foo<C> c2 = (Foo<C>)b; // Force conversion. c2._ptr == 0
                           // This behavoir is correct
	
}

#endif // 0
#endif //ACDK_GENDOC
