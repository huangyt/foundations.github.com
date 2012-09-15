// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 

#if 0
#ifndef Objects_h
#define Objects_h

class Object;


#define implements virtual public
#define extends public

class Object;
class InterfaceBase
{
public:
  virtual Object* getObject() = 0;
};

class ObjectBase
: implements InterfaceBase
{
protected:
  ObjectBase() {}
public:
  virtual ~ObjectBase() { }
  
};

class Object
: extends ObjectBase
{
public:
  virtual ~Object() { }
  Object* getObject() { return this; }
};




typedef ORef<Object> RObject;

class Comparable;
typedef IRef<Comparable> RComparable;

class Comparable
: implements InterfaceBase
{
public:
  virtual int compare(RComparable other) = 0;
};

class Writable
: implements InterfaceBase
{
public:
  virtual void write(int c) = 0;
};

typedef IRef<Writable> RWritable;


class StringWritable
: implements Writable
{
public:
  virtual void write(const char* cptr) = 0;
};

typedef IRef<StringWritable> RStringWritable;

class Number
: public Object
, implements Comparable
, implements StringWritable
{
public:
  virtual int compare(RComparable other)
  {
    return 0;
  }
  virtual void write(int c) { }
  virtual void write(const char* cptr) { }
};

typedef ORef<Number> RNumber;

#endif //Objects_h

#endif
