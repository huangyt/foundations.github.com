// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "DClazzInfo.h"
#include "SymbolTable.h"
//#include "OpCode.h"
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/ClazzAttributesRes.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/BinaryDataWriter.h>
#include <acdk/io/BinaryDataReader.h>

#include <acdk/io/PrintWriter.h>

namespace acdk {
namespace aci {

using namespace acdk::lang::dmi;


const int AddressMarker = 0xFFFEFFFE;
int 
findAddressOffset(IN(RbyteArray) ba, const void* addr)
{
  byte* begin = ba->begin();
  byte* it = begin;
  byte* end = ba->end();
  for (; it < end; ++it)
  {
    if (*((int*)it) == AddressMarker)
    {
      it += 4;
      if (*((int*)it) == int(addr))
        return (it - begin) + sizeof(int);
    }
  }
  return 0;
}

void*
getAddressOffset(IN(RbyteArray) ba, const void* addr)
{
  if (addr == 0)
    return 0;
  int of = findAddressOffset(ba, addr);
  if (of == 0)
    return (void*)-1;
  return ba->data() + of;
}

template <class T>
void resolveToPersist(IN(RbyteArray) ba, T*& t)
{
  t = (T*)findAddressOffset(ba, t);
}

void writeCString(acdk::io::BinaryDataWriter& out, const char* cstr)
{
  while (*cstr != 0)
  {
    out.write(*cstr);
    ++cstr;
  }
  out.write(0);
}

void writeSuperInfo(acdk::io::BinaryDataWriter& out, acdk::io::MemWriter& bout, const ClazzSuperInfo* si)
{
  /*
  int flags;
  void* attributeRes;
  const ClazzInfo* type;
  */
  out.writeInt(AddressMarker);
  out.writeInt((int)si);
  out.writeInt(si->flags);
  out.writeInt(0); // attributeRes
  out.writeInt(0); 
}

void writeClazzInfo(acdk::io::BinaryDataWriter& out, acdk::io::MemWriter& bout, const ClazzInfo* ci)
{
  out.writeInt(AddressMarker);
  out.writeInt((int)ci);
  out.writeInt(ci->flags);
  out.writeInt(int(0)); // attributeRes
  out.writeInt(int(ci->name));
  out.writeInt(int(ci->ns));
  out.writeInt(int(ci->interfaces));
  out.writeInt(0);
  out.writeInt(int(ci->fields));
  out.writeInt(0);
  out.writeInt(int(ci->methods));
  out.writeInt(0);
  out.writeInt(int(0)); // creator
  out.writeInt(int(0)); // arraycreator
  out.writeInt(int(0)); // arrayarraycreator
  out.writeInt(int(0)); // thisClass
  out.writeLong(0); // serialized
  out.writeInt(int(0)); // static_dispatch
  out.writeInt(0); // static_dispatch
  out.writeInt(0); // userInfo
  out.writeInt(0); // unitInfo
  out.writeInt(0); // _next
  out.writeInt(0); // _unitNext

  out.writeInt(AddressMarker);
  out.writeInt(int(ci->name));
  writeCString(out, ci->name);

  out.writeInt(AddressMarker);
  out.writeInt(int(ci->ns));
  writeCString(out, ci->ns);
 
  RbyteArray ba = bout.getBuffer();
  byte* data = ba->data();
  ClazzInfo* tclazz = (ClazzInfo*)(data + findAddressOffset(ba, ci));
  resolveToPersist(ba, tclazz->name);
  resolveToPersist(ba, tclazz->ns);

  if (ci->interfaces != 0)
  {
    int i;
    for (i = 0; ci->interfaces[i] != 0; ++i)
    {
      writeSuperInfo(out, bout, ci->interfaces[i]);
    }
  }
  //tclazz->name = (char*)findAddressOffset(ba, tclazz->name);
  //tclazz->ns = (char*)findAddressOffset(ba, tclazz->ns);
  /*
    ClazzInfo.name
    ClazzInfo.ns
    ClazzInfo.flags 
    ClazzInfo.attributeResPtr 
    ClazzInfo.namePtr 
    ClazzInfo.nsPtr
    ClazzInfo.interfacesPtr
    ClazzInfo.interfacesCount
    ClazzInfo.fieldsPtr
    ClazzInfo.fieldsCount
    ClazzInfo.methodsPtr
    ClazzInfo.methodsCount
    ClazzInfo.creator = 0
    ClazzInfo.array_creator = 0
    ClazzInfo.array_array_creator = 0
    ClazzInfo.thisClass = 0
    ClazzInfo.serialVersionUID = 0
    ClazzInfo.static_dispatch = 0
    ClazzInfo.collectableFields = 0
    ClazzInfo.userInfo = 0
    ClazzInfo.unitInfo = 0
    ClazzInfo._next = 0
    ClazzInfo._unitNext = 0;

    ClazzInfo->interfaces[0].Ptr
    ClazzInfo->interfaces[n].Ptr = 0
    
    ClazzSuperInfo.flags
    ClazzSuperInfo.attributeResPtr
    ClazzSuperInfo.typeNamePtr // will replaced by resolved type ptr.



  */
}

template <class T>
void resolveToMem(IN(RbyteArray) ba, T*& t)
{
  int ofs = int(t);
  if (ofs == 0)
    t  = 0;
  else
    t = (T*)ba->begin() + ofs;
}

ClazzInfo*
readClazzInfo(acdk::io::BinaryDataReader& in, acdk::io::MemReader& min)
{
  in.readInt();
  in.readInt();
  ClazzInfo* ci = (ClazzInfo*)min.cit();
  RbyteArray ba = min.getBuffer();
  resolveToMem(ba, ci->name);
  resolveToMem(ba, ci->ns);
  return ci;
}

void 
DClazzInfo::writeObject(IN(acdk::io::RObjectWriter) oout)
{

  acdk::io::MemWriter bout;
  acdk::io::BinaryDataWriter out(&bout, acdk::lang::LittleEndian); // ### TODO platform depending
  writeClazzInfo(out, bout, _clazzInfo);

  acdk::io::MemReader bin(bout.getBuffer());
  acdk::io::BinaryDataReader in(&bin, acdk::lang::LittleEndian); // ### TODO platform depending
  ClazzInfo* nci = readClazzInfo(in, bin);
  
}

void 
DClazzInfo::readObject(IN(acdk::io::RObjectReader) in)
{

}

} // aci
} // acdk


