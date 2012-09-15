#include <acdk.h>
#include <acdk/java/serialization/ClassDescription.h>



::acdk::java::serialization::MemberTypeMapping a2jser_JavaSerSample_field__x = 
{
  "_x", // acdk_name
  "int", // acdk_type
  "_x", // java_name
  "I" // java_type
};

::acdk::java::serialization::MemberTypeMapping a2jser_JavaSerSample_field__y = 
{
  "_y", // acdk_name
  "int", // acdk_type
  "_y", // java_name
  "I" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_JavaSerSample_fields[] = 
{
  &a2jser_JavaSerSample_field__x,
  &a2jser_JavaSerSample_field__y,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_JavaSerSample = 
{
  "JavaSerSample", // acdk_name
  "JavaSerSample", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x8ec745aeeeefa3b0), // UID
  JLONG_CONSTANT(0xd02f4135bf6de609), //[] UID
  a2jser_JavaSerSample_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_JavaSerSample(&a2jser_JavaSerSample);


