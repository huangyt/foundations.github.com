/*
 *  MICO --- an Open Source CORBA implementation
 *  Copyright (c) 1997-2001 by The Mico Team
 *
 *  This file was automatically generated. DO NOT EDIT!
 */

#include <CORBA.h>
#include <mico/throw.h>

#ifndef __IDLMAPPINGTEST_H__
#define __IDLMAPPINGTEST_H__




namespace tests {

namespace acdkx {

namespace orb {


class AdressBook;
typedef AdressBook *AdressBook_ptr;
typedef AdressBook_ptr AdressBookRef;
typedef ObjVar< AdressBook > AdressBook_var;
typedef ObjOut< AdressBook > AdressBook_out;

}
}
}






namespace tests {

namespace acdkx {

namespace orb {


struct AdressInfo;
typedef TVarVar< AdressInfo > AdressInfo_var;
typedef TVarOut< AdressInfo > AdressInfo_out;


struct AdressInfo {
  #ifdef HAVE_TYPEDEF_OVERLOAD
  typedef AdressInfo_var _var_type;
  #endif
  #ifdef HAVE_EXPLICIT_STRUCT_OPS
  AdressInfo();
  ~AdressInfo();
  AdressInfo( const AdressInfo& s );
  AdressInfo& operator=( const AdressInfo& s );
  #endif //HAVE_EXPLICIT_STRUCT_OPS

  CORBA::String_var name;
  CORBA::String_var street;
  CORBA::Long streetnumber;
};

typedef SequenceTmpl< AdressInfo,MICO_TID_DEF> AdressInfoSeq;
#ifdef _WINDOWS
static AdressInfoSeq _dummy_AdressInfoSeq;
#endif
typedef TSeqVar< SequenceTmpl< AdressInfo,MICO_TID_DEF> > AdressInfoSeq_var;
typedef TSeqOut< SequenceTmpl< AdressInfo,MICO_TID_DEF> > AdressInfoSeq_out;


/*
 * Base class and common definitions for interface AdressBook
 */

class AdressBook : 
  virtual public CORBA::Object
{
  public:
    virtual ~AdressBook();

    #ifdef HAVE_TYPEDEF_OVERLOAD
    typedef AdressBook_ptr _ptr_type;
    typedef AdressBook_var _var_type;
    #endif

    static AdressBook_ptr _narrow( CORBA::Object_ptr obj );
    static AdressBook_ptr _narrow( CORBA::AbstractBase_ptr obj );
    static AdressBook_ptr _duplicate( AdressBook_ptr _obj )
    {
      CORBA::Object::_duplicate (_obj);
      return _obj;
    }

    static AdressBook_ptr _nil()
    {
      return 0;
    }

    virtual void *_narrow_helper( const char *repoid );

    typedef SequenceTmpl< CORBA::Long,MICO_TID_DEF> longseq;
    #ifdef _WINDOWS
    static longseq _dummy_longseq;
    #endif
    typedef TSeqVar< SequenceTmpl< CORBA::Long,MICO_TID_DEF> > longseq_var;
    typedef TSeqOut< SequenceTmpl< CORBA::Long,MICO_TID_DEF> > longseq_out;

    typedef StringSequenceTmpl<CORBA::String_var> stringseq;
    #ifdef _WINDOWS
    static stringseq _dummy_stringseq;
    #endif
    typedef TSeqVar< StringSequenceTmpl<CORBA::String_var> > stringseq_var;
    typedef TSeqOut< StringSequenceTmpl<CORBA::String_var> > stringseq_out;

    virtual void ping() = 0;
    virtual void testArray( const longseq& longs, stringseq_out strings ) = 0;
    virtual AdressInfo* getAddressInfoA( const char* name ) = 0;
    virtual void getAddressInfoB( const char* name, AdressInfo_out adressinfo ) = 0;
    virtual void setAddressInfo( const char* name, const AdressInfo& adressinfo ) = 0;
    virtual AdressInfoSeq* getAllAdressInfos() = 0;
    virtual void setOtherAdressBook( AdressBook_ptr other ) = 0;
    virtual void getOtherAdressBook( AdressBook_out other ) = 0;

  protected:
    AdressBook() {};
  private:
    AdressBook( const AdressBook& );
    void operator=( const AdressBook& );
};

// Stub for interface AdressBook
class AdressBook_stub:
  virtual public AdressBook
{
  public:
    virtual ~AdressBook_stub();
    void ping();
    void testArray( const longseq& longs, stringseq_out strings );
    AdressInfo* getAddressInfoA( const char* name );
    void getAddressInfoB( const char* name, AdressInfo_out adressinfo );
    void setAddressInfo( const char* name, const AdressInfo& adressinfo );
    AdressInfoSeq* getAllAdressInfos();
    void setOtherAdressBook( AdressBook_ptr other );
    void getOtherAdressBook( AdressBook_out other );

  private:
    void operator=( const AdressBook_stub& );
};

class AdressBook_skel :
  virtual public StaticMethodDispatcher,
  virtual public AdressBook
{
  public:
    AdressBook_skel( const CORBA::BOA::ReferenceData & = CORBA::BOA::ReferenceData() );
    virtual ~AdressBook_skel();
    AdressBook_skel( CORBA::Object_ptr obj );
    virtual bool dispatch( CORBA::StaticServerRequest_ptr __req, CORBA::Environment &_env );
    AdressBook_ptr _this();

};

}
}
}


#ifndef MICO_CONF_NO_POA

#endif // MICO_CONF_NO_POA

extern CORBA::StaticTypeInfo *_marshaller_tests_acdkx_orb_AdressInfo;

extern CORBA::StaticTypeInfo *_marshaller_tests_acdkx_orb_AdressBook;

extern CORBA::StaticTypeInfo *_marshaller__seq_tests_acdkx_orb_AdressInfo;

#endif
