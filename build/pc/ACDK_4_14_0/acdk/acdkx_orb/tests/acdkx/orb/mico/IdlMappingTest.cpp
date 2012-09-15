/*
 *  MICO --- an Open Source CORBA implementation
 *  Copyright (c) 1997-2001 by The Mico Team
 *
 *  This file was automatically generated. DO NOT EDIT!
 */

#include "IdlMappingTest.h"

//--------------------------------------------------------
//  Implementation of stubs
//--------------------------------------------------------
#ifdef HAVE_EXPLICIT_STRUCT_OPS
tests::acdkx::orb::AdressInfo::AdressInfo()
{
}

tests::acdkx::orb::AdressInfo::AdressInfo( const AdressInfo& _s )
{
  name = ((AdressInfo&)_s).name;
  street = ((AdressInfo&)_s).street;
  streetnumber = ((AdressInfo&)_s).streetnumber;
}

tests::acdkx::orb::AdressInfo::~AdressInfo()
{
}

tests::acdkx::orb::AdressInfo&
tests::acdkx::orb::AdressInfo::operator=( const AdressInfo& _s )
{
  name = ((AdressInfo&)_s).name;
  street = ((AdressInfo&)_s).street;
  streetnumber = ((AdressInfo&)_s).streetnumber;
  return *this;
}
#endif

class _Marshaller_tests_acdkx_orb_AdressInfo : public CORBA::StaticTypeInfo {
    typedef tests::acdkx::orb::AdressInfo _MICO_T;
  public:
    StaticValueType create () const;
    void assign (StaticValueType dst, const StaticValueType src) const;
    void free (StaticValueType) const;
    CORBA::Boolean demarshal (CORBA::DataDecoder&, StaticValueType) const;
    void marshal (CORBA::DataEncoder &, StaticValueType) const;
};


CORBA::StaticValueType _Marshaller_tests_acdkx_orb_AdressInfo::create() const
{
  return (StaticValueType) new _MICO_T;
}

void _Marshaller_tests_acdkx_orb_AdressInfo::assign( StaticValueType d, const StaticValueType s ) const
{
  *(_MICO_T*) d = *(_MICO_T*) s;
}

void _Marshaller_tests_acdkx_orb_AdressInfo::free( StaticValueType v ) const
{
  delete (_MICO_T*) v;
}

CORBA::Boolean _Marshaller_tests_acdkx_orb_AdressInfo::demarshal( CORBA::DataDecoder &dc, StaticValueType v ) const
{
  return
    dc.struct_begin() &&
    CORBA::_stc_string->demarshal( dc, &((_MICO_T*)v)->name._for_demarshal() ) &&
    CORBA::_stc_string->demarshal( dc, &((_MICO_T*)v)->street._for_demarshal() ) &&
    CORBA::_stc_long->demarshal( dc, &((_MICO_T*)v)->streetnumber ) &&
    dc.struct_end();
}

void _Marshaller_tests_acdkx_orb_AdressInfo::marshal( CORBA::DataEncoder &ec, StaticValueType v ) const
{
  ec.struct_begin();
  CORBA::_stc_string->marshal( ec, &((_MICO_T*)v)->name.inout() );
  CORBA::_stc_string->marshal( ec, &((_MICO_T*)v)->street.inout() );
  CORBA::_stc_long->marshal( ec, &((_MICO_T*)v)->streetnumber );
  ec.struct_end();
}

CORBA::StaticTypeInfo *_marshaller_tests_acdkx_orb_AdressInfo;





/*
 * Base interface for class AdressBook
 */

tests::acdkx::orb::AdressBook::~AdressBook()
{
}

void *
tests::acdkx::orb::AdressBook::_narrow_helper( const char *_repoid )
{
  if( strcmp( _repoid, "IDL:tests/acdkx/orb/AdressBook:1.0" ) == 0 )
    return (void *)this;
  return NULL;
}

tests::acdkx::orb::AdressBook_ptr
tests::acdkx::orb::AdressBook::_narrow( CORBA::Object_ptr _obj )
{
  tests::acdkx::orb::AdressBook_ptr _o;
  if( !CORBA::is_nil( _obj ) ) {
    void *_p;
    if( (_p = _obj->_narrow_helper( "IDL:tests/acdkx/orb/AdressBook:1.0" )))
      return _duplicate( (tests::acdkx::orb::AdressBook_ptr) _p );
    if (!strcmp (_obj->_repoid(), "IDL:tests/acdkx/orb/AdressBook:1.0") || _obj->_is_a_remote ("IDL:tests/acdkx/orb/AdressBook:1.0")) {
      _o = new tests::acdkx::orb::AdressBook_stub;
      _o->MICO_SCOPE(CORBA,Object::operator=)( *_obj );
      return _o;
    }
  }
  return _nil();
}

tests::acdkx::orb::AdressBook_ptr
tests::acdkx::orb::AdressBook::_narrow( CORBA::AbstractBase_ptr _obj )
{
  return _narrow (_obj->_to_object());
}

class _Marshaller_tests_acdkx_orb_AdressBook : public CORBA::StaticTypeInfo {
    typedef tests::acdkx::orb::AdressBook_ptr _MICO_T;
  public:
    StaticValueType create () const;
    void assign (StaticValueType dst, const StaticValueType src) const;
    void free (StaticValueType) const;
    void release (StaticValueType) const;
    CORBA::Boolean demarshal (CORBA::DataDecoder&, StaticValueType) const;
    void marshal (CORBA::DataEncoder &, StaticValueType) const;
};


CORBA::StaticValueType _Marshaller_tests_acdkx_orb_AdressBook::create() const
{
  return (StaticValueType) new _MICO_T( 0 );
}

void _Marshaller_tests_acdkx_orb_AdressBook::assign( StaticValueType d, const StaticValueType s ) const
{
  *(_MICO_T*) d = ::tests::acdkx::orb::AdressBook::_duplicate( *(_MICO_T*) s );
}

void _Marshaller_tests_acdkx_orb_AdressBook::free( StaticValueType v ) const
{
  CORBA::release( *(_MICO_T *) v );
  delete (_MICO_T*) v;
}

void _Marshaller_tests_acdkx_orb_AdressBook::release( StaticValueType v ) const
{
  CORBA::release( *(_MICO_T *) v );
}

CORBA::Boolean _Marshaller_tests_acdkx_orb_AdressBook::demarshal( CORBA::DataDecoder &dc, StaticValueType v ) const
{
  CORBA::Object_ptr obj;
  if (!CORBA::_stc_Object->demarshal(dc, &obj))
    return FALSE;
  *(_MICO_T *) v = ::tests::acdkx::orb::AdressBook::_narrow( obj );
  CORBA::Boolean ret = CORBA::is_nil (obj) || !CORBA::is_nil (*(_MICO_T *)v);
  CORBA::release (obj);
  return ret;
}

void _Marshaller_tests_acdkx_orb_AdressBook::marshal( CORBA::DataEncoder &ec, StaticValueType v ) const
{
  CORBA::Object_ptr obj = *(_MICO_T *) v;
  CORBA::_stc_Object->marshal( ec, &obj );
}

CORBA::StaticTypeInfo *_marshaller_tests_acdkx_orb_AdressBook;


/*
 * Stub interface for class AdressBook
 */

tests::acdkx::orb::AdressBook_stub::~AdressBook_stub()
{
}

void tests::acdkx::orb::AdressBook_stub::ping()
{
  CORBA::StaticRequest __req( this, "ping" );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
}


void tests::acdkx::orb::AdressBook_stub::testArray( const tests::acdkx::orb::AdressBook::longseq& _par_longs, tests::acdkx::orb::AdressBook::stringseq_out _par_strings )
{
  CORBA::StaticAny _sa_longs( CORBA::_stcseq_long, &_par_longs );
  CORBA::StaticAny _sa_strings( CORBA::_stcseq_string );
  CORBA::StaticRequest __req( this, "testArray" );
  __req.add_in_arg( &_sa_longs );
  __req.add_out_arg( &_sa_strings );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
  _par_strings = (tests::acdkx::orb::AdressBook::stringseq*) _sa_strings._retn();
}


tests::acdkx::orb::AdressInfo* tests::acdkx::orb::AdressBook_stub::getAddressInfoA( const char* _par_name )
{
  CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name );
  CORBA::StaticAny __res( _marshaller_tests_acdkx_orb_AdressInfo );

  CORBA::StaticRequest __req( this, "getAddressInfoA" );
  __req.add_in_arg( &_sa_name );
  __req.set_result( &__res );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
  return (tests::acdkx::orb::AdressInfo*) __res._retn();
}


void tests::acdkx::orb::AdressBook_stub::getAddressInfoB( const char* _par_name, tests::acdkx::orb::AdressInfo_out _par_adressinfo )
{
  CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name );
  CORBA::StaticAny _sa_adressinfo( _marshaller_tests_acdkx_orb_AdressInfo );
  CORBA::StaticRequest __req( this, "getAddressInfoB" );
  __req.add_in_arg( &_sa_name );
  __req.add_out_arg( &_sa_adressinfo );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
  _par_adressinfo = (tests::acdkx::orb::AdressInfo*) _sa_adressinfo._retn();
}


void tests::acdkx::orb::AdressBook_stub::setAddressInfo( const char* _par_name, const tests::acdkx::orb::AdressInfo& _par_adressinfo )
{
  CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name );
  CORBA::StaticAny _sa_adressinfo( _marshaller_tests_acdkx_orb_AdressInfo, &_par_adressinfo );
  CORBA::StaticRequest __req( this, "setAddressInfo" );
  __req.add_in_arg( &_sa_name );
  __req.add_in_arg( &_sa_adressinfo );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
}


tests::acdkx::orb::AdressInfoSeq* tests::acdkx::orb::AdressBook_stub::getAllAdressInfos()
{
  CORBA::StaticAny __res( _marshaller__seq_tests_acdkx_orb_AdressInfo );

  CORBA::StaticRequest __req( this, "getAllAdressInfos" );
  __req.set_result( &__res );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
  return (tests::acdkx::orb::AdressInfoSeq*) __res._retn();
}


void tests::acdkx::orb::AdressBook_stub::setOtherAdressBook( tests::acdkx::orb::AdressBook_ptr _par_other )
{
  CORBA::StaticAny _sa_other( _marshaller_tests_acdkx_orb_AdressBook, &_par_other );
  CORBA::StaticRequest __req( this, "setOtherAdressBook" );
  __req.add_in_arg( &_sa_other );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
}


void tests::acdkx::orb::AdressBook_stub::getOtherAdressBook( tests::acdkx::orb::AdressBook_out _par_other )
{
  CORBA::StaticAny _sa_other( _marshaller_tests_acdkx_orb_AdressBook, &_par_other.ptr() );
  CORBA::StaticRequest __req( this, "getOtherAdressBook" );
  __req.add_out_arg( &_sa_other );

  __req.invoke();

  mico_sii_throw( &__req, 
    0);
}


class _Marshaller__seq_tests_acdkx_orb_AdressInfo : public CORBA::StaticTypeInfo {
    typedef SequenceTmpl< tests::acdkx::orb::AdressInfo,MICO_TID_DEF> _MICO_T;
  public:
    StaticValueType create () const;
    void assign (StaticValueType dst, const StaticValueType src) const;
    void free (StaticValueType) const;
    CORBA::Boolean demarshal (CORBA::DataDecoder&, StaticValueType) const;
    void marshal (CORBA::DataEncoder &, StaticValueType) const;
};


CORBA::StaticValueType _Marshaller__seq_tests_acdkx_orb_AdressInfo::create() const
{
  return (StaticValueType) new _MICO_T;
}

void _Marshaller__seq_tests_acdkx_orb_AdressInfo::assign( StaticValueType d, const StaticValueType s ) const
{
  *(_MICO_T*) d = *(_MICO_T*) s;
}

void _Marshaller__seq_tests_acdkx_orb_AdressInfo::free( StaticValueType v ) const
{
  delete (_MICO_T*) v;
}

CORBA::Boolean _Marshaller__seq_tests_acdkx_orb_AdressInfo::demarshal( CORBA::DataDecoder &dc, StaticValueType v ) const
{
  CORBA::ULong len;
  if( !dc.seq_begin( len ) )
    return FALSE;
  ((_MICO_T *) v)->length( len );
  for( CORBA::ULong i = 0; i < len; i++ ) {
    if( !_marshaller_tests_acdkx_orb_AdressInfo->demarshal( dc, &(*(_MICO_T*)v)[i] ) )
      return FALSE;
  }
  return dc.seq_end();
}

void _Marshaller__seq_tests_acdkx_orb_AdressInfo::marshal( CORBA::DataEncoder &ec, StaticValueType v ) const
{
  CORBA::ULong len = ((_MICO_T *) v)->length();
  ec.seq_begin( len );
  for( CORBA::ULong i = 0; i < len; i++ )
    _marshaller_tests_acdkx_orb_AdressInfo->marshal( ec, &(*(_MICO_T*)v)[i] );
  ec.seq_end();
}

CORBA::StaticTypeInfo *_marshaller__seq_tests_acdkx_orb_AdressInfo;

struct __tc_init_IDLMAPPINGTEST {
  __tc_init_IDLMAPPINGTEST()
  {
    _marshaller_tests_acdkx_orb_AdressInfo = new _Marshaller_tests_acdkx_orb_AdressInfo;
    _marshaller_tests_acdkx_orb_AdressBook = new _Marshaller_tests_acdkx_orb_AdressBook;
    _marshaller__seq_tests_acdkx_orb_AdressInfo = new _Marshaller__seq_tests_acdkx_orb_AdressInfo;
  }
};

static __tc_init_IDLMAPPINGTEST __init_IDLMAPPINGTEST;

//--------------------------------------------------------
//  Implementation of skeletons
//--------------------------------------------------------

tests::acdkx::orb::AdressBook_skel::AdressBook_skel( const CORBA::BOA::ReferenceData &_id )
{
  CORBA::ImplementationDef_var _impl =
    _find_impl( "IDL:tests/acdkx/orb/AdressBook:1.0", "AdressBook" );
  _create_ref( _id,
    0,
    _impl,
    "IDL:tests/acdkx/orb/AdressBook:1.0" );
  register_dispatcher( new StaticInterfaceDispatcherWrapper< AdressBook_skel>( this ) );
}

tests::acdkx::orb::AdressBook_skel::AdressBook_skel( CORBA::Object_ptr _obj )
{
  CORBA::ImplementationDef_var _impl =
    _find_impl( "IDL:tests/acdkx/orb/AdressBook:1.0", "AdressBook" );
  assert( !CORBA::is_nil( _impl ) );
  _restore_ref( _obj,
    CORBA::BOA::ReferenceData(),
    0,
    _impl );
  register_dispatcher( new StaticInterfaceDispatcherWrapper< AdressBook_skel>( this ) );
}

tests::acdkx::orb::AdressBook_skel::~AdressBook_skel()
{
}

bool tests::acdkx::orb::AdressBook_skel::dispatch( CORBA::StaticServerRequest_ptr __req, CORBA::Environment & /*_env*/ )
{
  #ifdef HAVE_EXCEPTIONS
  try {
  #endif
    switch (mico_string_hash (__req->op_name(), 13)) {
    case 0:
      if( strcmp( __req->op_name(), "getAddressInfoB" ) == 0 ) {
        CORBA::String_var _par_name;
        CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name._for_demarshal() );
        AdressInfo* _par_adressinfo;
        CORBA::StaticAny _sa_adressinfo( _marshaller_tests_acdkx_orb_AdressInfo );

        __req->add_in_arg( &_sa_name );
        __req->add_out_arg( &_sa_adressinfo );

        if( !__req->read_args() )
          return true;

        getAddressInfoB( _par_name.inout(), _par_adressinfo );
        _sa_adressinfo.value( _marshaller_tests_acdkx_orb_AdressInfo, _par_adressinfo );
        __req->write_results();
        delete _par_adressinfo;
        return true;
      }
      if( strcmp( __req->op_name(), "setOtherAdressBook" ) == 0 ) {
        AdressBook_var _par_other;
        CORBA::StaticAny _sa_other( _marshaller_tests_acdkx_orb_AdressBook, &_par_other._for_demarshal() );

        __req->add_in_arg( &_sa_other );

        if( !__req->read_args() )
          return true;

        setOtherAdressBook( _par_other.inout() );
        __req->write_results();
        return true;
      }
      break;
    case 2:
      if( strcmp( __req->op_name(), "getOtherAdressBook" ) == 0 ) {
        AdressBook_ptr _par_other;
        CORBA::StaticAny _sa_other( _marshaller_tests_acdkx_orb_AdressBook, &_par_other );

        __req->add_out_arg( &_sa_other );

        if( !__req->read_args() )
          return true;

        getOtherAdressBook( _par_other );
        __req->write_results();
        CORBA::release( _par_other );
        return true;
      }
      break;
    case 3:
      if( strcmp( __req->op_name(), "testArray" ) == 0 ) {
        longseq _par_longs;
        CORBA::StaticAny _sa_longs( CORBA::_stcseq_long, &_par_longs );
        stringseq* _par_strings;
        CORBA::StaticAny _sa_strings( CORBA::_stcseq_string );

        __req->add_in_arg( &_sa_longs );
        __req->add_out_arg( &_sa_strings );

        if( !__req->read_args() )
          return true;

        testArray( _par_longs, _par_strings );
        _sa_strings.value( CORBA::_stcseq_string, _par_strings );
        __req->write_results();
        delete _par_strings;
        return true;
      }
      break;
    case 7:
      if( strcmp( __req->op_name(), "setAddressInfo" ) == 0 ) {
        CORBA::String_var _par_name;
        CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name._for_demarshal() );
        AdressInfo _par_adressinfo;
        CORBA::StaticAny _sa_adressinfo( _marshaller_tests_acdkx_orb_AdressInfo, &_par_adressinfo );

        __req->add_in_arg( &_sa_name );
        __req->add_in_arg( &_sa_adressinfo );

        if( !__req->read_args() )
          return true;

        setAddressInfo( _par_name.inout(), _par_adressinfo );
        __req->write_results();
        return true;
      }
      break;
    case 8:
      if( strcmp( __req->op_name(), "ping" ) == 0 ) {

        if( !__req->read_args() )
          return true;

        ping();
        __req->write_results();
        return true;
      }
      break;
    case 9:
      if( strcmp( __req->op_name(), "getAllAdressInfos" ) == 0 ) {
        AdressInfoSeq* _res;
        CORBA::StaticAny __res( _marshaller__seq_tests_acdkx_orb_AdressInfo );
        __req->set_result( &__res );

        if( !__req->read_args() )
          return true;

        _res = getAllAdressInfos();
        __res.value( _marshaller__seq_tests_acdkx_orb_AdressInfo, _res );
        __req->write_results();
        delete _res;
        return true;
      }
      break;
    case 12:
      if( strcmp( __req->op_name(), "getAddressInfoA" ) == 0 ) {
        CORBA::String_var _par_name;
        CORBA::StaticAny _sa_name( CORBA::_stc_string, &_par_name._for_demarshal() );

        AdressInfo* _res;
        CORBA::StaticAny __res( _marshaller_tests_acdkx_orb_AdressInfo );
        __req->add_in_arg( &_sa_name );
        __req->set_result( &__res );

        if( !__req->read_args() )
          return true;

        _res = getAddressInfoA( _par_name.inout() );
        __res.value( _marshaller_tests_acdkx_orb_AdressInfo, _res );
        __req->write_results();
        delete _res;
        return true;
      }
      break;
    }
  #ifdef HAVE_EXCEPTIONS
  } catch( CORBA::SystemException_catch &_ex ) {
    __req->set_exception( _ex->_clone() );
    __req->write_results();
    return true;
  } catch( ... ) {
    assert( 0 );
    return true;
  }
  #endif
  return false;
}

tests::acdkx::orb::AdressBook_ptr tests::acdkx::orb::AdressBook_skel::_this()
{
  return tests::acdkx::orb::AdressBook::_duplicate( this );
}

