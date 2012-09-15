
#define MICO_CONF_IMR
//#include <CORBA-SMALL.h>
#include <CORBA.h>
#include "IdlMappingTest.h"    

#include <string>
#include <map>
#include <fstream>

using namespace std;

using namespace ::tests::acdkx::orb;

// generate with 
// idl --c++-suffix cpp --boa --no-poa IdlMappingTest.idl

class AdressBookImpl 
: virtual public ::tests::acdkx::orb::AdressBook_skel
{
  typedef map<string, ::tests::acdkx::orb::AdressInfo> AdressMap;
  AdressMap _addresses;
  AdressBook_var _other;
public:
  
   
  virtual void ping()
  {
    cout << "ping()"  << endl;
  }
  virtual void testArray( const longseq& longs, stringseq_out strings )
  {
    for (int i = 0; i < longs.length(); i++) {
      cout << "ta: " << longs[i] << endl;
    }
    
    char* buffer[2] = {"Hallo", "Leute"};
    strings = new stringseq(2, 2, buffer);
  }
  virtual AdressInfo* getAddressInfoA( const char* name )
  {
    string str(name);
    AdressMap::iterator it = _addresses.find(str);
    if (it == _addresses.end())
      return 0;
    return new AdressInfo(it->second);
  }
  virtual void getAddressInfoB( const char* name, AdressInfo_out adressinfo )
  {
    string str(name);
    AdressMap::iterator it = _addresses.find(str);
    if (it == _addresses.end())
      adressinfo = 0;
    else
      adressinfo = new AdressInfo(it->second);
  }
  virtual void setAddressInfo( const char* name, const AdressInfo& adressinfo ) 
  {
    cout << "setAddressInfo():"  
        << "\n  name: " << (const char*)adressinfo.name 
        << "\n  street: " << (const char*)adressinfo.street
        << "\n  streetnumber: " << adressinfo.streetnumber
        << endl;
    string str(name);
    _addresses[str] = adressinfo;
  }
  virtual AdressInfoSeq* getAllAdressInfos() 
  {
    AdressInfoSeq* ret = new AdressInfoSeq(_addresses.size());
    AdressMap::iterator it = _addresses.begin();
    for (int i = 0; it != _addresses.end(); ++it, ++i)
    {
      (*ret)[i] = it->second;
    }
    return ret;
  }
  virtual void setOtherAdressBook( AdressBook_ptr other ) 
  {
    _other = other->_duplicate(other);
    testWithOther();
  }
  virtual void getOtherAdressBook( AdressBook_out other )
  {
    other = _other;//_other->_duplicate(other);
  }
  void testWithOther()
  {
    AdressInfo adressinfo;
    adressinfo.name = (const char*)"a";
    adressinfo.street = (const char*)"A-Street";
    adressinfo.streetnumber = 1;
    _other->setAddressInfo( (const char*)"A", adressinfo);
    adressinfo.name = (const char*)"b";
    adressinfo.street = (const char*)"b-Street";
    adressinfo.streetnumber = 2;
    _other->setAddressInfo( (const char*)"B", adressinfo);
    adressinfo.name = (const char*)"c";
    adressinfo.street = (const char*)"c-Street";
    adressinfo.streetnumber = 3;
    _other->setAddressInfo( (const char*)"C", adressinfo);
    AdressInfo_var retadressinfo;
    _other->getAddressInfoB( (const char*)"B", retadressinfo);
    if (retadressinfo->streetnumber == 2)
      std::cout << "testWithOther() is OK" << std::endl;
    else
      std::cout << "testWithOther() FAILS" << std::endl;
  }
};



int main(int argc, char* argv[])   {
  CORBA::ORB_var  orb;
  CORBA::BOA_var  boa;
  AdressBook_var        obj_impl;

        // Initialisierung
  orb = CORBA::ORB_init(argc, argv, "mico-local-orb");
  boa = orb->BOA_init(argc, argv, "mico-local-boa");

        // Objekt-Implementation erzeugen
  obj_impl = new AdressBookImpl();
  CORBA::Object_var oir = orb->resolve_initial_references("InterfaceRepository");
  CORBA::Repository_var ir = CORBA::Repository::_narrow(oir.in());
  //PrimitiveDef_var llvar = ir->get_primitive(pk_longlong);

        // Objekt-Referenz (als String) ausgeben
  cout << "Objekt-Referenz dieser AdressBookImpl-Implementation:" << endl;
  string ostr = orb->object_to_string(obj_impl);
  cout << "\t" << ostr << endl;
  cout.flush();
  {
    ofstream f("D:\\artefaktur\\acdk\\bin\\AdressBook.ref");
    f << ostr << endl;
  }
        // Warten auf Klienten
  boa->impl_is_ready (CORBA::ImplementationDef::_nil());
  orb->run ();

  return 0;
}

