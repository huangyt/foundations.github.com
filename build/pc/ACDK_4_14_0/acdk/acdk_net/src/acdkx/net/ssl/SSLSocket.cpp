
#include "SSLSocket.h"

#include <acdk/net/TCPSocket.h>
#include <acdk/net/SocketException.h>


#undef final
#include <openssl/crypto.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <openssl/ssl.h>
#include <openssl/err.h>


namespace acdkx {
namespace net {
namespace ssl {

using namespace acdk::net;
using namespace acdk::io;




struct BioReaderWriterWrapper
{
  RReader _in;
  RWriter _out;
  bool _readEof;
  BioReaderWriterWrapper(IN(RReader) rin, IN(RWriter) rout)
    : _in(rin)
    , _out(rout)
    , _readEof(false)
  {
  }
  static BIO* createBio(IN(RReader) rin, IN(RWriter) rout);
  static int  write(BIO* bio, const char* buf, int num)
  {
    BioReaderWriterWrapper* wrapper = reinterpret_cast<BioReaderWriterWrapper*>(bio->ptr);
    wrapper->_out->write((const byte*)buf, 0, num);
    return num;
  }
  static int  read(BIO* bio, char* buf, int size)
  {
    BioReaderWriterWrapper* wrapper = reinterpret_cast<BioReaderWriterWrapper*>(bio->ptr);
    int ret = wrapper->_in->read((byte*)buf, 0, size);
    if (ret == -1)
    {
      wrapper->_readEof = true;
      return 0;
    }
    return ret;
  }
  static long ctrl(BIO* bio, int cmd, long num, void* ptr)
  {
    BioReaderWriterWrapper* wrapper = reinterpret_cast<BioReaderWriterWrapper*>(bio->ptr);
    switch (cmd) 
    {
    case BIO_CTRL_RESET:
	    return num = 0;
    case BIO_C_FILE_SEEK:
    case BIO_C_FILE_TELL:
	    return 0;
    case BIO_CTRL_INFO:
	    return num;
    case BIO_C_GET_FD:
	    if (bio->init) 
      {
	      int* ip = (int *)ptr;
	      if (ip != 0) 
		      *ip = bio->num;
	      return bio->num;
  	  } 
      return -1;
    case BIO_CTRL_GET_CLOSE:
	    return bio->shutdown;
	    
    case BIO_CTRL_SET_CLOSE:
	    return bio->shutdown = (int)num;
    case BIO_CTRL_EOF:
      return wrapper->_readEof == true ? 1 : 0;
    case BIO_CTRL_PENDING:
	    //dprintf(stderr, "BIO_CTRL_PENDING(%d)\n", (int) ret);
	    
      //ret = (Tcl_InputBuffered(chan) ? 1 : 0);
	    break;
    case BIO_CTRL_WPENDING:
	    return 0;
    case BIO_CTRL_DUP:
      return num;
	    
    case BIO_CTRL_FLUSH:
      if (wrapper->_out != Nil)
      {
        wrapper->_out->flush();
        return 0;
      }
      return -1;
    case BIO_C_SET_FD:
    default:
	    return 0; 
    }
    return 0;
  }
  static int allocate(BIO *bio)
  {
    bio->init = 0;
	  bio->num = 0;
	  bio->ptr = 0;
	  return 1;
  }
  static int  free(BIO *bio)
  {
    if (bio == 0 || bio->ptr == 0)
      return 0;
    BioReaderWriterWrapper* wrapper = reinterpret_cast<BioReaderWriterWrapper*>(bio->ptr);
    bio->ptr = 0;
    delete wrapper;
	return 0;
  }
  static int  puts(BIO *h, char *str)
  {
    return -1;
  }
  static int  gets(BIO *h, char *str, int size)
  {
    return -1;
  }

};



BIO_METHOD bioReaderWriterWrapperMethods = 
{
    0xACDC, "ACDK",
    BioReaderWriterWrapper::write,
    BioReaderWriterWrapper::read,
    0, //BioReaderWriterWrapper::puts,
    0, //BioReaderWriterWrapper::gets,	/* BioGets */
    BioReaderWriterWrapper::ctrl,
    BioReaderWriterWrapper::allocate,
    BioReaderWriterWrapper::free,
};

BIO* 
BioReaderWriterWrapper::createBio(IN(RReader) rin, IN(RWriter) rout)
{
  BioReaderWriterWrapper* wrapper = new BioReaderWriterWrapper(rin, rout);
  BIO *bio = BIO_new(&bioReaderWriterWrapperMethods);
  bio->ptr		= (char*)wrapper;
  bio->init		= 1;
  bio->shutdown	= 0;
  return bio;
}




class ACDKX_NET_SSL_PUBLIC SSLSocketImpl
: extends acdk::net::TCPSocket
{
  foreign static SSL_CTX* _sslContext;
  foreign SSL *_ssl;
public:
  SSLSocketImpl()
    : TCPSocket()
  {
  }
  ~SSLSocketImpl();
  virtual void create(bool stream);
  virtual  void connect(IN(RInetAddress) address, int port);
  virtual void close();

  virtual int read();
  virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);  
  // Methods inherited from Writer
  virtual void flush();

  foreign virtual void write(const byte* cstr, int offset, int len);
  virtual void write(byte c);
  virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);
protected:
  static void initOpenSLL();
  foreign void _createCtx();
  
};

SSL_CTX* SSLSocketImpl::_sslContext = 0;

//virtual 
void 
SSLSocketImpl::create(bool stream)
{
  TCPSocket::create(stream);
}

//virtual  
void 
SSLSocketImpl::connect(IN(RInetAddress) address, int port)
{
  TCPSocket::connect(address, port);
  _createCtx();
  _ssl = SSL_new(_sslContext);
  SSL_set_fd(_ssl, fd->c_fd());
  int ret = SSL_connect(_ssl);
  if (ret < 0)
  {
    THROW0(SocketException); // #### TODO better Ex
  }
}


//virtual 
void 
SSLSocketImpl::close()
{
  if (_ssl != 0)
  {
    SSL_shutdown(_ssl);
    _ssl = 0;
  }
  TCPSocket::close();
}

SSLSocketImpl::~SSLSocketImpl()
{
  close();
}

int 
SSLSocketImpl::read()
{
  byte buf[1];
  int ret = read(buf, 0, 1);
  if (ret == -1)
    return ret;
  return buf[0];
}

int 
SSLSocketImpl::read(IN(RbyteArray) buffer, int offset, int len)
{
  return read(buffer->data(), offset, (len == -1 ? buffer->length() - offset : len));
}

int 
SSLSocketImpl::read(byte* buffer, int offset, int len)
{
  char* ptr = (char*)buffer + offset;
  int ret = SSL_read(_ssl, ptr, len);
  int err = SSL_get_error(_ssl, ret);
  if (err == SSL_ERROR_ZERO_RETURN)
    return -1;
  if (err == SSL_ERROR_NONE)
    return ret;
  THROW0(SocketException); 
  return -1;
}

void 
SSLSocketImpl::flush()
{
}


void 
SSLSocketImpl::write(const byte* cstr, int offset, int len)
{
  int ret = SSL_write(_ssl, cstr + offset, len);
  if (ret < 0)
    THROW0(SocketException); 
}

void 
SSLSocketImpl::write(byte c)
{
  byte buf[1];
  buf[0] = c;
  write(buf, 0, 1);
}

void 
SSLSocketImpl::write(IN(RbyteArray) ch, int offset, int len)
{
  if (len == -1)
    len = ch->length() - offset;
  write(ch->data() + offset, 0, len);
}

void
SSLSocketImpl::_createCtx()
{
  if (_sslContext != 0)
    return;
  initOpenSLL();
  _sslContext = SSL_CTX_new(SSLv23_client_method());        
}

void 
SSLSocketImpl::initOpenSLL()
{
  static bool _sslInited = false;
  if (_sslInited == true)
    return;

  OpenSSL_add_ssl_algorithms();
  SSL_load_error_strings();
  _sslInited = true;
}

SSLSocket::SSLSocket()
: Socket(new SSLSocketImpl())
{
}

SSLSocket::SSLSocket(IN(RString) host, int port)
: Socket(new SSLSocketImpl())
{
  RInetAddress address = InetAddress::getByName(host);
  _theSocket->create(true);
  try {
    _theSocket->connect(address, port);
    _opened = true;
  } catch (RSocketException ex) {
    _theSocket->close();
    throw ex;
  }

  
}


SSLSocket::SSLSocket(IN(acdk::net::RInetAddress) address, int port)
: Socket(new SSLSocketImpl())
{
  try {
    _theSocket->connect(address, port);
  } catch (RSocketException ) {
    _theSocket->close();
    throw;
  }
}





/*
  virtual RStringArray getEnabledCipherSuites() = 0;
  virtual void setEnabledCipherSuites(IN(RStringArray) suites) = 0;
  virtual RStringArray getEnabledProtocols() = 0;
  virtual void setEnabledProtocols(IN(RStringArray) protocols) = 0;
  virtual bool getEnableSessionCreation() = 0;
  virtual void setEnableSessionCreation(bool enable) = 0;
  virtual bool getNeedClientAuth() = 0;
  virtual void setNeedClientAuth(bool needAuth) = 0;
  virtual RSSLSession getSession() = 0;
  virtual RStringArray getSupportedCipherSuites() = 0;
  virtual RStringArray getSupportedProtocols() = 0;
  virtual bool getUseClientMode() = 0;
  virtual void setUseClientMode(bool clientMode) = 0;
  virtual bool getWantClientAuth() = 0;
  virtual void setWantClientAuth(bool wantAuth) = 0;
  virtual void startHandshake() = 0;

*/
} // namespace acdkx
} // namespace net
} // namespace ssl

