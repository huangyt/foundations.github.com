#ifndef acdkx_net_ssl_SSLSocket_h
#define acdkx_net_ssl_SSLSocket_h

#include <acdk.h>
#include <acdk/net/Socket.h>
#include "Config.h"
//#include "SSLSession.h"

namespace acdkx {
namespace net {
namespace ssl {

USING_CLASS(acdk::net::, Socket);

ACDK_DECL_CLASS(SSLSocket);

/**
  Implements a SSL Socket on basis of TCPSocket.
*/
class ACDKX_NET_SSL_PUBLIC SSLSocket
: extends Socket
{
  ACDK_WITH_METAINFO(SSLSocket)
  
public:
  SSLSocket();
  SSLSocket(IN(RString) host, int port);
  SSLSocket(IN(acdk::net::RInetAddress) address, int port);

  //virtual void addHandshakeCompletedListener(IN(RHandshakeCompletedListener) listener) = 0;
  //virtual void removeHandshakeCompletedListener(IN(RHandshakeCompletedListener) listener) = 0;
  /*
  virtual RStringArray getEnabledCipherSuites();
  virtual void setEnabledCipherSuites(IN(RStringArray) suites);
  virtual RStringArray getEnabledProtocols();
  virtual void setEnabledProtocols(IN(RStringArray) protocols);
  virtual bool getEnableSessionCreation();
  virtual void setEnableSessionCreation(bool enable);
  virtual bool getNeedClientAuth();
  virtual void setNeedClientAuth(bool needAuth);
  virtual RSSLSession getSession();
  virtual RStringArray getSupportedCipherSuites();
  virtual RStringArray getSupportedProtocols();
  virtual bool getUseClientMode();
  virtual void setUseClientMode(bool clientMode);
  virtual bool getWantClientAuth();
  virtual void setWantClientAuth(bool wantAuth);
  virtual void startHandshake() = 0;
  */
 
private:
  
};

} // namespace acdkx
} // namespace net
} // namespace ssl

#endif //acdkx_net_ssl_SSLSocket_h
