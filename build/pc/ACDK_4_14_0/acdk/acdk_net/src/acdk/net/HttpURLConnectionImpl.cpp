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
// $Header: /cvsroot/acdk/acdk/acdk_net/src/acdk/net/HttpURLConnectionImpl.cpp,v 1.9 2005/03/30 17:30:15 kommer Exp $

#include "HttpURLConnectionImpl.h"

namespace acdk {
namespace net {

  
HttpURLConnectionImpl::HttpURLConnectionImpl(IN(RURL) url)
: HttpURLConnection(url)
{
  doOutput = false;
  with_proxy = false;
  RString proxy = System::getProperty("http_proxy", Nil);
  if(proxy != Nil) {
    RURL u = new URL(proxy);
    with_proxy = true;
    proxy_name = u->getHost();
    proxy_port = u->getPort();
  }
  headers = new HeaderFieldHelper();
}

RSocket 
HttpURLConnectionImpl::createSocket(IN(RString) host, int port)
{
  return new Socket(host, port);
}

void 
HttpURLConnectionImpl::connect()
{
  RLineNumberCharReaderImpl in_char_stream;
  acdk::io::RPrintWriter out_writer;
  if (with_proxy)  {
    // Connect up
    socket = createSocket(proxy_name, proxy_port);
    
    out_stream = socket->getOutputStream();
    in_stream = socket->getInputStream();
    in_char_stream = new LineNumberCharReaderImpl(new ByteToCharReader(in_stream));
    
    out_writer = new PrintWriter(out_stream); 
    
    // Send the request
    out_writer->print(getRequestMethod() + " " + 
      getURL()->getProtocol() + "://" + 
      getURL()->getHost() + 
      (getURL()->getPort() == -1 
      ? RString("") 
      : RString(RString(":") + getURL()->getPort())) 
      + "/" + getURL()->getFile() + " HTTP/1.1\r\n");
  } else {
    // Connect up
    if (url->getPort() == -1)
      socket = createSocket(url->getHost(), getStandardPort());
    else
      socket = createSocket(url->getHost(), url->getPort());
    
    out_stream = socket->getOutputStream();
    in_stream = socket->getInputStream();
    in_char_stream = new LineNumberCharReaderImpl(new ByteToCharReader(in_stream));
    
    out_writer = new PrintWriter(out_stream); 
    
    // Send the request
    out_writer->print(getRequestMethod() + " " + getURL()->getFile() + " HTTP/1.1\r\n");
  }
  
  RString propval = getRequestProperty("host");
  if (propval == Nil)
    out_writer->print("Host: " + getURL()->getHost() + "\r\n");
  else
    out_writer->print("Host: " + propval + "\r\n");
  out_writer->print("Connection: close\r\n");
  
  propval = getRequestProperty("user-agent");
  if (propval == Nil)
    out_writer->print("User-Agent: jcl/0->0\r\n");
  else
    out_writer->print("User-Agent: " + propval + "\r\n");
  
  propval = getRequestProperty("accept");
  if (propval == Nil)
    out_writer->print("Accept: */*\r\n");
  else
    out_writer->print("Accept: " + propval + "\r\n");
  
  out_writer->print("\r\n");
  out_writer->flush();
  
  // Parse the reply
  RString line = in_char_stream->readLine();
  RString saveline = line;
  
  int idx = line->indexOf(" " );
  if ((idx == -1) || (line->length() < (idx + 6)))
    THROW1(IOException, "Server reply was unparseable: " + saveline);
  
  line = line->substring(idx + 1);
  RString code = line->substring(0, 3);
  try {
    responseCode = (HttpResponceCode)Integer::parseInt(code);
  } catch (NumberFormatException e) {
    THROW1(IOException, "Server reply was unparseable: " + saveline);
  } 
  responseMessage = line->substring(4);
  
  // Now read the header lines
  RString key;
  RString value;
  for (;;) 
  {
    line = in_char_stream->readLine()->trim(TrimRight | TrimNewLines);
    if (line->equals(""))
      break;
    
    // Check for folded lines
    
    if (line->startsWith(" ") || line->startsWith("\t")) 
    {
      line = line->trim(TrimLeft | TrimWhiteSpace)->trim(TrimRight | TrimNewLines);
      if (line->length() == 1)
        THROW1(IOException, "Server header lines were unparseable: " + line);
      
      value = value + " " + line;
    } 
    else 
    {
      if (key != Nil) 
      {
        headers->addHeaderField(key, value);
        key = Nil;
        value = Nil;
      }
      
      // Parse out key and value
      idx = line->indexOf(":");
      
      if ((idx == -1) || (line->length() < (idx + 2)))
        THROW1(IOException, "Server header lines were unparseable: " + line);
      
      key = line->substring(0, idx);
      value = line->substring(idx + 1);
      value = value->trim(TrimLeft | TrimWhiteSpace);
      if (value->length() == 1)
        THROW1(IOException, "Server header lines were unparseable: " + line);
        
      //value = value->substring(1);
    }
  }
  if (key != Nil) 
  {
    headers->addHeaderField(key, value);
  }
  connected = true;
}
  
void
HttpURLConnectionImpl::setRequestMethod(IN(RString) method)
{
  if (method->equalsIgnoreCase("GET") || method->equalsIgnoreCase("HEAD"))
    HttpURLConnection::setRequestMethod(method);
  else
    THROW1(ProtocolException, "Unsupported or unknown request method " + method);
}

} // namespace acdk
} // namespace net




