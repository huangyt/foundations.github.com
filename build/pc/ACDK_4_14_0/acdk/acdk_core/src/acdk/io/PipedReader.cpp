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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/PipedReader.cpp,v 1.14 2005/03/08 12:45:36 kommer Exp $





#include <acdk.h>
#include "PipedReader.h"
#include "PipedWriter.h"
#include <acdk/lang/System.h>

#include "IOException.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/InterruptedException.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

//static 
int PipedReader::PIPE_SIZE = 1024;
  


PipedReader::PipedReader()
: AbstractStorageReader(),
  _in(),
  _connected(false),
  _closed(false),
  _buffer(new (allocator()) byteArray(PIPE_SIZE)),
  _inPos(-1),
  _outPos(0)
{
}
  
PipedReader::PipedReader(IN(RPipedWriter) in)
: AbstractStorageReader(),
  _in(in),
  _connected(false),
  _closed(false),
  _buffer(new (allocator()) byteArray(PIPE_SIZE)),
  _inPos(-1),
  _outPos(0)
{
}

//virtual 
void 
PipedReader::connect(IN(RPipedWriter) in)
{
  if (in == _in)
    return;

  if (_connected)
    THROW1(IOException, "Pipe already connected");

  if (_closed)
    THROW1(IOException, "Stream is closed and cannot be reopened");
  {
    SYNCTHIS();
    in->connect(this);
    _connected = true;

  } // synchronized
}

//virtual 
void 
PipedReader::close()
{
  SYNCTHIS();
  _closed = true;
  notifyAll();
}

//virtual 
bool 
PipedReader::ready()
{
  if (_inPos == -1)
    return false;
  if (_outPos == (_inPos - 1))
    return false;
  if ((_outPos == _buffer.length()) && (_inPos == 0))
    return false;
  return true;
}

//virtual 
int 
PipedReader::read()
{
  byteArray buffer(1);
  read(SR(byteArray, buffer), 0, 1);
  return (unsigned char) buffer[0];
}

//virtual 
int 
PipedReader::read(IN(RbyteArray) buffer, int offset, int len)
{
  if (_connected == false)
    THROW1(IOException, "Pipe not connected"); 
  {
    SYNCHRONIZETHIS();
    if (len == -1)
      len = buffer->length() - offset;
    long pipe_size = PIPE_SIZE;
    int bytes_read = 0;
    for (;;) {
      if (_inPos != -1) { // If there are bytes, take them
        int desired_bytes = len - bytes_read;
        if (_outPos > _inPos) { // We are _inPos a "wrap" condition
          if (desired_bytes > (pipe_size - _outPos)) {
            if (_inPos == 0)
              desired_bytes = (pipe_size - _outPos) - 1;
            else
              desired_bytes = pipe_size - _outPos;
            acdk::lang::System::arraycopy(buffer, _outPos, _buffer, offset + bytes_read, desired_bytes);
            bytes_read += desired_bytes;
            _outPos += desired_bytes;
            desired_bytes = len - bytes_read;
            
            if (_outPos == pipe_size)
              _outPos = 0;
            
            notifyAll();
          } else {
            if ((_outPos + desired_bytes) == _inPos)
              --desired_bytes;
            
            if (((_outPos + desired_bytes) == pipe_size) && (_inPos == 0)) 
              desired_bytes = (pipe_size - _outPos) - 1;
            
            acdk::lang::System::arraycopy(buffer, _outPos, _buffer, offset + bytes_read, desired_bytes); 
            
            bytes_read += desired_bytes;
            _outPos += desired_bytes;
            desired_bytes = len - bytes_read;
            
            if (_outPos == pipe_size)
              _outPos = 0;
            
            notifyAll();
          }
        }
        
        // We are in a "no wrap" or condition (can also be fall through
        // from above
        if (_inPos > _outPos) {
          if (desired_bytes >= ((_inPos - _outPos) - 1))
            desired_bytes = (_inPos - _outPos) - 1;
          
          acdk::lang::System::arraycopy(buffer, _outPos, _buffer, offset + bytes_read, desired_bytes);
          
          bytes_read += desired_bytes;
          _outPos += desired_bytes;
          desired_bytes = len - bytes_read;
          
          if (_outPos == pipe_size)
            _outPos = 0;
          
          notifyAll();
        }
      }
      
      // If we are done, return
      if (bytes_read == len)
        return(bytes_read);
      
      // Return a short count if necessary
      if (bytes_read < len)
        ;//### if (try_not_to_block)
        //###  return bytes_read;
        
        // Handle the case where the end of stream was encountered.
        if (_closed == true) {
          // We never let in == _outPos so there might be one last byte
          // available that we have not copied yet.
          if (_inPos != -1) {
            _buffer[offset + bytes_read] = buffer[_outPos];
            _inPos = -1;
            ++_outPos;
            ++bytes_read;
          }
          
          if (bytes_read != 0)
            return bytes_read;
          else
            return -1;
        }
        
        // Wait for a byte to be read
        try {
          wait();
        } catch (RInterruptedException ) { 
          ; 
        }
    } 
  } // SYNCHRONIZETHIS();
  return -1;
}


void 
PipedReader::pipewrite(byte c)
{
  SYNCHRONIZETHIS();
  if (_connected == false) 
    THROW1(IOException, "Pipe not connected");
  if (_closed == true) 
    THROW1(IOException, "Pipe closed");
  byteArray chbuf(1);
  chbuf[0] = c;
  pipewrite(&chbuf, 0, 1);
}

void 
PipedReader::pipewrite(const byte* buffer, int offset, int len)
{
  byteArray ba(buffer + offset, len - offset);
  pipewrite(&ba, offset, len);
}

void 
PipedReader::pipewrite(IN(RbyteArray) buffer, int offset, int len)
{
  if (_connected == false) 
    THROW1(IOException, "Pipe not connected");
  if (_closed == true) 
    THROW1(IOException, "Pipe closed");
  if (len <= 0)
    return;
  {
    SYNCHRONIZETHIS();

    int total_written = 0;
    
    while (total_written < len) {
      // If we are not at the end of the buffer with _outPos = 0
      if (!((_inPos == (buffer.length() - 1)) && (_outPos == 0))) {
        // This is the "no wrap" situation
        if ((_inPos - 1) >= _outPos) {
          int bytes_written = 0;
          if (buffer.length() - _inPos > (len - total_written))
            bytes_written = (len - total_written);
          else if (_outPos == 0)
            bytes_written = (buffer.length() - _inPos) - 1;
          else 
            bytes_written = (buffer.length() - _inPos);
          
          if (bytes_written > 0) 
            acdk::lang::System::arraycopy(_buffer, offset + total_written, buffer, _inPos,  bytes_written);
          total_written += bytes_written;
          _inPos += bytes_written;
          
          if (_inPos == buffer.length())
            _inPos = 0;
          
          notifyAll();
        }
        // This is the "wrap" situtation
        if ((_outPos > _inPos) && (total_written != len)) {
          int bytes_written = 0;
          
          // Do special processing if we are at the beginning
          if (_inPos == -1) {
            _inPos = 0;
            if (buffer.length() > len)
              bytes_written = len;
            else
              bytes_written = buffer.length() - 1;
          } else if ((_outPos - _inPos) - 1 < (len - total_written)) {
            bytes_written = (_outPos - _inPos) - 1;
          } else {
            bytes_written = len - total_written;
          }
          
          // If the buffer is full, wait for it to empty out
          if ((_outPos - 1) == _inPos) {
            try {         
              wait(); 
            } catch (InterruptedException e)  { 
              continue; 
            }
          }
          
          acdk::lang::System::arraycopy(_buffer, offset + total_written, buffer, _inPos, bytes_written);
          total_written += bytes_written;
          _inPos += bytes_written;
          
          if (_inPos == buffer.length())
            _inPos = 0;
          
          notifyAll();
        }
      }  else { // Wait for some reads to occur before we write anything.
        try {
          wait();
        } catch (RInterruptedException) { 
          ; 
        }
      }
    }
  } // SYNCHRONIZETHIS();
  
}

//virtual 
void 
PipedReader::reset()
{
  //### /???
}

//virtual 
RString 
PipedReader::getDeviceName()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return "[PipedWriter]"; 
} 

//virtual 
bool 
PipedReader::isWriteable()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return false; 
} 

//virtual 
bool 
PipedReader::isReadable()  
{ 
  THROW1(UnsupportedOperationException, "Not Implemented Yet");
  return false; 
}

} // io
} // acdk

