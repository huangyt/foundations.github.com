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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/RandomAccessFile.h,v 1.13 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_RandomAccessFile_h
#define acdk_io_RandomAccessFile_h

#include "DataReader.h"
#include "DataWriter.h"
#include "Storage.h"
#include "FileReaderWriterImpl.h"
#include "File.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

enum SeekPos;

ACDK_DECL_CLASS(RandomAccessFile);

/**
  Access a standard system file.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC RandomAccessFile
: extends acdk::lang::Object,
  implements DataReader,
  implements DataWriter,
  implements Storage
{
  ACDK_WITH_METAINFO(RandomAccessFile)
  FileReaderWriterImpl _inOut;
  RFile _file;
  RDataReader _dataReader;
  RDataWriter _dataWriter;
  
public:
  /**
   * Additional features not implemented by jdk:
   * @param dataReader
   *   FileReader using for inputs. Default is BinaryReader. You can set
   *   also other DataReader like ASCIIDataReader, XMLDataReader, ....
   * @param dataWriter
   *   FileWriter using for outputs. Default is BinaryWriter. You can set
   *   also other DataWriters like ASCIIDataWriter, XMLDataWriter, .... */
  RandomAccessFile(IN(RFile) file, IN(RString) mode, IN(RDataReader) dataReader = Nil, IN(RDataWriter) dataWriter = Nil);
  /**
   * Additional features not implemented by jdk:
   * @param dataReader
   *   FileReader using for inputs. Default is BinaryReader. You can set
   *   also other DataReader like ASCIIReader, XMLReader, ....
   * @param dataWriter
   *   FileWriter using for outputs. Default is BinaryWriter. You can set
   *   also other DataWriters like ASCIIWriter, XMLWriter, .... */
  RandomAccessFile(IN(RString) name, IN(RString) mode, IN(RDataReader) dataReader = Nil, IN(RDataWriter) dataWriter = Nil);
  virtual ~RandomAccessFile();

  foreign virtual void setIn(IN(RReader) reader);
  foreign virtual void setOut(IN(RWriter) writer);
  
  foreign virtual void write(const byte* cstr, int offset, int len);
  foreign virtual void write(byte b);
  foreign virtual void write(IN(RbyteArray) ch, int offset = 0, int len = -1);

  foreign virtual void flush();
  foreign virtual void close();
  foreign virtual jlong seek(SeekPos seekrel, jlong seekpos);
  foreign virtual jlong skip(jlong n) { return seek(SeekCur, n); }
  virtual RFileDescriptor getFD();
  virtual jlong getFilePointer();
  virtual jlong length();

  foreign virtual int read();
  foreign virtual int read(IN(RbyteArray) buffer, int offset = 0, int len = -1);
  foreign virtual int read(byte* buffer, int offset, int len);
  foreign virtual void reset();
  foreign virtual bool ready();
 
  foreign virtual void seek(jlong pos);

  /** Note: on Non-Unix it may not work correctly, if newLength < currentLenght */
  virtual void setLength(jlong newLength);
  virtual int skipBytes(int n);
 

  /// implemented DataReader
  foreign virtual bool readBoolean();

  /// implemented DataReader
  foreign virtual char readChar();
  /// implemented DataReader
  foreign virtual uc2char readUcChar();
  /// implemented DataReader
  foreign virtual double readDouble();
  /// implemented DataReader
  foreign virtual float readFloat();
  /// implemented DataReader
  foreign virtual int readInt();
  /// implemented DataReader
  foreign virtual jlong readLong();
  /// implemented DataReader
  foreign virtual short readShort();
  /** JDK implements readUTF instead of readString. The string will be
   *  terminated by '\0' and isn't UTF conform. */
  foreign virtual RString readString();
  /** Not yet implemented (use readString instead). */
  foreign virtual RString readUTF();
  
  /// implemented DataWriter
  foreign virtual void writeBoolean(bool b);
  /// implemented DataWriter
  foreign virtual void writeChar(char b);
  /// implemented DataWriter
  foreign virtual void writeUcChar(uc2char b);
  /// implemented DataWriter
  foreign virtual void writeShort(short b);
  /// implemented DataWriter
  foreign virtual void writeInt(int b);
  /// implemented DataWriter
  foreign virtual void writeLong(jlong b);
  /// implemented DataWriter
  foreign virtual void writeFloat(float b);
  /// implemented DataWriter
  foreign virtual void writeDouble(double b);

  /**
   * Writes the given string.  JDK implements readUTF instead of
   * readString. The string will be terminated by '\0' and isn't UTF
   * conform.
   * @param str
   *   String to write.  */
  foreign virtual void writeString(IN(RString) str);
  /**
   * Not yet implemented (Use writeString instead).
   */
  foreign virtual void writeUTF(IN(RString) str);
  
  /// implemented FileterWriter
  foreign virtual RStorage getStorage();

// Storage
  /// implemented Storage
  foreign virtual RString getDeviceName();
  /// implemented Storage
  foreign virtual bool isWriteable() 
  {
    return _inOut.isWriteable();
  }
  /// implemented Storage
  foreign virtual bool isReadable()
  {
    return _inOut.isReadable();
  }
  /// implemented FilterReader
  foreign virtual RReader getStorageReader() { return this; }
  /// implemented FilterWriter
  foreign virtual RWriter getStorageWriter() { return this; }
};


} // io
} // acdk

#endif //acdk_io_RandomAccessFile_h

