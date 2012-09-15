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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/File.h,v 1.30 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_File_h
#define acdk_io_File_h
#include <acdk.h>
#include "FilenameFilter.h"
#include "FileFilter.h"
#include "FileReader.h"
#include "Serializable.h"
#include "FileImpl.h"

#include <acdk/lang/Comparable.h>


namespace acdk {
namespace io {

using namespace acdk::lang;


ACDK_DECL_CLASS(File);

/** 
  The File class is a fassade to a virtual filesystem.
  The implementation of a File should implement the FileImpl interface.
  
  Please refer also to gw_ref[acdk_io_FileSystem].
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.30 $
  @date $Date: 2005/04/09 19:26:44 $
  @see FileImpl FileSystem
  @see 
*/



class ACDK_CORE_PUBLIC File
: extends acdk::lang::Object,
  implements Serializable,
  implements acdk::lang::Comparable
{
  ACDK_WITH_METAINFO(File)
protected:
  static const char _pathSeparatorChar;
  static const char _nameSeparatorChar;
  RFileImpl _fileImpl;
public:
  static RObject create_instance() { return new File(RFileImpl(Nil)); }
  //static void registerFileImpl(RFileImplFactory fimplfactory);
  static RFileImpl getFileImpl(IN(RString) fname);
  File(IN(RFileImpl) fileimpl)
  : _fileImpl(fileimpl)
  {
  }
  File(IN(RString) path);
  File(IN(RFile) parent, IN(RString) child);  
  File(IN(RString) parent, IN(RString) child);  
  RFileImpl getFileImpl() { return _fileImpl; }
  virtual RFile makeChild(RString subfile) { return _fileImpl->makeChild(subfile); }
  /**
    return the seperator for PATH declarations
    on Windows this is ';' on unices it is ':'
  */
  static char pathSeparatorChar() { return _pathSeparatorChar; }
  /**
    return the seperator for PATH declarations
    on Windows this is ';' on unices it is ':'
  */
  static RString pathSeparator();
  /*
    return the seperator of file path components
    on Windows this is '\' , on unices it is '/'
  */
  static char separatorChar() { return _nameSeparatorChar; }
  /*
    return the seperator of file path components
    on Windows this is '\' , on unices it is '/'
  */
  static RString separator();
  /*
    return the seperator of file path components
    on Windows this is '\n\r' , on unices it is '\n'
  */
  static RString endOfLine();

// Comparable
  foreign virtual int compareTo(IN(RObject) o);
  virtual int compareTo(IN(RFile) o);
  foreign virtual bool equals(IN(RObject) o);
  foreign virtual int hashCode();
  /**
    API: JDK
    */
  RString getCanonicalPath() { return _fileImpl->getCanonicalPath(); }
  /**
    API: JDK
    */
  RString getAbsolutePath() { return _fileImpl->getAbsolutePath(); }
  /**
    API: JDK
    */
  RString getName() { return _fileImpl->getName(); }
  /**
    API: JDK
    */
  RString getParent() { return _fileImpl->getParentFile()->getPath(); }
  /**
    API: JDK
    */
  RFile getParentFile() { return _fileImpl->getParentFile(); }
  /**
    API: JDK
    */
  RString getPath() { return _fileImpl->getPath(); }
  /**
    API: JDK
    */
  bool isAbsolute() { return _fileImpl->isAbsolute(); }

  /**
    API: JDK
    
  */
/** @group retrive Information about File */
 /**
    API: JDK
    */
  bool exists() { return _fileImpl->exists(); }
  /**
    API: JDK
    */
  bool canRead() { return _fileImpl->canRead(); }
  /**
    API: JDK
    */
  bool canWrite() { return _fileImpl->canWrite(); }
  /**
    API: JDK
    */
  bool isDirectory() { return _fileImpl->isDirectory(); }
  /**
    API: JDK
    */
  bool isFile() { return _fileImpl->isFile(); }
  /**
    API: JDK
    */
  bool isHidden() { return _fileImpl->isHidden(); }
  /**
    API: JDK
    */
  jlong length() { return _fileImpl->length(); }

/** @group file manipulation */
  /**
    API: JDK
    */
  bool createNewFile() { return _fileImpl->createNewFile(); }
  /**
    API: like JDK
    in java the method is named as delete
    */
  bool deleteFile() { return _fileImpl->deleteFile(); }
  /**
    @todo File::deleteOnExit not supported yet
  */
  void deleteOnExit();
  /**
    return the filename matching 
    @param filter if filter is Nil return all found files
    @param listFlags combination of FileListFlags
           if listFlags has FileListRecursive the returned names are
           relative to this File
  */
  RStringArray list(IN(RFilenameFilter) filter = Nil, int listFlags = FileListBoth)
  { 
    return _fileImpl->list(filter, listFlags);
  }
  RFileArray listFiles(IN(RFileFilter) filter = Nil, int listFlags = FileListBoth)
  {
    return _fileImpl->listFiles(filter, listFlags);
  }
  /**
    return the 'roots' of the file system.
    On Windows all mapped drives and on unices simply '/'
  */
  static RFileArray listRoots();
           
  /**
    API: JDK
    Status: not implemented  */
  jlong lastModified() { return _fileImpl->lastModified(); }
  /** 
    API: enhanced
    Status: not tested
  */
  jlong fileCreated() { return _fileImpl->fileCreated(); }
  /**
    mode is the file access bit mask.
    On non Unix platforms it will be ignored.
  */
  bool mkdir(int mode = 0777) { return _fileImpl->mkdir(mode); }
  /**
    try to make directories recursive
  */
  bool mkdirs(int mode = 0777);
  /**
    rename the file. If rename success this File doesn't exists any more
  */
  bool renameTo(IN(RFile) dest) { return _fileImpl->renameTo(dest); }

  bool setLastModified(jlong time) { return _fileImpl->setLastModified(time); }
  bool setFileCreated(jlong time) { return _fileImpl->setFileCreated(time); }
  /** 
    set the file read only or read/write
    @return true on success
  */
  bool setReadOnly(bool doReadOnly) { return setFileAttributes(FileInfoCanWrite, doReadOnly == true ? 0 : FileInfoCanWrite); }
  
  /**
    set the file attribute flags 
    @param mask mask which flags should be set, bit combination FileInfoFlags
    @param flags flags to set, bit combination FileInfoFlags
    @return true on success
  */
  bool setFileAttributes(int mask, int flags) { return _fileImpl->setFileAttributes(mask, flags); }
  /**
    return the cached file information for this file
  */
  virtual RFileInfo getFileInfo() { return _fileImpl->getFileInfo(); }
  
  foreign RString toString();
  
  
  /**
    API: JDK
  */
  static RFile createTempFile(IN(RString) prefix, IN(RString) suffix);
  /**
    API: JDK
  */
  static RFile createTempFile(IN(RString) prefix, IN(RString) suffix, IN(RFile) directory);
  /**
    return the current working directory
   */
  static RString getCWD();
  /**
    return the current working directory
  */
  static RFile getCWDFile();
  /**
    set a new working directory
    @return false if new directory doesn't exists
  */
  static bool setCWD(IN(RString) newpath);

  /** Merges parent and child path.
    API: extended
    
    
    @param parent parent path element ('/usr/opt', 'c:\\files\\project')
    @param child child path element ('local/myapp', 'myproject\\doc')
    @return the concated string ('/usr/opt/local/myapp', 'c:\\files\\project\\myproject\\doc')
    */
  static RString concat(IN(RString) parent, IN(RString) child);
  /**
    return a reader of this file
  */
  virtual RReader getReader();
  /**
    return a writer of this file
  */
  virtual RWriter getWriter();

  /** 
    reads the complete file into a String
    API: ACDK
  */
  RString loadAscii();
  /** 
    reads the complete file into a ByteArray
    API: ACDK
  */
  RbyteArray loadBinary();

};



}
}

#endif //acdk_io_File_h

